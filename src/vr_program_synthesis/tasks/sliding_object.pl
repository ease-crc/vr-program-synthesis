:- module(sliding_object, []).

:- multifile vrps:equivalent_robot_task/4.

:- use_module(library('semweb/rdf_db'),
	[ rdf_split_url/3 ]).

vrps:equivalent_robot_task(Action, 'http://www.artiminds.com/kb/artm.owl#SlidingObject', Robot, RobotTask) :-
    % Precondition 1: Object grasped
    object_grasped_at_beginning_of_action(Action, Gripper, GraspedObject),
    % Precondition 2: Grasped object in contact with some other object
    object_supported_at_beginning_of_action(Action, GraspedObject, Supporter), Supporter \= Gripper,

    % Runtime conditions
    arm_moved_during_action(Action),
    \+gripper_moved_during_action(Action),
    % Runtime condition 1: Object grasped
    object_grasped_during_action(Action, Gripper, GraspedObject),
    % Runtime condition 2: Grasped object in contact with some other object
    objects_touch_during_action(Action, GraspedObject, OtherObject), 
    OtherObject \= Gripper, 

    % Postcondition 1: Object still grasped
    object_grasped_at_end_of_action(Action, Gripper, GraspedObject),
    % Postcondition 2: Grasped object still in contact with some other object
    objects_touch_at_end_of_action(Action, GraspedObject, OtherObject),
    OtherObject \= Gripper,

    is_performed_by(HumanAction, Agent),
    has_time_interval(HumanAction, StartTime, EndTime),

    % Grasped object is locatum (the sliding object); other object is relatum (the object it is sliding against)

    rdf_split_url(_, RelatumFrame, Supporter),
    end_effector_pose(Agent, StartTime, [world, T0, Q0]),     % in world coords
    end_effector_pose(Agent, EndTime, [world, T1, Q1]),       % in world coords

    % Relative transformation: final pose in coordinate system of initial pose
    transform_between(
		[world, frame_before, T0,Q0],
		[world, frame_after, T1,Q1],
		[frame_after, frame_before, T_rel, Q_rel]),
    
    kb_project([
        new_iri(RobotTask, 'http://www.artiminds.com/kb/artm.owl#SlidingObject'), is_individual(RobotTask), instance_of(RobotTask, 'http://www.artiminds.com/kb/artm.owl#SlidingObject')
    ]),

    % Task parameters: Locatum (grasped object, the moving object), relatum (the supporting object), relative motion
    set_pose_parameter(RobotTask, [RelatumFrame, T_rel, Q_rel], 'http://www.artiminds.com/kb/artm.owl#RelativeMotion'),
    kb_project([
        task_has_locatum(RobotTask, GraspedObject),
        task_has_relatum(RobotTask, Supporter)
    ]), !.