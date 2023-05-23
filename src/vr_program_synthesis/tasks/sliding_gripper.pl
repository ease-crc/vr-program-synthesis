:- module(sliding_gripper, []).

:- multifile vrps:equivalent_robot_task/4.

:- use_module(library('semweb/rdf_db'),
	[ rdf_split_url/3 ]).

vrps:equivalent_robot_task(Action, 'www.artiminds.com/kb/artm.owl#SlidingGripper', Robot, RobotTask) :-
    % Precondition 1: No object grasped
    \+object_grasped_at_beginning_of_action(Action, _, _),
    % Precondition 2: Robot or gripper in contact with some object
    is_performed_by(Action, Agent), has_gripper(Agent, Gripper),
    (objects_touch_at_beginning_of_action(Action, Agent, Supporter);
     objects_touch_at_beginning_of_action(Action, Gripper, Supporter)),

    % Runtime conditions
    arm_moved_during_action(Action),
    % Runtime condition 1: No object grasped
    \+object_grasped_during_action(Action, _, _), 
    % Runtime condition 2: Robot or gripper in contact with some object
    (objects_touch_during_action(Action, Agent, Supporter);
     objects_touch_during_action(Action, Gripper, Supporter)),

    % Postcondition 1: No object grasped
    \+object_grasped_at_end_of_action(Action, _, _),
    % Postcondition 2: Robot or gripper in contact with some object
    (objects_touch_at_end_of_action(Action, Agent, Supporter);
     objects_touch_at_end_of_action(Action, Gripper, Supporter)),

    has_time_interval(HumanAction, StartTime, EndTime),

    % Gripper is locatum (the sliding object); other object is relatum (the object it is sliding against)
    rdf_split_url(_, RelatumFrame, Supporter),
    end_effector_pose(Agent, StartTime, [world, T0, Q0]),     % in world coords
    end_effector_pose(Agent, EndTime, [world, T1, Q1]),       % in world coords

    % Relative transformation: final pose in coordinate system of initial pose
    transform_between(
		[world, frame_before, T0,Q0],
		[world, frame_after, T1,Q1],
		[frame_after, frame_before, T_rel, Q_rel]),

    kb_project([
        new_iri(RobotTask, 'http://www.artiminds.com/kb/artm.owl#SlidingGripper'), is_individual(RobotTask), instance_of(RobotTask, 'http://www.artiminds.com/kb/artm.owl#SlidingGripper')
    ]),


    % Task parameters: Locatum (gripper, the moving object), relatum (the supporting object), relative motion
    set_pose_parameter(RobotTask, [RelatumFrame, T_rel, Q_rel], 'http://www.artiminds.com/kb/artm.owl#RelativeMotion'),
    kb_project([
        task_has_locatum(RobotTask, Gripper),
        task_has_relatum(RobotTask, Supporter)
    ]),!.