:- module(assuming_arm_pose_constrained, []).

:- multifile vrps:equivalent_robot_task/4.

% Free-space motion which has contact with something at some point during the motion (but not at the start)

vrps:equivalent_robot_task(Action, 'http://www.artiminds.com/kb/artm.owl#AssumingArmPoseConstrained', Robot, RobotTask) :-
    writeln('equivalent_robot_task? AssumingArmPoseConstrained'),
    % Precondition 1: No contact
    findall(S1, distinct(has_initial_situation(Action, S1)), InitialSituations),
    format('Precondition 1: Initial situations: ~w~n', [InitialSituations]),
    situations_are_collision_free(InitialSituations),
    writeln('Precondition 1: Situations are collision free'), !,

    % Runtime conditions: Arm movement, but there is a collision at some point
    arm_moved_during_action(Action),
    writeln('Runtime conditions: arm_moved_during_action'),
    \+gripper_moved_during_action(Action),
    writeln('Runtime conditions: not gripper_moved_during_action'),
    action_has_collision(Action),
    writeln('Runtime conditions: not action_is_collision_free'), !,

    % No postconditions

    % Get information about goal pose
    writeln('Parameterizing task'),
    is_performed_by(HumanAction, Agent),
    has_time_interval(HumanAction, StartTime, EndTime),

    end_effector_pose(Agent, StartTime, [world, T0, Q0]),     % in world coords
    end_effector_pose(Agent, EndTime, [world, T1, Q1]),       % in world coords

    % Relative transformation: final pose in coordinate system of initial pose
    transform_between(
		[world, frame_before, T0,Q0],
		[world, frame_after, T1,Q1],
		[frame_after, frame_before, T_rel, Q_rel]),

    format('equivalent_robot_task: relative transform: ~w~n', [T_rel]),
    kb_project([
        new_iri(RobotTask, 'http://www.artiminds.com/kb/artm.owl#AssumingArmPoseConstrained'), is_individual(RobotTask),
        instance_of(RobotTask, 'http://www.artiminds.com/kb/artm.owl#AssumingArmPoseConstrained')
    ]),

    set_pose_parameter(RobotTask, [frame_before, T_rel, Q_rel], 'http://www.artiminds.com/kb/artm.owl#RelativeMotion'), !.