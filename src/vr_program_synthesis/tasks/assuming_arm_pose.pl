:- module(assuming_arm_pose, []).

:- multifile vrps:equivalent_robot_task/4.

vrps:equivalent_robot_task(Action, 'http://www.ease-crc.org/ont/SOMA.owl#AssumingArmPose', Robot, RobotTask) :-
    % No pre- or postconditions (is always possible)
    % Runtime condition: Collision-free arm motion, no gripper motion
    arm_moved_during_action(Action),
    \+gripper_moved_during_action(Action),
    action_is_collision_free(Action),

    kb_project([
        new_iri(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#AssumingArmPose'), is_individual(RobotTask), instance_of(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#AssumingArmPose')
    ]),

    is_performed_by(HumanAction, Agent),
    has_time_interval(HumanAction, StartTime, EndTime),
    % Goal pose in world coordinates
    end_effector_pose(Agent, EndTime, GoalPose),
    set_pose_parameter(RobotTask, GoalPose, 'http://www.artiminds.com/kb/artm.owl#GoalPose'), !.