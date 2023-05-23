:- module(releasing, []).

:- multifile vrps:equivalent_robot_task/4.

% Releasing is like "Open Gripper"

vrps:equivalent_robot_task(Action, 'http://www.ease-crc.org/ont/SOMA.owl#Releasing', Robot, RobotTask) :-
    % Precondition 1: Some object is grasped in some situation
    object_grasped_at_beginning_of_action(Action, Gripper, Object),

    % postcondition 1: No object grasped in any situation
    \+object_grasped_at_end_of_action(Action, _, _),
    % postcondition 2: The previously grasped object is now in contact with something that is not the gripper (otherwise it would be 'Dropping')
    objects_touch_at_end_of_action(Action, Object, Object2),
    Object2 \= Gripper, 

    % Task parameters: Moving object is Locatum, motion target is Relatum
    kb_project([
        new_iri(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#Releasing'), is_individual(RobotTask), instance_of(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#Releasing'),
        task_has_locatum(RobotTask, Object),
        task_has_relatum(RobotTask, Object2)
    ]), !.
