:- module(picking_up, []).

:- multifile vrps:equivalent_robot_task/4.

vrps:equivalent_robot_task(Action, 'http://www.ease-crc.org/ont/SOMA.owl#PickingUp', Robot, RobotTask) :-
    % Precondition 1: Some object grasped in some situation
    object_grasped_at_beginning_of_action(Action, Gripper, GraspedObject),
    % Precondition 2: The grasped object is supported by something that does not constrain it along more than 1 dimension
    object_supported_at_beginning_of_action(Action, GraspedObject, SupportingObject),
    constrains_dof(SupportingObject, NumDofConstrained), NumDofConstrained < 2,
    GraspedObject \= SupportingObject,

    % Postcondition 1: Object still grasped
    object_grasped_at_end_of_action(Action, Gripper, GraspedObject),
    % postcondition 2: Grasped object not supported by something anymore
    forall(distinct(has_terminal_situation(Action, Situation2)),
        \+object_supported_in_situation(GraspedObject, _, Situation2)),

    % Task parameters: Moving object is Locatum, supporting object is Relatum
    kb_project([
        new_iri(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#PickingUp'), is_individual(RobotTask), instance_of(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#PickingUp'),
        task_has_locatum(RobotTask, GraspedObject),
        task_has_relatum(RobotTask, SupportingObject)
    ]), !.