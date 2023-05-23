:- module(putting_down, []).

:- multifile vrps:equivalent_robot_task/4.

% Putting Down is like "Move Linear Relative Contact", or "Placing" without "Releasing"

vrps:equivalent_robot_task(Action, 'http://www.ease-crc.org/ont/SOMA.owl#PuttingDown', Robot, RobotTask) :-
    % Precondition 1: Some object is grasped in some situation
    object_grasped_at_beginning_of_action(Action, Gripper, GraspedObject),
    % Precondition 2: GraspedObject is not supported by anything
    \+object_supported_at_beginning_of_action(Action, GraspedObject, _),

    % postcondition 1: Object still grasped
    object_grasped_at_end_of_action(Action, Gripper, GraspedObject),
    % postcondition 2: The previously grasped object is now in contact with something that is not the gripper (otherwise it would be 'Dropping')
    objects_touch_at_end_of_action(Action, GraspedObject, Support),
    Support \= Gripper,
    has_feature(Support, Feature), kb_call(instance_of(Feature, vrps:'SurfaceFeature')),
    
    % Task parameters: Moving object is Locatum, motion target is Relatum
    kb_project([
        new_iri(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#PuttingDown'), is_individual(RobotTask), instance_of(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#PuttingDown'),
        task_has_locatum(RobotTask, GraspedObject),
        task_has_relatum(RobotTask, Support)
    ]), !.