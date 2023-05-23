:- module(placing, []).

:- multifile vrps:equivalent_robot_task/4.

% Placing is like "Ungrasp Contact"

vrps:equivalent_robot_task(Action, 'http://www.ease-crc.org/ont/SOMA.owl#Placing', Robot, RobotTask) :-  
    % Precondition 1: Some object is grasped in some situation
    object_grasped_at_beginning_of_action(Action, Gripper, GraspedObject),
    % Precondition 2: GraspedObject is not supported by anything
    \+object_supported_at_beginning_of_action(Action, GraspedObject, _),

    % postcondition 1: No object grasped in any situation
   \+object_grasped_at_end_of_action(Action, Gripper, GraspedObject), 

    % postcondition 2: The previously grasped object is now in contact with something that is not the gripper (otherwise it would be 'Dropping')
    objects_touch_at_end_of_action(Action, GraspedObject, Support),
    Support \= Gripper,
    has_feature(Support, Feature), kb_call(instance_of(Feature, vrps:'SurfaceFeature')),

    % Task parameters: Moving object is Locatum, motion target is Relatum
    kb_project([
        new_iri(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#Placing'), is_individual(RobotTask), instance_of(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#Placing'),
        task_has_locatum(RobotTask, GraspedObject),
        task_has_relatum(RobotTask, Support)
    ]), !.