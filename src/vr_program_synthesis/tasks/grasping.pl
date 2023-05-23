:- module(grasping, []).

:- multifile vrps:equivalent_robot_task/4.

vrps:equivalent_robot_task(Action, 'http://www.ease-crc.org/ont/SOMA.owl#Grasping', Robot, RobotTask) :-
    % Precondition 1: No object grasped
    \+object_grasped_at_beginning_of_action(Action, _, _),
    
    % Postcondition 1: Object grasped
    object_grasped_at_end_of_action(Action, Gripper, GraspedObject),

    % Task parameters: Grasped object is patient
    kb_project([
        new_iri(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#Grasping'), is_individual(RobotTask), instance_of(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#Grasping'),
        new_iri(PatientRole, 'http://www.ease-crc.org/ont/SOMA.owl#Patient'), is_individual(PatientRole), instance_of(PatientRole, 'http://www.ease-crc.org/ont/SOMA.owl#Patient'),
        task_has_role_with_filler(RobotTask, PatientRole, GraspedObject)
    ]).