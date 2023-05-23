:- module(retracting, []).

:- multifile vrps:equivalent_robot_task/4.

vrps:equivalent_robot_task(Action, 'http://www.ease-crc.org/ont/SOMA.owl#Retracting', Robot, RobotTask) :-
    % Precondition 1: No object grasped
    \+object_grasped_at_beginning_of_action(Action, _, _),
    % Precondition 2: Robot or gripper in contact with some object
    is_performed_by(Action, Agent), has_gripper(Agent, Gripper),
    distinct(has_initial_situation(Action, Situation1)),
    (objects_touch_in_situation(Agent, Object, Situation1);
    objects_touch_in_situation(Gripper, Object,  Situation1)),

    % postcondition 1: No object grasped in any situation, robot in contact with any object in any situation
    forall(distinct(has_terminal_situation(Action, Situation2)),
           (\+object_grasped_in_situation(_, _, Situation2),
            situation_is_collision_free(Situation2)
           )),

     % Task parameters: Object to depart from
    kb_project([
        new_iri(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#Retracting'), is_individual(RobotTask), instance_of(RobotTask, 'http://www.ease-crc.org/ont/SOMA.owl#Retracting'),
        task_has_relatum(RobotTask, Object)
    ]), !.