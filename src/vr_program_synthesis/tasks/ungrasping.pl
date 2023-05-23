:- module(ungrasping, []).

:- multifile vrps:equivalent_robot_task/4.

% Ungrasping is like "Ungrasp" in the RPS: "Approach" and "Release"

vrps:equivalent_robot_task(Action, 'http://www.artiminds.com/kb/artm.owl#Ungrasping', Robot, RobotTask) :-
    % Precondition 1: Some object is grasped in some situation
    object_grasped_at_beginning_of_action(Action, Gripper, MovingObject),
    % Precondition 2: MovingObject is not supported by anything
    \+object_supported_at_beginning_of_action(Action, MovingObject, _),
    writeln('vrps:equivalent_robot_task Ungrasping: Preconditions satisfied'),
    
    % Here only the special case of ungrasping something inside or above a container is considered
    \+object_grasped_at_end_of_action(Action, Gripper, MovingObject),
    has_time_interval(Action, _, EndTime), time_scope(EndTime, EndTime, QS),
    instance_of(Container, C), kb_call(subclass_of(C, soma:'DesignedContainer')),
    (shape_contains(MovingObject, Container, QS);
     is_ontop_of(MovingObject, Container, QS)),
    writeln('vrps:equivalent_robot_task Ungrasping: Runtime conditions satisfied'),

    % Task parameters: Moving object is Locatum, target object is Relatum
    kb_project([
        new_iri(RobotTask, 'http://www.artiminds.com/kb/artm.owl#Ungrasping'), is_individual(RobotTask), instance_of(RobotTask, 'http://www.artiminds.com/kb/artm.owl#Ungrasping'),
        task_has_locatum(RobotTask, MovingObject),
        task_has_relatum(RobotTask, Container)
    ]), 
    writeln('vrps:equivalent_robot_task Ungrasping: Task patameterized'),
    !.