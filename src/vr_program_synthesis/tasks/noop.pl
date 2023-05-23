:- module(noop, []).

:- multifile vrps:equivalent_robot_task/4.

vrps:equivalent_robot_task(Action, 'http://www.artiminds.com/kb/artm.owl#NoOp', Robot, RobotTask) :-
    % No pre- or postconditions (is always possible)
    % Runtime condition: Nothing happened (no arm movement, no gripper movement)
    \+arm_moved_during_action(Action),
    \+gripper_moved_during_action(Action),

    kb_project([
        new_iri(RobotTask, 'http://www.artiminds.com/kb/artm.owl#NoOp'), is_individual(RobotTask), instance_of(RobotTask, 'http://www.artiminds.com/kb/artm.owl#NoOp')
    ]), !.