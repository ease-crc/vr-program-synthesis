:- module(vr_program_synthesis, [
    causes_transition/2,
    create_action/2
]).

:- use_module(library('reasoning/OWL/plowl/individual')).
:- use_module(utils).


transition_moves_arm(Transition) :-
    kb_call([
        instance_of(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#StateTransition'),
        holds(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#hasInitialScene', InitialScene),
        holds(InitialScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', InitialState), is_state(InitialState),
        holds(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalScene', TerminalScene),
        holds(TerminalScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', TerminalState), is_state(TerminalState)
    ]),
    end_effector_moved(InitialState, TerminalState).

transition_grasps_object(Transition, Object) :-
    write('transition_grasps_object? for '), writeln(Transition),
    kb_call([
        instance_of(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#StateTransition'),
        holds(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#hasInitialScene', InitialScene),
        holds(InitialScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', InitialState), is_state(InitialState)]),
    not(object_grasped(InitialState,_)),
    kb_call([
        holds(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalScene', TerminalScene),
        holds(TerminalScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', TerminalState), is_state(TerminalState)]),
    object_grasped(TerminalState, Object),
    writeln('transition_grasps_object!').

transition_ungrasps_object(Transition, Object) :-
    write('transition_ungrasps_object? for '), writeln(Transition),
    kb_call([
        instance_of(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#StateTransition'),
        holds(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#hasInitialScene', InitialScene),
        holds(InitialScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', InitialState), is_state(InitialState)]),
    object_grasped(InitialState,Object),
    kb_call([
        holds(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalScene', TerminalScene),
        holds(TerminalScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', TerminalState), is_state(TerminalState)]),
    not(object_grasped(TerminalState, Object)),
    writeln('transition_ungrasps_object!').

transition_is_linear(Transition) :-
    write('transition_is_linear? for '), writeln(Transition),
    kb_call([
        instance_of(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#StateTransition'),
        holds(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#hasInitialScene', InitialScene),
        holds(InitialScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', InitialState), is_state(InitialState),
        has_time_interval(InitialState, InitialStartTime, InitialEndTime),
        has_participant(InitialState, Robot),
        holds(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalScene', TerminalScene),
        holds(TerminalScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', TerminalState), is_state(TerminalState),
        has_time_interval(TerminalState, TerminalStartTime, TerminalEndTime)]),
    end_effector_trajectory(Robot, InitialStartTime, TerminalEndTime, EETrajectory),!,
    motion_is_linear(EETrajectory),
    writeln('transition_is_linear!').

% A MoveToPointAction causes a change in the pose of the end effector.
% It also has a joint-space linearity constraint on the motion.
causes_transition(Action, Transition) :-
    is_individual(Action), instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#MoveToPointAction'),
    transition_moves_arm(Transition),
    not(transition_is_linear(Transition)).  % TODO: Replace with statement that transition is linear in joint space

% A MoveLinearAction causes a change in the pose of the end effector.
% It also has a cartesian linearity constraint on the motion.
causes_transition(Action, Transition) :-
    is_individual(Action), instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#MoveLinearAction'),
    transition_moves_arm(Transition),
    transition_is_linear(Transition).

% A GraspAction causes a transition from a situation where no object is grasped to one where an object is grasped.
% It also moves the arm.
causes_transition(Action, Transition) :-
    is_individual(Action), instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#GraspAction'),
    transition_grasps_object(Transition, _),
    transition_moves_arm(Transition).

% An OpenGripperAction causes a transition from a situation where an object is grasped to one where no object is grasped
% It does not move the arm.
causes_transition(Action, Transition) :-
    is_individual(Action), instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#OpenGripperAction'),
    transition_ungrasps_object(Transition, _),
    not(transition_moves_arm(Transition)).


%%%%%%%%%% ACTION PARAMETERIZATION %%%%%%%%%%%

create_action(Action, Transition) :-
    is_individual(Action), instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#MoveToPointAction'),
    causes_transition(Action, Transition),
    kb_call(holds(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalScene', TerminalScene)),

    % Setup properties common to all actions
    create_action_generic(Action, Transition, Task),

    % The ee pose at the terminal scene of the transition is the goal pose of the motion
    kb_call(holds(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalScene', TerminalScene)),
    kb_call(holds(TerminalScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', TerminalState)),
    kb_call(has_time_interval(TerminalState, TerminalStartTime, TerminalEndTime)),
    kb_call(has_participant(TerminalState, Robot)),
    end_effector_pose(Robot, TerminalEndTime, EEPoseTerminal),
    set_goal_pose_parameter(Task, EEPoseTerminal).

% Move Linear
create_action(Action, Transition) :-
    is_individual(Action), instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#MoveLinearAction'),
    causes_transition(Action, Transition),

    % Setup properties common to all actions
    create_action_generic(Action, Transition, Task),

    % The ee pose at the terminal scene of the transition is the goal pose of the motion
    kb_call(holds(Transition, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalScene', TerminalScene)),
    kb_call(holds(TerminalScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', TerminalState)),
    kb_call(has_time_interval(TerminalState, TerminalStartTime, TerminalEndTime)),
    kb_call(has_participant(TerminalState, Robot)),
    end_effector_pose(Robot, TerminalEndTime, EEPoseTerminal),
    set_goal_pose_parameter(Task, EEPoseTerminal).

% Grasp
create_action(Action, Transition) :-
    is_individual(Action), instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#GraspAction'),
    causes_transition(Action, Transition),
    transition_grasps_object(Transition, Object),

    % Setup properties common to all actions
    create_action_generic(Action, Transition, Task),

    % The grasped object has a patient role in this action
    kb_project([
        new_iri(Patient, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#Role'), is_individual(Patient), instance_of(Patient, 'http://www.ease-crc.org/ont/SOMA.owl#Patient'), has_task_role(Task, Patient),  has_role(Object, Patient)
    ]).

% Open Gripper
create_action(Action, Transition) :-
    is_individual(Action), instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#OpenGripperAction'),
    causes_transition(Action, Transition),
    transition_ungrasps_object(Transition, Object),

    % Setup properties common to all actions
    create_action_generic(Action, Transition, Task).
