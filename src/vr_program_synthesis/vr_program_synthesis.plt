:- use_module(library('semweb/rdf_db')).
:- use_module(vr_program_synthesis).
:- use_module(utils).
:- begin_rdf_tests(
    'vr_program_synthesis',
    'package://knowrob/owl/test/swrl.owl',
    [ namespace('http://knowrob.org/kb/swrl_test#'),
        setup(vr_program_synthesis_test_setup),
        cleanup(vr_program_synthesis_test_cleanup)
    ]).

vr_program_synthesis_test_setup :-
    tf_logger_enable,

    ros_urdf:urdf_load(kio:'RAMStick_1','/home/lab019/alt/catkin_ws/src/ilias/vr_program_synthesis/urdf/objects/ddr3-4Gbyte-ram-module-kingston.urdf'),
    ros_urdf:urdf_load(ur5:'UR5Robotiq_0','/home/lab019/alt/catkin_ws/src/ilias/vr_program_synthesis/urdf/robots/ur5_robotiq.urdf'),

    % common objects
    kb_project([
        new_iri(Finger1), instance_of(Finger1, 'http://www.ease-crc.org/ont/SOMA.owl#Finger'),
        new_iri(Finger2), instance_of(Finger2, 'http://www.ease-crc.org/ont/SOMA.owl#Finger')
    ]),
    time_scope(0, 100, QS),
    kb_call(has_base_link_name(kio:'RAMStick_1', BaseLinkName)),
    tf_set_pose(BaseLinkName, [world, [0.0, 0.1, 2.32], [0.0, 0.0, 0.0, 1.0]],
        QS),
    kb_call(has_grasp_point(kio:'RAMStick_1', GraspPointName)),
    tf_set_pose(GraspPointName, [BaseLinkName, [0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 1.0]],
        QS),

    % transition 1: robot end effector moved (linear)
    kb_project([
        new_iri(InitialScene), is_individual(InitialScene), instance_of(InitialScene, 'http://www.ease-crc.org/ont/SOMA.owl#Scene'),
        is_state(initialState),
        has_participant(initialState, kio:'RAMStick_1'),
        has_participant(initialState, 'http://knowrob.org/kb/UR5Robotiq.owl#UR5Robotiq_0'),
        holds(InitialScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', initialState),
        has_time_interval(initialState, 3.0, 4.0),

        new_iri(TerminalScene), is_individual(TerminalScene), instance_of(TerminalScene, 'http://www.ease-crc.org/ont/SOMA.owl#Scene'),
        is_state(terminalState),
        has_participant(terminalState, kio:'RAMStick_1'),
        has_participant(terminalState, 'http://knowrob.org/kb/UR5Robotiq.owl#UR5Robotiq_0'),
        holds(TerminalScene, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', terminalState),
        has_time_interval(terminalState, 5.0, 6.0),

        is_individual(moveLinearTransition), instance_of(moveLinearTransition, 'http://www.ease-crc.org/ont/SOMA.owl#StateTransition'),
        holds(moveLinearTransition, 'http://www.ease-crc.org/ont/SOMA.owl#hasInitialScene', InitialScene),
        holds(moveLinearTransition, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalScene', TerminalScene)
        ]),
    time_scope(3.0, 4.0, QScope), tf_set_pose('ee_link', [world,[1.0,0.4,2.32],[0.0,0.0,0.0,1.0]], QScope),
    time_scope(5.0, 6.0, QScope1), tf_set_pose('ee_link', [world,[2.0,0.4,2.32],[0.0,0.0,0.0,1.0]], QScope1),

    % transition 2: object grasped
    kb_project([
        new_iri(InitialScene1), is_individual(InitialScene1), instance_of(InitialScene1, 'http://www.ease-crc.org/ont/SOMA.owl#Scene'),
        new_iri(InitialState1), is_state(InitialState1),
        has_participant(InitialState1, kio:'RAMStick_1'),
        has_participant(InitialState1, 'http://knowrob.org/kb/UR5Robotiq.owl#UR5Robotiq_0'),
        holds(InitialScene1, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', InitialState1),
        has_time_interval(InitialState1, 7.0, 8.0),

        new_iri(TerminalScene1), is_individual(TerminalScene1), instance_of(TerminalScene1, 'http://www.ease-crc.org/ont/SOMA.owl#Scene'),
        new_iri(TerminalState1), is_state(TerminalState1),
        has_participant(TerminalState1, kio:'RAMStick_1'),
        has_participant(TerminalState1, 'http://knowrob.org/kb/UR5Robotiq.owl#UR5Robotiq_0'),
        holds(TerminalScene1, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', TerminalState1),
        has_time_interval(TerminalState1, 9.0, 10.0),

        is_individual(graspTransition), instance_of(graspTransition, 'http://www.ease-crc.org/ont/SOMA.owl#StateTransition'),
        holds(graspTransition, 'http://www.ease-crc.org/ont/SOMA.owl#hasInitialScene', InitialScene1),
        holds(graspTransition, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalScene', TerminalScene1),

        % State that in the terminal state, object is now in contact with the fingers
        is_individual(contactState), instance_of(contactState, 'http://www.ease-crc.org/ont/SOMA.owl#ContactState'),
        holds(contactState, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#classifies', TerminalState1),
        new_iri(ContactRole), is_individual(ContactRole), is_role(ContactRole),
        holds(ContactRole, 'http://www.ease-crc.org/ont/SOMA.owl#hasStateType', contactState),
        has_role(kio:'RAMStick_1', ContactRole),
        new_iri(ContactRole1), is_individual(ContactRole1), is_role(ContactRole1),
        holds(ContactRole1, 'http://www.ease-crc.org/ont/SOMA.owl#hasStateType', contactState),
        has_role(Finger1, ContactRole1),
        new_iri(ContactRole2), is_individual(ContactRole2), is_role(ContactRole2),
        holds(ContactRole2, 'http://www.ease-crc.org/ont/SOMA.owl#hasStateType', contactState),
        has_role(Finger2, ContactRole2)
        ]),
    time_scope(7.0, 8.0, QScope2), tf_set_pose('ee_link', [world,[3.0,0.4,2.32],[0.0,0.0,0.0,1.0]], QScope2),
    time_scope(9.0, 10.0, QScope3), tf_set_pose('ee_link', [world,[4.0,0.4,2.32],[0.0,0.0,0.0,1.0]], QScope3),

    % transition 3: robot end effector moved (nonlinear)
    kb_project([
        new_iri(InitialScene2), is_individual(InitialScene2), instance_of(InitialScene2, 'http://www.ease-crc.org/ont/SOMA.owl#Scene'),
        new_iri(InitialState2), is_state(InitialState2),
        has_participant(InitialState2, kio:'RAMStick_1'),
        has_participant(InitialState2, 'http://knowrob.org/kb/UR5Robotiq.owl#UR5Robotiq_0'),
        holds(InitialScene2, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', InitialState2),
        has_time_interval(InitialState2, 9.0, 10.0),

        new_iri(TerminalScene2), is_individual(TerminalScene2), instance_of(TerminalScene2, 'http://www.ease-crc.org/ont/SOMA.owl#Scene'),
        new_iri(TerminalState2), is_state(TerminalState2),
        has_participant(TerminalState2, kio:'RAMStick_1'),
        has_participant(TerminalState2, 'http://knowrob.org/kb/UR5Robotiq.owl#UR5Robotiq_0'),
        holds(TerminalScene2, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesEvent', TerminalState2),
        has_time_interval(TerminalState2, 11.0, 12.0),

        is_individual(moveToPointTransition), instance_of(moveToPointTransition, 'http://www.ease-crc.org/ont/SOMA.owl#StateTransition'),
        holds(moveToPointTransition, 'http://www.ease-crc.org/ont/SOMA.owl#hasInitialScene', InitialScene2),
        holds(moveToPointTransition, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalScene', TerminalScene2)
        ]),
    time_scope(9.0, 10.0, QScope4), tf_set_pose('ee_link', [world,[1.0,0.4,2.32],[0.0,0.0,0.0,1.0]], QScope4),
    time_scope(10.1, 10.9, QScope5),  tf_set_pose('ee_link', [world,[3.0,0.4,2.32],[0.0,0.0,0.0,1.0]], QScope5),
    time_scope(11.0, 12.0, QScope6), tf_set_pose('ee_link', [world,[2.0,0.7,2.32],[0.0,0.0,0.0,1.0]], QScope6),

    tf_logger_disable.

vr_program_synthesis_test_cleanup :-
    lang_db:drop_graph('user'),
    lang_db:drop_graph('vr_program_synthesis'),
    lang_db:drop_graph('vr_program_synthesis_objects'),
    lang_db:drop_graph('UR5Robotiq').

test('end_effector_pose') :-
    end_effector_pose('http://knowrob.org/kb/UR5Robotiq.owl#UR5Robotiq_0', 4.0, Pose1),
    Pose1 = [world,[1.0,0.4,2.32],[0.0,0.0,0.0,1.0]],
    end_effector_pose('http://knowrob.org/kb/UR5Robotiq.owl#UR5Robotiq_0', 5.0, Pose2),
    Pose2 = [world,[2.0,0.4,2.32],[0.0,0.0,0.0,1.0]].

test('has_time_interval') :-
    kb_call(has_time_interval(initialState, Begin1, End1)),
    Begin1=3.0, End1=4.0,
    kb_call(has_time_interval(terminalState, Begin2, End2)),
    Begin2=5.0, End2=6.0.

test('object_grasped') :-
    kb_call([
        is_state(State), holds(contactState, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#classifies', State)
    ]),
    object_grasped(State, Object),
    Object='http://knowrob.org/kb/vr_program_synthesis_objects.owl#RAMStick_1'.

test('grasp_point') :-
    kb_call(has_grasp_point(kio:'RAMStick_1', GraspPointName)),
    current_scope(QS),
    tf_get_pose(GraspPointName, [world, Pos, Ori], QS, _),
    % Test whether the grasp point in world coordinates is the same as the pose of the object's base link in world coordinates (should be the case for kio:'RAMStick_1')
    kb_call(has_base_link_name(kio:'RAMStick_1', BaseLinkName)),
    tf_get_pose(BaseLinkName, [RefFrame, Pos2, Ori2], QS, _),
    Pos=Pos2, Ori=Ori2.

test('causes_transition_move_linear') :-
    kb_call(subclass_of(ActionType, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#Action')),
    kb_project([
        new_iri(Action), is_individual(Action), instance_of(Action, ActionType)
    ]),
    causes_transition(Action, moveLinearTransition),
    kb_call(instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#MoveLinearAction')).

test('causes_transition_move_to_point') :-
    kb_call(subclass_of(ActionType, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#Action')),
    kb_project([
        new_iri(Action), is_individual(Action), instance_of(Action, ActionType)
    ]),
    causes_transition(Action, moveToPointTransition),
    kb_call(instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#MoveToPointAction')).

test('causes_transition_grasp') :-
    kb_call(subclass_of(ActionType, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#Action')),
    kb_project([
        new_iri(Action), is_individual(Action), instance_of(Action, ActionType)
    ]),
    causes_transition(Action, graspTransition),
    kb_call(instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#GraspAction')).

test('create_action_move_linear') :-
    kb_call(subclass_of(ActionType, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#Action')),
    kb_project([
        new_iri(Action), is_individual(Action), instance_of(Action, ActionType)
    ]),
    create_action(Action, moveLinearTransition),

    % Validate that action is of right type, and that it has the right goal state
    kb_call([
        instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#MoveLinearAction'),
        is_task(Task), holds(Task, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#classifies', Action),
        holds(Task, 'http://www.ease-crc.org/ont/SOMA.owl#hasInputParameter', PoseParam),
        holds(PoseParam, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#classifies', GoalPoseRegion),
        holds(GoalPoseRegion, 'http://www.ease-crc.org/ont/SOMA.owl#hasPositionData', GoalPoseString)
    ]),
    term_string(GoalPose, GoalPoseString),
    GoalPose = [world,[2.0,0.4,2.32],[0.0,0.0,0.0,1.0]].

test('create_action_move_to_point') :-
    kb_call(subclass_of(ActionType, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#Action')),
    kb_project([
        new_iri(Action), is_individual(Action), instance_of(Action, ActionType)
    ]),
    create_action(Action, moveToPointTransition),

    % Validate that action is of right type, and that it has the right goal state
    kb_call([
        instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#MoveToPointAction'),
        is_task(Task), holds(Task, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#classifies', Action),
        holds(Task, 'http://www.ease-crc.org/ont/SOMA.owl#hasInputParameter', PoseParam),
        holds(PoseParam, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#classifies', GoalPoseRegion),
        holds(GoalPoseRegion, 'http://www.ease-crc.org/ont/SOMA.owl#hasPositionData', GoalPoseString)
    ]),
    term_string(GoalPose, GoalPoseString),
    GoalPose = [world,[2.0,0.7,2.32],[0.0,0.0,0.0,1.0]].

test('create_action_grasp') :-
    kb_call(subclass_of(ActionType, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#Action')),
    kb_project([
        new_iri(Action), is_individual(Action), instance_of(Action, ActionType)
    ]),
    create_action(Action, graspTransition),

    % Validate that action is of right type, and that the object is correctly
    % classified as the patient in the action.
    kb_call([
        instance_of(Action, 'http://www.artiminds.com/kb/artm.owl#GraspAction'),
        has_participant(Action, Participant)
    ]),
    (Participant='http://knowrob.org/kb/vr_program_synthesis_objects.owl#RAMStick_1' -> kb_call(is_patient(Participant))).