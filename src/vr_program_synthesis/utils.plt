:- use_module(library('semweb/rdf_db')).
:- use_module(utils).
:- begin_rdf_tests(
    'utils',
    'package://knowrob/owl/test/swrl.owl',
    [ namespace('http://knowrob.org/kb/swrl_test#'),
        setup(utils_test_setup),
        cleanup(utils_test_cleanup)
    ]).

utils_test_setup :-
    tf_logger_enable,
    kb_project([
        new_iri(Agent), is_individual(Agent), instance_of(Agent, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#PhysicalAgent'),
        new_iri(Hand), is_individual(Hand), instance_of(Hand, 'http://www.ease-crc.org/ont/SOMA.owl#Hand'),
        has_end_link(Agent, Hand),
        new_iri(Thumb), is_individual(Thumb), instance_of(Thumb, 'http://www.ease-crc.org/ont/SOMA.owl#Thumb'),
        new_iri(IndexFinger), is_individual(IndexFinger), instance_of(IndexFinger, 'http://www.ease-crc.org/ont/SOMA.owl#IndexFinger'),
        holds(Hand, 'http://www.ease-crc.org/ont/SOMA.owl#hasFinger', Thumb), holds(Hand, 'http://www.ease-crc.org/ont/SOMA.owl#hasFinger', IndexFinger),

        is_individual(situation1), instance_of(situation1, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#Situation'),
        new_iri(State1), is_individual(State1), instance_of(State1, 'http://www.ease-crc.org/ont/SOMA.owl#State'),
        holds(State1, 'http://www.ease-crc.org/ont/SOMA.owl#involvesEffector', Hand),
        has_time_interval(State1, 1, 2),
        holds(situation1, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', State1),
        holds(situation1, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesAgent', Agent)
    ]),
    time_scope(1, 2, QScope),
    tf_set_pose(IndexFinger, [world, [0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 1.0]], QScope),
    tf_set_pose(Thumb, [world, [0.02, 0.0, 0.0], [0.0, 0.0, 0.0, 1.0]], QScope),
    tf_logger_disable.

utils_test_cleanup :-
    lang_db:drop_graph('user'),
    lang_db:drop_graph('vr_program_synthesis'),
    lang_db:drop_graph('vr_program_synthesis_objects'),
    lang_db:drop_graph('UR5Robotiq').

test('gripper_open') :-
    gripper_open(2),
    gripper_open(0.02),
    not(gripper_open(0.005)).

test('gripper_open_in_situation') :-
    gripper_open_in_situation(situation1).