:- use_module(library('semweb/rdf_db')).
:- use_module(library('lang/db')).
:- use_module(test_scenarios).
:- use_module(action_inference_vr_neem).
:- begin_rdf_tests(
    'action_inference_vr_neem',
    'package://knowrob/owl/test/swrl.owl',
    [ namespace('http://knowrob.org/kb/swrl_test#'),
        setup(action_inference_vr_neem_test_setup),
        cleanup(action_inference_vr_neem_test_cleanup)
    ]).

action_inference_vr_neem_test_setup :-
    ensure_loaded('/home/lab019/alt/catkin_ws/src/neem-interface/src/neem-interface.pl'),
    remember('/home/lab019/alt/vr-neem-converter/output/ILIAS_1_T/1640009368.486422').

action_inference_vr_neem_test_cleanup :-
    mem_clear_memory.

test('action_inference_action_0') :-
    % Expect an AssumingArmPose task
    kb_call(is_action(Action)), has_time_interval(Action, 7.294466, 7.404583),
    equivalent_robot_task(Action, RobotTask),
    kb_call(instance_of(RobotTask, soma:'AssumingArmPose')).

test('action_inference_action_1') :-
    % Expect an AssumingArmPose task
    kb_call(is_action(Action)), has_time_interval(Action, 7.404583, 8.915608),
    equivalent_robot_task(Action, RobotTask),
    kb_call(instance_of(RobotTask, soma:'AssumingArmPose')).

test('action_inference_action_2') :-
    % Expect a Reaching task
    kb_call(is_action(Action)), has_time_interval(Action, 8.915608, 10.671001),
    equivalent_robot_task(Action, RobotTask),
    kb_call(instance_of(RobotTask, soma:'Reaching')).

test('action_inference_action_3') :-
    % Expect a Grasping task
    kb_call(is_action(Action)), has_time_interval(Action, 10.671001, 11.8),
    equivalent_robot_task(Action, RobotTask),
    kb_call(instance_of(RobotTask, soma:'Grasping')).

test('action_inference_action_4') :-
    % PickUp in demonstration --> ExtractFromPeg task
    kb_call(is_action(Action)), has_time_interval(Action, 11.8, 13.581123),
    equivalent_robot_task(Action, RobotTask),
    kb_call([
        instance_of(RobotTask, artm:'ExtractHoleFromPeg'),
        task_has_locatum(RobotTask, 'http://knowrob.org/kb/ameva_log.owl#xV-vxMHrR0GJBOzkrti7FA'),
        task_has_relatum(RobotTask, 'http://knowrob.org/kb/ameva_log.owl#tMwn8o6kC0aDQZSrcgRqwQ')
    ]).

test('action_inference_action_5') :-
    % Expect an AssumingArmPose task
    kb_call(is_action(Action)), has_time_interval(Action, 13.581123, 15.65922),
    equivalent_robot_task(Action, RobotTask),
    kb_call(instance_of(RobotTask, soma:'AssumingArmPose')).

test('action_inference_action_6') :-
    % Expect a Placing task
    kb_call(is_action(Action)), has_time_interval(Action, 15.65922, 15.935316),
    equivalent_robot_task(Action, RobotTask),
    kb_call([
        instance_of(RobotTask, soma:'Placing'),
        task_has_locatum(RobotTask, 'http://knowrob.org/kb/ameva_log.owl#xV-vxMHrR0GJBOzkrti7FA'),
        task_has_relatum(RobotTask, 'http://knowrob.org/kb/ameva_log.owl#SrBxKF-Nzke1IHqGuc-PkA')
    ]).

test('action_inference_action_7') :-
    % Expect an AssumingArmPose task
    kb_call(is_action(Action)), has_time_interval(Action, 15.935316, 16.314367),
    equivalent_robot_task(Action, RobotTask),
    kb_call(instance_of(RobotTask, 'http://www.artiminds.com/kb/artm.owl#Sliding')).

test('action_inference_action_8') :-
    % Expect an AssumingArmPose task
    kb_call(is_action(Action)), has_time_interval(Action, 16.314367, 16.379852),
    equivalent_robot_task(Action, RobotTask),
    kb_call([
        instance_of(RobotTask, soma:'Retracting'),
        task_has_locatum(RobotTask, 'http://knowrob.org/kb/ameva_log.owl#xV-vxMHrR0GJBOzkrti7FA')
    ]).

test('action_inference_action_9') :-
    % Expect an AssumingArmPose task
    kb_call(is_action(Action)), has_time_interval(Action, 16.379852, 17.823286),
    equivalent_robot_task(Action, RobotTask),
    kb_call(instance_of(RobotTask, soma:'AssumingArmPose')).
