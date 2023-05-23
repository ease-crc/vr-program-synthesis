:- use_module(library('rostest')).
:- use_module(library('semweb/rdf_db')).

:- use_module('motion_analysis').

:- begin_rdf_tests(
    'motion_analysis',
    'package://knowrob/owl/test/swrl.owl',
    [ namespace('http://knowrob.org/kb/swrl_test#'),
      setup(motion_analysis_setup),
      cleanup(motion_analysis_cleanup)
    ]).

:- rdf_meta(test_motion_is_linear(r)).
:- rdf_meta(test_motion_not_linear(r)).

motion_analysis_setup :-
	tf_mng_drop,
	tf_logger_enable.

motion_analysis_cleanup :-
	tf_logger_disable,
	tf_mng_drop.

% some test facts
test_pose_linear_pos0([world,[1.0,0.4,2.32],[0.0,0.0,0.0,1.0]], 1593178679.123).
test_pose_linear_pos1([world,[2.0,0.5,2.31],[0.0,0.0,0.0,1.0]], 1593178680.123).
test_pose_linear_pos2([world,[3.0,0.6,2.30],[0.0,0.0,0.0,1.0]], 1593178681.123).

test_pose_linear_ori0([world,[1.0,0.4,2.32],[ 0.0698801, 0.0465867, 0.0232934, 0.9961947 ]], 1593178679.123).   % axangle [3,2,1], 10 deg
test_pose_linear_ori1([world,[1.0,0.4,2.32],[ 0.1392283, 0.0928189, 0.0464094, 0.9848078 ]], 1593178680.123).   % axangle [3,2,1], 20 deg
test_pose_linear_ori2([world,[1.0,0.4,2.32], [ 0.2075169, 0.1383446, 0.0691723, 0.9659258 ]], 1593178681.123).   % axangle [3,2,1], 30 deg

test_pose_nonlinear_pos0([world,[0.0,1.0,0.0],[0.0,0.0,0.0,1.0]], 1593178680.123).
test_pose_nonlinear_pos1([world,[0.0,0.0,1.0],[0.0,0.0,0.0,1.0]], 1593178681.123).
test_pose_nonlinear_pos2([world,[0.0,1.0,0.0],[0.0,0.0,0.0,1.0]], 1593178682.123).

test_pose_nonlinear_ori0([world,[1.0,0.4,2.32],[ 0.0698801, 0.0465867, 0.0232934, 0.9961947 ]], 1593178679.123).   % axangle [3,2,1], 10 deg
test_pose_nonlinear_ori1([world,[1.0,0.4,2.32],[ 0.0232934, 0.0465867, 0.0698801, 0.9961947 ]], 1593178679.123).   % axangle [1,2,3], 10 deg
test_pose_nonlinear_ori2([world,[1.0,0.4,2.32],[ 0.1392283, 0.0928189, 0.0464094, 0.9848078 ]], 1593178680.123).   % axangle [3,2,1], 20 deg

test_motion_is_linear(Trajectory) :-
    assert_true(motion_is_linear(Trajectory)).

test_motion_not_linear(Trajectory) :-
    assert_false(motion_is_linear(Trajectory)).

test('motion_is_linear') :-
    test_pose_linear_pos0(Pose0,Stamp0),
	test_pose_linear_pos1(Pose1,Stamp1),
	test_pose_linear_pos2(Pose2,Stamp2),
    test_motion_is_linear([Stamp0-Pose0, Stamp1-Pose1, Stamp2-Pose2]),

    test_pose_linear_ori0(Pose3,Stamp3),
	test_pose_linear_ori1(Pose4,Stamp4),
	test_pose_linear_ori2(Pose5,Stamp5),
    test_motion_is_linear([Stamp3-Pose3, Stamp4-Pose4, Stamp5-Pose5]),

    test_pose_nonlinear_pos0(Pose6, Stamp6),
    test_pose_nonlinear_pos1(Pose7, Stamp7),
    test_pose_nonlinear_pos2(Pose8, Stamp8),
    test_motion_not_linear([Stamp6-Pose6, Stamp7-Pose7, Stamp8-Pose8]),

    test_pose_nonlinear_ori0(Pose9, Stamp9),
    test_pose_nonlinear_ori1(Pose10, Stamp10),
    test_pose_nonlinear_ori2(Pose11, Stamp11),
    test_motion_not_linear([Stamp9-Pose9, Stamp10-Pose10, Stamp11-Pose11]).
