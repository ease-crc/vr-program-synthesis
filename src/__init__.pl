:- register_ros_package(knowrob).

:- load_owl('/home/lab019/alt/catkin_ws/src/soma/owl/SOMA-ACT.owl', [namespace(soma)]).
:- load_owl('package://vr_program_synthesis/owl/artm.owl', [ namespace(artm, 'http://www.artiminds.com/kb/artm.owl#') ]).
:- load_owl('package://vr_program_synthesis/owl/vr_program_synthesis_objects.owl',
    [namespace(kio, 'http://knowrob.org/kb/vr_program_synthesis_objects.owl#')]).
:- load_owl('package://vr_program_synthesis/owl/vr_program_synthesis.owl', [namespace(vrps, 'http://www.artiminds.com/kb/vr_program_synthesis.owl#')]).
:- load_owl('package://vr_program_synthesis/owl/UR5Robotiq.owl', [ namespace(ur5, 'http://knowrob.org/kb/UR5Robotiq.owl#') ]).
:- load_owl('package://vr_program_synthesis/owl/item_hardware.owl', [ namespace(item, 'http://knowrob.org/kb/item_hardware.owl#') ]).

:- use_directory('vr_program_synthesis').
:- use_directory('motion_analysis').
