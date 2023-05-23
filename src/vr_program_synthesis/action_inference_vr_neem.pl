:- module(action_inference_vr_neem, [
    equivalent_robot_task/2,
    list_all_actions/0
]).

:- use_module(library('semweb/rdf_db'),
	[ rdf_split_url/3 ]).

:- use_module('tasks/assuming_arm_pose').
:- use_module('tasks/assuming_arm_pose_constrained').
:- use_module('tasks/extract_hole_from_peg').
:- use_module('tasks/grasping').
:- use_module('tasks/noop').
:- use_module('tasks/insert_hole_onto_peg').
:- use_module('tasks/picking_up').
:- use_module('tasks/placing').
:- use_module('tasks/putting_down').
:- use_module('tasks/releasing').
:- use_module('tasks/retracting').
:- use_module('tasks/sliding_gripper').
:- use_module('tasks/sliding_object').
:- use_module('tasks/ungrasping').

set_base_pose(Robot, BasePose) :-
    kb_call(holds(Robot, 'http://knowrob.org/kb/urdf.owl#hasBaseLink', BaseLink)),
    time_scope(=(0), =<('Infinity'), FScope),
    tf_logger_enable,
    tf_set_pose(BaseLink, BasePose, FScope),
    tf_logger_disable.

has_base_frame(Robot, BaseFrame) :-
    kb_call(holds(Robot, 'http://knowrob.org/kb/urdf.owl#hasBaseLink', BaseLink)),
    rdf_split_url(_,BaseFrame,BaseLink).

equivalent_robot_task(HumanAction, RobotTask) :-
    executes_task(HumanAction, HumanActionTask),
    equivalent_robot_task(HumanAction, HumanActionTask, RobotTask).

equivalent_robot_task(HumanAction, HumanActionTask, RobotTask) :-
    % For some task types, the tasks in the NEEM are already good
    instance_of(HumanActionTask, TaskType),
    member(TaskType, ['http://www.ease-crc.org/ont/SOMA.owl#Reaching']),
    RobotTask=HumanActionTask.

equivalent_robot_task(HumanAction, HumanActionTask, RobotTask) :-
    instance_of(HumanActionTask, 'http://www.ease-crc.org/ont/SOMA.owl#PhysicalTask'),
    distinct(subclass_of(RobotTaskType, 'http://www.ease-crc.org/ont/SOMA.owl#PhysicalTask')),
    format('equivalent_robot_task? ~w~n', RobotTaskType),
    % HumanAction satisfies the Pre- and Postconditions? --> can create equivalent parameterized robot task
    vrps:equivalent_robot_task(HumanAction, RobotTaskType, 'http://knowrob.org/kb/UR5Robotiq.owl#UR5Robotiq_0', RobotTask).


%%%%%%%%%%%%% TESTING

list_all_actions :-
    % List all actions in the NEEM, with start and end times.
    findall(Action, kb_call(is_action(Action)), Actions),
    forall(member(Action1, Actions),
           (has_time_interval(Action1, StartTime, EndTime),
            kb_call([executes_task(Action1, Task), instance_of(Task, TaskType), subclass_of(TaskType, dul:'Task')]),
            format('Action ~w of TaskType ~w: ~w -> ~w~n', [Action1, TaskType, StartTime, EndTime]))).

infer_all_tasks(Action, RobotTask, ActionStartTime, ActionEndTime) :-
    kb_call(is_action(Action)),
    has_time_interval(Action, ActionStartTime, ActionEndTime),
    equivalent_robot_task(Action, RobotTask).