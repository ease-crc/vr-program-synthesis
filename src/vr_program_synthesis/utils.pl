:- module(utils, [
    has_time_interval(r,t,t),
    end_effector_moved/2,
    end_effector_moved/3,
    end_effector_pose/3,
    end_effector_trajectory/4,
    euclidean_distance/3,
    has_gripper/2,
    object_grasped/2,
    has_feature/2,
    set_pose_parameter/3,
    get_pose_parameter/3,
    has_grasp_point/2,

    object_grasped_in_situation/3,
    object_grasped_at_beginning_of_action/3,
    object_grasped_during_action/3,
    object_grasped_at_end_of_action/3,

    objects_touch_in_situation/3,
    objects_touch_at_beginning_of_action/3,
    objects_touch_during_action/3,
    objects_touch_at_end_of_action/3,

    object_supported_in_situation/3,
    object_supported_at_beginning_of_action/3,

    arm_moved_during_action/1,
    gripper_moved_during_action/1,
    
    gripper_open/1,
    gripper_open_in_situation/1,
    gripper_closed_in_situation/1,
    gripper_opening_at_timestamp/3,
    gripper_open_at_timestamp/2,
    gripper_closed_at_timestamp/2,
    gripper_moved/2,
    has_initial_situation/2,
    has_terminal_situation/2,
    has_runtime_situation/2,
    co_occurring_situation/2,
    action_has_collision/1,
    action_is_collision_free/1,
    situation_is_collision_free/1,
    situations_are_collision_free/1,
    lists_equal_unordered/2,
    objects_touch_during_interval/4,
    task_has_role_with_filler/3,
    task_has_locatum/2,
    task_has_relatum/2
]).

has_time_interval(Event, StartTime, EndTime) +>
    new_iri(Interval, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#TimeInterval'),
    has_time_interval(Event, Interval),
    holds(Interval, 'http://www.ease-crc.org/ont/SOMA.owl#hasIntervalBegin', StartTime),
    holds(Interval, 'http://www.ease-crc.org/ont/SOMA.owl#hasIntervalEnd', EndTime).

has_time_interval(Event, StartTime, EndTime) ?>
    has_time_interval(Event, Interval),
    holds(Interval, 'http://www.ease-crc.org/ont/SOMA.owl#hasIntervalBegin', StartTime),
    holds(Interval, 'http://www.ease-crc.org/ont/SOMA.owl#hasIntervalEnd', EndTime).

% FIXME (UGLY HACK FOR FAKE NEEMS)
agent_has_end_link_name('http://knowrob.org/kb/UR5Robotiq.owl#UR5Robotiq_0', 'http://knowrob.org/kb/UR5Robotiq.owl#tool0').

% Whether the end effector of the robot has moved between two given States
end_effector_moved(StateBegin, StateEnd) :-
    % format('end_effector_moved? ~w ~w~n', [StateBegin, StateEnd]),
    kb_call([
        has_time_interval(StateBegin, InitialStartTime, _),
        has_time_interval(StateEnd, _, TerminalEndTime),
        has_participant(StateBegin, Robot)
    ]).
    % end_effector_moved(Robot, InitialStartTime, TerminalEndTime).

% Whether the end effector of the given Robot has moved more than 1 mm between two given Timestamps
end_effector_moved(Robot, TimestampBegin, TimestampEnd) :-
    % format('end_effector_moved? ~w ~w ~w~n', [Robot, TimestampBegin, TimestampEnd]),
    end_effector_pose(Robot, TimestampBegin, [_, PosInitial, _]),
    end_effector_pose(Robot, TimestampEnd, [_, PosTerminal, _]),
    euclidean_distance(PosInitial, PosTerminal, Dist),
    format('end_effector_moved: ~w~n', Dist),
    Dist > 0.001.
    % writeln('end_effector_moved!').

% Get the end effector pose for a given Robot at a given Timestamp
end_effector_pose(Robot, Timestamp, Pose) :-
    % format('end_effector_pose? ~w ~w~n', [Robot, Timestamp]),
    has_end_link(Robot, EE),
    time_scope(=<(Timestamp), >=(Timestamp), QScope),
    tf_get_pose(EE, Pose, QScope, _).
    % format('Pose: ~w~n', [Pose]).

% Get the end effector trajectory for a given Robot between two given Timestamps
end_effector_trajectory(Robot, TimeStampStart, TimeStampEnd, EETrajectory) :-
    has_end_link(Robot, EE),
    tf_mng_trajectory(EE, TimeStampStart, TimeStampEnd, EETrajectory).

has_gripper(Agent, Gripper) ?+>
    has_end_link(Agent, Gripper),
    instance_of(Gripper, 'http://www.ease-crc.org/ont/SOMA.owl#PrehensileEffector').


% Ask/tell whether an Object is part of an Agent's kinematic chain during a given Action
part_of_kinematic_chain_during_action(Agent, Object, Action) :-
    distinct(has_runtime_situation(Action, Situation)),
    part_of_kinematic_chain_in_situation(Agent, Object, Situation).

% Ask/tell whether an Object is part of an Agent's kinematic chain in a given Situation
part_of_kinematic_chain_in_situation(Agent, Object, Situation) :-
    (has_gripper(Agent, Object);    % The gripper is part of the kinematic chain
    (has_gripper(Agent, Gripper), object_grasped_in_situation(Object, Gripper, Situation))). % Any grasped object is part of the kinematic chain

% An Object is part of an Agent's kinematic chain in a collection of situations if it is part of its kinematic chain in at least one of the collection's members
part_of_kinematic_chain_in_situations(Agent, Object, Situations) :-
    member(S, Situations), part_of_kinematic_chain_in_situation(Agent, Object, S).

% Whether a given Object is grasped in a given State, or which Object is grasped in a given State
object_grasped(State, Object) :-
    holds(StateType, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#classifies', State),
    instance_of(StateType, 'http://www.ease-crc.org/ont/SOMA.owl#ContactState'),
    holds(R, 'http://www.ease-crc.org/ont/SOMA.owl#hasStateType', ContactState),
    has_role(Object, R),
    is_physical_object(Object),
    holds(R1, 'http://www.ease-crc.org/ont/SOMA.owl#hasStateType', ContactState),
    has_role(Finger1, R1),
    instance_of(Finger1, 'http://www.ease-crc.org/ont/SOMA.owl#Finger'),
    holds(R2, 'http://www.ease-crc.org/ont/SOMA.owl#hasStateType', ContactState),
    has_role(Finger2, R2),
    instance_of(Finger2, 'http://www.ease-crc.org/ont/SOMA.owl#Finger'),
    Finger1\=Finger2, Finger1\=Object, Finger2\=Object.

has_feature(Object, Feature) :-
    instance_of(Object, 'http://knowrob.org/kb/knowrob.owl#ShoppingBasket'),
    instance_of(Feature, 'http://www.artiminds.com/kb/vr_program_synthesis.owl#SurfaceFeature').

has_feature(Object, Feature) :-
    kb_call(holds(Object, soma:'hasFeature', Feature)).

set_pose_parameter(Task, [RefFrame, Pos, Ori], ParameterRoleType) :-
    term_string([RefFrame, Pos, Ori], PoseAsString),
    kb_project([
        new_iri(PoseParam, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#Parameter'), is_individual(PoseParam), instance_of(PoseParam, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#Parameter'),
        holds(Task, 'http://www.ease-crc.org/ont/SOMA.owl#hasInputParameter', PoseParam),
        new_iri(PoseRole, ParameterRoleType), is_individual(PoseRole), instance_of(PoseRole, ParameterRoleType),
        has_role(PoseParam, PoseRole),
        new_iri(PoseRegion, 'http://www.ease-crc.org/ont/SOMA.owl#DPose'), is_individual(PoseRegion), instance_of(PoseRegion, 'http://www.ease-crc.org/ont/SOMA.owl#DPose'),
        holds(PoseParam, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#classifies', PoseRegion),
        holds(PoseRegion, 'http://www.ease-crc.org/ont/SOMA.owl#hasReferenceFrame', RefFrame),
        holds(PoseRegion, 'http://www.ease-crc.org/ont/SOMA.owl#hasPositionData', PoseAsString)
    ]).

get_pose_parameter(Task, [RefFrame, Pos, Ori], ParameterRoleType) :-
    holds(Task, 'http://www.ease-crc.org/ont/SOMA.owl#hasInputParameter', PoseParam),
    has_role(PoseParam, PoseRole), instance_of(PoseRole, ParameterRoleType),
    holds(PoseParam, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#classifies', PoseRegion), instance_of(PoseRegion, 'http://www.ease-crc.org/ont/SOMA.owl#DPose'),
    holds(PoseRegion, 'http://www.ease-crc.org/ont/SOMA.owl#hasReferenceFrame', RefFrame),
    holds(PoseRegion, 'http://www.ease-crc.org/ont/SOMA.owl#hasPositionData', PoseAsString),
    term_string([RefFrame, Pos, Ori], PoseAsString).

new_robot_action_for_task(ActionType, TaskType, Robot, Action, Task) :-
    kb_project([
        new_iri(Action, ActionType), is_individual(Action), instance_of(Action,ActionType),
        new_iri(Task, TaskType), is_individual(Task), instance_of(Task, TaskType),
        holds(Task, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#classifies', RobotAction),
        is_performed_by(Action, Robot)
    ]).

% Get the name of the grasp point for a given object
has_grasp_point(Object, GraspPointName) ?>
    holds(Object, 'http://www.artiminds.com/kb/artm.owl#hasGraspPoint', GraspPointName).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SITUATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

binary_related_in_situation(Thing1, Thing2, Situation, DescriptionType, RoleType1, RoleType2) +>
    new_iri(Description), is_individual(Description), instance_of(Description, DescriptionType),

    % Binding Thing1 to Role1
    new_iri(Binding1), is_individual(Binding1), instance_of(Binding1, 'http://www.ease-crc.org/ont/SOMA.owl#RoleFillerBinding'),
    holds(Binding1, 'http://www.ease-crc.org/ont/SOMA.owl#hasBindingFiller', Thing1),
    new_iri(Role1), is_individual(Role1), instance_of(Role1, RoleType1),
    holds(Binding1, 'http://www.ease-crc.org/ont/SOMA.owl#hasBindingRole', Role1),

    % Binding Thing2 to Role2
    new_iri(Binding2), is_individual(Binding2), instance_of(Binding2, 'http://www.ease-crc.org/ont/SOMA.owl#RoleFillerBinding'),
    holds(Binding2, 'http://www.ease-crc.org/ont/SOMA.owl#hasBindingFiller', Thing2),
    new_iri(Role2), is_individual(Role2), instance_of(Role2, RoleType2),
    holds(Binding2, 'http://www.ease-crc.org/ont/SOMA.owl#hasBindingRole', Role2),

    holds(Description, 'http://www.ease-crc.org/ont/SOMA.owl#hasBinding', Binding1),
    holds(Description, 'http://www.ease-crc.org/ont/SOMA.owl#hasBinding', Binding2),
    satisfies(Situation, Description).

binary_related_in_situation(Thing1, Thing2, Situation, DescriptionType, RoleType1, RoleType2) ?>
    is_individual(Description), instance_of(Description, DescriptionType),
    satisfies(Situation, Description),
    holds(Description, 'http://www.ease-crc.org/ont/SOMA.owl#hasBinding', Binding1),
    holds(Description, 'http://www.ease-crc.org/ont/SOMA.owl#hasBinding', Binding2),
    Binding1 \= Binding2,
    holds(Binding1, 'http://www.ease-crc.org/ont/SOMA.owl#hasBindingFiller', Thing1),
    holds(Binding1, 'http://www.ease-crc.org/ont/SOMA.owl#hasBindingRole', Role1), instance_of(Role1, RoleType1),
    holds(Binding2, 'http://www.ease-crc.org/ont/SOMA.owl#hasBindingFiller', Thing2),
    holds(Binding2, 'http://www.ease-crc.org/ont/SOMA.owl#hasBindingRole', Role2), instance_of(Role2, RoleType2).

object_grasped_in_situation(Object, Gripper, Situation) ?+>
    binary_related_in_situation(Object, Gripper, Situation, vrps:'GraspRelation', vrps:'GraspedObject', vrps:'GripperRole').

object_grasped_at_beginning_of_action(Action, Gripper, Object) :-
    distinct(has_initial_situation(Action, Situation)),
    kb_call(object_grasped_in_situation(Object, Gripper, Situation)).

object_grasped_during_action(Action, Gripper, Object) :-
    distinct(has_runtime_situation(Action, Situation)),
    kb_call(object_grasped_in_situation(Object, Gripper, Situation)).

object_grasped_at_end_of_action(Action, Gripper, Object) :-
    distinct(has_terminal_situation(Action, Situation)), 
    object_grasped_in_situation(Object, Gripper, Situation).

objects_touch_in_situation(Object1, Object2, Situation) ?+>
    binary_related_in_situation(Object1, Object2, Situation, vrps:'ContactRelation', soma:'ConnectedObject', soma:'ConnectedObject'),
    Object1 \== Object2.

objects_touch_at_beginning_of_action(Action, ObjectA, ObjectB) :-
    distinct(has_initial_situation(Action, Situation)),
    objects_touch_in_situation(ObjectA, ObjectB, Situation).

objects_touch_during_action(Action, ObjectA, ObjectB) :-
    distinct(has_runtime_situation(Action, Situation)),
    objects_touch_in_situation(ObjectA, ObjectB, Situation).

objects_touch_at_end_of_action(Action, ObjectA, ObjectB) :-
    distinct(has_terminal_situation(Action, Situation)),
    objects_touch_in_situation(ObjectA, ObjectB, Situation).

object_supported_in_situation(Supportee, Supporter, Situation) ?+>
    binary_related_in_situation(Supportee, Supporter, Situation, vrps:'SupportRelation', soma:'SupportedObject', soma:'Supporter'),
    Supportee \== Supporter.

object_supported_at_beginning_of_action(Action, Object, Supporter) :-
    distinct(has_initial_situation(Action, Situation)),
    kb_call(object_supported_in_situation(Object, Supporter, Situation)).

arm_moved_during_action(Action) :-
    is_action(Action),
    is_performed_by(Action, Agent),
    has_time_interval(Action, StartTime, EndTime),
    end_effector_moved(Agent, StartTime, EndTime), !.

gripper_moved_during_action(Action) :-
    is_action(Action),
    is_performed_by(Action, Agent),
    has_gripper(Agent, Gripper),
    has_time_interval(Action, StartTime, EndTime),
    gripper_opening_at_timestamp(Gripper, StartTime, InitialOpening),
    gripper_opening_at_timestamp(Gripper, EndTime, FinalOpening),
    gripper_moved(InitialOpening, FinalOpening), 
    GripperDistance is (FinalOpening - InitialOpening), !.

support_constrains_in_2dof(Supporter) ?>
    instance_of(Supporter, vrps:'Peg').

euclidean_distance([X1,X2,X3],[Y1,Y2,Y3],D) :-
    D is sqrt((Y1-X1)*(Y1-X1)+(Y2-X2)*(Y2-X2)+(Y3-X3)*(Y3-X3)).

gripper_open(GripperOpening) :-
    GripperOpening > 0.01.

gripper_moved(GripperOpening1, GripperOpening2) :-
    % writeln('gripper_moved?'),
    GripperDistance is abs(GripperOpening1 - GripperOpening2),
    % format('GripperDistance: ~w~n', GripperDistance),
    GripperDistance > 0.02.

gripper_opening_in_situation(Situation, GripperOpening) :-
    % Dangerous: Here, it yields the gripper openings at arbitrary times during the events in which
    % the situation manifests
    kb_call([
        holds(Situation, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', Event),
        holds(Situation, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesObject', Gripper), instance_of(Gripper, 'http://www.ease-crc.org/ont/SOMA.owl#PrehensileEffector'),
        holds(Gripper, 'http://www.ease-crc.org/ont/SOMA.owl#hasFinger', Thumb), instance_of(Thumb, 'http://www.ease-crc.org/ont/SOMA.owl#Thumb'),
        holds(Gripper, 'http://www.ease-crc.org/ont/SOMA.owl#hasFinger', IndexFinger), instance_of(IndexFinger, 'http://www.ease-crc.org/ont/SOMA.owl#IndexFinger'),
        has_time_interval(Event, StartTime, EndTime)
    ]),
    time_scope(=<(StartTime), >=(EndTime), QScope),
    tf_get_pose(Thumb, [_, PosThumb, _], QScope, _),
    tf_get_pose(IndexFinger, [_, PosIndex, _], QScope, _),
    euclidean_distance(PosThumb, PosIndex, GripperOpening).

gripper_open_in_situation(Situation) :-
    % format('gripper_open_in_situation? ~w~n', Situation),
    gripper_opening_in_situation(Situation, GripperOpening),
    %format('gripper_opening: ~w', GripperOpening),
    gripper_open(GripperOpening).
    % writeln('gripper_open_in_situation!').

gripper_closed_in_situation(Situation) :-
    % format('gripper_closed_in_situation? ~w~n', Situation),
    gripper_opening_in_situation(Situation, GripperOpening),
    %format('gripper_opening: ~w', GripperOpening),
    not(gripper_open(GripperOpening)),
    writeln('gripper_closed_in_situation!').

gripper_opening_at_timestamp(Gripper, Timestamp, GripperOpening) :-
    % format('gripper_opening_at_timestamp? ~w ~w~n', [Gripper, Timestamp]),
    kb_call([
        holds(Gripper, 'http://www.ease-crc.org/ont/SOMA.owl#hasFinger', Thumb), instance_of(Thumb, 'http://www.ease-crc.org/ont/SOMA.owl#Thumb'),
        holds(Gripper, 'http://www.ease-crc.org/ont/SOMA.owl#hasFinger', IndexFinger), instance_of(IndexFinger, 'http://www.ease-crc.org/ont/SOMA.owl#IndexFinger')
    ]),
    time_scope(=<(Timestamp), >=(Timestamp), QScope),
    tf_get_pose(Thumb, [_, PosThumb, _], QScope, _),
    tf_get_pose(IndexFinger, [_, PosIndex, _], QScope, _),
    euclidean_distance(PosThumb, PosIndex, GripperOpening).
    % format('gripper_opening_at_timestamp? ~w ~w ~w~n', [Gripper, Timestamp, GripperOpening]).

gripper_open_at_timestamp(Gripper, Timestamp) :-
    % format('gripper_open_at_timestamp?  ~w~n', Timestamp),
    gripper_opening_at_timestamp(Gripper, Timestamp, GripperOpening),
    gripper_open(GripperOpening).
    % format('gripper_open_at_timestamp!  ~w~n', Timestamp).

gripper_closed_at_timestamp(Gripper, Timestamp) :-
    % format('gripper_closed_at_timestamp?  ~w~n', Timestamp),
    gripper_opening_at_timestamp(Gripper, Timestamp, GripperOpening),
    not(gripper_open(GripperOpening)).
    % format('gripper_closed_at_timestamp!  ~w~n', Timestamp).

co_occurring_situation(Situation, OtherSituation) ?+>
    holds(Situation, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', State),
    is_state(State),
    holds(OtherSituation, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', State).

% A collision is allowed during an action if eiher both objects are part of the kinematic chain,
% or neither object is part of the kinematic chain
collision_allowed_during_action(Action, Object1, Object2) :-
    is_performed_by(Action, Agent),
    (
        part_of_kinematic_chain_during_action(Agent, Object1, Action),
        part_of_kinematic_chain_during_action(Agent, Object2, Action)
    );
    (
        \+part_of_kinematic_chain_during_action(Agent, Object1, Action),
        \+part_of_kinematic_chain_during_action(Agent, Object2, Action)
    );
    (
        format('Collision not allowed during ~w: Offending objects: ~w, ~w~n', [Action, Object1, Object2]),
        fail
    ).

% A collision is allowed between two objects in a situation if eiher both objects are part of the kinematic chain,
% or neither object is part of the kinematic chain
collision_allowed_in_situation(Agent, Situation, Object1, Object2) :-
    % writeln('collision_allowed_in_situation?'),
    (
        part_of_kinematic_chain_in_situation(Agent, Object1, Situation),
        part_of_kinematic_chain_in_situation(Agent, Object2, Situation)
        % writeln('Both are part of kinematic chain')
    );
    (
        \+part_of_kinematic_chain_in_situation(Agent, Object1, Situation),
        \+part_of_kinematic_chain_in_situation(Agent, Object2, Situation)
        % writeln('Neither is part of kinematic chain')
    );
    (
        % format('Offending objects: ~w, ~w, Situation: ~w~n', [Object1, Object2, Situation]),
        fail
    ).
    % writeln('collision_allowed_in_situation!').

% A collision is allowed between two objects in a situation if either both objects are part of the kinematic chain,
% or neither object is part of the kinematic chain
collision_allowed_in_situations(Agent, Situations, Object1, Object2) :-
    % writeln('collision_allowed_in_situations?'),
    (
        part_of_kinematic_chain_in_situations(Agent, Object1, Situations),
        part_of_kinematic_chain_in_situations(Agent, Object2, Situations)
        % writeln('Both are part of kinematic chain')
    );
    (
        \+part_of_kinematic_chain_in_situations(Agent, Object1, Situations),
        \+part_of_kinematic_chain_in_situations(Agent, Object2, Situations)
        % writeln('Neither is part of kinematic chain')
    );
    (
        % format('Offending objects: ~w, ~w, Situations: ~w~n', [Object1, Object2, Situations]),
        fail
    ).
    % writeln('collision_allowed_in_situations!').

action_has_collision(Action) :-
    distinct(has_runtime_situation(Action, RS1)),
    objects_touch_in_situation(Object1, Object2, RS1),
    \+collision_allowed_during_action(Action, Object1, Object2).

action_is_collision_free(Action) :-
    % \+action_has_collision(Action).
    forall(distinct(has_runtime_situation(Action, RS1)),
                      situation_is_collision_free(RS1)).

situation_is_collision_free(Situation) :-
    % format('situation_is_collision_free? ~w~n', Situation),
    holds(Situation, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesAgent', Agent),
    forall(objects_touch_in_situation(Object1, Object2, Situation), collision_allowed_in_situation(Agent, Situation, Object1, Object2)).
    % format('situation_is_collision_free! ~w~n', Situation).

situations_are_collision_free(Situations) :-
    % format('situations_are_collision_free? ~w~n'),
    forall(member(S, Situations),
                      forall(objects_touch_in_situation(Object1, Object2, S),
                             (
                                holds(S, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesAgent', Agent),
                                collision_allowed_in_situations(Agent, Situations, Object1, Object2)
                             )
                            )
          ).
    % format('situations_are_collision_free! ~w~n').

has_initial_situation(Action, Situation) :-
    holds(SituationTransition, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', Action),
    instance_of(SituationTransition, 'http://www.ease-crc.org/ont/SOMA.owl#SituationTransition'),
    holds(SituationTransition, 'http://www.ease-crc.org/ont/SOMA.owl#hasInitialSituation', Situation).

has_terminal_situation(Action, Situation) :-
    holds(SituationTransition, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', Action),
    instance_of(SituationTransition, 'http://www.ease-crc.org/ont/SOMA.owl#SituationTransition'),
    holds(SituationTransition, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalSituation', Situation).

has_runtime_situation(Action, Situation) :-
    holds(Situation, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', Action),
    instance_of(Situation, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#Situation'),
    \+instance_of(Situation, 'http://www.ease-crc.org/ont/SOMA.owl#SituationTransition').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MISC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lists_equal_unordered(List1, List2) :-
    % format('lists_equal_unordered? ~w ~w~n', [List1, List2]),
    list_to_set(List1, Set1),
    list_to_set(List2, Set2),
    intersection(Set1, Set2, IS),
    IS=Set1, IS=Set2.

objects_touch_during_interval(Object1, Object2, StartTime, EndTime) :-
    % Holds if there is a ContactState spanning at least the interval, which relates Object1 and Object2
    kb_call([
        is_state(State), holds(StateType, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#classifies', State),
        instance_of(StateType, 'http://www.ease-crc.org/ont/SOMA.owl#ContactState'),
        has_time_interval(State, StateStartTime, StateEndTime)
    ]),
    \+(StateEndTime < StartTime),
    \+(StateStartTime > EndTime),
    kb_call([
        holds(Situation, soma:'manifestsIn', State),
        objects_touch_in_situation(Object1, Object2, Situation)
    ]).
    % format('objects_touch_during_interval: ~w ~w ~w ~w~n', [Object1, Object2, StartTime, EndTime]).

objects_touch_in_action(Object1, Object2, Action) :-
    distinct(has_runtime_situation(Action, Situation)),
    objects_touch_in_situation(Object1, Object2, Situation).

task_has_role_with_filler(Task, Role, Filler) ?+>
    has_task_role(Task, Role), has_role(Filler, Role).

task_has_relatum(Task, Relatum) +>
    new_iri(RelatumRole, dul:'Role'), is_individual(RelatumRole), instance_of(RelatumRole, soma:'RelatumRole'),
    task_has_role_with_filler(Task, RelatumRole, Relatum).

task_has_relatum(Task, Relatum) ?>
    instance_of(RelatumRole, soma:'RelatumRole'), task_has_role_with_filler(Task, RelatumRole, Relatum).

task_has_locatum(Task, Locatum) +>
    new_iri(LocatumRole, dul:'Role'), is_individual(LocatumRole), instance_of(LocatumRole, soma:'LocatumRole'),
    task_has_role_with_filler(Task, LocatumRole, Locatum).

task_has_locatum(Task, Locatum) ?>
    instance_of(LocatumRole, soma:'LocatumRole'), task_has_role_with_filler(Task, LocatumRole, Locatum).
