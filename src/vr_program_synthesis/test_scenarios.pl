:- module(test_scenarios, [
    pick_place_simple/0
]).

:- use_module(utils).

set_poses([X,Y,Z], [QX, QY, QZ, QW], GripperOpening, StartTime, EndTime) :-
    time_scope(StartTime, EndTime, QS),
    tf_set_pose(hand, [world, [X,Y,Z], [QX, QY, QZ, QW]], QS),
    PosThumbX is X - GripperOpening / 2,
    PosIndexX is X + GripperOpening / 2,
    tf_set_pose(thumb, [world, [PosThumbX, Y, Z], [QX, QY, QZ, QW]], QS),
    tf_set_pose(indexFinger, [world, [PosIndexX, Y, Z], [QX, QY, QZ, QW]], QS).

pose_a([0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 1.0]).
pose_b([0.0, 0.0, 0.5], [0.0, 0.0, 0.0, 1.0]).
time_state_1(1.0, 2.0).
time_state_2(3.0, 4.0).
time_state_3(5.0, 6.0).

pick_place_simple :-
    tf_logger_enable,
    time_state_1(StartTime1, EndTime1),
    time_state_2(StartTime2, EndTime2),
    time_state_3(StartTime3, EndTime3),
    kb_project([
        % Agent
        is_individual(agent), instance_of(agent, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#PhysicalAgent'),
        has_end_link(agent, hand),

        % Hand & fingers
        is_individual(hand), instance_of(hand, 'http://www.ease-crc.org/ont/SOMA.owl#Hand'),
        is_individual(thumb), instance_of(thumb, 'http://www.ease-crc.org/ont/SOMA.owl#Thumb'), holds(hand, 'http://www.ease-crc.org/ont/SOMA.owl#hasFinger', thumb),
        is_individual(indexFinger), instance_of(indexFinger, 'http://www.ease-crc.org/ont/SOMA.owl#IndexFinger'), holds(hand, 'http://www.ease-crc.org/ont/SOMA.owl#hasFinger', indexFinger),

        % state1: Object not grasped, EE at position A
        is_state(state1), has_time_interval(state1, StartTime1, EndTime1),
        has_participant(state1, agent),
        holds(state1, 'http://www.ease-crc.org/ont/SOMA.owl#involvesEffector', hand),
        has_participant(state1, kio:'RAMStick_1'),

        is_individual(situation1), instance_of(situation1, 'http://www.ease-crc.org/ont/SOMA.owl#Scene'),
        holds(situation1, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesAgent', agent),
        holds(situation1, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesObject', kio:'RAMStick_1'),
        holds(situation1, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', state1),

        % action1: Grasp object, do not move EE
        is_action(action1), has_time_interval(action1, 2.0, 3.0),
        has_participant(action1, agent),
        holds(action1, 'http://www.ease-crc.org/ont/SOMA.owl#involvesEffector', hand),
        has_participant(action1, kio:'RAMStick_1'),

        is_individual(situation2), instance_of(situation2, 'http://www.ease-crc.org/ont/SOMA.owl#Scene'),
        holds(situation2, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesAgent', agent),
        holds(situation2, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesObject', kio:'RAMStick_1'),
        new_iri(SituationTransition1), is_individual(SituationTransition1), instance_of(SituationTransition1, 'http://www.ease-crc.org/ont/SOMA.owl#SituationTransition'),
        holds(SituationTransition1, 'http://www.ease-crc.org/ont/SOMA.owl#hasInitialSituation', situation1),
        holds(SituationTransition1, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalSituation', situation2),
        holds(SituationTransition1, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', action1),

        % state2: Object grasped, EE at position A
        is_state(state2), has_time_interval(state2, StartTime2, EndTime2),
        has_participant(state2, agent),
        holds(state2, 'http://www.ease-crc.org/ont/SOMA.owl#involvesEffector', hand),
        has_participant(state2, kio:'RAMStick_1'),

        object_grasped_in_situation(kio:'RAMStick_1', hand, situation2),
        holds(situation2, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', state2),

        % action2: Release object, move EE
        is_action(action2), has_time_interval(action2, 4.0, 5.0),
        has_participant(action2, agent),
        holds(action2, 'http://www.ease-crc.org/ont/SOMA.owl#involvesEffector', hand),
        has_participant(action2, kio:'RAMStick_1'),

        is_individual(situation3), instance_of(situation3, 'http://www.ease-crc.org/ont/SOMA.owl#Scene'),
        holds(situation3, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesAgent', agent),
        holds(situation3, 'http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#includesObject', kio:'RAMStick_1'),
        new_iri(SituationTransition2), is_individual(SituationTransition2), instance_of(SituationTransition2, 'http://www.ease-crc.org/ont/SOMA.owl#SituationTransition'),
        holds(SituationTransition2, 'http://www.ease-crc.org/ont/SOMA.owl#hasInitialSituation', situation2),
        holds(SituationTransition2, 'http://www.ease-crc.org/ont/SOMA.owl#hasTerminalSituation', situation3),
        holds(SituationTransition2, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', action2),

        % state3: Object not grasped, EE at position B
        is_state(state3), has_time_interval(state3, StartTime3, EndTime3),
        has_participant(state3, agent),
        holds(state3, 'http://www.ease-crc.org/ont/SOMA.owl#involvesEffector', hand),
        has_participant(state3, kio:'RAMStick_1'),
        holds(situation3, 'http://www.ease-crc.org/ont/SOMA.owl#manifestsIn', state3)
    ]),

    % state1: position A, gripper open
    pose_a(PosA, OriA), set_poses(PosA, OriA, 0.1, StartTime1, EndTime1),
    % state2: position A, gripper closed
    set_poses(PosA, OriA, 0.01, StartTime2, EndTime2),
    % state3: position B, gripper open
    pose_b(PosB, OriB), set_poses(PosB, OriB, 0.1, StartTime3, EndTime3),

    tf_logger_disable.