:- module(insert_hole_onto_peg, []).

:- multifile vrps:equivalent_robot_task/4.

object_supported_by_peg_in_situation(Object, Situation) :-
    % format('~w supported by Peg in Situation ~w?~n', [Object, Situation]),
    object_supported_in_situation(Object, Peg, Situation),
    instance_of(Peg, 'http://www.artiminds.com/kb/vr_program_synthesis.owl#Peg').

vrps:equivalent_robot_task(Action, 'http://www.artiminds.com/kb/artm.owl#InsertHoleOntoPeg', Robot, RobotTask) :-
    % Precondition 1: Some object grasped in some situation
    object_grasped_at_beginning_of_action(Action, Gripper, HoleObject),
    % Precondition 2: The grasped object is not touching anything other than the gripper
    forall(objects_touch_at_beginning_of_action(Action, HoleObject, OtherObject),
           OtherObject = Gripper),

    % Runtime condition 1: Object still grasped
    object_grasped_during_action(Action, Gripper, HoleObject),

    % postcondition 1: Grasped object in contact with a Peg (supported would be better, but the supportedBy annotation is too unreliable)
    objects_touch_at_end_of_action(Action, HoleObject, PegObject),
    kb_call(instance_of(PegObject, vrps:'Peg')),
    
    kb_project([
        new_iri(RobotTask, 'http://www.artiminds.com/kb/artm.owl#InsertHoleOntoPeg'),
        is_individual(RobotTask), instance_of(RobotTask, 'http://www.artiminds.com/kb/artm.owl#InsertHoleOntoPeg'),
        task_has_locatum(RobotTask, HoleObject),
        task_has_relatum(RobotTask, PegObject)
    ]), !.