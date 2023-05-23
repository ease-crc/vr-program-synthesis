:- module(extract_hole_from_peg, []).

:- multifile vrps:equivalent_robot_task/4.

object_supported_by_peg_in_situation(Object, Situation) :-
    % format('~w supported by Peg in Situation ~w?~n', [Object, Situation]),
    object_supported_in_situation(Object, Peg, Situation),
    instance_of(Peg, 'http://www.artiminds.com/kb/vr_program_synthesis.owl#Peg').

vrps:equivalent_robot_task(Action, 'http://www.artiminds.com/kb/artm.owl#ExtractHoleFromPeg', Robot, RobotTask) :-
    % Precondition 1: Object grasped in some situation
    object_grasped_at_beginning_of_action(Action, _, HoleObject),
    % Precondition 2: The grasped object is supportedBy some other object of type Peg in some situation
    objects_touch_at_beginning_of_action(Action, HoleObject, PegObject), 
    instance_of(PegObject, 'http://www.artiminds.com/kb/vr_program_synthesis.owl#Peg'),
    
    % Runtime condition 1: Object still grasped
    object_grasped_during_action(Action, _, HoleObject),

    % Rostcondition 1: Object still grasped
    object_grasped_at_end_of_action(Action, _, HoleObject),

    % Postcondition 2: Grasped object not supportedBy a Peg anymore
    forall(distinct(has_terminal_situation(Action, Situation)),
           \+object_supported_by_peg_in_situation(HoleObject, Situation)),

    kb_project([
        new_iri(RobotTask, 'http://www.artiminds.com/kb/artm.owl#ExtractHoleFromPeg'),
        is_individual(RobotTask), instance_of(RobotTask, 'http://www.artiminds.com/kb/artm.owl#ExtractHoleFromPeg'),
        task_has_locatum(RobotTask, HoleObject),
        task_has_relatum(RobotTask, PegObject)
    ]), 
    
    instance_of(HoleObject, HoleObjectClass), kb_call(subclass_of(HoleObjectClass, dul:'PhysicalObject')),

    instance_of(PegObject, PegObjectClass), kb_call(subclass_of(PegObjectClass, dul:'PhysicalObject')),

    format('Hole object (locatum): ~w of class ~w~n', [HoleObject, HoleObjectClass]),
    format('Peg object (relatum): ~w of class ~w~n', [PegObject, PegObjectClass]), !.