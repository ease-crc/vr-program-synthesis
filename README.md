# vr-program-synthesis

Companion repository for the paper "Knowledge-Driven Robot Program Synthesis from Human VR Demonstrations", presented at the 20th International Conference on Principles of Knowledge Representation and Reasoning (KR2023) ([arXiv](TODO)).

The repository contains the source code for our reasoner and our task representation.

## High-level overview

### Representation of task knowledge

Taks knowledge is represented as using a hybrid representation: Taxonomic knowledge (which tasks exists, and how they hierarchically relate to each other) is represented in an ontology, while semantic knowledge (what e.g. `Grasping` is in terms of pre-, runtime- and postconditions, what parameters it has and how it relates to `Action`s) is represented in a Prolog rulebase.

The task ontology is *owl/artm.owl* and uses [SOMA](https://ease-crc.github.io/soma/) as a foundational ontology. Tasks are subclasses of `http://www.ease-crc.org/ont/SOMA.owl#PhysicalTask`.

The semantic task definitions can be found under *src/vr_program_synthesis/tasks*. Each task is defined via the Prolog predicate `equivalent_robot_task(Action, TaskType, Robot, RobotTask)`, which relates a given `Action` to a task individual (`RobotTask`) of a given `TaskType` (subclass of `PhysicalTask`) if the action meets the pre-, runtime- and postconditions of the `TaskType`.

### Reasoning about tasks and events

This repo implements the interpretation of action sequences demonstrated in VR (Section 4 of the paper). At this point, VR demonstrations are represented as narrative-enabled episodic memories (NEEMs), ontology individuals of class `http://www.ease-crc.org/ont/SOMA.owl#PhysicalAction` with associated timestamped hand trajectories and additional semantic annotations parsed from VR force-dynamic event data (see [vr-neem-converter](https://github.com/ease-crc/vr-neem-converter)).

Our reasoner is implemented in *src/vr_program_synthesis/vr_program_synthesis.pl* as a set of prolog redicates which relate a NEEM to the corresponding set of candidate task sequences. The implementations of Algorithms 2 and 3 in the paper can be found there.

Our reasoner is deployed as a plugin for the [KnowRob](https://www.knowrob.org/) KR&R engine. For a tutorial on how to run it with KnowRob, see [the official KnowRob documentation](https://www.knowrob.org/doc/create_your_own_knowrob_package).
