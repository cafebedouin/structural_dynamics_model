% Axiom contradictions for kernel: equal_protection_clause
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% diversity_reading↔colorblind_reading: Diversity reading permits race as one factor in holistic admissions to achieve educational benefits; colorblind reading categorically prohibits any racial classification in state action. No framework can hold both: accepting diversity rationale requires accepting race-conscious decision-making, which colorblindness categorically forbids.

narrative_ontology:cs_axiom_contradiction(race_consciousness_narrowly_permissible, formal_equality_requires_race_neutrality).
narrative_ontology:cs_axiom_contradiction(formal_equality_requires_race_neutrality, race_consciousness_narrowly_permissible).
narrative_ontology:cs_story_uid(equal_protection_clause_contradictions, '0ed9cafe-b350-4433-b255-adc5213e951f').
narrative_ontology:cs_created_at('0ed9cafe-b350-4433-b255-adc5213e951f', '').
