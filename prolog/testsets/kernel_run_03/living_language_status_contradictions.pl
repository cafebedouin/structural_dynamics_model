% Axiom contradictions for kernel: living_language_status
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% liturgical_preservation_reading↔native_generation_reading: No single framework can hold both 'recitation of fixed texts = living language' and 'only native daily generation = living language' as simultaneously true. Accepting the native-generation axiom requires rejecting liturgical recitation as sufficient; accepting the liturgical axiom requires rejecting native transmission as necessary. The rabbinical excommunication of Ben-Yehuda demonstrates this mutual exclusivity — his project was framed as desecration precisely because it rejected liturgical sufficiency.

narrative_ontology:cs_axiom_contradiction(interpretive_authorization, native_speaker_daily_life_requirement).
narrative_ontology:cs_axiom_contradiction(native_speaker_daily_life_requirement, interpretive_authorization).
narrative_ontology:cs_story_uid(living_language_status_contradictions, '121a8887-081d-4cd3-b592-611698bca4ee').
narrative_ontology:cs_created_at('121a8887-081d-4cd3-b592-611698bca4ee', '').
