% Axiom contradictions for kernel: us_constitution_text
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% originalist_reading↔living_constitutionalist_reading: Originalism's axiom that meaning is fixed at ratification is mutually exclusive with living constitutionalism's axiom that meaning evolves with society. No single coherent framework can hold both 'the Constitution means what it meant in 1788' and 'the Constitution means what contemporary values require' as simultaneously true for the same provision.

narrative_ontology:cs_axiom_contradiction(meaning_fixed_at_ratification, constitutional_meaning_evolves_with_society).
narrative_ontology:cs_axiom_contradiction(constitutional_meaning_evolves_with_society, meaning_fixed_at_ratification).
narrative_ontology:cs_story_uid(us_constitution_text_contradictions, 'cca3f78e-eac6-47cb-ab9a-2457117787ee').
narrative_ontology:cs_created_at('cca3f78e-eac6-47cb-ab9a-2457117787ee', '').
