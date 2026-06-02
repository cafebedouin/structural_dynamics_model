% Axiom contradictions for kernel: plural_marriage_mandate
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.
:- multifile narrative_ontology:cs_contradiction_of/2.

% exogenous_override_reading↔endogenous_reinterpretation_reading: No single framework can hold both 'federal coercion invalidates prophetic authority' (exogenous) and 'prophetic authority legitimately responds to circumstances via new revelation' (endogenous). The first denies the second's core claim that Woodruff's revelation was genuine rather than coerced.
% institutional_pragmatism_reading↔endogenous_reinterpretation_reading: No single framework can hold both 'revelation narrative is strategic legitimation of survival calculus' (pragmatism) and 'revelation represents genuine divine communication' (endogenous). The first treats the second's foundational claim as instrumental fiction rather than theological reality.

narrative_ontology:cs_axiom_contradiction(decree_sufficiency_without_internalization, prophetic_succession_authority_remains_valid).
narrative_ontology:cs_axiom_contradiction(prophetic_succession_authority_remains_valid, decree_sufficiency_without_internalization).
narrative_ontology:cs_axiom_contradiction(institutional_survival_as_primary_motive, prophetic_succession_authority_remains_valid).
narrative_ontology:cs_axiom_contradiction(prophetic_succession_authority_remains_valid, institutional_survival_as_primary_motive).
narrative_ontology:cs_story_uid(plural_marriage_mandate_contradictions, '558c4303-c67c-430c-bb7b-07ef0d19310c').
narrative_ontology:cs_contradiction_of(plural_marriage_mandate_contradictions, plural_marriage_mandate).
narrative_ontology:cs_created_at('558c4303-c67c-430c-bb7b-07ef0d19310c', '').
