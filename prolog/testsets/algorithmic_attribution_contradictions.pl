% Axiom contradictions for kernel: algorithmic_attribution
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% products_liability_reading↔expressive_attribution_reading: One holds that firm-attribution of an algorithmic loop grounds liability for its effects; the other holds that the identical attribution grounds a constitutional right to broadcast that loop's output free of content-based restriction. A single framework cannot treat the same attributed act as both a loss-generating product defect subject to redesign mandates and as protected expression immune from content-based mandate — the essay's own 'sword/shield, same finding' observation is precisely this incompatibility, not a mere disagreement about desirability.
% expressive_attribution_reading↔technician_intent_reading: The expressive reading locates the firm's authorship in the act of curation itself, independent of any mental state about mechanism (Anderson/Moody do not require the firm to have understood *why* the algorithm worked). The technician reading makes a documented theory of mechanism the anchor of responsibility. If curation is protected expression regardless of whether anyone understood the mechanism, then the absence of a memo cannot be doing the exculpatory work the technician reading assigns it — the two cannot both be the operative test for whether the firm answers for the outcome.

narrative_ontology:cs_axiom_contradiction(control_of_process_grounds_liability_regardless_of_fault, algorithmic_curation_is_firm_expression).
narrative_ontology:cs_axiom_contradiction(algorithmic_curation_is_firm_expression, control_of_process_grounds_liability_regardless_of_fault).
narrative_ontology:cs_axiom_contradiction(algorithmic_curation_is_firm_expression, attribution_requires_documented_mental_state).
narrative_ontology:cs_axiom_contradiction(attribution_requires_documented_mental_state, algorithmic_curation_is_firm_expression).
