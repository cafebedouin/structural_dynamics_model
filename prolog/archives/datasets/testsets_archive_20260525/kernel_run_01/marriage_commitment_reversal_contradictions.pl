% Axiom contradictions for kernel: marriage_commitment_reversal
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% exogenous_override_reading↔endogenous_reinterpretation_reading: Exogenous reading holds that external coercion was the primary causal mechanism (practice would not have reversed absent federal threat); endogenous reading holds that internal divine revelation was the primary causal mechanism (practice reversed because God's will changed). No single coherent framework can hold both: either the Manifesto was capitulation to force (exogenous) or obedience to revelation (endogenous). Joseph F. Smith's 1891 denial that Manifesto was revelation supports exogenous reading; Manifesto's canonization as divinely inspired supports endogenous reading. The axioms are mutually exclusive regarding causal primacy.

narrative_ontology:cs_axiom_contradiction(state_decree_legitimizes_standardization, revelation_permits_reinterpretation).
narrative_ontology:cs_axiom_contradiction(revelation_permits_reinterpretation, state_decree_legitimizes_standardization).
