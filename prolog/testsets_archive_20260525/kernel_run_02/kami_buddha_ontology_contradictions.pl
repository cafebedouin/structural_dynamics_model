% Axiom contradictions for kernel: kami_buddha_ontology
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% honji_suijaku_monism↔incoherent_bundle: Monism requires a coherent unified ontology with systematic theoretical grounding; incoherent bundle denies the possibility of such coherence and attributes persistence to institutional practice rather than philosophical truth. One cannot simultaneously hold that honji suijaku provides coherent systematization AND that the system is fundamentally unsystematized.

narrative_ontology:cs_axiom_contradiction(single_ultimate_reality_principle, incoherence_permanent_feature).
narrative_ontology:cs_axiom_contradiction(incoherence_permanent_feature, single_ultimate_reality_principle).
