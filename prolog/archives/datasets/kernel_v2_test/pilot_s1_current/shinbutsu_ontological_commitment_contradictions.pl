% Axiom contradictions for kernel: shinbutsu_ontological_commitment
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% partition_reading↔incoherence_reading: Partition reading asserts a stable domain-separation framework; incoherence reading asserts no stable framework existed. Partition is a coherence claim; incoherence denies all coherence claims.

narrative_ontology:cs_axiom_contradiction(domain_separability, no_transcendent_unity).
narrative_ontology:cs_axiom_contradiction(no_transcendent_unity, domain_separability).
