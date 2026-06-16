% Axiom contradictions for kernel: visual_evidentiary_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% indexical_realism↔epistemic_collapse: Indexical realism holds that the physics of light capture grounds recoverable truth; epistemic collapse holds that this grounding is irreversibly severed by generative AI. No framework can hold both that detection can restore verification capacity AND that the visual system is utterly useless.
% indexical_realism↔post_evidentiary: Indexical realism grounds authority in physical indexicality; post-evidentiary denies that indexicality ever grounded authority (social consensus did). No framework can hold both that media are indexical traces AND that indexicality was never the source of evidentiary power.
% epistemic_collapse↔distributed_verification: Epistemic collapse holds that no verification method can succeed; distributed verification holds that cryptographic/consensus methods can succeed. No framework can hold both that verification is impossible AND that distributed verification works.

narrative_ontology:cs_axiom_contradiction(indexical_traces_recoverable, verification_impossibility_at_scale).
narrative_ontology:cs_axiom_contradiction(verification_impossibility_at_scale, indexical_traces_recoverable).
narrative_ontology:cs_axiom_contradiction(indexical_traces_recoverable, consensus_primacy_over_indexicality).
narrative_ontology:cs_axiom_contradiction(consensus_primacy_over_indexicality, indexical_traces_recoverable).
narrative_ontology:cs_axiom_contradiction(verification_impossibility_at_scale, cryptographic_provenance_as_truth_warrant).
narrative_ontology:cs_axiom_contradiction(cryptographic_provenance_as_truth_warrant, verification_impossibility_at_scale).
