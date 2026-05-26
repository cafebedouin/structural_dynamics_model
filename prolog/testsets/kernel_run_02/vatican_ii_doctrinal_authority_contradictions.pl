% Axiom contradictions for kernel: vatican_ii_doctrinal_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% rupture_progressive_reading↔rupture_traditionalist_reading: Progressive rupture reading holds that the break with tradition is legitimate and Spirit-guided; traditionalist rupture reading holds that the break with tradition is illegitimate and heterodox. Both agree on rupture but assign opposite normative valence. A single framework cannot hold both 'this rupture is divinely authorized' and 'this rupture is heterodox' as simultaneously true.

narrative_ontology:cs_axiom_contradiction(vatican_ii_as_necessary_rupture, vatican_ii_contains_substantive_rupture).
narrative_ontology:cs_axiom_contradiction(vatican_ii_contains_substantive_rupture, vatican_ii_as_necessary_rupture).
