% Axiom contradictions for kernel: second_amendment_text
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% collective_militia_reading↔individual_right_reading: Collective reading holds that constitutional protection runs to the state as institutional entity (state's right to maintain armed militia); individual reading holds protection runs to persons as rights-bearers. No single framework can simultaneously treat the Amendment as protecting state institutional capacity AND individual personal liberty—these are categorically different constitutional subjects.

narrative_ontology:cs_axiom_contradiction(prefatory_clause_binding, operative_clause_independent_scope).
narrative_ontology:cs_axiom_contradiction(operative_clause_independent_scope, prefatory_clause_binding).
