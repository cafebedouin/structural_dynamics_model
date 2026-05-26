% Axiom contradictions for kernel: sovereign_legitimacy
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% monarchical_reading↔republican_reading: Monarchical axiom (authority inheres in bloodline, non-revocable by subjects) and republican axiom (authority delegated by consent, revocable by governed) cannot coexist in a single coherent framework. Accepting republican axiom as true requires rejecting the monarchical claim that inherited status grounds legitimate rule independent of popular will.

narrative_ontology:cs_axiom_contradiction(hereditary_succession_legitimacy, popular_consent_as_legitimacy_source).
narrative_ontology:cs_axiom_contradiction(popular_consent_as_legitimacy_source, hereditary_succession_legitimacy).
