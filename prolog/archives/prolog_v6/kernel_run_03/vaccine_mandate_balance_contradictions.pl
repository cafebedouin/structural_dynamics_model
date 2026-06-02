% Axiom contradictions for kernel: vaccine_mandate_balance
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% public_health_primary↔bodily_autonomy_primary: Public-health-primary holds that consent can be legitimately overridden when collective harm exceeds individual autonomy cost. Bodily-autonomy-primary holds that consent is categorically inviolable regardless of collective harm. No single coherent framework can hold both: accepting consent-override-permissibility requires rejecting consent-inviolability, and vice versa.

narrative_ontology:cs_axiom_contradiction(public_health_necessity_supersedes_consent, bodily_autonomy_inviolable).
narrative_ontology:cs_axiom_contradiction(bodily_autonomy_inviolable, public_health_necessity_supersedes_consent).
narrative_ontology:cs_story_uid(vaccine_mandate_balance_contradictions, 'a5ebd0aa-e600-4d89-93e9-2b4377bd40df').
narrative_ontology:cs_created_at('a5ebd0aa-e600-4d89-93e9-2b4377bd40df', '').
