% Axiom contradictions for kernel: human_dignity_ai_governance
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% magisterial_integralist_reading↔secular_humanist_reading: The Magisterial reading holds that human dignity's ultimate ground is theological (imago Dei) and that the Church has unique interpretive authority over natural law. The secular humanist reading denies any theological foundation is necessary or authoritative for public reason. No single framework can simultaneously hold both that religious authority is epistemically privileged for governance and that democratic reason alone suffices.
% magisterial_integralist_reading↔techno_optimist_reading: The Magisterial reading treats human finitude and embodiment as essential to dignity (to be accepted, not transcended), and views technological enhancement as potentially dehumanizing. The techno-optimist reading treats biological limits as defects to be overcome and enhancement as dignity-increasing. No coherent framework can hold both that human limits are constitutive of dignity and that transcending them is the path to greater dignity.
% secular_humanist_reading↔techno_optimist_reading: The secular humanist reading grounds dignity in equal moral status and rights that constrain what can be done to persons (e.g., non-instrumentalization). The techno-optimist reading subordinates equal status to utilitarian outcomes and treats persons as optimization targets. While both reject theological grounding, they cannot coexist: one treats dignity as a constraint on optimization, the other treats optimization as the content of dignity.

narrative_ontology:cs_axiom_contradiction(imago_dei_ontological_dignity, dignity_as_rational_autonomy).
narrative_ontology:cs_axiom_contradiction(dignity_as_rational_autonomy, imago_dei_ontological_dignity).
narrative_ontology:cs_axiom_contradiction(imago_dei_ontological_dignity, capability_expansion_as_dignity).
narrative_ontology:cs_axiom_contradiction(capability_expansion_as_dignity, imago_dei_ontological_dignity).
narrative_ontology:cs_axiom_contradiction(dignity_as_rational_autonomy, capability_expansion_as_dignity).
narrative_ontology:cs_axiom_contradiction(capability_expansion_as_dignity, dignity_as_rational_autonomy).
