% Axiom contradictions for kernel: income_support_commitment
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% freedom_floor_reading↔dependency_trap_reading: Freedom-floor axiom: exit capacity from labor market is autonomy-enabling and dignifying. Dependency-trap axiom: exit capacity from labor market is skill-atrophying and parasitic. No single framework can hold both — one must accept that labor market exit is either emancipatory or degrading, not both simultaneously.
% freedom_floor_reading↔targeting_efficiency_reading: Freedom-floor axiom: universality eliminates stigma and bureaucratic gatekeeping, making unconditional support superior. Targeting axiom: concentration on need is fiscally and morally superior to universal distribution. No single framework can hold both — one must accept either that universality's non-discrimination justifies its cost, or that targeting's efficiency justifies its means-testing.
% dependency_trap_reading↔targeting_efficiency_reading: These readings coexist — both accept work-incentive preservation as legitimate concern. Dependency-trap opposes UBI because it removes work requirements; targeting opposes UBI because it dilutes concentration on need. A single conservative framework can hold both objections as complementary.

narrative_ontology:cs_axiom_contradiction(income_support_enables_autonomy, labor_participation_economically_necessary).
narrative_ontology:cs_axiom_contradiction(labor_participation_economically_necessary, income_support_enables_autonomy).
narrative_ontology:cs_axiom_contradiction(income_support_enables_autonomy, demonstrated_need_justifies_differential_access).
narrative_ontology:cs_axiom_contradiction(demonstrated_need_justifies_differential_access, income_support_enables_autonomy).
narrative_ontology:cs_axiom_contradiction(labor_participation_economically_necessary, demonstrated_need_justifies_differential_access).
narrative_ontology:cs_axiom_contradiction(demonstrated_need_justifies_differential_access, labor_participation_economically_necessary).
narrative_ontology:cs_story_uid(income_support_commitment_contradictions, '188b9356-41ca-4b6e-a3ee-d22635afe60f').
narrative_ontology:cs_created_at('188b9356-41ca-4b6e-a3ee-d22635afe60f', '').
