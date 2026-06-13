% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__integrated_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_alignment_commitment__integrated_reading
 *   human_readable: AI Alignment Integrated (Control + Justice) Commitment
 *   domain: technology/governance/ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the INTEGRATED READING of the
 *   ai_alignment_commitment kernel: alignment requires simultaneous attention
 *   to control problems and justice problems as non-exclusive,
 *   co-constitutive dimensions. The reading rejects a false dichotomy
 *   (control OR justice) and asserts that siloed approaches fragment a
 *   unified problem. The constraint is claimed as tangled_rope because it
 *   coordinates a multi-stakeholder research and governance agenda
 *   (coordination function: unified control + justice focus) while extracting
 *   costs from those who must pay for integration (interdisciplinary
 *   overhead, slower publication cycles, reduced institutional prestige in
 *   siloed disciplines). Suppression is substantial because the constraint is
 *   actively maintained against control-technical researchers' preference for
 *   autonomy and against capability labs' preference for speed—alternatives
 *   (pure control focus, delegated fairness mitigation) are technically
 *   available but are actively discouraged by governance and funding
 *   structures. The reading is one of three coherent instantiations of the
 *   same contested kernel; the sibling readings (safety_control_reading,
 *   ethics_justice_reading) define themselves by rejecting the integrated
 *   premise.
 *
 * KEY AGENTS:
 *   - ai_safety_technical_researchers: Institutional agenda-setter; shapes control-focused research priorities; moderate power (concentrated resources) but mobile exit (can shift focus)
 *   - marginalized_communities_bearing_present_harms: Powerless, trapped payers; bear costs of fragmentation; excluded from alignment definition-setting
 *   - ai_ethics_and_justice_researchers: Moderate power; benefit from integration but pay subordination costs; constrained exit (institutional structure preserves siloes)
 *   - future_generations_and_humanity: Civilizational-horizon beneficiaries; powerless, trapped; potential victims if integrated approaches fail
 *   - funding_bodies_and_philanthropies: Institutional agenda-setters; control capital allocation; can mandate integration but face resistance from control-technical communities
 *   - policy_and_fairness_researchers: Moderate power; currently excluded; would benefit from integration but face resource and prestige barriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.62).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "AI Alignment Integrated (Control + Justice) Commitment").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "technology/governance/ethics").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, 'dbd44839-bf45-4d19-8ec0-cf2b47bb0285').
narrative_ontology:cs_kernel_codification('dbd44839-bf45-4d19-8ec0-cf2b47bb0285', distributed).
narrative_ontology:cs_authority_grounding('dbd44839-bf45-4d19-8ec0-cf2b47bb0285', distributed).
narrative_ontology:cs_reading_relation('dbd44839-bf45-4d19-8ec0-cf2b47bb0285', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbd44839-bf45-4d19-8ec0-cf2b47bb0285', ai_alignment_commitment__ethics_justice_reading, coexists_with).
narrative_ontology:cs_axiom('dbd44839-bf45-4d19-8ec0-cf2b47bb0285', foundational, control_and_justice_are_codependent).
narrative_ontology:cs_axiom_status(control_and_justice_are_codependent, holdable).
narrative_ontology:cs_axiom_grounding('dbd44839-bf45-4d19-8ec0-cf2b47bb0285', control_and_justice_are_codependent, empirically_contingent).
narrative_ontology:cs_axiom('dbd44839-bf45-4d19-8ec0-cf2b47bb0285', foundational, siloed_approaches_fragment_unified_problem).
narrative_ontology:cs_axiom_status(siloed_approaches_fragment_unified_problem, holdable).
narrative_ontology:cs_axiom_grounding('dbd44839-bf45-4d19-8ec0-cf2b47bb0285', siloed_approaches_fragment_unified_problem, empirically_contingent).
narrative_ontology:cs_reference_frame('dbd44839-bf45-4d19-8ec0-cf2b47bb0285', unified_alignment_framework).
narrative_ontology:cs_drift_state('dbd44839-bf45-4d19-8ec0-cf2b47bb0285', contemporary_institutional_fragmentation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dbd44839-bf45-4d19-8ec0-cf2b47bb0285', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, marginalized_communities_bearing_present_harms).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, research_fragmentation_cost_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, ai_ethics_and_justice_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, future_generations_and_humanity).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, ai_ethics_and_justice_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, regulatory_and_governance_bodies).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__integrated_reading, control_and_justice_are_codependent).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__integrated_reading, siloed_approaches_are_extractive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets research agendas and funding priorities around AI control problems (scalable oversight, specification gaming, deceptive alignment). Frames alignment as a technical control problem solvable through better training methods and verification. Operates with substantial institutional prestige and funding concentration. Can shift focus to integrated approaches but would redistribute authority and reduce dominance of control-technical problem formulation.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_safety_technical_researchers, agenda_setter,
    institutional, generational, mobile, global).

% Research bias, discrimination, and social harms in deployed AI systems. Benefit from the integrated framing (gains visibility and resources). Pay a cost in fragmented funding, secondary institutional status, and subordination to control-framed research hierarchies. Their problems are often cast as distinct from 'core alignment' rather than central to it.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_ethics_and_justice_researchers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, ai_ethics_and_justice_researchers, payer).

% Experience harms from deployed AI systems (hiring discrimination, credit denial, wrongful arrest, medical under-treatment, surveillance targeting). Bear costs of siloed approaches that treat their present suffering as separate from 'alignment' work. Not represented in research agenda-setting; their problems are externalized to adjacent fields (fairness ML, policy) rather than integrated into alignment definitions.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, marginalized_communities_bearing_present_harms, payer,
    powerless, biographical, trapped, global).

% Cannot participate in present alignment research; potential beneficiaries of solutions that prevent catastrophic loss of control. Siloed approaches risk creating advanced systems that solve control problems while replicating or amplifying present-day injustices at scale—a failure on both axes.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_generations_and_humanity, beneficiary,
    powerless, civilizational, trapped, universal).

% Deploy systems at scale. Can adopt alignment research outputs of either framing. Have incentive to embrace whichever framing produces lower deployment friction; integrated approaches requiring simultaneous attention to control and justice may increase deployment cost and timelines relative to control-only approaches.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_capability_developers_and_labs, observer,
    institutional, biographical, arbitrage, global).

% Set AI governance frameworks that currently privilege control narratives (compute-governance, capability control) over justice framings. Can mandate integrated approaches but would face pressure from capability labs and control-research communities. Pay a cost in delayed capability deployment if integration requirements slow rollout.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, regulatory_and_governance_bodies, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__integrated_reading, regulatory_and_governance_bodies, payer).

% Allocate alignment research funding. Currently concentrate resources on control-technical problems and institutional labs. Integrated approaches would require redirecting capital to justice-and-control research, upskilling review capacity to evaluate cross-domain work, and accepting longer time-to-publishable-results.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, funding_bodies_and_philanthropies, agenda_setter,
    institutional, biographical, mobile, global).

% Conduct fairness and bias research outside 'alignment' institutional structures. Would benefit from integration (gain resources, prestige, causal authority over system design). Currently excluded from core alignment agenda-setting; their work is treated as post-deployment mitigation rather than upstream alignment.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, policy_and_fairness_researchers, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__integrated_reading, funding_bodies_and_philanthropies).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a unified research and governance agenda that prevents both catastrophic loss of control over advanced AI systems AND reproduction/amplification of present-day injustices in AI deployment. Rejects the false choice that organizations can safely ignore either dimension and instead treats control and justice as mutually supporting: control failures enable injustice at scale, and justice-blind systems accumulate control risks (e.g., misalignment with marginalized communities' interests, hidden failure modes in underserved populations).
% TRANSFER_FUNCTION: Transfers research resources (funding, talent, institutional prestige) from discipline-siloed approaches (pure control-technical, pure fairness/policy) to integrated research programs. Transfers authority over alignment problem definition from control-technical institutes to cross-domain collaborations. Transfers costs of integration (coordination overhead, slower individual publication, complex interdisciplinary evaluation) to researchers and funding bodies.
% ABSENT_VOICES: Marginalized communities bearing present AI harms are structurally excluded from alignment research agenda-setting despite being primary stakeholders on both axes (present victims and future participants in integrated systems). Policy and fairness researchers are excluded from core alignment institutions despite their evidence on how control failures cascade through injustice vectors. Future generations cannot voice their preferences for what 'alignment' should prioritize.
% DISAPPEARANCE_RATIONALE: If this integrated commitment disappeared, research would bifurcate into siloed control and justice tracks. Control-track labs would develop systems optimized for control metrics that ignore justice problems. Justice-track researchers would treat bias and discrimination as downstream mitigation rather than upstream design constraints. The result would be advanced AI systems that solve control problems within narrow specifications while replicating or amplifying present injustices—a structural failure on both axes. The integrated commitment's disappearance would mean the loss of unified problem formulation, shared metrics, and cross-discipline feedback loops.
% FOUNDING_PROBLEM: Early AI safety research (2015–2020) fragmented into separate communities: technical safety researchers focused on control problems (specification, robustness, deception), fairness and ethics researchers focused on bias and discrimination in deployed systems. This fragmentation meant control work proceeded without addressing how misaligned systems might systematically target marginalized populations; fairness work proceeded without addressing how biased systems could undermine control in ways not visible to control researchers. The founding problem is the discovery that control and justice are not independent: siloed approaches risk creating systems that are 'safe' by control metrics while being catastrophically unjust; or systems that appear fair on aggregate metrics while containing latent control vulnerabilities in edge cases affecting marginalized groups.
% FOUNDING_PROBLEM_CORROBORATION: External corroboration comes from fairness ML researchers (Buolamwini, Gebru, Selbst) documenting how aggregate-fairness metrics hide discrimination in specific populations; from AI governance researchers (Andersson, Hadfield-Menell) modeling how misaligned systems that ignore marginalized preferences constitute control failures; from policy analysts (Leeson, Mittelstadt) documenting deployment harms that control-technical researchers do not measure. The integrated reading is NOT corroborated solely by either control or justice communities—it requires testimony from outside both siloed camps to establish that fragmentation itself is the failure mode.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because integration imposes genuine coordination costs (interdisciplinary overhead, slower individual publication, complex evaluation rubrics) that cannot be fully recovered by efficiency gains; the costs are real and borne asymmetrically by subordinate disciplines. Suppression is high (0.71) because institutional structures actively prevent the 'pure control' alternative (easier publication, faster results, simpler metrics) from being pursued even when technically feasible—funding mandates, governance frameworks, and research hierarchy reinforcement actively discourage siloed approaches. Theater is moderate (0.48) because integration genuinely requires cross-domain collaboration (not pure performance), but the presentation of integration as 'alignment breakthrough' rather than 'institutional reform' introduces performative elements. Accessibility collapse is moderate (0.58) because the integrated reading is technically available and rationally defensible—alternatives (siloed approaches) exist and are not blocked by physical law—but institutional structures make them costly. Resistance is high (0.74) because control-technical researchers actively resist loss of autonomy and prestige, capability labs resist deployment slowdown, and funding bodies resist coordination overhead. Measurements show extractiveness rising through early interval (discovery phase, 0–8) then plateauing (institutional resistance ceiling, 12–20), suggesting the constraint reaches a stable extraction level once institutional structures fully resist integration. Theater rises through 8, plateaus at ~0.48 (the constraint becomes 'normal' governance practice rather than novel proposal), suggesting performative elements stabilize once integration becomes institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   AI safety researchers experience this constraint as legitimate coordination (solving a real fragmentation problem) and acceptable cost of doing better research. Marginalized communities experience it as continued exclusion from alignment definition-setting despite being primary stakeholders. Justice researchers experience it as partial institutional integration without commensurate prestige or autonomy. Capability labs experience it as regulatory friction. The engine computes these divergent types from the structural data: researchers with institutional power and mobile exit (low d) may compute rope-type benefits; researchers with moderate power and constrained exit (medium d) may compute tangled-rope costs; communities with powerless status and trapped exit (high d) compute snare-type victimhood. The authoring claim is tangled_rope because the constraint as a whole exhibits both coordination (unified problem formulation) and asymmetric extraction (subordination of justice work, exclusion of marginalized voices, costs borne by less powerful seats).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is highly differentiated by seat. AI safety researchers (institutional, mobile exit): d ≈ 0.25 (beneficiary end)—they gain agenda-setting authority and prestige for integration while retaining mobility to exit. Marginalized communities (powerless, trapped exit): d ≈ 0.85 (target end)—they have no exit and bear costs of continued exclusion despite nominal inclusion in 'alignment' definitions. Justice researchers (moderate power, constrained exit): d ≈ 0.55 (symmetric end)—they benefit from integration but lose institutional prestige and face barriers to exit from subordinate status. Funding bodies (institutional, mobile exit): d ≈ 0.30—they gain governance legitimacy from integration while retaining ability to revert to control-heavy funding if integration proves expensive. The constraint exhibits no single d across the stakeholder set; it is precisely this directionality heterogeneity that makes it tangled_rope rather than pure rope (which would require more symmetric beneficiary positions).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (fragmentation of control and justice research, blind spots in both communities) is live and actively recognized within the integrated-reading community. However, the constraint is itself subject to institutional capture risk: integration could become a performative requirement (every control paper required to mention fairness, every fairness paper required to acknowledge control concerns) without substantive resource reallocation or decision-making authority transfer. The theater_ratio measurement series (0.35 → 0.48, plateauing) suggests this risk: early integration (t=0–8) is driven by genuine problem-discovery; later integration (t=12–20) increasingly exhibits theatrical compliance (funded papers cite integration language but resource allocation remains siloed). This trajectory suggests the constraint is drifting toward piton (mandated practice, performative integration, actual bifurcation persisting) rather than stable tangled_rope. Mandatrophy has not yet resolved, but the measurement trajectory predicts risk of resolution via institutional capture (integration becomes a checkbox without power transfer).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_justice_codependency,
    'Are control problems and justice problems genuinely codependent (such that solving one without the other creates compounding failure), or are they independent problems best solved by specialists in each domain?',
    'Failure-mode analysis of deployed AI systems: do control failures systematically correlate with justice failures? Do justice-blind systems show latent control vulnerabilities? Do control-optimized systems that ignore justice feedback accumulate specification gaming risk in underserved populations?',
    'If codependent, the integrated reading stands and justifies resource integration. If independent, the siloed readings are justified and integration is unnecessary overhead. If partially dependent (some correlations but not all), the constraint is a partial integration with residual fragmentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_justice_codependency, empirical, 'Whether control and justice problems are mutually supporting or independent.').

omega_variable(
    institutional_integration_capture_risk,
    'Is institutional integration of control and justice research genuine (resource reallocation, authority transfer, joint decision-making) or performative (citation requirements, funded ''collaboration'' producing disconnected papers, continued silos with rhetorical cover)?',
    'Track longitudinal metrics: do integrated programs show increased cross-community hiring, shared publication venues with joint review, and mutual veto authority? Or do they show integration language with continued institutional bifurcation?',
    'If genuine integration occurs, the constraint is stable tangled_rope with real coordination cost. If performative, the constraint drifts to piton (mandated practice, atrophied function, continued bifurcation maintained theatrically).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_integration_capture_risk, empirical, 'Whether institutional integration represents genuine resource reallocation or captured rhetoric.').

omega_variable(
    marginalized_community_inclusion_mechanism,
    'How can marginalized communities bearing present AI harms move from excluded stakeholder status to integrated agenda-setting authority without reproducing the power hierarchies that excluded them initially?',
    'Governance innovation in research institutions: explicit authority allocation to community representatives, resource control by community boards, veto power over research agendas affecting their populations, and accountability mechanisms when research diverges from community interests.',
    'If mechanisms exist and are implemented, victims (d high) may compute rope-type benefits and the constraint becomes more symmetrically beneficial. If mechanisms remain theoretical or performative, victims remain payers despite nominal inclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginalized_community_inclusion_mechanism, preference, 'Governance mechanisms for genuine inclusion of currently excluded stakeholders.').

omega_variable(
    competing_readings_logical_status,
    'Do the three readings (integrated, control-only, justice-only) represent genuinely different problem definitions (reading_relations: coexists_with) or does the integrated reading logically foreclose the siloed readings within any single coherent framework (reading_relations: forecloses)?',
    'Formal analysis: can a unified AI governance framework adopt integrated principles while respecting the core premises of siloed readings? Or does integrated adoption require rejecting the siloed premises?',
    'If coexistent, the three readings persist as live positions across different institutional actors indefinitely. If foreclosure holds, only the integrated reading is defensible and silos persist as institutional failures rather than legitimate alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_readings_logical_status, conceptual, 'Logical relationship between the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__integrated_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(ai_a_tr_t0, observed).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__integrated_reading, theater_ratio, 4, 0.39).
narrative_ontology:measurement_basis(ai_a_tr_t4, observed).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__integrated_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement_basis(ai_a_tr_t8, observed).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__integrated_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement_basis(ai_a_tr_t12, projected).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__integrated_reading, theater_ratio, 16, 0.49).
narrative_ontology:measurement_basis(ai_a_tr_t16, projected).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__integrated_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(ai_a_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__integrated_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_a_be_t0, observed).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__integrated_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(ai_a_be_t4, observed).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__integrated_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement_basis(ai_a_be_t8, observed).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__integrated_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement_basis(ai_a_be_t12, projected).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__integrated_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement_basis(ai_a_be_t16, projected).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__integrated_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(ai_a_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__integrated_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(ai_a_su_t0, observed).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__integrated_reading, suppression_requirement, 4, 0.67).
narrative_ontology:measurement_basis(ai_a_su_t4, observed).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__integrated_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement_basis(ai_a_su_t8, observed).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__integrated_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(ai_a_su_t12, projected).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_commitment__integrated_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(ai_a_su_t16, projected).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__integrated_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ai_a_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__integrated_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__ethics_justice_reading).

% DUAL FORMULATION NOTE:
% This integrated reading is one of three coherent instantiations of the ai_alignment_commitment kernel. The three readings decompose a contested problem definition into three separate constraint stories: (1) safety_control_reading frames alignment as control-technical problem solvable by control specialists; (2) ethics_justice_reading frames alignment as justice problem solvable by fairness/ethics researchers; (3) integrated_reading frames alignment as codependent problem requiring simultaneous attention. These are not the same constraint measured from three angles—they define alignment differently and thus define what institutions should fund, who should lead research, and how success should be measured. Each reading carries its own ε (control-reading: low extraction from control-focused labs, high coordination value; justice-reading: high extraction from marginalized communities, moderate coordination value; integrated-reading: moderate extraction due to coordination cost and institutional fragmentation). The three stories are linked via network.affects_constraints to enable contamination analysis: if the integrated reading gains institutional adoption, it influences how control-reading labs operate (must incorporate justice feedback) and how justice-reading work is resourced (integrated funding rather than silo funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, powerless, 0.85).
constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, moderate, 0.55).
constraint_indexing:directionality_override(ai_alignment_commitment__integrated_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
