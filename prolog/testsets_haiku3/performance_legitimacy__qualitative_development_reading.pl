% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Performance Legitimacy via Qualitative Development Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   The qualitative development reading grounds state legitimacy in
 *   structural transformation — innovation, industrial upgrading,
 *   sustainability, and efficiency gains — rather than raw GDP growth. The
 *   state's planning authority identifies high-tech sectors and state-backed
 *   innovation ecosystems as beneficiaries and reallocates capital from
 *   property-dependent local governments and traditional manufacturing. This
 *   reading is one of four contested interpretations of a shared kernel
 *   (performance_legitimacy); it coexists with quantitative_growth_reading,
 *   livelihood_security_reading, and techno_nationalist_reading, each
 *   instantiating different beneficiary/victim structures and different ε
 *   values. The JSON generates THIS reading only — the qualitative
 *   development reading as a stand-alone constraint with its own metrics and
 *   structural claims.
 *
 * KEY AGENTS:
 *   - Central planning authority: Institutional agenda-setter; sets legitimacy criteria and resource allocation rules; operates the qualitative development frame.
 *   - High-tech sectors: Powerful beneficiaries; receive preferential capital access, R&D subsidies, regulatory forbearance; can arbitrage globally.
 *   - State-backed innovation ecosystem: Institutional beneficiary; research institutions, innovation zones, state-aligned venture capital; constrained exit.
 *   - Traditional manufacturing: Moderate payer; faces deprioritization, capital withdrawal, worker deskilling; constrained by asset specificity.
 *   - Property-dependent local governments: Moderate payers; historically relied on land development revenue; caught in fiscal crisis as qualitative development reframes growth.
 *   - Displaced manufacturing workers: Powerless payers; face unemployment and deskilling; identity-locked to manufacturing employment; explicitly excluded from retraining priority.
 *   - Citizens in non-innovation regions: Powerless, excluded; live outside high-tech zones; regional inequality deepens; not articulated in legitimacy frame.
 *   - International trade partners: Institutional observers; see industrial policy shift and potential protectionism.
 *   - Sustainability advocates: Organized beneficiaries; gain narrative legitimacy from explicit sustainability framing; mobile exit if commitment weakens.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.72).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy via Qualitative Development Reading").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, 'fd1835e1-1539-40b6-b6a8-3d610bc079b3').
narrative_ontology:cs_kernel_codification('fd1835e1-1539-40b6-b6a8-3d610bc079b3', formalized).
narrative_ontology:cs_authority_grounding('fd1835e1-1539-40b6-b6a8-3d610bc079b3', extraction).
narrative_ontology:cs_interpretation_layer_present('fd1835e1-1539-40b6-b6a8-3d610bc079b3').
narrative_ontology:cs_reading_relation('fd1835e1-1539-40b6-b6a8-3d610bc079b3', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd1835e1-1539-40b6-b6a8-3d610bc079b3', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd1835e1-1539-40b6-b6a8-3d610bc079b3', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('fd1835e1-1539-40b6-b6a8-3d610bc079b3', foundational, innovation_over_growth).
narrative_ontology:cs_axiom_status(innovation_over_growth, holdable).
narrative_ontology:cs_axiom_grounding('fd1835e1-1539-40b6-b6a8-3d610bc079b3', innovation_over_growth, empirically_contingent).
narrative_ontology:cs_axiom('fd1835e1-1539-40b6-b6a8-3d610bc079b3', foundational, sustainability_constraint_on_development).
narrative_ontology:cs_axiom_status(sustainability_constraint_on_development, holdable).
narrative_ontology:cs_axiom_grounding('fd1835e1-1539-40b6-b6a8-3d610bc079b3', sustainability_constraint_on_development, instrumental).
narrative_ontology:cs_reference_frame('fd1835e1-1539-40b6-b6a8-3d610bc079b3', high_quality_structural_transformation).
narrative_ontology:cs_drift_state('fd1835e1-1539-40b6-b6a8-3d610bc079b3', contemporary_execution_gap, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd1835e1-1539-40b6-b6a8-3d610bc079b3', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, sustainability_advocates).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, displaced_manufacturing_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the qualitative development mandate: legitimacy flows from demonstrated innovation capacity, industrial upgrading, sustainability metrics, and efficiency gains rather than raw GDP expansion. Controls resource allocation toward high-tech sectors, venture capital infrastructure, and research institutions. Justifies the mandate as escaping middle-income trap and ensuring long-term competitiveness.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_planning_authority, agenda_setter,
    institutional, generational, analytical, national).

% Receives preferential access to state capital, R&D subsidies, and regulatory forbearance. Legitimacy narrative positions them as engines of structural transformation. Can exit to global markets or relocate if domestic support declines. Benefits from the innovation frame regardless of actual output.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    powerful, generational, arbitrage, global).

% State research institutions, innovation zones, venture capital funds channeled through state-aligned vehicles. Absorbs substantive resources; legitimacy depends on demonstrating transformative output. Cannot exit but can claim mission-drift (pivoting toward downstream commercialization rather than knowledge production).
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    institutional, generational, constrained, national).

% Faces deprioritization and withdrawal of state support under the qualitative development frame. Capital that once flowed to industrial expansion is diverted to high-tech sectors. Workers face skill obsolescence as the narrative frames traditional manufacturing as low-value. Exit is constrained by asset specificity and geographic immobility of factories.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing, payer,
    moderate, biographical, constrained, national).

% Historically relied on land development and property taxation for revenues. The qualitative development mandate reframes property-driven growth as destabilizing and low-quality. Local fiscal crises ensue as land sales decline and they cannot exit the jurisdiction. Caught between central mandate and local revenue collapse.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    moderate, biographical, trapped, local).

% Face unemployment and deskilling as traditional manufacturing is written out of the legitimacy narrative. Retraining programs are subordinate to innovation investment. Cannot exit due to asset specificity of skills and geographic dependence. Identity tied to manufacturing employment with no pathway articulated in the new regime.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, displaced_manufacturing_workers, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, displaced_manufacturing_workers, excluded).

% Live outside high-tech innovation zones and benefit minimally from the structural transformation narrative. Regional inequality deepens as resources concentrate. Their concerns (employment, services, local stability) are not articulated in the legitimacy frame; they are told to wait for spillover benefits.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, citizens_in_non_innovation_regions, excluded,
    powerless, biographical, trapped, regional).

% Observe the qualitative development reading as it shapes domestic industrial policy, tariffs on traditional goods, and preferential treatment for high-tech sectors. May experience disruption to existing supply chains or disadvantage if the reading is used to justify strategic protectionism.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, international_trade_partners, observer,
    institutional, generational, analytical, global).

% Support the qualitative development frame because it explicitly positions sustainability and efficiency as legitimacy criteria, distinct from raw growth. Can advocate for stronger environmental standards within the framework. Benefits from narrative alignment even where actual environmental outcomes lag commitments.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, sustainability_advocates, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of directing state investment and institutional capacity toward long-term structural competitiveness (innovation, sustainability, efficiency) rather than short-term GDP growth. Aligns diverse actors (research institutions, venture capital, industrial policy) toward shared metrics of qualitative advancement.
% TRANSFER_FUNCTION: Transfers state capital (subsidies, R&D funding, preferential regulatory treatment, access to innovation zones) from property-dependent local governments and traditional manufacturing sectors TO high-tech sectors and state-backed innovation ecosystems. Moves legitimacy narrative from growth-delivery to transformation-delivery.
% ABSENT_VOICES: Displaced manufacturing workers, property-dependent local governments, citizens in non-innovation regions, and traditional manufacturing employers are structurally excluded from the framing process. Their preference for steady-state manufacturing employment and local property-based fiscal stability contradicts the qualitative development reading and is not articulated in the legitimacy claim.
% DISAPPEARANCE_RATIONALE: If the qualitative development legitimacy frame vanished, state capital would flow back toward property development and traditional manufacturing; local governments would restore fiscal models around land; regional inequality metrics would stabilize; and the narrative justification for worker displacement would disappear, forcing reckoning with social costs.
% FOUNDING_PROBLEM: Economic growth at previous rates had become ecologically unsustainable and was producing diminishing employment gains; technological leadership was slipping relative to rivals. The state needed a legitimacy claim that decoupled state performance from raw GDP while redirecting investment toward long-term competitive advantage.
% FOUNDING_PROBLEM_CORROBORATION: High-tech sectors and innovation institutions attest the founding problem is live and the reading is necessary. International development analysts and sustainability researchers corroborate that previous growth models were unsustainable. Traditional manufacturing sectors and local governments attest the founding problem was exaggerated to justify capital reallocation, and that employment and fiscal stability remain unsolved — sources outside the benefiting parties dispute the reading's framing of the problem.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) is high because the qualitative development frame justifies capital reallocation that materially harms traditional manufacturing and local governments despite the narrative of common benefit. Suppression (0.72) is substantial because enforcement requires actively blocking alternative legitimacy frames (quantitative growth, livelihood security) and preventing dissenting sectors from influencing resource allocation — the central authority must maintain narrative dominance over competing readings. Theater (0.41, moderate-low) reflects genuine innovation activity but also mounting performative indicators: published papers without commercialization, innovation zones with subsidized tenants, sustainability targets missed but reported as 'in progress.' Accessibility collapse (0.58) is moderate: traditional sectors recognize they are losing capital access but cannot articulate an alternative legitimacy frame within the state's institutional logic; they are trapped, not persuaded. Resistance (0.71) is substantial: manufacturing sectors, local governments, and worker advocacy actively contest the reading, but lack institutional power to change the central authority's frame. The measurement series show extractiveness rising as spillover delays and regional inequality deepens; theater rising as performance reporting emphasizes outputs over outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The central authority and high-tech beneficiaries experience the qualitative development reading as genuine coordination — escaping unsustainable growth, building long-term competitiveness, modernizing the economy. Traditional manufacturing and local government seats experience the same constraint as pure extraction — their capital is diverted, their legitimacy is erased from the narrative, their workers are written off as obstacles to transformation. The agenda-setter frame treats this divergence as inevitable transition cost; the payer frames treat it as manufactured crisis. The engine computes this structural asymmetry from the beneficiary/victim declarations and exit options: beneficiaries with arbitrage/mobile exit options and powerful institutional positioning compute low d (beneficiary directionality); payers with constrained/identity-locked exit and moderate/powerless positioning compute high d (target directionality). The divergence is not a measurement error — it is the structural fact the classification system exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The central planning authority (institutional, analytical positioning, controls the frame itself) sits at d ≈ 0.0 (full beneficiary — the constraint legitimizes its authority and justifies its resource allocation decisions). High-tech sectors (powerful, global arbitrage exit, beneficiary role) sit at d ≈ 0.1-0.2 (strong beneficiary position — they receive capital, can exit if support shifts, benefit from narrative prioritization). State-backed innovation ecosystem (institutional beneficiary, constrained exit, mission depends on the frame) sits at d ≈ 0.2-0.3 (moderate-to-beneficiary — benefits from the frame but cannot exit if it becomes extractive). Traditional manufacturing (moderate power, constrained exit by asset specificity, payer role) sits at d ≈ 0.7-0.8 (strong target position — capital is extracted, identity cannot easily shift, exit options collapse under the frame). Property-dependent local governments (moderate power, trapped exit, payer role) sit at d ≈ 0.75-0.85 (strong target — fiscal model collapses under the frame, no exit). Displaced manufacturing workers (powerless, identity-locked exit, explicitly excluded) sit at d ≈ 0.9+ (full target — extraction is total, suppression is internalized through narrative erasure, no articulated pathway forward). Citizens in non-innovation regions (powerless, trapped, excluded) sit at d ≈ 0.8-0.9 (strong target — excluded from benefit narrative, trapped by geography, no voice in frame construction).
 *
 * MANDATROPHY ANALYSIS:
 *   The qualitative development reading shows no evidence of mandatrophy (mandate obsolescence). The founding problem — unsustainable growth and slipping technological leadership — remains substantively contested (status: contested). The reading continues to perform institutional functions: it justifies capital reallocation, legitimizes deindustrialization, channels investment toward high-tech, and provides a narrative frame that allows the central authority to claim transformation rather than decline. However, the measurement series reveal rising theater (0.22 → 0.41): performance reporting emphasizes innovation pipeline and sustainability targets, but commercialization rates and actual behavioral change in displaced regions remain opaque. This suggests the constraint is accumulating a secondary performance function (signaling legitimacy) alongside its primary function (directing capital). If theater continues rising above 0.6 while actual innovation spillover and employment gains remain concentrated, the constraint would approach piton territory (atrophied primary function, sustained by theatrical maintenance). The current state (0.41 theater) is better described as tangled_rope showing early-stage Goodhart drift: the legitimacy metric (innovation targets, sustainability indicators) is beginning to diverge from the real outcome metric (employment, regional equality, actual commercialization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competing_legitimacy_readings,
    'Is the qualitative development reading structurally incompatible with the livelihood_security_reading and quantitative_growth_reading, or are they alternative frames for the same constraint?',
    'Examine whether a single policy framework could simultaneously prioritize innovation, job creation, property-based local revenue, and raw growth. If sustainable integration exists, the readings coexist; if trade-offs force institutional choice, one reading''s implementation suppresses the others.',
    'If integration is impossible, classify as coexists_with (different parties, different frames). If one reading''s institutional dominance prevents the others from operating, reclassify edges toward forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_legitimacy_readings, conceptual, 'Whether the four performance legitimacy readings are logically incompatible or represent alternative priority orderings.').

omega_variable(
    innovation_outcome_measurement,
    'What portion of the state''s measured innovation output is genuine structural transformation versus theater — demonstrated patents and commercialization versus subsidy-dependent R&D with limited real-world application?',
    'Track patent-to-commercialization ratios, ratio of state-funded research becoming commercial products, return on venture capital deployed through state-aligned funds over a 10+ year horizon.',
    'High theater (real innovation signal weak) would support reclassifying extractiveness upward and the constraint toward snare; low theater would support the tangled_rope framing as stated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_outcome_measurement, empirical, 'Whether the innovation legitimacy claim tracks genuine transformation or subsidy-dependent signaling.').

omega_variable(
    regional_spillover_asymmetry,
    'Do innovation and high-tech sector gains actually spillover to non-innovation regions and displaced manufacturing areas, or do they concentrate geographically and sectorally with minimal diffusion?',
    'Measure employment growth, wage growth, and human capital investment in regions outside primary innovation zones. Compare spillover rates to the state''s official efficiency and equity claims.',
    'Minimal spillover would strengthen the characterization of traditional manufacturing and local governments as pure payers with no beneficiary pathway; significant spillover would support a more symmetric coordination interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_spillover_asymmetry, empirical, 'Whether qualitative development gains diffuse equitably or concentrate on high-tech beneficiaries.').

omega_variable(
    sustainability_enforcement_asymmetry,
    'Are sustainability and efficiency metrics enforced equally across high-tech and traditional sectors, or are high-tech sectors given greater regulatory forbearance in pursuit of innovation targets?',
    'Audit environmental compliance enforcement, labor standard enforcement, and efficiency audits comparing high-tech sectors against traditional manufacturing over the interval.',
    'Asymmetric enforcement (forbearance for high-tech) would strengthen the extraction reading and support mandatrophy analysis; equal enforcement would support the coordination framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sustainability_enforcement_asymmetry, empirical, 'Whether sustainability criteria are applied uniformly or selectively to enable high-tech prioritization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__qualitative_development_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__qualitative_development_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__qualitative_development_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__qualitative_development_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__qualitative_development_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__qualitative_development_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__qualitative_development_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__qualitative_development_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__qualitative_development_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__qualitative_development_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__qualitative_development_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__qualitative_development_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__qualitative_development_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__qualitative_development_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__qualitative_development_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__qualitative_development_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% The performance_legitimacy kernel decomposes into four structurally distinct constraint stories, each instantiating a different reading of how state performance legitimizes authority. The qualitative_development_reading prioritizes innovation and industrial upgrading; structural beneficiaries are high-tech sectors and state innovation ecosystems; structural victims are traditional manufacturing and property-dependent local governments. Each sibling reading carries a different ε (extractiveness), different beneficiary/victim structure, and different measured metrics. They are linked via network.affects_constraints because they compete for institutional dominance in a single state authority — the authority cannot simultaneously prioritize all four without contradiction. The stories share the kernel (performance legitimacy) but diverge on what performance means, who defines it, and what gets sacrificed in pursuit of it. See each story's cs_structure.reading_relations for the logical relationships between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__qualitative_development_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
