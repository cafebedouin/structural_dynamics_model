% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: AI Risk Governance: Bridge Reading (Present Harms + Existential Risks Unified)
 *   domain: AI_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   AI risk governance presents a contested kernel: should the field
 *   prioritize existential risks (superintelligence misalignment), present
 *   harms (bias, surveillance, labor displacement), or treat both as
 *   structurally entangled concerns requiring unified frameworks? This
 *   constraint story instantiates the bridge reading — the commitment that
 *   unified governance is necessary because present AI failures (misaligned
 *   incentives, power asymmetries, inadequate testing, corner-case
 *   brittleness) are structurally coupled to existential risk scenarios. The
 *   bridge reading claims that governance addressing both timeframes reduces
 *   blind spots and aligns institutional incentives. However, the structural
 *   data reveals a paradox: the unified framework mandate has created
 *   bottleneck institutions (the ~5% of researchers/institutions that span
 *   both domains) that capture resources and set legitimacy standards, while
 *   both present-harm and existential-risk communities experience the
 *   constraint as extraction. Present-harm advocates see resources flowing to
 *   'integrated safety-ethics' initiatives rather than demonstrable harm
 *   mitigation. Existential-risk researchers see legitimacy pressure to frame
 *   speculative long-term risks in present-harm language. Both face
 *   suppression of specialized alternatives: a researcher pursuing focused
 *   near-term harm mitigation or purely existential safety research faces
 *   institutional pressure to demonstrate 'unified framework' integration.
 *   The constraint exhibits all six DR types perspectivally: the bridging
 *   institutions experience it as pure coordination (Rope); the governance
 *   theater around unified frameworks is performative (Piton); both victim
 *   classes experience extraction (Snare); moderate actors in each community
 *   experience mixed coordination and extraction (Tangled Rope); the
 *   analytical observer risks naturalizing this institutional arrangement as
 *   logically necessary (Mountain, false summit).
 *
 * KEY AGENTS:
 *   - Marginalized Populations (Present Harms): Primary victim (powerless/trapped, biographical) — bear algorithmic bias, surveillance, labor displacement TODAY while governance discourse defers action via unified framework framing
 *   - Future Humanity (Existential Risk): Primary victim (powerless/trapped, civilizational) — depend on present governance decisions for existential safety; trapped in scenarios they cannot negotiate
 *   - Harm-Mitigation Communities: Secondary actor (moderate/constrained, biographical) — constrained by funding scarcity and pressure to adopt unified framing; benefit from legitimacy but sacrificed specialization
 *   - Existential Risk Communities: Secondary actor (moderate/constrained, generational) — constrained by legitimacy pressure (existential risks are contested); benefit from unified framing but face specialization pressure
 *   - Bridging Institutions: Primary beneficiary (institutional/arbitrage, immediate) — ~5% of papers/institutions spanning both domains; control resource allocation and legitimacy conferral
 *   - Governance Bodies (Policy Frameworks): Institutional maintainer (institutional/constrained, generational) — produce unified-framework governance theater (advisory boards, risk frameworks); maintain infrastructure without functional integration
 *   - Analytical Observer: Sees the constraint as logical necessity (analytical/analytical, civilizational) — risks naturalizing institutional choice as inherent rational requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.42).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.58).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "AI Risk Governance: Bridge Reading (Present Harms + Existential Risks Unified)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "AI_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, 'c61fb2a0-1e62-43a7-89a3-f79438d2db2a').
narrative_ontology:cs_kernel_codification('c61fb2a0-1e62-43a7-89a3-f79438d2db2a', distributed).
narrative_ontology:cs_authority_grounding('c61fb2a0-1e62-43a7-89a3-f79438d2db2a', distributed).
narrative_ontology:cs_reading_relation('c61fb2a0-1e62-43a7-89a3-f79438d2db2a', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('c61fb2a0-1e62-43a7-89a3-f79438d2db2a', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('c61fb2a0-1e62-43a7-89a3-f79438d2db2a', foundational, present_existential_structural_coupling).
narrative_ontology:cs_axiom_status(present_existential_structural_coupling, holdable).
narrative_ontology:cs_axiom_grounding('c61fb2a0-1e62-43a7-89a3-f79438d2db2a', present_existential_structural_coupling, empirically_contingent).
narrative_ontology:cs_axiom('c61fb2a0-1e62-43a7-89a3-f79438d2db2a', foundational, governance_integration_reduces_blind_spots).
narrative_ontology:cs_axiom_status(governance_integration_reduces_blind_spots, holdable).
narrative_ontology:cs_axiom_grounding('c61fb2a0-1e62-43a7-89a3-f79438d2db2a', governance_integration_reduces_blind_spots, instrumental).
narrative_ontology:cs_reference_frame('c61fb2a0-1e62-43a7-89a3-f79438d2db2a', multi_timescale_governance_imperative).
narrative_ontology:cs_drift_state('c61fb2a0-1e62-43a7-89a3-f79438d2db2a', contemporary_institutional_capture, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c61fb2a0-1e62-43a7-89a3-f79438d2db2a', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, marginalized_populations_present).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity_existential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED POPULATIONS — SNARE — Trapped by algorithmic bias, surveillance, labor displacement, and content moderation systems deployed today. Cannot exit extraction in biographical time. The unified framework discourse delays resources: both existential-risk advocates and present-harm advocates use 'we must address both' as rhetorical cover while actual funding and research capacity flow elsewhere (to bridging institutions). Full experienced extraction.
constraint_indexing:constraint_classification(ai_risk_governance_priority__bridge_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE HUMANITY — SNARE — Trapped in an existential scenario where present governance decisions (training protocols, compute allocation, safety prioritization) may foreclose human agency if superintelligence emerges unaligned. Cannot exit or negotiate. The unified framework paradoxically delays both existential safety research and near-term harm mitigation by concentrating resources in bridging institutions rather than distributing to focused specialists. Full experienced extraction.
constraint_indexing:constraint_classification(ai_risk_governance_priority__bridge_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: EXISTING HARM-MITIGATION COMMUNITIES — TANGLED ROPE — Constrained by funding scarcity and institutional pressure to adopt 'unified frameworks' that dilute focus on demonstrable present harms. But also benefit from the bridge reading's legitimacy gain — 'we must address both' gives their work higher prestige than narrow present-harm focus alone. Moderate extraction with some genuine coordination benefit.
constraint_indexing:constraint_classification(ai_risk_governance_priority__bridge_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXISTENTIAL RISK RESEARCH COMMUNITIES — TANGLED ROPE — Constrained by legitimacy pressure: existential risks are contested/speculative, so 'we must address both' provides cover for existential research funding within broader governance mandates. But also genuine coordination benefit — near-term harm research surfaces failure modes (misalignment with human values, power asymmetries) that inform existential safety. Moderate extraction with real coordination content.
constraint_indexing:constraint_classification(ai_risk_governance_priority__bridge_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BRIDGING INSTITUTIONS — ROPE — The ~5% of institutions/papers that span both present-harm and existential-risk discourse. These brokers benefit from the unified framework mandate: they become the arbiters of 'how to address both,' controlling resource allocation, standard-setting, and legitimacy conferral. They experience the constraint as pure coordination — integrating two important research domains — while capturing disproportionate resources. Net beneficiary with mobility to allocate resources across domains.
constraint_indexing:constraint_classification(ai_risk_governance_priority__bridge_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GOVERNANCE BODIES AND POLICY FRAMEWORKS — PITON — Perform 'integrated governance' theater: advisory boards, risk assessment frameworks, multi-stakeholder initiatives that claim to address both harms and existential risks. These are largely performative — they produce reports, consultations, and legitimacy signals while actual safety research and harm mitigation remain separated institutionally. The unified framework mandate justifies the governance infrastructure's existence even as it fails to coordinate real research. Theater_ratio high; functional integration low.
constraint_indexing:constraint_classification(ai_risk_governance_priority__bridge_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — MOUNTAIN — From a civilizational frame, risk governance simply MUST address multiple timescales; it is logically impossible to safely govern AI systems that affect both present humans and future existence without considering both dimensions. The unified framework is not a contingent institutional choice but an inherent requirement of rational governance. However, the base properties contradict this: identifiable beneficiaries (bridging institutions), suppressed alternatives (specialized focus on either domain), and enforced institutional apparatus all indicate false summit. The 'natural law' framing naturalizes what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(ai_risk_governance_priority__bridge_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_risk_governance_priority__bridge_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_risk_governance_priority__bridge_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, TR),
    TR >= 0.70.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate. The bridge reading claims genuine coordination benefit — unified governance does reduce some blind spots (e.g., recognizing that present misalignment with human values informs existential safety). However, the empirical flow of resources contradicts the coordination claim: bridging institutions capture disproportionate resources relative to research output, while both present-harm and existential-risk specialists see their work de-prioritized. The moderate extractiveness reflects this contradiction: the constraint has real coordination content but is structured as extraction bottleneck. Suppression (0.58): Moderate-high. Alternatives are suppressed: researchers pursuing focused present-harm mitigation face institutional pressure to demonstrate 'integration'; pure existential-safety researchers face questions about relevance. Institutional incentives reward bridging work over specialization. However, suppression is not total — specialized research communities persist, advocacy for both timeframes continues. Theater ratio (0.65): Moderate-high. Governance theater is substantial: multi-stakeholder initiatives, advisory boards, 'integrated risk frameworks' that claim to address both dimensions but remain institutionally separated. However, not purely performative — real research attempting integration exists; the theater reflects genuine tension between coordination ideal and institutional reality rather than complete functional emptying.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between bridging institutions (who experience the constraint as pure coordination) and both victim classes (who experience extraction). Both present-harm and existential-risk communities occupy intermediate positions: they benefit from the unified framework's legitimacy and integrated research insights, but suffer from resource bottlenecks and specialization pressure. The gap reveals that the bridge reading has successfully constructed a narrative of logical necessity ('we MUST address both') that functions as institutional legitimacy while actual resource and research capacity flows to bottleneck actors. The analytical observer risks collapsing the gap by naturalizing the institutional arrangement as inherent to rational governance. The false-summit test is key here: if unified governance is truly logically necessary (as the mountain perspective claims), then beneficiaries should not exist (no one should extract from a law of logic). But identifiable beneficiaries do exist — bridging institutions with discretionary resource allocation power. The engine's FSM will reclassify this as a false summit, revealing that the 'natural law' framing is a committer strategy to naturalize a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from structural position: power level, exit options, and relationship to the extraction flow. Marginalized populations and future humanity are both trapped (no exit) and victims of extraction — they derive d ≈ 0.95, producing maximum f(d) ≈ 1.42, maximum experienced extraction. Harm-mitigation and existential-risk communities are moderate-power agents facing constrained exits (career penalties for specialization) and mixed victim/benefit status — they derive d ≈ 0.60–0.70, producing f(d) ≈ 0.95–1.10, moderate extraction. Bridging institutions are beneficiaries with arbitrage options (they can reallocate resources between domains) — they derive d ≈ 0.15, producing f(d) ≈ -0.01, negative extraction (they benefit from the constraint). Governance bodies are institutional actors with constrained exits (institutional inertia) and secondary victim status (their theater doesn't solve the underlying problem) — they derive d ≈ 0.50, producing f(d) ≈ 0.65, moderate extraction. The analytical observer derives d ≈ 0.72 (analytical observer canonical), producing f(d) ≈ 1.15, and risk of false-summit naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy (the paradox of classification instability) by explicitly embracing the kernel contest: the bridge reading is ONE COHERENT POSITION within a three-way dispute about governance priority. The mandatrophy would arise if we tried to force a single classification ('this IS a Tangled Rope') without acknowledging that different institutional frameworks would arrive at different answers. The bridge reading's claim is that unification IS the correct answer because present harms and existential risks are coupled. The existential_risk_reading's claim is that existential prevention is prior and justifies any present institutional suboptimality. The near_term_harms_reading's claim is that present realities take priority and existential theorizing is luxury reasoning. Each is internally coherent from within its reference frame. The mandatrophy is resolved by modeling all three as live options whose relationship is determined by reading_relations and axiom conflict analysis. The engine's false-summit detection will identify the analytical observer's mountain as false, revealing the institutional contingency underlying the bridge reading's naturalization strategy. The classification (Tangled Rope) reflects the actual structural data: genuine coordination content mixed with bottleneck extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bridging_institution_dependency,
    'Are bridging institutions genuine coordinators of two research domains, or are they bottlenecks that benefit from enforced integration?',
    'Network analysis: if removing bridging institutions from funding chains increases fragmentation (resources can''t flow between domains), they are coordinators. If removing them increases specialization but not research quality decline, they are extractive bottlenecks. Longitudinal comparison of funding efficiency and research output pre/post bridging-institution integration.',
    'If coordinators: the constraint is structural coordination (Rope). If bottlenecks: the constraint is structural extraction (Tangled Rope or Snare from subordinate perspectives). Classification changes based on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bridging_institution_dependency, empirical, 'Whether bridging institutions provide genuine coordination or extractive gatekeeping').

omega_variable(
    unified_framework_theoretical_necessity,
    'Is unified governance of present harms and existential risks theoretically necessary, or is it a contingent institutional choice?',
    'Philosophical/mathematical analysis: can a system coherently govern present-harm mitigation without reference to existential scenarios, and separately govern existential safety without addressing present power asymmetries and misalignment signals? If yes to either: unification is contingent, not necessary. If no: unification is structurally required.',
    'If contingent: the mountain perspective is a false summit (naturalizing institutional choice as law). The constraint should be reclassified based on institutional structure, not logical necessity. If necessary: the mountain is genuine; the extractiveness and suppression metrics must be reinterpreted as inherent coordination costs rather than extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unified_framework_theoretical_necessity, conceptual, 'Whether unified governance is theoretically necessary or institutionally contingent').

omega_variable(
    present_harms_existential_empirical_coupling,
    'Are present AI harms (bias, surveillance, labor displacement) empirically coupled to existential risk scenarios (superintelligence misalignment), or do they operate through independent mechanisms?',
    'Causal analysis: do failure modes in present systems (power asymmetry, value misalignment, inadequate testing, corner-case brittle behavior) directly transfer to existential scenarios? If mechanisms are independent: separate governance frameworks suffice. If coupled: integrated governance reduces blind spots.',
    'If coupled: unified framework is structurally justified; classification remains Tangled Rope with genuine coordination content. If independent: unified framework is imposed coordination that sacrifices specialization efficiency; classification shifts toward Snare or extractive Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_harms_existential_empirical_coupling, empirical, 'Empirical coupling between present AI harms and existential risk mechanisms').

omega_variable(
    kernel_reading_foreclosure_logic,
    'Does the bridge reading logically foreclose the existential-risk-only or present-harms-only readings, or do all three remain coherent within different institutional frameworks?',
    'Logical analysis: if a framework can legitimately prioritize present harms while acknowledging but deferring existential concerns, the near_term_harms_reading remains holdable. If a framework can coherently treat existential risks as primary while accepting near-term harms as instrumental consequences, the existential_risk_reading remains holdable. Foreclosure occurs only if the bridge reading''s axioms directly contradict the sibling reading''s core premises.',
    'If no foreclosure: all three readings coexist in different communities/frameworks. The relationship is coexists_with. If foreclosure: the bridge reading claims logical priority; relationship becomes forecloses. This affects how the engine models the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_logic, conceptual, 'Whether the bridge reading logically forecloses sibling readings or coexists with them').

omega_variable(
    marshalling_unified_framework_as_cover,
    'Is the unified framework mandate genuinely enabling resource redistribution toward integrated safety-ethics research, or is it functioning as rhetorical cover while resource flows remain fragmented?',
    'Empirical funding flow analysis: proportion of AI safety/ethics research funding (2020–2026) directed toward explicitly integrated projects vs. projects claiming integration in framing but remaining institutionally separated. Survey of researchers: what percentage report that unified framework mandates actually changed their research priorities vs. forced reframing of the same work?',
    'If enabling: extractiveness is legitimate coordination cost (ε should be lower). If cover: extractiveness reflects institutional capture masquerading as integration (ε should be higher, classification may shift toward Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marshalling_unified_framework_as_cover, empirical, 'Whether unified framework mandate enables real resource redistribution or functions as rhetorical cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airgov_bridge_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(airgov_bridge_tr_t3, ai_risk_governance_priority__bridge_reading, theater_ratio, 3, 0.58).
narrative_ontology:measurement(airgov_bridge_tr_t6, ai_risk_governance_priority__bridge_reading, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(airgov_bridge_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(airgov_bridge_be_t3, ai_risk_governance_priority__bridge_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(airgov_bridge_be_t6, ai_risk_governance_priority__bridge_reading, base_extractiveness, 6, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(airgov_bridge_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(airgov_bridge_su_t3, ai_risk_governance_priority__bridge_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(airgov_bridge_su_t6, ai_risk_governance_priority__bridge_reading, suppression_requirement, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, algorithmic_bias_mitigation_framework).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, superintelligence_safety_research_prioritization).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'ai_risk_governance_priority'. The existential_risk_reading and near_term_harms_reading are sibling constraints instantiating the same kernel under different axioms. All three are linked via network.affects_constraints — they are not separate domains but different institutional framings of the same governance problem. Algorithmic bias mitigation and superintelligence safety are subordinate constraints that flow from this kernel-level dispute: their prioritization and resource allocation depend on which reading of the kernel dominates governance institutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
