% ============================================================================
% CONSTRAINT STORY: intelligence_as_sovereignty_transfer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intelligence_as_sovereignty_transfer, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: intelligence_as_sovereignty_transfer
 *   human_readable: Intelligence Sharing as Sovereignty Transfer in Targeted Strikes
 *   domain: international_relations/intelligence_operations/constitutional_law
 *
 * SUMMARY:
 *   The provision of high-fidelity targeting intelligence from one state to
 *   another for use in lethal operations creates a structural transfer of
 *   kill decision authority that bypasses domestic legal constraints in the
 *   provider state while concentrating legal and political risk in the
 *   executor state. This constraint operates at the intersection of
 *   international law (sovereignty and use of force), domestic constitutional
 *   law (war powers and executive authority), and intelligence operations
 *   (classification and oversight). The mechanism is not incidental but
 *   structural: when State A provides State B with real-time targeting
 *   intelligence of sufficient specificity that State B's strike timing
 *   adjusts based on that intelligence, the kill decision has materially
 *   transferred from B to A, even though the formal legal authorization
 *   remains with B. This arrangement serves genuine coordination functions
 *   (burden-sharing, collective security, operational efficiency) while
 *   simultaneously extracting war-making authority from legislative oversight
 *   in the provider state and eroding international legal constraints on use
 *   of force. The constraint has intensified over the 45-year interval as
 *   intelligence capabilities have improved (enabling higher-fidelity
 *   targeting), operational tempo has increased (more strikes enabled by
 *   shared intelligence), and legal formalism has grown (more elaborate
 *   procedures for maintaining plausible deniability). However, the rate of
 *   intensification has moderated significantly in recent years as
 *   institutional checks have reasserted themselves, international legal
 *   scrutiny has increased, and some intelligence sharing arrangements have
 *   been formalized through treaty frameworks that provide greater
 *   congressional oversight. The theater_ratio reflects the gap between
 *   formal legal separation (intelligence provision vs execution decision)
 *   and material reality (intelligence provision determines execution
 *   decision), but this gap has stabilized and begun to narrow as
 *   transparency mechanisms have improved.
 *
 * KEY AGENTS:
 *   - U.S. Executive Branch: Primary beneficiary (institutional/arbitrage) — gains operational capability to enable strikes serving U.S. interests while avoiding domestic legal constraints on direct action; can choose which operations to support and when to invoke plausible deniability
 *   - Allied Intelligence Services: Secondary beneficiary (institutional/mobile) — gain targeting capability that would be prohibitively expensive to develop independently; can decline to act on provided intelligence
 *   - Congressional War Powers: Primary victim (powerless/trapped) — constitutional authority to declare war and authorize military force is structurally bypassed when kill decisions are formally made by allied states but materially determined by U.S. intelligence provision; trapped by executive classification and operational secrecy, but retains some structural authority through appropriations
 *   - Congressional Intelligence Committees: Secondary victim (moderate/constrained) — receive classified briefings but face career and political costs for challenging executive prerogatives; experience both coordination (legitimate oversight) and extraction (authority bypassed)
 *   - Target State Sovereignty: Tertiary victim (powerless/trapped) — cannot prevent strikes enabled by intelligence it cannot detect or counter; sovereignty extracted through intelligence-enabled targeting on its territory, though strikes may serve legitimate collective security functions
 *   - International Legal Norms: Abstract victim (powerless/trapped) — legal constraints on use of force systematically eroded by intelligence sharing arrangements that bypass formal authorization requirements
 *   - International Legal Scholars: Analytical observers (moderate/constrained) — can document and critique but cannot enforce alternative norms; recognize hybrid coordination-extraction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intelligence_as_sovereignty_transfer, 0.38).
domain_priors:suppression_score(intelligence_as_sovereignty_transfer, 0.52).
domain_priors:theater_ratio(intelligence_as_sovereignty_transfer, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intelligence_as_sovereignty_transfer, extractiveness, 0.38).
narrative_ontology:constraint_metric(intelligence_as_sovereignty_transfer, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(intelligence_as_sovereignty_transfer, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intelligence_as_sovereignty_transfer, tangled_rope).
narrative_ontology:human_readable(intelligence_as_sovereignty_transfer, "Intelligence Sharing as Sovereignty Transfer in Targeted Strikes").
narrative_ontology:topic_domain(intelligence_as_sovereignty_transfer, "international_relations/intelligence_operations/constitutional_law").

domain_priors:requires_active_enforcement(intelligence_as_sovereignty_transfer).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intelligence_as_sovereignty_transfer, u_s_executive_branch).
narrative_ontology:constraint_beneficiary(intelligence_as_sovereignty_transfer, allied_intelligence_services).
narrative_ontology:constraint_victim(intelligence_as_sovereignty_transfer, congressional_war_powers).
narrative_ontology:constraint_victim(intelligence_as_sovereignty_transfer, target_state_sovereignty).
narrative_ontology:constraint_victim(intelligence_as_sovereignty_transfer, international_legal_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONGRESSIONAL WAR POWERS (TANGLED ROPE) — Trapped by executive classification and operational secrecy, but retains some structural authority through appropriations and statutory reporting requirements. Cannot exercise full constitutional oversight when the kill decision is formally made by an allied state but materially determined by U.S. intelligence provision. Experiences both coordination (legitimate intelligence sharing for collective security) and extraction (war-making authority bypassed through sovereignty transfer mechanism).
constraint_indexing:constraint_classification(intelligence_as_sovereignty_transfer, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL INTELLIGENCE COMMITTEES (TANGLED ROPE) — Constrained by classification and Gang of Eight restrictions but not entirely powerless. Receive briefings on intelligence sharing arrangements and can theoretically defund programs, but face career and political costs for challenging executive prerogatives in national security. Experience both coordination (legitimate intelligence oversight) and extraction (authority bypassed through allied execution). Moderate power with constrained exit produces mixed classification.
constraint_indexing:constraint_classification(intelligence_as_sovereignty_transfer, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. EXECUTIVE BRANCH (ROPE) — Primary beneficiary. Experiences the arrangement as pure coordination: intelligence sharing enables allied operations that serve U.S. strategic interests while avoiding domestic legal constraints on direct action. Arbitrage exit option: can choose which operations to support with intelligence, which allies to enable, and when to invoke plausible deniability. Net beneficiary with full agency.
constraint_indexing:constraint_classification(intelligence_as_sovereignty_transfer, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALLIED INTELLIGENCE SERVICES (ROPE) — Secondary beneficiary. Gain operational capability (high-fidelity targeting intelligence) that would be prohibitively expensive to develop independently. Mobile exit: can decline to act on provided intelligence or develop independent targeting capacity. Experience coordination: intelligence sharing solves collective action problem of tracking mobile targets across jurisdictions.
constraint_indexing:constraint_classification(intelligence_as_sovereignty_transfer, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL SCHOLARS (TANGLED ROPE) — Constrained by state practice and opinio juris formation. Recognize the sovereignty transfer mechanism as both coordination (intelligence sharing is legitimate state cooperation) and extraction (the mechanism systematically erodes legal constraints on use of force). Can document and critique but cannot enforce alternative norms. Generational time horizon: norms evolve slowly through state practice.
constraint_indexing:constraint_classification(intelligence_as_sovereignty_transfer, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TARGET STATE SOVEREIGNTY (TANGLED ROPE) — Trapped by intelligence asymmetry and military capability gap. Cannot prevent strikes enabled by intelligence it cannot detect or counter. The sovereignty transfer mechanism extracts decision authority, but the strikes may serve legitimate collective security functions (counterterrorism coordination). Experiences both the coordination aspect (international cooperation against shared threats) and extraction (strikes occur on its territory based on decisions made in foreign capitals).
constraint_indexing:constraint_classification(intelligence_as_sovereignty_transfer, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes the arrangement as hybrid coordination-extraction. Genuine coordination function: intelligence sharing enables collective security operations and burden-sharing among allies. Asymmetric extraction: the mechanism systematically transfers kill decision authority from provider to executor, bypassing domestic legal constraints in the provider state while concentrating legal and political risk in the executor state. The sovereignty transfer is structural, not incidental. Analytical perspective required for tangled rope detection.
constraint_indexing:constraint_classification(intelligence_as_sovereignty_transfer, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intelligence_as_sovereignty_transfer_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intelligence_as_sovereignty_transfer, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intelligence_as_sovereignty_transfer, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(intelligence_as_sovereignty_transfer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The sovereignty transfer mechanism extracts war-making authority from congressional oversight and erodes international legal constraints, but the extraction is not total — oversight mechanisms remain (Gang of Eight briefings, appropriations power, statutory reporting requirements) and legal constraints bind (domestic prohibitions on assassination, international humanitarian law). The value reflects that the arrangement systematically bypasses constitutional and international legal checks while maintaining formal compliance and some residual accountability. Recent formalization of some intelligence sharing arrangements through treaty frameworks has slightly reduced extractiveness by providing greater congressional oversight. Suppression (0.52): Moderate. Significant barriers to challenging the arrangement include executive classification (prevents public debate), operational secrecy (prevents congressional oversight), alliance politics (challenging intelligence sharing strains allied relationships), and national security framing (dissent characterized as undermining collective defense). Congressional committees face career risk for challenging executive prerogatives. Target states lack capacity to resist. But suppression is not total — legislative constraints exist (appropriations, statutory reporting requirements), international legal mechanisms bind (ICC jurisdiction, universal jurisdiction for war crimes), and some public debate occurs through declassified reporting. Recent transparency initiatives and judicial review of some intelligence sharing arrangements have slightly reduced suppression. Theater ratio (0.48): Moderate. The formal legal separation between intelligence provision and execution decision is substantially performative when intelligence specificity reaches the level of real-time targeting data. The elaborate procedures for maintaining plausible deniability (liaison channels, compartmented briefings, formal authorization by executor state) serve partly to insulate the provider from accountability. But the theater is not total — the formal separation does provide real legal insulation (provider state is not directly liable for executor state's actions under international law) and real political insulation (domestic opposition focuses on executor state). The theater has increased over the interval but has stabilized and begun to decline as institutional checks have reasserted themselves and some arrangements have been formalized through treaty frameworks.
 *
 * PERSPECTIVAL GAP:
 *   The executive branch sees pure coordination (Rope): intelligence sharing solves the collective action problem of tracking mobile threats across jurisdictions and enables burden-sharing among allies. Allied intelligence services also see coordination (Rope): they gain capability they could not afford to develop independently. Congressional war powers sees mixed coordination and extraction (Tangled Rope): intelligence sharing serves legitimate collective security functions but systematically bypasses constitutional checks. Congressional intelligence committees see mixed coordination and extraction (Tangled Rope): they receive briefings (coordination) but cannot effectively exercise oversight when the kill decision is formally made by an allied state (extraction). Target state sovereignty sees mixed coordination and extraction (Tangled Rope): strikes may serve legitimate collective security functions but occur based on decisions made in foreign capitals. International legal scholars see the hybrid structure (Tangled Rope): genuine coordination function (collective security) coexists with systematic erosion of legal constraints (extraction). The analytical observer recognizes that all perspectives are structurally valid readings of the same arrangement — the sovereignty transfer mechanism simultaneously coordinates allied operations and extracts decision authority from domestic and international legal constraints, but the extraction is not total.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. executive branch is the primary beneficiary: it gains operational capability to enable strikes serving U.S. strategic interests while avoiding domestic legal constraints on direct action (no congressional authorization required, no U.S. forces in harm's way, plausible deniability for civilian casualties). Arbitrage exit option: can choose which operations to support with intelligence, which allies to enable, and when to invoke plausible deniability. This produces low directionality (d ≈ 0.10) and negative effective extraction (the constraint subsidizes this agent). Allied intelligence services are secondary beneficiaries: they gain targeting capability that would be prohibitively expensive to develop independently. Mobile exit: can decline to act on provided intelligence or develop independent capacity. This produces low directionality (d ≈ 0.25) and low effective extraction. Congressional war powers is the primary victim: constitutional authority is structurally bypassed, though not entirely eliminated (trapped by classification and operational secrecy but retains appropriations power). This produces high directionality (d ≈ 0.85) and high effective extraction, but not maximum. Congressional intelligence committees are secondary victims: they experience both coordination (legitimate oversight briefings) and extraction (authority bypassed through allied execution), with constrained exit (can theoretically defund programs but face career and political costs). This produces moderate-high directionality (d ≈ 0.65) and moderate effective extraction, yielding tangled_rope classification. Target state sovereignty is a tertiary victim: trapped by intelligence asymmetry and military capability gap, but may benefit from collective security coordination. This produces high directionality (d ≈ 0.88) and high effective extraction. International legal scholars are analytical observers with constrained exit (can document and critique but cannot enforce alternative norms), producing moderate directionality (d ≈ 0.72) and the analytical perspective required for tangled_rope detection.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled_rope classification requires recognizing BOTH genuine coordination function AND asymmetric extraction in the same structural arrangement. The executive branch's rope perspective is not false — intelligence sharing does solve real coordination problems (burden-sharing, operational efficiency, collective security). But the congressional war powers tangled_rope perspective is also not false — the mechanism does systematically bypass constitutional checks on war-making authority while retaining some residual oversight capacity. The tangled_rope classification from the analytical perspective captures the structural reality: the arrangement coordinates allied operations (rope function) while extracting decision authority from legislative oversight (snare function). The mandatrophy error would be to classify this as pure coordination (ignoring the constitutional bypass) or pure extraction (ignoring the genuine collective security function). The perspectival gap between beneficiary (rope) and victims (tangled_rope) is the diagnostic signature that prevents mislabeling. The sovereignty transfer is not incidental but structural: when intelligence specificity reaches the level of real-time targeting data, the kill decision materially transfers from executor to provider, regardless of formal legal authorization. This is the extraction mechanism. But the intelligence sharing also enables operations that neither state could conduct as effectively alone. This is the coordination function. Both are real. The tangled_rope classification captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_determination_threshold,
    'At what level of intelligence specificity does provision constitute material determination of the kill decision rather than mere enabling?',
    'Legal analysis of causation standards; comparison with domestic law standards for accomplice liability and conspiracy; empirical analysis of strike timing correlation with intelligence provision',
    'If threshold is low (general threat assessment): most intelligence sharing remains coordination. If threshold is high (real-time targeting data): sovereignty transfer mechanism applies broadly, and many arrangements reclassify from rope to tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_determination_threshold, conceptual, 'Threshold for intelligence specificity constituting material determination').

omega_variable(
    plausible_deniability_effectiveness,
    'Does the formal separation of intelligence provision from execution decision actually insulate the provider from legal and political accountability, or is it purely theatrical?',
    'Historical analysis of accountability outcomes when intelligence-enabled strikes produce civilian casualties or political blowback; comparison of domestic political consequences for provider vs executor states',
    'If effective: theater_ratio should be lower (the separation has real legal function). If purely theatrical: theater_ratio should be higher (the separation is performative compliance with constraints that no longer bind).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(plausible_deniability_effectiveness, empirical, 'Whether formal separation provides real legal insulation').

omega_variable(
    allied_autonomy_counterfactual,
    'Would the executor state conduct the same strikes with the same timing and targeting in the absence of provider intelligence, or does intelligence provision materially alter the decision?',
    'Counterfactual analysis of executor state''s independent intelligence capacity and historical strike patterns; comparison of strike frequency and targeting precision before and after intelligence sharing arrangements',
    'If executor would strike anyway: intelligence sharing is pure coordination (rope from more perspectives). If intelligence provision materially alters strike decisions: sovereignty transfer is real (tangled_rope or snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_autonomy_counterfactual, empirical, 'Counterfactual autonomy of executor state strike decisions').

omega_variable(
    congressional_notification_sufficiency,
    'Does Gang of Eight notification constitute meaningful congressional oversight, or does classification prevent effective exercise of war powers check?',
    'Analysis of historical Gang of Eight briefings on intelligence sharing; assessment of whether classified briefings enabled congressional action (funding restrictions, legislative constraints) or merely provided post-hoc notification',
    'If notification enables oversight: congressional perspective shifts from snare toward tangled_rope (constrained but not trapped). If notification is purely informational: snare classification confirmed (trapped by classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_notification_sufficiency, empirical, 'Whether classified notification enables meaningful oversight').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intelligence_as_sovereignty_transfer, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intel_sov_theater_early_cold_war, intelligence_as_sovereignty_transfer, theater_ratio, 0, 0.32).
narrative_ontology:measurement(intel_sov_theater_post_cold_war, intelligence_as_sovereignty_transfer, theater_ratio, 15, 0.38).
narrative_ontology:measurement(intel_sov_theater_post_9_11, intelligence_as_sovereignty_transfer, theater_ratio, 25, 0.44).
narrative_ontology:measurement(intel_sov_theater_drone_era, intelligence_as_sovereignty_transfer, theater_ratio, 35, 0.46).
narrative_ontology:measurement(intel_sov_theater_current, intelligence_as_sovereignty_transfer, theater_ratio, 45, 0.48).

% Extraction over time
narrative_ontology:measurement(intel_sov_extract_early_cold_war, intelligence_as_sovereignty_transfer, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(intel_sov_extract_post_cold_war, intelligence_as_sovereignty_transfer, base_extractiveness, 15, 0.29).
narrative_ontology:measurement(intel_sov_extract_post_9_11, intelligence_as_sovereignty_transfer, base_extractiveness, 25, 0.34).
narrative_ontology:measurement(intel_sov_extract_drone_era, intelligence_as_sovereignty_transfer, base_extractiveness, 35, 0.36).
narrative_ontology:measurement(intel_sov_extract_current, intelligence_as_sovereignty_transfer, base_extractiveness, 45, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intelligence_as_sovereignty_transfer, enforcement_mechanism).
narrative_ontology:affects_constraint(intelligence_as_sovereignty_transfer, executive_war_powers_expansion).
narrative_ontology:affects_constraint(intelligence_as_sovereignty_transfer, classification_as_accountability_shield).
narrative_ontology:affects_constraint(intelligence_as_sovereignty_transfer, alliance_burden_sharing).

% DUAL FORMULATION NOTE:
% This constraint is part of a family of executive authority expansion mechanisms in national security law. Related constraints include: (1) executive_war_powers_expansion (the broader trend of executive branch claiming inherent authority for military operations without congressional authorization), (2) classification_as_accountability_shield (the use of secrecy to prevent oversight), and (3) alliance_burden_sharing (the coordination benefits of intelligence sharing among allies). Each has its own extractiveness value reflecting different structural mechanisms, but all are linked through the common pattern of executive authority expanding through operational practice that bypasses formal legal constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intelligence_as_sovereignty_transfer, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
