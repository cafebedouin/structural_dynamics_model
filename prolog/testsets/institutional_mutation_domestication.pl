% ============================================================================
% CONSTRAINT STORY: institutional_mutation_domestication
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_mutation_domestication, []).

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
 *   constraint_id: institutional_mutation_domestication
 *   human_readable: The Jedi Bureaucratic Capture
 *   domain: political/social
 *
 * SUMMARY:
 *   The Jedi represent a high-agency institutional mutation — an independent
 *   epistemic and martial tradition with its own organizational logic,
 *   doctrine, and decision-making authority. The bureaucratic capture
 *   describes the process by which this mutation is integrated into the
 *   stable structures of the Galactic Republic. This integration exhibits the
 *   signature pattern of institutional domestication: the republic gains
 *   access to Jedi capabilities (military, diplomatic, crisis response) while
 *   the Jedi gain resources, legitimacy, and political influence. However,
 *   the underlying asymmetry is structural — the republic can exit or
 *   redefine the relationship at any time, while the Jedi cannot abandon
 *   institutional integration without ceasing to function as a political
 *   force. Over 500 years, the theater ratio has increased from 0.35 to 0.64,
 *   indicating that procedural compliance with senate oversight has become
 *   increasingly performative while genuine Force-tradition autonomy has
 *   declined. The extractiveness metric shows corresponding increase from
 *   0.28 to 0.52, reflecting accumulating constraints on independent Jedi
 *   action, temple doctrine modifications to accommodate state requirements,
 *   and increasing dependency on republic funding and political protection.
 *
 * KEY AGENTS:
 *   - Galactic Republic Executive: Primary beneficiary (institutional/arbitrage) — gains military power, diplomatic capacity, crisis response capability with minimal extraction cost
 *   - Senate Coordination Apparatus: Primary beneficiary (institutional/arbitrage) — uses Jedi as policy instrument, maintains legislative control, can redefine mandate at will
 *   - Jedi Council: Organized secondary actor (organized/constrained) — retains some agency through internal doctrine and council autonomy but faces increasing reporting requirements and senate oversight
 *   - Individual Jedi Padawan: Primary victim (moderate/trapped) — no meaningful exit from institutional system; identity, career, and economic survival entirely dependent on temple integration
 *   - Force Tradition (Abstract): Primary victim (powerless/trapped) — ancient epistemic and martial tradition cannot preserve autonomy while functioning as state apparatus; institutional requirements extract doctrinal independence
 *   - Separatist/Resistance Movement: Counter-actor (organized/constrained) — represents potential exit path through institutional schism or alternative force-user structure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — observes increasing performative content masking underlying extraction and potential structural instability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_mutation_domestication, 0.52).
domain_priors:suppression_score(institutional_mutation_domestication, 0.58).
domain_priors:theater_ratio(institutional_mutation_domestication, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_mutation_domestication, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_mutation_domestication, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(institutional_mutation_domestication, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_mutation_domestication, tangled_rope).
narrative_ontology:human_readable(institutional_mutation_domestication, "The Jedi Bureaucratic Capture").
narrative_ontology:topic_domain(institutional_mutation_domestication, "political/social").

domain_priors:requires_active_enforcement(institutional_mutation_domestication).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_mutation_domestication, galactic_republic_executive).
narrative_ontology:constraint_beneficiary(institutional_mutation_domestication, senate_coordination_apparatus).
narrative_ontology:constraint_victim(institutional_mutation_domestication, jedi_operational_autonomy).
narrative_ontology:constraint_victim(institutional_mutation_domestication, force_tradition_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FORCE TRADITION (SNARE) — The ancient Force tradition cannot exit the bureaucratic integration without ceasing to function as a political actor. Trapped within institutional requirements (reporting structures, senate oversight, temple regulations), the tradition's original epistemic autonomy is extracted through mandatory coordination with state apparatus. Maximum experienced extraction — abstract institutional knowledge cannot organize for its own preservation.
constraint_indexing:constraint_classification(institutional_mutation_domestication, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE INDIVIDUAL JEDI PADAWAN (SNARE) — Young force-users have no meaningful exit from the temple system. Birth into the order, childhood training, identity formation within the institution, and career structure all funnel toward perpetual institutional service. High extraction of individual agency — padawans cannot exit without abandoning identity, social position, and economic survival.
constraint_indexing:constraint_classification(institutional_mutation_domestication, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE JEDI COUNCIL (TANGLED ROPE) — Organized institutional leadership experiences both extraction and coordination benefit. The council gains resources, legitimacy, and political influence through republic recognition (coordination function). Simultaneously, the council is constrained by reporting requirements, senate oversight, and inability to conduct independent military or diplomatic operations (extraction). The council retains some agency — it can negotiate terms, refuse missions (with political cost), and maintain internal doctrine. Moderate effective extraction.
constraint_indexing:constraint_classification(institutional_mutation_domestication, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE GALACTIC REPUBLIC EXECUTIVE (ROPE) — The republic's executive apparatus experiences the Jedi primarily as a coordination mechanism: a specialized agency for conflict resolution, diplomatic troubleshooting, and crisis response. The republic captures substantial benefits (military advantage, political leverage, intelligence capacity) while experiencing minimal extraction costs. The republic has full arbitrage options — it can redirect resources, redefine Jedi mandate, or marginalize the order if coordination costs exceed benefits. Net beneficiary — extraction runs toward the republic.
constraint_indexing:constraint_classification(institutional_mutation_domestication, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE SENATE COORDINATION APPARATUS (ROPE) — The legislative branch uses the Jedi as a coordination instrument: they provide military power projection without full senate control, enable diplomatic missions without direct state deployment, and serve as a unified force for planetary-scale problems. The senate experiences low extraction costs — it sets broad mandates and receives compliance without requiring continuous oversight. Full arbitrage: the senate can modify the Jedi mandate through legislation, defund operations, or shift diplomatic burdens to other agencies.
constraint_indexing:constraint_classification(institutional_mutation_domestication, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (PITON) — From a long-term civilizational view, the Jedi bureaucratic integration is largely performative: the republic frames the Jedi as a coordinated service agency while the Jedi maintain internal doctrine that contradicts state interests (the Rule of Two, individual Force mastery, non-attachment). The theatre ratio (0.64) reflects that much of the institutional interaction is procedural compliance theater rather than genuine coordination. The analytical observer sees institutional inertia: the Jedi structure persists through historical habit and mutual benefit despite increasing functional misalignment.
constraint_indexing:constraint_classification(institutional_mutation_domestication, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: THE RESISTANCE FORCES / SEPARATISM MOVEMENT (SCAFFOLD) — From the perspective of those resisting bureaucratic domestication, the constraint is temporary and can be dissolved through schism or revolution. The Separatist movement views Jedi integration as an extractive arrangement that can be terminated by building an alternative force-user structure (Sith organization, independent force academies). This perspective sees a sunset clause: the constraint lasts only so long as institutional loyalty is maintained through career incentives and identity formation. If alternative pathways exist (Sith recruitment, underground academies), the extraction mechanism loses force. Low effective extraction because resistance actors see an exit path.
constraint_indexing:constraint_classification(institutional_mutation_domestication, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_mutation_domestication_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_mutation_domestication, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_mutation_domestication, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_mutation_domestication, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_mutation_domestication, TR),
    TR >= 0.70.

:- end_tests(institutional_mutation_domestication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high, reflecting the asymmetric structural relationship between the republic and the Jedi. The republic captures substantial benefits (military advantage, diplomatic capability, crisis response) while maintaining full exit options (can defund, redefine mandate, or marginalize the order). The Jedi gain resources and political legitimacy but lose operational autonomy and doctrinal independence. The extraction is not as severe as pure Snare (0.66+) because the Jedi retain some agency through council autonomy and internal doctrine. However, extraction exceeds pure coordination threshold (0.45) because the republic can unilaterally modify terms and the Jedi have no equivalent exit capacity. Suppression (0.58): Moderate-high. Significant institutional barriers prevent independent Jedi action: reporting requirements, senate oversight, legal restrictions on military operations, funding dependency, and career structure that makes exit economically impossible for individual practitioners. However, suppression is not total — the Jedi Council retains significant operational discretion, some missions are genuinely self-directed, and internal force training continues. Theater ratio (0.64): Increasing over the interval. At t=0 (early republic integration), the arrangement was primarily functional coordination — genuine collaboration on shared problems. By t=500, substantial theater has accumulated: senate oversight is largely procedural compliance rather than genuine policy control; Jedi reports are filtered through institutional narrative rather than authentic assessment; mission briefings follow hierarchical protocol rather than collaborative deliberation. The theater increase reflects Goodhart drift — institutional metrics (compliance measures, reporting procedures, oversight metrics) have replaced genuine coordination indicators.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across power levels and exit options. The republic executive and senate see pure coordination (Rope) — they are gaining institutional capability with manageable oversight costs. The Jedi Council sees mixed coordination and extraction (Tangled Rope) — they gain resources and legitimacy but lose autonomy, and this mix creates ongoing tension. Individual padawans see pure extraction (Snare) — they have no exit option and maximum exposure to institutional constraints. The Force tradition itself (treated as a victim aggregate) experiences extraction as institutional erosion — the ancient doctrine cannot preserve independence while serving state interests. The separatist/resistance movement sees a temporary extractive arrangement (Scaffold) with a structural sunset — if alternative force-user pathways can be established, the extraction mechanism dissolves. The analytical observer sees institutional dysfunction theater (Piton) — the procedural compliance mechanisms are increasingly performative, masking underlying structural misalignment between Force-tradition autonomy and state apparatus integration.
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline derives directionality (d) for each agent from power level, exit options, and beneficiary/victim status. The republic (institutional/arbitrage/beneficiary) derives d ≈ 0.0-0.15 (full beneficiary), producing negative effective extraction from republic perspective. The Jedi Council (organized/constrained/victim) derives d ≈ 0.55-0.65 (mixed victim-beneficiary), producing moderate extraction. Individual padawans (moderate/trapped/victim) derive d ≈ 0.85-0.95 (nearly full target), producing high experienced extraction. The Force tradition (powerless/trapped/victim) derives d ≈ 1.0 (full target), producing maximum extraction. The separatist movement (organized/constrained with alternative-exit pathway) derives d ≈ 0.45-0.50 (symmetric), producing moderate extraction that decreases as alternative pathways mature.
 *
 * MANDATROPHY ANALYSIS:
 *   INSTITUTIONAL MUTATION DOMESTICATION: This constraint resolves potential mandatrophy through the network of victim declarations and asymmetric power distribution. The temptation to classify as pure Rope (coordination with minimal extraction) is rejected because: (1) the republic has unilateral exit capacity; (2) the Jedi have no equivalent exit capacity; (3) victims are explicitly declared (Force tradition integrity, Jedi operational autonomy); (4) the theater ratio has increased to 0.64, indicating procedural compliance theater replacing genuine coordination. The classification as Tangled Rope (rather than pure Snare at the institutional level) is justified by: (1) beneficiary declarations (republic executive, senate coordination apparatus gain genuine coordination benefits); (2) the Jedi Council retains meaningful agency through internal doctrine and council autonomy; (3) the constraint is not solely extractive — it enables Jedi political influence and resources. The piton perspective (analytical/civilizational) identifies the increasing performance theater as institutional inertia — the arrangement persists through mutual benefit and historical habit despite functional misalignment. The scaffold perspective (resistance/separatism) demonstrates that the constraint is not immutable — alternative force-user structures could provide an exit path. This multi-perspectival resolution prevents both false naturalization (as pure coordination) and false totality (as pure extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    force_tradition_degradation_threshold,
    'At what point does bureaucratic integration fundamentally alter the Force tradition such that it becomes institutionally subordinate rather than independently ethical?',
    'Historical analysis of temple doctrine changes correlated with legislative mandates; longitudinal tracking of Force philosophy evolution; comparison with pre-republic force traditions',
    'If threshold crossed early: Jedi are already structurally compromised (extraction > 0.65, reclassifies to Snare from council perspective). If threshold remains distant: current integration is stable coordination (extraction ≤ 0.45).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(force_tradition_degradation_threshold, conceptual, 'Threshold for institutional subordination of Force tradition').

omega_variable(
    mission_autonomy_vs_mandate_compliance,
    'Can the Jedi Council genuinely refuse senate-directed missions, or does political/economic dependence make refusal impossible?',
    'Historical case analysis of mission refusals; correlation between council autonomy and republic resource allocation; examination of coercive mechanisms implicit in budget/temple maintenance',
    'If genuine refusal possible: Council has meaningful agency (Tangled Rope stable). If refusal impossible: Council is captured (reclassifies to Snare from Council perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mission_autonomy_vs_mandate_compliance, empirical, 'Whether Jedi Council can autonomously refuse senate missions').

omega_variable(
    sith_emergence_causation,
    'Does Sith ideology emerge as a reaction to Jedi bureaucratic domestication (parasitic response to extracted autonomy) or as independent philosophical opposition?',
    'Ideological genealogy analysis; comparison of Sith doctrines with pre-integration Force traditions; timeline correlation between bureaucratic pressure and Sith recruitment expansion',
    'If parasitic response: bureaucratic capture is generating its own structural opposition, validating Snare classification from Force-tradition perspective. If independent opposition: constraint is not creating instability (remains Tangled Rope stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sith_emergence_causation, empirical, 'Whether Sith emergence is reaction to bureaucratic domestication').

omega_variable(
    republic_state_capacity_dependence,
    'Is the republic''s military/diplomatic capacity genuinely dependent on Jedi integration, or could equivalent functions be provided by non-force-user institutions?',
    'Comparative institutional analysis; cost-benefit modeling of Jedi vs alternative crisis-response structures; analysis of historical periods with/without Jedi service',
    'If genuinely dependent: republic cannot exit (asymmetric extraction favors republic). If alternatives available: republic maintains full arbitrage (Rope from republic perspective remains stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(republic_state_capacity_dependence, empirical, 'Whether republic military/diplomatic capacity is Jedi-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_mutation_domestication, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jedi_tr_t0, institutional_mutation_domestication, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jedi_tr_t250, institutional_mutation_domestication, theater_ratio, 250, 0.5).
narrative_ontology:measurement(jedi_tr_t500, institutional_mutation_domestication, theater_ratio, 500, 0.64).

% Extraction over time
narrative_ontology:measurement(jedi_be_t0, institutional_mutation_domestication, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(jedi_be_t250, institutional_mutation_domestication, base_extractiveness, 250, 0.4).
narrative_ontology:measurement(jedi_be_t500, institutional_mutation_domestication, base_extractiveness, 500, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_mutation_domestication, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_mutation_domestication, separatist_military_mobilization).
narrative_ontology:affects_constraint(institutional_mutation_domestication, senate_legislative_capture).
narrative_ontology:affects_constraint(institutional_mutation_domestication, force_doctrine_erosion).

% DUAL FORMULATION NOTE:
% This constraint represents the institutional integration of a high-agency mutation. Upstream constraints include republic state capacity expansion (enabling integration) and force-tradition philosophical drift (enabling compromise). Downstream constraints include separatist emergence as parasitic response to extracted autonomy and incremental doctrine erosion through repeated legislative compromise. The constraint family demonstrates how institutional capture operates at multiple levels: at the organizational level (Jedi Council), the individual level (padawans), and the epistemic level (Force tradition integrity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_mutation_domestication, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
