% ============================================================================
% CONSTRAINT STORY: bureaucratic_self_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_self_preservation, []).

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
 *   constraint_id: bureaucratic_self_preservation
 *   human_readable: The Inertial Office
 *   domain: political/administrative
 *
 * SUMMARY:
 *   Bureaucratic self-preservation is a structural constraint that emerges
 *   when an administrative body's survival incentives diverge from its
 *   original mission. An office tasked with delivering a public service or
 *   enforcing a mandate develops institutional interest in budget growth,
 *   headcount expansion, and regulatory scope expansion — not primarily
 *   because leadership is individually corrupt, but because organizational
 *   survival in competitive budget cycles rewards growth metrics. This
 *   constraint creates a hybrid mechanism: the bureau coordinates internal
 *   resource allocation and protects professional expertise (genuine
 *   coordination function), while simultaneously extracting resources from
 *   the public and mission effectiveness (extraction function). The office
 *   becomes increasingly oriented toward demonstrating its own necessity
 *   rather than achieving its stated mission. Theater increases (compliance
 *   reports, procedural legitimacy, justifications for continued existence)
 *   while actual mission focus declines. Mid-level bureaucrats experience the
 *   constraint as mixed — job security through budget growth but ethical
 *   conflict with mission. Leadership experiences it as pure coordination.
 *   The public experiences it as snare — trapped in a system optimized for
 *   its own survival, not for service delivery.
 *
 * KEY AGENTS:
 *   - Public Interest / Beneficiaries of Original Mission: Primary victim (powerless/trapped) — citizens and constituencies dependent on bureau services; bear extraction costs through resource diversion, service quality degradation, and mission drift
 *   - Bureau Leadership: Primary beneficiary (institutional/arbitrage) — executives, top administrators who benefit from budget expansion, increased authority, and larger organizational portfolios; can exit to private sector or other agencies
 *   - Mid-Level Career Bureaucrats: Secondary victim (moderate/constrained) — professional staff caught between mission orientation and organizational survival logic; face career constraints if they resist self-preservation mechanisms
 *   - Legislative Oversight: Organized actor (organized/constrained) — parliament, congressional committees, budget committees that provide funding but lack detailed information to distinguish mission drift from necessity
 *   - Civil Service System: Institutional structure (institutional/arbitrage) — seniority protections, pension rules, job classification systems that were designed to protect professional judgment but now function as inertial anchors
 *   - Analytical Observer: Civilizational view (analytical/analytical) — institutional analyst or political scientist examining the constraint from outside, able to see both coordination and extraction functions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_self_preservation, 0.58).
domain_priors:suppression_score(bureaucratic_self_preservation, 0.65).
domain_priors:theater_ratio(bureaucratic_self_preservation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_self_preservation, extractiveness, 0.58).
narrative_ontology:constraint_metric(bureaucratic_self_preservation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bureaucratic_self_preservation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_self_preservation, tangled_rope).
narrative_ontology:human_readable(bureaucratic_self_preservation, "The Inertial Office").
narrative_ontology:topic_domain(bureaucratic_self_preservation, "political/administrative").

domain_priors:requires_active_enforcement(bureaucratic_self_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_self_preservation, administrative_hierarchy).
narrative_ontology:constraint_beneficiary(bureaucratic_self_preservation, bureau_leadership).
narrative_ontology:constraint_victim(bureaucratic_self_preservation, public_interest).
narrative_ontology:constraint_victim(bureaucratic_self_preservation, mission_effectiveness).
narrative_ontology:constraint_victim(bureaucratic_self_preservation, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC INTEREST (SNARE) — Citizens and beneficiaries of the bureau's original mission are trapped. They cannot exit the system; they are subject to its rules and dependent on its services. As the bureau prioritizes self-preservation over mission, the public bears extraction costs (tax resources, degraded service quality, mission drift) with no exit option. No alternative mechanism to satisfy the original mandate. Maximum experienced extraction.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL CAREER BUREAUCRATS (TANGLED ROPE) — Face mixed incentives. The bureau's self-preservation creates job security and predictable advancement (coordination benefit). However, they are also trapped by the constraint: professional mission orientation conflicts with organizational survival logic; attempting to redirect resources toward original mission faces institutional resistance. Constrained exit — can transfer to other agencies but at career cost. Experience both coordination (job security through budget growth) and extraction (mission constraints, ethical compromise).
constraint_indexing:constraint_classification(bureaucratic_self_preservation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUREAU LEADERSHIP (ROPE) — Direct beneficiaries with arbitrage options. Bureau growth creates larger budgets, expanded leadership positions, increased prestige, and control over resource allocation. Leadership can move to private sector or other agencies if necessary (arbitrage exit). Experiences the constraint as pure coordination: organizing internal resources to maximize budget and survive budget cycles. Net extraction flows toward leadership.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGISLATIVE OVERSIGHT (TANGLED ROPE) — Organized but constrained. Legislatures benefit from bureau infrastructure and capacity to implement policy (coordination function). However, they also experience extraction: bureaus lobby for budget preservation, resist mission realignment, and create information asymmetries about actual performance. Legislators are constrained by electoral cycles and political costs of agency reorganization. Can exit through abolition but faces institutional resistance and bureaucratic expertise capture.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL SERVICE SYSTEM (PITON) — The bureaucratic self-preservation constraint is now maintained largely through institutional theater. Civil service protections, pension systems, and seniority rules were designed to create professional insulation from political pressure. They now function primarily as inertial anchors preventing mission realignment. The original coordination function (protecting professional expertise) has degraded; the system persists through theatrical compliance with procedural legitimacy rather than functional necessity. Theater ratio high; effective coordination function low.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, bureaucratic self-preservation solves a genuine coordination problem (insulating professional judgment from short-term political pressure) while simultaneously creating extraction (budget maximization divorced from mission outcome). The constraint is neither pure coordination nor pure extraction but a hybrid that has degraded over time as budget defense has consumed mission alignment. Theater has increased; coordination function has decreased; extraction mechanism has hardened.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_self_preservation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_self_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_self_preservation, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_self_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The bureau captures resources through budget growth that diverges from mission-critical spending. Over the 30-year interval, extractiveness increased from 0.28 to 0.58 as mission metrics decoupled from resource allocation. The trajectory reflects Goodhart's law at work: as budget becomes the metric of organizational health, budget maximization supplants mission execution. Suppression (0.65): Moderately high. Multiple barriers prevent mission realignment: civil service protections make workforce reduction difficult, legislative inertia resists agency abolition or merger, budget cycles create dependencies on continuous expansion, and informational asymmetry (bureau controls data on its own performance) suppresses alternative narratives. However, suppression is not total — oversight bodies retain formal authority, budget scrutiny occurs, and political pressure can force reorganization (though at high cost). Theater ratio (0.68): High and increasing. Traditional bureaucratic procedures (annual budget justifications, performance metrics, organizational reorganizations) have increasingly become performative. The bureau justifies its existence through procedural legitimacy and demonstrated activity rather than outcome achievement. Over the interval, theater increased from 0.35 to 0.68, indicating classic Piton-like degradation where form persists beyond function.
 *
 * PERSPECTIVAL GAP:
 *   Bureau leadership and public interest occupy opposite ends of the extraction flow. Leadership sees the constraint as coordination — organizing resources, securing budget, managing personnel — experiencing net negative extraction (they benefit). The public sees snare — trapped in a system that increasingly serves its own survival rather than the original mission. Mid-level bureaucrats occupy the structural middle: they experience both coordination (job security through bureau growth) and extraction (mission constraints, ethical conflicts). Legislative oversight experiences asymmetric extraction: they fund the bureau but cannot directly control its spending priorities due to informational capture and organizational expertise. The civil service system embodies the perspectival gap most clearly: designed to provide coordination (insulating professional judgment from political pressure), it now functions as inertial extraction (preventing mission realignment, protecting budget growth from scrutiny). The analytical observer sees a Tangled Rope: genuine coordination function (professional insulation) degraded but still present; extraction mechanism (budget maximization) hardened and visible. The perspectival minimum is satisfied by range from powerless to institutional, with organized and analytical perspectives capturing the institutional dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position within the extraction flow. Bureau leadership occupies low d (0.1-0.2) — beneficiaries with arbitrage exit, experiencing the constraint as coordination. Mid-level bureaucrats occupy moderate-high d (0.55-0.65) — constrained victims who also benefit from job security. Public interest occupies maximum d (0.95+) — trapped without exit, bearing extraction with no compensation. Legislative oversight occupies moderate d (0.50-0.60) — constrained by information asymmetry and institutional path dependence, neither pure beneficiary nor full victim. Civil service system occupies low d (0.1) — institutional anchor that benefits from its own persistence. The analytical observer occupies d ≈ 0.72 (observer baseline) to see the full perspectival structure. The sigmoid f(d) converts these to experienced extractiveness: beneficiaries with low d see negative/neutral chi (coordination), victims with high d see high chi (snare), and mixed actors see intermediate chi (tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that bureaucratic self-preservation is AUTHENTICALLY tangled — not pure coordination mislabeled as extraction, and not pure extraction mislabeled as coordination. The genuine coordination function is professional insulation: bureaus create expertise, institutional memory, and multi-year planning horizons that elected officials cannot. This is real coordination value. The extraction function is equally real: budget-maximization incentives diverge from mission outcome, resources flow to survival activities (justification, administrative overhead) rather than mission delivery. Over the 30-year interval, the ratio has shifted toward extraction (theater up, mission focus down), but both functions persist. The constraint is not a false snare (pretending to coordinate while purely extracting) nor a false rope (pretending to purely coordinate while also extracting). It is legitimately tangled. The mandatrophy resolves because the architecture is genuinely hybrid: abolishing the bureau loses professional insulation (real loss); maintaining status quo costs mission effectiveness (real loss). The tension cannot be resolved by reclassification — it reflects authentic structural conflict. The analytics task is to identify where in the tangled rope spectrum the constraint currently operates and whether the ratio is shifting toward unacceptable extraction (movement toward snare) or remaining within acceptable coordination-extraction balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mission_versus_survival_causality,
    'Does budget growth occur because of successful mission execution or does mission drift occur as a consequence of budget-maximization incentives?',
    'Time-series analysis of budget trajectories vs mission outcome metrics; causal inference from budget changes to subsequent mission scope changes; comparison of agencies with mission-aligned vs budget-aligned performance metrics',
    'If growth driven by mission success: constraint is partly coordination (temporary misalignment). If growth driven by lobbying/survival logic: constraint is pure extraction masked as coordination. Classification changes from Tangled Rope to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_versus_survival_causality, empirical, 'Causal direction between budget growth and mission drift').

omega_variable(
    alternative_mission_providers,
    'Could the original bureau mission be more effectively delivered by alternative institutional structures (NGOs, market mechanisms, other agencies) if the original bureau dissolved?',
    'Comparative institutional analysis; pilot programs using alternative delivery mechanisms; historical analysis of bureau reorganizations and outcomes; measurement of mission effectiveness under different institutional arrangements',
    'If yes: the constraint is pure extraction (snare) — the bureau''s existence itself is extractive. If no: constraint is legitimate coordination — the bureau''s survival reflects genuine mission necessity. If mixed: tangled rope is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_mission_providers, empirical, 'Whether alternative institutional structures could deliver the same mission').

omega_variable(
    bureaucratic_capture_depth,
    'To what degree has the original regulatory/service mission been supplanted by internal bureaucratic processes (compliance theater, procedural legitimacy, administrative overhead) that no longer serve the public mission?',
    'Audit of time allocation and budget distribution across mission vs administrative functions; measurement of service delivery quality vs administrative headcount; stakeholder perception surveys on whether bureau serves its stated mission',
    'High capture (>70% theater): classification is Piton degradation. Low capture (<30% theater): constraint is primarily Tangled Rope. Classification changes substantively.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bureaucratic_capture_depth, empirical, 'Degree of bureaucratic capture by internal processes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_self_preservation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bsp_tr_t0, bureaucratic_self_preservation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bsp_tr_t15, bureaucratic_self_preservation, theater_ratio, 15, 0.52).
narrative_ontology:measurement(bsp_tr_t30, bureaucratic_self_preservation, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(bsp_be_t0, bureaucratic_self_preservation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bsp_be_t15, bureaucratic_self_preservation, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(bsp_be_t30, bureaucratic_self_preservation, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_self_preservation, enforcement_mechanism).
narrative_ontology:affects_constraint(bureaucratic_self_preservation, regulatory_capture).
narrative_ontology:affects_constraint(bureaucratic_self_preservation, principal_agent_divergence).
narrative_ontology:affects_constraint(bureaucratic_self_preservation, budgetary_commons_tragedy).

% DUAL FORMULATION NOTE:
% Bureaucratic self-preservation can be decomposed into three structurally distinct constraints: (1) Information asymmetry between bureau and oversight (affecting observable performance reporting), (2) Budget cycle incentives that reward growth over efficiency (affecting fiscal structure), (3) Civil service inertia that makes workforce realignment costly (affecting labor market dynamics). Each has different ε and different perspectives. This story models the hybrid constraint; decomposed stories address the component mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bureaucratic_self_preservation, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
