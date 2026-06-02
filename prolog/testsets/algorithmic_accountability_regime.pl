% ============================================================================
% CONSTRAINT STORY: algorithmic_accountability_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_accountability_regime, []).

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
 *   constraint_id: algorithmic_accountability_regime
 *   human_readable: Algorithmic Accountability Regime
 *   domain: technology_governance/regulation
 *
 * SUMMARY:
 *   The algorithmic accountability regime — the ensemble of regulatory
 *   mandates (GDPR Article 22, EU AI Act, state-level algorithmic
 *   transparency laws), audit requirements, impact assessments, and
 *   compliance mechanisms — exemplifies a tangled rope constraint that
 *   combines genuine coordination function with asymmetric extraction. On the
 *   coordination side: the regime establishes shared standards for
 *   algorithmic governance, defines user rights to explanation and
 *   algorithmic decision challenge, and creates accountability mechanisms
 *   that legitimate algorithmic decision-making by establishing oversight
 *   structures. On the extraction side: platforms capture the regime by
 *   controlling audit procedures (hiring and funding third-party auditors),
 *   controlling information disclosure (aggregate statistics that obscure
 *   decision criteria), and using compliance theater to satisfy regulators
 *   while maintaining opacity at the decision level. The regime's theater
 *   ratio (0.78) reflects that transparency reports, impact assessments, and
 *   audit findings are substantially performative — they create the
 *   appearance of accountability without enabling meaningful verification by
 *   users, competitors, or regulators. The extractiveness trajectory (0.42 →
 *   0.58 over six years) shows accumulating extraction as platforms develop
 *   more sophisticated compliance theater and regulatory agencies become more
 *   dependent on platform data for verification. The suppression trajectory
 *   (0.50 → 0.65) reflects growing barriers to meaningful algorithmic
 *   challenge: users face cognitive barriers to understanding algorithmic
 *   decisions, procedural barriers to mounting appeals, and structural
 *   barriers to competing with established platforms.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture regulatory legitimacy while maintaining algorithmic opacity; achieve cost leverage over competitors through scale economies in compliance
 *   - Affected Users: Primary victim (powerless/trapped) — subject to algorithmic decisions with no transparency into criteria, no meaningful appeal mechanism, and no exit from platform infrastructure
 *   - Regulatory Agencies: Secondary beneficiary and victim (organized/constrained) — genuine coordination function (developing standards) but also structurally dependent on platforms for data access and technical expertise (extraction mechanism)
 *   - Competing Platforms: Mixed victim-beneficiary (moderate/constrained) — benefit from shared standards but bear disproportionate compliance burden relative to incumbent platforms
 *   - Algorithmic Justice Movement: Organized advocates (organized/constrained) — building alternative oversight mechanisms (community audits, distributed monitoring, interpretability tools) with sunset logic
 *   - Third-Party Auditors: Institutional actor (institutional/arbitrage) — hired by platforms to provide independent verification but structurally dependent on platform cooperation and funding
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional choices (opacity of neural networks) as natural limits of mathematics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_accountability_regime, 0.58).
domain_priors:suppression_score(algorithmic_accountability_regime, 0.65).
domain_priors:theater_ratio(algorithmic_accountability_regime, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_accountability_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_accountability_regime, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(algorithmic_accountability_regime, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_accountability_regime, tangled_rope).
narrative_ontology:human_readable(algorithmic_accountability_regime, "Algorithmic Accountability Regime").
narrative_ontology:topic_domain(algorithmic_accountability_regime, "technology_governance/regulation").

domain_priors:requires_active_enforcement(algorithmic_accountability_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_accountability_regime, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_accountability_regime, regulatory_agencies).
narrative_ontology:constraint_victim(algorithmic_accountability_regime, affected_users).
narrative_ontology:constraint_victim(algorithmic_accountability_regime, algorithmic_transparency).
narrative_ontology:constraint_victim(algorithmic_accountability_regime, competing_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED USER (SNARE) — Subject to algorithmic decisions (credit denial, hiring filter, content suppression, ad targeting) with no transparency into decision criteria and no meaningful appeal mechanism. The accountability regime produces audit reports that users cannot access, impact assessments they cannot challenge, and compliance statements they cannot verify. Exit is impossible — the platforms are infrastructural. Maximum extraction with minimal recourse.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLATFORM OPERATOR (ROPE) — The accountability regime provides coordination function: it legitimates algorithmic decision-making by creating an audit trail, satisfies regulatory requirements through compliance theater, and enables platforms to claim transparency while controlling what information regulators and users can access. The regime serves genuine coordination (establishing rules for algorithmic governance) but the platform experiences it primarily as legitimacy subsidy. Platforms benefit from the appearance of oversight without the substance of constraint.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AGENCY (TANGLED ROPE) — Genuine coordination function: developing standards for algorithmic transparency, establishing audit procedures, creating accountability mechanisms. But also significant extraction: regulatory agencies become dependent on platforms for data access, face resource constraints that prevent thorough verification, and see their mandate captured by the need to maintain platform cooperation. The regime extracts resources from agencies (staff time, expertise) while limiting their actual enforcement capacity. Mixed coordination-extraction hybrid.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPETING PLATFORM (TANGLED_ROPE) — The accountability regime creates coordination around algorithmic standards (benefits all platforms through established rules). But also functions as extraction: compliance costs are fixed, creating barriers to entry for smaller competitors. Incumbent platforms achieve cost leverage through scale and can lobby regulators to maintain high compliance bars. Smaller platforms experience the regime as both coordination (shared rules) and extraction (disproportionate burden relative to market share).
constraint_indexing:constraint_classification(algorithmic_accountability_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: AUDIT AND TRANSPARENCY APPARATUS (PITON) — Algorithmic impact assessments, third-party audits, transparency reports, and compliance statements have become largely performative. Audits are conducted by firms hired by platforms; transparency reports contain aggregate statistics that obscure decision criteria; impact assessments follow template procedures disconnected from actual outcomes. The apparatus persists through institutional inertia and theater — required by regulation but widely understood to produce limited verification value. Theater ratio of 0.78 reflects that the compliance machinery is substantially ritual.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPLEXITY VIEW (MOUNTAIN) — From a universal/civilizational perspective, algorithmic accountability faces a natural limit: the complexity of modern machine learning systems (billions of parameters, non-linear decision surfaces, emergent behaviors from training data interactions) makes perfect transparency impossible. No audit procedure can fully specify how a neural network will respond to novel inputs. This perspective frames accountability constraints as hitting a natural barrier — the mathematics of opacity. However, this naturalizes what are actually institutional choices: platforms could use more interpretable models, could grant users meaningful access to decision criteria, could implement appeals mechanisms. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ALGORITHMIC JUSTICE MOVEMENT (SCAFFOLD) — Organized advocates (AI transparency groups, civil rights organizations, academic researchers) see the accountability regime as a transitional framework with a sunset clause: they are building alternative mechanisms for algorithmic oversight that bypass platform-controlled audits. Community-based algorithmic audits, distributed monitoring networks, open-source interpretability tools, and right-to-explanation legislation represent scaffolding designed to replace the current regime. Low effective extraction because organized agents have agency and see an exit path through alternative structures. Sunset estimated at 10-15 years as alternative mechanisms mature.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_accountability_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_accountability_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_accountability_regime, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_accountability_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_accountability_regime, TR),
    TR >= 0.70.

:- end_tests(algorithmic_accountability_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regime produces substantial benefits for platforms (legitimacy, regulatory compliance) relative to the constraints imposed (disclosure requirements, audit obligations). The trajectory from 0.42 to 0.58 reflects accumulating extraction as platforms develop more sophisticated compliance theater. Suppression (0.65): High. Meaningful barriers to user challenge include: cognitive complexity of algorithmic decision criteria, procedural friction in appeal mechanisms, platforms' control of data needed for independent verification, regulatory agencies' resource constraints preventing thorough audits, and switching costs that prevent users from exiting to competitors. Theater ratio (0.78): Very high. Transparency reports contain aggregate statistics that obscure decision criteria; impact assessments follow template procedures disconnected from actual outcomes; audits are conducted by firms hired by platforms; explanations provided to users are often post-hoc narratives rather than actual model logic. The performative content has increased over time as regulatory pressure has grown — platforms have become more sophisticated at producing compliance theater rather than substantive transparency. The regime's core mechanism is legitimation: it allows platforms to claim accountability while maintaining control over verification. Users see transparency reports; regulators see audit findings; but neither can independently verify algorithmic decision criteria or challenge outcomes with meaningful effect.
 *
 * PERSPECTIVAL GAP:
 *   The gap is produced by differential access to verification and differential ability to challenge decisions. Platforms see coordination (shared rules, established procedures); users see extraction (opaque decisions, ineffective appeals). Regulators see both but are structurally dependent on platforms for verification, giving platforms extractive advantage. The gap reveals the regime's core mechanism: it legitimates algorithmic authority while limiting meaningful challenge to that authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Platform operators (beneficiary + arbitrage) experience low d, translating to negative effective extraction χ (the regime subsidizes their operations). Affected users (victim + trapped) experience high d, translating to high χ (maximum experienced extraction). Regulatory agencies (mixed + constrained) experience moderate d, reflecting both their coordination function and their dependence on platforms. The regime's extraction mechanisms are most severe for powerless agents with no alternatives (users subject to algorithmic decisions with no exit option); they diminish for organized agents with agency and alternatives (algorithmic justice movement building parallel structures).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy by distinguishing the regime's stated function (coordination: establishing legitimate algorithmic governance) from its actual effect (extraction: legitimating platforms' opacity while preventing meaningful user challenge). The regime IS a tangled rope — not a confusion between two types but a genuine hybrid. The coordination function is real: the regime establishes shared standards, defines user rights, creates accountability procedures. The extraction mechanism is also real: platforms control audit procedures, users lack meaningful appeal mechanisms, regulators depend on platforms for verification. The mandatrophy resolves not by choosing between the two but by recognizing both as structural features. The perspectival gap (platform sees rope; user sees snare) is not a classification error but a diagnostic signal that the regime's coordination function accrues to platforms while its extractive mechanisms target powerless users. The analytical observer's mountain perspective (algorithmic complexity as natural law) is a false summit — it naturalizes institutional choices that could be made differently (platforms could use more interpretable models, could implement stronger appeal mechanisms, could grant genuine data access). The regime's theater ratio (0.78) and rising extractiveness (0.42 → 0.58) indicate that as regulatory pressure intensifies, platforms develop more sophisticated compliance theater rather than substantive transparency, confirming the tangled-rope classification over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transparency_sufficiency_threshold,
    'What level of algorithmic transparency (decision criteria, training data composition, feature weights) is actually achievable without destroying the platform''s competitive advantage or the model''s performance?',
    'Comparative analysis of transparency across regulatory regimes (EU AI Act, GDPR Article 22, US state laws); documentation of specific model components platforms claim cannot be disclosed and technical justification for that claim; empirical testing of whether disclosed decision criteria match actual model behavior',
    'If achievable transparency is minimal: accountability regime is structurally theater. If substantial transparency is feasible but withheld: regime is pure extraction masquerading as coordination. If trade-offs are genuine: regime becomes constrained coordination with legitimacy justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transparency_sufficiency_threshold, empirical, 'Feasible level of algorithmic transparency without compromising model performance or competitive advantage').

omega_variable(
    audit_independence_capture,
    'Can third-party algorithmic audits remain independent when auditors are hired, funded, and contractually bound by the platforms they audit?',
    'Institutional analysis of audit firm incentives, scope of audit access, confidentiality restrictions, and publication rights; comparison of audit findings vs independent academic research on the same platforms; tracking of cases where auditors identified severe issues and subsequent platform response',
    'If capture is structural: audit apparatus is pure theater (Piton confirmed). If auditors achieve meaningful independence: accountability regime has real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_independence_capture, empirical, 'Whether third-party audits remain independent under platform funding').

omega_variable(
    user_recourse_mechanism_efficacy,
    'Do algorithmic accountability regimes provide users with effective mechanisms to challenge or appeal algorithmic decisions, or do ''right to explanation'' provisions function as information asymmetry that reinforces platform authority?',
    'Data on appeal rates, overturn rates, and user outcomes following algorithmic decision challenges; comparison of user satisfaction and actual outcome changes between platforms with strong vs weak appeal mechanisms; analysis of whether explanations provided to users match technical model structure or are post-hoc narratives',
    'If appeal mechanisms are effective: regime provides genuine coordination benefit to users. If explanations are narrative (matching user expectations rather than model logic): the regime extracts legitimacy while producing no material change in outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_recourse_mechanism_efficacy, empirical, 'Efficacy of user appeal and explanation mechanisms in algorithmic accountability').

omega_variable(
    regulatory_capacity_asymmetry,
    'Do regulatory agencies possess the technical capacity and resource allocation to perform meaningful algorithmic verification independent of platform data and cooperation?',
    'Staffing analysis of regulatory bodies overseeing algorithmic systems (AI/ML expertise, data science capacity, relative to platform engineering teams); documentation of audit procedures that do not depend on platform-provided data or access; tracking of enforcement actions that required algorithmic re-specification or meaningful constraints on platform operations',
    'If capacity is asymmetric: regulatory agencies are structurally dependent on platforms for verification (extraction mechanism). If capacity is adequate: oversight has teeth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capacity_asymmetry, empirical, 'Whether regulatory agencies have technical capacity for independent algorithmic verification').

omega_variable(
    democratic_legitimacy_of_regulatory_capture,
    'Is the regulatory agency''s dependence on platform expertise and data access a form of epistemic capture that should be classified differently than standard regulatory capture?',
    'Analysis of how platforms participate in regulatory standard-setting; documentation of regulatory decisions justified by appeal to technical necessity claims made by platforms; comparison of regulatory standards in jurisdictions with high vs low epistemic dependence on industry experts',
    'If epistemic capture is structurally inevitable: the regime is extraction masquerading as coordination by design. If alternative epistemic bases exist: regulatory independence is possible through institutional restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_of_regulatory_capture, conceptual, 'Whether platform dependence constitutes epistemic capture distinct from standard regulatory capture').

omega_variable(
    false_summit_naturalization,
    'Does framing algorithmic opacity as a natural law of machine learning mathematics serve the interests of platforms seeking to avoid meaningful transparency?',
    'Historical analysis of when ''algorithmic opacity is inevitable'' arguments were deployed vs when platforms made interpretability investments; comparison of interpretability across jurisdictions with different regulatory intensity; technical audit of whether specific opacity claims are structural necessities or engineering choices',
    'If naturalization is descriptive: complexity constraints are real and accountability regime must work within them. If naturalization is strategic: the mountain perspective is a false summit and extraction mechanisms remain unchanged by the regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether algorithmic opacity is naturalized to avoid accountability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_accountability_regime, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algoacct_tr_t0, algorithmic_accountability_regime, theater_ratio, 0, 0.55).
narrative_ontology:measurement(algoacct_tr_t3, algorithmic_accountability_regime, theater_ratio, 3, 0.68).
narrative_ontology:measurement(algoacct_tr_t6, algorithmic_accountability_regime, theater_ratio, 6, 0.78).

% Extraction over time
narrative_ontology:measurement(algoacct_be_t0, algorithmic_accountability_regime, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(algoacct_be_t3, algorithmic_accountability_regime, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(algoacct_be_t6, algorithmic_accountability_regime, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(algoacct_su_t0, algorithmic_accountability_regime, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(algoacct_su_t3, algorithmic_accountability_regime, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(algoacct_su_t6, algorithmic_accountability_regime, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_accountability_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(algorithmic_accountability_regime, platform_epistemic_authority).
narrative_ontology:affects_constraint(algorithmic_accountability_regime, ai_model_interpretability_limits).
narrative_ontology:affects_constraint(algorithmic_accountability_regime, regulatory_agency_capture).

% DUAL FORMULATION NOTE:
% The algorithmic accountability regime decomposes into three structurally distinct constraints: (1) The regime itself (this story) — the ensemble of regulatory mandates and compliance procedures, ε=0.58, Tangled Rope. (2) Platform epistemic authority (separate story) — the authority platforms derive from controlling algorithmic data and expertise, ε≈0.70, Snare. (3) Model interpretability limits (separate story) — technical constraints on neural network transparency, ε≈0.15, Mountain. The regime's extractiveness depends on platform epistemic advantage (story 2) and is downstream of technical interpretability constraints (story 3).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_accountability_regime, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
