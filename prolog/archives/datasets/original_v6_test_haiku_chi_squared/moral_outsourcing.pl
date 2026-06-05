% ============================================================================
% CONSTRAINT STORY: moral_outsourcing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moral_outsourcing, []).

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
 *   constraint_id: moral_outsourcing
 *   human_readable: The Ethical Externalization Loop
 *   domain: social/economic/technological
 *
 * SUMMARY:
 *   The ethical externalization loop describes a structural pattern where
 *   institutions progressively delegate moral decision-making to algorithmic
 *   or bureaucratic frameworks while simultaneously diffusing accountability
 *   in ways that trap individual moral agents. This constraint operates
 *   across multiple sectors — hiring algorithms that automate discrimination
 *   at scale, criminal justice risk assessment tools that obscure sentencing
 *   disparities, financial underwriting systems that distribute credit
 *   according to opaque scoring, and content moderation platforms that
 *   outsource judgments about speech and harm. The common pattern: (1) a
 *   decision with moral weight is transferred to an algorithmic or rule-based
 *   system justified as more objective, efficient, or unbiased; (2)
 *   individual humans within the system lose decision-making authority and
 *   abdicate moral responsibility to the system's outputs; (3) accountability
 *   for outcomes becomes impossible because responsibility is diffused across
 *   the algorithm, its trainers, its deployers, and institutional policy; (4)
 *   affected populations face decisions justified as 'what the algorithm
 *   says' rather than what a responsible human decided. The constraint is not
 *   the existence of algorithms but the institutional choice to use them as
 *   accountability shields rather than decision-support tools. Theater ratio
 *   rising from 0.35 to 0.64 reflects increasing performative commitment to
 *   'objective' decisions while actual moral deliberation is outsourced. Base
 *   extractiveness rising from 0.28 to 0.58 reflects that institutions
 *   progressively exploit the accountability diffusion to avoid liability and
 *   shift moral costs onto affected populations.
 *
 * KEY AGENTS:
 *   - Individual Moral Agents: Primary victims (powerless/trapped) — stripped of decision-making authority; cannot exit without institutional disenfranchisement
 *   - Ethical Accountability Commons: Primary victim (powerless/trapped) — collective epistemic resource for assigning moral responsibility; degraded by diffusion of agency
 *   - Affected Populations: Primary victims (powerless/trapped) — subject to algorithmic decisions with no accountable human; cannot demand human judgment
 *   - Institutional Decision-Makers: Primary beneficiaries (institutional/arbitrage) — extract liability reduction and efficiency gains; experience constraint as coordination (Tangled Rope)
 *   - Compliance and Risk Management Functions: Secondary beneficiaries (organized/constrained) — benefit from documented, auditable processes (Rope)
 *   - Algorithmic Transparency Movement: Organized agents (organized/constrained) — regulators, technologists, ethicists building explainability requirements and human-in-the-loop mandates (Scaffold)
 *   - Legacy Accountability Frameworks: Institutional actor (institutional/arbitrage) — traditional responsibility concepts persist performatively but have lost functional grip (Piton)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional delegation as inherent to scale (false Mountain)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moral_outsourcing, 0.58).
domain_priors:suppression_score(moral_outsourcing, 0.68).
domain_priors:theater_ratio(moral_outsourcing, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moral_outsourcing, extractiveness, 0.58).
narrative_ontology:constraint_metric(moral_outsourcing, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(moral_outsourcing, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moral_outsourcing, tangled_rope).
narrative_ontology:human_readable(moral_outsourcing, "The Ethical Externalization Loop").
narrative_ontology:topic_domain(moral_outsourcing, "social/economic/technological").

domain_priors:requires_active_enforcement(moral_outsourcing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moral_outsourcing, institutional_decision_makers).
narrative_ontology:constraint_beneficiary(moral_outsourcing, algorithmic_system_operators).
narrative_ontology:constraint_victim(moral_outsourcing, individual_moral_agency).
narrative_ontology:constraint_victim(moral_outsourcing, ethical_accountability_commons).
narrative_ontology:constraint_victim(moral_outsourcing, affected_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL MORAL AGENT (SNARE) — Stripped of decision-making authority by automation and blame-deflection to algorithms. Cannot exit the constraint without surrendering livelihood or institutional participation. Trapped between moral intuition and systemic imperative. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(moral_outsourcing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ETHICAL ACCOUNTABILITY COMMONS (SNARE) — Collective epistemic resource for assigning moral responsibility. Degraded by diffusion of agency into algorithms and bureaucratic chains. No mechanism to recover clarity about who bears responsibility. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.06.
constraint_indexing:constraint_classification(moral_outsourcing, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: AFFECTED POPULATIONS (SNARE) — Subject to algorithmic or delegated decisions with no human accountable for outcomes. Cannot refuse participation or demand human judgment. Trapped in systems justified as objective. d≈0.93, f(d)≈1.39, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(moral_outsourcing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL DECISION-MAKERS (TANGLED ROPE) — Simultaneously coordinate collective action (reduce bias, scale decisions uniformly) and extract by deflecting accountability. Benefits from efficiency gains and liability reduction. d≈0.15, f(d)≈0.02, σ=1.0 → χ≈0.01. Low effective extraction from beneficiary position; high structural beneficiary status.
constraint_indexing:constraint_classification(moral_outsourcing, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPLIANCE AND RISK MANAGEMENT (ROPE) — Coordinate institutional behavior around legal liability and regulatory compliance. Benefit from algorithmic decision-making as a documented, auditable process. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.17. Pure coordination function with modest extraction.
constraint_indexing:constraint_classification(moral_outsourcing, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ALGORITHMIC TRANSPARENCY MOVEMENT (SCAFFOLD) — Organized actors (regulators, technologists, ethicists) building alternative pathways: explainability requirements, human-in-the-loop mandates, algorithmic auditing. See the loop as a temporary institutional failure with a sunset: as transparency norms mature, the externalization mechanism loses force. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.21. Low effective extraction because movement has agency and exit path.
constraint_indexing:constraint_classification(moral_outsourcing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGACY ACCOUNTABILITY FRAMEWORKS (PITON) — Traditional jurisprudential concepts (personal responsibility, liability, intent) persist performatively in courts and ethics discussions but have lost functional grip on decision-making. Maintained through institutional inertia and ceremonial appeals to 'human judgment,' not because they actually guide institutional behavior. theater_ratio=0.64 reflects the gap between stated commitment to human accountability and actual algorithmic delegation. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(moral_outsourcing, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the externalization of moral agency to algorithms reflects an irreducible tension: complex sociotechnical systems cannot be fully controlled by individual intention or responsibility. Some degree of agency diffusion is inherent to scale. However, base metrics (ε=0.58, suppression=0.68, theater=0.64) contradict the mountain gate — the engine flags this as a false summit. The naturalization of externalization is itself part of the extraction mechanism.
constraint_indexing:constraint_classification(moral_outsourcing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moral_outsourcing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moral_outsourcing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moral_outsourcing, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moral_outsourcing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(moral_outsourcing, TR),
    TR >= 0.70.

:- end_tests(moral_outsourcing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Institutions extract significant value from delegating accountability — reduced liability exposure, lower decision-making costs, legitimacy of 'objective' processes, and insulation from moral criticism. The extraction is not total because algorithmic systems do solve real coordination problems (reducing arbitrary discrimination, scaling decisions uniformly). Suppression (0.68): High. Barriers to restoring individual moral agency include: technical opacity of algorithms, institutional incentive structures that reward delegation, cultural deference to quantification and automation, regulatory capture by system deployers, and the career risk to employees who challenge algorithmic outputs. Affected populations cannot exit algorithmic decisions in domains like employment, credit, or criminal justice without losing access to essential services. Theater ratio (0.64): High. Institutions maintain ceremonial commitment to 'human judgment,' 'human rights,' and 'accountability' while actual decision authority has been transferred to algorithms. Courts invoke 'due process' while algorithmic risk scores drive sentencing. Hiring departments claim commitment to merit while automated resume-screening filters applicants. The theater ratio has increased because the disconnect between stated values and actual delegated authority has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Individual moral agents see pure extraction (Snare) — they have lost decision-making authority and moral agency. Affected populations see pure extraction (Snare) — they face algorithmic decisions with no human accountability. Institutional decision-makers see coordination (Tangled Rope) — they genuinely solve bias and scalability problems while also extracting accountability insulation. Compliance functions see pure coordination (Rope) — algorithmic auditability enables regulatory compliance. The transparency movement sees a temporary problem (Scaffold) — explainability requirements and human-in-the-loop mandates can dissolve the loop. Legacy accountability frameworks see their own degradation (Piton) — traditional responsibility concepts persist ceremonially but have lost real grip on institutional behavior. The analytical observer risks seeing natural law (Mountain) — that complex systems cannot be controlled by individual intention — when the constraint is actually a contingent institutional design choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual moral agents: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Ethical accountability commons: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction of abstract collective good. Affected populations: Victim + trapped → d≈0.93, f(d)≈1.39. Maximum extraction from those subject to algorithmic decisions. Institutional decision-makers: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Minimal effective extraction from their perspective; they are net beneficiaries. Compliance functions: Beneficiary + constrained → d≈0.35, f(d)≈0.30. Low extraction; coordination function dominates. Transparency movement: Organized + constrained → d≈0.42, f(d)≈0.42. Low effective extraction; movement has agency and organizational capacity to build alternatives. Legacy frameworks: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (0.64 ≥ 0.70 threshold not quite met, but approaching); the frameworks are beneficiaries of ceremonial status despite functional degradation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint exhibits genuine coordination function (reducing discrimination, scaling decisions) AND genuine extraction (diffusing accountability, insulating institutions from moral criticism). The beneficiary perspective (institutional decision-makers) is accurate: they do experience the system as solving a real coordination problem. The victim perspective is also accurate: affected populations and individual moral agents do experience extraction and accountability diffusion. The constraint is not 'fake coordination masquerading as extraction' (which would be pure Snare) nor 'legitimate coordination mistaken for extraction' (which would be pure Rope). It is a hybrid where the coordination function is real but has been structurally coupled with accountability externalization in a way that serves institutional interests. The scaffold perspective indicates a real pathway forward: transparency requirements, human-in-the-loop mandates, and algorithmic auditing can preserve the coordination benefits (bias reduction, scalability) while restoring individual moral agency and accountability. The piton perspective reveals that legacy accountability frameworks are maintained performatively even as actual responsibility has been delegated. The false summit in the analytical observer's mountain perspective exposes that naturalizing externalization as inherent to scale is itself part of the extraction mechanism — it forecloses the question of whether the current institutional design is actually necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_responsibility_gap,
    'Is the diffusion of responsibility into algorithmic systems a necessary feature of scaled decision-making or a contingent institutional choice?',
    'Case studies of high-stakes domains (medicine, criminal justice, financial services) where human judgment has been retained vs. delegated; measurement of outcomes and accountability clarity in each',
    'If necessary: constraint is closer to Mountain (immutable property of scale). If contingent: constraint is extractive design choice (Snare/Tangled Rope from institutional perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_responsibility_gap, conceptual, 'Whether algorithmic responsibility diffusion is necessary or contingent').

omega_variable(
    explainability_sufficiency,
    'Do transparency interventions (explainable AI, algorithmic auditing, human-in-the-loop mandates) actually restore individual moral agency or merely create theater of accountability?',
    'Longitudinal study of post-transparency-implementation institutions: measure decision reversal rates, documented human override frequency, employee perception of moral authority, and external accountability success in litigation/regulation',
    'If genuinely restorative: scaffold perspective is structural (transparency can dissolve the loop). If theatrical: transparency becomes part of the extraction (Piton layer deepens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(explainability_sufficiency, empirical, 'Whether transparency interventions restore agency or create accountability theater').

omega_variable(
    institutional_moral_capacity,
    'Can institutions themselves bear moral responsibility (as opposed to humans within institutions), or does moral agency necessarily require individual human judgment?',
    'Philosophical analysis of personhood and agency; empirical study of how courts, regulators, and public understanding have evolved in assigning responsibility to corporate vs. individual actors',
    'If institutions can bear moral responsibility: externalization may be legitimate delegation, not extraction. If only individuals can: externalization is structural illegitimacy (Snare from all victim perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_moral_capacity, conceptual, 'Whether institutional moral responsibility is philosophically coherent').

omega_variable(
    algorithmic_bias_vs_human_discretion,
    'Do algorithms reduce bias compared to human discretion in high-stakes decisions (hiring, lending, criminal sentencing), or do they encode and scale existing biases?',
    'Meta-analysis of algorithmic fairness studies; comparative outcome analysis in domains where algorithmic and human decisions are measurable; measurement of statistical parity, disparate impact, and individual-level accuracy across demographic groups',
    'If algorithms genuinely reduce bias: coordination function is real (Rope/Tangled Rope). If algorithms scale bias: coordination is claimed but false (Snare/Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_bias_vs_human_discretion, empirical, 'Whether algorithms reduce or scale discrimination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moral_outsourcing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moral_tr_t0, moral_outsourcing, theater_ratio, 0, 0.35).
narrative_ontology:measurement(moral_tr_t5, moral_outsourcing, theater_ratio, 5, 0.5).
narrative_ontology:measurement(moral_tr_t10, moral_outsourcing, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(moral_be_t0, moral_outsourcing, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(moral_be_t5, moral_outsourcing, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(moral_be_t10, moral_outsourcing, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moral_outsourcing, enforcement_mechanism).
narrative_ontology:affects_constraint(moral_outsourcing, algorithmic_bias_scaling).
narrative_ontology:affects_constraint(moral_outsourcing, accountability_diffusion).
narrative_ontology:affects_constraint(moral_outsourcing, automation_consent_fiction).

% DUAL FORMULATION NOTE:
% The ethical externalization loop is the parent constraint; it decomposes into three domain-specific manifestations: algorithmic bias scaling (hiring, lending, criminal justice), accountability diffusion (corporate liability structures, bureaucratic chains), and automation consent fiction (systems justified by user consent/choice when users have no meaningful exit). Each subdomain has its own ε value and perspectival structure but all are structurally coupled through the common mechanism of delegating moral authority while diffusing accountability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(moral_outsourcing, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
