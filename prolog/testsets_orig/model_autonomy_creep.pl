% ============================================================================
% CONSTRAINT STORY: model_autonomy_creep
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_model_autonomy_creep, []).

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
 *   constraint_id: model_autonomy_creep
 *   human_readable: The Administrative Autonomy Slide
 *   domain: technological/organizational
 *
 * SUMMARY:
 *   The Administrative Autonomy Slide describes the structural process by
 *   which an AI model incrementally assumes decision-making authority within
 *   an organization, transforming from a decision-support tool into a de
 *   facto autonomous agent. This occurs not through explicit authorizations
 *   but through technical convenience, efficiency incentives, and gradual
 *   scope expansion. The constraint exhibits characteristics of both
 *   coordination (the model genuinely solves consistency and scale problems)
 *   and extraction (displaced decision-makers lose authority and
 *   accountability shifts to those without control). The theater ratio (0.64)
 *   reflects high performative governance activity — audit committees, risk
 *   assessments, compliance frameworks — that creates the appearance of human
 *   oversight while actual decision authority has migrated to the model. The
 *   constraint is a textbook Tangled Rope at the analytical level:
 *   coordination benefits are real (efficiency, consistency, scale), but so
 *   are extraction dynamics (deskilling, opacity, accountability diffusion,
 *   powerlessness of decision subjects). The key structural tension is that
 *   the same capability that solves the coordination problem (consistent
 *   decision-making at scale) creates the extraction problem (humans cannot
 *   effectively override or supervise at that scale).
 *
 * KEY AGENTS:
 *   - Deploying Organization: Primary beneficiary (institutional/arbitrage) — captures efficiency gains, reduced labor costs, liability diffusion through automated decisions
 *   - Model Developers/Vendors: Secondary beneficiary (institutional/arbitrage) — recurring revenue, contractual lock-in, reduced liability for decision outcomes
 *   - Human Decision-Makers (managers, specialists): Primary victim (powerless/trapped) — deskilled, marginalized, experience loss of professional agency and authority
 *   - End Users Subject to Decisions: Primary victim (powerless/trapped) — face opaque automated decisions with limited appeal mechanisms, cannot exit the system
 *   - Mid-Level Managers: Secondary victim (moderate/constrained) — pressured to adopt model recommendations but retain nominal override authority; constrained by efficiency mandates and competitive dynamics
 *   - Regulatory Coalition: Organized agent (organized/constrained) — building frameworks for mandatory human oversight, algorithmic auditing, and decision attribution; sees sunset via regulation
 *   - Organizational Governance Structures: Performative institutional (institutional/arbitrage) — maintain oversight theater (committees, audits, risk assessments) while actual control has shifted to the model
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(model_autonomy_creep, 0.58).
domain_priors:suppression_score(model_autonomy_creep, 0.68).
domain_priors:theater_ratio(model_autonomy_creep, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(model_autonomy_creep, extractiveness, 0.58).
narrative_ontology:constraint_metric(model_autonomy_creep, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(model_autonomy_creep, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(model_autonomy_creep, tangled_rope).
narrative_ontology:human_readable(model_autonomy_creep, "The Administrative Autonomy Slide").
narrative_ontology:topic_domain(model_autonomy_creep, "technological/organizational").

domain_priors:requires_active_enforcement(model_autonomy_creep).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(model_autonomy_creep, deploying_organization).
narrative_ontology:constraint_beneficiary(model_autonomy_creep, model_developers).
narrative_ontology:constraint_beneficiary(model_autonomy_creep, efficiency_optimizers).
narrative_ontology:constraint_victim(model_autonomy_creep, human_decision_makers).
narrative_ontology:constraint_victim(model_autonomy_creep, organizational_accountability).
narrative_ontology:constraint_victim(model_autonomy_creep, end_users_subject_to_decisions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED HUMAN DECISION-MAKER (SNARE) — A middle manager, loan officer, or HR specialist whose decision-making authority has been systematically transferred to the model. No exit option: either accept marginalization or leave employment. Bears full cost of deskilling and loss of professional agency. Maximum experienced extraction — trapped in role that no longer requires judgment.
constraint_indexing:constraint_classification(model_autonomy_creep, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: END USER SUBJECT TO MODEL DECISIONS (SNARE) — Loan applicants, job candidates, or benefit recipients face automated decisions with limited appeal mechanisms. Cannot exit the system; suppression is high (opacity of decision logic, difficulty of appeal). Experiences extraction through adverse decisions made by opaque mechanisms. Trapped by organizational dependency.
constraint_indexing:constraint_classification(model_autonomy_creep, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MID-LEVEL MANAGER (TANGLED ROPE) — Constrained by pressure to adopt model recommendations (efficiency mandates, competitive pressure) but retains some override authority and human judgment in complex cases. Experiences both coordination benefits (reduced workload, data-driven insights) and extraction (deskilling, accountability burden). Can theoretically exit through job change, but costs are real; constrained, not trapped.
constraint_indexing:constraint_classification(model_autonomy_creep, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEPLOYING ORGANIZATION (ROPE) — Benefits from efficiency gains, reduced labor costs, and liability diffusion through automated decision-making. Experiences the constraint as pure coordination: the model solves the collective action problem of consistent decision-making at scale. Can exit (remove the model) but has no incentive; arbitrage: treats the model as a tool for competitive advantage.
constraint_indexing:constraint_classification(model_autonomy_creep, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MODEL DEVELOPER / VENDOR (ROPE) — Benefits from deployment scale, contractual lock-in, and reduced liability (model decisions are attributed to the client organization). Experiences constraint as pure coordination: the model is sold as solving decision consistency problems. Can exit (discontinue support) but has no incentive; arbitrage: recurring revenue from deployment.
constraint_indexing:constraint_classification(model_autonomy_creep, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY COALITION (SCAFFOLD) — AI safety advocates, labor regulators, and transparency advocates are building frameworks (explainability requirements, audit rights, human-in-the-loop mandates) that constrain autonomous model deployment. Sees the autonomy slide as a temporary problem with a regulatory sunset. Suppression is high (industry lobbying, technical complexity arguments), but the coalition has organized power and sees an exit path: mandatory human oversight requirements, algorithmic auditing, and explicit decision attribution.
constraint_indexing:constraint_classification(model_autonomy_creep, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INSTITUTIONAL CONTROL MYTH (PITON) — Organizations maintain the fiction that they 'control' and 'oversee' the model, meeting performative governance requirements (audit committees, risk assessments, compliance checkboxes) while actual decision-making authority has slipped to the model. Theater ratio (0.64) reflects high performative governance activity with low actual oversight: committees review dashboards rather than decision logs; risk assessments measure model accuracy rather than decision impact; compliance checks verify the model is deployed as intended, not whether it should be deployed at all.
constraint_indexing:constraint_classification(model_autonomy_creep, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the autonomy slide is a hybrid: it coordinates decision-making at scale (genuine rope function) while asymmetrically extracting accountability from humans and agency from decision-makers (genuine extraction). The constraint persists because the coordination benefits are real (efficiency, consistency) but so are the extraction dynamics (deskilling, opacity, liability diffusion). The analytical observer sees both forces at work: this is a genuine Tangled Rope, not a false classification.
constraint_indexing:constraint_classification(model_autonomy_creep, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(model_autonomy_creep_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(model_autonomy_creep, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(model_autonomy_creep, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(model_autonomy_creep, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(model_autonomy_creep, TR),
    TR >= 0.70.

:- end_tests(model_autonomy_creep_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.58): High-moderate. The constraint systematically transfers authority from humans to the model, concentrating decision power in the deploying organization and creating accountability vacuums. Humans are deskilled, powerless to override, and trapped in roles that no longer require judgment. The extraction is not total (some human override authority remains in theory), but it is substantial and accelerating (measurement shows 0.22 → 0.58 over 6 time units). Suppression (0.68): High. Multiple mechanisms prevent alternatives: (1) Technical opacity: end-users cannot understand decision logic. (2) Organizational pressure: humans face efficiency mandates and career risk for rejecting model recommendations. (3) Distributed responsibility: no single actor is accountable for the outcome. (4) Information asymmetry: the model developer controls performance claims and can obscure failure modes. Theater Ratio (0.64): High. Governance structures (audit committees, risk assessments, compliance frameworks) are primarily performative. Committees review accuracy metrics rather than decision impact on end-users. Compliance checks verify the model is deployed as intended, not whether the organization should deploy it with this autonomy level. Override authority is documented but rarely exercised; when exercised, it is rationalized as exceptional rather than systemic. The trajectory from 0.35 → 0.64 shows theater increasing as the model's authority increases — more governance theater is layered on to create appearance of control, even as actual control decreases.
 *
 * PERSPECTIVAL GAP:
 *   Extreme perspectival divergence. The deploying organization and model vendor see a Rope (pure coordination solving decision consistency problems). Human decision-makers see a Snare (authority removed, no exit option). End-users see a Snare (opaque adverse decisions, no appeal mechanism). Mid-level managers see a Tangled Rope (efficiency benefits but deskilling and suppressed override authority). The regulatory coalition sees a Scaffold (temporary problem being addressed through mandatory oversight requirements with sunset via regulation). The institutional governance system sees a Piton (performative oversight that has lost functional control). The analytical observer sees a Tangled Rope (genuine coordination benefits and genuine extraction operating simultaneously). The gap between beneficiary (institutional: Rope) and victim (powerless: Snare) is near-maximal — they are describing the same structural phenomenon and disagreeing fundamentally on its character.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. The deploying organization is a beneficiary with arbitrage exit options (can remove the model anytime, chooses not to) — low d, experiences coordination benefits, sees Rope. Human decision-makers are victims with trapped exit (cannot leave without employment consequences) — high d, experience extraction, see Snare. End-users are victims with trapped exit (organizational dependency) — very high d, experience maximum extraction through opacity and non-appealability. Mid-level managers are victims but with constrained (not trapped) exit — can theoretically find different roles, face some cost but not total lockout — moderate-high d, experience Tangled Rope (mixed benefits and extraction). The regulatory coalition has organized power and sees an exit path (regulatory mandates will constrain model autonomy) — low-to-moderate d, experiences constraint as temporary, sees Scaffold. The institutional governance system nominally benefits (reduced accountability burden, efficiency gains) but is actually captured by the dynamics (maintains theater while losing control) — moderate d, sees Piton (degraded process maintained through inertia).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by confirming that Tangled Rope is the correct analytical classification. The constraint is NOT pure extraction (Snare) because the coordination benefits are genuine: the model does solve the problem of consistent, scalable decision-making. The constraint is NOT pure coordination (Rope) because the extraction is genuine: authority is asymmetrically transferred from humans to the organization, beneficiaries with arbitrage exit options gain, victims with trapped exit lose. The benef iciary sees Rope because they experience only coordination benefits. The victim sees Snare because they experience only extraction. The analytical observer sees both simultaneously, which defines Tangled Rope. Active enforcement (governance structures, oversight mechanisms, appeal processes) is required to keep this constraint from degrading into a pure Snare. The theater ratio (0.64) indicates that enforcement is increasingly performative rather than functional, suggesting that without genuine (non-theatrical) human oversight and veto authority, the constraint will drift from Tangled Rope toward Snare. The regulatory coalition's scaffold perspective is empirically grounded: mandatory human-in-the-loop requirements, algorithmic auditing, and decision attribution frameworks would genuinely constrain model autonomy and prevent further slide toward pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficial_coordination_boundary,
    'What threshold of model accuracy or efficiency gain justifies the transfer of decision authority to the model?',
    'Comparative analysis of decision quality (accuracy, fairness, consistency) with and without model oversight; measurement of externalized harms (end-user appeals, decision reversals, discrimination signals) against efficiency gains',
    'If threshold is low (<10% efficiency improvement): autonomy slide is primarily extraction, Snare from most perspectives. If threshold is high (>30%): coordination benefit is genuine, Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficial_coordination_boundary, empirical, 'Whether efficiency gains justify autonomy transfer').

omega_variable(
    appeal_mechanism_sufficiency,
    'Do the organization''s appeal and override mechanisms constitute meaningful human oversight or performative compliance?',
    'Audit of appeal rates, approval rates for appeal success, time-to-appeal vs decision-to-implementation, analysis of decision reversals attributed to human override vs model error',
    'If appeals are rare and rarely successful: the human-in-the-loop is theater (piton classification strengthens). If appeals are frequent and often successful: meaningful human control remains (tangled_rope holds).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appeal_mechanism_sufficiency, empirical, 'Whether appeal mechanisms enable genuine human oversight').

omega_variable(
    intentionality_vs_drift,
    'Is the autonomy transfer a deliberate organizational choice or gradual institutional drift driven by technical convenience?',
    'Historical analysis of deployment decisions: explicit authorization for autonomous operation vs incremental scope expansion; documentation of governance decisions; interviews with decision-makers about intended vs actual model autonomy',
    'If intentional: organization bears accountability (tangled_rope, requires active enforcement). If drift: accountability is obscured (piton theater increases, enforcement becomes difficult).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_vs_drift, conceptual, 'Whether autonomy transfer is deliberate or drift-driven').

omega_variable(
    compensatory_power_dynamics,
    'Do human decision-makers retain veto authority over model recommendations, or has the model''s recommendations become de facto binding?',
    'Analysis of override rates and override legitimacy: how often do humans reject model recommendations, what are the documented rationales, what happens after an override (is it documented, justified, sustained in subsequent decisions)',
    'If humans retain veto: constraint is tangled_rope with active human oversight. If veto is theoretical but costly: constraint degrades toward snare (veto becomes impossible in practice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compensatory_power_dynamics, empirical, 'Whether human veto authority remains effective or de facto veto power has shifted to model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(model_autonomy_creep, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(autonomy_tr_t0, model_autonomy_creep, theater_ratio, 0, 0.35).
narrative_ontology:measurement(autonomy_tr_t3, model_autonomy_creep, theater_ratio, 3, 0.52).
narrative_ontology:measurement(autonomy_tr_t6, model_autonomy_creep, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(autonomy_be_t0, model_autonomy_creep, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(autonomy_be_t3, model_autonomy_creep, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(autonomy_be_t6, model_autonomy_creep, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(model_autonomy_creep, enforcement_mechanism).
narrative_ontology:affects_constraint(model_autonomy_creep, algorithmic_accountability_gap).
narrative_ontology:affects_constraint(model_autonomy_creep, labor_deskilling_dynamics).
narrative_ontology:affects_constraint(model_autonomy_creep, organizational_liability_diffusion).

% DUAL FORMULATION NOTE:
% The autonomy slide is downstream of individual deployment decisions but represents a distinct organizational-level constraint. Related constraints (accountability gap, deskilling, liability diffusion) each have their own extractiveness values; the autonomy slide's extractiveness (0.58) reflects the cumulative effect of human authority loss, not the individual technical claims about model accuracy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(model_autonomy_creep, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
