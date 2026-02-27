% ============================================================================
% CONSTRAINT STORY: peter_principle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_peter_principle, []).

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
 *   constraint_id: peter_principle
 *   human_readable: The Peter Principle (Promotion to Incompetence)
 *   domain: organizational/labor/social
 *
 * SUMMARY:
 *   The Peter Principle describes a structural mechanism embedded in
 *   hierarchical organizations where promotion decisions are based on
 *   demonstrated competence in the current role rather than predicted
 *   competence in the next role. This creates a system where competent
 *   performers are systematically promoted until they reach a level where
 *   they are incompetent and remain there, permanently mismatch. The
 *   principle operates as a self-reinforcing snare: subordinates of
 *   incompetent managers must compensate for managerial failure, preventing
 *   their own advancement; the promotion ritual masks this extraction through
 *   objective-sounding evaluation criteria that do not actually predict
 *   role-fit; and incompetent incumbents have low exit costs (via lateral
 *   moves or other firms), while capable subordinates have high exit costs
 *   (career penalties, credential devaluation). The theater_ratio (0.68)
 *   reflects that formal promotion processes appear meritocratic and
 *   evidence-based but in practice correlate weakly with subsequent
 *   performance, especially when assessing fit for fundamentally different
 *   role requirements. The extractiveness (0.52) is moderate: not as severe
 *   as intentional wage theft, but sustained misallocation of human capital
 *   and organizational resources driven by structural incentives rather than
 *   competence.
 *
 * KEY AGENTS:
 *   - Competent Subordinates: Primary victims (powerless/trapped) — must remediate managerial failures; promotion blocked by incumbent incompetence; exit costly
 *   - Competent Mid-Managers: Secondary victims (moderate/constrained) — caught between incompetent seniors and need to mask/compensate; constrained exit
 *   - Incumbent Incompetent Managers: Primary beneficiaries (institutional/arbitrage) — retain salary, status, and authority despite incompetence; low exit cost if removed (lateral moves, severance)
 *   - Organizational Productivity/Customer Outcomes: Victim (powerless/trapped) — abstract collective good; bears full cost of misalignment; no self-correction mechanism
 *   - Promotion Ritual (Formal Performance Review System): Institutional actor (institutional/arbitrage) — maintains theater; appears objective while mispredicting role-fit
 *   - Organizational Reform Coalition (HR, OD consultants, unions, IC track advocates): Organized agents (organized/arbitrage) — see rope structure; alternative career pathways (lateral moves, specialist tracks, external hire) as viable coordination solutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(peter_principle, 0.52).
domain_priors:suppression_score(peter_principle, 0.58).
domain_priors:theater_ratio(peter_principle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peter_principle, extractiveness, 0.52).
narrative_ontology:constraint_metric(peter_principle, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(peter_principle, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(peter_principle, snare).
narrative_ontology:human_readable(peter_principle, "The Peter Principle (Promotion to Incompetence)").
narrative_ontology:topic_domain(peter_principle, "organizational/labor/social").

domain_priors:requires_active_enforcement(peter_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(peter_principle, organizational_incumbents).
narrative_ontology:constraint_victim(peter_principle, competent_subordinates).
narrative_ontology:constraint_victim(peter_principle, organizational_productivity).
narrative_ontology:constraint_victim(peter_principle, customer_outcomes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPETENT SUBORDINATE (SNARE) — Trapped in a hierarchical structure that promotes incompetent managers above them. Cannot exit without career penalty; must bear the burden of remedying managerial failures. Has no authority to halt their own demotion via incompetent supervisor. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(peter_principle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETENT MID-MANAGER (SNARE) — Faces extraction from above (incompetent seniors) and below (must compensate for own subordinate's escalation). Exit is costly (credential penalty, severance loss) but theoretically possible. d≈0.78, f(d)≈1.12, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(peter_principle, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT ORGANIZATIONAL ELITE (TANGLED ROPE) — Beneficiaries of the promotion mechanism. Incompetent manager can arbitrage (move to another firm; lateral transfer). The Peter Principle coordination function: clear promotion pathway reduces uncertainty about career progression and keeps ambitious middle managers striving. But extraction mechanism is also real: incompetent incumbents prevent capable lower-level managers from advancing. d≈0.15, f(d)≈0.02, σ=1.0 → χ≈0.01. Net beneficiary; relatively low effective extraction because they have exit via arbitrage.
constraint_indexing:constraint_classification(peter_principle, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROMOTION RITUAL (PITON) — Formal promotion procedures (performance reviews, merit evaluation, succession planning) are substantially performative. The theater: organizations conduct objective-sounding evaluations and promotion criteria, but actual promotion often reflects tenure, political skill, conformity, and luck rather than evidence that the candidate will perform in the higher role. theater_ratio=0.68 reflects that the ritual persists despite poor predictive validity. Firms maintain promotion theater because alternatives (transparent incompetence firing, lottery promotion, external hire) carry higher explicit costs or political resistance.
constraint_indexing:constraint_classification(peter_principle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (SNARE FROM EFFICIENCY VIEW) — From a civilizational perspective tracking organizational productivity, the Peter Principle is a pure extraction mechanism: talent is systematically mismatch with roles in a way that degrades performance. The constraint is that hierarchies structurally promote based on success in lower roles, not fit for higher roles, creating a self-reinforcing cycle of incompetence at senior levels. ε=0.52, suppression=0.58 show this is neither law of nature nor pure coordination. d≈0.90, f(d)≈1.30, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(peter_principle, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZATIONAL REFORM COALITION (ROPE) — Organized groups (management consultants, HR departments, employee unions, organizational psychologists) see the Peter Principle as a coordination problem with a solvable structure. Solutions: 360 reviews, 360-degree feedback, competency modeling, lateral moves, IC/specialist tracks that reward mastery without requiring management, probationary promotions with clear failure conditions. These reforms reduce extraction by creating alternative career pathways. d≈0.35, f(d)≈0.33, σ=0.9 → χ≈0.15. Low effective extraction; coordination mechanism dominates.
constraint_indexing:constraint_classification(peter_principle, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peter_principle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(peter_principle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peter_principle, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(peter_principle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(peter_principle, TR),
    TR >= 0.70.

:- end_tests(peter_principle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The Peter Principle extracts value from competent subordinates (time spent managing upward, career stagnation, turnover) and organizational productivity (misaligned role fit, reduced output). However, it is not maximal extraction because: (a) some firms do implement alternative promotion mechanisms (IC tracks, lateral specialist roles) that reduce extraction; (b) the mechanism operates partly through structural misalignment rather than intentional coercion; (c) exit is constrained but not impossible for subordinates, and removal of incompetent managers is theoretically possible (though politically costly). Suppression (0.58): Significant barriers to resolution include: organizational politics (incompetent incumbents resist demotion), career identity (people define self-worth by title/level, making lateral moves psychologically costly), institutional inertia (promotion ritual is normalized), and information asymmetry (incompetent managers control information flow and can misrepresent their subordinates' contributions). But suppression is not total — exit paths exist, reform coalitions have successfully implemented alternative models in some firms, and external pressures (talent competition, firm performance) create incentives to fix the system. Theater (0.68): Promotion processes use objective language (merit evaluation, performance criteria, competency models) but do not actually predict fit for higher roles. The theater has increased over time as organizations have formalized evaluation procedures without improving their predictive validity. Measurement substitution (Goodhart drift): organizations that optimize for the evaluation metric (past role performance) degrade the actual outcome (future role competence). The measurement drift reflects that visible domain expertise (which is easy to assess) substitutes for latent managerial competence (which is hard to assess).
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows stark perspectival divergence. Incompetent managers see promotion as deserved reward and coordination mechanism (clear career pathway reduces uncertainty). Their subordinates see the same system as pure extraction (Snare). Reform-minded organizations see the constraint as coordination problem with known solutions (Rope). The promotion ritual sees itself as meritocratic (Piton — performative rather than functional). Organizational efficiency views it as a structural mismatch problem (Snare from civilizational scale). The key divergence is between those who benefit from the current mechanism (incumbents, beneficiaries of status quo) and those who bear its costs (subordinates, organizational productivity). The incompetent manager's perspective is not captured fully by the institutional/arbitrage tuple alone because arbitrage exit is asymmetric: easy for the beneficiary (move sideways, switch firms) but costly for the victim (career penalty).
 *
 * DIRECTIONALITY LOGIC:
 *   Competent Subordinates: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Competent Mid-Managers: Victim + constrained → d≈0.78, f(d)≈1.12. High extraction but theoretically escapable. Incumbent Incompetent Managers: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Net beneficiary; low effective extraction due to exit ease. Organizational Productivity: Victim + trapped → d≈0.90, f(d)≈1.30. Abstract collective cannot exit. Promotion Ritual: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification from theater gate, not from high chi. Organizational Reform Coalition: Organized + arbitrage → d≈0.35, f(d)≈0.33. Low effective extraction; coalition has agency and sees path to rope classification through alternative career structures.
 *
 * MANDATROPHY ANALYSIS:
 *   The Peter Principle resolves mandatrophy by showing that the snare classification (claimed_type) is structurally sound despite the presence of a rope-like coordination function (clear promotion pathway). The coordination exists but is asymmetric: it benefits incumbents (clear path to status) while extracting from subordinates (clear ceiling on their advancement once incumbents are in place). This is precisely the mandatrophy definition: a structure that appears to be pure coordination (everyone gets a clear pathway) but operates asymmetrically to extract from those with fewer alternatives. The reform coalition's perspective (Rope) is not a contradiction of the snare classification but a perspectival alternative: if alternative promotion mechanisms were adopted (lateral specialist tracks, IC ladders, external hire thresholds), the constraint would shift toward Rope. The snare classification holds at the baseline (organizational status quo in most large firms). The organizational reform coalition's perspective shows that mandatrophy can be resolved through structural change, not just through frame-switching.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_validity_incompetence,
    'Can performance in a lower role validly predict performance in the next higher role, or are the skills entirely orthogonal?',
    'Longitudinal studies tracking promotion outcomes by prior role performance; meta-analysis of pre/post-promotion manager effectiveness data; role-specific competency frameworks showing skill transfer',
    'If orthogonal skills: Peter Principle is a structural law of hierarchies (Mountain from efficiency view). If partial transfer: conditional promotion based on assessed target-role competency becomes viable (Rope from reform view).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_validity_incompetence, empirical, 'Whether lower-role performance predicts higher-role competence').

omega_variable(
    exit_cost_reality,
    'How constraining are actual exit costs for incompetent managers or their subordinates? Can they realistically leave?',
    'Labor market analysis: turnover rates of mid-managers reporting to incompetent seniors vs baseline; wage penalty for lateral moves; time-to-reemployment for managers exiting hierarchies',
    'If high exit costs: snare classification confirmed. If low costs: constraint operates through status/identity rather than true coercion (scaffold with cultural sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_reality, empirical, 'Actual exit costs and reemployment feasibility for trapped subordinates').

omega_variable(
    alternative_promotion_models_viability,
    'Can alternative promotion mechanisms (lateral specialist tracks, IC (Individual Contributor) ladders, external hire, competency-based transfer) actually scale to replace traditional hierarchical promotion without creating other extraction mechanisms?',
    'Comparative org study: firms with alternative promotion models vs traditional hierarchies; measurement of promotion fairness perceptions, retention of competent middle managers, senior role performance variance',
    'If viable at scale: Peter Principle is a contingent institutional choice (Snare now, Rope after reform). If not: it is deeply structural to hierarchy (approaches Mountain).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_promotion_models_viability, conceptual, 'Whether alternative promotion systems can viably replace traditional hierarchy').

omega_variable(
    competence_assessment_bias,
    'Are current promotion assessment mechanisms biased toward visible domain-specific competence (which doesn''t transfer) rather than latent managerial/leadership competence (which might)?',
    'Analysis of promotion criteria vs manager success outcomes; psychometric validation of assessment instruments; blind review vs identified review of promotion candidates',
    'If structural bias: Peter Principle is partly an artifact of poor measurement (Snare maintainable through better assessment). If assessments are valid: competence orthogonality is real (Peter Principle is deep structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_assessment_bias, empirical, 'Whether promotion assessments conflate domain performance with managerial readiness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(peter_principle, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(peter_tr_t0, peter_principle, theater_ratio, 0, 0.48).
narrative_ontology:measurement(peter_tr_t25, peter_principle, theater_ratio, 25, 0.58).
narrative_ontology:measurement(peter_tr_t50, peter_principle, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(peter_be_t0, peter_principle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(peter_be_t25, peter_principle, base_extractiveness, 25, 0.44).
narrative_ontology:measurement(peter_be_t50, peter_principle, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(peter_principle, resource_allocation).
narrative_ontology:affects_constraint(peter_principle, middle_management_burnout).
narrative_ontology:affects_constraint(peter_principle, talent_pipeline_bottleneck).
narrative_ontology:affects_constraint(peter_principle, organizational_performance_plateau).

% DUAL FORMULATION NOTE:
% The Peter Principle is a constraint family decomposable into (1) the promotion mechanism itself (this story: ε=0.52, institutional + structural incentive misalignment), (2) the talent pipeline effect (downstream: competent mid-managers trapped, burnout acceleration), and (3) performance degradation (downstream: organizational output capped by senior-level incompetence). Each has distinct ε values and resolution pathways, but they are linked: fixing the promotion mechanism cascades to reduce burnout and improve performance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(peter_principle, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
