% ============================================================================
% CONSTRAINT STORY: insult_wisdom_training
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_insult_wisdom_training, []).

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
 *   constraint_id: insult_wisdom_training
 *   human_readable: The Odd Assignment (Paying for Insults)
 *   domain: religious/social/philosophical
 *
 * SUMMARY:
 *   A disciple seeks wisdom from a master teacher and is given an unusual
 *   assignment: for three years, he must pay anyone who insults him. This
 *   constraint creates a structural tension between the master's stated
 *   pedagogical goal (cultivating equanimity and ego-dissolution) and the
 *   imposed mechanism (financial extraction + forced humiliation +
 *   suppression of normal social reciprocity). The constraint exhibits
 *   characteristics of both pure extraction (snare) and hybrid
 *   coordination-extraction (tangled rope) depending on the observer's
 *   position. From the disciple's perspective, it is a snare — he is trapped,
 *   must pay for harm, and cannot exit without forfeiting access to wisdom.
 *   From the master's perspective, it is a coordination mechanism for
 *   achieving a legitimate pedagogical outcome. From the global institutional
 *   perspective, it is a theatrical vestigial practice (piton) maintained
 *   through traditional authority. The core analytical question is whether
 *   the assignment's extractive structure is a necessary cost of legitimate
 *   wisdom training or a rationalization for pure rent-seeking dressed in
 *   pedagogical language.
 *
 * KEY AGENTS:
 *   - Disciple Subject: Primary victim (powerless/trapped) — bears financial and psychological cost; cannot exit without abandoning pursuit of wisdom
 *   - Master Teacher: Primary beneficiary (institutional/arbitrage) — designs and enforces the assignment; controls access to wisdom; benefits from the disciple's demonstrated commitment
 *   - Insulter Community: Secondary actor (moderate/constrained) — paid to insult; implicated in the pedagogical system; benefits financially and socially from participation
 *   - Traditional Wisdom Institution: Institutional observer (institutional/arbitrage) — maintains the practice through authority and inertia; sees the assignment as a credible test of commitment within the tradition
 *   - Analytical Observer: Civilizational context (analytical/analytical) — evaluates whether the assignment's extractiveness is justified by coordination function or represents rationalized exploitation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(insult_wisdom_training, 0.58).
domain_priors:suppression_score(insult_wisdom_training, 0.68).
domain_priors:theater_ratio(insult_wisdom_training, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(insult_wisdom_training, extractiveness, 0.58).
narrative_ontology:constraint_metric(insult_wisdom_training, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(insult_wisdom_training, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(insult_wisdom_training, snare).
narrative_ontology:human_readable(insult_wisdom_training, "The Odd Assignment (Paying for Insults)").
narrative_ontology:topic_domain(insult_wisdom_training, "religious/social/philosophical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(insult_wisdom_training, master_teacher).
narrative_ontology:constraint_beneficiary(insult_wisdom_training, insulters_community).
narrative_ontology:constraint_victim(insult_wisdom_training, disciple_subject).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISCIPLE (SNARE) — Powerless, trapped in a three-year commitment with no exit option without losing access to the master's teaching. Must pay anyone who insults them, creating financial extraction tied to humiliation. The constraint is extractive toward the disciple and suppressive: exit requires abandoning the pursuit of wisdom. The disciple experiences this as maximum extraction — they cannot refuse insults, cannot retaliate, and cannot leave without forfeiting their objective.
constraint_indexing:constraint_classification(insult_wisdom_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MASTER TEACHER (ROPE) — Institutional power, arbitrage exit (can teach or not teach). The master designed this assignment as a coordination mechanism for cultivating equanimity and ego-dissolution. From the master's perspective, the disciple's payment and forced humiliation solve the collective problem of ego-attachment in wisdom-seeking. The master experiences the constraint as pure coordination — a method that works, not as extraction. Benefits from the disciple's sustained engagement and demonstrated commitment.
constraint_indexing:constraint_classification(insult_wisdom_training, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE INSULTER COMMUNITY (TANGLED ROPE) — Moderate power, constrained exit (implicated in the local community, bound by social norms of reciprocity). They benefit from receiving payment for insults (financial gain, social license to insult) but are also coordinated into playing a role in the master's pedagogical system. They cannot fully exit without social consequences, but they also cannot ignore the financial incentive. Mixed coordination (fulfilling the master's design) and extraction (capturing rents from the disciple's humiliation).
constraint_indexing:constraint_classification(insult_wisdom_training, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal perspective, the constraint exhibits both genuine coordination function (ego-dissolution training) and asymmetric extraction (financial burden, sustained humiliation, power asymmetry). The three-year duration, the mandatory payment, the suppression of normal social reciprocity all point to extraction overlaid on coordination. The master's pedagogical intent (coordination) is inseparable from the disciple's imposed costs (extraction). This is the defining structure of tangled rope.
constraint_indexing:constraint_classification(insult_wisdom_training, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: THE TRADITIONAL WISDOM INSTITUTION (PITON) — Viewed from the institutional framework of traditional wisdom schools globally, the odd assignment is a theatrical test of commitment. The performative aspects (public payment, public insults, ritual humiliation) dominate the actual pedagogical function. Many wisdom traditions use humiliation-based training, but the specific form of this assignment (paying for insults) appears to be a vestigial practice maintained through institutional inertia rather than proven pedagogical efficacy. Theater ratio is high because the visible display of commitment matters as much as actual ego-dissolution.
constraint_indexing:constraint_classification(insult_wisdom_training, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(insult_wisdom_training_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(insult_wisdom_training, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(insult_wisdom_training, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(insult_wisdom_training, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(insult_wisdom_training, TR),
    TR >= 0.70.

:- end_tests(insult_wisdom_training_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint imposes quantifiable financial costs on the disciple (payment for each insult) and unquantifiable psychological costs (sustained humiliation, loss of social status, forced powerlessness). The three-year duration suggests extraction is not momentary but sustained. However, extractiveness is not extreme (0.70+) because the master claims a genuine pedagogical function (ego-dissolution), the disciple theoretically consents, and there is a terminal endpoint (three years, not perpetual). The upward trajectory from 0.48 to 0.58 over the training interval reflects increasing psychological burden as the disciple becomes more aware of the mechanism's harshness. Suppression (0.68): High. The assignment suppresses normal social reciprocity (the disciple cannot retaliate), suppresses the disciple's exit options (leaving means forfeiting wisdom), and suppresses the disciple's ability to maintain social standing (public humiliation). The three-year fixed duration is itself suppressive — the disciple cannot negotiate, shorten the timeline, or escape based on demonstrated progress. Theater ratio (0.62): Moderate-high. The visible spectacle of payment and public insults comprises a significant portion of the assignment's mechanism. The performative aspect (proving commitment through endurance of humiliation) is inseparable from the pedagogical mechanism. However, theater is not dominant (>0.70) because the actual ego-dissolution training, while difficult to verify, may be real.
 *
 * PERSPECTIVAL GAP:
 *   The constraint illustrates fundamental disagreement about what constitutes wisdom training. The master sees the assignment as solving the coordination problem of ego-attachment — disciples with large egos will not learn; the assignment creates conditions that force ego-dissolution. From the master's institutional perspective, this is rope: coordination mechanism, low perceived extraction, necessary overhead. The disciple initially consents because the master's authority is trusted. But after experiencing the assignment's reality (real financial costs, ongoing humiliation, three-year burden), the disciple's perspective may shift toward snare: the extraction is now salient, the exit option is illusory (sunk cost fallacy makes leaving psychologically harder as time progresses), and the pedagogical claim is unverifiable. The insulter community occupies a different gap — they benefit financially from the assignment and are coordinated into fulfilling the master's design, but they are not responsible for the disciple's suffering. The traditional wisdom institution sees the assignment as a test of commitment (piton perspective): the specific mechanism matters less than the visible proof that the disciple endured hardship. The analytical observer's tangled rope classification reflects the genuine tension: the assignment may contain real coordination value, but it is inseparable from and potentially justified by extraction that serves the master's authority rather than the disciple's development.
 *
 * DIRECTIONALITY LOGIC:
 *   The disciple's directionality value (d) is high — they are the target of extraction. Powerless status + trapped exit + victim classification → d ≈ 0.90-0.95 → f(d) ≈ 1.35. The master's directionality value is low — they are the beneficiary. Institutional status + arbitrage exit + beneficiary classification → d ≈ 0.05-0.15 → f(d) ≈ -0.10. The insulter community's directionality is moderate — they are coordinated but also benefit. Moderate status + constrained exit + beneficiary-victim hybrid → d ≈ 0.40-0.50 → f(d) ≈ 0.40-0.65. These directionality flows determine experienced extraction: high for the disciple, low for the master, moderate for the insulters. The three-year duration means sustained directionality over time, not a momentary event.
 *
 * MANDATROPHY ANALYSIS:
 *   The assignment creates a mandatrophy between the master's coordination narrative (this is ego-dissolution training, necessary for wisdom) and the structural reality of extraction (the disciple pays, the disciple is humiliated, the disciple cannot exit, the master benefits from the disciple's continued engagement). The resolution hinges on whether the coordination function is genuine and necessary. If ego-dissolution training cannot be achieved without financial extraction and sustained humiliation, the tangled rope classification holds: extraction is real but justified by coordination value. If alternative training methods exist or if the pedagogy is ineffective, the assignment is pure snare: the coordination narrative is rationalization. The empirical omegas (wisdom acquisition mechanism, alternative training efficacy) are designed to test this. The conceptual omega (authentic consent) probes whether the disciple's initial agreement is meaningful. If the disciple cannot have known the true psychological and financial burden ex ante, the consent is vitiated, and the snare classification is confirmed regardless of pedagogical outcome. The assignment's theater ratio (0.62) suggests that the performative aspect (visible commitment through endurance) is substantial but not dominant, consistent with tangled rope rather than pure piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wisdom_acquisition_mechanism,
    'Does paying for insults actually produce wisdom, or does it produce conditioned tolerance that mimics wisdom?',
    'Longitudinal comparison of disciples who underwent the assignment vs. those who underwent alternative training; assessment of wisdom via epistemological coherence, behavioral adaptation, and teaching effectiveness post-training',
    'If mechanism works: constraint is coordinate-dominant (Rope from master perspective). If mechanism fails: constraint is pure extraction with theatrical justification (Snare confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wisdom_acquisition_mechanism, empirical, 'Whether the assignment produces authentic wisdom or conditioned tolerance').

omega_variable(
    alternative_training_efficacy,
    'Are there demonstrably more efficient ego-dissolution training methods that do not require financial extraction or public humiliation?',
    'Comparative study of wisdom-tradition training methods; measurement of ego-dissolution markers (equanimity, response flexibility, non-reactive awareness) across methods; analysis of outcome variance explained by humiliation component vs. other factors',
    'If alternatives exist: the odd assignment is rendered unnecessary (pure snare extraction). If no alternatives match efficacy: the assignment''s suppression is justified by coordination function (snare→tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_training_efficacy, empirical, 'Whether alternative training methods achieve equivalent outcomes').

omega_variable(
    disciple_authentic_consent,
    'Does the disciple''s initial acceptance of the assignment constitute genuine consent, or does epistemic asymmetry (master''s superior knowledge of the process) vitiate consent?',
    'Analysis of information available to disciple before accepting; comparison with informed consent standards in research ethics; examination of whether disciples can accurately predict psychological impacts; study of disciples who withdrew and their reported reasons',
    'If authentic consent: suppression is reduced (transforms toward tangled rope). If consent vitiated by asymmetry: suppression is confirmed as structural (pure snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disciple_authentic_consent, conceptual, 'Whether disciple''s initial acceptance is authentic informed consent').

omega_variable(
    extraction_rate_arbitrariness,
    'Why is the payment amount unspecified? Does the absence of a payment schedule create extractive arbitrariness that exceeds the pedagogical requirement?',
    'Historical analysis of actual payment amounts demanded; comparison with local economic baseline (subsistence wage, typical transaction costs); assessment of whether payment variance correlates with insult severity vs. insulter negotiating power',
    'If payments are arbitrary and excessive: extractiveness confirmed as high (0.58+). If payments are calibrated to insult/commitment level: extractiveness may be lower (coordinate logic dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_rate_arbitrariness, empirical, 'Whether unspecified payment schedule enables arbitrary extraction').

omega_variable(
    three_year_duration_justification,
    'Is the three-year timeline pedagogically necessary, or is it an arbitrary suppression duration that extends extraction beyond the minimum required for ego-dissolution?',
    'Comparative analysis of wisdom-tradition training durations; measurement of ego-dissolution learning curves; identification of diminishing returns or plateau points',
    'If three years is pedagogically optimal: duration is justified (coordinate). If plateau occurs earlier: duration becomes pure extraction (snare intensified).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(three_year_duration_justification, empirical, 'Whether three-year duration is pedagogically necessary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(insult_wisdom_training, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(insult_tr_t0, insult_wisdom_training, theater_ratio, 0, 0.5).
narrative_ontology:measurement(insult_tr_t1, insult_wisdom_training, theater_ratio, 1, 0.58).
narrative_ontology:measurement(insult_tr_t2, insult_wisdom_training, theater_ratio, 2, 0.62).

% Extraction over time
narrative_ontology:measurement(insult_be_t0, insult_wisdom_training, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(insult_be_t1, insult_wisdom_training, base_extractiveness, 1, 0.54).
narrative_ontology:measurement(insult_be_t2, insult_wisdom_training, base_extractiveness, 2, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(insult_wisdom_training, enforcement_mechanism).
narrative_ontology:affects_constraint(insult_wisdom_training, ego_attachment_suppression).
narrative_ontology:affects_constraint(insult_wisdom_training, epistemic_authority_asymmetry).

% DUAL FORMULATION NOTE:
% The odd assignment is downstream of the master's epistemic authority and the disciple's desire for wisdom. It is a specific structural realization of the general constraint that asymmetric access to valued knowledge (wisdom) enables extraction mechanisms. The assignment's extractiveness is contingent on whether the pedagogical claim is genuine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(insult_wisdom_training, institutional, 0.1).
constraint_indexing:directionality_override(insult_wisdom_training, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
