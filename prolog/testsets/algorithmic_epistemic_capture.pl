% ============================================================================
% CONSTRAINT STORY: algorithmic_epistemic_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_epistemic_capture, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: algorithmic_epistemic_capture
 *   human_readable: The Feedback Loop Blindfold
 *   domain: technological/social/cognitive
 *
 * SUMMARY:
 *   This constraint describes the process by which an individual's
 *   information diet is shaped by recommendation algorithms optimizing for
 *   engagement. While these systems solve a genuine coordination problem
 *   (finding relevant content in an ocean of data), their optimization
 *   function is not aligned with epistemic health or accuracy. The result is
 *   a feedback loop where users are shown progressively more extreme or
 *   engaging content, creating a personalized reality bubble that is highly
 *   resistant to outside information. This 'blindfold' is not imposed by
 *   overt censorship but by the subtle, continuous curation of reality.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) - Profit from maximizing user engagement via ad revenue and data collection.
 *   - Individual Users: Primary victim (powerless/trapped) - Their attention is extracted and their worldview is distorted. Trapped by network effects.
 *   - Societal Epistemic Commons: Secondary victim (powerless/trapped) - Degraded by the spread of misinformation and polarization.
 *   - Regulators: Organized actor (organized/constrained) - Attempt to mitigate harms through policy scaffolds.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_epistemic_capture, 0.65).
domain_priors:suppression_score(algorithmic_epistemic_capture, 0.75).
domain_priors:theater_ratio(algorithmic_epistemic_capture, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, extractiveness, 0.65).
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, theater_ratio, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_epistemic_capture, tangled_rope).
narrative_ontology:human_readable(algorithmic_epistemic_capture, "The Feedback Loop Blindfold").
narrative_ontology:topic_domain(algorithmic_epistemic_capture, "technological/social/cognitive").

domain_priors:requires_active_enforcement(algorithmic_epistemic_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_epistemic_capture, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_epistemic_capture, advertisers).
narrative_ontology:constraint_victim(algorithmic_epistemic_capture, individual_users).
narrative_ontology:constraint_victim(algorithmic_epistemic_capture, societal_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The user is trapped by network effects and high friction-to-exit. They experience the system as pure extraction of their attention and data, at the cost of a narrowing, distorted worldview. The coordination function is invisible, subsumed by the overwhelming sense of being manipulated. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.11.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the platform's perspective, the algorithm is a pure coordination mechanism solving the problem of information overload to maximize user satisfaction (proxied by engagement). The extraction is seen as a fair exchange for the service. Negative externalities are off-balance-sheet. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analyst sees both the genuine coordination function (solving content discovery) and the highly extractive, asymmetric consequences (epistemic capture for profit). This is the canonical view that recognizes the hybrid nature of the constraint. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A regulatory body (e.g., EU) sees the problem as a market failure correctable by policy scaffolds like algorithmic transparency laws or data portability mandates. These interventions are temporary supports intended to create a healthier market, with a sunset clause implied by the eventual establishment of new norms. d≈0.40, f(d)≈0.40, σ=1.1 → χ≈0.29.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_epistemic_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_epistemic_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_epistemic_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_epistemic_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(algorithmic_epistemic_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65) is high, representing the immense value of captured attention and data, and the high cost of a distorted worldview to the user. Suppression (0.75) is high because while alternatives exist, the friction to switch (loss of social graph, network effects) is immense, effectively trapping users. The theater ratio (0.50) reflects the narrative that these platforms are neutral tools for 'connection' or 'information discovery', which masks the underlying, purely mechanical goal of engagement maximization.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The platform operator experiences a highly efficient coordination machine (Rope) that generates profit. The end-user experiences a coercive trap (Snare) that manipulates their reality for someone else's benefit. The regulator sees a correctable market failure that can be managed with temporary rules (Scaffold). The analytical observer, able to see all parts of the system, recognizes it as a Tangled Rope, where a genuine coordination function has been weaponized for asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (platforms) with arbitrage exit options have a very low directionality (d), resulting in a negative effective extraction (χ) - they see it as a Rope. Victims (users) who are trapped have a very high d, leading to a high χ that pushes the classification to Snare. Organized actors (regulators) with constrained exit options have a moderate d, allowing them to perceive the system as a problem to be solved (Scaffold), rather than an inescapable trap.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a primary exhibit for resolving mandatrophy. To label the system as only a Snare ignores the powerful, genuine coordination function that makes it so attractive and effective. To label it as only a Rope ignores the vast, demonstrable epistemic and social harm it causes. The Tangled Rope classification, from the analytical perspective, correctly holds both truths in tension, identifying the structure as a hybrid system where coordination is the delivery mechanism for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_intentionality,
    'Is the resulting epistemic harm an intentional, optimized-for outcome, or an unavoidable negative externality of optimizing for engagement?',
    'Internal documentation from platform operators; A/B testing data correlating model changes with polarization metrics.',
    'If intentional, the constraint is a pure Snare even from the analytical view. If an externality, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_intentionality, empirical, 'Distinguishing between intentional harm and negative externality').

omega_variable(
    personalization_manipulation_threshold,
    'At what point does effective content personalization become coercive manipulation?',
    'Cognitive science research on choice architecture; legal and ethical standard-setting for digital environments.',
    'Defines the boundary between the Rope (coordination) and Snare (extraction) components of the system. A low threshold for manipulation increases the perceived extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(personalization_manipulation_threshold, conceptual, 'The threshold between personalization and manipulation').

omega_variable(
    alternative_viability,
    'Can non-extractive, decentralized alternatives achieve the network effects necessary to provide a viable exit option for trapped users?',
    'Market analysis of federated social media adoption rates (e.g., Fediverse) and user retention metrics.',
    'If alternatives become viable, the ''trapped'' exit option for users becomes ''mobile'', fundamentally lowering the system''s suppression score and effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_viability, empirical, 'Viability of non-extractive alternatives to break network effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_epistemic_capture, 2008, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algo_tr_t0, algorithmic_epistemic_capture, theater_ratio, 0, 0.2).
narrative_ontology:measurement(algo_tr_t8, algorithmic_epistemic_capture, theater_ratio, 8, 0.4).
narrative_ontology:measurement(algo_tr_t15, algorithmic_epistemic_capture, theater_ratio, 15, 0.5).

% Extraction over time
narrative_ontology:measurement(algo_be_t0, algorithmic_epistemic_capture, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(algo_be_t8, algorithmic_epistemic_capture, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(algo_be_t15, algorithmic_epistemic_capture, base_extractiveness, 15, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_epistemic_capture, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_epistemic_capture, political_polarization).
narrative_ontology:affects_constraint(algorithmic_epistemic_capture, misinformation_spread).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
