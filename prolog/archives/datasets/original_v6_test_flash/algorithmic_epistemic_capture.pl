% ============================================================================
% CONSTRAINT STORY: algorithmic_epistemic_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
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
 *   In this scenario, individuals are increasingly reliant on algorithms to
 *   curate their information intake. These algorithms, optimized for
 *   engagement, inadvertently create feedback loops that reinforce existing
 *   beliefs, regardless of their accuracy. This leads to a cognitive echo
 *   chamber, limiting exposure to diverse perspectives and critical thinking.
 *
 * KEY AGENTS:
 *   - Content Platforms: Primary beneficiaries (institutional/constrained) - benefit from increased user engagement and advertising revenue.
 *   - Advertisers: Secondary beneficiaries (powerful/mobile) - benefit from targeted advertising.
 *   - Individual Cognitive Diversity: Primary victim (powerless/trapped) - suffers from limited exposure to diverse perspectives.
 *   - Societal Epistemic Commons: Victim (moderate/constrained) - degraded by the fragmentation of shared knowledge.
 *   - Academic Researchers: Neutral (organized/mobile) - study the effects of algorithmic filtering.
 *   - The Analytical Observer: Sees the big picture (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_epistemic_capture, 0.6).
domain_priors:suppression_score(algorithmic_epistemic_capture, 0.7).
domain_priors:theater_ratio(algorithmic_epistemic_capture, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, extractiveness, 0.6).
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(algorithmic_epistemic_capture, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_epistemic_capture, snare).
narrative_ontology:human_readable(algorithmic_epistemic_capture, "The Feedback Loop Blindfold").
narrative_ontology:topic_domain(algorithmic_epistemic_capture, "technological/social/cognitive").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_epistemic_capture, content_platforms).
narrative_ontology:constraint_beneficiary(algorithmic_epistemic_capture, advertisers).
narrative_ontology:constraint_victim(algorithmic_epistemic_capture, individual_cognitive_diversity).
narrative_ontology:constraint_victim(algorithmic_epistemic_capture, societal_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Trapped in a filter bubble, the individual is unaware of alternative perspectives and lacks the means to escape the algorithmic feed.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Platforms benefit from increased engagement but are also constrained by the need to maintain user trust and avoid regulatory scrutiny.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The sharing of knowledge that enables people to collectively create new knowledge and beliefs. As more knowledge is filtered by algorithms, this commons is degraded and becomes more extractive and less useful for the average person.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Researchers benefit from grant funding and academic publications, yet also face significant challenges in gathering user data and overcoming the filter bubbles that hinder scientific progress.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Advertisers benefit from targeting consumers with personalized advertisements, while also suffering from concerns about brand safety and declining relevance as user attention fragments into ever smaller filter bubbles.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Sees the system as a tangled rope where incentives align to keep the user engaged in a loop of personalized content, extracting attention and reinforcing existing beliefs, whether accurate or not.
constraint_indexing:constraint_classification(algorithmic_epistemic_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   The extractiveness is high (0.60) because the individual's attention and cognitive resources are being captured by the algorithm, limiting their ability to explore alternative viewpoints. Suppression (0.70) is also high as the algorithm actively filters out dissenting opinions, creating a closed information environment. The theater_ratio is low (0.30) as the algorithm's function is primarily to engage, rather than to deceive or obfuscate.
 *
 * PERSPECTIVAL GAP:
 *   The individual, trapped within the filter bubble, experiences a loss of cognitive diversity and critical thinking, classifying it as a snare. Platforms and advertisers, however, might view the system as a tangled rope. They gain benefits from increased engagement and revenue, while also bearing the risk of user backlash or regulatory intervention. The analytical observer recognizes the emergent snare, a system optimized for engagement that unintentionally leads to widespread epistemic capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the content platforms and advertisers. Victims are individual cognitive diversity and societal epistemic commons. Content platforms and advertisers benefit from high user engagement, and user engagement is increased when the algorithm feeds the user only information they agree with. Individual users and the societal epistemic commons suffer as diversity of information is diminished, harming cognitive diversity and overall societal knowledge.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_vs_accuracy,
    'To what extent can engagement-optimized algorithms also promote accurate information?',
    'Empirical studies comparing the performance of different algorithms in promoting both engagement and accuracy; analysis of the trade-offs between these two objectives',
    'If algorithms can effectively promote accuracy while maintaining engagement, then the problem is a coordination failure that can be solved with better algorithms. If there is a fundamental trade-off, then the problem is a more serious extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engagement_vs_accuracy, empirical, 'The trade-off between engagement and accuracy in algorithmic recommendations').

omega_variable(
    user_awareness_and_agency,
    'To what extent are individuals aware of and able to resist the influence of algorithmic filter bubbles?',
    'Surveys and experiments measuring user awareness and control over algorithmic recommendations; analysis of the effectiveness of different interventions aimed at promoting critical thinking and media literacy',
    'If individuals are largely unaware and unable to resist, then the problem is a serious form of epistemic capture. If individuals are aware and able to resist, then the problem is less serious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_awareness_and_agency, empirical, 'User awareness of algorithmic filter bubbles').

omega_variable(
    societal_polarization,
    'To what extent is algorithmic filtering contributing to societal polarization and fragmentation?',
    'Statistical analysis of the relationship between algorithmic filtering and measures of societal polarization; case studies of specific events or issues where algorithmic filtering may have played a significant role',
    'If algorithmic filtering is a major driver of polarization, then the societal consequences are significant. If algorithmic filtering is less important, then the societal consequences are less significant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(societal_polarization, empirical, 'The impact of algorithmic filtering on societal polarization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_epistemic_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algo_tr_t0, algorithmic_epistemic_capture, theater_ratio, 0, 0.1).
narrative_ontology:measurement(algo_tr_t5, algorithmic_epistemic_capture, theater_ratio, 5, 0.2).
narrative_ontology:measurement(algo_tr_t10, algorithmic_epistemic_capture, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(algo_be_t0, algorithmic_epistemic_capture, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(algo_be_t5, algorithmic_epistemic_capture, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(algo_be_t10, algorithmic_epistemic_capture, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_epistemic_capture, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
