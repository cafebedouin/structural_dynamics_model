% ============================================================================
% CONSTRAINT STORY: openai_default_data_training
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openai_default_data_training, []).

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
 *   constraint_id: openai_default_data_training
 *   human_readable: Default Use of ChatGPT User Data for Model Training
 *   domain: technological
 *
 * SUMMARY:
 *   OpenAI's default use of ChatGPT user data for model training presents a
 *   complex scenario where the benefits of improved AI models must be
 *   balanced against the ethical considerations of user privacy and consent.
 *   The policy extracts user information to improve the tool for general use,
 *   but individual users may find that this extraction is not fairly
 *   consented to, particularly given a difficult opt-out process. The
 *   long-term implications for data security and bias amplification are also
 *   key concerns.
 *
 * KEY AGENTS:
 *   - ChatGPT Users: Primary target (powerless/trapped or moderate/constrained) — provides data used for model training, may lack awareness or control.
 *   - OpenAI: Primary beneficiary (institutional/arbitrage) — improves its models and gains a competitive advantage.
 *   - Future ChatGPT Users: Secondary beneficiary (powerful/mobile) — benefits from improved model capabilities.
 *   - Analytical Observer: Analyzes the constraint (analytical/analytical) — considers broader ethical and societal implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_default_data_training, 0.55).
domain_priors:suppression_score(openai_default_data_training, 0.4).
domain_priors:theater_ratio(openai_default_data_training, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_default_data_training, extractiveness, 0.55).
narrative_ontology:constraint_metric(openai_default_data_training, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(openai_default_data_training, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openai_default_data_training, tangled_rope).
narrative_ontology:human_readable(openai_default_data_training, "Default Use of ChatGPT User Data for Model Training").
narrative_ontology:topic_domain(openai_default_data_training, "technological").

domain_priors:requires_active_enforcement(openai_default_data_training).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openai_default_data_training, openai).
narrative_ontology:constraint_beneficiary(openai_default_data_training, future_chatgpt_users).
narrative_ontology:constraint_victim(openai_default_data_training, chatgpt_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual users, lacking technical skills or awareness, are largely unaware of the default data use policy and find it difficult to opt-out. They bear the cost of their data being used for model training without explicit consent.
constraint_indexing:constraint_classification(openai_default_data_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% More technically aware users can attempt to opt-out, but the process may be cumbersome, and the long-term effects of data usage are uncertain. They benefit from improved models but bear some risk of data misuse. Constrained exit.
constraint_indexing:constraint_classification(openai_default_data_training, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% OpenAI benefits from using user data to improve its models, enhancing its competitive advantage and user experience. This can be seen as a coordination mechanism for improving the general utility of the AI system. Data provides direct profit to OpenAI.
constraint_indexing:constraint_classification(openai_default_data_training, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% Future users benefit from the improved models trained on previous users' data. Their experience improves through this process. Benefits outweight costs.
constraint_indexing:constraint_classification(openai_default_data_training, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% From an analytical perspective, this data usage represents a tangled rope, balancing the benefits of improved AI models with the ethical concerns around user privacy and consent. It also creates potential for bias amplification.
constraint_indexing:constraint_classification(openai_default_data_training, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_default_data_training_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openai_default_data_training, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openai_default_data_training, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openai_default_data_training, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(openai_default_data_training_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The data extraction is significant, as it directly contributes to the improvement of OpenAI's models and, consequently, their profitability and competitive advantage. Users provide their data, often without fully understanding the implications. Suppression (0.40): While users have an option to opt-out, the process is not always straightforward, and the default setting favors data collection. The lack of prominent, user-friendly consent mechanisms suppresses user control over their data. Theater ratio (0.30): The opt-out mechanism is not entirely performative, but it can be perceived as such by users who find it difficult to navigate or who are unaware of its existence.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions actors occupy in relation to the data flow. OpenAI, as the beneficiary, sees the data usage as a necessary coordination for improving the service. Future users benefit from this improvement. However, individual users, especially those unaware of the policy, experience it as a snare, with their data being used without full consent. Analytical observers recognize both the benefits and ethical concerns, leading to a tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   OpenAI benefits directly from using user data. Individual users bear the cost of their data being used, even if they are unaware of or unable to easily opt-out. Future users also benefit from the improvements the data makes to the models. The analytical perspective considers both sides.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_privacy_threshold,
    'What level of data anonymization is sufficient to protect user privacy while still enabling effective model training?',
    'Technical research into anonymization techniques and their impact on model performance; surveys of user privacy expectations.',
    'If a high level of anonymization is sufficient, the ethical concerns are mitigated. If not, stronger consent mechanisms are needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_privacy_threshold, empirical, 'Acceptable data privacy threshold').

omega_variable(
    consent_mechanism_effectiveness,
    'How effective are different consent mechanisms (e.g., opt-in vs. opt-out) in ensuring users understand and agree to the data usage policy?',
    'A/B testing of different consent mechanisms; analysis of user opt-out rates and feedback.',
    'If opt-out is sufficient, default training can continue. If opt-in is necessary, significant changes to the system are required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_mechanism_effectiveness, empirical, 'Effectiveness of different consent mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openai_default_data_training, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(open_tr_t0, openai_default_data_training, theater_ratio, 0, 0.25).
narrative_ontology:measurement(open_tr_t5, openai_default_data_training, theater_ratio, 5, 0.3).
narrative_ontology:measurement(open_tr_t10, openai_default_data_training, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(open_be_t0, openai_default_data_training, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(open_be_t5, openai_default_data_training, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(open_be_t10, openai_default_data_training, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openai_default_data_training, information_standard).
narrative_ontology:affects_constraint(openai_default_data_training, data_privacy_regulations).
narrative_ontology:affects_constraint(openai_default_data_training, ai_bias_amplification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
