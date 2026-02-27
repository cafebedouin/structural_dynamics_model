% ============================================================================
% CONSTRAINT STORY: silicon_lexicon_overload
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silicon_lexicon_overload, []).

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
 *   constraint_id: silicon_lexicon_overload
 *   human_readable: The Silicon Lexicon (Corporate Tech-Speak)
 *   domain: linguistic/technological/social
 *
 * SUMMARY:
 *   The specialized jargon of the tech industry (e.g., "bandwidth,"
 *   "alignment," "synergy") has escaped its original context and become a
 *   mandatory social and professional protocol in many corporate
 *   environments. This creates a complex dynamic where the jargon serves as a
 *   coordination mechanism within the tech industry and between specialized
 *   teams, but also extracts clarity and inclusivity from broader
 *   communication. Non-technical employees are pressured to adopt the jargon,
 *   and clear communication is suppressed by the performative use of
 *   buzzwords.
 *
 * KEY AGENTS:
 *   - Management: Primary beneficiary (institutional/arbitrage) - benefits from perceived efficiency and innovation.
 *   - Tech Consultants: Secondary beneficiary (institutional/arbitrage) - benefits from being able to speak the language of business and translate technical concepts into marketable strategies.
 *   - Non-Technical Employees: Primary victim (powerless/trapped) - forced to adopt the jargon to participate.
 *   - Clear Communication: Abstract victim (powerless/trapped) - the clarity and nuance of communication is lost in favor of buzzwords and jargon.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silicon_lexicon_overload, 0.55).
domain_priors:suppression_score(silicon_lexicon_overload, 0.7).
domain_priors:theater_ratio(silicon_lexicon_overload, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silicon_lexicon_overload, extractiveness, 0.55).
narrative_ontology:constraint_metric(silicon_lexicon_overload, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(silicon_lexicon_overload, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silicon_lexicon_overload, tangled_rope).
narrative_ontology:human_readable(silicon_lexicon_overload, "The Silicon Lexicon (Corporate Tech-Speak)").
narrative_ontology:topic_domain(silicon_lexicon_overload, "linguistic/technological/social").

domain_priors:requires_active_enforcement(silicon_lexicon_overload).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(silicon_lexicon_overload, management).
narrative_ontology:constraint_beneficiary(silicon_lexicon_overload, tech_consultants).
narrative_ontology:constraint_victim(silicon_lexicon_overload, non_technical_employees).
narrative_ontology:constraint_victim(silicon_lexicon_overload, clear_communication).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The non-technical employee is forced to adopt the jargon to participate, with limited ability to exit or influence the language.
constraint_indexing:constraint_classification(silicon_lexicon_overload, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Management benefits from the perceived efficiency and innovation signaled by the jargon, experiencing it as a coordination tool.
constraint_indexing:constraint_classification(silicon_lexicon_overload, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Mid-level managers are both pressured to use the jargon and benefit from its signaling effect within the company, but are also constrained by its limitations in actual communication.
constraint_indexing:constraint_classification(silicon_lexicon_overload, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Linguists observe the jargon as a degraded form of communication, persisting through institutional inertia but lacking functional clarity. The original function of efficient technical communication atrophies into performative adoption.
constraint_indexing:constraint_classification(silicon_lexicon_overload, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The jargon provides some coordination benefits (a shared lexicon within a technical field) but also extracts clarity from communication across groups.
constraint_indexing:constraint_classification(silicon_lexicon_overload, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silicon_lexicon_overload_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(silicon_lexicon_overload, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silicon_lexicon_overload, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(silicon_lexicon_overload, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(silicon_lexicon_overload, TR),
    TR >= 0.70.

:- end_tests(silicon_lexicon_overload_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The jargon extracts clarity and inclusivity, forcing non-technical employees to learn a new language and hindering effective communication. Suppression (0.70): High suppression. Clear communication is suppressed by the widespread use of jargon and buzzwords. The theater ratio (0.85) is high because much of the jargon is used performatively, to signal belonging and competence rather than to convey information effectively.
 *
 * PERSPECTIVAL GAP:
 *   Non-technical employees experience the jargon as a snare, while management experiences it as a rope. Mid-level managers have a tangled-rope perspective. Linguists view the jargon as a piton. The analytical observer recognizes both the coordination and extraction aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   Management benefits from the lexicon because its use reinforces authority and expertise. The mid-level manager is subject to the demands of the lexicon, and also reaps some benefit of using this jargon in signaling competence to higher ups. The non-technical employee is trapped, and forced to adopt this lexicon as a matter of course. Linguists observe this process objectively and recognize the degraded communication it represents.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by showing that the lexicon has both benefits and drawbacks. While it can facilitate communication within specialized contexts, it also extracts clarity and inclusivity. Classifying it solely as a coordination tool would ignore the negative impacts on non-technical employees and clear communication. The tangled-rope classification acknowledges both the coordination and extraction aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clarity_vs_efficiency,
    'To what extent does the lexicon obfuscate meaning versus provide efficient shorthand within specialized contexts?',
    'Comparative studies of communication effectiveness using jargon versus plain language in various settings.',
    'Determines the magnitude of the extraction from clarity and indicates whether alternative terminologies could offer equivalent coordination at a lower cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clarity_vs_efficiency, empirical, 'Quantifies the tradeoff between clarity and efficiency in lexicon use.').

omega_variable(
    signaling_value,
    'How much of the lexicon''s adoption is driven by genuine technical necessity versus social signaling?',
    'Surveys and ethnographic studies of workplace communication patterns; analysis of lexicon usage in different corporate cultures.',
    'Dictates whether the lexicon is primarily a coordination tool or a status marker. Impacts the potential for alternative linguistic strategies and the feasibility of mitigating extractive effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signaling_value, empirical, 'Assesses the proportion of adoption due to social signaling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silicon_lexicon_overload, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sili_tr_t0, silicon_lexicon_overload, theater_ratio, 0, 0.5).
narrative_ontology:measurement(sili_tr_t5, silicon_lexicon_overload, theater_ratio, 5, 0.7).
narrative_ontology:measurement(sili_tr_t10, silicon_lexicon_overload, theater_ratio, 10, 0.85).

% Extraction over time
narrative_ontology:measurement(sili_be_t0, silicon_lexicon_overload, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sili_be_t5, silicon_lexicon_overload, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(sili_be_t10, silicon_lexicon_overload, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silicon_lexicon_overload, information_standard).
narrative_ontology:affects_constraint(silicon_lexicon_overload, corporate_innovation_theater).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
