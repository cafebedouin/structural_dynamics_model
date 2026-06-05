% ============================================================================
% CONSTRAINT STORY: australia_social_ban_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_australia_social_ban_2026, []).

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
 *   constraint_id: australia_social_ban_2026
 *   human_readable: Australian Under-16 Social Media Ban
 *   domain: political/technological
 *
 * SUMMARY:
 *   Effective December 2025, Australia's ban on social media for under-16s
 *   shifts the digital burden of proof onto platforms. While touted as a
 *   child protection measure, it also creates a complex landscape of
 *   enforcement, access limitations, and potential circumvention, impacting
 *   various stakeholders differently.
 *
 * KEY AGENTS:
 *   - Australian Government: Beneficiary (institutional/arbitrage) - Gains political capital and asserts regulatory control.
 *   - Social Media Platforms: Beneficiary (powerful/mobile) - Adapts to regulatory clarity and develops new technologies.
 *   - Australian Under-16s: Victim (powerless/trapped) - Limited access and potential isolation.
 *   - Parents of Australian Under-16s: Victim (moderate/constrained) - Responsibility of enforcement and potential conflict with children.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(australia_social_ban_2026, 0.55).
domain_priors:suppression_score(australia_social_ban_2026, 0.7).
domain_priors:theater_ratio(australia_social_ban_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(australia_social_ban_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(australia_social_ban_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(australia_social_ban_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(australia_social_ban_2026, tangled_rope).
narrative_ontology:human_readable(australia_social_ban_2026, "Australian Under-16 Social Media Ban").
narrative_ontology:topic_domain(australia_social_ban_2026, "political/technological").

domain_priors:requires_active_enforcement(australia_social_ban_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(australia_social_ban_2026, australian_government).
narrative_ontology:constraint_beneficiary(australia_social_ban_2026, social_media_platforms).
narrative_ontology:constraint_victim(australia_social_ban_2026, australian_under_16s).
narrative_ontology:constraint_victim(australia_social_ban_2026, parents_of_australian_under_16s).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Under-16s in Australia experience the ban as a snare, as they are deprived of access to social media platforms, potentially limiting their social interactions and access to information. Their exit options are limited within the Australian jurisdiction.
constraint_indexing:constraint_classification(australia_social_ban_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Parents experience a tangled rope. They benefit from the perceived safety and reduced screen time for their children but are also burdened with the responsibility of enforcement and potentially face strained relationships with children seeking access.
constraint_indexing:constraint_classification(australia_social_ban_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The Australian government views the ban as a rope, a means to coordinate the protection of minors online and assert regulatory control. They benefit from the perceived goodwill and political capital.
constraint_indexing:constraint_classification(australia_social_ban_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Social media platforms initially bear the cost of enforcement but ultimately benefit from a clearer regulatory landscape and the potential for more robust age verification technologies, leading to a scaffold classification as they adapt and find new revenue streams. They can exit by adapting business models and developing services outside Australia.
constraint_indexing:constraint_classification(australia_social_ban_2026, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% From an analytical perspective, the ban represents a tangled rope, exhibiting both coordination (protection of minors) and extraction (limitation of access and potential data collection). The long-term effects on societal norms and technological development are uncertain.
constraint_indexing:constraint_classification(australia_social_ban_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(australia_social_ban_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(australia_social_ban_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(australia_social_ban_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(australia_social_ban_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(australia_social_ban_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate - The ban extracts access from under-16s. Suppression (0.70): High - The ban actively suppresses access. Theater Ratio (0.30): Relatively low, as the enforcement has a material effect.
 *
 * PERSPECTIVAL GAP:
 *   Under-16s experience a snare while the government sees a rope. This is because the ban directly restricts the access and freedoms of the under-16s. The government focuses on the coordination aspect, but the unintended consequences on minors are real. The analytical observer sees a complex mix of extraction and coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is based on the beneficiaries and victims. The Australian government benefits in public perception, while social media platforms benefit from clearer regulations. Under-16s and parents are negatively impacted.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_age_verification,
    'How effective are age verification technologies in preventing under-16s from accessing social media?',
    'Empirical studies on the accuracy and bypass rates of age verification systems.',
    'High effectiveness could justify the ban as a successful coordination mechanism; low effectiveness would indicate a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_age_verification, empirical, 'Effectiveness of age verification measures.').

omega_variable(
    unintended_consequences,
    'What are the unintended consequences of the ban, such as increased use of VPNs or alternative platforms?',
    'Data analysis on internet traffic and usage patterns among Australian under-16s.',
    'Significant unintended consequences could shift the classification towards a snare, highlighting the limitations of the ban.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unintended_consequences, empirical, 'Potential for unintended consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(australia_social_ban_2026, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aust_tr_t0, australia_social_ban_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(aust_tr_t3, australia_social_ban_2026, theater_ratio, 3, 0.25).
narrative_ontology:measurement(aust_tr_t5, australia_social_ban_2026, theater_ratio, 5, 0.3).

% Extraction over time
narrative_ontology:measurement(aust_be_t0, australia_social_ban_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(aust_be_t3, australia_social_ban_2026, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(aust_be_t5, australia_social_ban_2026, base_extractiveness, 5, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(australia_social_ban_2026, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
