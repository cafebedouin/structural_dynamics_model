% ============================================================================
% CONSTRAINT STORY: copyleft_viral_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyleft_viral_licensing, []).

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
 *   constraint_id: copyleft_viral_licensing
 *   human_readable: Copyleft Viral Licensing (e.g., GPL)
 *   domain: technological/legal
 *
 * SUMMARY:
 *   Copyleft licensing, exemplified by the GNU General Public License (GPL),
 *   is a legal strategy that uses copyright law to guarantee that software
 *   and its derivative works remain free and open-source. This is achieved by
 *   requiring that any modified or extended versions of the software also be
 *   licensed under the same copyleft license, ensuring a 'viral' effect of
 *   openness. However, this creates a tension between the freedom to use and
 *   modify software and the restrictions placed on creating proprietary
 *   derivative works.
 *
 * KEY AGENTS:
 *   - Proprietary Software Developers: Primary target (powerless/trapped) - Restricted from creating proprietary derivative works
 *   - Copyleft Community: Primary beneficiary (organized/arbitrage) - Benefits from the assurance of software freedom and continued openness
 *   - End Users: Secondary beneficiary (institutional/arbitrage) - Benefits from the availability of free software
 *   - Dual Licensing Companies: Moderate actors (powerful/mobile) - Balance between open source and proprietary models
 *   - Analytical Observer: Civilizational perspective (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyleft_viral_licensing, 0.5).
domain_priors:suppression_score(copyleft_viral_licensing, 0.6).
domain_priors:theater_ratio(copyleft_viral_licensing, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyleft_viral_licensing, extractiveness, 0.5).
narrative_ontology:constraint_metric(copyleft_viral_licensing, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(copyleft_viral_licensing, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyleft_viral_licensing, tangled_rope).
narrative_ontology:human_readable(copyleft_viral_licensing, "Copyleft Viral Licensing (e.g., GPL)").
narrative_ontology:topic_domain(copyleft_viral_licensing, "technological/legal").

domain_priors:requires_active_enforcement(copyleft_viral_licensing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyleft_viral_licensing, copyleft_community).
narrative_ontology:constraint_beneficiary(copyleft_viral_licensing, end_users).
narrative_ontology:constraint_victim(copyleft_viral_licensing, proprietary_software_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Developers who wish to create proprietary software are effectively excluded from using copyleft-licensed code. They are trapped if they want to build upon existing copyleft projects but cannot release their derivative work under a proprietary license.
constraint_indexing:constraint_classification(copyleft_viral_licensing, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% The copyleft community benefits from the viral nature of the license, as it ensures that improvements and derivatives remain free and open. They can fork projects and have recourse to legal enforcement if the license is violated.
constraint_indexing:constraint_classification(copyleft_viral_licensing, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Companies that offer dual licensing models (e.g., open-source and commercial licenses) experience both benefits and constraints. They can leverage the open-source community while also extracting value through commercial licenses. They are mobile, as they can change their licensing strategy.
constraint_indexing:constraint_classification(copyleft_viral_licensing, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% End users benefit from the availability of free software and the assurance that it will remain free in the future. They have the arbitrage option of choosing between free and proprietary solutions.
constraint_indexing:constraint_classification(copyleft_viral_licensing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer sees a mixed coordination and extraction mechanism. Copyleft promotes collaboration and ensures software freedom but also restricts the choices of some developers.
constraint_indexing:constraint_classification(copyleft_viral_licensing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyleft_viral_licensing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyleft_viral_licensing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyleft_viral_licensing, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyleft_viral_licensing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyleft_viral_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): Moderate. Copyleft extracts from proprietary developers by preventing them from incorporating copyleft code into proprietary projects. Suppression (0.60): High. Copyleft effectively suppresses the creation of proprietary derivatives of copyleft-licensed code. Theater Ratio (0.30): Low. Copyleft licensing is more functional than theatrical; its main purpose is to ensure software freedom and openness, and it achieves this directly through legal mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the conflicting interests of different stakeholders. Proprietary software developers view copyleft as a snare, as it restricts their ability to create closed-source software. The copyleft community, on the other hand, sees it as a rope, as it enables the continued freedom and openness of software. Dual Licensing Companies see a tangled rope. The analytical observer sees both aspects and classifies it as tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary developers are victims because they lose the option to create proprietary derivatives. The copyleft community benefits because the viral effect ensures that derived works remain open. Dual licensing companies experience a mix of both. End users benefit from the guarantee of perpetual software freedom. Directionality is determined by the power, exit options, and beneficiary/victim status of each agent.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_derivative_work,
    'How broadly should ''derivative work'' be defined?',
    'Legal precedent and community consensus.',
    'A broad definition strengthens the viral effect but may discourage contributions. A narrow definition reduces the license''s effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_derivative_work, conceptual, 'Defines the scope of derivative work under the copyleft license.').

omega_variable(
    enforcement_effectiveness,
    'How effectively can copyleft licenses be enforced?',
    'Court cases and legal analysis.',
    'Effective enforcement deters violations and maintains software freedom. Weak enforcement undermines the license''s purpose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness, empirical, 'Determines the effectiveness of copyleft license enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyleft_viral_licensing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyleft_viral_licensing, theater_ratio, 0, 0.2).
narrative_ontology:measurement(copy_tr_t5, copyleft_viral_licensing, theater_ratio, 5, 0.3).
narrative_ontology:measurement(copy_tr_t10, copyleft_viral_licensing, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyleft_viral_licensing, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(copy_be_t5, copyleft_viral_licensing, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(copy_be_t10, copyleft_viral_licensing, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyleft_viral_licensing, information_standard).
narrative_ontology:affects_constraint(copyleft_viral_licensing, open_source_development_model).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
