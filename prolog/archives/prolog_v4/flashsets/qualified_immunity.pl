% ============================================================================
% CONSTRAINT STORY: qualified_immunity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qualified_immunity
 *   human_readable: Qualified Immunity Doctrine
 *   domain: political/legal
 *
 * SUMMARY:
 *   Qualified immunity is a U.S. legal doctrine that shields government
 *   officials from liability in civil lawsuits unless their conduct violates
 *   clearly established statutory or constitutional rights, and there's no
 *   precedent saying they acted wrongly. This protection is provided only for
 *   liability in civil lawsuits; qualified immunity provides no protection
 *   from criminal prosecution. The doctrine aims to balance the need to hold
 *   public officials accountable when they exercise power irresponsibly and
 *   the need to shield them from harassment, distraction, and liability when
 *   they perform their duties reasonably.
 *
 * KEY AGENTS:
 *   - Law Enforcement Officers: Primary beneficiaries, protected from liability.
 *   - Victims of Police Misconduct: Primary targets, unable to seek redress.
 *   - Municipalities: Secondary beneficiaries, shielded from financial burden of lawsuits.
 *   - Judiciary: Interprets and applies the doctrine, balancing competing interests.
 *   - Analytical Observer: Assesses the overall impact of the doctrine on justice and accountability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity, 0.6).
domain_priors:suppression_score(qualified_immunity, 0.7).
domain_priors:theater_ratio(qualified_immunity, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity, extractiveness, 0.6).
narrative_ontology:constraint_metric(qualified_immunity, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(qualified_immunity, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity, tangled_rope).
narrative_ontology:human_readable(qualified_immunity, "Qualified Immunity Doctrine").
narrative_ontology:topic_domain(qualified_immunity, "political/legal").

domain_priors:requires_active_enforcement(qualified_immunity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity, municipalities).
narrative_ontology:constraint_victim(qualified_immunity, victims_of_police_misconduct).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of victims who are often unable to seek redress for rights violations due to the immunity shield.
constraint_indexing:constraint_classification(qualified_immunity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of law enforcement officers who are protected from liability, allowing them to perform their duties without fear of frivolous lawsuits.
constraint_indexing:constraint_classification(qualified_immunity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analyzes the balance between protecting officers and providing recourse for victims, recognizing both coordination and extraction.
constraint_indexing:constraint_classification(qualified_immunity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% The judiciary finds itself upholding a doctrine with questionable origins, and of questionable value.
constraint_indexing:constraint_classification(qualified_immunity, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qualified_immunity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qualified_immunity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(qualified_immunity, TR),
    TR >= 0.70.

:- end_tests(qualified_immunity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The doctrine significantly limits the ability of victims of police misconduct to seek legal redress, effectively extracting their right to sue for damages. Suppression (0.70): High. The 'clearly established law' standard sets a high bar for overcoming qualified immunity, suppressing potential lawsuits and limiting accountability for officials. Theater Ratio (0.30): Low. The doctrine is functionally effective in shielding officials from liability, with relatively little performative activity beyond the legal process itself.
 *
 * PERSPECTIVAL GAP:
 *   Victims see a snare, law enforcement see a rope, and analytical observers see a tangled rope because the doctrine simultaneously protects officers and limits recourse for victims, creating a complex interplay of coordination and extraction. The judiciary perspective is that of a piton, as it is often forced to uphold the problematic doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement officers benefit from the protection afforded by the doctrine, experiencing it as a coordination mechanism that enables them to perform their duties without fear of frivolous lawsuits. Victims of police misconduct bear the costs of the doctrine, as it limits their ability to seek redress for rights violations. The judiciary must balance these competing interests when applying the doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The qualified immunity doctrine exhibits characteristics of both a coordination mechanism (protecting officers) and an extraction mechanism (limiting recourse for victims). The tangled rope classification captures this duality, acknowledging both the benefits and the costs of the doctrine. The competing perspectives (snare vs rope) highlight the need for careful consideration of the doctrine's impact on all stakeholders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_test,
    'How can a fair balancing test be developed and applied to effectively weigh the need to protect law enforcement officers from unwarranted liability against the imperative of providing recourse for victims of police misconduct?',
    'Develop and apply metrics to measure the degree to which qualified immunity protects officers from liability for conduct violating constitutional rights, and whether those rights were clearly established at the time.',
    'If the test favors officers, victims have difficulty obtaining redress. If the test favors victims, officers may hesitate in their duties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test, preference, 'How to balance interests.').

omega_variable(
    clearly_established_law,
    'How should "clearly established law" be defined and determined to ensure that law enforcement officers have fair notice of the legal boundaries of their conduct while also safeguarding the ability of victims to seek redress for violations of their constitutional rights?',
    'Legal scholarship, expert panels',
    'If narrowly defined, less accountability for officers. If broadly defined, more accountability for officers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_law, conceptual, 'Meaning of clearly established law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qual_tr_t5, qualified_immunity, theater_ratio, 5, 0.2).
narrative_ontology:measurement(qual_tr_t10, qualified_immunity, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(qual_be_t5, qualified_immunity, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(qual_be_t10, qualified_immunity, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(qualified_immunity, civil_asset_forfeiture).
narrative_ontology:affects_constraint(qualified_immunity, excessive_force).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
