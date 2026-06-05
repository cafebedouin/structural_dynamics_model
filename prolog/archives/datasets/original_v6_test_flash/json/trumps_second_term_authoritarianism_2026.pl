% ============================================================================
% CONSTRAINT STORY: trumps_second_term_authoritarianism_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [DRAFT]
% ============================================================================

:- module(constraint_trumps_second_term_authoritarianism_2026, []).

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
 *   constraint_id: trumps_second_term_authoritarianism_2026
 *   human_readable: Electoral Authoritarianism (Trump II, Year One)
 *   domain: political
 *
 * SUMMARY:
 *   A year into a hypothetical second Trump term, the U.S. experiences a
 *   decline in democratic norms characterized by increased executive power,
 *   politicization of government agencies, suppression of dissent, and
 *   attacks on independent media. This scenario examines the nature of this
 *   authoritarian drift, classifying its structural role from different
 *   perspectives. Extractiveness arises from the diversion of public
 *   resources, the erosion of institutional checks, and the suppression of
 *   voter access. Suppression arises from the limitation of public discourse,
 *   the chilling effect of government rhetoric, and the imposition of legal
 *   and administrative barriers to political participation.
 *
 * KEY AGENTS:
 *   - Trump Administration: Primary beneficiary (institutional/arbitrage) – gains power and reduces constraints.
 *   - Democratic Institutions: Primary victim (powerless/trapped) – lose independence and ability to check executive power.
 *   - Independent Media: Secondary victim (moderate/constrained) – faces pressure and limitations on access and reporting.
 *   - Civil Society Organizations: Potential countervailing force (organized/mobile) – can mobilize resistance and expose abuses.
 *   - Loyalist Officials: Beneficiaries (moderate/constrained) - support for trump administration is favored.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trumps_second_term_authoritarianism_2026, 0.6).
domain_priors:suppression_score(trumps_second_term_authoritarianism_2026, 0.7).
domain_priors:theater_ratio(trumps_second_term_authoritarianism_2026, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trumps_second_term_authoritarianism_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(trumps_second_term_authoritarianism_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(trumps_second_term_authoritarianism_2026, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trumps_second_term_authoritarianism_2026, tangled_rope).
narrative_ontology:human_readable(trumps_second_term_authoritarianism_2026, "Electoral Authoritarianism (Trump II, Year One)").
narrative_ontology:topic_domain(trumps_second_term_authoritarianism_2026, "political").

domain_priors:requires_active_enforcement(trumps_second_term_authoritarianism_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trumps_second_term_authoritarianism_2026, trump_administration).
narrative_ontology:constraint_beneficiary(trumps_second_term_authoritarianism_2026, loyalist_officials).
narrative_ontology:constraint_victim(trumps_second_term_authoritarianism_2026, democratic_institutions).
narrative_ontology:constraint_victim(trumps_second_term_authoritarianism_2026, voter_access).
narrative_ontology:constraint_victim(trumps_second_term_authoritarianism_2026, independent_media).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Democratic institutions (rule of law, independent judiciary) are trapped and bear the full cost of authoritarian drift.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Independent media faces constraints (threats, limited access) but also retains some ability to report and hold power accountable.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The Trump administration benefits from the constraint as coordination; it creates a more favorable environment for policy implementation and reduces oversight.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Civil society organizations are organized, have some exit options (ability to relocate resources, publicize abuses), but are still affected by the overall decline in democratic norms.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% The Republican party has largely become performative, as the party does not challenge Trump's power.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% An analytical observer sees the mixed coordination and extraction of electoral authoritarianism, affecting the entire system.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trumps_second_term_authoritarianism_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trumps_second_term_authoritarianism_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trumps_second_term_authoritarianism_2026, TR),
    TR >= 0.70.

:- end_tests(trumps_second_term_authoritarianism_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is estimated at 0.60 because while democratic institutions are weakened, they are not completely dismantled. Suppression is 0.70 reflecting a significant but not total curtailment of dissent and independent reporting. Theater ratio is 0.75, reflecting some performative compliance with democratic procedures, though often hollow.
 *
 * PERSPECTIVAL GAP:
 *   Democratic institutions see a Snare because they are trapped and bear the full cost. Independent media sees Tangled Rope because they are constrained but can still operate. The Trump administration sees Rope because it benefits. Organized civil society can see a way out. A Republican Party can see that their power is a piton, now performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Trump admin, loyalist officials) experience the constraint as coordination (low d). Victims (democratic institutions, voter access, independent media) experience it as extraction (high d). Organized agents (civil society) have a mix.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_resistance_threshold,
    'At what level of institutional resistance does the authoritarian drift halt or reverse?',
    'Monitor court challenges, congressional oversight, and agency independence; correlate with policy outcomes and public opinion.',
    'High resistance: Tangled Rope transitions to Scaffold. Low resistance: Tangled Rope solidifies into Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_resistance_threshold, empirical, 'Level of institutional resistance required to halt authoritarian drift').

omega_variable(
    public_opinion_tipping_point,
    'What is the tipping point in public opinion where support for authoritarian measures declines significantly?',
    'Track polling data on support for democratic norms, trust in government, and approval of specific policies.',
    'If support declines sharply: potential for reversal. If support remains stable: consolidation of authoritarian power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_opinion_tipping_point, empirical, 'Tipping point in public opinion regarding authoritarian measures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trumps_second_term_authoritarianism_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trum_tr_t0, trumps_second_term_authoritarianism_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(trum_tr_t6, trumps_second_term_authoritarianism_2026, theater_ratio, 6, 0.7).
narrative_ontology:measurement(trum_tr_t12, trumps_second_term_authoritarianism_2026, theater_ratio, 12, 0.75).

% Extraction over time
narrative_ontology:measurement(trum_be_t0, trumps_second_term_authoritarianism_2026, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(trum_be_t6, trumps_second_term_authoritarianism_2026, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(trum_be_t12, trumps_second_term_authoritarianism_2026, base_extractiveness, 12, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trumps_second_term_authoritarianism_2026, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
