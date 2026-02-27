% ============================================================================
% CONSTRAINT STORY: hygiene_disposal_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hygiene_disposal_protocol, []).

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
 *   constraint_id: hygiene_disposal_protocol
 *   human_readable: Institutional Hygiene Protocol (Incineration)
 *   domain: clinical/institutional
 *
 * SUMMARY:
 *   This protocol involves the mandatory incineration of personal belongings
 *   of patients diagnosed with Scarlet Fever (or other highly contagious
 *   disease) to prevent the spread of infection within an institutional
 *   setting. This safety measure comes at the expense of the patients'
 *   personal property and, potentially, emotional well-being. The policy
 *   requires the suppression of any possible alternative actions that
 *   patients might wish to perform, such as cleaning and reclaiming their own
 *   belongings.
 *
 * KEY AGENTS:
 *   - Patients' Personal Effects: Primary target (powerless/trapped) — bears extraction (loss of belongings).
 *   - Waste Management Personnel: Secondary target (moderate/constrained) — bear extraction (risk of exposure).
 *   - Hospital Administrators: Primary beneficiary (institutional/arbitrage) — benefit from reduced legal/financial risk.
 *   - Public Health Officials: Secondary beneficiary (institutional/arbitrage) — benefit from disease control.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hygiene_disposal_protocol, 0.6).
domain_priors:suppression_score(hygiene_disposal_protocol, 0.8).
domain_priors:theater_ratio(hygiene_disposal_protocol, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hygiene_disposal_protocol, extractiveness, 0.6).
narrative_ontology:constraint_metric(hygiene_disposal_protocol, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(hygiene_disposal_protocol, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hygiene_disposal_protocol, snare).
narrative_ontology:human_readable(hygiene_disposal_protocol, "Institutional Hygiene Protocol (Incineration)").
narrative_ontology:topic_domain(hygiene_disposal_protocol, "clinical/institutional").

domain_priors:requires_active_enforcement(hygiene_disposal_protocol).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hygiene_disposal_protocol, hospital_administrators).
narrative_ontology:constraint_beneficiary(hygiene_disposal_protocol, public_health_officials).
narrative_ontology:constraint_victim(hygiene_disposal_protocol, patients_personal_effects).
narrative_ontology:constraint_victim(hygiene_disposal_protocol, waste_management_personnel).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The disposal protocol leads to a complete loss of patient belongings with no recourse.
constraint_indexing:constraint_classification(hygiene_disposal_protocol, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Workers are required to handle potentially infectious materials, accepting risk in service of public health.
constraint_indexing:constraint_classification(hygiene_disposal_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% This is seen as a necessary measure for preventing widespread outbreaks.
constraint_indexing:constraint_classification(hygiene_disposal_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% While adhering to protocol, they balance cost, legal liability and public health concerns.
constraint_indexing:constraint_classification(hygiene_disposal_protocol, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% This is viewed as a tradeoff between individual rights and public health imperatives.
constraint_indexing:constraint_classification(hygiene_disposal_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hygiene_disposal_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hygiene_disposal_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hygiene_disposal_protocol, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hygiene_disposal_protocol, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hygiene_disposal_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. The incineration protocol results in a direct and uncompensated loss of personal property for patients. Suppression (0.8): Very High. Patients have no say in the matter; their belongings are taken and destroyed regardless of their wishes. Theater Ratio (0.3): Low. The protocol is implemented with little ceremony and minimal public display.
 *
 * PERSPECTIVAL GAP:
 *   The protocol appears as a Snare to the patient due to the loss of their possessions. To Public Health Officials, it is seen as a Rope - pure coordination in defense of public health. The Waste Management Personnel see this as Tangled Rope because they incur some extra risk to their own personal safety.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients bear the cost (d=1) and have no exit (trapped), and thus experience high extraction. Public Health Officials benefit (d=0) and can shift focus (arbitrage), and thus perceive coordination. Waste Management Personnel have some agency (constrained), placing them between the extremes.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patient_autonomy_vs_public_health,
    'What is the relative value of patient autonomy and the right to personal property versus public health interests?',
    'A legal and ethical determination based on prevailing social values and risk assessments.',
    'If patient autonomy is paramount, the protocol may be modified or abandoned. If public health is prioritized, the protocol will likely remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_autonomy_vs_public_health, preference, 'Preference assigned to patient autonomy.').

omega_variable(
    availability_of_alternative_methods,
    'Are there alternative methods of disinfection and disposal that are equally effective but less intrusive?',
    'Scientific research into new disinfection technologies and methods.',
    'If alternative methods are found, the current protocol may be relaxed or replaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(availability_of_alternative_methods, empirical, 'Availability of alternative disposal methods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hygiene_disposal_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hygi_tr_t0, hygiene_disposal_protocol, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hygi_tr_t5, hygiene_disposal_protocol, theater_ratio, 5, 0.2).
narrative_ontology:measurement(hygi_tr_t10, hygiene_disposal_protocol, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(hygi_be_t0, hygiene_disposal_protocol, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(hygi_be_t5, hygiene_disposal_protocol, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(hygi_be_t10, hygiene_disposal_protocol, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hygiene_disposal_protocol, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
