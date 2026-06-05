% ============================================================================
% CONSTRAINT STORY: ancestral_pueblo_hydrology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ancestral_pueblo_hydrology, []).

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
 *   constraint_id: ancestral_pueblo_hydrology
 *   human_readable: Ancestral Puebloan Hydrological Debt
 *   domain: environmental/social
 *
 * SUMMARY:
 *   This constraint models the collapse of the Chaco Canyon and Mesa Verde
 *   urban centers (c. 900-1300 CE) as a consequence of exceeding hydrological
 *   carrying capacity. Early settlers benefited from abundant rainfall and
 *   fertile land, leading to population growth and increased agricultural
 *   demands. This eventually led to deforestation, soil erosion, and
 *   depletion of water resources, creating a hydrological debt that future
 *   generations had to bear. The resulting ecological damage combined with
 *   prolonged drought periods contributed to social unrest and ultimately,
 *   the abandonment of these once-thriving settlements.
 *
 * KEY AGENTS:
 *   - Early Settlers: Primary beneficiaries (powerful/mobile) – Initially benefited from resource abundance.
 *   - Late Settlers: Primary victims (powerless/trapped) – Faced the consequences of resource depletion.
 *   - Chacoan Leadership: Constrained actors (institutional/constrained) - Oversaw resource allocation but failed to prevent collapse
 *   - Archaeological Observer: Analytical (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ancestral_pueblo_hydrology, 0.6).
domain_priors:suppression_score(ancestral_pueblo_hydrology, 0.7).
domain_priors:theater_ratio(ancestral_pueblo_hydrology, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, extractiveness, 0.6).
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ancestral_pueblo_hydrology, tangled_rope).
narrative_ontology:human_readable(ancestral_pueblo_hydrology, "Ancestral Puebloan Hydrological Debt").
narrative_ontology:topic_domain(ancestral_pueblo_hydrology, "environmental/social").

domain_priors:requires_active_enforcement(ancestral_pueblo_hydrology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ancestral_pueblo_hydrology, early_settlers).
narrative_ontology:constraint_victim(ancestral_pueblo_hydrology, late_settlers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of late settlers who experience the full brunt of hydrological debt as a snare, with limited exit options due to established societal structures and resource depletion.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% Perspective of Chacoan leadership who benefitted initially but were ultimately constrained by the environmental limits and increasing social strain.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective of an archaeologist who can see the long-term impact but has no active role in the system.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective of early settlers who initially benefited from resource exploitation.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancestral_pueblo_hydrology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ancestral_pueblo_hydrology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ancestral_pueblo_hydrology, TR),
    TR >= 0.70.

:- end_tests(ancestral_pueblo_hydrology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.6) because the system extracted resources beyond its sustainable capacity. Suppression is also high (0.7) due to the lack of viable alternatives for later generations. Theater Ratio is now set to 0.75 to reflect the performative aspects of rituals intended to ensure continued resource availability.
 *
 * PERSPECTIVAL GAP:
 *   Early settlers see the initial benefits as coordination (rope), while later settlers experience the hydrological debt as a trap (snare). Chacoan leadership might have initially viewed the system as rope, but were ultimately constrained and failed to address the issue. The archaeologist sees the long-term piton outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Early settlers benefited, d close to 0. Late settlers bore costs, d close to 1. Leadership was caught in the middle and initially benefited. Archeologist is analytical.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_change_vs_resource_management,
    'To what extent was the collapse caused by climate change versus unsustainable resource management practices?',
    'Paleoclimatic data analysis combined with archaeological evidence of resource use.',
    'Different classifications depending on which factor was dominant. If climate, more mountain-like. If management, more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_change_vs_resource_management, empirical, 'Relative contribution of climate change vs resource management.').

omega_variable(
    social_complexity_threshold,
    'At what level of social complexity did the system become unsustainable?',
    'Correlation of population size, social stratification, and environmental degradation.',
    'Impacts the power/exit options assigned to victims and the nature of the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_complexity_threshold, empirical, 'Social complexity threshold for unsustainability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ancestral_pueblo_hydrology, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ance_tr_t0, ancestral_pueblo_hydrology, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ance_tr_t50, ancestral_pueblo_hydrology, theater_ratio, 50, 0.5).
narrative_ontology:measurement(ance_tr_t100, ancestral_pueblo_hydrology, theater_ratio, 100, 0.75).

% Extraction over time
narrative_ontology:measurement(ance_be_t0, ancestral_pueblo_hydrology, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ance_be_t50, ancestral_pueblo_hydrology, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(ance_be_t100, ancestral_pueblo_hydrology, base_extractiveness, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ancestral_pueblo_hydrology, deforestation_impact).
narrative_ontology:affects_constraint(ancestral_pueblo_hydrology, soil_erosion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
