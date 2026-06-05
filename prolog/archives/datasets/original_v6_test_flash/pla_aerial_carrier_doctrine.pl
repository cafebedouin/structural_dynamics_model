% ============================================================================
% CONSTRAINT STORY: pla_aerial_carrier_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pla_aerial_carrier_doctrine, []).

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
 *   constraint_id: pla_aerial_carrier_doctrine
 *   human_readable: China's Development of an Aerial Drone Carrier Doctrine
 *   domain: technological/political
 *
 * SUMMARY:
 *   Based on reports of China developing a large, unmanned aerial vehicle
 *   (UAV) capable of launching and recovering smaller drones, this constraint
 *   models the strategic doctrine this capability enables. It examines the
 *   implications of this technology for regional stability, power projection,
 *   and the strategic balance between China and its potential adversaries.
 *
 * KEY AGENTS:
 *   - PLA: Primary beneficiary (institutional/arbitrage) - Gains enhanced power projection and surveillance capabilities.
 *   - Chinese Government: Secondary beneficiary (institutional/constrained) - Enhances geopolitical influence and territorial claims.
 *   - Potential Adversaries: Primary victim (powerless/trapped) - Face increased surveillance and strike capabilities with limited counter-options.
 *   - Regional Stability: Secondary victim (moderate/constrained) - Faces increased risk of conflict and arms races.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pla_aerial_carrier_doctrine, 0.55).
domain_priors:suppression_score(pla_aerial_carrier_doctrine, 0.6).
domain_priors:theater_ratio(pla_aerial_carrier_doctrine, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, extractiveness, 0.55).
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(pla_aerial_carrier_doctrine, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pla_aerial_carrier_doctrine, tangled_rope).
narrative_ontology:human_readable(pla_aerial_carrier_doctrine, "China's Development of an Aerial Drone Carrier Doctrine").
narrative_ontology:topic_domain(pla_aerial_carrier_doctrine, "technological/political").

domain_priors:requires_active_enforcement(pla_aerial_carrier_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pla_aerial_carrier_doctrine, pla).
narrative_ontology:constraint_beneficiary(pla_aerial_carrier_doctrine, chinese_government).
narrative_ontology:constraint_victim(pla_aerial_carrier_doctrine, potential_adversaries).
narrative_ontology:constraint_victim(pla_aerial_carrier_doctrine, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Potential adversaries, particularly smaller nations or non-state actors, find themselves trapped by the PLA's enhanced surveillance and strike capabilities. They lack the resources to effectively counter this asymmetric threat and must adapt their strategies under constant pressure.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Regional powers may see both benefits and costs. They gain access to advanced drone technology through cooperation with China, but are simultaneously constrained by China's increased influence and potential for coercion. Their exit options are limited by economic and geopolitical realities.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% The PLA benefits significantly from this doctrine, enhancing its power projection, surveillance capabilities, and strategic flexibility. They can leverage this technology for territorial claims and regional influence. Their exit option is arbitrage as they control the application of the technology.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Analytical observer assesses the technology as a tangled rope due to the combination of coordination for power projection, and asymmetric extraction of freedom of action from potential adversaries.
constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pla_aerial_carrier_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pla_aerial_carrier_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pla_aerial_carrier_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(pla_aerial_carrier_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The technology allows China to exert influence and control over disputed territories, but the effectiveness of this extraction depends on the countermeasures developed by other nations. Suppression (0.60): Moderate-High. Potential adversaries face significant barriers to countering this technology. Theater Ratio (0.30): Low. The technology is currently under development and primarily functional; the theater is low but may increase over time.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_adoption_speed,
    'How quickly will the PLA integrate this technology into its operational doctrine?',
    'Monitoring PLA training exercises, official publications, and deployment patterns.',
    'Faster adoption increases the perceived threat and accelerates regional arms races. Slower adoption gives potential adversaries more time to develop countermeasures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_adoption_speed, empirical, 'Speed of PLA''s aerial drone carrier doctrine adoption.').

omega_variable(
    countermeasure_effectiveness,
    'How effective will countermeasures be in neutralizing the threat posed by these aerial drone carriers?',
    'Testing and evaluation of anti-drone technologies, electronic warfare capabilities, and strategic deception techniques.',
    'Highly effective countermeasures reduce the extraction and shift the balance of power. Ineffective countermeasures exacerbate the security dilemma.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countermeasure_effectiveness, empirical, 'Effectiveness of countermeasures against PLA drone carriers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pla_aerial_carrier_doctrine, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pla__tr_t0, pla_aerial_carrier_doctrine, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pla__tr_t5, pla_aerial_carrier_doctrine, theater_ratio, 5, 0.2).
narrative_ontology:measurement(pla__tr_t10, pla_aerial_carrier_doctrine, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(pla__be_t0, pla_aerial_carrier_doctrine, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(pla__be_t5, pla_aerial_carrier_doctrine, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(pla__be_t10, pla_aerial_carrier_doctrine, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pla_aerial_carrier_doctrine, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
