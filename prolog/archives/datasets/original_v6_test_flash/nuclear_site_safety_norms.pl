% ============================================================================
% CONSTRAINT STORY: nuclear_site_safety_norms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_site_safety_norms, []).

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
 *   constraint_id: nuclear_site_safety_norms
 *   human_readable: International Nuclear Site Non-Proliferation and Safety Norms
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   This constraint represents the system of international treaties,
 *   monitoring (e.g., by the IAEA), and geopolitical norms that prohibit
 *   military attacks on nuclear power infrastructure. The norms aim to
 *   prevent the catastrophic release of radiation that could result from such
 *   attacks. These are an important mechanism for avoiding ecological
 *   disaster. The IAEA has a mandate to monitor the world and bring awareness
 *   to any violations. This system benefits most parties but constrains
 *   strategic military actions.
 *
 * KEY AGENTS:
 *   - Civilian Populations: Potential victims of nuclear attacks (powerless/trapped)
 *   - Nuclear Energy Operators: Operators of nuclear facilities (moderate/constrained)
 *   - Major Nuclear Powers: Enforcers of the norms (powerful/mobile)
 *   - International Atomic Energy Agency (IAEA): Monitors of the norms (institutional/arbitrage)
 *   - Target Nations Strategic Flexibility: Loses ability to attack nuclear sites (organized/constrained)
 *   - Analytical Observer: Observing the long-term system behavior (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_site_safety_norms, 0.45).
domain_priors:suppression_score(nuclear_site_safety_norms, 0.5).
domain_priors:theater_ratio(nuclear_site_safety_norms, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_site_safety_norms, extractiveness, 0.45).
narrative_ontology:constraint_metric(nuclear_site_safety_norms, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(nuclear_site_safety_norms, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_site_safety_norms, tangled_rope).
narrative_ontology:human_readable(nuclear_site_safety_norms, "International Nuclear Site Non-Proliferation and Safety Norms").
narrative_ontology:topic_domain(nuclear_site_safety_norms, "geopolitical/technological").

domain_priors:requires_active_enforcement(nuclear_site_safety_norms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_site_safety_norms, civilian_populations).
narrative_ontology:constraint_beneficiary(nuclear_site_safety_norms, nuclear_energy_operators).
narrative_ontology:constraint_victim(nuclear_site_safety_norms, target_nations_strategic_flexibility).
narrative_ontology:constraint_victim(nuclear_site_safety_norms, rogue_states_proliferation_options).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Civilians near nuclear sites are trapped and powerless to exit the system of norms. The failure of the norms results in catastrophic extraction. The risk of nuclear disaster is ever-present.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Operators constrained by regulations, but benefit from the norms via security and predictability. They gain from adherence and are hurt by violations. There is some mobility, but exiting the network of regulation is expensive.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% These actors are central to enforcing the norms, and benefit in terms of global stability. They do not fully bear the costs of adherence because they have strategic flexibility and second-strike capabilities. They are mobile - can choose to violate or not, but there are consequences either way.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% The IAEA benefits from the norms via funding and influence. Their compliance mechanisms increase international cooperation. They can arbitrage information from multiple sources to achieve its monitoring mandate.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Nations that are potentially targeted by nuclear attacks lose some strategic flexibility by adhering to the norms but gain in terms of reduced risk of attack.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical observer of the long-term impacts of the norms.
constraint_indexing:constraint_classification(nuclear_site_safety_norms, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_site_safety_norms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_site_safety_norms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_site_safety_norms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(nuclear_site_safety_norms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate because the norms constrain military options. Suppression is moderate because there is still a risk of violation. Theater ratio is relatively low, because there is a direct benefit from compliance.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer notes that this is a tangled rope. Civilian populations may disagree and view this as a pure snare because of the high impact of failure, however small. Nuclear operators are more likely to see this as a rope, since they receive direct benefit. Nations might want to violate these norms under some circumstances, so they see the entanglement.
 *
 * DIRECTIONALITY LOGIC:
 *   The various agents have different structural positions. Nations need strategic options, so they don't want to give up capabilities, so these norms are a net extraction. Civilians cannot leave the area, so this is extraction for them because if things go badly, they are doomed. For nuclear operators, having the norms is beneficial because it protects their equipment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rogue_state_incentives,
    'What are the conditions under which a rogue state would violate the norms?',
    'Game-theoretic modeling of rogue state incentives',
    'High violation incentive leads to a snare classification. Low violation incentive leads to a rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rogue_state_incentives, conceptual, 'Conditions for rogue state violation').

omega_variable(
    enforcement_effectiveness,
    'How effective are the enforcement mechanisms?',
    'Statistical analysis of treaty compliance',
    'Effective enforcement leads to a rope. Ineffective enforcement leads to a piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness, empirical, 'Effectiveness of enforcement mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_site_safety_norms, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_site_safety_norms, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nucl_tr_t25, nuclear_site_safety_norms, theater_ratio, 25, 0.2).
narrative_ontology:measurement(nucl_tr_t50, nuclear_site_safety_norms, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_site_safety_norms, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(nucl_be_t25, nuclear_site_safety_norms, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(nucl_be_t50, nuclear_site_safety_norms, base_extractiveness, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_site_safety_norms, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_site_safety_norms, non_proliferation_treaty).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
