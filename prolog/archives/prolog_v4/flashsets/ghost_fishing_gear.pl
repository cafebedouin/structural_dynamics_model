% ============================================================================
% CONSTRAINT STORY: ghost_fishing_gear
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ghost_fishing_gear, []).

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
 *   constraint_id: ghost_fishing_gear
 *   human_readable: Persistence of Abandoned, Lost, or Discarded Fishing Gear (ALDFG)
 *   domain: ecological/economic
 *
 * SUMMARY:
 *   Industrial fishing operations frequently lose or abandon gear (nets,
 *   traps, lines) at sea. This abandoned, lost, or discarded fishing gear
 *   (ALDFG), also known as 'ghost gear,' continues to fish unattended,
 *   entangling marine life, damaging habitats, and disrupting food webs. The
 *   persistence of ALDFG poses a significant threat to marine ecosystems and
 *   coastal communities, representing a complex problem with ecological,
 *   economic, and social dimensions.
 *
 * KEY AGENTS:
 *   - Marine Ecosystems: Primary target (powerless/trapped) - bears the brunt of the ecological damage from ALDFG.
 *   - Small-Scale Fishers: Secondary target (moderate/constrained) - suffers economic losses due to reduced fish stocks and gear damage.
 *   - Commercial Fishing Fleets: Primary beneficiary (institutional/arbitrage) - benefits from reduced operational costs associated with abandoning gear.
 *   - Environmental NGOs: Organized actor (organized/constrained) - works to mitigate the ALDFG problem through cleanup efforts and policy advocacy.
 *   - International Fisheries Management Organizations: Institutional actor (institutional/constrained) - responsible for sustainable fisheries management but often ineffective in addressing ALDFG.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ghost_fishing_gear, 0.65).
domain_priors:suppression_score(ghost_fishing_gear, 0.7).
domain_priors:theater_ratio(ghost_fishing_gear, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ghost_fishing_gear, extractiveness, 0.65).
narrative_ontology:constraint_metric(ghost_fishing_gear, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ghost_fishing_gear, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ghost_fishing_gear, snare).
narrative_ontology:human_readable(ghost_fishing_gear, "Persistence of Abandoned, Lost, or Discarded Fishing Gear (ALDFG)").
narrative_ontology:topic_domain(ghost_fishing_gear, "ecological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ghost_fishing_gear, commercial_fishing_fleets).
narrative_ontology:constraint_victim(ghost_fishing_gear, marine_ecosystems).
narrative_ontology:constraint_victim(ghost_fishing_gear, small_scale_fishers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Marine Ecosystems (Snare) - Marine ecosystems are the primary victims, facing ongoing harm from ghost fishing. They are essentially trapped, unable to escape the persistent threat. This results in a high degree of extractiveness.
constraint_indexing:constraint_classification(ghost_fishing_gear, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Small-Scale Fishers (Tangled Rope) - Small-scale fishers are negatively impacted by ALDFG, which reduces fish stocks and damages their gear. They are constrained in their ability to address the problem due to limited resources and political influence. However, they also benefit somewhat from cleaner oceans and better fishing grounds if ALDFG is reduced. This creates a mixed experience of extraction and coordination.
constraint_indexing:constraint_classification(ghost_fishing_gear, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective 3: Commercial Fishing Fleets (Rope) - From the perspective of large commercial fishing fleets, the loss of gear is primarily a coordination problem. While they may incur some costs from lost gear, they also benefit from reduced regulation and lower operational expenses associated with not retrieving damaged or lost equipment.
constraint_indexing:constraint_classification(ghost_fishing_gear, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Environmental NGOs (Scaffold) - Environmental NGOs see the ALDFG issue as a coordination problem that can be addressed through organized efforts such as cleanup initiatives, policy advocacy, and technological solutions. They are somewhat constrained by funding and political barriers but believe their efforts can eventually lead to a sunset of the ALDFG problem through increased awareness and better management practices.
constraint_indexing:constraint_classification(ghost_fishing_gear, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 5: International Fisheries Management Organizations (Piton) - These organizations are intended to manage fisheries sustainably but often lack the resources and enforcement capabilities to effectively address ALDFG. Their function has degraded over time, becoming more of a performative exercise than a functional solution. They are constrained in their ability to enforce regulations due to political pressures and jurisdictional limitations.
constraint_indexing:constraint_classification(ghost_fishing_gear, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 6: The Analytical Observer (Tangled Rope) - From a global, long-term perspective, the ALDFG problem represents a tangled rope. It involves both coordination failures (lack of incentives for responsible fishing practices) and extraction (damage to marine ecosystems and small-scale fisheries). Active enforcement and international cooperation are needed to mitigate the problem.
constraint_indexing:constraint_classification(ghost_fishing_gear, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ghost_fishing_gear_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ghost_fishing_gear, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ghost_fishing_gear, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ghost_fishing_gear, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ghost_fishing_gear, TR),
    TR >= 0.70.

:- end_tests(ghost_fishing_gear_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. ALDFG causes significant and ongoing harm to marine ecosystems, extracting resources and disrupting ecological balance. Suppression (0.70): High. The problem persists due to lack of enforcement, inadequate incentives for responsible gear management, and the difficulty of retrieving gear from the ocean floor. Theater Ratio (0.30): Low. While some efforts are made to address the problem, they are often insufficient to significantly reduce the scale of ALDFG. The efforts are more about solving the problem and less about just looking like they are doing so.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap arises from the differing impacts and roles of various stakeholders. Marine ecosystems experience ALDFG as a pure snare, with no benefits and significant harm. Small-scale fishers experience it as a tangled rope, with both costs (reduced fish stocks) and some benefits (potential for improved fishing grounds if ALDFG is reduced). Commercial fishing fleets see it as a coordination problem (rope), with some costs (lost gear) but overall benefits (reduced expenses). Environmental NGOs view it as a problem that can be addressed through collective action (scaffold), while international management organizations often struggle to effectively address the issue (piton).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural position of each agent. Marine ecosystems, as primary victims, experience high extraction. Small-scale fishers experience mixed extraction and coordination. Commercial fishing fleets benefit from reduced costs, resulting in low or negative extraction. Environmental NGOs aim to reduce extraction through organized efforts. International management organizations are constrained and often ineffective, leading to a degraded function.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that different actors perceive ALDFG differently based on their structural positions. Classifying ALDFG solely as a snare would ignore the coordination aspects (e.g., the need for international cooperation) and the potential for solutions. Similarly, classifying it solely as a coordination problem would ignore the significant harm caused to marine ecosystems and small-scale fisheries. The tangled rope classification for the analytical observer captures the complexity of the issue, while the perspectives of individual actors reflect their specific experiences and roles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gear_degradation_rate,
    'What is the average degradation rate of different types of fishing gear in various marine environments?',
    'Material science studies and long-term monitoring of ALDFG in different ocean regions.',
    'Impacts the perceived urgency and scale of the problem. Faster degradation reduces long-term harm, slower degradation increases it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gear_degradation_rate, empirical, 'Degradation rate of fishing gear').

omega_variable(
    fisher_compliance_incentives,
    'How can incentives be designed to encourage responsible gear management and retrieval by fishers?',
    'Economic modeling of different incentive mechanisms (deposit refund systems, gear buyback programs).',
    'Determines the effectiveness of policy interventions. Stronger incentives improve compliance, weaker incentives lead to continued ALDFG.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fisher_compliance_incentives, preference, 'Incentives for responsible gear management').

omega_variable(
    ecosystem_resilience,
    'To what extent are marine ecosystems resilient to the impacts of ghost fishing?',
    'Ecological studies assessing the recovery rates of different marine habitats after ALDFG removal.',
    'Determines the long-term consequences of ALDFG. Higher resilience reduces negative impacts, lower resilience exacerbates them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_resilience, empirical, 'Ecosystem resilience to ghost fishing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ghost_fishing_gear, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ghos_tr_t0, ghost_fishing_gear, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ghos_tr_t10, ghost_fishing_gear, theater_ratio, 10, 0.25).
narrative_ontology:measurement(ghos_tr_t20, ghost_fishing_gear, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(ghos_be_t0, ghost_fishing_gear, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ghos_be_t10, ghost_fishing_gear, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ghos_be_t20, ghost_fishing_gear, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ghost_fishing_gear, resource_allocation).
narrative_ontology:affects_constraint(ghost_fishing_gear, sustainable_fishing_practices).
narrative_ontology:affects_constraint(ghost_fishing_gear, marine_plastic_pollution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
