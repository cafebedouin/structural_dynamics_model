% ============================================================================
% CONSTRAINT STORY: scurvy_maritime_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scurvy_maritime_extraction, []).

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
 *   constraint_id: scurvy_maritime_extraction
 *   human_readable: The Scurvy/Empire Trade-off
 *   domain: biological/political/economic
 *
 * SUMMARY:
 *   During the Age of Sail (15th-18th centuries), scurvy (Vitamin C
 *   deficiency) was a primary biological constraint on long-distance sea
 *   voyages. This constraint story analyzes scurvy not merely as a disease,
 *   but as a structural component of maritime empires, creating a trade-off
 *   between imperial expansion and the health and well-being of sailors. The
 *   classification varies significantly depending on the perspective,
 *   highlighting the indexical nature of this framework.
 *
 * KEY AGENTS:
 *   - Sailors: Primary victims (powerless/trapped) - Experienced the deadly effects of scurvy with little agency to mitigate the risk.
 *   - Maritime Empires: Primary beneficiaries (institutional/arbitrage) - Gained significant strategic and economic advantages from voyages despite the human cost.
 *   - Naval Physicians: Moderate influence (moderate/constrained) - Attempted to treat scurvy with limited knowledge and resources, constrained by prevailing theories.
 *   - Analytical Historian: Observer (analytical/analytical) - Analyzes the long-term historical trade-offs from a distanced perspective.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scurvy_maritime_extraction, 0.65).
domain_priors:suppression_score(scurvy_maritime_extraction, 0.7).
domain_priors:theater_ratio(scurvy_maritime_extraction, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scurvy_maritime_extraction, extractiveness, 0.65).
narrative_ontology:constraint_metric(scurvy_maritime_extraction, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(scurvy_maritime_extraction, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scurvy_maritime_extraction, tangled_rope).
narrative_ontology:human_readable(scurvy_maritime_extraction, "The Scurvy/Empire Trade-off").
narrative_ontology:topic_domain(scurvy_maritime_extraction, "biological/political/economic").

domain_priors:requires_active_enforcement(scurvy_maritime_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scurvy_maritime_extraction, maritime_empires).
narrative_ontology:constraint_victim(scurvy_maritime_extraction, sailors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ILL-FATED SAILOR (SNARE) - Enlisted sailors were essentially trapped on long voyages with limited or no access to Vitamin C, experiencing the full, often fatal, impact of scurvy. Limited exit options due to naval discipline and the practicalities of seafaring. High extraction, low coordination benefit from their perspective. Death is the ultimate trapped exit.
constraint_indexing:constraint_classification(scurvy_maritime_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NAVAL PHYSICIAN (TANGLED ROPE) - Attempted to mitigate the effects of scurvy with limited knowledge and resources. Constrained by prevailing medical theories and the difficulty of procuring fresh provisions. Benefits indirectly through career advancement and prestige, but also bears the burden of witnessing and attempting to treat the disease. The extraction comes from the limited efficacy of the cures, but the coordination benefit is the attempt to address the problem.
constraint_indexing:constraint_classification(scurvy_maritime_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MARITIME EMPIRES (ROPE) - Gained significant economic and strategic advantage from long-distance voyages despite the high cost in human lives. Scurvy was a constraint they actively managed, accepting a certain level of loss as the price of empire. They could 'arbitrage' by replacing lost crew and continuing voyages. From their perspective, the trade-off was a necessary evil to enable exploration, trade, and naval power.
constraint_indexing:constraint_classification(scurvy_maritime_extraction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ANALYTICAL HISTORIAN (TANGLED ROPE) - Analyzes the historical trade-offs, recognizing the extraction from sailors but also the coordination in enabling empire and globalization. Understands both the benefits and the costs of the scurvy/empire trade-off over the long term.
constraint_indexing:constraint_classification(scurvy_maritime_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scurvy_maritime_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scurvy_maritime_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scurvy_maritime_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scurvy_maritime_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(scurvy_maritime_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Scurvy led to significant loss of life and debilitating illness among sailors, a direct extraction of health and well-being. Suppression (0.70): High. Limited alternatives available to sailors during long voyages, limited knowledge and resources to combat scurvy. Theater Ratio (0.3): Relatively low. While ineffective, efforts to combat scurvy were genuine (e.g. lemons). There wasn't much 'show' to the 'care'.
 *
 * PERSPECTIVAL GAP:
 *   The sailor experiences a snare - a deadly trap with no escape. The empire views it as a rope - a manageable coordination problem in pursuit of empire. The physician experiences it as a tangled rope - a mix of genuinely trying to improve things, but extracting effort nonetheless.
 *
 * DIRECTIONALITY LOGIC:
 *   Sailors (victims, trapped) have high directionality, experiencing the full impact of the extraction. Empires (beneficiaries, arbitrage) have negative directionality, benefiting from the extraction even at the cost of lives. Physicians (moderate, constrained) have intermediate directionality, experiencing both costs and benefits. The directionality drives the classification differences between snare, rope, and tangled rope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitamin_c_understanding,
    'How long would it have taken to discover/accept the Vitamin C deficiency model?',
    'Counterfactual analysis of medical knowledge progression, examining alternative paths of research and experimentation.',
    'Earlier acceptance would shift the constraint towards rope/scaffold with active mitigation. Delayed acceptance sustains extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitamin_c_understanding, conceptual, 'Counterfactual on the timing of Vitamin C understanding').

omega_variable(
    provisioning_feasibility,
    'How feasible was consistent provisioning of Vitamin C sources on long voyages?',
    'Logistical modeling of supply chains, preservation techniques, and resource availability at various ports.',
    'High feasibility makes the continued extraction deliberate choice (snare). Low feasibility suggests unavoidable limit (mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provisioning_feasibility, empirical, 'Logistical feasibility of Vitamin C provisioning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scurvy_maritime_extraction, 1500, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scur_tr_t1500, scurvy_maritime_extraction, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(scur_tr_t1650, scurvy_maritime_extraction, theater_ratio, 1650, 0.3).
narrative_ontology:measurement(scur_tr_t1800, scurvy_maritime_extraction, theater_ratio, 1800, 0.4).

% Extraction over time
narrative_ontology:measurement(scur_be_t1500, scurvy_maritime_extraction, base_extractiveness, 1500, 0.75).
narrative_ontology:measurement(scur_be_t1650, scurvy_maritime_extraction, base_extractiveness, 1650, 0.65).
narrative_ontology:measurement(scur_be_t1800, scurvy_maritime_extraction, base_extractiveness, 1800, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scurvy_maritime_extraction, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
