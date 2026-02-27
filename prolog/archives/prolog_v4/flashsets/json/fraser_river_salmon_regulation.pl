% ============================================================================
% CONSTRAINT STORY: fraser_river_salmon_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fraser_river_salmon_regulation, []).

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
 *   constraint_id: fraser_river_salmon_regulation
 *   human_readable: Fraser River Salmon Regulation
 *   domain: economic
 *
 * SUMMARY:
 *   The regulation of Fraser River salmon, initially intended for
 *   conservation, has evolved into a complex system that extracts value from
 *   indigenous communities while benefiting commercial fishing interests and
 *   government revenue. This constraint exhibits a range of DR types,
 *   reflecting the different perspectives of key stakeholders.
 *
 * KEY AGENTS:
 *   - Indigenous Communities: Primary target (powerless/trapped)
 *   - Commercial Fishing Industry: Beneficiary (moderate/constrained)
 *   - Government Regulators: Beneficiary (institutional/arbitrage)
 *   - Analytical Observer: Evaluator (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fraser_river_salmon_regulation, 0.6).
domain_priors:suppression_score(fraser_river_salmon_regulation, 0.7).
domain_priors:theater_ratio(fraser_river_salmon_regulation, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, extractiveness, 0.6).
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fraser_river_salmon_regulation, tangled_rope).
narrative_ontology:human_readable(fraser_river_salmon_regulation, "Fraser River Salmon Regulation").
narrative_ontology:topic_domain(fraser_river_salmon_regulation, "economic").

domain_priors:requires_active_enforcement(fraser_river_salmon_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fraser_river_salmon_regulation, commercial_fishing_industry).
narrative_ontology:constraint_beneficiary(fraser_river_salmon_regulation, government_revenue).
narrative_ontology:constraint_victim(fraser_river_salmon_regulation, indigenous_communities).
narrative_ontology:constraint_victim(fraser_river_salmon_regulation, conservation_efforts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Indigenous communities, historically dependent on Fraser River salmon, face restricted access and limited influence on regulation, experiencing it as a snare.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% The commercial fishing industry benefits from the regulation through quotas and access, but is also constrained by the regulations themselves, viewing the system as a tangled rope.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Government regulators, benefiting from revenue and perceived conservation efforts, view the regulation as a rope that coordinates resource use.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational perspective, the regulation increasingly functions as a piton, an outdated system maintained for political reasons, failing to effectively address conservation and equitable resource allocation.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fraser_river_salmon_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fraser_river_salmon_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fraser_river_salmon_regulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fraser_river_salmon_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fraser_river_salmon_regulation, TR),
    TR >= 0.70.

:- end_tests(fraser_river_salmon_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. The regulation extracts significant value from Indigenous communities through limited access and loss of traditional fishing rights. Commercial fisheries and government revenue are primary beneficiaries. Suppression (0.7): High. Limited options for Indigenous communities to challenge regulation. High barriers to entry for new commercial fishing enterprises. Theater Ratio (0.75): High. Significant pretense of scientific management and conservation efforts, but increasing evidence of political influence.
 *
 * PERSPECTIVAL GAP:
 *   Indigenous communities perceive the regulation as a snare, restricting their access to a vital resource. Commercial fishing interests see it as a tangled rope, balancing benefits with constraints. Government regulators view it as a rope, coordinating resource use. The analytical observer sees it as a piton: an outdated system failing to achieve its stated goals.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by power, exit options, and beneficiary/victim status. Indigenous communities, with limited power and exit options, experience the highest extraction. Commercial fisheries benefit, but are also constrained. Government regulators benefit directly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conservation_vs_extraction,
    'To what extent is the regulation genuinely promoting conservation versus primarily serving as a tool for resource extraction?',
    'Independent ecological audits and cost-benefit analyses assessing the impact of regulations on salmon populations and ecosystem health.',
    'If primarily conservation, reclassifies towards a rope. If primarily extraction, confirms snare classification for Indigenous communities and reinforces tangled rope for commercial interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conservation_vs_extraction, empirical, 'Determine if regulations prioritize conservation or extraction.').

omega_variable(
    indigenous_influence,
    'How much influence do Indigenous communities genuinely have on the regulation process?',
    'Assess the degree to which Indigenous knowledge is integrated into decision-making and whether Indigenous concerns are adequately addressed in regulatory outcomes.',
    'Increased influence could shift their perspective from snare towards tangled rope. Continued exclusion solidifies the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_influence, empirical, 'Determine the influence of Indigenous communities on regulations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fraser_river_salmon_regulation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fras_tr_t0, fraser_river_salmon_regulation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(fras_tr_t10, fraser_river_salmon_regulation, theater_ratio, 10, 0.65).
narrative_ontology:measurement(fras_tr_t20, fraser_river_salmon_regulation, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(fras_be_t0, fraser_river_salmon_regulation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fras_be_t10, fraser_river_salmon_regulation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(fras_be_t20, fraser_river_salmon_regulation, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fraser_river_salmon_regulation, resource_allocation).
narrative_ontology:affects_constraint(fraser_river_salmon_regulation, pacific_salmon_treaty).
narrative_ontology:affects_constraint(fraser_river_salmon_regulation, coastal_fisheries_management).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
