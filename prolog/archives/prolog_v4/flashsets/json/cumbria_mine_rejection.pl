% ============================================================================
% CONSTRAINT STORY: cumbria_mine_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cumbria_mine_rejection, []).

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
 *   constraint_id: cumbria_mine_rejection
 *   human_readable: UK government rejection of the Woodhouse Colliery coal mine
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK government's rejection of the Woodhouse Colliery coal mine
 *   exemplifies the conflict between environmental commitments and local
 *   economic development. While the decision aligns with national climate
 *   goals, it also imposes costs on the local Cumbrian economy and
 *   potentially the UK steel industry. The constraint highlights the
 *   complexities of balancing competing priorities in the context of a
 *   transition to a low-carbon economy. Some actors see this rejection as a
 *   necessary tool for climate governance, while others experience it as a
 *   direct economic blow.
 *
 * KEY AGENTS:
 *   - UK Climate Commitments: Primary beneficiary (institutional/arbitrage) - Aligns with long-term goals, enforces policy consistency.
 *   - Cumbria Local Economy: Primary victim (powerless/trapped) - Bears cost of lost jobs and investment.
 *   - UK Steel Industry: Secondary victim (moderate/constrained) - Constrained, may seek alternatives.
 *   - Renewable Energy Sector: Secondary beneficiary (organized/mobile) - Benefits from reduced competitor, transition to sustainable energy.
 *   - Environmental Groups: Key beneficiary (institutional/mobile) - Groups advocate for climate goals
 *   - Woodhouse Colliery Investors: Impacted victim (moderate/constrained) - Constrained by the planning change.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cumbria_mine_rejection, 0.55).
domain_priors:suppression_score(cumbria_mine_rejection, 0.7).
domain_priors:theater_ratio(cumbria_mine_rejection, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cumbria_mine_rejection, extractiveness, 0.55).
narrative_ontology:constraint_metric(cumbria_mine_rejection, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cumbria_mine_rejection, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cumbria_mine_rejection, tangled_rope).
narrative_ontology:human_readable(cumbria_mine_rejection, "UK government rejection of the Woodhouse Colliery coal mine").
narrative_ontology:topic_domain(cumbria_mine_rejection, "economic/political").

domain_priors:requires_active_enforcement(cumbria_mine_rejection).
narrative_ontology:has_sunset_clause(cumbria_mine_rejection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cumbria_mine_rejection, uk_climate_commitments).
narrative_ontology:constraint_beneficiary(cumbria_mine_rejection, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(cumbria_mine_rejection, environmental_groups).
narrative_ontology:constraint_victim(cumbria_mine_rejection, cumbria_local_economy).
narrative_ontology:constraint_victim(cumbria_mine_rejection, woodhouse_colliery_investors).
narrative_ontology:constraint_victim(cumbria_mine_rejection, uk_steel_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the local Cumbrian economy, which is trapped and powerless to reverse the decision, bearing the cost of lost jobs and investment.
constraint_indexing:constraint_classification(cumbria_mine_rejection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of the UK steel industry, constrained by the decision but with some ability to seek alternative coal sources or lobby for policy changes. Experiences mixed benefits and costs. May benefit from green transition long-term.
constraint_indexing:constraint_classification(cumbria_mine_rejection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of the UK's climate commitments, benefiting from the decision as it aligns with long-term environmental goals. Can effectively 'arbitrage' global carbon markets. This perspective sees the constraint as a tool for enforcing policy consistency.
constraint_indexing:constraint_classification(cumbria_mine_rejection, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The renewable energy sector benefits from this decision in the short-term, as it temporarily removes a competitor and provides a boost, while the renewable transition matures. They are able to exit fossil fuels.
constraint_indexing:constraint_classification(cumbria_mine_rejection, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% The UK planning system continues to operate as usual, but the decision reflects a broader shift in priorities. The emphasis has shifted from resource extraction to climate mitigation.
constraint_indexing:constraint_classification(cumbria_mine_rejection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the mine rejection represents a tangled rope. The benefits of reduced carbon emissions are somewhat offset by the costs to the local economy and the potential for increased reliance on imported coal, and the UK demonstrates its commitment to global climate agreements, while causing localized economic loss.
constraint_indexing:constraint_classification(cumbria_mine_rejection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cumbria_mine_rejection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cumbria_mine_rejection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cumbria_mine_rejection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cumbria_mine_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cumbria_mine_rejection, TR),
    TR >= 0.70.

:- end_tests(cumbria_mine_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. This represents a mix of benefit for climate commitments and extraction from the regional economy, where it is a primary snare. Suppression (0.70): High. The decision suppresses alternative pathways, especially for local interests. The decision represents a firm climate goal priority. Theater ratio (0.75): High. High theater ratio implies the decision is in part a symbolic gesture to demonstrate climate leadership on the global stage, beyond just delivering tangible outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The decision is perceived differently by various stakeholders. While the UK's climate commitments benefit, the local Cumbrian economy bears the cost of lost jobs and investment. The UK steel industry experiences mixed impacts, as it faces the need to adapt to alternative coal sources. The renewable energy sector sees the decision as an opportunity for growth.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (UK Climate Commitments and Renewable Energy Sector) benefit from the decision, aligning with long-term environmental goals, while victims (Cumbria Local Economy and UK Steel Industry) face economic losses or constraints. This asymmetric impact defines the tangled rope nature of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The case illustrates the policy complexities by combining short-term economic disruption and long-term climate goals. The mine would have provided local benefits (employment), but these benefits would have clashed with the overall climate goal. The rejection prioritizes the reduction of carbon emissions by placing constraints on the coal industry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imported_coal_offset,
    'To what extent will the rejection of the Cumbrian mine lead to increased imports of coking coal from other countries?',
    'Analysis of import data and steel industry sourcing practices.',
    'If imports significantly increase, the climate benefits are reduced, making the constraint a less effective tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imported_coal_offset, empirical, 'Whether the climate benefits are offset by increased coal imports.').

omega_variable(
    local_economic_diversification,
    'How effectively can the Cumbrian economy diversify into alternative industries to replace the jobs and investment lost from the mine rejection?',
    'Tracking of economic development initiatives and employment rates in Cumbria.',
    'If diversification is successful, the local economic costs are mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_economic_diversification, empirical, 'Ability of the Cumbrian economy to diversify.').

omega_variable(
    policy_consistency_tradeoffs,
    'What are the broader policy tradeoffs between supporting domestic industries and meeting climate targets?',
    'Economic modeling and policy analysis.',
    'Clarifies if the rejection undermines steel industry or strengthens commitment to renewables.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_consistency_tradeoffs, conceptual, 'Policy tradeoffs related to supporting domestic industry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cumbria_mine_rejection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cumb_tr_t0, cumbria_mine_rejection, theater_ratio, 0, 0.6).
narrative_ontology:measurement(cumb_tr_t5, cumbria_mine_rejection, theater_ratio, 5, 0.7).
narrative_ontology:measurement(cumb_tr_t10, cumbria_mine_rejection, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(cumb_be_t0, cumbria_mine_rejection, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cumb_be_t5, cumbria_mine_rejection, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cumb_be_t10, cumbria_mine_rejection, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cumbria_mine_rejection, enforcement_mechanism).
narrative_ontology:affects_constraint(cumbria_mine_rejection, uk_climate_policy).
narrative_ontology:affects_constraint(cumbria_mine_rejection, global_coal_market).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
