% ============================================================================
% CONSTRAINT STORY: cumbria_mine_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cumbria_mine_rejection
 *   human_readable: UK Government Rejection of the Woodhouse Colliery Coal Mine
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK government's rejection of the Woodhouse Colliery coal mine in
 *   Cumbria in 2022 represents a structural constraint operating across
 *   multiple institutional levels and political interests. The constraint is
 *   the regulatory framework—national environmental and climate policy backed
 *   by planning law and international net-zero commitments—that was
 *   ultimately deployed to block a regionally-desired investment. From the
 *   perspective of affected mining communities and regional economic
 *   interests, the rejection is experienced as pure extraction with no exit
 *   option: high-wage employment terminated by decision-makers with no
 *   accountability to the region. From the perspective of climate policy
 *   actors, the rejection is coordination: enforcing net-zero commitments
 *   that require decarbonization. From the perspective of the central
 *   government, it is tangled rope: the constraint simultaneously advances
 *   climate credibility and extracts political capital from the region. The
 *   constraint exhibits high suppression (barriers to alternative employment
 *   and coal-industry reversal) and moderate theater (the performative
 *   quality of announcing climate commitment while continuing high-carbon
 *   activities elsewhere in the economy). The extractiveness value reflects
 *   that while the regional community experiences severe extraction, the
 *   policy serves a stated coordination function, making it neither pure
 *   snare nor pure coordination.
 *
 * KEY AGENTS:
 *   - Cumbrian Mining Community and Industry: Primary victims (powerless/trapped) — faces job losses and economic collapse with no viable alternatives in region
 *   - Cumbrian Regional Economy: Primary victim (moderate/constrained) — loses tax base and high-wage employment; constrained ability to redeploy capital and labor
 *   - UK Central Government: Primary institutional actor (powerful/constrained) — benefits from climate policy enforcement, bears political costs of regional disruption; constrained by competing commitments
 *   - Climate Policy Framework and International Commitments: Primary beneficiary (institutional/arbitrage) — benefits from regulatory enforcement that advances net-zero targets and international credibility
 *   - Just-Transition Programs and Green Investment: Organized secondary actor (organized/mobile) — positioned to provide exit path with explicit sunset clause, but implementation remains uncertain
 *   - International Environmental Governance: Institutional observer (institutional/arbitrage) — sees mine rejection as theater demonstrating UK climate commitment; benefits from perception management
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cumbria_mine_rejection, 0.52).
domain_priors:suppression_score(cumbria_mine_rejection, 0.68).
domain_priors:theater_ratio(cumbria_mine_rejection, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cumbria_mine_rejection, extractiveness, 0.52).
narrative_ontology:constraint_metric(cumbria_mine_rejection, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cumbria_mine_rejection, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cumbria_mine_rejection, tangled_rope).
narrative_ontology:human_readable(cumbria_mine_rejection, "UK Government Rejection of the Woodhouse Colliery Coal Mine").
narrative_ontology:topic_domain(cumbria_mine_rejection, "economic/political").

domain_priors:requires_active_enforcement(cumbria_mine_rejection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cumbria_mine_rejection, climate_policy_advocates).
narrative_ontology:constraint_beneficiary(cumbria_mine_rejection, uk_net_zero_credibility).
narrative_ontology:constraint_victim(cumbria_mine_rejection, cumbrian_coal_industry).
narrative_ontology:constraint_victim(cumbria_mine_rejection, local_employment).
narrative_ontology:constraint_victim(cumbria_mine_rejection, regional_economic_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CUMBRIAN MINING COMMUNITY (SNARE) — Trapped by regulatory rejection with no viable alternative employment in region. Career expectations formed over decades suddenly terminated by national policy change. Cannot exit the constraint; bears full cost of closure.
constraint_indexing:constraint_classification(cumbria_mine_rejection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CUMBRIAN REGIONAL ECONOMY (SNARE) — Constrained by loss of high-wage employment and tax base. Regional development plans built on mining assumptions become obsolete. Limited mobility options for capital and labor redeployment.
constraint_indexing:constraint_classification(cumbria_mine_rejection, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UK CLIMATE POLICY FRAMEWORK (ROPE) — Benefits from demonstrating commitment to net-zero targets and Paris Agreement obligations. Experiences the constraint as coordination: rejecting fossil fuel expansion reinforces credibility with international partners and enables carbon budgeting. Net beneficiary through regulatory enforcement that advances stated policy objectives.
constraint_indexing:constraint_classification(cumbria_mine_rejection, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UK CENTRAL GOVERNMENT (TANGLED ROPE) — Constrained by competing demands: pledged climate commitments vs electoral pressure from affected regions, global trade dependencies vs domestic employment expectations. Benefits from enforcing climate policy (international credibility) but bears political costs of regional job losses. Mixed extraction and coordination — the rejection enforces net-zero while simultaneously extracting legitimacy from local communities.
constraint_indexing:constraint_classification(cumbria_mine_rejection, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSITION INFRASTRUCTURE AND GREEN INVESTMENT (SCAFFOLD) — Organized actors (just-transition funds, renewable energy zones, skills retraining programs) see the mine rejection as a temporary pain point on the path to lower-carbon regional economy. High suppression is tolerated because there is an explicit sunset: transition funding, renewable manufacturing clusters, and green hydrogen projects are intended to replace coal-economy employment. Classification requires explicit transition framework and sunset clause.
constraint_indexing:constraint_classification(cumbria_mine_rejection, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL ENVIRONMENTAL GOVERNANCE (PITON) — The mine rejection is partially performative display of climate commitment to international forums while UK continues high-carbon activities in other sectors (aviation, financial services, imported goods). The regulatory theater—ceremonial climate leadership—persists despite limited functional decarbonization. Theater ratio elevated by the gap between visible gesture (rejecting domestic coal) and systemic extraction (continued carbon-intensive economy).
constraint_indexing:constraint_classification(cumbria_mine_rejection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: CLIMATE IMPERATIVE / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, carbon budgets are hard physical constraints: net-zero commitments are non-negotiable atmospheric realities, not policy choices. This perspective sees the rejection as inevitable, unchangeable, and emerging naturally from thermodynamics and climate physics. However, the analytical observer should detect this as potential false naturalization of what is actually a political/institutional constraint: the 'inevitability' narrative masks contingent policy design choices.
constraint_indexing:constraint_classification(cumbria_mine_rejection, mountain,
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
    constraint_indexing:constraint_classification(cumbria_mine_rejection, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.52): High-moderate. The central government extraction of regional employment and economic viability is substantial but serves a stated policy function (climate commitment). The value reflects that some extraction is genuine (regional costs bear disproportionately on one locality) but the constraint also coordinates genuine climate policy. If the constraint were pure extraction, extractiveness would be ≥0.66; if pure coordination, ≤0.35. Suppression (0.68): High. Structural barriers include lack of alternative coking coal demand in the region, limited skills transferability, geographic immobility constraints, and career expectations formed over decades. Alternative employment is not available at equivalent wages. No exit mechanism exists for the community itself, though transition programs theoretically provide organizational exit path. Theater ratio (0.58): Moderate-high. Significant performative component: the UK rejects domestic coal while continuing substantial carbon imports and aviation growth, suggesting partial commitment to decarbonization rhetoric. However, rejecting the mine does serve real climate accounting function, so theater is not dominant.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between the region's experience of permanent extraction (snare) and the national policy perspective of temporary coordination leading to green transition (scaffold/rope). The mining community experiences the constraint as unchangeable and terminal—extractiveness is high (0.92+ from their perspective). The transition infrastructure sees the same constraint as temporary suppression with an explicit sunset (renewable energy zones, reskilled workforce, green manufacturing clusters). The central government experiences the constraint as constrained rope—advancing climate objectives while bearing political costs. The gap emerges because exit options differ radically: the community has none (trapped exit, regional d ≈0.95); the government has multiple (arbitrage exit, national d ≈0.25); transition programs have mobile exit through new sectors. The piton perspective reveals performative dimensions: the mine rejection announces climate commitment to international observers while the UK's actual consumption-based carbon footprint remains high. The mountain perspective risks naturalizing a contingent policy choice as inevitable climate physics. The analytical observer must track whether the theater (performative commitment) or the function (real climate accounting) dominates over time.
 *
 * DIRECTIONALITY LOGIC:
 *   The powerless agent (mining community) has trapped exit, making d very high (0.92-0.95), resulting in high f(d) ≈1.35. The institutional agent (UK government) benefits from policy enforcement (low d ≈0.25, f(d) ≈-0.01) but is politically constrained, producing moderate d ≈0.50, f(d) ≈0.65 when considering the political extraction from the region. The organized agent (transition programs) has mobile exit options (d ≈0.45), producing f(d) ≈0.55, making the constraint appear as temporary scaffold rather than permanent snare. The constraint's directionality varies dramatically across perspectives: from 0.95 (trapped community) to 0.25 (beneficiary policy framework) to 0.50 (constrained government). This perspectival variance justifies tangled_rope classification—the same constraint exhibits both high extraction (from one angle) and real coordination (from another).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the tangled_rope mandatrophy by establishing that the same regulation simultaneously serves two structural functions: (1) advancing climate policy coordination (real function: UK net-zero commitment requires decarbonization), and (2) extracting regional employment and economic viability (real extraction: unequal burden distributed to one locality). The mandatrophy is resolved by showing that without the extraction component, the policy would not be durable—central government extracts political capital (demonstrating commitment) from the region. Without the coordination component, the constraint would be pure snare—the government would have no legitimate basis for enforcement. The tangled_rope classification confirms both: high suppression (0.68) prevents region from escaping; active enforcement (central government override of local support) is necessary; beneficiaries (climate policy) and victims (mining community) are distinct; χ ≈ 0.45-0.55 places it in the tangled_rope band (0.40-0.90). The mandatrophy is NOT resolved by showing the constraint is 'really' rope or 'really' snare—it is genuinely both, and the indexical framework correctly captures that by assigning different classifications to different observers while computing a unified χ that reflects the hybrid nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    just_transition_sufficiency,
    'Do proposed green transition programs genuinely provide equivalent economic opportunity for displaced mining workers and communities?',
    'Empirical comparison of transition fund allocation vs retraining outcomes; longitudinal employment and wage tracking for displaced workers; regional GDP recovery timelines',
    'If sufficient: scaffold classification confirmed—temporary suppression with functional exit path. If insufficient: constraint reverts to snare—permanent extraction masquerading as temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_sufficiency, empirical, 'Whether just-transition programs provide genuine alternative opportunities').

omega_variable(
    uk_decarbonization_credibility,
    'Does rejecting domestic coal while maintaining carbon-intensive imports and aviation establish genuine climate commitment or performative theater?',
    'UK consumption-based carbon accounting vs production-based; actual emissions trajectory vs net-zero commitment timeline; policy consistency across sectors',
    'If genuine: central government rope classification confirmed. If performative: constraint revealed as piton—exhibition of commitment without functional decarbonization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(uk_decarbonization_credibility, empirical, 'Whether UK decarbonization is genuine or performative').

omega_variable(
    alternative_coking_coal_supply,
    'Does global coking coal supply sufficiency allow the UK to reject domestic supply without functional economic cost to steel production?',
    'International coking coal market analysis; UK steel industry input sourcing; global price stability and geopolitical supply chain risk',
    'If sufficient global supply: rejection is coordination (UK benefits internationally, regional cost is acceptable). If insufficient: rejection creates hidden extraction downstream (higher steel costs, manufacturing competitiveness loss).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coking_coal_supply, empirical, 'Whether global coking coal supply can replace rejected domestic source').

omega_variable(
    regional_political_mandate,
    'Was the mine rejection driven by evidence-based climate policy or by shifting political sentiment that overrode established planning procedures?',
    'Timeline of policy positions and planning decisions; analysis of planning inspector recommendations vs government override; public consultation data and sentiment shifts',
    'If evidence-based: tangled_rope is stable classification with coherent logic. If politically driven override: constraint is better classified as snare—extraction disguised as policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_political_mandate, conceptual, 'Whether rejection was policy-driven or politically expedient').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cumbria_mine_rejection, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cumbria_tr_t0, cumbria_mine_rejection, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cumbria_tr_t3, cumbria_mine_rejection, theater_ratio, 3, 0.48).
narrative_ontology:measurement(cumbria_tr_t6, cumbria_mine_rejection, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(cumbria_be_t0, cumbria_mine_rejection, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cumbria_be_t3, cumbria_mine_rejection, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(cumbria_be_t6, cumbria_mine_rejection, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cumbria_mine_rejection, enforcement_mechanism).
narrative_ontology:affects_constraint(cumbria_mine_rejection, uk_net_zero_transition_pathway).
narrative_ontology:affects_constraint(cumbria_mine_rejection, steel_supply_chain_vulnerability).
narrative_ontology:affects_constraint(cumbria_mine_rejection, regional_economic_inequality).

% DUAL FORMULATION NOTE:
% The mine rejection constraint is downstream of both UK net-zero policy commitments and regional coal-industry viability claims. Upstream constraint (net-zero pathway) drives this constraint's existence; lateral constraints (steel supply chain, regional inequality) are affected by this constraint's enforcement. The three-story family reflects decomposition by structural domain rather than by observable—each story has independent ε but shares institutional relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cumbria_mine_rejection, powerful, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
