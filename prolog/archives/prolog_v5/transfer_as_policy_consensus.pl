% ============================================================================
% CONSTRAINT STORY: transfer_as_policy_consensus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transfer_as_policy_consensus, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: transfer_as_policy_consensus
 *   human_readable: Transfer as Policy Consensus Across Zionist Political Spectrum (1937-1948)
 *   domain: political_history/nationalism_studies/settler_colonial_studies
 *
 * SUMMARY:
 *   Between 1937 (Peel Commission endorsement of partition with transfer) and
 *   1948 (Plan D implementation), ideologically diverse Zionist factions
 *   converged on population transfer as necessary and morally acceptable
 *   policy. Labor Zionists (Ben-Gurion: 'We must expel Arabs and take their
 *   places'), Revisionists (Jabotinsky's acceptance of transfer in partition
 *   context), and Cultural Zionists (despite Ahad Ha'am's earlier warnings)
 *   aligned on demographic elimination despite disagreeing on economics,
 *   governance, and relationship to diaspora. This consensus coordinated
 *   state-building strategy while extracting catastrophically from
 *   Palestinian Arab population and suppressing binational alternatives. The
 *   constraint exhibits tangled_rope structure: genuine coordination function
 *   (unified Zionist action) coexisting with severe asymmetric extraction
 *   (Nakba) and active enforcement (military implementation, historiographic
 *   denial). Theater ratio rises post-1948 as official historiography denies
 *   planned transfer despite archival evidence.
 *
 * KEY AGENTS:
 *   - Palestinian Arab Population: Primary victim (powerless/trapped) — bore full cost of demographic elimination logic with no exit; experienced consensus as coordinated dispossession across all Zionist factions
 *   - Zionist Leadership Across Spectrum: Primary beneficiary (institutional/arbitrage) — Labor, Revisionist, and Cultural Zionist leaders coordinated on transfer despite ideological differences; enabled unified state-building strategy
 *   - Dissenting Zionist Intellectuals: Secondary victim (moderate/constrained) — Magnes, Buber, binational advocates marginalized within movement; constrained by institutional power but benefited from Zionist infrastructure
 *   - British Mandate Administration: Institutional actor (institutional/constrained) — Peel Commission legitimized transfer; constrained by contradictory mandate obligations and international legal norms
 *   - International Human Rights Framework: Organized agents (organized/mobile) — post-1948 legal norms (UDHR, Genocide Convention) created sunset pathway delegitimizing transfer
 *   - Post-1967 Official Historiography: Institutional actor (institutional/arbitrage) — maintained voluntary exodus narrative despite archival evidence; high theater ratio
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees mixed coordination-extraction structure requiring active enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transfer_as_policy_consensus, 0.38).
domain_priors:suppression_score(transfer_as_policy_consensus, 0.62).
domain_priors:theater_ratio(transfer_as_policy_consensus, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transfer_as_policy_consensus, extractiveness, 0.38).
narrative_ontology:constraint_metric(transfer_as_policy_consensus, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(transfer_as_policy_consensus, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transfer_as_policy_consensus, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(transfer_as_policy_consensus, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transfer_as_policy_consensus, tangled_rope).
narrative_ontology:human_readable(transfer_as_policy_consensus, "Transfer as Policy Consensus Across Zionist Political Spectrum (1937-1948)").
narrative_ontology:topic_domain(transfer_as_policy_consensus, "political_history/nationalism_studies/settler_colonial_studies").

domain_priors:requires_active_enforcement(transfer_as_policy_consensus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transfer_as_policy_consensus, zionist_leadership_across_spectrum).
narrative_ontology:constraint_beneficiary(transfer_as_policy_consensus, jewish_settlement_enterprise).
narrative_ontology:constraint_victim(transfer_as_policy_consensus, palestinian_arab_population).
narrative_ontology:constraint_victim(transfer_as_policy_consensus, international_legal_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN ARAB POPULATION (SNARE) — Trapped by military superiority, international indifference, and coordinated policy across all Zionist factions. No exit from the demographic elimination logic. Experiences maximum extraction: dispossession framed as demographic necessity by all political camps regardless of ideological differences on other matters.
constraint_indexing:constraint_classification(transfer_as_policy_consensus, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DISSENTING ZIONIST INTELLECTUALS (TANGLED ROPE) — Figures like Judah Magnes, Martin Buber, Ahad Ha'am's followers who advocated binational solutions. Constrained by institutional marginalization and the demographic imperative's framing power, but benefited from Zionist institutional infrastructure while opposing transfer consensus. Mixed experience: coordination benefits of movement membership alongside extraction through suppression of alternative visions.
constraint_indexing:constraint_classification(transfer_as_policy_consensus, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ZIONIST LEADERSHIP (ROPE) — Labor Zionists (Ben-Gurion, Weizmann), Revisionists (Jabotinsky), and Cultural Zionists converged on transfer as coordination solution to the demographic problem. Experienced as necessary alignment for state-building: ideological diversity on economics and governance coexisted with consensus on population management. Net beneficiaries of the coordination: transfer consensus enabled unified action during 1948.
constraint_indexing:constraint_classification(transfer_as_policy_consensus, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: BRITISH MANDATE ADMINISTRATION (TANGLED ROPE) — Peel Commission (1937) endorsed partition with transfer, legitimizing the consensus. Britain benefited from Zionist coordination (stable interlocutor) but was constrained by international legal norms and Arab resistance. Mixed position: facilitated transfer logic through partition proposal while facing extraction through mandate's contradictory obligations.
constraint_indexing:constraint_classification(transfer_as_policy_consensus, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL HUMAN RIGHTS FRAMEWORK (SCAFFOLD) — Emerging post-WWII legal norms (UDHR 1948, Genocide Convention 1948, later Fourth Geneva Convention 1949) created alternative framing that delegitimized population transfer. Organized international institutions building sunset pathway: transfer consensus became legally and morally untenable under evolving humanitarian law. Low extraction because framework has exit (can impose costs through international law) and sees temporal limit to transfer's acceptability.
constraint_indexing:constraint_classification(transfer_as_policy_consensus, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: POST-1967 OFFICIAL HISTORIOGRAPHY (PITON) — Israeli state narrative maintained that 1948 exodus was voluntary or result of Arab leaders' calls to flee, not planned transfer. Theater ratio high: archival evidence (Plan D, leadership statements) contradicts official story, but narrative persisted through institutional inertia and identity maintenance needs. Performative denial of transfer consensus despite documentary record.
constraint_indexing:constraint_classification(transfer_as_policy_consensus, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Transfer consensus coordinated Zionist factions around demographic imperative (genuine coordination function: unified state-building strategy) while extracting from Palestinian population (asymmetric cost: dispossession) and suppressing alternative visions (binationalism). Requires active enforcement through military implementation and historiographic management. Mixed structure visible from analytical distance: not pure extraction (real coordination problem for Zionist movement) but not pure coordination (severe asymmetric costs).
constraint_indexing:constraint_classification(transfer_as_policy_consensus, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transfer_as_policy_consensus_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transfer_as_policy_consensus, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transfer_as_policy_consensus, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(transfer_as_policy_consensus, TR),
    TR >= 0.70.

:- end_tests(transfer_as_policy_consensus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Transfer consensus coordinated Zionist factions (genuine coordination benefit) but extracted catastrophically from Palestinian population (dispossession, refugee crisis). Value reflects mixed function: not pure extraction (real strategic coordination for Zionist movement) but significant asymmetric cost. Lower than pure snare because coordination function is genuine, not theatrical. Suppression (0.62): Moderate-high. Binational alternatives were institutionally marginalized; Palestinian resistance was militarily overwhelmed; international legal constraints were weak pre-1948. But suppression not total: dissent existed within Zionist movement; some international actors opposed transfer; legal norms were evolving. Theater ratio (0.58): Moderate-high. Initially low (1937-1948: transfer openly discussed in leadership circles) but rising sharply post-1948 as official historiography denied planned transfer despite documentary evidence (Plan D, leadership statements). Current value reflects post-1967 performative denial.
 *
 * PERSPECTIVAL GAP:
 *   Palestinian population sees pure extraction (snare): coordinated dispossession with no exit. Zionist leadership sees coordination (rope): necessary alignment for state-building. Dissenting intellectuals see mixed structure (tangled_rope): benefit from movement while being suppressed. British Mandate sees mixed obligations (tangled_rope): facilitated consensus while constrained by legal norms. International framework sees temporary problem with sunset (scaffold): evolving law delegitimizes transfer. Official historiography sees degraded narrative (piton): performative denial despite evidence. Analytical observer sees tangled_rope: genuine coordination function coexisting with severe asymmetric extraction and active enforcement. Gap reveals how structural position determines whether consensus appears as necessary coordination or coordinated extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab population: victims with trapped exit → high d → high experienced extraction (snare). Zionist leadership: beneficiaries with arbitrage exit → low d → low/negative experienced extraction (rope). Dissenting intellectuals: mixed position (benefit from movement infrastructure, constrained by marginalization) → moderate d → moderate extraction (tangled_rope). British Mandate: institutional actor constrained by contradictory obligations → moderate-high d → moderate extraction (tangled_rope). International human rights framework: organized agents with mobile exit and growing power → low d → low extraction (scaffold). Official historiography: institutional beneficiary with arbitrage exit but high theater → low d but piton classification via theater gate. Analytical observer: sees full mixed structure → moderate d → tangled_rope via coordination + extraction + enforcement gates.
 *
 * MANDATROPHY ANALYSIS:
 *   Transfer consensus resolves mandatrophy by demonstrating that coordination and extraction are not mutually exclusive. The consensus genuinely coordinated Zionist factions around demographic imperative (Labor and Revisionist leaders aligned despite economic/governance disagreements), meeting coordination criterion. Simultaneously, it extracted catastrophically from Palestinian population (dispossession, refugee crisis) and suppressed binational alternatives (Magnes, Buber marginalized), meeting extraction and enforcement criteria. Tangled_rope classification captures this mixed structure: not mislabeling coordination as extraction (the strategic alignment was real) nor extraction as coordination (the asymmetric costs were real). The constraint demonstrates that political consensus can simultaneously solve coordination problems for one group while imposing severe costs on another — the indexical classification system reveals this by showing different types from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_vs_contingency,
    'Was transfer consensus an inevitable outcome of demographic arithmetic in a settler-colonial context, or a contingent political choice among available alternatives?',
    'Comparative analysis of other settler-colonial cases with demographic minorities; counterfactual analysis of binational proposals'' structural viability; examination of moments when transfer consensus was contested within Zionist movement',
    'If inevitable: constraint shifts toward mountain (demographic imperative determines policy). If contingent: remains tangled_rope (political coordination with alternatives suppressed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_vs_contingency, conceptual, 'Whether transfer consensus was structurally inevitable or politically contingent').

omega_variable(
    coordination_vs_cover_story,
    'Did transfer consensus genuinely coordinate Zionist factions around a shared strategic problem, or did it provide moral cover for extraction that would have occurred regardless?',
    'Analysis of internal debates and private correspondence; examination of whether factions with different ideological commitments actually modified behavior based on consensus; assessment of whether consensus preceded or followed military capability for implementation',
    'If genuine coordination: tangled_rope classification confirmed (mixed function). If cover story: reclassify toward snare (extraction with coordination theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_cover_story, empirical, 'Whether consensus coordinated action or rationalized predetermined extraction').

omega_variable(
    international_law_sunset_effectiveness,
    'Did post-1948 international legal norms actually create a sunset for transfer''s acceptability, or merely drive it underground into deniable forms?',
    'Longitudinal analysis of population transfer practices post-1948; comparison of explicit vs implicit transfer mechanisms; assessment of international enforcement effectiveness',
    'If effective sunset: scaffold perspective validated (temporal limit real). If merely driven underground: scaffold perspective is aspirational; piton perspective (performative compliance) becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_sunset_effectiveness, empirical, 'Whether international law created real sunset or performative compliance').

omega_variable(
    dissent_suppression_mechanism,
    'Was binational alternative structurally unviable due to demographic imperative, or was it suppressed through institutional power despite structural viability?',
    'Analysis of binational proposals'' institutional support and resource access; examination of whether demographic imperative was empirical constraint or framing device; comparison with successful binational arrangements in other contexts',
    'If structurally unviable: dissenting intellectuals'' constrained position reflects genuine coordination necessity. If suppressed: higher extractiveness (viable alternative eliminated for factional advantage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissent_suppression_mechanism, conceptual, 'Whether binational alternative was structurally unviable or politically suppressed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transfer_as_policy_consensus, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1937_peel, transfer_as_policy_consensus, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_1942_biltmore, transfer_as_policy_consensus, theater_ratio, 5, 0.42).
narrative_ontology:measurement(theater_1948_implementation, transfer_as_policy_consensus, theater_ratio, 11, 0.58).
narrative_ontology:measurement(theater_1957_official_history, transfer_as_policy_consensus, theater_ratio, 20, 0.72).

% Extraction over time
narrative_ontology:measurement(extract_1937_peel, transfer_as_policy_consensus, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(extract_1942_biltmore, transfer_as_policy_consensus, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(extract_1948_implementation, transfer_as_policy_consensus, base_extractiveness, 11, 0.38).
narrative_ontology:measurement(extract_1957_consolidation, transfer_as_policy_consensus, base_extractiveness, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transfer_as_policy_consensus, identity_coordination).

% DUAL FORMULATION NOTE:
% Transfer consensus is downstream of demographic_elimination_imperative (the perceived necessity of Jewish demographic majority). The upstream constraint (demographic imperative) has mountain characteristics from Zionist perspective (perceived as immutable demographic arithmetic) but is itself a framing device that naturalizes contingent political choices. Transfer consensus is the policy coordination layer that operationalized the demographic imperative across ideologically diverse factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transfer_as_policy_consensus, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
