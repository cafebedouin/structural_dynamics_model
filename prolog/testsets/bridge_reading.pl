% ============================================================================
% CONSTRAINT STORY: bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bridge_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bridge_reading
 *   human_readable: Unified AI Risk Governance Framework (Bridge Reading)
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   The bridge reading of AI risk governance proposes that present
 *   algorithmic harms to marginalized populations and existential risks from
 *   advanced AI systems are structurally entangled — not competing priorities
 *   but interconnected concerns requiring unified frameworks. This reading
 *   instantiates one specific resolution of the contested kernel
 *   'ai_risk_governance_priority' against two sibling readings: an
 *   existential-risk-first reading that deprioritizes near-term ethics work
 *   as a distraction from civilizational-scale threats, and a
 *   near-term-harms-first reading that treats existential risk as speculative
 *   and diverts resources from present suffering. The bridge reading occupies
 *   a middle position: claiming that integrated governance can simultaneously
 *   address both without pure compromise. The constraint exhibits the
 *   characteristic tangled_rope structure: genuine coordination function
 *   (bridging separated research communities, creating shared methodology,
 *   establishing integrated funding mechanisms) paired with asymmetric
 *   extraction (benefiting broker institutions disproportionately,
 *   concentrating resource flows through a small brokerage bottleneck,
 *   constraining both research communities through cross-domain obligations).
 *   The theater_ratio (0.52) reflects that while integration rhetoric has
 *   spread widely (conferences now routinely convene both communities, papers
 *   claim integrated approaches), the actual institutional structures remain
 *   substantially fragmented — committees on integration meet while
 *   departmental walls, funding silos, and credential paths persist. The
 *   measurement trajectory shows theater increasing from 0.35 to 0.52 and
 *   extractiveness increasing from 0.28 to 0.38, indicating that as the
 *   bridging narrative gains institutional legitimacy, both performative
 *   activity and resource concentration through broker institutions have
 *   intensified.
 *
 * KEY AGENTS:
 *   - Present Marginalized Populations: Primary victim (powerless/trapped) — bear algorithmic harms on immediate timescales; structurally excluded from governance; experience resource diversion toward long-term work
 *   - Future Humanity (Existential Risk Dimension): Primary victim (powerless/trapped) — voiceless in present decision-making; trapped by lock-in from present AI development choices
 *   - Near-Term Harm Research Community: Secondary victim and partial beneficiary (moderate/constrained) — constrained by funding bias and publication gatekeeping; benefit from bridging frame's legitimation and methodological access
 *   - Existential Risk Research Community: Secondary victim and partial beneficiary (moderate/constrained) — constrained by integration obligations and diluted focus; benefit from legitimation and domain expertise access
 *   - Bridging Institutions (5% of papers, 85% of cross-field links): Primary beneficiary (institutional/arbitrage) — capture disproportionate funding, visibility, and influence through brokerage position; experience constraint as pure coordination
 *   - Pre-Bridging Institutional Structures: Inertial actor (institutional/constrained) — maintain fragmented silos through inertia; perform diminished functions under integration narrative; experience piton classification
 *   - Emerging Unified Research Pathways: Organized agent (organized/mobile) — actively constructing integrated programs with generational sunset logic; scaffold perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bridge_reading, 0.38).
domain_priors:suppression_score(bridge_reading, 0.48).
domain_priors:theater_ratio(bridge_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bridge_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(bridge_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(bridge_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bridge_reading, tangled_rope).
narrative_ontology:human_readable(bridge_reading, "Unified AI Risk Governance Framework (Bridge Reading)").
narrative_ontology:topic_domain(bridge_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bridge_reading, '873d5128-4dac-438f-ab7c-eac0523b6af4').
narrative_ontology:cs_created_at('873d5128-4dac-438f-ab7c-eac0523b6af4', '').
narrative_ontology:cs_kernel_codification('873d5128-4dac-438f-ab7c-eac0523b6af4', distributed).
narrative_ontology:cs_authority_grounding('873d5128-4dac-438f-ab7c-eac0523b6af4', distributed).
narrative_ontology:cs_kernel_id(bridge_reading, ai_risk_governance_priority).
narrative_ontology:cs_reading_relation('873d5128-4dac-438f-ab7c-eac0523b6af4', existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('873d5128-4dac-438f-ab7c-eac0523b6af4', near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('873d5128-4dac-438f-ab7c-eac0523b6af4', foundational, present_future_harms_entangled_not_competing).
narrative_ontology:cs_axiom_status(present_future_harms_entangled_not_competing, holdable).
narrative_ontology:cs_axiom_grounding('873d5128-4dac-438f-ab7c-eac0523b6af4', present_future_harms_entangled_not_competing, deontological).
narrative_ontology:cs_axiom('873d5128-4dac-438f-ab7c-eac0523b6af4', foundational, unified_frameworks_enable_coordination_benefits).
narrative_ontology:cs_axiom_status(unified_frameworks_enable_coordination_benefits, holdable).
narrative_ontology:cs_axiom_grounding('873d5128-4dac-438f-ab7c-eac0523b6af4', unified_frameworks_enable_coordination_benefits, instrumental).
narrative_ontology:cs_reference_frame('873d5128-4dac-438f-ab7c-eac0523b6af4', integrated_governance_possibility).
narrative_ontology:cs_drift_state('873d5128-4dac-438f-ab7c-eac0523b6af4', contemporary_institutional_fragmentation, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bridge_reading, bridging_institutions).
narrative_ontology:constraint_beneficiary(bridge_reading, integrated_safety_ethics_research).
narrative_ontology:constraint_victim(bridge_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(bridge_reading, future_humanity).
narrative_ontology:constraint_victim(bridge_reading, near_term_harm_expertise).
narrative_ontology:constraint_victim(bridge_reading, existential_risk_expertise).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESENT MARGINALIZED POPULATIONS (SNARE) — Trapped by algorithmic harms (bias, surveillance, labor exploitation) without exit or alternative systems. The bridging framework's integrated approach does not protect against immediate extraction because present systems operate on timescales that preclude waiting for unified long-term frameworks. These populations bear extraction from both existing harms AND from resource diversion toward existential-risk work.
constraint_indexing:constraint_classification(bridge_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE HUMANITY / EXISTENTIAL RISK DIMENSION (SNARE) — Trapped by long-term trajectory lock-in from present AI development choices. Cannot exit or renegotiate terms. Maximum extraction from the present generation's externality dumping. The constraint's bridging mandate does not reduce their structural victimhood — future humanity remains voiceless.
constraint_indexing:constraint_classification(bridge_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: NEAR-TERM HARM RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by funding bias toward existential risk, publication gatekeeping favoring AI-safety framings, and career visibility penalties for near-term ethics work. Also beneficiaries of the bridging frame's legitimation of their research domain and collaborative access to safety researchers' methodologies. Moderate extraction with real coordination benefits.
constraint_indexing:constraint_classification(bridge_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: EXISTENTIAL RISK RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by obligation to engage near-term harms work and diluted focus from the bridging framework's integrated mandate. Also beneficiaries of legitimation from near-term ethics communities and access to domain expertise in power structures, political economy, and affected communities. Moderate extraction with real coordination benefits.
constraint_indexing:constraint_classification(bridge_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BRIDGING INSTITUTIONS (ROPE) — A small set of research institutions (labs, centers, funding bodies, conference organizers) that position themselves at the intersection of near-term ethics and existential risk benefit substantially from the bridging frame. They capture disproportionate funding, visibility, and influence precisely because they broker between the two communities. Experiences the constraint as pure coordination: facilitating dialogue is their functional role, and the bridging mandate legitimates their brokerage position. Low extraction because benefits align with function.
constraint_indexing:constraint_classification(bridge_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PRE-BRIDGING INSTITUTIONAL INERTIA (PITON) — The fragmented institutional structure (separate safety/ethics conferences, funding silos, citation networks, credential paths) persists through inertia even as the bridging reading gains rhetorical acceptance. Institutions that benefited from fragmentation maintain parallel structures performing diminished functions. Theater ratio high: committees on integration meet while departmental walls remain. Extraction mechanism degraded because the bridging narrative provides justification for pre-existing structures without requiring structural change.
constraint_indexing:constraint_classification(bridge_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: EMERGING UNIFIED RESEARCH PATHWAYS (SCAFFOLD) — New programs, funding mechanisms, and training pipelines explicitly organized around integrated safety-ethics curricula and collaborative research exemplars. Scaffold classification: these pathways have high agency (they are being actively constructed), demonstrable alternatives (specific examples exist), and a generational sunset clause (as unified pipelines mature, the older fragmented structures lose legitimacy and resource flow). Theater is moderate because construction is visible and measurable.
constraint_indexing:constraint_classification(bridge_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some tension between present and future harm prevention is inherent to risk governance: resource constraints, epistemic uncertainty about long-term trajectories, and present suffering all compete for attention. The observation that 'both matter and cannot be perfectly balanced' can be naturalized as an immutable law of ethics. However, the structural data contradicts the mountain classification — the constraint is substantially constructed by institutional fragmentation and brokerage structures, not by irreducible physical/logical limits. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(bridge_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bridge_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bridge_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bridge_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(bridge_reading, TR),
    TR >= 0.70.

:- end_tests(bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The bridging framework coordinates between two communities and creates genuine research benefits (methodological exchange, combined expertise, expanded scope). But the coordination occurs through a brokerage bottleneck — 5% of papers account for 85% of cross-field links — which concentrates benefits and resource flows. The moderate value reflects real coordination (not pure extraction) paired with resource concentration (asymmetric benefit distribution). If bridging were fully distributed, extractiveness would drop to ~0.15 (rope range); if brokerage becomes more concentrated, it rises toward 0.55+ (snare range). Suppression (0.48): Moderate-high. Barriers to entry include: (1) dual expertise requirements (researchers must credibly address both timescales), (2) institutional gatekeeping from pre-bridging conferences and journals, (3) funding mechanisms that require proposal language satisfying both risk frameworks, (4) career risk for researchers viewed as prioritizing 'wrong' timescale. But these are surmountable (emerging pathways exist), not absolute. Theater ratio (0.52): Moderate-high and rising. Integration rhetoric has proliferated (committees formed, statements issued, papers titled with both frameworks), but actual institutional structures remain largely fragmented. New programs claiming integration exist, but parallel pre-bridging structures continue in diminished capacity. The rise from 0.35 to 0.52 over the interval reflects increasing performative activity as the bridging narrative gains legitimacy without proportional structural transformation.
 *
 * PERSPECTIVAL GAP:
 *   The bridge reading produces a seven-way perspectival split reflecting the constraint's structural complexity. Marginalized populations and future humanity perceive pure extraction (snare) because their interests are externalized and costs imposed without consent. Both research communities perceive mixed coordination-extraction (tangled_rope) because they genuinely collaborate while experiencing resource constraints and cross-domain obligations. Broker institutions perceive pure coordination (rope) because their position enables them to benefit from rather than bear costs of bridging. Pre-bridging institutional structures perceive degraded theater (piton) because the integration narrative justifies structures that no longer serve their original function. Emerging unified pathways perceive temporary coordination (scaffold) because they are actively constructing alternatives with clear exit paths. The analytical observer at civilizational scope risks perceiving natural law (mountain) — treating the tension between near and long-term imperatives as immutable — but the structural data reveals this as a false summit. The constraint is substantially constructed by institutional fragmentation and brokerage capture, not by irreducible ethical limits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values vary dramatically across perspectives based on structural position. Marginalized populations experience high d (~0.95) because they bear harms while bearing costs of integration work; future humanity has maximum d (~1.0) because they are completely voiceless and externalized. Near-term and existential researchers experience moderate d (~0.55) as constrained agents with some benefit. Broker institutions experience very low d (~0.10) as net beneficiaries with arbitrage capacity. Pre-bridging institutions experience moderate-high d (~0.65) as constrained institutions forced to justify degraded functions. The deriv engine computes chi = ε × f(d) × σ(S) separately for each perspective: for marginalized populations, the very high d and global scope σ(S)=1.2 yields chi ≈ 0.38 × 1.4 × 1.2 ≈ 0.64 (high experienced extraction); for broker institutions, low d yields chi ≈ 0.38 × -0.10 × 1.2 ≈ -0.05 (negative extraction, net benefit). The perspectival gap is stark and diagnostic: the same base constraint produces snare outcomes for victims and rope outcomes for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The bridge reading avoids mandatrophy through legitimate tangled_rope classification: genuine coordination benefits coexist with asymmetric extraction. The framework coordinates methodological advances, legitimates both research communities, and creates new research pathways. Simultaneously, it extracts from both communities through cross-domain obligations and concentrates benefits through institutional brokerage. The constraint is neither pure extraction (snare) nor pure coordination (rope) — it genuinely instantiates both. The false-summit risk (analytical observer perceiving natural law) is identified by declaring beneficiaries (bridging institutions) on the mountain perspective. The engine's false-summit detector will flag the natural-law reading as naturalization of contingent institutional arrangements, routing to omega variables for investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bridging_mechanism_brittleness,
    'Does the integrated framework depend on a small set of broker institutions, or can bridging become distributed across the research ecosystem?',
    'Network analysis tracking whether cross-field collaboration edges remain concentrated or disperse; measurement of whether prestige/funding flows through broker institutions or becomes diffuse',
    'If concentrated: constraint remains tangled_rope with high vulnerability to institutional failure. If dispersed: transforms toward stable rope with resilient coordination. Current data (5% of papers → 85% of links) shows structural brittleness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bridging_mechanism_brittleness, empirical, 'Whether bridging mechanism depends on institutional bottleneck').

omega_variable(
    resource_zero_sum_framing,
    'Is funding diversion from near-term harms to existential risk (or vice versa) an inevitable zero-sum game, or is the bridging frame creating genuinely new resource flows?',
    'Longitudinal funding data: total research resources allocated to AI governance across both domains pre-bridging vs post-bridging; whether integrated funding represents new money or reshuffled allocation',
    'If zero-sum: constraint remains high-extraction snare for whichever domain loses resources. If genuinely new: constraint validates bridging mandate''s promise of non-competitive resource expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_zero_sum_framing, empirical, 'Whether bridging creates new resources or redistributes existing ones').

omega_variable(
    temporal_scale_incommensurability,
    'Can research frameworks meaningfully integrate work operating on 1-5 year timescales (near-term harms) with work operating on 50+ year timescales (existential risk) without methodological degradation?',
    'Meta-analysis of publications claiming integration: assess whether integrated frameworks actually coordinate different timescale work or merely juxtapose it; examine whether temporal scale differences create systematic misalignment in methodological rigor standards',
    'If incommensurable: bridging frame is theater; true integration impossible. If integrable: validates tangled_rope rather than piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_scale_incommensurability, conceptual, 'Whether different temporal scales can be genuinely integrated').

omega_variable(
    kernel_reading_ambiguity,
    'Is the bridge reading a genuine unified framework, or a strategic narrative that obscures the incompatibility between prioritizing present harms and prioritizing existential risk?',
    'Analysis of whether integrated governance frameworks make different resource allocation decisions than separate frameworks would; examination of cases where near-term and long-term imperatives conflict and how bridging institutions resolve them',
    'If genuine integration: classification confirmed as tangled_rope. If strategic narrative: classification degraded to piton (theater without functional integration).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether bridge reading represents genuine integration or strategic narrative').

omega_variable(
    sibling_reading_coexistence,
    'Can all three readings (bridge, existential-risk-first, near-term-harms-first) coexist as live positions in institutional governance frameworks, or does adopting one foreclose the others?',
    'Institutional policy analysis: examination of AI governance bodies (policy committees, funding boards, corporate responsibility structures) to determine whether they explicitly endorse one reading or attempt to hold multiple readings simultaneously; analysis of resource allocation outcomes when multiple readings coexist',
    'If coexistence possible: reading_relations = ''coexists_with''. If foreclosure occurs: reading_relations updated to ''forecloses'' or ''influenced_by''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, empirical, 'Whether multiple readings can coexist in practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bridge_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brid_tr_t0, bridge_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(brid_tr_t3, bridge_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(brid_tr_t6, bridge_reading, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(brid_be_t0, bridge_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(brid_be_t3, bridge_reading, base_extractiveness, 3, 0.34).
narrative_ontology:measurement(brid_be_t6, bridge_reading, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bridge_reading, resource_allocation).
narrative_ontology:affects_constraint(bridge_reading, existential_risk_reading).
narrative_ontology:affects_constraint(bridge_reading, near_term_harms_reading).
narrative_ontology:affects_constraint(bridge_reading, ai_governance_funding_allocation).
narrative_ontology:affects_constraint(bridge_reading, researcher_career_path_incentives).

% DUAL FORMULATION NOTE:
% The kernel 'ai_risk_governance_priority' decomposes into three readings with distinct ε values and victim/beneficiary structures. This file (bridge_reading, ε=0.38, tangled_rope) instantiates the unified integration position. Existential_risk_reading (ε>0.50, snare from near-term community perspective) instantiates prioritization of long-term trajectory control. Near_term_harms_reading (ε>0.50, snare from existential-risk community perspective) instantiates prioritization of present suffering. The three readings coexist as live institutional positions competing for resource allocation and governance authority. Each reading's ε value reflects its own structural properties when isolated; when multiple readings operate simultaneously, contamination effects and institutional coupling create higher effective extraction system-wide.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
