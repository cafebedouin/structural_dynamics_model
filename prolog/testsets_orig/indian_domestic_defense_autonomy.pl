% ============================================================================
% CONSTRAINT STORY: indian_domestic_defense_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_domestic_defense_autonomy, []).

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
 *   constraint_id: indian_domestic_defense_autonomy
 *   human_readable: Indian Domestic Defense Autonomy and Institutional Dependency
 *   domain: geopolitical/military/institutional
 *
 * SUMMARY:
 *   India's defense autonomy presents a structural constraint that combines
 *   genuine coordination requirements (strategic partnerships,
 *   interoperability with allies) with extractive lock-in (technology
 *   dependency, procurement preferences, intellectual property restrictions).
 *   The constraint has evolved from post-Cold War alignment mechanics into a
 *   multi-layered institutional arrangement where foreign defense suppliers
 *   maintain primary relationships with Indian military services while the
 *   domestic defense industrial base is systematically constrained. The 'Make
 *   in India' initiative (2014 onwards) represents an organized coalition's
 *   response with a generational sunset clause — building indigenous
 *   capability in advanced platforms, supply chains, and design. The theater
 *   ratio (0.58) reflects significant performative institutional activity:
 *   strategic partnership frameworks, technology transfer memoranda, and
 *   autonomy commitments exist alongside sustained import dependency. The
 *   extractiveness (0.58) indicates hybrid coordination-extraction: genuine
 *   strategic benefits from allied relationships coexist with opportunity
 *   costs and capability constraints imposed by the procurement system.
 *
 * KEY AGENTS:
 *   - Domestic Defense Manufacturing Sector: Primary victim (powerless/trapped) — bears full cost of import lock-in through lost market access and constrained capability development
 *   - Military Services (Procurement Decision-Makers): Primary structural position (moderate/constrained) — experience mixed coordination and extraction; balance operational requirements against autonomy constraints
 *   - Foreign Defense Suppliers and Allied States: Primary beneficiary (institutional/arbitrage) — sustain revenue streams and geopolitical alignment through managed dependency
 *   - Indigenous Defense Development Coalition: Organized response (organized/constrained) — DRDO, HAL, academic partnerships building alternative pathways with sunset logic
 *   - Strategic Alliance Institutional Framework: Theater maintenance (institutional/arbitrage) — perpetuates alliance structures that justify continued dependency
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent defense-industrial arrangements as immutable structural limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_domestic_defense_autonomy, 0.58).
domain_priors:suppression_score(indian_domestic_defense_autonomy, 0.65).
domain_priors:theater_ratio(indian_domestic_defense_autonomy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_domestic_defense_autonomy, extractiveness, 0.58).
narrative_ontology:constraint_metric(indian_domestic_defense_autonomy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(indian_domestic_defense_autonomy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_domestic_defense_autonomy, tangled_rope).
narrative_ontology:human_readable(indian_domestic_defense_autonomy, "Indian Domestic Defense Autonomy and Institutional Dependency").
narrative_ontology:topic_domain(indian_domestic_defense_autonomy, "geopolitical/military/institutional").

domain_priors:requires_active_enforcement(indian_domestic_defense_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_domestic_defense_autonomy, foreign_defense_suppliers).
narrative_ontology:constraint_beneficiary(indian_domestic_defense_autonomy, institutional_defense_establishment).
narrative_ontology:constraint_victim(indian_domestic_defense_autonomy, domestic_defense_industrial_base).
narrative_ontology:constraint_victim(indian_domestic_defense_autonomy, strategic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC DEFENSE MANUFACTURING SECTOR (SNARE) — Trapped by import dependency and institutional procurement preferences that favor foreign systems. High suppression (licensing restrictions, indigenous production timelines, capital requirements). Cannot exit without state support that is systematically withheld. Maximum experienced extraction through technology lock-in and opportunity costs.
constraint_indexing:constraint_classification(indian_domestic_defense_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITARY SERVICES (TANGLED ROPE) — Constrained by technology availability, performance requirements, and standardization pressures. Benefits from operational interoperability with allied systems and access to cutting-edge platforms. Bears extraction cost through dependency and reduced autonomy. Genuine coordination function (interoperability with strategic partners) exists alongside asymmetric extraction (technology lock-in).
constraint_indexing:constraint_classification(indian_domestic_defense_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOREIGN DEFENSE SUPPLIERS AND ALLIED STATES (ROPE) — Benefits from sustained procurement relationships and technology transfer tied to political alignment. Experiences the constraint as coordination: sustained relationships enable defense partnerships and strategic alignment. Net beneficiary — extraction flows toward this institutional position.
constraint_indexing:constraint_classification(indian_domestic_defense_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIGENOUS DEFENSE INDUSTRY DEVELOPMENT COALITION (SCAFFOLD) — Organized actors (DRDO, Hindustan Aeronautics, BharatRatna initiatives, academic-industrial partnerships) see import dependency as a temporary structural problem with a sunset: domestic design capability, advanced manufacturing, and supply chain autonomy are being built incrementally. Suppression is significant but declining as investment increases. Classification as scaffold reflects genuine exit pathway with generational timeline.
constraint_indexing:constraint_classification(indian_domestic_defense_autonomy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STRATEGIC ALLIANCE INSTITUTIONAL FRAMEWORK (PITON) — Alliance commitments and technology transfer agreements (Indo-US defense cooperation frameworks, QUAD interoperability standards) are maintained as performative institutional structures. Theater ratio high (0.58): the institutional frameworks assume ongoing dependency despite stated commitment to 'Make in India' defense autonomy. The framework persists through inertia and strategic convenience even as its functional coordination role decays.
constraint_indexing:constraint_classification(indian_domestic_defense_autonomy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL LIMITS VIEW (MOUNTAIN) — From a civilizational/global perspective, the constraint appears immutable: integrated global defense supply chains, scientific specialization, and manufacturing comparative advantage create structural limits on defense autonomy for any non-hegemonic power. This perspective risks naturalizing what is contingent — India's dependency is a structural choice within global arms markets, not a law of nature. The engine's false summit detector will identify this as naturalization of institutional arrangements.
constraint_indexing:constraint_classification(indian_domestic_defense_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_domestic_defense_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_domestic_defense_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_domestic_defense_autonomy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indian_domestic_defense_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indian_domestic_defense_autonomy, TR),
    TR >= 0.70.

:- end_tests(indian_domestic_defense_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through opportunity costs (domestic sector foregoes market share and R&D investment), technology dependency (foreign systems set operational requirements), and procurement preferences (military services default to proven foreign platforms). The extraction is not as severe as full snare (0.72+) because genuine coordination benefits exist — interoperability with allied forces, access to advanced technology, standardization with strategic partners. Measurement shows extraction increasing over the interval as the gap between stated autonomy targets and actual procurement patterns widens. Suppression (0.65): Moderate-high. Barriers to domestic capability development include: licensing restrictions on advanced technologies, capital requirements for independent R&D, timeline pressures (military modernization deadlines favor immediate procurement over long-term development), career incentives for procurement officers favoring proven foreign solutions, and geopolitical alignment logic (choosing foreign suppliers signals alliance commitment). Suppression is not maximal (0.80+) because some pathways exist: technology transfer agreements, licensed production, government funding for DRDO/HAL. Theater ratio (0.58): Moderate. Strategic partnership frameworks and 'Make in India' commitments are substantive but partially performative. Defense cooperation memoranda are signed regularly, yet procurement patterns show persistent foreign preference. Indigenous development timelines slip, yet reinvestment continues. The theater is functional but increasingly decorative — institutional actors maintain the framework for strategic signaling even as its practical coordination role decays.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the powerless domestic sector's snare and the beneficiary institutional framework's rope/piton reveals the constraint's hybrid nature. The same procurement system appears as pure extraction to one perspective and pure coordination to another. The scaffold perspective (indigenous coalition) is organizationally meaningful — real investments in DRDO, HAL, supply chain development, and academic-industrial partnerships — but whether the sunset is real depends on omega resolution: if geopolitical alignment requires permanent dependency (omega: geopolitical_constraint_persistence), the scaffold is aspirational. If dependency is economic/structural rather than political (omega: autonomous_capability_threshold), the timeline extends beyond current estimates.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) reflects the agent's structural relationship to the extraction flow. Foreign suppliers benefit (low d, negative chi) — they have arbitrage options and institutional power. The domestic sector is victimized (high d, high f(d), high chi) — trapped with no exit and powerless to change procurement preferences. Military services are constrained beneficiaries (moderate d) — they gain from allied relationships and technology access but lose autonomy and bear dependency costs. The indigenous coalition has escape pathways (lower d through organized power and constrained exit options) — they can invest incrementally in autonomous capability. The piton classification derives from theater ratio, not from high extraction chi — the institutional framework maintains legitimacy through performative activity despite functional necessity declining.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINING CLASSIFICATION PREVENTS MISLABELING: If India's defense autonomy were classified as pure rope (coordination for strategic partnership), it would obscure the systematic extraction from the domestic sector and the opportunity costs borne by the national innovation system. If classified as pure snare (complete lock-in), it would miss the genuine coordination benefits of allied relationships and the realistic possibility of autonomous capability development. The tangled_rope classification (with piton at institutional level) captures the actual hybrid: real coordination coexists with real extraction, and the institutional theater maintains both. The scaffold perspective (organized coalition with sunset) is included to show that escape is structurally possible — mandatrophy resolution confirms the classification by noting that the constraint could decompose if autonomous capability matured, but currently exhibits all six DR types depending on perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomous_capability_threshold,
    'What level of domestic design and manufacturing capability constitutes genuine autonomy vs. managed dependency with local assembly?',
    'Empirical assessment of design-to-production cycle independence, technology transfer depth, and supply chain sourcing ratios; comparison with peer military-industrial bases (Israel, Turkey, South Korea)',
    'If threshold requires 90%+ indigenous content: India remains trapped for 2-3 decades. If threshold is 60%+ with critical path autonomy: sunset clause becomes credible within 10-15 years.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomous_capability_threshold, empirical, 'Definition of autonomy threshold for defense capability').

omega_variable(
    geopolitical_constraint_persistence,
    'Is the import dependency structural (inevitable given comparative advantage and scale) or political (maintained through strategic coercion by suppliers and allied states)?',
    'Counterfactual analysis: would India pursue autonomous defense development at current pace absent current geopolitical alignments? Scenario modeling with different strategic partner configurations.',
    'If structural: mountain classification justified — autonomy indefinitely bounded by economic factors. If political: piton classification confirmed — the institutional setup maintains dependency that could be abandoned at strategic cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_constraint_persistence, conceptual, 'Structural vs political sources of defense dependency').

omega_variable(
    technology_transfer_sufficiency,
    'Does licensed production of foreign platforms (HAL HAJ, Brahmos) meaningfully build indigenous capability or merely assemble systems with imported intellectual property?',
    'Assessment of design modifications, core technology absorption, and capability development trajectory for licensed platforms; measurement of technology diffusion to adjacent domains',
    'If genuine capacity-building: scaffold sunset becomes realistic. If assembly-only: suppression remains high and victim status of domestic industry persists longer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_transfer_sufficiency, empirical, 'Effectiveness of licensed production for capability development').

omega_variable(
    strategic_alignment_cost,
    'What portion of the extraction cost is inherent to coordinating with strategic partners vs. artificial lock-in maintained by defense suppliers?',
    'Comparative analysis of technology transfer terms in different alliance relationships; measurement of vendor lock-in effects (interoperability lock, ecosystem costs, upgrade dependencies)',
    'If alignment cost > 70%: most extraction is genuine coordination overhead. If lock-in effects > 40% of total cost: snare classification gains strength for domestic sector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_alignment_cost, empirical, 'Cost attribution for strategic alignment vs vendor lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_domestic_defense_autonomy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inddef_tr_t0, indian_domestic_defense_autonomy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(inddef_tr_t5, indian_domestic_defense_autonomy, theater_ratio, 5, 0.5).
narrative_ontology:measurement(inddef_tr_t10, indian_domestic_defense_autonomy, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(inddef_be_t0, indian_domestic_defense_autonomy, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(inddef_be_t5, indian_domestic_defense_autonomy, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(inddef_be_t10, indian_domestic_defense_autonomy, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_domestic_defense_autonomy, enforcement_mechanism).
narrative_ontology:affects_constraint(indian_domestic_defense_autonomy, critical_defense_supply_chain_vulnerability).
narrative_ontology:affects_constraint(indian_domestic_defense_autonomy, allied_interoperability_lock_in).
narrative_ontology:affects_constraint(indian_domestic_defense_autonomy, research_institutional_brain_drain).

% DUAL FORMULATION NOTE:
% Indian defense autonomy decomposes into related constraints: supply chain vulnerability (ε≈0.65, snare), interoperability lock-in (ε≈0.48, tangled_rope), and brain drain from domestic R&D (ε≈0.55, snare). This story addresses the systemic institutional constraint; siblings address specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indian_domestic_defense_autonomy, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
