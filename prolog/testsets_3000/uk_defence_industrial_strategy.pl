% ============================================================================
% CONSTRAINT STORY: uk_defence_industrial_strategy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_defence_industrial_strategy, []).

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
 *   constraint_id: uk_defence_industrial_strategy
 *   human_readable: UK Defence Industrial Strategy Constraint
 *   domain: defence_policy/industrial_policy
 *
 * SUMMARY:
 *   The UK Defence Industrial Strategy creates a structural constraint
 *   coupling government procurement authority, tier-one contractor
 *   consolidation, and SME supply chain participation into an extractive
 *   coordination mechanism. The strategy presents concentration as necessary
 *   for technological capability, security compliance, and geopolitical
 *   autonomy. However, the constraint exhibits all characteristics of a
 *   Tangled Rope: genuine coordination functions (allied interoperability,
 *   security standards, industrial continuity) exist alongside asymmetric
 *   extraction (price controls on SME suppliers, technology transfer
 *   asymmetry, regional concentration). The theater_ratio (0.68) reflects
 *   substantial performative activity: procurement reviews, compliance
 *   documentation, and security protocols that produce limited verification
 *   of actual industrial resilience or technological sovereignty. The
 *   extractiveness trajectory (0.42 → 0.58 over 10 years) shows accumulation
 *   of extraction mechanisms and erosion of coordination justification as
 *   geopolitical shocks expose fragility in concentrated supply chains. This
 *   constraint is a diagnostic exemplar of how industrial policy naturalizes
 *   extraction as security requirement.
 *
 * KEY AGENTS:
 *   - Tier-One Prime Contractors (BAE Systems, Rolls-Royce, Thales UK): Institutional beneficiaries (arbitrage exit) — capture strategic market access, preferential procurement, supplier dependency control, technology transfer from subcontractors
 *   - SME Supply Chain Participants: Primary victims (trapped exit) — face regulatory compliance burdens, security clearance costs, asymmetric contract terms, long payment cycles, technology lock-in, regional dependency
 *   - Ministry of Defence Procurement: Institutional actor (constrained exit) — enforces extraction mechanism while constrained by allied obligations, budget pressures, and strategic autonomy requirements
 *   - Supply Chain Resilience Advocates: Organized agents (constrained exit) — government reform initiatives, supply chain associations pushing for diversification and regional manufacturing alternatives
 *   - Defence Procurement Bureaucracy: Institutional maintenance apparatus (arbitrage exit) — sustains performative review and compliance systems with declining coordination function
 *   - Analytical Observer: Civilizational perspective (analytical exit) — risks naturalizing contingent policy choices as inherent security requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_defence_industrial_strategy, 0.58).
domain_priors:suppression_score(uk_defence_industrial_strategy, 0.65).
domain_priors:theater_ratio(uk_defence_industrial_strategy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_defence_industrial_strategy, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_defence_industrial_strategy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uk_defence_industrial_strategy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_defence_industrial_strategy, tangled_rope).
narrative_ontology:human_readable(uk_defence_industrial_strategy, "UK Defence Industrial Strategy Constraint").
narrative_ontology:topic_domain(uk_defence_industrial_strategy, "defence_policy/industrial_policy").

domain_priors:requires_active_enforcement(uk_defence_industrial_strategy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_defence_industrial_strategy, tier_one_defence_contractors).
narrative_ontology:constraint_beneficiary(uk_defence_industrial_strategy, ministry_of_defence_procurement).
narrative_ontology:constraint_victim(uk_defence_industrial_strategy, small_medium_enterprises).
narrative_ontology:constraint_victim(uk_defence_industrial_strategy, supply_chain_resilience).
narrative_ontology:constraint_victim(uk_defence_industrial_strategy, technological_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SME SUPPLY CHAIN PARTICIPANT (SNARE) — Structurally trapped by dependency on tier-one contractor relationships and defence procurement cycles. Faces high suppression: regulatory compliance burdens, security clearance requirements, long payment cycles, and specification lock-in prevent alternative markets. Extraction manifest in asymmetric contract terms, delayed payments, and technology transfer without proportional compensation. No viable exit — the defence sector represents significant portion of viable industrial work in many UK regions.
constraint_indexing:constraint_classification(uk_defence_industrial_strategy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER DEFENCE CONTRACTOR (TANGLED ROPE) — Constrained exit: heavily dependent on defence contracts but maintains some diversification into civilian sectors. Experiences both coordination benefits (stable procurement relationships, technology access) and extraction (price controls, sole-source dependencies on larger contractors, limited innovation autonomy). High suppression from regulatory environment and security requirements. Effective extraction moderate but significant.
constraint_indexing:constraint_classification(uk_defence_industrial_strategy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TIER-ONE PRIME CONTRACTOR (ROPE) — Experiences the strategy as coordination mechanism: guaranteed market access, strategic technology transfer from suppliers, regulatory moats protecting market share. Arbitrage capacity through exports and diversified portfolios. Benefits from supply chain control and preferential procurement. Minimal extraction experienced — constraint subsidizes this actor through assured demand and subcontractor dependency.
constraint_indexing:constraint_classification(uk_defence_industrial_strategy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INDUSTRIAL POLICY REFORM MOVEMENT (SCAFFOLD) — Organized actors (supply chain associations, government reform initiatives, regional development bodies) perceive the current concentration as a temporary coordination failure with structural sunset. Emergence of dual-use technologies, allied industrial integration, and regional manufacturing clusters create alternative pathways. Sunset logic: as alternative procurement channels and supply chain resilience mechanisms mature (estimated 10-15 years), direct dependency on tier-one relationships loses structural necessity. Constraint classified as scaffold with sunset clause during this transition period.
constraint_indexing:constraint_classification(uk_defence_industrial_strategy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEFENCE PROCUREMENT BUREAUCRACY (PITON) — The constraint maintenance apparatus (procurement rules, security protocols, technical standards) is substantially performative. Theater ratio (0.68) reflects that much procedural activity (committee reviews, compliance documentation, risk assessments) produces limited functional verification of actual supply chain resilience or industrial capacity. The bureaucracy sees its own process as degraded — sustained through institutional inertia and path dependency rather than active coordination function. Regulatory complexity persists despite modest real security benefit.
constraint_indexing:constraint_classification(uk_defence_industrial_strategy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MINISTRY OF DEFENCE INSTITUTION (TANGLED ROPE) — Constrained by NATO obligations, allied technology sharing requirements, and strategic autonomy imperatives. Experiences the constraint as both coordination (allied interoperability) and extraction (technology access costs, capability gaps, budget pressure from industrial base expansion requirements). Institutional directionality: the MoD both enforces extraction on the supply chain and experiences extraction from geopolitical constraints. Extraction moderate at institutional level.
constraint_indexing:constraint_classification(uk_defence_industrial_strategy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOPOLITICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, some concentration in defence industrial capacity is presented as immutable: complex weapons systems require specialized expertise, supply chains require security vetting, and strategic autonomy requires domestic industrial base protection. This perspective naturalizes institutional concentration as inherent to security requirements. However, the structural data reveals this as a false summit: the concentration is contingent on policy choices (procurement rules, technology control regimes, regional investment patterns), not inherent necessity. The engine will flag this as naturalization.
constraint_indexing:constraint_classification(uk_defence_industrial_strategy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_defence_industrial_strategy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_defence_industrial_strategy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_defence_industrial_strategy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_defence_industrial_strategy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_defence_industrial_strategy, TR),
    TR >= 0.70.

:- end_tests(uk_defence_industrial_strategy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Tier-one contractors extract significant value through supply chain control, preferential pricing, and technology access asymmetry. Suppression (0.65): Moderate-high. Multiple barriers prevent SME exit: security clearance requirements, regulatory compliance costs, regional manufacturing dependencies, payment cycle lock-in, and limited alternative markets for specialized defense capabilities. Theater_ratio (0.68): High. Procurement reviews, security assessments, and technical standards produce substantial performative activity with limited real verification of industrial resilience or technological sovereignty — actual capability often determined by political relationships and contractor capacity rather than formal procurement criteria. The trajectory shows increasing theater and extractiveness over the interval as geopolitical shocks expose fragility while procedural responses concentrate rather than diffuse.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates significant perspectival divergence. Tier-one contractors see Rope — coordination enabling technological capability and market stability. SMEs see Snare — extraction with trapped exit and high suppression. Mid-tier contractors see Tangled Rope — mixed coordination and extraction with constrained mobility. The MoD institution sees Tangled Rope — balancing allied coordination against domestic supply chain extraction costs. The organized reform movement sees Scaffold — a temporary concentration being dissolved by alternative procurement channels and regional manufacturing emergence. The procurement bureaucracy sees Piton — its own ritual degraded to theater, sustained by inertia. The analytical observer risks Mountain — naturalizing concentration as inherent to security requirements — but structural data reveals this as false summit: the concentration is policy-contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural position and exit capacity. Tier-one contractors experience low d (0.15-0.25): beneficiaries with arbitrage capacity. SME participants experience high d (0.85-0.95): victims with trapped exits. Mid-tier contractors experience moderate d (0.60-0.70): constrained exit with both coordination benefits and extraction costs. MoD institution experiences moderate-high d (0.70-0.80): constrained by allied obligations while enforcing extraction on supply chain. The organized reform movement experiences low-moderate d (0.40-0.50): constrained but with agency and exit pathways. Beneficiary/victim declarations establish the extraction flow: tier-one contractors benefit; SMEs, supply chain resilience, and technological sovereignty bear costs.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves mandatrophy by demonstrating that genuine coordination functions (allied interoperability, security standards) coexist with asymmetric extraction (price controls, technology transfer asymmetry). The requirement for active enforcement (complex procurement rules, security vetting, technical standards) confirms the hybrid classification. Beneficiary (tier-one contractors) and victim (SME supply chain, technological resilience) declarations establish the extraction asymmetry. Theater_ratio (0.68) confirms that substantial procedural activity does not translate to functional security or resilience verification. The constraint is neither pure coordination (Rope) nor pure extraction (Snare) but genuine mixture. The mandatrophy flags the risk of naturalizing extraction as inherent coordination cost — the analytical mountain perspective is a false summit that policy rhetoric frequently invokes ('defence requires concentration') but structural data contradicts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tier_one_dependency_necessity,
    'Is tier-one contractor dominance a structural requirement for defence industrial efficiency or a consequence of policy concentration choices?',
    'Comparative analysis of allied defence industrial bases (France, Germany, Poland) showing alternative structural models; analysis of actual technical requirements vs. stated consolidation rationales; dual-sourcing cost studies',
    'If structural necessity: constraint approaches Mountain classification — consolidation unavoidable. If policy choice: Snare classification confirmed — concentration is contingent extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tier_one_dependency_necessity, empirical, 'Whether tier-one dominance is structurally necessary or policy-contingent').

omega_variable(
    supply_chain_resilience_paradox,
    'Does supply chain concentration actually improve or degrade resilience to geopolitical shocks and industrial disruption?',
    'Stress testing of current supply chain under scenarios: allied sanctions, component supply disruption, single-point-of-failure analysis; comparison with distributed supplier models during stress periods',
    'If concentration improves resilience: extraction is justified coordination overhead. If concentration degrades resilience: theatre ratio rises and snare classification strengthens — the constraint maintains myths of security while generating fragility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_resilience_paradox, empirical, 'Whether concentration improves or degrades supply chain resilience').

omega_variable(
    dual_use_technology_boundary,
    'What fraction of defence industrial capacity serves genuinely military-only functions vs. dual-use or civilian-convertible capabilities?',
    'Product taxonomy analysis; comparison with allied dual-use industrial bases; assessment of actual security sensitivity of component-level production',
    'If high dual-use percentage: scope for alternative procurement pathways and supply chain diversification is large — scaffold sunset is real. If low dual-use: current concentration reflects genuine security requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_technology_boundary, empirical, 'Proportion of genuinely military-only vs. dual-use industrial capacity').

omega_variable(
    allied_interoperability_extraction,
    'Does technology access through allied relationships (NATO, Five Eyes) justify asymmetric extraction within UK supply chain, or do these represent separate constraints?',
    'Decomposition of UK industrial base extraction from geopolitical extraction; analysis of whether allied technology access actually flows to UK SMEs or concentrates with tier-one contractors',
    'If allied access enables distributed supply chain: SME extraction unjustified and should be reduced. If allied access concentrates with tier-one: extraction is layered coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_interoperability_extraction, conceptual, 'Whether allied technology access justifies UK domestic extraction').

omega_variable(
    regional_resilience_vs_concentration,
    'Can regional industrial diversification reduce geopolitical supply chain risk while maintaining defence capacity?',
    'Regional capability mapping; case studies of successful decentralized defence industrial bases; cost-benefit analysis of regional manufacturing hubs vs. current concentration',
    'If viable: scaffold perspective confirmed and sunset timeline becomes tractable. If infeasible: current concentration justified and constraint approaches Rope or Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_resilience_vs_concentration, empirical, 'Feasibility of regional diversification maintaining defence capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_defence_industrial_strategy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukdis_tr_t0, uk_defence_industrial_strategy, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ukdis_tr_t5, uk_defence_industrial_strategy, theater_ratio, 5, 0.6).
narrative_ontology:measurement(ukdis_tr_t10, uk_defence_industrial_strategy, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(ukdis_be_t0, uk_defence_industrial_strategy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ukdis_be_t5, uk_defence_industrial_strategy, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ukdis_be_t10, uk_defence_industrial_strategy, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_defence_industrial_strategy, enforcement_mechanism).
narrative_ontology:affects_constraint(uk_defence_industrial_strategy, semiconductor_supply_chain_security).
narrative_ontology:affects_constraint(uk_defence_industrial_strategy, defence_technology_transfer_regime).
narrative_ontology:affects_constraint(uk_defence_industrial_strategy, regional_manufacturing_resilience).

% DUAL FORMULATION NOTE:
% UK Defence Industrial Strategy decomposes into three structurally distinct constraints: (1) the industrial consolidation constraint (this story, ε=0.58, Tangled Rope) addressing procurement efficiency and market structure; (2) the semiconductor supply chain security constraint (ε=0.72, Snare) addressing critical component sourcing; (3) the technology transfer regime constraint (ε=0.42, Tangled Rope) addressing allied capability sharing. These share domain (defence) and institution (MoD) but have distinct ε values reflecting different observable bases: industrial consolidation, component-level supply, and information asymmetry respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_defence_industrial_strategy, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
