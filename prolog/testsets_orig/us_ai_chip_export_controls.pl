% ============================================================================
% CONSTRAINT STORY: us_ai_chip_export_controls
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_ai_chip_export_controls, []).

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
 *   constraint_id: us_ai_chip_export_controls
 *   human_readable: US Export Controls on Advanced AI Chips to China
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   US export controls on advanced semiconductor chips represent a
 *   geopolitical chokepoint mechanism designed to restrict China's AI
 *   capabilities through supply-chain control. The constraint exhibits hybrid
 *   tangled-rope structure: it coordinates legitimate strategic objectives
 *   (preventing adversary technological dominance) while simultaneously
 *   extracting economic rent (monopoly pricing for US/allied chip producers)
 *   and suppressing alternatives (forcing resource-intensive domestic
 *   development in China). The mechanism's effectiveness depends critically
 *   on maintaining unified allied enforcement (Taiwan, South Korea,
 *   Netherlands chipmaking/equipment) and preventing circumvention routes
 *   (chiplet assemblies, indirect imports, third-party sourcing). The
 *   constraint's evolution shows increasing theater: formal export controls
 *   on individual chips encounter growing gaps as workarounds proliferate
 *   (chiplet designs, parameter-reduced models, distributed training across
 *   regions). The regime is simultaneously effective (measurably slowing
 *   Chinese model development) and failing (unable to prevent access
 *   indefinitely). This diagnostic tension defines the tangled-rope
 *   character: real coordination value married to real extraction, genuine
 *   enforcement coupled with theatrical gaps.
 *
 * KEY AGENTS:
 *   - US AI Industry & Semiconductor Manufacturers: Primary beneficiary (institutional/arbitrage) — secured market dominance, pricing power, delayed Chinese competition. Maximum exit flexibility.
 *   - Chinese AI Research Ecosystem: Primary victim (powerless/trapped) — forced inferior technical paths, resource diversion, sanctions risk. No exit options.
 *   - Chinese Government/Strategic Competitors: Secondary victim (organized/constrained) — constrained but not trapped; benefits from forced domestic R&D but experiences tech lag and supply anxiety.
 *   - US Commerce Department: Enforcer (institutional/constrained) — maintains control apparatus; experiences growing theater ratio as workarounds proliferate.
 *   - Taiwan, South Korea, Netherlands: Allied producers (powerful/mobile) — pressured to align but retain mobile exit options; can fragment or renegotiate supply chains.
 *   - Global AI Research (non-Chinese): Moderate actor (moderate/mobile) — competitive advantages from Chinese restriction offset by supply fragmentation and higher costs.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing technological containment as inevitable, overlooking historical precedent of chokepoint dissolution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_ai_chip_export_controls, 0.58).
domain_priors:suppression_score(us_ai_chip_export_controls, 0.72).
domain_priors:theater_ratio(us_ai_chip_export_controls, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_ai_chip_export_controls, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_ai_chip_export_controls, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_ai_chip_export_controls, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_ai_chip_export_controls, tangled_rope).
narrative_ontology:human_readable(us_ai_chip_export_controls, "US Export Controls on Advanced AI Chips to China").
narrative_ontology:topic_domain(us_ai_chip_export_controls, "geopolitical/technological").

domain_priors:requires_active_enforcement(us_ai_chip_export_controls).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_ai_chip_export_controls, us_ai_industry).
narrative_ontology:constraint_beneficiary(us_ai_chip_export_controls, us_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(us_ai_chip_export_controls, us_national_security_apparatus).
narrative_ontology:constraint_victim(us_ai_chip_export_controls, chinese_ai_research_ecosystem).
narrative_ontology:constraint_victim(us_ai_chip_export_controls, chinese_semiconductor_industry).
narrative_ontology:constraint_victim(us_ai_chip_export_controls, global_chip_supply_chain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHINESE AI RESEARCH ECOSYSTEM (SNARE) — Trapped without meaningful exit options. Unable to source advanced chips legally; faces escalating sanctions for circumvention attempts. Bears full extraction cost: forced to pursue inferior technical paths, slower model development, massive resource diversion to workarounds. Suppression is nearly total — the constraint exists specifically to prevent exit routes.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CHINESE GOVERNMENT & STRATEGIC COMPETITORS (TANGLED ROPE) — Organized actor with constrained exit. Faces resource diversion and technical delays, but also benefits from the constraint through: (a) domestic semiconductor investment incentives, (b) national autonomy from supply-chain dependency, (c) justification for state-directed R&D spending. The constraint both extracts (tech lag) and coordinates (forces domestic chip development). Active enforcement required; genuine hybrid structure.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US AI INDUSTRY & SEMICONDUCTOR MANUFACTURERS (ROPE) — Primary beneficiary. Experiences the constraint as coordination: prevents Chinese competition, secures market dominance, increases export pricing power. Has arbitrage exit: can choose compliance strategies, lobby for policy adjustment, or shift supply chains. Net benefit significantly exceeds extraction — this is their constraint design.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US COMMERCE DEPARTMENT / EXPORT CONTROL APPARATUS (PITON) — Institutional actor maintaining enforcement rituals that increasingly exceed functional capability. Theater ratio reflects growing divergence between stated control objectives and enforcement reality: jurisdiction gaps, corporate workarounds, re-export schemes, and chiplet circumvention create large gaps between regulatory text and actual control. The apparatus persists through institutional inertia and bureaucratic necessity, not because it effectively prevents chip access. Theater has increased as workarounds proliferate.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GLOBAL SUPPLY CHAIN & ALLIED NATIONS (SCAFFOLD) — Powerful actors experiencing temporary constraints with emerging exit paths. Taiwan, South Korea, Netherlands (chip equipment) face pressure to align export policies, but have mobile exit options: they can shift supply chains, negotiate bilateral deals, or develop domestic alternatives. The constraint exhibits sunset dynamics: as Chinese domestic chip capabilities mature and as allied nations develop competing supply chains, the control mechanism weakens. Sunset mechanism is coalition fragmentation rather than formal policy revision.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: NON-CHINESE AI RESEARCH & GLOBAL ECOSYSTEM (TANGLED ROPE) — Moderate actor with mobile exit options experiencing mixed effects. Benefits from Chinese research restriction (reduced computational competition, pricing advantages). Harmed by supply-chain fragmentation (regional monopolies, higher costs, slower innovation). Has mobile exit: can source from multiple regions, invest in alternatives, or relocate. The constraint coordinates some research standardization while extracting through raised capital costs.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — At the civilizational scope, one might perceive technological chokepoint dynamics as immutable natural law: any advancing civilization's core technologies are scarce and defended. However, this is a false summit. The US export control regime is a contingent institutional arrangement, not a law of nature. Historical precedent (memory chips, supercomputers) shows chokepoints dissolve as technologies proliferate and alternatives emerge. The naturalization of strategic containment as inevitable overlooks that technological diffusion is the actual constraint that cannot be contained long-term.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_ai_chip_export_controls_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_ai_chip_export_controls, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_ai_chip_export_controls, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_ai_chip_export_controls, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_ai_chip_export_controls, TR),
    TR >= 0.70.

:- end_tests(us_ai_chip_export_controls_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not total. The constraint extracts significant value from Chinese actors (forced R&D spending, delayed capabilities) and from global consumers (restricted supply, higher prices), but Chinese alternatives and chiplet workarounds are emerging, reducing extraction efficacy over time. The initial value (0.32) reflects a functioning but incomplete control; the current value (0.58) reflects increasing enforcement intensity and supplier alignment, but not perfect suppression. The measurement trajectory shows active extraction intensification: each new control iteration (2022 controls → 2023 expanded → 2024 allied pressure) increases extractiveness until chiplet alternatives mature. Suppression (0.72): High but not absolute. Barriers to Chinese access are severe (legal restrictions, re-export controls, equipment limitations, talent constraints), but significant gaps persist (indirect purchasing, third-party intermediaries, heterogeneous architectures, eventual domestic capability). Theater ratio (0.48): Moderate and increasing. The control apparatus maintains substantial functional enforcement (chip flows genuinely restricted, supply chains genuinely fragmented), but growing theater as regulatory text diverges from enforcement reality: Export Administration Regulations increasingly describe ideal control rather than achieved control. Chiplet designs, distributed training, and parameter-reduced models represent evasion strategies that create symbolic-functional gaps.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The US AI industry sees pure coordination (Rope) — efficient market control with externalities as minor collateral cost. The Chinese ecosystem sees pure extraction (Snare) — trapped without alternatives, bearing full cost. The US enforcement apparatus sees its ritual as functional (Rope) but objective analysis reveals increasing theater (Piton) as workarounds multiply. The global supply chain sees temporary disruption with emerging exits (Scaffold) as alternatives develop. Allied producers see constrained but mobile positions (Tangled Rope) — pressured but not trapped. The analytical observer risks false naturalization (Mountain) but the structural data reveals a contingent institutional arrangement maintained through active enforcement rather than inherent necessity. No single perspective dominates; the presheaf over observation positions reveals the full six-type spectrum.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values track agent structural position in the extraction flow. Chinese ecosystem: d ≈ 0.95 (trapped victim) → f(d) ≈ 1.42 → experiences maximum χ despite moderate ε. US beneficiary: d ≈ 0.05 (beneficiary with arbitrage) → f(d) ≈ -0.12 → experiences negative effective extraction (profit). US enforcer: d ≈ 0.55 (organized victim of enforcement burden) → f(d) ≈ 0.75 → moderate χ but theater moderates perceived severity. Scope modifier σ(global) = 1.2 amplifies χ for global-scope perspectives due to verification difficulty across jurisdictions. The engine's derivation chain confirms that beneficiaries with arbitrage experience net benefit; victims with no exit experience maximum burden.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint resolves mandatrophy through explicit hybrid structure. (1) Genuine coordination function: US national security objective of preventing adversary AI dominance is real and accepted across political spectrum. (2) Asymmetric extraction: Simultaneously extracts economic rent via monopoly pricing and tech-leadership preservation. (3) Active enforcement required: Maintains force through legal restrictions, allied pressure, supply-chain monitoring. (4) Dual beneficiary structure: US AI industry gains market power AND US strategic security is preserved (genuine public good). The classification is NOT a false coordinate as either pure extraction (which ignores security coordination value) or pure coordination (which ignores rent extraction). The mandatrophy is resolved by acknowledging both functions operate simultaneously, making Tangled Rope the accurate type. The false mountain perspective (inevitable technological chokepoint) is correctly identified as naturalization: historically, technological chokepoints dissolve faster than political institutions can maintain controls, suggesting the constraint is institutional rather than natural-law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_chinese_chip_maturity_timeline,
    'What is the realistic timeline for Chinese domestic semiconductor manufacturing to achieve parity with restricted US/Taiwan nodes for AI training?',
    'Technical benchmarking of Chinese fab output (SMIC, Huawei HiSilicon); defect rates and yield data; process node capability tracking',
    'If < 5 years: controls are temporary coordination (Scaffold) with real sunset. If > 15 years: controls function as long-term extraction (Snare). Timeline determines classification robustness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_chinese_chip_maturity_timeline, empirical, 'Timeline for Chinese domestic chip capability parity').

omega_variable(
    chiplet_circumvention_sufficiency,
    'Can advanced AI model training be accomplished using chiplet assemblies and heterogeneous architectures that bypass individual-chip export restrictions?',
    'Technical feasibility analysis; modeling of chiplet-based training systems; examination of bandwidth, latency, and scaling requirements',
    'If sufficient: export controls are severely compromised (Piton theater dominates). If insufficient: controls remain functionally effective (Tangled Rope extraction is real).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chiplet_circumvention_sufficiency, empirical, 'Whether chiplet architectures enable training without restricted chips').

omega_variable(
    allied_nation_compliance_sustainability,
    'Will Taiwan, South Korea, and Netherlands maintain export control alignment as Chinese economic pressure and domestic alternatives increase?',
    'Bilateral negotiation data; supply-chain audits; detection of indirect re-export schemes; shifting diplomatic positioning',
    'If compliance holds: Scaffold with long sunset (20+ years). If compliance fractures: Rope dominates as controls become merely coordination among willing US allies, not global enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_nation_compliance_sustainability, empirical, 'Sustainability of allied nation export control compliance').

omega_variable(
    us_domestic_chip_capacity_cost,
    'Can US domestic AI chip production meet global demand at price points competitive with unrestricted supply, or is the control regime sustaining above-market pricing indefinitely?',
    'Manufacturing cost data (TSMC vs US fabs); capital expenditure trends; wafer-output capacity projections; pricing pressure analysis',
    'If cost-competitive: controls coordinate global market (Rope). If perpetually above-market: controls extract via monopoly rent (Snare/Tangled Rope with indefinite extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_domestic_chip_capacity_cost, empirical, 'Whether US domestic production is cost-competitive at scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_ai_chip_export_controls, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aichip_tr_t0, us_ai_chip_export_controls, theater_ratio, 0, 0.28).
narrative_ontology:measurement(aichip_tr_t3, us_ai_chip_export_controls, theater_ratio, 3, 0.38).
narrative_ontology:measurement(aichip_tr_t6, us_ai_chip_export_controls, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(aichip_be_t0, us_ai_chip_export_controls, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(aichip_be_t3, us_ai_chip_export_controls, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(aichip_be_t6, us_ai_chip_export_controls, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_ai_chip_export_controls, global_infrastructure).
narrative_ontology:affects_constraint(us_ai_chip_export_controls, chinese_semiconductor_industry_self_sufficiency).
narrative_ontology:affects_constraint(us_ai_chip_export_controls, global_ai_training_cost_index).
narrative_ontology:affects_constraint(us_ai_chip_export_controls, taiwan_geopolitical_leverage).

% DUAL FORMULATION NOTE:
% The US export control regime decomposes into two distinct constraints: (1) Chip-level controls (immediate supply restriction, high extraction, high suppression) — creates immediate bottleneck, (2) Ecosystem-level constraints (capability development delay, forced domestic investment) — creates long-term constraint with sunset dynamics as Chinese alternatives mature. The unified regime exhibits tangled-rope structure only when analyzed as hybrid; analyzed separately, component constraints range from Rope (strategic coordination) to Snare (short-term supply starvation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_ai_chip_export_controls, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
