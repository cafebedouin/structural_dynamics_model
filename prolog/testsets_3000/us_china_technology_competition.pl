% ============================================================================
% CONSTRAINT STORY: us_china_technology_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_technology_competition, []).

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
 *   constraint_id: us_china_technology_competition
 *   human_readable: US-China Technology Competition as Coordination-Extraction Hybrid
 *   domain: geopolitical/economic/technology
 *
 * SUMMARY:
 *   The US-China technology competition is not a single constraint but a
 *   hybrid system exhibiting all characteristics of a tangled_rope: it
 *   possesses genuine coordination functions (mobilizing investment in
 *   semiconductor manufacturing, organizing supply-chain resilience,
 *   justifying R&D spending) while simultaneously extracting through
 *   suppression (export controls, visa restrictions, market access barriers,
 *   supply-chain fragmentation). The constraint has intensified since 2018
 *   (US trade war initiation) and escalated sharply since 2022 (advanced
 *   semiconductor export bans). The extractiveness trajectory shows 35%
 *   baseline (pre-2018, during era of embedded economic interdependence)
 *   rising to 58% by 2024 (current export control regime). Theater ratio has
 *   also risen from 38% (when security rationales were tethered to specific
 *   technologies) to 54% (current state where security rhetoric has become
 *   more elastic and harder to operationalize). The constraint operates
 *   simultaneously at multiple scales: bilateral (US-China direct
 *   competition), institutional (Wassenaar Arrangement, multilateral export
 *   controls), sectoral (semiconductor industry reorganization), and
 *   individual (scientist visa restrictions, talent recruitment pressures).
 *   Six distinct institutional actors occupy different structural positions:
 *   the US military-industrial complex and Chinese state security apparatus
 *   benefit as primary coordinators; intermediate suppliers like TSMC face
 *   constrained arbitrage; developing economies and scientists are trapped;
 *   supply-chain resilience initiatives perceive a temporary problem with a
 *   sunset. The analytical observer risks naturalizing this as immutable
 *   great-power competition, but structural decomposition reveals
 *   policy-contingent arrangements that could be reclassified through
 *   international cooperation or supply-chain investment.
 *
 * KEY AGENTS:
 *   - US Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — captures sustained defense funding justification and captive advanced-technology markets
 *   - Chinese State Security Apparatus: Primary beneficiary (institutional/arbitrage) — justifies state control over private tech sector and centralized innovation governance through sanctions narrative
 *   - US Technology Industry: Secondary beneficiary but also constrained (organized/constrained) — benefits from protected markets and government subsidies; faces compliance costs and export restrictions
 *   - Chinese State Enterprises: Secondary beneficiary but also constrained (organized/constrained) — benefits from protected domestic market and state subsidies; faces US technology denial and supply-chain pressure
 *   - Developing Economies: Primary victim (powerless/trapped) — trapped in supply-chain dependencies with no exit options; forced to choose geopolitical alignment
 *   - International Scientific Community: Mixed victim/identity-locked (powerless/identity_locked) — structurally mobile but identity-fused with open-science norms; constrained by export controls and visa restrictions
 *   - Intermediate Technology Suppliers (TSMC, SK Hynix, ASML): Moderate position (moderate/constrained) — benefit from competition-driven demand but face pressure to choose alignment
 *   - Supply Chain Resilience Initiatives: Organized coalition (organized/constrained) — perceive temporary coordination challenge with sunset clause through regional fab investment
 *   - Cold War Security Framework (Export Control Bureaucracies): Institutional actor maintaining piton status (institutional/arbitrage) — benefits from continued control mechanisms; sees own process as partly degraded but maintains it through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_technology_competition, 0.58).
domain_priors:suppression_score(us_china_technology_competition, 0.68).
domain_priors:theater_ratio(us_china_technology_competition, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_technology_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_china_technology_competition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_china_technology_competition, theater_ratio, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_technology_competition, tangled_rope).
narrative_ontology:human_readable(us_china_technology_competition, "US-China Technology Competition as Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(us_china_technology_competition, "geopolitical/economic/technology").

domain_priors:requires_active_enforcement(us_china_technology_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_technology_competition, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(us_china_technology_competition, chinese_state_enterprises).
narrative_ontology:constraint_beneficiary(us_china_technology_competition, semiconductor_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(us_china_technology_competition, defense_contractors).
narrative_ontology:constraint_victim(us_china_technology_competition, global_innovation_ecosystem).
narrative_ontology:constraint_victim(us_china_technology_competition, international_scientific_collaboration).
narrative_ontology:constraint_victim(us_china_technology_competition, developing_economies).
narrative_ontology:constraint_victim(us_china_technology_competition, consumers_in_both_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING ECONOMY TECH SECTOR (SNARE) — Trapped between US and Chinese supply chain control. Cannot exit semiconductor dependency, cannot build autonomous chip fabs, cannot access cutting-edge technology without choosing geopolitical alignment. Maximum extraction with minimal coordination benefit. No alternative pathways; structural constraints are immutable from this position.
constraint_indexing:constraint_classification(us_china_technology_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL SCIENTIFIC COMMUNITY (SNARE/IDENTITY_LOCKED) — Structurally mobile (could theoretically collaborate internationally) but identity-fused with open-science norms and collaborative research. Export controls, visa restrictions, and institutional nationality screening trap scientists within competitive frameworks they did not choose and intellectually oppose. Identity lock: the scientist's professional identity is constituted through international collaboration; exit means abandoning the core meaning of their work. The suppression is both structural (legal barriers) and internalized (cognitive capture by nationalist framing of 'protecting' innovation).
constraint_indexing:constraint_classification(us_china_technology_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: US TECHNOLOGY INDUSTRY (TANGLED ROPE) — Constrained by export controls and supply-chain geopolitics but benefits from protected market access, government subsidies, and forced outsourcing of manufacturing. Genuine coordination function: the constraint organizes investment in domestic semiconductor manufacturing and supply-chain diversification. But asymmetric extraction: smaller firms and startups face higher compliance costs and market restrictions. Some escape through arbitrage (moving manufacturing offshore); larger firms navigate through lobbying.
constraint_indexing:constraint_classification(us_china_technology_competition, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CHINESE STATE TECHNOLOGY ENTERPRISES (TANGLED ROPE) — Constrained by US export bans and technology denial but benefits from protected domestic market and state subsidies. Coordination function: the constraint coordinates domestic chip design, manufacturing, and supply-chain building. Extraction: US sanctioning creates asymmetric costs; state enterprises extract from competitors through preferential state funding and market protection. Exit costs are very high for individual firms; exit via state subsidy is theoretically available to favored entities.
constraint_indexing:constraint_classification(us_china_technology_competition, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: US MILITARY-INDUSTRIAL COMPLEX (ROPE) — Primary beneficiary. The constraint organizes defense investment, justifies funding increases, and creates captive markets for advanced technology. Experiences the constraint as coordination: geopolitical competition creates sustained demand for innovation and military capability. Massive arbitrage options: can shape policy, lobbies for export controls, benefits from supply-chain restrictions. Net extraction runs toward this agent.
constraint_indexing:constraint_classification(us_china_technology_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CHINESE STATE SECURITY APPARATUS (ROPE) — Primary beneficiary. The constraint justifies technology investment, centralizes control over private sector innovation through national champions, and coordinates state resources toward technological self-sufficiency. Experiences the constraint as coordination: US sanctions create legitimate justification for state control and centralized innovation governance. Arbitrage options: state can reshape policy, direct investment, and consolidate control over tech sector. Net extraction runs toward this agent.
constraint_indexing:constraint_classification(us_china_technology_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERMEDIATE TECHNOLOGY SUPPLIERS (TANGLED ROPE) — Moderate power, constrained exit. Companies like TSMC, SK Hynix, ASML occupy a middle position: they benefit from the competition (high demand for foundry services, equipment sales to both sides) but face intense pressure to choose geopolitical alignment. Exit costs are rising as supply-chain decoupling progresses. Coordination benefit: the constraint has forced investment in alternative foundries and localization. Extraction: restricted market access, forced diversification, political pressure from both US and China.
constraint_indexing:constraint_classification(us_china_technology_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: COLD WAR SECURITY FRAMEWORK (PITON) — The US-China tech competition is partly a continuation of Cold War-era export control architecture (COCOM, Wassenaar Arrangement, EAR restrictions). These institutional mechanisms persist despite their original target (Soviet Union) being gone for 35 years. Theater ratio is high: the security rhetoric ('protecting national security') is maintained, but the actual functional necessity is contested. The constraint persists through institutional inertia and because security bureaucracies have internalized the control mechanisms as normal. Theater has increased as rationales shift from Soviet containment to 'protecting strategic advantage' — a more elastic and harder-to-falsify justification.
constraint_indexing:constraint_classification(us_china_technology_competition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: SUPPLY CHAIN RESILIENCE INITIATIVES (SCAFFOLD) — Actors building alternative semiconductor fabs, diversifying supply chains, and investing in technology independence (US CHIPS Act, EU Chips Act, Japanese Rapidus, South Korean government support) see the competition as a temporary coordination challenge with a sunset clause. The hypothesis: distributed regional manufacturing reduces dependency on any single geopolitical actor, creating exit pathways. Low effective extraction because the organized coalition has agency and perceives an end state. Sunset timeline: 10-15 years for mature alternative supply chains in key regions.
constraint_indexing:constraint_classification(us_china_technology_competition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / NATURALIZING VIEW (MOUNTAIN) — The analytically naive position treats the US-China tech competition as an immutable feature of great-power geopolitics — inherent to multipolarity, reducible to material capabilities and geographic position. This perspective risks naturalizing what is actually a contingent institutional arrangement (export controls, supply-chain policy, investment priorities) as 'the' structure of international competition. The false summit marker: the constraint can be reclassified through policy change, supply-chain investment, and international cooperation. It is not a law of nature.
constraint_indexing:constraint_classification(us_china_technology_competition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_technology_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_technology_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_technology_competition, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_technology_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_china_technology_competition, TR),
    TR >= 0.70.

:- end_tests(us_china_technology_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significantly through suppression (export controls, supply-chain fragmentation, talent restrictions) but retains genuine coordination functions (mobilizes investment, justifies R&D, organizes supply-chain resilience). The value reflects that extraction is neither total (some coordination benefit exists; some arbitrage options available to powerful actors) nor minimal (real costs to trapped and constrained agents are substantial). The trajectory from 0.35 to 0.58 reflects escalation since 2018 and particularly the 2022 semiconductor export ban regime. Suppression (0.68): High. Multiple binding mechanisms: export controls (legal, enforceable), supply-chain fragmentation (economic, structural), visa restrictions (legal), technology denial (technical), and cognitive capture of security rhetoric (internalized). Suppression has multiple mechanisms and is difficult for any agent to circumvent unilaterally, hence high value. Theater ratio (0.54): Moderate. The security rhetoric persists (protection from strategic threat) but has become increasingly elastic. Original Cold War controls had specific targets (Soviet Union); current controls justify 'protecting strategic advantage' which is harder to falsify. The theater has risen from 38% (more specific, operationalized rationales) to 54% (broader, more rhetorical justifications). This is not yet the piton threshold (0.70) but reflects significant performative content in how the constraint is justified and maintained.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a wide perspectival range from Snare (developing economies, trapped scientists) through Tangled Rope (industry, state enterprises) to Rope (military-industrial beneficiaries) to Scaffold (supply-chain initiatives) to Piton (security bureaucracies) to Mountain (false naturalizing view). The gap is maximized between powerless victims (d ≈ 0.90, Snare experience) and institutional beneficiaries (d ≈ 0.05, Rope experience) — the same constraint appears to one as pure extraction with no exit, to the other as pure coordination with full arbitrage. Middle-position agents (TSMC, industry players, scientists) experience Tangled Rope or constrained Snare depending on their organizational capacity and identity commitments. The mountain perspective is revealing: it treats the constraint as immutable great-power competition ('multipolarity requires tech separation') but the structural analysis shows policy-contingent arrangements that could be reframed through cooperation. This perspectival gap is diagnostic of whether the constraint is inherent or constructed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows from structural position and exit options. Military-industrial complex: benefits from constraint (d ≈ 0.05) + arbitrage exit → low f(d) → negative effective extraction. Chinese state apparatus: similar beneficiary position (d ≈ 0.10) + arbitrage exit → low f(d) → benefits from constraint. US technology industry: mixed (beneficiary through subsidies + victim through restrictions) (d ≈ 0.45) + constrained exit → moderate f(d) ≈ 0.60. Chinese state enterprises: similar mixed position (d ≈ 0.50) + constrained exit → moderate f(d) ≈ 0.65. Developing economies: pure victim (d ≈ 0.90) + trapped exit → high f(d) ≈ 1.25 → maximum experienced extraction. Scientists: mixed position (structurally mobile but identity-locked) (d ≈ 0.80) with identity_locked exit → high f(d) ≈ 1.10 → significant experienced extraction despite structural mobility. Intermediate suppliers: mixed beneficiary/victim (d ≈ 0.55) + constrained exit → moderate f(d) ≈ 0.75. Supply chain initiatives: organized agents (d ≈ 0.40) + constrained exit → moderate f(d) ≈ 0.55 → lower experienced extraction because organization provides agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in US-China tech competition is the claim that the constraint is both essential coordination (justified by security and resilience needs) and harmful extraction (creates costs for scientists, developing economies, innovation velocity). Resolution requires recognizing that both are true: the constraint DOES coordinate around security goals and supply-chain resilience, AND it DOES extract through suppression and market fragmentation. The Tangled Rope classification resolves the apparent contradiction. The constraint is not 'really' coordination with extraction as window dressing, nor 'really' extraction disguised as coordination — it is genuinely hybrid. The mandatrophy resolves by accepting that coordination and extraction are not mutually exclusive; they are orthogonal properties. A constraint can satisfy both simultaneously. The key question becomes: what is the equilibrium ratio of coordination benefit to extraction cost? If supply-chain resilience initiatives succeed (Scaffold sunset realization), the ratio shifts toward coordination. If export controls become purely rhetorical maintenance with no functional benefit (Piton degradation), the ratio shifts toward extraction. The constraint is not misclassified; it is properly classified as Tangled Rope, and the question is whether it will mature toward Rope (more coordination, less extraction) or degrade toward Snare (more extraction, less coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    export_control_effectiveness,
    'Do US export controls on advanced semiconductors and chip design tools meaningfully slow Chinese technological development, or do they primarily accelerate internal R&D while creating gaps that third-party vendors fill?',
    'Longitudinal comparison of Chinese semiconductor capability advancement rates pre- and post-2022 export controls; tracking of third-party fill-in vendors and licensing workarounds; assessment of real-world capability gaps vs sanctions intentions',
    'If effective: extraction is real and suppression is structurally binding (Snare classification holds). If ineffective: extraction is mostly theater and the constraint is degrading (Piton reclassification likely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(export_control_effectiveness, empirical, 'Whether US export controls on semiconductors are functionally effective or create workarounds').

omega_variable(
    supply_chain_decoupling_feasibility,
    'Can the US and allied nations build economically viable alternative semiconductor and advanced technology supply chains that are truly independent of Chinese sources, or does decoupling require permanent economic efficiency costs and protected markets?',
    'Cost-benefit analysis of regional fabs vs TSMC equivalents; modeling of supply-chain resilience vs economic performance; assessment of whether alternative supply chains require sustained government subsidy or become economically self-sustaining',
    'If feasible without permanent subsidy: scaffold sunset is real and alternative pathways are structurally viable. If requiring permanent protection: the constraint shifts from tangled_rope toward snare with regard to consumers and developing economies (who bear the efficiency cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_decoupling_feasibility, empirical, 'Whether economically viable alternative semiconductor supply chains can be built independently').

omega_variable(
    innovation_acceleration_vs_fragmentation,
    'Does geopolitical competition in technology accelerate global innovation (justified as competitive pressure driving R&D) or does supply-chain fragmentation, export restrictions, and reduced collaboration primarily degrade innovation velocity in both systems?',
    'Comparative analysis of innovation metrics (patents, publications, startup formation, commercial deployment) in periods of open collaboration vs current fragmentation; measurement of duplicate/redundant R&D in separated ecosystems vs efficiency gains from competition',
    'If acceleration: the constraint has a genuine coordination function for innovation (tangled_rope classification holds). If fragmentation-driven degradation: the constraint is primarily extractive with minimal coordination (snare reclassification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_acceleration_vs_fragmentation, empirical, 'Whether geopolitical tech competition accelerates or degrades global innovation').

omega_variable(
    identity_lock_mechanism_in_science,
    'For international scientists experiencing export controls and visa restrictions, is the binding mechanism structural (material barriers to collaboration) or internalized (identity fusion with open-science norms creating perceived impossibility of work under restrictions)?',
    'Post-restriction analysis of scientist migration patterns, publication productivity changes, and reported vs actual barriers to continued collaboration; assessment of whether restrictions change behavior through material constraint vs cognitive reframing of what ''proper science'' means under nationalist frameworks',
    'If structural: identity_locked reclassifies to trapped (purely material barriers). If internalized: identity_locked stands, revealing that the suppression is partly self-imposed through cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_science, empirical, 'Whether scientific collaboration restrictions operate through structural or internalized binding mechanisms').

omega_variable(
    institutional_inertia_in_cold_war_controls,
    'Are current export control mechanisms (EAR, ITAR, Wassenaar) functionally necessary for national security or do they persist primarily through bureaucratic inertia and security establishment self-perpetuation?',
    'Historical analysis of control origins (post-COCOM), current threat assessment vs original rationales, cost-benefit analysis of control overhead vs documented security gains, assessment of whether removal would create measurable national security degradation or primarily bureaucratic disruption',
    'If primarily inertia: the piton classification is correct and the constraint is theater masquerading as necessity. If functionally necessary: the constraint shifts toward justified tangled_rope (extraction is the cost of real security coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_inertia_in_cold_war_controls, preference, 'Whether export control mechanisms are functionally necessary or institutionally inertial').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_technology_competition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_china_technology_competition, theater_ratio, 0, 0.38).
narrative_ontology:measurement(us_c_tr_t5, us_china_technology_competition, theater_ratio, 5, 0.46).
narrative_ontology:measurement(us_c_tr_t10, us_china_technology_competition, theater_ratio, 10, 0.54).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_china_technology_competition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(us_c_be_t5, us_china_technology_competition, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(us_c_be_t10, us_china_technology_competition, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_technology_competition, global_infrastructure).
narrative_ontology:affects_constraint(us_china_technology_competition, semiconductor_supply_chain_security).
narrative_ontology:affects_constraint(us_china_technology_competition, ai_chip_export_controls).
narrative_ontology:affects_constraint(us_china_technology_competition, foreign_talent_acquisition_restrictions).
narrative_ontology:affects_constraint(us_china_technology_competition, quantum_computing_race).

% DUAL FORMULATION NOTE:
% The US-China technology competition is upstream of multiple specific constraints (semiconductor supply chain, AI chip exports, talent competition, quantum computing) which inherit both the coordination and extraction properties of the parent constraint. Each downstream constraint can be analyzed separately with its own epsilon and perspectives, but all show network effects from the parent's policy regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_china_technology_competition, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
