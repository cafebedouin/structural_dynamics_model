% ============================================================================
% CONSTRAINT STORY: scurvy_maritime_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   constraint_id: scurvy_maritime_extraction
 *   human_readable: The Scurvy/Empire Trade-off: Biological Constraint and Institutional Extraction
 *   domain: biological/political/economic
 *
 * SUMMARY:
 *   Scurvy was a primary killer on long-distance ocean voyages during the Age
 *   of Sail (15th-18th centuries), but it represents not a pure biological
 *   constraint but a structural tension between biological limits and
 *   institutional extraction. The constraint operates at multiple levels: (1)
 *   the irreducible biological fact that humans require dietary vitamin C;
 *   (2) the technical problem of provisioning ships for months-long voyages
 *   without refrigeration; (3) the institutional choice to minimize
 *   provisions to maximize cargo space and profit; (4) the power dynamic that
 *   allows companies to impose mortality on crews without accountability.
 *   This constraint exhibits the full spectrum of DR classification. From the
 *   crew's perspective, it is a pure snare: they face 40-50% mortality from a
 *   preventable cause with no exit option. From the company's perspective, it
 *   is a coordination problem (rope): how to provision ships efficiently.
 *   From the analytical observer's perspective, it risks being falsely
 *   naturalized as a mountain — 'humans need vitamin C, long voyages are
 *   dangerous' — when the actual constraint is institutional: 'companies
 *   choose to withhold known cures to maximize profit.' The constraint's
 *   extractiveness increased over the period as company provisioning
 *   practices became more systematized and less ad-hoc, and as the scientific
 *   understanding of scurvy became available (late 17th century) yet was
 *   systematically delayed in implementation (until 1795 in British Navy).
 *   The theater ratio increased over the same period as naval institutions
 *   developed elaborate ideologies of 'hardy sailor' culture and natural
 *   hardship to justify known-preventable mortality.
 *
 * KEY AGENTS:
 *   - Ship's Crew: Primary victim (powerless/trapped) — face 40-50% mortality rates from scurvy; no exit short of mutiny or desertion
 *   - Imperial Trading Companies (e.g., VOC, EIC): Primary beneficiary (institutional/arbitrage) — maximize profit by minimizing provisions and cargo space allocation to crew support
 *   - Metropolitan Governments (Portugal, Spain, England, Netherlands): Secondary beneficiary and organized actor (organized/constrained) — regulate trade routes, deploy naval power, extract colonial wealth; see scurvy as strategic constraint on imperial expansion
 *   - Colonial Settlements: Secondary victim and moderate actor (moderate/constrained) — receive manufactured goods and capital but export raw materials under asymmetric terms; cannot exit imperial supply chains
 *   - Naval Establishment: Institutional actor (institutional/arbitrage) — maintains provisioning ideology and crew control systems despite knowledge of scurvy causes; represents inertial piton perspective
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional choices as biological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scurvy_maritime_extraction, 0.52).
domain_priors:suppression_score(scurvy_maritime_extraction, 0.68).
domain_priors:theater_ratio(scurvy_maritime_extraction, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scurvy_maritime_extraction, extractiveness, 0.52).
narrative_ontology:constraint_metric(scurvy_maritime_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(scurvy_maritime_extraction, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scurvy_maritime_extraction, tangled_rope).
narrative_ontology:human_readable(scurvy_maritime_extraction, "The Scurvy/Empire Trade-off: Biological Constraint and Institutional Extraction").
narrative_ontology:topic_domain(scurvy_maritime_extraction, "biological/political/economic").

domain_priors:requires_active_enforcement(scurvy_maritime_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scurvy_maritime_extraction, imperial_trading_companies).
narrative_ontology:constraint_beneficiary(scurvy_maritime_extraction, metropolitan_governments).
narrative_ontology:constraint_beneficiary(scurvy_maritime_extraction, ship_owners_capital_holders).
narrative_ontology:constraint_victim(scurvy_maritime_extraction, ship_crews).
narrative_ontology:constraint_victim(scurvy_maritime_extraction, colonies_indigenous_populations).
narrative_ontology:constraint_victim(scurvy_maritime_extraction, long_distance_maritime_expansion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SHIP'S CREW (SNARE) — Pressed or conscripted sailors face 40-50% mortality rates on long voyages, primarily from scurvy. No exit short of mutiny or desertion (both punishable by death). The biological constraint (vitamin C deficiency) is real, but its severity is actively amplified by provisioning decisions made to maximize company profit. Crews see pure extraction: survival requires vitamin-rich provisions (citrus, fresh greens) that are deliberately withheld to save cargo space for tradeable goods. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.72. Terminal extraction — crews have no bargaining power.
constraint_indexing:constraint_classification(scurvy_maritime_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE COLONIAL SETTLEMENT (TANGLED ROPE) — Colonies experience mixed coordination and extraction. Long-distance supply chains from Europe provide manufactured goods and capital, enabling colonial settlement. But these same chains extract raw materials and agricultural surplus under asymmetric terms. Colonies cannot exit the imperial trading network (constrained) — independence requires local production capacity they lack. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.50. The constraint enables colonial expansion (coordination function) while simultaneously extracting resources (asymmetric extraction) — defining tangled_rope.
constraint_indexing:constraint_classification(scurvy_maritime_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE IMPERIAL TRADING COMPANY (ROPE) — From the company's structural position, scurvy is a coordination problem, not an extraction mechanism. The biological constraint limits voyage duration; solving it requires coordination: provisioning decisions, sailing route optimization, crew knowledge exchange. The company benefits from long-distance trade and sees the constraint as a technical problem to solve through innovation (faster routes, better navigation, eventually citrus provisioning). Arbitrage position: can shift to routes with lower scurvy incidence (via fresh supplies at way-stations), can invest in provisioning if it improves profit. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01. Negative effective extraction — the company's solution is to invest in voyage efficiency, which also reduces crew mortality (coincidental alignment).
constraint_indexing:constraint_classification(scurvy_maritime_extraction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE METROPOLE GOVERNMENT (TANGLED ROPE) — Governments experience scurvy as a strategic constraint on imperial expansion. The biological limit on voyage duration constrains the scope of colonial acquisition and trade volume (coordination function — scurvy limits how much empire is buildable). But governments also extract from the empire: they regulate trade routes, levy tariffs, deploy naval power to secure routes, and redirect colonial wealth toward metropolitan centers. The government cannot fully exit (constrained) — abandoning long-distance trade surrenders geopolitical power to rival nations. d≈0.52, f(d)≈0.65, σ=1.1 → χ≈0.37. Symmetric position: the constraint both enables and limits imperial strategy.
constraint_indexing:constraint_classification(scurvy_maritime_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE NAVAL ESTABLISHMENT (PITON) — Naval institutions maintain the scurvy extraction system through inertia despite knowing better. By the 18th century, scurvy's cause and cure (vitamin C, citrus) were established, yet the provisioning system persisted — treating scurvy as an inevitable naval fact rather than a solvable problem. Theater ratio: captains and admirals maintained an ideology of 'hardy sailor' culture, natural hardship, and inevitable mortality, even as the biological solution (lemon juice rations) became available. The institutional ritual (press gangs, sparse provisions, acceptance of high mortality) persisted not because it was functional but because it served cost-minimization narratives and crew social control. theater_ratio=0.62 reflects moderate institutional performance covering low functional benefit. By the late 18th century, even as the solution was known, implementation lagged for decades (Britain formalized citrus rations in 1795, century after the mechanism was understood).
constraint_indexing:constraint_classification(scurvy_maritime_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, vitamin C deficiency on long ocean voyages is presented as an immutable constraint of human physiology: the human body cannot synthesize ascorbic acid and cannot store it long-term. This creates an absolute requirement for dietary vitamin C replacement. The biological constraint is real (accessibility_collapse≈0.88, resistance≈0.08). However, the structural data (ε=0.52, suppression=0.68, theater=0.38) reveals this as a false summit. The mountain is the biological fact (vitamin C requirement); the extraction is the institutional choice to withold known solutions to maximize profit. The constraint is not 'humans need vitamin C' (mountain); the constraint is 'trading companies deliberately provision ships without adequate vitamin C despite knowing the requirement' (tangled_rope → snare from crew perspective). The 'natural law' framing naturalizes what is an institutional choice.
constraint_indexing:constraint_classification(scurvy_maritime_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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

test(piton_threshold) :-
    domain_priors:theater_ratio(scurvy_maritime_extraction, TR),
    TR >= 0.70.

:- end_tests(scurvy_maritime_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The constraint combines an irreducible biological requirement (vitamin C) with a fully reversible institutional choice (withhold provisions). By 1700, the biological component was understood (scurvy as nutritional disease); the extraction component was the deliberate choice to suppress cure implementation. The 0.52 value reflects that the institutional layer is substantial (not a minor add-on to biology) but not total (the biological substrate is real). Early in the period (1500), extractiveness was lower (0.32) because provisioning was more ad-hoc and crew knowledge was less systematized. By 1750, extractiveness had increased to 0.52 as company provisioning systems became more standardized and the science was known but suppressed. Suppression (0.68): High. Crews face multiple suppression mechanisms: (1) legal — pressed/conscripted sailors have no legal recourse; (2) epistemic — crew knowledge of scurvy cures was suppressed in institutional hierarchies; (3) physical — malnutrition and disease prevent organized resistance; (4) structural — no alternative maritime employment available. Theater ratio (0.38→0.62): Moderate, increasing. Early in the period, scurvy was treated as a mysterious medical problem (low theater). By 1700-1750, naval institutions had developed elaborate narratives of 'hardy sailor' culture, natural hardship, and inevitable mortality despite knowing the cause and cure. The theater increased as the ideology became more conscious and systematized. Claimed type: Tangled Rope. The constraint has both a genuine coordination function (solving long-distance sailing problems, enabling colonial expansion) and a substantial extraction component (crew mortality from withheld provisions). Both elements are structurally necessary — it is not a snare with a thin coordination veneer.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a critical perspectival gap between the beneficiary and victim that maps onto the institutional/powerless power differential. From the company's perspective, the constraint is a coordination problem (rope) — how to balance provisioning costs with voyage efficiency. From the crew's perspective, the constraint is pure extraction (snare) — deliberate withholding of a known cure. From the government's perspective, the constraint is mixed (tangled_rope) — it enables empire but limits expansion. From the naval establishment's perspective, the constraint is degraded performance justified by ideology (piton) — scurvy is treated as inevitable despite being preventable. The false mountain perspective (analytical observer naturalizing as biological necessity) is the most dangerous, as it obscures the institutional choice. The perspectival gap is not between different measurements of the same thing, but between different structural positions within a hierarchical system that benefits from naturalizing extraction as biology.
 *
 * DIRECTIONALITY LOGIC:
 *   Ship's crew: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum directionality toward target. The crew has zero exit options and bears the full cost of the constraint. Colonial settlements: Victim + constrained → d≈0.68, f(d)≈1.05. High directionality toward target but not maximal — some options exist (trade diversification, local production) but are costly. Imperial trading companies: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Negative directionality toward target — net beneficiary. Can shift to provisioning solutions if profitable. Metropolitan governments: Mixed beneficiary and victim, organized + constrained → d≈0.52, f(d)≈0.65. Symmetric directionality — scurvy limits empire but empire extraction depends on the constraint's existence. Naval establishment: Institutional + arbitrage → d≈0.10, f(d)≈-0.06. Institutional beneficiary of the status quo (maintains control systems justified by hardship ideology). Directionality is low because arbitrage options exist (can shift to citrus provisioning) but are not exercised due to institutional inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that scurvy is not a pure biological mountain but a tangled coordination-extraction hybrid. The false summit risk is the 'naturalization trap' — treating institutional choices as biological necessity. The resolution: The biological component (vitamin C requirement) is real and approximately mountain-like (accessibility_collapse would be ~0.88, resistance ~0.08 for the pure biological claim). But the constraint story is not about biology — it is about the institutional choice to suppress solutions to biology. The claimed constraint is 'long-distance sailing under conditions of withheld provisions' (tangled_rope), not 'human vitamin C requirement' (mountain). These are different constraints with different ε values. If the story were about pure biology, ε would be ≤0.25 (mountain); the 0.52 value reflects the institutional extraction layer. The mandatrophy is resolved by keeping the biological and institutional constraints separate in the conceptual model, even though they are entangled in the historical system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cure_knowledge_lag,
    'Why did naval institutions maintain high-mortality provisioning systems decades after the vitamin C cure was scientifically established and practically available?',
    'Historical analysis of scurvy treatment protocols in naval archives; timing of citrus procurement vs. scientific publications; institutional resistance to provisioning investment',
    'If lag was epistemological (knowledge unknown): constraint is biological mountain. If lag was institutional (knowledge available but suppressed): constraint is extraction system (snare/tangled_rope) — the biological substrate masks an institutional choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cure_knowledge_lag, empirical, 'Historical lag between scurvy cure discovery and naval implementation').

omega_variable(
    provisioning_cost_vs_mortality,
    'What was the actual cost-benefit calculation: did the cargo space saved by minimal provisioning exceed the loss from crew mortality and reduced voyage efficiency?',
    'Economic reconstruction of company ledgers; calculation of profit per voyage vs. crew mortality rates; comparison of high-provision vs. low-provision voyage profitability',
    'If provisioning was economically optimal: constraint reflects real trade-offs (tangled_rope). If provisioning was not economically optimal but was enforced anyway: constraint is pure extraction (snare), motivated by power/control rather than profit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provisioning_cost_vs_mortality, empirical, 'Economic calculation of provisioning costs vs. mortality losses').

omega_variable(
    crew_exit_alternatives,
    'Did sailors have any genuine exit options beyond ship service, or was maritime labor fully trapped?',
    'Labor market analysis: wage rates, alternative employment, desertion rates and punishments, indentured vs. voluntary crew composition',
    'If exit options existed: crews constrained (d≈0.75) → tangled_rope. If no exit: crews trapped (d≈0.92) → snare. This changes the powerless perspective classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crew_exit_alternatives, empirical, 'Historical labor market options for maritime workers').

omega_variable(
    indigenous_population_scurvy,
    'Did indigenous populations in colonial settlements experience the same scurvy burden, or was dietary access (fresh provisions) distributed unequally within settlements?',
    'Historical medical records from colonial settlements; diet reconstruction; differential mortality by population group',
    'If indigenous populations had better access to fresh provisions: scurvy became an instrument of colonial control (extraction mechanism). If uniform: scurvy is a shared biological constraint (coordination problem). This changes whether scurvy is fundamentally biological or fundamentally institutional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_population_scurvy, empirical, 'Differential scurvy burden across colonial populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scurvy_maritime_extraction, 1500, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scurvy_tr_t1500, scurvy_maritime_extraction, theater_ratio, 1500, 0.25).
narrative_ontology:measurement(scurvy_tr_t1650, scurvy_maritime_extraction, theater_ratio, 1650, 0.42).
narrative_ontology:measurement(scurvy_tr_t1750, scurvy_maritime_extraction, theater_ratio, 1750, 0.62).

% Extraction over time
narrative_ontology:measurement(scurvy_be_t1500, scurvy_maritime_extraction, base_extractiveness, 1500, 0.32).
narrative_ontology:measurement(scurvy_be_t1650, scurvy_maritime_extraction, base_extractiveness, 1650, 0.48).
narrative_ontology:measurement(scurvy_be_t1750, scurvy_maritime_extraction, base_extractiveness, 1750, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scurvy_maritime_extraction, resource_allocation).
narrative_ontology:affects_constraint(scurvy_maritime_extraction, colonial_supply_chain_asymmetry).
narrative_ontology:affects_constraint(scurvy_maritime_extraction, maritime_labor_coercion).

% DUAL FORMULATION NOTE:
% Scurvy represents a constraint family decomposed into (1) the biological requirement for vitamin C (mountain, ε≈0.08) and (2) the institutional choice to withhold provisions (tangled_rope/snare, ε≈0.52). The corpus treats them as separate constraints linked via network dependency: the biological mountain is upstream and affects the institutional extraction story. Failure to decompose leads to false naturalization of institutional choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(scurvy_maritime_extraction, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
