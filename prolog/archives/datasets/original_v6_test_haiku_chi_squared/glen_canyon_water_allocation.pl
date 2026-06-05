% ============================================================================
% CONSTRAINT STORY: glen_canyon_water_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_glen_canyon_water_allocation, []).

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
 *   constraint_id: glen_canyon_water_allocation
 *   human_readable: Colorado River Water Allocation under the Colorado River Compact
 *   domain: political/economic/environmental
 *
 * SUMMARY:
 *   The Colorado River Compact of 1922 created a binding legal allocation
 *   framework governing 36 million acre-feet of water rights among seven U.S.
 *   states and Mexico. The constraint operates as a hybrid: it provides
 *   essential coordination function (defining priority order, preventing
 *   unilateral diversion, enabling interstate cooperation) while
 *   simultaneously extracting resources from riparian ecosystems, tribal
 *   nations, and Mexican border communities to maintain agricultural
 *   guarantees for Lower Basin states. The constraint has become increasingly
 *   extractive over its 104-year history due to climate change reducing
 *   actual basin flow to 80-85% of the compact's 16.5 MAF assumption. This
 *   mismatch forces the allocation framework to maintain agricultural
 *   guarantees through ecosystem degradation, tribal water suppression, and
 *   unsustainable reservoir drawdown. The constraint exhibits all six
 *   classification types depending on structural position: pure extraction
 *   (Snare) for ecosystems and tribes; mixed coordination-extraction (Tangled
 *   Rope) for agricultural states and federal operators; coordination (Rope)
 *   for upper basin states; theatrical maintenance (Piton) of the original
 *   compact; temporary adaptive governance (Scaffold) in recent drought
 *   agreements; and the constant temptation to naturalize as inevitable water
 *   scarcity (false Mountain). The fundamental mandatrophy is whether the
 *   allocation framework solves a coordination problem (how to fairly
 *   distribute a shared resource) or enforces an extraction scheme (who gets
 *   to use water at whose expense). The answer is both: it accomplishes
 *   genuine inter-state coordination while externalizing massive costs onto
 *   ecosystems and indigenous peoples.
 *
 * KEY AGENTS:
 *   - Lower Basin Agricultural States (California, Arizona, Nevada): Primary beneficiary (powerful/constrained) — guaranteed 7.5 MAF each in priority, capturing economic surplus from irrigation agriculture
 *   - Upper Basin States (Colorado, Wyoming, Utah, New Mexico): Secondary beneficiary (institutional/arbitrage) — allocated 8.5 MAF collectively with flexibility to develop interstate water markets
 *   - Federal Bureau of Reclamation: Institutional coordinator (institutional/constrained) — manages reservoir operations, hydropower generation, interstate coordination; constrained by mandate to maintain multiple objectives
 *   - Riparian Ecosystems and Fish Populations: Primary victim (powerless/trapped) — bear full cost of reduced instream flows, reservoir fluctuation, altered temperature regimes; no exit option
 *   - Tribal Nations (Colorado River Basin): Secondary victim (powerless/trapped) — senior water rights often subordinated through regulatory interpretation; trapped by jurisdictional boundaries and historical marginalization
 *   - Mexican Border Communities: Tertiary victim (moderate/constrained) — guaranteed 1.5 MAF but actual delivery often constrained by upstream shortage; trapped by bilateral dependence
 *   - Future Basin Sustainability: Abstract victim (analytical/trapped) — ecosystem collapse, aquifer depletion, and irreversible species loss are paths that increase ε toward 1.0
 *   - Drought Contingency Planning Coalition: Organized adaptive agent (organized/constrained) — recent frameworks (2019, 2023 agreements) attempt structured transition with sunset logic and real hydrological data
 *   - Original Compact Framework: Institutional inertia (institutional/arbitrage) — maintained through legal momentum and interstate political gridlock despite recognition of hydrological invalidity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(glen_canyon_water_allocation, 0.58).
domain_priors:suppression_score(glen_canyon_water_allocation, 0.68).
domain_priors:theater_ratio(glen_canyon_water_allocation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(glen_canyon_water_allocation, extractiveness, 0.58).
narrative_ontology:constraint_metric(glen_canyon_water_allocation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(glen_canyon_water_allocation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(glen_canyon_water_allocation, tangled_rope).
narrative_ontology:human_readable(glen_canyon_water_allocation, "Colorado River Water Allocation under the Colorado River Compact").
narrative_ontology:topic_domain(glen_canyon_water_allocation, "political/economic/environmental").

domain_priors:requires_active_enforcement(glen_canyon_water_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(glen_canyon_water_allocation, lower_basin_agricultural_interests).
narrative_ontology:constraint_beneficiary(glen_canyon_water_allocation, upper_basin_states).
narrative_ontology:constraint_beneficiary(glen_canyon_water_allocation, federal_hydropower_operators).
narrative_ontology:constraint_beneficiary(glen_canyon_water_allocation, interstate_compact_framework).
narrative_ontology:constraint_victim(glen_canyon_water_allocation, lower_basin_riparian_ecosystems).
narrative_ontology:constraint_victim(glen_canyon_water_allocation, tribal_water_rights).
narrative_ontology:constraint_victim(glen_canyon_water_allocation, mexican_border_communities).
narrative_ontology:constraint_victim(glen_canyon_water_allocation, future_basin_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RIPARIAN ECOSYSTEMS (SNARE) — Cannot exit; bears full cost of reduced instream flows and ecosystem degradation. Lake Mead and Powell reservoir operations prioritize human water extraction over environmental flows. Suppression is absolute: no alternative water source exists; ecosystem cannot advocate. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRIBAL NATIONS (SNARE) — Legally recognized senior water rights often subordinated in practice through allocation frameworks that predate or minimize tribal claims. Trapped by jurisdictional boundaries and historical marginalization. No exit option; bear costs of ecosystem collapse affecting treaty rights. d≈0.90, f(d)≈1.35, σ=0.9 → χ≈0.70.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: LOWER BASIN AGRICULTURE (TANGLED ROPE) — Primary beneficiary of allocation; receives guaranteed volume and seniority in drought conditions. But also constrained: cannot reduce agricultural demand without economic collapse; constrained exit because agriculture dominates economy and depends on permanent water guarantees. The constraint provides coordination (who gets how much, priority in shortage) AND asymmetric extraction (guarantees at expense of riparian systems and tribal nations). d≈0.48, f(d)≈0.62, σ=0.9 → χ≈0.33.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: UPPER BASIN STATES (ROPE) — Experience the allocation as coordination mechanism solving intra-regional distribution problem. Beneficiary from the compact's creation (legitimized water rights). Exit options: arbitrage through interstate water rights trading, federal projects, and prior appropriation flexibility. d≈0.25, f(d)≈0.16, σ=0.9 → χ≈0.08.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: FEDERAL HYDROPOWER OPERATORS (TANGLED ROPE) — Coordination function: manages reservoir operations, power generation, and minimizes conflict among states. Extraction function: controls instream flows, prioritizes electricity generation and municipal delivery over environmental/tribal needs. Constrained by legal mandate to coordinate without reducing power revenue. d≈0.52, f(d)≈0.70, σ=0.9 → χ≈0.37.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: CLIMATE HYDROLOGY (TANGLED ROPE) — Structural change in precipitation, snowpack, and runoff creates a mismatch between compact allocation (100% of assumed flow) and physical reality (80-85% observed). The constraint now enforces impossible coordination: the compact assumes surplus that does not exist. This creates a Tangled Rope character: the allocation framework still provides coordination function (defines priority order in shortage) but also forces extraction of environmental and tribal water to maintain agricultural guarantees. d≈0.70, f(d)≈1.08, σ=1.1 → χ≈0.63.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ADAPTIVE GOVERNANCE COALITION (SCAFFOLD) — Recent agreements (2019 Interim Guidelines, 2023 Lower Basin Agreement) create temporary frameworks for water shortage coordination with explicit sunset logic: as climate hydrological baselines shift, allocation percentages and trigger points will need renegotiation. These are structured as temporary measures with built-in review windows. Theater ratio lower than the original compact (0.55 vs 0.64) because contingency frameworks engage real hydrological data rather than legalized assumptions. d≈0.45, f(d)≈0.50, σ=0.9 → χ≈0.21.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ORIGINAL COMPACT INERTIA (PITON) — The 1922 Colorado River Compact and its legal architecture persist through institutional momentum despite 2+ decades of recognition that the hydrological assumptions are false. Theater ratio=0.64: much regulatory activity (annual allocations, interstate negotiations, legal proceedings) maintains the appearance of a functioning system while its core hydrology-allocation mismatch has been unsolved. The framework is maintained because replacement is gridlocked, not because it works. d≈0.08, f(d)≈-0.04, σ=0.9 → χ≈-0.02.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 9: HYDROLOGICAL NATURAL LAW (MOUNTAIN — FALSE SUMMIT) — Deep temptation to naturalize the constraint as 'water scarcity is inevitable and allocation is natural law.' But base properties (ε=0.58, suppression=0.68, theater=0.64) contradict mountain requirements. The physical limit (river flow) is real, but the allocation mechanism is contingent human design. This perspective represents the risk of false naturalization that reveals the structural constraint to be social/political rather than natural law.
constraint_indexing:constraint_classification(glen_canyon_water_allocation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(glen_canyon_water_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(glen_canyon_water_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(glen_canyon_water_allocation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(glen_canyon_water_allocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(glen_canyon_water_allocation, TR),
    TR >= 0.70.

:- end_tests(glen_canyon_water_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint enforces agricultural water guarantees by depleting groundwater reserves, reducing ecosystem flows, and constraining tribal access. The 0.58 value reflects that much extraction remains coordinated through legal frameworks (not pure coercion), but climate change has increased the extraction component as the constraint now forces choices between agriculture and ecosystem survival that did not exist at signing. The value has increased from 0.35 (1922) to 0.58 (2026) as the hydrological mismatch widened. Suppression (0.68): High. Multiple barriers prevent alternatives: legal lock-in through interstate compact (cannot renegotiate without unanimous agreement), structural dependence of southwestern agriculture on guaranteed water, interstate political gridlock, and international treaty obligations. Tribal and environmental advocates face highest suppression: no legal exit from subordination; political channels are captured by agricultural interests. Theater ratio (0.64): Moderate-high. Significant regulatory machinery (annual allocations, interstate negotiations, environmental assessments, legal proceedings) maintains appearance of functioning system while core problem (hydrological deficit) remains unresolved. Recent adaptive governance frameworks lower theater through explicit focus on hydrological reality and trigger-based mechanisms, but the original compact's theater remains high.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is extreme — the same allocation mechanism appears as essential coordination (upper basin states, federal operators) versus pure extraction (riparian ecosystems, tribes). Lower Basin agricultural states occupy the middle ground: they see the constraint as providing legitimate coordination (water rights certainty) and beneficent subsidies (guaranteed supplies in shortage), while victims see the same mechanism as coercive extraction. The gap increases with time horizon: immediate perspective sees working legal system; generational perspective sees ecosystem degradation and tribal marginalization; civilizational perspective risks naturalizing the constraint as inevitable water scarcity. The adaptive governance coalition introduces a new perspectival position (scaffold) that sees the original constraint as obsolete but still maintains coordination function through new frameworks. The most dangerous perspective is the 'hydrological natural law' view that mistakes political choice (allocation framework) for physical necessity (water scarcity), creating false mountain classification that prevents structural reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Lower Basin agricultural states: Beneficiary + constrained → d≈0.48, f(d)≈0.62. Significant benefits (guaranteed allocation, seniority in shortage) but constrained by dependence on permanent allocation and political lock-in. Riparian ecosystems: Victim + trapped → d≈0.92, f(d)≈1.38. No exit, no advocacy, full cost absorption. Tribal nations: Victim + trapped → d≈0.90, f(d)≈1.35. Senior water rights in law but subordinated in practice; trapped by jurisdiction and historical power imbalance. Federal operators: Beneficiary + constrained → d≈0.52, f(d)≈0.70. Benefits from coordinating role and hydropower revenue, but constrained by conflicting mandates (power vs. environment vs. irrigation). Upper basin states: Beneficiary + arbitrage → d≈0.25, f(d)≈0.16. Low extraction from their perspective; flexible water rights and interstate market opportunities. Adaptive governance coalition: Organized + constrained → d≈0.45, f(d)≈0.50. Low effective extraction; coalition has agency and explicitly recognizes need for structural change. Original compact: Institutional + arbitrage → d≈0.08, f(d)≈-0.04. Piton classification comes from theater gate (≥0.70 would apply if measured separately); institutional perspective sees its own process as degraded.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY: The constraint's classification hinges on whether one reads the allocation framework as solving a coordination problem (Rope) or enforcing extraction (Snare/Tangled Rope). The mandatrophy resolves by decomposing the temporal dimension: AT SIGNING (1922), the constraint may have functioned as genuine Rope — equitable coordination among upper and lower basin states with no foreseen ecosystem cost because basin hydrology seemed abundant. NOW (2026), with climate-driven hydrological deficit and full recognition of ecosystem/tribal costs, the constraint is structurally Snare from ecosystem/tribal perspectives and Tangled Rope from agricultural state perspectives. The claim that the allocation 'solves a coordination problem' requires the empirical premise that the allocation can sustain both human needs AND ecosystem function. Climate change falsified that premise. The mandatrophy is resolved not by choosing one type, but by recognizing the TEMPORAL TRANSITION: Rope → Tangled Rope → Snare as the hydrological surplus disappears. The current classification (Tangled Rope, ε=0.58) captures this intermediate state. Future direction (next 20-50 years) depends on whether adaptive governance frameworks can genuinely renegotiate allocation or whether political gridlock forces a pure Snare outcome (ecosystem collapse, tribal rights suppression, ε→0.80+). The false mountain temptation ('water scarcity is inevitable, allocation reflects natural limits') must be rejected: the natural limit is real, but the allocation mechanism is contingent and increasingly extractive. Current classification stands: Tangled Rope with omegas addressing whether the adaptive scaffold is real or theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_flow_assumption_fallacy,
    'Is the 1922 compact''s hydrological baseline (16.5 MAF annually) a natural law or a negotiated false premise that becomes increasingly extractive as climate shifts?',
    'Historical hydrological reconstruction (paleoclimate, instrumental record); characterization of whether basin ever truly provided 16.5 MAF at the allocation point; analysis of what percentage of extraction comes from reserves vs. current flow',
    'If baseline was always fiction: compact is Snare by design (extractive from ecosystems/tribes from origin). If baseline was valid at signing and climate shifted: constraint transitioned from Rope to Tangled Rope/Snare (classification change over time). Either way, current ε > 0.58 is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compact_flow_assumption_fallacy, empirical, 'Whether the compact baseline flow assumption is scientifically defensible').

omega_variable(
    tribal_water_rights_primacy,
    'Do tribal senior water rights (prior to compact) constitute binding constraints on lower basin agricultural allocation, or are they subordinated through political/legal interpretation?',
    'Comprehensive audit of tribal water rights claims vs. actual delivery; case law review of how federal courts have treated tribal claims relative to compact priority; water balance analysis accounting for tribal claims as senior',
    'If tribal rights are legally binding: current lower basin allocation is structurally invalid and suppression of tribal rights is explicit coercion (ε increases to 0.70+, suppression to 0.80+). If tribal rights are negotiable/secondary: suppression is less absolute but manifests through regulatory gridlock and historical marginalization (current scores remain valid).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tribal_water_rights_primacy, conceptual, 'Whether tribal water rights are legally binding constraints on allocation').

omega_variable(
    adaptive_governance_sunset_realism,
    'Do the recent drought contingency agreements (2019, 2023) represent genuine structural transition toward adaptive management, or are they theater that defers the fundamental redistribution required by climate change?',
    'Longitudinal analysis: do trigger-based reductions actually occur as hydrological conditions cross thresholds, or do political exemptions and extensions prevent them? Assessment of whether contingency frameworks reduce extractiveness (theater_ratio, suppression) over a 10-year horizon or merely delay hard choices.',
    'If genuine sunset: scaffold perspective is structural and extraction will decline as new framework matures (theaters ratio drops below 0.50, suppression drops as agreements become operational). If theater: extractiveness will increase as old and new frameworks conflict, and the constraint will classify as pure Snare in 10-15 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_governance_sunset_realism, empirical, 'Whether recent drought contingency agreements enable real adaptive transition or defer hard choices').

omega_variable(
    ecosystem_restoration_threshold,
    'Is there a minimum instream flow threshold below which riparian ecosystems cannot function, and if so, can the allocation framework sustain agriculture AND ecosystems?',
    'Ecological flow requirements for threatened species (Colorado pikeminnow, razorback sucker); hydrological modeling of basin carrying capacity under climate projections; cost-benefit analysis of agricultural loss vs. ecosystem restoration',
    'If threshold exists and is incompatible with current agriculture: constraint is structurally unsustainable and ε will trend toward 1.0 (pure extraction with ecosystem collapse). If flexibility exists: constraint might stabilize as tangled rope through trade and efficiency improvements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_restoration_threshold, empirical, 'Minimum ecological flow requirements and basin carrying capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(glen_canyon_water_allocation, 1922, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(glen_tr_t0, glen_canyon_water_allocation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(glen_tr_t50, glen_canyon_water_allocation, theater_ratio, 50, 0.56).
narrative_ontology:measurement(glen_tr_t100, glen_canyon_water_allocation, theater_ratio, 100, 0.64).

% Extraction over time
narrative_ontology:measurement(glen_be_t0, glen_canyon_water_allocation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(glen_be_t50, glen_canyon_water_allocation, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(glen_be_t100, glen_canyon_water_allocation, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(glen_canyon_water_allocation, resource_allocation).
narrative_ontology:affects_constraint(glen_canyon_water_allocation, colorado_river_tribal_water_rights).
narrative_ontology:affects_constraint(glen_canyon_water_allocation, southwest_agricultural_groundwater_depletion).
narrative_ontology:affects_constraint(glen_canyon_water_allocation, lake_mead_powell_ecosystem_collapse).
narrative_ontology:affects_constraint(glen_canyon_water_allocation, colorado_salinity_control_compact).

% DUAL FORMULATION NOTE:
% The Colorado River allocation constraint family includes separate stories for tribal water rights (legal/jurisdictional), ecosystem function (hydrological/ecological), salinity control (technical/binational), and groundwater depletion (sustainability). Each has its own ε value reflecting different structural perspectives. The allocation framework affects all downstream constraints through its hydrological prioritization. The adaptive governance agreements (2019, 2023) represent structured transition that reduces theater_ratio and potentially sunset the most extractive components, but this transition is not yet guaranteed — omegas address whether the scaffold is real or another layer of theater.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(glen_canyon_water_allocation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
