% ============================================================================
% CONSTRAINT STORY: orbital_data_center_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orbital_data_center_2026, []).

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
 *   constraint_id: orbital_data_center_2026
 *   human_readable: SpaceX Million-Satellite Orbital Compute Network
 *   domain: technological/geopolitical
 *
 * SUMMARY:
 *   The proposed SpaceX million-satellite orbital compute network represents
 *   a structural entanglement of genuine technical coordination (solving
 *   latency constraints for global computing) with asymmetric extraction
 *   (locking in US military-technological advantage, degrading orbital
 *   commons sustainability, eliminating radio astronomy observation windows).
 *   The constraint exhibits Snare characteristics from the perspective of the
 *   global orbital commons, radio astronomy community, and non-US satellite
 *   operators — these agents face trapped exit, suppression through
 *   regulatory capture, and forced exposure to catastrophic risk (Kessler
 *   cascade). Simultaneously, from SpaceX and US military perspectives, the
 *   deployment is pure Rope-like coordination: solving legitimate problems of
 *   global compute access and military intelligence latency. The constraint's
 *   mandatrophy is resolved by recognizing that the perspectival gap is
 *   genuine and structural: different agents occupy fundamentally different
 *   positions relative to orbital scarcity, regulatory authority, and
 *   technological lock-in. The international space governance framework (ITU,
 *   COPUOS, OST) exhibits Piton characteristics — a performative regulatory
 *   apparatus that generates years of environmental review but has proven
 *   ineffective at constraining unilateral deployment by well-capitalized
 *   actors.
 *
 * KEY AGENTS:
 *   - SpaceX Corporate: Primary beneficiary (institutional/arbitrage) — captures first-mover advantage in orbital compute slots, manufacturing scale, and latency arbitrage
 *   - US Military & Intelligence: Primary beneficiary (institutional/arbitrage) — gains unilateral access to global surveillance and signal intelligence infrastructure; strategic technological lock-in
 *   - Radio Astronomy Community: Primary victim (powerless/trapped) — loses critical observation windows (1.4 GHz hydrogen line, 5 GHz masers) to orbital RF contamination; no exit option
 *   - Global Orbital Commons: Primary victim (powerless/trapped) — fragmentation by unilateral mega-constellation increases Kessler cascade risk; no reversibility mechanism
 *   - Non-US Satellite Operators: Secondary victim (moderate/constrained) — compete against cost-optimized SpaceX infrastructure; constrained exit through regulatory and economic barriers
 *   - International Space Governance (ITU/COPUOS/OST): Institutional actor (institutional/arbitrage) — maintains coordination framework but lacks enforcement mechanism for unilateral deployment by signatories
 *   - Regulatory Oversight (FCC/NOAA): Institutional actor (institutional/arbitrage) — grants licenses and coordinates with ITU but has low functional constraint on post-approval deployment decisions
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks false summit classification (naturalizing contingent geopolitical advantage as inevitable orbital scarcity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orbital_data_center_2026, 0.58).
domain_priors:suppression_score(orbital_data_center_2026, 0.68).
domain_priors:theater_ratio(orbital_data_center_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orbital_data_center_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(orbital_data_center_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(orbital_data_center_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orbital_data_center_2026, snare).
narrative_ontology:human_readable(orbital_data_center_2026, "SpaceX Million-Satellite Orbital Compute Network").
narrative_ontology:topic_domain(orbital_data_center_2026, "technological/geopolitical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orbital_data_center_2026, spacex_corporate).
narrative_ontology:constraint_beneficiary(orbital_data_center_2026, us_military_intelligence).
narrative_ontology:constraint_victim(orbital_data_center_2026, global_orbital_commons).
narrative_ontology:constraint_victim(orbital_data_center_2026, radio_astronomy_community).
narrative_ontology:constraint_victim(orbital_data_center_2026, non_us_satellite_operators).
narrative_ontology:constraint_victim(orbital_data_center_2026, space_debris_mitigation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RADIO ASTRONOMY COMMUNITY (SNARE) — Cannot exit the orbital environment; trapped by the irreversible nature of orbital pollution. A million satellites with optical and RF emissions eliminate observation windows for critical radio frequency bands (1.4 GHz hydrogen line, 5 GHz masers, Ka-band spectroscopy). No technical alternative exists at comparable cost or capability. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(orbital_data_center_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORBITAL COMMONS & SPACE SUSTAINABILITY (SNARE) — The global orbital environment is a commons with no central authority; once fragmented by unilateral million-satellite deployment, no exit mechanism exists. Kessler cascade risk rises exponentially; mitigation capacity is globally insufficient. Trapped in shared vulnerability. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(orbital_data_center_2026, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-US SATELLITE OPERATORS (SNARE) — Competing satellite operators (European Space Agency, Intelsat, Chinese systems) face constrained exit: they cannot avoid shared orbital space, cannot block deployment, cannot match SpaceX's cost economics, cannot access equivalent orbital compute infrastructure. Exit options limited to legal appeal (ineffective) or costly redundancy deployment (economically constrained). d≈0.80, f(d)≈1.18, σ=1.2 → χ≈0.81.
constraint_indexing:constraint_classification(orbital_data_center_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: SPACEX CORPORATE (ROPE) — From SpaceX's view, the constraint is pure coordination: deploying orbital compute solves the legitimate problem of latency-sensitive applications (financial trading, AI inference, quantum key distribution). The satellite network coordinates global access to compute infrastructure. Deployment maximizes SpaceX's arbitrage advantage (first-mover access to orbital slots, manufacturing scale). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; negative chi means coordination yields surplus value.
constraint_indexing:constraint_classification(orbital_data_center_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: US MILITARY & INTELLIGENCE (ROPE) — For DoD and IC, the constraint is coordination of global surveillance and signal intelligence infrastructure. A US-controlled orbital compute layer provides unilateral access to exabyte-scale data processing with minimal latency and no foreign intermediaries. Strategic arbitrage: lock in US military-technological advantage. Exit option (arbitrage) exists because hostile actors cannot cost-match or block the deployment. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(orbital_data_center_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL SPACE GOVERNANCE (TANGLED ROPE) — ITU, COPUOS, ITAR frameworks provide coordination function (orbital slot allocation, frequency coordination) AND serve as extraction mechanism (rich nations enforce standards that constrain poor nations' launch capacity). This perspective sees both coordination benefit (orbital access norms) and asymmetric extraction (enforcement against non-compliant operators). Constrained exit: no nation can ignore orbital governance without risking sanctions. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(orbital_data_center_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY OVERSIGHT (PITON) — FCC licensing, ITU coordination, OST treaty enforcement are substantially performative. FCC grants SpaceX licenses for Starlink expansion; ITU coordination is largely a rubber-stamp (frequency band allocation happens de facto post-deployment). Theater ratio (0.55) reflects that regulatory review takes years, generates environmental impact statements, but enforcement is weak once deployment begins. Regulatory mechanism persists through institutional inertia despite low functional constraint on actual deployment decisions. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(orbital_data_center_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN CLAIM) — Risk of naturalizing the constraint: 'orbital space is finite and must be rationally allocated by whoever acts first' (makes first-mover advantage seem inevitable/natural). The analytical observer might misclassify this as a mountain if focusing on orbital scarcity. However, ε=0.58, suppression=0.68 reveal this is not an immutable natural law but a contingent geopolitical outcome. The scarcity is real; the extraction mechanism (US unilateral deployment) is contingent. This perspective exposes the false summit trap.
constraint_indexing:constraint_classification(orbital_data_center_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orbital_data_center_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(orbital_data_center_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(orbital_data_center_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(orbital_data_center_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(orbital_data_center_2026, TR),
    TR >= 0.70.

:- end_tests(orbital_data_center_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. SpaceX's deployment extracts value across multiple dimensions: (1) latency arbitrage (0-10ms advantage for high-frequency trading, AI inference); (2) first-mover orbital slot allocation (one million satellites occupying prime LEO bands); (3) regulatory capture (FCC licensing process conducted in parallel with technical deployment). However, extractiveness is not maximal (≤0.70) because genuine coordination benefits exist: the network does solve real latency constraints, does enable low-cost global compute access, and does address legitimate military communication requirements. The extraction is asymmetric (some benefit, some bear costs) rather than purely parasitic. Suppression (0.68): High. Multiple mechanisms suppress alternatives: (1) technological: launch cost barriers ($50-100 per kg for competitors vs $1.5-2 per kg for SpaceX's Starship); (2) regulatory: orbital slot allocation favors first-mover (ITU framework operates first-come-first-served); (3) geopolitical: US export controls on satellite technology constrain non-US alternatives; (4) epistemic: opacity around military applications prevents critical assessment. Theater ratio (0.55): Moderate. Regulatory review processes (FCC environmental impact statements, ITU coordination meetings, NORAD conjunction assessments) consume significant resources and time but generate minimal actual constraint on deployment. The performative element emerges because regulatory decisions are post-facto ratification of deployment trajectories rather than gates that block deployment. However, theater is not extreme (0.55 vs 0.70+) because some regulatory mechanisms (FCC licensing, ITU frequency coordination) do impose real, if modest, constraints.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and structural, not resolvable by better information. From SpaceX/US military perspective: Rope (legitimate coordination solving latency/surveillance problems; beneficiaries perceive minimal suppression because they benefit from regulatory capture). From radio astronomy/orbital commons perspective: Snare (trapped exit, no alternative, catastrophic risk, unilateral imposition; victims perceive maximum suppression). From international space governance perspective: Tangled Rope (coordination function exists — orbital slot allocation — but enforcement mechanism is asymmetric; benefits rich nations with launch capacity, constrains poor nations). From regulatory perspective: Piton (theater-heavy, lacking functional enforcement; persists through institutional inertia, not effectiveness). The constraint does not appear as the same type to different observers because their structural positions relative to extraction and exit are genuinely different.
 *
 * DIRECTIONALITY LOGIC:
 *   SpaceX: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary position; first-mover advantage, no exit costs. US Military/IC: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Strategic beneficiary; unilateral access to compute/surveillance infrastructure; exit option exists (can deploy own system if needed, but prefers SpaceX's cost advantage). Radio astronomy: Victim + trapped → d≈0.93, f(d)≈1.40. Irreversible loss of observation bands; no technical alternative; cannot exit orbital environment. Orbital commons: Victim + trapped → d≈0.95, f(d)≈1.42. Abstract commons bears cascade risk; no central authority can exit or enforce constraints. Non-US operators: Victim + constrained → d≈0.80, f(d)≈1.18. Can theoretically launch competing constellation but face cost barriers ($billions) and regulatory delays (years); practically constrained. International governance: Mixed institutional → d≈0.55, f(d)≈0.75. Coordination function (slot allocation) + asymmetric enforcement against weaker signatories = tangled rope; constrained exit because withdrawal from OST has geopolitical costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by explicit perspectival decomposition. The same deployment structure (one million satellites) produces six different classifications because the observer's position relative to extraction and exit is fundamentally different. SpaceX sees coordination; the orbital commons sees extraction. Both are correct from their positions. The mandatrophy resolution is NOT to find 'the true type' but to recognize that the presheaf of perspectives encodes structural information about who benefits, who bears costs, and whether exit options exist. The false summit (Mountain classification from analytical observer naturalizing orbital scarcity) is exposed by observing that ε=0.58, suppression=0.68 violate the immutability thresholds. The constraint is contingent (depends on SpaceX's manufacturing scale, US military support, regulatory capture, other nations' choices), not inevitable. Mandatrophy is RESOLVED by showing that the six-type perspectival gap is the actual structure of the constraint, not a measurement ambiguity to be eliminated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orbital_debris_cascade_threshold,
    'At what satellite density does Kessler cascade risk shift from manageable to catastrophic/irreversible?',
    'Statistical collision models; historical debris generation rates; comparison with current LEO congestion thresholds (MASTER model, NASA ORDEM)',
    'If threshold is below one million satellites: deployment violates sustainability gates and should trigger international injunction. If above: deployment proceeds but increases catastrophic risk for all operators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orbital_debris_cascade_threshold, empirical, 'Orbital debris cascade threshold density').

omega_variable(
    radio_frequency_interference_remediation,
    'Can orbital compute satellites be technically modified to eliminate RFI and optical contamination of ground radio telescopes without incurring prohibitive cost or capability loss?',
    'Engineering feasibility study; cost-benefit analysis of phased-array shielding, frequency-hopping avoidance, optical coating; comparison with total system cost',
    'If remediation is feasible and low-cost: constraint shifts toward Rope (coordination) — the network becomes compatible with astronomy. If infeasible or expensive: constraint remains Snare — extraction from astronomy community is built-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(radio_frequency_interference_remediation, empirical, 'Technical feasibility of RFI mitigation').

omega_variable(
    us_military_dependency_lock_in,
    'Does a SpaceX-controlled orbital compute layer create de facto dependency for global communications infrastructure on US corporate/military control?',
    'Market analysis of compute pricing; competitive entry barriers (launch cost, orbital slot availability, regulatory approval); correlation between SpaceX deployment and changes in non-US satellite operator profitability',
    'If yes: constraint is a geopolitical snare (non-US operators trapped in dependency). If no: constraint is rope-like (multiple parallel systems can coexist). Determines whether non-US victim classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_military_dependency_lock_in, empirical, 'Degree of geopolitical dependency lock-in').

omega_variable(
    international_treaty_enforcement,
    'Can OST Article IX (prevention of harmful interference) be enforced against unilateral mega-constellation deployment if domestic regulatory approval is granted?',
    'Legal analysis of OST enforcement mechanisms; historical precedent (prior mega-constellation deployments, ASAT testing); willingness of other signatories to escalate sanctions',
    'If enforceable: constraint is mitigated by international law (Tangled Rope remains). If not enforceable: constraint is pure Snare for non-compliant operators; OST framework is Piton (performative).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_treaty_enforcement, conceptual, 'Enforceability of international orbital treaties').

omega_variable(
    latency_arbitrage_necessity,
    'Is sub-10ms global latency truly necessary for the claimed applications (financial trading, AI inference, QKD), or is this extracted value based on marginal performance advantages and competitive differentiation?',
    'Market analysis of willingness-to-pay for latency reduction; benchmark comparison (fiber-optic cables vs orbital compute); application-specific feasibility studies',
    'If truly necessary: deployment is coordination (solving real technical problem). If marginal/competitive: deployment is pure extraction (SpaceX captures rents from latency advantage). Affects whether Rope vs Snare classification is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latency_arbitrage_necessity, preference, 'Necessity of orbital sub-10ms latency for claimed applications').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orbital_data_center_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orbitdc_tr_t0, orbital_data_center_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(orbitdc_tr_t5, orbital_data_center_2026, theater_ratio, 5, 0.47).
narrative_ontology:measurement(orbitdc_tr_t10, orbital_data_center_2026, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(orbitdc_be_t0, orbital_data_center_2026, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(orbitdc_be_t5, orbital_data_center_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(orbitdc_be_t10, orbital_data_center_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orbital_data_center_2026, global_infrastructure).
narrative_ontology:affects_constraint(orbital_data_center_2026, radio_spectrum_allocation_framework).
narrative_ontology:affects_constraint(orbital_data_center_2026, international_orbital_treaty_enforcement).
narrative_ontology:affects_constraint(orbital_data_center_2026, satellite_launch_cost_competition).

% DUAL FORMULATION NOTE:
% The orbital compute network decomposes into two structurally distinct constraints: (1) Latency-constrained compute architecture (ε≈0.32, technical coordination problem, Rope-like at global scope) and (2) Geopolitical lock-in through unilateral deployment (ε≈0.58, extraction through regulatory/technological asymmetry, Snare-like). The joint constraint exhibits tangled characteristics because deployment simultaneously solves the technical problem and extracts geopolitical advantage. Upstream constraints: radio_spectrum_allocation_framework (ITU coordination rules that permit mega-constellations), satellite_launch_cost_competition (SpaceX's cost advantage enables deployment scale). Downstream constraints: international_orbital_treaty_enforcement (OST enforcement weakened by deployment precedent), space_debris_mitigation_capacity (Kessler cascade risk accumulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
