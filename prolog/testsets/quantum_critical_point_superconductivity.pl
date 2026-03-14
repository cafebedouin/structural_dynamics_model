% ============================================================================
% CONSTRAINT STORY: quantum_critical_point_superconductivity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_critical_point_superconductivity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: quantum_critical_point_superconductivity
 *   human_readable: Quantum Critical Point Superconductivity Mechanism and Verification
 *   domain: condensed_matter_physics/high_temperature_superconductivity
 *
 * SUMMARY:
 *   The quantum critical point (QCP) superconductivity constraint represents
 *   a structural tension between paradigmatic coherence and empirical
 *   adequacy in condensed matter physics. The QCP framework — the proposal
 *   that superconducting pairing in many materials is mediated by quantum
 *   critical fluctuations — has organized theoretical research, shaped
 *   funding priorities, and structured graduate training for over two
 *   decades. Yet accumulating evidence suggests QCP mechanisms may explain
 *   only a subset of high-temperature superconductors, or explain them
 *   alongside competing mechanisms of equivalent complexity. This creates a
 *   classic paradigm extraction scenario: institutional and career incentives
 *   concentrate on defending and extending the QCP narrative despite growing
 *   empirical ambiguity, while alternative mechanistic approaches are
 *   structurally suppressed through funding scarcity, publication bias, and
 *   conceptual barriers. The constraint exhibits all six DR types from
 *   different structural positions, revealing how a single scientific
 *   framework can simultaneously function as pure extraction (snare), mixed
 *   coordination-extraction (tangled rope), genuine coordination (rope), a
 *   degraded historical artifact (piton), a temporary scaffold being replaced
 *   by materials-first discovery, and a false natural law (mountain). The
 *   theater_ratio (0.68) reflects that extensive review articles maintain
 *   narrative coherence and plausibility despite the framework's predictive
 *   limitations — the theatrical investment in QCP has increased from 0.52 to
 *   0.68 over the interval as the gap between framework scope and empirical
 *   coverage has widened.
 *
 * KEY AGENTS:
 *   - Critical Fluctuation Narrative Proponents: Primary beneficiary (institutional/arbitrage) — senior theorists, review authors, and program officers who established QCP as dominant framework; experience paradigmatic authority and funding priority
 *   - Alternative Mechanism Researchers: Primary victim (powerless/trapped) — early-career scientists pursuing non-QCP mechanisms; face publication bias, funding scarcity, and citation suppression
 *   - Experimental Precision Groups: Secondary victim (moderate/constrained) — face resource-intensive requirements to distinguish QCP signatures and career risk of null results; also benefit from QCP organizational framework for understanding phase diagrams
 *   - Strongly-Correlated Theory Community: Identity-locked actor (moderate/identity_locked) — theorists whose professional identity is constituted through quantum criticality; structurally mobile but unable to abandon framework without identity dissolution
 *   - Materials Discovery Coalition: Organized agents (organized/constrained) — machine-learning, high-entropy, and topological programs building mechanism-agnostic discovery; see QCP as temporary organizing principle with sunset
 *   - Conventional Superconductivity Paradigm: Institutional artifact (institutional/arbitrage) — BCS/Eliashberg framework persists through inertia; maintenance mechanism is performative (piton classification)
 *   - Field Conceptual Clarity: Abstract victim (powerless/trapped) — the epistemic commons cannot organize; bears full cost of paradigmatic incoherence and mechanistic confusion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_critical_point_superconductivity, 0.52).
domain_priors:suppression_score(quantum_critical_point_superconductivity, 0.48).
domain_priors:theater_ratio(quantum_critical_point_superconductivity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_critical_point_superconductivity, extractiveness, 0.52).
narrative_ontology:constraint_metric(quantum_critical_point_superconductivity, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(quantum_critical_point_superconductivity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_critical_point_superconductivity, tangled_rope).
narrative_ontology:human_readable(quantum_critical_point_superconductivity, "Quantum Critical Point Superconductivity Mechanism and Verification").
narrative_ontology:topic_domain(quantum_critical_point_superconductivity, "condensed_matter_physics/high_temperature_superconductivity").

domain_priors:requires_active_enforcement(quantum_critical_point_superconductivity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_critical_point_superconductivity, critical_fluctuation_narrative_proponents).
narrative_ontology:constraint_beneficiary(quantum_critical_point_superconductivity, funding_concentration_beneficiaries).
narrative_ontology:constraint_victim(quantum_critical_point_superconductivity, alternative_mechanism_research).
narrative_ontology:constraint_victim(quantum_critical_point_superconductivity, experimental_precision_requirements).
narrative_ontology:constraint_victim(quantum_critical_point_superconductivity, field_conceptual_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE MECHANISM RESEARCHER (SNARE) — Trapped in a funding and publication ecosystem that treats quantum critical point mediation as the dominant explanatory framework. Cannot exit without abandoning career trajectory. Faces publication bias, funding scarcity, and citation suppression for non-QCP mechanisms. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXPERIMENTAL PRECISION GROUP (TANGLED ROPE) — Experiences genuine coordination benefit (QCP framework provides organizing principle for understanding phase diagrams and critical exponents) alongside extraction (resource-intensive experimental requirements to distinguish QCP signatures from competing mechanisms; career risk if experiments fail to confirm QCP predictions).
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: QCP NARRATIVE LEADERSHIP (ROPE) — Institutional actors (senior theorists, review authors, funding program officers) who established the QCP framework experience it primarily as coordination. The narrative provides structure for organizing decades of disparate observations. Benefits from paradigmatic authority and grant priority. Effective extraction approaches zero from their perspective — they are the beneficiary nucleus.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MATERIALS DISCOVERY COALITION (SCAFFOLD) — Organized agents (high-entropy superconductor discovery, machine-learning materials screening, topological superconductor programs) are building alternative verification pathways that bypass QCP categorization entirely. See QCP as a temporary organizing principle (sunset: 20-30 years) that will be replaced by materials-first, mechanism-agnostic discovery. Low effective extraction because coalition has agency and clear exit path.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONVENTIONAL SUPERCONDUCTIVITY PARADIGM (PITON) — The BCS/Eliashberg framework persists as a theoretical reference point despite decades of apparent inadequacy for high-temperature superconductors. Maintains institutional presence in textbooks, graduate curricula, and reference reviews. Theater-high (0.68): extensive review articles and conference talks maintain the appearance that conventional mechanisms might explain corner cases, yet the framework's predictive power has atrophied. Piton classification derives from theater gate rather than extraction severity.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: STRONGLY-CORRELATED ELECTRON THEORY (IDENTITY-LOCKED) — Theorists whose professional identity is constituted through the QCP/quantum criticality framework cannot abandon it without ceasing to be 'quantum critical point theorists.' Structurally mobile (could work on other problems) but identity-fused with the QCP narrative. Experience the constraint as both coordination (QCP provides unifying principle for their research program) and extraction (pressure to defend QCP against accumulating anomalies, career investment in a single interpretive framework). Identity-lock manifests as inability to see that alternative mechanisms might be equally or more productive — the agent's professional self IS the QCP specialist.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL NECESSITY VIEW (MOUNTAIN) — From the civilizational/universal perspective, some appeal to quantum criticality appears inevitable: complex phase diagrams DO exhibit critical scaling, fluctuations DO diverge near transitions, and universal behavior IS observed. This perspective risks seeing QCP as an immutable natural law — a structural feature of many-body physics itself. However, the base properties reveal this as a false summit: the conflation of 'quantum criticality as mathematical structure' (mountain) with 'quantum criticality as THE mechanism for high-Tc superconductivity' (contingent institutional claim) naturalizes what is actually an extractive research concentration.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: HISTORICAL NARRATIVE GUARD (PITON) — Institutional preservation of the QCP framework despite accumulating empirical challenges (cuprates do not show predicted critical scaling exponents; iron pnictides exhibit QCP but with contradictory signatures; heavy fermion systems show QCP signatures decoupled from superconductivity). The narrative persists through historical investment, citation networks, and graduate training. Theater-ratio high: extensive review articles maintain narrative coherence despite internal contradictions. Function has atrophied — the QCP framework now explains fewer unexpected phenomena than competing mechanisms — but institutional inertia preserves it.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_critical_point_superconductivity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_critical_point_superconductivity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quantum_critical_point_superconductivity, TR),
    TR >= 0.70.

:- end_tests(quantum_critical_point_superconductivity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The QCP framework concentrates funding, publication priority, and prestige on a specific mechanistic hypothesis despite significant empirical ambiguity. Researchers pursuing alternative mechanisms face publication delays, reduced funding access, and citation suppression. The extraction is not as severe as a pure snare (ε ≥ 0.66) because: (1) QCP does explain real phenomena in some materials, (2) alternative mechanism research is not impossible (merely constrained), (3) the mechanism-agnostic discovery coalition is building exit pathways. Suppression (0.48): Moderate. Significant barriers exist (funding concentration, publication bias, theoretical prestige effects, graduate training emphasis on QCP), but not total — alternative research is possible, published, and developing institutional foundations. Theater ratio (0.68): Moderately high. Review articles maintain extensive narrative coherence and plausibility despite declining predictive power — the theatrical investment serves to defend the framework against empirical challenges. Theater has increased from 0.52 to 0.68 over the interval as the gap between framework scope and coverage has widened, requiring more explanatory effort.
 *
 * PERSPECTIVAL GAP:
 *   The strongest perspectival gap appears between the QCP narrative leadership (rope/institutional/arbitrage, χ approaching negative values) and alternative mechanism researchers (snare/powerless/trapped, χ ≈ 0.80+). Both observe the same funding concentration and citation patterns, yet experience radically different constraint types. For the beneficiary, QCP coordination is genuinely useful; for the victim, the same institutional arrangement is pure extraction. The secondary gap appears within the identity-locked theory community: they classify as tangled_rope (genuine coordination + extraction) but from an identity_locked perspective that prevents recognizing the extraction as problematic. At the biographical time horizon, identity_locked produces rope (the agent perceives mutability in principle), whereas trapped would produce mountain (immutable perception). This diagnostic gap reveals that the binding mechanism is cognitive rather than purely material.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) reflects the agent's structural position: beneficiaries of the QCP paradigm (institutional with arbitrage exit) derive d from their privileged access to funding and prestige, producing low d (−0.12 to 0.02) and thus negative or near-zero χ. The analytical observer derives d ≈ 0.73 from the position of universal witness to paradigmatic extraction. Alternative mechanism researchers derive d from victim status plus trapped exit (no alternative career path in current market), producing high d ≈ 0.95 and high f(d) ≈ 1.42. Experimental precision groups derive d from mixed beneficiary/victim status (benefit from QCP organization but bear extraction costs of resource requirements) and constrained exit, producing moderate d ≈ 0.65 and moderate f(d) ≈ 1.00. The identity-locked theory community derives d from victim status (cannot exit) and identity-locked exit modulation, producing d ≈ 0.89 and f(d) ≈ 1.28. Note that identity_locked does not have its own canonical d — it derives from structural data (victim status + impossible-to-exit professional identity) rather than from a single predefined value.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint does not exhibit fatal type-conflation because all six types are legitimate perspectival readings from structurally distinct observation positions. The mandatrophy resolves when we recognize that 'is QCP the correct mechanism?' is not a yes/no question but a perspectival lattice: from the beneficiary's position (institutional/arbitrage), QCP is coordination (rope). From the victim's position (powerless/trapped), QCP is extraction (snare). From the materials-agnostic coalition (organized/constrained), QCP is temporary scaffolding. The false summit (mountain) arises when the analytical observer conflates 'quantum criticality as mathematical structure in phase diagrams' (universally true, immutable) with 'quantum criticality as THE mechanistic explanation for high-Tc superconductivity' (empirically contested, institutionally contingent). The framework resolves the mandatrophy by showing that the question is not 'which type is correct?' but 'under what conditions would an agent with position (P, T, E, S) classify this constraint as type C?' The answer that emerges is: agents positioned as beneficiaries with institutional power and arbitrage exit see rope; agents positioned as victims with low power and trapped exit see snare; agents with organized power and generation-scale horizons see scaffold; historical observers see piton. No single type is 'the answer' — the presheaf of classifications IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qcp_vs_fluctuation_decoupling,
    'Are quantum critical fluctuations mechanistically responsible for pairing, or merely correlated with superconducting regions of the phase diagram?',
    'Direct measurement of pairing mechanism via Fourier-transform scanning tunneling spectroscopy, resonant inelastic x-ray scattering, and angle-resolved photoemission; systematic correlation between critical exponents and pairing strength across material families',
    'If mechanistic: QCP framework legitimately organizes high-Tc physics. If correlative only: QCP is a descriptive taxonomy, not an explanatory mechanism — research priority should shift to materials-first discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qcp_vs_fluctuation_decoupling, empirical, 'Whether quantum criticality mechanistically drives pairing or merely correlates with superconductivity').

omega_variable(
    universal_vs_material_specific,
    'Do quantum critical exponents observed in cuprates, pnictides, and heavy fermion systems reflect a universal underlying physics or material-specific combinations of different mechanisms?',
    'High-precision critical exponent measurement across 10+ material families; theoretical prediction of material-specific vs universal scalings; correlation analysis of exponents with chemical composition and band structure',
    'If universal: QCP framework captures deep structural principle. If material-specific: each family requires distinct mechanistic understanding — QCP is a post-hoc classification, not a forward prediction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_vs_material_specific, empirical, 'Whether QCP critical exponents reflect universal or material-specific physics').

omega_variable(
    identity_lock_vs_genuine_coordination,
    'To what degree does the QCP framework provide genuine explanatory power versus serving as a professional identity anchor for theorists whose careers depend on quantum criticality?',
    'Post-identity-break analysis: track research productivity and mechanism diversity for theorists who abandon QCP focus vs those who remain committed; measure citation patterns and mechanistic explanatory breadth for QCP papers vs alternative-mechanism papers after controlling for seniority and resources',
    'If genuine coordination: QCP framework will naturally persist because it''s useful. If largely identity-lock: expect field reorganization around mechanism-agnostic discovery within 15-20 years as cohort replacement occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_genuine_coordination, empirical, 'Degree to which QCP serves as identity anchor vs genuine explanatory framework').

omega_variable(
    funding_concentration_extraction,
    'What is the magnitude of research opportunity cost imposed by funding concentration on QCP mechanisms versus mechanisms-agnostic superconductor discovery?',
    'Grant funding analysis: proportion of NSF, DOE, and international funding dedicated to QCP vs alternative mechanisms; productivity analysis of funded vs unfunded research groups; time-to-discovery metrics for alternative mechanisms with equivalent initial resources',
    'If high opportunity cost: suppression metric (0.48) is understated, and the constraint is more extractive (tangled_rope) than classified. If low: suppression is accurate, and alternative mechanisms have comparable funding access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_concentration_extraction, empirical, 'Magnitude of research opportunity cost from funding concentration on QCP mechanisms').

omega_variable(
    theater_ratio_sustainability,
    'Is the theater_ratio (0.68) stable, declining, or increasing? Is performative investment in QCP narrative increasing or decreasing?',
    'Citation analysis: track ratio of review articles to new experimental results; measure review article length and citation network density; analyze citation velocity (how quickly reviews are cited) vs mechanism-specific papers',
    'If increasing: constraint is degrading toward piton (theater approaching 1.0). If stable: current classification holds. If decreasing: QCP narrative is consolidating toward genuine explanatory function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_sustainability, empirical, 'Trajectory of theater ratio for QCP narrative investment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_critical_point_superconductivity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qcps_tr_t0, quantum_critical_point_superconductivity, theater_ratio, 0, 0.52).
narrative_ontology:measurement(qcps_tr_t7, quantum_critical_point_superconductivity, theater_ratio, 7, 0.6).
narrative_ontology:measurement(qcps_tr_t15, quantum_critical_point_superconductivity, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(qcps_be_t0, quantum_critical_point_superconductivity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(qcps_be_t7, quantum_critical_point_superconductivity, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(qcps_be_t15, quantum_critical_point_superconductivity, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_critical_point_superconductivity, resource_allocation).
narrative_ontology:boltzmann_floor_override(quantum_critical_point_superconductivity, 0.18).
narrative_ontology:affects_constraint(quantum_critical_point_superconductivity, cuprate_pseudogap_mechanism).
narrative_ontology:affects_constraint(quantum_critical_point_superconductivity, iron_pnictide_superconductivity).
narrative_ontology:affects_constraint(quantum_critical_point_superconductivity, heavy_fermion_unconventional_pairing).

% DUAL FORMULATION NOTE:
% QCP superconductivity decomposes into: (1) Quantum Criticality as Mathematical Structure in Phase Diagrams (ε=0.08, mountain, universal), (2) QCP as Pairing Mechanism in Cuprates (ε=0.48, tangled_rope, contested), (3) QCP as Pairing Mechanism in Iron Pnictides (ε=0.42, tangled_rope, lower confidence), (4) QCP as Pairing Mechanism in Heavy Fermions (ε=0.35, rope, more established). The base story (quantum_critical_point_superconductivity) aggregates these three mechanism-specific constraints at the meta-level of research program coordination. Upstream: phase diagram taxonomy provides descriptive framework. Downstream: mechanism-specific constraints inherit extraction patterns and identity-lock dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_critical_point_superconductivity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
