% ============================================================================
% CONSTRAINT STORY: consciousness_physics_programs
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consciousness_physics_programs, []).

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
 *   constraint_id: consciousness_physics_programs
 *   human_readable: Consciousness Physics Programs as Extractive Institutional Constraint
 *   domain: physics/consciousness_studies/institutional_funding
 *
 * SUMMARY:
 *   Consciousness physics programs (variously formulated as Penrose-Hameroff
 *   orchestrated objective reduction, Integrated Information Theory, Global
 *   Workspace Theory applied to physical substrates, and emergentist
 *   consciousness-physics coupling) create an institutional constraint where
 *   career advancement and research funding reward engagement with
 *   consciousness frameworks that lack clear empirical grounding. The
 *   constraint manifests as extraction: early-career researchers commit
 *   professional capital and identity to consciousness research in exchange
 *   for institutional positions and publication venues, but face suppressed
 *   exit options and reduced ability to critique the frameworks themselves.
 *   The theater_ratio has risen from 0.62 to 0.85 over 30 years as frameworks
 *   have accumulated without falsification or empirical specificity,
 *   suggesting institutional inertia overriding epistemic function. This is
 *   not a coordinate failure of scientists to solve consciousness — it is an
 *   extractive apparatus that leverages the genuine difficulty of the
 *   consciousness problem to sustain a system where career advancement
 *   depends on maintaining speculative frameworks with minimal possibility of
 *   falsification.
 *
 * KEY AGENTS:
 *   - Early-Career Physicists: Primary victims (powerless/trapped) — commit professional identity and career capital to consciousness frameworks; face suppressed exit options and reputational penalties for departure.
 *   - Dissident Researchers: Secondary victims (powerless/identity_locked) — structurally mobile but identity-fused through years in consciousness programs; cannot exercise exit despite theoretical ability.
 *   - Program Administrators and Consciousness Framework Gatekeepers: Primary beneficiaries (institutional/arbitrage) — control resource allocation, journal editorial boards, conference programs; benefit from career stability and institutional prestige.
 *   - Mainstream Physics Community: Secondary actor (moderate/constrained) — contaminated by consciousness physics claims; bears epistemic cost and resource allocation misalignment; also benefits from some coordination infrastructure.
 *   - Institutional Research Apparatus (Universities, Funding Agencies, Journals): Institutional actor maintaining constraint (institutional/arbitrage) — benefits from program continuation through prestige and institutional prestige narratives.
 *   - Scientific Epistemic Integrity: Victim at system level (analytical/analytical) — global epistemic commons is contaminated; speculative frameworks are presented with empirical authority; no self-correction mechanism operates at civilizational scale.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consciousness_physics_programs, 0.58).
domain_priors:suppression_score(consciousness_physics_programs, 0.68).
domain_priors:theater_ratio(consciousness_physics_programs, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consciousness_physics_programs, extractiveness, 0.58).
narrative_ontology:constraint_metric(consciousness_physics_programs, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(consciousness_physics_programs, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consciousness_physics_programs, snare).
narrative_ontology:human_readable(consciousness_physics_programs, "Consciousness Physics Programs as Extractive Institutional Constraint").
narrative_ontology:topic_domain(consciousness_physics_programs, "physics/consciousness_studies/institutional_funding").

domain_priors:requires_active_enforcement(consciousness_physics_programs).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consciousness_physics_programs, program_administrators).
narrative_ontology:constraint_beneficiary(consciousness_physics_programs, credentialed_consciousness_researchers).
narrative_ontology:constraint_beneficiary(consciousness_physics_programs, funding_gatekeepers).
narrative_ontology:constraint_victim(consciousness_physics_programs, early_career_physicists).
narrative_ontology:constraint_victim(consciousness_physics_programs, dissident_researchers).
narrative_ontology:constraint_victim(consciousness_physics_programs, scientific_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER PHYSICIST (SNARE) — Trapped in the consciousness physics program ecosystem. Career advancement requires publishing in consciousness-linked journals and citing established consciousness frameworks. Exit costs are catastrophic: departing the program marks the researcher as unreliable or non-committed. The institutional machinery extracts credibility and career capital from early researchers while offering speculative frameworks with minimal empirical grounding. Zero exit options.
constraint_indexing:constraint_classification(consciousness_physics_programs, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISSIDENT RESEARCHER (SNARE) — Structurally mobile (could publish elsewhere, change fields) but identity-locked through years of investment in consciousness physics framework. Professional identity, co-authorship networks, institutional affiliation, and career trajectory are all constituted through the consciousness program. Exit would require abandoning the identity constructed through the constraint. Experiences the constraint as unchangeable despite theoretical mobility.
constraint_indexing:constraint_classification(consciousness_physics_programs, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: PROGRAM ADMINISTRATORS (ROPE) — Experiences the constraint as pure coordination: organizing research efforts, allocating resources, mediating between research groups. Benefits from institutional position and career stability. Can arbitrage across competing consciousness frameworks. Sees the system as functional and beneficial.
constraint_indexing:constraint_classification(consciousness_physics_programs, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MAINSTREAM PHYSICS COMMUNITY (TANGLED ROPE) — Constrained by funding structures and institutional prestige allocation. Bears costs through epistemic contamination and resource allocation away from empirically grounded research. Also benefits from coordination within physics disciplines and access to consciousness program infrastructure. Extraction is real but mixed with coordination functions.
constraint_indexing:constraint_classification(consciousness_physics_programs, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL RESEARCH APPARATUS (PITON) — The consciousness physics program persists through institutional inertia despite minimal empirical validation. Research output is highly theatrical: publications citing consciousness frameworks that lack experimental grounding, conferences with limited critical scrutiny, metrics that reward citation frequency over reproducibility. The apparatus sees itself as degraded — it no longer performs its core epistemic function — yet maintains itself through career path dependence and prestige networks.
constraint_indexing:constraint_classification(consciousness_physics_programs, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SCIENTIFIC EPISTEMIC INTEGRITY (SNARE) — From a civilizational/universal perspective, consciousness physics programs extract credibility from the broader scientific epistemic commons. Speculative frameworks are presented with empirical authority despite limited grounding. This contaminates the knowledge base globally. No escape mechanism exists at the system level — the constraint persists through institutional reinforcement.
constraint_indexing:constraint_classification(consciousness_physics_programs, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consciousness_physics_programs_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consciousness_physics_programs, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consciousness_physics_programs, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consciousness_physics_programs, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consciousness_physics_programs, TR),
    TR >= 0.70.

:- end_tests(consciousness_physics_programs_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts career capital and professional time from early researchers while offering speculative frameworks with minimal empirical grounding. Unlike pure predatory extraction (e.g., debt traps), consciousness physics programs offer some genuine coordination: shared research infrastructure, publication venues, collaborative networks. However, the ratio of extraction to genuine coordination favors extraction — most publications are framework-internal rather than empirically generalizable. The value reflects this mixed but predominantly extractive character. Suppression (0.68): Moderate-high. Early-career physicists face substantial suppression: publication bias against consciousness framework criticism, career penalties for departing, social pressure to maintain commitment, limited alternative career paths if consciousness research has consumed professional identity. However, suppression is not total — some researchers do exit and rebuild careers elsewhere. The value reflects significant but not insurmountable barriers. Theater ratio (0.81): High and rising. Consciousness physics research is substantially performative: publications citing consciousness frameworks that lack divergent testable predictions, conferences with low critical scrutiny, metrics that reward citation frequency over reproducibility. The theater has increased over the measurement interval as frameworks have accumulated without empirical progress, suggesting the apparatus is increasingly performing rather than functioning. This rising theater is diagnostic of Piton degradation operating in parallel with Snare extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival fragmentation across observer positions. Program administrators see coordination (Rope) — genuine collaboration within consciousness frameworks. Mainstream physics sees mixed effects (Tangled Rope) — contamination mixed with resource access. Early-career physicists see pure extraction (Snare) — career advancement requires submission to ungrounded frameworks. Dissident researchers see inescapable extraction despite structural mobility (Snare with identity_locked exit) — the constraint is psychologically irreversible. The analytical observer sees system-level epistemic degradation (Snare at civilizational scale). The institutional research apparatus sees its own performance as degraded (Piton) — yet continues operating through inertia. This gap reveals the constraint's core mechanism: different institutional positions experience it as coordination, mixed benefit, or pure extraction depending on their power and exit options. The beneficiaries genuinely experience coordination; the powerless genuinely experience extraction. Both are correct — they are measuring different structural components of the same institution.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is derived from their structural position via the sigmoid directionality function. Program administrators benefit from consciousness frameworks (low d → negative chi, experienced as net benefit). Early-career physicists are targets of the system (high d → high chi, experienced as extraction). Dissident researchers are targets but with theoretical mobility and cognitive lock (high d from victim status + identity_locked exit = very high chi, but the internalized identity frame prevents escape). Mainstream physics community is partially targeted (moderate d → moderate chi). The analytical observer sees the system as a whole targets scientific integrity (high d from epistemic victim status → high chi). The institutional apparatus itself experiences arbitrage freedom (low d → negative chi). The directionality computation captures these structural relationships: beneficiary-with-arbitrage experiences low/negative chi; victim-with-trapped experiences high chi; victim-with-identity-locked experiences high chi with internalized frame preventing perception of exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves potential mandatrophy ambiguity (Is consciousness physics a coordination mechanism or an extractive apparatus?) by showing that BOTH are structurally true at different positions. From the beneficiary perspective (institutional administrator), it is genuine coordination — they are solving the coordination problem of organizing consciousness research. From the victim perspective (early-career physicist), it is pure extraction — they are sacrificing career capital for speculative frameworks they cannot evaluate. The mandatrophy resolves by recognizing that classification is indexical: the same structure can be Rope from one position and Snare from another. The Snare classification applies globally because it is the only one that captures the irreducible asymmetry — beneficiaries have arbitrage options (can move between consciousness frameworks or to other research areas); victims have trapped or identity_locked options (cannot exit without career cost or identity dissolution). The constraint is a Snare because the asymmetry is irreducible: no reclassification at different time horizons or exit option recalibrations produces symmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_grounding_threshold,
    'At what level of experimental specificity do consciousness physics claims transition from speculative to grounded?',
    'Direct empirical test: Can consciousness physics predictions be distinguished from null hypotheses at p<0.05 with pre-registered studies? Can competing consciousness frameworks make divergent predictions that can be tested?',
    'If threshold is attainable: constraint could be Tangled Rope (mixed coordination-extraction). If threshold is fundamentally unattainable: classification as Snare is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_grounding_threshold, empirical, 'Whether consciousness physics claims can reach empirical specificity').

omega_variable(
    framework_incommensurability,
    'Are competing consciousness physics frameworks incommensurable (speaking past each other) or do they make mutually testable predictions?',
    'Systematic analysis of framework predictions; attempt to derive divergent testable consequences from competing frameworks; assess whether frameworks are logically connected or orthogonal.',
    'If incommensurable: each framework is a closed system extracting resources without possibility of falsification. Snare classification confirmed. If commensurable: empirical arbitration is possible, suggesting potential Scaffold characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_incommensurability, empirical, 'Whether consciousness frameworks make mutually testable predictions').

omega_variable(
    career_incentive_structure,
    'How much of the extractiveness derives from career incentive misalignment versus unavoidable epistemic uncertainty?',
    'Retrospective analysis of early consciousness researchers: Which pursued consciousness physics due to genuine belief versus career positioning? Comparison of career trajectories: Do those who depart consciousness programs face measurable professional penalties?',
    'If high career misalignment: suggests possibility of policy intervention (realigning incentives could reduce extractiveness). If low alignment: extractiveness is inherent to the problem domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(career_incentive_structure, empirical, 'Attribution of extractiveness to incentive structure versus epistemic uncertainty').

omega_variable(
    identity_lock_mechanism,
    'Is the dissident researcher''s identity-lock mechanism primarily cognitive capture (internalized consciousness framework), relational (professional networks constituted through consciousness programs), or institutional (career path dependence)?',
    'Qualitative interviews with researchers who departed consciousness programs; analysis of citation networks and co-authorship patterns post-departure; assessment of career trajectory cost.',
    'If primarily cognitive: identity-locked classification is appropriate. If primarily relational/institutional: classification should shift to constrained (barriers are external, not fused with identity). Changes therapeutic intervention theory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Nature of identity-lock mechanism in dissident researchers').

omega_variable(
    theater_ratio_measurement_basis,
    'Does the high theater_ratio (0.81) reflect genuine irreducibility of consciousness physics claims (inherent mystery requiring speculative frameworks) or institutional performance (careers and funding depend on the appearance of progress)?',
    'Comparative analysis of theater_ratio across consciousness physics versus established physics research programs; assessment of metrics used (citations, publications, conference attendance versus reproducibility, experimental validation, falsification attempts).',
    'If inherent: theater is unavoidable given current understanding (Tangled Rope plausible). If institutional: theater is maintenance mechanism for extractive apparatus (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_measurement_basis, empirical, 'Attribution of high theater ratio to epistemic versus institutional causes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consciousness_physics_programs, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(consphys_tr_t0, consciousness_physics_programs, theater_ratio, 0, 0.62).
narrative_ontology:measurement(consphys_tr_t10, consciousness_physics_programs, theater_ratio, 10, 0.71).
narrative_ontology:measurement(consphys_tr_t20, consciousness_physics_programs, theater_ratio, 20, 0.81).
narrative_ontology:measurement(consphys_tr_t30, consciousness_physics_programs, theater_ratio, 30, 0.85).

% Extraction over time
narrative_ontology:measurement(consphys_be_t0, consciousness_physics_programs, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(consphys_be_t10, consciousness_physics_programs, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(consphys_be_t20, consciousness_physics_programs, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(consphys_be_t30, consciousness_physics_programs, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consciousness_physics_programs, resource_allocation).
narrative_ontology:affects_constraint(consciousness_physics_programs, quantum_consciousness_measurement_problem).
narrative_ontology:affects_constraint(consciousness_physics_programs, integrated_information_theory_empirical_grounding).
narrative_ontology:affects_constraint(consciousness_physics_programs, penrose_hameroff_orchestrated_objective_reduction).

% DUAL FORMULATION NOTE:
% Consciousness physics programs form a constraint family. The upstream constraints (specific consciousness frameworks: OR, IIT, GWT) have their own empirical status and extractiveness values. The consciousness_physics_programs constraint captures the institutional apparatus that sustains these frameworks regardless of their empirical status. Decomposition reflects ε-invariance principle: the empirical claim (does consciousness collapse wavefunction?) has different ε than the institutional claim (does the research apparatus extract value despite weak empirical grounding?).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consciousness_physics_programs, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
