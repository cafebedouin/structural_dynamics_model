% ============================================================================
% CONSTRAINT STORY: continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_continuous_refresh_hybrid, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: continuous_refresh_hybrid
 *   human_readable: Continuous Refresh Hybrid: Competence Retention Through Mandatory Drill Cycles
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Competence retention in safety-critical operations (nuclear, aviation,
 *   maritime, high-consequence maintenance) faces a fundamental tension:
 *   competence validation cannot be a one-time event (skills decay, personnel
 *   turn over, scenarios evolve) yet continuous validation (repeated testing,
 *   mandatory drills) imposes significant extraction costs on operational
 *   personnel. This constraint models one specific reading of how
 *   organizations resolve this tension: competence retention is fundamentally
 *   process-dependent, not state-validated; safety records prove continuous
 *   exercise works, not that simulation equals catastrophe. This reading
 *   treats the continuous-refresh mandate as a genuine hybrid constraint — it
 *   coordinates a real safety function while extracting labor, compliance
 *   overhead, and organizational resource. An alternative reading
 *   (simulation_as_proxy) would hold that realistic simulation can substitute
 *   for continuous refresh, reducing extraction. Another alternative
 *   (real_catastrophe_only) would hold that actual incident experience is the
 *   only valid competence measure. This story instantiates the
 *   continuous-refresh-as-process reading and explicitly rejects the claim
 *   that one-time validation is sufficient.
 *
 * KEY AGENTS:
 *   - Operational Personnel (Powerless/Trapped): Primary victims — face mandatory drills with no structural exit; one failure can end certification; asymmetric burden of proof
 *   - Safety Regulator (Institutional/Arbitrage): Primary beneficiary — captures authority and credibility from visible drill programs; low extraction experienced; high beneficiary position
 *   - Maintenance Supervisor (Moderate/Constrained): Secondary actor — constrained to organize drills but benefits from safety culture; mixed coordination-extraction
 *   - Catastrophe Prevention Function (Organized/Mobile): Structured beneficiary — abstract safety collective that depends on continuous competence; coordinates with the mandate's core purpose
 *   - Resource Allocation Function (Moderate/Constrained): Secondary actor — bears opportunity cost of drill budget; benefits from reduced catastrophe liability
 *   - Analytical Observer (Analytical/Analytical): Civilizational view — risks naturalizing the continuous-refresh requirement as inherent law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuous_refresh_hybrid, 0.58).
domain_priors:suppression_score(continuous_refresh_hybrid, 0.65).
domain_priors:theater_ratio(continuous_refresh_hybrid, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuous_refresh_hybrid, extractiveness, 0.58).
narrative_ontology:constraint_metric(continuous_refresh_hybrid, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(continuous_refresh_hybrid, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(continuous_refresh_hybrid, "Continuous Refresh Hybrid: Competence Retention Through Mandatory Drill Cycles").
narrative_ontology:topic_domain(continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuous_refresh_hybrid, '846b13ff-0a25-4c60-8826-3c09a6346eac').
narrative_ontology:cs_created_at('846b13ff-0a25-4c60-8826-3c09a6346eac', '').
narrative_ontology:cs_kernel_codification('846b13ff-0a25-4c60-8826-3c09a6346eac', formalized).
narrative_ontology:cs_authority_grounding('846b13ff-0a25-4c60-8826-3c09a6346eac', practice).
narrative_ontology:cs_interpretation_layer_present('846b13ff-0a25-4c60-8826-3c09a6346eac').
narrative_ontology:cs_kernel_id(continuous_refresh_hybrid, competence_exercise_validity).
narrative_ontology:cs_reading_relation('846b13ff-0a25-4c60-8826-3c09a6346eac', simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('846b13ff-0a25-4c60-8826-3c09a6346eac', real_catastrophe_only, influences).
narrative_ontology:cs_axiom('846b13ff-0a25-4c60-8826-3c09a6346eac', foundational, competence_is_process_dependent).
narrative_ontology:cs_axiom_status(competence_is_process_dependent, holdable).
narrative_ontology:cs_axiom_grounding('846b13ff-0a25-4c60-8826-3c09a6346eac', competence_is_process_dependent, empirically_contingent).
narrative_ontology:cs_axiom('846b13ff-0a25-4c60-8826-3c09a6346eac', foundational, safety_record_validates_continuous_exercise).
narrative_ontology:cs_axiom_status(safety_record_validates_continuous_exercise, holdable).
narrative_ontology:cs_axiom_grounding('846b13ff-0a25-4c60-8826-3c09a6346eac', safety_record_validates_continuous_exercise, empirically_contingent).
narrative_ontology:cs_reference_frame('846b13ff-0a25-4c60-8826-3c09a6346eac', competence_as_continuous_skill_maintenance).
narrative_ontology:cs_drift_state('846b13ff-0a25-4c60-8826-3c09a6346eac', contemporary_automation_era, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuous_refresh_hybrid, safety_regulator).
narrative_ontology:constraint_beneficiary(continuous_refresh_hybrid, catastrophe_prevention_function).
narrative_ontology:constraint_victim(continuous_refresh_hybrid, operational_personnel).
narrative_ontology:constraint_victim(continuous_refresh_hybrid, resource_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATIONAL PERSONNEL (SNARE) — Trapped in mandatory continuous drill cycles with no structural way out. Exit is impossible; career termination follows refusal. The burden of proof for competence is perpetual and asymmetric: one drill failure can terminate certification, but passing drills provides no guarantee of permanent validation. Maximum experienced extraction with minimal benefit visibility.
constraint_indexing:constraint_classification(continuous_refresh_hybrid, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAINTENANCE SUPERVISOR (TANGLED ROPE) — Constrained by mandates to schedule and oversee drills but also benefits from reduced catastrophe risk on their watch. The constraint coordinates genuine safety function (continuous competence maintenance) while extracting labor and compliance overhead. Significant but not maximal extraction — the supervisor has some agency in scheduling and some real benefit from the safety function.
constraint_indexing:constraint_classification(continuous_refresh_hybrid, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SAFETY REGULATOR (ROPE) — Experiences the continuous-refresh mandate as pure coordination with net benefit. Demonstrating active drill programs reduces liability and enables regulatory credibility. The regulator benefits from visibility into competence metrics and can arbitrage this knowledge into enforcement authority. Low experienced extraction; high beneficiary position.
constraint_indexing:constraint_classification(continuous_refresh_hybrid, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CATASTROPHE PREVENTION FUNCTION (ROPE) — Abstract collective good (safety culture, systemic resilience, institutional memory) benefits structurally from continuous-refresh mandate. The constraint coordinates the function it exists to protect. Organized actors (safety committees, incident investigators) see this as genuine coordination with high stakes. Mobile at generational horizon — the function can exit through catastrophe (constraint fails) but has strong incentive to stay engaged.
constraint_indexing:constraint_classification(continuous_refresh_hybrid, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RESOURCE ALLOCATION (TANGLED ROPE) — Constrained to allocate training budget, facility time, and personnel hours to continuous drills. Benefits from reduced catastrophe liability and insurance costs, but extraction is real: opportunity cost of drill budget elsewhere, scheduling friction, measurement overhead. Mixed coordination-extraction at generational horizon — the function both enables and bears costs.
constraint_indexing:constraint_classification(continuous_refresh_hybrid, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, competence decay is inherent to human cognition and motor memory: skills atrophy without practice, decision-making degrades without scenario exposure, and no one-time validation survives the passage of time and turnover. This perspective sees continuous refresh as an immutable property of safety engineering itself — a law of learning and forgetting. However, the structural data (extractiveness 0.58, suppression 0.65, beneficiaries, victims) contradicts mountain classification; the engine will compute this as a false summit.
constraint_indexing:constraint_classification(continuous_refresh_hybrid, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(continuous_refresh_hybrid_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(continuous_refresh_hybrid, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(continuous_refresh_hybrid, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(continuous_refresh_hybrid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The continuous-refresh mandate extracts labor (time away from primary operations), compliance overhead (certification management, scheduling), and psychological burden (performance pressure, career vulnerability). However, extraction is not maximal (0.66+) because the mandate genuinely coordinates a safety function that operational personnel themselves benefit from — reduced catastrophe risk protects their lives and livelihoods. The extraction is asymmetric: regulator and safety function benefit more than operational personnel bear costs, but not to the snare-level (0.66+) degree. Suppression (0.65): High. Operational personnel face significant barriers to exit: legal requirement for certification, career termination for refusal, institutional mandate with no opt-out pathway. However, suppression is not absolute — personnel can exit the role entirely (career change) though at high cost. The suppression reflects both institutional barriers (certification loss) and internalized belief that competence cannot be maintained without continuous validation. Theater ratio (0.48): Moderate. The continuous-refresh mandate has a functional component (drills do preserve some competence; safety records improve with active programs) but also performative elements (some drills are checklist exercises rather than scenario-based learning; compliance metrics matter more than competence gains). The ratio reflects that the mechanism is genuinely mixed but not yet degraded to pure theater (Piton would require theater > 0.70).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. The operational personnel (powerless/trapped) see a Snare — pure extraction with no viable exit. The regulator (institutional/arbitrage) sees a Rope — pure coordination with net benefit. The maintenance supervisor (moderate/constrained) sees a Tangled Rope — genuine coordination of safety with unavoidable extraction overhead. The resource allocation function (moderate/constrained, generational) also sees Tangled Rope but at a different horizon and with different costs in focus. The catastrophe prevention function (organized/mobile, generational) sees Rope — genuine coordination of the safety collective with mobile agency at civilizational scale. The analytical observer risks seeing a Mountain — competence decay as a law of learning inherent to human nature — but the structural data reveals this as a false summit: the continuous-refresh requirement is institutional, not natural. The gap between the snare perspective (operational personnel) and the rope perspective (regulator) is diagnostic of asymmetric extraction; the gap between the snare view and the mountain view (analytical) reveals the naturalization mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness depends on their position in the extraction flow. The regulator benefits from the mandate's existence and visibility — they arbitrage the drill data into enforcement authority. The operational personnel bear the mandate's costs (time, career risk, performance pressure) with minimal structural benefit. The maintenance supervisor operates at the coordination boundary — they both execute the mandate (constrained) and benefit from its safety function (partial beneficiary). The resource allocation function bears opportunity cost. The catastrophe prevention function (abstract) benefits from competence maintenance but has no direct budget or personnel. The key asymmetry: the regulator accumulates authority and credibility without bearing operational cost; the personnel bear operational cost without accumulating authority. The supervisor, maintenance allocator, and prevention function occupy intermediate positions. This structure — benefits to high-power agents, costs to low-power agents — is the signature of Tangled Rope extraction masquerading as coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint embodies the mandatrophy problem: it cannot be classified as pure coordination (the extraction is real and asymmetric) nor as pure extraction (the coordination function is genuine — continuous competence maintenance demonstrably improves safety). The resolution is Tangled Rope: the constraint coordinates a real safety function while extracting labor and compliance overhead from operational personnel asymmetrically. The analytical observer's temptation to classify as Mountain (competence decay is natural law) is a mandatrophy failure — it naturalizes the institutional choice to make competence validation continuous rather than state-based. The suppression value (0.65) confirms that exit is severely constrained, pointing away from pure Rope (which would have low suppression). The extraction value (0.58) rules out pure Snare (which requires ≥ 0.66) but confirms the asymmetry. The Tangled Rope classification resolves the mandatrophy by holding both truths: genuine safety coordination AND asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drill_fidelity_sufficiency,
    'Do realistic drill simulations actually preserve competence for real-world catastrophic scenarios, or do they encode false confidence that fails under true stress?',
    'Post-incident analysis: comparison of competence measures in drills vs. actual emergency response; tracking of drill-trained vs. non-drill-trained personnel performance in real events; longitudinal competence decay curves with and without continuous refresh',
    'If drills preserve competence: continuous-refresh mandate is genuine coordination (Rope from regulator view). If drills create false confidence: mandate is extraction mechanism masquerading as safety (Snare becomes more salient). If drills partially preserve: hybrid model (Tangled Rope) is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(drill_fidelity_sufficiency, empirical, 'Whether drill competence transfers to real catastrophic scenarios').

omega_variable(
    optimal_refresh_frequency,
    'What drill frequency is actually necessary to prevent competence decay — is continuous refresh optimal, or does diminishing returns create pure extraction beyond some threshold?',
    'Competence decay studies with multiple refresh intervals; analysis of critical near-misses: how many were caused by skill decay vs. other factors; cost-benefit analysis of incident prevention per dollar of training investment',
    'If optimal frequency < mandate frequency: structural extraction is present and may be deliberate (Snare tendency). If optimal = mandate: mandate is efficient coordination (Rope). If optimal > mandate: mandate is insufficient (reclassifies to Scaffold — temporary insufficient measure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_refresh_frequency, empirical, 'Optimal drill frequency for competence retention').

omega_variable(
    reading_kernel_boundary,
    'Is this constraint one reading (continuous-refresh-as-process) of a contested kernel (what competence validation is), or is it a structurally distinct constraint from the simulation-as-proxy reading and real-catastrophe-only reading?',
    'Formal analysis: if the three readings share the same beneficiary/victim structure and differ only in how they interpret the evidence of competence (process vs. state vs. event), they are readings of one kernel. If they have different victim sets or different underlying structural relationships, they are separate constraints requiring separate stories.',
    'If readings of one kernel: all three stories link via cs_structure.reading_relations. If separate constraints: network decomposition instead; each has independent ε and structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_boundary, conceptual, 'Whether continuous-refresh is one kernel reading or a distinct constraint').

omega_variable(
    suppression_mechanism_institutional_vs_cognitive,
    'Does the suppression (0.65) derive from institutional barriers (career termination, certification loss) or from internalized cognitive frames (personnel internalizing the belief that they cannot be competent without continuous validation)?',
    'Exit analysis: survey of reasons personnel cite for participating in drills (institutional requirement vs. internalized belief in necessity); observation of post-mandate behavior (do personnel continue drills if mandate is removed?); identity fusion assessment (do personnel define themselves through continuous competence validation?)',
    'If institutional: suppression is structural and remains constant. If cognitive: personnel who exit the system carry suppression with them; the constraint''s effective suppression is higher than measured (internalized component persists post-exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional_vs_cognitive, empirical, 'Whether suppression is institutional or internalized').

omega_variable(
    mandatrophy_continuous_vs_sufficient,
    'Has the continuous-refresh mandate become degraded theater (Piton) — persisting through institutional inertia despite low functional verification — or does it remain a genuine Tangled Rope coordinating safety function with unavoidable extraction?',
    'Theater tracking: measurement of actual incident prevention attribution (how much of modern safety performance derives from continuous drills vs. other factors like design improvements, automation, redundancy?); organizational narrative analysis (are drills justified functionally or performatively?); comparison with alternative competence assurance models in other safety-critical domains',
    'If theater_ratio rises above 0.65: Piton reclassification likely. If theater_ratio stays below 0.50: genuine Tangled Rope confirmed. Theater_ratio at 0.48 currently suggests functional integrity, but drift trajectory matters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_continuous_vs_sufficient, empirical, 'Whether continuous-refresh mandate has degraded to theater (Piton tendency)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuous_refresh_hybrid, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(contref_tr_t0, continuous_refresh_hybrid, theater_ratio, 0, 0.35).
narrative_ontology:measurement(contref_tr_t10, continuous_refresh_hybrid, theater_ratio, 10, 0.42).
narrative_ontology:measurement(contref_tr_t20, continuous_refresh_hybrid, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(contref_be_t0, continuous_refresh_hybrid, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(contref_be_t10, continuous_refresh_hybrid, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(contref_be_t20, continuous_refresh_hybrid, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuous_refresh_hybrid, resource_allocation).
narrative_ontology:affects_constraint(continuous_refresh_hybrid, simulation_as_proxy).
narrative_ontology:affects_constraint(continuous_refresh_hybrid, real_catastrophe_only).

% DUAL FORMULATION NOTE:
% The continuous_refresh_hybrid constraint is downstream of the kernel question 'what constitutes valid competence evidence?' The three reading stories (continuous_refresh_hybrid, simulation_as_proxy, real_catastrophe_only) are siblings in the kernel decomposition, not parent-child dependencies. They should be linked via cs_structure.reading_relations (foreclosure, coexistence, influence) rather than affects_constraints (causal dependency). The network edge here indicates that this reading influences the practical viability of the sibling readings — if continuous refresh is genuinely necessary, then simulation_as_proxy is empirically falsified and real_catastrophe_only becomes the only alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
