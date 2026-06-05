% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: AI Safety Dual Priority Commitment (Both XRisk and Near-Term Harms)
 *   domain: artificial_intelligence/technology_governance/safety
 *
 * SUMMARY:
 *   The dual-priority reading of the AI safety kernel commits to addressing
 *   both existential risk (extinction-level outcomes from misaligned
 *   superintelligent systems) and near-term harms (algorithmic bias, labor
 *   displacement, surveillance, misinformation from deployed systems) as
 *   non-competing priorities requiring coordinated resources and research.
 *   This reading instantiates a constraint that coordinates across two victim
 *   populations, both foundational to AI safety: those harmed by present
 *   systems and humanity at large exposed to future superintelligent
 *   misalignment. The constraint exhibits the signature of a tangled_rope
 *   with significant theater ratio: genuine coordination benefits exist
 *   (shared safety infrastructure, spillover research, unified governance
 *   framework), but asymmetric extraction occurs through resource allocation
 *   and institutional capture (well-resourced existential-risk labs dominate
 *   safety agendas while near-term harm prevention remains underfunded). The
 *   theater ratio (0.64) reflects the gap between stated dual-priority
 *   commitment and actual implementation: many institutions adopt
 *   dual-priority language performatively while maintaining existential-risk
 *   priority in practice.
 *
 * KEY AGENTS:
 *   - Near-Term Harm Populations: Primary victim (powerless/trapped) — subjected to documented AI harms with no exit; bear costs of diverted safety research attention
 *   - Humanity (Existential Risk Frame): Primary victim (powerless/trapped at civilizational scale) — exposed to extinction-level alignment failure; cannot exit exposure
 *   - Safety-Aligned AI Labs: Primary beneficiary (organized/constrained) — benefit from dual-priority framing that legitimizes both safety research and governance; constrained by reputational demands and resources
 *   - National AI Governance Institutions: Secondary beneficiary (institutional/constrained) — coordinate safety and regulation using dual-priority framework; constrained by competing stakeholder pressures
 *   - AI Industry: Institutional actor (institutional/arbitrage) — adopts safety theater performatively while maintaining business models; uses dual-priority language without fundamental product changes
 *   - AI Safety Research Field: Primary coordinator (institutional/arbitrage) — experiences dual priority as productive coordination; benefits from field-wide legitimacy and shared infrastructure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional allocation as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.52).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.58).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety Dual Priority Commitment (Both XRisk and Near-Term Harms)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "artificial_intelligence/technology_governance/safety").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, '88eb7ec1-9b68-4f87-b863-782a77f746dc').
narrative_ontology:cs_kernel_codification('88eb7ec1-9b68-4f87-b863-782a77f746dc', distributed).
narrative_ontology:cs_authority_grounding('88eb7ec1-9b68-4f87-b863-782a77f746dc', extraction).
narrative_ontology:cs_reading_relation('88eb7ec1-9b68-4f87-b863-782a77f746dc', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('88eb7ec1-9b68-4f87-b863-782a77f746dc', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('88eb7ec1-9b68-4f87-b863-782a77f746dc', foundational, both_priorities_noncompeting).
narrative_ontology:cs_axiom_status(both_priorities_noncompeting, holdable).
narrative_ontology:cs_axiom_grounding('88eb7ec1-9b68-4f87-b863-782a77f746dc', both_priorities_noncompeting, instrumental).
narrative_ontology:cs_axiom('88eb7ec1-9b68-4f87-b863-782a77f746dc', foundational, unified_safety_infrastructure_required).
narrative_ontology:cs_axiom_status(unified_safety_infrastructure_required, holdable).
narrative_ontology:cs_axiom_grounding('88eb7ec1-9b68-4f87-b863-782a77f746dc', unified_safety_infrastructure_required, empirically_contingent).
narrative_ontology:cs_reference_frame('88eb7ec1-9b68-4f87-b863-782a77f746dc', dual_mandate_coordination).
narrative_ontology:cs_drift_state('88eb7ec1-9b68-4f87-b863-782a77f746dc', contemporary_institutional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88eb7ec1-9b68-4f87-b863-782a77f746dc', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_research_field).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, safety_aligned_labs).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harm_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, existential_risk_preventionists).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, resource_constrained_safety_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEAR-TERM HARM POPULATIONS (SNARE) — Trapped in algorithmic bias, employment displacement, surveillance harm. The dual-priority commitment dilutes resources away from documented present harms in favor of speculative future risks. No exit from the constraint; maximum extraction experienced. These populations cannot exit or negotiate; they bear costs while existential-risk research captures disproportionate attention and funding.
constraint_indexing:constraint_classification(ai_safety_commitment__dual_priority_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HUMANITY / EXISTENTIAL RISK FRAME (SNARE) — Trapped by the inevitability of superintelligent AI development if not redirected. Existential preventionists see the dual-priority reading as diluting existential-risk work with near-term distractions. If extinction-level alignment failure occurs, present-day harm mitigation becomes moot — but the dual-priority commitment splits effort. No exit from civilizational-scale exposure; maximum extraction by resource scarcity.
constraint_indexing:constraint_classification(ai_safety_commitment__dual_priority_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SAFETY-ALIGNED AI LABS (TANGLED ROPE) — Benefit from the dual-priority framing as it legitimizes both safety research and near-term governance, expanding their mandate. Constrained by reputational risk (perceived as existential-focused while ignoring present harms) and by actual resource limitations. Also benefit from coordination benefit: the dual-priority framework aligns safety-alignment research with harm-mitigation (bias audits, transparency research) creating genuine spillover. Net beneficiary with mixed experience.
constraint_indexing:constraint_classification(ai_safety_commitment__dual_priority_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIONAL AI GOVERNANCE INSTITUTIONS (TANGLED ROPE) — Coordinate on dual-priority safety (existential + near-term) to justify comprehensive regulation. Constrained by competing pressures: business lobbying (minimize regulation), different stakeholder demands (labor unions vs tech companies vs civil rights groups). Genuine coordination function exists (aligning safety research with deployment standards), but asymmetric extraction through regulatory authority. Benefits from legitimacy the dual-priority frame provides.
constraint_indexing:constraint_classification(ai_safety_commitment__dual_priority_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AI INDUSTRY SAFETY THEATER (PITON) — Tech companies adopt dual-priority safety language (addressing both XRisk and near-term harms) performatively, without fundamental product changes. The commitment is largely theatrical: internal ethics committees, safety research papers, governance conversations that preserve business models. High theater ratio (0.64) reflects the gap between commitment and implementation. Piton because the functional capacity to address either priority coherently has atrophied — the theater persists through institutional inertia.
constraint_indexing:constraint_classification(ai_safety_commitment__dual_priority_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TRANSITIONAL SAFETY GOVERNANCE FRAMEWORK (SCAFFOLD) — The dual-priority commitment is a temporary holding pattern: it coordinates safety research and harm mitigation under a shared umbrella while the field matures. The sunset emerges when either (a) near-term harm prevention is sufficiently institutionalized that it no longer requires existential-risk researchers' attention, or (b) existential-risk research produces deployment guardrails that simultaneously address near-term harms. Low effective extraction because organized actors (safety researchers, policy coalitions) see the framework as transitional with a clear exit condition.
constraint_indexing:constraint_classification(ai_safety_commitment__dual_priority_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: AI SAFETY RESEARCH FIELD / COORDINATION VIEW (ROPE) — The dual-priority commitment solves a genuine coordination problem: existential and near-term safety researchers operate in parallel silos with minimal cross-fertilization. The commitment creates shared epistemic standards, joint research agendas, and resource pooling (safety infrastructure used by both communities). Net beneficiary; experiences the constraint as productive coordination rather than extraction. Benefits from field-wide legitimacy the dual priority framing provides.
constraint_indexing:constraint_classification(ai_safety_commitment__dual_priority_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational vantage, the dual-priority commitment appears as an immutable fact of AI development: both existential risk and near-term harms are inherent to any powerful technology. Coordination on both is unavoidable. The tension between them is not contingent but structural — inherent to the impossibility of addressing one without infrastructure that affects the other. However, this perspective risks naturalizing what is actually a contingent institutional allocation: funding, researcher time, and political attention ARE scarce, and choices about relative priority are made by institutional actors, not by nature.
constraint_indexing:constraint_classification(ai_safety_commitment__dual_priority_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_safety_commitment__dual_priority_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_safety_commitment__dual_priority_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, TR),
    TR >= 0.70.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. The constraint begins at 0.35 (pure coordination period) and accumulates to 0.52 (current state) as institutional capture and resource competition intensify. The trajectory reflects increasing institutional pressure to choose between priorities despite dual-priority framing. Extractiveness is not as high as pure snare (0.66+) because genuine spillover benefits exist (interpretability research applies to both categories, shared transparency standards), and organized actors (safety coalitions) retain some agency in resource allocation. Suppression (0.58): Moderate-high. Barriers to near-term harm prevention include: (a) existential-risk framing's dominance in safety discourse, (b) funding concentration in well-resourced labs focused on long-term alignment, (c) publication incentives favoring theoretical over applied harm mitigation, (d) power asymmetry (near-term harm constituencies lack institutional representation compared to existential-risk researchers). Theater ratio (0.64): Rising from 0.48. Many safety institutions adopt dual-priority commitment performatively — publish governance papers on both, sit on dual-purpose committees — while resource allocation and actual research time follow existential-risk priorities. Tech industry adoption of 'dual-priority safety' language with minimal product changes drives the theater ratio upward. The theater reflects the gap between institutional commitment and functional implementation.
 *
 * PERSPECTIVAL GAP:
 *   The dual-priority reading produces maximal perspectival divergence across the constraint landscape. Trapped populations (near-term harmed, existentially exposed) experience snare classification with no exit. Well-resourced safety labs experience rope-like coordination with net benefit. Industry experiences piton (performative commitment with low functional change). Governance institutions experience tangled_rope (genuine coordination with asymmetric extraction via regulatory authority). The analytical observer risks mountain classification (seeing dual priority as inevitable), but structural data contradicts this — the commitment is contested, resource-driven, and contingent on institutional choices. The gap reveals the core tension: the constraint attempts to hold incompatible victim sets (present-harmed and future-exposed) with different time horizons and evidence profiles, creating sustained resource competition despite unified commitment language.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from power level, exit options, and beneficiary/victim status. Powerless trapped populations experience d ≈ 0.95 (maximum victimhood), f(d) ≈ 1.42. Organized safety labs with constrained exit but beneficiary status experience d ≈ 0.40 (mixed), f(d) ≈ 0.40. Institutional actors with arbitrage options but beneficiary status experience d ≈ 0.05-0.15 (low extraction experienced), f(d) ≈ -0.12 to -0.01. The engine derives d from the beneficiary/victim declarations and exit profiles; the directionality overrides array remains empty because the structural derivation correctly captures the relationships. The constraint produces high χ for powerless victims, low χ for institutional beneficiaries, and intermediate χ for organized actors, generating the perspectival gap signature.
 *
 * MANDATROPHY ANALYSIS:
 *   The dual-priority constraint does not resolve mandatrophy through multiple types but through exposure of the coherence problem itself. The constraint claims tangled_rope (genuine coordination + asymmetric extraction), but stress tests reveal the potential for decomposition under scarcity: if resource constraints force choice, the dual-priority reading forecloses under one of its own axioms (equal commitment to both). The mandatrophy is not 'which type is correct' but 'is this reading stable or transitional?' The rising theater ratio (0.48→0.64) and rising extractiveness (0.35→0.52) over the interval suggest the constraint is degrading toward snare (near-term abandonment in practice despite dual-priority rhetoric) or scaffold (temporary holding pattern until institutional learning produces stable allocation). The omega variables (resource allocation ratio, spillover validity, institutional capture, kernel coherence) are diagnostic of whether the dual-priority reading is a stable tangled_rope or a decomposing false coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_vs_near_term_resource_allocation,
    'Under genuine scarcity, what ratio of resources optimally addresses existential risk vs near-term harms? Is this ratio determinable or preference-dependent?',
    'Counterfactual analysis: compare outcomes under different allocation ratios; empirical tracking of which safety interventions reduce both categories of harm vs only one; historical comparison to other dual-priority commitments (e.g., pandemic prevention vs endemic disease) to identify whether a stable allocation emerges or remains contested.',
    'If ratio is determinable: the dual-priority framework is a false compromise, and one reading (existential or near-term) should dominate. If ratio is preference-dependent: the framework is correct as a normative holding pattern pending institutional learning. If no stable ratio exists: the constraint is fundamentally incoherent and will decompose under resource stress.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_vs_near_term_resource_allocation, conceptual, 'Whether optimal resource allocation between existential and near-term AI safety is determinable or preference-dependent').

omega_variable(
    alignment_research_spillover_validity,
    'Do existential-risk alignment techniques (interpretability, mechanistic understanding, formal verification) actually improve near-term harm prevention, or are they orthogonal research paths that share only surface terminology?',
    'Empirical tracing: map published alignment research to documented impact on bias mitigation, labor-displacement prevention, surveillance-harm reduction. Control for selection bias (papers might claim connection that isn''t mechanistic). Case studies of safety interventions that apply both sets of techniques vs only one.',
    'If spillover is genuine and substantial: dual-priority framework is justified (genuine coordination benefit). If spillover is minimal or mythical: the constraint is extraction dressed as coordination — existential-risk work displaces near-term work under shared labels. If spillover is asymmetric (alignment→near-term but not vice versa): hierarchy exists despite dual-priority framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_research_spillover_validity, empirical, 'Whether existential-risk alignment research produces genuine spillover benefits for near-term harm prevention').

omega_variable(
    institutional_capture_risk_of_dual_priority,
    'Does the dual-priority framework allow well-resourced existential-risk labs to capture the entire safety research agenda under the pretext of addressing both, while near-term harm prevention remains chronically underfunded?',
    'Longitudinal funding analysis: track allocation of AI safety funding across existential vs near-term subcategories over 5-10 years. Researcher time analysis: survey safety researchers on actual effort allocation vs stated dual-priority commitment. Governance participation analysis: measure presence of near-term harm experts in existential-risk research steering committees vs inverse.',
    'If capture is occurring: the constraint is a snare, not a tangled rope (extraction via institutional concentration). If allocation is genuinely dual: framework is holding. If allocation is oscillating: the constraint is unstable and will decompose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_risk_of_dual_priority, empirical, 'Whether well-resourced existential-risk labs capture the safety agenda despite dual-priority framing').

omega_variable(
    kernel_reading_coherence,
    'Is the dual-priority reading a coherent commitment, or does it attempt to hold logically incompatible stances that will diverge under stress?',
    'Stress-test analysis: model scenarios where existential-risk prevention requires suppressing near-term transparency/harm investigation (e.g., keeping AI capabilities opaque to avoid misuse), and vice versa (transparency required for near-term harm auditing risks accelerating capabilities). Identify conflicts that force prioritization choices. Compare to other contested kernels (e.g., constitutional rights readings that coexist vs contradict).',
    'If truly coherent: the constraint is a stable tangled_rope. If logically inconsistent: the constraint will fracture into sibling readings under resource pressure, foreclosing the dual-priority reading. If coherent but instrumentally unstable: the constraint is a scaffold with built-in sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coherence, conceptual, 'Whether dual-priority reading is coherent or logically incompatible with its sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aidp_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(aidp_tr_t3, ai_safety_commitment__dual_priority_reading, theater_ratio, 3, 0.58).
narrative_ontology:measurement(aidp_tr_t6, ai_safety_commitment__dual_priority_reading, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(aidp_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aidp_be_t3, ai_safety_commitment__dual_priority_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(aidp_be_t6, ai_safety_commitment__dual_priority_reading, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(aidp_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(aidp_su_t3, ai_safety_commitment__dual_priority_reading, suppression_requirement, 3, 0.54).
narrative_ontology:measurement(aidp_su_t6, ai_safety_commitment__dual_priority_reading, suppression_requirement, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__dual_priority_reading, 0.18).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_governance_regulatory_capture).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, alignment_transparency_tradeoff).

% DUAL FORMULATION NOTE:
% The dual-priority reading is one of three constraint stories decomposing the contested AI safety kernel. Each reading has its own extractiveness value: existential_risk_reading (ε ≈ 0.38, Rope—pure coordination on preventing extinction), near_term_harms_reading (ε ≈ 0.45, Tangled_rope—coordination with harm-prevention extraction), dual_priority_reading (ε ≈ 0.52, Tangled_rope—coordination with resource-allocation extraction). The network relationships trace resource competition and institutional capture between readings. The dual-priority reading occupies institutional space upstream of both siblings, defining what 'AI safety' includes, and thus shapes their operating environments without foreclosing either.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
