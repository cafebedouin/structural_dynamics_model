% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
    narrative_ontology:cs_story_uid/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance: Near-Term Harms Priority Reading
 *   domain: technology/governance/ethics
 *
 * SUMMARY:
 *   AI risk governance faces a fundamental framing contest over
 *   prioritization. This constraint instantiates ONE reading: that governance
 *   MUST prioritize mitigating demonstrated present harms (bias,
 *   misinformation, labor displacement, surveillance) affecting marginalized
 *   populations now. The reading claims these harms are empirically
 *   documented, morally urgent, and sufficient to justify governance even if
 *   existential superintelligence risks are unresolved. The kernel is shared
 *   with two sibling readings: the existential_risk_reading
 *   (superintelligence prevention must be primary) and the bridge_reading
 *   (both concerns are structurally entangled, not competitive). This
 *   constraint models ONLY the near-term-harms reading as a clean,
 *   ε-invariant story. The contest itself is routed to omega variables and
 *   cs_structure. The claim/metric divergence is intentional: the reading is
 *   CLAIMED as tangled_rope (genuine coordination function + asymmetric
 *   extraction) while metrics show substantial extractiveness and high
 *   suppression—the engine's computation of the gap is the measurement the
 *   corpus takes.
 *
 * KEY AGENTS:
 *   - marginalized_populations_global_south: Structural beneficiary; face documented algorithmic harms; trapped exit; powerless power → high directionality toward target
 *   - workers_displaced_by_automation: Structural beneficiary; organized constituency; constrained exit; biographical horizon → moderate directionality toward target
 *   - technology_companies: Structural payer; institutional power; arbitrage exit available; bear compliance cost → directionality toward target from company perspective, toward beneficiary from governance perspective
 *   - algorithmic_justice_advocates: Beneficiary + agenda-setter dual role; moderate power; shape what counts as 'real harm' and what gets funded → set the constraint's boundaries
 *   - superintelligence_research_community: Structurally excluded payer; opportunity cost from resource reallocation; constrained exit (research dependent on funding allocation decisions) → high directionality toward target but excluded from agenda
 *   - regulatory_authorities: Agenda-setter; institutional power; political legitimacy tied to addressing visible harms → enforce the near-term framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.71).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance: Near-Term Harms Priority Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "technology/governance/ethics").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, 'f0bcc8e3-5421-46dd-8019-3249ed92ee55').
narrative_ontology:cs_kernel_codification('f0bcc8e3-5421-46dd-8019-3249ed92ee55', formalized).
narrative_ontology:cs_authority_grounding('f0bcc8e3-5421-46dd-8019-3249ed92ee55', extraction).
narrative_ontology:cs_interpretation_layer_present('f0bcc8e3-5421-46dd-8019-3249ed92ee55').
narrative_ontology:cs_reading_relation('f0bcc8e3-5421-46dd-8019-3249ed92ee55', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0bcc8e3-5421-46dd-8019-3249ed92ee55', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('f0bcc8e3-5421-46dd-8019-3249ed92ee55', foundational, empirical_harm_demonstrability_primacy).
narrative_ontology:cs_axiom_status(empirical_harm_demonstrability_primacy, holdable).
narrative_ontology:cs_axiom_grounding('f0bcc8e3-5421-46dd-8019-3249ed92ee55', empirical_harm_demonstrability_primacy, empirically_contingent).
narrative_ontology:cs_axiom('f0bcc8e3-5421-46dd-8019-3249ed92ee55', foundational, present_moral_urgency_priority).
narrative_ontology:cs_axiom_status(present_moral_urgency_priority, holdable).
narrative_ontology:cs_axiom_grounding('f0bcc8e3-5421-46dd-8019-3249ed92ee55', present_moral_urgency_priority, deontological).
narrative_ontology:cs_reference_frame('f0bcc8e3-5421-46dd-8019-3249ed92ee55', algorithmic_harm_empirical_demonstrability).
narrative_ontology:cs_drift_state('f0bcc8e3-5421-46dd-8019-3249ed92ee55', contemporary_superintelligence_focus_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f0bcc8e3-5421-46dd-8019-3249ed92ee55', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations_global_south).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, algorithmic_justice_advocates).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, superintelligence_research_community).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is substantial because the governance structure moves resources from existential-risk research to near-term harm mitigation and imposes compliance burden on technology companies. The trajectory shows rise from 0.45 to 0.68 as governance frameworks (AI Act, FTC enforcement, corporate commitments) crystallize—early in the interval, the constraint is soft and aspirational; by t=20 it is legislative and binding. Suppression (0.71) is high because existential-risk concerns are systematically excluded from near-term-harm governance: the framing itself suppresses alternative prioritization by treating superintelligence risk as orthogonal or speculative. Theater (0.42) is moderate and stable—fairness audits and bias mitigation are real functions (not pure performance), but the constraining of existential research through funding reallocation is a secondary effect disguised by public commitment to 'safer AI.' Accessibility_collapse (0.48) is below the rope threshold because alternatives to this governance framing persist (existential-risk prioritization remains live in research institutions and some technology companies, particularly scaling-focused labs). Resistance (0.72) is high: technology companies resist compliance costs, existential-risk researchers resist resource reallocation, and some governance voices argue that near-term harm governance is performative while systemic risks grow. All measurements share one time grid: every metric authored at every time point to avoid OQ-105-style misalignment. The series tracks governance crystallization (early aspirational, late legislative) with observed data through t=12 and projected extrapolation to t=20.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute divergent types from identical structural data. Technology companies (payer seat) compute near-term-harm governance as extraction imposed on them (high suppression of their preferred allocation to long-term safety, high cost, asymmetric to company benefit). Marginalized populations (beneficiary seat) compute it as genuine coordination (they benefit from fairness constraints, alternatives are absent, coordination function is real). Regulatory authorities (agenda-setter seat) compute it as justifiable enforcement of a real harm-mitigation function against resistant payers. Existential-risk researchers (excluded payer seat) compute suppression of their concerns as the core constraint mechanism: they are not consulted on risk prioritization, their alternative framing is treated as illegitimate, and funding allocation is made without their input. The engine computes this divergence from power atoms, exit_options, and beneficiary/victim declarations; the commentary explains the structural asymmetry that generates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations carry d near 1.0 (full target): they face trapped exit (cannot avoid AI systems), powerless positioning (cannot negotiate terms), and biographical time horizon (harms accumulate immediately). Workers displaced by automation carry d around 0.75 (high target): organized power slightly improves exit (collective bargaining, union action), but displacement is concrete and biographical. Technology companies carry complex d: as payers on compliance costs, d ≈ 0.85 (high target from governance perspective); as beneficiaries of resource diversion from existential research (which might otherwise constrain their scaling), d might approach 0.3 (moderate beneficiary). Algorithmic justice advocates carry d ≈ 0.1 (near-beneficiary): they set the agenda, define what harms count, and collect legitimacy and career advancement. Superintelligence researchers carry d ≈ 0.9 (high target): they are the excluded payer whose concerns are suppressed. The schema does not support per-stakeholder d declarations, so the constraint-level d encodes the dominant asymmetry (technology company as payer carrying most extraction from governance perspective), but the commentary names the distributional complexity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (algorithmic harms are happening now and must be mitigated) is live and corroborated by independent researchers and regulators. However, a secondary mandatrophy question persists: has governance of near-term harms become a performance of address-ability while systemic resource constraints prevent meaningful mitigation? Evidence of mandatrophy creep: (1) technology companies conduct fairness audits, publish findings, and continue deployment of the same systems; (2) regulatory frameworks set standards but enforcement is resource-limited and slow; (3) funding for bias mitigation pales against overall AI R&D budgets. The constraint does not meet the classical piton definition (atrophied function, mostly theater) because the coordination function is real and the resistance is substantial—but the trajectory shows theater_ratio rising toward 0.42, and the gap between governance commitments and outcome mitigation suggests a constraint that is increasingly performing address-ability rather than achieving mitigation. The mandatrophy is not resolved, but it is flagged: if theater_ratio continues rising and accessibility_collapse remains low (alternatives to near-term framing stay live), the constraint may transition from tangled_rope toward piton (performance with declining function). The founding problem status is 'live' because the harms documented at founding are still being generated; status would shift to 'dead' only if those specific harms were systematically eliminated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    superintelligence_risk_empirical_status,
    'What is the empirical probability and timeline of artificial superintelligence scenarios that could annihilate or permanently constrain humanity''s potential, and what is the marginal prevention value of additional research resources allocated away from near-term-harms governance?',
    'Systematic review of superintelligence risk estimates across independent research groups; cost-benefit analysis of research-resource allocation comparing near-term harm prevention (known, measurable, implemented) against existential risk mitigation (speculative, enormous in impact if realized, highly dependent on research capacity). Compare governance outcomes under both prioritizations over a 10+ year horizon.',
    'If superintelligence risk is empirically high and marginally preventable by the reallocated research, the exclusion of existential-risk concerns from near-term governance becomes unjustifiable—the governance framing should shift toward bridge_reading or a negotiated resource split. If superintelligence risk is empirically low or if additional research has low marginal prevention value, near-term-harms prioritization is vindicated. If risk is real but research has high marginal value regardless of governance framing, the constraint becomes a pure extraction mechanism (near-term advocates extracting resources for coordination they don''t need).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(superintelligence_risk_empirical_status, empirical, 'The empirical basis for superintelligence risk and the marginal research value of prevention efforts').

omega_variable(
    fair_allocation_possibility,
    'Is a genuine negotiation between near-term-harms and existential-risk governance feasible that allocates resources to both without zero-sum conflict, or is the constraint inherently competitive for scarce policy attention and research funding?',
    'Test through multi-stakeholder dialogue structured around resource-sharing scenarios: can regulatory frameworks address near-term harms while preserving existential-risk research budgets? Can technology companies absorb fairness costs without cutting long-term safety R&D? Natural experiment: jurisdictions that attempt bridge_reading implementation and their outcomes.',
    'If fair allocation is feasible, the near-term-harms reading becomes unnecessarily exclusive, and governance should migrate toward bridge_reading. If resource constraints force zero-sum conflict, the constraint''s extractiveness is vindicated (genuine competition over scarce resources), and the question shifts to what the correct priority is. If fair allocation is theoretically possible but politically blocked by advocates who benefit from exclusive framing, the constraint reveals itself as extraction disguised as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_allocation_possibility, empirical, 'Whether resource allocation between near-term-harms and existential-risk governance is structurally competitive or negotiable').

omega_variable(
    near_term_harms_mitigation_efficacy,
    'Does governance prioritization of near-term algorithmic harms (fairness audits, bias mitigation, algorithmic impact assessments, regulatory frameworks) actually reduce the rate or severity of algorithmic discrimination and labor displacement in affected populations, or does it primarily perform address-ability while systems continue unchanged?',
    'Longitudinal measurement of algorithmic harm rates (facial recognition accuracy parity, credit access equity, employment algorithm outcomes, content moderation parity) in jurisdictions with near-term-harms governance vs. without. Compare harm trajectories 5–10 years before and after governance adoption.',
    'If mitigation is efficacious (harm rates decline), the constraint''s coordination function is real and extraction justified (companies fund real mitigation). If harm rates remain flat or accelerate despite governance, the constraint is piton-trajectory (performance without function, extraction without coordination benefit). This measurement directly tests the founding problem claim: ''harms are happening now and governance will address them.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_term_harms_mitigation_efficacy, empirical, 'The causal effect of near-term-harms governance on actual algorithmic harm reduction in marginalized populations').

omega_variable(
    kernel_reading_foreclosure,
    'Do the core premises of the near-term-harms reading and the existential-risk reading logically foreclose each other (one must be false for the other to be true), or do they coexist as different priorities held by different actors?',
    'Formal analysis of the logical structure of each reading''s founding claims. Can both be true simultaneously? (Yes: present harms can be real AND superintelligence risk can be real.) Can both be legitimate governance priorities? (Contested—depends on resource availability and decision-maker values.) Do they compete for the same epistemic foundations? (No: present harms are empirically documented; superintelligence risk depends on speculative models of advanced AI behavior.) The resolution is conceptual, not empirical: if the readings do not foreclose each other logically, they coexist_with rather than foreclose.',
    'If readings foreclose each other, classification of the constraint as tangled_rope is incorrect (it is a snare on one side, justified coordination on the other, with no unified framework). If readings coexist, near-term-harms governance is a legitimate but partial priority, and the constraint''s extraction (resource diversion from existential research) is a side effect of prioritization, not an intentional mechanism. Coexistence supports bridge_reading as the superior framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'The logical compatibility of the near-term-harms and existential-risk reading premises').

omega_variable(
    excluded_actor_suppression_mechanism,
    'Is the suppression of existential-risk concerns in near-term-harms governance structural (framing boundaries that actors accept as legitimate) or internalized (excluded actors believe their concerns are less legitimate or less urgent), and what would be required to dissolve the suppression?',
    'Qualitative interviews with existential-risk researchers about their perception of exclusion: do they accept the boundary (structural suppression, legitimate prioritization), or do they believe their concerns are being dismissed (internalized suppression, illegitimate exclusion)? Measure by post-exit trajectory: do researchers who leave near-term-harms governance maintain their concerns, or do they internalize the dismissal and shift priorities?',
    'If suppression is primarily internalized, the constraint carries psychological/cultural extraction (excluded parties self-silence). If structural, the constraint is a boundary-drawing mechanism (legitimate governance requires boundary-setting; exclusion is not extraction, just prioritization). If suppression is mixed, the constraint''s extractiveness includes both the resource diversion and the psychological cost of exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_actor_suppression_mechanism, empirical, 'Whether suppression of existential-risk concerns is structural (boundary-based) or internalized (identity-based)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 4, 0.61).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, algorithmic_bias_regulatory_frameworks).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, labor_displacement_ai_governance).

% DUAL FORMULATION NOTE:
% This story is the near_term_harms_reading of a three-reading kernel (ai_risk_governance_priority). The ε-invariance principle requires separate stories for logically distinct constraints. The near-term-harms reading focuses governance on demonstrated present algorithmic harms to marginalized populations; it has high ε on deployed-system harm mitigation, low ε on speculative superintelligence prevention. The existential_risk_reading inverts this: low ε on present harms (treats them as inevitable externalities of scaling), high ε on superintelligence prevention as the governing constraint. The bridge_reading treats both as structurally entangled components of one larger constraint requiring unified frameworks. These are three different ε profiles, three different victim/beneficiary structures, three different governance implications. The reading_relations field in cs_structure documents their logical relationships; omega variables carry the committer-frame contest. Do not attempt to fold all three readings into one constraint—the result would be incoherent ε, ambiguous classification, and loss of the discriminative signal the corpus measures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
