% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope Under Interpretive Enforcement Vacuum
 *   domain: intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   GPL Section 2(b) aims to specify when software modifications trigger
 *   copyleft obligations—when a derivative work must inherit GPL terms. The
 *   clause's language ('based upon,' 'derivative work') inherits ambiguity
 *   from copyright law itself. For thirty years, absence of definitive
 *   judicial precedent has allowed two incoherent readings to coexist as
 *   legitimate interpretations: FSF-aligned projects and strong copyleft
 *   advocates read broadly (dynamic linking, plugins, aggregated code →
 *   derivative works → copyleft applies), while industry developers and
 *   narrow-scope interpreters read the same clause narrowly (only direct
 *   textual/object-code modifications → derivative works → copyleft limited).
 *   The enforcement vacuum is the structural fact: no binding precedent
 *   defeats either reading, allowing adopters to navigate the ambiguity
 *   through community alignment and risk assessment. This reading
 *   instantiates the constraint under the enforcement vacuum: uncertainty
 *   itself is the operative mechanism, creating a tangled_rope where
 *   FSF-aligned communities and strong copyleft advocates benefit from
 *   preserved interpretive authority, while clarity-seeking adopters and
 *   industry developers bear elevated compliance costs and enforcement risk.
 *
 * KEY AGENTS:
 *   - fsf_aligned_projects: Interpreters and enforcers of expansive copyleft scope; identity-locked to the strong reading; benefit from ambiguity's preservation of their authority
 *   - strong_copyleft_advocates: Ideological coalition favoring maximal freedom propagation; identity-fused with expansive interpretation; lack enforcement capacity but influence through community consensus
 *   - clarity_seeking_adopters: Face elevated transaction costs from ambiguous scope definition; constrained exit (cannot avoid GPL code without ecosystem cost); pay through legal review overhead and compliance uncertainty
 *   - industry_ecosystem_developers: Exploit narrower reading for flexibility; powerful enough to bet on low enforcement probability; pay through compliance risk and selective disclosure overhead
 *   - fsf: Institutional authority holder; lacks courtroom enforcement capacity; benefits from vacuum through preserved agenda-setting power without precedent threat
 *   - courts: Institutional power to create binding precedent; excluded by litigation settlement patterns
 *   - proprietary_software_vendors: Adversarially excluded by GPL's freedom mandate; would prefer narrower scope but have no standing in GPL governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.52).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope Under Interpretive Enforcement Vacuum").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, '7e022b9d-f8a0-45b3-af4d-0478499c58d5').
narrative_ontology:cs_kernel_codification('7e022b9d-f8a0-45b3-af4d-0478499c58d5', fixed_text).
narrative_ontology:cs_authority_grounding('7e022b9d-f8a0-45b3-af4d-0478499c58d5', lineage).
narrative_ontology:cs_interpretation_layer_present('7e022b9d-f8a0-45b3-af4d-0478499c58d5').
narrative_ontology:cs_reading_relation('7e022b9d-f8a0-45b3-af4d-0478499c58d5', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e022b9d-f8a0-45b3-af4d-0478499c58d5', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('7e022b9d-f8a0-45b3-af4d-0478499c58d5', foundational, absence_of_binding_precedent_licenses_plurality).
narrative_ontology:cs_axiom_status(absence_of_binding_precedent_licenses_plurality, holdable).
narrative_ontology:cs_axiom_grounding('7e022b9d-f8a0-45b3-af4d-0478499c58d5', absence_of_binding_precedent_licenses_plurality, deontological).
narrative_ontology:cs_axiom('7e022b9d-f8a0-45b3-af4d-0478499c58d5', foundational, enforcement_vacuum_is_structural_feature).
narrative_ontology:cs_axiom_status(enforcement_vacuum_is_structural_feature, holdable).
narrative_ontology:cs_axiom_grounding('7e022b9d-f8a0-45b3-af4d-0478499c58d5', enforcement_vacuum_is_structural_feature, conventional).
narrative_ontology:cs_reference_frame('7e022b9d-f8a0-45b3-af4d-0478499c58d5', copyleft_scope_maximalism).
narrative_ontology:cs_drift_state('7e022b9d-f8a0-45b3-af4d-0478499c58d5', contemporary_cloud_and_plugin_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7e022b9d-f8a0-45b3-af4d-0478499c58d5', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_aligned_projects).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, strong_copyleft_advocates).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, industry_ecosystem_developers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.38 reflects moderate constraint severity: clarity-seeking adopters bear real costs (legal review, compliance redesign, relationship negotiation), but the cost is below snare-level because industry developers have arbitrage options and FSF-aligned projects still provide coordination benefit. Suppression at 0.52 reflects the constraint's enforcement structure: FSF-aligned communities can sanction non-compliance, litigation risk constrains industry developers' freedom of action, and interpretive uncertainty itself suppresses clarity-seeking by raising the cost of definitive answers. Theater ratio at 0.41 reflects that some enforcement activity is performative: community education about copyleft is real, but increasingly the activity serves to maintain interpretive authority in the face of industry challenge rather than to solve the original coordination problem. Accessibility collapse at 0.48 is moderate: alternatives exist (permissive licenses, proprietary code) but GPL's network effects make them costly; the measured value reflects partial foreclosure. Resistance at 0.67 is high: industry developers, clarity-seeking adopters, and even some open-source communities resist expansive scope claims; this is not a constraint everyone has accepted. The claimed type is tangled_rope because the arrangement provides genuine coordination (shared freedom framework) AND asymmetric extraction (ambiguity → legal costs borne by clarity-seekers and risk by industry, authority preserved by FSF-aligned parties). The measurement series show extractiveness rising from 0.31 to 0.39 in the first 15 years as cloud computing and plugin architectures intensified the scope question, then stabilizing around 0.38 as adoption patterns settled and communities developed de facto scope conventions—the constraint hardened into a stable but contested equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF-aligned seat, the constraint is genuine coordination: GPL provides shared freedom guarantees and the scope question is a technical detail best left to evolving interpretation as technology changes. From the clarity-seeking and industry developer seats, the same structure is extractive: they pay compliance costs to navigate ambiguity that the agenda-setter could resolve but does not. From the court seat (excluded), the constraint is a legal gap awaiting closure. The engine computes per-seat classification from the directional data: FSF-aligned projects compute as beneficiaries (d near 0.0, low directionality), clarity-seeking adopters as high-d targets (pay without control), industry developers as moderate-d payors (bear risk but exploit arbitrage for returns). The divergence is structural, not measurement error—the same constraint genuinely operates differently from different power positions.
 *
 * DIRECTIONALITY LOGIC:
 *   FSF-aligned projects and strong copyleft advocates are declared beneficiaries: they collect the benefit of interpretive authority (ability to influence behavior without courtroom defeat) and do not bear the compliance costs clarity-seeking imposes. Clarity-seeking adopters and industry developers are declared victims/payers: they bear elevated legal review costs (clarity-seekers), enforcement risk (industry developers), and relationship negotiation overhead (both). The directionality derivation places FSF-aligned projects at d~0.1 (structured beneficiary, arbitrage exit through authority maintenance), strong copyleft advocates at d~0.15 (ideological beneficiary, identity-locked exit), clarity-seeking at d~0.7 (high targets, constrained exit through ecosystem dependence), and industry developers at d~0.55 (moderate-high targets, arbitrage exit available but risky). The FSF institutional seat is d~0.2 (agenda-setter beneficiary, arbitrage through authority preservation). These are not overridden; they follow from beneficiary/victim + exit_options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need to specify when derivative works trigger copyleft) is contested rather than resolved. Absence of binding precedent means neither the strong copyleft reading nor the narrow reading has been validated by courts. The constraint persists in a state of licensed plurality: multiple interpretations coexist as legitimate because none has been overruled. This is not mandatrophy (the founding problem being long dead and the constraint persisting as pure theater) because the problem is still live—scope ambiguity is still a real coordination challenge. However, the absence of resolution mechanisms (binding precedent, authoritative guidance, license update from FSF clarifying intent) allows the constraint to persist indefinitely without forcing the founding problem to closure. The disappearance verdict is world_rearranges: if the enforcement vacuum were filled (court ruling, FSF clarification), the constraint's substance would transform from ambiguity-navigation to either strong obligation or narrow scope. The founding_problem_status is contested: FSF-aligned parties say scope circumvention is still a threat; industry commentators say technical modularity has addressed the concern. This contested status is the structural signal that the founding problem has not been resolved, so mandatrophy is not yet present, but the absence of closure mechanisms means mandatrophy could accumulate if the problem-solving capacity stays absent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the GPL Section 2(b) copyleft scope clause a single constraint with one correct interpretation, or does it genuinely instantiate multiple structurally distinct constraints depending on how adoption context and interpretive community shape enforcement?',
    'Binding judicial precedent would resolve toward the strong_copyleft_reading or narrow_scope_reading. Persistent absence of such precedent, coupled with continued coexistence of both readings in different communities, supports the enforcement_vacuum_reading frame: the constraint''s substance varies by context.',
    'If the enforcement vacuum is the true structural fact (not a temporary gap), then this reading is accurate and the constraint is a tangled_rope where uncertainty itself is the operative mechanism. If a court eventually rules definitively, this reading collapses and one of the sibling readings becomes canonical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether GPL scope ambiguity is a constraint property or a gap awaiting resolution.').

omega_variable(
    interpretive_community_enforcement_capacity,
    'Does the enforcement vacuum persist because courts have not tested the boundaries, or because FSF-aligned communities deliberately avoid litigation that might produce unfavorable precedent?',
    'Historical analysis of GPL enforcement cases that settled: do the settlement patterns reflect litigation risk aversion by both parties, or strategic choice by FSF to preserve ambiguity? Interviews with GPL license counsel and FSF leadership on why test cases were not pursued.',
    'If avoidance is deliberate, the vacuum is intentionally maintained and benefits to FSF-aligned parties are structural (preserved interpretive authority without courtroom risk). If avoidance is mutual risk-aversion, the vacuum is accidental and neither side prefers it — this would lower the beneficiary/victim asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_community_enforcement_capacity, empirical, 'Whether the enforcement vacuum is maintained strategically or by accident.').

omega_variable(
    clarity_seeking_transaction_cost_measurement,
    'How much of the measured extractiveness (0.38) is borne by clarity-seeking adopters through elevated legal review, compliance auditing, and relationship negotiation overhead?',
    'Survey of GPL-using companies on compliance cost as a function of scope uncertainty: licensing review time, legal expense, architecture redesign cycles, community negotiation burden. Comparison to post-clarity scenarios (hypothetical or from narrow-scope jurisdictions).',
    'If clarity-seeking cost is the dominant extraction vector, removing the uncertainty would deflate the measured extractiveness significantly. If extraction is distributed across multiple payers (including risk-bearing by industry developers and FSF benefit from preserved authority), the structure persists across clarity scenarios.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clarity_seeking_transaction_cost_measurement, empirical, 'How much extractiveness is driven by clarity costs vs. other mechanisms.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.52) structural (external barriers: litigation risk, community sanction, technical lock-in) or internalized (adopters have internalized both readings as equally valid and police their own ambiguity tolerance)?',
    'Post-clarification trajectory: if a binding court ruling settles the scope question, observe whether adopters who had internalized both readings pivot to the new reading without external pressure, or whether they resist and require ongoing enforcement.',
    'If suppression is structural, removing the ambiguity removes the suppression. If internalized, adopters carry the conformity bias even after clarity, and measured suppression underestimates the true restraint on behavior.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is external barriers or internalized ambiguity tolerance.').

omega_variable(
    industry_arbitrage_sustainability,
    'How long can industry developers sustain the strategy of exploiting interpretive ambiguity (narrower reading, selective disclosure, risk-betting) before community enforcement or legal action raises the cost above the arbitrage margin?',
    'Monitoring GPL enforcement threats and litigation patterns against industry adopters using narrow interpretations; analysis of whether enforcement probability is increasing, constant, or decreasing over time.',
    'If industry arbitrage is unsustainable and enforcement probability is rising, the constraint is shifting from tangled_rope toward snare (industry developers are becoming victims with weakening exit). If arbitrage is stable, the tangled_rope structure persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_arbitrage_sustainability, empirical, 'Whether industry arbitrage strategy is durable or approaching enforcement tipping point.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(gpl__tr_t30, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(gpl__tr_t35, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(gpl__be_t30, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(gpl__be_t35, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 35, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 5, 0.47).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(gpl__su_t30, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(gpl__su_t35, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 35, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__enforcement_vacuum_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% The GPL copyleft scope clause (Section 2(b)) instantiates three structurally distinct constraints depending on whether the operative rule is the strong interpretation (expansive derivative work boundary), the narrow interpretation (textual/object-code only), or the enforcement vacuum (ambiguity itself as the constraint). Each reading has different ε, different beneficiary/victim structure, and different temporal trajectory. They form a constraint family linked by the same kernel (the GPL text) but decomposed by interpretive reading. The enforcement_vacuum_reading does not assert that either sibling reading is structurally correct; it describes the actual situation: uncertainty is the constraint. Strong copyleft and narrow scope readings each assert their interpretation is correct; the vacuum reading asserts that the absence of closure keeps both readings live.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
