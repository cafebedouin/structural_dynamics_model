% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Compulsory Medical Intervention Mandate â Bodily Autonomy Primary Reading
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the mandate_legitimacy_scope kernel. The constraint is the coercive
 *   public health mandate regime that compels medical intervention without
 *   individualized informed consent, operationalized through state emergency
 *   powers or statutory police power. From this reading, the
 *   unvaccinated-coerced are unequivocal victims, the state is a rights
 *   violator, and the extraction of bodily self-determination is high
 *   regardless of any genuine public health coordination function. The
 *   constraint is claimed as tangled_rope because the public health benefit
 *   (herd immunity) is structurally real, but the same mechanism
 *   simultaneously enforces a unilateral transfer of bodily control. The
 *   claim/metric independence is maintained: the metrics describe an actively
 *   enforced, highly extractive arrangement, while the claimed type reflects
 *   the author's assessment that genuine coordination coexists with the
 *   extraction.
 *
 * KEY AGENTS:
 *   - state_public_health_authority: Primary agenda_setter (institutional/arbitrage) â sets and enforces the mandate
 *   - coerced_unvaccinated: Primary target (powerless/trapped) â bears the extraction of bodily autonomy
 *   - vulnerable_populations: Primary beneficiary of coordination (powerless/constrained) â receives herd immunity protection
 *   - civil_liberties_observers: Analytical observer (organized/analytical) â monitors rights violations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.85).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.8).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Compulsory Medical Intervention Mandate â Bodily Autonomy Primary Reading").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, '521231b3-9ed5-41cb-bd68-c23861db5d7b').
narrative_ontology:cs_kernel_codification('521231b3-9ed5-41cb-bd68-c23861db5d7b', formalized).
narrative_ontology:cs_authority_grounding('521231b3-9ed5-41cb-bd68-c23861db5d7b', lineage).
narrative_ontology:cs_interpretation_layer_present('521231b3-9ed5-41cb-bd68-c23861db5d7b').
narrative_ontology:cs_reading_relation('521231b3-9ed5-41cb-bd68-c23861db5d7b', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('521231b3-9ed5-41cb-bd68-c23861db5d7b', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('521231b3-9ed5-41cb-bd68-c23861db5d7b', foundational, bodily_integrity_trumps_collective_benefit).
narrative_ontology:cs_axiom_status(bodily_integrity_trumps_collective_benefit, holdable).
narrative_ontology:cs_axiom_grounding('521231b3-9ed5-41cb-bd68-c23861db5d7b', bodily_integrity_trumps_collective_benefit, deontological).
narrative_ontology:cs_axiom('521231b3-9ed5-41cb-bd68-c23861db5d7b', foundational, informed_consent_non_derogable).
narrative_ontology:cs_axiom_status(informed_consent_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('521231b3-9ed5-41cb-bd68-c23861db5d7b', informed_consent_non_derogable, deontological).
narrative_ontology:cs_reference_frame('521231b3-9ed5-41cb-bd68-c23861db5d7b', informed_consent_sovereignty).
narrative_ontology:cs_drift_state('521231b3-9ed5-41cb-bd68-c23861db5d7b', public_health_emergency_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('521231b3-9ed5-41cb-bd68-c23861db5d7b', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, vulnerable_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, coerced_unvaccinated).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, bodily_autonomy_absolute_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets compulsory vaccination or medical intervention policy under emergency powers or police power statutes. Enforces compliance through employment exclusion, fines, mobility restrictions, and criminal penalties. Frames the mandate as necessary collective health coordination and maintains that individual consent is overridden by communal necessity.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Must submit to unwanted medical intervention or forfeit employment, education, transportation, and social participation. No viable geographic or occupational exit exists because the mandate is enforced nationwide or across all major employers. Experience the arrangement as a direct transfer of bodily control to the state.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, coerced_unvaccinated, payer,
    powerless, biographical, trapped, national).

% Receive reduced exposure risk due to elevated community coverage but do not direct policy design. Depend on the compliance of others for protection against severe disease. Cannot opt out of the herd-immunity benefit but also bear no direct cost of the mandate.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, vulnerable_populations, beneficiary,
    powerless, biographical, constrained, national).

% Monitor constitutional and human rights implications of the mandate. File litigation challenging the compulsion, publish analyses of disproportionate impact, and represent coerced individuals in legal proceedings. Do not collect from or pay into the constraint.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, civil_liberties_observers, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves herd immunity or high vaccination coverage by eliminating free-rider incentives through centralized state compulsion, protecting individuals who cannot be vaccinated or are at high risk of severe disease.
% TRANSFER_FUNCTION: Transfers bodily self-determination and informed consent from individuals to the state public health authority; transfers risk-reduction benefits from the coerced unvaccinated to vulnerable populations and the broader community.
% ABSENT_VOICES: Individuals with naturally acquired immunity, those with niche medical contraindications that broad exemptions overlook, and clinicians dissenting on safety or necessity grounds are excluded from policy advisory roles and regulatory hearings.
% DISAPPEARANCE_RATIONALE: If the mandate disappeared overnight, unvaccinated individuals would regain full bodily autonomy and social participation; disease incidence would likely rise absent substitution by voluntary uptake; the state's emergency authority would contract; public health strategy would reorganize around persuasion and targeted protection rather than universal compulsion.
% FOUNDING_PROBLEM: Preventing epidemic spread of serious infectious disease when voluntary vaccination rates are insufficient to achieve herd immunity, leaving vulnerable populations exposed.
% FOUNDING_PROBLEM_CORROBORATION: Public health historians and epidemiologists outside the immediate state apparatus attest that insufficient voluntary coverage was a real problem in specific outbreaks. Civil liberties scholars and dissenting bioethicists attest that less restrictive alternatives â targeted protection, voluntary campaigns, and therapeutics â were underutilized, contesting whether compulsion was necessary to solve the problem. No neutral party unanimously corroborates the necessity of the coercive arrangement.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85 at interval end) because the constraint transfers control over bodily integrity from individuals to the state without individualized consent. Suppression is high (0.80) because persistence depends on active penalties, employment exclusion, and criminalization of refusal. Theater ratio is moderate (0.45) because enforcement involves performative compliance (credential verification, documentation, status checking) that exceeds pure medical necessity. Accessibility collapse is substantial (0.75) because non-compliant individuals lose access to work, transport, and social life. Resistance is high (0.70) due to sustained constitutional litigation and non-compliance movements. The temporal series show extraction and suppression hardening as the mandate matures and enforcement infrastructure deepens.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (state health authority) experiences the constraint as legitimate emergency coordination saving lives; the payer seat (coerced unvaccinated) experiences it as unilateral rights violation. The engine will compute these as divergent types: the state seat may compute toward rope or scaffold if the coordination function dominates its perspective, while the trapped payer seat will compute toward snare. The tangled_rope claim captures the structural reality that both experiences are grounded in the same arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_public_health_authority is structurally a beneficiary of compliance and expanded authority (low d), despite its agenda-setting role. The coerced_unvaccinated are full targets (high d) because they bear costs with no viable exit. Vulnerable_populations are beneficiaries of the coordination outcome (low d). The divergence is extreme because the trapped exit of the victims amplifies effective extraction while the arbitrage exit of the state dampens it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) â the herd immunity benefit to vulnerable populations is real and would be lost if the mandate were purely performative. It also prevents mislabeling it as pure coordination (rope) â the active suppression of refusal and the extraction of bodily autonomy are not incidental costs but core features of the arrangement. If the founding problem (insufficient voluntary coverage) is solved or was never acute, the constraint risks piton transition: theater rises, beneficiaries fade, and enforcement persists by institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bodily_autonomy_vs_police_power,
    'Is the state''s police power to compel medical intervention constitutionally unlimited in emergencies, or is bodily autonomy a non-derogable right?',
    'Constitutional court ruling establishing absolute versus proportionate limits on public health compulsion.',
    'If absolute bodily autonomy is affirmed, the constraint reclassifies toward snare (the coordination function becomes legally illegitimate); if proportionate police power is affirmed, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bodily_autonomy_vs_police_power, conceptual, 'Whether the mandate''s authority is legally absolute or balanced.').

omega_variable(
    herd_immunity_counterfactual,
    'Would voluntary vaccination rates have been sufficient to protect vulnerable populations in the absence of the mandate?',
    'Comparative analysis of jurisdictions with similar demographics, culture, and healthcare access but no compulsion.',
    'If voluntary rates would have sufficed, the coordination function is cover and the constraint is a snare; if not, the coordination function is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(herd_immunity_counterfactual, empirical, 'Whether the mandate''s coordination outcome was achievable without coercion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external penalties and exclusion) or internalized (acceptance of state medical authority)?',
    'Post-mandate revocation trajectory: if compliance behavior persists after penalties are removed, suppression was partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s extractive depth is greater than surface metrics indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mbap_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mbap_tr_t4, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 4, 0.25).
narrative_ontology:measurement(mbap_tr_t8, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 8, 0.3).
narrative_ontology:measurement(mbap_tr_t12, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 12, 0.35).
narrative_ontology:measurement(mbap_tr_t16, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 16, 0.4).
narrative_ontology:measurement(mbap_tr_t20, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 20, 0.42).
narrative_ontology:measurement(mbap_tr_t24, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(mbap_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(mbap_be_t4, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 4, 0.8).
narrative_ontology:measurement(mbap_be_t8, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 8, 0.82).
narrative_ontology:measurement(mbap_be_t12, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 12, 0.83).
narrative_ontology:measurement(mbap_be_t16, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 16, 0.84).
narrative_ontology:measurement(mbap_be_t20, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(mbap_be_t24, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 24, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(mbap_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(mbap_su_t4, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(mbap_su_t8, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(mbap_su_t12, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(mbap_su_t16, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 16, 0.77).
narrative_ontology:measurement(mbap_su_t20, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(mbap_su_t24, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 24, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the mandate_legitimacy_scope kernel, which decomposes into three structurally distinct claims about the legitimacy of public health mandates: an absolutist bodily autonomy reading (this file), a public health primary reading, and a proportionality reading. Each reading has a distinct epsilon, beneficiary/victim structure, and normative grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
