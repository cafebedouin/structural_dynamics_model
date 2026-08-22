% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority — Proportionality Reading
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint models the proportionality reading of public health
 *   mandate authority — the view that mandate legitimacy depends on a sliding
 *   scale assessment of threat severity, availability of less restrictive
 *   alternatives, magnitude of coercion, and duration of imposition. Unlike
 *   the public_health_primary reading (mandates as obligation to protect the
 *   vulnerable commons) or the bodily_autonomy_primary reading (mandates as
 *   categorical violation of bodily sovereignty), this reading instantiates a
 *   dynamic constraint whose extractiveness and victim set shift with the
 *   epidemiological and policy context. During a high-threat event
 *   (Ebola-like pathogen), extractiveness rises toward the unvaccinated as
 *   targets; during a low-threat seasonal virus, extractiveness falls and may
 *   invert toward the immunocompromised as victims of inadequate protection.
 *   The constraint requires active enforcement (court orders, sanctions,
 *   mandates) and coordinates collective action around a genuine public
 *   health problem while extracting compliance costs asymmetrically.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda_setter (institutional/biographical/arbitrage/global) — sets threat assessments and mandate scope
 *   - immunocompromised_population: beneficiary (powerless/generational/trapped/local) — gains protection when mandates function
 *   - unvaccinated_under_high_threat: payer (moderate/biographical/constrained/national) — bears compliance costs when threat is high
 *   - immunocompromised_under_low_threat: payer (powerless/generational/trapped/local) — bears infection risk when mandates are relaxed
 *   - bodily_autonomy_advocates: excluded (organized/biographical/identity_locked/national) — categorical objectors structurally excluded from proportionality calculus
 *   - courts: observer (institutional/generational/analytical/national) — adjudicates proportionality challenges
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.45).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.55).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority — Proportionality Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '650abca9-8285-46a9-9d9d-3945222609e2').
narrative_ontology:cs_kernel_codification('650abca9-8285-46a9-9d9d-3945222609e2', distributed).
narrative_ontology:cs_authority_grounding('650abca9-8285-46a9-9d9d-3945222609e2', lineage).
narrative_ontology:cs_interpretation_layer_present('650abca9-8285-46a9-9d9d-3945222609e2').
narrative_ontology:cs_reading_relation('650abca9-8285-46a9-9d9d-3945222609e2', public_health_mandate_authority__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('650abca9-8285-46a9-9d9d-3945222609e2', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('650abca9-8285-46a9-9d9d-3945222609e2', foundational, state_coercion_requires_proportional_justification).
narrative_ontology:cs_axiom_status(state_coercion_requires_proportional_justification, holdable).
narrative_ontology:cs_axiom_grounding('650abca9-8285-46a9-9d9d-3945222609e2', state_coercion_requires_proportional_justification, deontological).
narrative_ontology:cs_axiom('650abca9-8285-46a9-9d9d-3945222609e2', foundational, threat_severity_calibrates_legitimate_coercion).
narrative_ontology:cs_axiom_status(threat_severity_calibrates_legitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('650abca9-8285-46a9-9d9d-3945222609e2', threat_severity_calibrates_legitimate_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('650abca9-8285-46a9-9d9d-3945222609e2', constitutional_proportionality_framework).
narrative_ontology:cs_drift_state('650abca9-8285-46a9-9d9d-3945222609e2', post_pandemic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('650abca9-8285-46a9-9d9d-3945222609e2', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_population).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_infrastructure).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_establishment).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, unvaccinated_under_high_threat).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, immunocompromised_under_low_threat).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, bodily_autonomy_advocates).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, proportionality_principle_in_law).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, least_restrictive_alternative_doctrine).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, dynamic_assessment_of_threat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assess threat severity, determine mandate scope, and enforce compliance through legal orders and sanctions. They control the proportionality calculus and can adjust mandates as conditions change. Their institutional position gives them exit via policy revision rather than personal compliance.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, biographical, arbitrage, national).

% Depend on collective mandate compliance for protection from severe disease. Cannot individually exit the risk environment — their vulnerability is biological and structural. Benefit when mandates function at high threat; become victims when mandates are relaxed under low threat but pathogen still circulates.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_population, beneficiary,
    powerless, generational, trapped, local).

% Face mandate requirements (vaccination, testing, movement restrictions) when threat assessment triggers high coercion. Can comply, seek exemptions, or resist — but exit from the constraint's reach requires leaving jurisdiction or changing status. Bear concentrated compliance costs during high-threat periods.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, unvaccinated_under_high_threat, payer,
    moderate, biographical, constrained, national).

% When threat assessment deems mandates disproportionate (low severity, high alternatives), mandates relax or lapse. Immunocompromised individuals then bear infection risk without the collective protection the mandate provided. Same biological vulnerability, but now on the victim side of the proportionality calculus.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_under_low_threat, payer,
    powerless, generational, trapped, local).

% Hold categorical objection to non-consensual medical intervention regardless of threat level. Structurally excluded from the proportionality calculus because their position rejects the sliding scale itself. Their exit is identity-locked — abandoning the objection would dissolve their organizational and personal identity. They experience the constraint as snare at all threat levels.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, biographical, identity_locked, national).

% Adjudicate proportionality challenges to mandates. Apply balancing tests (severity, alternatives, coercion, duration) that instantiate this reading's logic. Their rulings determine where the sliding scale lands in specific cases, making them the operational interpreters of the constraint's dynamic parameters.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action against infectious disease by calibrating mandate intensity to threat severity, ensuring that coercion is proportional to the public health necessity and that less restrictive alternatives are exhausted first.
% TRANSFER_FUNCTION: Moves compliance costs (vaccination, testing, movement restriction, economic burden) from the immunocompromised/vulnerable population to the general population during high-threat periods; moves infection risk from the general population to the immunocompromised during low-threat periods when mandates relax.
% ABSENT_VOICES: Future generations who inherit the precedent of state mandate authority; populations in jurisdictions without proportionality frameworks (authoritarian mandates or categorical bans); individuals with medical contraindications to vaccines who are neither immunocompromised nor voluntary refusers — their specific situation is not a recognized category in the proportionality calculus.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, mandates would either become categorical (public_health_primary takes over — permanent high coercion) or be categorically banned (bodily_autonomy_primary takes over — zero mandate authority). The dynamic calibration mechanism would disappear, leaving only the two fixed endpoints. The world rearranges because the sliding scale is the only structure that permits mandates to exist at all while containing their coercion.
% FOUNDING_PROBLEM: How to authorize state coercion for public health without enabling unlimited state power over bodies — the constitutional problem of defining when collective necessity overrides individual sovereignty in medical intervention.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts in multiple jurisdictions (German Bundesverfassungsgericht, Canadian Supreme Court, US Supreme Court Jacobson lineage) attest the proportionality framework was built to solve this problem. Public health scholars (Gostin, Childress, Kass) corroborate the framing. Bodily autonomy advocates contest whether the problem was ever solved or merely displaced.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).
:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.45 reflects the midpoint of the dynamic range: under high threat (Ebola-like) extraction approaches 0.7-0.8 (severe coercion, limited alternatives); under low threat (seasonal virus) extraction approaches 0.1-0.2 (minimal coercion, broad alternatives). The measurement series captures this oscillation over a 90-period cycle (e.g., pandemic waves). Suppression 0.55 reflects that enforcement is active but not total — mandates face legal challenges and compliance varies. Theater ratio 0.25 indicates some performative maintenance (expired mandates kept on books, symbolic enforcement) but genuine coordination function persists. Accessibility collapse 0.4 shows alternatives exist (remote work, masking, testing) but narrow under high threat. Resistance 0.6 reflects sustained legal and political contestation across the cycle.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat types from structural data: public_health_authorities (d near 0.0) see a rope/tangled_rope coordinating collective protection; immunocompromised (d near 0.0 under high threat, near 1.0 under low threat) experience a constraint that flips between coordination and extraction; unvaccinated (d near 1.0 under high threat, near 0.5 under low threat) experience extraction that varies with threat level; bodily_autonomy_advocates (d=1.0, identity_locked) experience a snare regardless of threat level. This seat divergence is the structural signature of a dynamic proportionality constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: immunocompromised_population (primary), healthcare_infrastructure, public_health_establishment. Victims declared: unvaccinated_under_high_threat (primary under high threat), immunocompromised_under_low_threat (primary under low threat), bodily_autonomy_advocates (always). Directionality derives from these declarations plus power/exit: authorities (institutional/arbitrage) get low d; immunocompromised (powerless/trapped) get high d when threat is low; unvaccinated (moderate/constrained) get high d when threat is high; bodily_autonomy_advocates (organized/identity_locked) get d=1.0 fixed. The sliding scale means d values shift across the interval — the engine's temporal integration handles this via the measurement series.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing catastrophic epidemic spread) is live under high threat but contested/dead under low threat. The constraint persists across the full cycle partly because the institutional machinery built for high-threat moments remains active during low-threat periods (mandatrophy), and partly because the proportionality framework itself is a genuine coordination mechanism that prevents both categorical mandates and categorical refusal. The theater ratio rise during low-threat periods (0.4 at T=60) signals performative maintenance of authority — mandates kept 'on the books' for readiness rather than necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_proportionality,
    'Is this constraint one reading of the public_health_mandate_authority kernel, and what would sibling readings change?',
    'Structural comparison of proportionality_reading with public_health_primary and bodily_autonomy_primary: different ε, different victim sets, different claimed_type. The proportionality_reading instantiates a dynamic constraint with threat-dependent extraction; the sibling readings instantiate fixed constraints.',
    'Confirms this is a kernel reading; routes committer structure to omega rather than standard fields. Classification diverges from siblings: proportionality_reading → tangled_rope; public_health_primary → rope; bodily_autonomy_primary → mountain/snare depending on seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_proportionality, conceptual, 'This constraint is the proportionality_reading of the public_health_mandate_authority kernel; siblings are public_health_primary and bodily_autonomy_primary.').

omega_variable(
    proportionality_measurement_ambiguity,
    'How should the sliding scale parameters (severity, alternatives, coercion, duration) be measured and weighted?',
    'Judicial precedent analysis across jurisdictions; epidemiological modeling of threat severity; constitutional law scholarship on balancing tests.',
    'If measurement converges, the dynamic constraint could stabilize into a predictable coordination mechanism; if measurement remains contested, extractiveness oscillates with political context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, conceptual, 'Irreducible ambiguity in operationalizing the proportionality test.').

omega_variable(
    victim_set_boundary_shift,
    'At what threat threshold does the victim set flip from unvaccinated to immunocompromised?',
    'Empirical study of judicial rulings and public health orders across pathogen severity gradients; analysis of when courts flip from protecting bodily autonomy to protecting vulnerable populations.',
    'Determines whether the constraint is genuinely dynamic (sliding scale) or merely rhetorical cover for fixed extraction. If the flip point is stable and observable, the reading has structural coherence; if it tracks political power, it is a false proportionality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_boundary_shift, empirical, 'Whether the victim boundary genuinely slides or is a political variable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phma_proportionality_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(phma_proportionality_tr_t0, observed).
narrative_ontology:measurement(phma_proportionality_tr_t30, public_health_mandate_authority__proportionality_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(phma_proportionality_tr_t30, observed).
narrative_ontology:measurement(phma_proportionality_tr_t60, public_health_mandate_authority__proportionality_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(phma_proportionality_tr_t60, observed).
narrative_ontology:measurement(phma_proportionality_tr_t90, public_health_mandate_authority__proportionality_reading, theater_ratio, 90, 0.3).
narrative_ontology:measurement_basis(phma_proportionality_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(phma_proportionality_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(phma_proportionality_be_t0, observed).
narrative_ontology:measurement(phma_proportionality_be_t30, public_health_mandate_authority__proportionality_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement_basis(phma_proportionality_be_t30, observed).
narrative_ontology:measurement(phma_proportionality_be_t60, public_health_mandate_authority__proportionality_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement_basis(phma_proportionality_be_t60, observed).
narrative_ontology:measurement(phma_proportionality_be_t90, public_health_mandate_authority__proportionality_reading, base_extractiveness, 90, 0.35).
narrative_ontology:measurement_basis(phma_proportionality_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(phma_proportionality_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(phma_proportionality_su_t0, observed).
narrative_ontology:measurement(phma_proportionality_su_t30, public_health_mandate_authority__proportionality_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(phma_proportionality_su_t30, observed).
narrative_ontology:measurement(phma_proportionality_su_t60, public_health_mandate_authority__proportionality_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement_basis(phma_proportionality_su_t60, observed).
narrative_ontology:measurement(phma_proportionality_su_t90, public_health_mandate_authority__proportionality_reading, suppression_requirement, 90, 0.45).
narrative_ontology:measurement_basis(phma_proportionality_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Part of the public_health_mandate_authority constraint family (3 readings of one kernel). This reading (proportionality) creates dynamic ε and sliding victim set; public_health_primary creates fixed coordination with immunocompromised as primary beneficiary; bodily_autonomy_primary creates fixed extraction with unvaccinated as primary victim. The three readings share the same referent (state mandate authority over medical intervention) but instantiate different constraints with different ε, different structural relationships, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__proportionality_reading, powerless, 0.85).
constraint_indexing:directionality_override(public_health_mandate_authority__proportionality_reading, moderate, 0.7).
constraint_indexing:directionality_override(public_health_mandate_authority__proportionality_reading, organized, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
