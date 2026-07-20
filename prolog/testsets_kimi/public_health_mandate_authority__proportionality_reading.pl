% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Public Health Mandate Authority â Proportionality Reading
 *   domain: public health law / constitutional rights / bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the proportionality_reading of the
 *   public_health_mandate_authority kernel. It models the arrangement whereby
 *   public health mandate legitimacy is contingent on a sliding-scale
 *   assessment of threat severity, alternative policy availability, coercion
 *   magnitude, and duration. Unlike the categorical bodily_autonomy_primary
 *   reading (which forecloses all mandate authority) and the
 *   public_health_primary reading (which minimizes individual limits), this
 *   reading produces a dynamic constraint where the victim boundary shifts
 *   with empirical conditions. Both immunocompromised populations and coerced
 *   mandate subjects can occupy victim status: the former when
 *   proportionality analysis fails to deliver promised protection, the latter
 *   when coercion exceeds what the threat justifies. The constraint
 *   coordinates collective disease response while extracting bodily autonomy;
 *   its proportionality veneer is meant to limit extraction but may become
 *   performative under institutional pressure.
 *
 * KEY AGENTS:
 *   - public_health_authority: Agenda setter (institutional/constrained) â administers mandates and performs proportionality justification.
 *   - coerced_mandate_subjects: Primary target (powerless/constrained) â bear bodily coercion and exclusion costs.
 *   - immunocompromised_population: Dual-positioned beneficiary/payer (moderate/constrained) â rely on collective protection but pay when proportionality fails.
 *   - healthcare_infrastructure: Secondary beneficiary (institutional/constrained) â gains capacity protection.
 *   - judicial_review_bodies: Analytical observer (institutional/analytical) â evaluate proportionality without experiencing coercion.
 *   - excluded_alternative_strategists: Excluded voice (moderate/constrained) â propose alternatives that proportionality analysis formally considers but often dismisses.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.6).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.65).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority â Proportionality Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public health law / constitutional rights / bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '32aa16c0-03cf-45f6-a193-20f29df9529f').
narrative_ontology:cs_kernel_codification('32aa16c0-03cf-45f6-a193-20f29df9529f', formalized).
narrative_ontology:cs_authority_grounding('32aa16c0-03cf-45f6-a193-20f29df9529f', lineage).
narrative_ontology:cs_interpretation_layer_present('32aa16c0-03cf-45f6-a193-20f29df9529f').
narrative_ontology:cs_reading_relation('32aa16c0-03cf-45f6-a193-20f29df9529f', public_health_mandate_authority__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('32aa16c0-03cf-45f6-a193-20f29df9529f', public_health_mandate_authority__public_health_primary, influences).
narrative_ontology:cs_axiom('32aa16c0-03cf-45f6-a193-20f29df9529f', foundational, coercion_proportionate_to_threat).
narrative_ontology:cs_axiom_status(coercion_proportionate_to_threat, holdable).
narrative_ontology:cs_axiom_grounding('32aa16c0-03cf-45f6-a193-20f29df9529f', coercion_proportionate_to_threat, deontological).
narrative_ontology:cs_axiom('32aa16c0-03cf-45f6-a193-20f29df9529f', foundational, threat_severity_empirically_indexed).
narrative_ontology:cs_axiom_status(threat_severity_empirically_indexed, holdable).
narrative_ontology:cs_axiom_grounding('32aa16c0-03cf-45f6-a193-20f29df9529f', threat_severity_empirically_indexed, empirically_contingent).
narrative_ontology:cs_reference_frame('32aa16c0-03cf-45f6-a193-20f29df9529f', constitutional_proportionality_default).
narrative_ontology:cs_drift_state('32aa16c0-03cf-45f6-a193-20f29df9529f', post_pandemic_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('32aa16c0-03cf-45f6-a193-20f29df9529f', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_population).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_infrastructure).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, coerced_mandate_subjects).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, immunocompromised_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers infectious disease response through legally mandated interventions. Must formally justify each mandate via proportionality analysis assessing threat severity, available alternatives, coercion magnitude, and duration. Captures expanded police power and policy compliance through the mandate mechanism, but is itself constrained by judicial proportionality review and statutory limits.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authority, agenda_setter,
    institutional, generational, constrained, national).

% Individuals subjected to mandatory medical intervention or exclusion from workplaces, transport, and social participation based on proportionality-determined threat levels. Exit is blocked by legal penalties, employment termination, and social exclusion. Bear the direct cost of bodily coercion, privacy intrusion, and autonomy loss.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, coerced_mandate_subjects, payer,
    powerless, biographical, constrained, national).

% Relies on collective compliance to reduce infection exposure. Benefits from herd protection when mandates are effective and proportionate. When proportionality analysis fails to account for transmission dynamics, vaccine leakage, or waning immunity, they remain unprotected and simultaneously exposed to the social externalities of coercion without gaining the promised risk reduction.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_population, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, immunocompromised_population, payer).

% Hospitals and health systems benefit from reduced patient surges during outbreaks. Mandate compliance shifts the burden of acute disease away from critical care capacity. Cannot exit the public health regulatory framework without ceasing to operate as licensed infrastructure.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, healthcare_infrastructure, beneficiary,
    institutional, generational, constrained, national).

% Courts and tribunals that review whether mandate designs satisfy proportionality tests. They evaluate epidemiological threat evidence, alternative policy fitness, and coercion minimization but do not themselves experience the mandate's bodily or social costs.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, judicial_review_bodies, observer,
    institutional, generational, analytical, national).

% Public health researchers and practitioners advocating non-pharmaceutical or voluntary strategies. Their proposed alternatives are formally considered in proportionality analysis but are often dismissed as insufficient by the agenda-setting authority, leaving them without meaningful influence on the final mandate design.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, excluded_alternative_strategists, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__proportionality_reading, public_health_authority).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protecting population health and healthcare system capacity during infectious disease outbreaks by coordinating collective behavioral compliance when voluntary measures are assessed as insufficient to prevent systemic collapse.
% TRANSFER_FUNCTION: Transfers bodily autonomy, privacy, and freedom of movement from individuals to public health authorities, and transfers infection risk exposure from vulnerable populations to the general compliant population.
% ABSENT_VOICES: Individuals who experience severe adverse effects from mandated compliance; alternative public health strategists emphasizing non-pharmaceutical interventions; and civil libertarians who reject the proportionality frame itself as insufficiently protective of bodily sovereignty.
% DISAPPEARANCE_RATIONALE: If proportionality-constrained mandate authority vanished, outbreak response would shift to voluntary measures or uncoordinated local policies; healthcare capacity management would fragment; the immunocompromised would lose structured collective protection; and coerced individuals would regain full bodily autonomy. The legal-institutional framework for emergency public health powers would require reconstruction.
% FOUNDING_PROBLEM: How to protect vulnerable populations and maintain healthcare system functionality during severe infectious disease outbreaks when individual voluntary behavior is assessed as insufficient to prevent systemic collapse.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and public health historians attest to the founding problem from within the benefiting framework. Civil liberties scholars, bioethicists, and legal scholars from outside the beneficiary set contest that the problem justifies coerced bodily intervention, arguing that voluntary frameworks and targeted protection suffice.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60 at interval end, peak 0.72) reflects that coercion is the central mechanism; even when proportionately limited, it extracts bodily autonomy. The dynamic range (0.35â0.72) captures variation with threat severity. Suppression (0.40â0.78) scales with enforcement intensity: penalties, exclusion, and social control rise with mandate scope. Theater_ratio (0.15â0.55) peaks when proportionality analysis becomes ritualized â when the same conclusion (mandate justified) is reached regardless of shifting threat metrics. Accessibility_collapse (0.50) indicates that once proportionality is accepted as the framework, non-coercive alternatives become institutionally invisible. Resistance (0.70) is high because the constraint generates sustained political and legal opposition from both civil libertarians and mandate skeptics.
 *
 * PERSPECTIVAL GAP:
 *   The public_health_authority seat perceives coordination (collective protection achieved through managed compliance). The coerced_mandate_subjects seat perceives extraction (bodily autonomy seized under empirical justification they dispute). The immunocompromised_population seat perceives conditional coordination that may convert to extraction if the mandate is under-inclusive or ineffective. The judicial_review_bodies seat perceives analytical governance. These divergences are structurally derived from the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authority has low d (beneficiary of expanded coercive capacity and policy efficacy). Coerced mandate subjects have high d (full target of bodily extraction). Immunocompromised population sits near symmetric but tilts toward target when proportionality fails â their d is elevated by the victim declaration and constrained exit. Healthcare infrastructure has low d (beneficiary of reduced surge). Judicial observers have analytical exit, placing them outside the extraction gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality framework is designed as a Scaffold-like limit: it carries an implicit sunset (duration assessment) and justifies transition, not steady-state coercion. However, in practice, the constraint risks Mandatrophy when the founding problem (acute outbreak) resolves but the mandate authority persists. The temporal measurements show theater_ratio peaking at T=24 when threat recedes but mandates continue â a classic piton drift signal. If the proportionality test were functioning as designed, base_extractiveness would track threat severity down symmetrically; the partial decoupling at T=24â30 indicates institutional inertia threatening to convert a conditional coordination into inertial extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'How would classification change if bodily_autonomy_primary or public_health_primary readings were adopted instead of this proportionality reading?',
    'Comparative analysis of the sibling constraint stories in the public_health_mandate_authority kernel family.',
    'Bodily_autonomy_primary would eliminate mandate authority entirely (no extraction, no coordination). Public_health_primary would relax proportionality limits and increase extractiveness. This reading sits between them as a conditional, dynamic constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'This constraint''s position within the contested kernel family.').

omega_variable(
    proportionality_as_limit_or_license,
    'Does the proportionality framework function as a genuine check on state coercion, or as a retrospective ritual that legitimates predetermined policy choices?',
    'Systematic review of proportionality determinations: measure correlation between threat severity metrics and mandate intensity, controlling for political variables.',
    'If ritualized, theater_ratio understates performative maintenance and the constraint computes closer to snare. If genuine, the dynamic victim boundary is a legitimate feature of adaptive governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_limit_or_license, empirical, 'Whether proportionality analysis is substantive or performative.').

omega_variable(
    dynamic_victim_boundary,
    'The victim set includes both coerced mandate subjects and the immunocompromised population depending on proportionality assessment accuracy. Is this boundary instability a feature of legitimate flexibility or a defect of arbitrary application?',
    'Outcome tracking: compare infection and compliance rates across jurisdictions with different proportionality thresholds to determine whether dynamic boundaries track real risk or administrative convenience.',
    'If arbitrary, the constraint''s effective extraction is higher for both groups than the structural measure suggests because neither can rely on stable protection. If legitimate, the constraint adapts correctly to varying threat levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_victim_boundary, empirical, 'Instability of victim boundary under proportionality assessment.').

omega_variable(
    threat_severity_objectivity,
    'Is threat severity in proportionality analysis assessed through falsifiable epidemiological metrics, or is it politically constructed to justify coercion levels selected on other grounds?',
    'Audit mandate justifications against contemporaneous independent epidemiological forecasts; measure divergence between expert consensus and declared threat levels.',
    'If constructed, the empirically_contingent axiom is operationally void and the constraint extracts regardless of actual threat. If objective, the sliding scale functions as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threat_severity_objectivity, empirical, 'Objectivity of threat severity assessment in proportionality analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__proportionality_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__proportionality_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__proportionality_reading, theater_ratio, 18, 0.45).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__proportionality_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__proportionality_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__proportionality_reading, theater_ratio, 36, 0.4).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__proportionality_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__proportionality_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__proportionality_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__proportionality_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__proportionality_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__proportionality_reading, base_extractiveness, 36, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__proportionality_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__proportionality_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__proportionality_reading, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__proportionality_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__proportionality_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__proportionality_reading, suppression_requirement, 36, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__public_health_primary).

% DUAL FORMULATION NOTE:
% The public_health_mandate_authority kernel decomposes into three structurally distinct constraints: bodily_autonomy_primary (categorical prohibition), proportionality_reading (conditional coercion), and public_health_primary (collective obligation). Each reading has distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
