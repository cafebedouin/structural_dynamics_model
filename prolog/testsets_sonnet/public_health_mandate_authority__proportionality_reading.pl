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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
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
 *   This constraint instantiates the proportionality reading of the
 *   public_health_mandate_authority kernel: mandate legitimacy is not fixed
 *   by a categorical commitment to either collective protection or individual
 *   bodily sovereignty, but is calculated on a sliding scale across four
 *   factors — threat severity, alternative availability, coercion magnitude,
 *   and imposition duration. Unlike the fixed-boundary readings
 *   (public_health_primary, bodily_autonomy_primary), this reading produces a
 *   DYNAMIC victim set: whether the immunocompromised or the
 *   unvaccinated/noncompliant end up bearing the greater structural cost
 *   depends on where the current threat assessment sits on the scale. A
 *   high-severity, no-alternative, short-duration mandate (e.g. an
 *   Ebola-level pathogen with no treatment) computes as low extraction and
 *   low suppression under this reading's own logic; a low-severity,
 *   alternative-rich, indefinite-duration mandate (e.g. a mild seasonal
 *   respiratory virus with an existing treatment pathway) computes as high
 *   extraction and high suppression. The measurement series traces one
 *   hypothetical severe-threat episode (rising then receding) to illustrate
 *   this oscillation structurally, not to claim any single historical event.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda_setter (institutional/analytical) — administers the sliding-scale test and sets mandate scope
 *   - immunocompromised_populations: beneficiary (powerless/trapped) — depends on population compliance for indirect protection
 *   - mandate_noncompliant_individuals: payer (moderate/constrained) — bears coercive cost scaled to the current threat assessment
 *   - medically_exempt_individuals_subject_to_review: payer/excluded (powerless/trapped) — legitimate exemption subject to ongoing proportionality review
 *   - courts_and_review_bodies: observer (institutional/analytical) — adjudicates whether specific applications of the test are proportionate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.42).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.48).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority — Proportionality Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '62e14a07-5981-43c4-94c0-082fd623ab45').
narrative_ontology:cs_kernel_codification('62e14a07-5981-43c4-94c0-082fd623ab45', distributed).
narrative_ontology:cs_authority_grounding('62e14a07-5981-43c4-94c0-082fd623ab45', distributed).
narrative_ontology:cs_reading_relation('62e14a07-5981-43c4-94c0-082fd623ab45', public_health_mandate_authority__public_health_primary, influences).
narrative_ontology:cs_reading_relation('62e14a07-5981-43c4-94c0-082fd623ab45', public_health_mandate_authority__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('62e14a07-5981-43c4-94c0-082fd623ab45', foundational, legitimacy_is_a_function_of_threat_conditions).
narrative_ontology:cs_axiom_status(legitimacy_is_a_function_of_threat_conditions, holdable).
narrative_ontology:cs_axiom_grounding('62e14a07-5981-43c4-94c0-082fd623ab45', legitimacy_is_a_function_of_threat_conditions, instrumental).
narrative_ontology:cs_axiom('62e14a07-5981-43c4-94c0-082fd623ab45', secondary, duration_of_imposition_independently_degrades_legitimacy).
narrative_ontology:cs_axiom_status(duration_of_imposition_independently_degrades_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('62e14a07-5981-43c4-94c0-082fd623ab45', duration_of_imposition_independently_degrades_legitimacy, empirically_contingent).
narrative_ontology:cs_created_at('62e14a07-5981-43c4-94c0-082fd623ab45', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_system_capacity).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, mandate_noncompliant_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, medically_exempt_individuals_subject_to_review).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and adjusts mandate scope by applying the sliding-scale test — threat severity, alternative availability, coercion magnitude, duration — to specific interventions (vaccination, masking, isolation). Can tighten or loosen mandates as epidemiological data changes. Bears reputational and legal cost if the proportionality assessment is later judged wrong in either direction.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Depend on population-level compliance for indirect protection (herd effects, reduced circulating pathogen load) because their own immune response to intervention may be inadequate. Cannot personally achieve the protection the mandate is designed to produce; their situation is the primary variable the proportionality test claims to weigh.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, local).

% Bear direct coercive costs of noncompliance — exclusion from employment, travel, education, or social participation — scaled (in this reading) to the proportionality assessment rather than applied categorically. Can exit particular jurisdictions or sectors at real cost, but cannot fully exit the mandate's reach where it is broadly imposed.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, mandate_noncompliant_individuals, payer,
    moderate, biographical, constrained, regional).

% Hold a legitimate medical exemption but must repeatedly justify it to gatekeeping authorities as the proportionality assessment shifts; the sliding-scale reading treats their exemption as provisional and reviewable rather than fixed, which can operate as a second, narrower coercive mechanism on people already excluded from the intervention on medical grounds.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, medically_exempt_individuals_subject_to_review, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, medically_exempt_individuals_subject_to_review, excluded).

% Represents hospital and clinical capacity as a finite shared resource; the proportionality test explicitly weighs threat to this capacity as one of its four axes. Not an actor itself but the abstract good the sliding-scale calculation is partly calibrated to protect.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, healthcare_system_capacity, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(public_health_mandate_authority__proportionality_reading, healthcare_system_capacity).

% Argue the proportionality test is applied post hoc to justify mandates already decided on other grounds, and that the four factors are weighted inconsistently across jurisdictions and pathogens without transparent criteria. Participate in litigation and public comment but do not control how the test is applied.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, civil_liberties_advocates, excluded,
    organized, biographical, constrained, national).

% Adjudicate specific mandate challenges by applying or refining the sliding-scale test to case facts. Can strike down mandates found disproportionate to threat severity or duration, which retroactively reshapes what public_health_authorities are permitted to impose.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, courts_and_review_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured method for calibrating coercive public health measures to actual threat conditions, so that emergency-level interventions are not applied to mild threats and inadequate interventions are not applied to severe ones — coordinating the population's collective exposure risk against the population's collective liberty cost.
% TRANSFER_FUNCTION: Moves compliance burden and coercive cost onto noncompliant and provisionally-exempt individuals in proportion (in principle) to threat severity, and moves protection benefit toward immunocompromised populations and healthcare capacity; the magnitude of the transfer is designed to fluctuate with the assessed threat rather than remain fixed.
% ABSENT_VOICES: Civil liberties advocates participate in litigation but do not set the proportionality criteria; medically exempt individuals subject to repeated review have limited standing to contest the review process itself, only individual determinations; future affected populations under an as-yet-unassessed pathogen have no voice in how the scale is currently calibrated.
% DISAPPEARANCE_RATIONALE: Public health authorities and immunocompromised advocates would say the world rearranges badly — mandate authority would default to either blanket imposition or blanket refusal, both worse than calibration. Civil liberties advocates would say the world is largely unchanged for the better, since the proportionality frame is argued to function mainly as legal cover for whatever mandate authorities already intended to impose. The dispute is genuinely unresolved because it depends on whether the calibration function is real or theatrical, which itself varies by episode.
% FOUNDING_PROBLEM: Courts and legislatures needed a workable legal standard between two unworkable extremes: categorical deference to public health authority (which permits arbitrarily severe measures against mild threats) and categorical bodily-autonomy immunity (which forbids any compulsory measure regardless of threat severity). The sliding-scale test was built to let mandate legitimacy track actual conditions.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside both the public-health-authority and civil-liberties-advocacy camps (e.g., administrative law academics analyzing Jacobson-lineage case law) attest the proportionality framework remains a live and evolving doctrinal tool, applied inconsistently across pathogens and jurisdictions. Civil liberties litigants attest the test has in practice rarely resulted in a court finding a mandate disproportionate once invoked, suggesting the founding problem of genuine calibration may be substantially unmet in application even where the doctrine remains formally live.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, contested).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction and suppression are authored as MODERATE and OSCILLATING rather than fixed high or low, because this reading's defining structural feature is that its extractiveness is a function of the current position on the four-factor scale, not a constant. The measurement series models a single severe-threat episode: extraction and suppression rise as threat severity increases and alternatives are scarce (t=6 to t=12), then fall as treatments become available and duration stretches past what the initial threat assessment justified (t=18 onward) — illustrating that duration itself becomes an independent extraction driver once the original threat has receded but the mandate persists (t=30 to t=36 shows a modest re-rise, representing renewed threat or bureaucratic inertia in unwinding the mandate). Theater ratio tracks moderately with suppression, reflecting that some enforcement infrastructure persists as institutional habit even as the underlying threat justification weakens.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter's seat (public_health_authorities), the arrangement is a genuine calibration tool that adapts coercion to conditions — a rope-like coordination mechanism when working as designed. From the payer seats, especially medically_exempt_individuals_subject_to_review, the same structure can look like enforcement that persists past its threat-justification, particularly during the low-severity tail of the measured interval (t=24 onward) where suppression remains elevated relative to the receding threat — this is exactly the tangled-rope signature the engine should detect: genuine coordination function at the peak, asymmetric extraction in the tail.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised populations and healthcare capacity are declared beneficiaries because the coordination function (reduced circulating pathogen load, preserved clinical capacity) genuinely accrues to them, and they cannot exit the need for population-level protection given their own physiological limits (trapped exit). Mandate_noncompliant_individuals and medically_exempt_individuals_subject_to_review are declared victims because the coercive cost — exclusion, review burden, compliance cost — falls on them directly, and their exit options are constrained-to-trapped depending on how essential the excluded activity is to their livelihood. The proportionality reading's structural distinctiveness is that d is not fixed across the interval for any of these groups: the SAME agent's effective extraction should rise and fall with the same threat-severity variable that drives the measurement series, which is the mechanism this reading is built to formalize.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading is structurally designed to PREVENT mandatrophy by making duration one of its four explicit factors — a mandate that outlives its founding threat is supposed to lose legitimacy under its own test. Whether this self-correction actually operates, or whether courts and review bodies defer to agenda-setters past the point of proportionate justification, is exactly the founding_problem_status='contested' finding above: the doctrine claims dynamic self-limitation, but corroborating evidence from outside the benefiting institutions suggests the correction rarely fires in practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_reading_identity,
    'Is the sliding-scale test a genuinely distinct constraint from public_health_primary and bodily_autonomy_primary, or is it functionally equivalent to whichever fixed reading the deciding authority already favors, dressed in calibration language?',
    'Track outcomes across many mandate challenges: if courts applying the proportionality test produce outcomes that vary meaningfully with actual threat/alternative/duration data (not just with which mandate is being challenged), the test is doing real calibration work. If outcomes correlate almost perfectly with institutional priors regardless of the four-factor data, the proportionality reading collapses into whichever fixed reading the adjudicator held before applying the test.',
    'If the test collapses into a fixed reading in practice, this constraint''s dynamic ε claim is false and the story should be re-authored as a disguised instance of one of the sibling readings rather than a third distinct kernel-reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_reading_identity, empirical, 'Whether the proportionality test does genuine calibration work or is cover for a predetermined fixed reading.').

omega_variable(
    duration_factor_self_correction,
    'Does the duration factor in the sliding-scale test actually cause mandates to be withdrawn or narrowed once the founding threat recedes, or does institutional inertia override the test''s own self-limiting logic?',
    'Compare mandate sunset timing against independent epidemiological threat-recession data across multiple jurisdictions and pathogens; a self-correcting test should show mandate withdrawal tracking threat decline with modest lag, not indefinite persistence.',
    'If duration systematically fails to trigger correction, the proportionality reading''s claimed advantage over the fixed readings (avoiding mandatrophy) is not realized in practice, and the measured tail-end extraction (t=24-36) is the norm rather than an aberration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duration_factor_self_correction, empirical, 'Whether the duration factor functions as designed or is overridden by institutional inertia.').

omega_variable(
    dynamic_victim_boundary_framing,
    'Is treating the victim set as genuinely dynamic (shifting between immunocompromised and noncompliant populations based on threat assessment) the most defensible framing, or does the proportionality test''s actual application always land on the same population (noncompliant individuals) regardless of the officially cited threat level — making the ''dynamic boundary'' claim itself a piece of legitimating language rather than a structural fact?',
    'Cross-reference historical mandate applications: if low-threat mandates (mild seasonal pathogens) are imposed with the same coercive severity as high-threat mandates, the victim boundary is not actually tracking the stated factors and the dynamic-boundary framing is aspirational rather than descriptive.',
    'A finding that the boundary is not actually dynamic would suggest this reading, despite its formal four-factor structure, computes closer to the public_health_primary reading in practice — informing which sibling reading this constraint''s real-world instances resemble.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dynamic_victim_boundary_framing, conceptual, 'Whether the dynamic victim-set claim is structurally real or a legitimating narrative over a fixed application pattern.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__proportionality_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__proportionality_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__proportionality_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__proportionality_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__proportionality_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__proportionality_reading, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__proportionality_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__proportionality_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__proportionality_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__proportionality_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__proportionality_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__proportionality_reading, base_extractiveness, 36, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__proportionality_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__proportionality_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__proportionality_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__proportionality_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__proportionality_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__proportionality_reading, suppression_requirement, 36, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint, public_health_primary, and bodily_autonomy_primary are three readings of the same kernel (public_health_mandate_authority) rather than one constraint viewed three ways. Each has its own ε: public_health_primary's ε is expected to be lower and more stable (collective benefit dominates the framing, fewer factors modulate it), bodily_autonomy_primary's ε is expected to be higher and more stable in the opposite direction (any compulsion is treated as maximal violation regardless of threat), and this proportionality reading's ε is the only one of the three that is authored as explicitly time-varying, since its defining structural claim is that legitimacy — and therefore effective extraction — tracks threat conditions rather than remaining fixed. All three should be read together as the decomposition of the colloquial phrase 'is a vaccine mandate legitimate,' which conflates three structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
