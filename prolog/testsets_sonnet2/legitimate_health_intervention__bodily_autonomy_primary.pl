% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Bodily-Autonomy-Primary Reading of Legitimate Health Intervention
 *   domain: public health policy / medical ethics / constitutional law
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primary reading of the
 *   contested 'legitimate health intervention' kernel: legitimacy requires
 *   informed, voluntary consent for medical interventions, and state coercion
 *   violates bodily integrity as a threshold matter regardless of the
 *   magnitude of public benefit claimed. Under this reading, mandate
 *   architecture that conditions employment, travel, or public access on
 *   accepting an intervention is coercive extraction dressed as coordination
 *   — the coordination function (population disease control) is real, but it
 *   is achieved by transferring the cost of universal uptake onto objectors
 *   whose consent is structurally overridden. This is ONE of three readings
 *   of the kernel; the public_health_primary and proportionality_reading
 *   siblings are separate constraint stories with their own ε and stakeholder
 *   sets, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - mandate_coerced_individuals: primary target (powerless/trapped) — bears job loss or exclusion for refusing
 *   - public_health_agencies: agenda_setter (institutional/analytical) — designs and defends the mandate architecture
 *   - employers_administering_mandates: secondary beneficiary/agenda_setter (organized/mobile) — implements enforcement, gains compliance tool
 *   - compliant_population_cohort: beneficiary (moderate/mobile) — retains full access, benefits from others bearing enforcement cost
 *   - courts_and_civil_liberties_litigants: analytical observer (institutional/analytical) — adjudicates the legitimacy claim this reading tests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.62).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.7).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Bodily-Autonomy-Primary Reading of Legitimate Health Intervention").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "public health policy / medical ethics / constitutional law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '1d5aefd8-939e-42b9-af7e-7c1b53d76a0e').
narrative_ontology:cs_kernel_codification('1d5aefd8-939e-42b9-af7e-7c1b53d76a0e', distributed).
narrative_ontology:cs_authority_grounding('1d5aefd8-939e-42b9-af7e-7c1b53d76a0e', distributed).
narrative_ontology:cs_reading_relation('1d5aefd8-939e-42b9-af7e-7c1b53d76a0e', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('1d5aefd8-939e-42b9-af7e-7c1b53d76a0e', legitimate_health_intervention__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('1d5aefd8-939e-42b9-af7e-7c1b53d76a0e', foundational, bodily_integrity_as_absolute_threshold).
narrative_ontology:cs_axiom_status(bodily_integrity_as_absolute_threshold, holdable).
narrative_ontology:cs_axiom_grounding('1d5aefd8-939e-42b9-af7e-7c1b53d76a0e', bodily_integrity_as_absolute_threshold, deontological).
narrative_ontology:cs_axiom('1d5aefd8-939e-42b9-af7e-7c1b53d76a0e', foundational, aggregate_benefit_cannot_cure_nonconsent).
narrative_ontology:cs_axiom_status(aggregate_benefit_cannot_cure_nonconsent, holdable).
narrative_ontology:cs_axiom_grounding('1d5aefd8-939e-42b9-af7e-7c1b53d76a0e', aggregate_benefit_cannot_cure_nonconsent, deontological).
narrative_ontology:cs_reference_frame('1d5aefd8-939e-42b9-af7e-7c1b53d76a0e', informed_consent_as_constitutional_baseline).
narrative_ontology:cs_drift_state('1d5aefd8-939e-42b9-af7e-7c1b53d76a0e', post_pandemic_mandate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1d5aefd8-939e-42b9-af7e-7c1b53d76a0e', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, employers_administering_mandates).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, compliant_population_cohort).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, medically_contraindicated_refusers).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, religious_and_conscience_objectors).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, bodily_integrity_as_constitutional_baseline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face loss of employment, school access, travel, or public accommodation unless they accept a medical intervention. Their consent, under this reading, is not meaningfully informed or voluntary once refusal carries material ruin — the choice is compliance or exclusion from ordinary civic life. Exit requires accepting severe economic or social cost.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_coerced_individuals, payer,
    powerless, biographical, trapped, national).

% Have documented medical reasons to decline the intervention but face the same enforcement apparatus as any other objector, forced to seek narrow, often discretionary exemptions administered by the same authority enforcing the mandate.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, medically_contraindicated_refusers, payer,
    powerless, biographical, trapped, national).

% Object on sincerely held religious or conscience grounds. Exemption processes exist in some jurisdictions but are frequently narrowed, litigated, or revoked, leaving objectors to choose between violating conscience and losing access to employment or institutions.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, religious_and_conscience_objectors, payer,
    powerless, biographical, constrained, national).

% Design and defend the mandate architecture, citing population-level benefit and citing legal precedent for compulsory measures. Under this reading, they are the entity whose legitimacy claim is being tested — their public-benefit justification does not, on this reading, cure the coercion because bodily integrity is treated as a threshold right rather than a variable to be weighed against aggregate outcomes.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Implement mandates as a condition of employment, often under government directive or liability-shielding incentive, and gain a workforce compliance tool. They bear little of the individual cost and gain administrative cover and potential legal protection for imposing the requirement.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, employers_administering_mandates, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, employers_administering_mandates, agenda_setter).

% Accept the intervention without objection and retain full access to employment, travel, and public life. They benefit from herd-level protection and from an enforcement regime that falls entirely on objectors rather than on them.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, compliant_population_cohort, beneficiary,
    moderate, biographical, mobile, national).

% Adjudicate challenges to mandate enforcement, weighing bodily integrity claims against state police-power arguments. Their rulings determine whether this reading's premise — that coercion is illegitimate regardless of public benefit — gains or loses ground in binding law.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, courts_and_civil_liberties_litigants, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level disease control by achieving high intervention uptake through mandated compliance rather than persuasion alone, reducing transmission and severe outcomes across a shared population.
% TRANSFER_FUNCTION: Moves the cost of achieving population coverage from the state and compliant majority onto individual objectors, who bear job loss, exclusion, or forced compliance so that aggregate uptake targets are met without relying on voluntary consent.
% ABSENT_VOICES: Individuals who would refuse if the true cost were transparent are structurally absent from the policymaking table; their objections are treated as noise to the coordination function rather than a legitimacy constraint, and exemption boards that hear them are typically staffed by the same agencies enforcing the mandate.
% DISAPPEARANCE_RATIONALE: If the coercive enforcement layer disappeared overnight — no employment, access, or travel conditioning on the intervention — mandate-coerced individuals would regain civic and economic access immediately, uptake among currently-compliant-but-reluctant individuals would likely fall, and employers would lose a compliance mechanism they currently rely on for liability management.
% FOUNDING_PROBLEM: Historical epidemics and low voluntary uptake of protective interventions produced population harm that voluntary persuasion campaigns failed to prevent quickly enough; mandates were built to close that gap.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and employers attest the founding problem remains live given ongoing transmission risk. Civil liberties litigants and bioethicists outside the enforcing agencies attest that, on the bodily-autonomy-primary reading, the founding problem's persistence does not establish that coercive means remain legitimate — legitimacy is a threshold question about consent, not a proportionality question about outcomes, and no party inside the enforcing structure treats it that way.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__bodily_autonomy_primary, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.35 to 0.62) as enforcement mechanisms harden from initial recommendation to conditioned access; this models a mandate regime that began as guidance and progressively tied material stakes (employment, travel, access) to compliance. Suppression is high and stabilizes around 0.70 because, once conditioning is in place, the coercive structure does not need to intensify further to hold — the threat of exclusion is a standing condition, not an escalating one. Theater is low-moderate (0.28) because the enforcement apparatus is mostly functional (real conditioning of real access) rather than symbolic, though administrative appeals and exemption boards carry some performative weight.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (public health agencies), the mandate is a proportionate coordination mechanism justified by outcomes. From the payer seats (coerced individuals, objectors), the same structure is coercion that cannot be cured by outcome — this is the exact fault line the bodily-autonomy-primary reading stakes out against its siblings, which weigh outcomes differently or primarily.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and employers who administer mandates sit near the beneficiary end: they set the terms, bear little personal cost, and gain either institutional legitimacy or liability protection. Mandate-coerced individuals, medically contraindicated refusers, and conscience objectors sit near the full-target end: their consent is structurally overridden by conditioning material survival (employment, access) on compliance, and their exit options are trapped or narrowly constrained. This reading treats their coercion as illegitimate independent of any aggregate benefit computation — the beneficiary/victim split tracks who controls the terms of consent, not who is 'right' about the underlying science.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents this reading from being flattened into either a pure snare (denying the genuine coordination value of achieving high uptake against communicable disease) or a pure rope (ignoring that coercion, on this reading's own axioms, is illegitimate regardless of outcome). The coordination function is real and named; the extraction is also real and asymmetric — both must be true simultaneously for the tangled_rope gate, and this reading's founding claim is precisely that the coordination cannot legitimate the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_validity_under_material_coercion,
    'Is consent obtained under threat of job loss or exclusion from public life meaningfully ''informed consent,'' or is it structurally coerced regardless of formal voluntariness?',
    'Legal and philosophical analysis of duress doctrine as applied to medical consent; comparison to established coercion thresholds in contract and criminal law.',
    'If material-stakes conditioning is found to vitiate consent, this reading''s classification of the mandate architecture as extractive strengthens further; if formal voluntariness (the individual technically could refuse) is held sufficient, the extraction reading weakens toward the proportionality reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_validity_under_material_coercion, conceptual, 'Whether conditioned consent under material threat counts as consent at all.').

omega_variable(
    kernel_reading_selection,
    'Is bodily-autonomy-primary the correct lens for evaluating THIS constraint, or would proportionality_reading or public_health_primary better capture the legitimacy question at stake?',
    'This is inherent kernel contest, not resolvable by data alone — the three readings represent genuinely different normative starting points. Political and judicial settlement (case law trajectory, constitutional doctrine evolution) is the closest available resolution mechanism, though it settles the mechanism, not the underlying premise.',
    'Adopting public_health_primary would remove mandate-coerced individuals from the victim set entirely (their refusal becomes the extraction, not the mandate). Adopting proportionality_reading would make ε conditional on disease severity rather than treating any coercion as intrinsically extractive. This reading''s ε and classification are stable ONLY within the bodily-autonomy-primary frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection, preference, 'Which of the three kernel readings should govern legitimacy assessment — an irreducible framing choice, not an empirical question.').

omega_variable(
    exemption_process_capture,
    'Do religious/medical exemption processes function as genuine accommodation mechanisms or as administrative theater that rarely grants relief while providing legal cover for the mandate?',
    'Empirical audit of exemption grant rates, appeal timelines, and administrative discretion exercised by the same agencies that designed the mandate.',
    'Low grant rates and agency-controlled discretion would raise the effective suppression and theater_ratio for the exemption pathway specifically; high, good-faith grant rates would lower measured suppression for the conscience-objector subgroup.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_process_capture, empirical, 'Whether exemption processes are real accommodation or enforcement theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 6, 0.14).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.18).
narrative_ontology:measurement(legi_tr_t18, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 18, 0.22).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 24, 0.25).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 30, 0.27).
narrative_ontology:measurement(legi_tr_t36, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(legi_be_t18, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(legi_be_t36, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 36, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(legi_su_t18, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(legi_su_t36, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 36, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimate_health_intervention kernel. public_health_primary authors a low-to-moderate ε (coercion justified by measurable outcomes, individual refusal treated as the extractive act). proportionality_reading authors an ε that scales with disease severity and intervention invasiveness. This story (bodily_autonomy_primary) authors a moderate-to-high ε driven by enforcement severity alone, independent of outcome — the defining structural difference is that this reading places mandate-coerced individuals in the victim set unconditionally, which the public_health_primary sibling does not do at all.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__bodily_autonomy_primary, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
