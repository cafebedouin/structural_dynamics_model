% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Authority — Public Health Primacy Reading
 *   domain: public health policy / constitutional law / bioethics
 *
 * SUMMARY:
 *   This story instantiates the public-health-primacy reading of the vaccine
 *   mandate legitimacy kernel: the claim that the state's duty to prevent
 *   collective harm justifies mandate authority, and that unvaccinated status
 *   is properly classified as a negative externality imposed on the
 *   collective rather than a purely private medical choice. Under this
 *   reading, the coordination problem (reaching herd-immunity thresholds) is
 *   treated as sufficient to license coercive enforcement against those who
 *   decline vaccination, with religious and medical exemption categories
 *   narrowed to what the administering bureaucracy is willing to recognize.
 *   This is a distinct constraint from the bodily-autonomy-primacy reading
 *   (which treats coercion as categorically impermissible regardless of
 *   outcome, producing near-zero epsilon because from that seat no legitimate
 *   mandate authority exists to measure) and from the risk-stratification
 *   reading (which conditions legitimacy on actuarial threshold and would
 *   find blanket mandates illegitimate but targeted ones permissible,
 *   producing a lower, threshold-gated epsilon). Each reading is authored as
 *   its own file per the epsilon-invariance principle; this file's epsilon
 *   (0.62) reflects the public-health-primacy reading's own view of the
 *   standing mandate arrangement as it actually operates, not the alternative
 *   arrangement it would replace.
 *
 * KEY AGENTS:
 *   - public_health_bureaucracy: institutional agenda-setter and beneficiary — designs mandate policy, administers exemption review, gains durable regulatory authority
 *   - vaccine_refusers: primary target — unvaccinated status reclassified as externality, bears exclusion from employment/education/public life
 *   - immunocompromised_populations: powerless beneficiary — depends on herd compliance for protection, cited as core justification
 *   - employers_and_institutions: organized secondary agenda-setter/payer — implements enforcement, absorbs litigation and workforce risk
 *   - courts_and_legislatures: analytical observer — adjudicates the constitutional limits of the externality framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.62).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.78).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Authority — Public Health Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public health policy / constitutional law / bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, 'f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1').
narrative_ontology:cs_kernel_codification('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1', distributed).
narrative_ontology:cs_authority_grounding('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1', extraction).
narrative_ontology:cs_interpretation_layer_present('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1').
narrative_ontology:cs_reading_relation('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1', foundational, collective_harm_duty_overrides_individual_medical_choice).
narrative_ontology:cs_axiom_status(collective_harm_duty_overrides_individual_medical_choice, holdable).
narrative_ontology:cs_axiom_grounding('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1', collective_harm_duty_overrides_individual_medical_choice, instrumental).
narrative_ontology:cs_axiom('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1', foundational, unvaccinated_status_constitutes_actionable_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_constitutes_actionable_externality, holdable).
narrative_ontology:cs_axiom_grounding('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1', unvaccinated_status_constitutes_actionable_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1', communicable_disease_emergency_police_power).
narrative_ontology:cs_drift_state('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1', post_acute_outbreak_normalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f6d0d483-6b77-4aa3-b4ba-f3988cc31ac1', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_majority).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, religious_exemption_seekers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, medically_ambiguous_case_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_and_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces mandate policy under a declared duty to prevent collective harm. Sets thresholds for what counts as sufficient risk to justify coercion, administers exemption processes, and gains durable emergency-adjacent authority each time a mandate is upheld. Bears none of the direct cost of noncompliance penalties; collects expanded regulatory jurisdiction and compliance-monitoring infrastructure as a byproduct of enforcement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, beneficiary).

% Receive reduced transmission risk and continued access to employment, travel, and public spaces without additional cost. Under this reading, their compliance is treated as the baseline and the constraint operates almost invisibly on them — they experience no coercion because they already comply.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_majority, beneficiary,
    organized, biographical, mobile, national).

% Cannot vaccinate or achieve full protection themselves and depend entirely on herd-level compliance for protection from exposure. This reading treats their vulnerability as the core justification for treating unvaccinated status as an externality imposed on others.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Lose employment, access to schools, travel, or public accommodation for declining vaccination on grounds ranging from medical caution to political objection. Under this reading their unvaccinated status is reclassified from a private medical choice into a positive externality imposed on the collective, which is the specific move that licenses coercion against them. Exit means accepting exclusion from major institutions.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers, payer,
    moderate, biographical, constrained, national).

% Seek exemption on sincerely held religious grounds but face bureaucratic review boards that can deny claims administratively. This reading treats their exemption claims as subordinate to the collective-harm duty, so denial is framed as legitimate risk management rather than as suppression of belief.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, religious_exemption_seekers, payer,
    powerless, biographical, constrained, national).

% Have contested or borderline medical contraindications not on the bureaucracy's approved exemption list. They cannot access exemption through the process this reading endorses, and are treated identically to voluntary refusers despite a different underlying situation — this reading's externality framing does not distinguish them.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, medically_ambiguous_case_holders, payer,
    powerless, biographical, trapped, national).

% Implement mandate enforcement at the point of contact — termination, disenrollment, denial of service — under legal obligation or liability incentive. They administer the coercion the bureaucracy authorizes but also absorb litigation risk and workforce disruption from doing so.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_and_institutions, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_and_institutions, payer).

% Adjudicate the constitutional and statutory limits of mandate authority, weighing the collective-harm duty against individual liberty claims. Their rulings determine whether this reading's externality framing survives judicial review or is narrowed.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reducing transmission of a communicable disease requires population-level immunity thresholds that voluntary individual choice alone may not reach; a mandate coordinates individually rational but collectively insufficient behavior into a herd-protective equilibrium.
% TRANSFER_FUNCTION: Moves the cost of achieving herd immunity from the collective (in the form of transmission risk, healthcare system strain, and vulnerability of the immunocompromised) onto the specific individuals who decline vaccination, converting their private medical choice into a compliance obligation enforced through exclusion from employment, education, and public life.
% ABSENT_VOICES: Vaccine refusers with sincere but non-religious objections have no institutionally recognized exemption category under this reading and are not heard as a distinct voice — they are folded entirely into the externality-imposing class. Medically ambiguous case holders are similarly unheard: their claims are adjudicated by boards using categories this reading treats as settled science rather than as contested medical judgment.
% DISAPPEARANCE_RATIONALE: If this reading's mandate authority vanished overnight, employers and institutions would lose their legal cover for exclusion, vaccine refusers would regain access to jobs and schools without penalty, and the public health bureaucracy would lose a significant lever of coercive authority and the monitoring infrastructure built to administer it. Vaccination rates among refusers would likely not rise without the enforcement apparatus, altering the population immunity calculus the reading depends on.
% FOUNDING_PROBLEM: Communicable disease outbreaks that individual voluntary vaccination decisions failed to suppress to herd-immunity thresholds, threatening vulnerable populations and overwhelming healthcare capacity during acute outbreak periods.
% FOUNDING_PROBLEM_CORROBORATION: Public health bureaucracies and vaccinated-majority advocates attest the founding problem remains live wherever vaccination coverage is below herd-immunity thresholds, citing epidemiological modeling. Independent legal scholars and some public-health ethicists outside the enforcing bureaucracy attest that for several mandated vaccines the acute outbreak justification has substantially resolved and the mandate now persists as institutionalized risk-aversion and administrative momentum rather than active outbreak response — a reading disputed by the bureaucracy itself.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because this reading converts an individual medical decision into a compliance obligation enforced through exclusion from major institutions, but is not maximal because the underlying coordination function (herd immunity) is genuine where coverage sits below threshold. Suppression (0.78) is high and authored independently of extractiveness because enforcement under this reading depends on excluding non-compliant individuals from employment, schooling, and public accommodation — a structural coercive mechanism, not merely a byproduct of extraction magnitude. Accessibility collapse (0.58) is moderate: some exemption pathways nominally exist but are narrowed by the bureaucracy's own categories, so alternatives partly but not fully collapse. Resistance (0.71) is high, reflecting substantial organized pushback from refusers, religious objectors, and some courts. The suppression-requirement series shows an enforcement ratchet during the acute crisis period (t=0 to t=8) followed by partial relaxation and stabilization (t=12 onward) as the acute outbreak justification weakened but did not fully retract — the founding-problem mismatch (status=contested, verdict=world_rearranges) flags this as a candidate zombie-authority pattern for downstream review, not a settled conclusion.
 *
 * PERSPECTIVAL GAP:
 *   From the bureaucracy's seat, this reading appears as coordination in service of a genuine collective-action problem — the engine should compute something closer to rope or tangled-rope-favoring-coordination from that seat. From the refuser and exemption-seeker seats, the identical structure computes as coercive extraction of compliance through institutional exclusion, because their directionality sits near the full-target end regardless of the bureaucracy's stated justification. This divergence is exactly what the tangled_rope classification is meant to hold: a genuine coordination function (herd immunity) coexists with asymmetric extraction (exclusion cost concentrated on a specific, identifiable class) requiring active enforcement to persist.
 *
 * DIRECTIONALITY LOGIC:
 *   The public health bureaucracy sits at the beneficiary end: it sets the externality framing, administers enforcement, and gains authority regardless of outcome variance. The vaccinated majority and immunocompromised populations are beneficiaries under this reading's own logic — the former bear no cost because compliance is the baseline, the latter are the structurally powerless class whose vulnerability the reading uses to justify coercion against others. Vaccine refusers, religious exemption seekers, and medically ambiguous case holders are the targets: their directionality is pushed toward the full-target end by constrained or trapped exit options (loss of employment/education access) and by this reading's specific move of denying them a legitimate private-choice framing. Employers and institutions occupy a genuinely dual seat: they administer enforcement (agenda-setter) but also bear litigation and disruption costs (payer) — hence the secondary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than dead specifically to avoid two errors this reading must not make: treating the mandate as permanently and unconditionally justified (which would erase the mandatrophy question entirely and license indefinite authority accumulation), and treating it as pure extraction with no coordination function (which would erase the genuine herd-immunity problem that motivated it during acute outbreak periods). The suppression-requirement trajectory — rising sharply during acute crisis, then only partially relaxing — is the empirical signature the mismatch consumer should examine: if founding_problem_status is later corroborated as dead by evidence outside the bureaucracy while disappearance_verdict remains world_rearranges, that combination is the capture/zombie-authority flag this reading's own metrics are structured to expose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_harm_duty_scope_ambiguity,
    'Does the state''s duty to prevent collective harm extend to compelling medical intervention on individuals who pose only probabilistic, not certain, transmission risk, or does this reading overextend a genuine public-health coordination function into a general license for bodily coercion?',
    'Comparative constitutional analysis across jurisdictions with narrower vs. broader collective-harm doctrines, paired with epidemiological data on actual marginal transmission risk attributable to specific unvaccinated individuals versus population-level effects.',
    'A narrow doctrinal scope would confine legitimate mandate authority to the risk-stratification reading''s threshold-gated model; a broad scope sustains this reading''s blanket-authority claim but raises the classification''s proximity to snare if extraction persists after the acute risk resolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_harm_duty_scope_ambiguity, conceptual, 'Whether collective-harm doctrine legitimately extends to blanket mandate authority or only to targeted, risk-proportionate intervention.').

omega_variable(
    externality_reclassification_validity,
    'Is treating unvaccinated status itself as an externality a valid extension of externality doctrine (analogous to pollution) or a category error that launders a private medical choice into a public-harm framing to license coercion?',
    'Philosophical and legal analysis of externality doctrine''s applicability to bodily/medical status as opposed to activity-based harms (e.g., emissions, contagion during active infection vs. status of being unvaccinated absent infection).',
    'If the reclassification is doctrinally sound, this reading''s coordination claim is stronger and the tangled-rope classification leans toward its coordination pole. If it is a category error, the coordination story is closer to cover for extraction and the classification leans toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_reclassification_validity, conceptual, 'Whether unvaccinated status is properly analogized to an externality-generating activity.').

omega_variable(
    sibling_reading_resource_competition,
    'Does this reading''s institutional entrenchment (courts upholding broad mandate authority, bureaucratic infrastructure built to administer it) foreclose meaningful future adoption of the risk-stratification reading''s proportionality standard, or can the two coexist as the acute-crisis and steady-state modes of the same legal framework?',
    'Track judicial and legislative outcomes over subsequent public health events: if courts increasingly cite proportionality/risk-stratification standards to narrow mandate scope, the readings are coexisting with the stratification reading gaining ground; if courts continue to uphold blanket authority under the collective-harm rationale, this reading remains dominant.',
    'Coexistence would validate the ''influences without foreclosing'' relation; a trend toward exclusive stratification-reading adoption would suggest this reading''s institutional entrenchment is being actively displaced rather than merely pressured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_resource_competition, empirical, 'Whether institutional entrenchment of the public-health-primacy reading forecloses the risk-stratification reading''s future adoption or merely creates resource-competition pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 4, 0.72).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 8, 0.85).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the vaccine_mandate_legitimacy kernel. bodily_autonomy_primacy_reading authors near-zero epsilon (no legitimate mandate authority exists to measure from that seat's premises); this file authors epsilon=0.62 (substantial coordination-cum-extraction under active enforcement); risk_stratification_reading authors a threshold-gated epsilon that is low for targeted mandates and rises sharply for blanket mandates. Each file has independent stakeholders, beneficiary/victim structure, and classification; they are linked here rather than merged because merging would violate epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
