% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Public Health Primacy Reading of Vaccine Mandate Legitimacy
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This story instantiates the public-health-primacy reading of the vaccine
 *   mandate legitimacy kernel: the state's police-power duty to prevent
 *   collective harm justifies compelling or conditioning vaccination, and
 *   unvaccinated status is treated structurally as a negative externality on
 *   the public rather than a private medical choice. This is ONE of three
 *   readings of a contested kernel (bodily_autonomy_primacy_reading treats
 *   state coercion as categorically impermissible;
 *   risk_stratification_reading ties legitimacy to actuarial thresholds and
 *   permits only targeted mandates). Those are separate constraints, not
 *   measured here; this story's epsilon, beneficiary/victim structure, and
 *   type are authored as clean and stable for this reading alone, per the
 *   epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - public_health_bureaucracy: primary agenda_setter/beneficiary — administers mandate policy, gains durable authority
 *   - unvaccinated_individuals: primary payer — reframed as externality-producing class, loses access
 *   - immunocompromised_populations: dependent beneficiary — cannot self-protect, relies on compliance of others
 *   - courts_and_legislatures: analytical observer — adjudicates proportionality of police power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.58).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.72).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Public Health Primacy Reading of Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, '08cfae5d-5d97-4d0a-8f54-3bd65146c6d5').
narrative_ontology:cs_kernel_codification('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5', distributed).
narrative_ontology:cs_authority_grounding('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5', extraction).
narrative_ontology:cs_interpretation_layer_present('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5').
narrative_ontology:cs_reading_relation('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5', foundational, collective_harm_duty_overrides_individual_medical_sovereignty).
narrative_ontology:cs_axiom_status(collective_harm_duty_overrides_individual_medical_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5', collective_harm_duty_overrides_individual_medical_sovereignty, instrumental).
narrative_ontology:cs_axiom('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5', foundational, unvaccinated_status_constitutes_actionable_externality_regardless_of_individual_risk).
narrative_ontology:cs_axiom_status(unvaccinated_status_constitutes_actionable_externality_regardless_of_individual_risk, holdable).
narrative_ontology:cs_axiom_grounding('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5', unvaccinated_status_constitutes_actionable_externality_regardless_of_individual_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5', jacobson_police_power_precedent).
narrative_ontology:cs_drift_state('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5', post_covid19_mandate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('08cfae5d-5d97-4d0a-8f54-3bd65146c6d5', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, religious_exemption_seekers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_injury_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_and_institutions).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, state_police_power_over_communicable_disease).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__public_health_primacy_reading, collective_harm_externality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers mandate policy, defines what counts as an acceptable exemption, and enforces compliance through licensing, employment conditions, and access restrictions. Gains durable emergency-powers precedent and expanded jurisdiction over individual medical decisions each time a mandate is upheld or unchallenged.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, beneficiary).

% Receives reduced transmission risk and social/legal normalcy (access to workplaces, schools, travel) as a direct product of high compliance among others. Bears little direct enforcement cost since they already comply; benefits from the externality framing that shifts blame for outbreaks onto the unvaccinated.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population, beneficiary,
    organized, biographical, mobile, national).

% Cannot vaccinate or mount full immune response themselves; depend entirely on herd-level compliance for protection. Have no independent means of enforcing the collective behavior they need and are structurally dependent on the mandate apparatus succeeding on their behalf.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Lose access to employment, education, travel, or public spaces depending on jurisdiction and sector. Are structurally reframed from autonomous medical decision-makers into a named externality-producing class whose bodily status is treated as a transmissible risk to be priced or excluded, regardless of individual risk profile, prior infection, or actual transmission behavior.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Seek exemption on sincerely held belief grounds but face increasingly narrow review standards as the bureaucracy tightens exemption criteria to preserve compliance rates. Often lack legal resources to litigate denial of exemption and face the same access losses as unvaccinated individuals without recourse.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, religious_exemption_seekers, payer,
    powerless, biographical, trapped, national).

% Report adverse reactions and seek their concerns weighed against mandate benefits, but are institutionally treated as noise or misinformation vectors rather than a legitimate risk-bearing population; their testimony rarely enters the policy record on equal footing with epidemiological modeling.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_injury_dissenters, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_injury_dissenters, excluded).

% Implement and enforce mandates as a condition of employment or enrollment under legal and liability pressure from the state and public health guidance, absorbing the administrative and legal cost of enforcement while having little say over the underlying policy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_and_institutions, agenda_setter,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, employers_and_institutions, payer).

% Adjudicate the scope of state police power against individual liberty claims, drawing on precedent (Jacobson v. Massachusetts and its descendants) to determine how far the collective-harm justification extends before it becomes disproportionate.
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
% COORDINATION_FUNCTION: Achieving population-level immunity thresholds that protect those who cannot vaccinate (infants, immunocompromised, allergic) requires near-universal compliance; individual vaccination decisions have genuine externalities on transmission dynamics that a purely voluntary system may undersupply.
% TRANSFER_FUNCTION: Moves compliance burden, bodily autonomy, and exit options from unvaccinated individuals to the vaccinated majority and the state; moves epistemic authority over what counts as an acceptable medical decision from the individual to the public health bureaucracy.
% ABSENT_VOICES: Vaccine injury dissenters and religious exemption seekers would argue that individual risk-benefit calculus and sincerely held belief deserve weight against population-level externality framing, but exemption review boards and public health messaging structurally exclude this testimony from the policy record as illegitimate or dangerous.
% DISAPPEARANCE_RATIONALE: If mandate authority vanished overnight, compliance would depend entirely on voluntary uptake and employer/insurer incentive structures; immunocompromised populations would lose their primary structural protection, vaccinated populations would lose the externality-based justification for excluding unvaccinated people from shared spaces, and the public health bureaucracy would lose a major lever of enforcement authority built during outbreak periods.
% FOUNDING_PROBLEM: Communicable disease outbreaks (smallpox historically, COVID-19 more recently) produced population-level harm that voluntary individual choice alone did not adequately prevent, creating pressure for state authority to compel or condition participation in vaccination to protect the collective, including those who cannot protect themselves.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and immunocompromised-advocacy groups outside the enforcing bureaucracy corroborate that herd-level protection thresholds are real and that below-threshold compliance produces measurable outbreak risk to vulnerable groups. However, independent legal scholars and civil-liberties organizations outside the bureaucracy also attest that mandate scope has in several instances outpaced the actuarial risk that would justify blanket (as opposed to targeted) authority, suggesting the founding problem has been used to justify broader jurisdiction than the underlying externality strictly requires.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects genuine but partial coordination benefit set against substantial cost transfer onto a specifically-named class (the unvaccinated) whose bodily autonomy is subordinated to an externality framing that does not distinguish individual transmission risk, prior immunity, or actual behavior. Suppression (0.72) is high and rising over the measured interval because enforcement (employment conditions, access restriction, narrowing exemption review) intensified as compliance targets were pursued, not because the underlying disease threat necessarily grew proportionally. Theater ratio (0.28) is moderate-low: the coordination function (herd protection of the immunocompromised) is real, but a growing share of enforcement activity (exemption review tightening, access gatekeeping in low-transmission contexts) serves compliance-maintenance rather than the marginal epidemiological benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health bureaucracy and employers/institutions sit at the agenda-setting end: they design and enforce the policy and, in the bureaucracy's case, accrue durable jurisdictional authority from its persistence. Vaccinated and immunocompromised populations are beneficiaries with different exposure: the immunocompromised are trapped-dependent beneficiaries (they cannot generate the protection themselves), while the vaccinated are mobile beneficiaries who mostly free-ride on already-complying status. Unvaccinated individuals, religious exemption seekers, and vaccine injury dissenters are payers with constrained-to-trapped exit: their bodily status itself is the object of the constraint, and exit requires either compliance (surrendering the autonomy claim) or accepting exclusion from employment, education, or public life.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) preserves the fact that the coordination function is real and not merely cover: immunocompromised populations genuinely depend on herd-level compliance, and this is not a manufactured beneficiary class. But the classification also refuses to let that genuine coordination function launder the disproportionate, hardening enforcement documented in the suppression_requirement series — a pure rope reading would erase the victim class entirely, while a pure snare reading would deny the constraint any real protective function. The founding_problem_status of 'contested' with independent corroboration on both sides is the mechanism that prevents this reading from being mistaken for settled: the epidemiological case for compliance thresholds is corroborated outside the bureaucracy, but so is the claim that mandate scope has exceeded actuarial necessity in specific applications — which is exactly the terrain the risk_stratification_reading occupies as a separate constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_uniformity_ambiguity,
    'Is unvaccinated status genuinely a uniform externality across all individuals, or does treating it as uniform collapse meaningful differences in actual transmission risk (prior infection, individual immune response, exposure context) that a risk-stratified approach would preserve?',
    'Comparative epidemiological analysis of outcomes under blanket mandate regimes versus risk-stratified regimes across comparable jurisdictions and pathogens.',
    'If the externality is genuinely uniform, the public-health-primacy framing is descriptively accurate and the high suppression is proportionate to actual collective risk. If transmission risk varies substantially by individual factors, the blanket externality framing overstates uniformity to justify broader authority than the underlying risk requires — supporting the risk_stratification_reading''s proportionality critique as the more accurate structural account for many cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_uniformity_ambiguity, empirical, 'Whether uniform externality framing accurately captures heterogeneous individual transmission risk.').

omega_variable(
    bureaucratic_authority_accumulation,
    'Does the public health bureaucracy''s expanded jurisdictional authority persist and generalize beyond the specific disease threat that justified it, functioning as a durable institutional gain independent of epidemiological necessity?',
    'Track whether mandate-adjacent authority (exemption review standards, emergency-powers precedent, access-conditioning infrastructure) is retained, narrowed, or repealed once the triggering outbreak''s severity declines.',
    'If authority persists past the epidemiological trigger, this corroborates the beneficiary declaration for public_health_bureaucracy as a genuine institutional gain rather than a pass-through of protective function — reinforcing the tangled_rope reading over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bureaucratic_authority_accumulation, empirical, 'Whether institutional authority gained under mandate justification outlives the triggering health threat.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that all three kernel readings (bodily_autonomy_primacy, public_health_primacy, risk_stratification) are simultaneously live in contemporary legal and political discourse, is there a principled basis for treating this reading as the operative one for a given jurisdiction, or does the selection itself depend on unstated political priors?',
    'Cross-jurisdictional survey of which reading courts and legislatures actually apply, and whether the pattern of selection correlates with independent variables (disease severity, prior civil liberties jurisprudence) or with contested political alignment.',
    'If reading selection tracks disease severity and actuarial evidence, the risk_stratification_reading may be the more defensible default and public_health_primacy the exception; if selection tracks political alignment independent of evidence, all three readings persist as genuinely coexisting contested positions rather than one being more ''correct.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether selection among kernel readings is evidence-driven or politically contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(vacc_tr_t32, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(vacc_tr_t40, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(vacc_tr_t48, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 48, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(vacc_be_t32, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(vacc_be_t40, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(vacc_be_t48, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 48, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(vacc_su_t32, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(vacc_su_t40, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(vacc_su_t48, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 48, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the vaccine_mandate_legitimacy kernel. bodily_autonomy_primacy_reading treats any state coercion as categorically impermissible and forecloses this reading's conditional-autonomy premise. risk_stratification_reading shares this reading's acceptance of state authority in principle but ties legitimacy to actuarial proportionality, producing a narrower victim set (only those in high-risk-transmission contexts) and a lower suppression profile for low-risk individuals. Each reading carries its own epsilon, beneficiary/victim declarations, and classification; they are not measurement variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__public_health_primacy_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
