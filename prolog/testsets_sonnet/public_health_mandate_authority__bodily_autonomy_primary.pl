% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate as Categorical Bodily-Sovereignty Violation (Bodily-Autonomy-Primary Reading)
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primary reading of the
 *   public_health_mandate_authority kernel: mandate authority is a
 *   categorical violation of bodily sovereignty, and no magnitude of
 *   collective benefit can license non-consensual medical intervention. Under
 *   this reading, the unvaccinated, medical exemption seekers, and religious
 *   objectors form the victim set because coercive exclusion from employment,
 *   education, or public life is treated as coercion harm regardless of the
 *   epidemiological stakes. The immunocompromised population is structurally
 *   EXCLUDED from the victim set here — their vulnerability generates no
 *   claim against another person's body under this reading's own logic, a
 *   sharp reversal from the sibling public_health_primary reading where they
 *   anchor the coordination function. Public-health-primary advocates bear
 *   zero extractiveness under this reading: no coercion is imposed on them by
 *   mandate authority existing or not; their exclusion is normative (their
 *   argument doesn't count), not material (they pay no cost).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.78).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate as Categorical Bodily-Sovereignty Violation (Bodily-Autonomy-Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, 'f760e3f0-ab39-45d8-bed3-4e40b7be7fc7').
narrative_ontology:cs_kernel_codification('f760e3f0-ab39-45d8-bed3-4e40b7be7fc7', distributed).
narrative_ontology:cs_authority_grounding('f760e3f0-ab39-45d8-bed3-4e40b7be7fc7', distributed).
narrative_ontology:cs_reading_relation('f760e3f0-ab39-45d8-bed3-4e40b7be7fc7', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('f760e3f0-ab39-45d8-bed3-4e40b7be7fc7', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('f760e3f0-ab39-45d8-bed3-4e40b7be7fc7', foundational, bodily_sovereignty_is_categorical_trump).
narrative_ontology:cs_axiom_status(bodily_sovereignty_is_categorical_trump, holdable).
narrative_ontology:cs_axiom_grounding('f760e3f0-ab39-45d8-bed3-4e40b7be7fc7', bodily_sovereignty_is_categorical_trump, deontological).
narrative_ontology:cs_axiom('f760e3f0-ab39-45d8-bed3-4e40b7be7fc7', foundational, no_collective_magnitude_licenses_bodily_invasion).
narrative_ontology:cs_axiom_status(no_collective_magnitude_licenses_bodily_invasion, holdable).
narrative_ontology:cs_axiom_grounding('f760e3f0-ab39-45d8-bed3-4e40b7be7fc7', no_collective_magnitude_licenses_bodily_invasion, deontological).
narrative_ontology:cs_reference_frame('f760e3f0-ab39-45d8-bed3-4e40b7be7fc7', individual_sovereign_body_doctrine).
narrative_ontology:cs_drift_state('f760e3f0-ab39-45d8-bed3-4e40b7be7fc7', contemporary_mandate_enforcement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f760e3f0-ab39-45d8-bed3-4e40b7be7fc7', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, employers_requiring_compliance).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, vaccine_manufacturers).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, medical_exemption_seekers).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, religious_objectors).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, state_police_power_over_bodies).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, collective_welfare_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues mandates conditioning employment, school attendance, travel, or public accommodation on accepting a medical intervention. Frames the requirement as necessary for population-level disease control and enforces it through licensing, employment law, and institutional gatekeeping. Bears none of the physical risk of the intervention itself.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Adopts and enforces the mandate as a condition of employment, shifting liability and public-health compliance burden onto workers. Faces regulatory and reputational risk for non-compliance but no bodily risk from the underlying intervention; can lobby for or against enforcement intensity.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, employers_requiring_compliance, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__bodily_autonomy_primary, employers_requiring_compliance, beneficiary).

% Receives guaranteed demand and liability shielding when a mandate is in force. Has no bodily stake in the constraint and profits directly from expanded uptake mandates create; can relocate production or lobbying focus across jurisdictions.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, vaccine_manufacturers, beneficiary,
    organized, biographical, arbitrage, global).

% Faces loss of employment, education access, or public participation for declining a non-consensual medical intervention on grounds of personal bodily sovereignty. Exit requires forfeiting livelihood, relocating to a jurisdiction without the mandate, or submitting under duress — none of which is a free choice from where they stand.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals, payer,
    powerless, biographical, trapped, local).

% Holds a documented medical contraindication but must navigate an adjudicated exemption process controlled by the same agenda-setting institutions; denial or bureaucratic delay forces the same coerced choice as for the unvaccinated generally, despite an acknowledged physical risk from compliance.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, medical_exemption_seekers, payer,
    powerless, biographical, trapped, local).

% Objects to the intervention on sincerely held religious grounds; exemption processes are frequently narrowed or eliminated during enforcement, leaving the same coercive choice structure as for other objectors.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, religious_objectors, payer,
    powerless, biographical, trapped, local).

% Under this reading, this group is EXCLUDED from the victim set: the constraint recognizes no duty to protect them that would license invading another person's body. They remain structurally vulnerable to community transmission, but this reading holds their vulnerability cannot generate a claim on someone else's bodily autonomy — a sharp structural delta from the public-health-primary reading, where they anchor the coordination function.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_patients, excluded,
    powerless, biographical, trapped, local).

% Argues the mandate is a justified collective-action mechanism protecting the vulnerable commons. Under this reading their position is treated as normatively foreclosed rather than merely disagreed with — no coercive cost is imposed ON them by this reading (their extraction is zero), but their claim carries no weight in the constraint's own logic; they are structurally excluded from the deliberation this reading recognizes as legitimate.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, excluded,
    organized, generational, analytical, national).

% Adjudicates challenges to mandates on constitutional and bioethical grounds, weighing bodily autonomy claims against state police-power arguments. Produces the record from which the reading's own legitimacy is contested and revised.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, bioethics_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized under this reading: the mandate's asserted coordination purpose (reducing disease transmission at population scale) is treated as incapable of generating legitimate authority over an individual's body, so no coordination function is credited to the arrangement — only the transfer function below is acknowledged as real.
% TRANSFER_FUNCTION: Moves bodily control from the individual to the state/employer apparatus, backed by the threat of exclusion from employment, education, or public life; in exchange the mandating institutions and manufacturers receive compliance, liability protection, and guaranteed uptake.
% ABSENT_VOICES: Under this reading, public-health-primary advocates and the immunocompromised population are the ones effectively absent from the deliberation that matters: their claims of collective benefit or vulnerability are treated as insufficient in principle to authorize bodily invasion, so they are heard but structurally cannot prevail.
% DISAPPEARANCE_RATIONALE: If mandate authority vanished overnight, unvaccinated individuals, medical exemption seekers, and religious objectors would regain unconditional access to employment, education, and public accommodation; enforcement bureaucracies built around compliance verification would dissolve; manufacturers would lose mandate-guaranteed demand. The arrangement's removal materially changes who can work, travel, and participate in public life.
% FOUNDING_PROBLEM: Historically, mandates were built to solve contagious-disease outbreaks threatening population-level mortality and overwhelmed healthcare capacity, using compulsion as a last-resort tool when voluntary uptake was judged insufficient.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and employers attest the founding problem remains live (ongoing transmission risk, healthcare capacity fragility). Civil liberties litigators, bioethicists writing outside public-health institutions, and constitutional scholars — sources outside the beneficiary set — attest that under a bodily-sovereignty framework the problem was never one susceptible to a compulsion-based solution in the first place, since no magnitude of collective threat can, on this reading, license non-consensual bodily intervention; the disagreement is over whether the problem could ever justify the tool, not merely whether it is still live.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steeply over the interval (0.35 to 0.78) as enforcement mechanisms (employment conditions, access restrictions) matured and hardened; suppression tracks closely, plateauing once mandate enforcement infrastructure reached institutional steady-state. Theater ratio stays comparatively low (0.28) because, on this reading, the enforcement apparatus is doing real coercive work rather than performing it — the harm is treated as substantive, not symbolic.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the mandate looks like ordinary regulatory administration of a public good. From the payer seats, the identical structure is a coercive apparatus overriding a categorical right. The engine computes this divergence from the declared beneficiary/victim/exit structure; this reading deliberately routes ALL classificatory weight to the payer seats' experience, which is the defining feature that distinguishes it from the sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies, employers, and manufacturers sit near the beneficiary end: they administer, enforce, or profit from the mandate and bear no bodily risk. Unvaccinated individuals, exemption seekers, and religious objectors sit near the full-target end: trapped exit, powerless standing, and the mandate's coercive machinery aimed directly at their bodily choices. Immunocompromised patients and public-health-primary advocates are excluded from the extraction calculus entirely under this reading — their stakes are acknowledged narratively but denied normative purchase.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (outbreak containment) is contested as either still live (agencies) or as never having been the kind of problem compulsion could legitimately solve (this reading's own framework). Because this reading denies in principle that any founding problem could authorize non-consensual bodily intervention, mandatrophy here is not a question of whether the function has atrophied — the function was never a legitimate function under this reading's axioms, so persistence of enforcement is read as pure extraction growth, visible in the rising extractiveness/suppression series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_bodily_autonomy,
    'Is bodily-autonomy-primary the correct reading of mandate authority''s legitimacy, or does it improperly foreclose the collective-welfare and proportionality readings that a plural liberal framework would hold simultaneously?',
    'Comparative constitutional analysis across jurisdictions that have adjudicated mandate challenges under strict scrutiny (bodily autonomy as fundamental right) versus rational basis / Jacobson-style deference (collective welfare authority) versus tiered proportionality tests; track which framework predominates in appellate outcomes over time.',
    'If courts converge on proportionality as the dominant workable framework, this reading''s categorical claim is empirically marginalized even where philosophically maintained; if strict bodily-autonomy scrutiny predominates, this reading''s structural delta (exclusion of immunocompromised from victim set, zero extraction on advocates) becomes the operative legal reality rather than one contested position among three.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_bodily_autonomy, conceptual, 'Whether the bodily-autonomy-primary reading is the correct or merely one live reading of the mandate-authority kernel.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does this reading''s categorical claim (no collective benefit ever justifies non-consensual intervention) logically foreclose the public_health_primary reading within a single legal framework, or can both persist as competing doctrines across different jurisdictions/courts?',
    'Track whether any single jurisdiction''s case law simultaneously upholds both a categorical bodily-autonomy right AND a collective-welfare mandate power without one displacing the other in adjudicated conflicts; persistence of both as live doctrine within one system would indicate coexistence rather than foreclosure.',
    'If foreclosure is correct, this reading and public_health_primary cannot both be law within the same system, making their relationship a genuine either/or; if coexistence is correct, courts are managing an unresolved doctrinal tension rather than one reading defeating the other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether bodily-autonomy-primary structurally forecloses or merely coexists with public-health-primary within a single legal system.').

omega_variable(
    immunocompromised_exclusion_moral_status,
    'Is the exclusion of immunocompromised patients from the victim set a defensible consequence of a principled bodily-sovereignty framework, or does it represent a blind spot this reading''s advocates would themselves reject if confronted with concrete harm cases?',
    'Survey bodily-autonomy-primary advocates directly on hypothetical cases where an unvaccinated individual''s choice causes a documented death of an immunocompromised third party; track whether the categorical claim holds or admits exceptions under concrete stakes.',
    'If the categorical claim holds under concrete stakes, the reading is genuinely principled rather than convenient; if advocates carve exceptions, the reading''s own framework is less stable than its stated axioms suggest, weakening the foreclosure claim against proportionality_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immunocompromised_exclusion_moral_status, preference, 'Whether excluding immunocompromised patients from the victim set survives contact with concrete third-party-harm cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t8, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 8, 0.14).
narrative_ontology:measurement(publ_tr_t16, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 16, 0.18).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 24, 0.22).
narrative_ontology:measurement(publ_tr_t32, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 32, 0.25).
narrative_ontology:measurement(publ_tr_t40, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 40, 0.27).
narrative_ontology:measurement(publ_tr_t48, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 48, 0.28).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(publ_be_t8, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(publ_be_t16, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(publ_be_t32, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 32, 0.76).
narrative_ontology:measurement(publ_be_t40, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(publ_be_t48, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 48, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(publ_su_t8, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(publ_su_t16, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(publ_su_t32, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(publ_su_t40, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(publ_su_t48, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 48, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the public_health_mandate_authority kernel. public_health_mandate_authority__public_health_primary reads the identical mandate structure as a collective-action Tangled Rope or Rope protecting the vulnerable commons, with the immunocompromised as beneficiaries/coordinated parties rather than excluded. public_health_mandate_authority__proportionality_reading reads legitimacy as a sliding scale rather than a categorical bar, producing intermediate ε depending on threat severity and duration. All three share the same underlying mandate mechanism but diverge sharply in claimed_type, victim/beneficiary sets, and ε because they instantiate different normative kernels for adjudicating the same coercive apparatus — per the ε-invariance principle, this divergence is why they are authored as three separate constraint stories rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__bodily_autonomy_primary, organized, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
