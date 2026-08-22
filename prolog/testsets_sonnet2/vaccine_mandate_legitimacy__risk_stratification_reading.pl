% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__risk_stratification_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Proportionality-Gated Vaccine Mandate Authority (Risk Stratification Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This story instantiates the risk-stratification reading of the
 *   vaccine-mandate-legitimacy kernel: mandate legitimacy is contingent on
 *   satisfying an actuarial risk threshold, so that blanket, population-wide
 *   mandates fail proportionality review while narrowly targeted mandates
 *   (calibrated to occupational or clinical exposure risk) remain
 *   permissible. This is a distinct constraint from the public-health-primacy
 *   reading (which treats unvaccinated status itself as an externality
 *   justifying broad mandate authority regardless of individual risk tier)
 *   and the bodily-autonomy-primacy reading (which treats state vaccination
 *   coercion as categorically impermissible regardless of any threshold).
 *   Under this reading, ε is moderate and rising slowly: the coordination
 *   function (protecting the genuinely vulnerable via correctly targeted
 *   mandates) is real, but the same threshold-drawing mechanism that
 *   legitimizes narrow mandates also creates a victim class whenever the
 *   threshold is drawn too coarsely — administrative convenience substituting
 *   for actuarial precision. The victim set's size is structurally sensitive
 *   to how finely the threshold is drawn, which is the expected structural
 *   delta for this reading.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda-setter that defines and defends the actuarial threshold
 *   - high_risk_occupational_cohorts and immunocompromised_patients_in_regulated_settings: genuine beneficiaries of correctly targeted mandates
 *   - low_risk_workers_swept_into_overbroad_mandates, occupational_groups_misclassified_as_high_risk, individuals_denied_individualized_risk_review: victims created when threshold-drawing is too coarse or stale
 *   - courts_and_reviewing_bodies: analytical seat that adjudicates proportionality and converts the reading into enforceable doctrine
 *   - employers_and_institutions_administering_mandates: intermediate agenda-setter/payer bearing implementation and litigation exposure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.42).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.38).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Proportionality-Gated Vaccine Mandate Authority (Risk Stratification Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '7271c777-b43f-4858-9ca4-d7bf95f31824').
narrative_ontology:cs_kernel_codification('7271c777-b43f-4858-9ca4-d7bf95f31824', distributed).
narrative_ontology:cs_authority_grounding('7271c777-b43f-4858-9ca4-d7bf95f31824', practice).
narrative_ontology:cs_interpretation_layer_present('7271c777-b43f-4858-9ca4-d7bf95f31824').
narrative_ontology:cs_reading_relation('7271c777-b43f-4858-9ca4-d7bf95f31824', vaccine_mandate_legitimacy__public_health_primacy_reading, influences).
narrative_ontology:cs_reading_relation('7271c777-b43f-4858-9ca4-d7bf95f31824', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, influences).
narrative_ontology:cs_axiom('7271c777-b43f-4858-9ca4-d7bf95f31824', foundational, mandate_legitimacy_requires_proportionality_to_actuarial_risk).
narrative_ontology:cs_axiom_status(mandate_legitimacy_requires_proportionality_to_actuarial_risk, holdable).
narrative_ontology:cs_axiom_grounding('7271c777-b43f-4858-9ca4-d7bf95f31824', mandate_legitimacy_requires_proportionality_to_actuarial_risk, empirically_contingent).
narrative_ontology:cs_axiom('7271c777-b43f-4858-9ca4-d7bf95f31824', secondary, categorical_mandate_authority_without_threshold_is_impermissible).
narrative_ontology:cs_axiom_status(categorical_mandate_authority_without_threshold_is_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('7271c777-b43f-4858-9ca4-d7bf95f31824', categorical_mandate_authority_without_threshold_is_impermissible, instrumental).
narrative_ontology:cs_reference_frame('7271c777-b43f-4858-9ca4-d7bf95f31824', least_restrictive_means_public_health_jurisprudence).
narrative_ontology:cs_drift_state('7271c777-b43f-4858-9ca4-d7bf95f31824', post_pandemic_threshold_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7271c777-b43f-4858-9ca4-d7bf95f31824', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_occupational_cohorts).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_patients_in_regulated_settings).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_agencies).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_workers_swept_into_overbroad_mandates).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, occupational_groups_misclassified_as_high_risk).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_denied_individualized_risk_review).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, employers_and_institutions_administering_mandates).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, proportionality_doctrine_in_public_health_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and defend the actuarial threshold that determines which occupational or clinical settings qualify for a mandate. They gain institutional legitimacy and epidemic control tools when courts accept the threshold as proportionate, and bear reputational and legal cost when a threshold is later struck down as either too broad or drawn in bad faith.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_agencies, beneficiary).

% Work in settings with documented elevated transmission or severe-outcome risk (ICU staff, long-term care workers). A targeted mandate calibrated to their actual risk profile provides real protection and legal cover for workplace safety demands; their exit from the sector is costly but the mandate itself does not misclassify them.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_occupational_cohorts, beneficiary,
    moderate, biographical, constrained, regional).

% Depend on the vaccination status of the staff and visitors around them because their own immune response to vaccination may be inadequate. A properly targeted mandate on their care environment is a coordination good they cannot secure any other way; they have no exit from needing care.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_patients_in_regulated_settings, beneficiary,
    powerless, biographical, trapped, local).

% Work in settings with actuarially low transmission or severe-outcome risk but are captured by a mandate drawn at institution-wide or occupation-wide granularity rather than at the granularity their individual risk profile would justify. Under this reading their inclusion is the proportionality failure the reading exists to name; their recourse is litigation or job loss.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_workers_swept_into_overbroad_mandates, payer,
    moderate, biographical, constrained, national).

% Are classified into a mandate-covered risk tier by a threshold definition that may reflect political convenience, administrative ease, or stale epidemiological data rather than current actuarial reality. Their remedy under this reading is to contest the threshold's evidentiary basis, not the mandate power itself.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, occupational_groups_misclassified_as_high_risk, payer,
    moderate, biographical, constrained, regional).

% Have an individual risk profile (prior infection, medical contraindication, isolated work environment) that would place them below the actuarial threshold if assessed individually, but the mandate is administered at a categorical rather than individualized level. They are the clearest case this reading identifies as illegitimate extraction dressed as proportionate policy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_denied_individualized_risk_review, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_denied_individualized_risk_review, excluded).

% Adjudicate whether a given mandate's threshold and scope satisfy proportionality: least-restrictive-means analysis, evidentiary basis of the risk classification, and availability of individualized exemption pathways. Their rulings are what convert this reading from a normative claim into enforceable doctrine.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, courts_and_reviewing_bodies, observer,
    institutional, generational, analytical, national).

% Implement whatever threshold public health guidance and law set, bearing compliance and litigation costs for both over-inclusion (challenged mandates) and under-inclusion (liability if an outbreak occurs in an under-mandated setting). They have limited ability to set the threshold themselves but full exposure to getting it wrong in either direction.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, employers_and_institutions_administering_mandates, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, employers_and_institutions_administering_mandates, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Calibrating mandate scope to actual, tiered risk data solves a genuine problem that blanket mandates and pure voluntarism both fail to solve: concentrating a coercive tool only where the marginal protection against severe collective harm is actuarially justified, while avoiding uncompensated imposition on populations whose individual or occupational risk does not warrant it.
% TRANSFER_FUNCTION: Moves the burden of bodily intervention and compliance cost from the population as a whole onto the specific subset an actuarial threshold identifies as high-risk-transmitting or high-risk-exposed, while moving protective benefit toward the immunocompromised and vulnerable populations those settings serve. Where the threshold is drawn too broadly or on stale data, it additionally transfers cost onto misclassified individuals and occupational groups without a corresponding benefit transfer.
% ABSENT_VOICES: Individuals with idiosyncratic risk-reducing facts (prior infection, remote work, medical contraindication) rarely get an individualized hearing before mandate imposition — categorical administration is cheaper than case-by-case review, so their objection is structurally unheard until litigation, if ever.
% DISAPPEARANCE_RATIONALE: If the proportionality doctrine that ties mandate legitimacy to an actuarial threshold disappeared, mandate authority would collapse toward one of the sibling readings' extremes — either unconditional public-health-primacy mandates or unconditional bodily-autonomy vetoes — and the courts, agencies, and employers currently operating a middle-tier compliance architecture (risk tiers, exemption pathways, sunset reviews) would have no doctrinal basis for that architecture and would have to rebuild it from whichever extreme prevailed.
% FOUNDING_PROBLEM: Courts and public health law needed a doctrine that could distinguish a genuinely necessary, narrowly tailored mandate (ICU staff during a virulent respiratory pandemic) from an administratively convenient but overbroad one (an entire municipal workforce regardless of transmission risk), because neither absolute deference to public health authority nor absolute bodily-autonomy veto could account for the empirical variance in actual risk across settings.
% FOUNDING_PROBLEM_CORROBORATION: Public health law scholars and several appellate courts attest the proportionality/threshold problem remains live — actuarial risk genuinely varies by occupational setting and mandate scope has repeatedly been litigated on exactly that variance. Civil liberties advocates outside the public-health-agency seat corroborate that the problem is real but argue current threshold-setting is frequently captured by administrative convenience rather than genuine actuarial rigor, meaning the doctrine's function is only partially live in practice.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).
:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) sits meaningfully below what a blanket-mandate reading would score, because the coordination function — protecting genuinely high-risk settings — is real and substantial under this reading; the residual extraction comes entirely from threshold misclassification, not from the mandate mechanism itself. Suppression (0.38) is moderate: enforcement exists (exclusion from employment, licensure conditions) but is narrower in scope than a population-wide mandate's enforcement apparatus would require. Accessibility collapse (0.35) is comparatively low because individualized exemption pathways are doctrinally required under this reading, even though administratively under-supplied. Resistance (0.55) is elevated because the threshold itself is a contestable, litigable object — every misclassified group has legal standing this reading affirmatively creates.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (public health agencies), the arrangement reads as principled, evidence-constrained coordination — a doctrine that legitimizes exactly the mandates that are actuarially justified and no more. From the misclassified-victim seats, the identical threshold-drawing apparatus reads as extraction dressed in the language of proportionality: the same administrative discretion that could draw the line precisely instead draws it at whatever granularity is cheapest to administer. The engine should register genuine seat divergence here because the coordination and extraction functions run through the identical mechanism (the threshold), which is the tangled-rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   High-risk occupational cohorts and immunocompromised patients in regulated settings are beneficiaries: the mandate, correctly targeted, subsidizes their safety at a cost they either share proportionately or do not bear directly. The three victim groups are targets whose d sits high: they bear the compliance/exclusion cost of a mandate calibrated to a risk tier they do not actually occupy, with constrained or trapped exit (job dependency, care dependency, lack of individualized review pathway). Public health agencies and employers are structurally mixed — agenda-setting power paired with real legal and reputational exposure when the threshold is later invalidated.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is precisely the doctrinal tool that prevents blanket mislabeling in either direction: it blocks the public-health-primacy reading from certifying every mandate as legitimate regardless of actual risk distribution, and it blocks the bodily-autonomy-primacy reading from certifying every mandate as illegitimate regardless of actual risk distribution. Its own mandatrophy risk is different: the threshold-setting apparatus can outlive the epidemiological moment that justified it (founding_problem_status: contested) if agencies continue applying yesterday's risk tiers to today's altered transmission dynamics — at that point the coordination function has died even though the enforcement machinery persists, which is exactly the founding_problem_status × disappearance_verdict mismatch the R5 interview is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_granularity_collapse_risk,
    'At what actuarial threshold does the risk-stratification reading functionally collapse into one of its sibling readings — either into public-health-primacy (threshold set so low that nearly all settings qualify) or into bodily-autonomy-primacy (threshold set so high that virtually no setting qualifies)?',
    'Track the actual threshold values agencies and courts adopt over time against total population coverage; a threshold trend approaching 0% or 100% of the working population would evidence collapse toward a sibling reading rather than genuine stratification.',
    'If the threshold is administratively set at either extreme, this reading is not structurally distinct from a sibling in practice, even though it remains doctrinally distinct on paper — the victim set size (the expected structural delta) is the diagnostic signal of which regime is actually operating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_granularity_collapse_risk, conceptual, 'Whether risk-stratification is a stable third position or an unstable midpoint that collapses toward one kernel extreme depending on threshold-setting practice.').

omega_variable(
    threshold_evidentiary_basis,
    'Is the actuarial threshold set from current, setting-specific epidemiological data, or from stale/generic risk categories applied by administrative convenience?',
    'Compare the epidemiological data cited in agency guidance and court records at the time a given mandate''s threshold was set against contemporaneous transmission and severe-outcome data for the specific setting.',
    'A threshold grounded in current setting-specific data supports a genuine coordination reading (low ε, low victim count); a threshold grounded in stale or generic categories supports a captured-doctrine reading (extraction dressed as proportionality, larger victim count) — this directly determines the size of the victim classes authored in this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_evidentiary_basis, empirical, 'Whether the threshold-drawing mechanism is evidentiarily rigorous or administratively convenient.').

omega_variable(
    individualized_review_availability,
    'Do individuals excluded by categorical threshold application have a real, accessible pathway to individualized risk review, or is the exemption pathway nominal?',
    'Audit exemption request approval rates, processing times, and appeal outcomes across jurisdictions administering targeted mandates under this doctrine.',
    'A nominal or inaccessible individualized review pathway would mean the doctrine''s stated proportionality safeguard is theater rather than function, raising the effective theater_ratio and reclassifying part of the measured coordination as performative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individualized_review_availability, empirical, 'Whether the individualized-review safeguard this reading requires is substantively available or merely nominal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, bodily_autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% Three constraints decompose the vaccine_mandate_legitimacy kernel: public_health_primacy_reading (broad state mandate authority, near-zero self-conceived victim set), bodily_autonomy_primacy_reading (categorical rejection of mandate coercion, treats any mandate as an unconditional victim-creating snare), and this story, risk_stratification_reading (mandate legitimacy contingent on an actuarial threshold — a tangled rope whose victim set size is a direct function of threshold granularity). Each carries its own ε and its own claimed_type; none averages over the others. This reading's threshold apparatus creates downstream pressure on both extremes: it constrains public_health_primacy's claim to unconditional authority by requiring proportionality, and it constrains bodily_autonomy_primacy's claim to categorical rejection by legitimizing narrowly-targeted mandates — hence 'influences' rather than 'forecloses' or mere 'coexists_with' in the reading_relations below.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
