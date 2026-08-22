% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Proportionality Reading: Severity-Scaled Coercion Legitimacy Boundary
 *   domain: Public Health Policy / Medical Ethics / Constitutional Law
 *
 * SUMMARY:
 *   This story instantiates the proportionality reading of the coercion
 *   legitimacy boundary kernel: coercion legitimacy is not a fixed yes/no (as
 *   in the public_health_primary and bodily_autonomy_primary readings) but
 *   scales continuously with a pathogen's severity and transmission dynamics.
 *   Measles (R0 ~12-18, meaningful case fatality, no effective treatment)
 *   clears the threshold for mandate-backed coercion; seasonal influenza (R0
 *   ~1.3, low case fatality in healthy populations, treatable) does not. The
 *   reading's structural signature is a variable victim set: whoever is
 *   regulated depends entirely on which pathogen is under discussion, and the
 *   coercion apparatus itself is genuinely inactive for low-severity diseases
 *   (flu_vaccine_refusers face zero enforcement) while genuinely active for
 *   high-severity ones (vaccine_hesitant_parents face real exclusion
 *   machinery for measles).
 *
 * KEY AGENTS:
 *   - public_health_departments: sets the severity/transmissibility threshold and administers case-by-case mandate decisions
 *   - immunocompromised_populations and school_age_children: structural beneficiaries who depend on the threshold being met for high-R0 diseases
 *   - vaccine_hesitant_parents and religious_exemption_seekers: bear the coercive costs when their objected-to pathogen clears the threshold
 *   - flu_vaccine_refusers: the reading's control case — autonomy preserved because the pathogen falls below threshold
 *   - courts_and_legislatures: analytical/adjudicative seat reviewing whether a given threshold determination was proportionate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.42).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.48).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Proportionality Reading: Severity-Scaled Coercion Legitimacy Boundary").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "Public Health Policy / Medical Ethics / Constitutional Law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '0f8a6223-29a0-40de-b559-02ded4e159bd').
narrative_ontology:cs_kernel_codification('0f8a6223-29a0-40de-b559-02ded4e159bd', distributed).
narrative_ontology:cs_authority_grounding('0f8a6223-29a0-40de-b559-02ded4e159bd', expertise).
narrative_ontology:cs_interpretation_layer_present('0f8a6223-29a0-40de-b559-02ded4e159bd').
narrative_ontology:cs_reading_relation('0f8a6223-29a0-40de-b559-02ded4e159bd', coercion_legitimacy_boundary__public_health_primary, influences).
narrative_ontology:cs_reading_relation('0f8a6223-29a0-40de-b559-02ded4e159bd', coercion_legitimacy_boundary__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('0f8a6223-29a0-40de-b559-02ded4e159bd', foundational, severity_gated_coercion_legitimacy).
narrative_ontology:cs_axiom_status(severity_gated_coercion_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0f8a6223-29a0-40de-b559-02ded4e159bd', severity_gated_coercion_legitimacy, instrumental).
narrative_ontology:cs_axiom('0f8a6223-29a0-40de-b559-02ded4e159bd', secondary, transmission_dynamics_are_the_relevant_empirical_input).
narrative_ontology:cs_axiom_status(transmission_dynamics_are_the_relevant_empirical_input, holdable).
narrative_ontology:cs_axiom_grounding('0f8a6223-29a0-40de-b559-02ded4e159bd', transmission_dynamics_are_the_relevant_empirical_input, empirically_contingent).
narrative_ontology:cs_reference_frame('0f8a6223-29a0-40de-b559-02ded4e159bd', jacobson_proportionality_baseline).
narrative_ontology:cs_drift_state('0f8a6223-29a0-40de-b559-02ded4e159bd', post_pandemic_polarization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f8a6223-29a0-40de-b559-02ded4e159bd', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, school_age_children).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_departments).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, general_population_via_herd_immunity).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, vaccine_hesitant_parents).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, religious_exemption_seekers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, low_severity_disease_mandate_targets).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, misclassified_moderate_risk_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, flu_vaccine_refusers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, school_age_children).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, proportionality_doctrine_in_public_health_law).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, least_restrictive_means_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines which pathogens cross the severity/transmissibility threshold that justifies mandates, using R0 estimates, case fatality rates, and outbreak modeling. Administers the case-by-case adjudication machinery — measles triggers school exclusion and mandate enforcement, seasonal flu does not. Bears reputational and legal costs when the threshold-drawing is challenged in court.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_departments, agenda_setter,
    institutional, generational, analytical, national).

% Cannot be vaccinated themselves and depend entirely on herd immunity thresholds being met in their community. Benefit directly when high-R0 diseases like measles trigger mandates for others; have no personal exit from the risk if mandates are relaxed or the threshold is drawn too permissively.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, local).

% Attend schools where mandate enforcement is concentrated; benefit from reduced outbreak risk but also bear the direct compliance burden (or exclusion from schooling) when their guardians resist. Have no independent voice in the threshold-setting process despite being the population most directly regulated.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, school_age_children, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, school_age_children, payer).

% Face school exclusion, fines, or loss of childcare access when refusing mandated vaccines for high-severity diseases. Argue the same coercive apparatus could be turned toward diseases they consider genuinely low-risk, and that the threshold itself is drawn by the same institution that benefits from broad compliance. Can relocate to jurisdictions with looser exemption rules, but at real cost to schooling and employment continuity.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vaccine_hesitant_parents, payer,
    moderate, biographical, constrained, regional).

% Hold sincere objections that predate the severity-scaling framework and do not vary with R0 or case fatality rate. The proportionality reading offers them no principled accommodation — their objection is categorical, but the regime only recognizes severity-scaled exceptions, so they are treated as noncompliant rather than as holding a different premise entirely.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, religious_exemption_seekers, payer,
    powerless, generational, constrained, regional).

% In jurisdictions or outbreak contexts where officials misjudge a pathogen's severity upward (e.g., early pandemic uncertainty, novel variant panic), face mandate-level coercion for what later proves to be a flu-like threat. Their situation reveals the reading's core vulnerability: the threshold is drawn prospectively under uncertainty, and misclassification imposes real costs before correction.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, low_severity_disease_mandate_targets, payer,
    moderate, biographical, mobile, local).

% Refuse seasonal influenza vaccination without consequence under this reading, since flu's R0 and typical case fatality rate fall below the mandate threshold. Their autonomy is fully preserved here — the reading's central claim is validated by their unimpeded exit.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, flu_vaccine_refusers, beneficiary,
    moderate, biographical, mobile, local).

% Adjudicate challenges to specific mandate decisions, applying proportionality and least-restrictive-means tests to determine whether a given pathogen's severity profile justified the coercion imposed. Their rulings retroactively validate or invalidate the threshold-drawing, but do not participate in the initial epidemiological determination.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Calibrates the level of permissible state coercion to the actual epidemiological stakes of a given pathogen, so that mandates track genuine collective-action failure (high transmissibility, high severity, free-rider herd-immunity problems) rather than applying uniformly regardless of threat level.
% TRANSFER_FUNCTION: Moves compliance burden (vaccination, exclusion risk, fines) onto individuals objecting to specific mandates, calibrated by disease severity, while relieving individuals of that burden entirely for low-severity pathogens like seasonal flu — the transfer is intermittent and threshold-gated rather than continuous.
% ABSENT_VOICES: Religious and categorical objectors are structurally unheard by this framework: their objection does not vary with severity, so the proportionality apparatus has no slot for it. They appear only as noncompliant cases, not as holders of a coherent competing premise. Populations in low-information or under-resourced jurisdictions rarely participate in the epidemiological threshold-setting process at all.
% DISAPPEARANCE_RATIONALE: Public health departments and immunocompromised populations would say the world rearranges catastrophically — herd immunity thresholds collapse for high-R0 diseases and outbreak risk rises sharply. Vaccine-hesitant parents and religious objectors would say very little changes for them personally, since the coercive apparatus specifically targeting them would vanish; they read its disappearance as restoration of autonomy, not disruption. The verdict genuinely differs by stakeholder position, which is why it is authored as contested rather than resolved.
% FOUNDING_PROBLEM: Uniform vaccine mandate policy (either mandate everything or coerce nothing) failed to track the actual variance in collective-action severity across pathogens — treating measles and seasonal flu identically either over-coerced for low-stakes diseases or under-protected against high-stakes ones. The proportionality reading was built to calibrate coercion to actual epidemiological risk.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and constitutional scholars outside the public health administrative apparatus (e.g., legal scholars writing on Jacobson v. Massachusetts's proportionality legacy) corroborate that severity-scaling is a coherent and independently defensible doctrine, not merely a post-hoc justification invented by health departments to preserve mandate authority. However, civil liberties organizations dispute that the threshold-drawing process is neutral, arguing the same institutions that benefit from compliance also control the severity determination — no fully independent arbiter exists outside the public health apparatus itself for the initial classification, only for after-the-fact judicial review.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, contested).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).
:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) sits meaningfully below either the public_health_primary reading (which would authorize coercion regardless of severity, producing higher ε) and the bodily_autonomy_primary reading (which would authorize none, producing near-zero ε for any coercive act at all). The moderate value reflects that this reading's coercion is real but genuinely gated — it activates only above a severity threshold, and deactivates below it. Suppression (0.48) is likewise moderate: enforcement exists but only for a subset of diseases, and even within that subset, exemption processes exist. Theater ratio (0.28) is modest and rising slightly — some jurisdictions maintain enforcement infrastructure (school exclusion registries, compliance tracking) for diseases whose actual outbreak risk has declined, a mild proxy-goal drift worth watching but not yet dominant.
 *
 * PERSPECTIVAL GAP:
 *   Public health departments and immunocompromised populations experience the same measles mandate as legitimate, calibrated coordination — the threshold-drawing process is exactly the mechanism that protects them. Vaccine-hesitant parents facing that same mandate experience it as coercion whose 'proportionality' justification is authored by the institution that benefits from compliance. Religious exemption seekers experience it as a category error: the entire severity-scaling apparatus is non-responsive to a categorical objection, so from their seat the reading looks like a more sophisticated version of the public_health_primary reading wearing moderation's clothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (immunocompromised populations, school-age children generally, public health departments as administrators) sit toward the low end of directionality because the arrangement subsidizes their safety without imposing symmetric cost on them. Victims (vaccine_hesitant_parents, religious_exemption_seekers, low_severity_disease_mandate_targets) sit toward the high end because the same structure that protects the beneficiaries extracts compliance or imposes exclusion costs on them. Flu_vaccine_refusers are a genuine low-d case — the reading imposes essentially no cost on them, which is the reading's own evidence for its coherence as calibrated rather than blanket coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading resists the mandatrophy risk that afflicts both blanket readings: public_health_primary risks perpetuating coercive machinery even after a pathogen's threat recedes (mandate outlives function), while bodily_autonomy_primary risks under-responding to a genuinely escalating threat (no machinery exists to activate when needed). By tying legitimacy to a re-measurable severity/transmissibility profile, this reading is structurally self-correcting — as a disease's R0 or case fatality rate changes (through evolution, treatment advances, or vaccination coverage itself), the threshold determination should in principle re-fire and deactivate the coercion. The founding_problem_status is authored as contested precisely because whether this self-correction actually occurs in practice (versus threshold-creep, where administrators keep diseases 'above threshold' after risk has genuinely declined) is the live empirical question — the rising theater_ratio and suppression_requirement trend in the measurements is the diagnostic signal for exactly this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_drawing_neutrality,
    'Is the severity/transmissibility threshold that separates ''mandate-justified'' from ''not mandate-justified'' pathogens drawn by a neutral epidemiological process, or does the same institution that benefits from broad compliance (public health departments) also control where the line falls?',
    'Compare threshold determinations made by independent epidemiological bodies (e.g., academic modeling consortiums with no enforcement authority) against determinations made by the enforcing public health departments themselves, across multiple pathogens and time periods, for systematic divergence.',
    'If independent and enforcing-body thresholds converge consistently, the proportionality reading is well-grounded as genuinely calibrated. If enforcing bodies systematically draw the threshold to capture more pathogens than independent epidemiology would justify, the reading functions as a legitimation layer over what is structurally closer to the public_health_primary reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_drawing_neutrality, empirical, 'Whether the severity threshold is epidemiologically neutral or institutionally self-serving.').

omega_variable(
    categorical_objection_incommensurability,
    'Can a severity-scaled coercion framework ever be extended to accommodate categorical (non-severity-contingent) objections like religious exemption, or is the incommensurability between scaled and categorical premises irreducible?',
    'Legal and philosophical analysis of whether proportionality frameworks in other domains (e.g., just war theory, self-defense law) have successfully integrated categorical exceptions without collapsing the scaling logic; compare to public health mandate case law on religious exemptions.',
    'If integration is possible, the proportionality reading could evolve a principled accommodation and reduce the extraction currently borne by religious_exemption_seekers. If genuinely incommensurable, this reading structurally cannot address a whole class of objectors regardless of how well-calibrated its severity thresholds become — a permanent residual extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_objection_incommensurability, conceptual, 'Whether severity-scaling and categorical objection are reconcilable within one framework.').

omega_variable(
    threshold_creep_under_novel_pathogens,
    'Under genuine uncertainty (a novel pathogen with unknown R0 and case fatality rate), does the proportionality framework systematically err toward over-classifying threat level (precautionary bias) in a way that imposes measles-level coercion on flu-level actual risk, and does correction happen promptly once better data arrives?',
    'Track historical cases of early-pandemic severity estimation (e.g., initial case fatality rate estimates versus later-revised estimates) against the corresponding mandate intensity imposed during the high-uncertainty window and how quickly mandates were relaxed as estimates were revised downward.',
    'If correction lags substantially behind data revision, the low_severity_disease_mandate_targets stakeholder group is not a rare edge case but a structurally recurring one every time a novel pathogen emerges — meaningfully raising the reading''s real-world extractiveness above the measles/flu steady-state baseline authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_creep_under_novel_pathogens, empirical, 'Whether severity misclassification under novel-pathogen uncertainty self-corrects promptly or persists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(coer_tr_t32, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(coer_be_t32, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 24, 0.46).
narrative_ontology:measurement(coer_su_t32, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 32, 0.47).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is the proportionality reading of the coercion_legitimacy_boundary kernel. It shares a network edge with public_health_primary (which this reading partially constrains by denying blanket coercion authority) and bodily_autonomy_primary (which this reading partially constrains by denying blanket exemption). The three readings differ in victim set and ε: public_health_primary would show higher ε (coercion applies regardless of severity), bodily_autonomy_primary would show near-zero ε (no coercion is authorized), and this reading sits between them with a variable, pathogen-contingent victim set. Each is authored as a separate file per the ε-invariance principle; this file does not average or blend their positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
