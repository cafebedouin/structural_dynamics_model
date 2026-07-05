% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Proportionality-Gated Vaccine Mandate Doctrine
 *   domain: public_health/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the proportionality_reading of the
 *   vaccine_mandate_balance kernel: mandates are permissible only when
 *   disease severity, transmission risk, and vaccine safety jointly clear
 *   strict thresholds, and any mandate must carry robust exemption pathways.
 *   Unlike the sibling readings — public_health_primary (collective
 *   protection overrides consent when voluntary compliance fails) and
 *   bodily_autonomy_primary (consent is inviolable regardless of collective
 *   benefit) — this reading refuses to settle the question categorically. Its
 *   whole point is that legitimacy is a function of the specific pathogen's
 *   parameters: a mandate justified against smallpox (high severity, high
 *   transmissibility, well-established vaccine safety) is not automatically
 *   justified against a mild seasonal pathogen. Because legitimacy is
 *   context-dependent rather than fixed, this constraint's ε is authored as
 *   moderate rather than extreme in either direction — it is measuring the
 *   doctrine's own operation as a graduated test, not the operation of any
 *   single mandate episode. Extraction rises slowly over the measured
 *   interval as case law accretes and the doctrine gets applied more often to
 *   marginal cases (lower-severity pathogens, contested vaccine-safety
 *   profiles) where the proportionality calculus is harder to satisfy
 *   honestly and where exemption administration becomes the real site of
 *   dispute.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter/beneficiary (institutional/analytical) — designs and defends threshold-calibrated mandates
 *   - immunocompromised_populations: beneficiary (powerless/trapped) — depends on population immunity the doctrine helps produce when triggered correctly
 *   - unexempted_objectors: payer (moderate/constrained) — bears compulsion without qualifying for exemption
 *   - borderline_case_workers: payer (powerless/trapped) — least power to contest threshold determinations affecting their employment
 *   - religious_exemption_claimants: excluded/payer (powerless/constrained) — nominally protected but often practically unprotected
 *   - courts_adjudicating_mandate_challenges: observer/beneficiary (institutional/analytical) — applies the test case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.38).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.42).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Proportionality-Gated Vaccine Mandate Doctrine").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, 'c614d52d-702d-4ca5-8362-8199763f919d').
narrative_ontology:cs_kernel_codification('c614d52d-702d-4ca5-8362-8199763f919d', distributed).
narrative_ontology:cs_authority_grounding('c614d52d-702d-4ca5-8362-8199763f919d', expertise).
narrative_ontology:cs_interpretation_layer_present('c614d52d-702d-4ca5-8362-8199763f919d').
narrative_ontology:cs_reading_relation('c614d52d-702d-4ca5-8362-8199763f919d', vaccine_mandate_balance__public_health_primary, influences).
narrative_ontology:cs_reading_relation('c614d52d-702d-4ca5-8362-8199763f919d', vaccine_mandate_balance__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('c614d52d-702d-4ca5-8362-8199763f919d', foundational, legitimacy_is_context_dependent_not_categorical).
narrative_ontology:cs_axiom_status(legitimacy_is_context_dependent_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('c614d52d-702d-4ca5-8362-8199763f919d', legitimacy_is_context_dependent_not_categorical, instrumental).
narrative_ontology:cs_axiom('c614d52d-702d-4ca5-8362-8199763f919d', foundational, exemption_robustness_is_a_precondition_of_valid_compulsion).
narrative_ontology:cs_axiom_status(exemption_robustness_is_a_precondition_of_valid_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('c614d52d-702d-4ca5-8362-8199763f919d', exemption_robustness_is_a_precondition_of_valid_compulsion, deontological).
narrative_ontology:cs_reference_frame('c614d52d-702d-4ca5-8362-8199763f919d', jacobson_proportionality_baseline).
narrative_ontology:cs_drift_state('c614d52d-702d-4ca5-8362-8199763f919d', post_pandemic_polarization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c614d52d-702d-4ca5-8362-8199763f919d', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, courts_adjudicating_mandate_challenges).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, unexempted_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, borderline_case_workers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, religious_exemption_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vaccine_manufacturers).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, proportionality_as_constitutional_test).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, least_restrictive_means_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and defend mandate policies calibrated to a three-part proportionality test — disease severity, transmission risk, vaccine safety — and must build robust exemption pathways into every mandate they issue. They gain legitimacy and enforceability precisely because the test disciplines them against overreach, but they also bear the administrative burden of proving each threshold is met for each pathogen, each time.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, public_health_agencies, beneficiary).

% Depend on population-level immunity for protection they cannot generate themselves. When the proportionality test is satisfied and a mandate issues, they benefit from reduced circulating pathogen; when the test fails to trigger (low-severity pathogen) or is undermined by broad exemptions, they bear elevated exposure risk that the framework does not directly compensate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Object to vaccination on grounds that do not qualify under the doctrine's exemption categories (typically secular philosophical objection lacking religious or documented medical grounding). They bear the mandate's compulsion directly — job loss, school exclusion, fines — while formally 'robust exemptions' exist that they do not qualify for. Their exit is constrained: relocate, litigate, or comply.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, unexempted_objectors, payer,
    moderate, biographical, constrained, national).

% Work in settings (healthcare, congregate care, schools) where mandates are proportionality-justified by transmission risk to vulnerable people they serve. They have the least individual bargaining power to contest the threshold determination and the fewest alternative employment options if they refuse; the doctrine's case-by-case calibration is decided far above their pay grade.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, borderline_case_workers, payer,
    powerless, biographical, trapped, national).

% Hold sincere religious objections that the doctrine formally protects via 'robust exemptions,' but in practice face inconsistent adjudication — some employers and jurisdictions narrow the exemption criteria so tightly that the formal protection does little. They are nominally inside the framework's protective clause but often experience its practical absence, and their adjudicators are typically the same institutions setting the mandate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, religious_exemption_claimants, excluded,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, religious_exemption_claimants, payer).

% Benefit from any mandate issuance regardless of the proportionality calculus's outcome — increased demand follows either a genuine high-severity mandate or a contested low-severity one. They are structurally outside the proportionality test itself, which governs state action against individuals, not the manufacturer's market position.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vaccine_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Apply the proportionality framework as the operative legal test when mandates are challenged, weighing severity, transmission risk, vaccine safety, and exemption robustness case by case. They gain a workable, non-categorical adjudicative tool that lets them avoid both absolute positions (public_health_primary's override, bodily_autonomy_primary's inviolability) — but this also means every hard case is litigated anew rather than settled by rule.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, courts_adjudicating_mandate_challenges, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, courts_adjudicating_mandate_challenges, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated, evidence-responsive test that lets mandates issue when justified by actual epidemiological facts (severity, transmissibility, vaccine safety) and recede when those facts don't support compulsion — avoiding both permanent blanket mandates and permanent blanket prohibition.
% TRANSFER_FUNCTION: Moves compliance burden (vaccination, documentation, employment risk) from the state's categorical authority onto a case-specific determination; shifts adjudicatory power to courts and agencies who apply the threshold test, and shifts risk exposure among populations depending on where the threshold is set for a given pathogen.
% ABSENT_VOICES: People whose objections fall outside recognized exemption categories (secular philosophical objectors) are structurally unrepresented in the exemption design — the doctrine speaks of 'robust exemptions' but the categories were drawn by the same institutions setting the mandate, without their input.
% DISAPPEARANCE_RATIONALE: If the proportionality test vanished, mandate policy would default to one of its sibling readings — either categorical public-health override or categorical bodily-autonomy inviolability — and the entire apparatus of threshold litigation, exemption adjudication, and case-by-case calibration that currently absorbs mandate disputes would disappear, replaced by a bright-line rule fight.
% FOUNDING_PROBLEM: Courts and legislatures needed a workable middle path between two absolutist positions (unlimited state power to compel medical intervention, and unconditional individual veto over public health measures) that could survive contact with real epidemics of varying severity without collapsing into either extreme.
% FOUNDING_PROBLEM_CORROBORATION: Public health law scholars and constitutional courts across multiple jurisdictions (echoing Jacobson v. Massachusetts's proportionality language) continue to treat the graduated test as operative doctrine in ongoing litigation; this corroboration comes from adjudicating courts rather than from public health agencies or manufacturers who benefit from mandate issuance, though courts do also gain from having a workable test to apply.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate: the doctrine's design goal is precisely to prevent unjustified compulsion, so baseline extraction should be lower than a categorical public-health-override reading and higher than a categorical bodily-autonomy reading would produce for the SAME mandate episodes. Suppression (0.42) sits at a similar moderate register — enforcement exists (job loss, exclusion for the unexempted) but is bounded by the exemption requirement the doctrine itself mandates. Theater ratio (0.28) captures that some proportionality review is genuine judicial and epidemiological analysis, but a nontrivial share becomes performative litigation ritual once the categories are established and agencies mostly go through the motions of demonstrating thresholds they've already decided to apply. Accessibility collapse is moderate (0.35) — alternatives (declining vaccination, accepting exclusion, seeking exemption) remain partially open, unlike a true mountain. Resistance (0.55) is substantial because the doctrine invites contestation by design: every mandate is litigable on the threshold question, which is a feature, not a bug, of a case-by-case test.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/court seat, this is a rope: a disciplined test that prevents both overreach and under-protection. From the unexempted objector or borderline-case worker seat, the same doctrine can compute as tangled_rope or worse — genuine coordination logic (protecting vulnerable populations) riding alongside real extraction (compulsion without a recognized exit) that requires active enforcement to hold. The engine should register this divergence structurally rather than have it resolved by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and courts sit near the coordination end: they administer and interpret a test they also benefit from having (it gives them a defensible, non-arbitrary basis for action or restraint). Immunocompromised populations benefit structurally from mandates that clear the threshold, but bear cost when the threshold isn't cleared for pathogens that still endanger them — a genuine two-sided directionality that the doctrine doesn't fully resolve. Unexempted objectors and borderline-case workers sit near the target end: they bear the mandate's compulsory force without commensurate structural power to contest the threshold-setting process. Religious exemption claimants are formally protected but the derivation is complicated by inconsistent real-world adjudication — hence their listing as excluded/payer rather than simple beneficiary of the exemption clause.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — avoiding both absolutist extremes while still permitting evidence-responsive public health action — remains live: novel pathogens continue to test the threshold apparatus, and courts continue to apply Jacobson-derived proportionality language in current litigation. This is not mandatrophy: the mandate (a graduated, falsifiable test) has not outlived its function so much as it faces recurring hard cases at its margins (moderate-severity pathogens, contested vaccine-safety profiles) where its proportionality logic is stressed but not obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_calibration_capture,
    'Are the severity/transmission/safety thresholds set by genuinely independent epidemiological analysis, or do the same agencies that benefit from mandate-issuance authority also control where the thresholds are drawn?',
    'Compare threshold-setting processes across jurisdictions with different institutional separations between the standard-setting body and the mandate-enforcing body; look for correlation between institutional overlap and threshold permissiveness.',
    'If agencies that benefit from mandate authority also set the thresholds that trigger it, the ''strict proportionality'' framing may function as legitimating cover for a de facto public_health_primary regime wearing proportionality language — pushing this reading''s real-world operation toward tangled_rope or snare despite its moderate claimed ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_capture, empirical, 'Whether threshold-setting is independent of mandate-issuing authority.').

omega_variable(
    exemption_robustness_in_practice,
    'Does ''robust exemptions'' as formally declared translate into robust exemptions as administered, or do downstream implementers (employers, schools, local health departments) narrow the exemption criteria in practice?',
    'Track exemption grant/denial rates and litigation outcomes across implementing jurisdictions; compare formal exemption category breadth to actual approval rates.',
    'A large gap between formal and practical exemption robustness would mean the doctrine''s core distinguishing feature (relative to public_health_primary) is not actually operative for most claimants, undermining the reading''s claim to occupy genuine middle ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_in_practice, empirical, 'Whether the exemption pathway functions as declared or is administratively hollowed out.').

omega_variable(
    pathogen_dependent_epsilon_variance,
    'Given that this reading''s own logic makes legitimacy pathogen-dependent, how much does ε actually vary across pathogen severity classes within THIS SINGLE reading, and does that variance threaten the ε-invariance principle for this constraint''s identity?',
    'Model ε separately for high-severity (smallpox-class) and low-severity (seasonal-flu-class) pathogen applications of the same proportionality test; if the variance is large and systematic rather than incidental noise, the doctrine''s application to different pathogen classes may itself warrant decomposition into further sub-stories.',
    'If ε varies systematically and widely by pathogen class even within the single proportionality-reading framework, this suggests the current single story may still be conflating structurally distinct constraint instances (e.g. ''proportionality test applied to high-severity pathogens'' vs ''applied to low-severity pathogens'') that should themselves be decomposed under the ε-invariance principle, rather than a single averaged ε suffic­ing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pathogen_dependent_epsilon_variance, conceptual, 'Whether pathogen-class variance within this one reading itself requires further decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__proportionality_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__proportionality_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__proportionality_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(vacc_tr_t32, vaccine_mandate_balance__proportionality_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(vacc_tr_t40, vaccine_mandate_balance__proportionality_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 16, 0.31).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(vacc_be_t32, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 32, 0.36).
narrative_ontology:measurement(vacc_be_t40, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(vacc_su_t32, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(vacc_su_t40, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the vaccine_mandate_balance kernel. public_health_primary treats collective protection as overriding consent categorically when voluntary compliance fails; bodily_autonomy_primary treats consent as categorically inviolable. This reading (proportionality) rejects both categorical resolutions in favor of a graduated, evidence-responsive test, and its ε is authored independently of either sibling's ε — each reading is its own constraint with its own victim set, its own beneficiary set, and its own classification, linked here for contamination-propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
