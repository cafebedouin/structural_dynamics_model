% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Proportionality Standard for Public Health Coercion (Severity/Transmission-Scaled)
 *   domain: public_health/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the proportionality reading of the coercion
 *   legitimacy kernel: the claim that state power to compel medical
 *   intervention should scale with a pathogen's severity and transmission
 *   dynamics rather than apply as a categorical rule. Under this reading,
 *   measles (R0 ~12-18, meaningful case fatality, no effective treatment)
 *   crosses the threshold that justifies mandatory vaccination and school
 *   exclusion, while seasonal influenza (lower R0, treatable, high
 *   population-level familiarity) does not, despite killing more people in
 *   absolute numbers in most years. This is deliberately authored as ONE
 *   reading among three siblings sharing the same kernel —
 *   public_health_primary (state may compel whenever collective
 *   harm-prevention outweighs autonomy, full stop) and
 *   bodily_autonomy_primary (no compulsion is ever permissible regardless of
 *   collective benefit) are separate constraints, not measurement variants of
 *   this one. Do not average across them; each has a distinct ε, victim set,
 *   and structural profile.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.42).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.48).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Proportionality Standard for Public Health Coercion (Severity/Transmission-Scaled)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, 'f822a9a9-a55b-4462-940d-8a163022cf65').
narrative_ontology:cs_kernel_codification('f822a9a9-a55b-4462-940d-8a163022cf65', distributed).
narrative_ontology:cs_authority_grounding('f822a9a9-a55b-4462-940d-8a163022cf65', practice).
narrative_ontology:cs_interpretation_layer_present('f822a9a9-a55b-4462-940d-8a163022cf65').
narrative_ontology:cs_reading_relation('f822a9a9-a55b-4462-940d-8a163022cf65', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('f822a9a9-a55b-4462-940d-8a163022cf65', coercion_legitimacy_boundary__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('f822a9a9-a55b-4462-940d-8a163022cf65', foundational, coercion_legitimacy_is_graduated_not_categorical).
narrative_ontology:cs_axiom_status(coercion_legitimacy_is_graduated_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('f822a9a9-a55b-4462-940d-8a163022cf65', coercion_legitimacy_is_graduated_not_categorical, instrumental).
narrative_ontology:cs_axiom('f822a9a9-a55b-4462-940d-8a163022cf65', foundational, severity_and_transmission_dynamics_are_the_relevant_scaling_variables).
narrative_ontology:cs_axiom_status(severity_and_transmission_dynamics_are_the_relevant_scaling_variables, holdable).
narrative_ontology:cs_axiom_grounding('f822a9a9-a55b-4462-940d-8a163022cf65', severity_and_transmission_dynamics_are_the_relevant_scaling_variables, empirically_contingent).
narrative_ontology:cs_reference_frame('f822a9a9-a55b-4462-940d-8a163022cf65', graduated_police_power_doctrine).
narrative_ontology:cs_drift_state('f822a9a9-a55b-4462-940d-8a163022cf65', post_covid19_mandate_litigation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f822a9a9-a55b-4462-940d-8a163022cf65', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, school_age_children).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, vaccine_hesitant_parents).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, religious_exemption_seekers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, individuals_with_borderline_risk_assessments).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, graduated_proportionality_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, harm_calculus_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the case-by-case severity/transmission threshold that determines whether a given pathogen justifies mandatory intervention (quarantine, mandatory vaccination, school exclusion). Administers the R0/mortality/latency calculus, updates thresholds as epidemiological data changes, and enforces mandates once a disease crosses the line. Collects legitimacy and institutional authority from being the visible arbiter of where the line sits.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Attend schools where high-R0 diseases like measles spread efficiently through unvaccinated pockets. Benefit directly from herd-immunity thresholds being enforced against classmates' non-vaccination, but have no voice in setting the threshold and cannot exit the school population they are embedded in.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, school_age_children, beneficiary,
    powerless, biographical, trapped, regional).

% Cannot be vaccinated themselves or face elevated risk from vaccine-preventable disease, so their survival depends on herd immunity maintained by others' compliance with the mandate. They benefit when the severity threshold is drawn to compel high-R0 disease vaccination but have no independent enforcement capacity of their own.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, regional).

% Bear the direct cost of compulsion: mandatory vaccination or exclusion from school enrollment for their children when a disease crosses the agency's severity threshold. Their exit options are homeschooling, relocation to a jurisdiction with weaker enforcement, or a narrow band of medical/religious exemptions that agencies increasingly restrict. They experience the same proportionality standard that spares them during flu season as a hard mandate during a measles outbreak.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vaccine_hesitant_parents, payer,
    moderate, biographical, constrained, regional).

% Hold sincere objections that the proportionality framework treats as a variable to be weighed against, not a categorical bar. As disease severity classifications tighten (e.g., measles resurgence), their exemption pathway narrows even though their underlying objection has not changed — the coercion applied to them is a direct function of a pathogen-severity score they had no role in calculating.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, religious_exemption_seekers, payer,
    powerless, biographical, constrained, regional).

% Are NOT compelled under this standard because seasonal influenza sits below the severity/transmission threshold, despite killing far more people annually than measles in absolute terms in most years. They are excluded from the coercion apparatus entirely and have no reason to contest a framework that does not reach them — their absence from the fight is itself a structural feature of where the line is drawn.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, influenza_sufferers_and_the_unvaccinated_flu_population, excluded,
    powerless, biographical, mobile, national).

% Supply the R0, case fatality rate, and transmission-dynamics data that the proportionality calculus is built on. They do not enforce mandates directly but their models are the load-bearing inputs the agenda-setter cites as justification, giving them substantial de facto authority over where the coercion line falls.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, epidemiologists_and_biostatisticians, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, epidemiologists_and_biostatisticians, agenda_setter).

% Adjudicate challenges to specific mandates by testing whether the invoked severity/transmission justification is proportionate to the compulsion imposed (rational basis to strict scrutiny depending on jurisdiction and right implicated). Their case law is what turns 'proportionality' from a policy slogan into an enforceable legal test, and their rulings can raise or lower the threshold agencies must clear.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, courts_and_constitutional_review_bodies, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated, evidence-linked standard for when state compulsion of medical intervention is justified, so that coercive power is calibrated to actual epidemiological risk rather than applied uniformly to all pathogens or withheld from all of them.
% TRANSFER_FUNCTION: Moves bodily-autonomy costs from the general population onto the specific subset of objectors whose disease-of-concern happens to score above the severity/transmission threshold at a given time; moves protection (reduced infection risk) from compliant populations to immunocompromised and otherwise-vulnerable beneficiaries.
% ABSENT_VOICES: Vaccine-hesitant parents and religious exemption seekers participate in litigation and comment periods but do not sit on the epidemiological panels that set the R0/severity thresholds that determine whether they will be compelled; the threshold-setting process itself is closed to the people it will bind.
% DISAPPEARANCE_RATIONALE: If the proportionality standard vanished, jurisdictions would default to one of its sibling readings — either uniform public-health-primary compulsion across all pathogens, or a uniform bodily-autonomy bar against any compulsion — and the entire case-by-case adjudication apparatus (epidemiological panels, court proportionality tests, graduated exemption regimes) would be replaced by a categorical rule. Measles mandates and flu non-mandates would no longer be different in kind, only in degree of political will.
% FOUNDING_PROBLEM: Neither absolute deference to state health authority nor absolute deference to individual consent produced defensible outcomes across the full range of infectious disease severity — a rule that mandated flu vaccination as readily as measles vaccination, or refused to compel either, both seemed intuitively wrong to courts and legislatures grappling with real outbreaks.
% FOUNDING_PROBLEM_CORROBORATION: Attested by constitutional law scholars analyzing Jacobson v. Massachusetts progeny and its limits, and by comparative public health law researchers documenting divergent state responses to measles versus influenza outbreaks — sources outside both the public-health-agency beneficiary set and the objector-payer set converge on describing the severity gradient as a real, persisting adjudicative problem rather than a resolved one.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extraction (0.42) sits in a moderate band because the standard genuinely limits coercion for the majority of low-severity pathogens (flu, common cold) while permitting it for a narrow, severity-triggered set (measles, and historically smallpox, polio). Suppression (0.48) reflects the real enforcement machinery behind measles-tier mandates — school exclusion, narrowed exemptions, occasional quarantine orders — applied to a bounded population once a pathogen crosses threshold. Theater ratio is low (0.22, rising slowly) because the epidemiological justification is substantially real, though rising slightly as exemption litigation increasingly turns on procedural compliance rather than fresh risk assessment. Accessibility collapse (0.4) is moderate: exemption pathways exist but narrow as classification tightens. Resistance (0.55) is meaningfully high because affected objectors litigate vigorously and case law is genuinely contested, unlike a settled natural-law constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and the epidemiologists who supply their models sit near the agenda-setting/beneficiary end: they administer the threshold and derive institutional legitimacy from being the visible, evidence-based arbiter. Immunocompromised populations and school-age children are structural beneficiaries with no enforcement power of their own — pure recipients of the coordination benefit. Vaccine-hesitant parents and religious exemption seekers are the clearest targets: the entire coercive apparatus activates specifically against them once their pathogen-of-concern crosses the severity line, and their exit options (homeschool, relocate, litigate) are real but costly. The influenza population is structurally excluded from the entire coercion apparatus — not because they lack risk, but because the threshold, as currently calibrated, does not reach them; this exclusion is itself a data point about where the proportionality line is actually drawn versus where raw mortality data alone would draw it.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading resists mandatrophy in two directions at once. Against public_health_primary, it prevents every future pathogen from being treated as automatically mandate-worthy merely because compulsion would reduce transmission — the standard requires fresh severity/transmission justification each time, which is exactly what keeps flu-tier mandates from ever being imposed. Against bodily_autonomy_primary, it prevents a categorical consent bar from surviving contact with genuinely high-mortality, high-R0 pathogens where uncompensated externalities to immunocompromised third parties are severe. The founding problem (neither pole produced defensible case-by-case outcomes) remains live precisely because new pathogens keep testing where the line should sit — this is not a solved problem masquerading as ongoing, it is a genuinely recurring adjudicative task.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_calibration_authority,
    'Who legitimately sets the severity/transmission threshold that separates mandate-worthy pathogens from non-mandate-worthy ones, and is that threshold itself subject to capture by the agencies that benefit from appearing decisive during outbreaks?',
    'Comparative analysis of threshold-setting processes across jurisdictions: are thresholds set by insulated epidemiological panels with defined update triggers, or by agencies with discretion to reclassify a pathogen''s severity score in response to political pressure or outbreak visibility (independent of actual mortality/R0 data)?',
    'If thresholds are calibrated by the same agencies that gain institutional authority from crossing them, the proportionality reading risks collapsing toward public_health_primary in practice even while retaining proportionality language — an FSM-adjacent drift internal to this reading rather than a shift to the sibling constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_authority, empirical, 'Whether threshold-setting is insulated from the agencies it empowers.').

omega_variable(
    severity_metric_choice_ambiguity,
    'Should severity be measured by case fatality rate, R0, years of life lost, absolute annual mortality, or some composite — and does the choice of metric predetermine which pathogens cross the coercion threshold in ways that are themselves contestable rather than purely empirical?',
    'Compare classification outcomes for a fixed set of historical pathogens (measles, influenza, pertussis, mumps) under each candidate metric; document cases where metric choice alone flips the mandate/no-mandate verdict.',
    'If metric choice determines outcomes at the margin (e.g., influenza''s absolute mortality exceeds measles''s in most years, yet influenza remains non-mandated), the ''objective'' proportionality standard carries a hidden conceptual/preference component the framework''s ε score should reflect as moderate rather than low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_metric_choice_ambiguity, conceptual, 'Whether the severity metric is a neutral empirical input or a contestable framing choice.').

omega_variable(
    kernel_reading_coexistence_stability,
    'Can the proportionality reading persist stably alongside its two siblings across different jurisdictions and courts, or does sustained contact with hard cases (e.g., a highly transmissible but low-mortality pathogen, or a low-transmissibility but catastrophic-mortality pathogen) force courts to collapse toward one of the categorical siblings?',
    'Longitudinal tracking of case law across multiple jurisdictions facing genuinely intermediate-severity outbreaks (e.g., mpox, certain influenza strain years) to see whether courts consistently apply graduated reasoning or drift toward categorical rules under pressure.',
    'If graduated reasoning proves unstable under intermediate cases, this reading may be a transitional equilibrium rather than a durable independent constraint, with implications for whether it should itself carry a has_sunset_clause-style structural note in future revisions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_stability, empirical, 'Whether the graduated standard survives contact with genuinely intermediate-severity pathogens.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(coer_tr_t32, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(coer_be_t32, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(coer_su_t32, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 32, 0.47).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'when is public health coercion legitimate' per the epsilon-invariance principle. public_health_primary resolves the kernel toward unconditional collective-harm-outweighs-autonomy compulsion (higher ε, broader victim set, no severity floor). bodily_autonomy_primary resolves it toward a categorical consent bar (near-zero ε for the state's compulsion function, but a correspondingly larger victim set among third parties who bear uncompensated externalities). This proportionality_reading sits structurally between them: moderate ε, a pathogen-severity-conditioned victim set, and genuine case-by-case adjudication machinery (courts, epidemiological panels) that neither sibling requires in the same form. All three share the underlying kernel (some standard must govern compelled medical intervention) but instantiate structurally distinct constraints with distinct beneficiary/victim data and distinct classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
