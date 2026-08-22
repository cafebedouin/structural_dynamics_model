% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality Standard for Legitimate Health Interventions
 *   domain: public health policy / medical ethics / constitutional law
 *
 * SUMMARY:
 *   This story authors the proportionality reading of the
 *   legitimate_health_intervention kernel: legitimacy is grounded neither in
 *   aggregate population outcomes alone nor in individual bodily autonomy
 *   alone, but in a weighing test that scales permissible intervention
 *   severity to disease-specific threat characteristics (transmissibility,
 *   case-fatality rate, at-risk population size). The standing arrangement
 *   under contest — the arrangement this ε is about — is the proportionality
 *   doctrine as currently applied by courts and health authorities, not any
 *   endorsed alternative. The mid-interval spike in extraction and theater
 *   (T=12) models a period of contested classification (e.g., a novel or
 *   emerging pathogen) where the proportionality calculus was invoked to
 *   justify interventions before the epidemiological facts had stabilized,
 *   followed by partial correction once better data arrived — this is a
 *   genuine drift feature of the doctrine's actual operation, not noise.
 *
 * KEY AGENTS:
 *   - public_health_authorities: institutional agenda-setter — classifies disease severity and calibrates intervention tier
 *   - high_risk_populations: moderate-power beneficiary — protected by genuinely severe interventions reserved for genuinely severe threats
 *   - low_risk_dissenting_individuals: powerless payer — bears interventions calibrated by others for threats they perceive as personally low
 *   - borderline_case_litigants: powerless, trapped payer — absorbs the cost of classificatory uncertainty in the ambiguous middle of the severity spectrum
 *   - judicial_review_bodies: institutional observer/beneficiary — applies the doctrine, gains a workable standard, imports epidemiological contest into legal contest
 *   - epidemiological_modelers: excluded — supplies the load-bearing severity estimates without a formal seat in the weighing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.38).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.42).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality Standard for Legitimate Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public health policy / medical ethics / constitutional law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a').
narrative_ontology:cs_kernel_codification('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a', distributed).
narrative_ontology:cs_authority_grounding('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a', practice).
narrative_ontology:cs_interpretation_layer_present('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a').
narrative_ontology:cs_reading_relation('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a', legitimate_health_intervention__public_health_primary, influences).
narrative_ontology:cs_reading_relation('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a', legitimate_health_intervention__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a', foundational, severity_weighted_legitimacy).
narrative_ontology:cs_axiom_status(severity_weighted_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a', severity_weighted_legitimacy, instrumental).
narrative_ontology:cs_axiom('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a', foundational, disease_characteristics_determine_permissible_coercion_ceiling).
narrative_ontology:cs_axiom_status(disease_characteristics_determine_permissible_coercion_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a', disease_characteristics_determine_permissible_coercion_ceiling, empirically_contingent).
narrative_ontology:cs_reference_frame('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a', graduated_least_restrictive_means_standard).
narrative_ontology:cs_drift_state('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a', post_pandemic_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('16b9e72c-a82a-40d2-a22a-5d4e5c6a1f0a', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, high_risk_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, judicial_review_bodies).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, low_risk_dissenting_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, borderline_case_litigants).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, graduated_intervention_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and apply the proportionality test itself — classifying diseases by transmissibility and case-fatality rate and calibrating intervention severity (advisory vs. quarantine vs. mandate) accordingly. They gain legitimacy cover: a graduated response defends against both public-health-primary claims of underreaction and autonomy claims of overreach, provided the classification holds up under review.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, public_health_authorities, beneficiary).

% Immunocompromised people, the elderly, and those in congregate settings benefit when interventions scale up proportionally to genuinely severe threats (measles-tier transmissibility, high case-fatality). The framework protects them precisely because it does not treat every pathogen identically — it reserves the most severe interventions for the diseases that actually threaten them.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, high_risk_populations, beneficiary,
    moderate, biographical, constrained, regional).

% Individuals who object to an intervention calibrated for a disease they perceive as low personal risk (e.g., healthy young adults facing school-exclusion for a moderately transmissible illness) bear the cost of a classification made by others. Their exit is constrained by school attendance, employment, or travel requirements tied to compliance; the proportionality test formally weighs their autonomy but the weighting is set upstream by the authority, not by them.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, low_risk_dissenting_individuals, payer,
    powerless, immediate, constrained, local).

% People whose disease falls in the ambiguous middle of the transmissibility/severity spectrum (moderate seasonal illness, emerging pathogens with uncertain case-fatality) become test cases for where the proportionality line sits. They absorb the cost of the framework's own uncertainty — often bearing severe interventions justified by precaution before the epidemiological picture resolves, with no seat at the table where the classification is set.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, borderline_case_litigants, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, borderline_case_litigants, excluded).

% Courts apply the proportionality test to adjudicate specific interventions, comparing severity to threat level using epidemiological evidence submitted by the parties. They benefit from having a workable doctrinal standard that avoids the all-or-nothing outcomes of pure autonomy or pure public-health frameworks, but their reliance on epidemiological classification as a legal input means contested science becomes contested law.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, judicial_review_bodies, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, judicial_review_bodies, beneficiary).

% Their transmissibility and case-fatality estimates are the load-bearing inputs to the whole proportionality calculation, but they are not parties to the legal or policy decision — they supply numbers that get weighted by others, often under time pressure before uncertainty has resolved, and are not consulted on how their confidence intervals should translate into intervention tiers.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, epidemiological_modelers, excluded,
    moderate, immediate, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated decision rule that lets the same legal and administrative apparatus handle both a mild seasonal illness and a highly lethal, highly transmissible outbreak without either always overriding individual choice or always deferring to it — coordinating a shared standard across radically different disease profiles.
% TRANSFER_FUNCTION: Moves discretion over which interventions are permissible from a fixed rule (either always defer to public health, or always defer to autonomy) to a case-by-case weighing exercise controlled by whichever institution classifies the disease's severity and calibrates the response — shifting the practical burden of proof onto whichever party disputes that classification.
% ABSENT_VOICES: Epidemiological modelers whose uncertain estimates become dispositive legal facts are not parties to the proportionality determination. Populations whose disease sits in the ambiguous middle of the severity spectrum have no formal say in where the classification line falls, and often only discover their placement after the intervention is imposed.
% DISAPPEARANCE_RATIONALE: Public health authorities and courts would say the world rearranges badly — interventions would revert to either blanket public-health mandates or blanket autonomy vetoes, both of which the proportionality standard was built to avoid. Low-risk dissenters and borderline litigants would say the world changes less than claimed — the classification discretion would simply move to whichever binary rule replaces it, and the underlying contest over whose harm counts more would persist under a different label.
% FOUNDING_PROBLEM: Courts and legislatures needed a workable middle ground between two failure modes: public-health-primary regimes that had historically justified severe, poorly calibrated interventions (indefinite quarantine, forced treatment) against low-threat conditions, and pure-autonomy regimes that left genuinely dangerous outbreaks (measles in unvaccinated clusters, novel respiratory pathogens) without any coercive tool at all.
% FOUNDING_PROBLEM_CORROBORATION: Public health law scholars and several appellate courts attest the problem remains live — citing recent outbreaks where courts had to distinguish permissible school-exclusion policies for measles from impermissible ones for milder illnesses. Civil liberties litigants attest the founding problem has been substantially resolved by informed-consent doctrine and that proportionality analysis now mostly launders discretionary overreach as calibrated response — this corroboration comes from outside the authorities who administer the standard, and it directly disputes the authorities' own account.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, contested).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).
:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) and suppression (0.42) sit at moderate levels because the doctrine's own structure is genuinely conditional — for a low-transmissibility, low-case-fatality disease the proportionality test itself should yield minimal intervention, so the framework is partly self-limiting. Resistance (0.55) is higher than extraction because the classification exercise is contested at every application: litigants, epidemiologists, and advocacy groups actively dispute where a given disease sits on the severity spectrum, and that dispute is the doctrine's normal operating mode, not a breakdown of it. Accessibility collapse (0.35) is moderate-low: individuals retain meaningful legal and political avenues (litigation, legislative appeal, epidemiological rebuttal) that a pure mountain or pure snare would foreclose.
 *
 * DIRECTIONALITY LOGIC:
 *   High-risk populations and the authorities that administer the standard sit toward the beneficiary end: the doctrine either protects them directly or gives them a defensible, court-tested tool. Low-risk dissenters and borderline litigants sit toward the target end: they bear the practical weight of a severity classification set by an institution they cannot bind, with constrained or trapped exit (school, employment, or legal-status conditions attached to compliance). Epidemiological modelers are structurally adjacent but excluded from the weighing decision itself, despite supplying its inputs — their d is not derived from beneficiary/victim status but from their absence from the decision, which the six_questions absent_voices field captures rather than the directionality math.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's founding problem — preventing both under- and over-reaction to disease threat — remains partly live (contested, not dead): outbreaks of highly transmissible, high-case-fatality diseases still occur and still test the standard's upper bound, while low-threat conditions still generate disputes at its lower bound. This keeps the arrangement from qualifying as pure mandatrophy (a dead-problem zombie), but the borderline-case pattern (T=8 to T=12 in the measurements) shows the doctrine's discretion being used to justify precautionary severity ahead of resolved evidence — exactly the drift a tangled_rope classification is meant to flag: real coordination function, real asymmetric cost falling on those least able to contest the classification in the moment it is applied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_capture_risk,
    'Is the disease-severity classification that drives the proportionality weighing performed by an epistemically independent process, or can the classifying authority tune the classification to justify a predetermined intervention level?',
    'Audit trail comparing epidemiological confidence intervals at the time of classification against the intervention tier selected; track whether tier selections cluster at the severity threshold that just barely justifies the desired intervention.',
    'If classification is capturable, the proportionality reading collapses toward whichever pole (public-health-primary or autonomy-primary) the classifying authority prefers, and the doctrine''s independent coordination function is largely cosmetic — pushing this story toward snare. If classification is robust, the tangled_rope reading (genuine coordination plus asymmetric cost on borderline cases) holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_capture_risk, empirical, 'Whether severity classification is independently constrained or authority-tunable.').

omega_variable(
    borderline_case_burden_allocation,
    'Should the cost of epidemiological uncertainty in borderline disease cases fall on the individuals subject to intervention (precautionary severity) or on the intervening authority (default to lighter intervention until evidence resolves)?',
    'This is not resolvable by further data — it is a value choice about who bears the risk of being wrong under uncertainty, independent of what the eventual epidemiological facts turn out to be.',
    'A norm favoring precautionary severity increases ε for borderline_case_litigants structurally, regardless of any given disease''s true severity; a norm favoring default-light-intervention shifts uncompensated risk onto high_risk_populations instead. Neither resolution eliminates the tradeoff — it relocates which victim group absorbs it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(borderline_case_burden_allocation, preference, 'Who should bear the cost of unresolved severity classification under the proportionality standard.').

omega_variable(
    reading_choice_framing,
    'Is treating ''proportionality'' as a distinct third reading (rather than as a procedural gloss applied within either public_health_primary or bodily_autonomy_primary) the correct decomposition, or does proportionality reasoning actually operate as a tie-breaking rule internal to one of the other two readings in most real adjudications?',
    'Survey of appellate opinions invoking proportionality analysis: do courts treat it as an independent third framework, or as a step within a public-health-primary or autonomy-primary opinion that ultimately resolves to one of those poles?',
    'If proportionality functions mostly as an internal tie-breaker rather than an independent legitimacy standard, this story may be better modeled as a modifier on the sibling readings'' ε rather than a freestanding constraint — but per the ε-invariance principle, the current decomposition is retained because the doctrine is cited and taught as an independently named standard with its own case law lineage (the classic ''least restrictive means'' proportionality test), which justifies treating it as a distinct kernel reading rather than folding it into a sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_choice_framing, conceptual, 'Whether proportionality is a genuinely independent reading or an internal step within the other two readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t4, legitimate_health_intervention__proportionality_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(legi_tr_t8, legitimate_health_intervention__proportionality_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__proportionality_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(legi_tr_t16, legitimate_health_intervention__proportionality_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__proportionality_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__proportionality_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(legi_be_t4, legitimate_health_intervention__proportionality_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__proportionality_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__proportionality_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(legi_be_t16, legitimate_health_intervention__proportionality_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__proportionality_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__proportionality_reading, base_extractiveness, 24, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(legi_su_t4, legitimate_health_intervention__proportionality_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__proportionality_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__proportionality_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(legi_su_t16, legitimate_health_intervention__proportionality_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__proportionality_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__proportionality_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimate_health_intervention kernel (public_health_primary, proportionality_reading, bodily_autonomy_primary). Each reading authors a distinct ε and victim set rather than three observation angles on one ε: public_health_primary treats individual refusal as pure externality (high suppression tolerance, low weight on autonomy), bodily_autonomy_primary treats state coercion as illegitimate regardless of aggregate benefit (near-total weight on individual consent), and this reading (proportionality_reading) authors a conditional weighing structure whose ε scales with disease transmissibility and case-fatality rate rather than sitting fixed at either pole. The three are linked here and in the sibling files via affects_constraints because judicial and legislative choices under one reading materially change the legitimacy conditions and resource availability under the others (e.g., a court adopting proportionality analysis narrows the domain in which pure public-health-primary or pure autonomy-primary arguments can prevail).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
