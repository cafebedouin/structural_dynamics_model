% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Proportionality Test for Vaccine Mandate Legitimacy
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the proportionality_reading of the
 *   mandate_legitimacy_scope kernel: mandate legitimacy is conditional on
 *   disease severity, vaccine safety/efficacy, and the absence of less
 *   restrictive alternatives — a tiered-scrutiny doctrine used by courts and
 *   agencies to distinguish measles-type mandates (high severity, high
 *   efficacy, few alternatives — legitimate under this reading) from flu-type
 *   mandates (lower severity, more alternatives available — illegitimate
 *   under this reading). This is a DIFFERENT constraint from
 *   public_health_primary (which grounds legitimacy in state authority to
 *   protect the vulnerable, full stop) and bodily_autonomy_primary (which
 *   treats any non-consensual medical compulsion as illegitimate regardless
 *   of collective benefit). The victim set here is conditional and
 *   pathogen-specific rather than fixed: whether an individual falls into a
 *   victim category depends on the disease parameters at hand, not on a
 *   categorical rule. The moderate ε (0.42) reflects that this reading,
 *   properly applied, permits proportionate compulsion but withholds it where
 *   the test fails — the extraction is real but bounded by the test's own
 *   logic, unlike the higher-ε unconditional public_health_primary reading or
 *   a snare that ignores proportionality entirely.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.42).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.48).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality Test for Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, 'f6880b89-ab5a-4a02-a385-fd87987f9b2f').
narrative_ontology:cs_kernel_codification('f6880b89-ab5a-4a02-a385-fd87987f9b2f', distributed).
narrative_ontology:cs_authority_grounding('f6880b89-ab5a-4a02-a385-fd87987f9b2f', practice).
narrative_ontology:cs_interpretation_layer_present('f6880b89-ab5a-4a02-a385-fd87987f9b2f').
narrative_ontology:cs_reading_relation('f6880b89-ab5a-4a02-a385-fd87987f9b2f', mandate_legitimacy_scope__public_health_primary, influences).
narrative_ontology:cs_reading_relation('f6880b89-ab5a-4a02-a385-fd87987f9b2f', mandate_legitimacy_scope__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('f6880b89-ab5a-4a02-a385-fd87987f9b2f', foundational, legitimacy_is_conditional_on_empirical_proportionality).
narrative_ontology:cs_axiom_status(legitimacy_is_conditional_on_empirical_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('f6880b89-ab5a-4a02-a385-fd87987f9b2f', legitimacy_is_conditional_on_empirical_proportionality, instrumental).
narrative_ontology:cs_axiom('f6880b89-ab5a-4a02-a385-fd87987f9b2f', foundational, least_restrictive_alternative_must_be_exhausted_before_compulsion).
narrative_ontology:cs_axiom_status(least_restrictive_alternative_must_be_exhausted_before_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('f6880b89-ab5a-4a02-a385-fd87987f9b2f', least_restrictive_alternative_must_be_exhausted_before_compulsion, empirically_contingent).
narrative_ontology:cs_reference_frame('f6880b89-ab5a-4a02-a385-fd87987f9b2f', tiered_scrutiny_balancing_framework).
narrative_ontology:cs_drift_state('f6880b89-ab5a-4a02-a385-fd87987f9b2f', post_pandemic_mandate_litigation_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f6880b89-ab5a-4a02-a385-fd87987f9b2f', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, courts_applying_tiered_scrutiny).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, low_risk_workers_under_broad_mandates).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, religious_and_philosophical_objectors).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, individuals_mandated_for_low_severity_pathogens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, vaccine_manufacturers).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, means_ends_fit_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__proportionality_reading, least_restrictive_alternative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and defend mandates by assembling severity data, efficacy studies, and alternative-measure analyses. Under this reading they must justify each mandate against a proportionality test rather than invoking blanket authority; this constrains which mandates they can sustain but also gives them a durable legitimacy framework when the test is met.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Adjudicate individual mandate challenges by weighing disease severity, vaccine safety/efficacy, and whether less restrictive alternatives (testing, masking, isolation) were available. Their rulings determine which mandates survive and which are struck down, effectively administering the proportionality standard case by case.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, courts_applying_tiered_scrutiny, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, courts_applying_tiered_scrutiny, observer).

% Depend on herd-level protection for high-severity, high-transmissibility diseases where their own vaccination may be medically contraindicated or ineffective. The proportionality reading protects them precisely because it permits mandates when severity is high and alternatives are weak, while withholding that protection for low-severity pathogens where they gain little anyway.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, local).

% Employed in settings where mandates are applied uniformly regardless of individual risk or the specific pathogen's severity profile. Under a strict proportionality reading they should be exempted from mandates targeting low-severity pathogens with viable alternatives, but institutional inertia and administrative convenience often apply the mandate anyway; challenging it requires litigation resources or job loss.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, low_risk_workers_under_broad_mandates, payer,
    moderate, biographical, constrained, national).

% Object on grounds the proportionality test does not fully address, since the test evaluates disease/vaccine parameters rather than the legitimacy of compelling any individual regardless of belief. When a mandate passes the proportionality test, their objection is overridden even though the test never engages their underlying claim; their only recourse is litigation or exemption-seeking, both uncertain.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, religious_and_philosophical_objectors, payer,
    powerless, biographical, constrained, national).

% Subject to mandates for conditions (e.g., seasonal influenza in low-risk settings) that this reading itself says should fail the proportionality test given lower severity and available alternatives like rapid testing. When mandates persist anyway through institutional lag or misapplication of the standard, they bear the compliance cost without the legitimating structure the reading promises them.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, individuals_mandated_for_low_severity_pathogens, payer,
    powerless, biographical, constrained, regional).

% Benefit whenever a mandate is upheld under the proportionality standard, since upheld mandates guarantee demand. They have no formal role in the legitimacy test but their efficacy and safety data are the evidentiary backbone courts and agencies rely on, giving them indirect influence over which mandates the test validates.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vaccine_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Supply the severity, transmissibility, and efficacy data the proportionality test formally requires. They do not adjudicate legitimacy themselves but their measurement choices (case fatality rate vs. hospitalization rate, R0 estimates, real-world vs. trial efficacy) materially shape which mandates the test will validate.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, epidemiologists_and_biostatisticians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, case-by-case standard for distinguishing mandates that are proportionate responses to serious disease threats from mandates that are administratively convenient but disproportionate impositions — allowing courts, agencies, and the public to evaluate mandates on consistent criteria rather than blanket authority or blanket refusal.
% TRANSFER_FUNCTION: Moves legitimacy itself: mandates that satisfy the three-factor test (severity, safety/efficacy, absence of less restrictive alternatives) are transferred coercive authority over individuals; mandates that fail the test lose that authority. Compliance costs and bodily autonomy concessions flow from mandated individuals to the collective good the mandate targets, but only where the test is satisfied — the reading's entire function is gating that flow.
% ABSENT_VOICES: Individuals whose religious or philosophical objection is not about disease severity or vaccine efficacy at all, but about the legitimacy of state compulsion over their body under any factual conditions, have no seat inside a test built entirely around empirical proportionality factors. Their objection is structurally unanswerable by this reading regardless of the data.
% DISAPPEARANCE_RATIONALE: Without the proportionality standard, mandate legitimacy would default to either unconditional state authority (public_health_primary) or unconditional individual veto (bodily_autonomy_primary) — courts would lose the middle-ground doctrinal tool that currently lets some mandates survive review while others are struck down; case outcomes for measles-type and flu-type mandates would converge toward whichever sibling reading filled the vacuum.
% FOUNDING_PROBLEM: Courts and legislatures needed a way to distinguish genuinely necessary public health interventions from pretextual or overbroad ones, after historical mandates were imposed with little differentiation between catastrophic and mild disease threats.
% FOUNDING_PROBLEM_CORROBORATION: Public health law scholars and several appellate courts (outside the agencies that benefit from having a legitimating framework) attest the problem remains live — citing continued disputes over whether specific mandates meet the severity and least-restrictive-alternative prongs. Civil liberties organizations, also outside the benefiting institutional set, corroborate that the test is still actively contested rather than settled, though they argue it under-protects autonomy interests the test was never designed to weigh.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).
:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate because the reading's entire design is to gate coercion by severity/efficacy/alternatives rather than apply it unconditionally — but extraction still occurs whenever the test is passed, or misapplied through institutional lag (as with the low_severity_pathogen victim group). Suppression (0.48) tracks enforcement of mandates that have cleared the test, which still requires compliance machinery (exclusion from employment/school, fines) though bounded by the doctrine's own limiting principle. Theater ratio (0.28) is present but not dominant — some administrative proportionality review is performative rubber-stamping rather than genuine severity analysis, a risk that grows slightly over the measured interval as agencies routinize the test rather than genuinely re-derive it per pathogen. Accessibility collapse (0.4) is moderate: individuals have some recourse through litigation and exemption processes, but the recourse is costly and uncertain. Resistance (0.55) is comparatively high because both institutional actors (challenging mandate scope in court) and individual objectors actively contest applications of the test.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities and courts experience this as a workable, self-limiting coordination mechanism that lets them justify necessary interventions while disclaiming overreach. Individuals mandated under pathogens that fail the proportionality test (or where the test is applied loosely) experience the identical structure as extraction dressed in analytical language — the same doctrine, computed from the opposite seat, is either principled gatekeeping or a rationalized mandate. The engine's per-seat computation should register the divergence between the powerless payer under a low-severity-pathogen mandate and the institutional agenda-setter administering the same test.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and courts sit as agenda-setters who administer the test; immunocompromised populations and vaccine manufacturers benefit when the test validates protective mandates. Payers are differentiated by whether the test, correctly applied to their specific pathogen, would or would not legitimate the mandate they face: individuals mandated for genuinely severe, low-alternative diseases experience a legitimately gated coercion (lower d than an unconditional mandate would produce), while individuals mandated for low-severity pathogens where the reading's own logic says the mandate should fail sit at high d — the constraint is failing on its own terms for them. Religious/philosophical objectors sit at high d regardless of pathogen severity because the test structurally cannot address their objection at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing catastrophic from mild disease threats before compelling vaccination — remains partially live: measles-type scenarios still clearly satisfy the test's severity and alternative-scarcity prongs. But mandatrophy risk is real where agencies apply the standard to marginal or low-severity pathogens through habit or precedent rather than fresh severity analysis, effectively treating a case-by-case test as a categorical license. Theater ratio's mild upward drift over the interval is the signal to watch: routinized proportionality review that stops re-deriving the answer per pathogen is the test decaying into its own cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_test_administration_capture,
    'Is the three-factor proportionality test being genuinely re-derived for each pathogen, or has it calcified into a rubber-stamp that agencies apply pro forma to mandates decided on other grounds?',
    'Audit trail comparison: for mandates issued under this reading, compare the severity/efficacy/alternatives analysis actually performed against the analysis the doctrine formally requires. Rising theater_ratio combined with declining mandate rejection rate would indicate capture.',
    'If captured, this reading functionally converges toward public_health_primary in practice while retaining proportionality''s legitimating vocabulary — a false-summit dynamic where the doctrine''s genuine gating function has atrophied into performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_administration_capture, empirical, 'Whether proportionality review remains substantive or has become procedural theater.').

omega_variable(
    objector_exclusion_from_test_scope,
    'Does the proportionality test''s silence on non-empirical (religious/philosophical) objections represent a legitimate scope limitation, or does it structurally launder a bodily-autonomy question into a severity-and-efficacy question the objector never raised?',
    'Legal-philosophical analysis of whether proportionality doctrine can coherently claim jurisdiction over objections it does not evaluate on their own terms; comparison with jurisdictions that carve out conscience exemptions independent of the severity test.',
    'If the exclusion is illegitimate scope-narrowing, the reading''s victim set should include religious/philosophical objectors categorically, not merely as a side effect of failing an inapplicable test — this would push ε and suppression higher for that specific group across all pathogen severities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objector_exclusion_from_test_scope, conceptual, 'Whether the test''s silence on conscience objections is a legitimate scope boundary or a category error.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that mandate_legitimacy_scope has at least three live readings (this proportionality reading, public_health_primary, bodily_autonomy_primary), what determines which reading a given court or jurisdiction actually applies, and is that selection itself principled or outcome-driven?',
    'Comparative jurisprudence analysis across jurisdictions and time periods to see whether reading-selection correlates with case outcome desired rather than with independently justified doctrinal commitment.',
    'If reading-selection is outcome-driven, the entire kernel is less a genuine three-way doctrinal dispute and more a menu from which decision-makers select whichever reading validates their predetermined conclusion — which would suggest all three readings are, at the meta-level, tools of a single underlying extraction/legitimation dynamic rather than independent structural claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the choice among kernel readings is principled or result-oriented.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint, public_health_primary, and bodily_autonomy_primary are three readings of the mandate_legitimacy_scope kernel. They share the same underlying dispute (when is compelled vaccination legitimate) but instantiate structurally distinct constraints: public_health_primary has a fixed, severity-independent victim set (anyone who resists a state-determined necessary mandate); bodily_autonomy_primary has a fixed, universal victim set (anyone subject to non-consensual medical compulsion, regardless of disease parameters); this proportionality_reading has a CONDITIONAL victim set that varies by pathogen severity, vaccine efficacy, and alternative availability. Each carries its own ε: this reading's ε (0.42) is deliberately moderate and pathogen-contingent, distinct from the higher, more categorical ε expected under public_health_primary's unconditional authority claim and the distinct extraction pattern of bodily_autonomy_primary's universal veto framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
