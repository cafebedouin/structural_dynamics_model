% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Proportionality Standard for Legitimate Health Intervention
 *   domain: public health policy / medical ethics / constitutional law
 *
 * SUMMARY:
 *   This story instantiates the proportionality reading of the
 *   legitimate_health_intervention kernel: legitimacy is a function of the
 *   fit between intervention severity and disease severity, with both
 *   population harm and individual autonomy entering the calculus but
 *   weighted differently depending on transmissibility and case-fatality
 *   rate. This is distinct from the public_health_primary reading (which
 *   treats population-level harm reduction as sufficient legitimacy on its
 *   own, making refusal an externality) and the bodily_autonomy_primary
 *   reading (which treats informed consent as a categorical bar regardless of
 *   population benefit). Under this reading, the same underlying kernel —
 *   what makes a health intervention legitimate — produces a conditional,
 *   disease-dependent constraint rather than an unconditional one: a
 *   measles-scale threat licenses far more coercion than a seasonal-flu-scale
 *   threat, and the constraint's victim set changes accordingly. ε is
 *   authored here as moderate (0.42) because the standard genuinely
 *   constrains maximal coercion (unlike public_health_primary) while still
 *   permitting substantial autonomy cost at high severity tiers (unlike
 *   bodily_autonomy_primary) — this value is specific to the proportionality
 *   reading and is not an average of the sibling readings' ε values.
 *
 * KEY AGENTS:
 *   - public_health_authorities: institutional agenda-setter administering the proportionality test
 *   - susceptible_population_high_severity_disease: primary beneficiary of calibrated protection
 *   - individuals_subject_to_low_severity_mandates: payer when severity is miscalibrated upward
 *   - religious_and_philosophical_objectors: payer whose autonomy claim is weighted rather than absolute
 *   - courts_applying_tiered_scrutiny: analytical observer applying the doctrine, dependent on epidemiological inputs it does not generate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.42).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.48).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality Standard for Legitimate Health Intervention").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public health policy / medical ethics / constitutional law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '1010c239-3ae2-414a-87e0-a6323e36690c').
narrative_ontology:cs_kernel_codification('1010c239-3ae2-414a-87e0-a6323e36690c', distributed).
narrative_ontology:cs_authority_grounding('1010c239-3ae2-414a-87e0-a6323e36690c', practice).
narrative_ontology:cs_interpretation_layer_present('1010c239-3ae2-414a-87e0-a6323e36690c').
narrative_ontology:cs_reading_relation('1010c239-3ae2-414a-87e0-a6323e36690c', legitimate_health_intervention__public_health_primary, influences).
narrative_ontology:cs_reading_relation('1010c239-3ae2-414a-87e0-a6323e36690c', legitimate_health_intervention__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('1010c239-3ae2-414a-87e0-a6323e36690c', foundational, severity_conditional_weighting_required).
narrative_ontology:cs_axiom_status(severity_conditional_weighting_required, holdable).
narrative_ontology:cs_axiom_grounding('1010c239-3ae2-414a-87e0-a6323e36690c', severity_conditional_weighting_required, instrumental).
narrative_ontology:cs_axiom('1010c239-3ae2-414a-87e0-a6323e36690c', foundational, no_categorical_priority_between_autonomy_and_population_harm).
narrative_ontology:cs_axiom_status(no_categorical_priority_between_autonomy_and_population_harm, holdable).
narrative_ontology:cs_axiom_grounding('1010c239-3ae2-414a-87e0-a6323e36690c', no_categorical_priority_between_autonomy_and_population_harm, conventional).
narrative_ontology:cs_reference_frame('1010c239-3ae2-414a-87e0-a6323e36690c', jacobson_graduated_police_power_standard).
narrative_ontology:cs_drift_state('1010c239-3ae2-414a-87e0-a6323e36690c', post_covid19_pandemic_litigation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1010c239-3ae2-414a-87e0-a6323e36690c', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, susceptible_population_high_severity_disease).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, courts_applying_tiered_scrutiny).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, individuals_subject_to_low_severity_mandates).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, religious_and_philosophical_objectors).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, populations_under_misclassified_severity_tiers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, vaccine_and_intervention_manufacturers).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, graduated_scrutiny_doctrine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, proportionality_as_constitutional_baseline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classify diseases by transmissibility and case-fatality rate, then calibrate intervention severity (school exclusion, quarantine, mandatory vaccination, fines) to that classification. Administers the proportionality test itself and therefore controls where thresholds are drawn and how contested severity data gets weighted.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Includes immunocompromised people, infants too young to vaccinate, and others who cannot independently protect themselves from high-transmissibility, high-fatality disease. They benefit directly when the proportionality standard permits strong intervention against measles-tier threats, since herd protection depends on population-level compliance they cannot personally compel.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, susceptible_population_high_severity_disease, beneficiary,
    organized, biographical, constrained, national).

% Face intervention (mandatory vaccination, workplace exclusion, movement restriction) calibrated to a disease later shown to be lower-severity than assumed at the time of the mandate. Bear the autonomy cost of an intervention whose proportionality was miscalibrated, with limited recourse until retrospective severity data is available.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individuals_subject_to_low_severity_mandates, payer,
    moderate, biographical, constrained, regional).

% Object to intervention on grounds the proportionality framework does not weight as heavily as measured population harm — their autonomy claim is treated as one input among several rather than a categorical bar. As disease severity rises, their exemption pathways narrow; at high severity tiers they may face exclusion from school, work, or public spaces with no accommodation.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, religious_and_philosophical_objectors, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, religious_and_philosophical_objectors, excluded).

% Bear the cost when the severity classification driving the proportionality calculation is wrong — either overstated (unnecessary coercion) or understated (insufficient protection followed by delayed escalation). Have no independent channel to contest the classification before the intervention is imposed.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, populations_under_misclassified_severity_tiers, payer,
    powerless, biographical, trapped, regional).

% Adjudicate challenges to specific interventions by testing whether severity of the measure matches severity of the threat. The doctrine gives them a workable standard (vindicating graduated scrutiny) that avoids the all-or-nothing binaries of the rival readings, but they depend entirely on the epidemiological data the health authorities supply for the threat-side of the comparison.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, courts_applying_tiered_scrutiny, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, courts_applying_tiered_scrutiny, beneficiary).

% Benefit when high-severity classification drives large-scale mandated uptake of their products, though they do not administer or set the proportionality standard themselves.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, vaccine_and_intervention_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated, defensible standard for calibrating how much coercion a health intervention may use, avoiding the failure modes of both an absolute autonomy veto (which would block interventions against genuinely catastrophic contagion) and an unconditional population-harm mandate (which would license maximal coercion against trivial threats).
% TRANSFER_FUNCTION: Moves autonomy costs onto individuals in proportion to a state-assessed severity score, and moves protection benefits to the susceptible population in proportion to the same score; when the severity assessment is wrong, the transfer is imposed on the wrong party in the wrong amount.
% ABSENT_VOICES: Individuals contesting a specific severity classification in real time rarely have a forum before the intervention is imposed — courts review after the fact, and the epidemiological classification itself is treated as a technical input rather than a contestable claim by the people it binds.
% DISAPPEARANCE_RATIONALE: Without a proportionality standard, courts and legislatures would default to one of the sibling readings — either a categorical autonomy bar that blocks even measles-tier interventions, or an unconditional population-harm standard that licenses maximal coercion for flu-tier threats. Either shift would visibly change which interventions survive legal and political challenge.
% FOUNDING_PROBLEM: Courts and legislatures needed a workable standard to distinguish interventions proportionate to genuine catastrophic contagion risk (smallpox, measles) from interventions disproportionate to comparatively mild seasonal illness, after both categorical-autonomy and categorical-population-harm standards failed in practice — one blocked necessary quarantine measures, the other justified excessive coercion for minor threats.
% FOUNDING_PROBLEM_CORROBORATION: Public health law scholars and constitutional courts outside the executive agencies that administer interventions attest the calibration problem remains active — cited in judicial opinions distinguishing Jacobson-type smallpox mandates from later challenges to lower-severity mandates. Civil liberties organizations, who are not beneficiaries of the standard, corroborate that the underlying tension (autonomy vs. population harm) has not been resolved, only structured.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.42) and suppression (0.48) sit in a moderate band because the proportionality standard is explicitly conditional — it authorizes strong coercion against high-transmissibility, high-fatality disease and comparatively little against mild disease, so its aggregate extractive profile is lower than an unconditional population-harm standard but higher than a pure autonomy standard. Resistance (0.55) is elevated relative to a mountain because the classification step (how severe is this disease, really?) is a genuine site of contest — objectors, courts, and epidemiologists dispute severity data in real time, and that dispute is where most of the constraint's friction lives. Accessibility collapse (0.40) is moderate: once a disease is classified at a given severity tier, the menu of legitimate interventions narrows substantially, but the classification itself remains open to challenge and revision, so full collapse does not occur. The measurements track a slow rise in extractiveness, theater, and suppression-requirement over time as the doctrine matures into settled case law and severity classification becomes more institutionally routinized — increasingly treated as a technical finding rather than a contestable claim.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (public health authorities) and the beneficiary seat (susceptible populations), the doctrine reads as principled calibration — a rope, or close to it, solving a genuine coordination problem without maximal coercion. From the payer seats (objectors, misclassified populations) the same structure reads as tangled: a genuine coordination function (protecting the vulnerable) riding alongside asymmetric extraction (their autonomy cost is discounted by a classification process they cannot contest ex ante). The engine's per-seat computation should reflect this divergence directly from the declared power/exit/scope data, not from any narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities sit near the beneficiary end: they administer the classification, control which interventions the standard licenses, and are institutionally credited when interventions succeed. The susceptible population at high severity tiers is a genuine beneficiary — the standard's conditional structure is precisely what lets strong intervention be justified on their behalf. Individuals subject to low-severity mandates and misclassified populations sit near the target end: they bear the intervention's costs specifically because the severity assessment (which they did not participate in producing) placed them in a higher-coercion tier. Religious and philosophical objectors are structurally trapped payers because their autonomy claim, while doctrinally acknowledged, is weighted rather than dispositive — at high severity it is regularly outweighed.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality standard resists mandatrophy better than either sibling reading precisely because its mandate is explicitly conditional on disease characteristics — it does not persist unconditionally once the founding problem (distinguishing catastrophic from mild threats) is solved; it re-derives legitimacy from current severity data. The founding problem remains live because new pathogens continue to require fresh severity classification, but the corroboration by outside courts and civil liberties scholars indicates the standard's application (not its underlying necessity) is the contested site — misclassification, not obsolescence, is the primary risk this reading names.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    severity_classification_authority,
    'Who has final authority to classify a disease''s severity tier when epidemiological data is contested or preliminary (as in a novel pathogen''s early spread), and does that authority itself introduce a form of unaccountable discretion the proportionality standard is supposed to prevent?',
    'Comparative analysis of judicial deference standards across jurisdictions when reviewing agency severity classifications during outbreak conditions versus settled endemic conditions.',
    'If courts defer nearly completely to agency classification during emergencies, the proportionality standard collapses toward public_health_primary in practice during exactly the periods when its constraining function matters most; if courts independently scrutinize classification even during emergencies, the standard holds its distinct conditional structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_classification_authority, empirical, 'Whether emergency deference erodes the proportionality standard''s distinctness from the public-health-primary reading.').

omega_variable(
    committer_frame_reading_selection,
    'Is the proportionality reading the dominant one in actual constitutional doctrine (as in Jacobson v. Massachusetts and its progeny), or is it a normatively attractive middle position that legal scholars favor but that courts apply inconsistently, sometimes defaulting to public_health_primary language and sometimes to bodily_autonomy_primary language depending on the political salience of the specific disease?',
    'Systematic doctrinal survey of case outcomes across disease types (smallpox/measles mandates vs. flu/HPV mandates) coded for which reading''s reasoning actually drove the holding.',
    'If courts apply proportionality reasoning inconsistently — invoking it as rhetoric while actually defaulting to one of the categorical siblings depending on the disease''s political salience — this story''s claimed_type (tangled_rope) may overstate the doctrine''s actual operative coherence; it may function more as a legitimating veneer over ad hoc severity-dependent politics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_reading_selection, conceptual, 'Whether the proportionality reading is a coherent operative doctrine or a rhetorical overlay on inconsistent case-by-case politics.').

omega_variable(
    victim_set_disease_dependence,
    'Given that the victim set is explicitly disease-dependent (per the expected structural delta), at what severity threshold does the constraint''s classification shift from tangled_rope (genuine coordination plus real extraction) toward something closer to rope (extraction becomes negligible because the intervention genuinely tracks a catastrophic threat) or toward snare (extraction becomes dominant because the severity classification was manufactured or exaggerated)?',
    'Threshold analysis comparing measured ε against independently verified case-fatality-rate and R0 data across a range of historical interventions (smallpox, measles, H1N1, seasonal flu, COVID variants).',
    'Establishing the threshold would let future disease-specific instantiations of this reading be classified with an empirical severity cutoff rather than the moderate composite ε authored here, which necessarily reflects an average across the reading''s conditional range rather than any single disease case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_disease_dependence, empirical, 'Where the disease-severity-dependent victim set crosses classification boundaries within the proportionality reading itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t8, legitimate_health_intervention__proportionality_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(legi_tr_t16, legitimate_health_intervention__proportionality_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__proportionality_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(legi_tr_t32, legitimate_health_intervention__proportionality_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(legi_tr_t40, legitimate_health_intervention__proportionality_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__proportionality_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(legi_be_t16, legitimate_health_intervention__proportionality_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__proportionality_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(legi_be_t32, legitimate_health_intervention__proportionality_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(legi_be_t40, legitimate_health_intervention__proportionality_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__proportionality_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement(legi_su_t16, legitimate_health_intervention__proportionality_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__proportionality_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement(legi_su_t32, legitimate_health_intervention__proportionality_reading, suppression_requirement, 32, 0.46).
narrative_ontology:measurement(legi_su_t40, legitimate_health_intervention__proportionality_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimate_health_intervention kernel, decomposed per the ε-invariance principle because the natural-language concept 'what makes a health intervention legitimate' covers structurally distinct claims with different ε profiles. public_health_primary treats population harm as sufficient legitimacy (higher ε, unconditional); bodily_autonomy_primary treats consent as a categorical bar (lower ε from the coercion side, but forecloses interventions the proportionality reading would permit); this reading (proportionality) sits between them with a disease-conditional ε. All three are linked bidirectionally in commentary and network fields; each carries its own stakeholders, metrics, and claimed_type rather than one story with a severity parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
