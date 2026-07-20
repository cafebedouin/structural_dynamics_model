% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity-of-Life Prohibition on Assisted Dying (Sanctity Reading)
 *   domain: medical ethics/bioethics/end-of-life policy
 *
 * SUMMARY:
 *   This constraint instantiates the sanctity reading of the
 *   end_of_life_authority kernel: the claim that intrinsic value of human
 *   life imposes a categorical prohibition on intentional life-ending
 *   regardless of individual preference. Enacted through criminal law and
 *   medical licensing, it is presented as protecting vulnerable populations
 *   from coercion into premature death. The structural delta of this reading
 *   is that the very groups it claims to protectâelderly, disabled,
 *   economically disadvantagedâenter the victim set, forced to endure
 *   prolonged suffering or resort to violent means while their autonomy is
 *   extracted by the enforcement apparatus.
 *
 * KEY AGENTS:
 *   - state_criminal_justice_apparatus: Primary agenda-setter (institutional/constrained) â enforces criminal prohibition
 *   - medical_licensing_boards: Secondary agenda-setter (institutional/constrained) â professionalizes the prohibition
 *   - institutional_religious_authority: Primary beneficiary (organized/mobile) â moral doctrine reflected in state policy
 *   - medical_preservationist_establishment: Secondary beneficiary (institutional/constrained) â gains simplified liability boundaries
 *   - terminally_ill_sufferers: Primary target (powerless/trapped) â bears extraction of autonomy and end-of-life agency
 *   - pressured_vulnerable_groups: Secondary target (powerless/trapped) â nominally protected, actually harmed
 *   - physicians: Constrained payer (moderate/constrained) â professional autonomy overridden
 *   - autonomy_advocacy_groups: Excluded voice (moderate/constrained) â kept out of policy rooms
 *   - bioethics_analyst: Analytical observer (analytical/analytical) â sees cross-jurisdictional structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.78).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.83).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.83).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity-of-Life Prohibition on Assisted Dying (Sanctity Reading)").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical ethics/bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, 'bb1e1b16-8a41-44fa-a67b-7dff6bc66f99').
narrative_ontology:cs_kernel_codification('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99', fixed_text).
narrative_ontology:cs_authority_grounding('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99', lineage).
narrative_ontology:cs_interpretation_layer_present('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99').
narrative_ontology:cs_reading_relation('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99', foundational, intrinsic_value_human_life).
narrative_ontology:cs_axiom_status(intrinsic_value_human_life, holdable).
narrative_ontology:cs_axiom_grounding('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99', intrinsic_value_human_life, deontological).
narrative_ontology:cs_axiom('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99', foundational, categorical_prohibition_intentional_killing).
narrative_ontology:cs_axiom_status(categorical_prohibition_intentional_killing, holdable).
narrative_ontology:cs_axiom_grounding('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99', categorical_prohibition_intentional_killing, deontological).
narrative_ontology:cs_reference_frame('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99', classical_preservationist_ethics).
narrative_ontology:cs_drift_state('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99', contemporary_autonomy_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('bb1e1b16-8a41-44fa-a67b-7dff6bc66f99', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, institutional_religious_authority).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, medical_preservationist_establishment).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_sufferers).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, pressured_vulnerable_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, physicians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces criminal prohibitions on assisted suicide and euthanasia through prosecution, sentencing, and penal law. Maintains the legal architecture that makes intentional life-ending by physicians a crime regardless of patient consent.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, state_criminal_justice_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Set and enforce professional standards that limit physicians to life-preserving and palliative interventions. Revoke licenses and impose professional sanctions on physicians who participate in assisted dying, even where patients request it.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, medical_licensing_boards, agenda_setter,
    institutional, generational, constrained, national).

% Doctrine of intrinsic human dignity and sanctity of life is reflected in secular law and medical ethics codes. Gains social legitimacy, institutional influence, and policy deference when the state enforces their moral framework without requiring them to administer the constraint.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, institutional_religious_authority, beneficiary,
    organized, generational, mobile, national).

% Hospitals, hospice chains, and medical institutions committed to absolute life-preservation benefit from bright-line moral boundaries that simplify end-of-liability, reduce decision complexity, and shield them from malpractice ambiguity in death-timing choices.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, medical_preservationist_establishment, beneficiary,
    institutional, generational, constrained, national).

% Denied legal access to medical assistance in dying; forced to endure prolonged physical and existential suffering or to resort to violent, unreliable, or socially isolating methods of self-destruction. Travel to permissive jurisdictions is often physically impossible due to illness or economically prohibitive.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, terminally_ill_sufferers, payer,
    powerless, immediate, trapped, local).

% Elderly, disabled, and economically disadvantaged persons who are nominally protected by the prohibition but bear its costs through loss of agency, extended unwanted existence, and inability to access desired end-of-life options. Their structural vulnerability is cited as the justification for the constraint that harms them.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pressured_vulnerable_groups, payer,
    powerless, immediate, trapped, local).

% Professional medical judgment and conscience are constrained by categorical prohibition; unable to honor persistent patient requests for assisted dying. Many experience moral distress when palliative measures fail; some engage in covert terminal sedation or withhold treatment, risking prosecution or professional ruin.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, physicians, payer,
    moderate, biographical, constrained, national).

% Advocate for patient self-determination and right-to-die legislation. Structurally excluded from medical ethics boards, licensing committees, and legislative drafting processes dominated by preservationist institutional voices.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, autonomy_advocacy_groups, excluded,
    moderate, biographical, constrained, national).

% Observes the structural asymmetry between the sanctity reading and autonomy reading of end-of-life authority. Tracks empirical outcomes across jurisdictions, notes divergence between claimed protection and measured harm to vulnerable populations.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, bioethics_analyst, observer,
    analytical, civilizational, analytical, universal).

narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the coercion of vulnerable individuals into premature death by removing assisted dying as a legal and medical option, thereby eliminating a channel through which familial, economic, or institutional pressure might operate.
% TRANSFER_FUNCTION: Moves the authority to determine the timing and manner of death from the individual patient and their treating physician to a categorical moral-legal prohibition and its enforcement apparatus.
% ABSENT_VOICES: Terminally ill individuals seeking release, disability-rights advocates who support autonomy over non-autonomy, and physicians who would provide assisted dying if permitted are largely excluded from policy frameworks dominated by preservationist institutional voices.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, jurisdictions would establish assisted-dying regulatory frameworks, physician roles would expand to include life-ending options, medical training and liability would reorient around safeguarded autonomy, and the institutional authority of preservationist medical and religious bodies would shift toward palliative and patient-centered models.
% FOUNDING_PROBLEM: The vulnerability of sick, elderly, disabled, and economically disadvantaged persons to subtle or overt pressure to end their lives; the risk that medical power over life and death could be abused or incrementally expanded beyond competent, consenting, terminal cases.
% FOUNDING_PROBLEM_CORROBORATION: Disability-rights organizations and elder-abuse advocates attest the vulnerability problem from outside the beneficiary set; terminal-illness advocacy groups and empirical public-health researchers from jurisdictions with assisted-dying regimes contest its magnitude and attest low coercion rates, corroborating a shifted-function reading from outside the preservationist institutions.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint forcibly extracts the fundamental autonomy of death-timing from suffering individuals, transferring it to a categorical prohibition. Suppression is higher (0.83) because persistence depends on criminal penalties, professional license revocation, and active exclusion of alternative frameworks; without this enforcement, physician practice and patient demand would rapidly restructure. Theater ratio is moderate-high (0.45) because a growing share of preservationist discourse is performative defense of institutional moral boundaries rather than genuine protection of vulnerable persons. Accessibility collapse is high (0.80): once the sanctity frame is codified, legal assisted-dying alternatives collapse entirely for those under the jurisdiction. Resistance is moderate (0.58): significant autonomy movements and jurisdictional defections exist but are unevenly distributed.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (criminal justice apparatus, licensing boards) and beneficiary seats (religious authority, preservationist institutions) experience the constraint as necessary moral coordination protecting society's most vulnerable. The payer seats (terminally ill, vulnerable groups, physicians) experience the same structure as enforced extraction that denies agency and prolongs suffering. The engine computes this divergence from the identical structural data; the divergence itself is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations paired with exit options. Religious authority and preservationist establishments have mobile or constrained exit but are declared beneficiariesâ their directionality sits near the beneficiary end, damping effective extraction. Terminally ill sufferers and pressured vulnerable groups are declared victims with trapped exitâ their directionality sits near the full-target end, amplifying effective extraction. Physicians are payers with constrained exit, placing them at moderate-to-high directionality. The structural asymmetry between trapped victims and mobile beneficiaries drives the seat-divergent classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare preserves the genuine coordination function that motivates the constraint: the protection of vulnerable persons from coercion is a real problem, and a society that solved it would have accomplished something. A snare classification would erase that, claiming the coordination story is pure cover. A rope classification would erase the victim set, claiming the arrangement is pure coordination. The tangled_rope classification captures that both are true through the same structure: the vulnerable are nominally coordinated (protected) and actually targeted (harmed), requiring active enforcement to hold the contradiction in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_or_constructed_doctrine,
    'Is the sanctity of human life a discovered natural law independent of human institutions, or a constructed doctrinal position maintained by specific religious and medical authorities?',
    'Comparative historical analysis: if the prohibition vanishes or transforms whenever the enforcing institutional authority weakens, the constraint is constructed; if it persists invariant to institutional change, it approaches natural-law status.',
    'If constructed, the constraint reclassifies toward snare/tangled_rope and its mountain-like immunity dissolves; if natural-law, the high extractiveness and victim declarations indicate a false-summit misappropriation of naturality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_constructed_doctrine, conceptual, 'Whether sanctity is discovered or constructed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of assisted-dying requests sustained primarily by structural enforcement (criminal law, licensing sanctions) or by internalized norms among physicians and patients?',
    'Post-legalization trajectory analysis in jurisdictions that have removed prohibition: if requests and provisions rise sharply after decriminalization, suppression was primarily structural; if rates remain low due to professional reticence, suppression is partially internalized.',
    'Internalized suppression would mean effective extraction exceeds the structural measure, as the constraint persists even after formal repeal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    vulnerability_protection_vs_extraction,
    'Does the categorical prohibition actually protect vulnerable groups from coercion into premature death, or does it harm them by removing agency and prolonging suffering?',
    'Empirical comparison of elder-abuse, suicide, and end-of-life distress outcomes between jurisdictions with prohibition and jurisdictions with safeguarded assisted-dying frameworks.',
    'If vulnerability outcomes are worse under prohibition, the coordination story collapses and the constraint shifts toward pure snare; if better, the tangled-rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_protection_vs_extraction, empirical, 'Whether vulnerable groups are protected or harmed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__sanctity_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__sanctity_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__sanctity_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(end__tr_t50, end_of_life_authority__sanctity_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__sanctity_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__sanctity_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__sanctity_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(end__be_t50, end_of_life_authority__sanctity_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__sanctity_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__sanctity_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__sanctity_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.81).
narrative_ontology:measurement(end__su_t50, end_of_life_authority__sanctity_reading, suppression_requirement, 50, 0.83).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the end_of_life_authority kernel, instantiating the sanctity reading. The kernel decomposes into at least three structurally distinct claims: autonomy_reading (individual right to control death), sanctity_reading (categorical prohibition on intentional life-ending), and slippery_slope_mechanism (empirical prediction of regime expansion). Each reading has different epsilon values, beneficiary/victim structures, and classification profiles. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
