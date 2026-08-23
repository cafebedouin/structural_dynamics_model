% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity-of-Life Prohibition on Intentional Life-Termination
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the sanctity_primary reading of the
 *   dignified_death kernel: the normative claim that human dignity resides in
 *   life's intrinsic value, making intentional life-termination a violation
 *   of transcendent moral law regardless of consent. Where this reading
 *   dominates law and medical ethics, the prohibition on assisted dying and
 *   euthanasia operates as a snare. The protection norm genuinely coordinates
 *   a shared moral order for the benefiting community, but it asymmetrically
 *   extracts prolonged suffering from vulnerable populations (elderly,
 *   disabled, poor) who cannot access legal release. The metric profile
 *   reflects this extraction: high base_extractiveness (0.62), high
 *   suppression (0.75), and rising theater_ratio (0.45) as enforcement
 *   increasingly performs moral boundary-maintenance rather than protecting
 *   identifiable individuals from non-consensual killing. The claimed type is
 *   snare; the metrics are authored independently to describe the
 *   constraint's actual operation.
 *
 * KEY AGENTS:
 *   - sanctity_advocacy_network: Primary agenda-setter/beneficiary (institutional/national/constrained) â derives moral authority from the absolute prohibition.
 *   - medical_establishment: Secondary agenda-setter/beneficiary (institutional/national/constrained) â enforces the boundary through professional ethics and licensing.
 *   - legal_enforcement_system: Tertiary agenda-setter (institutional/national/constrained) â codifies and prosecutes violations.
 *   - vulnerable_patients: Primary target (powerless/local/trapped) â bears the cost of prolonged suffering and denied agency.
 *   - autonomy_advocates: Excluded voice (organized/national/mobile) â structurally absent from sanctity-dominated policy forums.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.62).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.75).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity-of-Life Prohibition on Intentional Life-Termination").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, '99b64810-3323-4482-94f3-0f78f0cf6d5b').
narrative_ontology:cs_kernel_codification('99b64810-3323-4482-94f3-0f78f0cf6d5b', fixed_text).
narrative_ontology:cs_authority_grounding('99b64810-3323-4482-94f3-0f78f0cf6d5b', lineage).
narrative_ontology:cs_interpretation_layer_present('99b64810-3323-4482-94f3-0f78f0cf6d5b').
narrative_ontology:cs_reading_relation('99b64810-3323-4482-94f3-0f78f0cf6d5b', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('99b64810-3323-4482-94f3-0f78f0cf6d5b', dignified_death__relational_autonomy, forecloses).
narrative_ontology:cs_axiom('99b64810-3323-4482-94f3-0f78f0cf6d5b', foundational, intrinsic_value_non_waivable).
narrative_ontology:cs_axiom_status(intrinsic_value_non_waivable, holdable).
narrative_ontology:cs_axiom_grounding('99b64810-3323-4482-94f3-0f78f0cf6d5b', intrinsic_value_non_waivable, deontological).
narrative_ontology:cs_axiom('99b64810-3323-4482-94f3-0f78f0cf6d5b', foundational, intentional_termination_always_wrong).
narrative_ontology:cs_axiom_status(intentional_termination_always_wrong, holdable).
narrative_ontology:cs_axiom_grounding('99b64810-3323-4482-94f3-0f78f0cf6d5b', intentional_termination_always_wrong, deontological).
narrative_ontology:cs_reference_frame('99b64810-3323-4482-94f3-0f78f0cf6d5b', absolute_prohibition_framework).
narrative_ontology:cs_drift_state('99b64810-3323-4482-94f3-0f78f0cf6d5b', contemporary_bioethics_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('99b64810-3323-4482-94f3-0f78f0cf6d5b', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, sanctity_advocacy_network).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, medical_establishment).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, vulnerable_patients).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, absolute_prohibition_against_killing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious and moral institutions that advance the doctrine of life's intrinsic sanctity. They set the ideological and legislative agenda against assisted dying, derive moral authority and institutional legitimacy from maintaining an absolute boundary against intentional killing, and resist any erosion of the prohibition as metaphysically catastrophic.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, sanctity_advocacy_network, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, sanctity_advocacy_network, beneficiary).

% Professional medical bodies and licensing authorities that enforce the prohibition through ethics codes, institutional policy, and professional discipline. They benefit from a clear ethical boundary that distinguishes healing from killing, and they actively suppress medical participation in assisted dying.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, medical_establishment, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, medical_establishment, beneficiary).

% State criminal justice and regulatory apparatus that codifies the prohibition in homicide and assisted-suicide statutes, investigates violations, and prosecutes clinicians or helpers who assist in life-termination.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, legal_enforcement_system, agenda_setter,
    institutional, generational, constrained, national).

% Elderly, disabled, impoverished, and terminally ill individuals who are legally and medically barred from accessing assisted dying. They bear the cost of prolonged suffering, invasive life-sustaining treatment, and the psychological burden of a trapped dying process, with no lawful exit available.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, vulnerable_patients, payer,
    powerless, immediate, trapped, local).

% Patient-rights organizations, disability-rights groups supporting choice, and secular bioethicists who argue for self-determination in dying. They are structurally excluded from policy-making in sanctity-dominated jurisdictions; their arguments are treated as morally illegitimate and are rarely given parity in legislative or clinical forums.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, autonomy_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates communal moral order and institutional ethical clarity by establishing an absolute, non-waivable boundary against intentional killing, preventing perceived moral hazard and protecting a shared metaphysical commitment to the intrinsic value of human life.
% TRANSFER_FUNCTION: Transfers the cost of prolonged dying and suffering onto vulnerable individuals; transfers moral authority, institutional legitimacy, and social cohesion to sanctity-advocacy networks and the medical-legal apparatus that enforces the prohibition.
% ABSENT_VOICES: Terminally ill patients who desire death, impoverished families witnessing prolonged suffering, and disability-rights advocates who support autonomous choice are excluded from policy design; their preferences are filtered through frameworks that treat a wish to die as irrational or morally void.
% DISAPPEARANCE_RATIONALE: If the sanctity prohibition vanished, jurisdictions would move to legalize and regulate assisted dying, medical practice would reorganize around patient-centered end-of-life options, vulnerable patients would gain lawful exit from coerced suffering, and the moral authority of sanctity institutions would sharply decline; the medical-legal boundary between killing and allowing-to-die would require fundamental renegotiation.
% FOUNDING_PROBLEM: Protecting human life from utilitarian devaluation, maintaining an absolute societal boundary against murder, and preventing state or medical power from being turned toward systematic killing of the vulnerable.
% FOUNDING_PROBLEM_CORROBORATION: Sanctity advocates and religious authorities attest the problem remains live, citing slippery-slope risks. Secular bioethicists, patient-rights organizations, and empirical researchers from outside the benefiting parties attest that non-consensual killing is already prohibited and the arrangement now inflicts harm on consenting sufferers; data from jurisdictions with legalized assisted dying corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.62 because the constraint systematically transfers the cost of dying onto trapped patients while the protection function is increasingly abstract. Suppression is 0.75 because the constraint's persistence depends on criminal law, professional discipline, and the active exclusion of rival frameworks (autonomy_primary). Theater_ratio at 0.45 reflects that a growing share of enforcement activity performs moral boundary-maintenance rather than preventing concrete harm to non-consenting parties. Accessibility_collapse is high (0.78) because once the sanctity framework is codified, lawful alternatives collapse for patients within the jurisdiction. Resistance at 0.60 captures sustained opposition from autonomy movements and some medical professionals. The temporal series show monotonic extraction accumulation from 1980â2020 as bioethical debate matured and enforcement hardened.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (sanctity institutions, medical establishment, legal system) experience the constraint as necessary moral coordination that preserves civilization against killing; their directionality is near the beneficiary end. The payer seat (vulnerable_patients) experiences the same structure as coerced prolongation of suffering; their directionality is near the full-target end. The excluded seat (autonomy_advocates) experiences the constraint as a suppressed alternative. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to sanctity_advocacy_network and medical_establishment: they collect moral authority, institutional legitimacy, and social coordination from the prohibition. Victim declarations map to vulnerable_patients: they bear the concrete cost of prolonged dying. The exit asymmetry is starkâinstitutional agenda-setters are constrained by doctrine but remain powerful, while vulnerable patients are trapped (no legal exit, often physically and economically unable to travel to permissive jurisdictions). This drives the engine's directionality derivation toward low d for beneficiaries and high d for victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as snare rather than tangled_rope or rope prevents mislabeling the moral coordination as benign. While the constraint does coordinate a shared moral order (a genuine public good for the benefiting community), the coordination story is cover for asymmetric extraction: the same structure that generates communal solidarity also traps vulnerable individuals in suffering. The absence of a sunset clause, the absence of meaningful opt-out for patients, and the active suppression of autonomy-based alternatives mark this as extraction dominant, not coordination dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protection_vs_prolongation_boundary,
    'Is the sanctity prohibition a genuine protection of vulnerable life against utilitarian erosion, or does it functionally prolong suffering without compensatory benefit?',
    'Comparative outcome analysis across jurisdictions: measuring suffering duration, patient autonomy, and vulnerable-group safety under prohibition vs. safeguarded legalization.',
    'If the prohibition prolongs suffering without protecting vulnerable groups from non-consensual killing, the constraint is a snare extracting suffering for moral order. If it genuinely prevents systematic devaluation, the extraction reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_vs_prolongation_boundary, conceptual, 'Whether the constraint''s protection function is real or cover for extraction.').

omega_variable(
    coercion_under_legalization_empirical,
    'Do vulnerable populations experience greater coercion from family or institutional pressure in jurisdictions with legalized assisted dying or under total prohibition?',
    'Empirical studies from Oregon, Netherlands, Canada, and Belgium measuring reported pressure, depression rates, and desire-to-die stability among elderly, disabled, and poor populations.',
    'If coercion is higher under prohibition (via medical paternalism or forced suffering), the sanctity narrative''s protection claim is undermined and the snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_under_legalization_empirical, empirical, 'Empirical locus of coercion for vulnerable groups across regulatory regimes.').

omega_variable(
    sanctity_kernel_reading_location,
    'This constraint is the sanctity_primary reading of the dignified_death kernel. Its core axiom (intrinsic value non-waivable) forecloses both autonomy_primary and relational_autonomy readings. Does this foreclosure hold across all institutional frameworks or only within strict deontological commitment systems?',
    'Doctrinal analysis of whether hybrid frameworks (e.g., professional conscience clauses within legal prohibition) can simultaneously hold sanctity and autonomy premises without contradiction.',
    'If foreclosure is absolute, the kernel is irreconcilable and political conflict is zero-sum. If coexistence is possible in hybrid frameworks, the forecloses relation should be downgraded to influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctity_kernel_reading_location, conceptual, 'Commitment-system location of this reading within the dignified_death kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dignified_death_sanctity_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dignified_death_sanctity_tr_t10, dignified_death__sanctity_primary, theater_ratio, 10, 0.3).
narrative_ontology:measurement(dignified_death_sanctity_tr_t20, dignified_death__sanctity_primary, theater_ratio, 20, 0.35).
narrative_ontology:measurement(dignified_death_sanctity_tr_t30, dignified_death__sanctity_primary, theater_ratio, 30, 0.4).
narrative_ontology:measurement(dignified_death_sanctity_tr_t40, dignified_death__sanctity_primary, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(dignified_death_sanctity_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dignified_death_sanctity_be_t10, dignified_death__sanctity_primary, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(dignified_death_sanctity_be_t20, dignified_death__sanctity_primary, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(dignified_death_sanctity_be_t30, dignified_death__sanctity_primary, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(dignified_death_sanctity_be_t40, dignified_death__sanctity_primary, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(dignified_death_sanctity_su_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(dignified_death_sanctity_su_t10, dignified_death__sanctity_primary, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(dignified_death_sanctity_su_t20, dignified_death__sanctity_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(dignified_death_sanctity_su_t30, dignified_death__sanctity_primary, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(dignified_death_sanctity_su_t40, dignified_death__sanctity_primary, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
