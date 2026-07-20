% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__developmental_potentiality_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Developmental Potentiality Reading of Legal Personhood Boundary
 *   domain: legal/constitutional/rights_theory
 *
 * SUMMARY:
 *   This constraint instantiates the developmental_potentiality_reading of
 *   the legal_personhood_boundary kernel. It fixes moral and legal personhood
 *   at the moment of conception, constructing the fetus as a full
 *   rights-bearer and subordinating pregnant persons' autonomy to that
 *   status. The state acquires expansive enforcement authority over pregnancy
 *   outcomes, while medical providers are conscripted into surveillance and
 *   criminalized for standard care. The constraint presents itself as solving
 *   the boundary problem of human moral status, but its operation
 *   asymmetrically extracts bodily autonomy and medical discretion from
 *   pregnant persons and providers.
 *
 * KEY AGENTS:
 *   - Pregnant persons: Primary targets (powerless/trapped) â bear extraction via subordinated autonomy and criminalized healthcare choices.
 *   - Fetus: Structurally juridified entity (powerless/trapped) â entered into the victim set as a passive legal object instrumentalized by the enforcement framework.
 *   - Medical providers: Secondary targets (moderate/constrained) â lose professional discretion and face criminalization.
 *   - State enforcement apparatus: Agenda-setter and gain capturer (institutional/analytical) â sets the boundary and captures expanded biopolitical authority.
 *   - Pro-life governance coalition: Beneficiary (organized/mobile) â collects political and cultural vindication without administering enforcement.
 *   - Bioethical analysts: Analytical observers (analytical/analytical) â document internal contradictions and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.82).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.85).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Developmental Potentiality Reading of Legal Personhood Boundary").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal/constitutional/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, 'd91da606-64c2-421f-8486-92857b306adc').
narrative_ontology:cs_kernel_codification('d91da606-64c2-421f-8486-92857b306adc', formalized).
narrative_ontology:cs_authority_grounding('d91da606-64c2-421f-8486-92857b306adc', lineage).
narrative_ontology:cs_interpretation_layer_present('d91da606-64c2-421f-8486-92857b306adc').
narrative_ontology:cs_reading_relation('d91da606-64c2-421f-8486-92857b306adc', legal_personhood_boundary__restrictive_anthropocentric_reading, coexists_with).
narrative_ontology:cs_reading_relation('d91da606-64c2-421f-8486-92857b306adc', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('d91da606-64c2-421f-8486-92857b306adc', foundational, conception_constitutes_personhood).
narrative_ontology:cs_axiom_status(conception_constitutes_personhood, holdable).
narrative_ontology:cs_axiom_grounding('d91da606-64c2-421f-8486-92857b306adc', conception_constitutes_personhood, deontological).
narrative_ontology:cs_axiom('d91da606-64c2-421f-8486-92857b306adc', foundational, potentiality_entails_full_moral_status).
narrative_ontology:cs_axiom_status(potentiality_entails_full_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('d91da606-64c2-421f-8486-92857b306adc', potentiality_entails_full_moral_status, deontological).
narrative_ontology:cs_reference_frame('d91da606-64c2-421f-8486-92857b306adc', developmental_potentiality_framework).
narrative_ontology:cs_drift_state('d91da606-64c2-421f-8486-92857b306adc', contemporary_constitutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d91da606-64c2-421f-8486-92857b306adc', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, pro_life_governance_coalition).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, fetus).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, medical_providers).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, conception_as_moral_origin).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the full structural cost of the personhood boundary: bodily autonomy is subordinated to fetal rights, abortion and certain contraceptions are criminalized, and pregnancy outcomes are subject to state surveillance and medical coercion. Geographic exit is increasingly constrained by criminalization of travel for care and extraterritorial prosecution.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    powerless, biographical, trapped, national).

% Declared a full rights-bearer from conception but lacks capacity to exercise those rights; its personhood status is invoked by state and advocacy actors to justify constraints on others. It is juridified without voice or exit, instrumentalized as a passive legal object within an enforcement framework that operates in its name but does not serve its agency.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetus, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__developmental_potentiality_reading, fetus).

% Criminalized for providing abortion and certain forms of reproductive care; forced to prioritize fetal personhood claims over patient autonomy and medical ethics; act as de facto state agents in pregnancy surveillance; face professional delicensing and imprisonment.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, medical_providers, payer,
    moderate, biographical, constrained, national).

% Sets the legal personhood boundary at conception and enforces it through criminal law, family courts, and medical regulation. Acquires expansive biopolitical authority to surveil pregnancies, adjudicate maternal conduct, and intervene in healthcare decisions. The authority itself is the primary extraction captured by this seat.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus, beneficiary).

% Achieves institutional vindication of its moral framework; gains political power, policy influence, and cultural hegemony from the constraint's operation. Does not administer enforcement directly but collects the legitimating benefits of the fetal personhood norm.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pro_life_governance_coalition, beneficiary,
    organized, generational, mobile, national).

% Document the internal contradictions of attributing full personhood to entities without cognitive capacity; analyze the constraint's extraction from pregnant persons and its instability as a legal framework.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, bioethical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__developmental_potentiality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes the boundary of the human moral community at conception, providing a bright-line rule for rights allocation and eliminating uncertainty about when legal protection begins for developing human organisms.
% TRANSFER_FUNCTION: Moves authority over pregnancy outcomes and bodily autonomy from pregnant persons to the state; moves medical discretion from providers to criminal law; moves political and cultural capital to pro-life governance coalitions.
% ABSENT_VOICES: Pregnant persons seeking termination are structurally excluded from the personhood framework; their autonomy claims are subordinated to the fetal rights narrative. Functional-capacity and restrictive-anthropocentric readings are excluded from the legal framework in jurisdictions where this reading dominates.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, abortion law would revert to competing readings or default frameworks; state authority over pregnancy would contract dramatically; medical providers would regain professional discretion; the biopolitical architecture built around conception-as-origin would require wholesale reconstruction.
% FOUNDING_PROBLEM: Uncertainty about the moral and legal boundary of human personhood, and the perceived need to protect nascent human life from destruction.
% FOUNDING_PROBLEM_CORROBORATION: Pro-life institutions and theological authorities attest the problem is live. Reproductive rights organizations, critical bioethicists, and affected medical associations attest the problem has been repurposed for biopolitical control; their testimony comes from outside the beneficiary set. Courts have issued contradictory findings on the question, corroborating contested status.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__developmental_potentiality_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint extracts fundamental bodily autonomy and medical privacy from pregnant persons. Suppression (0.85) is higher still because the arrangement requires active criminal enforcement, surveillance, and provider conscription to persist. Theater ratio (0.45) is moderate: genuine moral coordination exists for the beneficiary coalition, but a growing share of enforcement activity is performativeâfocused on prosecuting individuals rather than supporting gestational outcomes. Accessibility collapse (0.78) is high because the bright-line conception rule collapses alternative boundary-drawing frameworks once institutionalized. Resistance (0.72) is high due to sustained reproductive rights movements, medical professional opposition, and jurisprudential dissent. Metrics are authored independently of the claimed type; the engine will measure the gap between the tangled_rope claim and the extracted profile.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience this constraint as necessary moral coordination that protects innocent human life. The payer seats experience it as state-enforced subordination of their autonomy to a legal construct they cannot contest. The engine computes this divergence from the structural data: the state and pro-life coalition sit near the beneficiary end of directionality, while pregnant persons, providers, and the juridified fetus sit near the full-target end.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (pro-life governance coalition, state enforcement apparatus) derive low directionality because the constraint subsidizes their authority and political goals. Victims (pregnant persons, medical providers, fetus) derive high directionality because the constraint extracts autonomy, professional discretion, and juridical agency from them. The fetus, though nominally protected, is assigned victim status because its personhood is purely instrumentalâinvoked to constrain others without conferring actionable capacity on the fetus itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both a genuine coordination function (solving the personhood boundary problem for a specific moral community) and declared victims (pregnant persons and providers who bear the asymmetric cost). A pure coordination reading would fail the tangled rope gate because it would lack victims; a pure extraction reading would fail because it would lack the coordination function that genuinely organizes the pro-life community. The presence of both elements, plus active enforcement, is what makes tangled rope the structurally accurate claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    juridified_fetus_status,
    'Does the fetus function structurally as a beneficiary of protection or as a victim of juridification and instrumentalization within this framework?',
    'Comparative legal analysis of jurisdictions with this reading: if fetal personhood correlates with improved gestational health outcomes and resource transfer to pregnant persons, beneficiary status is supported; if it correlates primarily with maternal criminalization and state surveillance without corresponding support, instrumentalization/victim status is supported.',
    'If the fetus is primarily instrumentalized, the constraint''s victim set is larger than its beneficiary set, pushing classification toward snare; if the fetus is a genuine beneficiary, the asymmetry is slightly reduced but still severe due to the burden on pregnant persons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(juridified_fetus_status, conceptual, 'Ambiguity of fetal structural position as beneficiary or victim of juridification').

omega_variable(
    founding_problem_authenticity,
    'Is the constraint''s founding problem genuinely the protection of nascent human life, or has the arrangement been repurposed for biopolitical control and reproductive governance?',
    'Historical institutional analysis tracing the funding, enforcement priorities, and legislative agendas of the beneficiary coalitions; compare rhetorical emphasis on life protection with material outcomes for infant mortality, maternal health, and family support.',
    'If repurposed, the founding_problem_status shifts to dead and the constraint exhibits mandatrophy or piton dynamics; if authentic, the coordination function remains live despite asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_authenticity, empirical, 'Whether the constraint serves its stated founding problem or has shifted to extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression purely structural (legal penalties, criminalization, geographic barriers), or does it include internalized normative compliance (self-policing by pregnant persons and providers)?',
    'Post-legalization behavior studies in jurisdictions that removed the constraint: if utilization of reproductive services surges beyond what structural availability predicts, internalized suppression was significant; if utilization tracks structural availability closely, suppression was primarily external.',
    'If internalized suppression is substantial, effective extractiveness exceeds the structural measure because targets carry the constraint with them after formal exit is available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, functional_capacity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legal_personhood_boundary kernel. The kernel decomposes into three structurally distinct constraints because each reading produces a different epsilon, different victim/beneficiary structures, and different enforcement architectures. The developmental_potentiality reading is linked to its siblings as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
