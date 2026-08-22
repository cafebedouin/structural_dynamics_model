% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__potential_based_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood Boundary — Potential-for-Rational-Agency Reading
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the potential-based reading of the
 *   personhood_boundary kernel: moral standing is grounded in the potential
 *   for rational agency, such that severely disabled infants judged to lack
 *   this potential fall outside the circle of protected persons. The reading
 *   emerged in 1970s bioethics (Tooley, Singer, Engelhardt) as a response to
 *   neonatology's new capacity to sustain severely impaired infants. It
 *   functions as a tangled rope: it coordinates genuine medical
 *   decision-making (the coordination function) while extracting the lives of
 *   a defined victim class (severely disabled infants without potential) for
 *   the benefit of institutional and parental decision-makers. The constraint
 *   requires active enforcement through hospital ethics committees, legal
 *   frameworks (Baby Doe amendments, state statutes), and professional
 *   guidelines that operationalize the potentiality test.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.78).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.72).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood Boundary — Potential-for-Rational-Agency Reading").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, 'b87b2b12-6829-49ba-818f-48604547be59').
narrative_ontology:cs_kernel_codification('b87b2b12-6829-49ba-818f-48604547be59', distributed).
narrative_ontology:cs_authority_grounding('b87b2b12-6829-49ba-818f-48604547be59', extraction).
narrative_ontology:cs_interpretation_layer_present('b87b2b12-6829-49ba-818f-48604547be59').
narrative_ontology:cs_reading_relation('b87b2b12-6829-49ba-818f-48604547be59', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('b87b2b12-6829-49ba-818f-48604547be59', personhood_boundary__fitness_contingent_reading, coexists_with).
narrative_ontology:cs_axiom('b87b2b12-6829-49ba-818f-48604547be59', foundational, rational_agency_potential_grounds_moral_status).
narrative_ontology:cs_axiom_status(rational_agency_potential_grounds_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('b87b2b12-6829-49ba-818f-48604547be59', rational_agency_potential_grounds_moral_status, deontological).
narrative_ontology:cs_axiom('b87b2b12-6829-49ba-818f-48604547be59', foundational, severely_disabled_infants_lack_rational_agency_potential).
narrative_ontology:cs_axiom_status(severely_disabled_infants_lack_rational_agency_potential, holdable).
narrative_ontology:cs_axiom_grounding('b87b2b12-6829-49ba-818f-48604547be59', severely_disabled_infants_lack_rational_agency_potential, empirically_contingent).
narrative_ontology:cs_axiom('b87b2b12-6829-49ba-818f-48604547be59', secondary, parental_medical_authority_to_assess_potentiality).
narrative_ontology:cs_axiom_status(parental_medical_authority_to_assess_potentiality, holdable).
narrative_ontology:cs_axiom_grounding('b87b2b12-6829-49ba-818f-48604547be59', parental_medical_authority_to_assess_potentiality, conventional).
narrative_ontology:cs_reference_frame('b87b2b12-6829-49ba-818f-48604547be59', pre_neonatal_intensive_care_ethics).
narrative_ontology:cs_drift_state('b87b2b12-6829-49ba-818f-48604547be59', contemporary_bioethics_consensus, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b87b2b12-6829-49ba-818f-48604547be59', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parents_of_severely_disabled_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, medical_decision_makers).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, institutional_ethics_committees).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, state_child_welfare_authorities).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants_without_potential).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, future_vulnerable_persons_under_expanded_criteria).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, rational_agency_as_moral_status_ground).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, potentiality_principle_in_bioethics).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, parental_authority_in_treatment_decisions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face decisions about life-sustaining treatment for infants with severe disabilities. The potential-based reading grants them authority to judge whether their child has potential for rational agency, effectively authorizing withdrawal of treatment. They bear the emotional burden but gain decision-making power that the constraint structures.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parents_of_severely_disabled_infants, agenda_setter,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, parents_of_severely_disabled_infants, beneficiary).

% Neonatologists, pediatricians, and hospital ethics committees who implement the potentiality criterion in practice. They gain professional discretion to assess "potential for rational agency" and recommend treatment withdrawal. Their institutional authority is reinforced by the reading's framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, medical_decision_makers, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, medical_decision_makers, beneficiary).

% Formal bodies that adjudicate contested cases. They acquire the mandate to apply the potentiality test, giving them gatekeeping power over which lives receive protection. Their legitimacy derives from being the authorized interpreters of the potentiality standard.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, institutional_ethics_committees, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, institutional_ethics_committees, beneficiary).

% Legal and administrative bodies that enforce treatment decisions. They gain a clear criterion (potential for rational agency) to authorize non-treatment, reducing litigation risk and administrative burden. The reading simplifies their enforcement role.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, state_child_welfare_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, state_child_welfare_authorities, beneficiary).

% Infants with severe congenital or acquired disabilities judged to lack potential for rational agency. They bear the full cost of the constraint — withdrawal of life-sustaining treatment — with no capacity for exit, resistance, or voice. Their moral standing is denied by the very criterion the constraint imposes.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants_without_potential, payer,
    powerless, immediate, trapped, local).

% Persons who may in the future fall under expanded applications of the potentiality criterion (e.g., dementia patients, traumatic brain injury survivors, cognitively impaired adults). They bear prospective extraction as the reading's logic migrates beyond infants. Exit is constrained by the precedential force of the criterion.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, future_vulnerable_persons_under_expanded_criteria, payer,
    powerless, biographical, constrained, national).

% Organizations and individuals who argue that moral standing is inherent and not contingent on cognitive capacity. They are structurally excluded from the constraint's authorizing framework because the potentiality reading treats their objection as category error — they contest the ground on which the constraint operates.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Academic critics who analyze the potentiality argument's logical structure, historical genealogy, and empirical consequences. They see the full constraint structure but hold no enforcement power. Their analyses document the extraction and its institutional beneficiaries.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, bioethics_scholars_potentiality_critics, observer,
    analytical, generational, analytical, global).

% Academic defenders who argue the potentiality criterion protects against arbitrary quality-of-life judgments and provides a principled boundary. They provide the intellectual architecture that legitimates the constraint's operation.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, bioethics_scholars_potentiality_defenders, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled criterion for allocating scarce life-sustaining medical resources and resolving intractable treatment conflicts for severely disabled infants, replacing ad hoc quality-of-life judgments with a capacity-based threshold.
% TRANSFER_FUNCTION: Transfers the burden of continued existence from severely disabled infants (who lose life-sustaining treatment) to parents, medical institutions, and the state (who avoid the costs of long-term care, moral distress, and resource allocation). Transfers decision authority to institutional gatekeepers.
% ABSENT_VOICES: The infants themselves — who cannot speak, resist, or exit — are the primary absent voice. Disability rights advocates who would object on inherent-dignity grounds are structurally excluded because the constraint defines their objection as irrelevant to the potentiality framework. Future persons who would be caught by criterion expansion are absent by temporal necessity.
% DISAPPEARANCE_RATIONALE: If the potentiality criterion vanished overnight, treatment decisions would revert to ad hoc quality-of-life assessments, best-interest standards, or sanctity-of-life defaults — each producing different outcomes for the same infants. Medical protocols, legal precedents, and ethics committee mandates built on the potentiality test would require reconstruction. The infant victim set would change immediately.
% FOUNDING_PROBLEM: Mid-20th century neonatology created the ability to sustain infants with previously fatal conditions, generating intractable conflicts between prolonging biological life and preventing suffering, with no principled criterion for withdrawal decisions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historical medical literature (e.g., the Baby Doe rules debates, Lorber's spina bifida selection criteria) from outside the current beneficiary set. However, current beneficiaries (medical institutions, state authorities) claim the problem remains live due to advancing neonatal capabilities. Disability rights organizations and critical bioethicists attest the problem was never about infants but about constructing a transferable exclusion logic.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__potential_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers the most fundamental good — continued life — from the most powerless class to institutional and parental beneficiaries. Suppression is high (0.72) because the victim class is definitionally unable to resist or exit, and the constraint's persistence depends on maintaining the epistemic authority of the potentiality assessment against inherent-dignity challenges. Theater ratio is moderate (0.25): the coordination function (resolving treatment conflicts) is real but increasingly serves as cover for the extraction. Accessibility collapse (0.65) reflects that once the potentiality framework is accepted, alternatives (sanctity of life, inherent dignity) appear as category errors within the framework. Resistance (0.48) is moderate — disability rights movements and critical bioethics contest the constraint but have not displaced its institutional entrenchment.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seats, the constraint appears as a necessary coordination mechanism that prevents suffering and allocates resources rationally. From the payer seats, it is a death sentence authorized by a criterion they cannot meet. The engine computes this divergence from the structural data: the same constraint is rope-like for the beneficiaries (genuine coordination) and snare-like for the victims (pure extraction), producing the tangled_rope classification. The claim (tangled_rope) and metrics are authored independently — the metrics describe the operational reality; the claim names the structural hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   The four agenda_setter/beneficiary seats (parents, medical decision-makers, ethics committees, state authorities) all derive decision authority and cost avoidance from the constraint — their directionality d is low (beneficiary end). The two payer seats (severely disabled infants, future vulnerable persons) bear the full extraction with trapped or constrained exit — their d is near 1.0 (target end). The excluded seat (disability rights advocates) is structurally locked out of the framework's legitimating grammar. The two observer seats (critical and defending scholars) sit at d=0.5 with analytical exit, seeing the structure without bearing its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (intractable neonatal treatment conflicts) was real but the solution (potentiality criterion) has migrated far beyond its original scope. The constraint now authorizes a transferable exclusion logic that extends to dementia, cognitive disability, and future enhancement debates. The coordination function persists but has become the vehicle for an expanding extraction. Mandatrophy is unresolved: the arrangement persists because its beneficiaries control the interpretive institutions that would be needed to replace it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potentiality_criterion_operationalization,
    'How is ''potential for rational agency'' operationalized in clinical and legal practice — by gestational age, diagnosis, prognostic scores, or clinician judgment?',
    'Analysis of ethics committee records, neonatal guidelines, and court decisions applying the potentiality test across jurisdictions and time.',
    'If operationalization is highly variable, the constraint''s extraction is amplified by discretion; if standardized, the extraction is systematized but the criterion''s arbitrariness is exposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potentiality_criterion_operationalization, empirical, 'Whether the potentiality criterion functions as a principled threshold or a discretionary gate.').

omega_variable(
    criterion_migration_beyond_infants,
    'Has the potentiality logic migrated to authorize exclusion of other cognitively impaired groups (dementia, TBI, intellectual disability)?',
    'Genealogical analysis of bioethics literature, advance directive law, and futility policies for cognitive criteria beyond infancy.',
    'If migration is documented, the constraint''s victim set and extractiveness are structurally larger than the infant-only framing; if contained, the reading''s scope is narrower than critics claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(criterion_migration_beyond_infants, empirical, 'Whether the constraint''s extraction boundary has expanded beyond its founding case.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural relationship to its sibling readings affect its classification stability?',
    'Structural comparison of the three readings'' beneficiary/victim sets, enforcement mechanisms, and epistemic premises to identify which elements are kernel-invariant and which are reading-dependent.',
    'If the potentiality reading''s extraction depends on kernel-invariant features (e.g., the very concept of a personhood boundary), it is more robust; if it depends on reading-specific premises, it is vulnerable to displacement by sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Whether the constraint''s classification derives from the kernel structure or the specific reading.').

omega_variable(
    suppression_internalized_vs_structural,
    'Is the suppression experienced by the victim class (and their advocates) primarily structural (legal/medical barriers) or internalized (disability community''s acceptance of quality-of-life framing)?',
    'Disability rights movement archives, internal movement debates, and empirical studies of disabled persons'' self-assessment vs. clinical quality-of-life assessments.',
    'If internalized suppression is significant, the constraint''s effective suppression exceeds the structural measure — the target population participates in its own exclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Whether suppression operates through external barriers or internalized frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(personhood_potential_tr_t1970, personhood_boundary__potential_based_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(personhood_potential_tr_t1980, personhood_boundary__potential_based_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(personhood_potential_tr_t1990, personhood_boundary__potential_based_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(personhood_potential_tr_t2000, personhood_boundary__potential_based_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(personhood_potential_tr_t2010, personhood_boundary__potential_based_reading, theater_ratio, 2010, 0.23).
narrative_ontology:measurement(personhood_potential_tr_t2020, personhood_boundary__potential_based_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(personhood_potential_tr_t2025, personhood_boundary__potential_based_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(personhood_potential_be_t1970, personhood_boundary__potential_based_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(personhood_potential_be_t1980, personhood_boundary__potential_based_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(personhood_potential_be_t1990, personhood_boundary__potential_based_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(personhood_potential_be_t2000, personhood_boundary__potential_based_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(personhood_potential_be_t2010, personhood_boundary__potential_based_reading, base_extractiveness, 2010, 0.71).
narrative_ontology:measurement(personhood_potential_be_t2020, personhood_boundary__potential_based_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(personhood_potential_be_t2025, personhood_boundary__potential_based_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(personhood_potential_su_t1970, personhood_boundary__potential_based_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(personhood_potential_su_t1980, personhood_boundary__potential_based_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(personhood_potential_su_t1990, personhood_boundary__potential_based_reading, suppression_requirement, 1990, 0.64).
narrative_ontology:measurement(personhood_potential_su_t2000, personhood_boundary__potential_based_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(personhood_potential_su_t2010, personhood_boundary__potential_based_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(personhood_potential_su_t2020, personhood_boundary__potential_based_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(personhood_potential_su_t2025, personhood_boundary__potential_based_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__potential_based_reading, 0.12).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, neonatal_treatment_withdrawal_protocols).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, advance_directive_cognitive_criteria).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, futility_policy_frameworks).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three readings with distinct ε values and victim sets. This reading (potential_based) has the highest extractiveness (0.78) because it excludes a defined class while granting authority to institutional beneficiaries. The birth_threshold_reading has near-zero extractiveness (universal inclusion). The fitness_contingent_reading sits between (excludes pre-fitness entities but may include severely disabled infants who demonstrate minimal fitness). The three stories form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__potential_based_reading, moderate, 0.2).
constraint_indexing:directionality_override(personhood_boundary__potential_based_reading, institutional, 0.15).
constraint_indexing:directionality_override(personhood_boundary__potential_based_reading, powerless, 0.98).
constraint_indexing:directionality_override(personhood_boundary__potential_based_reading, organized, 0.65).
constraint_indexing:directionality_override(personhood_boundary__potential_based_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
