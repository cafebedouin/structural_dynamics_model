% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood Grounded in Potential for Rational Agency
 *   domain: moral philosophy/historical ethics/commitment systems
 *
 * SUMMARY:
 *   This constraint instantiates the potential-based reading of the
 *   personhood_boundary kernel: the claim that moral personhood is grounded
 *   in the potential for rational agency, which excludes severely disabled
 *   infants who lack this potential. It is a commitment system constraint
 *   operating in moral philosophy, bioethics, and clinical practice. The
 *   constraint coordinates the moral community by offering a cognitive
 *   criterion for standing, but asymmetrically extracts full moral status
 *   from excluded infants and concentrates decisional authority in clinical
 *   and parental hands. The victim set is precisely those entities without
 *   potential for personhood-relevant capacities. KEY AGENTS (by structural
 *   relationship): - bioethics_institutions: Primary agenda-setter
 *   (institutional/constrained) â maintains and enforces the criterion. -
 *   cognitively_typical_humans: Primary beneficiary (organized/mobile) â
 *   secure standing. - severely_disabled_newborns: Primary target
 *   (powerless/trapped) â bear total extraction of moral standing. -
 *   disability_rights_advocates: Analytical observer (organized/mobile) â
 *   resists the framework. - parents_of_affected_newborns: Dual-positioned
 *   payer/beneficiary (moderate/constrained) â suffer costs but are granted
 *   decisional authority.
 *
 * KEY AGENTS:
 *   - bioethics_institutions: Primary agenda-setter (institutional/constrained) â maintains and enforces the criterion
 *   - cognitively_typical_humans: Primary beneficiary (organized/mobile) â secure standing
 *   - severely_disabled_newborns: Primary target (powerless/trapped) â bear total extraction of moral standing
 *   - disability_rights_advocates: Analytical observer (organized/mobile) â resists the framework
 *   - parents_of_affected_newborns: Dual-positioned payer/beneficiary (moderate/constrained) â suffer costs but are granted decisional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.65).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.75).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood Grounded in Potential for Rational Agency").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral philosophy/historical ethics/commitment systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, 'b1e4f8de-1837-43e0-afe0-57de59af4d58').
narrative_ontology:cs_kernel_codification('b1e4f8de-1837-43e0-afe0-57de59af4d58', formalized).
narrative_ontology:cs_authority_grounding('b1e4f8de-1837-43e0-afe0-57de59af4d58', lineage).
narrative_ontology:cs_interpretation_layer_present('b1e4f8de-1837-43e0-afe0-57de59af4d58').
narrative_ontology:cs_reading_relation('b1e4f8de-1837-43e0-afe0-57de59af4d58', personhood_boundary__birth_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1e4f8de-1837-43e0-afe0-57de59af4d58', personhood_boundary__fitness_contingent_reading, coexists_with).
narrative_ontology:cs_axiom('b1e4f8de-1837-43e0-afe0-57de59af4d58', foundational, potential_for_rational_agency_sufficient).
narrative_ontology:cs_axiom_status(potential_for_rational_agency_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('b1e4f8de-1837-43e0-afe0-57de59af4d58', potential_for_rational_agency_sufficient, deontological).
narrative_ontology:cs_axiom('b1e4f8de-1837-43e0-afe0-57de59af4d58', foundational, absence_of_potential_permits_exclusion).
narrative_ontology:cs_axiom_status(absence_of_potential_permits_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('b1e4f8de-1837-43e0-afe0-57de59af4d58', absence_of_potential_permits_exclusion, deontological).
narrative_ontology:cs_reference_frame('b1e4f8de-1837-43e0-afe0-57de59af4d58', rational_agency_as_personhood_ground).
narrative_ontology:cs_drift_state('b1e4f8de-1837-43e0-afe0-57de59af4d58', contemporary_disability_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b1e4f8de-1837-43e0-afe0-57de59af4d58', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, bioethics_institutions).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, cognitively_typical_humans).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_newborns).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parents_of_affected_newborns).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, parents_of_affected_newborns).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and teach the potentiality criterion for personhood; publish clinical guidelines for decision-making at the beginning of life; their professional authority and institutional role depend on offering principled boundaries for moral standing.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, bioethics_institutions, agenda_setter,
    institutional, generational, constrained, universal).

% Receive the protection of personhood status without question under this criterion; their moral standing is the default against which marginal cases are measured, and they face no existential threat of exclusion.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, cognitively_typical_humans, beneficiary,
    organized, civilizational, mobile, universal).

% Classified as lacking the potential for rational agency; denied full moral standing; subject to medical decisions about withdrawal of care, non-treatment, or resource allocation based on the criterion applied by others.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_newborns, payer,
    powerless, immediate, trapped, local).

% Reject cognitive criteria for personhood as discriminatory; argue for unconditional moral standing of all humans regardless of capacity; resist the application of potentiality frameworks in law, medicine, and public policy.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_rights_advocates, observer,
    organized, generational, mobile, global).

% Must navigate clinical consultations where their child's personhood is questioned; bear emotional, ethical, and social costs; simultaneously granted decisional authority over treatment, which functions as both a burden and a delegated power under the framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parents_of_affected_newborns, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, parents_of_affected_newborns, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decisive criterion for membership in the moral community, resolving uncertainty about whether infants with profound cognitive disabilities possess standing by tying personhood to the potential for rational agency.
% TRANSFER_FUNCTION: Moves moral standing and its legal protections away from infants judged to lack potential for rational agency and toward those who possess it; transfers discretionary authority over the life and death of excluded infants to bioethical and clinical judgment.
% ABSENT_VOICES: The excluded infants themselves cannot speak for themselves; disability rights advocates who affirm unconditional personhood are present in discourse but structurally marginalized within bioethics frameworks that privilege cognitive criteria.
% DISAPPEARANCE_RATIONALE: Clinical protocols for infants with severe cognitive impairment would default toward full protective status; the authority of bioethicists and clinicians to sanction withdrawal of care on the basis of cognitive prognosis would dissolve; legal frameworks would need to recognize unconditional personhood from conception or birth.
% FOUNDING_PROBLEM: How to determine which human beings qualify as persons deserving of full moral and legal protection, particularly at the margins of cognitive life where capacities are absent or profoundly diminished.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights advocates and some theological ethicists attest the problem is live but misframed; bioethicists and clinicians attest the boundary is necessary to resolve triage and resource dilemmas. Independent human rights frameworks from outside the benefiting parties increasingly reject cognitive thresholds.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) is high because the constraint strips moral standing entirely from excluded infants; suppression (0.75) is higher because the excluded cannot resist and their exclusion is enforced by clinical protocols and philosophical argument. Theater ratio (0.45) reflects moderate performativity: the coordination function (defining personhood) is real, but a substantial share of intellectual activity defends a boundary that conveniently eliminates burdensome cases. Accessibility collapse (0.70) is high within the framework (once potentiality is accepted, alternatives collapse), but resistance (0.55) is moderate due to sustained critique from disability rights and care ethics. The claim/metric independence is maintained: the reading is claimed as tangled_rope while metrics honestly describe substantial extraction and suppression.
 *
 * PERSPECTIVAL GAP:
 *   The bioethics institution seat experiences the constraint as necessary coordination (resolving who counts), while the severely disabled newborn seat experiences total exclusion; the disability advocate seat sees performative boundary-maintenance masking extraction. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Bioethics institutions and cognitively typical humans are structural beneficiaries (low d), deriving authority and secure status. Severely disabled newborns are full targets (high d), trapped by their condition and the criterion that interprets it as absence. Parents occupy an ambiguous middle: granted authority (beneficiary signal) but bearing grievous costs (payer signal), producing a derived d near symmetric but pulled toward target by the irreversibility of their child's exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination function (resolving moral community membership) and asymmetric extraction (excluded infants lose standing). A pure coordination reading (rope) would fail the victim gate; a pure extraction reading (snare) would ignore the genuine philosophical work the criterion performs in resolving boundary disputes. Tangled_rope captures the hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the potentiality criterion represent a discovered moral fact about the grounds of personhood, or a constructed boundary serving the interests of the cognitively powerful?',
    'Historical genealogy tracing the criterion''s deployment alongside eugenics and resource rationing versus its justification in natural law traditions.',
    'If constructed, the constraint''s classification shifts toward snare; if discovered, it remains a contested tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Natural-law vs constructed ambiguity for the potentiality criterion').

omega_variable(
    potentiality_scope,
    'Does ''potential for rational agency'' refer to species-normal potential or individualized prognosis, and does it include infants with fluctuating or uncertain trajectories?',
    'Meta-analysis of clinical outcomes and philosophical disambiguation of ''potential''.',
    'Changes the size of the victim set and the severity of extraction; species-normal potential is more inclusive, individualized prognosis more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potentiality_scope, empirical, 'Ambiguity in the scope of potential').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the exclusion of severely disabled infants from personhood sustained by structural barriers (clinical protocols, legal frameworks) or by internalized devaluation of disabled life among decision-makers?',
    'Post-reform trajectory analysis: if exclusion persists after structural barriers are removed, suppression is partially internalized.',
    'Internalized suppression would raise effective extraction and indicate deeper capture than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary__potential_based_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__potential_based_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary__potential_based_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__potential_based_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary__potential_based_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pers_be_t10, personhood_boundary__potential_based_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__potential_based_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(pers_be_t30, personhood_boundary__potential_based_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__potential_based_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(pers_be_t50, personhood_boundary__potential_based_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(pers_su_t10, personhood_boundary__potential_based_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__potential_based_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(pers_su_t30, personhood_boundary__potential_based_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__potential_based_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(pers_su_t50, personhood_boundary__potential_based_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three structurally distinct constraints because the natural-language label 'personhood' conflates competing criteria (birth, potential, demonstrated fitness) with different epsilon values, victim sets, and enforcement requirements. This story addresses the potential-based criterion only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
