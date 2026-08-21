% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood Boundary: Potential-Based Reading
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint defines personhood based on the potential for rational
 *   agency, a philosophical position that grants moral standing to entities
 *   capable of developing reason and self-awareness. It implies that severely
 *   disabled infants who lack this potential may not possess full moral
 *   standing, allowing for different ethical considerations and
 *   decision-making frameworks regarding their care. This is one reading of
 *   the broader 'personhood_boundary' kernel, which is highly contested in
 *   moral philosophy and bioethics.
 *
 * KEY AGENTS:
 *   - parents_and_guardians: Agenda setter / Beneficiary (powerful/constrained)
 *   - medical_professionals: Agenda setter / Beneficiary (institutional/constrained)
 *   - society_at_large: Beneficiary (organized/constrained)
 *   - severely_disabled_infants_lacking_potential: Payer / Excluded (powerless/trapped)
 *   - disability_rights_advocates: Observer / Payer (organized/mobile)
 *   - ethicists_and_philosophers: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.7).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.8).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood Boundary: Potential-Based Reading").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, 'c1d7eb51-1763-4175-be1d-ae45da806cc4').
narrative_ontology:cs_kernel_codification('c1d7eb51-1763-4175-be1d-ae45da806cc4', formalized).
narrative_ontology:cs_authority_grounding('c1d7eb51-1763-4175-be1d-ae45da806cc4', expertise).
narrative_ontology:cs_interpretation_layer_present('c1d7eb51-1763-4175-be1d-ae45da806cc4').
narrative_ontology:cs_reading_relation('c1d7eb51-1763-4175-be1d-ae45da806cc4', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('c1d7eb51-1763-4175-be1d-ae45da806cc4', personhood_boundary__fitness_contingent_reading, coexists_with).
narrative_ontology:cs_axiom('c1d7eb51-1763-4175-be1d-ae45da806cc4', foundational, rational_agency_is_basis_for_personhood).
narrative_ontology:cs_axiom_status(rational_agency_is_basis_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('c1d7eb51-1763-4175-be1d-ae45da806cc4', rational_agency_is_basis_for_personhood, deontological).
narrative_ontology:cs_axiom('c1d7eb51-1763-4175-be1d-ae45da806cc4', foundational, potential_for_agency_confers_standing).
narrative_ontology:cs_axiom_status(potential_for_agency_confers_standing, holdable).
narrative_ontology:cs_axiom_grounding('c1d7eb51-1763-4175-be1d-ae45da806cc4', potential_for_agency_confers_standing, deontological).
narrative_ontology:cs_reference_frame('c1d7eb51-1763-4175-be1d-ae45da806cc4', enlightenment_rationality_framework).
narrative_ontology:cs_drift_state('c1d7eb51-1763-4175-be1d-ae45da806cc4', contemporary_bioethics_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c1d7eb51-1763-4175-be1d-ae45da806cc4', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parents_and_guardians).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, medical_professionals).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, society_at_large).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants_lacking_potential).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, disability_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain significant decision-making authority and flexibility regarding the care and life-sustaining treatment for infants deemed to lack the potential for rational agency. They bear the emotional burden but also the power to make profound choices.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parents_and_guardians, agenda_setter,
    powerful, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, parents_and_guardians, beneficiary).

% Are tasked with assessing the 'potential for rational agency' and guiding parents/guardians in decisions. This framework provides a basis for medical ethics and resource allocation, offering clarity in difficult cases but also placing a heavy interpretive burden on them.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, medical_professionals, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, medical_professionals, beneficiary).

% Benefits from a clear, albeit contested, definition of personhood that informs legal, ethical, and resource allocation frameworks. This clarity can reduce societal ambiguity and conflict in cases involving profound disability, but at the cost of excluding some human lives from full moral standing.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, society_at_large, beneficiary,
    organized, generational, constrained, global).

% Are the primary targets of this constraint, as they are deemed to lack the 'potential for rational agency' and thus may be excluded from full moral standing and associated rights. They have no voice or agency in this determination.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants_lacking_potential, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, severely_disabled_infants_lacking_potential, excluded).

% Actively challenge this definition of personhood, arguing for broader inclusion based on inherent human dignity rather than cognitive potential. They bear the moral and political cost of advocating for those excluded by the dominant framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_rights_advocates, observer,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, disability_rights_advocates, payer).

% Engage in ongoing debate and analysis of the criteria for personhood, the definition of 'potential for rational agency,' and the ethical implications of such boundaries. They contribute to the intellectual framework but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, ethicists_and_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__potential_based_reading, parents_and_guardians).
narrative_ontology:fixing_cost_class(personhood_boundary__potential_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared philosophical and ethical boundary for personhood, guiding moral and legal decision-making regarding the status and rights of human beings, particularly in cases of profound cognitive impairment.
% TRANSFER_FUNCTION: Transfers moral standing and associated rights from entities deemed to lack the potential for rational agency to those who possess it, or to decision-makers (parents, medical professionals) who act on behalf of those excluded.
% ABSENT_VOICES: The severely disabled infants themselves are structurally absent from the conversation; their moral standing is determined by others. Disability rights advocates speak on their behalf, but the primary subjects of the constraint cannot articulate their own claims.
% DISAPPEARANCE_RATIONALE: If this potential-based personhood boundary vanished overnight, society would face a fundamental re-evaluation of moral obligations, legal protections, and resource allocation for all human life, regardless of cognitive potential. This would necessitate a profound reorganization of ethical, medical, and legal systems, particularly concerning end-of-life care and the rights of the profoundly disabled.
% FOUNDING_PROBLEM: To define the moral and legal boundary of personhood in a way that aligns with philosophical traditions emphasizing rational agency, while also addressing the practical and ethical challenges of caring for human beings with varying degrees of cognitive capacity.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing debates among ethicists, legal scholars, and medical professionals, as well as the continuous advocacy by disability rights groups, corroborate that the founding problem of defining personhood and its implications remains a live and contested issue, far from a settled consensus.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (providing a framework for difficult ethical decisions and resource allocation) but also involves significant, asymmetric extraction from those excluded from personhood. Extractiveness is high (0.7) due to the profound loss of moral standing and rights for the excluded. Suppression is also high (0.8) because the excluded have no means to resist or exit the definition. Theater ratio is low (0.1) as the philosophical and ethical justifications are taken seriously, not merely performative. Resistance (0.6) comes from disability rights advocates and some ethicists who challenge this boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of parents, medical professionals, and society, this constraint provides a necessary framework for navigating complex ethical dilemmas, offering clarity and guiding difficult decisions. From the perspective of the excluded infants and their advocates, it is a deeply extractive and suppressive boundary that denies fundamental rights based on a contested definition of potential. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Parents and medical professionals are beneficiaries and agenda-setters, gaining decision-making power and clarity, placing them at the beneficiary end of directionality. Society at large also benefits from this clarity. Severely disabled infants are the primary targets, bearing the full cost of exclusion, placing them at the target end. Disability rights advocates, while observers, bear a moral cost and actively resist, positioning them closer to the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Snare by acknowledging its genuine, albeit contested, coordination function in providing a framework for ethical decision-making. Conversely, it prevents mislabeling it as a pure Rope by highlighting the severe extraction and suppression experienced by the excluded, which is often obscured by the coordination narrative. The 'live' status of the founding problem, despite the high extraction, indicates it is not a Piton, as the debate around its function is ongoing and active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_potential_for_rational_agency,
    'How is ''potential for rational agency'' precisely defined and measured, and what degree of certainty is required for such a determination?',
    'Advancements in neuroscience and developmental biology, coupled with philosophical consensus on the criteria for ''potential'' versus ''actual'' agency, could refine or challenge the boundary.',
    'A more stringent or expansive definition of ''potential'' would alter the victim set, either expanding or contracting the scope of exclusion, thereby shifting the constraint''s effective extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_potential_for_rational_agency, empirical, 'Ambiguity in defining the core criterion for personhood.').

omega_variable(
    personhood_boundary_kernel_reading,
    'This constraint is the ''potential_based_reading'' of the ''personhood_boundary'' kernel. What would a ''birth_threshold_reading'' or ''fitness_contingent_reading'' change structurally?',
    'Analysis of the structural deltas between this reading and its siblings, as documented in the `cs_structure` section.',
    'A ''birth_threshold_reading'' would expand the victim set to include all born humans, regardless of potential, significantly reducing extraction from the current victim group. A ''fitness_contingent_reading'' would likely narrow the victim set further or make exclusion more stringent, potentially increasing extraction from a smaller group.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personhood_boundary_kernel_reading, conceptual, 'Contextualizes this constraint as one reading of a contested kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of agency, inability to communicate) or internalized (e.g., through societal norms that devalue certain lives)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., through legal recognition of rights), reclassify as partially internalized. For infants, this is largely structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. For this constraint, the suppression is overwhelmingly structural due to the inherent lack of agency of the victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the excluded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__potential_based_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__potential_based_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(pers_tr_t60, personhood_boundary__potential_based_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(pers_tr_t80, personhood_boundary__potential_based_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(pers_tr_t100, personhood_boundary__potential_based_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__potential_based_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__potential_based_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(pers_be_t60, personhood_boundary__potential_based_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(pers_be_t80, personhood_boundary__potential_based_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(pers_be_t100, personhood_boundary__potential_based_reading, base_extractiveness, 100, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__potential_based_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__potential_based_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(pers_su_t60, personhood_boundary__potential_based_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(pers_su_t80, personhood_boundary__potential_based_reading, suppression_requirement, 80, 0.8).
narrative_ontology:measurement(pers_su_t100, personhood_boundary__potential_based_reading, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'personhood_boundary' kernel. Its structural properties and metrics are distinct from other readings, which are modeled as separate constraints. This reading emphasizes potential for rational agency as the basis for personhood, leading to the exclusion of severely disabled infants lacking such potential.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
