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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood Boundary: Potential for Rational Agency Reading
 *   domain: moral_philosophy/bioethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'potential_based_reading' of the
 *   'personhood_boundary' kernel. It describes a philosophical and ethical
 *   framework where personhood, and thus full moral standing, is granted
 *   based on an entity's potential for rational agency. This reading allows
 *   for the exclusion of severely disabled infants or other entities deemed
 *   to lack this potential from full moral consideration, granting parental
 *   and medical authorities significant discretion in decisions regarding
 *   their care and status. The constraint functions as a 'tangled_rope'
 *   because it provides a coordination mechanism for complex ethical dilemmas
 *   (defining who counts) but simultaneously enables significant extraction
 *   from those excluded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.8).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.85).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood Boundary: Potential for Rational Agency Reading").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/bioethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, '66f1b2ae-bf20-498a-b292-8191d1e0378a').
narrative_ontology:cs_kernel_codification('66f1b2ae-bf20-498a-b292-8191d1e0378a', formalized).
narrative_ontology:cs_authority_grounding('66f1b2ae-bf20-498a-b292-8191d1e0378a', expertise).
narrative_ontology:cs_interpretation_layer_present('66f1b2ae-bf20-498a-b292-8191d1e0378a').
narrative_ontology:cs_reading_relation('66f1b2ae-bf20-498a-b292-8191d1e0378a', personhood_boundary__birth_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('66f1b2ae-bf20-498a-b292-8191d1e0378a', personhood_boundary__fitness_contingent_reading, coexists_with).
narrative_ontology:cs_axiom('66f1b2ae-bf20-498a-b292-8191d1e0378a', foundational, potential_for_rational_agency_is_personhood_basis).
narrative_ontology:cs_axiom_status(potential_for_rational_agency_is_personhood_basis, holdable).
narrative_ontology:cs_axiom_grounding('66f1b2ae-bf20-498a-b292-8191d1e0378a', potential_for_rational_agency_is_personhood_basis, deontological).
narrative_ontology:cs_reference_frame('66f1b2ae-bf20-498a-b292-8191d1e0378a', enlightenment_rationalism_framework).
narrative_ontology:cs_drift_state('66f1b2ae-bf20-498a-b292-8191d1e0378a', contemporary_bioethics_debate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('66f1b2ae-bf20-498a-b292-8191d1e0378a', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, medical_ethicists_parents).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, resource_allocating_institutions).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, infants_lacking_rational_potential).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, universal_personhood_advocates).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, rational_agency_as_moral_basis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors define and apply the criteria for personhood based on potential for rational agency, making critical decisions regarding the moral standing and care of infants. They benefit from the clarity and authority this framework provides in difficult ethical dilemmas, particularly concerning resource allocation.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, medical_ethicists_parents, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, medical_ethicists_parents, beneficiary).

% These are the primary targets of the constraint, as their moral standing and rights are diminished or denied under this reading. They bear the ultimate cost of exclusion from full personhood, with no capacity to resist or exit.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, infants_lacking_rational_potential, payer,
    powerless, immediate, trapped, local).

% These groups actively challenge the potential-based personhood boundary, arguing for universal moral standing for all human beings regardless of capacity. They bear the costs of continuous advocacy against an entrenched philosophical and institutional framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, universal_personhood_advocates, payer,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, universal_personhood_advocates, observer).

% Hospitals, governments, and other institutions that manage healthcare and social resources benefit from a framework that allows for differential allocation based on personhood status. This reading provides a justification for difficult decisions regarding life-sustaining treatment and long-term care.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, resource_allocating_institutions, beneficiary,
    institutional, generational, constrained, national).

% Advocates who believe personhood begins universally at birth are excluded from the core decision-making framework of this reading, as their premise is fundamentally different. They operate in a competing ethical framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, birth_threshold_personhood_advocates, excluded,
    organized, generational, constrained, global).

% Advocates for personhood contingent on demonstrated fitness (not just potential) are also excluded from this reading's framework, as their criteria are more stringent and based on actualized capacities rather than potential. They represent a distinct, often more radical, ethical position.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, fitness_contingent_personhood_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__potential_based_reading, medical_ethicists_parents).
narrative_ontology:fixing_cost_class(personhood_boundary__potential_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal understanding of moral standing and rights, providing a framework for ethical decision-making regarding the care and treatment of human beings, particularly in cases of severe disability or developmental immaturity.
% TRANSFER_FUNCTION: Transfers moral standing, rights, and access to resources from entities deemed to lack the potential for rational agency to those who possess it, or to institutions making decisions on their behalf.
% ABSENT_VOICES: The infants themselves, lacking rational agency, are inherently absent from the conversation. Their interests are represented by others, but their direct voice is suppressed by the very definition of personhood. Advocates for universal personhood are present but often marginalized within the dominant discourse.
% DISAPPEARANCE_RATIONALE: If this specific reading of personhood vanished overnight, the ethical and legal landscape surrounding the treatment of severely disabled infants would undergo a profound reorganization. Decisions about life support, resource allocation, and parental authority would lack a foundational justification, leading to widespread re-evaluation of moral obligations and rights.
% FOUNDING_PROBLEM: The problem of defining who counts as a moral person, particularly at the margins of human life (e.g., infants, individuals with profound cognitive impairments), and how to justly allocate societal resources and moral obligations.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing debates in bioethics, legal scholarship, and public discourse, as evidenced by numerous academic publications, legislative hearings, and advocacy campaigns from diverse perspectives, corroborate that the founding problem remains live and highly contested.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.8) is high because it directly denies full moral standing and rights to a class of beings, which is a severe form of extraction. Suppression (0.85) is also high, as the excluded individuals are inherently voiceless, and their advocates face deeply entrenched philosophical and institutional resistance. The theater ratio (0.1) is low, reflecting that the application of these criteria, while contested, is generally a serious and non-performative ethical exercise. Accessibility collapse (0.6) is moderate, as alternative personhood definitions exist and are debated, but this reading still holds significant sway in medical and ethical practice. Resistance (0.7) is high due to active advocacy from universal personhood groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of medical ethicists and parents, this framework provides necessary clarity and authority for making agonizing decisions, seen as a coordination function. From the perspective of the excluded infants and their advocates, it is a mechanism of profound extraction and suppression, denying fundamental rights. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical ethicists and parents, along with resource-allocating institutions, are beneficiaries as they gain clarity, authority, and justification for resource decisions (low directionality). Infants lacking rational potential are full targets, bearing the direct cost of exclusion (high directionality). Universal personhood advocates are also targets, as they bear the cost of challenging the established framework (high directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the severe extraction from the excluded) or a pure Snare (which would ignore the genuine, albeit contested, coordination function of providing a framework for moral decision-making). The 'potential for rational agency' provides a coordination story, but its application creates asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_potential_ambiguity,
    'How is ''potential for rational agency'' precisely defined and measured, especially in cases of severe disability or uncertain prognosis?',
    'Development of universally accepted, empirically verifiable criteria for assessing potential, or a shift to a different, less ambiguous criterion for personhood.',
    'If the definition remains ambiguous or inconsistently applied, the constraint''s extractiveness and suppression could be higher than measured, as arbitrary application would increase the vulnerability of the excluded. If clarified, it could reduce arbitrary exclusion but might still maintain the boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_potential_ambiguity, conceptual, 'Ambiguity in defining the core criterion for personhood.').

omega_variable(
    kernel_reading_status,
    'Is this ''potential_based_reading'' a genuinely distinct and coherent ethical framework, or is it a rationalization for resource allocation decisions?',
    'Analysis of the internal consistency of the philosophical arguments, and empirical study of the correlation between the application of this reading and resource scarcity or societal convenience.',
    'If it is primarily a rationalization, its effective extractiveness is higher, and its coordination function is largely theatrical. If it is a genuinely held ethical position, the measured extractiveness reflects a deeply held, albeit contested, moral boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'Whether the reading is a genuine ethical framework or a rationalization.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of voice for infants) or internalized (societal acceptance of the framework)?',
    'Post-exit suppression trajectory: if societal acceptance shifts and advocacy gains traction, but infants still lack voice, the structural component is dominant. If the framework loses legitimacy, internalized suppression diminishes.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the framework''s legitimacy is deeply embedded. If purely structural, removing the philosophical barrier would have a more immediate impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1970, personhood_boundary__potential_based_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(pers_tr_t1980, personhood_boundary__potential_based_reading, theater_ratio, 1980, 0.13).
narrative_ontology:measurement(pers_tr_t1990, personhood_boundary__potential_based_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(pers_tr_t2000, personhood_boundary__potential_based_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(pers_tr_t2010, personhood_boundary__potential_based_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(pers_tr_t2025, personhood_boundary__potential_based_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t1970, personhood_boundary__potential_based_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(pers_be_t1980, personhood_boundary__potential_based_reading, base_extractiveness, 1980, 0.73).
narrative_ontology:measurement(pers_be_t1990, personhood_boundary__potential_based_reading, base_extractiveness, 1990, 0.76).
narrative_ontology:measurement(pers_be_t2000, personhood_boundary__potential_based_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(pers_be_t2010, personhood_boundary__potential_based_reading, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement(pers_be_t2025, personhood_boundary__potential_based_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1970, personhood_boundary__potential_based_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(pers_su_t1980, personhood_boundary__potential_based_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(pers_su_t1990, personhood_boundary__potential_based_reading, suppression_requirement, 1990, 0.81).
narrative_ontology:measurement(pers_su_t2000, personhood_boundary__potential_based_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(pers_su_t2010, personhood_boundary__potential_based_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(pers_su_t2025, personhood_boundary__potential_based_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'personhood_boundary' kernel. Its structural properties and metrics are distinct from other readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
