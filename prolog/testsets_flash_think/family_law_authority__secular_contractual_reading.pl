% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Secular Marriage Contract under State Law
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint describes marriage as a civil contract between autonomous
 *   individuals, recognized and regulated solely by state law. It emphasizes
 *   gender-symmetric rights, state registration as the primary criterion for
 *   validity, and permits interfaith marriage. This reading stands in
 *   contrast to religious or customary interpretations of marriage, asserting
 *   the state's exclusive authority in defining legal marital status.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.45).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.55).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Secular Marriage Contract under State Law").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, 'cd12ba15-d681-4497-9e01-5687eed78256').
narrative_ontology:cs_kernel_codification('cd12ba15-d681-4497-9e01-5687eed78256', formalized).
narrative_ontology:cs_authority_grounding('cd12ba15-d681-4497-9e01-5687eed78256', practice).
narrative_ontology:cs_interpretation_layer_present('cd12ba15-d681-4497-9e01-5687eed78256').
narrative_ontology:cs_reading_relation('cd12ba15-d681-4497-9e01-5687eed78256', family_law_authority__hindu_dharmashastra_reading, forecloses).
narrative_ontology:cs_reading_relation('cd12ba15-d681-4497-9e01-5687eed78256', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd12ba15-d681-4497-9e01-5687eed78256', family_law_authority__christian_canonical_reading, forecloses).
narrative_ontology:cs_reading_relation('cd12ba15-d681-4497-9e01-5687eed78256', family_law_authority__parsi_zoroastrian_reading, forecloses).
narrative_ontology:cs_axiom('cd12ba15-d681-4497-9e01-5687eed78256', foundational, individual_autonomy_in_contract).
narrative_ontology:cs_axiom_status(individual_autonomy_in_contract, holdable).
narrative_ontology:cs_axiom_grounding('cd12ba15-d681-4497-9e01-5687eed78256', individual_autonomy_in_contract, deontological).
narrative_ontology:cs_axiom('cd12ba15-d681-4497-9e01-5687eed78256', foundational, state_as_sole_arbiter_of_legal_status).
narrative_ontology:cs_axiom_status(state_as_sole_arbiter_of_legal_status, holdable).
narrative_ontology:cs_axiom_grounding('cd12ba15-d681-4497-9e01-5687eed78256', state_as_sole_arbiter_of_legal_status, conventional).
narrative_ontology:cs_reference_frame('cd12ba15-d681-4497-9e01-5687eed78256', enlightenment_legal_positivism).
narrative_ontology:cs_drift_state('cd12ba15-d681-4497-9e01-5687eed78256', contemporary_social_evolution, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cd12ba15-d681-4497-9e01-5687eed78256', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, contracting_individuals).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, state_legal_system).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, individuals_seeking_religious_only_recognition).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, children_of_unregistered_unions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, legal_profession).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, contracting_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enter into a legally recognized union, gaining rights and responsibilities regarding property, inheritance, and children. They bear the costs of state registration, legal fees for prenuptial agreements or divorce, and adherence to state-defined terms.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, contracting_individuals, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, contracting_individuals, payer).

% Establishes and administers the legal framework for marriage, ensuring uniformity, resolving disputes, and collecting administrative fees. It benefits from maintaining social order and a clear legal basis for family structures.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_legal_system, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__secular_contractual_reading, state_legal_system, beneficiary).

% Are excluded from granting legally binding marital status, though they may perform religious ceremonies. Their definitions of marriage are not primary for state recognition, leading to a diminished role in the legal sphere.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_institutions, excluded,
    organized, generational, constrained, national).

% Bear the costs of lacking legal protections, inheritance rights, and clear parental status that are automatically conferred by state-recognized marriage. Their situation is often dependent on the legal status of their parents' relationship.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, children_of_unregistered_unions, payer,
    powerless, biographical, trapped, national).

% Benefits from the legal complexities of civil marriage, including drafting prenuptial agreements, handling divorce proceedings, and advising on inheritance and family law matters.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, universally applicable legal framework for intimate relationships, defining rights, responsibilities, and property division, thereby reducing uncertainty for individuals and the state across diverse populations.
% TRANSFER_FUNCTION: Transfers legal rights and responsibilities between individuals (e.g., spousal support, inheritance, parental rights) and transfers administrative authority over marital status from religious or customary bodies to the state.
% ABSENT_VOICES: Religious communities who believe marriage should be solely governed by their tenets, and individuals who desire legal recognition for non-traditional relationships not fully encompassed by the civil contract model, are often marginalized in the legislative process.
% DISAPPEARANCE_RATIONALE: If state-recognized civil marriage vanished overnight, the legal basis for property rights, inheritance, parental responsibilities, and numerous social benefits tied to marital status would collapse, leading to widespread legal and social chaos and requiring a complete reorganization of family law.
% FOUNDING_PROBLEM: The founding problem was the lack of uniform legal recognition and protection for marital unions, leading to disputes over property, inheritance, and the legitimacy of children, particularly as societies became more diverse and secular, moving away from singular religious or customary authorities.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, social workers, and family courts consistently attest to the ongoing need for a clear, secular legal framework for family relationships, independent of religious or customary claims. This is evidenced by continuous legislative updates and judicial rulings addressing evolving social norms and family structures.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.45) due to administrative fees, legal costs associated with entry and exit, and the prescriptive nature of state-defined terms, which can impose burdens even in a 'contractual' framework. Suppression is moderate (0.55) as it actively excludes religious-only recognition and enforces state-defined terms, though alternatives like cohabitation exist. Theater ratio is low (0.10) because the state's administrative and legal functions are genuinely performed. The claimed type is 'rope' as it primarily functions as a coordination mechanism for legal clarity and social stability, despite the inherent costs and state enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, this is an efficient and equitable system for managing social relations. From the perspective of some individuals, particularly those from religious communities, it can be seen as an imposition that diminishes the spiritual or customary significance of marriage, or as a bureaucratic hurdle. The legal profession, meanwhile, benefits from the system's complexities.
 *
 * DIRECTIONALITY LOGIC:
 *   Contracting individuals are beneficiaries through legal clarity and protections, but also payers through fees and adherence to state terms. The state legal system is an agenda-setter and beneficiary, administering the system and maintaining social order. Religious institutions are excluded from legal authority over marriage. Children of unregistered unions are victims, bearing the costs of non-recognition.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate for legal clarity and protection in relationships remains live. The constraint prevents mislabeling genuine coordination (standardized legal framework) as pure extraction, while acknowledging the state's power and the costs it imposes. The contest lies in whether the state's exclusive authority has outlived its necessity or if it remains the most effective means of achieving its coordination goals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_role_coordination_vs_extraction,
    'Is the state''s role in defining and regulating marriage primarily a coordination function, or does it also serve as a mechanism for state extraction (e.g., through fees, control over personal life)?',
    'Comparative analysis of state revenue from marriage-related services versus administrative costs, and analysis of legislative intent behind specific regulations.',
    'If primarily extractive, the constraint''s effective extractiveness (χ) would be higher, potentially shifting its classification towards a Tangled Rope or Snare for certain seats. If purely coordination, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_role_coordination_vs_extraction, empirical, 'Ambiguity regarding the state''s primary motivation in marriage regulation.').

omega_variable(
    secular_religious_recognition_tension,
    'To what extent does the exclusive state recognition of civil marriage suppress or marginalize religious and customary forms of marriage, and what are the social costs of this suppression?',
    'Sociological studies on the impact of secular marriage laws on religious communities, and legal reforms allowing for dual recognition or alternative forms of legal partnership.',
    'If suppression is high and social costs are significant, the constraint''s suppression metric might be understated, and its classification could lean more towards a Snare for affected communities. If religious recognition can coexist without legal conflict, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_religious_recognition_tension, conceptual, 'Tension between secular legal recognition and religious/customary definitions of marriage.').

omega_variable(
    individual_autonomy_realization,
    'How fully is the ideal of ''autonomous individuals'' realized within the state''s contractual framework, given potential power imbalances in relationships and the prescriptive nature of state-defined terms?',
    'Empirical studies on marital power dynamics, access to legal resources, and the impact of mandatory legal terms on individual agency within marriage.',
    'If individual autonomy is significantly constrained in practice, the extractiveness and suppression for individuals might be higher than measured, potentially shifting the classification for the ''contracting_individuals'' seat towards a Tangled Rope or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individual_autonomy_realization, empirical, 'The gap between the ideal of individual autonomy and its practical realization in civil marriage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__secular_contractual_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__secular_contractual_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__secular_contractual_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__secular_contractual_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__secular_contractual_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__secular_contractual_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__secular_contractual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fami_be_t10, family_law_authority__secular_contractual_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(fami_be_t20, family_law_authority__secular_contractual_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(fami_be_t30, family_law_authority__secular_contractual_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(fami_be_t40, family_law_authority__secular_contractual_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(fami_be_t50, family_law_authority__secular_contractual_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__secular_contractual_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fami_su_t10, family_law_authority__secular_contractual_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(fami_su_t20, family_law_authority__secular_contractual_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(fami_su_t30, family_law_authority__secular_contractual_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(fami_su_t40, family_law_authority__secular_contractual_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(fami_su_t50, family_law_authority__secular_contractual_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, inheritance_law_authority).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, parental_rights_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'family_law_authority' kernel, each representing a distinct structural claim about the nature and governance of marriage. This secular_contractual_reading focuses on state law, while other readings (e.g., religious, customary) emphasize different authorities and definitions. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
