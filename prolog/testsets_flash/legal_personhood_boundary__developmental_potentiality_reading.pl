% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Legal Personhood Boundary: Developmental Potentiality Reading
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint defines legal personhood as beginning at conception,
 *   asserting that any human life trajectory holder is a rights-bearer. It is
 *   a specific reading of the broader 'legal_personhood_boundary' kernel.
 *   This reading structurally places the fetus/embryo into the victim set
 *   from conception, subordinates the pregnant person's autonomy to these
 *   fetal rights, and grants the state enforcement authority over pregnancy
 *   outcomes. The constraint operates as a snare, extracting autonomy and
 *   imposing costs on pregnant persons and healthcare providers, while
 *   benefiting anti-abortion advocates and state enforcement agencies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.85).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.92).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, snare).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Legal Personhood Boundary: Developmental Potentiality Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, 'e0a1be27-c61f-4c7b-8914-7136be0eb2bd').
narrative_ontology:cs_kernel_codification('e0a1be27-c61f-4c7b-8914-7136be0eb2bd', formalized).
narrative_ontology:cs_authority_grounding('e0a1be27-c61f-4c7b-8914-7136be0eb2bd', lineage).
narrative_ontology:cs_interpretation_layer_present('e0a1be27-c61f-4c7b-8914-7136be0eb2bd').
narrative_ontology:cs_reading_relation('e0a1be27-c61f-4c7b-8914-7136be0eb2bd', legal_personhood_boundary__restrictive_anthropocentric_reading, influences).
narrative_ontology:cs_reading_relation('e0a1be27-c61f-4c7b-8914-7136be0eb2bd', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_axiom('e0a1be27-c61f-4c7b-8914-7136be0eb2bd', foundational, human_life_potentiality_equals_personhood).
narrative_ontology:cs_axiom_status(human_life_potentiality_equals_personhood, holdable).
narrative_ontology:cs_axiom_grounding('e0a1be27-c61f-4c7b-8914-7136be0eb2bd', human_life_potentiality_equals_personhood, deontological).
narrative_ontology:cs_axiom('e0a1be27-c61f-4c7b-8914-7136be0eb2bd', secondary, state_has_compelling_interest_in_fetal_life).
narrative_ontology:cs_axiom_status(state_has_compelling_interest_in_fetal_life, holdable).
narrative_ontology:cs_axiom_grounding('e0a1be27-c61f-4c7b-8914-7136be0eb2bd', state_has_compelling_interest_in_fetal_life, conventional).
narrative_ontology:cs_reference_frame('e0a1be27-c61f-4c7b-8914-7136be0eb2bd', conception_as_personhood_origin).
narrative_ontology:cs_drift_state('e0a1be27-c61f-4c7b-8914-7136be0eb2bd', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e0a1be27-c61f-4c7b-8914-7136be0eb2bd', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, anti_abortion_advocates).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_agencies).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, healthcare_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their bodily autonomy and reproductive choices are subordinated to the legal rights of the fetus from conception. They bear the direct costs of restricted healthcare access and potential criminalization of pregnancy outcomes.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    powerless, biographical, trapped, national).

% Is granted full legal personhood and associated rights from the moment of conception, regardless of developmental stage. This status is asserted on its behalf by other agents.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetus_embryo, beneficiary,
    powerless, generational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__developmental_potentiality_reading, fetus_embryo).

% Actively promote and enforce this reading of personhood, viewing it as a moral imperative. They benefit from the legal codification of their worldview and the expansion of state power to protect fetal life.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, anti_abortion_advocates, agenda_setter,
    organized, generational, mobile, national).

% Face legal and ethical dilemmas, potential criminal liability, and restrictions on medical practice due to the expanded legal status of the fetus. Their professional autonomy is curtailed.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, healthcare_providers, payer,
    moderate, biographical, constrained, local).

% Acquire new authority and mandates to regulate and monitor pregnancy outcomes, investigate miscarriages, and enforce restrictions on reproductive healthcare. They benefit from expanded institutional scope and power.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Actively resist this reading, arguing for bodily autonomy and reproductive freedom. Their arguments are often marginalized or legally overridden by the enforcement of fetal personhood.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pro_choice_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, early boundary for legal personhood, aiming to coordinate legal protections and moral obligations around the earliest stages of human development.
% TRANSFER_FUNCTION: Transfers legal rights and protections to the fetus from conception, simultaneously transferring autonomy and decision-making power away from pregnant persons and healthcare providers to the state and legal system.
% ABSENT_VOICES: Those who advocate for personhood based on functional capacity or born status are excluded from the foundational premise of this reading; they would argue that potentiality alone is insufficient for full legal rights and that the costs to pregnant persons are unjustifiable.
% DISAPPEARANCE_RATIONALE: If this reading of personhood vanished, the legal landscape around reproductive rights, medical practice, and state intervention in pregnancy would fundamentally shift. Pregnant persons would regain full bodily autonomy, and the legal system would need to re-establish a new, later boundary for personhood, likely leading to a significant reorganization of legal and social norms.
% FOUNDING_PROBLEM: The perceived moral problem of protecting human life from its earliest stages, and the desire to establish a consistent legal framework for human rights that encompasses all stages of development.
% FOUNDING_PROBLEM_CORROBORATION: Anti-abortion advocates and some religious organizations consistently attest that the problem of protecting fetal life is live and urgent. While contested by others, the moral and legal arguments for this position remain active and are supported by a significant segment of the population and legal scholars, not solely the direct beneficiaries.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).

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
 *   The high extractiveness (0.85) reflects the significant loss of bodily autonomy and reproductive freedom for pregnant persons. Suppression (0.92) is very high due to the active legal and social enforcement mechanisms that restrict access to abortion and other reproductive healthcare, often criminalizing actions deemed harmful to the fetus. The low theater ratio (0.1) indicates that the constraint is genuinely functional in achieving its stated goal of protecting fetal life, with minimal performative maintenance. Resistance is high (0.8) due to ongoing legal challenges and social activism from pro-choice movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of anti-abortion advocates, this is a just and necessary protection of life, a 'mountain' of moral truth. From the perspective of pregnant persons and pro-choice advocates, it is a 'snare' that extracts fundamental rights and imposes severe burdens. The engine's classification will reflect the latter, given the high extractiveness and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons and healthcare providers are clear targets (high d) as they bear the direct costs and restrictions. The fetus/embryo is a beneficiary (low d), as rights are asserted on its behalf. Anti-abortion advocates and state enforcement agencies are agenda-setters and beneficiaries (low d), actively shaping and benefiting from the constraint's operation and expanded authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_vs_legal_personhood,
    'Is the assertion of moral status at conception equivalent to the granting of full legal personhood, or are these distinct concepts?',
    'Philosophical and legal consensus on the distinction between moral status (a philosophical claim) and legal personhood (a construct of positive law).',
    'If distinct, the legal personhood claim could be re-evaluated independently of the moral claim, potentially reducing its suppressive force. If equivalent, the legal constraint is more deeply entrenched.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_vs_legal_personhood, conceptual, 'Distinction between moral status and legal personhood.').

omega_variable(
    fetal_rights_vs_maternal_autonomy_balance,
    'How should the asserted rights of the fetus be balanced against the established bodily autonomy rights of the pregnant person?',
    'Judicial rulings or legislative action establishing a clear framework for balancing these competing rights, potentially through a trimester-based approach or a ''compelling state interest'' test.',
    'A re-balancing could reduce the extractiveness and suppression on pregnant persons, potentially shifting the constraint towards a tangled rope or even a rope if a genuine coordination function for both parties is found. If no balance is struck, the snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fetal_rights_vs_maternal_autonomy_balance, preference, 'Balancing fetal rights against maternal autonomy.').

omega_variable(
    enforcement_scope_creep,
    'To what extent does the state''s enforcement authority, granted by fetal personhood, extend beyond abortion to other aspects of pregnancy and reproductive health?',
    'Analysis of legal precedents and legislative trends regarding state intervention in pregnancy, including criminalization of drug use during pregnancy, forced medical procedures, or restrictions on travel for abortion.',
    'If enforcement scope is broad and expanding, the constraint''s effective suppression is higher and more pervasive than currently measured, impacting a wider range of behaviors and potentially leading to a reclassification as a more severe snare. If limited, the current metrics hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_scope_creep, empirical, 'Scope creep of state enforcement due to fetal personhood.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1970, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(lega_tr_t1985, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(lega_tr_t2000, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(lega_tr_t2010, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(lega_tr_t2024, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(lega_be_t1970, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(lega_be_t1985, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(lega_be_t2000, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(lega_be_t2010, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(lega_be_t2024, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1970, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(lega_su_t1985, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1985, 0.78).
narrative_ontology:measurement(lega_su_t2000, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(lega_su_t2010, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(lega_su_t2024, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, reproductive_healthcare_access_regulations).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, state_surveillance_of_pregnancy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legal_personhood_boundary' kernel. Its high extractiveness and suppression are distinct from other readings that may have different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
