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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Legal Personhood at Conception (Developmental Potentiality Reading)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint instantiates the 'developmental potentiality' reading of
 *   the legal personhood boundary, asserting that personhood begins at
 *   conception and any human life trajectory holder is a rights-bearer. This
 *   reading has profound implications, primarily subordinating the bodily
 *   autonomy of pregnant persons to the legal rights of a fetus and granting
 *   the state significant enforcement authority over pregnancy outcomes. The
 *   constraint is actively enforced through legal prohibitions and penalties,
 *   leading to high extraction from pregnant persons and healthcare
 *   providers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.85).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.9).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, snare).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Legal Personhood at Conception (Developmental Potentiality Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '2f5eb865-aadb-4802-84ee-bd8ca8a70e02').
narrative_ontology:cs_kernel_codification('2f5eb865-aadb-4802-84ee-bd8ca8a70e02', formalized).
narrative_ontology:cs_authority_grounding('2f5eb865-aadb-4802-84ee-bd8ca8a70e02', lineage).
narrative_ontology:cs_interpretation_layer_present('2f5eb865-aadb-4802-84ee-bd8ca8a70e02').
narrative_ontology:cs_reading_relation('2f5eb865-aadb-4802-84ee-bd8ca8a70e02', legal_personhood_boundary__restrictive_anthropocentric_reading, influences).
narrative_ontology:cs_reading_relation('2f5eb865-aadb-4802-84ee-bd8ca8a70e02', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_axiom('2f5eb865-aadb-4802-84ee-bd8ca8a70e02', foundational, human_life_begins_at_conception).
narrative_ontology:cs_axiom_status(human_life_begins_at_conception, holdable).
narrative_ontology:cs_axiom_grounding('2f5eb865-aadb-4802-84ee-bd8ca8a70e02', human_life_begins_at_conception, deontological).
narrative_ontology:cs_axiom('2f5eb865-aadb-4802-84ee-bd8ca8a70e02', foundational, potentiality_confers_rights).
narrative_ontology:cs_axiom_status(potentiality_confers_rights, holdable).
narrative_ontology:cs_axiom_grounding('2f5eb865-aadb-4802-84ee-bd8ca8a70e02', potentiality_confers_rights, deontological).
narrative_ontology:cs_reference_frame('2f5eb865-aadb-4802-84ee-bd8ca8a70e02', pre_sentience_rights_framework).
narrative_ontology:cs_drift_state('2f5eb865-aadb-4802-84ee-bd8ca8a70e02', contemporary_legal_landscape, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2f5eb865-aadb-4802-84ee-bd8ca8a70e02', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, anti_abortion_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, healthcare_providers).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, reproductive_rights_advocates).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary burden of the constraint, losing bodily autonomy and control over reproductive decisions. Their biological reality and legal restrictions combine to create a trapped exit condition.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    powerless, biographical, trapped, national).

% Gains authority and resources to monitor and enforce pregnancy outcomes, prosecuting violations of fetal personhood. This expands state power into private medical and personal decisions.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Face severe legal restrictions on medical care, potential criminalization for providing services deemed to violate fetal rights, and profound ethical dilemmas that conflict with patient autonomy.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, healthcare_providers, payer,
    organized, biographical, constrained, national).

% Achieve their core policy and moral goals, seeing legal recognition for their stance that life begins at conception and that a fetus is a full rights-bearer. They benefit from the state's enforcement of this view.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, anti_abortion_advocates, beneficiary,
    organized, generational, mobile, national).

% Their arguments for bodily autonomy, reproductive freedom, and personhood based on sentience or birth are actively suppressed and legally invalidated by this constraint. They are excluded from the legal framework's foundational premises.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, reproductive_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Analyze the legal, philosophical, and constitutional implications of defining personhood at conception, its historical context, and its impact on rights theory and jurisprudence. They do not directly benefit or pay, but critically evaluate the constraint's operation.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, legal_scholars_constitutional_law, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to establish a clear, universal boundary for legal personhood, resolving ambiguity about when human life gains legal protection and moral status, thereby providing a consistent legal framework for the protection of human life.
% TRANSFER_FUNCTION: Transfers bodily autonomy and decision-making power from pregnant persons to the state and the legal system, in favor of the legal status and rights of the fetus. It also transfers enforcement authority and resources to the state to regulate pregnancy outcomes.
% ABSENT_VOICES: Reproductive rights advocates, secular ethicists, and many medical professionals are structurally excluded from the foundational premise of this constraint. They would argue for bodily autonomy, evidence-based medical practice, and a separation of moral/theological claims from legal personhood, but their perspectives are legally overridden.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, legal systems would revert to prior or alternative personhood definitions (e.g., birth, viability, sentience). Abortion access would likely expand significantly, the legal landscape around reproductive rights would fundamentally shift, and bodily autonomy for pregnant persons would be legally restored, leading to a major reorganization of healthcare and legal practice.
% FOUNDING_PROBLEM: Ambiguity regarding the legal status of a fetus and the moral imperative, for some, to protect human life from its earliest stages, aiming to prevent the termination of what is considered a human life.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (anti-abortion advocates, some religious organizations) attest the problem is still live and urgent, citing ongoing moral concerns about abortion. Opponents (reproductive rights advocates, medical organizations, secular legal scholars) argue the 'founding problem' is a moral/theological claim being imposed as a legal one, and that the constraint's persistence is driven by ideological rather than genuinely unresolved legal ambiguity. Independent legal analysis often highlights the contested nature of the underlying philosophical claims, supporting the 'contested' status of the problem's framing.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__developmental_potentiality_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.85) is high due to the severe imposition on bodily autonomy and reproductive freedom. Suppression (0.90) is also very high, as the constraint relies on robust legal and state enforcement to prohibit alternatives (e.g., abortion) and to control medical practice. The theater ratio is low (0.10) because the constraint is a high-stakes, actively enforced legal reality with direct, tangible consequences, not a performative one. Accessibility collapse is near total (0.95) for alternatives like abortion access. Resistance is high (0.80) reflecting ongoing social and political contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of anti-abortion advocates and the state enforcement apparatus, this constraint is framed as a necessary protection of human life (a 'rope' or even 'mountain' of natural law). However, from the perspective of pregnant persons and healthcare providers, it operates as a severe 'snare' that extracts fundamental rights and imposes significant burdens through coercion. The engine's classification will highlight this divergence between the claimed coordination function and the actual extractive operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The state enforcement apparatus and anti-abortion advocates are clear beneficiaries, gaining authority and achieving their policy goals. Pregnant persons and healthcare providers are the primary targets, bearing the costs of lost autonomy, legal risk, and restricted medical practice. Reproductive rights advocates are excluded, as their core arguments are legally invalidated by this constraint's premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personhood_grounding_ambiguity,
    'Is legal personhood fundamentally grounded in developmental potentiality, or in demonstrable functional capacities (e.g., sentience, self-awareness)?',
    'Philosophical consensus on the nature of personhood, or a societal shift in moral intuitions that becomes codified in law. This is primarily a conceptual and preference-based question.',
    'If functional capacity were adopted, the victim set of this constraint would change dramatically, and the constraint itself would likely be reclassified or dissolved, as its core premise would be invalidated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personhood_grounding_ambiguity, conceptual, 'Ambiguity in the foundational criteria for legal personhood.').

omega_variable(
    state_interest_vs_individual_autonomy,
    'To what extent does the state''s interest in protecting potential life override an individual''s fundamental right to bodily autonomy and privacy?',
    'Judicial precedent or legislative action that explicitly balances these competing interests, or a constitutional amendment clarifying the scope of individual rights versus state power in this domain.',
    'A stronger emphasis on individual autonomy would reduce the constraint''s extractiveness and suppression, potentially leading to reclassification as a less coercive type. Conversely, a stronger state interest would reinforce its snare-like operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_interest_vs_individual_autonomy, preference, 'Balancing state interest in life protection against individual autonomy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal prohibitions, criminalization) or internalized (social stigma, religious pressure, lack of information)?',
    'Post-legal-change analysis: if legal prohibitions are removed but access to reproductive healthcare remains low due to social stigma or lack of resources, then internalized suppression plays a significant role.',
    'If internalized suppression is substantial, the effective suppression experienced by pregnant persons is higher than the structural legal measures alone suggest, making exit more difficult even if legal barriers are reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in reproductive rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1970, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(lega_tr_t1980, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1980, 0.13).
narrative_ontology:measurement(lega_tr_t1990, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(lega_tr_t2000, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(lega_tr_t2010, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(lega_tr_t2020, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(lega_tr_t2025, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(lega_be_t1970, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(lega_be_t1980, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(lega_be_t1990, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(lega_be_t2000, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(lega_be_t2010, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(lega_be_t2020, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2020, 0.83).
narrative_ontology:measurement(lega_be_t2025, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1970, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(lega_su_t1980, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement(lega_su_t1990, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(lega_su_t2000, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(lega_su_t2010, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(lega_su_t2020, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2020, 0.88).
narrative_ontology:measurement(lega_su_t2025, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, abortion_access_laws).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, contraception_access_laws).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, maternal_healthcare_standards).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, fetal_homicide_laws).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
