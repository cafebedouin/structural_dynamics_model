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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint represents the 'developmental potentiality' reading of
 *   the legal personhood boundary, asserting that personhood and full rights
 *   begin at conception due to the inherent potential for human life. This
 *   reading places the fetus/embryo in the victim set and subordinates the
 *   autonomy of pregnant persons to fetal rights, leading to substantial
 *   extraction and suppression. The constraint is claimed as a 'snare' due to
 *   its coercive nature and identifiable victims, despite proponents often
 *   framing it as a 'mountain' (natural law) or 'rope' (coordination around
 *   life's onset).
 *
 * KEY AGENTS:
 *   - pregnant_persons: Primary target (powerless/trapped) — bears extraction and loss of autonomy.
 *   - fetus_embryo: Primary beneficiary (powerless/trapped) — receives legal rights and protections.
 *   - anti_abortion_advocates: Agenda setter (organized/mobile) — benefits from and enforces the constraint.
 *   - state_enforcement_agencies: Agenda setter (institutional/constrained) — benefits from expanded authority.
 *   - healthcare_providers: Payer (moderate/constrained) — bears costs through legal and ethical dilemmas.
 *   - pro_choice_advocates: Excluded (organized/constrained) — would object but are structurally marginalized.
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
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, snare).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Legal Personhood Boundary: Developmental Potentiality Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '136dd2eb-c938-41e3-bf05-23da42aa8d5a').
narrative_ontology:cs_kernel_codification('136dd2eb-c938-41e3-bf05-23da42aa8d5a', formalized).
narrative_ontology:cs_authority_grounding('136dd2eb-c938-41e3-bf05-23da42aa8d5a', extraction).
narrative_ontology:cs_interpretation_layer_present('136dd2eb-c938-41e3-bf05-23da42aa8d5a').
narrative_ontology:cs_reading_relation('136dd2eb-c938-41e3-bf05-23da42aa8d5a', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_reading_relation('136dd2eb-c938-41e3-bf05-23da42aa8d5a', legal_personhood_boundary__restrictive_anthropocentric_reading, influences).
narrative_ontology:cs_axiom('136dd2eb-c938-41e3-bf05-23da42aa8d5a', foundational, human_life_potential_equals_rights).
narrative_ontology:cs_axiom_status(human_life_potential_equals_rights, holdable).
narrative_ontology:cs_axiom_grounding('136dd2eb-c938-41e3-bf05-23da42aa8d5a', human_life_potential_equals_rights, deontological).
narrative_ontology:cs_axiom('136dd2eb-c938-41e3-bf05-23da42aa8d5a', foundational, conception_is_life_onset).
narrative_ontology:cs_axiom_status(conception_is_life_onset, holdable).
narrative_ontology:cs_axiom_grounding('136dd2eb-c938-41e3-bf05-23da42aa8d5a', conception_is_life_onset, theological).
narrative_ontology:cs_reference_frame('136dd2eb-c938-41e3-bf05-23da42aa8d5a', unqualified_fetal_personhood).
narrative_ontology:cs_drift_state('136dd2eb-c938-41e3-bf05-23da42aa8d5a', contemporary_bioethics_debate, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('136dd2eb-c938-41e3-bf05-23da42aa8d5a', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, anti_abortion_advocates).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_agencies).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, healthcare_providers).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, sanctity_of_life_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their bodily autonomy and reproductive choices are directly constrained by the legal recognition of fetal personhood from conception. They bear the primary costs of this constraint through forced pregnancy, limited healthcare access, and potential criminalization of pregnancy outcomes.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    powerless, biographical, trapped, national).

% Receives legal rights and protections from conception, elevating its status to a rights-bearer. This is a non-agent entity that benefits from the constraint's operation.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetus_embryo, beneficiary,
    powerless, biographical, trapped, universal).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__developmental_potentiality_reading, fetus_embryo).

% Actively promote and enforce this reading of personhood, benefiting from its legal codification and the resulting restrictions on abortion and reproductive healthcare. They set the agenda for legislative and judicial action.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, anti_abortion_advocates, agenda_setter,
    organized, generational, mobile, national).

% Acquire new authority and mandates to enforce laws derived from fetal personhood, including monitoring pregnancy outcomes, prosecuting individuals for perceived harm to a fetus, and regulating reproductive healthcare. They benefit from expanded institutional power.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Face legal and ethical dilemmas, increased liability, and restrictions on their medical practice due to the legal status of the fetus. They bear the costs of navigating complex legal frameworks and potential criminalization for providing standard care.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, healthcare_providers, payer,
    moderate, biographical, constrained, local).

% Actively resist this reading of personhood, advocating for reproductive autonomy and access to abortion. While they are in the conversation, their core arguments are often structurally excluded from the legal framework once this reading is codified.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pro_choice_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate legal and moral frameworks around a consistent definition of human life's onset, providing a clear boundary for rights-bearing status.
% TRANSFER_FUNCTION: Transfers legal rights and protections to the fetus from conception, and transfers autonomy and decision-making power away from pregnant persons and healthcare providers to the state and the fetus.
% ABSENT_VOICES: The voices of those who advocate for personhood based on functional capacity or who prioritize bodily autonomy are actively suppressed or excluded from the legal framework once this reading is codified. Their arguments are not given equal weight in the legal discourse.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal landscape around reproductive rights would fundamentally shift. Abortion would likely be decriminalized, pregnant persons would regain full bodily autonomy, and state enforcement agencies would lose their mandates over pregnancy outcomes. Healthcare practices would revert to prioritizing the pregnant person's health and choices.
% FOUNDING_PROBLEM: To establish a clear, unambiguous starting point for human life and moral status, resolving perceived ambiguities in earlier legal and philosophical traditions regarding when a human organism becomes a rights-bearer.
% FOUNDING_PROBLEM_CORROBORATION: Anti-abortion advocates and some religious organizations attest that the problem of defining life's onset is still live and requires this clear boundary. Pro-choice advocates and many medical organizations contest this, arguing that the problem is a moral/theological one, not a legal one requiring state enforcement, and that the 'solution' creates more harm than it solves. Independent bioethicists often highlight the ongoing philosophical debate without endorsing a single legal solution.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The high extractiveness (0.85) reflects the profound loss of bodily autonomy and reproductive freedom for pregnant persons. Suppression (0.90) is severe due to legal prohibitions, criminalization risks, and the active enforcement required to maintain the constraint. Theater ratio is low (0.10) because the constraint's function is direct and coercive, not performative; it genuinely restricts behavior. Accessibility collapse is moderate (0.70) as legal avenues for reproductive choice are significantly curtailed, though not entirely eliminated in all contexts. Resistance is high (0.80) due to ongoing social and political movements opposing the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of anti-abortion advocates, this constraint is a necessary 'mountain' or 'rope' that protects vulnerable life and establishes a clear moral boundary. From the perspective of pregnant persons and pro-choice advocates, it is a 'snare' that extracts fundamental rights and imposes state control over personal medical decisions. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons are full targets (high d) due to direct bodily imposition and limited exit. The fetus/embryo is a full beneficiary (low d) as it gains rights without cost. Anti-abortion advocates and state agencies are beneficiaries/agenda-setters (low d) as they achieve their policy goals and expand their authority. Healthcare providers are targets (high d) due to legal risks and practice restrictions. Pro-choice advocates are excluded, their arguments structurally marginalized.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (protecting fetal life from conception) is actively pursued and enforced. However, the 'founding problem' of establishing a clear moral boundary is contested, with opponents arguing that the legal solution is an overreach that creates new problems rather than solving the original one. The classification as a snare prevents mislabeling this as a coordination mechanism when its primary effect is coercive extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_norm,
    'Is the claim that personhood begins at conception a natural law (mountain) or a constructed legal/moral norm (snare)?',
    'Philosophical consensus on the inherent, non-negotiable nature of the claim, or empirical evidence of its cultural and historical contingency. Legal precedent alone is insufficient.',
    'If a natural law, the constraint''s extractiveness would be re-evaluated as an inherent cost of reality. If a constructed norm, its high extractiveness and suppression confirm its snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_norm, conceptual, 'Ambiguity regarding the ontological status of the personhood boundary.').

omega_variable(
    bodily_autonomy_vs_fetal_rights_priority,
    'How should the conflict between a pregnant person''s bodily autonomy and asserted fetal rights be resolved?',
    'Societal consensus, judicial precedent, or legislative action that clearly prioritizes one over the other, or establishes a framework for balancing them.',
    'Prioritizing bodily autonomy would significantly reduce extractiveness and suppression for pregnant persons. Prioritizing fetal rights, as this reading does, maintains high extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bodily_autonomy_vs_fetal_rights_priority, preference, 'Irreducible conflict of fundamental rights.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, criminalization) or internalized (social stigma, fear of prosecution)?',
    'Post-legal-change suppression trajectory: if suppression persists after legal barriers are removed, reclassify as partially internalized. Surveys of pregnant persons'' decision-making under different legal regimes.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — pregnant persons carry the suppression with them even in less restrictive legal environments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for reproductive choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lega_tr_t5, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(lega_tr_t15, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(lega_be_t5, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 10, 0.83).
narrative_ontology:measurement(lega_be_t15, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(lega_su_t5, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(lega_su_t15, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 15, 0.88).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, reproductive_healthcare_access_constraint).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, maternal_fetal_conflict_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legal_personhood_boundary' kernel. It defines personhood from conception, influencing related constraints on reproductive healthcare and maternal-fetal conflict. Sibling readings (functional_capacity_reading, restrictive_anthropocentric_reading) offer alternative definitions of personhood.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
