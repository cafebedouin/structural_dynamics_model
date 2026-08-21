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
 *   human_readable: Legal Personhood at Conception (Developmental Potentiality Reading)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint defines legal personhood as beginning at conception,
 *   asserting that any human life trajectory holder is a rights-bearer from
 *   that point. This 'developmental potentiality' reading of the legal
 *   personhood boundary kernel has profound implications for reproductive
 *   rights, medical practice, and state authority over pregnancy outcomes. It
 *   is claimed by its proponents as a necessary coordination mechanism for
 *   protecting human life, but its operation involves substantial extraction
 *   from pregnant persons and active suppression of alternative legal and
 *   ethical framings.
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
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Legal Personhood at Conception (Developmental Potentiality Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, 'ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569').
narrative_ontology:cs_kernel_codification('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569', formalized).
narrative_ontology:cs_authority_grounding('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569', lineage).
narrative_ontology:cs_interpretation_layer_present('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569').
narrative_ontology:cs_reading_relation('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_reading_relation('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569', legal_personhood_boundary__restrictive_anthropocentric_reading, influences).
narrative_ontology:cs_axiom('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569', foundational, human_life_begins_at_conception).
narrative_ontology:cs_axiom_status(human_life_begins_at_conception, holdable).
narrative_ontology:cs_axiom_grounding('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569', human_life_begins_at_conception, deontological).
narrative_ontology:cs_axiom('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569', foundational, potentiality_confers_rights).
narrative_ontology:cs_axiom_status(potentiality_confers_rights, holdable).
narrative_ontology:cs_axiom_grounding('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569', potentiality_confers_rights, deontological).
narrative_ontology:cs_reference_frame('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569', unborn_life_sacred_framework).
narrative_ontology:cs_drift_state('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569', contemporary_legal_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ba5bfdf2-efd4-4b53-a46d-74a4f9c0f569', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, fetus).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, pro_life_advocates).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_agencies).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, medical_providers).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, right_to_life_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, fetal_personhood_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary burden of this constraint, losing bodily autonomy and decision-making power over their own reproductive health. Their life trajectory is fundamentally altered by the legal obligation to carry a pregnancy to term.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    powerless, immediate, trapped, national).

% Is granted full legal personhood and associated rights from the moment of conception, becoming a legal subject with protections that can override the autonomy of the pregnant person.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetus, beneficiary,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__developmental_potentiality_reading, fetus).

% Face severe legal restrictions on the medical care they can provide, including potential criminalization for offering services deemed to violate fetal rights. This creates ethical dilemmas and limits their professional practice.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, medical_providers, payer,
    organized, biographical, constrained, national).

% Achieve their core policy and moral objectives by establishing legal personhood at conception, gaining significant political influence and seeing their worldview codified into law.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pro_life_advocates, beneficiary,
    organized, generational, mobile, national).

% Acquire new powers and mandates to regulate and enforce pregnancy outcomes, potentially including surveillance and criminal prosecution related to reproductive decisions. This expands their institutional scope and authority.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Are actively excluded from the legal framework's foundational premise regarding personhood, despite their organized resistance. Their arguments for bodily autonomy and reproductive freedom are legally subordinated.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pro_choice_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate society around the protection of all human life from conception, establishing a clear and consistent legal status for the unborn and guiding moral and legal obligations.
% TRANSFER_FUNCTION: Transfers bodily autonomy and decision-making power from pregnant persons to the state (acting on behalf of the fetus), and legal/political power to groups advocating for fetal personhood.
% ABSENT_VOICES: Pro-choice advocates, ethicists emphasizing bodily autonomy, and those who define personhood by demonstrable cognitive capacity are structurally excluded from the legal framework's foundational premise. Their perspectives are actively suppressed or dismissed within the legal discourse.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, legal frameworks would revert to prior or alternative understandings of personhood, abortion access would expand, and the legal status of the fetus would fundamentally change. This would reorganize medical practice, legal advocacy, and the political landscape around reproductive rights.
% FOUNDING_PROBLEM: The perceived moral and legal problem of human life being terminated before birth, and the lack of legal protection for the unborn, which is seen as a failure to uphold the sanctity of life.
% FOUNDING_PROBLEM_CORROBORATION: Pro-life organizations, religious institutions, and some legal scholars consistently attest to the problem's live status, citing ongoing abortions as evidence. Opponents (pro-choice groups, medical associations, human rights organizations) contest this framing, arguing it's a moral/religious claim, not a universally accepted legal problem, and that the 'problem' is a constructed one used to justify control over bodies.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.85) reflects the significant loss of bodily autonomy and decision-making power imposed on pregnant persons. Suppression (0.90) is severe due to the active legal and institutional enforcement required to maintain this definition of personhood, including criminalization threats for medical providers and the effective collapse of alternatives like abortion. The theater ratio is low (0.10) because the constraint is genuinely and actively enforced, with real-world consequences, rather than being performative. Resistance is high (0.80) due to ongoing social and political contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pro-life advocates and state enforcement agencies, this constraint is a necessary and just mechanism for protecting vulnerable life (a 'tangled rope' with a strong coordination claim). From the perspective of pregnant persons and medical providers, it operates as a highly extractive and suppressive 'snare', leveraging state power to control bodies and medical decisions. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The fetus is the primary beneficiary, gaining legal rights and protections. Pro-life advocates benefit by achieving their policy goals. State enforcement agencies benefit from expanded authority. Pregnant persons are the primary targets, bearing the costs of lost autonomy. Medical providers are also targets, facing legal risks and restrictions. Pro-choice advocates are excluded from the legal framework's premise, making them indirect targets whose views are suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personhood_definition_ambiguity,
    'Is legal personhood fundamentally tied to developmental potentiality, or to demonstrable functional capacities (e.g., sentience, self-awareness)?',
    'A societal consensus shift or a definitive judicial/legislative ruling that redefines the foundational criteria for legal personhood, potentially incorporating neuroscientific or philosophical understandings of consciousness.',
    'If personhood is redefined by functional capacity, the victim set would change, and the constraint''s extractiveness on early-stage pregnancies would decrease significantly, potentially reclassifying it as a different type (e.g., a rope for later-stage life).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personhood_definition_ambiguity, conceptual, 'Ambiguity in the foundational definition of legal personhood.').

omega_variable(
    bodily_autonomy_vs_fetal_rights_priority,
    'How should the rights of a pregnant person to bodily autonomy be balanced against the asserted rights of a fetus?',
    'A legal framework that explicitly defines the hierarchy or scope of these competing rights, or a societal shift in moral values that prioritizes one over the other in cases of conflict.',
    'If bodily autonomy is prioritized, the constraint''s suppression and extractiveness on pregnant persons would decrease, potentially shifting its classification away from a snare. If fetal rights are further prioritized, extractiveness and suppression would intensify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_vs_fetal_rights_priority, preference, 'Irreducible conflict between bodily autonomy and fetal rights.').

omega_variable(
    functional_capacity_reading_delta,
    'What would be the structural changes if the `functional_capacity_reading` of legal personhood were adopted instead of this developmental potentiality reading?',
    'Analysis of legal and ethical frameworks based on functional capacity, such as those applied to non-human animals or individuals in persistent vegetative states.',
    'Adopting the functional capacity reading would likely remove early-stage fetuses from the victim set, significantly reducing the constraint''s extractiveness on pregnant persons and medical providers, and shifting the focus of legal protection to later stages of development or post-birth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_capacity_reading_delta, conceptual, 'Impact of adopting the functional_capacity_reading sibling.').

omega_variable(
    restrictive_anthropocentric_reading_delta,
    'What would be the structural changes if the `restrictive_anthropocentric_reading` of legal personhood were adopted instead of this developmental potentiality reading?',
    'Analysis of legal and ethical frameworks that limit personhood to born humans with cognitive capacity, potentially excluding some individuals with severe cognitive impairments.',
    'Adopting the restrictive anthropocentric reading would narrow the scope of personhood, potentially excluding some individuals currently protected, but it would still maintain a human-centric view. This reading, however, is less expansive than the developmental potentiality reading in terms of when personhood begins, thus reducing the extractiveness on early-stage pregnancies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restrictive_anthropocentric_reading_delta, conceptual, 'Impact of adopting the restrictive_anthropocentric_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t2000, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(lega_tr_t2005, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2005, 0.11).
narrative_ontology:measurement(lega_tr_t2010, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(lega_tr_t2015, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(lega_tr_t2020, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(lega_tr_t2025, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(lega_be_t2000, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(lega_be_t2005, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement(lega_be_t2010, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(lega_be_t2015, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2015, 0.82).
narrative_ontology:measurement(lega_be_t2020, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(lega_be_t2025, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t2000, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(lega_su_t2005, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2005, 0.83).
narrative_ontology:measurement(lega_su_t2010, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2010, 0.86).
narrative_ontology:measurement(lega_su_t2015, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2015, 0.88).
narrative_ontology:measurement(lega_su_t2020, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2020, 0.89).
narrative_ontology:measurement(lega_su_t2025, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
