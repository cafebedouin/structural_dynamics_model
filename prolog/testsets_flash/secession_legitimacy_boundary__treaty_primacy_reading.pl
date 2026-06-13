% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Secession Legitimacy Boundary (Treaty Primacy Reading)
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint asserts that Indigenous treaty rights, as pre-existing
 *   and foundational, supersede both federal and provincial authority, making
 *   any provincial secession illegitimate without the explicit consent of
 *   treaty holders. It is a 'tangled_rope' because it coordinates the
 *   relationship between Indigenous nations and the Crown (federal
 *   government) while simultaneously extracting from provincial separatist
 *   movements by denying their unilateral claims. The constraint requires
 *   active enforcement by the federal government to uphold treaty obligations
 *   and resist provincial overreach.
 *
 * KEY AGENTS:
 *   - indigenous_nations: Primary beneficiary (institutional/generational) — their inherent sovereignty and treaty rights are affirmed.
 *   - federal_government_as_treaty_partner: Agenda setter/beneficiary (institutional/generational) — upholds its constitutional and moral obligations, gains legitimacy as a protector of rights.
 *   - provincial_separatist_movements: Primary target/payer (organized/biographical) — their claims to unilateral secession are delegitimized, forcing negotiation with Indigenous peoples.
 *   - provincial_governments_seeking_unilateral_secession: Payer (institutional/generational) — their authority is constrained by pre-existing treaty obligations.
 *   - international_observers: Observer (analytical/civilizational) — monitor compliance with international law and Indigenous rights.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.65).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.75).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Secession Legitimacy Boundary (Treaty Primacy Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, '5774859d-44ec-4116-9146-5abf6102c7d2').
narrative_ontology:cs_kernel_codification('5774859d-44ec-4116-9146-5abf6102c7d2', fixed_text).
narrative_ontology:cs_authority_grounding('5774859d-44ec-4116-9146-5abf6102c7d2', lineage).
narrative_ontology:cs_interpretation_layer_present('5774859d-44ec-4116-9146-5abf6102c7d2').
narrative_ontology:cs_reading_relation('5774859d-44ec-4116-9146-5abf6102c7d2', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('5774859d-44ec-4116-9146-5abf6102c7d2', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('5774859d-44ec-4116-9146-5abf6102c7d2', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('5774859d-44ec-4116-9146-5abf6102c7d2', foundational, treaty_as_sacred_covenant).
narrative_ontology:cs_axiom_status(treaty_as_sacred_covenant, holdable).
narrative_ontology:cs_axiom_grounding('5774859d-44ec-4116-9146-5abf6102c7d2', treaty_as_sacred_covenant, deontological).
narrative_ontology:cs_axiom('5774859d-44ec-4116-9146-5abf6102c7d2', foundational, indigenous_sovereignty_predates_crown).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_predates_crown, holdable).
narrative_ontology:cs_axiom_grounding('5774859d-44ec-4116-9146-5abf6102c7d2', indigenous_sovereignty_predates_crown, deontological).
narrative_ontology:cs_reference_frame('5774859d-44ec-4116-9146-5abf6102c7d2', pre_confederation_treaty_relationship).
narrative_ontology:cs_drift_state('5774859d-44ec-4116-9146-5abf6102c7d2', contemporary_reconciliation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5774859d-44ec-4116-9146-5abf6102c7d2', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, federal_government_as_treaty_partner).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_separatist_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments_seeking_unilateral_secession).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, treaty_as_sacred_covenant).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__treaty_primacy_reading, inherent_indigenous_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their inherent sovereignty and treaty rights are affirmed and protected, providing a strong legal and moral basis to resist unilateral provincial secession. Their identity is deeply tied to their ancestral lands and treaty relationships.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_nations, beneficiary,
    institutional, generational, identity_locked, regional).

% Bound by constitutional and moral obligations to uphold treaties, it acts as a guarantor of Indigenous rights against provincial claims. This role enhances its legitimacy but also creates political friction with provincial governments.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government_as_treaty_partner, agenda_setter,
    institutional, generational, constrained, national).

% Their political project of unilateral secession is directly challenged and delegitimized by the requirement of Indigenous consent, forcing them to engage in complex negotiations or abandon their claims.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_separatist_movements, payer,
    organized, biographical, constrained, regional).

% Their authority to unilaterally determine the province's future is constrained by pre-existing treaty obligations and the federal government's role as a treaty partner. They face legal and political challenges if they ignore Indigenous consent.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments_seeking_unilateral_secession, payer,
    institutional, generational, constrained, national).

% Monitor the situation for compliance with international human rights law and Indigenous rights declarations, potentially influencing global opinion and diplomatic pressure on the federal and provincial governments.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_nations).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between Indigenous nations and the Crown (federal government) by affirming the enduring nature and supremacy of treaty agreements, providing a framework for ongoing nation-to-nation relations.
% TRANSFER_FUNCTION: Transfers ultimate authority over territorial claims and political legitimacy from provincial unilateralism to a framework requiring Indigenous consent, effectively transferring political leverage and resource control.
% ABSENT_VOICES: Provincial citizens who believe in absolute popular sovereignty within provincial borders, and who would object to Indigenous consent as a prerequisite for secession, are often excluded from the direct treaty dialogue, their views mediated through provincial political structures.
% DISAPPEARANCE_RATIONALE: If this reading vanished, provincial separatist movements would immediately assert unilateral claims, Indigenous nations would lose a critical legal and moral shield, and the entire federal-provincial-Indigenous relationship would destabilize, leading to widespread legal challenges and potential conflict over land and resources.
% FOUNDING_PROBLEM: The historical and ongoing assertion of colonial sovereignty over Indigenous lands and peoples, leading to the erosion of Indigenous rights and self-determination, and the potential for provincial secession to further undermine these rights.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal scholars, elders, and international human rights bodies consistently corroborate that the founding problem of colonial assertion and the need to affirm treaty primacy remains live and urgent. This corroboration comes from outside the direct federal or provincial political structures.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because it directly curtails the political aspirations of provincial separatist movements, forcing them to contend with Indigenous sovereignty. Suppression (0.75) is high due to the active legal and political enforcement required to uphold treaty primacy against provincial claims. The theater ratio (0.4) reflects that while the principle is real, its full enforcement against a determined provincial government might involve performative legal battles and political posturing. The increasing extractiveness and suppression over time reflect the growing recognition and assertion of Indigenous rights, leading to greater resistance from those whose claims are curtailed.
 *
 * PERSPECTIVAL GAP:
 *   Indigenous nations experience this as a 'rope' or even a 'mountain' (affirmation of inherent rights), while provincial separatist movements experience it as a 'snare' (their path to unilateral secession is blocked). The federal government, as a treaty partner, experiences it as a 'tangled_rope' (coordinating its relationship with Indigenous nations while extracting from provincial overreach).
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are full beneficiaries (d=0.0) as their rights are affirmed and protected. The federal government, as a treaty partner, is a beneficiary (d=0.1) as it fulfills its constitutional and moral obligations, enhancing its legitimacy. Provincial separatist movements and provincial governments seeking unilateral secession are targets (d=0.9-1.0) as their claims are directly challenged and their options constrained by the requirement of Indigenous consent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling the assertion of Indigenous sovereignty as mere political obstruction. By framing it as a 'tangled_rope', it acknowledges both the genuine coordination function (upholding treaty relationships) and the asymmetric extraction from those who would ignore these rights. It highlights that the 'mandate' of Indigenous rights is not 'atrophied' but is actively being asserted and enforced, leading to a re-evaluation of sovereignty claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of treaty primacy, or a strategic framing to block secession?',
    'Analysis of historical jurisprudence and Indigenous legal traditions independent of federal or provincial political interests.',
    'If a genuine reflection, it strengthens Indigenous sovereignty; if strategic, it exposes a political maneuver using Indigenous rights as a shield.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''treaty_primacy_reading'' of the ''secession_legitimacy_boundary'' kernel. Sibling readings include ''constitutional_impossibility_reading'', ''popular_sovereignty_reading'', and ''grievance_threshold_reading''. This reading asserts Indigenous consent is a prerequisite for legitimate secession, which would place Indigenous peoples in the victim set if secession proceeds without consultation.').

omega_variable(
    enforcement_capacity_ambiguity,
    'Does the federal government possess the political will and legal capacity to enforce treaty primacy against a determined provincial secessionist movement?',
    'Observation of federal response to a hypothetical or actual provincial declaration of independence without Indigenous consent.',
    'If enforcement capacity is low, the constraint''s effective suppression is lower than stated, and its classification shifts towards a piton or even a snare for Indigenous nations if their rights are ignored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_ambiguity, empirical, 'The actual capacity to enforce treaty primacy against a provincial government is uncertain, impacting the constraint''s real-world suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'secession_legitimacy_boundary' kernel. Its structural delta is that neither federal nor provincial authority can unilaterally alter treaty relationships, and separatist claims are invalid without Indigenous consent. Indigenous peoples enter the victim set if secession proceeds without consultation. It is linked to other readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
