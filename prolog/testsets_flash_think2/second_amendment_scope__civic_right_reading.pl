% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment: Individual Right Conditioned on Civic Militia Participation
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the 'civic right' reading of the
 *   Second Amendment, which interprets the right to bear arms as an
 *   individual right conditioned on participation in a well-regulated
 *   militia. This reading attempts to reconcile the 'individual right' and
 *   'militia' clauses of the amendment, asserting that the right is not
 *   absolute but tied to a civic duty. It stands in contrast to readings that
 *   emphasize an unrestricted individual right or a purely collective state
 *   right.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.55).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.6).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment: Individual Right Conditioned on Civic Militia Participation").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, 'd0a700b3-6cfa-460e-816a-8212ee233856').
narrative_ontology:cs_kernel_codification('d0a700b3-6cfa-460e-816a-8212ee233856', fixed_text).
narrative_ontology:cs_authority_grounding('d0a700b3-6cfa-460e-816a-8212ee233856', lineage).
narrative_ontology:cs_interpretation_layer_present('d0a700b3-6cfa-460e-816a-8212ee233856').
narrative_ontology:cs_reading_relation('d0a700b3-6cfa-460e-816a-8212ee233856', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('d0a700b3-6cfa-460e-816a-8212ee233856', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_axiom('d0a700b3-6cfa-460e-816a-8212ee233856', foundational, individual_right_conditioned_on_militia_service).
narrative_ontology:cs_axiom_status(individual_right_conditioned_on_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('d0a700b3-6cfa-460e-816a-8212ee233856', individual_right_conditioned_on_militia_service, deontological).
narrative_ontology:cs_axiom('d0a700b3-6cfa-460e-816a-8212ee233856', foundational, well_regulated_militia_is_necessary_for_free_state).
narrative_ontology:cs_axiom_status(well_regulated_militia_is_necessary_for_free_state, holdable).
narrative_ontology:cs_axiom_grounding('d0a700b3-6cfa-460e-816a-8212ee233856', well_regulated_militia_is_necessary_for_free_state, conventional).
narrative_ontology:cs_reference_frame('d0a700b3-6cfa-460e-816a-8212ee233856', founding_era_militia_concept).
narrative_ontology:cs_drift_state('d0a700b3-6cfa-460e-816a-8212ee233856', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d0a700b3-6cfa-460e-816a-8212ee233856', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, unrestricted_gun_rights_advocates).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, collective_right_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, general_public).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals possess the right to bear arms, but it is conditioned on their potential or actual participation in a civic militia. They benefit from the right but bear the cost of its conditionality and potential duties. Exiting means renouncing the right or refusing civic duty.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, militia_eligible_citizens, payer).

% States are responsible for organizing and maintaining militias, and thus have a legitimate interest in regulating arms ownership to ensure a 'well-regulated' force. They benefit from the security provided by a civic militia but bear the cost of administration and potential legal challenges.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% These groups advocate for an individual right to bear arms that is not conditioned on militia service. From their perspective, the conditionality of this reading imposes an undue burden or restriction, making them targets of its extractive aspect. Their exit is through legal and political challenge.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, unrestricted_gun_rights_advocates, payer,
    organized, biographical, constrained, national).

% These groups argue the Second Amendment protects only the state's right to maintain a militia, not an individual right. This reading, by asserting an individual right (even if conditioned), fundamentally excludes their interpretation. Their exit is through continued legal and political advocacy.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, collective_right_advocates, excluded,
    organized, biographical, constrained, national).

% The federal courts, particularly the Supreme Court, are the ultimate arbiters of the Second Amendment's meaning. They interpret the text, historical context, and evolving societal norms to define the scope of the right. Their role is to administer and enforce the constraint through legal precedent.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% The public benefits from the collective security a well-regulated militia might provide, but also bears the societal costs of gun violence and the administrative burden of any regulations tied to militia service. Their options are limited by the prevailing legal interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, general_public, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure the security of a free state by coordinating individual arms ownership with the collective defense capabilities of a civic militia, balancing individual liberty with public safety.
% TRANSFER_FUNCTION: Transfers the responsibility for maintaining a 'well-regulated militia' to individuals (through conditional rights and potential duties) and to state governments (through regulatory authority), in exchange for the right to bear arms and collective security.
% ABSENT_VOICES: Advocates for an unrestricted individual right would object to any conditionality, arguing it infringes on a fundamental liberty. Advocates for a purely collective right would object to the recognition of any individual right, arguing it misinterprets the amendment's primary purpose.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the legal and political landscape surrounding gun ownership would be fundamentally altered. Either an unrestricted individual right or a purely collective right would likely emerge, leading to a complete overhaul of arms regulation, militia organization, and the balance of power between citizens and the state regarding firearms.
% FOUNDING_PROBLEM: To address concerns about both federal overreach (by ensuring an armed populace capable of forming militias) and the need for an organized, effective defense force for the newly formed states, while also acknowledging an individual's right to self-defense.
% FOUNDING_PROBLEM_CORROBORATION: Historical documents from the founding era, including debates during the ratification of the Constitution and early state constitutions, corroborate the intent to balance individual arms bearing with militia service. Legal scholars and historians, from various perspectives, continue to attest to the ongoing relevance of these foundational tensions, even if their interpretations differ.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it performs a genuine coordination function (ensuring a civic militia for state security) but also involves asymmetric extraction. Individuals seeking to exercise the right bear the cost of conditionality and potential militia duties, while the state benefits from the organized force. Active enforcement is required to define militia eligibility, regulate arms, and adjudicate disputes over the scope of the right. Extractiveness is moderate, reflecting the burden of conditionality. Suppression is moderate due to the legal and practical limitations placed on the right. Theater ratio is low, as the militia concept, while debated, is a genuine component of this reading's justification.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments and some citizens, this reading provides a balanced approach to rights and responsibilities. However, from the perspective of unrestricted gun rights advocates, it is an extractive imposition on a fundamental liberty. Collective right advocates see it as a misinterpretation that grants too much individual power. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible citizens are beneficiaries as they possess the right, but also payers due to the associated duties. State governments are agenda-setters and beneficiaries, gaining security from the militia. Unrestricted gun rights advocates are payers, as the conditionality extracts from their desired scope of the right. Collective right advocates are excluded, as their core premise of no individual right is foreclosed by this reading. The federal judiciary acts as an agenda-setter, interpreting and enforcing the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_relevance_ambiguity,
    'Is the concept of a ''well-regulated militia'' as understood at the founding still relevant to contemporary national security and civic defense, or has its functional role atrophied?',
    'Empirical analysis of modern defense structures, state-level militia organization, and the actual role of civilian arms in national security scenarios. Legal scholarship on the evolving interpretation of ''militia''.',
    'If the militia concept is functionally obsolete, the conditionality of the right becomes more extractive and less justifiable as a coordination mechanism, potentially shifting the classification towards a Snare or Piton. If it remains relevant, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_relevance_ambiguity, empirical, 'The functional relevance of the militia clause in modern context.').

omega_variable(
    conditionality_enforcement_feasibility,
    'How practically feasible and constitutionally permissible is it for states to define and enforce ''militia eligibility'' and ''well-regulated'' status for individual arms bearers in the contemporary era?',
    'Legal challenges to state-level militia regulations, empirical studies on the administrative burden and effectiveness of such regulations, and judicial rulings on their constitutionality.',
    'If enforcement is practically infeasible or consistently struck down, the ''conditioned'' aspect of the right becomes performative, increasing the theater_ratio and potentially shifting the classification towards a Piton. If feasible, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_enforcement_feasibility, empirical, 'The practical and legal feasibility of enforcing militia conditionality.').

omega_variable(
    framing_underdetermination_second_amendment,
    'Is the ''civic right'' framing the most defensible interpretation of the Second Amendment, or do alternative framings (unconditioned individual right, purely collective right) offer equally coherent, albeit different, structural accounts?',
    'Continued legal and historical scholarship, judicial review, and public debate. The engine''s cross-reading analysis will highlight structural divergences.',
    'If alternative framings are equally coherent, the ''civic right'' reading''s classification is understood as one among several valid, competing interpretations, rather than a definitive structural truth. This reinforces the conceptual omega regarding the kernel''s contested nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_second_amendment, conceptual, 'Underdetermination of the Second Amendment''s core meaning by different interpretive frames.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__civic_right_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__civic_right_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__civic_right_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__civic_right_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(seco_tr_t50, second_amendment_scope__civic_right_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__civic_right_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__civic_right_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__civic_right_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__civic_right_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(seco_be_t50, second_amendment_scope__civic_right_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__civic_right_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__civic_right_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__civic_right_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__civic_right_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(seco_su_t50, second_amendment_scope__civic_right_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, gun_control_legislation_federal).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, state_militia_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_scope' kernel. It is linked to its sibling readings and to related legislative constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
