% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment: Collective Security Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'collective security' reading of the
 *   Second Amendment, which interprets the 'right of the people to keep and
 *   bear arms' as primarily conditioned by the necessity of a 'well regulated
 *   Militia' for the security of a free state. Under this reading, the state
 *   retains significant authority to regulate firearms to serve collective
 *   security, allowing for licensing, registration, and restrictions on
 *   certain weapons. This reading was dominant for much of US history until
 *   challenged by more recent individual rights interpretations. The metrics
 *   reflect the period before the landmark Heller decision (2008), which
 *   shifted the legal landscape.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.45).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.3).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment: Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'c24d3fa8-646d-4af9-b1a5-0c03a24b8457').
narrative_ontology:cs_kernel_codification('c24d3fa8-646d-4af9-b1a5-0c03a24b8457', fixed_text).
narrative_ontology:cs_authority_grounding('c24d3fa8-646d-4af9-b1a5-0c03a24b8457', lineage).
narrative_ontology:cs_interpretation_layer_present('c24d3fa8-646d-4af9-b1a5-0c03a24b8457').
narrative_ontology:cs_reading_relation('c24d3fa8-646d-4af9-b1a5-0c03a24b8457', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('c24d3fa8-646d-4af9-b1a5-0c03a24b8457', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('c24d3fa8-646d-4af9-b1a5-0c03a24b8457', foundational, militia_clause_conditions_right).
narrative_ontology:cs_axiom_status(militia_clause_conditions_right, holdable).
narrative_ontology:cs_axiom_grounding('c24d3fa8-646d-4af9-b1a5-0c03a24b8457', militia_clause_conditions_right, conventional).
narrative_ontology:cs_axiom('c24d3fa8-646d-4af9-b1a5-0c03a24b8457', foundational, state_power_to_regulate_for_public_safety).
narrative_ontology:cs_axiom_status(state_power_to_regulate_for_public_safety, holdable).
narrative_ontology:cs_axiom_grounding('c24d3fa8-646d-4af9-b1a5-0c03a24b8457', state_power_to_regulate_for_public_safety, conventional).
narrative_ontology:cs_reference_frame('c24d3fa8-646d-4af9-b1a5-0c03a24b8457', historical_collective_right_precedent).
narrative_ontology:cs_drift_state('c24d3fa8-646d-4af9-b1a5-0c03a24b8457', post_heller_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c24d3fa8-646d-4af9-b1a5-0c03a24b8457', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_agencies).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, public_safety_advocates).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, militia_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agencies interpret the Second Amendment as permitting robust state regulation of firearms to ensure collective security. They benefit from the authority to implement licensing, registration, and other control measures, viewing these as essential for public order.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for stricter gun control measures, believing that the collective security reading of the Second Amendment provides the necessary constitutional basis for such policies. They benefit from the legal and political space this reading creates for their agenda.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, public_safety_advocates, beneficiary,
    organized, biographical, mobile, national).

% Experience the constraint through licensing requirements, waiting periods, and restrictions on certain types of firearms. They bear the costs of compliance and feel their individual right to bear arms is curtailed by the emphasis on collective security.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    moderate, biographical, constrained, local).

% The ultimate arbiter of constitutional interpretation. While this reading has historically held sway, recent decisions have shifted towards an individual rights interpretation, making the Court's current stance a critical factor for the constraint's future.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% Members of organized, state-regulated militias (e.g., National Guard) whose right to bear arms is explicitly protected and conditioned by this reading. They benefit from the clarity of their role within the collective security framework.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, militia_members, beneficiary,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's authority to regulate firearms with the constitutional right to bear arms, aiming to balance individual liberties with the collective need for public safety and an organized civic defense.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy over firearm ownership to the state, in exchange for perceived collective security and public order. This includes the transfer of decision-making power regarding permissible arms and ownership conditions.
% ABSENT_VOICES: Advocates for an expansive individual right to bear arms, independent of militia service, are often marginalized in discussions framed by this reading. They would argue that the focus on collective security unduly infringes on fundamental personal liberties.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape for firearms regulation would be fundamentally altered. State regulatory agencies would lose a key constitutional justification for their policies, leading to widespread legal challenges and a significant shift towards less restrictive gun laws, reorganizing the balance between individual and collective rights.
% FOUNDING_PROBLEM: The founding problem was to ensure the security of a free state through an organized militia, while also acknowledging a right to bear arms, in a context where standing armies were viewed with suspicion.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historians, as well as public safety organizations, corroborate that the tension between individual rights and collective security, particularly concerning firearms, remains a live and pressing issue in contemporary society. This is attested by ongoing debates and legislative efforts.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).
:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) as it imposes burdens on individual gun owners but is framed as a necessary cost for collective good. Suppression is moderate (0.30) as it relies on active state enforcement of regulations, but does not completely eliminate firearm ownership. Theater ratio is low (0.10) because the regulatory actions are generally seen as genuinely serving public safety, not merely performative. Resistance is moderate (0.55) reflecting ongoing political and legal challenges from individual rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state regulatory agencies, this is a legitimate and necessary coordination mechanism. From the perspective of individual gun owners, it is an extractive constraint that infringes on their rights. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory agencies and public safety advocates are beneficiaries, as this reading grants them the authority and justification for their policies (low directionality). Individual gun owners are payers, experiencing restrictions and compliance costs (high directionality). Militia members are also beneficiaries, as their role is explicitly protected and defined within this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_relevance_ambiguity,
    'Is the ''well regulated Militia'' clause still relevant in its original sense, given the modern military and law enforcement structures?',
    'Historical and legal scholarship on the evolution of military and civic defense, and judicial rulings on the contemporary interpretation of the militia''s role.',
    'If the militia clause is deemed anachronistic, the collective security reading loses a foundational premise, potentially shifting towards an individual rights interpretation. If it retains relevance (e.g., through the National Guard), the reading''s justification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_relevance_ambiguity, conceptual, 'Ambiguity regarding the contemporary relevance of the militia clause.').

omega_variable(
    empirical_effectiveness_of_regulation,
    'Do the firearm regulations enacted under this reading empirically reduce gun violence and enhance collective security?',
    'Public health and criminological studies on the effects of specific gun control measures on rates of violence, accidental deaths, and crime.',
    'Strong empirical evidence of effectiveness would bolster the legitimacy of the collective security reading. Lack of evidence, or evidence of unintended negative consequences, would weaken its justification and increase resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_effectiveness_of_regulation, empirical, 'Uncertainty about the empirical effectiveness of regulations justified by this reading.').

omega_variable(
    balancing_individual_vs_collective_rights,
    'What is the optimal balance point between individual rights to bear arms and the collective right to public safety, and how should this be constitutionally enshrined?',
    'Ongoing legislative debate, judicial interpretation, and public discourse reflecting societal values and evolving understandings of rights.',
    'A shift in societal consensus or judicial precedent towards prioritizing individual rights would diminish the scope of the collective security reading, while a renewed emphasis on collective safety could expand it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balancing_individual_vs_collective_rights, preference, 'The irreducible normative choice in balancing competing rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 1900, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__collective_security_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_text__collective_security_reading, theater_ratio, 1934, 0.08).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_text__collective_security_reading, theater_ratio, 1968, 0.12).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__collective_security_reading, theater_ratio, 2008, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__collective_security_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(seco_be_t1934, second_amendment_text__collective_security_reading, base_extractiveness, 1934, 0.4).
narrative_ontology:measurement(seco_be_t1968, second_amendment_text__collective_security_reading, base_extractiveness, 1968, 0.5).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__collective_security_reading, base_extractiveness, 2008, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1900, second_amendment_text__collective_security_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(seco_su_t1934, second_amendment_text__collective_security_reading, suppression_requirement, 1934, 0.25).
narrative_ontology:measurement(seco_su_t1968, second_amendment_text__collective_security_reading, suppression_requirement, 1968, 0.35).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__collective_security_reading, suppression_requirement, 2008, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
