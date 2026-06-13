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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment Scope: Civic Right Reading (Militia-Conditioned Individual Right)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the 'civic right' reading of the Second
 *   Amendment, which interprets the right to bear arms as an individual
 *   right, but one that is conditioned on or closely tied to participation in
 *   a well-regulated militia. This reading emphasizes the civic duty aspect
 *   of arms ownership, allowing for state regulation to ensure that arms are
 *   used for collective defense rather than purely private purposes. It
 *   stands in contrast to both a purely individual right (unconditioned) and
 *   a purely collective right (only states can maintain militias).
 *
 * KEY AGENTS:
 *   - militia_eligible_citizens: Primary beneficiary (moderate/constrained) — benefits from the right but bears the condition of service.
 *   - state_governments: Secondary beneficiary (institutional/analytical) — benefits from the ability to regulate and maintain militias.
 *   - citizens_unwilling_to_serve: Primary victim (moderate/constrained) — bears extraction by having their right conditioned on an unwanted service.
 *   - federal_government: Secondary victim (institutional/analytical) — constrained in its ability to impose broad federal gun control measures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.45).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.3).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment Scope: Civic Right Reading (Militia-Conditioned Individual Right)").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '37c6f97c-bf9a-49ba-ac70-8d9a8dc90744').
narrative_ontology:cs_kernel_codification('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744', fixed_text).
narrative_ontology:cs_authority_grounding('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744', lineage).
narrative_ontology:cs_interpretation_layer_present('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744').
narrative_ontology:cs_reading_relation('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744', foundational, individual_right_conditioned_by_civic_duty).
narrative_ontology:cs_axiom_status(individual_right_conditioned_by_civic_duty, holdable).
narrative_ontology:cs_axiom_grounding('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744', individual_right_conditioned_by_civic_duty, deontological).
narrative_ontology:cs_axiom('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744', foundational, well_regulated_militia_essential_for_free_state).
narrative_ontology:cs_axiom_status(well_regulated_militia_essential_for_free_state, holdable).
narrative_ontology:cs_axiom_grounding('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744', well_regulated_militia_essential_for_free_state, conventional).
narrative_ontology:cs_reference_frame('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744', founding_era_civic_republicanism).
narrative_ontology:cs_drift_state('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('37c6f97c-bf9a-49ba-ac70-8d9a8dc90744', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, citizens_unwilling_to_serve).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, federal_government).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republicanism).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, states_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who are eligible for militia service and wish to exercise their right to bear arms under the condition of civic participation. They benefit from the right but are subject to state regulation for militia purposes.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    moderate, biographical, constrained, national).

% Entities that retain the authority to organize and regulate militias, and thus to condition the individual right to bear arms on militia service. They benefit from this regulatory power for public safety and defense.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_governments, beneficiary,
    institutional, generational, analytical, national).

% Individuals who desire to exercise their right to bear arms but are unwilling or unable to participate in militia service, finding the condition extractive of their autonomy. Their options are to comply, forgo the right, or challenge the condition.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, citizens_unwilling_to_serve, payer,
    moderate, biographical, constrained, national).

% The national authority whose power to enact broad federal gun control legislation is constrained by the individual right, even if conditioned on militia service. It bears the cost of limited legislative scope.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_government, payer,
    institutional, generational, analytical, national).

% Organizations and individuals who advocate for expansive interpretations of the Second Amendment, often challenging the 'civic right' reading in favor of a purely individual right. They observe and influence the legal and political discourse.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, gun_rights_advocates, observer,
    organized, generational, mobile, national).

% Organizations and individuals who advocate for stricter gun control measures, often challenging the individual right aspect of this reading in favor of a more collective or state-centric interpretation. They observe and influence the legal and political discourse.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, gun_control_advocates, observer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__civic_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_scope__civic_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the individual right to bear arms with the collective security needs of the state by linking arms ownership to a regulated militia, ensuring a trained and disciplined body for defense.
% TRANSFER_FUNCTION: Transfers the unencumbered right to bear arms from individuals to the state's regulatory authority, in exchange for the collective benefit of a 'well regulated Militia' for security.
% ABSENT_VOICES: Those who believe in an absolute, unconditioned individual right to bear arms are often marginalized in discussions that emphasize the militia clause's conditioning effect. Similarly, those who believe only states should have arms are excluded from the individual right aspect.
% DISAPPEARANCE_RATIONALE: If this reading of the Second Amendment vanished, the legal landscape for gun ownership would drastically shift. Either a purely individual right or a purely collective right would likely dominate, leading to significant changes in gun laws, state powers, and individual liberties, reorganizing the entire framework of arms regulation.
% FOUNDING_PROBLEM: The need to balance individual liberty to own arms with the collective security of the newly formed states, particularly against potential federal overreach or foreign invasion, by ensuring a citizen militia.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars, independent of gun rights or gun control advocacy groups, corroborate that the founding problem involved both individual rights and collective security concerns related to militias. The debate over the balance remains live, as attested by ongoing legal challenges and legislative efforts from various non-beneficiary academic and policy institutions.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).
:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while individuals have a right, it comes with a civic obligation that some may not desire, effectively extracting their autonomy. Suppression (0.3) is low to moderate, reflecting the state's ability to regulate arms for militia purposes, but not to outright prohibit them. Theater ratio (0.1) is low as the militia concept, while debated, is still a live legal and theoretical construct in this reading. Accessibility collapse (0.4) is moderate, as alternatives to conditioned ownership exist (e.g., not owning arms, or owning them under stricter conditions), but the core right is still present. Resistance (0.5) is moderate, reflecting ongoing legal and political debates over the interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'militia_eligible_citizens', the constraint is a conditional benefit, offering a right with a duty. For 'citizens_unwilling_to_serve', it's an extractive condition on a fundamental right. 'State_governments' see it as a legitimate regulatory power, while the 'federal_government' may view it as a limitation on its legislative authority. The engine will compute these divergences based on the declared roles and attributes.
 *
 * DIRECTIONALITY LOGIC:
 *   'Militia_eligible_citizens' are beneficiaries (d=0.2) as they gain the right to bear arms, albeit with a condition. 'State_governments' are also beneficiaries (d=0.1) as they retain regulatory authority over militias. 'Citizens_unwilling_to_serve' are victims (d=0.8) as the condition extracts their unencumbered right. The 'federal_government' is a victim (d=0.7) as its power to regulate arms is constrained by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging both the individual right and the civic condition. If the militia condition were to become purely theatrical (high theater_ratio) while still extracting from 'citizens_unwilling_to_serve', it would signal mandatrophy, shifting towards a Piton or Snare. The current moderate extractiveness and low theater ratio suggest the mandate is still considered functional within this interpretive framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_vs_pure_individual_right,
    'Is the individual right to bear arms truly conditioned on militia participation, or is the militia clause merely prefatory language to an unconditioned individual right?',
    'Further Supreme Court rulings clarifying the ''well regulated Militia'' clause''s operative effect on individual ownership, or a constitutional amendment explicitly defining the scope.',
    'If the right is unconditioned (individual_right_reading), the constraint''s extractiveness from ''citizens_unwilling_to_serve'' would decrease, and the ''state_governments'' beneficiary role would diminish, shifting the classification towards a Rope or even a Mountain for the individual right.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civic_vs_pure_individual_right, conceptual, 'Ambiguity between a civic-conditioned individual right and a pure individual right.').

omega_variable(
    civic_vs_collective_right,
    'Does the Second Amendment protect an individual''s right to arms for militia service, or only the state''s right to maintain a militia (collective_right_reading)?',
    'Supreme Court clarification on whether the ''right of the people'' refers to individuals or the collective body of the state.',
    'If interpreted as a purely collective right, the ''militia_eligible_citizens'' would cease to be beneficiaries, and the constraint would shift towards a Mountain for state authority, with negligible individual extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civic_vs_collective_right, conceptual, 'Ambiguity between a civic-conditioned individual right and a collective state right.').

omega_variable(
    militia_relevance_drift,
    'Given the modern military and law enforcement structures, is the concept of a ''well regulated Militia'' still functionally relevant to the security of a free state, or has its original purpose atrophied?',
    'Empirical analysis of the role of unorganized militias in contemporary state security, or a societal consensus shift on the necessity of such a body.',
    'If the militia''s relevance has atrophied, the ''civic_right_reading'' would lose its primary justification, potentially leading to a re-evaluation of the constraint''s purpose and a shift towards a Piton or a Snare if the conditioning remains but the service is meaningless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_relevance_drift, empirical, 'Functional relevance of the militia clause in modern context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__civic_right_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__civic_right_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__civic_right_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__civic_right_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__civic_right_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__civic_right_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_scope' kernel, each with different structural properties and classifications. This 'civic_right_reading' emphasizes the individual right conditioned on militia participation, distinct from a purely individual or purely collective right.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
