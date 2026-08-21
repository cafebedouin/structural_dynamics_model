% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment: Civic Republican Reading (Armed Citizenship for Self-Governance)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint story models the civic republican reading of the Second
 *   Amendment, which interprets the right to keep and bear arms as
 *   fundamentally tied to the duty of citizens to participate in a
 *   'well-regulated militia' for the purpose of republican self-governance.
 *   It is neither a purely individual liberty nor solely a state's right, but
 *   a civic right-duty. This reading acknowledges the necessity of regulation
 *   (moderate extractiveness on training/qualification) to ensure the
 *   'well-regulated' aspect, which is seen as essential for the coordination
 *   function of a free state.
 *
 * KEY AGENTS:
 *   - civic_militia_members: Dual beneficiary (right + duty) / Payer (training/qualification) — (organized/constrained)
 *   - state_legislatures_and_courts: Agenda setter (regulate) — (institutional/analytical)
 *   - general_citizenry: Beneficiary (self-governance) — (moderate/mobile)
 *   - unregulated_arms_advocates: Payer (bear cost of regulation) / Excluded (view not fully adopted) — (organized/constrained)
 *   - pacifist_citizens: Excluded (view not central) — (powerless/constrained)
 *   - constitutional_scholars: Analytical observer — (analytical/universal)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.35).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.2).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment: Civic Republican Reading (Armed Citizenship for Self-Governance)").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, 'c32a0a60-c12f-4c78-8b4b-4ba06df64379').
narrative_ontology:cs_kernel_codification('c32a0a60-c12f-4c78-8b4b-4ba06df64379', fixed_text).
narrative_ontology:cs_authority_grounding('c32a0a60-c12f-4c78-8b4b-4ba06df64379', lineage).
narrative_ontology:cs_interpretation_layer_present('c32a0a60-c12f-4c78-8b4b-4ba06df64379').
narrative_ontology:cs_reading_relation('c32a0a60-c12f-4c78-8b4b-4ba06df64379', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('c32a0a60-c12f-4c78-8b4b-4ba06df64379', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('c32a0a60-c12f-4c78-8b4b-4ba06df64379', foundational, armed_citizenry_for_self_governance).
narrative_ontology:cs_axiom_status(armed_citizenry_for_self_governance, holdable).
narrative_ontology:cs_axiom_grounding('c32a0a60-c12f-4c78-8b4b-4ba06df64379', armed_citizenry_for_self_governance, deontological).
narrative_ontology:cs_axiom('c32a0a60-c12f-4c78-8b4b-4ba06df64379', foundational, well_regulated_implies_civic_duty).
narrative_ontology:cs_axiom_status(well_regulated_implies_civic_duty, holdable).
narrative_ontology:cs_axiom_grounding('c32a0a60-c12f-4c78-8b4b-4ba06df64379', well_regulated_implies_civic_duty, conventional).
narrative_ontology:cs_reference_frame('c32a0a60-c12f-4c78-8b4b-4ba06df64379', founding_era_republican_ideal).
narrative_ontology:cs_drift_state('c32a0a60-c12f-4c78-8b4b-4ba06df64379', contemporary_polarized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c32a0a60-c12f-4c78-8b4b-4ba06df64379', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, civic_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_polity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, general_citizenry).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, civic_militia_members).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, unregulated_arms_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens who actively participate in organized, well-regulated militias, viewing it as both a right and a civic duty. They benefit from the right to bear arms for collective defense and self-governance, but bear the costs of training, qualification, and adherence to regulations.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, civic_militia_members, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, civic_militia_members, payer).

% Government bodies responsible for regulating the militia and arms-bearing, ensuring it is 'well-regulated' in a manner consistent with republican self-governance. They interpret and enforce the balance between individual right and collective duty.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, state_legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% The broader populace who benefit from the security and self-governance capacity of an armed citizenry, even if they do not actively participate in militias. They bear the diffuse societal costs and responsibilities associated with a widely armed populace.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, general_citizenry, beneficiary,
    moderate, biographical, mobile, national).

% Groups and individuals who advocate for an expansive, largely unregulated individual right to bear arms, often resisting the 'well-regulated' aspect as an infringement. They bear the 'cost' of regulations and restrictions that this reading upholds.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, unregulated_arms_advocates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, unregulated_arms_advocates, excluded).

% Citizens who, for moral or other reasons, oppose arms bearing. Their perspective is not central to this reading of the right, and they bear the societal costs of living in an armed republic without directly participating in its defense.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, pacifist_citizens, excluded,
    powerless, biographical, constrained, national).

% Academics and legal experts who analyze the historical context, philosophical underpinnings, and contemporary implications of the Second Amendment from a civic republican perspective.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective defense and self-governance of the republic by empowering citizens to form well-regulated militias, ensuring a balance between individual capacity and collective responsibility.
% TRANSFER_FUNCTION: Transfers the responsibility for collective security and the maintenance of a free state, in part, to an armed and civically engaged citizenry, requiring them to bear the duty of training and regulation.
% ABSENT_VOICES: Those advocating for a purely individualistic, unregulated right to bear arms (e.g., libertarian interpretations) or a purely state-controlled, collective right (e.g., some early 20th-century interpretations) are not fully represented, as this reading seeks a balance between these extremes.
% DISAPPEARANCE_RATIONALE: If the civic republican understanding of the Second Amendment vanished, it would fundamentally alter the relationship between citizens and the state regarding defense and self-governance, potentially leading to either an over-militarized individual populace or a disarmed citizenry dependent solely on state forces, both of which would reshape the republic.
% FOUNDING_PROBLEM: To ensure the security of a free state and the capacity for popular sovereignty against both internal tyranny and external threats, by empowering citizens to participate in collective defense through a well-regulated militia.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts from the founding era (e.g., Federalist Papers, Anti-Federalist writings), contemporary political philosophy, and legal scholarship from outside purely pro-gun or anti-gun advocacy groups corroborate the ongoing relevance of self-governance and citizen participation in security, even if the specific form of 'militia' is debated.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because the civic republican reading accepts and even requires certain regulations (e.g., training, qualification, militia organization) as part of the 'well-regulated' aspect, which imposes costs on citizens but is deemed necessary for the collective good. Suppression is low (0.20) as the right itself is protected, but not zero due to the enforcement of these regulations. Theater ratio is low (0.10) because the right is actively exercised and debated, with real consequences for both individual citizens and the state. Resistance is moderate (0.45) reflecting ongoing political and legal contestation over the scope of 'well-regulated' and the balance between individual and collective aspects.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civic militia members, the constraint is a beneficial coordination mechanism that enables self-governance, with acceptable costs. For unregulated arms advocates, it is seen as an extractive imposition of state control on a fundamental individual right. State legislatures and courts view it as a necessary framework for maintaining public order and security. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Civic militia members are beneficiaries due to the protection of their right to bear arms for civic purposes, but also payers due to the duties and regulations involved. The republican polity (represented by the general citizenry) is a beneficiary of the coordinated defense. State legislatures and courts act as agenda setters, defining and enforcing the 'well-regulated' aspect. Unregulated arms advocates are payers as they bear the costs of regulations they oppose, and are excluded in the sense that their purely individualistic interpretation is not fully adopted. Pacifist citizens are excluded as their views are not central to the right's interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by acknowledging the genuine coordination function (self-governance, collective defense) while also accounting for the moderate extraction imposed by 'well-regulated' requirements. It avoids treating the right as a pure individual liberty (which would be a Rope with lower extraction) or a pure state prerogative (which might be a Snare for individuals). The 'live' status of the founding problem, despite ongoing contestation, suggests the mandate is still relevant, though its interpretation is highly dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    well_regulated_interpretation_ambiguity,
    'What constitutes a ''well-regulated militia'' in contemporary society, and how should this inform modern arms regulation?',
    'Ongoing judicial interpretation, legislative action, and evolving societal consensus on the role of citizen militias and the nature of ''regulation''.',
    'A stricter interpretation of ''well-regulated'' would increase extractiveness on individual arms bearers (higher d for them), potentially shifting the classification towards a Tangled Rope for those seats. A looser interpretation would decrease extractiveness, moving it closer to a pure Rope or even a Mountain for individual rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(well_regulated_interpretation_ambiguity, conceptual, 'Ambiguity in the ''well-regulated militia'' clause and its implications for modern arms control.').

omega_variable(
    civic_duty_viability_in_modern_republic,
    'Is the civic republican ideal of an armed citizenry as a prerequisite for self-governance still a viable and effective mechanism for ensuring a free state in the context of modern military technology and political structures?',
    'Empirical studies on the effectiveness of civilian militias in modern defense, comparative political analysis of armed vs. unarmed republics, and ongoing public discourse on civic responsibility.',
    'If the ideal is deemed no longer viable, the coordination function of the constraint would atrophy, potentially increasing its theater ratio and shifting it towards a Piton or Snare if the remaining enforcement primarily serves extractive purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_duty_viability_in_modern_republic, empirical, 'The contemporary viability of the civic republican ideal of armed citizenship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1850, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(seco_tr_t1950, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1791, 0.25).
narrative_ontology:measurement(seco_be_t1850, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1850, 0.28).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(seco_be_t1950, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1950, 0.32).
narrative_ontology:measurement(seco_be_t2000, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1791, 0.15).
narrative_ontology:measurement(seco_su_t1850, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1850, 0.17).
narrative_ontology:measurement(seco_su_t1900, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement(seco_su_t1950, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1950, 0.19).
narrative_ontology:measurement(seco_su_t2000, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment kernel, each with different structural properties. This civic republican reading emphasizes the balance between individual right and collective duty for self-governance, distinct from purely individual or purely state-centered interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
