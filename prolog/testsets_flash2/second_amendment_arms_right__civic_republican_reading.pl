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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Arms Right (Civic Republican Reading)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the civic republican reading of the Second
 *   Amendment, which interprets the right to bear arms as intrinsically
 *   linked to the duty of citizens to participate in a well-regulated militia
 *   for the purpose of republican self-governance. It is neither a purely
 *   individual right nor solely a state prerogative. This reading emphasizes
 *   a balance between individual capacity and collective responsibility, with
 *   moderate regulatory authority to ensure civic competence. This is one
 *   reading of the 'second_amendment_arms_right' kernel, alongside the
 *   'individual_right_reading' and 'collective_right_reading'.
 *
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
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Arms Right (Civic Republican Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, 'd5ebad6c-fd16-440e-bf94-9e764925c65c').
narrative_ontology:cs_kernel_codification('d5ebad6c-fd16-440e-bf94-9e764925c65c', fixed_text).
narrative_ontology:cs_authority_grounding('d5ebad6c-fd16-440e-bf94-9e764925c65c', lineage).
narrative_ontology:cs_interpretation_layer_present('d5ebad6c-fd16-440e-bf94-9e764925c65c').
narrative_ontology:cs_reading_relation('d5ebad6c-fd16-440e-bf94-9e764925c65c', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5ebad6c-fd16-440e-bf94-9e764925c65c', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('d5ebad6c-fd16-440e-bf94-9e764925c65c', foundational, armed_citizenry_for_republican_virtue).
narrative_ontology:cs_axiom_status(armed_citizenry_for_republican_virtue, holdable).
narrative_ontology:cs_axiom_grounding('d5ebad6c-fd16-440e-bf94-9e764925c65c', armed_citizenry_for_republican_virtue, deontological).
narrative_ontology:cs_axiom('d5ebad6c-fd16-440e-bf94-9e764925c65c', foundational, well_regulated_implies_civic_competence).
narrative_ontology:cs_axiom_status(well_regulated_implies_civic_competence, holdable).
narrative_ontology:cs_axiom_grounding('d5ebad6c-fd16-440e-bf94-9e764925c65c', well_regulated_implies_civic_competence, conventional).
narrative_ontology:cs_reference_frame('d5ebad6c-fd16-440e-bf94-9e764925c65c', founding_era_civic_republicanism).
narrative_ontology:cs_drift_state('d5ebad6c-fd16-440e-bf94-9e764925c65c', contemporary_legal_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d5ebad6c-fd16-440e-bf94-9e764925c65c', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, civic_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_self_governance_ideal).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, unqualified_arms_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, civic_militia_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These citizens benefit from the right to bear arms for civic duty, enabling participation in a well-regulated militia. They also bear the costs of training and qualification requirements, which are seen as necessary for responsible civic participation. Their identity is tied to this dual role of right-holder and duty-bearer.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, civic_militia_members, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, civic_militia_members, payer).

% These authorities are responsible for establishing and enforcing regulations on arms ownership, ensuring a 'well-regulated' militia. Their power is constrained by the civic participation norm, requiring regulations to facilitate, not impede, responsible armed citizenship. They face resistance from those who interpret the right more broadly.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Individuals who do not meet the civic republican standard for arms ownership (e.g., lack of training, criminal record) are denied the right or face restrictions. They bear the cost of these regulations, which are justified by the collective good of a responsible citizenry. Their options are to comply, seek qualification, or challenge the regulations.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, unqualified_arms_owners, payer,
    powerless, immediate, constrained, local).

% Advocates for an expansive individual right to bear arms, independent of militia service, find their arguments marginalized by this reading. They would object to any regulation tied to civic duty or collective good, viewing it as an infringement on personal liberty. They are excluded from the core framing of this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, individual_rights_advocates, excluded,
    organized, biographical, constrained, national).

% The abstract ideal of a self-governing republic, where citizens are capable of defending their liberty, is vindicated and strengthened by this interpretation. It benefits from the constraint's emphasis on civic virtue and collective responsibility.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, republican_self_governance_ideal, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(second_amendment_arms_right__civic_republican_reading, republican_self_governance_ideal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the individual right to bear arms with the collective responsibility of a well-regulated militia, ensuring a citizenry capable of self-defense and civic participation without devolving into unchecked individualism or state monopoly on force.
% TRANSFER_FUNCTION: Transfers the responsibility for public safety and civic defense to a trained citizenry, while transferring the burden of qualification and regulation compliance to individual arms owners. It also transfers legitimacy from purely individualistic claims to a civic-republican framework.
% ABSENT_VOICES: Advocates for an unrestricted individual right to bear arms, as well as those who believe only the state should possess significant armed force, are marginalized. They would argue for either absolute individual liberty or absolute state control, respectively, but are excluded from the civic-republican synthesis.
% DISAPPEARANCE_RATIONALE: If this civic-republican interpretation vanished, the legal and political landscape around the Second Amendment would immediately polarize further, likely defaulting to either an individualistic or state-centric reading. Regulatory efforts would lose their civic justification, and the concept of an armed citizenry as a check on tyranny would be fundamentally altered, leading to a significant rearrangement of constitutional understanding and public policy.
% FOUNDING_PROBLEM: The founding problem was how to secure a free state against both internal tyranny and external threats, without creating a standing army that could itself become tyrannical, by relying on a virtuous, armed citizenry.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era, political theorists, and some legal scholars outside of specific advocacy groups corroborate that the concept of a 'well-regulated militia' tied to civic duty was central to the original understanding, and that the tension between individual liberty and collective security remains a live problem in contemporary society.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness (0.35) is moderate, reflecting the costs of training and qualification requirements imposed on citizens, which are seen as necessary for the civic function of the right. Suppression (0.20) is low, as the reading aims to facilitate, not prohibit, responsible arms ownership, but does suppress unqualified ownership. Theater ratio (0.10) is low, as the civic function is genuinely pursued, not merely performed. Accessibility collapse is moderate (0.40) because while the right is broadly accessible, it is not unrestricted. Resistance is moderate (0.30) from those who advocate for more individualistic or state-centric interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civic militia members, the constraint is a beneficial coordination mechanism that enables their dual role as right-holders and duty-bearers. From the perspective of unqualified arms owners, it is an extractive and suppressive mechanism that denies them a right. Regulatory authorities see it as a necessary framework for public order. The engine will compute these divergent classifications based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Civic militia members are dual beneficiaries/payers: they benefit from the right and the ideal of self-governance, but pay through training and qualification. Regulatory authorities are agenda-setters, balancing civic participation with public safety. Unqualified arms owners are payers, bearing the costs of exclusion or restriction. Individual rights advocates are excluded, as their framing is outside this reading's core.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_duty_vs_individual_choice,
    'To what extent can civic duty be legitimately imposed as a condition for exercising a constitutional right, without undermining the ''right'' aspect?',
    'Legal scholarship and judicial precedent clarifying the balance between civic obligations and individual liberties in constitutional interpretation, particularly in cases involving ''positive'' rights or duties.',
    'If civic duty is found to be an illegitimate condition, the extractiveness and suppression for ''civic_militia_members'' would increase, potentially reclassifying this reading towards a Tangled Rope or Snare for those compelled to participate. If it''s affirmed, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_duty_vs_individual_choice, conceptual, 'Ambiguity in balancing civic duty and individual rights within constitutional frameworks.').

omega_variable(
    militia_relevance_in_modern_era,
    'Is the concept of a ''well-regulated militia'' as a prerequisite for republican self-governance still empirically relevant in the context of modern military and policing capabilities?',
    'Empirical studies on the effectiveness of civilian militias in contemporary defense and security contexts, and political science analysis of the role of armed citizenry in maintaining republican governance.',
    'If the militia concept is found to be empirically irrelevant, the ''republican_self_governance_ideal'' beneficiary would become theatrical, increasing the overall theater_ratio and potentially shifting the constraint towards a Piton, as its core justification would atrophy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militia_relevance_in_modern_era, empirical, 'Empirical relevance of the ''well-regulated militia'' concept in modern society.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__civic_republican_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__civic_republican_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(seco_tr_t30, second_amendment_arms_right__civic_republican_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__civic_republican_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(seco_tr_t50, second_amendment_arms_right__civic_republican_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(seco_be_t30, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(seco_be_t50, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(seco_su_t30, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 30, 0.19).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 40, 0.21).
narrative_ontology:measurement(seco_su_t50, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'second_amendment_arms_right' kernel. This 'civic_republican_reading' emphasizes the right as tied to civic duty for self-governance, distinct from purely individual or state-centric interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
