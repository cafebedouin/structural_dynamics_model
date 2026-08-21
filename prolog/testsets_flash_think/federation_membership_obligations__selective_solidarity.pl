% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Federation Membership Obligations: Selective Solidarity Reading
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the 'selective solidarity' reading of
 *   federation membership obligations, where free movement rights and welfare
 *   access are tiered based on an individual's contribution history and
 *   economic activity status. It is a response to perceived fiscal pressures
 *   on national welfare states, aiming to balance free movement with national
 *   budgetary concerns. The constraint is claimed as a Tangled Rope,
 *   reflecting its dual function of coordinating labor mobility while
 *   extracting from economically inactive individuals through restricted
 *   welfare access.
 *
 * KEY AGENTS:
 *   - Member States: Primary agenda-setters and beneficiaries, enforcing tiered rights.
 *   - Economically Active Mobile Workers: Beneficiaries of free movement, largely unaffected by restrictions.
 *   - Economically Inactive Mobile Workers: Primary targets/payers, facing restricted welfare access.
 *   - National Welfare Agencies: Implementers of the tiered system.
 *   - Migrant Rights Advocates: Payers, bearing the costs of challenging the system.
 *   - European Court of Justice: Observer and occasional agenda-setter, interpreting the legal framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.68).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.75).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Federation Membership Obligations: Selective Solidarity Reading").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, '29b890b3-c369-4f5d-8bb2-8721c9c6bae0').
narrative_ontology:cs_kernel_codification('29b890b3-c369-4f5d-8bb2-8721c9c6bae0', formalized).
narrative_ontology:cs_authority_grounding('29b890b3-c369-4f5d-8bb2-8721c9c6bae0', lineage).
narrative_ontology:cs_interpretation_layer_present('29b890b3-c369-4f5d-8bb2-8721c9c6bae0').
narrative_ontology:cs_reading_relation('29b890b3-c369-4f5d-8bb2-8721c9c6bae0', federation_membership_obligations__integration_primary, forecloses).
narrative_ontology:cs_reading_relation('29b890b3-c369-4f5d-8bb2-8721c9c6bae0', federation_membership_obligations__member_sovereignty_primary, influences).
narrative_ontology:cs_axiom('29b890b3-c369-4f5d-8bb2-8721c9c6bae0', foundational, welfare_access_contingent_on_contribution).
narrative_ontology:cs_axiom_status(welfare_access_contingent_on_contribution, holdable).
narrative_ontology:cs_axiom_grounding('29b890b3-c369-4f5d-8bb2-8721c9c6bae0', welfare_access_contingent_on_contribution, empirically_contingent).
narrative_ontology:cs_axiom('29b890b3-c369-4f5d-8bb2-8721c9c6bae0', foundational, economic_activity_as_primary_federation_benefit).
narrative_ontology:cs_axiom_status(economic_activity_as_primary_federation_benefit, holdable).
narrative_ontology:cs_axiom_grounding('29b890b3-c369-4f5d-8bb2-8721c9c6bae0', economic_activity_as_primary_federation_benefit, instrumental).
narrative_ontology:cs_reference_frame('29b890b3-c369-4f5d-8bb2-8721c9c6bae0', contributory_welfare_state_within_federation).
narrative_ontology:cs_drift_state('29b890b3-c369-4f5d-8bb2-8721c9c6bae0', contemporary_political_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('29b890b3-c369-4f5d-8bb2-8721c9c6bae0', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, member_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, economically_active_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, migrant_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively implement and enforce policies that tier free movement rights and welfare access based on contribution history and economic activity, aiming to protect national welfare systems from perceived fiscal burdens. They benefit from reduced welfare expenditure for certain migrant groups.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, member_states, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from relatively unhindered free movement and access to welfare in host states, as their economic activity aligns with the contributory principle. They may perceive themselves as protected from 'welfare tourism' by these rules.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_active_mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Face significant restrictions on welfare access and sometimes on residency rights, even if they are citizens of the federation. They bear the costs of exclusion and administrative hurdles, often leading to precarity.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_workers, payer,
    powerless, immediate, constrained, continental).

% Are tasked with implementing the tiered welfare access rules, requiring them to assess economic activity and contribution history. They operate within the legal framework set by member states and the federation, balancing fiscal concerns with legal obligations.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, national_welfare_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Challenge the legality and morality of tiered rights, arguing for universal access based on citizenship or residency. They bear the costs of litigation, public campaigns, and supporting affected individuals, often facing political resistance.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, migrant_rights_advocates, payer,
    organized, generational, constrained, continental).

% Interprets federation law regarding free movement and welfare access, often navigating the tension between national sovereignty and federation principles. Its rulings can shape the practical application and legitimacy of selective solidarity.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, european_court_of_justice, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, european_court_of_justice, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the free movement of economically active labor within the federation while attempting to manage perceived fiscal burdens on national welfare systems by restricting access for economically inactive individuals.
% TRANSFER_FUNCTION: Transfers potential welfare costs away from member states (for economically inactive migrants) and transfers the burden of proof and justification for welfare access onto mobile workers based on their contribution status.
% ABSENT_VOICES: Future generations of mobile workers who might face increasingly restrictive conditions, and those advocating for a more expansive, universal understanding of federation citizenship rights that decouples welfare access from economic activity.
% DISAPPEARANCE_RATIONALE: If the tiered rights and contributory principle vanished overnight, all federation citizens would have equal welfare access regardless of economic activity, leading to significant shifts in national welfare budgets, migration patterns, and a fundamental re-evaluation of federation citizenship and solidarity principles.
% FOUNDING_PROBLEM: The perceived problem of 'welfare tourism' and unsustainable fiscal burdens on national welfare states arising from the free movement of economically inactive federation citizens.
% FOUNDING_PROBLEM_CORROBORATION: Member states and some political factions attest that the problem is live, citing national budget pressures and public sentiment. Migrant rights organizations and some economic analyses dispute the scale of 'welfare tourism,' arguing it is exaggerated or a pretext for discrimination; their counter-evidence comes from independent research and legal challenges.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is substantial, as it denies full welfare access to a segment of federation citizens, shifting costs away from member states. Suppression (0.75) is high due to legal and administrative barriers that actively prevent full access for economically inactive individuals. The theater ratio (0.40) reflects the tension between the rhetoric of 'free movement' and the practical reality of tiered rights, where some enforcement is genuinely about fiscal management, but a significant portion serves to maintain the extractive tiering. The slight dip in extractiveness and suppression towards the end of the interval might reflect ongoing legal challenges or political shifts, but the overall trend is one of sustained, high-level extraction and enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Member states and economically active mobile workers perceive this constraint as a necessary coordination mechanism for a sustainable federation, ensuring that free movement does not unduly burden national welfare systems. Economically inactive mobile workers and migrant rights advocates, however, experience it as a highly extractive and suppressive mechanism that undermines fundamental federation citizenship rights and creates a two-tier system.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states are clear beneficiaries (d=0.0-0.2) as they reduce welfare outlays. Economically active mobile workers also benefit (d=0.2-0.4) from a system that facilitates their mobility while managing perceived 'freeloaders.' Economically inactive mobile workers are clear targets (d=0.8-1.0), bearing the costs of restricted access. Migrant rights advocates are also targets (d=0.7-0.9) as they expend resources to challenge the system. National welfare agencies are agenda-setters with some payer aspects (d=0.4-0.6) as they administer the complex system. The ECJ is an analytical observer (d=0.5) but can shift towards agenda-setter depending on its rulings.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope by highlighting the asymmetric extraction from economically inactive mobile workers, despite the coordination function for active labor. It also avoids mislabeling it as a pure Snare by acknowledging the genuine (though contested) coordination problem it purports to solve for member states regarding welfare sustainability. The 'contested' status of the founding problem in the six questions further supports the Tangled Rope classification, indicating an ongoing dispute over whether the coordination function still justifies the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'This constraint is the ''selective_solidarity'' reading of the ''federation_membership_obligations'' kernel. How would the classification change under sibling readings?',
    'Analyze the ''integration_primary'' and ''member_sovereignty_primary'' readings as separate constraint stories, comparing their structural properties and classifications.',
    'The ''integration_primary'' reading would likely classify as a Rope or Scaffold, emphasizing universal rights and coordination, with lower extraction. The ''member_sovereignty_primary'' reading might classify as a Snare or Tangled Rope, with potentially higher extraction and suppression, emphasizing national closure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Contextualizes this constraint as one interpretation of a contested kernel.').

omega_variable(
    welfare_tourism_empirical_basis,
    'Is the ''welfare tourism'' problem, which grounds this reading, empirically significant enough to justify the tiered rights structure, or is it an exaggerated concern?',
    'Comprehensive, independent empirical studies on the fiscal impact of economically inactive mobile workers on national welfare systems, disaggregated by type of benefit and duration of stay.',
    'If the problem is empirically negligible, the justification for the tiered rights collapses, reclassifying the constraint closer to a Snare (pure extraction). If it''s significant, it strengthens the coordination aspect, potentially pushing it towards a more balanced Tangled Rope or even a Rope (if extraction is proportional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_tourism_empirical_basis, empirical, 'Assesses the empirical validity of the founding problem for selective solidarity.').

omega_variable(
    long_term_integration_impact,
    'Does the ''selective solidarity'' approach, by creating tiered rights, undermine the long-term social and political integration goals of the federation?',
    'Longitudinal sociological and political science studies tracking social cohesion, political participation, and identity formation among different groups of federation citizens under tiered rights regimes.',
    'If it significantly undermines integration, the ''coordination'' function claimed by this reading is revealed as self-defeating or counterproductive to broader federation goals, potentially reclassifying it as a Snare (if the negative externalities outweigh any coordination benefit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_integration_impact, empirical, 'Examines the broader, unintended consequences of tiered rights on federation cohesion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t2004, federation_membership_obligations__selective_solidarity, theater_ratio, 2004, 0.25).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_obligations__selective_solidarity, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(fede_tr_t2012, federation_membership_obligations__selective_solidarity, theater_ratio, 2012, 0.35).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_obligations__selective_solidarity, theater_ratio, 2016, 0.4).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_obligations__selective_solidarity, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_obligations__selective_solidarity, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(fede_be_t2004, federation_membership_obligations__selective_solidarity, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(fede_be_t2008, federation_membership_obligations__selective_solidarity, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(fede_be_t2012, federation_membership_obligations__selective_solidarity, base_extractiveness, 2012, 0.65).
narrative_ontology:measurement(fede_be_t2016, federation_membership_obligations__selective_solidarity, base_extractiveness, 2016, 0.68).
narrative_ontology:measurement(fede_be_t2020, federation_membership_obligations__selective_solidarity, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(fede_be_t2024, federation_membership_obligations__selective_solidarity, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t2004, federation_membership_obligations__selective_solidarity, suppression_requirement, 2004, 0.6).
narrative_ontology:measurement(fede_su_t2008, federation_membership_obligations__selective_solidarity, suppression_requirement, 2008, 0.65).
narrative_ontology:measurement(fede_su_t2012, federation_membership_obligations__selective_solidarity, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement(fede_su_t2016, federation_membership_obligations__selective_solidarity, suppression_requirement, 2016, 0.75).
narrative_ontology:measurement(fede_su_t2020, federation_membership_obligations__selective_solidarity, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(fede_su_t2024, federation_membership_obligations__selective_solidarity, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, eu_citizenship_rights).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, national_welfare_provision).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_obligations' kernel, focusing on tiered rights based on contribution. It is linked to 'integration_primary' and 'member_sovereignty_primary' as sibling interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
