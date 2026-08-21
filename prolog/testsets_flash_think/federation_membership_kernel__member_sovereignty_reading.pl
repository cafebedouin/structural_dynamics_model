% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member State Sovereignty over Free Movement and Welfare
 *   domain: Political Economy / Federalism / Migration Policy / Welfare State Theory
 *
 * SUMMARY:
 *   This constraint represents the 'member sovereignty' reading of free
 *   movement within a federal or quasi-federal system (e.g., the EU). It
 *   asserts that national welfare state capacity and labor market protection
 *   are legitimate bounds on free movement rights, granting member states
 *   authority to exclude economically inactive migrants and protect social
 *   solidarity institutions. This reading is often advanced by national
 *   governments and conservative political parties, particularly in
 *   net-receiving states. The constraint is claimed as a 'tangled_rope'
 *   because it aims to coordinate the protection of national welfare systems
 *   (a genuine coordination function) but does so through mechanisms that
 *   extract from and suppress the mobility of certain migrant groups and
 *   sending states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.7).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.8).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member State Sovereignty over Free Movement and Welfare").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "Political Economy / Federalism / Migration Policy / Welfare State Theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, '02248117-1771-41ed-b192-fb3012873107').
narrative_ontology:cs_kernel_codification('02248117-1771-41ed-b192-fb3012873107', formalized).
narrative_ontology:cs_authority_grounding('02248117-1771-41ed-b192-fb3012873107', lineage).
narrative_ontology:cs_interpretation_layer_present('02248117-1771-41ed-b192-fb3012873107').
narrative_ontology:cs_reading_relation('02248117-1771-41ed-b192-fb3012873107', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('02248117-1771-41ed-b192-fb3012873107', federation_membership_kernel__welfare_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('02248117-1771-41ed-b192-fb3012873107', foundational, national_welfare_state_integrity_paramount).
narrative_ontology:cs_axiom_status(national_welfare_state_integrity_paramount, holdable).
narrative_ontology:cs_axiom_grounding('02248117-1771-41ed-b192-fb3012873107', national_welfare_state_integrity_paramount, conventional).
narrative_ontology:cs_axiom('02248117-1771-41ed-b192-fb3012873107', foundational, member_state_border_control_prerogative).
narrative_ontology:cs_axiom_status(member_state_border_control_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('02248117-1771-41ed-b192-fb3012873107', member_state_border_control_prerogative, conventional).
narrative_ontology:cs_reference_frame('02248117-1771-41ed-b192-fb3012873107', national_sovereignty_over_borders_and_welfare).
narrative_ontology:cs_drift_state('02248117-1771-41ed-b192-fb3012873107', post_enlargement_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('02248117-1771-41ed-b192-fb3012873107', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, national_welfare_recipients).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, national_labor_unions).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_member_states).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, mobile_workers_from_sending_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert their right to control access to their welfare systems and labor markets, viewing it as essential for national sovereignty and social cohesion. They actively legislate and enforce policies to exclude economically inactive migrants and protect domestic labor.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_member_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, receiving_member_states, beneficiary).

% Citizens who rely on national welfare provisions and perceive free movement as a potential strain on these systems. They benefit from policies that restrict access to welfare benefits for non-contributing migrants, ensuring the perceived sustainability of their social safety nets.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, national_welfare_recipients, beneficiary,
    organized, biographical, constrained, national).

% Advocate for policies that protect domestic labor markets from perceived downward pressure on wages and working conditions that might result from unrestricted labor mobility. They benefit from restrictions on migrant labor that prioritize national workers.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, national_labor_unions, beneficiary,
    organized, biographical, constrained, national).

% Individuals who move to a member state but are deemed economically inactive or unable to support themselves. They face exclusion from welfare benefits, deportation, or significant barriers to integration, bearing the direct costs of this constraint.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, regional).

% Workers from other member states who, while economically active, face increased scrutiny, administrative burdens, and potential discrimination due to the emphasis on national welfare protection. Their mobility is constrained, and their access to certain social rights may be delayed or denied.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, mobile_workers_from_sending_states, payer,
    moderate, biographical, constrained, regional).

% These states experience brain drain when their skilled workers leave, and face challenges in supporting their citizens who are excluded or repatriated from receiving states. They bear the cost of reduced remittances and potential social instability.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_member_states, payer,
    institutional, generational, constrained, regional).

% Bodies like the European Commission and the European Court of Justice, which typically advocate for expansive free movement rights, find their authority challenged by member states asserting national sovereignty. They observe and mediate, but their interpretations are often resisted.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, eu_institutions, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To protect the fiscal sustainability and social solidarity of national welfare states by allowing member states to manage the social and economic impact of free movement, particularly concerning economically inactive migrants.
% TRANSFER_FUNCTION: Transfers the perceived costs of social support and labor market competition from national taxpayers and welfare recipients in receiving states to economically inactive migrants, mobile workers, and their sending states.
% ABSENT_VOICES: Migrant advocacy groups, human rights organizations, and some supranational legal scholars would object, arguing for the primacy of individual free movement rights and non-discrimination, and that the economic benefits of migration outweigh perceived welfare costs.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, member states would lose a key mechanism for managing migration and welfare, leading to significant internal political and economic reorganization. This could include increased intra-EU migration, potential strain on national welfare systems, and a re-evaluation of national social contracts, or conversely, a more integrated and flexible European labor market.
% FOUNDING_PROBLEM: The perceived strain on national welfare states and labor markets in receiving member states, particularly following EU enlargements that expanded free movement to lower-income countries, leading to concerns about 'welfare tourism' and social dumping.
% FOUNDING_PROBLEM_CORROBORATION: National political discourse, public opinion polls in receiving states, and some academic analyses corroborate the perception of strain and the need for national control, even if the extent of actual 'welfare tourism' is debated by independent economic studies.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the policies enacted under this reading impose significant costs on migrants and sending states, often disproportionate to the actual fiscal strain. Suppression is also high (0.8) due to active enforcement through border controls, administrative hurdles, and legal challenges to migrant rights. The theater ratio is moderate (0.4) as there is a genuine concern for welfare state sustainability, but also a degree of political posturing and symbolic enforcement that exceeds purely functional needs. Accessibility collapse is high for economically inactive migrants, as their alternatives for support are severely limited. Resistance is moderate-high from those negatively affected and from supranational institutions advocating for broader rights.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of receiving member states, this constraint is a necessary 'rope' or 'scaffold' to manage the social and economic impacts of free movement. From the perspective of migrants and sending states, it operates as a 'snare' or 'tangled_rope' that restricts fundamental rights and imposes significant costs. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving member states, national welfare recipients, and national labor unions are beneficiaries, as they gain protection for their welfare systems and labor markets. Economically inactive migrants, mobile workers from sending states, and sending member states are victims, bearing the costs of exclusion, restricted access, and brain drain. EU institutions act as observers, often challenging this reading but ultimately constrained by member state sovereignty.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_strain_empirical_basis,
    'To what extent is the perceived strain on national welfare states from free movement empirically substantiated, versus being a politically amplified narrative?',
    'Comprehensive, independent longitudinal studies comparing fiscal impacts of migration under different policy regimes, disaggregated by migrant status and economic activity.',
    'If empirical strain is low, the justification for exclusion weakens, reclassifying the constraint closer to a pure snare. If strain is high, the coordination function is stronger, supporting a tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_strain_empirical_basis, empirical, 'Empirical basis for welfare state strain claims.').

omega_variable(
    reading_framing_ambiguity,
    'Is this constraint primarily a legitimate exercise of national sovereignty to protect social solidarity, or a re-framing of protectionism and xenophobia as welfare defense?',
    'Analysis of policy intent, legislative debates, and public discourse, alongside the actual economic and social outcomes for both migrants and domestic populations, compared to alternative policy approaches.',
    'If primarily protectionist, the ''coordination'' aspect of the tangled_rope diminishes, pushing it closer to a snare. If genuinely about social solidarity, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Conceptual framing of national welfare protection vs. protectionism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of economically inactive migrants structural (legal barriers, administrative hurdles) or internalized (fear of deportation, social stigma leading to self-exclusion)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., through continued social stigma or difficulty re-entering the labor market) after formal legal barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for migrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t2004, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2004, 0.25).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(fede_tr_t2012, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2012, 0.35).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2016, 0.38).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(fede_be_t2004, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(fede_be_t2008, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(fede_be_t2012, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2012, 0.65).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2016, 0.68).
narrative_ontology:measurement(fede_be_t2020, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2020, 0.69).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t2004, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2004, 0.65).
narrative_ontology:measurement(fede_su_t2008, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(fede_su_t2012, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2012, 0.75).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2016, 0.78).
narrative_ontology:measurement(fede_su_t2020, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2020, 0.79).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_kernel', focusing on member state sovereignty. Its structural properties and metrics differ significantly from the 'integration_reading' and 'welfare_coordination_reading', necessitating separate constraint stories linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
