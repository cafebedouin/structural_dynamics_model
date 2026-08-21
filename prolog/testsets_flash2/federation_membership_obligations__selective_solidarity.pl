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
 *   access are tiered based on an individual's economic contribution and
 *   activity status. It bifurcates mobile workers into those with full rights
 *   (economically active) and those with restricted rights (economically
 *   inactive), distributing cost-bearing by contribution rather than
 *   universal citizenship. This reading is a response to perceived fiscal
 *   pressures on national welfare states within a free movement area.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.68).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.75).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Federation Membership Obligations: Selective Solidarity Reading").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, '2fbde4a1-cd01-405a-9f13-1a5ed4ebf506').
narrative_ontology:cs_kernel_codification('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506', formalized).
narrative_ontology:cs_authority_grounding('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506', lineage).
narrative_ontology:cs_interpretation_layer_present('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506').
narrative_ontology:cs_reading_relation('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506', foundational, welfare_access_contingent_on_contribution).
narrative_ontology:cs_axiom_status(welfare_access_contingent_on_contribution, holdable).
narrative_ontology:cs_axiom_grounding('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506', welfare_access_contingent_on_contribution, conventional).
narrative_ontology:cs_axiom('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506', foundational, fiscal_sustainability_trumps_universal_access).
narrative_ontology:cs_axiom_status(fiscal_sustainability_trumps_universal_access, holdable).
narrative_ontology:cs_axiom_grounding('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506', fiscal_sustainability_trumps_universal_access, instrumental).
narrative_ontology:cs_reference_frame('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506', contributory_welfare_federalism).
narrative_ontology:cs_drift_state('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2fbde4a1-cd01-405a-9f13-1a5ed4ebf506', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, net_contributor_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, economically_active_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, high_welfare_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and enforce policies that link free movement rights to economic contribution, reducing their welfare burden from economically inactive migrants. They benefit from a mobile workforce without bearing the full social costs of universal access.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, net_contributor_member_states, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from free movement and access to labor markets, often with full welfare rights due to their contributory status. Their mobility is facilitated, but their rights are contingent on maintaining economic activity.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_active_migrants, beneficiary,
    moderate, biographical, mobile, regional).

% Face restricted access to welfare benefits and social services, often experiencing precarity and social exclusion. Their rights are curtailed based on their lack of economic contribution, despite their citizenship status.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_migrants, payer,
    powerless, immediate, trapped, local).

% Bear the administrative and political costs of implementing tiered welfare access, often facing legal challenges and social pressure. They are forced to adapt their universal welfare systems to a contributory principle for mobile citizens.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, high_welfare_member_states, payer,
    institutional, generational, constrained, national).

% Adjudicate disputes over the interpretation of free movement and welfare access, often balancing national sovereignty with federal principles. Their rulings shape the practical application of selective solidarity.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, supranational_courts, observer,
    institutional, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the fiscal sustainability of national welfare states within a free movement area by linking welfare access to economic contribution, ensuring that mobile citizens contribute to the systems they access.
% TRANSFER_FUNCTION: Transfers the burden of welfare provision for economically inactive mobile citizens from host member states to either their home states or to the individuals themselves, based on contribution history.
% ABSENT_VOICES: Advocacy groups for universal social rights and non-contributory welfare access are often marginalized in policy debates, arguing for a citizenship-based principle over a contributory one. Economically inactive migrants themselves have limited voice in policy formulation.
% DISAPPEARANCE_RATIONALE: If selective solidarity vanished, member states would either revert to full national welfare closure (restricting free movement) or be forced to adopt a more universal, citizenship-based welfare access for all mobile citizens, leading to significant fiscal and political reorganization.
% FOUNDING_PROBLEM: The tension between national welfare state sustainability and the principle of free movement for all citizens within a federal structure, particularly concerning economically inactive individuals who might become a fiscal burden.
% FOUNDING_PROBLEM_CORROBORATION: Member states, particularly those with high welfare provisions, consistently attest to the live nature of this problem, citing ongoing fiscal pressures and public opinion. Supranational institutions acknowledge the challenge of balancing free movement with national social security systems.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the fiscal sustainability of welfare states (beneficiaries: net contributor member states) while simultaneously extracting from and suppressing economically inactive migrants (victims). Extractiveness is high (0.68) due to the denial of full welfare access to a class of citizens. Suppression is also high (0.75) as it requires active enforcement through legal frameworks and administrative checks to differentiate and restrict access. The theater ratio is low (0.20) because the enforcement is largely functional, directly achieving its goal of limiting welfare access for specific groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of net contributor member states, this is a necessary coordination mechanism for fiscal responsibility. From the perspective of economically inactive migrants, it is a discriminatory extraction that undermines the principle of equal citizenship. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Net contributor member states and economically active migrants are beneficiaries, as the constraint reduces their fiscal burden and facilitates their mobility, respectively. Economically inactive migrants and high welfare member states are payers, bearing the costs of restricted access and administrative overhead, respectively. Supranational courts act as observers, mediating the interpretation and application of these rules.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_sustainability_necessity,
    'Is the selective solidarity principle genuinely necessary for the fiscal sustainability of member state welfare systems, or are there alternative, less extractive coordination mechanisms?',
    'Comparative analysis of federal systems with universal welfare access and free movement, or economic modeling of alternative funding mechanisms for welfare provision for mobile citizens.',
    'If not strictly necessary, the constraint''s extractiveness would be re-evaluated as higher, and its coordination function as weaker, potentially reclassifying it towards a Snare. If proven necessary, its coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_necessity, empirical, 'Assesses the empirical necessity of linking welfare access to economic contribution for fiscal stability.').

omega_variable(
    citizenship_vs_contribution_principle,
    'Which normative principle should govern welfare access in a federal system with free movement: universal citizenship or economic contribution?',
    'This is a preference-based question, resolvable through political deliberation, legislative reform, or constitutional amendment, reflecting societal values.',
    'A shift towards a universal citizenship principle would fundamentally alter the constraint, likely reducing its extractiveness and suppression for economically inactive migrants, potentially transforming it into a Rope or even dissolving it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(citizenship_vs_contribution_principle, preference, 'Examines the underlying normative conflict between citizenship-based and contribution-based welfare rights.').

omega_variable(
    reading_structural_divergence,
    'How do the structural properties (extractiveness, suppression, beneficiary/victim sets) of the ''selective_solidarity'' reading compare to the ''integration_primary'' and ''member_sovereignty_primary'' readings of federation_membership_obligations?',
    'Comparative analysis of constraint stories for each reading, focusing on their distinct metric profiles and stakeholder impacts.',
    'Significant divergence would confirm that these are structurally distinct constraints, not merely different opinions on the same one. Convergence would suggest a need to refine the kernel decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_structural_divergence, conceptual, 'Documents the structural differences between this reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__selective_solidarity, theater_ratio, 5, 0.23).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__selective_solidarity, theater_ratio, 10, 0.21).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__selective_solidarity, theater_ratio, 15, 0.2).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__selective_solidarity, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__selective_solidarity, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__selective_solidarity, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__selective_solidarity, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__selective_solidarity, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__selective_solidarity, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__selective_solidarity, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__selective_solidarity, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__selective_solidarity, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'federation_membership_obligations' kernel, each with different structural properties and stakeholder impacts. This 'selective_solidarity' reading focuses on contribution-based rights, influencing and coexisting with the 'integration_primary' and 'member_sovereignty_primary' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
