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
 *   human_readable: Federation Selective Solidarity: Tiered Free Movement and Welfare Access
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint, 'Federation Selective Solidarity,' is a specific reading
 *   of the 'federation_membership_obligations' kernel. It instantiates a
 *   system where free movement rights and welfare access are tiered based on
 *   an individual's contribution history and economic activity status. This
 *   reading emphasizes fiscal sustainability and national welfare protection,
 *   leading to a bifurcation of mobile workers into those with full rights
 *   (economically active) and those with restricted rights (economically
 *   inactive). The constraint operates as a Tangled Rope, coordinating free
 *   movement with welfare access while simultaneously extracting from and
 *   suppressing the rights of certain mobile populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.7).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.75).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.7).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Federation Selective Solidarity: Tiered Free Movement and Welfare Access").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, 'e406445e-7de1-4438-9842-9081d247486d').
narrative_ontology:cs_kernel_codification('e406445e-7de1-4438-9842-9081d247486d', formalized).
narrative_ontology:cs_authority_grounding('e406445e-7de1-4438-9842-9081d247486d', extraction).
narrative_ontology:cs_interpretation_layer_present('e406445e-7de1-4438-9842-9081d247486d').
narrative_ontology:cs_reading_relation('e406445e-7de1-4438-9842-9081d247486d', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('e406445e-7de1-4438-9842-9081d247486d', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('e406445e-7de1-4438-9842-9081d247486d', foundational, contributory_principle_supremacy).
narrative_ontology:cs_axiom_status(contributory_principle_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('e406445e-7de1-4438-9842-9081d247486d', contributory_principle_supremacy, conventional).
narrative_ontology:cs_axiom('e406445e-7de1-4438-9842-9081d247486d', foundational, fiscal_sustainability_as_primary_goal).
narrative_ontology:cs_axiom_status(fiscal_sustainability_as_primary_goal, holdable).
narrative_ontology:cs_axiom_grounding('e406445e-7de1-4438-9842-9081d247486d', fiscal_sustainability_as_primary_goal, instrumental).
narrative_ontology:cs_reference_frame('e406445e-7de1-4438-9842-9081d247486d', contributory_welfare_state_model).
narrative_ontology:cs_drift_state('e406445e-7de1-4438-9842-9081d247486d', contemporary_migration_debates, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e406445e-7de1-4438-9842-9081d247486d', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, net_contributing_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, economically_active_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, net_receiving_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and interprets the rules governing free movement and welfare coordination within the federation, actively enforcing the contributory principle to manage fiscal sustainability and social cohesion. They frame this as a necessary balance.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, federation_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from reduced welfare burdens and a mobile workforce that is primarily economically active and contributing. They advocate for policies that reinforce the contributory principle to protect national welfare systems, though they are constrained by overall federation law.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, net_contributing_member_states, beneficiary,
    institutional, generational, constrained, national).

% Bear administrative costs and potential social integration challenges for economically inactive mobile workers, even with restricted welfare access. They may also face pressure to contribute to common funds or manage internal social tensions arising from tiered rights.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, net_receiving_member_states, payer,
    institutional, generational, constrained, national).

% Enjoy full free movement and welfare rights due to their contribution history and economic activity. They benefit from the single market and social security portability, aligning with the selective solidarity principle.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_active_mobile_workers, beneficiary,
    moderate, biographical, mobile, regional).

% Face significantly restricted access to welfare benefits, potential deportation, and social exclusion due to lack of contribution history or economic activity, despite being within the free movement area. Their mobility is effectively constrained by economic status.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_workers, payer,
    powerless, immediate, trapped, regional).

% Argue for universal rights based on citizenship, opposing the tiered system as discriminatory and undermining the foundational principles of solidarity and free movement. They are often excluded from the direct policy-making process that shapes this constraint.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, integration_advocates, excluded,
    organized, generational, analytical, global).

% Argue for stronger national control over welfare and migration, seeing even selective solidarity as an infringement on national autonomy. They are also often excluded from the specific policy compromises that define this constraint.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, member_sovereignty_advocates, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To manage the fiscal sustainability of national welfare systems and maintain social cohesion within a free movement area by linking welfare access to economic contribution and activity status.
% TRANSFER_FUNCTION: Transfers welfare costs and social security burdens based on contribution history and economic activity, effectively shifting responsibility from net-contributing states to individual mobile workers or their states of origin, and from the federation to national systems.
% ABSENT_VOICES: Advocates for universal citizenship rights and unconditional solidarity are largely absent from the policy-making that entrenches this tiered system; they would argue for a more inclusive approach to welfare access for all residents.
% DISAPPEARANCE_RATIONALE: If the tiered system vanished overnight, there would be immediate and significant pressure on national welfare systems, potential for increased social friction, and a need to renegotiate the fundamental basis of solidarity and citizenship within the federation. The mobile labor market would also experience significant shifts.
% FOUNDING_PROBLEM: The perceived fiscal strain on national welfare systems due to free movement, concerns about 'welfare tourism,' and anxieties regarding social dumping and the sustainability of social security models in an integrated market.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments, particularly those facing fiscal pressures or high immigration, attest to the problem's live status. Independent economic analyses and some public opinion polls also corroborate concerns about welfare system sustainability, though the proposed solutions are highly contested.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.7) is high because it systematically denies full welfare access to a segment of mobile workers, shifting costs and responsibilities. Suppression (0.75) is also high, reflecting the active enforcement mechanisms (legal, administrative) required to maintain these tiers and restrict access. The theater ratio (0.2) is relatively low, as the system is functionally designed to achieve its stated goal of selective solidarity, even if that goal is contested. The metrics show a gradual increase in both extractiveness and suppression over the interval, reflecting a hardening of this policy approach.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of net-contributing member states and economically active mobile workers, this constraint appears as a necessary and fair coordination mechanism that protects national resources. However, from the perspective of economically inactive mobile workers and integration advocates, it functions as a highly extractive and suppressive barrier, undermining fundamental rights and solidarity. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Federation institutions and net-contributing member states are beneficiaries, as they shape and benefit from the fiscal and social stability this tiered system aims to provide. Economically active mobile workers are also beneficiaries, enjoying full rights within the system. Economically inactive mobile workers are clear targets/victims, bearing the direct costs of restricted access and social exclusion. Net-receiving member states are payers, as they manage the administrative burden and social implications of these tiered rights, even if some fiscal burden is reduced.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring its coordination function in managing fiscal sustainability within a free movement area). The analysis highlights that while it addresses a genuine coordination problem (welfare state sustainability), it does so through asymmetric extraction and suppression, rather than purely cooperative means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine instantiation of ''federation_membership_obligations'' or a policy choice that deviates from its core principles?',
    'Legal and political analysis comparing this reading''s operational outcomes against the foundational treaties and stated goals of the federation.',
    'If it deviates, the constraint''s legitimacy is undermined, potentially reclassifying it as a Snare or a degraded Piton from an external observer''s perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''selective_solidarity'' reading of the ''federation_membership_obligations'' kernel.').

omega_variable(
    structural_delta_validation,
    'Does the observed policy and practice truly bifurcate mobile workers into employed (full rights) vs. economically inactive (restricted rights) and distribute cost-bearing by contribution status?',
    'Empirical studies on welfare access rates, legal challenges, and migration patterns of different worker categories within the federation.',
    'If the bifurcation is less pronounced or cost-bearing is not strictly by contribution, the extractiveness and suppression metrics might be lower, potentially shifting the classification towards a Rope or a less extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_validation, empirical, 'Verifies the expected structural delta of this reading regarding worker bifurcation and cost distribution.').

omega_variable(
    contest_with_integration_primary,
    'To what extent does this ''selective_solidarity'' reading genuinely coexist with or actively undermine the ''integration_primary'' reading''s emphasis on universal EU citizenship rights?',
    'Analysis of court rulings, legislative debates, and public discourse to determine if the universalist interpretation is being systematically marginalized or if both remain viable, albeit competing, policy frames.',
    'If ''selective_solidarity'' is actively foreclosing ''integration_primary'', it suggests a more coercive and less coordinative underlying structure, potentially increasing its effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contest_with_integration_primary, conceptual, 'Examines the dynamic tension between selective solidarity and universal integration principles.').

omega_variable(
    contest_with_member_sovereignty_primary,
    'How does this ''selective_solidarity'' reading balance federation-level coordination with national welfare state closure authority, compared to the ''member_sovereignty_primary'' reading?',
    'Comparative legal analysis of national welfare policies and federation-level directives, assessing the degree of national autonomy retained versus ceded under this reading.',
    'If ''selective_solidarity'' is perceived as too centralizing by member sovereignty advocates, it could increase resistance and political instability; if too permissive, it might fail to address fiscal concerns, leading to calls for stronger national closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contest_with_member_sovereignty_primary, conceptual, 'Analyzes the compromise between federation-level solidarity and national sovereignty.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, administrative hurdles) or internalized (fear of deportation, social stigma leading to self-restriction) for economically inactive mobile workers?',
    'Post-exit suppression trajectory: if suppression persists (e.g., self-exclusion from services) after legal barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for mobile workers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t4, federation_membership_obligations__selective_solidarity, theater_ratio, 4, 0.12).
narrative_ontology:measurement(fede_tr_t8, federation_membership_obligations__selective_solidarity, theater_ratio, 8, 0.15).
narrative_ontology:measurement(fede_tr_t12, federation_membership_obligations__selective_solidarity, theater_ratio, 12, 0.17).
narrative_ontology:measurement(fede_tr_t16, federation_membership_obligations__selective_solidarity, theater_ratio, 16, 0.19).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__selective_solidarity, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t4, federation_membership_obligations__selective_solidarity, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(fede_be_t8, federation_membership_obligations__selective_solidarity, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(fede_be_t12, federation_membership_obligations__selective_solidarity, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(fede_be_t16, federation_membership_obligations__selective_solidarity, base_extractiveness, 16, 0.69).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__selective_solidarity, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t4, federation_membership_obligations__selective_solidarity, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(fede_su_t8, federation_membership_obligations__selective_solidarity, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(fede_su_t12, federation_membership_obligations__selective_solidarity, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(fede_su_t16, federation_membership_obligations__selective_solidarity, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__selective_solidarity, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_labor_market_regulations).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_social_security_coordination).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_citizenship_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
