% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Welfare Coordination for Free Movement
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint describes the EU's approach to free movement as a
 *   coordination mechanism for national welfare systems, rather than through
 *   supranational harmonization. It focuses on the enforcement of
 *   anti-social-dumping rules while preserving member state welfare design
 *   autonomy. The reading highlights the costs borne by posted workers and
 *   receiving state labor markets, which are often obscured by the official
 *   narrative of balanced coordination. The claimed type is 'tangled_rope'
 *   because it genuinely coordinates (free movement) but also extracts (from
 *   posted workers and receiving labor markets) through asymmetric rules.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.65).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.7).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Welfare Coordination for Free Movement").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, 'c19a026f-9b65-4095-a341-97542bb00a42').
narrative_ontology:cs_kernel_codification('c19a026f-9b65-4095-a341-97542bb00a42', formalized).
narrative_ontology:cs_authority_grounding('c19a026f-9b65-4095-a341-97542bb00a42', lineage).
narrative_ontology:cs_interpretation_layer_present('c19a026f-9b65-4095-a341-97542bb00a42').
narrative_ontology:cs_reading_relation('c19a026f-9b65-4095-a341-97542bb00a42', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('c19a026f-9b65-4095-a341-97542bb00a42', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('c19a026f-9b65-4095-a341-97542bb00a42', foundational, national_welfare_autonomy_is_paramount).
narrative_ontology:cs_axiom_status(national_welfare_autonomy_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('c19a026f-9b65-4095-a341-97542bb00a42', national_welfare_autonomy_is_paramount, conventional).
narrative_ontology:cs_axiom('c19a026f-9b65-4095-a341-97542bb00a42', foundational, anti_social_dumping_rules_are_necessary).
narrative_ontology:cs_axiom_status(anti_social_dumping_rules_are_necessary, holdable).
narrative_ontology:cs_axiom_grounding('c19a026f-9b65-4095-a341-97542bb00a42', anti_social_dumping_rules_are_necessary, instrumental).
narrative_ontology:cs_reference_frame('c19a026f-9b65-4095-a341-97542bb00a42', managed_integration_with_national_autonomy).
narrative_ontology:cs_drift_state('c19a026f-9b65-4095-a341-97542bb00a42', contemporary_enlargement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c19a026f-9b65-4095-a341-97542bb00a42', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, sending_member_states).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_member_state_labor_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The European Commission and Court of Justice of the European Union (ECJ) interpret and enforce rules balancing free movement with national welfare autonomy, particularly anti-social-dumping measures. They benefit from the stability of the single market and the perception of managed integration.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from the ability of their citizens to seek employment in other EU countries, alleviating domestic unemployment and generating remittances. However, they lose skilled workers without direct fiscal compensation for their education and social investment.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_member_states, beneficiary,
    organized, generational, constrained, national).

% Experience dual pressure from free movement: posted workers (often with 2-year social levy exemptions) can undercut local wages, and permanent migrants increase competition for jobs and social services. This creates downward pressure on wages and strains welfare systems, leading to social dumping concerns.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_member_state_labor_markets, payer,
    organized, biographical, constrained, national).

% Are often employed under conditions that, while legal under EU rules, lead to lower wages and social contributions than local workers, making them targets for cost-competition. They face precarious employment and limited access to full welfare benefits in the host state, despite contributing to its economy.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, immediate, constrained, regional).

% Are the underlying structures that bear the costs of free movement without direct representation in EU-level policy debates. Their design autonomy is preserved, but they are forced to adapt to the fiscal and social pressures of migration without supranational harmonization or compensatory mechanisms.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, national_welfare_systems, excluded,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(federation_membership_kernel__welfare_coordination_reading, national_welfare_systems).

% Observe and advocate for the protection of national labor standards and welfare provisions against social dumping. They represent the interests of local workers and often contest the current balance of free movement and national autonomy.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, trade_unions, observer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the operation of national welfare systems across EU member states to facilitate free movement of labor while preventing outright social dumping and preserving national welfare design autonomy.
% TRANSFER_FUNCTION: Transfers labor and economic activity from sending to receiving states, and implicitly transfers some social costs (e.g., welfare strain, wage depression) from mobile workers and employers to receiving state labor markets and welfare systems.
% ABSENT_VOICES: The direct fiscal and social costs borne by national welfare systems are not fully represented in the EU's policy-making, which prioritizes free movement and single market principles. Local communities and social service providers in receiving states often bear uncompensated burdens.
% DISAPPEARANCE_RATIONALE: If this coordination framework vanished, free movement would likely collapse into unmanaged competition or be severely restricted by national protectionist measures, leading to significant economic and social disruption across the EU. National welfare systems would face immediate, unmitigated pressures.
% FOUNDING_PROBLEM: The original problem was to enable economic integration and labor mobility across diverse national welfare systems without requiring full harmonization, which was politically unfeasible.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and member states generally agree that the problem of balancing free movement with national welfare autonomy remains live, though they dispute the effectiveness and fairness of the current coordination mechanisms. Academic studies and national policy debates corroborate the ongoing tension.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the system, while enabling free movement, creates conditions for social dumping and wage undercutting, transferring value from vulnerable workers and local labor markets. Suppression (0.70) is significant due to the legal and institutional barriers preventing alternative labor market arrangements or stronger national protections. Theater ratio (0.20) is moderate; while there's genuine coordination, the 'preservation of autonomy' narrative sometimes masks the actual pressures on national systems. The metrics reflect a system that has become more extractive and suppressive over time as free movement expanded without commensurate social harmonization.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU institutions, this is a complex but necessary coordination to maintain the single market. From the perspective of posted workers and receiving state labor markets, it's a system that facilitates exploitation and unfair competition. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a 'rope' or 'scaffold' and victims experiencing a 'snare' or 'tangled_rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions and sending member states are beneficiaries, as they achieve political and economic goals (single market, unemployment relief). Posted workers and receiving state labor markets are victims, bearing the direct costs of wage depression and welfare strain. Trade unions and national welfare systems act as observers or excluded parties, advocating for or absorbing the consequences without full agency in the EU-level design.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_dumping_quantification,
    'What is the precise economic and social cost of ''social dumping'' facilitated by the current free movement and welfare coordination rules?',
    'Comprehensive, independent econometric studies comparing wage and social contribution differentials for posted workers versus local equivalents, and their impact on national welfare budgets.',
    'If costs are demonstrably high, it would strengthen the ''snare'' classification for affected workers and receiving states, potentially leading to demands for stricter enforcement or harmonization. If low, it would support the ''rope'' framing of the coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_dumping_quantification, empirical, 'Quantifying the true costs of social dumping under current EU rules.').

omega_variable(
    welfare_design_autonomy_vs_pressure,
    'To what extent does the ''preservation of member state welfare design autonomy'' truly exist, given the pressures exerted by free movement and anti-social-dumping rules?',
    'Comparative policy analysis across member states, examining the actual policy space available for welfare reform and the degree to which national choices are constrained by EU law and market dynamics.',
    'If autonomy is largely illusory, it would shift the constraint closer to a ''snare'' for national welfare systems, as they are forced to adapt without genuine choice. If substantial, it would reinforce the ''tangled_rope'' aspect of managed coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_design_autonomy_vs_pressure, conceptual, 'Assessing the reality of national welfare design autonomy under EU free movement.').

omega_variable(
    coordination_vs_extraction_framing,
    'Is the primary function of the current framework genuine coordination of diverse welfare systems, or is it primarily an extractive mechanism that leverages free movement for economic advantage?',
    'Analysis of policy outcomes: if the benefits of free movement are broadly distributed and costs are mitigated, it''s coordination. If benefits concentrate and costs externalize, it''s extraction. This is the core contest between the ''welfare_coordination_reading'' and the ''integration_reading'' (which often downplays costs).',
    'A resolution towards extraction would reclassify the constraint as a ''snare'' for many seats, while a resolution towards coordination would support a ''rope'' or ''tangled_rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_framing, preference, 'The fundamental framing of the EU''s free movement and welfare interaction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1992, 0.45).
narrative_ontology:measurement(fede_be_t2000, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(fede_be_t2008, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2016, 0.63).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1992, 0.5).
narrative_ontology:measurement(fede_su_t2000, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(fede_su_t2008, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2008, 0.65).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_kernel', focusing on the coordination of national welfare systems. It is linked to the 'integration_reading' and 'member_sovereignty_reading' which offer alternative interpretations of free movement within the EU.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
