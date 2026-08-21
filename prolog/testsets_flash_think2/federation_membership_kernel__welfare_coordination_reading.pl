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
 *   human_readable: EU Free Movement via Coordinated National Welfare Systems
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the EU's approach to free movement, where it
 *   operates through the coordination of national welfare systems rather than
 *   supranational harmonization. The EU enforces anti-social-dumping rules to
 *   prevent exploitation while preserving member state welfare design
 *   autonomy. This reading acknowledges the coordination function but
 *   highlights the extractive elements, particularly for posted workers and
 *   receiving state labor markets, as well as the fiscal burden on sending
 *   states. It is one reading of the broader 'federation_membership_kernel'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.7).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.65).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Free Movement via Coordinated National Welfare Systems").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, 'd877be20-1bed-4104-b56c-3730318ebe88').
narrative_ontology:cs_kernel_codification('d877be20-1bed-4104-b56c-3730318ebe88', formalized).
narrative_ontology:cs_authority_grounding('d877be20-1bed-4104-b56c-3730318ebe88', lineage).
narrative_ontology:cs_interpretation_layer_present('d877be20-1bed-4104-b56c-3730318ebe88').
narrative_ontology:cs_reading_relation('d877be20-1bed-4104-b56c-3730318ebe88', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('d877be20-1bed-4104-b56c-3730318ebe88', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('d877be20-1bed-4104-b56c-3730318ebe88', foundational, national_welfare_autonomy_preserved).
narrative_ontology:cs_axiom_status(national_welfare_autonomy_preserved, holdable).
narrative_ontology:cs_axiom_grounding('d877be20-1bed-4104-b56c-3730318ebe88', national_welfare_autonomy_preserved, conventional).
narrative_ontology:cs_axiom('d877be20-1bed-4104-b56c-3730318ebe88', foundational, anti_social_dumping_legitimate).
narrative_ontology:cs_axiom_status(anti_social_dumping_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d877be20-1bed-4104-b56c-3730318ebe88', anti_social_dumping_legitimate, conventional).
narrative_ontology:cs_reference_frame('d877be20-1bed-4104-b56c-3730318ebe88', balanced_welfare_autonomy_and_free_movement).
narrative_ontology:cs_drift_state('d877be20-1bed-4104-b56c-3730318ebe88', contemporary_eu_policy_context, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d877be20-1bed-4104-b56c-3730318ebe88', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_citizens_general).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, receiving_member_states_autonomy).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, sending_member_states_mobility).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_labor_markets).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_member_states_fiscal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces EU free movement rules and anti-social-dumping directives, aiming to balance market integration with national welfare autonomy. Benefits from the stability and legitimacy of the coordinated system.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Retain significant autonomy over their national welfare system design, avoiding supranational harmonization. This autonomy is a core benefit of the coordination approach.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_member_states_autonomy, beneficiary,
    institutional, generational, constrained, national).

% Experience dual pressure from posted workers (wage undercutting due to social levy exemptions) and permanent migrants (displacement effects), bearing social costs without full fiscal compensation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_labor_markets, payer,
    organized, biographical, constrained, national).

% Benefit from their citizens' right to free movement, allowing for labor export and remittances, which can alleviate domestic unemployment and boost national income.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_member_states_mobility, beneficiary,
    institutional, generational, constrained, national).

% Lose skilled and unskilled workers without direct fiscal compensation for their education and social investment, potentially leading to brain drain and demographic challenges.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_member_states_fiscal, payer,
    institutional, generational, constrained, national).

% Are subject to cost-competition posting, often experiencing wage undercutting due to temporary social levy exemptions and cabotage rules, making them vulnerable to exploitation despite anti-social-dumping rules.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, immediate, constrained, regional).

% Benefit from the general principle of free movement, allowing them to live, work, and study across the EU, enhancing personal and professional opportunities.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_citizens_general, beneficiary,
    moderate, biographical, mobile, continental).

% Often advocate for stronger labor protections and higher wages, viewing the current system as contributing to wage depression and social dumping. Their proposals for stricter enforcement or harmonization are often resisted by the current coordination framework.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, national_labor_unions_receiving_states, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable free movement of persons within the EU while respecting the diversity and autonomy of national welfare systems, preventing 'social dumping' and ensuring fair competition among member states.
% TRANSFER_FUNCTION: Facilitates the transfer of labor and economic activity across borders, while also transferring social costs (e.g., welfare claims, labor market pressure) to receiving states and human capital loss to sending states, mediated by anti-social-dumping rules.
% ABSENT_VOICES: Advocates for full supranational welfare harmonization (who would argue for a more integrated system), and potentially more radical national protectionists (who would argue for stricter border controls). National labor unions in receiving states often feel their concerns about wage undercutting are not fully addressed.
% DISAPPEARANCE_RATIONALE: If this coordination framework vanished overnight, member states would likely re-impose significant border controls and welfare restrictions to protect national systems, leading to a collapse of free movement or an unmanaged race to the bottom in social standards, forcing a complete renegotiation of EU foundational principles.
% FOUNDING_PROBLEM: How to reconcile the principle of free movement of persons with the existence of diverse national welfare states and labor market regulations, preventing both exploitation and unsustainable burdens on national systems.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and most member states attest that balancing free movement with welfare autonomy remains a live and complex problem, as evidenced by ongoing legislative debates, ECJ rulings, and national political discourse. Labor unions and social policy experts also corroborate the ongoing tension.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates free movement and preserves national autonomy (beneficiaries), but simultaneously enables and enforces asymmetric extraction from posted workers and receiving/sending state labor markets (victims). Extractiveness is high (0.7) due to the documented wage undercutting and fiscal imbalances. Suppression (0.65) is necessary to maintain the anti-social-dumping rules and prevent member states from fully opting out or unilaterally imposing stricter controls. Theater ratio is moderate (0.25) as the anti-social-dumping rules have a real function, but their effectiveness in preventing all forms of exploitation is debated, with some enforcement being performative to maintain the 'fair competition' narrative.
 *
 * PERSPECTIVAL GAP:
 *   EU institutions and general EU citizens perceive this as a successful coordination mechanism balancing diverse interests. However, posted workers, national labor unions, and specific member state labor markets experience it as an extractive system that imposes significant costs and pressures, despite the stated goals of fair competition and autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions and general EU citizens are beneficiaries, experiencing low directionality. Receiving member states benefit from welfare autonomy but their labor markets bear costs, leading to a more symmetric or slightly targeted directionality. Sending member states benefit from citizen mobility but bear fiscal costs, also leading to a more symmetric or slightly targeted directionality. Posted workers are clear targets, experiencing high directionality due to wage undercutting and constrained exit options. National labor unions are excluded and bear costs, placing them firmly at the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the system as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination). The 'anti-social-dumping' rules, while intended to coordinate, also serve to manage the extractive dynamics inherent in a system that allows for significant labor cost differentials. The persistence of these extractive elements, despite the coordination narrative, indicates it is not a Piton, as identifiable parties (EU institutions, some member states) actively benefit from its operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_coordination_effectiveness,
    'Are the anti-social-dumping rules and coordination mechanisms genuinely effective in preventing exploitation and ensuring fair competition, or do they primarily serve to legitimize existing extractive practices?',
    'Empirical studies on wage convergence/divergence for posted workers, analysis of enforcement outcomes, and comparative studies with fully harmonized or fully sovereign systems.',
    'If largely ineffective, the extractiveness metric would be re-evaluated upward, and the coordination function''s authenticity would be questioned, pushing the classification closer to a Snare. If highly effective, extractiveness would be lower, reinforcing the Rope aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_coordination_effectiveness, empirical, 'Assesses the true impact of anti-social-dumping rules on exploitation.').

omega_variable(
    fiscal_compensation_feasibility,
    'Is the lack of fiscal compensation for sending states (for worker loss) a structural extraction inherent to this coordination model, or an unaddressed coordination problem that could be resolved with feasible policy mechanisms?',
    'Economic modeling of inter-state fiscal transfers for labor mobility, and political feasibility analysis of such mechanisms within the EU framework.',
    'If feasible and unaddressed, it highlights a deliberate choice to allow extraction. If structurally unfeasible, it points to an irreducible cost of this specific coordination model, potentially lowering the perceived extractiveness from this specific angle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_compensation_feasibility, conceptual, 'Examines whether fiscal imbalances are inherent or resolvable.').

omega_variable(
    kernel_reading_validity,
    'Is this ''welfare_coordination_reading'' a coherent and stable interpretation of the federation_membership_kernel, or is it an unstable compromise constantly pulled between the ''integration_reading'' and ''member_sovereignty_reading''?',
    'Longitudinal analysis of ECJ jurisprudence, legislative outcomes, and member state policy shifts over decades, assessing the stability of the ''balance'' it claims to strike.',
    'If unstable, it suggests the underlying kernel is more fundamentally contested than this reading implies, potentially leading to a re-evaluation of the ''claimed_type'' as a more volatile or transitional form (e.g., Scaffold or a highly contested Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Assesses the stability and coherence of this specific kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fede_tr_t6, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement(fede_tr_t12, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(fede_tr_t18, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement(fede_tr_t24, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(fede_tr_t30, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(fede_be_t6, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(fede_be_t12, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(fede_be_t18, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(fede_be_t24, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement(fede_be_t30, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t6, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(fede_su_t12, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 12, 0.61).
narrative_ontology:measurement(fede_su_t18, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 18, 0.63).
narrative_ontology:measurement(fede_su_t24, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(fede_su_t30, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_kernel', each representing a distinct structural interpretation of EU free movement. This 'welfare_coordination_reading' focuses on the balance between national welfare autonomy and anti-social-dumping rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
