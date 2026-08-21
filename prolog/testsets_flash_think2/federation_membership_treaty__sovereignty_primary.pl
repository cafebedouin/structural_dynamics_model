% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Federation Membership Treaty: Sovereignty-Primary Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty_primary' reading of a
 *   federation's membership treaty, where free movement is explicitly
 *   conditional on member state consent. From this perspective, states retain
 *   significant authority to protect national labor markets and welfare
 *   systems, justifying restrictions on mobility. The constraint functions as
 *   a Tangled Rope, coordinating national interests while extracting costs
 *   from mobile workers and pro-integration advocates through active
 *   enforcement of border and labor market controls.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.65).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.75).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Federation Membership Treaty: Sovereignty-Primary Reading").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, 'f25306f6-862b-473a-b93a-68c8f3bd7872').
narrative_ontology:cs_kernel_codification('f25306f6-862b-473a-b93a-68c8f3bd7872', formalized).
narrative_ontology:cs_authority_grounding('f25306f6-862b-473a-b93a-68c8f3bd7872', lineage).
narrative_ontology:cs_interpretation_layer_present('f25306f6-862b-473a-b93a-68c8f3bd7872').
narrative_ontology:cs_reading_relation('f25306f6-862b-473a-b93a-68c8f3bd7872', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('f25306f6-862b-473a-b93a-68c8f3bd7872', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('f25306f6-862b-473a-b93a-68c8f3bd7872', foundational, national_sovereignty_is_primary).
narrative_ontology:cs_axiom_status(national_sovereignty_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('f25306f6-862b-473a-b93a-68c8f3bd7872', national_sovereignty_is_primary, conventional).
narrative_ontology:cs_axiom('f25306f6-862b-473a-b93a-68c8f3bd7872', foundational, welfare_state_integrity_requires_border_control).
narrative_ontology:cs_axiom_status(welfare_state_integrity_requires_border_control, holdable).
narrative_ontology:cs_axiom_grounding('f25306f6-862b-473a-b93a-68c8f3bd7872', welfare_state_integrity_requires_border_control, empirically_contingent).
narrative_ontology:cs_reference_frame('f25306f6-862b-473a-b93a-68c8f3bd7872', member_state_autonomy_framework).
narrative_ontology:cs_drift_state('f25306f6-862b-473a-b93a-68c8f3bd7872', contemporary_globalization_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f25306f6-862b-473a-b93a-68c8f3bd7872', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_states).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, domestic_welfare_recipients).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, pro_integration_advocates).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, national_sovereignty_principle).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, subsidiarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary authority over borders, immigration, and social policy. They use this constraint to protect national interests, manage labor markets, and ensure the sustainability of welfare systems for their citizens. They actively enforce restrictions on free movement.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_states, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from reduced competition from mobile workers, which is perceived to maintain wage stability and employment levels for domestic workers. This protection is a key justification for the constraint.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_labor_markets, beneficiary,
    organized, biographical, constrained, national).

% Benefit from the perceived protection of national welfare systems, ensuring that resources are primarily allocated to citizens and long-term residents, reducing strain on public services.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, domestic_welfare_recipients, beneficiary,
    moderate, biographical, constrained, national).

% Face significant administrative and legal barriers to entry, requiring visas, work permits, and often proving self-sufficiency. This limits their ability to seek employment and residence freely across the federation, incurring costs in time, money, and lost opportunities.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    powerless, immediate, constrained, regional).

% Advocate for fewer restrictions on free movement, viewing national controls as undermining the federation's core principles of integration and solidarity. They bear the political and social costs of a less integrated system and actively campaign against these restrictions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, pro_integration_advocates, payer,
    organized, generational, mobile, global).

% Oversee the implementation of treaties and mediate disputes between member states. From this reading's perspective, their power to override national sovereignty on migration and labor market policy is limited, acting more as facilitators than ultimate authorities.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federation_institutions, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the sovereign rights of member states to control their borders and manage their internal affairs with the limited principle of free movement within a federation, allowing states to protect national labor markets and welfare systems.
% TRANSFER_FUNCTION: Transfers control over labor market and welfare policy from a potential central federal authority (or a more integrated vision) back to individual member states. It also transfers the burden of restricted mobility onto mobile workers, who face increased costs and reduced opportunities.
% ABSENT_VOICES: Mobile workers who are denied entry or face significant hurdles, and businesses that seek a wider talent pool across the federation. These voices are largely excluded from the national decision-making processes that set these conditions, and their interests are often subordinated to national priorities.
% DISAPPEARANCE_RATIONALE: If member states lost the ability to control their borders and labor markets overnight, there would be immediate, large-scale shifts in population distribution, labor supply, and welfare system demands. This would fundamentally alter the political, economic, and social landscape of the federation, leading to significant instability and reorganization.
% FOUNDING_PROBLEM: To balance the desire for economic integration and cooperation within a federation with the fundamental sovereign right of member states to control their borders, protect their citizens' welfare, and manage their national economies.
% FOUNDING_PROBLEM_CORROBORATION: Member states consistently assert the ongoing need for national control over borders and welfare systems, citing public opinion, national security, and economic stability. This is corroborated by ongoing political debates, legislative actions within member states, and the outcomes of national elections, which frequently feature these issues.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.65) because mobile workers face significant barriers and costs, and the benefits of free movement are curtailed. Suppression is high (0.75) due to active enforcement by member states through border controls, visa requirements, and labor market regulations. The theater ratio is moderate (0.25) as the stated purpose of protecting national interests is genuinely held, though some enforcement may be performative or disproportionate. The metrics reflect a hardening of national control over the interval.
 *
 * PERSPECTIVAL GAP:
 *   This reading diverges significantly from 'integration_primary' (which views free movement as constitutive) and 'subsidiarity_balance' (which seeks proportionality). From the 'sovereignty_primary' seat, national control is a fundamental right and a necessary condition for federation stability, whereas other readings might view it as an impediment to deeper integration or an overreach of national power.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states, national labor markets, and domestic welfare recipients are beneficiaries, as the constraint protects their perceived interests and resources. Mobile workers are clear targets, bearing the direct costs of restricted movement. Pro-integration advocates also bear costs by seeing their vision of a more integrated federation undermined. Federation institutions act as observers, mediating within the bounds set by this sovereignty-focused interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_protectionism,
    'Is the assertion of member state consent a genuine expression of national sovereignty, or primarily a mechanism for nationalistic protectionism and rent-seeking within labor markets?',
    'Comparative economic analysis of labor market outcomes in states with varying degrees of mobility restrictions, alongside political science analysis of the motivations behind policy decisions.',
    'If primarily protectionist, the constraint''s effective extractiveness is higher than justified by coordination, and its classification shifts closer to a Snare. If genuinely sovereign, the coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_protectionism, conceptual, 'Ambiguity between genuine sovereignty and protectionist motives.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of free movement primarily structural (legal barriers, border controls) or internalized (fear of discrimination, administrative burden leading to self-restriction)?',
    'Post-exit suppression trajectory: if mobile workers continue to face significant barriers or self-restrict even after formal legal barriers are reduced, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as mobile workers carry the suppression with them after formal exit attempts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for mobile workers.').

omega_variable(
    economic_net_impact,
    'What is the net economic impact of restricted free movement on the federation as a whole, considering both national labor market protection and potential losses in productivity, innovation, and economic growth due to reduced mobility?',
    'Comprehensive econometric modeling and counterfactual analysis comparing economic performance under different mobility regimes across the federation.',
    'If the net economic impact is negative for the federation, the coordination story is weakened, and the constraint''s overall legitimacy and efficiency are called into question, potentially shifting its classification towards a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_net_impact, empirical, 'Overall economic impact of restricted free movement on the federation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fede_tr_t5, federation_membership_treaty__sovereignty_primary, theater_ratio, 5, 0.21).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__sovereignty_primary, theater_ratio, 10, 0.23).
narrative_ontology:measurement(fede_tr_t15, federation_membership_treaty__sovereignty_primary, theater_ratio, 15, 0.24).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t5, federation_membership_treaty__sovereignty_primary, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__sovereignty_primary, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(fede_be_t15, federation_membership_treaty__sovereignty_primary, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(fede_su_t5, federation_membership_treaty__sovereignty_primary, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__sovereignty_primary, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(fede_su_t15, federation_membership_treaty__sovereignty_primary, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'federation_membership_treaty' kernel, each with different structural properties and classifications. This 'sovereignty_primary' reading emphasizes national control, while 'integration_primary' prioritizes free movement and 'subsidiarity_balance' seeks a proportional middle ground.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
