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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Federation Membership Treaty (Sovereignty Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty primary' reading of a
 *   federation's membership treaty, where free movement is explicitly
 *   conditional on member state consent. This reading prioritizes national
 *   control over labor markets and welfare systems, viewing federal free
 *   movement as a privilege granted by states, not an inherent right. The
 *   structural delta for this reading is that local labor markets and
 *   national welfare systems are beneficiaries, while mobile workers are
 *   victims due to restricted access. National regulatory autonomy is highly
 *   preserved.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.65).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.7).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Federation Membership Treaty (Sovereignty Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, 'cddb6f3f-c7ee-45f5-8848-e86db4e01b7c').
narrative_ontology:cs_kernel_codification('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c', formalized).
narrative_ontology:cs_authority_grounding('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c', lineage).
narrative_ontology:cs_interpretation_layer_present('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c').
narrative_ontology:cs_reading_relation('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c', foundational, national_sovereignty_precedes_federal_integration).
narrative_ontology:cs_axiom_status(national_sovereignty_precedes_federal_integration, holdable).
narrative_ontology:cs_axiom_grounding('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c', national_sovereignty_precedes_federal_integration, conventional).
narrative_ontology:cs_axiom('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c', foundational, member_state_consent_is_foundational_for_mobility).
narrative_ontology:cs_axiom_status(member_state_consent_is_foundational_for_mobility, holdable).
narrative_ontology:cs_axiom_grounding('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c', member_state_consent_is_foundational_for_mobility, conventional).
narrative_ontology:cs_reference_frame('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c', westphalian_state_autonomy).
narrative_ontology:cs_drift_state('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c', contemporary_federal_court_rulings, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('cddb6f3f-c7ee-45f5-8848-e86db4e01b7c', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_states).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, federation_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary authority over national borders, labor market regulations, and welfare provisions. They consent to free movement but assert the right to impose conditions to protect national interests, often leading to complex administrative hurdles for mobile workers. They benefit from controlling access to their social and economic resources.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_states, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the ability of member states to regulate the influx of foreign labor, preventing perceived downward pressure on wages or employment for domestic workers. This is an abstract entity, not an active agent.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_labor_markets, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_non_agent(federation_membership_treaty__sovereignty_primary, national_labor_markets).

% Benefit from member states' ability to restrict access to social benefits for non-nationals, aiming to preserve the fiscal sustainability of public services. This is an abstract entity, not an active agent.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_welfare_systems, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_non_agent(federation_membership_treaty__sovereignty_primary, national_welfare_systems).

% Face significant administrative burdens, conditional access to social benefits, and potential discrimination in labor markets due to national protective measures. Their 'free movement' is heavily qualified by state consent and national regulations, making their mobility costly and uncertain.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    powerless, immediate, constrained, regional).

% Bear the cost of fragmented policy implementation and the erosion of the principle of free movement. They are tasked with upholding the treaty but are constrained by member states' assertion of sovereignty, leading to internal friction and reduced effectiveness of federal policies.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federation_institutions, payer,
    institutional, generational, constrained, continental).

% Monitor the impact of national restrictions on the rights of mobile workers, advocating for more inclusive and less conditional interpretations of free movement. They highlight the human cost of administrative barriers and discriminatory practices.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the terms under which member states allow free movement while retaining sovereign control over their internal affairs, balancing federal integration with national autonomy.
% TRANSFER_FUNCTION: Transfers regulatory authority and economic benefits to member states (by allowing them to protect national labor markets and welfare systems) at the cost of restricted mobility and increased administrative burden for mobile workers.
% ABSENT_VOICES: Advocates for a truly borderless federation, who would argue that national restrictions undermine the foundational principles of integration and create a two-tiered system of citizenship, are marginalized by the emphasis on national sovereignty.
% DISAPPEARANCE_RATIONALE: If this conditional free movement vanished, member states would either fully open their borders (leading to rapid demographic and economic shifts) or fully close them (fragmenting the federation). The current balance, however imperfect, structures significant flows and policies.
% FOUNDING_PROBLEM: The original problem was how to achieve economic integration and free movement across sovereign states without dissolving national identities or overwhelming national social systems.
% FOUNDING_PROBLEM_CORROBORATION: Member states consistently attest that the problem of balancing national sovereignty with federal integration remains live, citing ongoing debates about migration, labor market protection, and welfare system sustainability. Federation institutions acknowledge the tension, though they advocate for different solutions.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.65) is substantial because mobile workers face significant barriers and costs, while member states retain considerable power to protect their interests. Suppression (0.70) is high due to the active enforcement of national regulations and administrative hurdles. Theater ratio (0.20) is moderate; while there's genuine coordination in managing federal-national relations, a portion of the 'protection' rhetoric serves to justify restrictions that benefit national interests. The metrics reflect the costs borne by mobile workers and the federal institutions in this reading.
 *
 * PERSPECTIVAL GAP:
 *   Member states perceive this as a necessary balance to preserve national integrity, while mobile workers experience it as a highly extractive and suppressive regime that undermines the promise of free movement. Federation institutions are caught between these two poles, attempting to reconcile conflicting mandates.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states, national labor markets, and national welfare systems are beneficiaries, as they gain from the ability to control and restrict free movement. Mobile workers and federation institutions are victims; mobile workers face direct costs and restrictions, while federation institutions bear the cost of a fragmented and less effective federal policy. This structural asymmetry drives the high extractiveness.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_integration_balance,
    'Is the current balance between national sovereignty and federal integration genuinely optimal for the federation''s long-term stability and prosperity, or does it disproportionately favor national interests at the expense of federal cohesion?',
    'Longitudinal economic and social impact studies comparing federations with different balances, and analysis of internal political stability under varying migration regimes.',
    'If the balance is found to be suboptimal, it would support re-evaluating the ''sovereignty primary'' reading towards greater integration or a more balanced subsidiarity approach, potentially reclassifying the constraint as more extractive from a federal perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_balance, empirical, 'The optimal balance point between national and federal authority in a multi-state federation.').

omega_variable(
    legitimacy_of_national_protection,
    'Are the ''protection'' claims for national labor markets and welfare systems genuinely necessary and proportionate, or are they used as cover for xenophobia or economic nationalism?',
    'Independent audits of labor market impacts and welfare system sustainability, comparing outcomes with and without specific restrictions, and analysis of public discourse for underlying motivations.',
    'If claims are found to be disproportionate or pretextual, it would expose the ''sovereignty primary'' reading as a Snare, where the coordination story is a cover for pure extraction and suppression of mobile workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_national_protection, conceptual, 'Whether national protection claims are legitimate or a cover for other motives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t5, federation_membership_treaty__sovereignty_primary, theater_ratio, 5, 0.17).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__sovereignty_primary, theater_ratio, 10, 0.18).
narrative_ontology:measurement(fede_tr_t15, federation_membership_treaty__sovereignty_primary, theater_ratio, 15, 0.19).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t5, federation_membership_treaty__sovereignty_primary, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__sovereignty_primary, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(fede_be_t15, federation_membership_treaty__sovereignty_primary, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t5, federation_membership_treaty__sovereignty_primary, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__sovereignty_primary, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(fede_su_t15, federation_membership_treaty__sovereignty_primary, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_treaty' kernel. This 'sovereignty_primary' reading emphasizes national control, contrasting with 'integration_primary' (federal rights) and 'subsidiarity_balance' (proportionality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
