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
 *   conditional on member state consent. States retain significant authority
 *   to protect national labor markets and welfare systems, often through
 *   administrative hurdles and conditional access to benefits. This reading
 *   prioritizes national autonomy over deeper federal integration, leading to
 *   a 'tangled rope' dynamic where some coordination occurs, but with
 *   substantial extraction from mobile workers and friction for federal
 *   institutions. The metrics reflect the ongoing enforcement required to
 *   maintain this balance.
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
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '5556da52-0ce8-4912-831f-860fb1918905').
narrative_ontology:cs_kernel_codification('5556da52-0ce8-4912-831f-860fb1918905', formalized).
narrative_ontology:cs_authority_grounding('5556da52-0ce8-4912-831f-860fb1918905', lineage).
narrative_ontology:cs_interpretation_layer_present('5556da52-0ce8-4912-831f-860fb1918905').
narrative_ontology:cs_reading_relation('5556da52-0ce8-4912-831f-860fb1918905', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('5556da52-0ce8-4912-831f-860fb1918905', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('5556da52-0ce8-4912-831f-860fb1918905', foundational, national_sovereignty_precedes_federal_integration).
narrative_ontology:cs_axiom_status(national_sovereignty_precedes_federal_integration, holdable).
narrative_ontology:cs_axiom_grounding('5556da52-0ce8-4912-831f-860fb1918905', national_sovereignty_precedes_federal_integration, conventional).
narrative_ontology:cs_axiom('5556da52-0ce8-4912-831f-860fb1918905', foundational, member_state_control_over_borders_is_fundamental).
narrative_ontology:cs_axiom_status(member_state_control_over_borders_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('5556da52-0ce8-4912-831f-860fb1918905', member_state_control_over_borders_is_fundamental, conventional).
narrative_ontology:cs_reference_frame('5556da52-0ce8-4912-831f-860fb1918905', westphalian_state_autonomy).
narrative_ontology:cs_drift_state('5556da52-0ce8-4912-831f-860fb1918905', contemporary_migration_crises_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5556da52-0ce8-4912-831f-860fb1918905', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_states).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, federation_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary authority over national borders, labor market regulations, and welfare provisions. They consent to free movement but assert the right to impose conditions to protect national interests, often leading to complex administrative hurdles for mobile workers. They benefit from controlling access to their social and economic resources.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_states, agenda_setter,
    institutional, generational, constrained, national).

% Are protected from perceived destabilization by unrestricted influxes of labor. This reading prioritizes the stability and regulatory autonomy of national employment conditions, benefiting existing workers and national economic planning.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_labor_markets, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(federation_membership_treaty__sovereignty_primary, national_labor_markets).

% Are shielded from potential strain due to immediate access by non-contributing mobile populations. This reading allows states to impose residency or contribution requirements, benefiting national taxpayers and existing beneficiaries.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_welfare_systems, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_non_agent(federation_membership_treaty__sovereignty_primary, national_welfare_systems).

% Face administrative burdens, conditional access to social benefits, and potential discrimination in labor markets due to national protective measures. Their 'free movement' is significantly curtailed by state-level consent requirements, making them bear the costs of national sovereignty assertions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    powerless, immediate, constrained, regional).

% Bear the cost of fragmented internal borders and inconsistent application of free movement principles. Their mandate for deeper integration is undermined by member states' assertion of primary sovereignty, leading to complex legal challenges and political friction.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federation_institutions, payer,
    organized, generational, constrained, continental).

% Benefit from the perceived protection of national resources and cultural identity, as their governments retain control over who enters and resides in the country. They may also experience reduced competition in certain labor sectors.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_citizens, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the limited integration of national economies and populations within a larger federation, allowing for some mobility while preserving member states' fundamental sovereign rights over their territories and social policies.
% TRANSFER_FUNCTION: Transfers regulatory autonomy and control over labor and welfare policy from the federation level back to individual member states, at the cost of reduced mobility and integration for workers.
% ABSENT_VOICES: Advocates for universal human rights and unrestricted mobility would object, arguing that national consent mechanisms create arbitrary barriers and undermine fundamental freedoms. They are often marginalized in national policy debates focused on state interests.
% DISAPPEARANCE_RATIONALE: If this reading of the treaty vanished, member states would lose a key justification for their border controls and welfare restrictions. The federation's institutions would likely assert greater authority over free movement, leading to a significant shift in migration patterns, labor market dynamics, and social policy across the bloc.
% FOUNDING_PROBLEM: The original problem was how to achieve economic integration and peace among sovereign nations without dissolving national identities or democratic control over domestic policy.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments and national electorates consistently attest that balancing national sovereignty with federal integration remains a live and critical problem. Independent political scientists and legal scholars corroborate that this tension is inherent to the federal project and continues to shape policy debates.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.65) is substantial because mobile workers face significant barriers and costs, while member states benefit from controlling access to their resources. Suppression (0.70) is high due to the active enforcement of national regulations and administrative requirements that limit genuine free movement. Theater ratio (0.20) is relatively low, as the national protections are genuinely implemented, though some rhetoric may overstate the threat of unrestricted movement. The increasing trend in extractiveness and suppression over time reflects a hardening of national positions in response to migration pressures.
 *
 * PERSPECTIVAL GAP:
 *   Member states perceive this as a legitimate exercise of sovereignty and necessary coordination to protect national interests. Mobile workers experience it as a snare, where the promise of 'free movement' is undermined by conditional access and administrative burdens. Federation institutions see it as a tangled rope, where the coordination function is hampered by persistent national-level extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states, national labor markets, and national welfare systems are beneficiaries (low d) as they gain control and protection. Mobile workers and federation institutions are victims/targets (high d) as they bear the costs of restricted movement and fragmented policy. National citizens are indirect beneficiaries, as their interests are prioritized by their governments.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_interest_definition,
    'How are ''national labor markets'' and ''welfare systems'' defined, and what specific threats do they face from unrestricted movement, as opposed to being used as a pretext for protectionism?',
    'Independent economic analysis comparing the actual impact of mobile workers on national systems versus the stated justifications for restrictions, disaggregated by sector and skill level.',
    'If the threats are found to be minimal or exaggerated, the justification for state consent mechanisms weakens, reclassifying the constraint closer to a snare. If threats are substantial, the coordination function is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_interest_definition, empirical, 'Ambiguity in the definition and actual threat to national interests.').

omega_variable(
    proportionality_of_restrictions,
    'Are the restrictions imposed by member states proportionate to the stated goals of protecting national interests, or do they create disproportionate barriers to free movement?',
    'Legal review by federal courts or arbitration bodies, assessing specific national measures against proportionality principles and the overall treaty objectives.',
    'If restrictions are found disproportionate, the constraint''s extractiveness and suppression would be re-evaluated upward, pushing it further towards a snare. If proportionate, the tangled rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_restrictions, conceptual, 'Whether national restrictions are proportionate or excessive.').

omega_variable(
    sovereignty_vs_integration_framing,
    'Is the ''sovereignty primary'' reading a defensible interpretation of the treaty''s original intent, or a later re-framing driven by political pressures and nationalistic sentiment?',
    'Historical-legal analysis of treaty negotiations, founding documents, and early jurisprudence, alongside contemporary political discourse analysis.',
    'If it''s a later re-framing, the constraint''s legitimacy is weakened, and its persistence might be seen as more reliant on active suppression rather than genuine coordination. If it aligns with original intent, its structural stability is higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_framing, conceptual, 'The historical and political grounding of the ''sovereignty primary'' interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__sovereignty_primary, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.15).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__sovereignty_primary, theater_ratio, 30, 0.18).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__sovereignty_primary, theater_ratio, 40, 0.19).
narrative_ontology:measurement(fede_tr_t50, federation_membership_treaty__sovereignty_primary, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__sovereignty_primary, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__sovereignty_primary, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__sovereignty_primary, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(fede_be_t50, federation_membership_treaty__sovereignty_primary, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__sovereignty_primary, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__sovereignty_primary, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__sovereignty_primary, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(fede_su_t50, federation_membership_treaty__sovereignty_primary, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_budget_contributions).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_common_agricultural_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_treaty' kernel. It emphasizes national sovereignty, contrasting with 'integration_primary' and 'subsidiarity_balance' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
