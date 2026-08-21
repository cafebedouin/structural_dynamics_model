% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Single Market Integration Primary: Free Movement as Constitutive
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'integration_primary' reading of the
 *   federation's membership treaty, where free movement is considered a
 *   fundamental, constitutive element of the single market. Restrictions on
 *   movement are viewed as presumptively illegitimate and require narrow
 *   justification. This reading prioritizes deeper integration and economic
 *   efficiency, often at the expense of national policy autonomy and local
 *   social cohesion. The metrics reflect a system that, while providing
 *   genuine coordination, also extracts significantly from certain member
 *   states and local populations, requiring active enforcement to maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.65).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.78).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Single Market Integration Primary: Free Movement as Constitutive").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, 'b43db46f-8c37-4f93-992c-c948b59e3054').
narrative_ontology:cs_kernel_codification('b43db46f-8c37-4f93-992c-c948b59e3054', fixed_text).
narrative_ontology:cs_authority_grounding('b43db46f-8c37-4f93-992c-c948b59e3054', lineage).
narrative_ontology:cs_interpretation_layer_present('b43db46f-8c37-4f93-992c-c948b59e3054').
narrative_ontology:cs_reading_relation('b43db46f-8c37-4f93-992c-c948b59e3054', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('b43db46f-8c37-4f93-992c-c948b59e3054', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('b43db46f-8c37-4f93-992c-c948b59e3054', foundational, free_movement_as_foundational_right).
narrative_ontology:cs_axiom_status(free_movement_as_foundational_right, holdable).
narrative_ontology:cs_axiom_grounding('b43db46f-8c37-4f93-992c-c948b59e3054', free_movement_as_foundational_right, deontological).
narrative_ontology:cs_axiom('b43db46f-8c37-4f93-992c-c948b59e3054', foundational, economic_integration_as_primary_goal).
narrative_ontology:cs_axiom_status(economic_integration_as_primary_goal, holdable).
narrative_ontology:cs_axiom_grounding('b43db46f-8c37-4f93-992c-c948b59e3054', economic_integration_as_primary_goal, instrumental).
narrative_ontology:cs_reference_frame('b43db46f-8c37-4f93-992c-c948b59e3054', founding_treaties_era).
narrative_ontology:cs_drift_state('b43db46f-8c37-4f93-992c-c948b59e3054', contemporary_migration_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b43db46f-8c37-4f93-992c-c948b59e3054', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, multinational_corporations).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the right to seek employment and reside anywhere within the single market, accessing broader economic opportunities and social benefits. Their mobility is a core tenet of this reading.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, regional).

% Benefit from a larger, more flexible labor pool and reduced administrative burdens for cross-border operations, optimizing their workforce and supply chains across the single market.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the costs of increased competition for jobs, potential wage depression in certain sectors, and strain on local public services due to rapid population influx, with limited mechanisms to control these effects.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_markets, payer,
    powerless, immediate, trapped, local).

% Experience fiscal pressure and administrative complexity from providing social benefits and services to a mobile population, often without corresponding tax contributions or adequate compensatory mechanisms.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    institutional, generational, constrained, national).

% Are bound by treaty obligations to uphold free movement, limiting their ability to implement national policies that restrict migration or protect domestic labor markets, even when facing internal political pressure or economic strain. They also enforce the treaty.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, member_state_governments, agenda_setter).

% Acts as the primary enforcer and interpreter of single market rules, including free movement. It initiates infringement procedures against member states that impose restrictions, ensuring the integration-primary reading is upheld.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, european_commission, agenda_setter,
    institutional, generational, analytical, continental).

% Interprets the foundational treaties and rules, consistently reinforcing the principle of free movement as a cornerstone of the single market, often prioritizing integration over national prerogatives.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, european_court_of_justice, observer,
    institutional, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates the efficient allocation of labor and capital across a large economic bloc, reducing transaction costs for businesses and expanding opportunities for individuals, thereby enhancing overall economic integration and competitiveness.
% TRANSFER_FUNCTION: Transfers economic opportunities and social benefits to mobile workers and multinational corporations, while imposing costs on local labor markets, national welfare systems, and member state governments through reduced policy autonomy.
% ABSENT_VOICES: Local communities and specific labor groups disproportionately affected by rapid migration flows often lack direct representation in the treaty-making and enforcement processes, and would advocate for stronger local protections or compensatory mechanisms.
% DISAPPEARANCE_RATIONALE: If the principle of free movement as constitutive of the single market vanished, member states would immediately reassert national border controls and labor market regulations, fragmenting the single market, disrupting supply chains, and forcing a fundamental re-evaluation of the entire federal project.
% FOUNDING_PROBLEM: The original problem was to overcome national protectionism and create a unified economic area to foster peace and prosperity in post-war Europe, preventing future conflicts through economic interdependence.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and the European Court of Justice consistently attest that the founding problem of economic fragmentation and potential conflict remains live, and that free movement is essential to its ongoing resolution. Member state governments, while acknowledging the historical context, increasingly contest the current balance, citing new challenges like global migration and fiscal pressures.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the benefits of free movement are concentrated among mobile workers and multinational corporations, while the costs (e.g., strain on welfare systems, wage competition) are borne by specific local populations and national governments. Suppression (0.78) is high due to the robust enforcement mechanisms of the European Commission and the European Court of Justice, which actively challenge and overturn national restrictions. Theater ratio is low (0.1) as the enforcement is genuinely aimed at upholding the treaty's principles, not merely performing compliance. The slight dip in extractiveness and suppression at the end of the interval reflects increased political resistance and calls for reform from member states, leading to some (minor) re-negotiation or re-interpretation pressures.
 *
 * PERSPECTIVAL GAP:
 *   The European Commission and Court of Justice perceive this as a necessary and beneficial coordination mechanism for the single market. In contrast, member state governments, particularly those facing high immigration or economic strain, experience it as an extractive constraint that limits their sovereign policy choices. Local labor markets and national welfare systems are primarily victims, experiencing the costs without significant agency.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and multinational corporations are clear beneficiaries, experiencing low directionality. Local labor markets and national welfare systems are direct targets, bearing significant costs, thus having high directionality. Member state governments are in a dual position: they are agenda-setters within the federation but also payers when their national interests conflict with the integration-primary reading, leading to a moderate-to-high directionality depending on the specific policy area. The European Commission and Court of Justice are agenda-setters and enforcers, with very low directionality, as they embody and uphold this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (economic integration, peace) is still live, but its implementation under the 'integration_primary' reading has led to significant asymmetric extraction. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring genuine coordination benefits). The ongoing contestation over the founding problem's status (live vs. dead) highlights the tension between the original mandate and its current operational effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_vs_sovereignty_balance,
    'Is the current balance between single market integration (free movement) and national sovereignty (member state control over borders/welfare) optimal, or has the ''integration_primary'' reading over-prioritized integration?',
    'Empirical analysis of economic and social outcomes in member states with high migration, coupled with a re-evaluation of the original treaty''s intent regarding the limits of integration versus national autonomy.',
    'If over-prioritized, the constraint''s effective extractiveness for member states would be re-evaluated as higher, potentially shifting its classification towards a Snare for those seats. If optimal, the current Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_sovereignty_balance, preference, 'The normative balance between integration and national control.').

omega_variable(
    subsidiarity_principle_application,
    'To what extent does the ''integration_primary'' reading adequately incorporate the principle of subsidiarity, allowing decisions to be taken at the lowest effective level?',
    'Legal and policy analysis comparing the application of free movement rules with the subsidiarity principle in specific cases, identifying instances where national or local solutions are demonstrably more effective but are overridden by EU law.',
    'If subsidiarity is consistently overridden, the suppression metric for member states would be re-evaluated as higher, and the constraint''s legitimacy (from a subsidiarity perspective) would diminish, reinforcing its extractive aspects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_principle_application, conceptual, 'The practical application of subsidiarity in free movement policy.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''integration_primary'' reading of the federation treaty, or is it a ''sovereignty_primary'' reading in disguise, where the ''sovereignty'' is merely shifted to the federal level?',
    'Comparative analysis of legal interpretations and enforcement actions across different federal systems, examining whether the locus of ultimate authority truly resides with the member states or has effectively been transferred to the federal institutions.',
    'If sovereignty is merely shifted, the ''member_state_governments'' would be reclassified as beneficiaries of a new, centralized sovereignty, rather than victims of lost autonomy, fundamentally altering the directionality and classification for that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity in the locus of sovereignty within the federal structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__integration_primary, theater_ratio, 10, 0.08).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__integration_primary, theater_ratio, 20, 0.1).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__integration_primary, theater_ratio, 30, 0.12).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__integration_primary, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__integration_primary, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__integration_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__integration_primary, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__integration_primary, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__integration_primary, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__integration_primary, 0.1).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, eu_common_agricultural_policy).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, eu_competition_law).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('integration_primary') of the 'federation_membership_treaty' kernel. It emphasizes free movement as constitutive of the single market. Sibling readings include 'sovereignty_primary' (emphasizing member state consent) and 'subsidiarity_balance' (emphasizing proportionality and national interests).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
