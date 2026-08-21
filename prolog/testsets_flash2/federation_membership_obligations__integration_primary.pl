% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__integration_primary, []).

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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement: Integration Primary Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'integration primary' reading of EU
 *   federation membership obligations, where free movement is a foundational
 *   right of EU citizenship and single market functioning. Under this
 *   reading, member state welfare boundaries must yield to mobility rights,
 *   leading to mobile workers entering the full welfare beneficiary set in
 *   receiving states, and ECJ authority expanding via case law. This reading
 *   is often championed by EU institutions and pro-integration advocates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.65).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.7).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement: Integration Primary Reading").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '903c9d23-c84f-4774-a854-5b8ad2ced57a').
narrative_ontology:cs_kernel_codification('903c9d23-c84f-4774-a854-5b8ad2ced57a', formalized).
narrative_ontology:cs_authority_grounding('903c9d23-c84f-4774-a854-5b8ad2ced57a', lineage).
narrative_ontology:cs_interpretation_layer_present('903c9d23-c84f-4774-a854-5b8ad2ced57a').
narrative_ontology:cs_reading_relation('903c9d23-c84f-4774-a854-5b8ad2ced57a', federation_membership_obligations__member_sovereignty_primary, influences).
narrative_ontology:cs_reading_relation('903c9d23-c84f-4774-a854-5b8ad2ced57a', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('903c9d23-c84f-4774-a854-5b8ad2ced57a', foundational, free_movement_as_foundational_right).
narrative_ontology:cs_axiom_status(free_movement_as_foundational_right, holdable).
narrative_ontology:cs_axiom_grounding('903c9d23-c84f-4774-a854-5b8ad2ced57a', free_movement_as_foundational_right, deontological).
narrative_ontology:cs_axiom('903c9d23-c84f-4774-a854-5b8ad2ced57a', foundational, single_market_requires_unfettered_mobility).
narrative_ontology:cs_axiom_status(single_market_requires_unfettered_mobility, holdable).
narrative_ontology:cs_axiom_grounding('903c9d23-c84f-4774-a854-5b8ad2ced57a', single_market_requires_unfettered_mobility, empirically_contingent).
narrative_ontology:cs_reference_frame('903c9d23-c84f-4774-a854-5b8ad2ced57a', ever_closer_union_principle).
narrative_ontology:cs_drift_state('903c9d23-c84f-4774-a854-5b8ad2ced57a', post_brexit_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('903c9d23-c84f-4774-a854-5b8ad2ced57a', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, multinational_corporations).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, local_unskilled_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, member_state_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the right to live, work, and claim social benefits across the EU, regardless of their contribution history in the host state. Their mobility is a core right under this reading.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Actively promote and enforce free movement as a foundational principle of the Union and the single market. The European Court of Justice (ECJ) expands its authority through case law that reinforces mobility rights over national welfare boundaries.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit from a larger, more flexible labor pool across member states, allowing them to optimize labor costs and talent acquisition without national restrictions. They lobby for stronger integration.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, multinational_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the adjustment costs of increased competition for jobs and downward pressure on wages in receiving states, particularly in sectors with high migrant labor. They have limited exit options due to skill sets and local ties.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, local_unskilled_labor, payer,
    powerless, immediate, trapped, local).

% Are obligated to provide social benefits to mobile EU citizens, leading to increased fiscal pressure and perceived strain on public services in host countries. Their ability to restrict access is constrained by ECJ rulings.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_welfare_systems, payer,
    institutional, generational, constrained, national).

% Fund the national welfare systems that are increasingly open to mobile EU citizens, leading to concerns about fairness and sustainability, particularly in net-receiving states. Their political influence is often fragmented.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_taxpayers, payer,
    organized, biographical, constrained, national).

% Are caught between their obligations to EU law and the demands of their national electorates regarding welfare access and labor market protection. They administer the system but bear political and fiscal costs.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, member_state_governments, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the free movement of persons, services, capital, and goods across the EU, enabling a single market and fostering a sense of European citizenship by removing internal borders and national discrimination.
% TRANSFER_FUNCTION: Transfers social welfare obligations from the state of origin to the host state for mobile EU citizens, and transfers economic benefits (e.g., labor flexibility) to multinational corporations and mobile workers, while transferring adjustment costs to local labor and national welfare budgets.
% ABSENT_VOICES: Nationalist political movements and segments of the local working class in net-receiving states, who would advocate for stronger national control over borders and welfare access, are often marginalized in EU-level policy debates.
% DISAPPEARANCE_RATIONALE: If this reading of free movement vanished, the EU single market would fragment, national borders would reassert control over labor mobility, and the concept of EU citizenship would be severely undermined. Economic and social structures across the continent would undergo significant reorganization.
% FOUNDING_PROBLEM: The original problem was to create a unified European economic area, prevent future conflicts, and foster economic growth through integration, requiring the free movement of factors of production, including labor.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and pro-integration academics attest that the problem of economic fragmentation and the need for deeper integration remain live. Critics (e.g., some national politicians, economists focused on fiscal sustainability) argue that the current interpretation of free movement has created new problems that overshadow the original benefits.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates free movement and market integration (beneficiaries: mobile EU citizens, multinational corporations, EU institutions) but also involves significant asymmetric extraction (victims: local unskilled labor, national welfare systems, member state taxpayers). The extractiveness (0.65) reflects the fiscal and social costs borne by host states and their citizens, while suppression (0.70) indicates the active legal and institutional enforcement required to override national welfare state autonomy. The rising extractiveness and suppression over time reflect the deepening of integration and the increasing legal challenges to national restrictions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU institutions and mobile citizens, this is a successful coordination mechanism for integration. From the perspective of local labor and national welfare systems, it is an extractive mechanism that imposes costs without commensurate benefits. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions are agenda-setters and beneficiaries, pushing for deeper integration. Mobile EU citizens and multinational corporations are direct beneficiaries of expanded mobility. Local unskilled labor and national welfare systems are primary payers, bearing the costs of increased competition and fiscal strain. Member state governments are caught in a dual role, administering the system while also bearing political costs from their taxpayers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_sustainability_impact,
    'What is the long-term fiscal impact of this reading on net-receiving member states'' welfare systems, considering both contributions and expenditures?',
    'Comprehensive, independent longitudinal economic studies comparing fiscal balances of mobile EU citizens with native populations, disaggregated by skill level and employment status.',
    'If a significant, uncompensated fiscal drain is demonstrated, it would strengthen the ''extraction'' component of the constraint, potentially reclassifying it towards a Snare from the perspective of national welfare systems. If fiscal neutrality or net positive contribution is shown, it would support the ''coordination'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_impact, empirical, 'Assesses the true fiscal burden or benefit of free movement on host states'' welfare systems.').

omega_variable(
    democratic_legitimacy_deficit,
    'To what extent does the expansion of ECJ authority in free movement cases, overriding national democratic decisions on welfare, contribute to a democratic legitimacy deficit in the EU?',
    'Political science research on public opinion regarding ECJ rulings, analysis of national parliamentary debates, and studies on the responsiveness of EU institutions to national concerns.',
    'If a significant and unaddressed legitimacy deficit is identified, it would highlight the ''suppression'' aspect of the constraint as a political rather than purely legal enforcement, potentially increasing the perceived extractiveness from the perspective of national electorates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_deficit, conceptual, 'Examines the tension between judicial integration and national democratic accountability.').

omega_variable(
    alternative_integration_models,
    'Are there alternative models of European integration that could achieve similar economic benefits without the same level of welfare state convergence or social costs?',
    'Comparative analysis of other federal or confederal systems, or theoretical modeling of ''multi-speed'' or ''differentiated integration'' scenarios.',
    'If viable alternatives exist that reduce extraction while maintaining coordination, it would challenge the necessity of the current ''integration primary'' reading and suggest that some of its extractive elements are not inherent to the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_integration_models, preference, 'Explores whether the current model of integration is the only path to its stated goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_obligations__integration_primary, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_obligations__integration_primary, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(fede_tr_t2007, federation_membership_obligations__integration_primary, theater_ratio, 2007, 0.15).
narrative_ontology:measurement(fede_tr_t2014, federation_membership_obligations__integration_primary, theater_ratio, 2014, 0.18).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_obligations__integration_primary, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_obligations__integration_primary, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_obligations__integration_primary, base_extractiveness, 1993, 0.4).
narrative_ontology:measurement(fede_be_t2000, federation_membership_obligations__integration_primary, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(fede_be_t2007, federation_membership_obligations__integration_primary, base_extractiveness, 2007, 0.55).
narrative_ontology:measurement(fede_be_t2014, federation_membership_obligations__integration_primary, base_extractiveness, 2014, 0.6).
narrative_ontology:measurement(fede_be_t2020, federation_membership_obligations__integration_primary, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(fede_be_t2024, federation_membership_obligations__integration_primary, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_obligations__integration_primary, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement(fede_su_t2000, federation_membership_obligations__integration_primary, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(fede_su_t2007, federation_membership_obligations__integration_primary, suppression_requirement, 2007, 0.6).
narrative_ontology:measurement(fede_su_t2014, federation_membership_obligations__integration_primary, suppression_requirement, 2014, 0.65).
narrative_ontology:measurement(fede_su_t2020, federation_membership_obligations__integration_primary, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(fede_su_t2024, federation_membership_obligations__integration_primary, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, eu_single_market_regulations).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, national_labor_market_policies).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'federation_membership_obligations' kernel. Other readings include 'member_sovereignty_primary' and 'selective_solidarity', which emphasize national control and tiered rights, respectively. Each reading instantiates a distinct constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
