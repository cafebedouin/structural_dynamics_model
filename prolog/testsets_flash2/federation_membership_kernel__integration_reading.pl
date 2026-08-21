% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement (Integrationist Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story represents the 'integrationist reading' of the EU's
 *   free movement kernel. It views free movement as a fundamental right and a
 *   core driver of EU integration, with supranational institutions (ECJ)
 *   interpreting its scope expansively. This reading prioritizes labor
 *   mobility and equal treatment across the Union, often at the expense of
 *   national welfare state autonomy and local labor market protections. The
 *   metrics reflect the increasing extractiveness and suppression experienced
 *   by national systems and local populations as this reading has gained
 *   dominance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.75).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement (Integrationist Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '18758341-e4f5-4243-90fb-8f8e1dd5486e').
narrative_ontology:cs_kernel_codification('18758341-e4f5-4243-90fb-8f8e1dd5486e', fixed_text).
narrative_ontology:cs_authority_grounding('18758341-e4f5-4243-90fb-8f8e1dd5486e', lineage).
narrative_ontology:cs_interpretation_layer_present('18758341-e4f5-4243-90fb-8f8e1dd5486e').
narrative_ontology:cs_reading_relation('18758341-e4f5-4243-90fb-8f8e1dd5486e', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('18758341-e4f5-4243-90fb-8f8e1dd5486e', federation_membership_kernel__welfare_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('18758341-e4f5-4243-90fb-8f8e1dd5486e', foundational, eu_citizenship_as_primary_status).
narrative_ontology:cs_axiom_status(eu_citizenship_as_primary_status, holdable).
narrative_ontology:cs_axiom_grounding('18758341-e4f5-4243-90fb-8f8e1dd5486e', eu_citizenship_as_primary_status, deontological).
narrative_ontology:cs_axiom('18758341-e4f5-4243-90fb-8f8e1dd5486e', foundational, single_market_completion_requires_unfettered_mobility).
narrative_ontology:cs_axiom_status(single_market_completion_requires_unfettered_mobility, holdable).
narrative_ontology:cs_axiom_grounding('18758341-e4f5-4243-90fb-8f8e1dd5486e', single_market_completion_requires_unfettered_mobility, empirically_contingent).
narrative_ontology:cs_reference_frame('18758341-e4f5-4243-90fb-8f8e1dd5486e', treaty_of_maastricht_vision).
narrative_ontology:cs_drift_state('18758341-e4f5-4243-90fb-8f8e1dd5486e', contemporary_welfare_state_pressures, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('18758341-e4f5-4243-90fb-8f8e1dd5486e', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, multinational_corporations).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_tax_bases).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The European Commission and European Court of Justice (ECJ) actively promote and enforce an expansive interpretation of free movement, viewing it as essential for deeper integration and single market completion. They issue directives and rulings that prioritize mobility over national regulatory autonomy.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, eu_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from the right to live, work, and access social benefits in any EU member state, regardless of economic activity. This enhances their individual opportunities and freedom, but they may face social integration challenges.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Benefit from a flexible, continent-wide labor pool, allowing them to optimize staffing and reduce labor costs by recruiting from lower-wage member states. They lobby for continued expansive interpretation of free movement.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, multinational_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the fiscal and administrative costs of providing social benefits and public services to a mobile population, often without corresponding fiscal transfers or sufficient national policy levers to manage demand. This creates pressure on national budgets and social cohesion.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Face increased competition for jobs, particularly in lower-skilled sectors, leading to wage depression or unemployment. Their ability to resist is limited by national labor laws and the supranational nature of the free movement right.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    powerless, immediate, trapped, local).

% Experience 'brain drain' as skilled workers migrate to wealthier member states, eroding their tax base and human capital. They lack effective mechanisms to retain talent or receive compensation for their investment in education.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_tax_bases, payer,
    institutional, generational, constrained, national).

% Are caught between their commitment to EU integration and the domestic political pressure to protect national welfare systems and labor markets. They must implement ECJ rulings even when they conflict with national policy preferences.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, member_state_governments, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, member_state_governments, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates labor allocation and ensures equal treatment across a single European market, preventing national protectionism from fragmenting the economic space and ensuring a common standard of citizenship rights.
% TRANSFER_FUNCTION: Transfers labor, human capital, and associated social costs from sending states to receiving states, and transfers decision-making authority over migration and welfare policy from national governments to supranational EU institutions.
% ABSENT_VOICES: National electorates in receiving states, who often express concerns about the fiscal and social impact on their welfare systems, are often framed as 'anti-EU' rather than as legitimate stakeholders in a coordination problem. Their concerns are often suppressed in the supranational discourse.
% DISAPPEARANCE_RATIONALE: If the expansive interpretation of free movement vanished, national borders would immediately reassert control over labor migration, leading to significant economic disruption, a fragmentation of the single market, and a fundamental re-evaluation of EU citizenship. The EU project itself would be profoundly altered.
% FOUNDING_PROBLEM: To prevent future European wars by fostering economic interdependence and creating a common European identity, ensuring that citizens of member states could live and work freely across borders, and completing a single market for goods, services, capital, and people.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and many mobile citizens attest that the founding problem of integration and market completion remains live. However, member state governments and national electorates increasingly contest whether the current expansive interpretation is the optimal solution, citing new challenges like welfare tourism and social dumping. Academic analysis from outside the benefiting parties supports the view that the problem has evolved beyond its initial framing.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates labor mobility and equal treatment (benefiting mobile citizens and multinational corporations) but does so with significant asymmetric extraction from receiving state welfare systems, displaced local labor, and sending state tax bases. Active enforcement by the ECJ is crucial for its persistence, overriding national attempts to limit its scope. The rising extractiveness and suppression over time reflect the increasing tension between supranational integration goals and national sovereignty concerns.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU institutions and mobile citizens, this is a successful Rope, delivering fundamental rights and economic integration. From the perspective of national welfare systems and local labor, it operates as a Snare, extracting resources and autonomy. The engine's classification as Tangled Rope captures this hybrid nature, where a genuine coordination function is intertwined with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions and mobile citizens are beneficiaries (low directionality), as the constraint directly serves their interests. Multinational corporations also benefit from expanded labor pools. Receiving state welfare systems, displaced local labor, and sending state tax bases are targets (high directionality), bearing the costs without direct compensation. Member state governments are in a complex position, acting as both agenda-setters (implementing EU law) and payers (managing domestic costs and resistance).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (deepening integration, completing the single market) is still live, but its operational form has shifted. What began as a coordination mechanism for economic integration has accumulated extractive layers, particularly concerning welfare access and labor market impacts. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the genuine coordination function for mobile citizens).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_compensation_mechanism,
    'Could a robust EU-level fiscal compensation mechanism mitigate the costs borne by receiving state welfare systems, thereby reducing extraction?',
    'Implementation and evaluation of a new EU fiscal transfer system specifically designed to address the externalities of free movement.',
    'If effective, this would reduce the extractiveness for receiving states, potentially shifting the constraint closer to a Rope by internalizing costs currently externalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_compensation_mechanism, preference, 'Whether fiscal transfers can balance the costs of free movement.').

omega_variable(
    labor_market_impact_differentiation,
    'Are the negative labor market impacts on local populations primarily due to free movement, or are they exacerbated by other factors like automation and global supply chains?',
    'Detailed econometric studies disaggregating the impact of free movement from other economic forces on local labor markets.',
    'If other factors are dominant, the perceived extractiveness from displaced local labor might be overstated for this specific constraint; if free movement is the primary driver, the current high extractiveness is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_market_impact_differentiation, empirical, 'Disentangling free movement''s labor market impact from other economic trends.').

omega_variable(
    supranational_vs_national_legitimacy,
    'Is the ECJ''s expansive interpretation of free movement perceived as legitimate by national electorates, or does it create a democratic deficit that fuels resistance?',
    'Longitudinal public opinion surveys across member states, combined with analysis of national electoral outcomes and referenda related to EU integration.',
    'If legitimacy is low, the constraint''s suppression is more coercive than consensual, indicating a deeper structural tension that could lead to political instability or calls for treaty revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_vs_national_legitimacy, conceptual, 'The perceived legitimacy of supranational judicial authority over national policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_kernel__integration_reading, base_extractiveness, 1992, 0.4).
narrative_ontology:measurement(fede_be_t2000, federation_membership_kernel__integration_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(fede_be_t2008, federation_membership_kernel__integration_reading, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__integration_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__integration_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_kernel__integration_reading, suppression_requirement, 1992, 0.5).
narrative_ontology:measurement(fede_su_t2000, federation_membership_kernel__integration_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(fede_su_t2008, federation_membership_kernel__integration_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__integration_reading, suppression_requirement, 2016, 0.72).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__integration_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_single_market_regulation).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, national_welfare_state_design).

% DUAL FORMULATION NOTE:
% This constraint is the 'integration_reading' of the 'federation_membership_kernel'. It is one of three distinct readings, each modeled as a separate constraint story, linked by their shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
