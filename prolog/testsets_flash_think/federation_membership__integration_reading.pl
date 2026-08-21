% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration (Integration Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'integration reading' of federation
 *   membership, where membership implies irreversible integration,
 *   supranational authority is legitimate, and free movement is a
 *   constitutional right. It is one reading of the 'federation_membership'
 *   kernel. This reading emphasizes the foundational and non-negotiable
 *   nature of these principles for the functioning of the federation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.7).
domain_priors:suppression_score(federation_membership__integration_reading, 0.6).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration (Integration Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, 'ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd').
narrative_ontology:cs_kernel_codification('ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd', formalized).
narrative_ontology:cs_authority_grounding('ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd', lineage).
narrative_ontology:cs_interpretation_layer_present('ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd').
narrative_ontology:cs_reading_relation('ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd', federation_membership__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd', foundational, supranational_law_supremacy).
narrative_ontology:cs_axiom_status(supranational_law_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd', supranational_law_supremacy, conventional).
narrative_ontology:cs_axiom('ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd', foundational, free_movement_fundamental_right).
narrative_ontology:cs_axiom_status(free_movement_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd', free_movement_fundamental_right, deontological).
narrative_ontology:cs_reference_frame('ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd', ever_closer_union_principle).
narrative_ontology:cs_drift_state('ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd', contemporary_nationalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ae6b0df4-bd28-4ef2-ba36-a3ef80ced6cd', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_federation_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, supranational_institutions).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, border_control_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, national_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These bodies (e.g., courts, commissions) interpret and enforce the foundational treaties and constitutional rights that guarantee free movement and the supremacy of supranational law. They benefit from the stability and expansion of the integrated market and their own legitimacy.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Individuals who exercise their constitutional right to live and work anywhere within the federation. They benefit from expanded opportunities, access to diverse labor markets, and the ability to move freely without border controls.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_federation_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Local economies and workers in regions experiencing significant influxes of mobile citizens. They bear the costs of increased competition for jobs, downward pressure on wages in certain sectors, and strain on local public services, with limited ability to control these flows.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    powerless, immediate, trapped, local).

% Political groups and citizens who advocate for national sovereignty over borders and migration policy. They perceive a loss of national control and identity, bearing the cost of policies they oppose but cannot unilaterally reverse due to supranational legal obligations.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, border_control_advocates, payer,
    organized, biographical, constrained, national).

% Member state governments are bound by supranational law to uphold free movement, even when it conflicts with national policy preferences or creates domestic political challenges. They implement supranational directives and bear the political and social costs of integration.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, national_governments, payer).

% Those who believe federation membership is a conditional treaty, not irreversible integration, and that national authority should retain border legitimacy. From the integration reading's perspective, their views are structurally excluded from the core legal framework.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, sovereignty_reading_adherents, excluded,
    organized, biographical, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To foster deep economic and political integration across member states, creating a single market and a shared political identity by guaranteeing the free movement of people, goods, services, and capital.
% TRANSFER_FUNCTION: Transfers labor, skills, and sometimes social welfare burdens across national borders, from local labor markets and national social systems to mobile citizens and the broader integrated economy.
% ABSENT_VOICES: Nationalist movements, protectionist labor unions, and local communities experiencing strain from rapid demographic shifts are often marginalized in supranational policy debates, despite bearing significant costs. They would advocate for greater national control over borders and migration.
% DISAPPEARANCE_RATIONALE: If the principle of irreversible integration and free movement vanished overnight, the entire legal, economic, and political architecture of the federation would unravel. Internal borders would reappear, trade would be disrupted, and the foundational premise of a 'union' would collapse, leading to a profound reorganization of the continent.
% FOUNDING_PROBLEM: To overcome centuries of national rivalries and devastating wars by binding nations into a common destiny, fostering economic interdependence, and establishing a framework for lasting peace and shared prosperity.
% FOUNDING_PROBLEM_CORROBORATION: Supranational court rulings, founding treaties, and academic analyses of peace dividends and economic growth within the federation consistently corroborate the ongoing relevance of these founding goals. While challenges exist, the core problem of preventing conflict and fostering cooperation is still considered live by proponents of integration.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high due to the significant economic and social costs borne by local labor markets and national communities from uncontrolled free movement, which are not fully compensated or mitigated. Suppression (0.6) is moderate, reflecting the active legal and political enforcement required to uphold supranational authority and free movement against persistent national resistance and calls for border controls. Theater ratio is low (0.1) because the principles of integration and free movement are genuinely central to the federation's operation, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of supranational institutions and mobile citizens, this constraint is a beneficial coordination mechanism for peace and prosperity. However, from the perspective of local labor markets and border control advocates, it functions as an extractive mechanism that imposes costs without adequate consent or compensation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Supranational institutions and mobile federation citizens are primary beneficiaries, gaining power, opportunities, and freedom. Local labor markets and border control advocates are victims, bearing the costs of economic disruption and perceived loss of sovereignty. National governments are in a dual role, acting as agenda-setters for implementation while also bearing political costs from their constituents.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (peace, prosperity, integration) is still considered live by its proponents. However, the high extractiveness and suppression, coupled with rising resistance, suggest that the mechanism for achieving this mandate has become asymmetric. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring the genuine coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_vs_treaty_ambiguity,
    'Is federation membership fundamentally an irreversible integration process, or a conditional treaty arrangement between sovereign states?',
    'A definitive ruling by the highest supranational court on the legal possibility of unilateral withdrawal or reassertion of national border controls, or a constitutional amendment clarifying the nature of membership.',
    'If resolved as a conditional treaty, the legitimacy of supranational authority and free movement as a constitutional right would be significantly weakened, potentially reclassifying this constraint towards a Snare or Piton from the perspective of national actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(integration_vs_treaty_ambiguity, conceptual, 'The core conceptual ambiguity regarding the nature of federation membership.').

omega_variable(
    labor_market_impact_quantification,
    'What is the precise, empirically verifiable impact of free movement on wages, employment, and public services in local labor markets across the federation?',
    'Comprehensive, independent economic studies using granular, longitudinal data across diverse regions, controlling for other economic variables.',
    'Clear evidence of severe, unmitigated negative impacts would strengthen the ''extraction'' component, potentially increasing the computed extractiveness and pushing the classification closer to a Snare. Evidence of net positive or easily mitigated impacts would support the ''coordination'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_impact_quantification, empirical, 'Empirical quantification of labor market impacts from free movement.').

omega_variable(
    supranational_legitimacy_source,
    'Does the legitimacy of supranational authority derive primarily from the consent of national governments (treaty-based), or from a direct democratic mandate of the federation''s citizens (constitutional-federalist)?',
    'A constitutional convention or referendum on the foundational principles of the federation, or a shift in the jurisprudence of the highest courts towards one grounding over the other.',
    'If legitimacy is primarily treaty-based, the ''integration reading'' becomes more vulnerable to national political shifts. If it''s direct-democratic, national resistance to free movement would be seen as challenging a broader popular mandate, potentially increasing the perceived suppression of national voices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_legitimacy_source, conceptual, 'The source of legitimacy for supranational authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1990, federation_membership__integration_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(fede_tr_t1995, federation_membership__integration_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(fede_tr_t2000, federation_membership__integration_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(fede_tr_t2005, federation_membership__integration_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(fede_tr_t2010, federation_membership__integration_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(fede_tr_t2015, federation_membership__integration_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(fede_tr_t2020, federation_membership__integration_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(fede_be_t1990, federation_membership__integration_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(fede_be_t1995, federation_membership__integration_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(fede_be_t2000, federation_membership__integration_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(fede_be_t2005, federation_membership__integration_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(fede_be_t2010, federation_membership__integration_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(fede_be_t2015, federation_membership__integration_reading, base_extractiveness, 2015, 0.69).
narrative_ontology:measurement(fede_be_t2020, federation_membership__integration_reading, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1990, federation_membership__integration_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(fede_su_t1995, federation_membership__integration_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(fede_su_t2000, federation_membership__integration_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(fede_su_t2005, federation_membership__integration_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(fede_su_t2010, federation_membership__integration_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(fede_su_t2015, federation_membership__integration_reading, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement(fede_su_t2020, federation_membership__integration_reading, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'integration_reading' of the 'federation_membership' kernel. It is structurally linked to the 'sovereignty_reading' as a competing interpretation of the same foundational concept.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
