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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership (Integration Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint describes the 'integration reading' of federation
 *   membership, where supranational authority is legitimate, free movement is
 *   a constitutional right, and membership implies irreversible integration.
 *   This reading emphasizes the benefits of a unified market and political
 *   stability, while downplaying or externalizing the costs borne by local
 *   labor markets and national border authorities. It is a contested reading
 *   of the 'federation_membership' kernel, with a sibling
 *   'sovereignty_reading' that emphasizes national control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.65).
domain_priors:suppression_score(federation_membership__integration_reading, 0.7).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership (Integration Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '1b92857f-1367-4919-bcc5-bc1a0a5b2831').
narrative_ontology:cs_kernel_codification('1b92857f-1367-4919-bcc5-bc1a0a5b2831', formalized).
narrative_ontology:cs_authority_grounding('1b92857f-1367-4919-bcc5-bc1a0a5b2831', lineage).
narrative_ontology:cs_interpretation_layer_present('1b92857f-1367-4919-bcc5-bc1a0a5b2831').
narrative_ontology:cs_reading_relation('1b92857f-1367-4919-bcc5-bc1a0a5b2831', federation_membership__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('1b92857f-1367-4919-bcc5-bc1a0a5b2831', foundational, supranational_law_supremacy).
narrative_ontology:cs_axiom_status(supranational_law_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('1b92857f-1367-4919-bcc5-bc1a0a5b2831', supranational_law_supremacy, deontological).
narrative_ontology:cs_axiom('1b92857f-1367-4919-bcc5-bc1a0a5b2831', foundational, free_movement_as_constitutional_right).
narrative_ontology:cs_axiom_status(free_movement_as_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('1b92857f-1367-4919-bcc5-bc1a0a5b2831', free_movement_as_constitutional_right, deontological).
narrative_ontology:cs_reference_frame('1b92857f-1367-4919-bcc5-bc1a0a5b2831', ever_closer_union).
narrative_ontology:cs_drift_state('1b92857f-1367-4919-bcc5-bc1a0a5b2831', contemporary_nationalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1b92857f-1367-4919-bcc5-bc1a0a5b2831', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, supranational_institutions).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, national_border_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, national_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constitutional right to free movement across member states, enabling access to wider labor markets, social services, and cultural opportunities without national border restrictions. Their mobility is a core tenet of this reading.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Administer and enforce the principles of irreversible integration and free movement. Their legitimacy and power are derived from the foundational commitment to a federalized structure, which this reading upholds. They coordinate policy across member states.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Bear the costs of increased competition, wage depression, and strain on local public services due to unrestricted influx of labor from other member states. They have limited mechanisms to control migration flows or mitigate impacts under this reading.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    powerless, immediate, trapped, local).

% Are constrained in their ability to control national borders and migration flows, as free movement is a constitutional right. They must enforce supranational directives, even when these conflict with national policy preferences or perceived security needs.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_border_authorities, payer,
    organized, biographical, constrained, national).

% Are bound by the principle of irreversible integration, limiting their ability to unilaterally withdraw from the federation or reassert full national sovereignty over migration. They participate in supranational decision-making but are also subject to its authority.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, national_governments, payer).

% Advocate for national sovereignty and border controls, viewing free movement as an erosion of national identity and economic stability. Their policy preferences are systematically excluded from the supranational agenda-setting process under this reading.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_populist_movements, excluded,
    organized, biographical, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates economic and political integration across member states by establishing a common legal framework, a single market, and the free movement of people, capital, goods, and services, thereby preventing internal trade barriers and fostering collective prosperity.
% TRANSFER_FUNCTION: Transfers sovereignty over border control and certain economic policies from national governments to supranational institutions, in exchange for the benefits of a larger integrated market and political stability. It also transfers labor and social costs to local communities from mobile citizens.
% ABSENT_VOICES: National populist movements and local communities bearing the brunt of labor displacement and strain on public services are largely excluded from the supranational decision-making processes that enshrine free movement as an irreversible right. They would advocate for greater national control and local protections.
% DISAPPEARANCE_RATIONALE: If the integration reading of federation membership vanished overnight, the constitutional right to free movement would collapse, national borders would re-emerge as primary control points, and the supranational institutions would lose their legitimacy. This would trigger a massive reorganization of labor markets, political structures, and economic relationships across the continent.
% FOUNDING_PROBLEM: The founding problem was to prevent future inter-state conflicts and foster economic recovery and prosperity in post-war Europe by binding nations together through shared institutions and economic interdependence.
% FOUNDING_PROBLEM_CORROBORATION: Supranational institutions and many national governments attest that the problem of inter-state conflict and economic instability remains live, requiring continued integration. However, national populist movements and some economists contest this, arguing that the current level of integration creates new forms of instability and democratic deficit. Independent historical analysis corroborates the initial problem, but its contemporary status is contested by a range of political and economic actors outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates economic and political integration (benefiting mobile citizens and supranational institutions) but also involves significant asymmetric extraction from local labor markets and national border authorities. The extractiveness (0.65) reflects the costs of labor displacement and loss of national control. Suppression (0.7) is high due to the active enforcement of supranational law over national preferences. Theater ratio (0.2) is low, as the integration project is still actively pursued, though some enforcement is performative in the face of national resistance.
 *
 * PERSPECTIVAL GAP:
 *   Mobile citizens and supranational institutions experience this as a beneficial Rope, enabling prosperity and stability. In contrast, local labor markets and national border authorities experience it as a Snare, extracting control and imposing costs without adequate recourse. The engine's per-seat classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile citizens are clear beneficiaries (d=0.0-0.1) due to enhanced opportunities. Supranational institutions are also beneficiaries (d=0.1-0.2) as their power and legitimacy are amplified. Local labor markets and national border authorities are targets (d=0.8-0.9) as they bear the costs of lost control and economic pressure. National governments are dual-positioned, benefiting from integration but paying in sovereignty (d=0.4-0.6).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing conflict, fostering prosperity) is still considered 'live' by its beneficiaries, but its status is 'contested' by those bearing the costs. This prevents mislabeling it as a Piton, as it still serves a perceived function for powerful actors. However, the rising extractiveness and suppression over time suggest a drift towards a more extractive form, where the original coordination function is increasingly overshadowed by rent-seeking and control consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_vs_sovereignty_ambiguity,
    'Is federation membership an irreversible integration (as this reading claims) or a conditional treaty (as the sovereignty reading claims)?',
    'A definitive legal ruling by the highest court of the federation on the right to unilateral withdrawal or reassertion of national border control, or a constitutional amendment clarifying the nature of membership.',
    'If resolved as irreversible integration, this reading''s classification as Tangled Rope would be reinforced, with higher suppression for national actors. If resolved as a conditional treaty, the constraint would likely reclassify towards a Rope or even a Scaffold, with lower extraction and suppression for national actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(integration_vs_sovereignty_ambiguity, conceptual, 'The fundamental nature of federation membership.').

omega_variable(
    labor_market_impact_measurement,
    'What is the precise economic impact of free movement on local labor markets, distinguishing between short-term disruption and long-term adjustment/benefit?',
    'Comprehensive, longitudinal econometric studies comparing regions with high and low immigration from other member states, controlling for other economic factors.',
    'If the negative impacts on local labor markets are empirically shown to be severe and persistent, the extractiveness metric would be further justified and potentially increased. If long-term benefits are demonstrated, the extractiveness might be re-evaluated downwards.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_impact_measurement, empirical, 'Quantifying the economic costs of free movement on local communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership__integration_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(fede_tr_t1999, federation_membership__integration_reading, theater_ratio, 1999, 0.12).
narrative_ontology:measurement(fede_tr_t2005, federation_membership__integration_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(fede_tr_t2011, federation_membership__integration_reading, theater_ratio, 2011, 0.18).
narrative_ontology:measurement(fede_tr_t2017, federation_membership__integration_reading, theater_ratio, 2017, 0.19).
narrative_ontology:measurement(fede_tr_t2024, federation_membership__integration_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership__integration_reading, base_extractiveness, 1993, 0.45).
narrative_ontology:measurement(fede_be_t1999, federation_membership__integration_reading, base_extractiveness, 1999, 0.5).
narrative_ontology:measurement(fede_be_t2005, federation_membership__integration_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(fede_be_t2011, federation_membership__integration_reading, base_extractiveness, 2011, 0.6).
narrative_ontology:measurement(fede_be_t2017, federation_membership__integration_reading, base_extractiveness, 2017, 0.63).
narrative_ontology:measurement(fede_be_t2024, federation_membership__integration_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership__integration_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(fede_su_t1999, federation_membership__integration_reading, suppression_requirement, 1999, 0.55).
narrative_ontology:measurement(fede_su_t2005, federation_membership__integration_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(fede_su_t2011, federation_membership__integration_reading, suppression_requirement, 2011, 0.65).
narrative_ontology:measurement(fede_su_t2017, federation_membership__integration_reading, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement(fede_su_t2024, federation_membership__integration_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership__integration_reading, 0.1).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'integration reading' of the 'federation_membership' kernel. Its sibling, 'federation_membership__sovereignty_reading', presents an alternative interpretation of the same kernel, emphasizing national control and conditional membership. Both are linked as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
