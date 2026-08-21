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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Single Market Free Movement as Primary Integration Principle
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'integration_primary' reading of the
 *   'federation_membership_treaty' kernel, which asserts that free movement
 *   is a foundational and constitutive element of the single market. Under
 *   this reading, any restrictions on free movement by member states are
 *   presumptively illegitimate and require narrow justification, with federal
 *   institutions actively enforcing this principle. This reading prioritizes
 *   the deepening of federal integration over national prerogatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.7).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.8).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Single Market Free Movement as Primary Integration Principle").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '0592fe28-9989-464a-8225-12d70f672323').
narrative_ontology:cs_kernel_codification('0592fe28-9989-464a-8225-12d70f672323', fixed_text).
narrative_ontology:cs_authority_grounding('0592fe28-9989-464a-8225-12d70f672323', lineage).
narrative_ontology:cs_interpretation_layer_present('0592fe28-9989-464a-8225-12d70f672323').
narrative_ontology:cs_reading_relation('0592fe28-9989-464a-8225-12d70f672323', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('0592fe28-9989-464a-8225-12d70f672323', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('0592fe28-9989-464a-8225-12d70f672323', foundational, free_movement_is_foundational_to_single_market).
narrative_ontology:cs_axiom_status(free_movement_is_foundational_to_single_market, holdable).
narrative_ontology:cs_axiom_grounding('0592fe28-9989-464a-8225-12d70f672323', free_movement_is_foundational_to_single_market, conventional).
narrative_ontology:cs_axiom('0592fe28-9989-464a-8225-12d70f672323', foundational, national_restrictions_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(national_restrictions_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('0592fe28-9989-464a-8225-12d70f672323', national_restrictions_presumptively_illegitimate, conventional).
narrative_ontology:cs_reference_frame('0592fe28-9989-464a-8225-12d70f672323', ever_closer_union).
narrative_ontology:cs_drift_state('0592fe28-9989-464a-8225-12d70f672323', contemporary_nationalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0592fe28-9989-464a-8225-12d70f672323', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, federal_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, single_market_businesses).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, member_states_with_local_concerns).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (e.g., European Commission, Court of Justice) interpret and enforce the treaties, prioritizing deeper integration and free movement as foundational. They gain legitimacy and expand their remit through this principle.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, federal_institutions, agenda_setter,
    institutional, generational, arbitrage, continental).

% Individuals who benefit directly from the right to live and work anywhere within the single market, accessing wider job opportunities and potentially higher wages. Their mobility is enabled by the constraint.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Companies that benefit from access to a larger, more flexible labor pool across member states, reducing labor costs and facilitating expansion. They advocate for the principle's strong enforcement.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, single_market_businesses, beneficiary,
    organized, biographical, mobile, continental).

% National governments that bear the political and social costs of perceived loss of control over borders, immigration, and labor market regulation. They are constrained by treaty obligations but face domestic pressure to restrict movement.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_states_with_local_concerns, payer,
    institutional, generational, constrained, national).

% Local economies and workers who may experience downward pressure on wages, increased competition for jobs, or strain on local services due to influxes of mobile workers. They have limited means to resist the federal principle.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_markets, payer,
    powerless, immediate, trapped, local).

% Public services and social security systems in member states that face increased demand from mobile workers and their families, potentially without corresponding increases in tax revenue or administrative capacity. They are legally obligated to provide access.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    institutional, generational, constrained, national).

% Political movements and citizens who prioritize national sovereignty and control over borders, often viewing free movement as an erosion of national identity and self-determination. Their arguments are often marginalized in federal discourse.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, sovereignty_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__integration_primary, federal_institutions).
narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates a unified economic area by removing barriers to labor mobility, enabling efficient allocation of human capital and fostering economic integration across member states, thereby strengthening the single market.
% TRANSFER_FUNCTION: Transfers the right to control national borders and labor market access from member states to the federal authority, in exchange for the benefits of a larger, more integrated market. It also transfers potential social and economic costs to local communities and national welfare systems.
% ABSENT_VOICES: Local communities and national populations concerned about the impact on public services, infrastructure, and social cohesion, whose concerns are often framed as protectionist or anti-integrationist by federal institutions. Sovereignty advocates are also excluded from the core framing.
% DISAPPEARANCE_RATIONALE: If the principle of free movement as a primary integration principle vanished, member states would immediately reassert border controls and national labor market regulations, fragmenting the single market and forcing a complete renegotiation of federal competencies and economic relationships. The entire federal project would be fundamentally altered.
% FOUNDING_PROBLEM: The fragmentation of post-war European economies by national borders, leading to inefficient resource allocation, limited economic growth, and political instability, hindering the creation of a truly common market.
% FOUNDING_PROBLEM_CORROBORATION: Federal institutions and pro-integration economists attest to the ongoing need for integration to prevent economic stagnation and political divergence. Member states and national economists often contest the extent to which the original problem remains paramount over new challenges, citing new problems arising from free movement itself. Legislative hearings and independent economic analyses from outside the benefiting parties support the contested status.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.7) reflects the significant transfer of sovereign control over borders and labor markets from member states to the federal level. Suppression (0.8) is high due to the active legal and political mechanisms employed by federal institutions to prevent or overturn national restrictions. The low theater ratio (0.1) indicates that the enforcement of free movement is a core, functional activity, not merely performative. Accessibility collapse (0.7) is substantial as member states' ability to implement alternative national policies is severely curtailed. Resistance (0.6) is moderate, reflecting ongoing political and social pushback from national populations and governments.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of federal institutions and pro-integration actors, this constraint functions as a Rope, solving a collective action problem for economic integration. However, from the perspective of member states, local labor markets, and national welfare systems, it operates as a Snare or Tangled Rope, extracting control and imposing costs without adequate compensation or recourse. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal institutions, mobile workers, and single-market businesses are the primary beneficiaries, gaining expanded authority, opportunities, and markets, respectively. Member states with local concerns, local labor markets, and national welfare systems are the primary payers, bearing the costs of reduced autonomy and increased strain on resources. Sovereignty advocates are excluded from the framing that defines the constraint's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope acknowledges the genuine coordination function of enabling a single market while simultaneously recognizing the asymmetric extraction from member states and local populations. It prevents mislabeling the constraint as a pure Rope (ignoring extraction) or a pure Snare (ignoring the coordination benefits for the single market as a whole).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_conditional_ambiguity,
    'Is free movement truly ''constitutive'' of the single market, or is it a ''conditional'' right subject to member state consent and capacity?',
    'Legal rulings from a supreme federal court that explicitly re-evaluate the foundational nature of free movement, or a treaty revision that redefines its scope and limits.',
    'If reclassified as conditional, the constraint''s suppression and extractiveness from member states would decrease, potentially shifting its type towards a Rope or even a Scaffold if temporary derogations become common.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutive_vs_conditional_ambiguity, conceptual, 'Ambiguity regarding the foundational status of free movement within the federal framework.').

omega_variable(
    local_impact_vs_federal_benefit_balance,
    'To what extent do the aggregate economic benefits of free movement at the federal level outweigh the localized social and economic costs borne by specific member states and communities?',
    'Comprehensive, independent economic and social impact assessments conducted at both federal and local levels, disaggregated by region and demographic, with transparent reporting of costs and benefits.',
    'If local costs are consistently found to outweigh federal benefits for specific regions, it would strengthen arguments for compensatory mechanisms or localized derogations, potentially reducing the constraint''s effective extractiveness on those communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_impact_vs_federal_benefit_balance, empirical, 'The balance between federal-level benefits and localized costs of free movement.').

omega_variable(
    enforcement_legitimacy_ambiguity,
    'Is the active enforcement of free movement by federal institutions perceived as legitimate protection of treaty rights, or as overreach and an imposition on national sovereignty?',
    'Public opinion surveys across member states, analysis of national political discourse, and the outcomes of national elections where free movement is a central issue. Legal challenges to federal rulings also provide evidence.',
    'If perceived as illegitimate overreach, resistance would likely increase, and the constraint''s persistence would depend more heavily on coercion, potentially pushing it closer to a Snare in the eyes of affected populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy_ambiguity, preference, 'Perceived legitimacy of federal enforcement of free movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__integration_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__integration_primary, theater_ratio, 20, 0.1).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__integration_primary, theater_ratio, 30, 0.1).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.1).
narrative_ontology:measurement(fede_tr_t50, federation_membership_treaty__integration_primary, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__integration_primary, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__integration_primary, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__integration_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(fede_be_t50, federation_membership_treaty__integration_primary, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__integration_primary, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__integration_primary, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__integration_primary, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(fede_su_t50, federation_membership_treaty__integration_primary, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
