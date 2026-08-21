% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary: Embedded Liberalism Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint represents the 'embedded liberalism' reading of the NAFTA
 *   jurisdictional boundary, where trade liberalization is balanced with the
 *   preservation of legitimate domestic policy space for environmental and
 *   labor standards. It acknowledges partial jurisdictional overlap and
 *   defensive authority for regulatory agencies, leading to moderate
 *   extraction through litigation costs. This reading contrasts with 'capital
 *   supremacy' (trade as overriding domestic law) and 'sovereignty primacy'
 *   (trade as subordinate to domestic law).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.45).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.3).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary: Embedded Liberalism Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'cb3caa3c-78ea-4764-a931-f10879532e8d').
narrative_ontology:cs_kernel_codification('cb3caa3c-78ea-4764-a931-f10879532e8d', fixed_text).
narrative_ontology:cs_authority_grounding('cb3caa3c-78ea-4764-a931-f10879532e8d', lineage).
narrative_ontology:cs_interpretation_layer_present('cb3caa3c-78ea-4764-a931-f10879532e8d').
narrative_ontology:cs_reading_relation('cb3caa3c-78ea-4764-a931-f10879532e8d', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb3caa3c-78ea-4764-a931-f10879532e8d', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('cb3caa3c-78ea-4764-a931-f10879532e8d', foundational, trade_liberalization_with_social_safeguards).
narrative_ontology:cs_axiom_status(trade_liberalization_with_social_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('cb3caa3c-78ea-4764-a931-f10879532e8d', trade_liberalization_with_social_safeguards, conventional).
narrative_ontology:cs_axiom('cb3caa3c-78ea-4764-a931-f10879532e8d', foundational, non_discriminatory_domestic_regulation_is_legitimate).
narrative_ontology:cs_axiom_status(non_discriminatory_domestic_regulation_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('cb3caa3c-78ea-4764-a931-f10879532e8d', non_discriminatory_domestic_regulation_is_legitimate, deontological).
narrative_ontology:cs_reference_frame('cb3caa3c-78ea-4764-a931-f10879532e8d', post_bretton_woods_compromise).
narrative_ontology:cs_drift_state('cb3caa3c-78ea-4764-a931-f10879532e8d', contemporary_globalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cb3caa3c-78ea-4764-a931-f10879532e8d', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_industries).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_advocacy_groups).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_unions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from predictable market access and reduced trade barriers, viewing domestic regulations as potential non-tariff barriers. They leverage the trade agreement's dispute resolution mechanisms to challenge regulations perceived as discriminatory or protectionist, but acknowledge the need for some legitimate domestic policy space.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Gain from expanded export markets and reduced costs due to trade liberalization. They support the framework that balances market access with domestic policy, as it provides stability for their operations while allowing for some national regulatory distinctiveness.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_industries, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of defending domestic environmental, labor, and health regulations against challenges under the trade agreement. They operate within the 'legitimate objectives' boundary, which allows them to maintain non-discriminatory standards, but face litigation risks and potential policy chill.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Advocate for strong domestic environmental standards and view trade agreements as potentially undermining these. They engage in legal and political efforts to ensure that environmental protections are recognized as legitimate domestic policy objectives, compatible with trade obligations.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_advocacy_groups, payer,
    organized, generational, constrained, regional).

% Seek to protect domestic labor standards and workers' rights, often seeing trade agreements as a threat to these. They work to ensure that labor standards are considered legitimate domestic policy and are not undermined by trade liberalization, facing pressure from capital mobility.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_unions, payer,
    organized, biographical, constrained, national).

% Interpret the trade agreement text to adjudicate disputes between member states and investors. Their rulings define the boundary between legitimate domestic policy and trade obligations, shaping the practical application of the embedded liberalism reading.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_panels, agenda_setter,
    institutional, immediate, analytical, regional).

% Benefit from increased availability of goods and potentially lower prices due to trade liberalization. They also benefit from domestic regulations that protect health and safety, which this reading seeks to preserve. Their interests are often diffuse and indirectly represented.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_consumers, beneficiary,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for international trade by reducing tariffs and non-tariff barriers, while attempting to coordinate the recognition of legitimate domestic regulatory objectives across member states.
% TRANSFER_FUNCTION: Facilitates the flow of goods, services, and capital across borders, transferring market access benefits to export-oriented industries and multinational corporations, while imposing litigation costs and policy constraints on domestic regulatory agencies and advocacy groups.
% ABSENT_VOICES: Small domestic businesses unable to compete with international firms, and marginalized communities disproportionately affected by environmental degradation or labor exploitation, often lack direct representation in the trade agreement's design and dispute resolution processes.
% DISAPPEARANCE_RATIONALE: If the NAFTA framework vanished, trade flows would be disrupted, tariffs would likely increase, and the balance between market access and domestic policy space would be renegotiated bilaterally or through other multilateral forums. The existing equilibrium, however imperfect, would collapse.
% FOUNDING_PROBLEM: To reduce trade barriers and foster economic integration among North American countries, while acknowledging the need for member states to maintain some level of domestic regulatory autonomy for social and environmental protection.
% FOUNDING_PROBLEM_CORROBORATION: Governments of member states, academic scholars of international political economy, and some business associations corroborate the ongoing relevance of balancing trade liberalization with domestic policy space. Environmental and labor groups, while critical of specific outcomes, generally acknowledge the initial intent to embed social concerns within the trade framework.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the costs borne by domestic regulatory agencies and advocacy groups in defending policies against trade challenges, but also the success in preserving some policy space. Suppression (0.30) is relatively low, as domestic actors retain significant agency to resist and defend their regulations, rather than being fully coerced. Theater ratio (0.15) is low, indicating that the stated balance between trade and domestic policy is largely functional, though not without contestation. The metrics reflect a dynamic equilibrium where both coordination and extraction are present, consistent with a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of multinational corporations, this reading provides a necessary framework for market access and predictability, with reasonable limits on arbitrary domestic regulation. From the perspective of domestic regulatory agencies and advocacy groups, it represents a constant struggle to preserve essential public protections against the pressures of trade liberalization, with the 'legitimate objectives' clause serving as a crucial but often contested defense.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational corporations and export-oriented industries are beneficiaries, gaining from market access and a framework that limits purely protectionist domestic policies. Domestic regulatory agencies, environmental groups, and labor unions are payers, bearing the costs of defending legitimate regulations and facing policy chill. Trade dispute panels act as agenda-setters, interpreting the agreement and shaping the boundary. Domestic consumers are diffuse beneficiaries of both trade and regulation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_objectives_definition,
    'How is ''legitimate domestic policy objective'' defined and applied by trade dispute panels, and does this definition genuinely protect non-discriminatory environmental/labor standards?',
    'Analysis of dispute panel rulings over time, focusing on cases involving environmental and labor standards, and comparison with the original intent of the ''legitimate objectives'' clause.',
    'If the definition is consistently narrow or biased towards trade liberalization, the effective extractiveness on domestic policy space is higher, pushing the constraint towards a Snare. If it is broad and consistently protective, extractiveness is lower, closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_objectives_definition, empirical, 'Ambiguity in the interpretation of ''legitimate objectives'' in trade disputes.').

omega_variable(
    reading_coexistence_stability,
    'Can the ''embedded liberalism'' reading genuinely coexist with the ''capital supremacy'' and ''sovereignty primacy'' readings in the long term, or will one eventually foreclose the others?',
    'Longitudinal study of legal precedents, political discourse, and institutional shifts in trade governance. Observe whether one reading gains decisive legal or political dominance, or if the contest remains a persistent feature.',
    'If ''capital supremacy'' forecloses ''embedded liberalism'', the constraint shifts towards a Snare. If ''sovereignty primacy'' forecloses it, the constraint might become a Rope (for coordination among sovereign states) or even a Mountain (if trade becomes purely voluntary). If coexistence persists, the Tangled Rope classification remains stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_stability, conceptual, 'The long-term stability of the coexistence of different readings of the NAFTA jurisdictional boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 1994, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2006, 0.15).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2012, 0.17).
narrative_ontology:measurement(naft_tr_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1994, 0.35).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2006, 0.43).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2012, 0.46).
narrative_ontology:measurement(naft_be_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1994, 0.25).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2006, 0.3).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2012, 0.32).
narrative_ontology:measurement(naft_su_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2020, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NAFTA jurisdictional boundary kernel. This 'embedded liberalism' reading balances market access with domestic policy space, contrasting with 'capital supremacy' (trade overrides domestic law) and 'sovereignty primacy' (domestic law subordinates trade).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
