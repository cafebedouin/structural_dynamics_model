% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS Strict EEZ Boundary Enforcement
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint describes the enforcement of Exclusive Economic Zone
 *   (EEZ) boundaries as defined by UNCLOS Article 57, asserting a
 *   200-nautical-mile limit for coastal states' exclusive resource rights and
 *   rejecting any overlay claims. It is a 'strict' reading of UNCLOS,
 *   prioritizing codified limits over historical usage or general freedom of
 *   navigation for resource exploitation. The constraint is actively enforced
 *   by coastal states, leading to significant extraction from those whose
 *   traditional access is denied.
 *
 * KEY AGENTS:
 *   - coastal_states_with_eez: Primary beneficiary (institutional/arbitrage) — gains exclusive resource control
 *   - overlapping_claimants: Primary victim (institutional/constrained) — loses access to disputed areas
 *   - distant_water_fishing_fleets: Primary victim (organized/constrained) — denied access to traditional fishing grounds
 *   - international_maritime_organizations: Agenda setter (institutional/analytical) — administers UNCLOS framework
 *   - non_ratifying_states: Excluded (institutional/constrained) — their alternative claims are suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.65).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.75).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Strict EEZ Boundary Enforcement").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, 'caaafd4f-690a-4d9f-a752-1f0d50bfcd35').
narrative_ontology:cs_kernel_codification('caaafd4f-690a-4d9f-a752-1f0d50bfcd35', fixed_text).
narrative_ontology:cs_authority_grounding('caaafd4f-690a-4d9f-a752-1f0d50bfcd35', lineage).
narrative_ontology:cs_interpretation_layer_present('caaafd4f-690a-4d9f-a752-1f0d50bfcd35').
narrative_ontology:cs_reading_relation('caaafd4f-690a-4d9f-a752-1f0d50bfcd35', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('caaafd4f-690a-4d9f-a752-1f0d50bfcd35', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('caaafd4f-690a-4d9f-a752-1f0d50bfcd35', foundational, unclos_article_57_primacy).
narrative_ontology:cs_axiom_status(unclos_article_57_primacy, holdable).
narrative_ontology:cs_axiom_grounding('caaafd4f-690a-4d9f-a752-1f0d50bfcd35', unclos_article_57_primacy, conventional).
narrative_ontology:cs_axiom('caaafd4f-690a-4d9f-a752-1f0d50bfcd35', foundational, exclusive_resource_sovereignty).
narrative_ontology:cs_axiom_status(exclusive_resource_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('caaafd4f-690a-4d9f-a752-1f0d50bfcd35', exclusive_resource_sovereignty, conventional).
narrative_ontology:cs_reference_frame('caaafd4f-690a-4d9f-a752-1f0d50bfcd35', unclos_codified_order).
narrative_ontology:cs_drift_state('caaafd4f-690a-4d9f-a752-1f0d50bfcd35', contemporary_geopolitical_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('caaafd4f-690a-4d9f-a752-1f0d50bfcd35', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_eez).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_fleets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states claim and enforce exclusive rights to marine resources within 200 nautical miles of their coasts, benefiting from the economic exploitation of fisheries, oil, and gas. They actively patrol and litigate to defend these boundaries.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_eez, beneficiary,
    institutional, generational, arbitrage, national).

% States with overlapping EEZ claims or historical claims that conflict with the strict UNCLOS interpretation. They bear the cost of lost access to resources and often engage in diplomatic disputes or low-level confrontations.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants, payer,
    institutional, generational, constrained, regional).

% Commercial fishing operations, often state-subsidized, that traditionally operated in areas now claimed as EEZs. They face exclusion, reduced catches, and increased operating costs, with limited options to find new, productive grounds.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_fleets, payer,
    organized, biographical, constrained, global).

% Bodies like the International Seabed Authority and the International Maritime Organization that administer aspects of UNCLOS. They coordinate the legal framework but also mediate disputes arising from its enforcement, balancing competing state interests.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_maritime_organizations, agenda_setter,
    institutional, civilizational, analytical, global).

% States that have not ratified UNCLOS but may still assert their own maritime claims or challenge the EEZ claims of others based on different interpretations of international law. Their positions are often marginalized by the strict EEZ reading.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifying_states, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_eez).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__strict_eez_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a clear, internationally recognized framework for allocating sovereign rights over marine resources, preventing open-access depletion and reducing disputes over maritime boundaries.
% TRANSFER_FUNCTION: Transfers exclusive rights to marine resources (fisheries, mineral rights, energy potential) from the global commons or historical claimants to coastal states, enforced by naval and legal means.
% ABSENT_VOICES: Indigenous communities with traditional fishing rights predating UNCLOS, and small island developing states whose EEZs are disproportionately affected by climate change and sea-level rise, are often excluded from the primary discourse, though their claims are sometimes voiced by NGOs or specific international bodies.
% DISAPPEARANCE_RATIONALE: If strict EEZ enforcement vanished, there would be an immediate scramble for marine resources, leading to increased conflict, overfishing, and environmental degradation. The global maritime order would fundamentally reorganize around competing claims and power projection.
% FOUNDING_PROBLEM: Unregulated exploitation of marine resources, overlapping and conflicting maritime claims, and the potential for international conflict over access to the oceans' wealth.
% FOUNDING_PROBLEM_CORROBORATION: The problem of resource management and conflict prevention remains live, attested by ongoing disputes and scientific reports on overfishing. However, the 'strict EEZ' solution is contested by states with historical claims and those advocating for a more equitable distribution of marine resources, as evidenced by ongoing diplomatic tensions and legal challenges.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because coastal states gain exclusive control over vast marine resources, denying access to others. Suppression (0.75) is high due to active naval patrols, legal challenges, and diplomatic pressure used to enforce EEZ claims against overlapping claimants and distant-water fleets. Theater ratio (0.20) is relatively low, as enforcement is genuinely aimed at securing resource control, not merely performance. The increasing extractiveness and suppression over time reflect the hardening of EEZ claims and the intensification of enforcement efforts as marine resources become scarcer.
 *
 * PERSPECTIVAL GAP:
 *   Coastal states experience this as a legitimate coordination mechanism for resource management, ensuring their sovereign rights and preventing overexploitation. Overlapping claimants and distant-water fishing fleets experience it as an extractive snare, denying them historical access and livelihoods. The international maritime organizations, while administering the framework, are caught between these competing claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states are clear beneficiaries (d=0.0-0.2) as they gain exclusive resource rights. Overlapping claimants and distant-water fishing fleets are targets (d=0.8-1.0) as they are actively excluded from traditional areas. International maritime organizations are more symmetric (d=0.5) as they coordinate the framework but do not directly extract. Non-ratifying states, while not direct victims of UNCLOS itself, become targets when their alternative claims are suppressed by states enforcing the strict EEZ reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely coordinates resource management (preventing open-access depletion) while simultaneously enabling asymmetric extraction by coastal states. It avoids being a Snare by having a clear, albeit contested, coordination function. It avoids being a Rope because of the high suppression and identifiable victims. The mandatrophy is not yet resolved as the founding problem of resource management is live, but the method of resolution (exclusive EEZs) has become a source of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unclos_ratification_universality,
    'Is UNCLOS Article 57 universally accepted as customary international law, even by non-ratifying states, or does non-ratification create a legitimate basis for alternative claims?',
    'International Court of Justice ruling on a dispute involving a non-ratifying state''s EEZ claim, or a clear shift in state practice by major maritime powers.',
    'If universal, the strict EEZ reading gains Mountain-like force; if not, its enforceability against non-ratifiers is weakened, potentially reclassifying it as a Snare for those states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unclos_ratification_universality, empirical, 'Ambiguity of UNCLOS Article 57''s status as customary international law for non-ratifiers.').

omega_variable(
    historical_rights_vs_unclos_primacy,
    'Does the strict EEZ reading genuinely supersede historical fishing rights and traditional claims, or does it merely suppress them without resolving the underlying legitimacy?',
    'A negotiated settlement or international arbitration that explicitly extinguishes historical rights in favor of UNCLOS EEZ, or persistent, successful challenges to EEZ enforcement based on historical claims.',
    'If historical rights are truly superseded, the strict EEZ reading is more robust; if merely suppressed, it carries a higher latent resistance and extractiveness, potentially shifting it towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_rights_vs_unclos_primacy, conceptual, 'The strict EEZ reading is one interpretation of the unclos_sovereignty_boundary kernel. It asserts that EEZ boundaries, as defined by UNCLOS Article 57 (200-nautical-mile limits), are exclusive and enforceable, and that no overlay claims (such as historical rights or traditional fishing grounds) are valid within these limits. This reading directly conflicts with the historical_rights_reading, which posits that historical usage creates sovereign rights that predate and override UNCLOS provisions. It also influences the non_ratifier_enforcement_reading by asserting the primacy of UNCLOS EEZ limits over general freedom of navigation principles for resource exploitation, even for non-ratifiers, thereby creating pressure on their claims to unrestricted access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(uncl_tr_t5, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(uncl_tr_t10, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(uncl_tr_t15, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(uncl_tr_t20, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(uncl_be_t5, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(uncl_be_t10, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(uncl_be_t15, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(uncl_be_t20, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(uncl_su_t5, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(uncl_su_t10, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(uncl_su_t15, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(uncl_su_t20, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, maritime_resource_exploitation_quotas).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, freedom_of_navigation_operations).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unclos_sovereignty_boundary' kernel. It focuses on the strict interpretation of EEZ limits, which directly impacts other readings concerning historical rights and non-ratifier enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
