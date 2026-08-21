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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS Strict EEZ Exclusivity (200-NM Limit)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the 'strict EEZ reading' of the UNCLOS
 *   sovereignty boundary kernel, asserting that Exclusive Economic Zone
 *   boundaries are exclusive and enforceable per UNCLOS Article 57's
 *   200-nautical-mile limits, with no overlay claims valid. This
 *   interpretation grants coastal states extensive control over marine
 *   resources, while actively suppressing historical claims or alternative
 *   frameworks for maritime access. The constraint is claimed as a Tangled
 *   Rope, reflecting its dual function of coordinating maritime governance
 *   while enabling significant asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.78).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.85).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Strict EEZ Exclusivity (200-NM Limit)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, 'd4698fe8-e9e5-4544-94ea-2ff399dff49b').
narrative_ontology:cs_kernel_codification('d4698fe8-e9e5-4544-94ea-2ff399dff49b', formalized).
narrative_ontology:cs_authority_grounding('d4698fe8-e9e5-4544-94ea-2ff399dff49b', lineage).
narrative_ontology:cs_interpretation_layer_present('d4698fe8-e9e5-4544-94ea-2ff399dff49b').
narrative_ontology:cs_reading_relation('d4698fe8-e9e5-4544-94ea-2ff399dff49b', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('d4698fe8-e9e5-4544-94ea-2ff399dff49b', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('d4698fe8-e9e5-4544-94ea-2ff399dff49b', foundational, unclos_article_57_is_definitive).
narrative_ontology:cs_axiom_status(unclos_article_57_is_definitive, holdable).
narrative_ontology:cs_axiom_grounding('d4698fe8-e9e5-4544-94ea-2ff399dff49b', unclos_article_57_is_definitive, conventional).
narrative_ontology:cs_axiom('d4698fe8-e9e5-4544-94ea-2ff399dff49b', foundational, no_pre_unclos_overrides).
narrative_ontology:cs_axiom_status(no_pre_unclos_overrides, holdable).
narrative_ontology:cs_axiom_grounding('d4698fe8-e9e5-4544-94ea-2ff399dff49b', no_pre_unclos_overrides, conventional).
narrative_ontology:cs_reference_frame('d4698fe8-e9e5-4544-94ea-2ff399dff49b', unclos_legal_framework).
narrative_ontology:cs_drift_state('d4698fe8-e9e5-4544-94ea-2ff399dff49b', contemporary_geopolitical_contestation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d4698fe8-e9e5-4544-94ea-2ff399dff49b', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_nations).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, states_with_overlapping_claims).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, landlocked_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states claim and enforce exclusive rights over resources within their 200-nautical-mile Exclusive Economic Zones, as defined by UNCLOS Article 57. They benefit from exclusive access to fisheries, oil, gas, and other marine resources, and actively suppress any competing claims.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, coastal_states, beneficiary).

% These nations, historically reliant on fishing in areas now designated as EEZs, face exclusion or require costly access agreements. They bear the economic cost of lost fishing grounds and reduced access, often resisting the strict enforcement of EEZ limits.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_nations, payer,
    powerful, biographical, constrained, global).

% States whose geographical proximity leads to overlapping EEZ claims, which are invalidated by a strict interpretation of UNCLOS Article 57. They are forced into bilateral negotiations or international arbitration, often losing historical access or potential resource wealth.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, states_with_overlapping_claims, payer,
    moderate, generational, trapped, regional).

% These states have no coastline and thus no EEZ. They are structurally excluded from the benefits of EEZ resource control and often advocate for more equitable access to marine resources, but their voice is often marginalized in this framework.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, landlocked_states, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, landlocked_states, payer).

% Bodies like the International Tribunal for the Law of the Sea (ITLOS) interpret and apply UNCLOS provisions, including those related to EEZs. They provide a forum for dispute resolution but do not directly set or enforce the boundaries themselves.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_maritime_tribunals, observer,
    institutional, civilizational, analytical, global).

% Major naval powers that have not ratified UNCLOS may assert freedom of navigation principles that challenge the strict exclusivity of EEZs, particularly regarding military activities. While not directly paying, their operations are constrained by this reading, and they are excluded from the UNCLOS-based consensus.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifying_naval_powers, excluded,
    powerful, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, internationally recognized framework for allocating and managing marine resources and jurisdiction, reducing potential conflicts over offshore wealth and promoting conservation within defined zones.
% TRANSFER_FUNCTION: Transfers exclusive economic control and resource benefits (e.g., fisheries, oil, gas, minerals) from the global commons or historical users to coastal states, based on their geographical proximity.
% ABSENT_VOICES: Landlocked states, states with historical fishing rights, and non-ratifying naval powers are largely excluded from the consensus that underpins this strict reading. They would advocate for more equitable resource sharing, recognition of historical usage, or broader freedom of navigation.
% DISAPPEARANCE_RATIONALE: If strict EEZ exclusivity vanished overnight, there would be a rapid return to unregulated competition for marine resources, leading to overexploitation, increased geopolitical tensions, and a collapse of existing maritime governance structures. The global maritime order would fundamentally reorganize.
% FOUNDING_PROBLEM: The 'Tragedy of the Commons' in marine resources, coupled with escalating technological capacity for offshore exploitation and overlapping national claims, led to unsustainable practices and frequent international disputes in the mid-20th century.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, UN reports on ocean governance, and the ongoing disputes brought before ITLOS corroborate the persistent nature of the founding problem. However, states with historical claims or landlocked states often dispute whether the strict EEZ framework is the most equitable or effective solution.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because coastal states gain exclusive control over vast, resource-rich areas, often at the expense of traditional users or landlocked states. Suppression is very high (0.85) due to the active naval and diplomatic enforcement required to maintain these exclusive zones against competing claims and historical practices. Theater ratio is relatively low (0.20) as the enforcement is largely functional, though diplomatic performances of 'international law' often mask the underlying power dynamics. Accessibility collapse is substantial for non-coastal states, and resistance is moderate from those whose access is curtailed.
 *
 * PERSPECTIVAL GAP:
 *   Coastal states perceive this as a legitimate and necessary framework for national resource security and environmental protection. Conversely, excluded or victimized states view it as an extractive mechanism that reallocates global commons for the benefit of a few, enforced by naval power and diplomatic pressure. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states are clear beneficiaries and agenda-setters, gaining exclusive resource rights. Distant water fishing nations, states with overlapping claims, and landlocked states are victims, losing access or potential resources. Non-ratifying naval powers are excluded from the UNCLOS framework but still assert their own principles, leading to ongoing tension. International tribunals act as observers, interpreting the framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unclos_interpretation_ambiguity,
    'Is UNCLOS Article 57''s 200-nautical-mile limit the sole and definitive determinant of maritime sovereignty, or do other principles (e.g., historical rights, freedom of navigation for non-ratifiers) hold co-equal or overriding authority?',
    'Resolution would require a definitive ruling by the International Court of Justice or a new global treaty explicitly clarifying the hierarchy of maritime claims, which is unlikely given current geopolitical dynamics.',
    'If other principles were deemed co-equal or overriding, the extractiveness and suppression of this strict EEZ reading would decrease, potentially reclassifying it towards a Rope or even a Piton if enforcement became untenable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unclos_interpretation_ambiguity, conceptual, 'Ambiguity regarding the definitive authority of UNCLOS Article 57 against other maritime claims.').

omega_variable(
    eez_resource_equity_vs_exclusivity,
    'Does the strict exclusivity of EEZs genuinely promote sustainable resource management and global equity, or does it primarily serve to concentrate wealth and power in coastal states?',
    'Empirical studies comparing resource sustainability and economic development outcomes in regions with strict EEZ enforcement versus those with more cooperative or shared management regimes, alongside a global consensus on ''equity'' metrics.',
    'If found to primarily concentrate wealth without significant sustainability benefits, the ''coordination'' function would be seen as cover, increasing the effective extraction and pushing the classification closer to a Snare. If sustainability and equitable development were clearly demonstrated, it would reinforce the Rope aspects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eez_resource_equity_vs_exclusivity, empirical, 'Whether EEZ exclusivity achieves its stated goals of sustainability and equity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1982, 0.25).
narrative_ontology:measurement(uncl_tr_t1992, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1992, 0.22).
narrative_ontology:measurement(uncl_tr_t2002, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2002, 0.2).
narrative_ontology:measurement(uncl_tr_t2012, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2012, 0.19).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1982, 0.6).
narrative_ontology:measurement(uncl_be_t1992, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1992, 0.68).
narrative_ontology:measurement(uncl_be_t2002, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2002, 0.73).
narrative_ontology:measurement(uncl_be_t2012, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2012, 0.76).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1982, 0.7).
narrative_ontology:measurement(uncl_su_t1992, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1992, 0.75).
narrative_ontology:measurement(uncl_su_t2002, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2002, 0.8).
narrative_ontology:measurement(uncl_su_t2012, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2012, 0.83).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, international_fishing_quotas).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, offshore_oil_gas_exploration_permits).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_sovereignty_boundary' kernel, each representing a distinct structural claim about maritime jurisdiction. This 'strict_eez_reading' emphasizes UNCLOS Article 57's definitive nature, contrasting with historical claims and non-ratifier assertions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
