% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Expansive Sovereignty via Artificial Island Construction (UNCLOS Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint describes the 'expansive construction' reading of UNCLOS
 *   maritime sovereignty, where states assert de facto territorial waters
 *   (12nm territorial sea or broader) around artificial islands built on
 *   submerged features or low-tide elevations. This interpretation leverages
 *   effective occupation and administrative control to generate sovereignty,
 *   despite UNCLOS Article 121(3) stating that 'rocks which cannot sustain
 *   human habitation or economic life of their own shall have no exclusive
 *   economic zone or continental shelf.' This reading extends that logic to
 *   artificial features, creating new maritime zones and restricting freedom
 *   of navigation.
 *
 * KEY AGENTS:
 *   - island_constructing_states: Primary beneficiary (institutional/arbitrage) — gains territorial control and resources.
 *   - neighboring_claimant_states: Primary victim (institutional/constrained) — loses potential maritime zones and faces increased geopolitical tension.
 *   - freedom_of_navigation_states: Victim (institutional/constrained) — faces restrictions on movement and overflight.
 *   - international_shipping: Victim (organized/constrained) — faces new navigational hazards and potential legal challenges.
 *   - international_legal_bodies: Observer (institutional/analytical) — adjudicates disputes and interprets UNCLOS.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.85).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.75).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, snare).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Sovereignty via Artificial Island Construction (UNCLOS Reading)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '55da1e80-fcab-4155-aeb8-15d7b50a5419').
narrative_ontology:cs_kernel_codification('55da1e80-fcab-4155-aeb8-15d7b50a5419', fixed_text).
narrative_ontology:cs_authority_grounding('55da1e80-fcab-4155-aeb8-15d7b50a5419', extraction).
narrative_ontology:cs_interpretation_layer_present('55da1e80-fcab-4155-aeb8-15d7b50a5419').
narrative_ontology:cs_reading_relation('55da1e80-fcab-4155-aeb8-15d7b50a5419', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('55da1e80-fcab-4155-aeb8-15d7b50a5419', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('55da1e80-fcab-4155-aeb8-15d7b50a5419', foundational, effective_occupation_generates_sovereignty).
narrative_ontology:cs_axiom_status(effective_occupation_generates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('55da1e80-fcab-4155-aeb8-15d7b50a5419', effective_occupation_generates_sovereignty, conventional).
narrative_ontology:cs_axiom('55da1e80-fcab-4155-aeb8-15d7b50a5419', foundational, artificial_features_can_generate_maritime_zones).
narrative_ontology:cs_axiom_status(artificial_features_can_generate_maritime_zones, holdable).
narrative_ontology:cs_axiom_grounding('55da1e80-fcab-4155-aeb8-15d7b50a5419', artificial_features_can_generate_maritime_zones, instrumental).
narrative_ontology:cs_reference_frame('55da1e80-fcab-4155-aeb8-15d7b50a5419', unilateral_maritime_expansion).
narrative_ontology:cs_drift_state('55da1e80-fcab-4155-aeb8-15d7b50a5419', contemporary_geopolitical_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('55da1e80-fcab-4155-aeb8-15d7b50a5419', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, international_shipping).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that undertake artificial island construction on submerged features or low-tide elevations, asserting de facto territorial waters and exclusive economic zones. They benefit from expanded territorial control, access to resources, and enhanced geopolitical influence. They actively enforce these claims through naval patrols and administrative presence.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, arbitrage, regional).

% States with overlapping maritime claims or proximity to the constructed features. They bear the costs of lost maritime zones, increased geopolitical tension, and potential military escalation. Their options are diplomatic protest, legal challenge, or military confrontation.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    institutional, generational, constrained, regional).

% States that advocate for and conduct freedom of navigation operations (FONOPs) to challenge excessive maritime claims. They bear the costs of maintaining naval presence and diplomatic friction. Their goal is to prevent the normalization of expansive claims that restrict international waters.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    institutional, generational, constrained, global).

% Commercial and private vessels that transit through or near the claimed maritime zones. They face increased navigational hazards, potential harassment, and the risk of legal challenges or detention if they do not comply with the constructing state's regulations. Their options are to reroute (costly) or comply (risky).
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_shipping, payer,
    organized, immediate, constrained, global).

% Organizations like the International Tribunal for the Law of the Sea (ITLOS) and the International Court of Justice (ICJ) that interpret UNCLOS and adjudicate maritime disputes. They provide legal opinions and rulings but lack direct enforcement power over sovereign states.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the perspective of the constructing state, it coordinates the assertion of national sovereignty and control over maritime space, ensuring security and resource exploitation within its claimed territorial limits.
% TRANSFER_FUNCTION: Transfers territorial control, access to natural resources (e.g., fisheries, hydrocarbons), and strategic military advantage from the international community and neighboring states to the island-constructing state.
% ABSENT_VOICES: The 'international community' as a collective, particularly those states without the capacity or geopolitical will to challenge these claims, are effectively absent from the direct negotiation. Their interests in freedom of navigation and shared maritime resources are suppressed by the unilateral actions of constructing states.
% DISAPPEARANCE_RATIONALE: If the claims to territorial waters from artificial islands vanished overnight, the geopolitical landscape of the affected regions would immediately shift. Neighboring states would reassert their own claims, freedom of navigation would expand, and the constructing states would lose significant strategic and economic advantages, leading to a major reorganization of maritime governance and power dynamics.
% FOUNDING_PROBLEM: The constructing states frame the problem as a need to secure national interests, protect maritime borders, and exploit resources in contested or previously unclaimed areas, often citing historical grievances or perceived threats.
% FOUNDING_PROBLEM_CORROBORATION: The constructing states' claims are primarily self-attested, supported by their domestic legal interpretations and strategic narratives. Neighboring claimant states, freedom-of-navigation states, and many international legal scholars dispute the legitimacy of these claims, citing UNCLOS and customary international law. Independent legal analysis and diplomatic protests from outside the benefiting parties corroborate the view that the 'founding problem' is a pretext for expansion.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading allows states to unilaterally claim vast maritime areas and resources that would otherwise be international waters. Suppression (0.75) is also high, as these claims are often enforced through naval presence, administrative control, and the implicit threat of force, effectively suppressing challenges to the de facto sovereignty. The theater ratio (0.2) is low because the construction and enforcement are genuinely aimed at establishing control, not merely performance. Accessibility collapse (0.6) is moderate as alternative interpretations exist, but the physical presence of artificial islands creates a strong de facto reality. Resistance (0.7) is high due to diplomatic protests, freedom of navigation operations, and legal challenges from other states.
 *
 * PERSPECTIVAL GAP:
 *   Island-constructing states perceive this as a legitimate exercise of sovereignty and a necessary measure for national security and resource acquisition. Neighboring states and freedom-of-navigation states perceive it as an illegal land grab and a violation of international law, leading to increased regional instability and restricted access to international waters. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Island-constructing states are full beneficiaries (d=0.0) as they gain territorial control and resources. Neighboring claimant states and freedom-of-navigation states are full targets (d=1.0) as they lose maritime zones and face restrictions. International shipping is also a target (d=0.8) due to increased costs and risks. International legal bodies are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Snare because its coordination story (e.g., 'securing national interests,' 'protecting maritime resources') is a cover for unilateral extraction of territorial control and resources. Its persistence depends on active enforcement and the suppression of alternative interpretations and challenges. There are clear victims who bear the costs of this expansive claim. The classification prevents mislabeling this as a legitimate coordination mechanism by highlighting the asymmetric extraction and coercive enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unclos_interpretation_ambiguity,
    'Is this constraint a legitimate interpretation of UNCLOS Article 121, or an opportunistic expansion of territorial claims?',
    'Adjudication by the International Tribunal for the Law of the Sea (ITLOS) or the International Court of Justice (ICJ) on a specific case, or a new UNCLOS amendment clarifying the status of artificial features.',
    'If deemed opportunistic, the constraint''s legitimacy collapses, and its effective extraction would be reclassified as pure coercion. If legitimized, it would shift towards a Tangled Rope, acknowledging a coordination function for the constructing state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unclos_interpretation_ambiguity, conceptual, 'Ambiguity in UNCLOS interpretation regarding artificial features.').

omega_variable(
    sibling_reading_impact_strict_geographic,
    'How would the adoption of the ''strict_geographic_reading'' alter the structural properties of this ''expansive_construction_reading''?',
    'A shift in international legal consensus or a binding judicial ruling affirming the strict geographic interpretation.',
    'The ''strict_geographic_reading'' would directly foreclose this ''expansive_construction_reading'', rendering its claims to territorial waters from artificial features null and void, collapsing its extractiveness and suppression to zero.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_strict_geographic, conceptual, 'Impact of strict geographic reading on expansive construction claims.').

omega_variable(
    sibling_reading_impact_hybrid_effective_control,
    'How would the adoption of the ''hybrid_effective_control_reading'' alter the structural properties of this ''expansive_construction_reading''?',
    'A shift in international legal consensus or a binding judicial ruling affirming the hybrid effective control interpretation.',
    'The ''hybrid_effective_control_reading'' would significantly reduce the scope of this ''expansive_construction_reading'', limiting territorial claims from artificial features to safety zones unless prolonged, unchallenged effective control is demonstrated, thereby reducing its extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_hybrid_effective_control, conceptual, 'Impact of hybrid effective control reading on expansive construction claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(uncl_be_t5, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(uncl_be_t15, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(uncl_su_t5, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(uncl_su_t15, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_operations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_maritime_sovereignty' kernel. This 'expansive_construction_reading' directly influences and is influenced by the 'strict_geographic_reading' and 'hybrid_effective_control_reading' as they represent competing interpretations of international maritime law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
