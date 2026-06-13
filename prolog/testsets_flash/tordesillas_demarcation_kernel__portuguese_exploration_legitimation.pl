% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__portuguese_exploration_legitimation, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Portuguese Exploration Legitimation via Treaty of Tordesillas
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This constraint represents the Portuguese reading of the Treaty of
 *   Tordesillas, which legitimized their prior exploration rights and
 *   established an exclusive zone for their expansion and trade east of the
 *   demarcation line, effectively excluding other European rivals. It is a
 *   'tangled rope' because it provided a coordination mechanism between
 *   Portugal and Spain, but simultaneously extracted from rival European
 *   powers and, by extension, from indigenous populations whose sovereignty
 *   was ignored. This reading emphasizes the inter-European dimension of
 *   exclusion and trade monopoly, rather than the direct territorial conquest
 *   and subjugation of indigenous peoples, which is more central to the
 *   Spanish reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.65).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.75).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.65).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Portuguese Exploration Legitimation via Treaty of Tordesillas").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '6a90e855-60d6-40b4-8627-3d288dc27f25').
narrative_ontology:cs_kernel_codification('6a90e855-60d6-40b4-8627-3d288dc27f25', formalized).
narrative_ontology:cs_authority_grounding('6a90e855-60d6-40b4-8627-3d288dc27f25', lineage).
narrative_ontology:cs_interpretation_layer_present('6a90e855-60d6-40b4-8627-3d288dc27f25').
narrative_ontology:cs_reading_relation('6a90e855-60d6-40b4-8627-3d288dc27f25', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('6a90e855-60d6-40b4-8627-3d288dc27f25', foundational, prior_discovery_grants_exclusive_rights).
narrative_ontology:cs_axiom_status(prior_discovery_grants_exclusive_rights, holdable).
narrative_ontology:cs_axiom_grounding('6a90e855-60d6-40b4-8627-3d288dc27f25', prior_discovery_grants_exclusive_rights, conventional).
narrative_ontology:cs_axiom('6a90e855-60d6-40b4-8627-3d288dc27f25', foundational, papal_bull_confirms_temporal_claims).
narrative_ontology:cs_axiom_status(papal_bull_confirms_temporal_claims, holdable).
narrative_ontology:cs_axiom_grounding('6a90e855-60d6-40b4-8627-3d288dc27f25', papal_bull_confirms_temporal_claims, theological).
narrative_ontology:cs_reference_frame('6a90e855-60d6-40b4-8627-3d288dc27f25', portuguese_maritime_hegemony).
narrative_ontology:cs_drift_state('6a90e855-60d6-40b4-8627-3d288dc27f25', post_westphalian_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('6a90e855-60d6-40b4-8627-3d288dc27f25', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_merchants).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_authority_in_temporal_affairs).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, first_discovery_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiary and enforcer of the treaty, using it to legitimize its claims to trade routes and territories east of the demarcation line, excluding other European powers. It actively dispatched fleets to defend these claims.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% European states (e.g., England, France, Netherlands) that were excluded from direct trade and exploration in the Portuguese-claimed territories. They faced diplomatic pressure, naval interdiction, and the legal precedent set by the treaty, forcing them to seek alternative routes or challenge the claims through force.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers, payer,
    powerful, generational, constrained, global).

% The Papacy's authority in arbitrating territorial disputes and legitimizing Christian expansion was reinforced by the treaty, even if its direct temporal power was waning. It benefited from its role as a moral and legal arbiter.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy, beneficiary,
    institutional, civilizational, analytical, universal).

% Merchants and trading companies operating under the Portuguese Crown benefited from the monopolistic control over lucrative trade routes (e.g., spice trade) and resources in the East, protected from direct European competition by the treaty.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_merchants, beneficiary,
    organized, biographical, mobile, global).

% The inhabitants of the territories claimed by Portugal were not consulted or recognized in the treaty. Their sovereignty was disregarded, and their lands and resources became objects of European competition, though this reading focuses on inter-European exclusion rather than direct indigenous subjugation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_populations_east, excluded,
    powerless, generational, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It aimed to coordinate the exploration and colonial expansion efforts of Portugal and Spain, preventing direct conflict between the two major Catholic maritime powers over newly discovered lands and trade routes.
% TRANSFER_FUNCTION: It transferred the exclusive right to explore and exploit territories east of the demarcation line to Portugal, and west to Spain, effectively transferring potential wealth and sovereignty from unrepresented indigenous populations to the signatory powers, and from rival European powers to Portugal and Spain.
% ABSENT_VOICES: Indigenous populations of the claimed territories were entirely absent from the negotiations and had no voice in the allocation of their lands. Other European maritime powers (e.g., England, France, Netherlands) were also excluded and would have objected to the division of the world between two powers.
% DISAPPEARANCE_RATIONALE: If the treaty and its associated claims vanished overnight, the historical trajectory of colonial expansion and international law would be fundamentally altered. The scramble for eastern trade routes would have been far more open and contested among all European powers from an earlier date, potentially leading to different colonial outcomes and power balances.
% FOUNDING_PROBLEM: The immediate problem was to prevent armed conflict between Portugal and Spain over the 'discovery' of new lands and sea routes, particularly after Columbus's voyages, which threatened to ignite a major European war.
% FOUNDING_PROBLEM_CORROBORATION: While the immediate problem of Iberian conflict was resolved for a time, the broader problem of legitimizing colonial claims and preventing inter-European conflict over global resources persisted and evolved. The treaty's authority was eventually challenged and superseded by other forms of international law and power dynamics. Historians and international legal scholars outside of Portuguese or Spanish national narratives widely corroborate that the specific problem of Iberian conflict was resolved, but the treaty's broader claims became obsolete as other powers rose.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the treaty granted Portugal a de facto monopoly over vast, resource-rich territories and trade routes, allowing them to dictate terms and accumulate wealth without competition. Suppression is also high (0.75) as Portugal actively enforced its claims through naval power and diplomatic pressure against other European nations. Theater ratio is moderate (0.20) as the treaty's legitimacy was increasingly challenged by other European powers who disregarded papal authority, making its enforcement more about raw power projection than legal consensus over time. The measurements show extractiveness and suppression peaking as Portuguese power consolidated, then gradually declining as other European powers challenged the treaty's legitimacy and enforcement became more costly.
 *
 * PERSPECTIVAL GAP:
 *   From the Portuguese perspective, the treaty was a legitimate and necessary coordination mechanism to secure their discoveries and prevent conflict. From the perspective of rival European powers, it was an arbitrary and extractive imposition. The engine's classification will reflect this divergence, likely showing a 'rope' or 'scaffold' for the Portuguese seat (coordination, temporary support for expansion) and a 'snare' or 'tangled rope' for the excluded European powers (pure extraction, enforced exclusion).
 *
 * DIRECTIONALITY LOGIC:
 *   The Portuguese Crown and its merchants are clear beneficiaries, gaining exclusive access and monopolistic profits. Rival European powers are the primary victims, being excluded from lucrative trade and exploration. The Papacy benefits from the reinforcement of its temporal authority, even if indirectly. Indigenous populations are excluded, their rights entirely unacknowledged by the treaty, making them indirect victims of the broader colonial project legitimized by such agreements.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy,
    'To what extent did other European powers genuinely recognize the Papacy''s authority to divide the world, versus using it as a convenient legal fiction?',
    'Analysis of diplomatic correspondence and legal challenges from non-Iberian powers; examination of instances where the treaty was openly defied without significant religious or political backlash.',
    'If recognition was low, the treaty''s ''coordination'' function was weaker, and its ''suppression'' of rivals relied more on raw military power than legal consensus, pushing it closer to a pure ''snare'' for excluded parties. If high, the coordination aspect was more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_legitimacy, empirical, 'The actual legitimacy of papal authority in international law for non-Iberian powers.').

omega_variable(
    indigenous_sovereignty_recognition,
    'How would the classification of this constraint change if indigenous sovereignty over the claimed territories was recognized as a foundational premise?',
    'Counterfactual legal analysis: re-evaluate the treaty''s legitimacy and beneficiary/victim structure from a framework that prioritizes indigenous land rights and self-determination.',
    'If indigenous sovereignty were recognized, the constraint would shift dramatically towards a ''snare'' or ''tangled rope'' for indigenous populations, with the Portuguese Crown becoming a clear ''agenda_setter'' of extraction from them, rather than merely a beneficiary of inter-European coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_sovereignty_recognition, conceptual, 'Impact of recognizing indigenous sovereignty on the constraint''s classification.').

omega_variable(
    inter_european_vs_indigenous_extraction,
    'Is the primary extraction of this constraint from rival European powers (trade monopoly) or from indigenous populations (land/resource appropriation)?',
    'Quantitative analysis of wealth flows: compare the economic value extracted from inter-European trade monopolies versus direct resource extraction from indigenous lands.',
    'If indigenous extraction is primary, this reading''s focus on inter-European rivalry is a misdirection, and the constraint is a more severe ''snare'' for indigenous peoples. If inter-European extraction is primary, this reading''s focus is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_european_vs_indigenous_extraction, empirical, 'Determining the primary target of extraction: rival European powers or indigenous populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.1).
narrative_ontology:measurement(tord_tr_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1550, 0.15).
narrative_ontology:measurement(tord_tr_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(tord_tr_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1650, 0.25).
narrative_ontology:measurement(tord_tr_t1700, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1700, 0.3).
narrative_ontology:measurement(tord_tr_t1750, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1750, 0.35).
narrative_ontology:measurement(tord_tr_t1800, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1800, 0.4).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.55).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1550, 0.6).
narrative_ontology:measurement(tord_be_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1650, 0.68).
narrative_ontology:measurement(tord_be_t1700, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1700, 0.65).
narrative_ontology:measurement(tord_be_t1750, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1750, 0.6).
narrative_ontology:measurement(tord_be_t1800, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1800, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.65).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1550, 0.7).
narrative_ontology:measurement(tord_su_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1600, 0.75).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1650, 0.7).
narrative_ontology:measurement(tord_su_t1700, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1700, 0.65).
narrative_ontology:measurement(tord_su_t1750, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1750, 0.6).
narrative_ontology:measurement(tord_su_t1800, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1800, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.1).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'tordesillas_demarcation_kernel'. It focuses on the Portuguese claim to eastern exploration and trade rights, excluding other European powers. The 'spanish_conquest_legitimation' reading focuses on the Spanish claim to western territorial conquest and indigenous subjugation. Both are distinct constraints arising from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
