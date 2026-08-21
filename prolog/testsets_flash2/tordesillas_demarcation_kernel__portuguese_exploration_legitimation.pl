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
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Treaty of Tordesillas: Portuguese Exploration Legitimation
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This constraint represents the Treaty of Tordesillas as a mechanism
 *   primarily legitimizing Portuguese exploration rights and excluding rival
 *   European powers from its eastern sphere of influence. The Papal treaty
 *   confirmed prior Portuguese claims and provided a legal basis for their
 *   trade monopolies and colonial expansion in Africa and Asia. While it
 *   served a coordination function between Spain and Portugal, it
 *   simultaneously extracted heavily from rival European powers and
 *   indigenous populations through enforced exclusivity and territorial
 *   claims. The claimed type is 'tangled_rope' because it genuinely
 *   coordinated between two major powers while enabling asymmetric extraction
 *   from others.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.65).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.7).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.65).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Treaty of Tordesillas: Portuguese Exploration Legitimation").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'e6a664ab-8b6d-4446-ad4a-f66b62be101a').
narrative_ontology:cs_kernel_codification('e6a664ab-8b6d-4446-ad4a-f66b62be101a', formalized).
narrative_ontology:cs_authority_grounding('e6a664ab-8b6d-4446-ad4a-f66b62be101a', lineage).
narrative_ontology:cs_interpretation_layer_present('e6a664ab-8b6d-4446-ad4a-f66b62be101a').
narrative_ontology:cs_reading_relation('e6a664ab-8b6d-4446-ad4a-f66b62be101a', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('e6a664ab-8b6d-4446-ad4a-f66b62be101a', foundational, prior_discovery_grants_exclusive_rights).
narrative_ontology:cs_axiom_status(prior_discovery_grants_exclusive_rights, holdable).
narrative_ontology:cs_axiom_grounding('e6a664ab-8b6d-4446-ad4a-f66b62be101a', prior_discovery_grants_exclusive_rights, conventional).
narrative_ontology:cs_axiom('e6a664ab-8b6d-4446-ad4a-f66b62be101a', foundational, papal_bull_confirms_temporal_claims).
narrative_ontology:cs_axiom_status(papal_bull_confirms_temporal_claims, holdable).
narrative_ontology:cs_axiom_grounding('e6a664ab-8b6d-4446-ad4a-f66b62be101a', papal_bull_confirms_temporal_claims, theological).
narrative_ontology:cs_reference_frame('e6a664ab-8b6d-4446-ad4a-f66b62be101a', papal_sanctioned_maritime_dominion).
narrative_ontology:cs_drift_state('e6a664ab-8b6d-4446-ad4a-f66b62be101a', post_westphalian_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('e6a664ab-8b6d-4446-ad4a-f66b62be101a', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_populations_east_of_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiary and enforcer of the treaty's eastern demarcation, using it to legitimize its prior claims and future explorations, particularly in Africa and Asia. It actively defends its trade monopolies and territorial claims against European rivals.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, constrained, global).

% The administrative and military arm of the Portuguese colonial empire, directly benefiting from the exclusive trade routes and fortified outposts legitimized by the treaty. Its operations are funded by the extraction of resources and control of trade.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india, beneficiary,
    institutional, generational, constrained, global).

% European nations (e.g., England, France, Netherlands) that were excluded from direct trade and exploration in the Portuguese-claimed eastern territories. They bore the cost of seeking alternative routes or engaging in piracy and later direct conflict to challenge the demarcation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers, payer,
    powerful, generational, constrained, global).

% The native peoples and polities in the territories claimed by Portugal. They were not consulted in the treaty and bore the costs of Portuguese colonization, resource extraction, and imposition of trade monopolies, often through violence and subjugation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_populations_east_of_line, payer,
    powerless, generational, trapped, local).

% The religious authority that brokered and legitimized the treaty. While not directly benefiting from the colonial extraction, its moral authority was invoked to sanction the division of the non-Christian world, reinforcing its own geopolitical influence.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy, observer,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aimed to prevent open warfare between Portugal and Spain over newly discovered lands by establishing a clear line of demarcation for their respective spheres of influence, thereby coordinating their colonial expansion efforts.
% TRANSFER_FUNCTION: Transferred perceived rights to exploration, trade, and sovereignty over non-European lands and peoples from a diffuse 'unclaimed' status to the exclusive control of the Portuguese Crown east of the demarcation line, enabling the extraction of resources and establishment of trade monopolies.
% ABSENT_VOICES: Indigenous populations of the claimed territories were entirely absent from the negotiations and would have objected to the fundamental premise of European powers claiming their lands and sovereignty. Other European powers not party to the treaty (e.g., England, France) were also excluded and later challenged its legitimacy.
% DISAPPEARANCE_RATIONALE: If the treaty and its underlying papal authority had vanished, the initial scramble for eastern trade routes and territories would have been far more chaotic, likely leading to earlier and more widespread direct conflict between all European maritime powers, fundamentally altering the course of colonial history and the distribution of global power.
% FOUNDING_PROBLEM: The escalating rivalry and potential for conflict between the burgeoning maritime powers of Portugal and Spain over the rights to newly discovered lands and trade routes, particularly after Columbus's voyages.
% FOUNDING_PROBLEM_CORROBORATION: Historians widely corroborate that the immediate problem of Spanish-Portuguese conflict was resolved. However, the broader problem of legitimizing colonial claims and preventing inter-European conflict over non-European lands persisted and evolved, leading to new treaties and conflicts. The Papacy's role in legitimizing such claims is no longer widely accepted by international law scholars outside of specific historical contexts.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the treaty granted exclusive rights to vast, resource-rich territories, enabling Portugal to establish highly profitable trade monopolies. Suppression is also high, as Portugal actively enforced its claims through naval power and military outposts, suppressing rival European attempts to enter its sphere and subjugating indigenous resistance. The theater ratio is moderate, reflecting that while the papal authority provided a ceremonial and diplomatic cover, the real enforcement was military and economic. The decline in extractiveness and suppression towards the end of the interval reflects the rise of other European powers and the eventual independence movements in former colonies.
 *
 * PERSPECTIVAL GAP:
 *   From the Portuguese perspective, the treaty was a legitimate instrument of international law, confirming their prior discoveries and preventing conflict. From the perspective of rival European powers, it was an arbitrary division of the world that denied them access. From the indigenous perspective, it was an illegitimate imposition of foreign sovereignty. The engine's classification will reflect these divergent experiences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Portuguese Crown and Estado da Índia are clear beneficiaries, gaining exclusive access and trade monopolies. Rival European powers are payers, forced to seek alternative routes or challenge the treaty's legitimacy. Indigenous populations are also payers, bearing the direct costs of colonization and resource extraction. The Papacy acts as an observer, lending moral and legal authority without direct material benefit from the colonial enterprise itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy,
    'To what extent did the Papacy''s spiritual authority genuinely legitimize the territorial claims in the eyes of all European powers, versus merely providing a convenient legal fiction?',
    'Analysis of diplomatic correspondence and legal challenges from non-Iberian European powers, and the extent to which they formally acknowledged or actively defied papal bulls regarding colonial claims.',
    'If papal authority was largely a legal fiction, the constraint''s suppression and legitimacy were more dependent on raw military power, potentially reclassifying it closer to a Snare for rival European powers. If widely accepted, it reinforces the Tangled Rope''s coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_legitimacy, conceptual, 'The true weight of papal authority in legitimizing colonial claims.').

omega_variable(
    indigenous_sovereignty_recognition,
    'How would the classification change if indigenous populations'' pre-existing sovereignty and property rights were recognized as foundational to the international legal order of the time?',
    'Counterfactual legal analysis based on alternative historical developments in international law that would have recognized indigenous sovereignty. This is a conceptual re-framing.',
    'If indigenous sovereignty were recognized, the treaty would be reclassified as a pure Snare, as its entire premise of dividing ''unclaimed'' lands would be illegitimate, and its extractiveness from indigenous populations would be seen as pure theft, not merely a cost of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_sovereignty_recognition, preference, 'Impact of recognizing indigenous sovereignty on the treaty''s classification.').

omega_variable(
    trade_monopoly_vs_territorial_conquest,
    'What was the primary extractive mechanism for Portugal: trade monopoly and resource extraction, or direct territorial conquest and settlement?',
    'Quantitative historical analysis of Portuguese colonial revenue streams, comparing profits from trade and resource extraction with those from direct land-based exploitation and settlement.',
    'If trade monopoly was dominant, the current classification as a Tangled Rope focused on trade exclusion is reinforced. If territorial conquest was primary, the constraint might lean more towards a Snare, with a stronger emphasis on land and labor extraction from indigenous populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trade_monopoly_vs_territorial_conquest, empirical, 'Distinguishing primary extractive mechanism for Portugal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1822).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.1).
narrative_ontology:measurement(tord_tr_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1550, 0.15).
narrative_ontology:measurement(tord_tr_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1650, 0.2).
narrative_ontology:measurement(tord_tr_t1750, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1750, 0.25).
narrative_ontology:measurement(tord_tr_t1822, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1822, 0.2).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.5).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1550, 0.6).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1650, 0.68).
narrative_ontology:measurement(tord_be_t1750, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1750, 0.72).
narrative_ontology:measurement(tord_be_t1822, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1822, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.55).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1550, 0.65).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1650, 0.75).
narrative_ontology:measurement(tord_su_t1750, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1750, 0.8).
narrative_ontology:measurement(tord_su_t1822, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1822, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
