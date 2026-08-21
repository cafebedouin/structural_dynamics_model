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
 *   This constraint represents the Portuguese reading of the Treaty of
 *   Tordesillas, focusing on its role in legitimizing Portuguese exploration
 *   rights and establishing a trade monopoly in the eastern hemisphere,
 *   excluding rival European powers. It is a Tangled Rope because it provided
 *   a coordination mechanism for Iberian powers while simultaneously
 *   extracting resources and sovereignty from indigenous populations and
 *   excluding other European nations. The extractiveness and suppression
 *   metrics reflect the active enforcement required to maintain this division
 *   and the costs borne by those excluded or subjugated.
 *
 * KEY AGENTS:
 *   - portuguese_crown: Agenda setter (institutional/constrained)
 *   - portuguese_estado_da_india: Beneficiary (institutional/constrained)
 *   - rival_european_powers: Payer (powerful/constrained)
 *   - indigenous_populations_east_of_line: Payer (powerless/trapped)
 *   - papacy: Observer (institutional/analytical)
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
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'e983e29c-f2cc-4df6-912d-115e47cd514c').
narrative_ontology:cs_kernel_codification('e983e29c-f2cc-4df6-912d-115e47cd514c', formalized).
narrative_ontology:cs_authority_grounding('e983e29c-f2cc-4df6-912d-115e47cd514c', lineage).
narrative_ontology:cs_interpretation_layer_present('e983e29c-f2cc-4df6-912d-115e47cd514c').
narrative_ontology:cs_reading_relation('e983e29c-f2cc-4df6-912d-115e47cd514c', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('e983e29c-f2cc-4df6-912d-115e47cd514c', foundational, prior_discovery_rights).
narrative_ontology:cs_axiom_status(prior_discovery_rights, holdable).
narrative_ontology:cs_axiom_grounding('e983e29c-f2cc-4df6-912d-115e47cd514c', prior_discovery_rights, conventional).
narrative_ontology:cs_axiom('e983e29c-f2cc-4df6-912d-115e47cd514c', foundational, exclusive_trade_monopoly).
narrative_ontology:cs_axiom_status(exclusive_trade_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('e983e29c-f2cc-4df6-912d-115e47cd514c', exclusive_trade_monopoly, instrumental).
narrative_ontology:cs_reference_frame('e983e29c-f2cc-4df6-912d-115e47cd514c', papally_sanctioned_portuguese_dominance).
narrative_ontology:cs_drift_state('e983e29c-f2cc-4df6-912d-115e47cd514c', post_enlightenment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e983e29c-f2cc-4df6-912d-115e47cd514c', '').
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

% The administrative and military arm of the Portuguese colonial empire, directly benefiting from the treaty's legitimation of its trade routes and fortified outposts in the East. It enforces the exclusion of rivals through naval power and diplomatic pressure.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india, beneficiary,
    institutional, generational, constrained, global).

% European nations (e.g., England, France, Netherlands) that were excluded from direct trade and exploration in the Portuguese-claimed eastern hemisphere. They bore the cost of needing to find alternative routes or challenge Portuguese claims, often leading to conflict.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers, payer,
    powerful, biographical, constrained, global).

% The native peoples and polities in the territories claimed by Portugal. While not the primary target of the treaty's *demarcation* (which was between European powers), they bore the ultimate cost of European colonization, resource extraction, and loss of sovereignty, legitimized by the treaty's underlying assumptions.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_populations_east_of_line, payer,
    powerless, generational, trapped, local).

% The religious authority that brokered and sanctioned the treaty. While not directly benefiting from the territorial claims, its moral and spiritual authority was leveraged to provide legitimacy to the European powers' divisions of the non-European world.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy, observer,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent open warfare between Portugal and Spain over newly discovered lands and trade routes by establishing a clear, Papally-sanctioned line of demarcation for their respective spheres of influence.
% TRANSFER_FUNCTION: Transfers exclusive rights to exploration, trade, and ultimately sovereignty over vast non-European territories from unrepresented indigenous populations to the Portuguese Crown, and from rival European powers to Portugal within its designated sphere.
% ABSENT_VOICES: Indigenous populations of the eastern hemisphere were entirely absent from the negotiation and would have objected to the premise of European powers dividing their lands. Other European maritime powers (e.g., England, France, Netherlands) were also excluded and later challenged the treaty's legitimacy.
% DISAPPEARANCE_RATIONALE: If the treaty and its underlying papal authority had vanished, the scramble for colonial territories would have been far more chaotic and immediate, likely leading to earlier and more widespread direct conflicts between European powers over eastern trade routes and lands, rather than a period of Portuguese dominance.
% FOUNDING_PROBLEM: Escalating tensions and potential conflict between the burgeoning maritime powers of Portugal and Spain over the rights to newly discovered lands and sea routes following Columbus's voyages.
% FOUNDING_PROBLEM_CORROBORATION: Historians widely corroborate that the immediate problem of Iberian conflict was resolved. However, the treaty's underlying premise of European rights to divide non-European lands is now universally rejected by international law and post-colonial scholarship, rendering the 'founding problem' as framed by the signatories obsolete and morally indefensible.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) is moderate-high, reflecting the significant wealth generated for Portugal through its enforced trade monopolies and colonial ventures. Suppression (0.70) is high due to the active naval and military enforcement required to deter rival European powers and subjugate indigenous populations. Theater ratio (0.20) is low, as the treaty's function was largely effective in its early centuries, though its moral legitimacy was always contested. The decline in extractiveness and suppression towards the end of the interval reflects the weakening of Portuguese power and the rise of other European colonial empires.
 *
 * PERSPECTIVAL GAP:
 *   The Portuguese Crown and Estado da Índia experienced this as a legitimate, beneficial coordination mechanism that secured their global ambitions. Rival European powers and indigenous populations experienced it as an illegitimate, extractive imposition. The Papacy, as an observer, viewed it as a necessary act of Christian arbitration. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The Portuguese Crown and Estado da Índia are clear beneficiaries, with their directionality skewed towards 0.0. Rival European powers and indigenous populations are targets, with directionality closer to 1.0, reflecting the costs and lack of exit options. The Papacy, as an arbiter, sits closer to 0.5, as it gained moral authority but no direct material benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing Iberian conflict) was largely resolved, but the structure persisted and evolved into a mechanism for colonial extraction. The classification as Tangled Rope prevents mislabeling it as pure coordination, highlighting the asymmetric extraction inherent in its operation even as its original coordination function atrophied. The 'dead' status of the founding problem, coupled with 'world_rearranges' for disappearance, signals a zombie-like persistence for extractive purposes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_legitimacy_source,
    'Is the treaty''s legitimacy derived from Papal authority, prior discovery, or effective occupation?',
    'Analysis of contemporary international legal theory and diplomatic correspondence from non-Iberian powers. If non-Iberian powers consistently rejected Papal authority, it points to effective occupation as the de facto source of legitimacy.',
    'If legitimacy is purely Papal, the constraint is more fragile to shifts in religious authority. If it''s effective occupation, it''s a more robust, self-enforcing (and potentially more violent) constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_legitimacy_source, conceptual, 'Source of the treaty''s perceived legitimacy.').

omega_variable(
    indigenous_sovereignty_recognition,
    'To what extent did the treaty''s European signatories acknowledge or deny the pre-existing sovereignty of indigenous populations?',
    'Examination of diplomatic records, legal treatises, and colonial charters for explicit or implicit recognition of indigenous land rights or political autonomy.',
    'If indigenous sovereignty was explicitly denied, the constraint''s extractiveness and suppression are higher than measured, as it actively erased existing political orders. If implicitly denied, the erasure was a consequence rather than a direct aim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_sovereignty_recognition, empirical, 'Recognition of indigenous sovereignty by European powers.').

omega_variable(
    kernel_reading_divergence,
    'What are the precise structural differences between the Portuguese and Spanish readings of the Tordesillas Demarcation Kernel?',
    'Comparative analysis of colonial policies, legal justifications, and historical outcomes in the Portuguese (eastern) vs. Spanish (western) spheres, focusing on the primary mode of interaction with indigenous populations (trade vs. conquest) and the nature of European rivalry.',
    'If the differences are primarily in the *mode* of extraction (trade monopoly vs. territorial conquest), the kernel is a single, highly extractive constraint with different regional manifestations. If the differences are in the *justification* for extraction, they are distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Distinguishing the Portuguese exploration legitimation from the Spanish conquest legitimation.').


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
narrative_ontology:measurement(tord_tr_t1822, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1822, 0.3).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.55).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1550, 0.6).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1650, 0.68).
narrative_ontology:measurement(tord_be_t1750, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1750, 0.72).
narrative_ontology:measurement(tord_be_t1822, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1822, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.6).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1550, 0.65).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1650, 0.75).
narrative_ontology:measurement(tord_su_t1750, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1750, 0.7).
narrative_ontology:measurement(tord_su_t1822, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1822, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
