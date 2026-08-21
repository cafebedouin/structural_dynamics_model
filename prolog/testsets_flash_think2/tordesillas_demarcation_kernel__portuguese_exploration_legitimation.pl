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
 *   The Treaty of Tordesillas, mediated by the Papacy in 1494, established a
 *   demarcation line dividing newly discovered lands and trade routes between
 *   Spain and Portugal. This constraint story instantiates the Portuguese
 *   reading, which interpreted the treaty as a confirmation of prior
 *   exploration rights and a legitimation of their exclusive trade monopolies
 *   and colonial claims east of the line, primarily targeting rival European
 *   powers. It was a Tangled Rope, providing coordination between Spain and
 *   Portugal while extracting heavily from other European nations and,
 *   implicitly, from indigenous populations.
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
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Treaty of Tordesillas: Portuguese Exploration Legitimation").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef').
narrative_ontology:cs_kernel_codification('0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef', formalized).
narrative_ontology:cs_authority_grounding('0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef', lineage).
narrative_ontology:cs_interpretation_layer_present('0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef').
narrative_ontology:cs_reading_relation('0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef', foundational, papal_authority_to_grant_rights).
narrative_ontology:cs_axiom_status(papal_authority_to_grant_rights, holdable).
narrative_ontology:cs_axiom_grounding('0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef', papal_authority_to_grant_rights, theological).
narrative_ontology:cs_axiom('0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef', foundational, prior_discovery_confers_legitimacy).
narrative_ontology:cs_axiom_status(prior_discovery_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef', prior_discovery_confers_legitimacy, conventional).
narrative_ontology:cs_reference_frame('0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef', papal_mediated_global_division).
narrative_ontology:cs_drift_state('0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef', rise_of_protestant_powers, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0eb702db-bc5b-4fa1-a2d1-aaaf9cc0a6ef', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_authority_in_international_law).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, doctrine_of_prior_discovery).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiary and enforcer of the treaty's terms east of the demarcation line. It leveraged papal authority to legitimize its claims to trade monopolies and exploration rights, actively defending these against rivals through naval power and diplomacy.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% The administrative and military arm of the Portuguese colonial empire in Asia and Africa, directly profiting from the trade monopolies and exclusive access to resources granted by the treaty. Its operations were directly enabled and protected by the demarcation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india, beneficiary,
    institutional, biographical, mobile, global).

% European states like England, France, and the Netherlands who rejected the Papal authority to divide the world. They bore the cost of exclusion from lucrative trade routes and territories, often resorting to piracy or direct military challenge to contest the demarcation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers, payer,
    powerful, biographical, constrained, global).

% The ultimate authority invoked to legitimize the demarcation line. While not directly extracting material wealth, its spiritual and political authority was used to mediate disputes and grant exclusive rights, reinforcing its own standing in international affairs.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, the_papacy, agenda_setter,
    institutional, civilizational, analytical, universal).

% The inhabitants of the lands being divided and claimed by European powers. They were entirely excluded from the treaty negotiations and their sovereignty was disregarded, though for this specific Portuguese reading, the primary extraction was trade monopoly from rivals, not direct land conquest from indigenous groups.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_populations, excluded,
    powerless, generational, trapped, local).

% Analyze the historical impact and legal precedents set by the Treaty of Tordesillas, often critiquing its foundational assumptions and its role in legitimizing colonial expansion.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, international_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent armed conflict and intense rivalry between the emerging colonial powers of Spain and Portugal by establishing a clear, papally-sanctioned demarcation line for their respective spheres of exploration and trade.
% TRANSFER_FUNCTION: Transfers exclusive rights to exploration, trade, and potential resource extraction east of the line to Portugal, effectively excluding other European powers from these lucrative opportunities.
% ABSENT_VOICES: Indigenous populations, whose lands and resources were being divided without their consent or knowledge, would object to the fundamental premise of European claims. Other European powers (e.g., England, France, Netherlands) who did not recognize papal authority would object to their exclusion from global trade and exploration.
% DISAPPEARANCE_RATIONALE: If the Treaty of Tordesillas and its underlying papal authority had vanished, the early modern period of global exploration and colonization would have unfolded very differently, likely with more widespread and immediate conflict among European powers over trade routes and territories, and a different distribution of colonial claims.
% FOUNDING_PROBLEM: Intense and escalating rivalry between Spain and Portugal over newly discovered territories and maritime trade routes, threatening to erupt into open warfare and destabilize European politics.
% FOUNDING_PROBLEM_CORROBORATION: Historians of international law and colonial history, independent of the former Portuguese or Spanish crowns, corroborate the historical context of intense rivalry and the Papacy's role in mediating it. While the specific problem between Spain and Portugal is dead, the legacy of colonial claims and resource distribution persists.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.65) reflects the significant economic gains Portugal derived from its exclusive trade routes and resources, enforced by naval power. Suppression (0.75) was high due to the active military and diplomatic efforts to exclude other European powers from the designated Portuguese sphere. Theater ratio (0.20) was low, as the treaty was a serious instrument of international law at the time, backed by real power, not mere performance. Accessibility collapse was moderate, as other European powers eventually found ways to challenge or circumvent the treaty, but not without significant cost and risk. Resistance was high, as nations like England and France actively defied the papal demarcation.
 *
 * PERSPECTIVAL GAP:
 *   From the Portuguese perspective, the treaty was a legitimate and necessary coordination mechanism to secure their hard-won exploration rights and prevent conflict. From the perspective of rival European powers, it was an arbitrary and extractive imposition, lacking universal legitimacy. The engine's classification as Tangled Rope captures this dual nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The Portuguese Crown and Estado da Índia were clear beneficiaries, gaining exclusive access and wealth. Rival European powers were the direct targets, facing exclusion and the costs of challenging the established order. The Papacy, while an agenda-setter, did not directly extract material wealth but gained political influence. Indigenous populations were structurally excluded and bore the ultimate costs of colonization, though this specific reading focuses on inter-European extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy,
    'To what extent was the Papacy''s authority to divide the world genuinely accepted by all European powers, or merely a convenient justification for Spain and Portugal?',
    'Analysis of diplomatic correspondence and legal challenges from non-Iberian European powers of the era. If rejection was widespread and consistent, it suggests the authority was primarily instrumental for the beneficiaries.',
    'If papal authority was widely rejected, the constraint''s suppression would be seen as purely coercive rather than ideologically reinforced, potentially increasing its effective extractiveness for non-Iberian powers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_authority_legitimacy, conceptual, 'The true scope of papal authority in legitimizing colonial claims.').

omega_variable(
    primary_victim_identification,
    'Is the primary victim of this reading truly rival European powers, or are indigenous populations the more fundamental target, even if indirectly for this specific interpretation?',
    'Re-evaluation of historical records to quantify the direct economic and social impact on indigenous populations from Portuguese trade monopolies versus the impact on rival European economies from exclusion.',
    'If indigenous populations are reclassified as the primary victim, the constraint''s extractiveness and suppression would be significantly higher, and its classification might shift towards a Snare, reflecting a more direct and severe form of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primary_victim_identification, empirical, 'Clarifying the primary target of extraction for the Portuguese reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.15).
narrative_ontology:measurement(tord_tr_t1515, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1515, 0.18).
narrative_ontology:measurement(tord_tr_t1536, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1536, 0.2).
narrative_ontology:measurement(tord_tr_t1557, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1557, 0.22).
narrative_ontology:measurement(tord_tr_t1578, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1578, 0.2).
narrative_ontology:measurement(tord_tr_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1600, 0.18).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.55).
narrative_ontology:measurement(tord_be_t1515, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1515, 0.6).
narrative_ontology:measurement(tord_be_t1536, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1536, 0.63).
narrative_ontology:measurement(tord_be_t1557, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1557, 0.65).
narrative_ontology:measurement(tord_be_t1578, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1578, 0.66).
narrative_ontology:measurement(tord_be_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1600, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.65).
narrative_ontology:measurement(tord_su_t1515, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1515, 0.7).
narrative_ontology:measurement(tord_su_t1536, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1536, 0.73).
narrative_ontology:measurement(tord_su_t1557, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1557, 0.75).
narrative_ontology:measurement(tord_su_t1578, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1578, 0.76).
narrative_ontology:measurement(tord_su_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1600, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Tordesillas Demarcation Kernel, focusing on Portuguese claims to exploration and trade rights east of the line. The sibling reading, 'spanish_conquest_legitimation', focuses on territorial conquest and indigenous subjugation west of the line.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
