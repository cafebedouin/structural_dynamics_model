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
 *   This constraint story analyzes the Treaty of Tordesillas from the
 *   perspective of 'Portuguese exploration legitimation.' It views the papal
 *   demarcation as confirming prior Portuguese exploration rights and
 *   establishing an exclusive sphere for trade and influence east of the
 *   line, primarily to exclude rival European powers. The constraint is
 *   claimed as a Tangled Rope, reflecting its dual function of coordinating
 *   Portuguese-Castilian expansion while extracting from other European
 *   nations and indigenous populations. The metrics reflect its active
 *   enforcement and the substantial, though not absolute, suppression of
 *   alternatives.
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
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Treaty of Tordesillas: Portuguese Exploration Legitimation").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'f20be2d9-b794-4cf4-84b1-9b72e293a03e').
narrative_ontology:cs_kernel_codification('f20be2d9-b794-4cf4-84b1-9b72e293a03e', fixed_text).
narrative_ontology:cs_authority_grounding('f20be2d9-b794-4cf4-84b1-9b72e293a03e', lineage).
narrative_ontology:cs_interpretation_layer_present('f20be2d9-b794-4cf4-84b1-9b72e293a03e').
narrative_ontology:cs_reading_relation('f20be2d9-b794-4cf4-84b1-9b72e293a03e', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('f20be2d9-b794-4cf4-84b1-9b72e293a03e', foundational, prior_discovery_confers_rights).
narrative_ontology:cs_axiom_status(prior_discovery_confers_rights, holdable).
narrative_ontology:cs_axiom_grounding('f20be2d9-b794-4cf4-84b1-9b72e293a03e', prior_discovery_confers_rights, conventional).
narrative_ontology:cs_axiom('f20be2d9-b794-4cf4-84b1-9b72e293a03e', foundational, papal_authority_to_demarcate).
narrative_ontology:cs_axiom_status(papal_authority_to_demarcate, holdable).
narrative_ontology:cs_axiom_grounding('f20be2d9-b794-4cf4-84b1-9b72e293a03e', papal_authority_to_demarcate, theological).
narrative_ontology:cs_reference_frame('f20be2d9-b794-4cf4-84b1-9b72e293a03e', papal_mediated_global_division).
narrative_ontology:cs_drift_state('f20be2d9-b794-4cf4-84b1-9b72e293a03e', rise_of_protestant_powers, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f20be2d9-b794-4cf4-84b1-9b72e293a03e', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_merchants).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_populations_east_of_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary architect and enforcer of the treaty's terms for the Portuguese sphere. It claimed exclusive rights to exploration, trade, and resource extraction east of the demarcation line, leveraging papal authority and naval power to defend these claims against rivals.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, civilizational, constrained, global).

% Benefited directly from the state-sanctioned monopolies on trade routes and resources in the Portuguese sphere. The treaty reduced competition, allowing them to accumulate wealth from spices, gold, and other commodities with less risk from European rivals.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_merchants, beneficiary,
    powerful, biographical, mobile, global).

% European states like England, France, and the Netherlands were excluded from the lucrative trade routes and potential colonial territories east of the Tordesillas line. They bore the cost of this exclusion through lost economic opportunities and the need to find alternative, often more difficult, routes.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers, payer,
    institutional, generational, constrained, global).

% Their sovereignty and territorial rights were entirely disregarded by the treaty, which unilaterally divided their lands between European powers. While the Portuguese reading focused on trade, it still asserted European dominion over their territories, leading to eventual exploitation and loss of autonomy.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_populations_east_of_line, excluded,
    powerless, generational, trapped, local).

% The Pope issued the bulls (e.g., Inter caetera, Dudum Siquidem) that preceded and confirmed the Treaty of Tordesillas, providing a crucial layer of religious and moral legitimacy to the demarcation. Its authority was invoked to justify the division of non-Christian lands.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy, agenda_setter,
    institutional, civilizational, analytical, universal).

% Analyze the historical context, legal implications, and long-term consequences of the Treaty of Tordesillas, including its role in shaping international law and colonial practices. They provide an external, critical perspective on its legitimacy and effects.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, international_law_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent direct military conflict and intense rivalry between the Portuguese and Castilian (Spanish) crowns over newly 'discovered' lands and maritime trade routes in the Atlantic, by establishing a clear, papally-sanctioned division of their respective spheres of influence.
% TRANSFER_FUNCTION: Transfers exclusive rights to exploration, trade, and potential resource extraction from rival European powers and indigenous populations to the Portuguese Crown and its merchants for all territories east of the demarcation line.
% ABSENT_VOICES: Indigenous populations whose lands and sovereignty were unilaterally divided without their consent or even knowledge; other European powers (e.g., England, France, Netherlands) who were excluded from these lucrative spheres and would later challenge the treaty's legitimacy.
% DISAPPEARANCE_RATIONALE: If the Treaty of Tordesillas and its underlying papal authority had vanished, the initial phase of European global expansion would have been far more chaotic, likely leading to earlier and more intense direct warfare between Portugal and Castile, and a different pattern of colonial claims by other European powers. The global distribution of colonial power and trade routes would have reorganized significantly.
% FOUNDING_PROBLEM: Escalating rivalry and potential warfare between the Portuguese and Castilian crowns over the rights to newly 'discovered' territories and maritime trade routes in the Atlantic, following Columbus's voyages.
% FOUNDING_PROBLEM_CORROBORATION: Historians of international law, colonial history, and the Papacy widely corroborate the intense rivalry between Portugal and Castile and the Papacy's role in mediating this dispute to prevent conflict. Independent historical accounts from the period attest to the diplomatic tensions and the perceived need for a formal agreement.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) stems from the monopoly rents Portugal could collect from its exclusive trade routes and resource claims. Suppression (0.75) was high due to the active naval enforcement by Portugal and the diplomatic weight of papal authority, which deterred direct challenges for a significant period. Theater ratio (0.20) is low because the claims were genuinely asserted and defended, not merely performative. Accessibility collapse (0.60) was substantial for excluded European powers, though not absolute, as they eventually found ways to circumvent or challenge the treaty. Resistance (0.50) was moderate, initially from Castile (resolved by the treaty) and later from other European powers who increasingly ignored its provisions.
 *
 * PERSPECTIVAL GAP:
 *   From the Portuguese perspective, the treaty was a legitimate, divinely sanctioned agreement that secured their rightful claims based on prior exploration. From the perspective of rival European powers, it was an arbitrary and unjust division of the world. From the indigenous perspective, it was an act of profound disregard for their existence and sovereignty. The engine's classification will highlight this divergence by computing different effective extraction values for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The Portuguese Crown and its merchants are clear beneficiaries, gaining exclusive access and monopoly profits. Rival European powers are targets, excluded from lucrative trade. Indigenous populations east of the line are also targets, as their sovereignty was disregarded, even if direct conquest was not the primary focus of this specific Portuguese reading. The Papacy, as the legitimizing authority, acts as an agenda-setter, providing the framework for the division.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy,
    'Was the Papacy''s authority to divide non-Christian lands genuinely accepted by all European powers, or merely a convenient justification for Portugal and Castile?',
    'Analysis of diplomatic correspondence and legal challenges from non-Iberian European powers during the 16th and 17th centuries. If challenges consistently denied papal authority, it suggests a convenient justification.',
    'If papal authority was widely rejected, the constraint''s underlying legitimacy was weaker, increasing its reliance on active enforcement and potentially raising its effective suppression for excluded parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_legitimacy, empirical, 'The extent of genuine acceptance of papal authority in international law.').

omega_variable(
    trade_vs_conquest_distinction,
    'Is the distinction between ''exploration rights/trade monopoly'' (Portuguese reading) and ''territorial conquest/indigenous subjugation'' (Spanish reading) genuinely separable, or did the former inevitably lead to the latter?',
    'Comparative historical analysis of Portuguese colonial practices in the East versus Spanish practices in the West, examining the extent of direct territorial control and indigenous displacement in each sphere.',
    'If the distinction is found to be largely semantic, with trade monopolies inevitably leading to de facto territorial control and subjugation, then the Portuguese reading''s extractiveness for indigenous populations would be higher than currently assessed, blurring the line between the two kernel readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trade_vs_conquest_distinction, conceptual, 'The structural separability of trade-based versus conquest-based colonial extraction.').

omega_variable(
    indigenous_sovereignty_recognition,
    'To what extent did the non-recognition of indigenous sovereignty constitute a separate, more fundamental constraint, rather than merely a consequence of the Tordesillas demarcation?',
    'Analysis of pre-Tordesillas European legal and theological doctrines regarding non-Christian peoples and their lands. If a consistent doctrine of non-recognition predates and is independent of Tordesillas, it suggests a separate constraint.',
    'If a separate, more fundamental constraint of non-recognition exists, the Tordesillas treaty merely instantiated it, and the primary extraction from indigenous populations should be attributed to that deeper constraint, with Tordesillas acting as an ''affects_constraint'' link.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_sovereignty_recognition, conceptual, 'Whether indigenous sovereignty disregard is a distinct, deeper constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.1).
narrative_ontology:measurement(tord_tr_t1515, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1515, 0.12).
narrative_ontology:measurement(tord_tr_t1536, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1536, 0.15).
narrative_ontology:measurement(tord_tr_t1557, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1557, 0.17).
narrative_ontology:measurement(tord_tr_t1578, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1578, 0.19).
narrative_ontology:measurement(tord_tr_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1600, 0.2).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.55).
narrative_ontology:measurement(tord_be_t1515, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1515, 0.58).
narrative_ontology:measurement(tord_be_t1536, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1536, 0.61).
narrative_ontology:measurement(tord_be_t1557, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1557, 0.63).
narrative_ontology:measurement(tord_be_t1578, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1578, 0.64).
narrative_ontology:measurement(tord_be_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1600, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.65).
narrative_ontology:measurement(tord_su_t1515, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1515, 0.68).
narrative_ontology:measurement(tord_su_t1536, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1536, 0.7).
narrative_ontology:measurement(tord_su_t1557, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1557, 0.72).
narrative_ontology:measurement(tord_su_t1578, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1578, 0.74).
narrative_ontology:measurement(tord_su_t1600, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1600, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% This constraint is one of two primary readings of the Tordesillas Demarcation kernel. This reading focuses on Portuguese claims to exploration and trade monopoly east of the line, while the sibling reading (spanish_conquest_legitimation) focuses on Spanish claims to territorial conquest and indigenous subjugation west of the line. Both are distinct but coexisting interpretations of the same foundational treaty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
