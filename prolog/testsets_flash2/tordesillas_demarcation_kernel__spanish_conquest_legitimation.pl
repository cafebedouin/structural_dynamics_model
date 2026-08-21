% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__spanish_conquest_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Demarcation as Spanish Conquest Legitimation (Tordesillas Kernel)
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This constraint represents the reading of the Papal Bulls (Inter Caetera,
 *   etc.) and the Treaty of Tordesillas as legitimizing Spanish territorial
 *   conquest and indigenous subjugation in the Americas. It provided a
 *   'legal' and 'moral' framework for the Spanish Crown and conquistadors to
 *   claim lands, extract resources, and impose their will on indigenous
 *   populations, often through violent means. The constraint is a Snare due
 *   to its high extraction from indigenous peoples and the active suppression
 *   required to maintain it.
 *
 * KEY AGENTS:
 *   - spanish_crown: Agenda setter (institutional/arbitrage)
 *   - spanish_conquistadors: Beneficiary (powerful/mobile)
 *   - catholic_church: Beneficiary (institutional/constrained)
 *   - indigenous_populations_west_of_line: Payer (powerless/trapped)
 *   - other_european_powers: Excluded (powerful/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.95).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.98).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.95).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Demarcation as Spanish Conquest Legitimation (Tordesillas Kernel)").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '1a9ecf04-b9a5-41c3-926b-369a3773d509').
narrative_ontology:cs_kernel_codification('1a9ecf04-b9a5-41c3-926b-369a3773d509', fixed_text).
narrative_ontology:cs_authority_grounding('1a9ecf04-b9a5-41c3-926b-369a3773d509', lineage).
narrative_ontology:cs_interpretation_layer_present('1a9ecf04-b9a5-41c3-926b-369a3773d509').
narrative_ontology:cs_reading_relation('1a9ecf04-b9a5-41c3-926b-369a3773d509', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('1a9ecf04-b9a5-41c3-926b-369a3773d509', foundational, papal_donation_grants_sovereignty).
narrative_ontology:cs_axiom_status(papal_donation_grants_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('1a9ecf04-b9a5-41c3-926b-369a3773d509', papal_donation_grants_sovereignty, theological).
narrative_ontology:cs_axiom('1a9ecf04-b9a5-41c3-926b-369a3773d509', foundational, non_christian_lands_are_res_nullius).
narrative_ontology:cs_axiom_status(non_christian_lands_are_res_nullius, holdable).
narrative_ontology:cs_axiom_grounding('1a9ecf04-b9a5-41c3-926b-369a3773d509', non_christian_lands_are_res_nullius, conventional).
narrative_ontology:cs_reference_frame('1a9ecf04-b9a5-41c3-926b-369a3773d509', papal_universal_jurisdiction).
narrative_ontology:cs_drift_state('1a9ecf04-b9a5-41c3-926b-369a3773d509', contemporary_international_law, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('1a9ecf04-b9a5-41c3-926b-369a3773d509', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_conquistadors).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Received papal sanction to claim and exploit vast territories west of the demarcation line, establishing colonial administration and extracting immense wealth. The grant provided a legal and moral basis for their actions against other European powers and indigenous peoples.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% Acted as agents of the Spanish Crown, directly benefiting from the license to conquer, enslave, and exploit indigenous populations and their resources. Their wealth and status were directly tied to the enforcement of this papal grant.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_conquistadors, beneficiary,
    powerful, biographical, mobile, continental).

% Gained vast new territories for evangelization and expanded its spiritual and temporal authority. The grants reinforced the Pope's role as a universal arbiter, despite later challenges to this authority.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church, beneficiary,
    institutional, civilizational, constrained, global).

% Were subjected to conquest, forced labor (encomienda), cultural destruction, and religious conversion. Their lands and resources were seized, and their sovereignty was denied by the European powers based on the papal decree. Resistance was met with extreme violence.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, generational, trapped, continental).

% Were nominally excluded from claiming territories west of the line by papal authority, though many later challenged this. They would have contested the legitimacy of the grant as a basis for exclusive Spanish claims.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, other_european_powers, excluded,
    powerful, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a framework for Spain to coordinate its colonial expansion and resource extraction efforts, minimizing internal disputes over claims and legitimizing its actions against indigenous peoples and rival European powers (initially).
% TRANSFER_FUNCTION: Transferred sovereignty over vast territories and their indigenous populations from pre-existing indigenous polities to the Spanish Crown, facilitating the extraction of natural resources, labor, and tribute.
% ABSENT_VOICES: Indigenous populations, whose sovereignty was unilaterally abrogated, were entirely absent from the negotiation and would have vehemently rejected the premise of European claims to their lands. Their voices were suppressed through violence and cultural erasure.
% DISAPPEARANCE_RATIONALE: If the papal grants and the Treaty of Tordesillas had vanished overnight, the legal and moral justifications for Spanish conquest would have collapsed. While conquest might still have occurred, its form, legitimacy, and the subsequent international legal order would have been fundamentally different, likely leading to more contested claims and different colonial outcomes.
% FOUNDING_PROBLEM: To prevent conflict between Spain and Portugal over newly discovered lands in the Atlantic and to legitimize Christian European claims over non-Christian territories for evangelization and resource exploitation.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars widely agree that the original problem of preventing conflict between Spain and Portugal was partially addressed, but the broader problem of legitimizing European claims over indigenous lands is now considered morally and legally indefensible. Indigenous rights organizations and post-colonial legal frameworks explicitly reject the premise of such grants.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is extremely high (0.95) as it enabled the systematic seizure of land, resources, and labor from indigenous populations. Suppression is near total (0.98) due to the military force, disease, and cultural destruction employed to enforce Spanish claims. The theater ratio (0.75) reflects that while evangelization was a stated goal, the primary function quickly became resource extraction and territorial control, with religious conversion often serving as a justification for subjugation. Accessibility collapse is high (0.90) as indigenous alternatives to Spanish rule were systematically destroyed. Resistance, though fierce, was ultimately overwhelmed (0.85).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Spanish Crown and the Catholic Church, the grants were a legitimate exercise of universal authority, coordinating European expansion and spreading Christianity. From the perspective of indigenous populations, it was an act of pure aggression and dispossession, enforced by overwhelming violence and cultural destruction. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spanish Crown and conquistadors are clear beneficiaries, receiving immense wealth and power. The Catholic Church also benefited from expanded influence and new converts. Indigenous populations are the primary victims, bearing the full cost of conquest and exploitation. Other European powers were initially excluded, making them targets of the constraint's exclusionary function.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing European conflict, spreading Christianity) quickly atrophied into a cover for pure extraction. The classification as a Snare prevents mislabeling it as a legitimate coordination mechanism, highlighting the coercive and extractive nature that persisted long after any genuine coordination function for European powers became secondary to the subjugation of indigenous peoples.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy,
    'Was the Pope''s authority to grant sovereignty over non-Christian lands genuinely accepted by all European powers, or was it primarily a convenient legal fiction for Spain and Portugal?',
    'Analysis of diplomatic correspondence and challenges from other European powers (e.g., France, England) to the papal grants, and the subsequent shift to ''effective occupation'' as a basis for colonial claims.',
    'If widely rejected, the constraint''s ''legitimacy'' as a coordination mechanism among European powers was weaker, making its persistence more reliant on raw power and less on shared belief, potentially increasing its effective suppression and extractiveness from a European perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_authority_legitimacy, empirical, 'The extent to which papal authority was genuinely accepted as a basis for territorial claims by all European states.').

omega_variable(
    evangelization_vs_extraction_priority,
    'To what extent was evangelization a genuine primary goal, versus a rhetorical justification for resource extraction and territorial control?',
    'Comparative analysis of resources allocated to missionary efforts versus military conquest and economic exploitation, and the treatment of indigenous converts versus non-converts.',
    'If evangelization was largely rhetorical, the ''coordination'' aspect of the constraint (spreading Christianity) was minimal, further solidifying its classification as a Snare by removing any genuine, non-extractive function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evangelization_vs_extraction_priority, empirical, 'The true priority of religious conversion versus economic exploitation in Spanish colonial policy.').

omega_variable(
    indigenous_sovereignty_recognition,
    'How would the classification change if indigenous sovereignty was recognized as a foundational premise from the outset?',
    'Counterfactual legal analysis based on modern international law and indigenous rights frameworks.',
    'If indigenous sovereignty was recognized, the entire premise of the papal grants would be illegitimate, reclassifying the constraint as a pure act of aggression with no coordination function, and potentially increasing its measured extractiveness and suppression to 1.0 from the indigenous perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_sovereignty_recognition, conceptual, 'Impact of recognizing indigenous sovereignty on the constraint''s legitimacy and classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1493, 1898).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1493, 0.5).
narrative_ontology:measurement(tord_tr_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1550, 0.65).
narrative_ontology:measurement(tord_tr_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1650, 0.75).
narrative_ontology:measurement(tord_tr_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1750, 0.7).
narrative_ontology:measurement(tord_tr_t1850, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1850, 0.6).
narrative_ontology:measurement(tord_tr_t1898, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1898, 0.45).

% Extraction over time
narrative_ontology:measurement(tord_be_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1493, 0.85).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1550, 0.92).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1650, 0.95).
narrative_ontology:measurement(tord_be_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1750, 0.93).
narrative_ontology:measurement(tord_be_t1850, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1850, 0.88).
narrative_ontology:measurement(tord_be_t1898, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1898, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1493, 0.75).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1550, 0.9).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1650, 0.98).
narrative_ontology:measurement(tord_su_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1750, 0.95).
narrative_ontology:measurement(tord_su_t1850, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1850, 0.9).
narrative_ontology:measurement(tord_su_t1898, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1898, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_exploration_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomienda_system).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, colonial_resource_extraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Tordesillas Demarcation Kernel. This reading focuses on the Spanish legitimation of conquest and subjugation, while 'portuguese_exploration_legitimation' focuses on the confirmation of prior exploration rights and exclusion of rivals east of the line. Both derive from the same papal decrees and treaty but emphasize different beneficiaries and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
