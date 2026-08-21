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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Grant as Spanish Conquest Legitimation (West of Tordesillas Line)
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This constraint story analyzes the 'spanish_conquest_legitimation'
 *   reading of the Tordesillas Demarcation Kernel. It focuses on how Papal
 *   grants and the Treaty of Tordesillas were interpreted by the Spanish
 *   Crown and Catholic Church to legitimize territorial conquest and the
 *   subjugation of indigenous populations in the Americas, west of the
 *   demarcation line. This reading frames the constraint as a Snare,
 *   characterized by high extraction and suppression, with indigenous peoples
 *   as primary victims and the Spanish colonial apparatus as the primary
 *   beneficiary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.92).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.95).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.92).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Grant as Spanish Conquest Legitimation (West of Tordesillas Line)").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'ec6b039e-658d-4c03-bd18-32989284ce3f').
narrative_ontology:cs_kernel_codification('ec6b039e-658d-4c03-bd18-32989284ce3f', fixed_text).
narrative_ontology:cs_authority_grounding('ec6b039e-658d-4c03-bd18-32989284ce3f', lineage).
narrative_ontology:cs_interpretation_layer_present('ec6b039e-658d-4c03-bd18-32989284ce3f').
narrative_ontology:cs_reading_relation('ec6b039e-658d-4c03-bd18-32989284ce3f', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('ec6b039e-658d-4c03-bd18-32989284ce3f', foundational, divine_mandate_for_conversion).
narrative_ontology:cs_axiom_status(divine_mandate_for_conversion, holdable).
narrative_ontology:cs_axiom_grounding('ec6b039e-658d-4c03-bd18-32989284ce3f', divine_mandate_for_conversion, theological).
narrative_ontology:cs_axiom('ec6b039e-658d-4c03-bd18-32989284ce3f', foundational, papal_donation_grants_sovereignty).
narrative_ontology:cs_axiom_status(papal_donation_grants_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ec6b039e-658d-4c03-bd18-32989284ce3f', papal_donation_grants_sovereignty, conventional).
narrative_ontology:cs_reference_frame('ec6b039e-658d-4c03-bd18-32989284ce3f', papal_supremacy_over_non_christian_lands).
narrative_ontology:cs_drift_state('ec6b039e-658d-4c03-bd18-32989284ce3f', post_enlightenment_international_law, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('ec6b039e-658d-4c03-bd18-32989284ce3f', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, divine_right_of_conquest).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate authority claiming sovereignty over vast territories and peoples west of the Tordesillas line, deriving legitimacy from Papal Bulls and enforcing its will through military and administrative power. Benefits from immense wealth extraction.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% The direct implementers of colonial policy, establishing encomiendas, mining operations, and forced labor systems. They manage the extraction of resources and labor, benefiting directly from the subjugation of indigenous populations.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, beneficiary,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, agenda_setter).

% Provided the theological and moral justification for conquest and conversion, receiving tithes, land grants, and expanding its spiritual dominion. Its authority was central to the legitimation narrative, even as its direct material benefits were substantial.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church, agenda_setter).

% The primary targets of conquest, subjected to forced labor, cultural destruction, religious conversion, and violence. Their lands, resources, and labor were systematically extracted, with no legitimate means of exit or appeal within the colonial system.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, immediate, trapped, local).

% While a beneficiary of the broader Tordesillas kernel, from the perspective of the Spanish conquest legitimation, the Portuguese Crown was excluded from claiming territories west of the line, its claims confined to the east. It would have contested Spanish incursions into its designated sphere.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_crown, excluded,
    institutional, generational, constrained, global).

% Study the historical context, legal justifications, and material consequences of the Papal grants and the Treaty of Tordesillas, analyzing their role in legitimizing colonial expansion and indigenous subjugation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, analytical_historians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__spanish_conquest_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a framework for European powers (specifically Spain and Portugal) to divide newly 'discovered' non-Christian lands, preventing direct conflict between them over territorial claims and coordinating their respective colonial expansions.
% TRANSFER_FUNCTION: Transferred sovereignty, land, resources, and the labor of indigenous populations from their existing polities to the Spanish Crown and its colonial administration, justified by Papal authority and the mandate to convert non-Christians.
% ABSENT_VOICES: Indigenous leaders and peoples, whose sovereignty was unilaterally denied and whose lands were claimed without their consent. Their voices were systematically suppressed through military force, legal fictions like 'terra nullius,' and forced conversion.
% DISAPPEARANCE_RATIONALE: If the Papal grants and the Treaty of Tordesillas had vanished, the entire legal and moral framework for Spanish colonial expansion would have collapsed. While conquest might still have occurred, its justification and the subsequent institutionalization of extraction would have been fundamentally different, leading to a radically altered history of the Americas.
% FOUNDING_PROBLEM: The problem of legitimizing European claims to non-Christian lands and preventing conflict between competing Catholic monarchies (Spain and Portugal) over newly 'discovered' territories.
% FOUNDING_PROBLEM_CORROBORATION: While the Spanish Crown and the Catholic Church at the time asserted the problem was live and their solution divinely ordained, modern international law and historical scholarship (from analytical historians and indigenous rights advocates) overwhelmingly attest that the 'problem' was a construct of European imperial ambition, and its 'solution' a mechanism for dispossession. The underlying premise of Papal authority to grant sovereignty over non-Christian lands is widely rejected today.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is extremely high (0.92) as the entire colonial enterprise was built on the systematic transfer of wealth and labor from indigenous societies to Spain. Suppression is also very high (0.95) due to military conquest, forced labor systems (encomienda, mita), and the violent suppression of indigenous resistance and cultural practices. Accessibility collapse is near total (0.90) for indigenous populations, who had no recognized legal or political alternatives. Resistance (0.85) was constant and widespread, but largely unsuccessful in overturning the colonial structure. Theater ratio (0.45) reflects that while religious conversion was a genuine goal for some, it increasingly served as a performative justification for economic exploitation, especially as Enlightenment ideas challenged the divine right of conquest.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Spanish Crown and Church, this arrangement was a divinely sanctioned mission of civilization and salvation, a legitimate exercise of authority. From the perspective of indigenous populations, it was an existential catastrophe of violent invasion, theft, and enslavement. The engine's classification as a Snare reflects the latter, structurally accurate perspective, despite the former's self-justifying narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spanish Crown, its colonial administration, and the Catholic Church were the clear beneficiaries, deriving immense material wealth, political power, and spiritual dominion. Indigenous populations west of the line were the unequivocal targets and victims, experiencing total dispossession and subjugation. The Portuguese Crown, while a beneficiary of the overall kernel, was structurally excluded from this specific Spanish sphere of conquest.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (divine right to conquer and convert) was never truly resolved in a way that acknowledged indigenous sovereignty. Instead, it persisted as a foundational myth for colonial rule. The 'founding problem' (legitimizing European claims) became a cover for ongoing extraction. The classification as a Snare prevents mislabeling this as a coordination mechanism, highlighting its coercive and extractive nature from the outset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy,
    'Was the Papal authority to grant sovereignty over non-Christian lands genuinely accepted as legitimate by all European powers, or was it primarily a convenient legal fiction for political and economic expansion?',
    'Analysis of diplomatic correspondence and legal challenges from non-Catholic European powers (e.g., England, France, Netherlands) regarding the validity of Papal grants in their own colonial claims.',
    'If widely rejected by other powers, it underscores the constructed and self-serving nature of the ''legitimacy'' for Spain, further solidifying the Snare classification. If genuinely accepted, it highlights the pervasive nature of the ideological capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_authority_legitimacy, conceptual, 'The true extent of Papal authority''s acceptance in international law.').

omega_variable(
    indigenous_sovereignty_recognition,
    'To what extent did Spanish legal and theological debates (e.g., Valladolid Controversy) genuinely consider indigenous sovereignty, and how did these debates impact actual colonial practice?',
    'Detailed historical analysis of the implementation of ''New Laws'' and other reforms intended to protect indigenous rights, and their actual effect on encomienda and forced labor systems.',
    'If debates led to meaningful, sustained changes in practice, it might suggest a slight, temporary reduction in extractiveness or suppression. If they remained largely theoretical or were systematically circumvented, it reinforces the high extraction and suppression metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_sovereignty_recognition, empirical, 'Impact of internal Spanish debates on indigenous rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1494, 1820).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1494, 0.2).
narrative_ontology:measurement(tord_tr_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1550, 0.3).
narrative_ontology:measurement(tord_tr_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1650, 0.4).
narrative_ontology:measurement(tord_tr_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1750, 0.5).
narrative_ontology:measurement(tord_tr_t1820, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1820, 0.45).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1494, 0.85).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1550, 0.9).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1650, 0.93).
narrative_ontology:measurement(tord_be_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1750, 0.92).
narrative_ontology:measurement(tord_be_t1820, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1820, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1494, 0.8).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1550, 0.9).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1650, 0.95).
narrative_ontology:measurement(tord_su_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1750, 0.93).
narrative_ontology:measurement(tord_su_t1820, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1820, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_exploration_legitimation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Tordesillas Demarcation Kernel, which also includes the 'portuguese_exploration_legitimation' reading. Both readings derive from the same Papal Bulls and Treaty, but legitimize different spheres of colonial activity and have distinct beneficiary/victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
