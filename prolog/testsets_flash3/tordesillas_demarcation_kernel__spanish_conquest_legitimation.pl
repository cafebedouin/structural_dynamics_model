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
 *   human_readable: Papal Grant as License for Spanish Conquest and Indigenous Subjugation (West of Tordesillas Line)
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This constraint represents the Spanish reading of the Papal Bulls (Inter
 *   Caetera, Dudum Siquidem) and the Treaty of Tordesillas, which granted
 *   Spain exclusive rights to 'discover' and claim lands west of a
 *   demarcation line. This reading interpreted the grant as a license for
 *   territorial conquest, resource extraction, and the subjugation of
 *   indigenous populations, justified by the 'discovery doctrine' and the
 *   imperative of Christian conversion. The constraint operated as a snare,
 *   enabling massive extraction and suppression of indigenous sovereignty and
 *   life.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.95).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.98).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.95).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Grant as License for Spanish Conquest and Indigenous Subjugation (West of Tordesillas Line)").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b').
narrative_ontology:cs_kernel_codification('4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b', fixed_text).
narrative_ontology:cs_authority_grounding('4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b', lineage).
narrative_ontology:cs_interpretation_layer_present('4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b').
narrative_ontology:cs_reading_relation('4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b', foundational, papal_grant_confers_sovereignty_over_non_christian_lands).
narrative_ontology:cs_axiom_status(papal_grant_confers_sovereignty_over_non_christian_lands, overridden).
narrative_ontology:cs_axiom_grounding('4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b', papal_grant_confers_sovereignty_over_non_christian_lands, theological).
narrative_ontology:cs_axiom('4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b', foundational, indigenous_peoples_lack_true_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_peoples_lack_true_sovereignty, overridden).
narrative_ontology:cs_axiom_grounding('4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b', indigenous_peoples_lack_true_sovereignty, conventional).
narrative_ontology:cs_reference_frame('4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b', divine_right_of_conquest).
narrative_ontology:cs_drift_state('4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b', contemporary_international_law, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('4993bfd8-c422-4580-ae1b-fcbe5a0d9c3b', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_sovereigns).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the vast territories claimed by Spain, establishing encomienda systems, forced labor, and resource extraction. Directly benefited from the subjugation of indigenous populations and the wealth generated.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Received a significant portion of the wealth extracted from the colonies, funding its European wars and imperial ambitions. The Papal grant provided a crucial legal and moral justification for its claims against other European powers.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, beneficiary,
    institutional, generational, arbitrage, global).

% Subjected to forced labor, land expropriation, cultural destruction, and religious conversion. Their traditional sovereignty was denied, and their lives were dictated by the colonial administration. Resistance was met with extreme violence.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, generational, trapped, continental).

% Their pre-existing claims to sovereignty and territorial control were entirely disregarded by the Papal grant and subsequent Spanish actions. They were not consulted and had no standing in the European legal framework that dispossessed them.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_sovereigns, excluded,
    powerless, generational, trapped, regional).

% Gained new converts and expanded its spiritual authority globally, while also receiving tithes and other forms of wealth from the colonies. The grants reinforced its temporal power to allocate non-Christian lands.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church, beneficiary,
    institutional, civilizational, mobile, global).

% Bound by the same Papal authority but focused on its own sphere of influence east of the line. While benefiting from its own grants, it observed Spanish actions as a precedent and potential challenge to its own claims.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_crown, observer,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a clear, albeit unilaterally declared, division of newly 'discovered' non-Christian lands between Spain and Portugal, aiming to prevent conflict between these two Catholic powers over colonial expansion.
% TRANSFER_FUNCTION: Transferred theoretical sovereignty over vast territories and their indigenous populations from pre-existing indigenous polities to the Spanish Crown, facilitating the extraction of resources and labor.
% ABSENT_VOICES: Indigenous populations and their leaders, whose lands and lives were being divided and claimed without their consent or even knowledge. Their voices were entirely excluded from the European legal and theological discourse that justified their subjugation.
% DISAPPEARANCE_RATIONALE: If the Papal grant's legitimating power vanished, the entire legal and moral framework for Spanish colonial claims would collapse. While the physical presence of Spanish administration would remain, its foundational justification would be gone, leading to a profound re-evaluation of sovereignty and historical claims.
% FOUNDING_PROBLEM: To prevent armed conflict between the Catholic monarchies of Spain and Portugal over the 'discovery' and exploitation of new lands, and to legitimize their claims under Christian doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars widely attest that the problem of preventing conflict between Spain and Portugal was resolved by the treaty, but the problem of legitimizing conquest over indigenous peoples is now universally rejected as a valid 'problem' to solve. The Papal authority to grant such rights is no longer recognized by international law.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is extremely high (0.95) due to the systematic expropriation of land, resources, and labor from indigenous peoples. Suppression is near total (0.98) as indigenous resistance was met with overwhelming military force and the denial of any legitimate claim to sovereignty. Theater ratio is low (0.1) because the 'civilizing mission' and 'conversion' narratives, while present, were largely cover for brutal material extraction, not the primary function. The claimed type is 'snare' because the coordination story (preventing conflict between Spain and Portugal) was a cover for pure extraction from identifiable victims.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Spanish Crown and colonial administration, the Papal grant was a legitimate exercise of authority, providing a clear mandate for their actions. From the perspective of indigenous populations, it was an act of profound injustice and violence, entirely lacking legitimacy. The engine's classification will highlight this divergence by computing a snare from the victims' seats, despite the claimed 'rope' (coordination) function by the beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spanish colonial administration and Crown were the primary beneficiaries, receiving immense wealth and expanded imperial power. Indigenous populations and their sovereigns were the clear victims, suffering dispossession, forced labor, and cultural destruction. The Catholic Church also benefited from expanded spiritual authority and new converts. The Portuguese Crown, while a party to the overall treaty, was an observer to this specific reading's application west of the line.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy,
    'Was the Papal authority to grant sovereignty over non-Christian lands a legitimate basis for international law at the time, or was it a contested claim even among European powers?',
    'Analysis of contemporary European legal and theological debates, diplomatic correspondence, and challenges by non-Catholic powers (e.g., England, France) to the Papal grants.',
    'If widely contested, the ''legitimacy'' of the Papal grant as a coordination mechanism among European powers is weakened, further exposing its extractive function towards indigenous peoples. If largely accepted by European powers, it highlights the systemic nature of colonial legal frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_authority_legitimacy, conceptual, 'The extent of European acceptance of Papal authority in territorial grants.').

omega_variable(
    indigenous_sovereignty_recognition,
    'To what extent did Spanish legal and theological thought acknowledge or debate the pre-existing sovereignty of indigenous nations, even while pursuing conquest?',
    'Examination of the Valladolid debate (1550-1551), writings of Bartolomé de las Casas, Francisco de Vitoria, and other contemporary Spanish jurists and theologians regarding indigenous rights and natural law.',
    'Evidence of significant internal debate and recognition of indigenous sovereignty would complicate the ''pure extraction'' narrative, suggesting a more tangled (though still extractive) internal logic. Lack of such recognition reinforces the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_sovereignty_recognition, empirical, 'Internal Spanish debate on indigenous sovereignty.').

omega_variable(
    mandatrophy_of_conversion_justification,
    'Did the ''conversion of souls'' justification for conquest genuinely drive Spanish policy, or was it primarily a rhetorical cover for material extraction?',
    'Comparative analysis of resources allocated to missionary efforts versus military conquest and resource extraction, and the treatment of converted versus unconverted indigenous populations.',
    'If conversion was a genuine, primary driver, the constraint might have a stronger (though still highly extractive) coordination function (e.g., for the Church). If primarily rhetorical, it reinforces the snare classification by exposing the cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_conversion_justification, empirical, 'The true motivation behind the ''conversion'' justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1493, 1898).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1493, 0.05).
narrative_ontology:measurement(tord_tr_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1550, 0.08).
narrative_ontology:measurement(tord_tr_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1650, 0.1).
narrative_ontology:measurement(tord_tr_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1750, 0.12).
narrative_ontology:measurement(tord_tr_t1850, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(tord_tr_t1898, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1898, 0.18).

% Extraction over time
narrative_ontology:measurement(tord_be_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1493, 0.85).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1550, 0.9).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1650, 0.95).
narrative_ontology:measurement(tord_be_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1750, 0.92).
narrative_ontology:measurement(tord_be_t1850, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1850, 0.88).
narrative_ontology:measurement(tord_be_t1898, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1898, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1493, 0.8).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1550, 0.9).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1650, 0.98).
narrative_ontology:measurement(tord_su_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1750, 0.95).
narrative_ontology:measurement(tord_su_t1850, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1850, 0.9).
narrative_ontology:measurement(tord_su_t1898, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1898, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_exploration_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomienda_system).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, forced_labor_repartimiento).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Tordesillas Demarcation Kernel. It focuses on the Spanish interpretation and its consequences for indigenous populations west of the line. The 'portuguese_exploration_legitimation' reading focuses on Portugal's claims east of the line.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
