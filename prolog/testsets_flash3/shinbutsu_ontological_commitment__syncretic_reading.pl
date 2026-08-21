% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Metaphysics (Syncretic Reading)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint describes the honji-suijaku (original ground and trace
 *   manifestation) metaphysics in pre-modern Japan, specifically from a
 *   'syncretic reading' perspective. Under this view, kami (Shinto deities)
 *   were understood as local manifestations (suijaku) of universal buddhas
 *   (honji). This framework facilitated the integration of Shinto practices
 *   into a Buddhist-dominated cosmology, leading to high institutional
 *   integration and doctrinal coherence, but also to the suppression of
 *   Shinto autonomy and the benefit of Buddhist hierarchy. The constraint is
 *   claimed as a Tangled Rope because it provided a coordination function
 *   (religious unity) but with significant asymmetric extraction.
 *
 * KEY AGENTS:
 *   - buddhist_institutions: Agenda-setter (institutional/arbitrage) — consolidated power and doctrine.
 *   - imperial_court: Beneficiary (institutional/constrained) — gained legitimacy and stability.
 *   - shinto_shrines: Payer (organized/identity_locked) — subsumed under Buddhist authority, lost autonomy.
 *   - local_kami_cults: Payer (powerless/trapped) — reinterpreted practices, lost original meaning.
 *   - shinto_scholars_and_priests: Excluded (moderate/constrained) — marginalized voices for independent Shinto theology.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.65).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.7).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Metaphysics (Syncretic Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '94033a71-0c1a-4f29-a311-20a3992b85da').
narrative_ontology:cs_kernel_codification('94033a71-0c1a-4f29-a311-20a3992b85da', formalized).
narrative_ontology:cs_authority_grounding('94033a71-0c1a-4f29-a311-20a3992b85da', lineage).
narrative_ontology:cs_interpretation_layer_present('94033a71-0c1a-4f29-a311-20a3992b85da').
narrative_ontology:cs_reading_relation('94033a71-0c1a-4f29-a311-20a3992b85da', shinbutsu_ontological_commitment__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('94033a71-0c1a-4f29-a311-20a3992b85da', shinbutsu_ontological_commitment__incoherence_reading, forecloses).
narrative_ontology:cs_axiom('94033a71-0c1a-4f29-a311-20a3992b85da', foundational, kami_are_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddha_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('94033a71-0c1a-4f29-a311-20a3992b85da', kami_are_buddha_manifestations, theological).
narrative_ontology:cs_axiom('94033a71-0c1a-4f29-a311-20a3992b85da', foundational, unified_cosmological_order).
narrative_ontology:cs_axiom_status(unified_cosmological_order, holdable).
narrative_ontology:cs_axiom_grounding('94033a71-0c1a-4f29-a311-20a3992b85da', unified_cosmological_order, theological).
narrative_ontology:cs_reference_frame('94033a71-0c1a-4f29-a311-20a3992b85da', unified_buddhist_shinto_cosmology).
narrative_ontology:cs_drift_state('94033a71-0c1a-4f29-a311-20a3992b85da', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('94033a71-0c1a-4f29-a311-20a3992b85da', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, imperial_court).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_shrines).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cults).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, buddhist_cosmological_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Positioned kami as local manifestations of universal buddhas, integrating Shinto practices under Buddhist doctrinal and institutional authority. Benefited from increased patronage and doctrinal coherence, consolidating their power and influence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefited from the syncretic framework by legitimizing its rule through a unified religious cosmology that incorporated both indigenous and imported traditions, fostering social stability and control.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, imperial_court, beneficiary,
    institutional, generational, constrained, national).

% Were often subsumed under Buddhist temples, with kami reinterpreted as local manifestations of buddhas. While their existence was preserved, their autonomy and distinct theological identity were suppressed, and their resources sometimes diverted.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_shrines, payer,
    organized, generational, identity_locked, local).

% Experienced the direct imposition of Buddhist interpretations on their indigenous practices. Their local traditions were recontextualized, often losing their original meaning and becoming subordinate to a larger Buddhist framework, with little recourse.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cults, payer,
    powerless, biographical, trapped, local).

% Those who sought to maintain a distinct Shinto identity and theology found their voices marginalized within the dominant syncretic discourse. Their attempts to articulate an independent Shinto cosmology were often dismissed or suppressed by the prevailing Buddhist-centric framework.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_scholars_and_priests, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a unified religious framework for the Japanese archipelago, integrating diverse local kami cults and imported Buddhism into a coherent cosmological and social order, reducing inter-religious conflict and facilitating centralized governance.
% TRANSFER_FUNCTION: Transferred doctrinal authority, institutional control, and material resources from autonomous Shinto shrines and local kami cults to Buddhist institutions, in exchange for cosmological legitimation and integration into a broader religious system.
% ABSENT_VOICES: Shinto scholars and priests advocating for an independent Shinto theology were marginalized; they would argue for the distinct and supreme nature of kami, rejecting their subordination to buddhas. Their absence allowed the Buddhist-centric syncretism to solidify.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku framework vanished, the religious landscape of pre-modern Japan would have been fundamentally different. Buddhist institutions would lose a significant source of legitimacy and control over local cults, and Shinto traditions would likely have developed more independently, leading to a fragmented or differently structured religious and political order.
% FOUNDING_PROBLEM: The need to reconcile indigenous Japanese kami worship with the rapidly spreading and politically influential imported Buddhist doctrines, to create a stable and unified religious and political order.
% FOUNDING_PROBLEM_CORROBORATION: While Buddhist institutions historically asserted the problem was live, modern historical and religious studies scholars (analytical observers) widely corroborate that the 'problem' of reconciling was largely a means to assert Buddhist doctrinal and institutional supremacy, and that the original problem of religious fragmentation was resolved through a hierarchical integration that benefited one party over another. The Meiji-era separation of Shinto and Buddhism (Shinbutsu-Bunri) further demonstrated the constructed nature of the 'unified' problem.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because Shinto traditions, while preserved, were systematically subordinated and reinterpreted to fit a Buddhist framework, leading to a loss of independent theological development and institutional autonomy. Suppression (0.70) was significant, enforced through doctrinal authority, institutional integration (e.g., jingūji, shrine-temples), and political patronage that favored Buddhist interpretations. The theater ratio (0.20) is relatively low, as the syncretic framework was genuinely functional in integrating religious practices and legitimizing political power, rather than being purely performative. The historical measurements show a rise in extractiveness and suppression as the honji-suijaku system became more entrenched, peaking before the Meiji Restoration.
 *
 * PERSPECTIVAL GAP:
 *   Buddhist institutions and the Imperial Court would have experienced this as a successful and beneficial coordination mechanism, providing religious and political stability. In contrast, Shinto shrines and local kami cults would have experienced it as a system of enforced subordination and cultural reinterpretation, where their traditions were co-opted and their autonomy diminished. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutions and the Imperial Court are clear beneficiaries, gaining power, resources, and legitimacy from the unified system. Shinto shrines and local kami cults are the primary targets, bearing the costs of doctrinal subordination and institutional control. Shinto scholars are excluded, their alternative interpretations suppressed by the dominant narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (which would ignore the asymmetric extraction from Shinto traditions) or a Snare (which would ignore the genuine coordination function of religious unity and political stability it provided for centuries). The 'dead' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that while the original problem of religious fragmentation was 'solved' (or rather, managed through hierarchical integration), the constraint persisted due to the benefits it accrued to the Buddhist institutions and the Imperial Court, rather than a continued genuine need for that specific form of coordination. This points to a form of mandatrophy where the constraint's function shifted from solving a problem to maintaining an extractive hierarchy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_shinto_agency,
    'To what extent did Shinto shrines and local kami cults actively participate in or resist the honji-suijaku integration, rather than being passively subsumed?',
    'Further historical and archaeological research into local shrine records, oral traditions, and material culture to identify instances of active negotiation, adaptation, or resistance to Buddhist influence.',
    'If significant agency and negotiation are found, the suppression metric might be slightly lower, and the constraint might lean more towards a complex Tangled Rope with more active, albeit unequal, participation. If passive subsumption is confirmed, the suppression and extractiveness values are robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_shinto_agency, empirical, 'Ambiguity regarding the active agency of Shinto entities in the syncretic process.').

omega_variable(
    syncretic_coherence_vs_incoherence,
    'Was the honji-suijaku framework truly a coherent ontological system, or was it an institutionally tolerated incoherence that allowed different interpretations to coexist without full integration?',
    'Analysis of philosophical texts and ritual practices for internal consistency versus evidence of pragmatic, context-dependent application without deep ontological synthesis. Comparison with the ''incoherence_reading'' of this kernel.',
    'If found to be more incoherent, the ''syncretic_reading''s'' claim of doctrinal coherence would weaken, potentially shifting its classification towards a more performative or less stable form of Tangled Rope, or even a Piton if the underlying coherence was minimal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_coherence_vs_incoherence, conceptual, 'The conceptual coherence of the syncretic framework itself.').

omega_variable(
    kernel_reading_relationship_syncretic,
    'How does this ''syncretic_reading'' structurally relate to the ''partition_reading'' and ''incoherence_reading'' of the shinbutsu_ontological_commitment kernel?',
    'Analysis of the core axioms and their logical implications for the possibility of holding alternative readings within the same commitment framework.',
    'This reading''s assertion of a unified cosmological order directly forecloses the ''partition_reading'' (separate domains) and the ''incoherence_reading'' (no stable ontological commitment) within a single, coherent framework. If these were found to coexist, the syncretic reading''s internal consistency would be challenged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship_syncretic, conceptual, 'Documents the structural relationships between this reading and its siblings within the shinbutsu_ontological_commitment kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t700, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 700, 0.1).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 900, 0.15).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1200, 0.2).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1500, 0.25).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1868, 0.2).

% Extraction over time
narrative_ontology:measurement(shin_be_t700, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 700, 0.45).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 900, 0.55).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1868, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t700, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 700, 0.4).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 900, 0.55).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1500, 0.75).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1868, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
