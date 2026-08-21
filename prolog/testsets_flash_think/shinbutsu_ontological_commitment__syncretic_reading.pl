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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Metaphysics (Syncretic Reading)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint represents the 'syncretic_reading' of the
 *   'shinbutsu_ontological_commitment' kernel, which posits that kami and
 *   buddhas are aspects of one unified cosmological order under honji-suijaku
 *   metaphysics. This framework, dominant for centuries in Japan, integrated
 *   Shinto and Buddhist practices by asserting Buddhist deities as the
 *   'original ground' (honji) and Shinto kami as their 'manifest traces'
 *   (suijaku). While presented as a harmonious synthesis, this reading
 *   highlights the resulting hierarchical integration that often led to
 *   Buddhist institutional dominance over Shinto shrines and traditions.
 *   Sibling readings include the 'partition_reading' (Shinto and Buddhism
 *   occupy separate domains) and the 'incoherence_reading' (no stable
 *   ontological commitment existed).
 *
 * KEY AGENTS:
 *   - Buddhist institutions: Primary agenda-setters and beneficiaries, defining and enforcing the metaphysical framework.
 *   - Shinto shrines and priests: Primary payers, losing autonomy and distinct identity.
 *   - Local kami worshippers: Bear the reinterpretation of their traditions.
 *   - Imperial court: Historically facilitated the framework for political stability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.75).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.8).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Metaphysics (Syncretic Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, 'f1292098-f972-4114-8518-719ac78ed570').
narrative_ontology:cs_kernel_codification('f1292098-f972-4114-8518-719ac78ed570', formalized).
narrative_ontology:cs_authority_grounding('f1292098-f972-4114-8518-719ac78ed570', lineage).
narrative_ontology:cs_interpretation_layer_present('f1292098-f972-4114-8518-719ac78ed570').
narrative_ontology:cs_reading_relation('f1292098-f972-4114-8518-719ac78ed570', shinbutsu_ontological_commitment__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('f1292098-f972-4114-8518-719ac78ed570', shinbutsu_ontological_commitment__incoherence_reading, forecloses).
narrative_ontology:cs_axiom('f1292098-f972-4114-8518-719ac78ed570', foundational, buddha_as_honji_kami_as_suijaku).
narrative_ontology:cs_axiom_status(buddha_as_honji_kami_as_suijaku, holdable).
narrative_ontology:cs_axiom_grounding('f1292098-f972-4114-8518-719ac78ed570', buddha_as_honji_kami_as_suijaku, theological).
narrative_ontology:cs_axiom('f1292098-f972-4114-8518-719ac78ed570', foundational, cosmological_unity_of_kami_and_buddhas).
narrative_ontology:cs_axiom_status(cosmological_unity_of_kami_and_buddhas, holdable).
narrative_ontology:cs_axiom_grounding('f1292098-f972-4114-8518-719ac78ed570', cosmological_unity_of_kami_and_buddhas, theological).
narrative_ontology:cs_reference_frame('f1292098-f972-4114-8518-719ac78ed570', honji_suijaku_orthodoxy).
narrative_ontology:cs_drift_state('f1292098-f972-4114-8518-719ac78ed570', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('f1292098-f972-4114-8518-719ac78ed570', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_clergy).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_shrines).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_priests).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, local_kami_worshippers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary proponents and beneficiaries of honji-suijaku, they defined the metaphysical framework, absorbed Shinto shrines into their administrative structures, and collected revenues. They actively enforced the doctrinal hierarchy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Gained prestige, roles (e.g., performing rites at Shinto shrines), and resources through the integration, solidifying their position within the religious landscape. Their careers were advanced by the syncretic framework.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_clergy, beneficiary,
    powerful, biographical, mobile, national).

% Often lost autonomy, distinct identity, and direct revenue streams, becoming subordinate to Buddhist temples. Their kami were reinterpreted as manifestations of Buddhist deities, diminishing their unique status.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_shrines, payer,
    organized, generational, constrained, local).

% Experienced a reduction in status and control over their own traditions, often performing rites under Buddhist supervision or having their roles subsumed by Buddhist monks. Their professional identity was constrained by the syncretic framework.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_priests, payer,
    moderate, biographical, constrained, local).

% Their traditional worship practices and understanding of kami were reinterpreted through a Buddhist lens, potentially alienating them from their indigenous beliefs or forcing them to accept a hierarchical view of their deities.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, local_kami_worshippers, payer,
    powerless, biographical, constrained, local).

% Historically, the court often endorsed or facilitated such syncretic frameworks for political stability and to consolidate religious authority, benefiting from a unified religious landscape even if not directly from the extraction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, imperial_court, agenda_setter,
    institutional, generational, constrained, national).

% Analyze the historical, theological, and institutional implications of honji-suijaku metaphysics, providing critical perspectives on its development and impact without being directly subject to its enforcement.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, scholarly_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutions).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__syncretic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a unified cosmological framework that integrated the indigenous worship of kami with the newly introduced Buddhist tradition, reducing potential conflict and offering a coherent worldview for a diverse religious landscape.
% TRANSFER_FUNCTION: Transferred institutional authority, doctrinal interpretive power, and often material resources (land, revenue) from Shinto shrines and practitioners to Buddhist institutions and clergy, solidifying Buddhist dominance.
% ABSENT_VOICES: Pure Shinto revivalists (e.g., Kokugaku scholars from the Edo period) who would argue for the distinct and superior nature of kami and the indigenous Japanese tradition, rejecting Buddhist interpretations and the honji-suijaku framework entirely.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku framework had never emerged or vanished, the historical and institutional landscape of Japanese religion would be fundamentally different. Shinto and Buddhist institutions would have developed along much more distinct lines, and the Meiji-era separation of kami and buddhas would have had a different historical context and impact.
% FOUNDING_PROBLEM: To reconcile the indigenous Japanese worship of kami with the newly introduced and powerful Buddhist tradition, providing a coherent theological and institutional framework for their coexistence and integration, thereby preventing religious conflict and facilitating the spread of Buddhism.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts from both Buddhist and Shinto traditions (though interpreted differently), and modern religious studies scholarship, corroborate the historical need for such reconciliation. Legislative-era documents and later academic analyses from outside the benefiting parties attest that the initial problem of integration was largely resolved long before the Meiji separation, and the framework persisted due to institutional inertia and benefit capture.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high because the honji-suijaku framework, while offering a unified cosmology, systematically channeled resources, authority, and interpretive power towards Buddhist institutions at the expense of Shinto autonomy. Suppression is high due to the active doctrinal and institutional enforcement required to maintain this hierarchical integration against local Shinto traditions and alternative interpretations. The theater ratio is moderate; while the metaphysical claim was genuinely held by many, the institutional practices of absorption and reinterpretation involved performative aspects to maintain the facade of unity while asserting Buddhist dominance. The historical measurements reflect the gradual entrenchment of this framework over centuries, leading to increased extraction and suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Buddhist institutions, the honji-suijaku framework was a successful and natural integration, a 'rope' that brought order and coherence. From the perspective of Shinto shrines and priests, it was a 'snare' or 'tangled rope' that systematically extracted their autonomy and resources under the guise of unity. The engine's computation of per-seat classifications will reflect this divergence based on the declared structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutions and clergy are clear beneficiaries, gaining authority and resources (low d). Shinto shrines, priests, and local worshippers are targets, bearing the costs of reinterpretation and loss of autonomy (high d). The Imperial Court, while an agenda-setter, also benefited from the stability provided by the unified religious system, placing them closer to the beneficiary end, though with some constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to reconcile two distinct religious traditions. While this initial problem was largely 'dead' by the later periods of its dominance, the framework persisted due to the concentrated benefits accruing to Buddhist institutions. The classification as a Tangled Rope prevents mislabeling this as pure coordination, highlighting the asymmetric extraction that continued long after the initial coordination problem was resolved. The Meiji-era state-mandated separation (Shinbutsu-bunri) effectively resolved the mandatrophy by dismantling the institutional integration, though the doctrinal legacy remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_syncretic_reading,
    'Is this constraint a genuine ''syncretic_reading'' of the shinbutsu_ontological_commitment kernel, or is it better understood as a ''partition_reading'' or ''incoherence_reading''?',
    'Analysis of primary historical and theological texts, focusing on explicit statements of ontological unity versus institutional coexistence or pragmatic incoherence. The ''syncretic_reading'' requires explicit claims of kami as manifestations of buddhas.',
    'If reclassified as a ''partition_reading'', the extractiveness and suppression would be lower, as the domains would be seen as separate rather than hierarchically integrated. If reclassified as an ''incoherence_reading'', the extraction would appear more arbitrary and less justified by a coherent metaphysical framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_syncretic_reading, conceptual, 'This constraint is the ''syncretic_reading'' of the ''shinbutsu_ontological_commitment'' kernel, asserting ontological unity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily doctrinal (internalized belief in Buddhist superiority) or institutional (Buddhist control over shrines and resources)?',
    'Post-Meiji Restoration analysis: if the hierarchical interpretation persisted in local practice despite institutional separation, it suggests a stronger internalized component. If it rapidly dissolved, it points to primarily structural suppression.',
    'If internalized, the constraint''s effective suppression was higher than the structural measure suggests, as the targets carried the suppression within their belief systems. If primarily institutional, the removal of external barriers would have been more immediately effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious belief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 794, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t794, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 794, 0.2).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 900, 0.25).
narrative_ontology:measurement(shin_tr_t1100, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1100, 0.3).
narrative_ontology:measurement(shin_tr_t1400, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1400, 0.35).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1868, 0.4).

% Extraction over time
narrative_ontology:measurement(shin_be_t794, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 794, 0.4).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 900, 0.5).
narrative_ontology:measurement(shin_be_t1100, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1100, 0.6).
narrative_ontology:measurement(shin_be_t1400, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1400, 0.7).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1868, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t794, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 794, 0.5).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 900, 0.6).
narrative_ontology:measurement(shin_su_t1100, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1100, 0.7).
narrative_ontology:measurement(shin_su_t1400, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1400, 0.75).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1868, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_ontological_commitment' kernel, each representing a distinct structural claim about the relationship between kami and buddhas in Japanese religious history.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
