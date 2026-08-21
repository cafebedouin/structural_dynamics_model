% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Quran 9:5 as Abrogating Universal Offensive Jihad
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   This constraint represents a specific, highly aggressive reading of
 *   Quranic verse 9:5, which asserts that this verse abrogates all prior
 *   peaceful verses, establishing universal offensive jihad as a standing
 *   legal obligation until polytheists submit or convert. This reading places
 *   all non-Muslims (and often dissenting Muslims) into the victim set,
 *   justifying first-strike violence and territorial expansion. The claimed
 *   type is 'snare' because the coordination story (unifying Muslims for a
 *   divine cause) is a cover for extreme extraction and suppression of
 *   alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.95).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.9).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.95).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Quran 9:5 as Abrogating Universal Offensive Jihad").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "islamic_jurisprudence/hermeneutics/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '3be9dfd4-6934-4b45-8828-f054c71c4432').
narrative_ontology:cs_kernel_codification('3be9dfd4-6934-4b45-8828-f054c71c4432', fixed_text).
narrative_ontology:cs_authority_grounding('3be9dfd4-6934-4b45-8828-f054c71c4432', lineage).
narrative_ontology:cs_interpretation_layer_present('3be9dfd4-6934-4b45-8828-f054c71c4432').
narrative_ontology:cs_reading_relation('3be9dfd4-6934-4b45-8828-f054c71c4432', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('3be9dfd4-6934-4b45-8828-f054c71c4432', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('3be9dfd4-6934-4b45-8828-f054c71c4432', foundational, abrogation_of_peaceful_verses).
narrative_ontology:cs_axiom_status(abrogation_of_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('3be9dfd4-6934-4b45-8828-f054c71c4432', abrogation_of_peaceful_verses, conventional).
narrative_ontology:cs_axiom('3be9dfd4-6934-4b45-8828-f054c71c4432', foundational, universal_offensive_jihad_obligation).
narrative_ontology:cs_axiom_status(universal_offensive_jihad_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3be9dfd4-6934-4b45-8828-f054c71c4432', universal_offensive_jihad_obligation, deontological).
narrative_ontology:cs_reference_frame('3be9dfd4-6934-4b45-8828-f054c71c4432', classical_jihad_doctrine).
narrative_ontology:cs_drift_state('3be9dfd4-6934-4b45-8828-f054c71c4432', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3be9dfd4-6934-4b45-8828-f054c71c4432', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, radical_clerics).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslims_in_conflict_zones).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, moderate_muslim_scholars).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, muslim_civilians_in_conflict_zones).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, doctrine_of_abrogation_nasikh).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, supremacy_of_islamic_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups interpret 9:5 as a divine mandate for universal offensive jihad, justifying violence against non-Muslims until conversion or submission. They actively enforce this interpretation through military action and ideological indoctrination, benefiting from the recruitment and territorial expansion it enables.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements, agenda_setter,
    organized, generational, identity_locked, global).

% These religious authorities propagate the abrogating_universal reading of 9:5, gaining immense influence and legitimacy within their communities. They benefit from the ideological power and financial support derived from their position as interpreters of divine command, even if they do not directly participate in violence.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, radical_clerics, beneficiary,
    powerful, biographical, constrained, regional).

% These individuals are directly targeted by groups adhering to this interpretation, facing demands for conversion, submission, or death. They bear the direct costs of violence, displacement, and loss of life, with extremely limited options for escape or resistance.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslims_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% These scholars actively resist the abrogating_universal interpretation, advocating for contextual or progressive readings. They face severe ideological pressure, threats, and accusations of apostasy from radical elements, risking their careers and lives for promoting alternative interpretations.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, moderate_muslim_scholars, payer,
    moderate, generational, constrained, global).

% While nominally beneficiaries of the 'expansion of Islam,' these civilians often bear the indirect costs of conflict, instability, and the radicalization of their societies. They may be forced to support jihadist movements, face internal repression, or become victims of retaliatory violence.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, muslim_civilians_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% These organizations document and condemn human rights abuses committed under the justification of this interpretation. They advocate for international law and protection of civilians, but their direct influence on the constraint's operation is limited.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, international_human_rights_organizations, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions of various jihadist groups and individuals under a unified theological justification for offensive warfare and territorial expansion, providing a clear mandate for their operations and a framework for recruitment.
% TRANSFER_FUNCTION: Transfers resources, territory, and human lives from non-Muslim populations (and often dissenting Muslim populations) to expansionist movements and their ideological leaders, under the guise of divine command.
% ABSENT_VOICES: The vast majority of non-Muslims, who are the primary targets, are entirely excluded from the interpretive discourse. Moderate Muslim voices advocating for peaceful coexistence are actively suppressed or marginalized within the interpretive framework that promotes this reading.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, the theological justification for many violent jihadist movements would collapse. While other factors might drive conflict, the specific divine mandate for universal offensive jihad would be removed, forcing a fundamental re-evaluation of their goals and methods, leading to a significant rearrangement of geopolitical and ideological landscapes.
% FOUNDING_PROBLEM: The problem of establishing the supremacy of Islam and dealing with perceived threats from polytheist and non-compliant communities in the early Islamic period.
% FOUNDING_PROBLEM_CORROBORATION: Adherents of this reading assert the problem is eternally live, citing ongoing 'threats' to Islam and the need for its global dominance. External corroboration is absent; moderate scholars and international observers dispute the premise, viewing it as a pretext for aggression rather than a response to a genuine, universally applicable problem.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) due to the direct authorization of violence, expropriation, and forced conversion/submission. Suppression is also very high (0.9) as this interpretation actively seeks to eliminate or subjugate all opposition, both external (non-Muslims) and internal (moderate Muslim voices). Theater ratio is low (0.1) because the function is directly implemented through violence and coercion, with little performative cover; the 'divine mandate' is taken literally by adherents. Accessibility collapse is high (0.85) as alternatives (peaceful coexistence, defensive jihad only) are explicitly rejected and suppressed. Resistance is high (0.7) due to the direct and violent opposition it generates from its targets.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adherents, this is a divinely ordained 'rope' for establishing justice and God's law. From the perspective of its victims and external observers, it is a 'snare' of extreme violence and oppression. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist jihadist movements and radical clerics are the primary beneficiaries and agenda-setters, as this interpretation grants them immense power, legitimacy, and resources (d near 0.0). Non-Muslims and moderate Muslim scholars are the primary targets/victims, bearing the full brunt of its extractive and suppressive force (d near 1.0). Muslim civilians in conflict zones are also victims, often caught in the crossfire or forced to comply (d near 1.0).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogation_doctrine_validity,
    'Is the doctrine of abrogation (nasikh) itself a valid hermeneutical principle, or is it a later jurisprudential construct used to resolve apparent contradictions?',
    'Deep historical and linguistic analysis of early Islamic texts and scholarly consensus on the development of hermeneutical principles.',
    'If abrogation is not a valid principle, the entire basis for this reading (that 9:5 supersedes peaceful verses) collapses, fundamentally altering its classification and reducing its legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_doctrine_validity, conceptual, 'Validity of the doctrine of abrogation as a hermeneutical tool.').

omega_variable(
    historical_context_vs_universal_command,
    'To what extent does Verse 9:5 refer to specific historical circumstances (treaty-breaking tribes in 7th-century Medina) versus establishing a universal, eternal legal command?',
    'Detailed historical-critical analysis of the verse''s revelation context, early commentaries, and the broader Quranic narrative.',
    'If the verse is primarily contextual, its universal application as a mandate for offensive jihad is undermined, shifting its classification towards a more defensive or time-bound interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_context_vs_universal_command, empirical, 'Contextual vs. universal application of Quran 9:5.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers, violence) or internalized (ideological indoctrination, fear of divine punishment)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., individuals leaving jihadist groups still adhere to the ideology), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true liberation more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qura_tr_t10, quran_9_5_scope__abrogating_universal, theater_ratio, 10, 0.12).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__abrogating_universal, theater_ratio, 20, 0.11).
narrative_ontology:measurement(qura_tr_t30, quran_9_5_scope__abrogating_universal, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(qura_be_t10, quran_9_5_scope__abrogating_universal, base_extractiveness, 10, 0.92).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__abrogating_universal, base_extractiveness, 20, 0.94).
narrative_ontology:measurement(qura_be_t30, quran_9_5_scope__abrogating_universal, base_extractiveness, 30, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(qura_su_t10, quran_9_5_scope__abrogating_universal, suppression_requirement, 10, 0.87).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__abrogating_universal, suppression_requirement, 20, 0.89).
narrative_ontology:measurement(qura_su_t30, quran_9_5_scope__abrogating_universal, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, identity_coordination).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, islamic_state_governance_model).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, sharia_law_implementation_in_conflict_zones).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
