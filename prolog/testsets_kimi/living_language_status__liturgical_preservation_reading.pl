% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Living Language Status via Liturgical Preservation
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical_preservation_reading of the
 *   living_language_status kernel: a language is considered living if its
 *   sacred texts are continuously recited, studied, and used in ritual. In
 *   the Hebrew context, this reading has been advanced by rabbinical
 *   authority to maintain that the language never died because liturgical
 *   continuity persisted, while delegitimizing secular modern Hebrew as
 *   desecration or mere utility. The kernel is contested by the
 *   native_generation_reading (which requires mother-tongue transmission) and
 *   the literary_continuity_reading (which requires productive
 *   literary/intellectual use). This story treats ONLY the liturgical reading
 *   as a clean Îµ-invariant constraint; sibling readings are separate files.
 *
 * KEY AGENTS:
 *   - rabbinical_authority: Primary agenda-setter and beneficiary (institutional/analytical) â maintains interpretive monopoly
 *   - secular_speech_community: Primary target (moderate/constrained) â daily speech delegitimized
 *   - liturgical_community: Coordinated beneficiary (organized/identity_locked) â preserves corpus through ritual
 *   - modernizing_intellectuals: Excluded voice (moderate/constrained) â literary production discounted
 *   - sociolinguistic_observers: Analytical observer (analytical/analytical) â documents usage gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.55).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.55).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status via Liturgical Preservation").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '3310ad71-5698-45b7-95b8-17a3a0d7bd0e').
narrative_ontology:cs_kernel_codification('3310ad71-5698-45b7-95b8-17a3a0d7bd0e', fixed_text).
narrative_ontology:cs_authority_grounding('3310ad71-5698-45b7-95b8-17a3a0d7bd0e', lineage).
narrative_ontology:cs_interpretation_layer_present('3310ad71-5698-45b7-95b8-17a3a0d7bd0e').
narrative_ontology:cs_reading_relation('3310ad71-5698-45b7-95b8-17a3a0d7bd0e', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('3310ad71-5698-45b7-95b8-17a3a0d7bd0e', living_language_status__literary_continuity_reading, influences).
narrative_ontology:cs_axiom('3310ad71-5698-45b7-95b8-17a3a0d7bd0e', foundational, liturgical_transmission_suffices_for_vitality).
narrative_ontology:cs_axiom_status(liturgical_transmission_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('3310ad71-5698-45b7-95b8-17a3a0d7bd0e', liturgical_transmission_suffices_for_vitality, conventional).
narrative_ontology:cs_axiom('3310ad71-5698-45b7-95b8-17a3a0d7bd0e', foundational, rabbinical_interpretive_authority_exclusive).
narrative_ontology:cs_axiom_status(rabbinical_interpretive_authority_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('3310ad71-5698-45b7-95b8-17a3a0d7bd0e', rabbinical_interpretive_authority_exclusive, theological).
narrative_ontology:cs_reference_frame('3310ad71-5698-45b7-95b8-17a3a0d7bd0e', classical_liturgical_authority_framework).
narrative_ontology:cs_drift_state('3310ad71-5698-45b7-95b8-17a3a0d7bd0e', post_statehood_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3310ad71-5698-45b7-95b8-17a3a0d7bd0e', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, liturgical_community).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the interpretation of sacred texts and the liturgical standards that define whether the language counts as living. Their authority derives from continuous textual transmission and ritual adjudication. They certify correct usage, train successors, and resist definitions that would grant living status to secular or literary speech alone.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, rabbinical_authority, beneficiary).

% Participates in daily and weekly ritual recitation, maintaining phonetic and grammatical standards through prayer and study. Their collective practice gives the liturgical corpus its continuous life, and their identity is constituted by this participation. They rely on rabbinical guidance for textual correctness.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, liturgical_community, beneficiary,
    organized, generational, identity_locked, global).

% Uses the language for daily communication, commerce, literature, and state administration. Their children acquire it natively in secular schools and media. Under this constraint, their speech is classified as utilitarian or desecratory rather than vitality-bearing, and their generational transmission does not count toward the language's living status.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, biographical, constrained, national).

% Produce new literature, journalism, and scholarship in the language, treating it as a medium for contemporary thought. They are structurally excluded from the vitality definition because their production is not liturgical, and their advocacy for literary or native-generation criteria is delegitimized.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, modernizing_intellectuals, excluded,
    moderate, biographical, constrained, national).

% Document and analyze the language's actual usage patterns across religious and secular domains. They measure native speaker counts, literacy rates, and functional domains, noting the gap between liturgical claims and sociolinguistic reality.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, sociolinguistic_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__liturgical_preservation_reading, rabbinical_authority).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a fixed sacred textual corpus across generations by embedding it in continuous ritual recitation and study, ensuring phonetic, grammatical, and interpretive fidelity without requiring territorial concentration or daily native speech.
% TRANSFER_FUNCTION: Moves definitional authority over linguistic vitality from secular daily speakers and modern literary producers to liturgical interpreters, transferring status and legitimacy to ritual domains while extracting recognition from productive speech communities.
% ABSENT_VOICES: Secular educators, modern literary figures, native-speaking parents, and sociolinguists who treat the language as a contemporary communicative tool are excluded from the living-language conversation; their practice is present in society but absent from the definitional framework.
% DISAPPEARANCE_RATIONALE: If the liturgical-sufficiency definition vanished, rabbinical authority would lose its monopoly on vitality-status, secular speech communities would gain legitimate standing, and the language would be reclassified as living through native and literary use â religious educational institutions and family law would rearrange around the new definitional center.
% FOUNDING_PROBLEM: Language death and assimilation during diaspora, when the community lacked territorial sovereignty and native speech transmission was threatened; liturgical continuity was the available preservation mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish diaspora attest the preservation problem was real and liturgical continuity was the functional response. Sociolinguists and secular Zionist educators attest the problem is substantially solved by modern statehood and native revival, corroborating that the liturgical-sufficiency claim now serves authority maintenance rather than survival.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as tangled_rope because it possesses both a genuine coordination function (preserving textual fidelity across diaspora) and asymmetric extraction (concentrating definitional authority in rabbinical interpreters while extracting legitimacy from secular speakers). Extractiveness is moderate (0.55) because the transfer is symbolic/status-based rather than material, but real. Suppression is moderate-high (0.55) because the constraint requires active enforcement of definitional boundaries against secular and literary alternatives. Theater ratio rises over time (0.48 at interval end) as the gap between liturgical definition and native-speaker reality widens, increasing performative maintenance. Accessibility collapse is moderate (0.45) because secular alternatives are socially present but normatively collapsed within the religious definitional framework. Resistance is moderate (0.50) because secular institutions and modernizers actively contest the definition.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinical seat, the constraint is continuity-preserving coordination without which the language would have dissolved. From the secular speech-community seat, the same structure is an active delegitimization mechanism that denies their generational transmission any vitality-status. The engine computes this divergence from structural data; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority sits near the beneficiary end (low d) because the constraint subsidizes their interpretive monopoly and institutional role. Liturgical community sits slightly above symmetric (moderate d) because they receive coordination benefits but pay identity-locked maintenance costs. Secular speech community and modernizing intellectuals sit near the target end (high d) because the constraint explicitly extracts legitimacy from their speech and writing, classifying it as non-vital. The derivation follows from beneficiary/victim declarations and exit options without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â diaspora language preservation â was genuine, but its status is contested: statehood and native revival have substantially altered conditions. The persistence of the liturgical-sufficiency definition after these conditions changed risks mandatrophy (coordination atrophied into performance). However, the theater ratio, while rising, has not crossed into piton territory because the liturgical coordination function remains structurally active and the beneficiary (rabbinical authority) is concentrated enough that this is not inertial drift but active maintenance. Classifying as tangled_rope captures the live coordination plus live extraction duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the liturgical_preservation_reading of the living_language_status kernel. How would classification change if the native_generation_reading were adopted instead?',
    'Comparative analysis of the three kernel readings'' stakeholder surfaces and epsilon profiles.',
    'Would shift from tangled_rope (coordination plus extraction) to a contested boundary between rope and mountain depending on empirical speaker counts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Position of this reading within the contested kernel.').

omega_variable(
    delegitimization_mechanism,
    'Is the suppression of the secular speech community structural (institutional control of education and religious courts) or internalized (secular speakers accepting their own delegitimization)?',
    'Survey of secular speech community self-classification and institutional boundary behavior.',
    'If internalized, effective extraction exceeds structural measure; if purely structural, resistance metric is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegitimization_mechanism, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    coordination_extraction_boundary,
    'Does liturgical preservation represent genuine coordination costs (necessary for textual fidelity) or is it cover for interpretive monopoly?',
    'Comparative analysis of textual preservation outcomes under rabbinical monopoly vs decentralized secular scholarly frameworks.',
    'If decentralized preservation is viable, the coordination component is smaller than claimed and extraction dominates; if rabbinical mediation is strictly necessary, coordination is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether coordination and extraction are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(livi_tr_t30, living_language_status__liturgical_preservation_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(livi_tr_t60, living_language_status__liturgical_preservation_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement(livi_tr_t90, living_language_status__liturgical_preservation_reading, theater_ratio, 90, 0.38).
narrative_ontology:measurement(livi_tr_t120, living_language_status__liturgical_preservation_reading, theater_ratio, 120, 0.44).
narrative_ontology:measurement(livi_tr_t150, living_language_status__liturgical_preservation_reading, theater_ratio, 150, 0.48).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(livi_be_t30, living_language_status__liturgical_preservation_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(livi_be_t60, living_language_status__liturgical_preservation_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(livi_be_t90, living_language_status__liturgical_preservation_reading, base_extractiveness, 90, 0.5).
narrative_ontology:measurement(livi_be_t120, living_language_status__liturgical_preservation_reading, base_extractiveness, 120, 0.53).
narrative_ontology:measurement(livi_be_t150, living_language_status__liturgical_preservation_reading, base_extractiveness, 150, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(livi_su_t30, living_language_status__liturgical_preservation_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(livi_su_t60, living_language_status__liturgical_preservation_reading, suppression_requirement, 60, 0.47).
narrative_ontology:measurement(livi_su_t90, living_language_status__liturgical_preservation_reading, suppression_requirement, 90, 0.53).
narrative_ontology:measurement(livi_su_t120, living_language_status__liturgical_preservation_reading, suppression_requirement, 120, 0.58).
narrative_ontology:measurement(livi_su_t150, living_language_status__liturgical_preservation_reading, suppression_requirement, 150, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'living language status' conflates three structurally distinct claims: liturgical sufficiency, native-generation necessity, and literary continuity. Each reading has a different epsilon, beneficiary/victim structure, and classification. They are modeled as separate constraints linked by network edges, not as one story with adjustable parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
