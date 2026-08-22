% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Hebrew Vitality as Native Daily Generation (Native-Daily Reading)
 *   domain: sociolinguistic/cultural/political
 *
 * SUMMARY:
 *   This constraint instantiates the native_daily_reading of the contested
 *   hebrew_vitality kernel. Under this reading, Hebrew is authentically alive
 *   only when generated natively in daily secular life; ritual liturgical
 *   recitation is reclassified as preservation, not vitality. The arrangement
 *   was constructed by the Zionist state-building project to solve the
 *   coordination problem of nation-wide vernacular communication, but it
 *   asymmetrically extracts sacral authority and definitional power from
 *   traditional liturgical communities who had maintained Hebrew for
 *   centuries as a sacred tongue. The constraint requires active
 *   institutional enforcement (compulsory education, military language
 *   policy, lexical engineering) to sustain the vernacular against the
 *   gravitational pull of diaspora languages and liturgical exclusivity.
 *
 * KEY AGENTS:
 *   - State Language Apparatus (agenda_setter / institutional / constrained): administers the enforcement machinery of vernacular Hebrew through education, military, and legal institutions.
 *   - Zionist State-Building Project (beneficiary / institutional / constrained): captures territorial legitimacy and demographic cohesion from the native-daily vitality claim.
 *   - Modern Hebrew Speakers (beneficiary / organized / mobile): inherit native-speaker status and cultural prestige from the revived vernacular.
 *   - Traditional Liturgical Communities (payer / moderate / identity_locked): bear desacralization costs as their liturgical custody is downgraded to mere preservation.
 *   - Language Policy Analysts (observer / analytical / analytical): study the revival as an extreme case of language planning and note the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.58).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.62).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Hebrew Vitality as Native Daily Generation (Native-Daily Reading)").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistic/cultural/political").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, '9b535352-4d01-4705-96a8-6d7048f3e899').
narrative_ontology:cs_kernel_codification('9b535352-4d01-4705-96a8-6d7048f3e899', implicit).
narrative_ontology:cs_authority_grounding('9b535352-4d01-4705-96a8-6d7048f3e899', extraction).
narrative_ontology:cs_interpretation_layer_present('9b535352-4d01-4705-96a8-6d7048f3e899').
narrative_ontology:cs_reading_relation('9b535352-4d01-4705-96a8-6d7048f3e899', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('9b535352-4d01-4705-96a8-6d7048f3e899', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('9b535352-4d01-4705-96a8-6d7048f3e899', foundational, only_native_generation_constitutes_vitality).
narrative_ontology:cs_axiom_status(only_native_generation_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('9b535352-4d01-4705-96a8-6d7048f3e899', only_native_generation_constitutes_vitality, deontological).
narrative_ontology:cs_reference_frame('9b535352-4d01-4705-96a8-6d7048f3e899', native_speaker_sovereignty).
narrative_ontology:cs_drift_state('9b535352-4d01-4705-96a8-6d7048f3e899', contemporary_post_revival, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9b535352-4d01-4705-96a8-6d7048f3e899', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, modern_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, traditional_liturgical_communities).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, vernacular_nationhood_doctrine).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, language_revival_state_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the institutional machinery of Hebrew vernacularization: compulsory education in Hebrew, military language indoctrination, lexical modernization through the Academy, and enforcement of Hebrew-only public signage. Could theoretically shift to bilingual or liturgical-dominant policies but is institutionally fused to the Zionist nation-building project.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, state_language_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Derives territorial legitimacy and demographic cohesion from the claim that Hebrew has been restored as a native daily language. The constraint that only native generation counts as vitality vindicates the state's existence as the site of authentic Jewish linguistic renewal.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, beneficiary,
    institutional, generational, constrained, national).

% Inherit native-speaker status in a language that was recently reconstructed; their daily speech is treated as the living kernel of Hebrew, conferring cultural prestige and insider status relative to diaspora or liturgical users.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, modern_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% Maintain that Hebrew's sanctity is inseparable from liturgical and textual custody. The redefinition of Hebrew vitality as mundane native speech desacralizes their relationship to the language, downgrading centuries of unbroken liturgical transmission to mere preservation while their children are pressured into state vernacular Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, traditional_liturgical_communities, payer,
    moderate, generational, identity_locked, national).

% Study the Hebrew revival as a case of extreme language planning. They observe the tension between coordinated nation-building and the asymmetric costs borne by traditional custodians, noting that no other language has undergone comparable institutional resurrection as a native tongue.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, language_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:fixing_cost_class(hebrew_vitality__native_daily_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified national vernacular for a territorially concentrated but linguistically heterogeneous population, solving the collective-action problem of daily communication across diaspora-origin communities with no shared mother tongue.
% TRANSFER_FUNCTION: Moves cultural authority, educational resources, and definitional power over living Hebrew from liturgical custodians and diaspora traditions to state institutions and native vernacular speakers; also transfers the semantic burden of lexical expansion from religious texts to modern institutional coinage.
% ABSENT_VOICES: Traditional liturgical communities who regard Hebrew's sanctity as incompatible with mundane vernacularization, and diaspora Jewish communities for whom Hebrew was never intended as a daily tongue. They would argue that desacralization destroys the kernel but are structurally absent from the state-centric vitality discourse.
% DISAPPEARANCE_RATIONALE: If the constraint vanished and liturgical preservation were re-recognized as vitality, the Zionist state's claim to have uniquely revived Hebrew would collapse; Hebrew would likely bifurcate into a sacred liturgical register and a mundane Israeli vernacular no longer privileged as the sole authentic form; state education and military language policy would require fundamental redesign.
% FOUNDING_PROBLEM: The Jewish diaspora lacked a shared modern vernacular for territorial nation-building; Hebrew existed primarily as a liturgical, scholarly, and mercantile language, not as a native mother tongue for modern governance, agriculture, or daily life in a sovereign state.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and state institutions attest the problem was urgent and solved by vernacular revival. Critical sociolinguists and religious historians outside the beneficiary set attest that a modern vernacular could have been constructed without marginalizing liturgical continuity; no neutral corroborator exists â testimony is split along ideological lines.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint genuinely coordinates a national vernacular for a heterogeneous population while simultaneously extracting sacral authority and definitional legitimacy from liturgical custodians. Suppression is moderately high (0.62) because the native-daily definition must be actively enforced through compulsory schooling, military language induction, and the marginalization of alternative Jewish languages; without this enforcement the vernacular would not have displaced liturgical primacy. Theater ratio is moderate (0.45) because state rituals of Hebrew-only perform national vitality even as genuine native speech has become routine, creating a mixture of authentic function and ideological performance. Accessibility collapse is high (0.70) because once the native-daily definition is institutionalized, liturgical Hebrew is culturally read as preserved rather than living, and diaspora alternatives (Yiddish, Ladino) are pushed to the margins. Resistance is moderate (0.55) because religious communities and some diaspora networks actively contest the desacralization, though they are structurally overpowered by the state apparatus.
 *
 * PERSPECTIVAL GAP:
 *   From the state-building and modern-speaker seats, the constraint reads as a remarkable coordination achievement that solved a genuine collective-action problem (nation-wide communication). From the traditional liturgical seat, the identical arrangement operates as an extractive displacement of sacred authority that forces a choice between liturgical fidelity and state membership. The engine should compute divergent per-seat types: beneficiary seats near rope, payer seats near snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project and modern Hebrew speakers are structural beneficiaries (low d, effective extraction damped or inverted into subsidy). Traditional liturgical communities are structural targets (high d, effective extraction amplified). Their exit is identity_locked because Hebrew liturgy constitutes their communal selfhood; abandoning Hebrew is not a coherent exit, so they bear the constraint's costs continuously. The state language apparatus is an agenda_setter whose directionality is structurally moderate; it administers the extraction but is institutionally fused to the project rather than personally collecting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lack of a shared modern Jewish vernacular for territorial nation-building) is contested but partially resolved: Hebrew is now natively spoken by millions. However, the arrangement persists beyond pure coordination necessity because it continues to extract legitimacy for the state and marginalize liturgical alternatives that would undermine the native-only claim. Tangled_rope classification prevents misreading the genuine coordination (nation-building) as pure extraction, while also preventing the state's legitimacy narrative from masking the asymmetric costs borne by traditional custodians.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_desacralization_necessity,
    'Is the desacralization of liturgical Hebrew an inevitable byproduct of vernacular revival, or an independent extractive layer imposed by the state-building project?',
    'Comparative analysis of other language revivals (e.g., Irish, Welsh) to determine whether sacred-to-vernacular transitions necessarily alienate traditional custodians, versus whether Hebrew''s unique sacral status made the conflict avoidable.',
    'If avoidable, the constraint is more extractive than coordinate; if inevitable, the extraction is a necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_desacralization_necessity, empirical, 'Whether desacralization was structurally inevitable or politically contingent.').

omega_variable(
    kernel_reading_sibling_divergence,
    'This constraint is the native_daily_reading of the hebrew_vitality kernel; how would classification change if the liturgical_reading (ritual preservation = vitality) or hybrid_continuity_reading were adopted instead?',
    'Cross-reading comparison of beneficiary/victim structures: liturgical_reading would likely classify as rope or mountain with no victims; hybrid would likely be tangled_rope with milder extraction.',
    'The kernel is structurally underdetermined; the high extraction measured here is reading-specific and would not inhere to the kernel under alternate readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_divergence, conceptual, 'This constraint is one reading of a contested kernel; sibling readings produce different structural profiles.').

omega_variable(
    enforcement_naturalness,
    'Does the persistence of native Hebrew speech depend on ongoing state enforcement, or has it become self-sustaining through genuine intergenerational transmission?',
    'Natural experiment from periods of weakened state capacity or from comparison with diaspora Hebrew-speaking communities lacking state backing.',
    'If self-sustaining, the constraint''s active enforcement requirement is overstated and it may be shifting toward rope; if still enforcement-dependent, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_naturalness, empirical, 'Whether native Hebrew vitality is institutionally maintained or organically reproduced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__native_daily_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(hebr_tr_t10, hebrew_vitality__native_daily_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__native_daily_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(hebr_tr_t30, hebrew_vitality__native_daily_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__native_daily_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(hebr_tr_t50, hebrew_vitality__native_daily_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__native_daily_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hebr_be_t10, hebrew_vitality__native_daily_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__native_daily_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(hebr_be_t30, hebrew_vitality__native_daily_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__native_daily_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(hebr_be_t50, hebrew_vitality__native_daily_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__native_daily_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hebr_su_t10, hebrew_vitality__native_daily_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(hebr_su_t20, hebrew_vitality__native_daily_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(hebr_su_t30, hebrew_vitality__native_daily_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(hebr_su_t40, hebrew_vitality__native_daily_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(hebr_su_t50, hebrew_vitality__native_daily_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the hebrew_vitality kernel. The native_daily_reading isolates the claim that only native daily generation constitutes vitality, separating it from the liturgical_reading (where ritual preservation is vitality) and the hybrid_continuity_reading (where both are necessary). Each reading carries a distinct epsilon and stakeholder structure; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
