% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Reformist Contextual Reading of Dharmasastra
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint story models the reformist contextual reading of the
 *   Dharmasastra corpus, which holds that the texts encode a timeless ethical
 *   core (dharma as righteous conduct) separable from historically contingent
 *   social prescriptions, particularly varna/jati hierarchy. This reading
 *   constrains how Hindu legal and ethical texts are interpreted and applied:
 *   it preserves textual authority and institutional continuity while
 *   softening strict caste enforcement through reinterpretation (e.g.,
 *   spiritual stages). The constraint coordinates religious continuity and
 *   ethical adaptation, but asymmetrically extracts dignity and status from
 *   lower-caste communities by preserving symbolic hierarchy. It is one
 *   reading of a contested kernel; sibling readings include orthodox
 *   literalism (eternal hierarchy) and abolitionist rejection (wholesale
 *   abandonment).
 *
 * KEY AGENTS:
 *   - reformist_interpreters: Agenda-setter (institutional/constrained) â administers the interpretive separation of ethical core from caste prescription, preserving textual authority.
 *   - upper_caste_communities: Primary beneficiary (powerful/mobile) â retains symbolic status and social prestige through spiritual-stage reinterpretation.
 *   - lower_caste_communities: Primary payer (powerless/identity_locked) â bears the cost of persistent symbolic subordination despite reduced strict enforcement.
 *   - abolitionist_movements: Excluded voice (organized/mobile) â rejected from reformist spaces for insisting on total textual abandonment.
 *   - orthodox_literalists: Excluded voice (organized/constrained) â rejected for insisting on literal eternal hierarchy.
 *   - critical_historians: Analytical observer (analytical/analytical) â corroborates historical contingency without entering normative authority claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.48).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.42).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.48).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Reformist Contextual Reading of Dharmasastra").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, '855b6ee7-d4c8-40fd-9a4d-3587cb37d40e').
narrative_ontology:cs_kernel_codification('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e', fixed_text).
narrative_ontology:cs_authority_grounding('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e', lineage).
narrative_ontology:cs_interpretation_layer_present('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e').
narrative_ontology:cs_reading_relation('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e', dharmasastra_corpus__abolitionist_rejection, influences).
narrative_ontology:cs_axiom('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e', foundational, dharma_separable_from_varna).
narrative_ontology:cs_axiom_status(dharma_separable_from_varna, holdable).
narrative_ontology:cs_axiom_grounding('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e', dharma_separable_from_varna, theological).
narrative_ontology:cs_axiom('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e', foundational, textual_authority_adaptive).
narrative_ontology:cs_axiom_status(textual_authority_adaptive, holdable).
narrative_ontology:cs_axiom_grounding('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e', textual_authority_adaptive, conventional).
narrative_ontology:cs_reference_frame('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e', dharmasastra_ethical_core).
narrative_ontology:cs_drift_state('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e', contemporary_post_reform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('855b6ee7-d4c8-40fd-9a4d-3587cb37d40e', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_interpreters).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, upper_caste_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, lower_caste_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the interpretive framework that separates Dharmasastra's ethical core from historically contingent caste prescriptions. Control temples, religious education, and scriptural commentary institutions. Their authority and institutional position depend on preserving textual legitimacy while actively rejecting oppressive literal readings.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_interpreters, agenda_setter,
    institutional, generational, constrained, national).

% Retain social prestige and symbolic status through the reformist reinterpretation of varna as spiritual stages or psychological types rather than strict legal hierarchy. They can move between orthodox and reformist religious frames without losing caste-derived dignity claims.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, upper_caste_communities, beneficiary,
    powerful, generational, mobile, national).

% Subject to persistent symbolic subordination where caste position is reinterpreted as spiritual development or past-life karma rather than abolished. Receive ethical instruction about righteous conduct but remain positioned lower in the spiritual hierarchy, making exit psychologically and socially costly.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, lower_caste_communities, payer,
    powerless, biographical, identity_locked, national).

% Reject Dharmasastra entirely as irredeemably oppressive and advocate wholesale abandonment of the textual tradition. Excluded from reformist hermeneutic spaces because their presence would collapse the premise that textual authority can be preserved while discarding oppressive elements.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_movements, excluded,
    organized, generational, mobile, national).

% Insist on the eternal, revealed truth of varna/jati hierarchy and literal observance of Dharmasastra prescriptions. Excluded from reformist interpretive communities because their presence would deny the historical contingency and separability that the reformist reading depends upon.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalists, excluded,
    organized, generational, constrained, national).

% Analyze Dharmasastra as historically layered texts reflecting contingent social conditions. They corroborate the historical separability claim from outside the normative authority structure, neither endorsing textual preservation nor religious reform.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, critical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__reformist_contextual, diffuse).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__reformist_contextual, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves ethical and legal continuity across generations by distinguishing timeless moral principles (dharma as righteous conduct) from historically contingent social arrangements, allowing the tradition to adapt to colonial and modern challenges without wholesale abandonment.
% TRANSFER_FUNCTION: Moves deference and interpretive authority from the literal text to the reformist scholarly community, and moves symbolic status from lower-caste communities to upper-caste communities through spiritual-stage reinterpretation that preserves hierarchy in softened form.
% ABSENT_VOICES: Abolitionist movements that reject all textual authority, and orthodox literalists that insist on eternal varna hierarchy, are structurally excluded from reformist hermeneutic spaces; their inclusion would collapse the reformist separation of ethical core from caste prescription.
% DISAPPEARANCE_RATIONALE: If the reformist contextual reading vanished overnight, Hindu legal-ethical discourse would polarize: practitioners would be forced toward rigid orthodox literalism or toward abolitionist rejection, fundamentally rearranging the landscape of normative authority and caste politics.
% FOUNDING_PROBLEM: Maintaining Dharmasastra's ethical and legal relevance after colonial critique and social reform movements challenged caste hierarchy, without rejecting the textual tradition entirely.
% FOUNDING_PROBLEM_CORROBORATION: Social historians and post-colonial scholars outside the Hindu reformist tradition attest that the contextual reading emerged in response to colonial modernity. Reformist interpreters attest it from within the tradition. Orthodox literalists and abolitionists dispute that the problem is soluble within the textual framework.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is medium because the reformist reading genuinely reduces the severity of caste enforcement but preserves its symbolic architecture; the hierarchy becomes spiritual-stage rhetoric rather than legal prescription, still extracting status. Suppression (0.42) is moderate: overt coercion is reduced compared to orthodox literalism, but the identity_locked exit of lower-caste communities and the social exclusion of abolitionist and orthodox voices maintain the constraint. Theater_ratio (0.45) reflects the performative work of claiming to discard oppression while preserving the textual authority that authorizes it. Accessibility_collapse (0.50) is moderate because alternatives (abolition, orthodoxy) are visible but discursively marginalized within reformist spaces. Resistance (0.55) is moderate-to-high because both orthodox literalists and abolitionists actively contest this mediating position.
 *
 * PERSPECTIVAL GAP:
 *   The reformist_interpreter seat experiences this constraint as necessary coordination â preserving tradition, offering ethical guidance, preventing wholesale cultural loss. The lower_caste_communities seat experiences it as softened but persistent extraction â their dignity-claims are still subordinated to a framework authored by upper-caste interpreters. The upper_caste_communities seat experiences it as mobility (they can choose orthodox or reformist frames while retaining prestige). The engine will compute divergent types across these seats: the agenda-setter may compute toward rope or tangled_rope, the payer toward snare or tangled_rope, the beneficiary toward rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist interpreters and upper_caste_communities are declared beneficiaries; their directionality is pushed toward the subsidy end (low d), though the interpreters' constrained exit and institutional power place them closer to symmetric than the mobile upper castes. Lower_caste_communities are declared victims with identity_locked exit, pushing their directionality toward full target (high d). The exclusion of abolitionist and orthodox voices is structural suppression that sustains the reformist frame but does not directly feed extraction metrics for those parties since they are not the primary victims of the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy mislabeling by acknowledging the genuine coordination function (ethical continuity, cultural preservation) while naming the asymmetric victim set. Without the victim declaration, the constraint might compute as rope; without the beneficiary declaration, it might compute as snare. The tangled_rope claim captures the hybrid structure: it is not merely a coordination mechanism with side-effects (rope), nor is it pure extraction with a coordination cover (snare). The founding problem â maintaining textual relevance after colonial/modern challenges â is contested: abolitionists claim it is dead because the texts themselves are the problem, while reformists claim it is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    varna_historicity_vs_eternality,
    'Are varna/jati prescriptions in Dharmasastra genuinely historical and contingent social arrangements separable from dharma, or are they eternally ordained and integral to the ethical framework?',
    'Historical-critical philology of Dharmasastra manuscripts combined with sociology of religion measuring doctrinal change across sampradayas.',
    'If eternally ordained, the reformist reading is a misreading and effective extraction is higher than measured (false consciousness). If genuinely historical, the reformist reading is structurally sound and extraction is limited to symbolic residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(varna_historicity_vs_eternality, conceptual, 'Whether caste prescriptions are historically contingent or eternal divine law').

omega_variable(
    symbolic_hierarchy_extraction,
    'Does the persistence of symbolic hierarchy (caste reinterpreted as spiritual stages) continue to extract status, opportunity, and dignity from lower-caste communities despite reduced strict enforcement?',
    'Sociological measurement of caste-based social and economic outcomes in communities dominated by reformist, orthodox, and abolitionist frameworks respectively.',
    'If yes, the reformist reading remains tangled_rope (coordination plus extraction); if no, it approaches rope. If outcomes are worse than abolitionist-framed communities, the reading may function as moral cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_hierarchy_extraction, empirical, 'Whether symbolic hierarchy still extracts measurable status from lower castes').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression in the reformist frame structural (exclusion from interpretive authority and hermeneutic spaces) or internalized (lower-caste acceptance of spiritual-stage identity framing)?',
    'Post-exit trajectory analysis: whether individuals who leave reformist communities retain internalized hierarchy beliefs, and whether excluded voices (abolitionists, orthodox) face material barriers or merely discursive marginalization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the hierarchy with them after social exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in reformist Dharmasastra').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dhar_tr_t14, dharmasastra_corpus__reformist_contextual, theater_ratio, 14, 0.3).
narrative_ontology:measurement(dhar_tr_t28, dharmasastra_corpus__reformist_contextual, theater_ratio, 28, 0.38).
narrative_ontology:measurement(dhar_tr_t42, dharmasastra_corpus__reformist_contextual, theater_ratio, 42, 0.42).
narrative_ontology:measurement(dhar_tr_t56, dharmasastra_corpus__reformist_contextual, theater_ratio, 56, 0.45).
narrative_ontology:measurement(dhar_tr_t70, dharmasastra_corpus__reformist_contextual, theater_ratio, 70, 0.47).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(dhar_be_t14, dharmasastra_corpus__reformist_contextual, base_extractiveness, 14, 0.52).
narrative_ontology:measurement(dhar_be_t28, dharmasastra_corpus__reformist_contextual, base_extractiveness, 28, 0.48).
narrative_ontology:measurement(dhar_be_t42, dharmasastra_corpus__reformist_contextual, base_extractiveness, 42, 0.46).
narrative_ontology:measurement(dhar_be_t56, dharmasastra_corpus__reformist_contextual, base_extractiveness, 56, 0.47).
narrative_ontology:measurement(dhar_be_t70, dharmasastra_corpus__reformist_contextual, base_extractiveness, 70, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dhar_su_t14, dharmasastra_corpus__reformist_contextual, suppression_requirement, 14, 0.45).
narrative_ontology:measurement(dhar_su_t28, dharmasastra_corpus__reformist_contextual, suppression_requirement, 28, 0.4).
narrative_ontology:measurement(dhar_su_t42, dharmasastra_corpus__reformist_contextual, suppression_requirement, 42, 0.38).
narrative_ontology:measurement(dhar_su_t56, dharmasastra_corpus__reformist_contextual, suppression_requirement, 56, 0.39).
narrative_ontology:measurement(dhar_su_t70, dharmasastra_corpus__reformist_contextual, suppression_requirement, 70, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
