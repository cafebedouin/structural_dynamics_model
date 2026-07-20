% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority: Composite Overdetermination Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The Second Vatican Council (1962-1965) produced sixteen documents
 *   officially presented as a coherent magisterial corpus. The
 *   composite-overdetermination reading treats this presentation as a
 *   constraint: a hermeneutical apparatus enforcing assent to ambiguous
 *   compromise formulations that simultaneously encode continuity and rupture
 *   ecclesiologies. Its coordination function is institutional (preserving
 *   communion across incompatible theological visions), while its extraction
 *   is the centralization of interpretive authority in the post-conciliar
 *   magisterium, which alone adjudicates licit readings. The 10-12% negative
 *   votes on key texts signal unresolved incompatibility structurally
 *   overridden rather than resolved. Traditionalists experience extraction
 *   through forced assent to rupture-leaning formulations; progressives
 *   through a hermeneutical ceiling; ordinary faithful through diffuse
 *   cognitive costs. The claim is tangled_rope because the ambiguity
 *   genuinely coordinated the council's supermajority, but the same structure
 *   now extracts through hermeneutical monopoly.
 *
 * KEY AGENTS:
 *   - post_conciliar_magisterium: agenda_setter (institutional/arbitrage) â controls interpretation and enforces licit readings
 *   - conciliar_drafters_and_bureaucrats: beneficiary (organized/mobile) â engineered ambiguous formulations and benefit from institutional stability
 *   - traditionalist_interpretive_community: payer (organized/identity_locked) â bears costs of forced assent to ambiguous or rupture-leaning texts
 *   - progressive_reform_community: payer (organized/constrained) â bears costs of hermeneutical ceiling preventing full reform
 *   - ordinary_faithful: payer (powerless/identity_locked) â bears diffuse cognitive and trust costs of unresolved ambiguity
 *   - academic_theologians: observer (moderate/analytical) â document textual ambiguity without controlling interpretation
 *   - conciliar_minority_dissenters: excluded (moderate/constrained) â rejected final texts, structurally overridden in the authoritative narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority: Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, '9d31c768-0846-4b7c-a5c0-fe88b7055ce9').
narrative_ontology:cs_kernel_codification('9d31c768-0846-4b7c-a5c0-fe88b7055ce9', fixed_text).
narrative_ontology:cs_authority_grounding('9d31c768-0846-4b7c-a5c0-fe88b7055ce9', lineage).
narrative_ontology:cs_interpretation_layer_present('9d31c768-0846-4b7c-a5c0-fe88b7055ce9').
narrative_ontology:cs_reading_relation('9d31c768-0846-4b7c-a5c0-fe88b7055ce9', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d31c768-0846-4b7c-a5c0-fe88b7055ce9', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('9d31c768-0846-4b7c-a5c0-fe88b7055ce9', foundational, conciliar_texts_encode_irreconcilable_ecclesiologies).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_irreconcilable_ecclesiologies, holdable).
narrative_ontology:cs_axiom_grounding('9d31c768-0846-4b7c-a5c0-fe88b7055ce9', conciliar_texts_encode_irreconcilable_ecclesiologies, empirically_contingent).
narrative_ontology:cs_axiom('9d31c768-0846-4b7c-a5c0-fe88b7055ce9', foundational, hermeneutical_control_supersedes_textual_clarity_as_authority_source).
narrative_ontology:cs_axiom_status(hermeneutical_control_supersedes_textual_clarity_as_authority_source, holdable).
narrative_ontology:cs_axiom_grounding('9d31c768-0846-4b7c-a5c0-fe88b7055ce9', hermeneutical_control_supersedes_textual_clarity_as_authority_source, instrumental).
narrative_ontology:cs_reference_frame('9d31c768-0846-4b7c-a5c0-fe88b7055ce9', irreducibly_ambiguous_corpus).
narrative_ontology:cs_drift_state('9d31c768-0846-4b7c-a5c0-fe88b7055ce9', contemporary_magisterial_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d31c768-0846-4b7c-a5c0-fe88b7055ce9', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_drafters_and_bureaucrats).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_interpretive_community).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reform_community).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, ordinary_faithful).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims ultimate interpretive authority over the conciliar texts, enforcing a hermeneutic that presents the council as a unified corpus. Benefits from expanded discretionary power because ambiguity allows it to adjudicate which readings are licit without revising the texts. Maintains institutional unity by controlling the meaning of compromise formulations.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_magisterium, agenda_setter,
    institutional, generational, arbitrage, global).

% Engineered ambiguous compromise formulations during the council to secure supermajority votes on contentious ecclesiological questions. Their institutional reputations and subsequent careers depend on the council being perceived as a success. They benefit from the ongoing need for interpretive mediation that their drafting strategy created.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_drafters_and_bureaucrats, beneficiary,
    organized, biographical, mobile, global).

% Reads key conciliar formulations as doctrinally ambiguous or rupture-leaning relative to prior magisterial teaching. Bears the cost of being required to assent to texts it experiences as incompatible with tradition, or of being marginalized and penalized for public dissent. Exit is blocked by deep identity-fusion with the pre-conciliar Church.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_interpretive_community, payer,
    organized, generational, identity_locked, global).

% Reads the conciliar texts as potentially mandating far-reaching reform but is frustrated by magisterial interpretations that fold ambiguous passages back into continuity. Bears the cost of a hermeneutical ceiling that prevents the full institutionalization of the rupture it believes the texts encode. Exit is constrained by investment in reform-from-within strategies and canonical dependencies.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, progressive_reform_community, payer,
    organized, generational, constrained, global).

% Receives doctrinal and liturgical formation from pastors subject to magisterial interpretation. Experiences confusion when continuity and rupture readings circulate simultaneously without authoritative resolution. Bears diffuse cognitive and trust costs. Religious identity is fused with family and community, making exit psychologically and socially costly.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, ordinary_faithful, payer,
    powerless, generational, identity_locked, global).

% Analyze conciliar redaction histories, voting records, and textual variants. Many document the overdetermined character of key passages but lack magisterial authority to make these readings normative. Their analytical seat permits identification of the structural ambiguity without controlling institutional interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, academic_theologians, observer,
    moderate, biographical, analytical, global).

% Theological minority at the council that rejected final texts on key votes (10-12% negative votes) because they recognized embedded incompatibility. Their objections were recorded in acta but structurally overridden by the compromise machinery. They are excluded from the authoritative narrative of a harmonious, univocal council.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, conciliar_minority_dissenters, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves global ecclesial communion by producing texts vague enough for theologically incompatible parties to assent to, thereby avoiding immediate schism during the council and maintaining a broad institutional umbrella afterward.
% TRANSFER_FUNCTION: Moves doctrinal clarity and interpretive autonomy from clarity-seeking communitiesâtraditionalists, progressives, and ordinary faithfulâto the post-conciliar magisterium and its hermeneutical bureaucracy, which gain expanded discretionary authority over meaning.
% ABSENT_VOICES: Theological minorities whose objections were overridden in final votes, and independent critical historians who document redactional incoherence but lack magisterial standing. Their exclusion is structural: the authoritative narrative requires a unified council, so dissenting testimonies are archived but not integrated into official hermeneutics.
% DISAPPEARANCE_RATIONALE: If the magisterial authority enforcing these ambiguous texts as a unified corpus vanished, the traditionalist and progressive readings would rapidly crystallize into separate institutional forms. The enforced ambiguity is the load-bearing element preventing open fragmentation; its disappearance would trigger immediate ecclesial rearrangement.
% FOUNDING_PROBLEM: The mid-twentieth-century Catholic Church faced a crisis of modernity, ecumenical pressure, and internal theological diversity that threatened institutional unity and legitimacy. The Council was convened to address this crisis without causing schism.
% FOUNDING_PROBLEM_CORROBORATION: Independent conciliar historians and sociologists of religion outside the magisterial beneficiary set attest the founding crisis of modernity and ecumenical pressure. The SSPX and progressive theologians attest the problem was either misdiagnosed or inadequately resolved by the ambiguous-compromise method. No party outside the benefiting set corroborates that the method successfully resolved the founding problem without creating new ones.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint transfers doctrinal clarity and interpretive autonomy from three victim groups to the magisterial center. Suppression is high (0.72) because the constraint's persistence requires active enforcement: dissenting readings (SSPX, unauthorized progressive theologies) are penalized or marginalized to maintain the univocal facade. Theater ratio is substantial (0.60) because a significant share of magisterial activity performs the unity of a corpus that the composite reading identifies as internally fractured. Accessibility collapse is moderate-high (0.65): for identity-locked agents (traditionalists, ordinary faithful), alternatives collapse under canonical and social pressure; for analytical observers, critical alternatives remain accessible. Resistance is moderate (0.58): traditionalist communities mount significant institutional resistance, while progressive resistance is more diffuse.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as necessary coordination preserving unity; the payer seats experience it as extraction of clarity and autonomy. The engine computes this divergence from identical structural data: same texts, opposite directionalities. Notably, the traditionalist and progressive seats share the payer role but for diametrically opposed theological reasons, producing a same-level lateral divergence where both ends of a polarity pay into the same center.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the post-conciliar magisterium (collects hermeneutical authority, directionality near 0.0) and the conciliar drafting bureaucracy (collects institutional stability, near 0.0). Victims are the three clarity-seeking communities (pay with assent and confusion, directionality near 1.0). Academic observers sit at analytical exit and derive no directionality. The excluded minority dissenters sit at high directionality but lack the structural power to convert it into effective resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by acknowledging the genuine coordination function: without ambiguous compromise, the council likely would have failed to achieve the supermajorities required for legitimacy, producing immediate schism. However, the same ambiguity that coordinated the council now extracts through hermeneutical monopoly. Mandatrophy is not declared because the founding problemâinstitutional unity under modernity pressureâremains live, but the solution mechanism has shifted from conciliar compromise to post-conciliar interpretive control. The Tangled Rope classification captures this precisely: the rope of unity and the extraction of centralized meaning-making are the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermined_encoding_or_unified_revelation,
    'Are the conciliar texts structurally overdetermined with incompatible ecclesiological visions, or does a deeper unity of revelation resolve apparent contradictions?',
    'Historical-redaction criticism of conciliar drafting records versus systematic-theological synthesis. If redaction analysis demonstrates irreconcilable redaction layers, the composite reading strengthens; if a successful hermeneutic of deeper unity is achieved, the continuity reading is favored.',
    'If overdetermined, the constraint is tangled_rope (coordination via ambiguity plus extraction via hermeneutical control). If unified, it tends toward rope or mountain (genuine doctrinal development with benign authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermined_encoding_or_unified_revelation, conceptual, 'Whether textual ambiguity is irreducible or resolvable by deeper unity').

omega_variable(
    magisterial_hermeneutic_as_coordination_or_extraction,
    'Does the post-conciliar magisterium''s interpretive authority over ambiguous texts serve a genuine coordination function (preserving communion across diversity) or an extractive one (centralizing power by controlling meaning)?',
    'Comparative analysis of interpretive authority scope before and after the council; measuring whether ambiguity expanded magisterial discretionary scope without corresponding accountability mechanisms.',
    'A pure coordination finding would shift classification toward rope; an expansion of discretionary scope without accountability shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_hermeneutic_as_coordination_or_extraction, empirical, 'Whether hermeneutical authority is coordination or extraction').

omega_variable(
    kernel_reading_incommensurability,
    'Can the continuity, rupture, and composite readings be adjudicated by shared criteria, or do they operate from incommensurable epistemic frameworks?',
    'Success or failure of ecumenical dialogue between the reading-communities; if no shared criteria exist, the kernel is irreducibly contested.',
    'If incommensurable, the constraint remains permanently contested (no resolution to mountain/rope); if adjudicable, one reading may achieve dominance and the constraint''s classification simplifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether sibling readings share adjudication criteria').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 40, 0.53).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 50, 0.57).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 60, 0.6).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vati_be_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(vati_be_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(vati_be_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(vati_be_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(vati_be_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(vati_be_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(vati_su_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(vati_su_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(vati_su_t30, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(vati_su_t40, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(vati_su_t50, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(vati_su_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
