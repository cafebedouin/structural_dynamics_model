% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Hermeneutic of Continuity Reading of Vatican II
 *   domain: ecclesiological/institutional/hermeneutics
 *
 * SUMMARY:
 *   The Second Vatican Council (1962â1965) produced documents whose
 *   relationship to prior Catholic teaching was ambiguousâpastoral in tone,
 *   occasionally novel in formulation, and drafted by competing theological
 *   factions. The 'continuity reading' is the hermeneutic principle that
 *   Vatican II must be interpreted as organic development within an
 *   unchanging tradition: apparent novelties are actually explications of
 *   implicit prior teaching. This reading is one of three major readings of a
 *   contested kernel (alongside progressive rupture and traditionalist
 *   rupture readings). It functions as an institutional constraint enforced
 *   by the Magisterium through encyclicals, liturgical legislation, and
 *   doctrinal interventions. Its structural signature is low extractiveness
 *   on strict doctrinal propositions (where genuine conceptual continuity can
 *   be argued) but high extractiveness on liturgical and pastoral practice
 *   (where the gap between pre-conciliar and post-conciliar reality is widest
 *   and most visibly contested).
 *
 * KEY AGENTS:
 *   - magisterium: Agenda-setter (institutional/generational/constrained) â administers and enforces the hermeneutic of continuity
 *   - progressive_theologians: Primary target (moderate/biographical/constrained) â bears costs through suppressed interpretive freedom
 *   - traditionalist_communities: Secondary target (moderate/generational/trapped) â bears costs through restricted liturgical practice and forced assent
 *   - conservative_orthodox_theologians: Primary beneficiary (organized/generational/constrained) â gains institutional position from defending continuity
 *   - independent_historians: Analytical observer (moderate/biographical/analytical) â documents the construction of the continuity narrative from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.58).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.64).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.46).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Hermeneutic of Continuity Reading of Vatican II").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiological/institutional/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, 'efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef').
narrative_ontology:cs_kernel_codification('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', fixed_text).
narrative_ontology:cs_authority_grounding('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', lineage).
narrative_ontology:cs_interpretation_layer_present('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef').
narrative_ontology:cs_reading_relation('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_reading_relation('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_axiom('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', foundational, hermeneutic_continuity_as_magisterial_norm).
narrative_ontology:cs_axiom_status(hermeneutic_continuity_as_magisterial_norm, holdable).
narrative_ontology:cs_axiom_grounding('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', hermeneutic_continuity_as_magisterial_norm, theological).
narrative_ontology:cs_axiom('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', foundational, implicit_tradition_explication_principle).
narrative_ontology:cs_axiom_status(implicit_tradition_explication_principle, holdable).
narrative_ontology:cs_axiom_grounding('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', implicit_tradition_explication_principle, theological).
narrative_ontology:cs_reference_frame('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', organic_development_in_unchanging_tradition).
narrative_ontology:cs_drift_state('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', post_conciliar_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('efc5d6ff-9112-4d65-9b9d-7bc2f1cb3fef', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, conservative_orthodox_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Catholic Church's teaching authority and enforces the hermeneutic of continuity through encyclicals, doctrinal interventions, and liturgical legislation. Bears institutional responsibility for presenting Vatican II as organically developed from prior tradition. Its authority to adjudicate contested interpretations is strengthened when discontinuity readings are ruled out.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, magisterium, agenda_setter,
    institutional, generational, constrained, global).

% Catholic theologians who identify progressive or ruptural elements in Vatican II documents. Their teaching, publications, and academic appointments are constrained by the requirement to read the Council in continuity with tradition; they face institutional scrutiny or exclusion if they publicly advance interpretations that suggest doctrinal discontinuity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_theologians, payer,
    moderate, biographical, constrained, global).

% Catholics attached to pre-conciliar liturgical forms and theological expressions. They are required to accept Vatican II as a continuous development of tradition to remain in full communion; their preferred liturgical practices are restricted and they bear the costs of being labeled schismatic or disobedient when they question the conciliar continuity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_communities, payer,
    moderate, generational, trapped, global).

% Theologians and bishops whose careers and institutional influence are amplified by articulating and defending the continuity reading. They receive preferential access to teaching positions, publishing platforms, and magisterial consultation by aligning their work with the hermeneutic that presents apparent conciliar novelties as implicit prior teaching.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, conservative_orthodox_theologians, beneficiary,
    organized, generational, constrained, global).

% Secular and non-Catholic historians who study Vatican II without theological commitment to magisterial authority. They document tensions between conciliar texts and pre-conciliar teaching, the political dynamics of conciliar drafting, and the post-conciliar construction of the continuity narrative.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, independent_historians, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__continuity_reading, magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified interpretive framework that prevents doctrinal chaos and institutional fragmentation by reading all conciliar changes as organic developments of existing tradition, thereby coordinating a global religious body around a single authorized narrative.
% TRANSFER_FUNCTION: Moves interpretive authority from local bishops, theologians, and dissenting communities to the centralized Magisterium as the sole arbiter of what counts as implicit prior teaching; moves liturgical and pastoral options away from progressive and traditionalist communities toward a continuity-governed mean.
% ABSENT_VOICES: Progressive theologians who read rupture as authorized are marginally present but structurally muted; traditionalist communities who see error in conciliar texts are present but denied standing; local bishops with pastoral judgments that conflict with centralized continuity enforcement are excluded from the hermeneutic conversation.
% DISAPPEARANCE_RATIONALE: Without the continuity hermeneutic, the Church's magisterial authority would face a crisis of legitimacy; competing rupture readings would fragment institutional unity, and the conciliar texts would be read as potentially contradictory with prior tradition, forcing a reordering of Catholic self-understanding, liturgical practice, and ecumenical posture.
% FOUNDING_PROBLEM: How to interpret the ambiguous, pastoral texts of the Second Vatican Council in a way that preserves institutional unity and doctrinal stability without admitting formal rupture with prior tradition.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship outside the beneficiary set corroborates the pre-conciliar need for reform and the conciliar texts' ambiguity, but no independent corroboration exists for the claim that a continuity hermeneutic was the uniquely necessary solution; progressive theologians and traditionalist historians dispute that the problem was solved rather than suppressed.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the asymmetry noted in the structural delta: doctrinal continuity claims are relatively low-cost (implicit epsilon near 0.30) but liturgical/pastoral enforcement is high-cost (implicit epsilon near 0.80), averaging to moderate-high extraction. Suppression (0.64) tracks the active enforcement required to maintain continuity against both progressive and traditionalist dissent. Theater ratio (0.38) captures the performative dimensionâsubstantial scholarly and magisterial labor is devoted to showing continuity where participants intuitively experience change. Accessibility collapse (0.46) is moderate because rupture readings persist in academic and traditionalist niches despite marginalization. Resistance (0.52) is substantial and bidirectional: progressive theologians resist the suppression of aggiornamento, while traditionalists resist the liturgical implications of continuity.
 *
 * PERSPECTIVAL GAP:
 *   The Magisterium and conservative theologians experience this constraint as rope-like coordination: it preserves unity, prevents schism, and stabilizes Catholic identity across time. Progressive theologians and traditionalist communities experience it as extraction: their genuine theological and liturgical commitments are ruled illegitimate a priori by a hermeneutic they did not choose and cannot exit without leaving the institution. The engine computes this divergence from the same structural dataâbeneficiary/victim declarations and exit optionsâwithout adjudicating which seat is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (magisterium, conservative_orthodox_theologians) derive low directionality: the constraint subsidizes their authority and institutional position. Victims (progressive_theologians, traditionalist_communities) derive high directionality: the constraint extracts interpretive freedom and liturgical options from them. The magisterium's exit is constrained rather than arbitrage because its identity is fused to the guardianship of traditionâabandoning continuity would collapse its authority structure. Progressive theologians have constrained exit (can leave the academy or the Church but at high biographical cost); traditionalist communities are nearer trapped because their religious identity is bound to forms now restricted by the same continuity logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâinterpreting an ambiguous Council without institutional fragmentationâis genuinely live for a global religious body. The continuity reading solves this coordination problem, which prevents classifying it as a pure snare. However, the problem's status is contested: traditionalists argue the problem was modernism requiring condemnation, not continuity; progressives argue the problem was pre-conciliar rigidity requiring honest rupture. Because the founding problem is contested and the constraint's disappearance would cause world-rearrangement (the Church would face an interpretive crisis), the mandatrophy mismatch flag does not fire as simple zombie-piton. The constraint is a living tangled rope: it coordinates genuine institutional needs while asymmetrically extracting from dissenting seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_natural_or_constructed,
    'Is the continuity between Vatican II and prior tradition objectively present in the conciliar texts, or is it a retroactive hermeneutic constructed by the Magisterium to preserve institutional authority?',
    'Comparative historical-textual analysis of conciliar drafts and pre-conciliar magisterial documents to measure objective continuity versus interpretive construction.',
    'If purely constructed, the constraint is more extractive than its doctrinal domain suggests; if objectively present, the extraction is limited to suppressing clearly erroneous readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_natural_or_constructed, empirical, 'Whether continuity is latent in texts or imposed by authority').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of discontinuity readings primarily structural (excommunication, removal from posts, denied imprimaturs) or internalized (self-censorship, anticipatory conformity by theologians)?',
    'Survey of theologians on perceived constraints; post-exit behavior of theologians who leave institutional positions.',
    'If internalized, effective suppression exceeds the structural measure and the constraint functions more as snare-like cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    doctrinal_liturgical_epsilon_asymmetry,
    'Does the continuity reading genuinely exhibit lower extractiveness in doctrinal domains and higher extractiveness in liturgical-pastoral domains, or is this distinction a strategic framing that obscures uniform magisterial control?',
    'Comparative case studies of doctrinal versus liturgical enforcement under the continuity frame; measurement of sanctions and restrictions across both domains.',
    'If asymmetry is real, the constraint is legitimately tangled rope with domain-variable extraction; if artificial, uniform extraction is masked by doctrinal theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_liturgical_epsilon_asymmetry, conceptual, 'Whether doctrinal-liturgical extraction asymmetry is genuine or strategic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(vati_be_t50, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 60, 0.59).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(vati_su_t50, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 60, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
