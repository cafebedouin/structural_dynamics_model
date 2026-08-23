% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__hybrid_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Correct Latin — Hybrid Reading: Transmitted Core with Textual Correction
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   Since the professionalization of classical philology in the nineteenth
 *   century, written Latin has been governed by a correctness standard whose
 *   hybrid form holds the classical grammatical core as transmitted through
 *   medieval practice as the legitimate baseline, subject to correction in
 *   orthography and vocabulary by ancient textual evidence. The standard is
 *   administered by a professional apparatus — critical editions, learned
 *   journals, curricula, examinations — that decides which forms stand and
 *   which get emended. It coordinates written Latin across countries,
 *   denominations, and generations; it also concentrates correction authority
 *   in the philological establishment while each correction's costs
 *   (re-learning, re-editing, emendation of traditional forms) fall on
 *   learners, teachers, and the transmission institutions themselves. The
 *   epsilon referent is the standing arrangement under contest — the
 *   correctness standard as it actually operates — assessed by this reading's
 *   own lights, never the fully reconstructed or fully traditional standard
 *   the sibling readings would install. The claimed type and the metrics are
 *   authored independently: the claim states what this reading takes the
 *   arrangement structurally to be; the metrics state what it descriptively
 *   does.
 *
 * KEY AGENTS:
 *   - classical_philological_establishment: agenda-setter and primary beneficiary (institutional / identity_locked) — sets the standard, adjudicates corrections, collects the authority the correcting role confers
 *   - latin_learners: primary target (powerless / constrained) — acquire and re-acquire the standard, bear each revision, hold no seat in the deciding bodies
 *   - medieval_transmission_institutions: dual-positioned party (institutional / identity_locked) — their transmitted core is the baseline the standard protects; their surface forms are what the correcting apparatus emends
 *   - secondary_school_latin_teachers: secondary target (moderate / constrained) — transmit the standard and apply it in examinations, absorbing each revision twice
 *   - medievalist_tradition_scholars: excluded voice (organized / constrained) — consulted as witnesses to transmission, seated in no adjudicating body
 *   - latin_literary_community: coordination beneficiary (organized / constrained) — writes and reads Latin across borders on the strength of the shared standard, bearing diffuse conformity costs
 *   - intellectual_historians: analytical observer — sees the whole structure from outside the practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.48).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.35).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Correct Latin — Hybrid Reading: Transmitted Core with Textual Correction").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, '0293e3e8-2176-4ece-83bd-fd91aada56a5').
narrative_ontology:cs_kernel_codification('0293e3e8-2176-4ece-83bd-fd91aada56a5', distributed).
narrative_ontology:cs_authority_grounding('0293e3e8-2176-4ece-83bd-fd91aada56a5', expertise).
narrative_ontology:cs_interpretation_layer_present('0293e3e8-2176-4ece-83bd-fd91aada56a5').
narrative_ontology:cs_reading_relation('0293e3e8-2176-4ece-83bd-fd91aada56a5', correct_latin__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0293e3e8-2176-4ece-83bd-fd91aada56a5', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('0293e3e8-2176-4ece-83bd-fd91aada56a5', foundational, medieval_transmission_preserves_classical_core).
narrative_ontology:cs_axiom_status(medieval_transmission_preserves_classical_core, holdable).
narrative_ontology:cs_axiom_grounding('0293e3e8-2176-4ece-83bd-fd91aada56a5', medieval_transmission_preserves_classical_core, empirically_contingent).
narrative_ontology:cs_axiom('0293e3e8-2176-4ece-83bd-fd91aada56a5', foundational, textual_evidence_binds_surface_correction).
narrative_ontology:cs_axiom_status(textual_evidence_binds_surface_correction, holdable).
narrative_ontology:cs_axiom_grounding('0293e3e8-2176-4ece-83bd-fd91aada56a5', textual_evidence_binds_surface_correction, instrumental).
narrative_ontology:cs_axiom('0293e3e8-2176-4ece-83bd-fd91aada56a5', secondary, correction_bounded_by_transmitted_core).
narrative_ontology:cs_axiom_status(correction_bounded_by_transmitted_core, holdable).
narrative_ontology:cs_axiom_grounding('0293e3e8-2176-4ece-83bd-fd91aada56a5', correction_bounded_by_transmitted_core, conventional).
narrative_ontology:cs_reference_frame('0293e3e8-2176-4ece-83bd-fd91aada56a5', transmitted_core_bounded_correction).
narrative_ontology:cs_drift_state('0293e3e8-2176-4ece-83bd-fd91aada56a5', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0293e3e8-2176-4ece-83bd-fd91aada56a5', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, classical_philological_establishment).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, latin_literary_community).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, latin_learners).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, medieval_transmission_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, medieval_transmission_institutions).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, secondary_school_latin_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University classics faculties, critical-edition editors, journal boards, and the learned academies that maintain the standard. They decide which forms are correct, which emendations enter the edited corpus, and what examinations require. Each accepted correction extends the body of forms they adjudicate; their professional standing, careers, and institutions are built on being the ones who adjudicate. Leaving the role would mean leaving the discipline itself — there is no version of their profession that does not include setting the standard.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, classical_philological_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Students in seminaries, classics programs, and medieval-studies training who must acquire the standard as it stands and re-acquire parts of it when corrections shift it. They hold no seat in the bodies that decide corrections; their recourse is to choose a different field of study, at the cost of their chosen vocation.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, latin_learners, payer,
    powerless, biographical, constrained, global).

% The liturgical, curial, notarial, and monastic traditions that carried Latin through the centuries and whose transmitted forms are the baseline the standard protects. Their grammatical core is what the standard preserves; their orthographic and vocabular particularities are what the correcting apparatus emends. They cannot exit without dissolving the tradition itself — the transmitted form is not something they use but something they are.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medieval_transmission_institutions, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, medieval_transmission_institutions, beneficiary).

% Teachers who transmit the standard to new learners and apply it in examinations. They bear each revision twice — re-learning the corrected forms themselves and re-teaching them — while holding no seat in the editorial and curricular bodies that produce the revisions.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, secondary_school_latin_teachers, payer,
    moderate, biographical, constrained, national).

% Scholars whose expertise is the transmitted medieval corpus. They are consulted as witnesses to what the tradition transmitted, but they hold no adjudicating seat in the correction process; the bodies that decide which forms stand are staffed by classical philologists. Their objections to particular emendations are recorded in prefaces and reviews but do not bind the standard.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medievalist_tradition_scholars, excluded,
    organized, biographical, constrained, continental).

% The dispersed community of people who write and read Latin — across countries, denominations, and centuries. They benefit from a single mutually intelligible standard: a text composed in one tradition remains legible and evaluable in another. They bear the conformity cost diffusely, in the labor of writing to a standard none of them individually sets.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, latin_literary_community, beneficiary,
    organized, generational, constrained, global).

% Scholars who study how the correctness standard arose, changed, and was defended, without adjudicating what correct Latin is. They see the whole structure — the transmission, the corrections, the interests — from outside the practice.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, intellectual_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__hybrid_reading, classical_philological_establishment).
narrative_ontology:fixing_cost_class(correct_latin__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains one written Latin standard across a dispersed, multilingual, multi-generational community, so that texts remain mutually intelligible and evaluable across borders and centuries; the correction mechanism keeps that standard anchored to its classical source without requiring the community to re-derive the language from ancient texts.
% TRANSFER_FUNCTION: Moves correction authority — the standing right to decide which forms are correct — from writers, teachers, and traditional institutions to the professional philological establishment; moves revision labor outward, so that each accepted correction is paid for in re-learning and re-editing by learners, teachers, and editors rather than by those who accepted it.
% ABSENT_VOICES: Medievalist tradition scholars and the liturgical communities whose forms get emended would object that the correction process treats transmission as raw material rather than as a party; they sit outside the editorial boards and curricular committees where corrections are decided. Latin learners would object that each revision is a cost imposed without consultation; they are, by definition, not yet in the profession.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, edited editions would lose their adjudicating rule and diverge into house styles; seminaries and classics curricula would each teach a different target; cross-border Latin writing would fragment into regional and confessional practices, as it did before standardization. The arrangements of everyone who writes, teaches, examines, or edits Latin depend on the standard's existence.
% FOUNDING_PROBLEM: After Latin ceased to be anyone's mother tongue but remained the scholarly and liturgical common language, the question arose what form written Latin should take: the form carried by continuous use, the form recoverable from ancient texts, or some combination. The hybrid answer — transmitted practice as baseline, textual evidence as corrector — was the working settlement of that question.
% FOUNDING_PROBLEM_CORROBORATION: Liturgical authorities outside the philological establishment attest the need for a stable, teachable standard — they required one for the liturgy even while resisting particular emendations. Intellectual historians document the Renaissance-to-nineteenth-century dispute over barbarous versus classical Latin as a real governing problem, not a retrospective construction. The recurring preface question in every critical edition — why emend this form and retain that one — attests that the problem still governs practice.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48 at interval end): the authority gradient is real — the right to decide what is correct accrues to a professional class while each decision's costs are displaced onto those who must conform — but it is bounded by rule-governed correction (evidence, not preference, nominally drives each emendation) and by a coordination benefit shared across the whole community. Suppression is moderate-low (0.35): enforcement runs through editorial gatekeeping, examinations, and curricular mandates rather than prohibition; exit exists (write in the vernacular, celebrate the liturgy in the vernacular, join the living-Latin movement), and the enforcement series falls across the interval as Latin's institutional reach contracts. Theater is low-moderate (0.28): the standard does real coordinating work, but a growing share of its maintenance is ceremonial — correcting texts few people read, examining forms in examinations that gate few careers. Accessibility_collapse is low (0.30): the alternatives do not collapse — writing by transmitted practice and writing by reconstructionist principle both remain live practices, embodied in the sibling readings' continuing factions. Resistance is moderate (0.50): the arrangement is contested from both flanks at once, tradition-partisans resisting emendation of transmitted forms and purists resisting the legitimacy granted to transmission. The three tracked series run on one shared time grid; they smooth over recurring micro-cycles — each major edition or curricular reform sparks a resistance flare that decays into acceptance — which are visible only at finer time resolution than this grid captures.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the establishment's seat the arrangement is the discipline's own rational achievement: corrections are improvements, the authority it collects is earned expertise, and the whole looks like coordination it built and maintains. From the learner's seat the same structure is a moving target set by others — each revision a cost incurred without consent — and it reads as extraction through and through. From the transmission institutions' seat it is a bargain never negotiated: their core legitimized, their surface expropriated, their compliance assumed. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The establishment sits at the beneficiary end: it collects the arrangement's principal yield (correction authority) and is identity-locked into the correcting role — its profession is the adjudication. Learners sit at the target end: they bear the transfer (revision labor) with constrained exit, since leaving means abandoning their chosen vocation. Teachers sit near the target end, bearing enforcement labor and double revision costs at moderate power. The transmission institutions are the genuinely dual case: their payer role derives a high directionality, but the arrangement simultaneously subsidizes them — it is their transmitted core the standard protects and legitimates — so their true position is nearer symmetric than the victim-side derivation alone would place them; omega dual_position_directionality tracks this. The literary community sits low-mid: net coordination benefit, diffuse conformity cost. Learners' coalition power is structurally weak: their exposure is transient — each cohort exits the learner role within years — so the bearing class never accumulates the continuity needed to organize, while the establishment's seat is permanent; this asymmetry, not any single cohort's powerlessness, is what stabilizes the gradient. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by each seat's derived direction and the arrangement's global scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — what form written Latin should take once no one acquires it natively but many still need it — remains live, so this is not a dead-mandate case. But the mandate has narrowed: from governing a republic of letters' common language to maintaining a specialist standard for a contracting community. The classification earns its keep by refusing both mislabels: calling the arrangement pure coordination would erase the authority gradient on which the correcting class's standing rests; calling it pure extraction would erase the genuine, broadly shared benefit of one mutually intelligible written Latin and the rule-governed character of its corrections. The tangled form holds both. The falling enforcement series raises the one live obsolescence question — whether maintenance is outliving obedience — and the omega tracks it rather than presuming an answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the hybrid reading of the correct_latin kernel; what would the sibling readings change structurally if they governed instead?',
    'Compare against the sibling stories correct_latin__continuity_reading and correct_latin__discontinuity_reading, authored as separate constraints over the same kernel.',
    'Under the continuity reading the victim set collapses — transmitted forms are fully legitimate, no correction extraction exists, and the arrangement approaches pure coordination. Under the discontinuity reading the victim set expands — all medieval transmission becomes deviation, the establishment''s authority becomes total, and the arrangement hardens toward enforced reconstruction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this is one of three readings of the correct_latin kernel; sibling readings are separate constraint stories, not positions inside this one.').

omega_variable(
    correctable_surface_boundary,
    'Is the line between the protected grammatical core and the correctable surface (orthography, vocabulary) actually drawable, or does every correction decision secretly re-decide it?',
    'Corpus study of accepted versus rejected emendations across a century of critical editions: if accepted corrections sort cleanly into surface classes, the boundary is real; if the accepted set is explicable only by editorial preference, it is not.',
    'If the boundary is not drawable, the hybrid reading collapses toward one of its siblings — everything correctable resolves to the discontinuity pole, nothing correctable to the continuity pole — and the arrangement should be recomputed under whichever pole the evidence supports.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correctable_surface_boundary, conceptual, 'Whether the hybrid reading''s core/surface distinction is a stable structural fact or a post hoc rationalization of editorial practice.').

omega_variable(
    dual_position_directionality,
    'Do the medieval transmission institutions sit nearer the target end or the beneficiary end of the arrangement''s direction, given that the standard simultaneously legitimizes their grammatical core and emends their surface forms?',
    'Track whether transmission institutions adopt corrected forms in their own new writing and editions (net movement toward the standard) or maintain transmitted forms against corrections (net resistance).',
    'If net-adopting, the extraction concentrates on learners and the institutional seats compute near-symmetric, pulling the arrangement toward pure coordination at those seats; if net-resisting, the payer-side derivation stands and the arrangement is more asymmetric than the dual role suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_position_directionality, empirical, 'Directionality ambiguity for the dual-positioned transmission institutions; the structural derivation from their payer role may overstate their target position.').

omega_variable(
    enforcement_erosion_or_internalization,
    'Does the falling enforcement series record the standard losing its grip, or the standard no longer needing enforcement because conformity has been internalized by a smaller, self-selecting community?',
    'Compare conformity to the standard in unenforced contexts — living-Latin conversation, informal scholarly writing, online Latin communities — against conformity in enforced contexts such as examinations and refereed editions.',
    'If grip is being lost, the arrangement is drifting toward theatrical persistence — maintenance rituals outliving obedience — and should be tracked for piton-like decay; if internalized, the falling series records a stable equilibrium and the arrangement is healthier than its enforcement numbers suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_erosion_or_internalization, empirical, 'Whether declining suppression_requirement reflects decay of the arrangement or maturation beyond the need for enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t20, correct_latin__hybrid_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(corr_tr_t20, observed).
narrative_ontology:measurement(corr_tr_t40, correct_latin__hybrid_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(corr_tr_t40, observed).
narrative_ontology:measurement(corr_tr_t60, correct_latin__hybrid_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement_basis(corr_tr_t60, observed).
narrative_ontology:measurement(corr_tr_t80, correct_latin__hybrid_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement_basis(corr_tr_t80, observed).
narrative_ontology:measurement(corr_tr_t100, correct_latin__hybrid_reading, theater_ratio, 100, 0.29).
narrative_ontology:measurement_basis(corr_tr_t100, observed).
narrative_ontology:measurement(corr_tr_t120, correct_latin__hybrid_reading, theater_ratio, 120, 0.28).
narrative_ontology:measurement_basis(corr_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__hybrid_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t20, correct_latin__hybrid_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(corr_be_t20, observed).
narrative_ontology:measurement(corr_be_t40, correct_latin__hybrid_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement_basis(corr_be_t40, observed).
narrative_ontology:measurement(corr_be_t60, correct_latin__hybrid_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(corr_be_t60, observed).
narrative_ontology:measurement(corr_be_t80, correct_latin__hybrid_reading, base_extractiveness, 80, 0.51).
narrative_ontology:measurement_basis(corr_be_t80, observed).
narrative_ontology:measurement(corr_be_t100, correct_latin__hybrid_reading, base_extractiveness, 100, 0.5).
narrative_ontology:measurement_basis(corr_be_t100, observed).
narrative_ontology:measurement(corr_be_t120, correct_latin__hybrid_reading, base_extractiveness, 120, 0.48).
narrative_ontology:measurement_basis(corr_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__hybrid_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t20, correct_latin__hybrid_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(corr_su_t20, observed).
narrative_ontology:measurement(corr_su_t40, correct_latin__hybrid_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(corr_su_t40, observed).
narrative_ontology:measurement(corr_su_t60, correct_latin__hybrid_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(corr_su_t60, observed).
narrative_ontology:measurement(corr_su_t80, correct_latin__hybrid_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement_basis(corr_su_t80, observed).
narrative_ontology:measurement(corr_su_t100, correct_latin__hybrid_reading, suppression_requirement, 100, 0.39).
narrative_ontology:measurement_basis(corr_su_t100, observed).
narrative_ontology:measurement(corr_su_t120, correct_latin__hybrid_reading, suppression_requirement, 120, 0.35).
narrative_ontology:measurement_basis(corr_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' covers three structurally distinct standards, decomposed per the epsilon-invariance principle into three stories over one kernel: correct_latin__continuity_reading (transmitted practice as the whole standard — negligible correction extraction, victim set collapses), correct_latin__discontinuity_reading (ancient witness as the whole standard — total correction authority, victim set expands to all transmission), and this story, the hybrid reading (partial continuity with targeted reform — bounded extraction, bounded victim set). Each story has its own epsilon, beneficiaries, and victims. The continuity sibling is the practice-base cited as evidence that the hybrid baseline is credible; the discontinuity sibling is the pressure cited as justification for the correction apparatus. This story links both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
