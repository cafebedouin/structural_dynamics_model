% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__dynamic_equivalence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Reading of Biblical Source Text Authority
 *   domain: religious/textual/hermeneutical
 *
 * SUMMARY:
 *   The dynamic equivalence reading claims that communicative effectiveness
 *   in the target language is the primary criterion for biblical translation;
 *   source-language structure is subordinated to intelligibility and pastoral
 *   mission. This is ONE reading of the contested kernel: biblical
 *   source-text authority. Sibling readings are formal equivalence (source
 *   structure is primary; reader bears responsibility for understanding
 *   through teaching) and critical reconstruction (textual establishment
 *   precedes any translation choice). This reading has dominated major
 *   translation committees and missionary translation bodies since the
 *   mid-20th century, producing translations like the NIV, NCV, and others
 *   that optimize for comprehension. Word-study scholars and
 *   formal-equivalence advocates see this dominance as extracting precision
 *   and authority structure from the text itself. The constraint is CLAIMED
 *   as tangled rope (real coordination benefit for lay accessibility + active
 *   enforcement against competing readings) and the metrics describe moderate
 *   extraction (0.62) with moderate suppression (0.48) — the engine will
 *   compute per-seat divergence; do not reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - dynamic_equivalence_translators: institutional agenda-setters who enforce the reading via translation committees and curriculum
 *   - lay_congregational_readers: beneficiaries receiving accessible translations without bearing direct cost
 *   - missionary_translation_contexts: beneficiaries in regions where formal equivalence is technically impossible
 *   - word_study_scholars: victims losing morphological transparency for detailed biblical research
 *   - historical_linguists: victims losing structural data necessary for linguistic reconstruction
 *   - formal_equivalence_advocates: excluded from translation committee decisions despite published alternative methodology
 *   - translation_governance_bodies: institutional observers adjudicating methodology across programs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.62).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.48).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Reading of Biblical Source Text Authority").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious/textual/hermeneutical").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, '3022bc5c-bafa-4dd7-a419-151220a07198').
narrative_ontology:cs_kernel_codification('3022bc5c-bafa-4dd7-a419-151220a07198', fixed_text).
narrative_ontology:cs_authority_grounding('3022bc5c-bafa-4dd7-a419-151220a07198', lineage).
narrative_ontology:cs_interpretation_layer_present('3022bc5c-bafa-4dd7-a419-151220a07198').
narrative_ontology:cs_reading_relation('3022bc5c-bafa-4dd7-a419-151220a07198', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('3022bc5c-bafa-4dd7-a419-151220a07198', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('3022bc5c-bafa-4dd7-a419-151220a07198', foundational, meaning_transfer_primary).
narrative_ontology:cs_axiom_status(meaning_transfer_primary, holdable).
narrative_ontology:cs_axiom_grounding('3022bc5c-bafa-4dd7-a419-151220a07198', meaning_transfer_primary, instrumental).
narrative_ontology:cs_axiom('3022bc5c-bafa-4dd7-a419-151220a07198', foundational, structure_subordinate_to_intelligibility).
narrative_ontology:cs_axiom_status(structure_subordinate_to_intelligibility, holdable).
narrative_ontology:cs_axiom_grounding('3022bc5c-bafa-4dd7-a419-151220a07198', structure_subordinate_to_intelligibility, instrumental).
narrative_ontology:cs_reference_frame('3022bc5c-bafa-4dd7-a419-151220a07198', accessibility_centered_translation).
narrative_ontology:cs_drift_state('3022bc5c-bafa-4dd7-a419-151220a07198', contemporary_scholarly_critique, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3022bc5c-bafa-4dd7-a419-151220a07198', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_congregational_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_translation_contexts).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, word_study_scholars).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, historical_linguists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Translation committees and mission organizations that adopt dynamic equivalence methodology. They set standards for translation choice, justify subordination of structural fidelity to communicative effect, and enforce the reading through translation publication, seminary curriculum, and pastoral training. Benefit from the reading's alignment with practical mission outcomes and broad audience accessibility.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, dynamic_equivalence_translators, agenda_setter,
    institutional, generational, constrained, global).

% Church members, congregational Bible study groups, and devotional readers. Receive translations optimized for comprehension and immediate spiritual application. The reading privileges their access and understanding over scholarly precision; dynamic translation embeds interpretive choices that facilitate pastoral use. Face no direct cost from the constraint; benefit from readability and relevance.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_congregational_readers, beneficiary,
    organized, biographical, mobile, global).

% Missionary organizations translating into languages with limited existing scholarship and no prior translation tradition. Depend on dynamic equivalence methodology because formal structural correspondence is impossible in many language pairs; the reading legitimizes translation choices that prioritize meaning transfer over form matching. Cannot easily exit without abandoning translation work.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_translation_contexts, beneficiary,
    moderate, biographical, constrained, regional).

% Academic Biblical scholars, lexicographers, and researchers performing detailed word studies. The dynamic equivalence reading deprives them of consistent morphological, syntactic, and lexical equivalence in modern translations. They must maintain access to source texts or formal-equivalence translations to conduct rigorous analysis. Trapped between the vernacular translations in pastoral use and the scholarly apparatus.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, word_study_scholars, payer,
    powerful, generational, constrained, global).

% Researchers studying biblical Hebrew, Aramaic, and Koine Greek; textual critics; and scholars examining diachronic language change. Dynamic equivalence translation obscures the historical record of source-language forms, making it difficult to reconstruct linguistic features from translation patterns. Their research methodologies depend on structural predictability that the reading subordinates.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, historical_linguists, payer,
    powerful, generational, constrained, global).

% Translation scholars and theologians who argue that source-language structure carries theological significance and cannot be subordinated without loss. They publish their critique, produce alternative translations, and advocate for different methodology, but the institutional weight of dynamic equivalence translation adoption (major translation committees, missionary agencies) marginalizes their position in practical translation decisions.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_advocates, excluded,
    powerful, generational, constrained, global).

% Textual critics and historical Jesus scholars who prioritize reconstructing the earliest recoverable text over either formal or dynamic translation choices. They view both formal and dynamic equivalence as post-hoc choices made after the textual basis is established. They observe and analyze the constraint from outside it, supporting neither reading.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, critical_reconstructive_scholars, excluded,
    analytical, generational, analytical, global).

% United Bible Societies, denominational publishing bodies, and ecumenical translation committees. Adjudicate methodology choices, certify translations, and shape the distribution of vernacular texts worldwide. Can observe the constraint's operation across multiple reading communities and enforce consistency within their own translation programs.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, translation_governance_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__dynamic_equivalence_reading, dynamic_equivalence_translators).
narrative_ontology:fixing_cost_class(biblical_source_text__dynamic_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces intelligible, theologically coherent translations accessible to non-specialist readers across diverse cultural contexts; solves the problem of how to transmit biblical meaning when direct structural correspondence is semantically impossible or obscuring. Enables missionary translation where no prior scholarly tradition exists.
% TRANSFER_FUNCTION: Transfers morphological precision, etymological transparency, and word-study granularity from the translation product, moving those properties away from lay readers and toward scholars; simultaneously transfers interpretive authority from source structure to translator judgment, moving decision-making power to institutional translation committees. Lay readers receive readability; scholars lose reliable access to structural patterns.
% ABSENT_VOICES: Formal equivalence advocates object that the reading subordinates theological significance carried by source structure; critical reconstructionists object that the reading defers textual establishment to adopt translation philosophy. Neither has formal seat in the major translation committees that enforce dynamic equivalence. Communities in developing-world translation contexts whose languages lack scholarly infrastructure cannot meaningfully contest the reading's imposition as the de facto standard.
% DISAPPEARANCE_RATIONALE: If the dynamic equivalence reading and its enforcement vanished, major translation committees would revert to formal-equivalence or develop hybrid methodologies; scholarly word-study tools would gain reliable morphological correspondence; lay readers would face less accessible translations unless new pedagogical structures emerged. Missionary translation programs would fragment by region-specific methodology rather than adopting a unified standard.
% FOUNDING_PROBLEM: Early Bible translations produced structural English that obscured meaning for common readers; formal scholarly apparatus was unavailable in missionary contexts; oral cultures required communication-focused translation rather than word-for-word rendering. The problem: how to make biblical meaning accessible when source structure is linguistically foreign.
% FOUNDING_PROBLEM_CORROBORATION: Translation practitioners and missionary organizations attest the problem is live and justify dynamic equivalence by its outcomes. Scholars of translation theory, textual critics, and word-study practitioners attest the founding problem was methodologically confused — that accessibility and structural fidelity are not inherently opposed — and that the reading persists as an institutional standard decoupled from its original justification. Academic translation theory and comparative linguistics (cited in peer-reviewed translation studies and historical-linguistic scholarship) document that the binary accessibility/fidelity framing was an artifact of 20th-century translation philosophy, not a structural requirement.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__dynamic_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__dynamic_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.62 by interval end (from 0.38 at start) because the reading's enforcement through translation committee gatekeeping progressively excludes formal equivalence as a live option in major translations, forcing scholars into dual-apparatus work (source text + academic translations) while lay readers benefit from dominant accessibility. Theater rises to 0.31 because the reading increasingly justifies methodological choices through accessibility metrics while simultaneously applying those metrics selectively — formal equivalence is rejected not for failure at accessibility but for methodological principle. Suppression stays moderate (0.48) because formal-equivalence and critical-reconstructive readers can still publish, teach, and advocate, but their materials occupy institutional margins. Resistance remains high (0.71) because scholars mount continuous counter-argument, and formal-equivalence translators produce alternative translations (ESV, NKJV, NASB) that compete for adoption. The measurement series is shared across all three metrics on a single time grid (0, 8, 16, 24, 32, 40 years from reading's institutional consolidation); extractiveness and suppression show monotonic rise over 32 years, then stabilize; theater plateaus at institutional saturation.
 *
 * PERSPECTIVAL GAP:
 *   The institutional translator seat and the word-study scholar seat compute radically different classifications from the same structural data. From the translator's perspective, the constraint is genuine coordination (provides accessible Bibles, enables worldwide mission, solves real language-pairing impossibilities) with minor overhead for scholarly precision — rope or tangled_rope from this seat. From the scholar's perspective, the constraint is asymmetric extraction (loses structural authority, imposes interpretive choices unilaterally, forces reliance on source text and specialty editions) maintained by institutional gatekeeping — tangled_rope or snare from this seat. The formal-equivalence advocate sits between: they see both the coordination value AND the extraction, making them tangled-rope advocates, but their formal-equivalence methodology is excluded from the major institutional implementation of the constraint. The engine computes per-seat type; the divergence IS the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional translators that set the agenda for dynamic equivalence receive low directionality (full beneficiaries: d ≈ 0.1–0.2) because they control the rules and collect institutional authority. Lay readers receive near-zero d (full beneficiaries) because they benefit without running the constraint or bearing costs. Missionary contexts are beneficiaries (d ≈ 0.2–0.3) but constrained in exit: they benefit from dynamic equivalence but cannot unilaterally adopt formal methods because they depend on institutional translation standard-setting. Scholars receive high d (0.75–0.95, targets) because they cannot opt out of the constraint's effects — they must maintain parallel scholarly apparatus to study morphological detail that dominant translations obscure. Formal-equivalence advocates face d ≈ 0.7–0.8 (targets, institutional pressure) but retain exit via publishing alternative translations, which keeps d below the full-target ceiling. The engine derives these from the beneficiary/victim declarations and exit_options authored for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The dynamic equivalence reading's founding problem was accessibility for lay congregations and missionary translation without prior scholarly apparatus. That problem is live — vernacular readers still benefit from comprehensible Bibles. But a secondary problem has emerged: the reading has become institutional orthodoxy defended not by its original accessibility justification but by methodological assertion and committee gatekeeping. Word-study scholars and formal-equivalence translators argue the founding problem was confused — that accessibility and structural fidelity are not inherently opposed — and that the constraint persists as institutional path-dependence rather than responding to renewed justification. The theater_ratio rise from 0.08 to 0.31 models this: more of the constraint's operation is devoted to institutional boundary maintenance (excluding formal-equivalence translators from major projects, controlling seminary curriculum) than to solving the original accessibility problem. The constraint has not achieved mandatrophy (the founding problem is still live), but it carries mandatrophy risk — if scholars establish that dynamic equivalence produces no measurable accessibility gain over hybrid or formal methods, the institutional enforcement would appear theatrical. The two-reading split (dynamic vs. formal) should dissolve if empirical evidence shows no performance gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the dynamic equivalence reading a legitimate interpretive stance on the biblical source-text kernel, or does it dissolve the kernel by subordinating source structure to translator judgment?',
    'Examine whether dynamic equivalence can maintain a stable reference point to the source text (even while prioritizing meaning transfer) or whether the translator''s target-language intuition becomes the de facto kernel.',
    'If the reading can maintain stable reference, it is a coherent alternative reading; if translator judgment becomes unbounded, the reading forecloses the formal-equivalence and critical-reconstructive readings by replacing the kernel itself. Terminal classification would shift from tangled_rope to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether dynamic equivalence maintains the source text as a binding constraint or dissolves it into translator discretion.').

omega_variable(
    extractiveness_measurement_basis,
    'Is the authored extractiveness (0.62) measuring the cognitive/linguistic precision lost to lay readers, or the structural-hermeneutical precision lost to scholars?',
    'Separate measurement of readability gain for lay audiences from precision loss for scholars. If both are large, the constraint is asymmetric extraction (tangled rope); if one dominates, the classification simplifies.',
    'High extractiveness for scholars + high beneficiary gain for lay readers supports tangled_rope. If extractiveness is primarily performative (small actual loss, large claimed loss), the constraint might be piton instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_measurement_basis, empirical, 'What the measured extractiveness is actually extracting and from whom.').

omega_variable(
    missionary_exit_option_ambiguity,
    'Are missionary translation organizations genuinely trapped by dynamic equivalence, or do they adopt it because it solves authentic problems in their contexts?',
    'Case study of missionary translation decisions in language communities where dynamic equivalence was not pre-selected: what methodology emerges when the constraint is not imposed?',
    'If genuinely chosen, missionaries are not victims but beneficiaries (exit_options upgrades to arbitrage or mobile). If externally imposed via denominational publishing gatekeeping, they remain trapped payers. The role and directionality of missionary_translation_contexts would shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(missionary_exit_option_ambiguity, empirical, 'Whether missionary adoption of dynamic equivalence reflects institutional coercion or autonomous choice.').

omega_variable(
    suppression_mechanism_formalized_vs_internalized,
    'Is the suppression of formal-equivalence and critical-reconstructive readings enacted through institutional gatekeeping (structural), or through the reading''s normative authority (internalized as methodological orthodoxy)?',
    'Track the publication and adoption history of formal-equivalence translations by major committees; assess whether rejection occurs at committee decisions or at distribution/adoption. Examine training programs for theological translators.',
    'Structural suppression (institutional exclusion) is stronger and more resistant to challenge. Internalized suppression (acceptance of the reading''s norms as legitimate methodology) is more fragile but harder to identify. If internalized, post-transition trajectories of suppression would differ from structural suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_formalized_vs_internalized, empirical, 'Structural vs. internalized suppression of competing readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t8, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(bibl_tr_t8, observed).
narrative_ontology:measurement(bibl_tr_t16, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(bibl_tr_t16, observed).
narrative_ontology:measurement(bibl_tr_t24, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(bibl_tr_t24, observed).
narrative_ontology:measurement(bibl_tr_t32, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement_basis(bibl_tr_t32, observed).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(bibl_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t8, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement_basis(bibl_be_t8, observed).
narrative_ontology:measurement(bibl_be_t16, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement_basis(bibl_be_t16, observed).
narrative_ontology:measurement(bibl_be_t24, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement_basis(bibl_be_t24, observed).
narrative_ontology:measurement(bibl_be_t32, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement_basis(bibl_be_t32, observed).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(bibl_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t8, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement_basis(bibl_su_t8, observed).
narrative_ontology:measurement(bibl_su_t16, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement_basis(bibl_su_t16, observed).
narrative_ontology:measurement(bibl_su_t24, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 24, 0.46).
narrative_ontology:measurement_basis(bibl_su_t24, observed).
narrative_ontology:measurement(bibl_su_t32, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement_basis(bibl_su_t32, observed).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(bibl_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_source_text__dynamic_equivalence_reading, 0.12).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% The biblical source-text kernel is instantiated in three structurally distinct constraint stories: formal_equivalence_reading (source structure primary, ε_form ≈ 0.15), dynamic_equivalence_reading (meaning transfer primary, ε_accessibility ≈ 0.62), and critical_reconstructive_reading (textual establishment primary, ε_textual-basis ≈ 0.38). Each story carries different beneficiary/victim sets, different extraction profiles, and different enforcement machinery. The three stories are linked by affects_constraints edges: formal_equivalence_reading influences dynamic_equivalence_reading (formal methodology competes for institutional adoption), dynamic_equivalence_reading influences critical_reconstructive_reading (translation choices presuppose textual decisions), critical_reconstructive_reading influences both (textual disputes undercut certainty in both translation readings). No reading forecloses another within a single interpreter's framework; they coexist across different factions of the academic and ecclesiastical communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__dynamic_equivalence_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
