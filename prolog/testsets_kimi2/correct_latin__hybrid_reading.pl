% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Correct Latin â Hybrid Reading (Classical Form Transmitted through Medieval Practice, Correctable via Textual Evidence)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The constraint 'correct Latin' under the hybrid reading governs
 *   philological practice by asserting that legitimate Latin is the Classical
 *   form as transmitted through medieval practice, yet subject to correction
 *   by textual evidence. It occupies a middle ground between pure continuity
 *   (medieval Latin as evolved legitimacy) and pure discontinuity (medieval
 *   Latin as corruption requiring full reconstruction). This reading grants
 *   partial legitimacy to medieval grammatical transmission while reserving
 *   the right to 'correct' orthography and vocabulary against classical
 *   textual witnesses. The constraint is actively enforced through critical
 *   edition protocols, peer review in philology journals, and Latin
 *   curricula. It coordinates scholarly Latin across periods but extracts
 *   authority from medievalists whose evidence is treated as secondary.
 *
 * KEY AGENTS:
 *   - classical_textual_critics: Primary agenda-setter (institutional/constrained) â administers the standard through critical editions and peer review
 *   - classical_academy: Primary beneficiary (institutional/constrained) â accrues prestige and resources from guarding the standard
 *   - medievalists: Primary payer (moderate/constrained) â bear costs of delegitimization and correction
 *   - linguistic_historians: Analytical observer (analytical/analytical) â sees the contest without normative stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.48).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.42).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Correct Latin â Hybrid Reading (Classical Form Transmitted through Medieval Practice, Correctable via Textual Evidence)").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, 'e62cedc2-7f79-4aa4-96f6-1ba626149dbc').
narrative_ontology:cs_kernel_codification('e62cedc2-7f79-4aa4-96f6-1ba626149dbc', fixed_text).
narrative_ontology:cs_authority_grounding('e62cedc2-7f79-4aa4-96f6-1ba626149dbc', expertise).
narrative_ontology:cs_interpretation_layer_present('e62cedc2-7f79-4aa4-96f6-1ba626149dbc').
narrative_ontology:cs_reading_relation('e62cedc2-7f79-4aa4-96f6-1ba626149dbc', correct_latin__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e62cedc2-7f79-4aa4-96f6-1ba626149dbc', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('e62cedc2-7f79-4aa4-96f6-1ba626149dbc', foundational, grammatical_core_continuity).
narrative_ontology:cs_axiom_status(grammatical_core_continuity, holdable).
narrative_ontology:cs_axiom_grounding('e62cedc2-7f79-4aa4-96f6-1ba626149dbc', grammatical_core_continuity, empirically_contingent).
narrative_ontology:cs_axiom('e62cedc2-7f79-4aa4-96f6-1ba626149dbc', foundational, textual_critical_sovereignty).
narrative_ontology:cs_axiom_status(textual_critical_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('e62cedc2-7f79-4aa4-96f6-1ba626149dbc', textual_critical_sovereignty, conventional).
narrative_ontology:cs_reference_frame('e62cedc2-7f79-4aa4-96f6-1ba626149dbc', classical_medieval_synthesis).
narrative_ontology:cs_drift_state('e62cedc2-7f79-4aa4-96f6-1ba626149dbc', contemporary_scholarly_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e62cedc2-7f79-4aa4-96f6-1ba626149dbc', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, classical_textual_critics).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, classical_academy).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, medievalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produces critical editions of Latin texts by comparing medieval manuscripts against classical textual witnesses; sets the editorial conventions that determine when a medieval form counts as a corruption to be corrected versus a legitimate transmission.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, classical_textual_critics, agenda_setter,
    institutional, generational, constrained, global).

% Edits and studies medieval Latin manuscripts; encounters the hybrid standard when their source orthography or vocabulary is marked as erroneous in favor of classical attestations, requiring them to defend medieval readings as transmitted rather than mistaken.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medievalists, payer,
    moderate, biographical, constrained, global).

% Funds professorships, certifies Latin curricula, and accredits editions that meet the hybrid standard; its reputation and student enrollment depend on maintaining authoritative control over what counts as correct Latin.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, classical_academy, beneficiary,
    institutional, generational, constrained, global).

% Documents the historical development of Latin from antiquity through the medieval period; observes the contest between normative standards without enforcing any single one in their own descriptive practice.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, linguistic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__hybrid_reading, classical_academy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scholarly communication across time by providing a shared standard for editing, teaching, and citing Latin texts, resolving the babel problem of competing local Latin norms in philology.
% TRANSFER_FUNCTION: Moves authority from medieval transmitters to classical philologists and textual critics; moves legitimacy from medieval orthographic and lexical practice to reconstructed classical forms whenever textual evidence is invoked.
% ABSENT_VOICES: Vernacular Latin users, liturgical Latin communities, and neo-Latin authors who employ Latin outside classical and medieval paradigms are excluded; they would object that the standard is an artificial construct imposed on a living historical language.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished, critical editions would fragment between pure reconstructionists and pure medievalists, textbooks would multiply competing norms, and the institutional authority of classical philology would diminish â scholarly Latin would rearrange around either stricter classical revival or fuller medieval autonomy.
% FOUNDING_PROBLEM: The corruption of ancient texts through medieval copying and the need for a reliable standard to edit, teach, and transmit Latin literacy after the rupture of antiquity.
% FOUNDING_PROBLEM_CORROBORATION: Philologists outside the classical academy â notably medievalists and sociolinguists â attest that the problem of textual corruption is real but that the hybrid solution exaggerates classical purity; independent historians of Latin note the standard's founding narrative omits the legitimate evolution of the language.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.48, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.48) is moderate because the hybrid reading genuinely acknowledges some medieval continuity (grammar), limiting the scope of correction; however, the reservation of textual corrective authority over orthography and vocabulary creates persistent extraction of legitimacy from medievalists. Suppression (0.42) is moderate: enforcement operates through editorial gatekeeping and curricular design rather than direct coercion. Theater ratio (0.35) reflects increasing ritualization of textual-critical method â the apparatus of stemmatics and recensio performs rigor even where underlying continuity is assumed. Accessibility collapse (0.38) is partial because alternative frameworks (continuity, discontinuity) remain visible and practiced. Resistance (0.40) comes from medievalist scholarship and sociolinguistic approaches that challenge the normative classical standard. The claim (tangled_rope) and metrics are independently authored: the metrics describe a constraint with real coordination function and real asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the classical_textual_critics seat, the arrangement is necessary coordination: without a standard, critical editing collapses into arbitrariness. From the medievalists seat, the same arrangement is partial extraction: their evidence is admitted only under classical supervision. The engine computes this divergence from structural data â identical institutional power levels would not produce identical classifications because the beneficiary/victim declarations and exit options differ.
 *
 * DIRECTIONALITY LOGIC:
 *   classical_textual_critics and classical_academy are beneficiaries: they collect authority, prestige, and institutional resources from the constraint's operation (d near 0.0). medievalists are victims: they bear the cost of having their materials treated as corruptible and their expertise subordinated (d near 1.0). linguistic_historians occupy an analytical seat with no stake in the constraint's persistence. The classical academy's exit is constrained by institutional identity; the medievalists' exit is constrained by the field's gatekeeping.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because it carries both a genuine coordination function (enabling cross-temporal scholarly Latin and textual criticism) and identifiable asymmetric extraction (medievalists pay through delegitimization). A pure rope would lack the victim structure; a pure snare would lack the acknowledged continuity. The hybrid reading's partial legitimation of medieval grammar is the structural feature that forces the tangled_rope classification rather than snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the hybrid reading''s partial legitimation of medieval forms structurally resolve the kernel contest, or does it merely defer the choice between continuity and discontinuity to case-by-case editorial judgment?',
    'Corpus analysis of editorial practice: if hybrid editors systematically correct medieval forms in predictable categories (orthography, morphology) while accepting others (syntax), the reading is a stable synthesis; if correction is ad hoc, it is deferred arbitration.',
    'If ad hoc, effective extractiveness is higher than the base metric suggests because medievalists face unpredictable delegitimization; if systematic, the constraint operates as a genuine coordination mechanism with bounded extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether hybrid reading stabilizes the kernel or defers the contest').

omega_variable(
    textual_evidence_authority,
    'Is the authority of textual evidence to correct medieval transmission grounded in empirical discovery of classical usage, or in a priori normative commitment to classical purity?',
    'Historical sociolinguistic analysis of the classical corpus: if the ''classical'' standard itself is a post-hoc construction from a heterogeneous ancient evidence base, then textual correction is circular.',
    'If circular, the constraint''s coordination function is cover for extraction; if empirically grounded, the correction mechanism is structurally warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_evidence_authority, empirical, 'Whether textual evidence authority is empirical or circular').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(corr_tr_t20, correct_latin__hybrid_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(corr_tr_t40, correct_latin__hybrid_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(corr_tr_t60, correct_latin__hybrid_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(corr_tr_t80, correct_latin__hybrid_reading, theater_ratio, 80, 0.35).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__hybrid_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(corr_be_t20, correct_latin__hybrid_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(corr_be_t40, correct_latin__hybrid_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(corr_be_t60, correct_latin__hybrid_reading, base_extractiveness, 60, 0.49).
narrative_ontology:measurement(corr_be_t80, correct_latin__hybrid_reading, base_extractiveness, 80, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(correct_latin__hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
