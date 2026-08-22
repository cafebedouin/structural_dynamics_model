% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__hybrid_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Correct Latin Kernel — Hybrid (Layered Reoccupation) Reading
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   Renaissance and late-medieval humanist philologists developed a standard
 *   of 'correct Latin' that accepted medieval inflectional morphology as
 *   legitimate (it had not, in their own analysis, meaningfully diverged from
 *   classical patterns) but treated medieval syntax and
 *   technical/administrative vocabulary as corrupted departures requiring
 *   recovery from classical manuscript sources. This produced a hybrid
 *   correctness standard: neither pure continuity nor pure discontinuity, but
 *   a stratified judgment applied differently to different linguistic levels.
 *   The standard functioned as genuine scholarly coordination — a shared
 *   register recoverable from a textual canon rather than fragmented regional
 *   practice — while simultaneously transferring prestige and administrative
 *   preferment away from practitioners of the existing scholastic and
 *   vernacular-inflected registers toward those credentialed in the
 *   reconstructed hybrid form.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.62).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Correct Latin Kernel — Hybrid (Layered Reoccupation) Reading").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, '7a65c759-315e-4268-8688-a810b5c8c9f2').
narrative_ontology:cs_kernel_codification('7a65c759-315e-4268-8688-a810b5c8c9f2', fixed_text).
narrative_ontology:cs_authority_grounding('7a65c759-315e-4268-8688-a810b5c8c9f2', lineage).
narrative_ontology:cs_interpretation_layer_present('7a65c759-315e-4268-8688-a810b5c8c9f2').
narrative_ontology:cs_reading_relation('7a65c759-315e-4268-8688-a810b5c8c9f2', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a65c759-315e-4268-8688-a810b5c8c9f2', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('7a65c759-315e-4268-8688-a810b5c8c9f2', foundational, morphology_syntax_lexicon_are_separately_adjudicable_strata).
narrative_ontology:cs_axiom_status(morphology_syntax_lexicon_are_separately_adjudicable_strata, holdable).
narrative_ontology:cs_axiom_grounding('7a65c759-315e-4268-8688-a810b5c8c9f2', morphology_syntax_lexicon_are_separately_adjudicable_strata, empirically_contingent).
narrative_ontology:cs_axiom('7a65c759-315e-4268-8688-a810b5c8c9f2', secondary, textual_recovery_is_legitimate_only_for_drifted_strata).
narrative_ontology:cs_axiom_status(textual_recovery_is_legitimate_only_for_drifted_strata, holdable).
narrative_ontology:cs_axiom_grounding('7a65c759-315e-4268-8688-a810b5c8c9f2', textual_recovery_is_legitimate_only_for_drifted_strata, conventional).
narrative_ontology:cs_reference_frame('7a65c759-315e-4268-8688-a810b5c8c9f2', classical_textual_corpus_as_recoverable_standard).
narrative_ontology:cs_drift_state('7a65c759-315e-4268-8688-a810b5c8c9f2', high_medieval_scholastic_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7a65c759-315e-4268-8688-a810b5c8c9f2', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, renaissance_humanist_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, classicizing_curial_scribes).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_latin_practitioners).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, vernacular_educated_clerics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compile grammars and stylistic manuals that certify which medieval forms are 'organic continuation' (inflectional morphology, largely) and which are 'corruption' requiring correction against classical texts (syntax, vocabulary, idiom). They administer the standard that determines correctness, and their scholarly authority and patronage income flow from being the certified experts capable of making that layered judgment.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, renaissance_humanist_philologists, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, renaissance_humanist_philologists, beneficiary).

% Chancery and curial writers who adopt the hybrid standard gain preferment in papal, royal, and civic administration; their careers are built on demonstrating fluency in the reconstructed register. They can move between courts that reward this competence, so their relationship to the standard is closer to opportunity than coercion.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, classicizing_curial_scribes, beneficiary,
    organized, generational, mobile, regional).

% Clerics, notaries, and scholastic writers trained in the living medieval register — its syntax, technical vocabulary, and idiom that had evolved for centuries to serve scholastic, legal, and liturgical needs. Under the hybrid standard, their syntax and lexicon are branded corrupt while their morphology is grudgingly accepted; they must retrain in reconstructed classical usage or see their existing texts and training devalued. Exit means abandoning a career built on the register they already command.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_latin_practitioners, payer,
    moderate, biographical, constrained, regional).

% Lower clergy and provincial notaries who learned Latin through vernacular-inflected local schooling rather than direct classical textual training. They cannot easily access the manuscript recovery and philological training the standard now requires, and are judged deficient in exactly the syntactic and lexical dimensions the hybrid reading treats as recoverable only through elite textual work.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, vernacular_educated_clerics, payer,
    powerless, biographical, trapped, local).

% Built an entire technical vocabulary and syntactic apparatus (quiddity, distinctiones, elaborate subordination structures) suited to disputation, which the humanist standard treats as barbarous lexical/syntactic corruption rather than legitimate technical development. They would object that their register served real argumentative functions the classical lexicon lacks, but the hybrid reading's authority to adjudicate correctness does not include their voice.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, scholastic_theologians, excluded,
    organized, generational, constrained, continental).

% The surviving classical manuscript corpus itself — what philologists actually had to recover syntax and lexicon from. Its patchy, unevenly-transmitted state determines which corrections were even possible, independent of anyone's preference.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, manuscript_transmission_lineage, observer,
    analytical, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single shared register for pan-European scholarly, administrative, and diplomatic communication by fixing which forms count as correct Latin, allowing writers trained in different regional traditions to be mutually intelligible and cross-checkable against a common textual standard.
% TRANSFER_FUNCTION: Moves prestige, ecclesiastical and administrative preferment, and pedagogical authority from practitioners of the existing medieval register (especially its scholastic syntax and technical lexicon) to philologists and scribes certified in the reconstructed hybrid standard; also transfers interpretive authority over 'what Latin really is' from living usage to textual recovery experts.
% ABSENT_VOICES: Scholastic theologians whose technical apparatus is branded corruption despite serving real argumentative functions; provincial and vernacular-trained clerics who lack access to the manuscript-recovery training the standard now demands but whose administrative competence the older register served adequately.
% DISAPPEARANCE_RATIONALE: If the hybrid correctness standard vanished, humanist patronage networks built on demonstrated classicizing competence would lose their credentialing function, scholastic technical vocabulary would likely regain legitimacy in disputational contexts, and provincial clerics currently penalized for 'corrupt' syntax would no longer be structurally disadvantaged relative to elite-trained scribes.
% FOUNDING_PROBLEM: Medieval Latin's syntax and lexicon had drifted far enough from surviving classical models, and regional variation had grown wide enough, that scholars judged a shared, textually-anchored standard necessary to restore cross-regional intelligibility and access to classical textual and rhetorical resources believed to be degraded or lost in transmission.
% FOUNDING_PROBLEM_CORROBORATION: Humanist philologists themselves attest the problem (textual corruption, loss of classical resources) as real and ongoing. Independent corroboration is thinner: later historical linguists confirm morphological continuity was genuine (supporting part of the founding claim) but many also document that scholastic syntax and lexicon were functional innovations rather than mere corruptions — a reading that undercuts the founding problem's universality and comes from outside the humanist beneficiary tradition, chiefly from modern historical linguistics and medieval intellectual historians with no stake in humanist credentialing.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and suppression (0.62) sit at moderate-high levels because the standard's authority is genuinely double-edged: the morphological continuity claim requires no coercion (it is descriptively defensible and largely accepted even by those it does not benefit), but the syntactic/lexical corruption judgment is where the enforcement bite lives — it is precisely the dimension along which credentialing, preferment, and pedagogical gatekeeping operate. Theater ratio (0.41) reflects that a meaningful share of correction activity is performative maintenance of humanist prestige rather than functionally necessary recovery, though genuine textual scholarship is also present. The temporal series shows extraction and suppression rising through the humanist consolidation period and then plateauing once the hybrid standard became institutionally settled in universities and chanceries.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists and classicizing scribes sit near the beneficiary end: they administer or profit from the correctness judgment. Medieval Latin practitioners and vernacular-educated clerics sit near the target end because the standard's enforcement falls specifically on the syntax/lexicon dimension where their existing competence is devalued — the morphological concession does little to offset this, since morphology was never the contested terrain of prestige. Scholastic theologians are excluded rather than positioned as simple victims: their objection (that their syntax and lexicon served real argumentative functions) is structurally absent from the standard's own adjudication process.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resists mandatrophy collapse in both directions: it does not let the genuine, largely uncontested morphological continuity finding launder the syntactic/lexical corruption judgment into equally uncontestable natural fact (which would hide real extraction behind real coordination), and it does not let the contested, credentialing-laden syntax/lexicon judgment retroactively delegitimize the morphological finding (which would discard genuine coordination as pure extraction). Treating the two strata separately is what keeps the tangled_rope classification honest — coordination (shared recoverable register) and extraction (credentialing gatekeeping on syntax/lexicon) coexist in the same structure without either canceling the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_choice_hybrid_vs_siblings,
    'Is the correct_latin_kernel better read as continuity_reading (single evolving system, reconstruction as internal correction), discontinuity_reading (two distinct systems, reconstruction as full symbolic reoccupation), or this hybrid_reading (stratified: morphology continuous, syntax/lexicon requiring recovery)?',
    'Comparative historical linguistic analysis distinguishing rates and mechanisms of morphological versus syntactic/lexical change across the medieval corpus, cross-checked against which forms actually required manuscript-based recovery versus which were native continuations documented in unbroken written use.',
    'If morphology and syntax/lexicon in fact drifted at comparable rates and by comparable mechanisms, the hybrid reading''s stratification collapses into either the continuity or discontinuity reading, and the tangled_rope classification (which depends on the coordination/extraction split tracking the morphology/syntax-lexicon split) would need to be reassessed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_hybrid_vs_siblings, conceptual, 'Committer-frame ambiguity: which kernel reading (continuity, discontinuity, or hybrid) best fits the historical evidence, and where the sibling readings'' disagreement is structurally located (the morphology/syntax-lexicon boundary).').

omega_variable(
    morphology_syntax_boundary_naturalness,
    'Is the specific boundary this reading draws — morphology legitimate, syntax/lexicon corrupt — itself a natural linguistic fact, or was it drawn where humanist philologists found it easiest to claim expertise and hardest for practitioners to contest?',
    'Compare the boundary against independent typological evidence on which linguistic strata (morphology vs. syntax vs. lexicon) actually show slower/faster diachronic change in unrelated language families, to test whether the humanist stratification tracks a real linguistic regularity or an expertise-convenient line.',
    'If the boundary tracks genuine typological regularities, the hybrid reading''s coordination function is more robust than its critics allow. If the boundary was drawn for credentialing convenience, the ''core morphology continuous'' finding may itself be a false-summit-style naturalization of a contestable methodological choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(morphology_syntax_boundary_naturalness, conceptual, 'Whether the morphology/syntax-lexicon split is a linguistic natural kind or a convenient credentialing boundary.').

omega_variable(
    scholastic_lexicon_functional_value,
    'Did scholastic technical vocabulary and syntax (branded ''corrupt'' by the hybrid standard) serve genuine argumentative functions that classical Latin lacked, making their suppression a real functional loss rather than pure correction?',
    'Analysis of scholastic disputational texts for cases where classical Latin lexicon lacks equivalent precision for technical distinctions (e.g., quidditas, haecceitas) and assessment of whether humanist ''correction'' actually degraded argumentative precision in affected domains.',
    'If scholastic vocabulary carried real functional value, the victim designation for scholastic theologians strengthens and the extractiveness score may be understated; if the vocabulary was genuinely superfluous or confused, the correction claim gains legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholastic_lexicon_functional_value, empirical, 'Whether the excluded scholastic register''s lexicon had genuine functional value beyond classical Latin''s resources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corr_tr_t50, correct_latin_kernel__hybrid_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(corr_tr_t100, correct_latin_kernel__hybrid_reading, theater_ratio, 100, 0.33).
narrative_ontology:measurement(corr_tr_t150, correct_latin_kernel__hybrid_reading, theater_ratio, 150, 0.37).
narrative_ontology:measurement(corr_tr_t200, correct_latin_kernel__hybrid_reading, theater_ratio, 200, 0.39).
narrative_ontology:measurement(corr_tr_t250, correct_latin_kernel__hybrid_reading, theater_ratio, 250, 0.41).
narrative_ontology:measurement(corr_tr_t300, correct_latin_kernel__hybrid_reading, theater_ratio, 300, 0.41).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(corr_be_t50, correct_latin_kernel__hybrid_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(corr_be_t100, correct_latin_kernel__hybrid_reading, base_extractiveness, 100, 0.47).
narrative_ontology:measurement(corr_be_t150, correct_latin_kernel__hybrid_reading, base_extractiveness, 150, 0.53).
narrative_ontology:measurement(corr_be_t200, correct_latin_kernel__hybrid_reading, base_extractiveness, 200, 0.56).
narrative_ontology:measurement(corr_be_t250, correct_latin_kernel__hybrid_reading, base_extractiveness, 250, 0.58).
narrative_ontology:measurement(corr_be_t300, correct_latin_kernel__hybrid_reading, base_extractiveness, 300, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(corr_su_t50, correct_latin_kernel__hybrid_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement(corr_su_t100, correct_latin_kernel__hybrid_reading, suppression_requirement, 100, 0.53).
narrative_ontology:measurement(corr_su_t150, correct_latin_kernel__hybrid_reading, suppression_requirement, 150, 0.58).
narrative_ontology:measurement(corr_su_t200, correct_latin_kernel__hybrid_reading, suppression_requirement, 200, 0.6).
narrative_ontology:measurement(corr_su_t250, correct_latin_kernel__hybrid_reading, suppression_requirement, 250, 0.62).
narrative_ontology:measurement(corr_su_t300, correct_latin_kernel__hybrid_reading, suppression_requirement, 300, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the correct_latin_kernel contested kernel. continuity_reading treats reconstruction as internal correction within one continuous system (low extraction claim); discontinuity_reading treats it as full symbolic reoccupation of a genuinely distinct system (extraction concentrated across the whole register); this hybrid_reading stratifies the judgment, conceding continuity for morphology while claiming genuine recovery-requirement for syntax/lexicon, producing an intermediate epsilon and a tangled_rope classification distinct from either sibling's likely classification. Each reading is authored as its own ε-invariant constraint per DP-001; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
