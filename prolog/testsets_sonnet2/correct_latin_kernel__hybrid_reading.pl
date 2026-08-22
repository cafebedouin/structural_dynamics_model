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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Humanist 'Correct Latin' Standard — Hybrid Continuity/Recovery Reading
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story authors the HYBRID reading of the correct_latin_kernel: the
 *   claim that core Latin morphology remained genuinely continuous from
 *   antiquity through the medieval period, requiring no reconstruction, while
 *   syntax and lexicon drifted enough that humanist reconstruction of
 *   classical norms constituted a real textual recovery rather than either
 *   pure internal correction (continuity_reading) or wholesale symbolic
 *   reoccupation (discontinuity_reading). The reconstruction is 'layered':
 *   morphology is left alone as legitimately continuous, and only the
 *   syntax/lexicon layer is treated as corrupted and in need of classical
 *   restoration via manuscript recovery. This partial-reoccupation structure
 *   is what makes the constraint tangled rather than a clean natural
 *   continuity or a clean invented standard — it genuinely coordinates
 *   cross-court intelligibility at the syntax/lexicon layer while extracting
 *   prestige and employment advantage from practitioners whose morphology was
 *   never at issue but whose functional syntax and vocabulary get
 *   reclassified as error.
 *
 * KEY AGENTS:
 *   - renaissance_humanist_grammarians: agenda_setter (institutional/arbitrage) — administers the layered standard, decides what counts as continuous vs. corrupt
 *   - classicizing_curial_secretariats: beneficiary (institutional/mobile) — gains prestige and interoperability from adopting the hybrid standard
 *   - medieval_notarial_latin_practitioners: payer (moderate/constrained) — functional syntax/lexicon reclassified as barbarism despite continuous morphology
 *   - vernacular_administrative_scribes: payer (powerless/trapped) — cannot acquire recovered classical syntax, most exposed to devaluation
 *   - philological_historians: observer (analytical/analytical) — reconstruct which features were actually continuous vs. actually recovered
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.52).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.58).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Humanist 'Correct Latin' Standard — Hybrid Continuity/Recovery Reading").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, '1a03d107-afad-49b5-8e1e-e49092ee3992').
narrative_ontology:cs_kernel_codification('1a03d107-afad-49b5-8e1e-e49092ee3992', fixed_text).
narrative_ontology:cs_authority_grounding('1a03d107-afad-49b5-8e1e-e49092ee3992', lineage).
narrative_ontology:cs_interpretation_layer_present('1a03d107-afad-49b5-8e1e-e49092ee3992').
narrative_ontology:cs_reading_relation('1a03d107-afad-49b5-8e1e-e49092ee3992', correct_latin_kernel__continuity_reading, influences).
narrative_ontology:cs_reading_relation('1a03d107-afad-49b5-8e1e-e49092ee3992', correct_latin_kernel__discontinuity_reading, influences).
narrative_ontology:cs_axiom('1a03d107-afad-49b5-8e1e-e49092ee3992', foundational, morphology_continuity_syntax_rupture_split).
narrative_ontology:cs_axiom_status(morphology_continuity_syntax_rupture_split, holdable).
narrative_ontology:cs_axiom_grounding('1a03d107-afad-49b5-8e1e-e49092ee3992', morphology_continuity_syntax_rupture_split, empirically_contingent).
narrative_ontology:cs_axiom('1a03d107-afad-49b5-8e1e-e49092ee3992', secondary, reconstruction_as_partial_reoccupation).
narrative_ontology:cs_axiom_status(reconstruction_as_partial_reoccupation, holdable).
narrative_ontology:cs_axiom_grounding('1a03d107-afad-49b5-8e1e-e49092ee3992', reconstruction_as_partial_reoccupation, conventional).
narrative_ontology:cs_reference_frame('1a03d107-afad-49b5-8e1e-e49092ee3992', classical_latin_textual_corpus).
narrative_ontology:cs_drift_state('1a03d107-afad-49b5-8e1e-e49092ee3992', high_medieval_chancery_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1a03d107-afad-49b5-8e1e-e49092ee3992', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, renaissance_humanist_grammarians).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, classicizing_curial_secretariats).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, print_grammar_publishers).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_notarial_latin_practitioners).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, vernacular_administrative_scribes).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, regional_ecclesiastical_latin_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, regional_ecclesiastical_latin_users).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, classical_norm_as_partial_natural_baseline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compile grammars and glossaries that certify which forms are 'correctly' Latin. They accept continuous morphology inherited from spoken practice as legitimate but treat medieval syntax and lexical innovation as corruption to be purged and replaced with textually recovered classical equivalents. They administer the standard through teaching posts, patronage networks, and printed authority.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, renaissance_humanist_grammarians, agenda_setter,
    institutional, generational, arbitrage, continental).

% Papal and princely chanceries adopt the hybrid standard to signal cultivated legitimacy in diplomatic correspondence. They gain prestige and interoperability with humanist courts by writing in reconstructed classical syntax layered onto continuous morphology, while retaining older forms where reconstruction proved unnecessary.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, classicizing_curial_secretariats, beneficiary,
    institutional, generational, mobile, continental).

% Sell grammars, dictionaries, and model letter-collections codifying the hybrid standard. Their commercial position depends on the standard remaining teachable and enforceable — a genuinely continuous, undivided medieval Latin would collapse the market for corrective textbooks.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, print_grammar_publishers, beneficiary,
    organized, biographical, mobile, continental).

% Trained scribes whose functional legal and administrative Latin — syntactically and lexically adapted to centuries of local practice — is reclassified as barbarism under the hybrid standard even though their morphology is judged continuous and thus untouched. They must relearn recovered classical syntax and vocabulary or lose standing in humanist-influenced chanceries.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_notarial_latin_practitioners, payer,
    moderate, biographical, constrained, regional).

% Local clerks who learned Latin through practical apprenticeship rather than textual philology. They cannot easily acquire the recovered classical syntax and lexicon the standard now demands, and their morphologically continuous but syntactically 'incorrect' Latin is devalued in documents that must circulate beyond their locality.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, vernacular_administrative_scribes, payer,
    powerless, biographical, trapped, local).

% Parish and monastic clergy whose liturgical and homiletic Latin retains medieval syntax and vocabulary judged corrupt under the hybrid standard, even though the same standard validates their inherited morphology. Some benefit from continued local acceptance of their register; others face pressure to conform when addressing humanist-trained superiors.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, regional_ecclesiastical_latin_users, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, regional_ecclesiastical_latin_users, beneficiary).

% Reconstruct the actual layered history of the standard — which features are genuinely continuous and which were textually recovered — often after the fact, once the practical stakes of the classification have passed.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, philological_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__hybrid_reading, renaissance_humanist_grammarians).
narrative_ontology:fixing_cost_class(correct_latin_kernel__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single teachable, textually anchored standard so that Latin documents produced across fragmented post-Roman polities remain mutually intelligible and carry comparable prestige — a real coordination problem where morphological drift alone would not have destroyed intelligibility but syntactic and lexical divergence increasingly did.
% TRANSFER_FUNCTION: Moves linguistic legitimacy and the practical advantages that follow it (chancery employment, ecclesiastical preferment, access to humanist patronage) from practitioners of functional medieval registers to practitioners trained in recovered classical syntax and vocabulary, while morphological continuity is left untaxed because it required no textual recovery to defend.
% ABSENT_VOICES: Working scribes and regional clergy whose registers are judged corrupt at the syntax/lexicon layer rarely leave grammars or manifestos of their own; their practice survives mainly in the documents themselves, which the standard's advocates cite as evidence of decline rather than as a competing linguistic authority.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished, the layered legitimacy structure it built — continuous morphology accepted, syntax and lexicon policed — would collapse into either uncontested medieval practice or an entirely different (discontinuity-style) reconstruction; chancery hiring, humanist patronage networks, and grammar-book markets built on the specific hybrid distinction would all need to reorganize around a different legitimacy criterion.
% FOUNDING_PROBLEM: As post-Roman Latin usage diverged regionally in syntax and vocabulary while morphology stayed relatively stable, humanist scholars faced genuine difficulty producing texts intelligible and prestigious across courts that increasingly measured Latin against classical models newly available through manuscript recovery.
% FOUNDING_PROBLEM_CORROBORATION: Humanist grammarians and curial secretariats attest the problem as ongoing (classical fidelity as an active standard). Independent philological historians, working from the surviving documentary record outside the humanist tradition, attest that morphological divergence was never functionally disruptive and that much of the 'corruption' being corrected was stable, communicatively adequate regional practice — suggesting the founding problem was real only for the syntax/lexicon layer, not for morphology, which the hybrid reading itself already concedes.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52) sits meaningfully below what a pure discontinuity/reoccupation reading would author, because morphological continuity is conceded and untaxed — a real chunk of medieval practice is left alone. Suppression (0.58) and theater_ratio (0.4) are moderate: enforcement targets specifically the syntax/lexicon layer, and a substantial share of grammatical policing (declaring functionally adequate medieval syntax 'corrupt') is performative status-marking rather than intelligibility-preserving function. Accessibility_collapse (0.62) reflects that once the hybrid standard is understood and taught, alternatives at the syntax/lexicon layer become hard to sustain professionally, though morphological practice remains genuinely unconstrained. The temporal series shows the standard hardening over two centuries as humanist grammars proliferate through print and curial adoption compounds enforcement pressure — a gradual layering-in of the recovery apparatus, not a sudden imposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist grammarians and curial secretariats sit near the beneficiary end: they set or profit from the classical-syntax criterion while their own morphology (already continuous) costs them nothing to defend. Medieval notarial practitioners and vernacular scribes sit near the target end: precisely the layer where they had functional, communicatively adequate practice (syntax, lexicon) is the layer reclassified as corrupt, while the layer where they were already 'safe' (morphology) offers no relief from the syntax/lexicon penalty. Regional ecclesiastical users are genuinely mixed — hence the secondary beneficiary role — because local acceptance sometimes shields them while translocal contact exposes them to the same penalty.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading's own founding-problem status is authored as contested rather than dead, which blocks premature mandatrophy declaration: the coordination function (cross-court intelligibility) plausibly remains partly live even after the layered standard's prestige function outlived its intelligibility function. Treating the whole standard as either pure coordination (continuity_reading's implicit frame) or pure extraction (discontinuity_reading's implicit frame) would mislabel the morphology layer, which the hybrid reading concedes needed no correction and thus was never load-bearing for the extraction at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    morphology_syntax_boundary_ambiguity,
    'Is the morphology/syntax-lexicon boundary this reading relies on a genuine linguistic fault line, or a retrospectively convenient division that lets humanist grammarians claim natural-continuity cover for the parts of the standard they didn''t need to defend while treating everything contestable as corruption?',
    'Comparative historical linguistic analysis of documentary corpora across the transition period, testing whether morphological forms actually show less regional/temporal variance than syntactic and lexical forms, independent of humanist classification.',
    'If the boundary tracks real variance differences, the hybrid reading''s partial-reoccupation structure is empirically grounded and the tangled_rope classification (partial genuine coordination, partial extraction) holds. If the boundary is retrospectively drawn to match whatever needed less textual recovery effort, the ''continuous morphology'' claim is itself a rhetorical move and the constraint is closer to a disguised discontinuity/reoccupation story wearing continuity''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morphology_syntax_boundary_ambiguity, empirical, 'Whether the morphology/syntax boundary is linguistically real or a rhetorically convenient partition.').

omega_variable(
    kernel_framing_committer_ambiguity,
    'Given that continuity_reading, discontinuity_reading, and hybrid_reading are three readings of the same correct_latin_kernel, is the choice to author THIS constraint as hybrid (rather than as one of the other two) itself defensible on the documentary evidence, or does the hybrid framing let humanist advocates claim continuity''s legitimacy for the parts of the standard that were easy while claiming discontinuity''s authority for the parts that were contested?',
    'Cross-reading comparison: generate all three sibling constraints with independently authored ε and stakeholder sets, then compare which reading''s beneficiary/victim structure best matches the surviving documentary record of who gained employment/prestige advantage and at which linguistic layer.',
    'If the hybrid reading is vindicated, its tangled_rope classification with a genuine morphology carve-out is the most accurate of the three. If it turns out to be a rhetorical hybrid constructed after the fact to legitimate an otherwise uniform reoccupation project, the discontinuity_reading''s higher ε is the more accurate account and this story''s lower extractiveness score understates the true extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_committer_ambiguity, conceptual, 'Whether the hybrid framing is analytically warranted or a legitimacy-borrowing construction across the kernel''s three readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corr_tr_t40, correct_latin_kernel__hybrid_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(corr_tr_t80, correct_latin_kernel__hybrid_reading, theater_ratio, 80, 0.32).
narrative_ontology:measurement(corr_tr_t120, correct_latin_kernel__hybrid_reading, theater_ratio, 120, 0.35).
narrative_ontology:measurement(corr_tr_t160, correct_latin_kernel__hybrid_reading, theater_ratio, 160, 0.38).
narrative_ontology:measurement(corr_tr_t200, correct_latin_kernel__hybrid_reading, theater_ratio, 200, 0.4).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(corr_be_t40, correct_latin_kernel__hybrid_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(corr_be_t80, correct_latin_kernel__hybrid_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(corr_be_t120, correct_latin_kernel__hybrid_reading, base_extractiveness, 120, 0.48).
narrative_ontology:measurement(corr_be_t160, correct_latin_kernel__hybrid_reading, base_extractiveness, 160, 0.5).
narrative_ontology:measurement(corr_be_t200, correct_latin_kernel__hybrid_reading, base_extractiveness, 200, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(corr_su_t40, correct_latin_kernel__hybrid_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(corr_su_t80, correct_latin_kernel__hybrid_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(corr_su_t120, correct_latin_kernel__hybrid_reading, suppression_requirement, 120, 0.53).
narrative_ontology:measurement(corr_su_t160, correct_latin_kernel__hybrid_reading, suppression_requirement, 160, 0.56).
narrative_ontology:measurement(corr_su_t200, correct_latin_kernel__hybrid_reading, suppression_requirement, 200, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'correct Latin required reconstruction' under the ε-invariance principle. continuity_reading authors near-mountain/rope ε (evolution, no reoccupation); discontinuity_reading authors higher ε (full symbolic reoccupation, no morphological exemption); this hybrid_reading authors mid-range ε reflecting a genuine partial carve-out. All three share the correct_latin_kernel and are linked via affects_constraints; none should be read as measuring the 'same' constraint at different observables — each has its own beneficiary/victim structure and its own stable ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
