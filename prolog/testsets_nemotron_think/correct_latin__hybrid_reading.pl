% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Hybrid Reading of Correct Latin: Medieval Continuity with Textual Correction
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The hybrid reading of 'correct Latin' emerged in the 15th century as
 *   humanist scholars (Valla, Erasmus, Poliziano) confronted a textual
 *   crisis: classical Latin works survived only through medieval manuscripts
 *   that had accumulated orthographic drift (ae→e, ti→ci, expanded
 *   abbreviations) and vocabulary substitutions while preserving the
 *   grammatical core. The hybrid solution ratified medieval grammar as
 *   continuous with classical Latin but subjected orthography and lexicon to
 *   correction against classical manuscript evidence. This reading became the
 *   foundation of modern critical editing (Teubner, OCT, CSEL) and Latin
 *   pedagogy. It coordinates by providing a single actionable standard for
 *   editors, teachers, and users, but extracts by delegitimizing medieval
 *   scribal orthography/vocabulary and concentrating editorial authority in
 *   the hands of those who control the classical manuscript arbiter.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.42).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.55).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Hybrid Reading of Correct Latin: Medieval Continuity with Textual Correction").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, '9540ea72-20e9-49fa-a4da-396f95e7f1a6').
narrative_ontology:cs_kernel_codification('9540ea72-20e9-49fa-a4da-396f95e7f1a6', distributed).
narrative_ontology:cs_authority_grounding('9540ea72-20e9-49fa-a4da-396f95e7f1a6', practice).
narrative_ontology:cs_interpretation_layer_present('9540ea72-20e9-49fa-a4da-396f95e7f1a6').
narrative_ontology:cs_reading_relation('9540ea72-20e9-49fa-a4da-396f95e7f1a6', correct_latin__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9540ea72-20e9-49fa-a4da-396f95e7f1a6', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('9540ea72-20e9-49fa-a4da-396f95e7f1a6', foundational, medieval_grammar_continuous_with_classical).
narrative_ontology:cs_axiom_status(medieval_grammar_continuous_with_classical, holdable).
narrative_ontology:cs_axiom_grounding('9540ea72-20e9-49fa-a4da-396f95e7f1a6', medieval_grammar_continuous_with_classical, empirically_contingent).
narrative_ontology:cs_axiom('9540ea72-20e9-49fa-a4da-396f95e7f1a6', foundational, textual_evidence_corrects_orthography_lexicon).
narrative_ontology:cs_axiom_status(textual_evidence_corrects_orthography_lexicon, holdable).
narrative_ontology:cs_axiom_grounding('9540ea72-20e9-49fa-a4da-396f95e7f1a6', textual_evidence_corrects_orthography_lexicon, empirically_contingent).
narrative_ontology:cs_axiom('9540ea72-20e9-49fa-a4da-396f95e7f1a6', secondary, critical_edition_as_legitimate_mediator).
narrative_ontology:cs_axiom_status(critical_edition_as_legitimate_mediator, holdable).
narrative_ontology:cs_axiom_grounding('9540ea72-20e9-49fa-a4da-396f95e7f1a6', critical_edition_as_legitimate_mediator, conventional).
narrative_ontology:cs_reference_frame('9540ea72-20e9-49fa-a4da-396f95e7f1a6', humanist_philological_practice).
narrative_ontology:cs_drift_state('9540ea72-20e9-49fa-a4da-396f95e7f1a6', contemporary_critical_edition_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9540ea72-20e9-49fa-a4da-396f95e7f1a6', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, humanist_scholars).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, modern_editorial_establishment).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, classical_textual_tradition).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, medieval_scribal_practice).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, vernacular_latin_users).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, classical_latin_as_normative_standard).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, textual_evidence_as_arbiter_of_correctness).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, partial_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% 15th-16th century scholars (Valla, Erasmus, Poliziano) who articulate the hybrid position: medieval Latin preserves grammatical core but requires correction of orthography and vocabulary from classical manuscripts. They set editorial standards, produce critical editions, and define the reform program. Their authority derives from philological expertise and access to manuscript sources.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, humanist_scholars, agenda_setter,
    organized, generational, mobile, continental).

% Contemporary critical editors, academic presses, and scholarly societies (Teubner, OCT, CSEL) who inherit and institutionalize the hybrid standard. They collect professional prestige, editorial control, and institutional resources from adjudicating correctness. Their editions become the reference texts for education and research.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, modern_editorial_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% The body of surviving classical Latin texts (Cicero, Caesar, Virgil, etc.) that functions as the external arbiter. It does not act but its existence structures the correction regime: every medieval deviation is measured against it. Listed as vindicated_proposition in base_properties; included here for completeness as a non-agent beneficiary of the hybrid reading's authority structure.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, classical_textual_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(correct_latin__hybrid_reading, classical_textual_tradition).

% The living medieval Latin tradition (9th-14th century): monastic scriptoria, chancery Latin, university disputation language, technical vocabulary. Its grammatical core is acknowledged as continuous with classical Latin, but its orthographic conventions (ae→e, ti→ci, expanded abbreviations) and vocabulary innovations are systematically corrected. The practitioners cannot exit the correction regime because the hybrid reading controls the textual transmission pipeline.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medieval_scribal_practice, payer,
    organized, generational, constrained, continental).

% Administrators, lawyers, physicians, and clergy who use Latin as a working language in the medieval and early modern period. They bear the cost of learning corrected forms, revising documents, and navigating the dual standard (medieval practice vs. humanist correction). Their exit is constrained by professional necessity — Latin remains the lingua franca of their domains.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, vernacular_latin_users, payer,
    moderate, biographical, constrained, regional).

% Scholars and institutions (some medieval universities, certain humanist conservatives, later neo-Latin movements) who hold that medieval Latin IS legitimate evolved Classical Latin and requires no correction. They are excluded from the hybrid reading's editorial infrastructure: their editions are not printed by major presses, their orthography is not taught. Their identity is fused with the continuity thesis — abandoning it dissolves their scholarly project.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, continuity_reading_proponents, excluded,
    organized, generational, identity_locked, continental).

% Purist humanists (early Valla, some Ciceronians) and later classical philologists who hold that ONLY classical texts authorize correctness; medieval Latin is entirely corrupt. They are excluded because the hybrid reading's partial legitimacy grant to medieval grammar undermines their total-reconstruction program. Their identity is fused with the purity thesis.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, discontinuity_reading_proponents, excluded,
    organized, generational, identity_locked, continental).

% Modern historical linguist who sees all three readings as contingent scholarly positions. Observes that the hybrid reading's coordination function (usable standard for editing/teaching) coexists with its extraction function (delegitimizing medieval orthography/vocabulary, concentrating editorial authority). No stake in any reading's victory.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, analytical_philologist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single actionable standard for editing, teaching, and using Latin that preserves the grammatical continuity of the medieval tradition while correcting its orthography and vocabulary against classical textual evidence — avoiding both the anarchy of uncorrected medieval practice and the impractical purism of full Ciceronian reconstruction.
% TRANSFER_FUNCTION: Moves editorial authority and normative force from medieval scribal practice (which produced the transmitted texts) to humanist/modern critical editors (who correct those texts against classical manuscripts). The transfer is partial: medieval grammar is ratified; medieval orthography and lexicon are superseded.
% ABSENT_VOICES: The medieval scribes and vernacular users themselves — they cannot speak in the modern scholarly conversation. Also excluded: continuity purists who would keep medieval forms uncorrected, and discontinuity purists who would discard medieval grammar entirely. Both are structurally excluded because the hybrid reading's editorial infrastructure (critical editions, curricula, standard grammars) only accommodates its own partial-correction logic.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished, critical editions would lose their correction principles (do we print medieval orthography? reconstruct classically?); Latin pedagogy would lose its consensus grammar; the Teubner/OCT apparatus would lack its guiding rationale. The entire textual transmission pipeline for Latin — from manuscript to classroom — would reorganize around either continuity or discontinuity poles.
% FOUNDING_PROBLEM: The 15th century crisis of Latin textual transmission: medieval manuscripts preserved classical works but with accumulated scribal errors, orthographic drift, and vocabulary substitutions. Humanists needed a principle to recover classical texts without discarding the medieval transmission that saved them. The hybrid reading solved this: trust the medieval grammatical skeleton, correct the flesh against classical witnesses.
% FOUNDING_PROBLEM_CORROBORATION: Humanist correspondence (Erasmus, Valla, Politian) attests the problem was live: they explicitly frame their editions as correcting medieval transmission using classical manuscripts. Modern codicology (Reynolds & Wilson, Bischoff) corroborates from outside the humanist tradition: manuscript stemmatics confirms medieval texts preserve grammatical core while corrupting orthography/lexicon. No beneficiary of the hybrid reading (modern editorial establishment) is needed to attest this — the manuscript evidence speaks independently.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).
:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the correction regime transfers normative authority from medieval practice to critical editors, but the grammatical core concession limits total extraction. Suppression (0.55) is significant: the hybrid standard is enforced through editorial gatekeeping (major presses only print corrected texts), educational curricula (school grammars teach the hybrid norm), and scholarly prestige — medieval orthography is not an option in serious publication. Theater ratio (0.28) reflects that the correction function is genuinely philological (manuscript comparison is real work) but a growing share of enforcement serves to maintain the editorial establishment's authority rather than improve texts. Accessibility collapse (0.45) is partial: alternative standards (continuity, discontinuity) exist but are marginalized in institutional channels. Resistance (0.62) is high: both excluded readings persist as live scholarly positions, and medieval Latin studies has recently reclaimed value in the 'un-corrected' transmission.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist/editorial seat (agenda_setter/beneficiary), the constraint appears as genuine coordination: a principled, evidence-based standard that saves classical texts from medieval corruption while honoring the transmission that preserved them. From the medieval scribal seat (payer), it appears as extraction: their living practice is partially delegitimized by an external standard they did not choose and cannot influence. From the excluded seats, it appears as suppression: their coherent alternatives are structurally barred from the editorial infrastructure. The engine computes this divergence from the declared power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist scholars and modern editorial establishment are structural beneficiaries (d ~ 0.15-0.25): they set the agenda, collect the prestige/resources, and have arbitrage-grade exit (they can work in other languages/fields). Medieval scribal practice and vernacular users are payers (d ~ 0.75-0.85): they bear the correction costs, have constrained exit (Latin was their professional lingua franca; the medieval tradition cannot 'exit' its own textual legacy). Continuity and discontinuity proponents are excluded (d ~ 0.9): their identity is fused to their reading (identity_locked), making exit from the dispute structurally impossible. The classical textual tradition (non-agent) sits at d=0.0 as the external arbiter — it benefits vacuously by being the standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (recovering classical texts from medieval transmission) was live in 1400-1600. By 1800, critical editions had largely solved it for the major authors. The constraint persists because the editorial establishment it created became self-justifying: the standard IS the institution. The hybrid reading now coordinates a scholarly ecosystem (editions, curricula, positions) that would lose its rationale if the standard were declared obsolete — but the textual evidence that originally motivated it has been largely exhausted. This is mandatrophy: the coordination function has atrophied to institutional maintenance, while the extraction function (editorial gatekeeping) persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_ambiguity,
    'Is the hybrid reading a stable synthesis or an unstable compromise that will resolve into continuity or discontinuity as manuscript evidence accumulates?',
    'Track whether critical editions increasingly adopt continuity (printing medieval orthography) or discontinuity (reconstructing purely classically) as manuscript coverage improves. The hybrid reading predicts stability; its siblings predict resolution toward their pole.',
    'If the hybrid reading resolves into discontinuity, its coordination function was temporary scaffolding for a purist outcome (→ scaffold). If it resolves into continuity, its correction function was a temporary imposition (→ snare with mandato-trophy). If it remains stable, it is a genuine tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_ambiguity, empirical, 'Whether the hybrid reading''s structural middle is an attractor or a transient.').

omega_variable(
    extraction_mechanism_ambiguity,
    'Does the hybrid reading''s extraction operate primarily through editorial gatekeeping (structural) or through internalized scholarly norms (internalized)?',
    'Survey whether scholars trained in the hybrid standard spontaneously correct medieval texts even without editorial pressure, or whether correction only occurs under institutional review.',
    'If internalized, suppression is higher than structural measures suggest — the constraint travels with the agent after institutional exit. This would increase effective extraction for identity_locked payers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the philological habitus.').

omega_variable(
    grammar_lexicon_boundary,
    'Is the boundary between ''grammatical core'' (ratified) and ''orthography/vocabulary'' (corrected) linguistically principled or pragmatically drawn?',
    'Linguistic analysis of which medieval deviations are morphological/syntactic vs. orthographic/lexical. The boundary is contested: e.g., medieval case collapse (loss of neuter, prepositional encroachment) — is this grammar (should be ratified) or corruption (should be corrected)?',
    'If the boundary is arbitrary, the hybrid reading''s coordination function is underspecified — it coordinates around a line that cannot be consistently drawn, making its extraction structurally unsupported. This would push classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grammar_lexicon_boundary, conceptual, 'Whether the hybrid reading''s selective ratification has a principled linguistic basis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 1400, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(correct_latin_hybrid_tr_t1400, correct_latin__hybrid_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(correct_latin_hybrid_tr_t1450, correct_latin__hybrid_reading, theater_ratio, 1450, 0.15).
narrative_ontology:measurement(correct_latin_hybrid_tr_t1500, correct_latin__hybrid_reading, theater_ratio, 1500, 0.22).
narrative_ontology:measurement(correct_latin_hybrid_tr_t1550, correct_latin__hybrid_reading, theater_ratio, 1550, 0.28).
narrative_ontology:measurement(correct_latin_hybrid_tr_t1600, correct_latin__hybrid_reading, theater_ratio, 1600, 0.31).
narrative_ontology:measurement(correct_latin_hybrid_tr_t1700, correct_latin__hybrid_reading, theater_ratio, 1700, 0.33).
narrative_ontology:measurement(correct_latin_hybrid_tr_t1800, correct_latin__hybrid_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(correct_latin_hybrid_tr_t1900, correct_latin__hybrid_reading, theater_ratio, 1900, 0.28).
narrative_ontology:measurement(correct_latin_hybrid_tr_t2000, correct_latin__hybrid_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(correct_latin_hybrid_tr_t2025, correct_latin__hybrid_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(correct_latin_hybrid_be_t1400, correct_latin__hybrid_reading, base_extractiveness, 1400, 0.25).
narrative_ontology:measurement(correct_latin_hybrid_be_t1450, correct_latin__hybrid_reading, base_extractiveness, 1450, 0.32).
narrative_ontology:measurement(correct_latin_hybrid_be_t1500, correct_latin__hybrid_reading, base_extractiveness, 1500, 0.41).
narrative_ontology:measurement(correct_latin_hybrid_be_t1550, correct_latin__hybrid_reading, base_extractiveness, 1550, 0.48).
narrative_ontology:measurement(correct_latin_hybrid_be_t1600, correct_latin__hybrid_reading, base_extractiveness, 1600, 0.52).
narrative_ontology:measurement(correct_latin_hybrid_be_t1700, correct_latin__hybrid_reading, base_extractiveness, 1700, 0.49).
narrative_ontology:measurement(correct_latin_hybrid_be_t1800, correct_latin__hybrid_reading, base_extractiveness, 1800, 0.45).
narrative_ontology:measurement(correct_latin_hybrid_be_t1900, correct_latin__hybrid_reading, base_extractiveness, 1900, 0.42).
narrative_ontology:measurement(correct_latin_hybrid_be_t2000, correct_latin__hybrid_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(correct_latin_hybrid_be_t2025, correct_latin__hybrid_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(correct_latin_hybrid_su_t1400, correct_latin__hybrid_reading, suppression_requirement, 1400, 0.35).
narrative_ontology:measurement(correct_latin_hybrid_su_t1450, correct_latin__hybrid_reading, suppression_requirement, 1450, 0.42).
narrative_ontology:measurement(correct_latin_hybrid_su_t1500, correct_latin__hybrid_reading, suppression_requirement, 1500, 0.55).
narrative_ontology:measurement(correct_latin_hybrid_su_t1550, correct_latin__hybrid_reading, suppression_requirement, 1550, 0.62).
narrative_ontology:measurement(correct_latin_hybrid_su_t1600, correct_latin__hybrid_reading, suppression_requirement, 1600, 0.65).
narrative_ontology:measurement(correct_latin_hybrid_su_t1700, correct_latin__hybrid_reading, suppression_requirement, 1700, 0.6).
narrative_ontology:measurement(correct_latin_hybrid_su_t1800, correct_latin__hybrid_reading, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(correct_latin_hybrid_su_t1900, correct_latin__hybrid_reading, suppression_requirement, 1900, 0.52).
narrative_ontology:measurement(correct_latin_hybrid_su_t2000, correct_latin__hybrid_reading, suppression_requirement, 2000, 0.53).
narrative_ontology:measurement(correct_latin_hybrid_su_t2025, correct_latin__hybrid_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% The correct_latin kernel decomposes into three constraint stories: continuity_reading (ratifies medieval Latin wholesale), discontinuity_reading (rejects medieval Latin wholesale), and hybrid_reading (partial ratification with textual correction). All three share the same referent (the Latin textual tradition) but instantiate different constraints with different ε, different beneficiary/victim structures, and different types. The hybrid reading influences both siblings by controlling the editorial infrastructure (critical editions, grammars, curricula) that materializes any reading into practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__hybrid_reading, organized, 0.2).
constraint_indexing:directionality_override(correct_latin__hybrid_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
