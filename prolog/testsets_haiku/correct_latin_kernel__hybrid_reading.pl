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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Medieval Latin Reconstruction via Hybrid Continuity-Discontinuity Thesis
 *   domain: intellectual/philological
 *
 * SUMMARY:
 *   Medieval Latin presents a puzzle: some forms look like Classical Latin
 *   corrupted by ignorance or carelessness; others look like systematic
 *   choices responding to medieval needs. The hybrid reading splits the
 *   difference: morphology (the internal structure of words) is treated as
 *   continuous with Classical norms—medieval scribes preserved and
 *   transmitted the inherited forms—but syntax (sentence structure) and
 *   lexicon (word choice) are treated as corrupted and requiring
 *   reconstruction back to Classical standards. This reading occupies a
 *   middle ground between full continuity (Medieval is just evolved
 *   Classical) and full discontinuity (Medieval is a separate language). The
 *   hybrid frame is institutionally powerful because it permits selective
 *   legitimacy: morphology gets to be correct (preserving continuity), while
 *   syntax and lexicon get to be wrong (justifying expert correction). This
 *   combination serves institutional philology's interests by validating both
 *   continuity claims and expert authority simultaneously.
 *
 * KEY AGENTS:
 *   - Institutional philology (universities, academies, textual-critical canons): Sets standards for correctness, controls who pronounces judgment on Latin fidelity, benefits from maintaining a framework that justifies expert reconstruction.
 *   - Latin grammar canon (Donatus, Priscian, modern grammars): The codified standard against which all Latin is measured; benefits from selective legitimacy that protects canonical authority.
 *   - Medieval scribes (powerless, trapped): Copied texts and made choices; retroactively judged by hybrid frame as either preserving (morphology) or corrupting (syntax/lexicon); could not defend choices; erasure of agency.
 *   - Medieval Latin practitioners (powerless, identity-locked): Monks, clergy, scholars using Latin as living language; treated as having failed to speak correctly; identity-locked because Latin legitimacy is central to their entire intellectual inheritance.
 *   - Continuity-reading scholars (excluded, constrained): Argue Medieval Latin is natural evolution; would undermine the correction enterprise if heard.
 *   - Discontinuity-reading scholars (excluded, constrained): Argue Medieval Latin is a separate system; would dissolve the hybrid frame if taken seriously.
 *   - Textual-criticism tradition (observer, institutional): Provides the technical apparatus (emendation protocols, apparatus criticus) that makes the constraint operational.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.62).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.58).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Medieval Latin Reconstruction via Hybrid Continuity-Discontinuity Thesis").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "intellectual/philological").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, 'bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d').
narrative_ontology:cs_kernel_codification('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d', fixed_text).
narrative_ontology:cs_authority_grounding('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d', lineage).
narrative_ontology:cs_interpretation_layer_present('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d').
narrative_ontology:cs_reading_relation('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d', foundational, morphology_continuity_legitimacy).
narrative_ontology:cs_axiom_status(morphology_continuity_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d', morphology_continuity_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d', foundational, syntax_lexicon_reconstruction_necessity).
narrative_ontology:cs_axiom_status(syntax_lexicon_reconstruction_necessity, holdable).
narrative_ontology:cs_axiom_grounding('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d', syntax_lexicon_reconstruction_necessity, deontological).
narrative_ontology:cs_reference_frame('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d', classical_latin_textual_purity).
narrative_ontology:cs_drift_state('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d', contemporary_linguistic_corpus_analysis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bad8ddf9-5ce3-4340-b4fd-9ded8e340b2d', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, institutional_philology).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, latin_grammar_canon).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, manuscript_scribes).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_latin_practitioners).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).

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
 *   Extractiveness is moderate-high (0.62 at interval end) because the constraint redistributes authority over interpretation from medieval agents to institutional arbiters, while keeping some morphological forms legitimate (avoiding the complete delegitimation that would make the system unstable). Suppression is moderate (0.58) because the constraint must actively exclude sibling readings from canonical authority—without suppressing continuity and discontinuity advocates, the hybrid frame collapses. Theater ratio (0.41) reflects that some genuine philological work happens (identifying real medieval variants, analyzing morphological patterns) but an increasing share of the effort is devoted to defending the hybrid frame itself against empirical challenge. The measurement series shows slow drift upward in extractiveness and theater ratio over the interval (t=0 to t=40), consistent with institutional gatekeeping hardening as the readings encounter more empirical evidence of systematic medieval linguistic coherence.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional-philology seat, the constraint is a reasonable classification framework: morphology IS continuous, syntax and lexicon DO require correction, and expert reconstruction IS justified. From the medieval-practitioners seat, the same constraint erases their agency and misrepresents their coherent choices as errors. From a continuity-reading seat, the hybrid frame is a compromise that fails empirically and serves only to protect institutional authority. From a discontinuity-reading seat, the entire hybrid frame is misconceived—the whole apparatus of 'correction' should be abandoned. The engine computes these divergences from the structural data: the hybrid reading treats institutional philology and the canon as beneficiaries (they control judgment and collect authority) while treating scribes and practitioners as victims (they bear retroactive judgment and lose agency). Continuity and discontinuity advocates are excluded because their readings threaten the institutional frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional philology is the agenda-setter with high power and high beneficiary position (d near 0.1-0.2: collects authority, sets standards, controls canon). Medieval practitioners are identity-locked victims: they cannot exit (Latin is central to their identity and legitimacy) and cannot defend (they are dead or their choices are reframed as errors before they can speak). Scribes are trapped victims: they bore the labor, could not exit, and are now judged retroactively. Continuity and discontinuity advocates have moderate power but are systematically excluded from canonical pronouncements—their directionality is asymmetrically disadvantaged by the same enforcement that protects the hybrid frame. The canon itself is not an agent but is the beneficiary (its authority is reinforced by selective legitimacy). Textual-criticism tradition is an observer seat: it executes the constraint's implications but does not decide the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—preserving Classical tradition while accommodating medieval practice—is contested. Institutional philology claims it is still live (Medieval texts still need correction to preserve Classical standards). Continuity advocates claim it is dead (natural evolution is not a problem, it is evidence). Discontinuity advocates claim it is misconceived (treating Medieval as corrupted Classical was never the right frame). The hybrid reading resolves this by selective legitimacy: 'the founding problem is half-live—morphology is solved (continuity) but syntax and lexicon require work (discontinuity).' This construction is attractive because it justifies both the existence of the institutional framework AND the continued need for expert intervention. But the measurement series shows rising theater_ratio and rising extractiveness, consistent with the frame becoming increasingly performative—more energy devoted to defending the reading against sibling readings than to solving the actual empirical puzzle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    morphology_syntax_lexicon_boundary,
    'Is the boundary between morphology (continuous, legitimate) and syntax/lexicon (discontinuous, corrupt) empirically stable, or does it shift depending on the analyst''s commitments?',
    'Systematic corpus analysis of medieval texts: classify all variants (morphological, syntactic, lexical) and measure their distributions. If medieval texts show systematic syntactic and lexical patterns that are NOT random corruption but follow coherent medieval rules, the boundary collapses.',
    'If the boundary is unstable, the hybrid reading is indefensible—either all of medieval Latin is continuous (full continuity) or all of it requires reclassification as a separate system (full discontinuity). The institutional claim to selective legitimacy fails.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(morphology_syntax_lexicon_boundary, empirical, 'Whether morphology/syntax/lexicon distinction is linguistically real or analytically imposed.').

omega_variable(
    reconstruction_agency_attribution,
    'When the hybrid reading prescribes ''reconstruction'' of medieval syntax and lexicon to Classical standards, is it recovering what medieval scribes intended but failed to execute, or is it imposing modern norms on medieval texts that were coherent in their own terms?',
    'Close reading of medieval texts with attention to whether variants appear random (suggesting error) or systematic (suggesting intentional medieval grammar). Evidence from colloquia, letters, and technical writing (where medieval Latin is most conservative) vs. narrative works (where innovation is most visible). Testimony from scholars with long exposure to medieval texts on whether corruption feels random or patterned.',
    'If reconstruction is recovery of failed intention, the hybrid frame holds. If medieval variants are systematic, the frame is inverted: reconstruction is imposition of modern preferences onto coherent medieval choices. This would shift classification toward discontinuity reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_agency_attribution, conceptual, 'Whether medieval texts are failed copies or evidence of a separate linguistic system.').

omega_variable(
    institutional_capture_of_philology,
    'To what extent does the hybrid reading''s institutional success depend on its empirical coherence versus its utility to institutional gatekeeping?',
    'Historical analysis of when and where the hybrid reading gained dominance. Did it spread because scholars found it empirically superior, or because institutional gatekeeping rewarded it (funding, positions, publication venues)? Evidence from scholars who argued for sibling readings: were they excluded due to empirical refutation or due to institutional power?',
    'If institutional capture is the primary driver, the constraint is closer to a snare than a tangled rope—the coordination function (defining a unified Latin standard) is secondary to the extraction function (centralizing authority). This would shift classification toward snare and raise mandatrophy questions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_philology, empirical, 'Whether the hybrid reading''s persistence is empirically grounded or institutionally sustained.').

omega_variable(
    committer_reading_stability,
    'The hybrid reading splits medieval Latin into continuous morphology and discontinuous syntax/lexicon. Is this split a natural linguistic discovery, or is it a strategic division chosen to satisfy competing constituencies (preserve continuity claims while justify expert correction)?',
    'Comparison with how other language transitions are analyzed (Old English to Middle English, Latin to Romance languages). If the morphology/syntax/lexicon distinction is applied consistently across language-change studies, it is likely a linguistic discovery. If applied selectively to Latin to satisfy institutional needs, it is a strategic reading.',
    'If strategic, the reading is a false compromise—neither continuity nor discontinuity, but a construction serving institutional interests. The omega itself is the committer-axis question: which reading (hybrid, continuity, discontinuity) best fits the actual evidence, regardless of institutional preference?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_stability, conceptual, 'Whether the hybrid reading reflects natural language dynamics or strategic institutional framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__hybrid_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(corr_tr_t5, correct_latin_kernel__hybrid_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(corr_tr_t10, correct_latin_kernel__hybrid_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(corr_tr_t15, correct_latin_kernel__hybrid_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(corr_tr_t25, correct_latin_kernel__hybrid_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(corr_tr_t40, correct_latin_kernel__hybrid_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__hybrid_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(corr_be_t5, correct_latin_kernel__hybrid_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(corr_be_t10, correct_latin_kernel__hybrid_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(corr_be_t15, correct_latin_kernel__hybrid_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(corr_be_t25, correct_latin_kernel__hybrid_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(corr_be_t40, correct_latin_kernel__hybrid_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__hybrid_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(corr_su_t5, correct_latin_kernel__hybrid_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(corr_su_t10, correct_latin_kernel__hybrid_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(corr_su_t15, correct_latin_kernel__hybrid_reading, suppression_requirement, 15, 0.57).
narrative_ontology:measurement(corr_su_t25, correct_latin_kernel__hybrid_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(corr_su_t40, correct_latin_kernel__hybrid_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'correct_latin_kernel' — the stabilized commitment to Latin textual transmission and correctness. The three readings decompose the kernel into distinct structural claims: continuity_reading treats all Medieval Latin as natural evolution (single constraint); discontinuity_reading treats Medieval Latin as a separate system (single constraint); hybrid_reading [THIS ONE] treats morphology as continuous and syntax/lexicon as discontinuous, requiring layered reconstruction (single constraint). Each reading has its own ε, its own beneficiary/victim structure, its own classification. They are NOT perspectives on one constraint; they are three distinct constraints instantiating three distinct readings of the same kernel. The network links record their interdependence: the hybrid reading influences both sibling readings by occupying the middle ground and excluding both from canonical authority. The three form a constraint family; the kernel is their shared reference point.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__hybrid_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
