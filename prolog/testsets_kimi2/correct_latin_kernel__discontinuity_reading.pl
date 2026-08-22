% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Discontinuity Reading of the Latin Philological Kernel
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint instantiates the discontinuity reading of the
 *   correct_latin_kernel in historical philology: the commitment that
 *   Classical Latin and Medieval Latin are distinct linguistic systems, that
 *   medieval manuscript variants represent corruptions rather than natural
 *   evolution, and that scholarly reconstruction must recover lost classical
 *   structure through symbolic reoccupation of the texts. The reading frames
 *   the medieval witness as a defective vessel and the classical original as
 *   the only legitimate target of editorial labor. It has coordinated
 *   centuries of textual recovery but simultaneously extracts epistemic
 *   authority from medievalist scholars and students by subordinating their
 *   subject matter to a classical purity norm.
 *
 * KEY AGENTS:
 *   - classical_philologists (institutional/constrained) â administer the editorial method and collect disciplinary prestige
 *   - humanist_pedagogical_institutions (institutional/constrained) â maintain curricula and certification standards
 *   - medievalist_scholars (moderate/constrained) â bear the epistemic subordination of their field
 *   - textual_editors (moderate/constrained) â execute reconstructive labor under classical targets
 *   - latin_students (powerless/identity_locked) â absorb the normative framework as linguistic intuition
 *   - historical_linguists (organized/mobile) â excluded voice whose evolutionary framework is kept outside the philological paradigm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.68).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.61).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Discontinuity Reading of the Latin Philological Kernel").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '03201e1a-2344-4b69-854f-39700533fcbb').
narrative_ontology:cs_kernel_codification('03201e1a-2344-4b69-854f-39700533fcbb', fixed_text).
narrative_ontology:cs_authority_grounding('03201e1a-2344-4b69-854f-39700533fcbb', lineage).
narrative_ontology:cs_interpretation_layer_present('03201e1a-2344-4b69-854f-39700533fcbb').
narrative_ontology:cs_reading_relation('03201e1a-2344-4b69-854f-39700533fcbb', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('03201e1a-2344-4b69-854f-39700533fcbb', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('03201e1a-2344-4b69-854f-39700533fcbb', foundational, medieval_forms_are_corruptions).
narrative_ontology:cs_axiom_status(medieval_forms_are_corruptions, holdable).
narrative_ontology:cs_axiom_grounding('03201e1a-2344-4b69-854f-39700533fcbb', medieval_forms_are_corruptions, empirically_contingent).
narrative_ontology:cs_axiom('03201e1a-2344-4b69-854f-39700533fcbb', foundational, reconstruction_requires_classical_target).
narrative_ontology:cs_axiom_status(reconstruction_requires_classical_target, holdable).
narrative_ontology:cs_axiom_grounding('03201e1a-2344-4b69-854f-39700533fcbb', reconstruction_requires_classical_target, conventional).
narrative_ontology:cs_reference_frame('03201e1a-2344-4b69-854f-39700533fcbb', classical_restoration_framework).
narrative_ontology:cs_drift_state('03201e1a-2344-4b69-854f-39700533fcbb', post_historical_linguistics_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('03201e1a-2344-4b69-854f-39700533fcbb', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, humanist_pedagogical_institutions).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medievalist_scholars).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, latin_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, textual_editors).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, textual_editors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the editorial method of textual criticism, train successors in stemmatics and emendation toward classical purity, control peer-review standards for critical editions, and hold the prestige-generating positions in academies and universities. Their professional identity and institutional authority are fused with the act of restoring classical texts from medieval witnesses deemed corrupt.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_philologists, agenda_setter,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, classical_philologists, beneficiary).

% Universities, academies, and elite secondary schools whose curricula, examinations, and prestige depend on maintaining Classical Latin as the sole normative form. They certify linguistic competence against classical standards and benefit from the disciplinary boundary that keeps medieval Latin subordinate and classical philology central to the humanities.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, humanist_pedagogical_institutions, beneficiary,
    institutional, civilizational, constrained, continental).

% Study medieval Latin texts and cultures but must justify their material against classical norms; their manuscripts are treated as defective vessels rather than autonomous objects. Their disciplinary autonomy is structurally subordinated to classical editorial frameworks, and funding flows preferentially to restoration projects over medieval-contextual study.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medievalist_scholars, payer,
    moderate, generational, constrained, continental).

% Execute the reconstructive method in practice, producing critical editions whose apparatus treats medieval variants as corruptions to be emended. They bear the labor of the constraint and are professionally rewarded only when the classical target is persuasively reconstructed; editions that treat medieval readings as legitimate face lower prestige and market acceptance.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, textual_editors, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, textual_editors, beneficiary).

% Taught to read Classical Latin as the normative, pure form and to approach medieval texts through the lens of error and corruption. Their linguistic intuition and aesthetic judgment are formed so that medieval morphology and syntax appear as failed classical usage, locking their interpretive habits to the discontinuity frame.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, latin_students, payer,
    powerless, biographical, identity_locked, local).

% Possess an alternative framework that treats medieval Latin variants as natural evolutionary forms within a continuous historical continuum. Their perspective is kept outside the philological editorial paradigm; they exit to general linguistics rather than contest the classical reconstruction apparatus from within.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, historical_linguists, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified editorial protocol for recovering classical texts from physically corrupted or variant medieval manuscripts, coordinating scholarly labor around a shared target language state and a common set of stemmatic procedures.
% TRANSFER_FUNCTION: Transfers epistemic authority, disciplinary prestige, and pedagogical centrality from medieval Latin textual traditions to classical philology; moves interpretive labor and edition standards from autonomous engagement with medieval forms toward reconstruction of hypothetical classical originals.
% ABSENT_VOICES: Historical linguists who read medieval variants as systematic natural change rather than error; medieval authors and scribes whose intentional linguistic choices are overwritten by the corruption frame; vernacular and documentary Latin specialists whose material falls outside the classical recovery paradigm.
% DISAPPEARANCE_RATIONALE: If the discontinuity mandate disappeared, critical editions would shift from reconstructing classical originals toward editing medieval texts on their own terms; the boundary between classical and medieval Latin would collapse into a historical continuum; classical philology would lose its privileged epistemic authority over the medieval period; curricula, hiring, and funding would reorganize around historical linguistics and autonomous medieval studies.
% FOUNDING_PROBLEM: Classical literary and legal texts survived only through medieval manuscripts that contained scribal errors, interpolations, and linguistic substitutions, creating a genuine need for a method to recover authentic ancient wording.
% FOUNDING_PROBLEM_CORROBORATION: The physical fact of manuscript variance is corroborated by paleographers and codicologists across scholarly traditions. However, the characterization of that variance as 'corruption' requiring classical-targeted reconstruction is attested primarily by the benefiting classical philological tradition. Historical linguists outside that tradition corroborate that the problem is better addressed by diachronic linguistic analysis than by normative reconstruction; no independent corroboration of the discontinuity framing exists outside the beneficiary set.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the framework decouples editorial labor from the actual historical character of medieval texts, substituting a classical target that serves the beneficiary tradition. Suppression (0.61) reflects active disciplinary enforcement through peer review, hiring, and edition standards that penalize medieval-autonomous approaches. Theater_ratio (0.52) is elevated because a significant share of reconstructive activity has become ritualized â stemmatic choices and apparatus construction that perform methodological purity rather than demonstrably improve textual accuracy. Accessibility_collapse (0.64) captures how alternatives (reading medieval Latin on its own terms) become epistemically invisible once the corruption frame is accepted. Resistance (0.48) reflects sustained but institutionally marginalized pushback from medievalists and historical linguists. The temporal series show extraction and enforcement peaking during the nineteenth-century consolidation of scientific philology, then moderating but persisting under twentieth-century historical-linguistic challenge.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences this constraint as a necessary and honorable method for saving ancient literature from oblivion; the payer seats experience it as a disciplinary cage that mischaracterizes their material and redirects their labor. The engine will compute this divergence from identical structural facts: classical_philologists and textual_editors share moderate-to-institutional power but diverge sharply because one administers the method and the other is administered by it. latin_students sit at powerless with identity_locked exit, placing them near the full-target end of the directionality spectrum despite their low global power.
 *
 * DIRECTIONALITY LOGIC:
 *   classical_philologists and humanist_pedagogical_institutions are structural beneficiaries: they set the agenda, certify competence, and accrue prestige from the classical restoration framework (low d). medievalist_scholars, textual_editors, and latin_students are targets: their labor, autonomy, or intuition is shaped by the classical target (high d). historical_linguists are excluded rather than directly targeted; their mobile exit to an alternative discipline keeps their structural relationship analytically distinct from the trapped or identity-locked victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â physically corrupted manuscripts â was real, and the coordination function â a shared editorial method for textual recovery â is genuine. Without the tangled_rope category, this constraint might be misread as a rope (pure coordination) because the recovery of classical texts is a real scholarly achievement. However, the beneficiary structure is asymmetric: classical philology and humanist institutions capture prestige, positions, and pedagogical centrality while medievalist scholars pay the cost of epistemic subordination. The persistence of the discontinuity frame after the rise of historical linguistics demonstrates extraction layered onto coordination, not coordination alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_discontinuity_ambiguity,
    'Is the discontinuity between Classical and Medieval Latin an empirical linguistic fact or a normative disciplinary construction?',
    'Comparative sociolinguistic analysis of medieval Latin usage independent of classical norms; examination of whether ''corruption'' judgments persist when classical prestige is bracketed.',
    'If constructed, the reconstruction mandate is an extractive disciplinary frame rather than neutral recovery; reclassification toward snare or piton becomes likely depending on beneficiary concentration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_discontinuity_ambiguity, conceptual, 'Whether the kernel''s discontinuity premise is empirical or constructed').

omega_variable(
    corruption_natural_change,
    'Do medieval Latin variants reflect systematic natural linguistic evolution rather than corruption of a classical ideal?',
    'Historical-linguistic phylogenetic analysis and variationist studies of medieval Latin corpora measured against universal language-change metrics.',
    'If natural evolution is the better descriptor, the reconstruction target (classical purity) is shown to be a disciplinary preference, undermining the coordination story and raising extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_natural_change, empirical, 'Empirical status of medieval Latin variation as natural change').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__discontinuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corr_tr_t20, correct_latin_kernel__discontinuity_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(corr_tr_t40, correct_latin_kernel__discontinuity_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(corr_tr_t60, correct_latin_kernel__discontinuity_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(corr_tr_t80, correct_latin_kernel__discontinuity_reading, theater_ratio, 80, 0.6).
narrative_ontology:measurement(corr_tr_t100, correct_latin_kernel__discontinuity_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__discontinuity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(corr_be_t20, correct_latin_kernel__discontinuity_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(corr_be_t40, correct_latin_kernel__discontinuity_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(corr_be_t60, correct_latin_kernel__discontinuity_reading, base_extractiveness, 60, 0.73).
narrative_ontology:measurement(corr_be_t80, correct_latin_kernel__discontinuity_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(corr_be_t100, correct_latin_kernel__discontinuity_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__discontinuity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(corr_su_t20, correct_latin_kernel__discontinuity_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(corr_su_t40, correct_latin_kernel__discontinuity_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(corr_su_t60, correct_latin_kernel__discontinuity_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(corr_su_t80, correct_latin_kernel__discontinuity_reading, suppression_requirement, 80, 0.65).
narrative_ontology:measurement(corr_su_t100, correct_latin_kernel__discontinuity_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the discontinuity reading of the correct_latin_kernel. The kernel decomposes into three structurally distinct claims: continuity (natural evolution, low extraction), discontinuity (distinct systems requiring reconstruction, substantially extractive), and hybrid (layered recovery, intermediate extraction). Each reading carries its own epsilon, beneficiary structure, and classification. This reading posits the strongest classical-target mandate and yields the highest extractiveness of the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
