% ============================================================================
% CONSTRAINT STORY: correct_latin__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: correct_latin__continuity_reading
 *   human_readable: Continuity Reading: Latin Correctness as Living Transmitted Practice
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint instantiates the continuity reading of the contested
 *   'correct Latin' kernel: the position that legitimate Latin is defined by
 *   unbroken transmission through living usage, such that medieval Latin is
 *   not a corruption of Classical Latin but its natural evolutionary
 *   continuation. This reading underwrites the professional legitimacy of
 *   medieval Latin studies, Romance historical linguistics, and living-Latin
 *   pedagogy, while displacing the normative authority of Classical-purist
 *   reconstruction. Two sibling constraints — discontinuity_reading
 *   (Classical form as fixed textual standard, medieval Latin as corruption)
 *   and hybrid_reading (partial continuity with targeted textual correction)
 *   — are separate constraint files with their own ε, beneficiaries, and
 *   classification; this file does not adjudicate between them, does not
 *   average their claims, and does not describe their contest internally. It
 *   authors one reading, cleanly, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - medieval_and_ecclesiastical_latinists: primary beneficiary — professional legitimacy
 *   - vernacular_romance_philologists: primary beneficiary — theoretical foundation
 *   - working_latin_pedagogues: beneficiary — pedagogical resource base
 *   - classicizing_humanist_purists: primary payer — loses gatekeeping authority
 *   - historical_linguists_observer: analytical observer — assesses evidentiary fit independent of professional stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__continuity_reading, 0.38).
domain_priors:suppression_score(correct_latin__continuity_reading, 0.42).
domain_priors:theater_ratio(correct_latin__continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__continuity_reading, rope).
narrative_ontology:human_readable(correct_latin__continuity_reading, "Continuity Reading: Latin Correctness as Living Transmitted Practice").
narrative_ontology:topic_domain(correct_latin__continuity_reading, "historical_linguistics/philology/intellectual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__continuity_reading, '3666441b-0d66-471e-879d-874c3d40fb67').
narrative_ontology:cs_kernel_codification('3666441b-0d66-471e-879d-874c3d40fb67', distributed).
narrative_ontology:cs_authority_grounding('3666441b-0d66-471e-879d-874c3d40fb67', practice).
narrative_ontology:cs_interpretation_layer_present('3666441b-0d66-471e-879d-874c3d40fb67').
narrative_ontology:cs_reading_relation('3666441b-0d66-471e-879d-874c3d40fb67', correct_latin__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('3666441b-0d66-471e-879d-874c3d40fb67', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('3666441b-0d66-471e-879d-874c3d40fb67', foundational, usage_continuity_constitutes_correctness).
narrative_ontology:cs_axiom_status(usage_continuity_constitutes_correctness, holdable).
narrative_ontology:cs_axiom_grounding('3666441b-0d66-471e-879d-874c3d40fb67', usage_continuity_constitutes_correctness, conventional).
narrative_ontology:cs_axiom('3666441b-0d66-471e-879d-874c3d40fb67', foundational, no_principled_classical_medieval_rupture_exists).
narrative_ontology:cs_axiom_status(no_principled_classical_medieval_rupture_exists, holdable).
narrative_ontology:cs_axiom_grounding('3666441b-0d66-471e-879d-874c3d40fb67', no_principled_classical_medieval_rupture_exists, empirically_contingent).
narrative_ontology:cs_reference_frame('3666441b-0d66-471e-879d-874c3d40fb67', unbroken_usage_continuity_standard).
narrative_ontology:cs_drift_state('3666441b-0d66-471e-879d-874c3d40fb67', post_carolingian_correctio_reception, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3666441b-0d66-471e-879d-874c3d40fb67', '').
narrative_ontology:cs_kernel_id(correct_latin__continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, medieval_and_ecclesiastical_latinists).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, vernacular_romance_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, working_latin_pedagogues).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, monastic_and_chancery_scribal_communities).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, classicizing_humanist_purists).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, linguistic_continuity_thesis).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, usage_based_norm_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study, teach, and edit medieval Latin texts (scholastic, liturgical, chancery, monastic) as legitimate objects in their own right rather than as corrupted deviations to be filtered out. The continuity reading validates their entire corpus as 'correct Latin' rather than a degraded shadow of Classical usage, which underwrites their field's institutional standing, funding, and curricular space.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, medieval_and_ecclesiastical_latinists, beneficiary,
    moderate, generational, mobile, continental).

% Trace the unbroken chain from spoken Late Latin through medieval registers into the Romance vernaculars. The continuity reading supplies the theoretical backbone for their entire discipline — without a living, evolving Latin there is no coherent bridge narrative from Cicero to Dante, and their explanatory apparatus collapses into a series of unexplained ruptures.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, vernacular_romance_philologists, beneficiary,
    moderate, generational, mobile, continental).

% Teach living, spoken, or actively composed Latin (in seminaries, immersion programs, some university departments) and rely on medieval and Neo-Latin texts as pedagogically rich, grammatically legitimate material. The continuity reading lets them draw on a millennium of continuously produced Latin rather than being restricted to a narrow Classical canon they must treat as the only authentic register.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, working_latin_pedagogues, beneficiary,
    powerless, biographical, constrained, national).

% Historical (non-agent, retrospective) beneficiary: the generations of scribes, notaries, and clerics whose actual usage constitutes the evidentiary basis of medieval Latin are retroactively vindicated as having practiced 'correct' Latin rather than having merely degraded it. Named for completeness; they cannot act on or collect from this vindication.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, monastic_and_chancery_scribal_communities, beneficiary,
    powerless, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(correct_latin__continuity_reading, monastic_and_chancery_scribal_communities).

% Hold that only Ciceronian/Augustan-era Latin is the true standard and that medieval forms are corruptions to be corrected or excluded. Under the continuity reading their entire reform program — restoring 'purity' by rejecting medieval vocabulary, syntax, and orthography — loses its legitimating premise; the reading treats their target of correction as itself correct. They bear a reputational and pedagogical cost: their editorial and normative authority over 'what counts as good Latin' is narrowed.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, classicizing_humanist_purists, payer,
    moderate, generational, constrained, continental).

% Editors who reconstruct Classical readings by emending manuscripts against medieval scribal 'corruptions.' Their editorial practice presupposes a discontinuity between authentic Classical language and later scribal transmission. The continuity reading does not directly engage them in this file (that is the discontinuity reading's domain) but their objection — that not all divergence from Classical usage is legitimate evolution, some of it is copying error — has no seat here.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, textual_reconstruction_editors, excluded,
    moderate, biographical, constrained, continental).

% Study language change as a general phenomenon and assess whether the continuity model (gradual, unbroken transmission with no principled break point) fits the documentary and comparative evidence for Latin's evolution, independent of which community's professional interests the finding serves.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, historical_linguists_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent normative standard — 'Latin is whatever the unbroken chain of living practice produced' — that lets medievalists, Romance philologists, and living-Latin pedagogues treat their overlapping corpora as one continuous, internally consistent object of study and teaching, rather than requiring each period to justify itself against an external Classical benchmark.
% TRANSFER_FUNCTION: Moves legitimating authority away from Classical-purist editors and grammarians (who would otherwise gatekeep what counts as 'real' Latin) toward medievalists, vernacular philologists, and communities of ongoing usage — control over the definition of 'correct Latin' shifts from a fixed textual canon to the accumulated evidentiary record of practice.
% ABSENT_VOICES: Classicizing humanist purists are present as payers, but the deeper absent voice is the ordinary historical population whose speech constituted 'living practice' and left no record — the reading valorizes attested continuity, but its evidentiary base is skewed toward literate, scribal, and clerical registers, silently excluding non-literate spoken registers that may have diverged faster or differently than the written record shows.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished as a live position, medieval Latin studies would lose its foundational legitimating premise overnight — departments, editions, and pedagogical programs built on 'medieval Latin as legitimate Latin' would need to either rebrand as studying a corrupted register or migrate wholesale to a hybrid/discontinuity framework, and Romance historical linguistics would lose its standard bridging narrative between Classical and vernacular forms.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century philology needed a framework that could explain how Classical Latin became the Romance languages without treating a thousand years of documented medieval usage as simply wrong — the continuity reading solved the problem of what to do with an enormous, continuously produced textual record that didn't match Classical norms but was too extensive and too functionally 'Latin' to dismiss.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists working on Romance language formation (a field with no institutional stake in defending either medievalists or Classical purists) corroborate that documented Latin usage shows gradual, unbroken phonological and morphosyntactic change with no clean break point separating 'Classical' from 'medieval' — this is attested independently of the medievalist community's professional interest in the continuity framing, via comparative reconstruction and dated inscriptional/documentary evidence.
narrative_ontology:disappearance_verdict(correct_latin__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__continuity_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__continuity_reading_tests).
:- end_tests(correct_latin__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.38 at interval end) because the continuity reading is primarily a legitimating framework rather than a resource-extraction mechanism — its main effect is redistributing scholarly and pedagogical authority, not material rents. It rises modestly over the measured interval as medieval Latin studies professionalized and institutionalized (more journals, chairs, editions built on the continuity premise, deepening the stakes for purist objectors). Suppression is moderate (0.42): the reading does not physically bar Classical-purist scholarship, but it does structurally marginalize purist normative claims within mainstream historical linguistics departments, and purist objections increasingly read as methodologically outdated rather than as a live alternative. Theater ratio is low-moderate (0.28) — the coordination function (a coherent account of Latin's diachronic development) is substantially real, not performative, though some invocation of 'living practice' in pedagogical contexts functions more as institutional branding than close textual argument. Resistance (0.55) is real and organized: classicizing purists have sustained institutional and editorial counter-traditions for over a century.
 *
 * DIRECTIONALITY LOGIC:
 *   Medievalists, Romance philologists, and living-Latin pedagogues are beneficiaries: the reading directly grounds their disciplinary legitimacy and expands their legitimate object of study, so directionality sits near the beneficiary end (low d). Classicizing purists are the structural payer: the reading narrows their claimed authority to define 'correct' Latin and displaces their reconstructive-correction project from settled orthodoxy to one contested position among several — moderate-high d, tempered by the fact that purist scholarship persists and retains institutional footholds (constrained, not trapped, exit). Textual reconstruction editors are excluded from this file's frame entirely rather than positioned as direct payers — their objection belongs structurally to the discontinuity reading, which is why they carry an `excluded` role here rather than `payer`. Monastic and chancery scribal communities are named as non-agent historical beneficiaries for evidentiary completeness; they cannot collect from a vindication issued centuries after the fact, and the `agent: false` flag prevents them from feeding the directionality computation as if they could act on the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (explaining how documented medieval Latin usage relates to Classical norms without either discarding a millennium of text or treating Classical Latin as arbitrary) remains live: comparative historical linguistics still needs an account of gradual change versus discrete corruption, and the evidentiary basis for gradual transmission (inscriptions, dated documents, comparative Romance reconstruction) is corroborated by observers outside the medievalist community's own professional interest. This is not a case of a mandate outliving its function — the underlying empirical question the reading answers is still contested and still generates research, which is why founding_problem_status is 'live' rather than 'dead.' The reading does not need Mandatrophy resolution; it needs continued engagement with its sibling readings, which this file deliberately routes to separate constraint stories rather than resolving internally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_discontinuity_evidentiary_status,
    'Does the documentary and comparative-linguistic evidence actually support unbroken gradual transmission from Classical to medieval Latin, or does it show identifiable rupture points (e.g., the Carolingian correctio, which self-consciously distinguished ''correct'' Latin from contemporary spoken Romance)?',
    'Systematic comparative analysis of dated inscriptional, documentary, and literary evidence across the transition period, cross-checked against Romance comparative reconstruction, conducted by historical linguists without institutional stake in either the medievalist or classicist professional communities.',
    'If rupture points are identifiable and linguistically significant (not merely ideological self-descriptions by contemporary grammarians), the continuity reading''s core premise weakens and the discontinuity or hybrid reading gains evidentiary support; if the evidence shows genuinely gradual change with no principled break, the continuity reading is strengthened relative to its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_discontinuity_evidentiary_status, empirical, 'Whether the empirical record supports gradual continuity or identifiable rupture between Classical and medieval Latin.').

omega_variable(
    committer_framing_selection,
    'Is the continuity/discontinuity/hybrid trichotomy the only defensible way to decompose the ''correct Latin'' kernel, or does the very act of asking ''which reading is right'' presuppose a single-answer framework that the historical evidence does not actually support (i.e., correctness may be irreducibly register- and community-relative with no unified standard ever having existed)?',
    'Examine whether medieval Latin-users themselves operated with a unified correctness standard or with multiple co-existing, non-competing registers (liturgical, notarial, literary) that were never adjudicated against a single external norm — if the latter, the kernel itself may be a modern philological retrojection.',
    'If correctness was always plural and register-relative even within the medieval period, the continuity reading (like its siblings) may be answering a question shaped by modern academic disputes rather than by any question medieval Latin-users themselves needed answered — this would not eliminate the reading''s institutional function but would relocate its ε referent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_selection, conceptual, 'Whether the continuity/discontinuity/hybrid trichotomy itself reflects the historical reality or is a modern academic imposition on it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(corr_tr_t20, correct_latin__continuity_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(corr_tr_t40, correct_latin__continuity_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(corr_tr_t60, correct_latin__continuity_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(corr_tr_t80, correct_latin__continuity_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(corr_tr_t100, correct_latin__continuity_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__continuity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(corr_be_t20, correct_latin__continuity_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(corr_be_t40, correct_latin__continuity_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement(corr_be_t60, correct_latin__continuity_reading, base_extractiveness, 60, 0.34).
narrative_ontology:measurement(corr_be_t80, correct_latin__continuity_reading, base_extractiveness, 80, 0.36).
narrative_ontology:measurement(corr_be_t100, correct_latin__continuity_reading, base_extractiveness, 100, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(correct_latin__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the correct_latin kernel. discontinuity_reading authors ε for the position that Classical Latin is the fixed correct standard and medieval Latin is corruption requiring textual reconstruction (a much higher-suppression, more extraction-oriented reading favoring purist editorial authority). hybrid_reading authors ε for the position that partial continuity holds but targeted correction against textual evidence remains legitimate (a moderate reading). Each reading has its own stable ε and its own beneficiary/victim structure per the ε-invariance principle; they are not to be averaged or reconciled into a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
