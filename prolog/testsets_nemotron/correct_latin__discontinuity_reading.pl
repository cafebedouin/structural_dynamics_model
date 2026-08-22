% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Correct Latin as Classical Reconstruction (Discontinuity Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The discontinuity reading of 'correct Latin' declares Classical Latin (c.
 *   75 BCE – 175 CE) the sole legitimate standard, treats all later Latin as
 *   corrupt deviation, and mandates reconstruction from textual sources as
 *   the only valid method. This reading became institutionalized through
 *   humanist education, the printing press, and the rise of classical
 *   philology as a university discipline. It coordinates textual scholarship
 *   around a single norm but extracts epistemic authority from
 *   continuous-practice communities — especially the Church and medieval
 *   Latin specialists — whose living usage is delegitimized. The constraint
 *   functions as a scaffold: its declared sunset was the completion of the
 *   Classical corpus reconstruction (largely achieved by c. 1900), yet it
 *   persists as the default standard in Classics departments and school
 *   curricula.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.38).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.52).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.21).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.21).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, scaffold).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Correct Latin as Classical Reconstruction (Discontinuity Reading)").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).
narrative_ontology:has_sunset_clause(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, '1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4').
narrative_ontology:cs_kernel_codification('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', fixed_text).
narrative_ontology:cs_authority_grounding('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', lineage).
narrative_ontology:cs_interpretation_layer_present('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4').
narrative_ontology:cs_reading_relation('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', correct_latin__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', foundational, classical_corpus_as_exclusive_norm).
narrative_ontology:cs_axiom_status(classical_corpus_as_exclusive_norm, holdable).
narrative_ontology:cs_axiom_grounding('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', classical_corpus_as_exclusive_norm, conventional).
narrative_ontology:cs_axiom('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', foundational, medieval_transmission_as_corrupt).
narrative_ontology:cs_axiom_status(medieval_transmission_as_corrupt, holdable).
narrative_ontology:cs_axiom_grounding('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', medieval_transmission_as_corrupt, empirically_contingent).
narrative_ontology:cs_axiom('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', secondary, reconstruction_from_texts_as_only_method).
narrative_ontology:cs_axiom_status(reconstruction_from_texts_as_only_method, holdable).
narrative_ontology:cs_axiom_grounding('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', reconstruction_from_texts_as_only_method, conventional).
narrative_ontology:cs_reference_frame('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', humanist_restoration_ideal).
narrative_ontology:cs_drift_state('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', post_critical_edition_completion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1fdc99a8-22cf-4a9a-9a96-fd1acacc7dd4', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classicist_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, humanist_educators).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, textual_critics).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_latin_practitioners).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, ecclesiastical_institutions).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, vernacular_scholars).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, classical_purity_doctrine).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, textual_authority_over_living_usage).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, historical_rupture_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the canon of 'correct' Latin through editorial practice, grammars, and academic appointments. They hold institutional authority in universities and academies to certify what counts as legitimate Latin. Their exit options include moving to adjacent fields (Greek, Romance philology) or leveraging their textual expertise for cultural capital.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classicist_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).

% Teach reconstructed Classical Latin as the exclusive standard of correctness in secondary and university curricula. They benefit from a stable, teachable norm that aligns with printed editions and examination systems. Their practice depends on the constraint's enforcement; switching to a continuity model would require new textbooks, retraining, and revised assessment.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, humanist_educators, beneficiary,
    organized, biographical, constrained, continental).

% Produce critical editions that reconstruct Classical texts from manuscript witnesses, often emending medieval transmissions. Their professional legitimacy and publication record depend on the premise that the medieval transmission is corrupt and requires expert reconstruction. They can pivot to other textual traditions but their methodological identity is bound to the emendation paradigm.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, textual_critics, beneficiary,
    organized, biographical, mobile, continental).

% Scholars working on medieval Latin texts whose usage is labeled 'corrupt' or 'degenerate' by the dominant standard. Their primary sources are treated as evidence for reconstruction rather than as legitimate Latin in their own right. They bear the cost of having their field's object of study delegitimized; exit requires reframing their work as 'Latin of the Middle Ages' rather than 'bad Classical Latin,' which marginalizes them in classicist-dominated departments.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_latin_practitioners, payer,
    moderate, biographical, constrained, continental).

% The Catholic Church and other bodies that maintained continuous Latin liturgical, legal, and administrative practice through the medieval and early modern periods. Their living Latin tradition is declared 'incorrect' by the reconstruction standard. They bear the cost of either adopting the reconstructed norm (abandoning their continuous practice) or being labeled as perpetuating corruption. Their identity is fused with Latin as a living sacramental language; exit from the constraint would fracture their self-understanding.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, ecclesiastical_institutions, payer,
    institutional, civilizational, identity_locked, global).

% Scholars of Romance languages and medieval vernaculars who see Latin's evolution as continuous with their field. They are excluded from the 'correct Latin' conversation because their evidence (vernacular outcomes, medieval usage) is treated as noise rather than data. They would argue for a continuity model but lack standing in classicist-controlled journals and curricula.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, vernacular_scholars, excluded,
    moderate, biographical, constrained, continental).

% Observes the constraint from outside the philological tradition, applying historical linguistics and sociolinguistic theory. Sees the 'correct Latin' standard as a constructed norm with extractive effects on medievalists and ecclesiastical users, while acknowledging its genuine coordination function for textual scholarship.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, analytical_linguist, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, teachable standard of 'correct Latin' for textual scholarship, education, and cross-generational communication about Classical texts — solving the coordination problem of which Latin to print, teach, and cite when the manuscript tradition is variant and the living practice has diverged.
% TRANSFER_FUNCTION: Moves epistemic authority and curricular control from continuous-practice communities (ecclesiastical, medievalist) to reconstruction specialists (classicists, textual critics). The arrangement transfers the power to define correctness from those who kept Latin alive to those who reconstruct it from texts.
% ABSENT_VOICES: Living Latin practitioners outside the academy (contemporary spoken-Latin communities, some traditionalist clergy) who maintain continuous oral practice but are excluded from the philological definition of correctness. Neo-Latin authors of the early modern period who wrote in a living Latin later declared 'post-Classical' and therefore secondary.
% DISAPPEARANCE_RATIONALE: If the discontinuity constraint vanished, medieval Latin would be rehabilitated as legitimate evolved Latin; ecclesiastical practice would regain epistemic parity with reconstructed Classical norms; textual criticism would shift from emendation to transmission history; curricula would teach Latin as a continuous tradition with period variation rather than a ruptured norm requiring reconstruction.
% FOUNDING_PROBLEM: Renaissance humanists encountered a manuscript tradition full of medieval corruptions and a living ecclesiastical Latin that had diverged significantly from Cicero and Virgil. They needed a stable standard to restore the 'pure' language of antiquity for cultural legitimacy and educational coherence.
% FOUNDING_PROBLEM_CORROBORATION: Humanist correspondence (Erasmus, Valla, Poliziano) attests the founding problem from within the benefiting tradition. Medievalists (e.g., Mantello & Rigg, 'Medieval Latin: An Introduction and Bibliographical Guide') and historians of education (e.g., Black, 'Humanism and Education in Medieval and Renaissance Italy') corroborate from outside: the 'corruption' narrative was a polemical construction, not a neutral linguistic observation; the living Latin of 1400 was functionally adequate for its users.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).
:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38) reflects the constraint's dual character: genuine coordination for textual scholarship (low inherent extraction) plus asymmetric delegation of authority from continuous practitioners to reconstruction specialists. Suppression (0.52) is moderate — the constraint persists through curricular monopoly and editorial gatekeeping, not physical coercion, but the exclusion is structural. Theater ratio (0.21) is low: the reconstruction function is real and ongoing (new editions, new manuscripts), but a growing share of enforcement activity (curricular mandates, 'correctness' policing) serves identity-maintenance rather than textual necessity. Accessibility collapse (0.68) is high: once the discontinuity premise is accepted, alternatives (continuity models, living Latin) become epistemically invisible within the field. Resistance (0.44) is moderate: medievalists and ecclesiastical scholars have pushed back, but the constraint's institutional embeddedness in Classics makes full reversal unlikely.
 *
 * PERSPECTIVAL GAP:
 *   From the classicist seat, the constraint is a rope: it solves the real coordination problem of textual stability. From the ecclesiastical seat, it is a snare: a living tradition of 1500 years is declared corrupt by outsiders who then sell the 'reconstructed' version back. From the medievalist seat, it is a tangled rope: they need the editorial infrastructure the constraint provides, but pay for it with delegitimization. The engine computes this divergence from the structural data — the authored claim (scaffold) captures the transitional intent but not the persistent extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Classicist philologists and textual critics are structural beneficiaries (d ~ 0.15–0.25): they collect professional legitimacy, publication venues, and curricular control from the constraint. Humanist educators are near-symmetric beneficiaries (d ~ 0.4): they gain a teachable standard but bear adaptation costs. Medieval Latin practitioners and ecclesiastical institutions are targets (d ~ 0.75–0.85): their living practice is declared incorrect, their authority transferred. Vernacular scholars are excluded (d not computed — they are not in the coordination game). The analytical observer sits at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (restoring a stable Classical standard for Renaissance culture) was substantially solved by the late 19th century: critical editions of all major Classical authors exist, the textual corpus is stabilized. Yet the constraint persists in curricula and disciplinary identity. The mandate has atrophied: what remains is coordination around a finished reconstruction, maintained because the beneficiaries (classicists, educators) have institutionalized the standard and the victims (medievalists, Church) lack the power to dislodge it. This is not pure extraction (the coordination function remains real for textual work) but the extraction/coordination ratio has inverted since the mandate's completion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reconstruction_necessity,
    'Is textual reconstruction from manuscript witnesses structurally necessary for accessing Classical Latin, or could a continuity-based approach (using medieval manuscripts as direct evidence of the evolving language) achieve comparable results?',
    'Comparative analysis of editorial outcomes: do discontinuity-based editions (emending toward Classical norms) and continuity-based editions (recording medieval transmission faithfully) produce different historical-linguistic conclusions about the Classical period?',
    'If continuity-based editing yields equivalent or better access to Classical Latin, the discontinuity constraint''s coordination function is overstated and its extraction is unjustified. If reconstruction is genuinely necessary, the coordination function is validated and the extraction is the price of that function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_necessity, empirical, 'Whether the discontinuity method is epistemically necessary or a choice that benefits its practitioners.').

omega_variable(
    ecclesiastical_continuity_legitimacy,
    'Does the Catholic Church''s continuous Latin practice (liturgical, legal, curial) from antiquity to the present constitute a legitimate transmission of the language, or is it genuinely a ''corrupt'' deviation that requires correction?',
    'Sociolinguistic analysis of the Church''s Latin: does it maintain grammatical continuity, semantic stability, and communicative functionality across the Classical-medieval boundary? Comparative assessment against the criteria used to label it ''corrupt.''',
    'If ecclesiastical Latin is a legitimate continuity, the discontinuity reading''s victim structure is confirmed (a living tradition delegitimized). If it is genuinely degenerate, the reading''s extraction from the Church is a side effect of a genuine epistemic standard.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecclesiastical_continuity_legitimacy, conceptual, 'Whether the primary victim group''s practice is legitimately continuous or genuinely corrupt — the core factual dispute underpinning the constraint''s moral topology.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''correct Latin'' kernel best framed as a linguistic norm (what counts as grammatical Latin), an educational standard (what gets taught), a textual protocol (what gets printed), or an identity claim (who owns Latin)? Different framings yield different cs_pattern classifications.',
    'Trace the constraint''s actual enforcement sites: university curricula (educational), critical editions (textual), ecclesiastical documents (institutional/identity), spoken-Latin communities (identity). Map which framing predicts the observed enforcement pattern.',
    'If framed as educational standard → scaffold (transitional, sunset at corpus completion). If framed as identity claim → snare (persistent extraction from excluded identities). If framed as textual protocol → rope (genuine coordination). The framing choice determines the constraint''s structural classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'CS-framing under-determination: the kernel admits multiple coherent structural framings that produce different classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 1400, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1400, correct_latin__discontinuity_reading, theater_ratio, 1400, 0.05).
narrative_ontology:measurement(corr_tr_t1500, correct_latin__discontinuity_reading, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(corr_tr_t1600, correct_latin__discontinuity_reading, theater_ratio, 1600, 0.12).
narrative_ontology:measurement(corr_tr_t1700, correct_latin__discontinuity_reading, theater_ratio, 1700, 0.16).
narrative_ontology:measurement(corr_tr_t1800, correct_latin__discontinuity_reading, theater_ratio, 1800, 0.19).
narrative_ontology:measurement(corr_tr_t1900, correct_latin__discontinuity_reading, theater_ratio, 1900, 0.21).

% Extraction over time
narrative_ontology:measurement(corr_be_t1400, correct_latin__discontinuity_reading, base_extractiveness, 1400, 0.15).
narrative_ontology:measurement(corr_be_t1500, correct_latin__discontinuity_reading, base_extractiveness, 1500, 0.22).
narrative_ontology:measurement(corr_be_t1600, correct_latin__discontinuity_reading, base_extractiveness, 1600, 0.31).
narrative_ontology:measurement(corr_be_t1700, correct_latin__discontinuity_reading, base_extractiveness, 1700, 0.36).
narrative_ontology:measurement(corr_be_t1800, correct_latin__discontinuity_reading, base_extractiveness, 1800, 0.38).
narrative_ontology:measurement(corr_be_t1900, correct_latin__discontinuity_reading, base_extractiveness, 1900, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1400, correct_latin__discontinuity_reading, suppression_requirement, 1400, 0.25).
narrative_ontology:measurement(corr_su_t1500, correct_latin__discontinuity_reading, suppression_requirement, 1500, 0.35).
narrative_ontology:measurement(corr_su_t1600, correct_latin__discontinuity_reading, suppression_requirement, 1600, 0.44).
narrative_ontology:measurement(corr_su_t1700, correct_latin__discontinuity_reading, suppression_requirement, 1700, 0.5).
narrative_ontology:measurement(corr_su_t1800, correct_latin__discontinuity_reading, suppression_requirement, 1800, 0.52).
narrative_ontology:measurement(corr_su_t1900, correct_latin__discontinuity_reading, suppression_requirement, 1900, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__discontinuity_reading, 0.03).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, latin_education_standard).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, textual_criticism_methodology).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, ecclesiastical_latin_authority).

% DUAL FORMULATION NOTE:
% Part of the correct_latin constraint family. This reading (discontinuity) declares rupture and external reconstruction; continuity_reading declares unbroken transmission; hybrid_reading declares partial continuity with targeted reform. The three readings share the kernel 'correct Latin' but instantiate different constraints with different ε, different beneficiary/victim structures, and different types. The discontinuity reading's ε (0.38) is higher than the continuity reading's would be (~0.15) because it extracts from continuous practitioners; the hybrid reading sits between.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__discontinuity_reading, institutional, 0.15).
constraint_indexing:directionality_override(correct_latin__discontinuity_reading, institutional, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
