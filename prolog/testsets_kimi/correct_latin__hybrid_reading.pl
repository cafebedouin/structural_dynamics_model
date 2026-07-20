% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Correct Latin Hybrid Reading (Classical Form Transmitted Through Medieval Practice, Correctable by Textual Evidence)
 *   domain: historical_linguistics/philology
 *
 * SUMMARY:
 *   This constraint story models the hybrid reading of the 'correct Latin'
 *   kernel: the claim that legitimate Latin is the Classical form as
 *   transmitted through medieval practice, subject to correction by textual
 *   evidence from antiquity. It is one of three readings of a contested
 *   kernel, situated between the continuity reading (medieval Latin as fully
 *   legitimate evolution) and the discontinuity reading (medieval Latin as
 *   corrupt deviation requiring reconstruction). The constraint coordinates
 *   transnational scholarly and ecclesiastical communication while extracting
 *   authority and status from medieval practitioners and students compelled
 *   to conform. It is authored as a tangled rope: genuine coordination
 *   function (shared learned lingua franca) combined with asymmetric
 *   extraction (concentration of definitional authority in humanist editors,
 *   delegitimization of scholastic Latin).
 *
 * KEY AGENTS:
 *   - Humanist scholars (agenda_setter/organized/mobile): Primary beneficiaries and rule-setters who recovered classical texts and established editorial criteria for correctness.
 *   - Medieval scholastics (payer/moderate/identity_locked): Primary targets whose linguistic practice was reclassified as corrupt, bearing status loss and retraining costs.
 *   - Students and clergy (payer/powerless/constrained): Compelled learners who bore the compliance costs of the new standard for educational and ecclesiastical advancement.
 *   - Ecclesiastical institutions (agenda_setter+payer/institutional/constrained): Enforced the standard through education and liturgy while being subjected to humanist correction of their own textual traditions.
 *   - Vernacular intellectuals (excluded/powerless/trapped): Excluded from the prestige economy of correct Latin, trapped in local audiences.
 *   - Print publishers (beneficiary/organized/mobile): Profited from standardization through textbook and edition markets.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.62).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.58).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Correct Latin Hybrid Reading (Classical Form Transmitted Through Medieval Practice, Correctable by Textual Evidence)").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "historical_linguistics/philology").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, 'e9ffb2af-a692-4ba9-a8ca-dd176dc6598d').
narrative_ontology:cs_kernel_codification('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d', fixed_text).
narrative_ontology:cs_authority_grounding('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d', lineage).
narrative_ontology:cs_interpretation_layer_present('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d').
narrative_ontology:cs_reading_relation('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d', correct_latin__continuity_reading, influences).
narrative_ontology:cs_reading_relation('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d', correct_latin__discontinuity_reading, influences).
narrative_ontology:cs_axiom('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d', foundational, textually_corrected_continuity).
narrative_ontology:cs_axiom_status(textually_corrected_continuity, holdable).
narrative_ontology:cs_axiom_grounding('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d', textually_corrected_continuity, conventional).
narrative_ontology:cs_axiom('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d', foundational, grammatical_core_legitimacy).
narrative_ontology:cs_axiom_status(grammatical_core_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d', grammatical_core_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d', classical_medieval_synthesis).
narrative_ontology:cs_drift_state('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d', modern_descriptive_linguistics, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e9ffb2af-a692-4ba9-a8ca-dd176dc6598d', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, humanist_scholars).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, print_publishers).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, medieval_scholastics).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, students_clergy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, ecclesiastical_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Renaissance humanists who recovered, edited, and printed classical Latin texts, establishing the criteria by which medieval manuscripts and usage were to be judged and corrected. They set the standard for correct Latin through prefaces, emended editions, and pedagogical grammars, gaining intellectual authority, court patronage, and institutional influence from their role as arbiters of linguistic purity.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, humanist_scholars, agenda_setter,
    organized, generational, mobile, continental).

% Printers and publishing houses that standardized and distributed humanist editions, grammars, and dictionaries across Europe. They benefited from the demand generated by the new correctness standard, creating a repeatable market for textbooks, reference works, and corrected classical texts aimed at schools and courts.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, print_publishers, beneficiary,
    organized, biographical, mobile, continental).

% University theologians, philosophers, and administrators trained in the scholastic Latin tradition whose grammatical habits, syntactic preferences, and technical vocabulary were publicly delegitimized by humanist critics. Their professional identity and accumulated scholarly output were reclassified as corrupt or barbarous, forcing costly linguistic retraining or precipitating status loss.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medieval_scholastics, payer,
    moderate, biographical, identity_locked, continental).

% Students in grammar schools, universities, and seminaries compelled to master the new humanist Latin through corrected textbooks, examinations, and liturgical reforms. They bore the time, effort, and opportunity costs of conforming to a standard that rendered their existing linguistic backgrounds deficient, with advancement in church or state contingent on compliance.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, students_clergy, payer,
    powerless, biographical, constrained, regional).

% The Catholic Church and its administrative hierarchy, which controlled education and liturgical language. They enforced Latin standards through curricula, synodal decrees, and liturgical books, but also suffered humanist correction of their medieval textual and administrative traditions, ultimately adopting the hybrid standard after the Council of Trent while ceding some autonomy over language evolution.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, ecclesiastical_institutions, payer).

% Scholars, poets, and thinkers working in Italian, French, German, and other vernaculars who were structurally excluded from the prestige economy of correct Latin. Their intellectual production was denied the transnational legitimacy that mastery of the Latin standard conferred, trapping them in localized or national audiences and institutional settings.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, vernacular_intellectuals, excluded,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining a shared transnational language for scholarship, theology, and diplomacy across politically fragmented, vernacular-differentiated Europe, enabling communication and prestige circulation across boundaries that would otherwise fragment the Republic of Letters.
% TRANSFER_FUNCTION: Moves authority to define legitimate language from medieval scholastic communities to humanist editors and classical textual evidence; moves status, educational credentials, and print revenue from medieval Latin practitioners to those who master the corrected classical norm and the apparatus that teaches it.
% ABSENT_VOICES: Vernacular intellectuals and medieval scholastics whose Latin was devalued; they were structurally excluded from the humanist discourse that established correctness, though their ongoing practice constituted the majority of actual Latin usage in Europe.
% DISAPPEARANCE_RATIONALE: Without the hybrid standard, European intellectual communication would lose its shared high-register language or revert to unregulated medieval forms; the authority structure of early modern education, print culture, and ecclesiastical administration would reorganize around either pure classical reconstruction, vernacular fragmentation, or unreformed scholastic continuity.
% FOUNDING_PROBLEM: The perceived corruption and regional fragmentation of Latin in the late Middle Ages, which threatened its function as a universal learned and liturgical lingua franca; classical texts offered an authoritative alternative source of stability and uniformity.
% FOUNDING_PROBLEM_CORROBORATION: Humanist scholars attest the problem was real and urgent. Modern historical linguists attest that medieval Latin was a legitimate evolutionary stage rather than a corruption; external corroboration from sociolinguistic history supports the functional-continuity reading, undermining the corruption narrative that justified the constraint's extraction.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the standard concentrates authority to define legitimacy in a narrow humanist editorial class and imposes heavy compliance costs on existing medieval practitioners, yet it remains below snare levels because the coordination function (transnational communication) is genuine and not merely cover. Suppression (0.58) reflects active pedagogical and institutional enforcement that sidelines medieval Latin alternatives without physically extinguishing them. Theater ratio (0.45) captures the significant performative dimension of humanist correctionâdisplay of classical erudition as status markerâwhile acknowledging real textual labor. Resistance (0.55) reflects documented scholastic and conservative ecclesiastical pushback against humanist linguistic reforms. The temporal series show extraction and enforcement rising through the fifteenth and sixteenth centuries as humanism gained institutional power, then slightly normalizing by 1700 as the standard became hegemonic and required less active suppression.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (humanist scholars) experiences the constraint as restoration of a natural order and genuine service to the Republic of Letters; the engine should compute a low directionality and mild effective extraction for this seat. The payer seats (medieval scholastics, constrained students) experience the same structure as an arbitrary imposition of unfamiliar norms that devalue their existing competence; the engine should compute high directionality and amplified extraction. Ecclesiastical institutions sit ambiguously between beneficiary and target due to their dual role as enforcers and subjects of correction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (humanist_scholars, print_publishers) derive low directionality from their structural position: they collect authority and revenue from the constraint's operation. Victims (medieval_scholastics, students_clergy) derive high directionality because they bear the costs of conformity and delegitimization without controlling the standard. Vernacular intellectuals are excluded rather than coordinated; their exclusion is the boundary enforcement that makes the standard valuable. The ecclesiastical_institutions stakeholder is dually positioned as both enforcer and subject of correction, but the structural data route its primary directionality through the agenda_setter role; its secondary payer role is documented in the secondary_role field and commentary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling in both directions. Against a pure coordination (rope) reading, the presence of identifiable victims (medieval scholastics, students_clergy) and active enforcement blocks the classification from collapsing into benign coordination: the standard does not merely facilitate communication but actively punishes non-compliance with status loss. Against a pure extraction (snare) reading, the genuine coordination functionâcenturies of transnational scholarly communication facilitated by a shared high-register languageâblocks classification as mere cover: removing the constraint would genuinely fragment the European intellectual field, not merely liberate victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_vs_sibling_readings,
    'How would the classification shift if the continuity reading or discontinuity reading were adopted instead of this hybrid reading?',
    'Compare stakeholder sets and extraction profiles across the constraint family: the continuity reading would remove medieval_scholastics from the victim set (their Latin would be fully legitimate), pushing classification toward rope; the discontinuity reading would deny all medieval legitimacy, intensifying extraction and pushing classification toward snare.',
    'Continuity reading would likely compute as rope; discontinuity reading would likely compute as snare. The hybrid reading''s tangled_rope classification depends on its specific balance of partial legitimacy and corrective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_vs_sibling_readings, conceptual, 'Structural sensitivity of classification to adoption of sibling kernel readings').

omega_variable(
    textual_correction_scope,
    'What proportion of medieval Latin variance falls under orthographic/vocabulary correction versus contested grammatical restructuring?',
    'Corpus analysis comparing medieval manuscripts against classical norms across lexical, morphological, syntactic, and orthographic dimensions; philological case studies of specific humanist editorial interventions.',
    'If correction is predominantly lexical and orthographic, extraction is lower and the coordination function dominates. If the grammatical core is also heavily contested, effective extraction rises and the victim set expands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_correction_scope, empirical, 'Empirical scope of textual correction and its impact on extractiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 0, 350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corr_tr_t70, correct_latin__hybrid_reading, theater_ratio, 70, 0.3).
narrative_ontology:measurement(corr_tr_t140, correct_latin__hybrid_reading, theater_ratio, 140, 0.42).
narrative_ontology:measurement(corr_tr_t210, correct_latin__hybrid_reading, theater_ratio, 210, 0.48).
narrative_ontology:measurement(corr_tr_t280, correct_latin__hybrid_reading, theater_ratio, 280, 0.45).
narrative_ontology:measurement(corr_tr_t350, correct_latin__hybrid_reading, theater_ratio, 350, 0.43).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(corr_be_t70, correct_latin__hybrid_reading, base_extractiveness, 70, 0.45).
narrative_ontology:measurement(corr_be_t140, correct_latin__hybrid_reading, base_extractiveness, 140, 0.55).
narrative_ontology:measurement(corr_be_t210, correct_latin__hybrid_reading, base_extractiveness, 210, 0.62).
narrative_ontology:measurement(corr_be_t280, correct_latin__hybrid_reading, base_extractiveness, 280, 0.6).
narrative_ontology:measurement(corr_be_t350, correct_latin__hybrid_reading, base_extractiveness, 350, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(corr_su_t70, correct_latin__hybrid_reading, suppression_requirement, 70, 0.42).
narrative_ontology:measurement(corr_su_t140, correct_latin__hybrid_reading, suppression_requirement, 140, 0.55).
narrative_ontology:measurement(corr_su_t210, correct_latin__hybrid_reading, suppression_requirement, 210, 0.62).
narrative_ontology:measurement(corr_su_t280, correct_latin__hybrid_reading, suppression_requirement, 280, 0.58).
narrative_ontology:measurement(corr_su_t350, correct_latin__hybrid_reading, suppression_requirement, 350, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the correct_latin constraint family. The kernel 'correct Latin' decomposes into three structurally distinct readings because each reading assigns different Îµ values, beneficiary/victim structures, and directionalities. The hybrid reading links to its siblings to enable contamination propagation and family-level analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
