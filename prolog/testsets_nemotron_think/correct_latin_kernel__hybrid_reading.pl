% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Hybrid Latin Correctness Standard: Morphology Continuous, Syntax/Lexicon Recovered
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The hybrid reading of the 'correct Latin' kernel asserts that Latin's
 *   core morphology (inflectional system, verb conjugations, noun
 *   declensions) survived medieval transmission continuously, but that syntax
 *   (word order, subordinate clause structure, use of moods) and lexicon
 *   (vocabulary shifts, semantic drift, neologisms) were corrupted and
 *   required recovery from classical texts. This reading legitimizes a
 *   layered reconstruction: the medieval manuscript tradition is the
 *   substrate for morphology, but humanist emendation is the authority for
 *   syntax and lexicon. The constraint operates through critical editions,
 *   grammars, and curricula that present this hybrid as 'restored classical
 *   Latin' rather than 'constructed standard.'
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.48).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.42).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Hybrid Latin Correctness Standard: Morphology Continuous, Syntax/Lexicon Recovered").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, '86c6d2ec-79fe-459a-bfbf-22848ff23544').
narrative_ontology:cs_kernel_codification('86c6d2ec-79fe-459a-bfbf-22848ff23544', fixed_text).
narrative_ontology:cs_authority_grounding('86c6d2ec-79fe-459a-bfbf-22848ff23544', lineage).
narrative_ontology:cs_interpretation_layer_present('86c6d2ec-79fe-459a-bfbf-22848ff23544').
narrative_ontology:cs_reading_relation('86c6d2ec-79fe-459a-bfbf-22848ff23544', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('86c6d2ec-79fe-459a-bfbf-22848ff23544', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('86c6d2ec-79fe-459a-bfbf-22848ff23544', foundational, morphological_continuity_legitimate).
narrative_ontology:cs_axiom_status(morphological_continuity_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('86c6d2ec-79fe-459a-bfbf-22848ff23544', morphological_continuity_legitimate, conventional).
narrative_ontology:cs_axiom('86c6d2ec-79fe-459a-bfbf-22848ff23544', foundational, syntactic_lexical_recovery_required).
narrative_ontology:cs_axiom_status(syntactic_lexical_recovery_required, holdable).
narrative_ontology:cs_axiom_grounding('86c6d2ec-79fe-459a-bfbf-22848ff23544', syntactic_lexical_recovery_required, empirically_contingent).
narrative_ontology:cs_reference_frame('86c6d2ec-79fe-459a-bfbf-22848ff23544', humanist_emendation_standard).
narrative_ontology:cs_drift_state('86c6d2ec-79fe-459a-bfbf-22848ff23544', contemporary_philological_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('86c6d2ec-79fe-459a-bfbf-22848ff23544', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, renaissance_humanists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, textual_editors).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_latin_practitioners).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, modern_latin_students).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, morphological_continuity_thesis).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, textual_recovery_necessity_for_syntax).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the hybrid standard through editorial practice, critical editions, and university curricula. Their professional authority rests on adjudicating which medieval forms are 'legitimate continuations' versus 'corruptions requiring emendation.' They collect professional prestige, editorial control, and curricular dominance.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, classical_philologists, agenda_setter,
    institutional, generational, arbitrage, global).

% Historical actors whose project of 'purifying' Latin by returning to classical models is retrospectively validated by the hybrid reading. The hybrid standard treats their emendations as scholarly recovery rather than ideological imposition, legitimizing their textual interventions.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, renaissance_humanists, beneficiary,
    organized, biographical, mobile, continental).

% Produce the critical editions that materialize the hybrid standard. Every edition requires deciding, word by word, whether a medieval manuscript reading reflects legitimate morphological continuity or a syntactic/lexical corruption requiring emendation. Their editorial judgment IS the constraint's enforcement mechanism.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, textual_editors, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, textual_editors, beneficiary).

% Medieval authors, scribes, and administrators whose Latin forms are retrospectively judged: their morphology is accepted as continuous with classical norms, but their syntax and vocabulary are systematically marked as 'degenerate' or 'corrupt' requiring correction. They cannot defend their usage; the constraint operates on their textual remains.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_latin_practitioners, payer,
    moderate, generational, identity_locked, continental).

% Must learn a reconstructed Latin that never existed as a living system: classical morphology fused with humanist-emended syntax and lexicon. Their effort to acquire 'correct' Latin is extracted by a standard that presents itself as recovered rather than constructed. Exit means abandoning the field or accepting 'incorrect' status.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, modern_latin_students, payer,
    powerless, biographical, constrained, global).

% Analyze the constraint from outside its normative frame. They document the layered reconstruction process, trace which emendations reflect genuine textual recovery versus ideological projection, and observe how the hybrid standard shapes editorial practice. Their analyses sometimes reinforce, sometimes challenge the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, historical_linguists, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, teachable standard of 'correct Latin' that bridges classical antiquity and modern scholarship, enabling shared textual criticism, critical editions, and pedagogical continuity without requiring scholars to master every historical layer independently.
% TRANSFER_FUNCTION: Moves editorial authority and definitional power from medieval textual witnesses (whose forms are selectively accepted or rejected) to modern philologists and editors, who decide which syntactic and lexical features count as 'legitimate continuity' versus 'corruption requiring recovery.'
% ABSENT_VOICES: Medieval authors and scribes whose usage is the object of judgment cannot speak for their own linguistic intentions. Neo-Latin writers of the early modern period who developed functional Latin for science and diplomacy are marginalized by a standard that treats their innovations as corruptions rather than adaptations.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished, critical editions would lose their principled basis for emendation, Latin pedagogy would fragment into competing norms (pure classical, medieval documentary, or descriptive), and the editorial profession would lose its central adjudicative function. The field would reorganize around either a stricter classical purism or a descriptive pluralism.
% FOUNDING_PROBLEM: Renaissance humanists confronted a Latin textual tradition where manuscript transmission had introduced syntactic and lexical changes they judged as corruptions. They needed a principled method to distinguish genuine classical morphology from medieval accretions, and to recover an authoritative text for cultural and educational use.
% FOUNDING_PROBLEM_CORROBORATION: Humanist correspondence and prefaces (Erasmus, Valla, Poliziano) attest the founding problem from within the benefiting tradition. Modern textual critics (Reynolds & Wilson, Timpanaro) corroborate the textual corruption problem from outside. Medievalists (Mantello & Rigg, Minnis) contest whether the problem was real or constructed, arguing medieval Latin was a functional system, not a corrupted one.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.48) reflects that the constraint extracts compliance from students and marginalizes medieval practitioners, but also provides genuine coordination value (shared editorial standard, teachable grammar). Suppression (0.42) is moderate: alternative Latinities (medieval documentary, neo-Latin, ecclesiastical) persist but are delegitimized as 'incorrect.' Theater ratio (0.38) captures performative adherence to 'purity' — editions ritualize emendation as 'recovery' even where manuscript evidence is ambiguous. Accessibility collapse (0.55) shows alternatives exist but are framed as errors. Resistance (0.52) reflects ongoing scholarly debate between medievalists, classicists, and linguists.
 *
 * PERSPECTIVAL GAP:
 *   From the philologist/editor seat, the constraint is coordination: a shared standard enabling textual criticism. From the medieval practitioner seat (retrospective), it is extraction: their functional system is retrospectively pathologized. From the student seat, it is extraction with coordination veneer: they pay the learning cost for a system that never existed. The engine computes this divergence from power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and textual editors are agenda_setters with institutional/organized power and arbitrage/constrained exit — they shape the standard and can move between subfields. Renaissance humanists are historical beneficiaries whose project is validated. Medieval practitioners are identity_locked payers: their textual remains are the constraint's object, and their 'voice' is fused to the medieval Latin identity the constraint judges. Modern students are constrained payers with limited exit (field abandonment). Historical linguists are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (recovering classical texts from corrupted manuscripts) was real but partial. The hybrid reading extends the mandate beyond its founding scope: having recovered classical syntax/lexicon for major authors, the standard was generalized to ALL Latin, including periods and genres where classical norms never applied. The constraint now extracts compliance for a universal standard that solves a problem that was always selective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_hybrid_reading,
    'Does the hybrid reading represent a genuine structural middle ground between continuity and discontinuity, or does it inherit the extraction dynamics of both sibling readings while claiming neutrality?',
    'Compare the hybrid reading''s beneficiary/victim structure and enforcement requirements against both siblings. If the hybrid extracts from medieval practitioners (like discontinuity) while claiming natural continuity (like continuity), it may be a false summit masking dual extraction.',
    'If the hybrid is structurally a false compromise, its claimed_type (tangled_rope) may mask a snare-like extraction profile. If genuine, it represents a rare case of partial coordination with acknowledged extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_hybrid_reading, conceptual, 'Whether the hybrid reading''s structural position is coherent or a cover story.').

omega_variable(
    morphology_syntax_boundary,
    'Is the morphology/syntax distinction structurally stable in Latin, or does the hybrid reading impose a theoretical boundary that Latin''s actual history does not support?',
    'Historical linguistic analysis: examine whether morphological change in Latin can be cleanly separated from syntactic change, or whether the boundary reflects 19th-century philological categories projected onto the data.',
    'If the boundary is artificial, the hybrid reading''s coordination function (preserving morphology) and extraction function (emending syntax) are co-constructed, not discovered. The constraint''s ε would be higher than measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(morphology_syntax_boundary, empirical, 'Whether the morphology/syntax split reflects linguistic reality or philological theory.').

omega_variable(
    textual_recovery_circularity,
    'Does ''textual recovery'' of classical syntax/lexicon presuppose the very standard it claims to discover?',
    'Trace editorial practice: when editors emend a medieval manuscript toward ''classical'' syntax, do they use classical parallels as independent evidence, or do they impose a pre-existing norm? Compare editions of texts with no classical parallels (technical, legal, administrative Latin).',
    'If recovery is circular, the constraint''s coordination function is theatrical — the ''recovered'' standard is the constraint''s own projection. This would increase effective extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_recovery_circularity, empirical, 'Whether the recovery operation is evidentially grounded or normatively circular.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 1400, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(correct_latin_hybrid_tr_t1400, correct_latin_kernel__hybrid_reading, theater_ratio, 1400, 0.25).
narrative_ontology:measurement(correct_latin_hybrid_tr_t1550, correct_latin_kernel__hybrid_reading, theater_ratio, 1550, 0.32).
narrative_ontology:measurement(correct_latin_hybrid_tr_t1700, correct_latin_kernel__hybrid_reading, theater_ratio, 1700, 0.38).
narrative_ontology:measurement(correct_latin_hybrid_tr_t1850, correct_latin_kernel__hybrid_reading, theater_ratio, 1850, 0.42).
narrative_ontology:measurement(correct_latin_hybrid_tr_t1950, correct_latin_kernel__hybrid_reading, theater_ratio, 1950, 0.38).
narrative_ontology:measurement(correct_latin_hybrid_tr_t2025, correct_latin_kernel__hybrid_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(correct_latin_hybrid_be_t1400, correct_latin_kernel__hybrid_reading, base_extractiveness, 1400, 0.35).
narrative_ontology:measurement(correct_latin_hybrid_be_t1550, correct_latin_kernel__hybrid_reading, base_extractiveness, 1550, 0.42).
narrative_ontology:measurement(correct_latin_hybrid_be_t1700, correct_latin_kernel__hybrid_reading, base_extractiveness, 1700, 0.48).
narrative_ontology:measurement(correct_latin_hybrid_be_t1850, correct_latin_kernel__hybrid_reading, base_extractiveness, 1850, 0.52).
narrative_ontology:measurement(correct_latin_hybrid_be_t1950, correct_latin_kernel__hybrid_reading, base_extractiveness, 1950, 0.49).
narrative_ontology:measurement(correct_latin_hybrid_be_t2025, correct_latin_kernel__hybrid_reading, base_extractiveness, 2025, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(correct_latin_hybrid_su_t1400, correct_latin_kernel__hybrid_reading, suppression_requirement, 1400, 0.3).
narrative_ontology:measurement(correct_latin_hybrid_su_t1550, correct_latin_kernel__hybrid_reading, suppression_requirement, 1550, 0.38).
narrative_ontology:measurement(correct_latin_hybrid_su_t1700, correct_latin_kernel__hybrid_reading, suppression_requirement, 1700, 0.45).
narrative_ontology:measurement(correct_latin_hybrid_su_t1850, correct_latin_kernel__hybrid_reading, suppression_requirement, 1850, 0.48).
narrative_ontology:measurement(correct_latin_hybrid_su_t1950, correct_latin_kernel__hybrid_reading, suppression_requirement, 1950, 0.43).
narrative_ontology:measurement(correct_latin_hybrid_su_t2025, correct_latin_kernel__hybrid_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__hybrid_reading, 0.03).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, latin_pedagogy_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, critical_edition_practice).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the correct_latin_kernel. The continuity_reading treats the kernel as a natural law (mountain); the discontinuity_reading treats it as a symbolic reoccupation (snare/tangled_rope); this hybrid_reading claims a tangled_rope position with genuine coordination (morphology) and acknowledged extraction (syntax/lexicon recovery). The three readings form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__hybrid_reading, moderate, 0.75).
constraint_indexing:directionality_override(correct_latin_kernel__hybrid_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
