% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Classical Latin Restoration Authority (Discontinuity Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the discontinuity reading of the contested
 *   'correct Latin' kernel: the claim that Classical Latin and Medieval Latin
 *   constitute two structurally distinct linguistic systems, such that
 *   recovering 'correct' Latin required a symbolic reoccupation project —
 *   reconstructing lost Classical structure from surviving texts rather than
 *   correcting a continuously evolving language from within. Under this
 *   reading, centuries of Medieval Latin usage are recategorized as
 *   corruption of a lost original, and the humanist reconstruction program
 *   becomes the sole legitimate authority on correctness. This is a distinct
 *   constraint from the continuity reading (which treats Medieval Latin as
 *   Classical Latin's natural internal evolution, with reconstruction as
 *   correction rather than recovery) and from the hybrid reading (which
 *   locates the discontinuity only in syntax and lexicon while treating
 *   morphology as continuous). Each reading has a different
 *   beneficiary/victim structure and a different ε — they are not the same
 *   constraint viewed three ways.
 *
 * KEY AGENTS:
 *   - renaissance_humanist_philologists: agenda-setters who declare and enforce the discontinuity claim
 *   - classical_grammar_pedagogy_institutions: beneficiaries via credentialing monopoly
 *   - medieval_latin_textual_tradition: payer, relabeled as corrupt without recourse
 *   - vernacular_derived_latinate_scribes: payer, devalued professional competence
 *   - modern_historical_linguists: analytical observers assessing the discontinuity claim on evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.52).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Classical Latin Restoration Authority (Discontinuity Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '054951ce-c06f-4bf0-b495-fedbcdd57e5a').
narrative_ontology:cs_kernel_codification('054951ce-c06f-4bf0-b495-fedbcdd57e5a', fixed_text).
narrative_ontology:cs_authority_grounding('054951ce-c06f-4bf0-b495-fedbcdd57e5a', lineage).
narrative_ontology:cs_interpretation_layer_present('054951ce-c06f-4bf0-b495-fedbcdd57e5a').
narrative_ontology:cs_reading_relation('054951ce-c06f-4bf0-b495-fedbcdd57e5a', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('054951ce-c06f-4bf0-b495-fedbcdd57e5a', correct_latin_kernel__hybrid_reading, influences).
narrative_ontology:cs_axiom('054951ce-c06f-4bf0-b495-fedbcdd57e5a', foundational, classical_medieval_structural_discontinuity).
narrative_ontology:cs_axiom_status(classical_medieval_structural_discontinuity, holdable).
narrative_ontology:cs_axiom_grounding('054951ce-c06f-4bf0-b495-fedbcdd57e5a', classical_medieval_structural_discontinuity, empirically_contingent).
narrative_ontology:cs_axiom('054951ce-c06f-4bf0-b495-fedbcdd57e5a', foundational, reconstruction_is_symbolic_recovery_not_correction).
narrative_ontology:cs_axiom_status(reconstruction_is_symbolic_recovery_not_correction, holdable).
narrative_ontology:cs_axiom_grounding('054951ce-c06f-4bf0-b495-fedbcdd57e5a', reconstruction_is_symbolic_recovery_not_correction, conventional).
narrative_ontology:cs_reference_frame('054951ce-c06f-4bf0-b495-fedbcdd57e5a', classical_ciceronian_norm).
narrative_ontology:cs_drift_state('054951ce-c06f-4bf0-b495-fedbcdd57e5a', high_renaissance_consolidation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('054951ce-c06f-4bf0-b495-fedbcdd57e5a', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, renaissance_humanist_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_grammar_pedagogy_institutions).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_latin_textual_tradition).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, vernacular_derived_latinate_scribes).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, classical_purity_doctrine).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, textual_symbol_as_sole_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declare Classical Latin a lost structural system that must be reconstructed symbol-by-symbol from surviving manuscripts, treating everything produced in the intervening centuries as corruption to be stripped away. Their scholarly authority, teaching posts, and patronage depend on the discontinuity being real and severe enough to require their specialized reconstructive expertise.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, renaissance_humanist_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, renaissance_humanist_philologists, beneficiary).

% Universities and cathedral schools that adopt the reconstructed Classical norm as the only teachable, examinable Latin. They gain a stable curriculum and a credentialing monopoly built on mastery of the reconstructed forms; they benefit whether or not the discontinuity claim is historically accurate.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_grammar_pedagogy_institutions, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Nine centuries of legal, liturgical, scientific, and administrative Latin writing are relabeled corrupt or barbarous under this reading. The texts cannot object or renegotiate their status; they are simply excluded from the reconstructed canon and stop being taught, copied in the old registers, or cited as linguistic evidence.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_latin_textual_tradition, payer,
    powerless, civilizational, trapped, continental).

% Working notaries, clerks, and clerics trained in Medieval Latin usage suddenly find their competence devalued once humanist institutions certify only the reconstructed Classical norm as correct. Their exit is retraining under humanist tutors or losing standing in legal and administrative writing; the norm-shift is enforced through examination and patronage, not persuasion alone.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, vernacular_derived_latinate_scribes, payer,
    powerless, biographical, constrained, regional).

% Scribal workshops whose accumulated conventions and orthographic habits are the very material being declared corrupted have no seat in the philological debate that reclassifies their output. Their practical knowledge of how the written language actually functioned across centuries is not solicited by the reconstruction project.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, manuscript_copyists_and_scribal_workshops, excluded,
    powerless, biographical, trapped, regional).

% Evaluate competing accounts of Latin's diachronic development using comparative and corpus evidence. They can assess whether the discontinuity claim, the continuity claim, or a layered account best fits the documentary record, without institutional stake in any humanist credentialing system.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, modern_historical_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__discontinuity_reading, renaissance_humanist_philologists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single stable, teachable norm of correct Latin for humanist scholarship, diplomacy, and print culture, replacing the wide regional variation of Medieval Latin usage with one reconstructed standard.
% TRANSFER_FUNCTION: Moves linguistic authority and credentialing value from institutions and scribes trained in the Medieval Latin tradition to humanist philologists and the schools that adopt their reconstructed Classical norm; moves prestige away from centuries of Medieval Latin textual production toward manuscript philology as a discipline.
% ABSENT_VOICES: The scribal workshops and administrative clerks who actually produced and used Medieval Latin for centuries are not consulted on whether their language was 'corrupt' — the reclassification is authored entirely by the humanist philological establishment and enforced through pedagogy and patronage rather than negotiated with the affected practitioners.
% DISAPPEARANCE_RATIONALE: If the discontinuity reading and its enforced reconstruction program vanished, Medieval Latin usage would likely have persisted as the legitimate working register of Latin, humanist credentialing monopolies over 'correct' Latin would not have formed, and centuries of administrative, legal, and liturgical Latin would not have been demoted to corruption requiring correction.
% FOUNDING_PROBLEM: Humanist scholars perceived a genuine structural gap between texts they could reconstruct as ancient (via classical manuscripts, inscriptions, and grammarians) and the Latin actually spoken and written around them, and sought a principled way to recover what they believed was a superior, lost linguistic system.
% FOUNDING_PROBLEM_CORROBORATION: Humanist philologists and the institutions they founded attest the discontinuity is real and their reconstruction genuinely recovers lost structure. Modern historical linguists working from corpus evidence outside the humanist tradition are divided: some corroborate substantial structural discontinuity in syntax and lexicon, others argue the perceived discontinuity was itself partly a rhetorical and pedagogical construction serving humanist institutional interests rather than a purely linguistic fact.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.58 at interval end) reflects the credentialing and prestige transfer from Medieval Latin practitioners to humanist reconstruction specialists — a transfer that depends on the discontinuity claim being institutionally accepted, not merely on its truth. Suppression (0.52) is moderate: enforcement runs through pedagogy, examination, and patronage rather than direct coercion, but it is active and rising as humanist institutions consolidate control over what counts as correct Latin. Theater ratio (0.42) is substantial because much of the reconstruction apparatus performs philological rigor while serving credentialing interests that would persist even if the underlying linguistic claim were revised.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists and the pedagogical institutions built on their authority are structural beneficiaries — d near the beneficiary end — because their expertise and curricula depend on the discontinuity being real and requiring specialists. Medieval Latin's textual tradition and the scribes trained in its conventions are targets — d near the full-target end — because they bear the cost of reclassification with no institutional recourse and trapped or constrained exit. The scribal workshops are excluded rather than coordinated: their absence from the debate is what allows the reclassification to proceed uncontested by the very evidence base it reinterprets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a perceived need to recover a superior ancient linguistic system) may or may not still be live — modern historical linguistics can adjudicate structural continuity/discontinuity independently of humanist credentialing interests, which is why founding_problem_status is authored as contested rather than dead or live. Classifying this as tangled_rope rather than snare recognizes a genuine coordination function (a stable teachable norm across humanist Europe) that coexists with asymmetric extraction (devaluing an entire prior tradition to establish credentialing authority) — collapsing it to pure extraction would erase the real standardization benefit; collapsing it to pure rope would erase the documented victim class and enforcement machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discontinuity_claim_naturalness_ambiguity,
    'Is the discontinuity between Classical and Medieval Latin a genuine structural-linguistic fact independently verifiable from the documentary record, or is it substantially a rhetorical construction serving humanist institutional interests in credentialing and prestige?',
    'Comparative corpus-linguistic analysis of syntax, morphology, and lexicon across the transition period, conducted by scholars with no institutional stake in humanist credentialing outcomes, cross-checked against the hybrid_reading''s claim that only syntax/lexicon (not morphology) show discontinuity.',
    'If the discontinuity is substantially real and severe, this reading''s claimed_type moves toward rope (genuine coordination need for reconstruction); if substantially constructed for institutional benefit, it moves toward snare (extraction dressed as linguistic necessity). The current tangled_rope claim assumes a real coordination function overlaid with real extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_claim_naturalness_ambiguity, empirical, 'Whether the discontinuity claim itself is naturally occurring or institutionally constructed.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the three kernel readings (continuity, discontinuity, hybrid) disagree — is it the degree of structural change, the correct unit of linguistic analysis (morphology vs. syntax vs. lexicon), or the normative question of whether change constitutes corruption versus evolution?',
    'A structural audit distinguishing empirical claims (how much changed, in which subsystems) from normative claims (whether change counts as corruption) across all three readings, comparing this story''s stakeholder/beneficiary structure against the continuity_reading and hybrid_reading files.',
    'If the disagreement is purely normative (all three readings agree on the empirical extent of change but differ on whether to call it corruption), the readings coexist as value-laden framings of the same evidence. If the disagreement is substantially empirical (readings disagree about what actually happened linguistically), one reading may be evidentially preferable and the others closer to false-consciousness constructs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Locating whether the kernel readings diverge empirically or normatively.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__discontinuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corr_tr_t40, correct_latin_kernel__discontinuity_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(corr_tr_t80, correct_latin_kernel__discontinuity_reading, theater_ratio, 80, 0.34).
narrative_ontology:measurement(corr_tr_t120, correct_latin_kernel__discontinuity_reading, theater_ratio, 120, 0.38).
narrative_ontology:measurement(corr_tr_t160, correct_latin_kernel__discontinuity_reading, theater_ratio, 160, 0.4).
narrative_ontology:measurement(corr_tr_t200, correct_latin_kernel__discontinuity_reading, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__discontinuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(corr_be_t40, correct_latin_kernel__discontinuity_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(corr_be_t80, correct_latin_kernel__discontinuity_reading, base_extractiveness, 80, 0.51).
narrative_ontology:measurement(corr_be_t120, correct_latin_kernel__discontinuity_reading, base_extractiveness, 120, 0.55).
narrative_ontology:measurement(corr_be_t160, correct_latin_kernel__discontinuity_reading, base_extractiveness, 160, 0.57).
narrative_ontology:measurement(corr_be_t200, correct_latin_kernel__discontinuity_reading, base_extractiveness, 200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__discontinuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(corr_su_t40, correct_latin_kernel__discontinuity_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(corr_su_t80, correct_latin_kernel__discontinuity_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement(corr_su_t120, correct_latin_kernel__discontinuity_reading, suppression_requirement, 120, 0.49).
narrative_ontology:measurement(corr_su_t160, correct_latin_kernel__discontinuity_reading, suppression_requirement, 160, 0.51).
narrative_ontology:measurement(corr_su_t200, correct_latin_kernel__discontinuity_reading, suppression_requirement, 200, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__discontinuity_reading, 0.1).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the correct_latin_kernel. continuity_reading treats the same historical material as internal evolution requiring correction, not recovery — it has a smaller victim set (no wholesale relabeling of Medieval Latin as corrupt) and correspondingly lower extractiveness. hybrid_reading splits the difference structurally (morphology continuous, syntax/lexicon discontinuous), producing a partial victim set and intermediate extractiveness. All three share the underlying kernel (what counts as 'correct' Latin and how reconstruction should proceed) but instantiate structurally distinct constraints with different beneficiary/victim maps; they must not be averaged into one ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
