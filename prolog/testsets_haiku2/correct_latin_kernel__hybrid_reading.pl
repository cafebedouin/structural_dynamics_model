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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Medieval Latin Textual Correction (Hybrid Kernel Reading)
 *   domain: intellectual/linguistic
 *
 * SUMMARY:
 *   This constraint embodies one reading of a contested kernel in historical
 *   linguistics: the proper relationship between Medieval and Classical
 *   Latin. The hybrid reading asserts that Medieval Latin preserves Classical
 *   morphology (noun and verb inflections remain largely intact) but requires
 *   reconstruction in syntax and lexicon (word order, vocabulary, idiomatic
 *   expressions deviate and must be corrected to classical norms). This
 *   reading serves as the framework for critical edition production and
 *   textual emendation in philological scholarship. The claim is independent
 *   of the metrics: the reading is CLAIMED as tangled_rope (genuine
 *   coordination problem to solve + asymmetric extraction from those who
 *   study medieval texts) while the authored metrics describe moderately
 *   extractive, actively enforced operation. The sibling readings
 *   (continuity: medieval forms are normal linguistic evolution;
 *   discontinuity: medieval Latin is a distinct system) offer competing
 *   framings of the same medieval textual archive.
 *
 * KEY AGENTS:
 *   - philological_reconstructionists: institutional authority setting the framework and controlling editions
 *   - classical_purists: powerful beneficiaries whose standing depends on the framework
 *   - medieval_latinists: moderate-power payers whose texts are subordinated to classical norms
 *   - textual_authenticity_advocates: moderate-power identity-locked payers invested in what scribes actually wrote
 *   - continuity_reading_advocates: excluded moderate-power scholars whose alternative reading is marginalized
 *   - discontinuity_reading_advocates: excluded moderate-power scholars whose radical reading is treated as outside-philology
 *   - editorial_consensus_apparatus: institutional agenda-setter operationalizing the framework
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
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Medieval Latin Textual Correction (Hybrid Kernel Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "intellectual/linguistic").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, '9c48315b-0814-4b55-bb47-f1dc24367118').
narrative_ontology:cs_kernel_codification('9c48315b-0814-4b55-bb47-f1dc24367118', distributed).
narrative_ontology:cs_authority_grounding('9c48315b-0814-4b55-bb47-f1dc24367118', extraction).
narrative_ontology:cs_interpretation_layer_present('9c48315b-0814-4b55-bb47-f1dc24367118').
narrative_ontology:cs_reading_relation('9c48315b-0814-4b55-bb47-f1dc24367118', correct_latin_kernel__continuity_reading, influences).
narrative_ontology:cs_reading_relation('9c48315b-0814-4b55-bb47-f1dc24367118', correct_latin_kernel__discontinuity_reading, influences).
narrative_ontology:cs_axiom('9c48315b-0814-4b55-bb47-f1dc24367118', foundational, morphology_preserved_syntax_reconstructed).
narrative_ontology:cs_axiom_status(morphology_preserved_syntax_reconstructed, holdable).
narrative_ontology:cs_axiom_grounding('9c48315b-0814-4b55-bb47-f1dc24367118', morphology_preserved_syntax_reconstructed, empirically_contingent).
narrative_ontology:cs_axiom('9c48315b-0814-4b55-bb47-f1dc24367118', secondary, classical_norms_as_correction_standard).
narrative_ontology:cs_axiom_status(classical_norms_as_correction_standard, holdable).
narrative_ontology:cs_axiom_grounding('9c48315b-0814-4b55-bb47-f1dc24367118', classical_norms_as_correction_standard, deontological).
narrative_ontology:cs_reference_frame('9c48315b-0814-4b55-bb47-f1dc24367118', classical_philological_authority).
narrative_ontology:cs_drift_state('9c48315b-0814-4b55-bb47-f1dc24367118', post_comparative_historical_analysis_2000_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9c48315b-0814-4b55-bb47-f1dc24367118', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, philological_reconstructionists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, classical_purists).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_latinists).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, textual_authenticity_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, editorial_consensus_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic scholars in classical philology who author the reconstruction protocols and edit critical editions. They establish which medieval forms are deemed corruptions versus legitimate morphological continuities. They control peer review, journal acceptance, and canonical text production. They set the evidentiary bar for what counts as authentic versus degraded.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, philological_reconstructionists, agenda_setter,
    institutional, generational, arbitrage, global).

% Classical scholars and tradition-custodians who benefit from a reading that privileges Ciceronian norms and treats medieval deviation as corruption. Their institutional standing depends on the assumption that Classical Latin represents the true form; the reconstruction framework vindicates that standing by making medieval forms intelligible as errors rather than innovations.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, classical_purists, beneficiary,
    powerful, generational, mobile, global).

% Scholars who study medieval Latin as a system in its own right, including monks, administrators, and the linguistic reality of medieval literate communities. They are structurally subordinated: their texts are edited according to classical standards they did not use; their linguistic forms are labeled corruptions rather than legitimate developments; their research requires navigating frameworks that treat their primary sources as degraded versions of an anterior system.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_latinists, payer,
    moderate, biographical, constrained, global).

% Scholars committed to understanding what medieval scribes actually wrote and why, viewing emendation as interpretation rather than recovery. They bear the cost of working against the grain of institutional editing practices; their readings are marked as deviant when they accept medieval forms as intentional. Professional reputation depends on submitting to the reconstructionist frame to be heard.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, textual_authenticity_advocates, payer,
    moderate, biographical, identity_locked, global).

% Scholars who argue Medieval Latin evolved naturally from Classical Latin according to normal linguistic laws. They would dispute the hybrid reading's asymmetry claim (that morphology is continuous but syntax/lexicon require reconstruction); they lack institutional voice in the reconstruction apparatus and their view is largely absent from critical editions.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, continuity_reading_advocates, excluded,
    moderate, biographical, constrained, global).

% Scholars who argue Medieval Latin is a distinct system requiring wholesale reconstruction rather than correction. They would dispute the hybrid reading's claim that morphology is preserved; their work is marginalized in institutional philology as radical or dilettante.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, discontinuity_reading_advocates, excluded,
    moderate, biographical, constrained, global).

% The collective institution of critical edition production, textual criticism standards, and peer-review gate-keeping that operationalizes the reconstruction protocols. It benefits from a stable framework (the hybrid reading's asymmetry) that permits selective emendation without requiring systematic re-theorization; it enforces the distinction between legitimate morphology and corrupted syntax/lexicon.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, editorial_consensus_apparatus, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, editorial_consensus_apparatus, beneficiary).

% The actual medieval manuscripts and their linguistic properties — not an actor but the primary evidence over which the readings contend. The different readings interpret the same archival facts differently.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, archival_scribal_record, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(correct_latin_kernel__hybrid_reading, archival_scribal_record).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__hybrid_reading, philological_reconstructionists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of comparing medieval textual fragments to classical norms by partitioning the medieval system: morphology is treated as internally coherent continuation, while syntax and lexicon are treated as corrupted and subject to correction. This permits scholarly communication about medieval texts without requiring a full relational model of Medieval Latin as a distinct system.
% TRANSFER_FUNCTION: Transfers interpretive authority from what medieval scribes actually wrote to what classical philologists determine they should have written. Medieval text-bearers lose the right to their own linguistic system; their forms are intelligible only as errors to be corrected, not as data describing their actual linguistic practice.
% ABSENT_VOICES: Continuity-reading advocates (Medieval Latin as natural evolution) and discontinuity-reading advocates (Medieval Latin as a distinct system) are structurally excluded from the reconstruction apparatus. Their disagreement with the hybrid framework's core claim — that some medieval forms are legitimately continuous while others require correction — is treated as outside-philology rather than as internal scholarly debate.
% DISAPPEARANCE_RATIONALE: If the hybrid reconstruction framework disappeared, critical editions would shift: either toward wholesale discontinuity (Medieval Latin edited as an independent system without correction-to-classical-norms) or toward pure continuity (medieval forms accepted as normal linguistic evolution). The scholarly interpretation of every medieval text would reorganize around a different set of evidentiary standards.
% FOUNDING_PROBLEM: Medieval Latin texts are difficult to parse using classical syntax and lexicon; classical scholars needed a method to recover readable classical-like versions from medieval corruptions in order to access the intellectual content beneath the textual surface.
% FOUNDING_PROBLEM_CORROBORATION: Classical philologists attest the founding problem is live: Medieval Latin does deviate enough from classical norms to pose genuine reading difficulty. Medieval Latinists and discontinuity advocates attest the founding problem is misconceived: the deviation is not corruption but legitimate medieval linguistic practice; the reading difficulty arises from treating medieval texts as failures rather than as data. Linguistic comparative-historical analysis shows medieval forms are consistent with regular sound-change patterns, supporting the continuity and discontinuity readings against the hybrid frame.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the hybrid framework transfers interpretive authority from medieval texts to classical reconstructionists: medieval forms are readable only as corruptions, not as legitimate medieval-system data. Suppression is substantial (0.58) because the framework actively enforces the morphology/syntax split and marginalizes competing readings. Theater is moderate (0.41) and rising: the scholarly apparatus performs a division of medieval linguistic labor (morphology is continuous, syntax/lexicon are corrupted) that becomes increasingly theatrical as alternative linguistic analyses suggest the entire medieval system might be coherent on its own terms. The measurement series tracks the accumulation of extractiveness and suppression as the institutional framework consolidates (t0 to t25) and then stabilizes as institutional inertia takes over. Accessibility_collapse is low-to-moderate (0.48) because alternative readings (continuity, discontinuity) remain live in the discipline even if institutionally marginalized. Resistance is moderate (0.52) because medieval Latinists and authenticity advocates mount real scholarly counter-arguments; they are not eliminated but are subordinated in the editorial apparatus.
 *
 * PERSPECTIVAL GAP:
 *   The classical purists and reconstructionist philologists compute as beneficiaries seeing genuine coordination (solving the medieval-to-classical reading problem); medieval Latinists and authenticity advocates compute as victims seeing arbitrary extraction (their texts edited according to alien norms). The editorial apparatus computes as both agenda-setter and beneficiary (administers the framework and benefits from its stability). The discontinuity and continuity reading advocates compute as excluded parties whose frameworks would reorganize the entire scholarly enterprise if adopted. The engine should compute the reconstruction specialists' seat differently from the medieval-focused seats due to their divergent power and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Philological reconstructionists hold institutional power, control the editorial apparatus, and have arbitrage-level exit (they can apply the framework across languages and historical periods, or deploy it selectively). They are beneficiaries: d approaches 0.0. Classical purists are powerful institutional actors whose authority and prestige depend on the framework vindicating classical norms; they are secondary beneficiaries: d moderate. Medieval Latinists are moderate-power scholars with constrained exit (they must work within the editorial framework to have their work published and cited); they are targets: d near 0.8. Textual authenticity advocates are moderate-power scholars with identity-locked exit (professional identity fused to textual authenticity as a scholarly commitment); they bear extraction: d approaches 1.0. Excluded advocates (continuity and discontinuity) are moderate-power scholars with constrained exit (they can write outside the institutional apparatus but lose audience and legitimacy); they are structurally oppressed by the framework's dominance: d moderate-high.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (medieval texts are hard to read using classical syntax) remains contested. Classical philologists say it is live; medieval specialists say it is misconceived (medieval text difficulty arises from applying classical norms, not from medieval corruption). The hybrid reading's core claim — that morphology is preserved while syntax/lexicon must be reconstructed — is what both sibling readings dispute. Continuity advocates say the entire system evolved normally; discontinuity advocates say medieval syntax/lexicon are not corruptions but legitimate medieval-system phenomena. The hybrid reading's mandate (provide a framework for interpreting medieval texts through classical lenses) persists, but increasingly it persists as theatrical apparatus rather than as solution to the founding problem. The framework does solve a coordination problem: it lets classical scholars and medieval specialists work with the same texts under unified editorial standards. But the extraction cost — the subordination of medieval textual authenticity to classical reconstructionist authority — is what the mandatrophy analysis exposes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    morphology_continuity_boundary,
    'Is the medieval preservation of classical noun/verb inflectional morphology structurally necessary evidence of continuity, or could it reflect scribal copying of classical exemplars without full internalization of morphological rules?',
    'Systematic analysis of medieval scribal error patterns, comparing intentional analogical changes (evidence of internalized rules) versus mechanical copying (evidence of external constraint). Comparative study of non-Latin medieval languages'' morphological preservation under Latin influence.',
    'If morphological preservation reflects copying without internalization, the hybrid reading''s core asymmetry (morphology continuous, syntax reconstructed) collapses into full discontinuity — medieval writers did not command classical morphology as system, only as template. This would move the reading toward discontinuity_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morphology_continuity_boundary, empirical, 'Whether medieval morphological forms are internalized continuations or copied templates.').

omega_variable(
    reconstruction_vs_authenticity_frame,
    'Is the emendation of medieval syntax/lexicon a legitimate philological operation (recovery of classical norms corrupted in transmission) or an imposition of alien norms on texts that instantiate a coherent but different medieval system?',
    'Close reading of medieval syntactic structures as internally coherent patterns; comparative analysis with contemporary non-Latin medieval languages to establish whether medieval-Latin syntax is idiosyncratic to Latin or reflects general medieval European linguistic properties. Historical documentation of medieval scribal intent (colophons, annotations, usage patterns across scribal hands).',
    'If medieval syntax is internally coherent and reflects a genuine medieval system, then emendation is destructive interpretation, not recovery. The hybrid reading transforms from coordination framework (solving a real reading problem) into pure extraction (imposing classical norms). If medieval syntax is indeed scribal error, the framework remains valid coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstruction_vs_authenticity_frame, conceptual, 'Whether medieval Latin syntax is corrupted classical form or legitimate medieval-system form.').

omega_variable(
    kernel_reading_committer_contest,
    'Which reading of the correct-latin-kernel — continuity (normal evolution), discontinuity (distinct system), or hybrid (asymmetric split) — does the archival evidence support, if interpreted without prior institutional allegiance?',
    'De-institutionalized analysis: have teams aligned with each reading independently code medieval texts for morphological, syntactic, and lexical patterns, without reference to critical editions reflecting the hybrid framework. Compare results for pattern coherence and explanatory power. Establish whether the patterns fit one reading better across multiple measures.',
    'Foreclosure of the losing readings (if evidence strongly supports continuity or discontinuity) would dissolve the kernel contest and reframe the entire constraint. If evidence remains ambiguous, the contest persists and the hybrid reading''s institutional dominance becomes pure extraction (winner-take-all without evidentiary resolution).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_contest, empirical, 'Which reading the medieval archive actually supports when interpretation is decoupled from institutional allegiance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of discontinuity and continuity readings structural (institutional gatekeeping, editorial control, hiring/promotion incentives that favor the hybrid frame) or internalized (scholars believe the hybrid frame is true and self-censor alternatives, or fear scholarship will be invalid if published under alien frames)?',
    'Post-suppression trajectory: if medieval scholars working outside the hybrid framework (publishing in non-mainstream journals, writing under pseudonyms or in different disciplinary contexts) continue to experience suppression after the framework loses institutional dominance, the suppression is partially internalized. Survey of scholars who left the field or shifted frameworks: do they report that the framework''s constraints persisted after leaving the institution?',
    'If suppression is mostly structural (institutional gatekeeping), removing the framework from the editorial apparatus would rapidly open the competing readings. If internalized (scholars have incorporated the framework into their self-concept), competing readings would take longer to rehabilitate even after institutional change. Classification impact: if internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of competing readings is structural or internalized in scholars'' sense-making.').

omega_variable(
    axiom_overriding_empirical_pressure,
    'Is the foundational axiom of the hybrid reading (that medieval syntax/lexicon are corrupted classical forms) empirically overridden by recent comparative-historical linguistic analysis suggesting medieval Latin syntax matches typological patterns of other medieval European languages?',
    'Meta-analysis of recent comparative studies (Banniard, Mantello, Stotz, others) showing medieval Latin syntactic innovations as pan-European medieval features rather than scribal corruptions. Strength of evidence consensus in the discipline.',
    'Empirical override of the axiom would classify the hybrid reading as status=overridden in its own tradition. This is not foreclosure (the reading is still holdable); it means the empirical grounds the axiom stood on have been substantially undercut. The reading persists as institutional inertia rather than as resolved scholarship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_overriding_empirical_pressure, empirical, 'Whether empirical linguistic evidence has overridden the axiom that medieval syntax is corruption.').


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
narrative_ontology:measurement(corr_tr_t20, correct_latin_kernel__hybrid_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(corr_tr_t25, correct_latin_kernel__hybrid_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(corr_tr_t30, correct_latin_kernel__hybrid_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(corr_tr_t40, correct_latin_kernel__hybrid_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__hybrid_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(corr_be_t5, correct_latin_kernel__hybrid_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(corr_be_t10, correct_latin_kernel__hybrid_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(corr_be_t15, correct_latin_kernel__hybrid_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(corr_be_t20, correct_latin_kernel__hybrid_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(corr_be_t25, correct_latin_kernel__hybrid_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(corr_be_t30, correct_latin_kernel__hybrid_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(corr_be_t40, correct_latin_kernel__hybrid_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__hybrid_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(corr_su_t5, correct_latin_kernel__hybrid_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(corr_su_t10, correct_latin_kernel__hybrid_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(corr_su_t15, correct_latin_kernel__hybrid_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(corr_su_t20, correct_latin_kernel__hybrid_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(corr_su_t25, correct_latin_kernel__hybrid_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(corr_su_t30, correct_latin_kernel__hybrid_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(corr_su_t40, correct_latin_kernel__hybrid_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__hybrid_reading, 0.06).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).

% DUAL FORMULATION NOTE:
% The correct_latin_kernel generates three constraint stories, one per reading of the disputed kernel: continuity (medieval Latin is normal evolution from classical), discontinuity (distinct medieval system), and hybrid (morphology continuous, syntax/lexicon reconstructed). All three share the same medieval-textual archive as referent but interpret it through different frameworks. Each reading has distinct ε (empirical support), beneficiary structures (classical purists vs. medieval specialists), and institutional support (hybrid dominates; sibling readings are marginalized). Links: continuity_reading influences this hybrid_reading (if medieval evolution is accepted, the asymmetric split collapses); discontinuity_reading forecloses the hybrid_reading's core morphology-continuity claim if empirically supported.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__hybrid_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
