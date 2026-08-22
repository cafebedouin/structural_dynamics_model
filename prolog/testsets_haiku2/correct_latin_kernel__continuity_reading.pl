% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: correct_latin_kernel__continuity_reading
 *   human_readable: Medieval Latin as Continuous Evolution — Kernel Reading
 *   domain: historical_linguistics/textual_authority
 *
 * SUMMARY:
 *   The continuity reading treats Medieval Latin as Classical Latin after
 *   organic linguistic evolution — innovations in grammar, phonology, and
 *   vocabulary are legitimate developments within a single language system,
 *   not corruptions or errors. Reconstruction is understood as internal
 *   correction: recovering the patterns and logic within medieval texts
 *   themselves, not emending them back to classical norms. This reading
 *   benefits ecclesiastical and medieval-scribal authority (their practice is
 *   legitimized as natural continuation) and imposes costs on
 *   classical-reconstruction philologists (who must now account for
 *   'legitimate' medieval divergence) and humanist editors (whose emendations
 *   are reframed as prescriptive purism rather than restoration of
 *   correctness). The constraint operates via institutional control of
 *   textual authority: medieval manuscripts are preserved and studied as
 *   valid instances of Latin, while the classical corpus is repositioned from
 *   absolute standard to one historical state of the language.
 *
 * KEY AGENTS:
 *   - medieval_scribal_tradition: produces and transmits texts; legitimized by continuity reading as maintaining correct Latin practice
 *   - classical_reconstruction_philologists: invested in recovering Classical norms; bear the cost of having medieval divergence treated as natural development rather than corruption
 *   - humanist_editors: Renaissance and early-modern scholars who emended medieval witnesses toward classical standards; their authority undermined by framing emendation as prescriptive reform
 *   - ecclesiastical_authority: Church institutions that produced and controlled medieval Latin; gains institutional legitimacy from continuity reading
 *   - linguistic_reconstruction_methodology: external analytical seat observing whether medieval variants are corruption or systematic evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, 0.62).
domain_priors:suppression_score(correct_latin_kernel__continuity_reading, 0.71).
domain_priors:theater_ratio(correct_latin_kernel__continuity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__continuity_reading, "Medieval Latin as Continuous Evolution — Kernel Reading").
narrative_ontology:topic_domain(correct_latin_kernel__continuity_reading, "historical_linguistics/textual_authority").

domain_priors:requires_active_enforcement(correct_latin_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__continuity_reading, 'e3819dab-bd6f-46b4-9d23-1e4155a867f9').
narrative_ontology:cs_kernel_codification('e3819dab-bd6f-46b4-9d23-1e4155a867f9', distributed).
narrative_ontology:cs_authority_grounding('e3819dab-bd6f-46b4-9d23-1e4155a867f9', lineage).
narrative_ontology:cs_interpretation_layer_present('e3819dab-bd6f-46b4-9d23-1e4155a867f9').
narrative_ontology:cs_reading_relation('e3819dab-bd6f-46b4-9d23-1e4155a867f9', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3819dab-bd6f-46b4-9d23-1e4155a867f9', correct_latin_kernel__hybrid_reading, influences).
narrative_ontology:cs_axiom('e3819dab-bd6f-46b4-9d23-1e4155a867f9', foundational, linguistic_continuity_through_natural_change).
narrative_ontology:cs_axiom_status(linguistic_continuity_through_natural_change, holdable).
narrative_ontology:cs_axiom_grounding('e3819dab-bd6f-46b4-9d23-1e4155a867f9', linguistic_continuity_through_natural_change, empirically_contingent).
narrative_ontology:cs_axiom('e3819dab-bd6f-46b4-9d23-1e4155a867f9', secondary, medieval_manuscript_as_valid_linguistic_evidence).
narrative_ontology:cs_axiom_status(medieval_manuscript_as_valid_linguistic_evidence, holdable).
narrative_ontology:cs_axiom_grounding('e3819dab-bd6f-46b4-9d23-1e4155a867f9', medieval_manuscript_as_valid_linguistic_evidence, conventional).
narrative_ontology:cs_reference_frame('e3819dab-bd6f-46b4-9d23-1e4155a867f9', classical_latin_as_standard_of_correctness).
narrative_ontology:cs_drift_state('e3819dab-bd6f-46b4-9d23-1e4155a867f9', post_historical_linguistic_professionalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e3819dab-bd6f-46b4-9d23-1e4155a867f9', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(correct_latin_kernel__continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, medieval_scribal_tradition).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, classical_reconstruction_philologists).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, humanist_editors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, manuscript_witnesses).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, ecclesiastical_authority).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, linguistic_continuity_doctrine).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, organic_language_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Medieval clerics and scribes produced texts in evolving Latin, adapting grammar and vocabulary to contemporary use while maintaining the institutional claim to be writing 'correct' Latin. The continuity reading legitimizes their practice: medieval innovations (nominative plural -ae becoming -as, subjunctive mood innovations, lexical borrowing) are not errors but natural linguistic development. They set the editorial agenda for manuscript transmission and interpretation — what counts as a 'good' medieval Latin text.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, medieval_scribal_tradition, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, medieval_scribal_tradition, beneficiary).

% Scholars invested in recovering Classical Latin norms (Cicero, Virgil) bear the cost of the continuity reading: their reconstruction work must now account for centuries of 'legitimate evolution' rather than treating medieval divergence as corruption to be stripped away. The constraint subordinates classical purity to medieval practice, requiring them to build two separate historical-grammars instead of one corrected text.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, classical_reconstruction_philologists, payer,
    institutional, generational, constrained, global).

% Renaissance and early-modern editors (Poliziano, Scaliger, and their traditions) invested in establishing classical texts by emending medieval witnesses. The continuity reading frames their work as prescriptive purism — a judgment that medieval variants are not natural evolution but editorial failures. Their authority to 'correct' texts depends on asserting medieval manuscripts are corrupted transmission, not legitimate variant practice. The constraint forces them to justify classical emendation as restoration rather than normative reform.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, humanist_editors, payer,
    powerful, biographical, mobile, global).

% Medieval manuscripts themselves are legitimized under the continuity reading: their variant forms are not errors to be corrected but evidence of living language practice. The constraint protects manuscripts from aggressive emendation and asserts their textual forms as valid instantiations of the Latin language at their historical moment.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, manuscript_witnesses, beneficiary,
    powerless, civilizational, trapped, universal).

% The Church produced and controlled medieval Latin texts. The continuity reading validates ecclesiastical Latin practice: monastic and clerical usage are not corruptions of classical purity but legitimate developments within the same language system. This protects the institutional legitimacy of ecclesiastical Latin transmission and interpretation.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, ecclesiastical_authority, beneficiary,
    institutional, generational, constrained, continental).

% The classical corpus (Cicero, Livy, Vergil, etc.) as a normative standard for Latin correctness is bracketed under the continuity reading: it becomes one historical state of the language, not the definitive standard against which all other Latin is measured. This exclusion is what the constraint enforces — classical texts retain authority as sources but lose authority as norms for correctness across all time periods.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, classical_texts_as_standard, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(correct_latin_kernel__continuity_reading, classical_texts_as_standard).

% Modern historical-linguistic frameworks assess Medieval Latin empirically: phonological changes, morphological leveling, semantic shifts can be traced through manuscript evidence. This external methodology provides testimony to whether medieval variants are 'corruption' (random variation) or 'evolution' (systematic change). The methodological seat observes the contest but is not party to it.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, linguistic_reconstruction_methodology, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__continuity_reading, ecclesiastical_authority).
narrative_ontology:fixing_cost_class(correct_latin_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single Latin language system spanning Classical and Medieval periods, permitting institutional continuity of ecclesiastical and scholarly practice without requiring re-legitimation at each linguistic change. Solves the problem of textual authority: which texts count as correct Latin and how do later communities relate to earlier textual models?
% TRANSFER_FUNCTION: Moves authority away from the classical corpus as absolute standard and toward medieval manuscripts as valid instances of linguistic practice. Transfers interpretive labor from humanist-editors (who emend toward classical norms) to medieval-scholars (who reconstruct usage patterns within medieval texts as-written). Transfers institutional legitimacy from classical reconstruction to medieval ecclesiastical practice.
% ABSENT_VOICES: Voices outside the learned/ecclesiastical tradition (merchant Latin, peripheral dialects, women's Latin use) have no seat at all; they are historically absent from the manuscript record and are not recovered by either the continuity or discontinuity reading. Voices calling for explicit medieval-Latin-as-distinct-system linguistics (the discontinuity reading) are present in the corpus but marginalized under the continuity framing.
% DISAPPEARANCE_RATIONALE: If this constraint vanished — if the continuity reading were rejected and Classical Latin reasserted as the sole standard for correctness — medieval manuscripts would be treated as corrupted transmission requiring aggressive emendation back to classical norms. Humanist editorial methods would be re-legitimated as correction rather than prescriptive reform. Ecclesiastical and scholarly practice built on medieval Latin would lose institutional grounding in continuity and would require explicit justification as 'non-standard but permissible.' The entire apparatus of textual criticism, paleography, and medieval philology would reorganize around a two-standards model (Classical correct; Medieval deviant).
% FOUNDING_PROBLEM: In the 4th–6th centuries, Latin as spoken evolved into Romance languages and as written in clerical/scholarly contexts developed innovations incomprehensible to classical readers. By the 12th century, a two-system interpretation emerged: either Medieval Latin is corrupted Classical Latin (Discontinuity), or it is Classical Latin naturally evolved (Continuity). The problem: how to legitimize a thousand years of institutional practice using texts that diverge from the classical standard, without asserting that all institutional learning is built on corruption.
% FOUNDING_PROBLEM_CORROBORATION: Philologists and linguists outside the benefiting parties (comparative Indo-European scholars, historical linguists studying language change broadly) confirm that medieval innovations show systematic phonological and morphological patterns consistent with natural language evolution, not random corruption. Medieval-manuscript scholars (paleographers, codicologists) attest that scribes did not view their innovations as errors but as proper Latin. However, classical philologists testify that the classical corpus was deliberately recovered and elevated as authoritative during the Renaissance — a deliberate choice to adopt a standard, not a neutral description of what 'correct Latin' always was.
narrative_ontology:disappearance_verdict(correct_latin_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__continuity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the continuity reading subordinates classical purity to medieval practice — this is a substantive re-ordering of textual authority with real costs to those invested in classical standards. Suppression is high (0.71) because the constraint requires active enforcement: humanist editors must be prevented from emending medieval manuscripts toward classical norms, and the classical corpus must be actively repositioned as non-normative. Theater is moderate (0.48) because the reading is grounded in genuine linguistic arguments (medieval innovations show systematic patterns consistent with natural language change), but institutional interests in ecclesiastical authority and medieval-manuscript preservation also drive its maintenance. The measurement series shows extractiveness and suppression rising steeply in the early interval (0–10), then plateauing (15–25): this reflects the solidification of the reading's authority during the late 19th–20th centuries as historical-linguistic frameworks were professionalized and institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   The medieval-scribal and ecclesiastical seats should perceive the constraint as coordination (legitimate language evolution, continuity of practice); the classical-reconstruction and humanist seats should perceive it as extraction (subordination of their standards to medieval practice, loss of editorial authority). The engine computes this divergence from the structural data. From the continuity reading's own seat, this is coordination solving a real problem (how to maintain institutional legitimacy across linguistic change). From the classical-purity seat, this is enforced subordination of one standard to another for institutional benefit to ecclesiastical actors.
 *
 * DIRECTIONALITY LOGIC:
 *   The medieval-scribal tradition (organized, generational time-horizon, constrained exit) sits at the beneficiary end: the continuity reading legitimizes their institutional practice and protects their manuscript tradition from aggressive emendation. Classical-reconstruction philologists (institutional, generational, constrained by academic career investment in classical studies) sit at the payer end: they must now account for two separate historical-grammar systems instead of one corrected text, and their authority to 'restore' classical correctness is undermined. Humanist editors occupy a complex seat: powerful institutional actors (Renaissance courts, printing houses), but with high mobile exit — they can abandon classical emendation without career penalty if the reading becomes dominant. The manuscript witnesses themselves are trapped (powerless, civilizational scope) but legitimized by the reading. Directionality for ecclesiastical authority is beneficiary (generational, institutional, constrained exit): the Church's medieval Latin practice gains legitimacy without requiring reform. The classical-texts-as-standard is excluded (not an agent, but a status-bearer: from normative standard to one historical instantiation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to legitimize a thousand years of ecclesiastical practice without asserting corruption) is LIVE, not dead — the continuity reading directly addresses it by asserting medieval innovations are natural evolution. However, the constraint shows characteristics of a tangled_rope rather than pure rope: the coordination function (maintaining institutional continuity) is real, but it is entangled with extraction (suppression of classical-standard authority) and requires active enforcement (preventing classical emendation, repositioning classical texts as non-normative). The theater ratio (0.48) indicates performative maintenance: the reading is grounded in linguistic science, but institutional interests in medieval authority and ecclesiastical legitimacy also sustain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_of_medieval_innovation,
    'Did medieval scribes and clerics deliberately innovate in Latin grammar, or did innovations arise as unintended consequences of language contact and transmission?',
    'Paleographic and codicological evidence: if scribes corrected innovations back to classical forms in revision (suggesting awareness of divergence), innovation was intentional; if innovations appear stable across copies (suggesting normalization), they were unintentional but systematized.',
    'Intentional innovation supports the continuity reading (natural language evolution); unintentional-but-systematized innovation could support the discontinuity reading (loss of classical control, new system emerging). High intentionality strengthens the case for treating medieval Latin as evolved Latin; low intentionality strengthens the case for treating it as degraded transmission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_medieval_innovation, empirical, 'Whether medieval Latin innovations were deliberate evolution or unintended drift.').

omega_variable(
    reading_institutional_capture,
    'Is the continuity reading''s dominance in modern philology driven by its explanatory power as a linguistic framework, or by institutional interests in medieval-manuscript preservation and ecclesiastical-authority legitimacy?',
    'Historical sociology of philology: examine whether the reading was adopted first by scholars with interests in medieval manuscripts/ecclesiastical history (institutional capture signal) or first by scholars with no prior commitment to medieval authority (neutral adoption signal). Compare adoption timing across academic centers.',
    'If capture, the constraint is primarily extractive (subordinating classical standards to institutional interests); if neutral, the constraint is primarily coordinative (solving a real textual-authority problem). This feeds the snare vs. tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_institutional_capture, conceptual, 'Whether the continuity reading''s institutional dominance reflects its analytical merit or institutional interest in medieval authority.').

omega_variable(
    sibling_reading_foreclosure,
    'Can the discontinuity reading (Medieval Latin as distinct system) coexist with the continuity reading within a single scholarly framework, or does one logically foreclose the other?',
    'Examine whether any contemporary scholar or school of thought maintains both readings simultaneously (coexistence) or whether the readings are held by distinct, competing factions (foreclosure potential). Look for hybrid models that incorporate elements of both.',
    'If readings genuinely foreclose, the cs_structure.reading_relations value is ''forecloses''; if they coexist as competing positions, it is ''coexists_with''. This determines whether the kernel contest is a matter of logical incompatibility or institutional competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether sibling readings logically foreclose or can coexist.').

omega_variable(
    medieval_latin_literacy_community,
    'What was the size and composition of the community that read and understood medieval Latin at its own time? Did medieval clerics treat medieval Latin as distinct from ''correct'' Latin, or as the same language correctly spoken?',
    'Textual evidence: medieval grammatical commentaries, prescriptive texts, metalinguistic remarks. If medieval authors distinguish their own practice from classical norms, the constraint may be imposing a retrospective unity; if they treat medieval practice as correct Latin, the constraint captures medieval self-perception.',
    'If medieval actors saw their practice as continuous with classical Latin, the continuity reading captures genuine historical consciousness; if they saw divergence, the constraint imposes a modern analytical category that medieval people would not recognize. This affects the constraint''s claim to be natural or imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_latin_literacy_community, empirical, 'Medieval linguistic self-perception: did they treat medieval Latin as continuous with or distinct from classical Latin?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__continuity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__continuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t5, correct_latin_kernel__continuity_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement_basis(corr_tr_t5, observed).
narrative_ontology:measurement(corr_tr_t10, correct_latin_kernel__continuity_reading, theater_ratio, 10, 0.43).
narrative_ontology:measurement_basis(corr_tr_t10, observed).
narrative_ontology:measurement(corr_tr_t15, correct_latin_kernel__continuity_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(corr_tr_t15, observed).
narrative_ontology:measurement(corr_tr_t20, correct_latin_kernel__continuity_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(corr_tr_t20, observed).
narrative_ontology:measurement(corr_tr_t25, correct_latin_kernel__continuity_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(corr_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__continuity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t5, correct_latin_kernel__continuity_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(corr_be_t5, observed).
narrative_ontology:measurement(corr_be_t10, correct_latin_kernel__continuity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(corr_be_t10, observed).
narrative_ontology:measurement(corr_be_t15, correct_latin_kernel__continuity_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(corr_be_t15, observed).
narrative_ontology:measurement(corr_be_t20, correct_latin_kernel__continuity_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(corr_be_t20, observed).
narrative_ontology:measurement(corr_be_t25, correct_latin_kernel__continuity_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(corr_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__continuity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t5, correct_latin_kernel__continuity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(corr_su_t5, observed).
narrative_ontology:measurement(corr_su_t10, correct_latin_kernel__continuity_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(corr_su_t10, observed).
narrative_ontology:measurement(corr_su_t15, correct_latin_kernel__continuity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(corr_su_t15, observed).
narrative_ontology:measurement(corr_su_t20, correct_latin_kernel__continuity_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(corr_su_t20, observed).
narrative_ontology:measurement(corr_su_t25, correct_latin_kernel__continuity_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(corr_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__continuity_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% The correct_latin_kernel decomposes into three constraint stories: continuity_reading (Medieval Latin as natural evolution; reconstruction as internal correction), discontinuity_reading (Medieval and Classical Latin are distinct systems; reconstruction requires symbolic recovery), and hybrid_reading (morphology continuous, syntax/lexicon require recovery). These are NOT the same constraint viewed from different angles — they have different beneficiary/victim structures, different ε values, different institutional consequences. Each reading instantiates a different constraint by the ε-invariance principle (OQ-26): changing which reading is dominant changes what extraction looks like, who benefits, what suppression is required. They are linked here as a constraint family affecting each other's institutional viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__continuity_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
