% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__reconstruction_reading, []).

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
 *   constraint_id: classical_latin_standard__reconstruction_reading
 *   human_readable: Classical Latin Reconstruction Standard (Humanist Reading)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   The humanist reconstruction reading of the Classical Latin kernel
 *   declares that correct Latin exists only in the Classical corpus (roughly
 *   100 BCE–100 CE) and must be recovered through philological archaeology —
 *   critical editing, textual emendation, and grammatical purification.
 *   Medieval Latin, the living Latin of universities, church, and law for a
 *   millennium, is systematically delegitimized as 'corruption,' 'barbarism,'
 *   and 'decay.' This reading installs a new gatekeeping class (humanist
 *   philologists, editors, educational reformers) whose authority derives
 *   from mastery of the reconstructed standard. The constraint operates as a
 *   snare: the coordination story (a unified Latin for scholarship and print)
 *   is real but thin; the extraction is the systematic transfer of epistemic
 *   legitimacy from communities of practice to a textualist elite.
 *   Suppression is high because the constraint's persistence depends on
 *   excluding the continuity reading from institutional channels — university
 *   chairs, printing privileges, curricular authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.78).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.85).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, snare).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Classical Latin Reconstruction Standard (Humanist Reading)").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, 'e928e7b7-9521-47c7-abff-b074fe5d00a3').
narrative_ontology:cs_kernel_codification('e928e7b7-9521-47c7-abff-b074fe5d00a3', fixed_text).
narrative_ontology:cs_authority_grounding('e928e7b7-9521-47c7-abff-b074fe5d00a3', lineage).
narrative_ontology:cs_interpretation_layer_present('e928e7b7-9521-47c7-abff-b074fe5d00a3').
narrative_ontology:cs_reading_relation('e928e7b7-9521-47c7-abff-b074fe5d00a3', classical_latin_standard__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('e928e7b7-9521-47c7-abff-b074fe5d00a3', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('e928e7b7-9521-47c7-abff-b074fe5d00a3', foundational, only_classical_corpus_is_normative).
narrative_ontology:cs_axiom_status(only_classical_corpus_is_normative, holdable).
narrative_ontology:cs_axiom_grounding('e928e7b7-9521-47c7-abff-b074fe5d00a3', only_classical_corpus_is_normative, conventional).
narrative_ontology:cs_axiom('e928e7b7-9521-47c7-abff-b074fe5d00a3', foundational, medieval_drift_is_categorically_illegitimate).
narrative_ontology:cs_axiom_status(medieval_drift_is_categorically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('e928e7b7-9521-47c7-abff-b074fe5d00a3', medieval_drift_is_categorically_illegitimate, conventional).
narrative_ontology:cs_axiom('e928e7b7-9521-47c7-abff-b074fe5d00a3', secondary, philological_reconstruction_is_sole_legitimate_method).
narrative_ontology:cs_axiom_status(philological_reconstruction_is_sole_legitimate_method, holdable).
narrative_ontology:cs_axiom_grounding('e928e7b7-9521-47c7-abff-b074fe5d00a3', philological_reconstruction_is_sole_legitimate_method, conventional).
narrative_ontology:cs_reference_frame('e928e7b7-9521-47c7-abff-b074fe5d00a3', classical_corpus_as_closed_normative_text).
narrative_ontology:cs_drift_state('e928e7b7-9521-47c7-abff-b074fe5d00a3', humanist_institutional_peak, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e928e7b7-9521-47c7-abff-b074fe5d00a3', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_philological_elite).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, renaissance_educational_reformers).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, textual_critics_and_editors).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, medieval_university_masters).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, ecclesiastical_latin_practitioners).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, scholastic_theologians).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, legal_draftsmen_and_notaries).
narrative_ontology:constraint_vindicates(classical_latin_standard__reconstruction_reading, classical_purity_doctrine).
narrative_ontology:constraint_vindicates(classical_latin_standard__reconstruction_reading, philological_authority_over_living_usage).
narrative_ontology:constraint_vindicates(classical_latin_standard__reconstruction_reading, historical_reconstruction_as_normative_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the emerging philological methodology, editorial standards, and educational curricula. Authors the grammars, commentaries, and critical editions that define 'correct' Latin. Gains professional prestige, patronage, and institutional positions from their monopoly on textual authority. Can move between courts, universities, and printing houses.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_philological_elite, agenda_setter,
    organized, generational, mobile, continental).

% Build new Latin curricula around reconstructed Classical models, displacing medieval textbooks. Gain legitimacy and funding by aligning with the humanist standard. Their exit is constrained by the sunk cost of curriculum reform and the network of humanist schools they administer.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, renaissance_educational_reformers, beneficiary,
    organized, biographical, constrained, continental).

% Produce critical editions that purify texts toward Classical norms. Their professional standing depends on the reconstruction standard; the printing market rewards editions that conform. Exit would mean abandoning the editorial methodology that defines their craft.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, textual_critics_and_editors, beneficiary,
    moderate, biographical, constrained, continental).

% Teach and administer Latin curricula rooted in medieval textual tradition and scholastic methodology. Their authority derives from continuous institutional practice, now delegitimized as 'barbarous' and 'corrupt.' Identity is fused to the medieval university master role; adopting humanist norms would dissolve their professional self-concept and institutional standing simultaneously.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, medieval_university_masters, payer,
    institutional, biographical, identity_locked, continental).

% Maintain Latin as the living language of liturgy, canon law, and papal administration. Their Latin incorporates centuries of organic development. The reconstruction standard brands their usage as erroneous, threatening the legitimacy of ecclesiastical documents and the continuity of ritual practice. Exit is constrained by the sacramental and juridical necessity of Latin in their domain.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, ecclesiastical_latin_practitioners, payer,
    institutional, generational, constrained, global).

% Construct theological systems in a technical Latin vocabulary forged through medieval disputation. The reconstruction standard treats their precise terminology as 'corruption,' forcing a choice between abandoning their conceptual vocabulary or accepting delegitimization. Identity is fused to the scholastic method; the vocabulary IS the theology.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, scholastic_theologians, payer,
    organized, biographical, identity_locked, continental).

% Produce legally binding documents in a Latin of established formulae and conventions. The reconstruction standard introduces uncertainty: does a will or contract lose force if its Latin fails humanist purity tests? They bear the cost of retooling or the risk of invalidation. Exit is constrained by the immediate legal necessity of their output.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, legal_draftsmen_and_notaries, payer,
    moderate, immediate, constrained, regional).

% Defend the legitimacy of unbroken Latin transmission through medieval practice. Their voices are structurally excluded from the humanist academies, printing privileges, and new university chairs. They would object that language evolves by use, not by archaeological recovery, but the institutional channels for their objection are closed by the very standard they contest.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, continuity_tradition_adherents, excluded,
    organized, generational, trapped, continental).

% Later scholars (e.g., Valla, Erasmus, modern historical linguists) who analyze the reconstruction standard as a historical phenomenon. They see the full structure: the textual archaeology, the delegitimization of living practice, the creation of a gatekeeping class. They neither collect nor pay in the original constraint but inherit its epistemic consequences.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, philological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, textually anchored norm for Latin correctness across fragmented medieval usages, enabling interregional scholarly communication and a stable target for the new print technology.
% TRANSFER_FUNCTION: Moves epistemic authority and institutional legitimacy from practice-based communities (universities, church, law) to a philological elite whose authority derives from textual reconstruction. The transfer is legitimacy itself: who gets to say what counts as Latin.
% ABSENT_VOICES: Continuity tradition adherents — medieval masters, ecclesiastical practitioners, and scholastic theologians who would defend the legitimacy of organic linguistic development. They are structurally excluded from the humanist academies and printing networks that enforce the reconstruction standard.
% DISAPPEARANCE_RATIONALE: If the reconstruction standard vanished overnight, Latin correctness would revert to the plural, practice-based norms of the medieval period — university curricula, ecclesiastical usage, and legal formulae would regain legitimacy without reference to Classical archaeological recovery. The humanist editorial industry would lose its normative monopoly. The print market would lose its single quality standard.
% FOUNDING_PROBLEM: The proliferation of regional and institutional Latin variants in the late medieval period threatened mutual intelligibility among scholars, and the manuscript tradition showed significant textual corruption from scribal error. Humanists framed this as a crisis of authority requiring return to pure sources.
% FOUNDING_PROBLEM_CORROBORATION: Humanist apologists (Valla, Erasmus, Poliziano) attest the crisis of textual corruption and intelligibility. Medieval university records and ecclesiastical documents show functional Latin communication continuing without Classical archaeological recovery — the 'crisis' was largely constructed by the humanist project itself, as attested by contemporary critics (e.g., the Cologne scholastic faction, the Sorbonne's initial resistance to humanist curricula).
narrative_ontology:disappearance_verdict(classical_latin_standard__reconstruction_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__reconstruction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__reconstruction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(classical_latin_standard__reconstruction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__reconstruction_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__reconstruction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__reconstruction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint transfers the very capacity to define correctness from living practitioners to archaeological reconstructors. The medieval masters, ecclesiastics, and scholastics don't just pay a cost — they lose the standing to say what Latin IS. Suppression (0.85) is higher still because the constraint actively disqualifies the continuity reading: medieval forms are not merely disfavored but declared illegitimate. Theater ratio (0.42) reflects genuine philological work (textual criticism, manuscript collation) mixed with performative purity rituals (correcting 'barbarisms' that were functional technical terms). Accessibility collapse (0.68) is substantial but not total: medieval Latin remains functional in ecclesiastical and legal domains despite the humanist verdict. Resistance (0.55) is significant — the continuity reading persists in universities, the Church, and legal practice well into the 17th century.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist seat, the constraint is a rope: a genuine coordination problem (fragmented medieval Latin) solved by textual recovery. From the medieval master's seat, it is a snare: their living practice is declared corrupt by an external standard they had no hand in creating. From the ecclesiastical seat, it is a tangled rope: the coordination function (a stable liturgical language) is real, but the extraction (delegitimization of their developed technical vocabulary) is asymmetric. The engine computes this divergence from the structural data — the declared beneficiaries, victims, exit options, and power levels.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philological elite are structural beneficiaries (d ~ 0.15): they collect epistemic rents, professional positions, and patronage from the standard they administer. Renaissance reformers and textual critics are secondary beneficiaries (d ~ 0.25–0.35): they gain legitimacy but are constrained by the editorial methodology. Medieval masters and scholastic theologians are identity-locked targets (d ~ 0.95): their professional self-concept is fused to the practice the constraint delegitimizes; exit would dissolve their identity. Ecclesiastical practitioners and legal draftsmen are constrained targets (d ~ 0.7–0.8): they bear costs but retain functional domains where the reconstruction standard cannot fully displace them. Continuity adherents are excluded and trapped (d ~ 1.0): they are the enforcement object itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (textual corruption, mutual unintelligibility) was real but exaggerated by the humanist project. The reconstruction standard solved it by installing a new authority structure that extracts legitimacy from the very communities that maintained Latin as a living language. The mandate (purify Latin) outlived its function: once the critical editions were established and print standardized the text, the ongoing delegitimization of medieval Latin served no coordination purpose — it became pure extraction maintaining the humanist class's gatekeeping role. The constraint persists because the humanist elite became the new institutional incumbents (university chairs, academies, editorial projects) and the continuity reading was structurally excluded from the channels of legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the reconstruction_reading a distinct constraint with its own ε, or a measurement variant of the classical_latin_standard kernel?',
    'Apply the ε-invariance test: if changing the observable (textual purity vs. functional intelligibility vs. historical continuity) changes the constraint''s extractiveness, beneficiary structure, or suppression profile, they are distinct constraints. This reading authors high extraction and suppression; the continuity reading would author low extraction (no delegitimization of practice) and low suppression (plural norms). The ε values diverge → distinct constraints.',
    'Confirms the kernel decomposition is structurally valid, not a framing artifact. Each reading gets its own constraint story, its own stakeholders, its own classification. The network.affects_constraints links capture the structural influence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Validates the ε-invariance decomposition of the classical_latin_standard kernel into three constraint stories.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.85) primarily structural (institutional exclusion, printing monopolies, curricular mandates) or internalized (medieval practitioners accepting the ''corruption'' verdict)?',
    'Trace the suppression trajectory after the humanist standard''s institutional peak (post-1650). If medieval Latin practice continues in ecclesiastical/legal domains without internalizing the corruption verdict, suppression was largely structural. If practitioners adopt humanist self-correction, internalization operated.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression after institutional enforcement relaxes. This would elevate the constraint toward snare even as structural suppression decays.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the delegitimization of medieval Latin practice.').

omega_variable(
    coordination_necessity_of_reconstruction,
    'Was a single reconstructed Classical standard structurally necessary for Renaissance scholarly communication and print, or did the humanist elite create the coordination problem they then solved?',
    'Counterfactual: examine regions/institutions where medieval Latin continued without humanist reconstruction (e.g., parts of Northern Europe, ecclesiastical courts). Did mutual intelligibility collapse? Did print technology require a single Classical norm, or could it have standardized on a living medieval koine?',
    'If the coordination problem was manufactured, the constraint is a snare from origin — the rope function is a cover story. If genuine, the constraint began as a tangled rope (coordination + extraction) and degraded toward snare as the coordination need diminished.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity_of_reconstruction, conceptual, 'Whether the coordination function is endogenous to the constraint or a pre-existing condition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 1350, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t1350, classical_latin_standard__reconstruction_reading, theater_ratio, 1350, 0.12).
narrative_ontology:measurement(clas_tr_t1400, classical_latin_standard__reconstruction_reading, theater_ratio, 1400, 0.18).
narrative_ontology:measurement(clas_tr_t1450, classical_latin_standard__reconstruction_reading, theater_ratio, 1450, 0.28).
narrative_ontology:measurement(clas_tr_t1500, classical_latin_standard__reconstruction_reading, theater_ratio, 1500, 0.38).
narrative_ontology:measurement(clas_tr_t1550, classical_latin_standard__reconstruction_reading, theater_ratio, 1550, 0.41).
narrative_ontology:measurement(clas_tr_t1600, classical_latin_standard__reconstruction_reading, theater_ratio, 1600, 0.43).
narrative_ontology:measurement(clas_tr_t1650, classical_latin_standard__reconstruction_reading, theater_ratio, 1650, 0.42).

% Extraction over time
narrative_ontology:measurement(clas_be_t1350, classical_latin_standard__reconstruction_reading, base_extractiveness, 1350, 0.35).
narrative_ontology:measurement(clas_be_t1400, classical_latin_standard__reconstruction_reading, base_extractiveness, 1400, 0.48).
narrative_ontology:measurement(clas_be_t1450, classical_latin_standard__reconstruction_reading, base_extractiveness, 1450, 0.62).
narrative_ontology:measurement(clas_be_t1500, classical_latin_standard__reconstruction_reading, base_extractiveness, 1500, 0.71).
narrative_ontology:measurement(clas_be_t1550, classical_latin_standard__reconstruction_reading, base_extractiveness, 1550, 0.76).
narrative_ontology:measurement(clas_be_t1600, classical_latin_standard__reconstruction_reading, base_extractiveness, 1600, 0.79).
narrative_ontology:measurement(clas_be_t1650, classical_latin_standard__reconstruction_reading, base_extractiveness, 1650, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t1350, classical_latin_standard__reconstruction_reading, suppression_requirement, 1350, 0.45).
narrative_ontology:measurement(clas_su_t1400, classical_latin_standard__reconstruction_reading, suppression_requirement, 1400, 0.58).
narrative_ontology:measurement(clas_su_t1450, classical_latin_standard__reconstruction_reading, suppression_requirement, 1450, 0.72).
narrative_ontology:measurement(clas_su_t1500, classical_latin_standard__reconstruction_reading, suppression_requirement, 1500, 0.81).
narrative_ontology:measurement(clas_su_t1550, classical_latin_standard__reconstruction_reading, suppression_requirement, 1550, 0.84).
narrative_ontology:measurement(clas_su_t1600, classical_latin_standard__reconstruction_reading, suppression_requirement, 1600, 0.86).
narrative_ontology:measurement(clas_su_t1650, classical_latin_standard__reconstruction_reading, suppression_requirement, 1650, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__reconstruction_reading, 0.08).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel decomposes into three constraint stories per the ε-invariance principle. This reconstruction_reading authors high extractiveness (0.78) and suppression (0.85) because it categorically delegitimizes medieval practice. The continuity_reading would author near-zero extractiveness (plural norms, no gatekeeping) and low suppression (alternatives not excluded). The hybrid_reading would author moderate extractiveness (selective legitimacy for post-Classical developments) and moderate suppression (some alternatives excluded, others incorporated). The three stories form a constraint family with the reconstruction_reading upstream: its textual authority is cited to delegitimize the continuity_reading and to constrain the hybrid_reading's legitimate domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classical_latin_standard__reconstruction_reading, institutional, 0.95).
constraint_indexing:directionality_override(classical_latin_standard__reconstruction_reading, organized, 0.85).
constraint_indexing:directionality_override(classical_latin_standard__reconstruction_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
