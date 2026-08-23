% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   The reconstruction reading of the classical Latin standard emerges in
 *   15th-century Italian humanism (Valla, Poliziano, Bude) and spreads
 *   through print and education across Europe. It asserts that correct Latin
 *   exists only in Classical authors (Cicero, Caesar, Virgil) and must be
 *   recovered through philological archaeology — textual criticism,
 *   manuscript collation, and grammatical reconstruction — requiring a
 *   discontinuous break from the medieval Latin that had evolved continuously
 *   since antiquity. Medieval forms are not developments but corruptions.
 *   This reading becomes the institutional standard in schools,
 *   chancelleries, and the Catholic Church (Tridentine reform), creating a
 *   philological gatekeeping class and delegitimizing centuries of
 *   practice-based Latin usage.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.75).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.8).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Classical Latin Reconstruction Standard (Humanist Reading)").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, 'db6c8b6e-effa-45ba-9dc3-db6457b91c65').
narrative_ontology:cs_kernel_codification('db6c8b6e-effa-45ba-9dc3-db6457b91c65', fixed_text).
narrative_ontology:cs_authority_grounding('db6c8b6e-effa-45ba-9dc3-db6457b91c65', lineage).
narrative_ontology:cs_interpretation_layer_present('db6c8b6e-effa-45ba-9dc3-db6457b91c65').
narrative_ontology:cs_reading_relation('db6c8b6e-effa-45ba-9dc3-db6457b91c65', classical_latin_standard__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('db6c8b6e-effa-45ba-9dc3-db6457b91c65', classical_latin_standard__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('db6c8b6e-effa-45ba-9dc3-db6457b91c65', foundational, classical_exclusivity_defines_correctness).
narrative_ontology:cs_axiom_status(classical_exclusivity_defines_correctness, holdable).
narrative_ontology:cs_axiom_grounding('db6c8b6e-effa-45ba-9dc3-db6457b91c65', classical_exclusivity_defines_correctness, conventional).
narrative_ontology:cs_axiom('db6c8b6e-effa-45ba-9dc3-db6457b91c65', foundational, medieval_forms_are_corruption_not_development).
narrative_ontology:cs_axiom_status(medieval_forms_are_corruption_not_development, holdable).
narrative_ontology:cs_axiom_grounding('db6c8b6e-effa-45ba-9dc3-db6457b91c65', medieval_forms_are_corruption_not_development, conventional).
narrative_ontology:cs_reference_frame('db6c8b6e-effa-45ba-9dc3-db6457b91c65', classical_textual_standard).
narrative_ontology:cs_drift_state('db6c8b6e-effa-45ba-9dc3-db6457b91c65', modern_linguistic_turn, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db6c8b6e-effa-45ba-9dc3-db6457b91c65', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_philologists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_educators).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, print_grammarians).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, medieval_latin_practitioners).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, clergy_using_medieval_latin).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, administrators_using_medieval_latin).
narrative_ontology:constraint_vindicates(classical_latin_standard__reconstruction_reading, classical_textual_authority).
narrative_ontology:constraint_vindicates(classical_latin_standard__reconstruction_reading, philological_reconstruction_as_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the reconstructed Classical standard through textual criticism, commentaries, and pedagogical grammars. Their professional authority and patronage depend on being the sole arbiters of 'correct' Latin. They control the philological apparatus that legitimates the standard.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_philologists, agenda_setter,
    organized, generational, mobile, continental).

% Teach the reconstructed Classical Latin in new humanist schools, displacing medieval curricula. Gain professional prestige and institutional positions by aligning with the philological standard. Their career advancement requires mastery and transmission of the reconstructed forms.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_educators, beneficiary,
    organized, biographical, constrained, continental).

% Produce and sell printed grammars, dictionaries, and editiones principes of Classical authors. The reconstruction standard creates a mass market for standardized Latin textbooks. Their commercial success depends on the standard's institutional adoption.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, print_grammarians, beneficiary,
    moderate, biographical, constrained, continental).

% Clerks, notaries, and scholars whose working Latin incorporates centuries of post-Classical development. Their practice is declared 'corrupt' and 'barbarous'; they must retrain in reconstructed forms or lose professional legitimacy. Exit requires learning a new linguistic system late in career.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, medieval_latin_practitioners, payer,
    moderate, biographical, constrained, continental).

% Ecclesiastical users for whom medieval Latin is the language of liturgy, canon law, and theology. The reconstruction standard brands their sacramental and legal language as incorrect. Their identity is fused with the medieval practice; adopting Classical forms threatens theological continuity and institutional self-understanding.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, clergy_using_medieval_latin, payer,
    institutional, generational, identity_locked, continental).

% Chancery officials, diplomats, and civic administrators whose documentary Latin follows medieval conventions. The new standard demands Classical forms in official documents, requiring costly retraining of staff and revision of formularies. Resistance risks appearing 'unlearned' to humanist patrons.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, administrators_using_medieval_latin, payer,
    institutional, biographical, constrained, continental).

% University-based scholastic philosophers and theologians whose technical Latin vocabulary and syntax are dismissed as 'barbarisms.' They would argue that their precise technical language serves philosophical rigor better than Ciceronian imitation, but are structurally excluded from the humanist-defined conversation about correctness.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, rival_scholastic_traditions, excluded,
    organized, generational, trapped, continental).

% Descriptive linguists and historians of language who analyze the reconstruction standard as a historical prescriptive project. They neither enforce nor pay the standard; they study its emergence, enforcement, and effects on Latin's trajectory from living language to learned code.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, modern_linguists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single fixed Latin standard for international communication across Europe, replacing divergent medieval regional varieties that threatened Latin's role as universal language of scholarship, diplomacy, and church.
% TRANSFER_FUNCTION: Moves authority over Latin correctness from practice-based institutional users (church, chancery, university) to philological experts who control the reconstructed Classical standard, concentrating gatekeeping power in a new humanist professional class.
% ABSENT_VOICES: Medieval Latin practitioners (clerks, clergy, scholastic philosophers) whose living practice was declared 'corrupt' without consultation; vernacular speakers whose interface with Latin changed as the reconstructed standard displaced accessible medieval forms.
% DISAPPEARANCE_RATIONALE: The reconstruction standard created the humanist Latin that dominated European scholarship, diplomacy, and education for three centuries; its removal would collapse the fixed standard enabling international Latin communication and return Latin to fragmented practice-based evolution or vernacular replacement.
% FOUNDING_PROBLEM: Medieval Latin had diverged into regional varieties with differing vocabulary, syntax, and orthography, threatening Latin's function as a universal language of scholarship, diplomacy, and ecclesiastical communication across Europe.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary humanists (Erasmus, Valla, Bude) attested the fragmentation problem was real; modern historians of Latin (Waquet, Leonhardt) confirm regional divergence threatened Latin's international role; but the problem is dead per modern linguists and historians — Latin is no longer a lingua franca, and the universal-language function has vanished.
narrative_ontology:disappearance_verdict(classical_latin_standard__reconstruction_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__reconstruction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__reconstruction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__reconstruction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__reconstruction_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction is high (0.75) because the standard systematically transfers authority and material benefits (positions, publications, patronage) from existing institutional users to a new philological elite. Suppression is very high (0.80) because medieval forms are actively suppressed through pedagogical reform, print standardization, and institutional mandates — not merely discouraged but branded as error. Theater ratio is moderate (0.40): the philological work is genuine scholarship, but a substantial share of enforcement activity serves gatekeeping rather than textual recovery. Accessibility collapse (0.70) is high for a constructed constraint: once the Classical standard is institutionalized, medieval practice becomes professionally illegitimate. Resistance (0.65) is substantial: scholastic universities, ecclesiastical conservatives, and vernacular movements all contested the standard.
 *
 * PERSPECTIVAL GAP:
 *   From the philologist seat, the constraint is genuine coordination: a recovered standard enabling precise international communication. From the medieval practitioner seat, it is enforced extraction: a new elite imposing an archaic code that serves their professional interests. From the clergy seat, it is identity threat: the language of prayer and sacrament declared corrupt. The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist philologists are structural beneficiaries (d near 0.0): they define the standard, control its interpretation, and collect professional rents. Humanist educators and print grammarians are secondary beneficiaries (d ~0.2): they gain from the standard's diffusion but depend on philologists for authority. Medieval practitioners, clergy, and administrators are targets (d near 1.0): they bear retraining costs, legitimacy loss, and identity disruption. Clergy are identity_locked — their sacramental language is fused with medieval practice, making exit existentially costly. Administrators are constrained — they can comply but at high organizational cost. Rival scholastic traditions are trapped: excluded from the correctness conversation entirely. Modern linguists are analytical (d=0.5): they observe without stakes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Latin's fragmentation threatening its universal function) was real and live in 1450. By 1650, Latin's role as lingua franca was already eroding under vernacular competition; by 1800, the problem was dead. Yet the reconstruction standard persisted in education and church long after its coordination function vanished — a classic mandatrophy trajectory where the gatekeeping class (philologists, classicists) maintained the standard because their authority depended on it, not because the coordination problem survived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_of_classical_form,
    'Did the coordination function (a single fixed standard for international Latin) genuinely require the specific Classical form, or would any stabilized standard (e.g., a regularized medieval Latin) have served equally well?',
    'Counterfactual historical analysis: compare regions where humanist Latin was adopted vs. where medieval Latin persisted; assess whether communication efficiency differed.',
    'If any stabilized standard would have worked, the Classical form''s specificity is extractive — it creates a gatekeeping class with specialized philological skills rather than solving a coordination problem that required that specific form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_of_classical_form, empirical, 'Whether the Classical form was functionally necessary for coordination or contingently selected to benefit philologists.').

omega_variable(
    suppression_mechanism_vs_gatekeeping,
    'Was the active suppression of medieval forms (branding as ''corruption'', pedagogical exclusion, institutional mandates) necessary for the coordination function, or did it primarily serve to create and protect the philological gatekeeping class?',
    'Analyze the timeline: did suppression intensity track coordination needs (e.g., diplomatic communication) or professionalization milestones (university chairs, printing privileges, papal briefs)?',
    'If suppression tracks professionalization more than coordination needs, the constraint''s extraction component is larger than its coordination component — supporting snare over tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_vs_gatekeeping, empirical, 'Whether suppression served coordination or gatekeeping.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reconstruction reading''s beneficiary/victim structure differ from the sibling readings of the classical_latin_standard kernel?',
    'Compare the three readings'' structural profiles: continuity_reading beneficiaries are institutional users of medieval Latin (clergy, chancery), victims are would-be reformers; hybrid_reading beneficiaries are both philologists and technical specialists, victims are minimal; reconstruction_reading beneficiaries are philologists, victims are medieval practitioners.',
    'The kernel''s contest is structurally a dispute over who benefits and who pays: each reading creates a different beneficiary/victim partition. This confirms the kernel is not a single constraint but a family of distinct constraints sharing a label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame structural delta across the classical_latin_standard kernel''s three readings.').

omega_variable(
    identity_lock_mechanism_clergy,
    'Is the clergy''s identity_locked exit status driven by theological conviction (Latin as sacramental language), institutional inertia (canon law codified in medieval Latin), or professional identity (clerical formation in medieval Latin)?',
    'Historical analysis of Tridentine reform debates: did bishops resist Classical Latin on theological grounds, practical grounds, or both? Track Jesuit vs. secular clergy adoption patterns.',
    'If theological, the identity lock is deontological (grounded in duty to tradition) and resistant to external pressure. If institutional/practical, it may erode when the institution shifts (as it did post-Vatican II). This affects whether the constraint''s extraction from clergy is structural or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_clergy, empirical, 'Mechanism of clergy identity lock to medieval Latin practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cls_recon_tr_t0, classical_latin_standard__reconstruction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cls_recon_tr_t40, classical_latin_standard__reconstruction_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(cls_recon_tr_t80, classical_latin_standard__reconstruction_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement(cls_recon_tr_t120, classical_latin_standard__reconstruction_reading, theater_ratio, 120, 0.37).
narrative_ontology:measurement(cls_recon_tr_t160, classical_latin_standard__reconstruction_reading, theater_ratio, 160, 0.39).
narrative_ontology:measurement(cls_recon_tr_t200, classical_latin_standard__reconstruction_reading, theater_ratio, 200, 0.4).

% Extraction over time
narrative_ontology:measurement(cls_recon_be_t0, classical_latin_standard__reconstruction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cls_recon_be_t40, classical_latin_standard__reconstruction_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(cls_recon_be_t80, classical_latin_standard__reconstruction_reading, base_extractiveness, 80, 0.65).
narrative_ontology:measurement(cls_recon_be_t120, classical_latin_standard__reconstruction_reading, base_extractiveness, 120, 0.7).
narrative_ontology:measurement(cls_recon_be_t160, classical_latin_standard__reconstruction_reading, base_extractiveness, 160, 0.73).
narrative_ontology:measurement(cls_recon_be_t200, classical_latin_standard__reconstruction_reading, base_extractiveness, 200, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(cls_recon_su_t0, classical_latin_standard__reconstruction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cls_recon_su_t40, classical_latin_standard__reconstruction_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(cls_recon_su_t80, classical_latin_standard__reconstruction_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement(cls_recon_su_t120, classical_latin_standard__reconstruction_reading, suppression_requirement, 120, 0.76).
narrative_ontology:measurement(cls_recon_su_t160, classical_latin_standard__reconstruction_reading, suppression_requirement, 160, 0.78).
narrative_ontology:measurement(cls_recon_su_t200, classical_latin_standard__reconstruction_reading, suppression_requirement, 200, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__reconstruction_reading, 0.08).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the reconstruction_reading of the classical_latin_standard kernel. The kernel decomposes into three structurally distinct constraints: continuity_reading (low extraction, practice-based authority), hybrid_reading (moderate extraction, dual coordination), and reconstruction_reading (high extraction, philological gatekeeping). Their ε values differ substantially: continuity ~0.15, hybrid ~0.40, reconstruction ~0.75. They share the label 'correct Latin' but have different failure modes, different empirical status, and different beneficiary/victim structures. The network edges reflect the historical sequence: reconstruction_reading structurally displaced continuity_reading in institutions, and hybrid_reading emerged as a negotiated position between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classical_latin_standard__reconstruction_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
