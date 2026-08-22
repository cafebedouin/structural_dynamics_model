% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Discontinuity Reading of Correct Latin: Ancient Texts as Sole Norm
 *   domain: intellectual_history/historical_linguistics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   correct_latin: the discontinuity_reading, under which correct Latin is
 *   exclusively the Classical form preserved in ancient texts, medieval usage
 *   sits outside the legitimate set as corrupt deviation, and command of the
 *   extinct form is reoccupied from textual symbols. The sibling readings —
 *   continuity_reading and hybrid_reading — are separate constraints in
 *   separate files; per the epsilon-invariance principle this file carries
 *   one stable epsilon over one referent: the standing arrangement under
 *   contest, namely the discontinuity standard as actually administered from
 *   the humanist recovery of the ancient corpus (t=0, circa 1520) to the
 *   present (t=500, circa 2020), assessed by this reading's own lights. The
 *   arrangement began as a solution to a real coordination problem
 *   (fragmented scholastic Latin) and drifted into a credentialing and
 *   identity machine: extraction concentrates in the philological
 *   establishment, costs fall on students, medievalists, living
 *   practitioners, and the Church's unbroken usage, and a growing share of
 *   enforcement activity is performative (composed exercises, prize
 *   examinations, correctness policing in shrinking circles). Assumptions
 *   stated: the interval maps elapsed years onto integer time points; the
 *   claim/metric pair is authored independently (claimed_type reflects my
 *   structural judgment; metrics reflect descriptive operation), and the
 *   engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - classical_philology_establishment: agenda-setter and principal beneficiary (institutional / identity_locked) — administers the standard, defines correctness, collects the deference and resources that flow to guardianship of the canon
 *   - classical_text_editors: secondary beneficiary (organized / constrained) — supply the reference texts the standard runs on
 *   - medieval_manuscript_scholars: primary target (organized / constrained) — their sources are pre-classified as deviant; career structure holds them in
 *   - latin_language_students: primary target (powerless / trapped) — bear the multi-year learning-cost transfer; no alternative credential path while enrolled
 *   - neo_latin_composers: dual-positioned target-beneficiary (moderate / identity_locked) — disciplined by the standard yet invested in the ideal it enforces
 *   - ecclesiastical_latin_authority: dual-positioned institutional party (institutional / constrained) — living usage judged by textual norms, yet stabilized by the fixed standard
 *   - romance_vernacular_communities: excluded voice (organized / continental) — descendants of the declared-deviant transmission, outside the conversation
 *   - historiographers_of_latin: analytical observer — sees the full structure and the fate of the rupture claim against evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.62).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.58).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Discontinuity Reading of Correct Latin: Ancient Texts as Sole Norm").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "intellectual_history/historical_linguistics").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, '9019b685-2bea-4558-b709-4d81fd30321e').
narrative_ontology:cs_kernel_codification('9019b685-2bea-4558-b709-4d81fd30321e', fixed_text).
narrative_ontology:cs_authority_grounding('9019b685-2bea-4558-b709-4d81fd30321e', lineage).
narrative_ontology:cs_interpretation_layer_present('9019b685-2bea-4558-b709-4d81fd30321e').
narrative_ontology:cs_reading_relation('9019b685-2bea-4558-b709-4d81fd30321e', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('9019b685-2bea-4558-b709-4d81fd30321e', correct_latin__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('9019b685-2bea-4558-b709-4d81fd30321e', foundational, rupture_between_classical_and_medieval_forms).
narrative_ontology:cs_axiom_status(rupture_between_classical_and_medieval_forms, holdable).
narrative_ontology:cs_axiom_grounding('9019b685-2bea-4558-b709-4d81fd30321e', rupture_between_classical_and_medieval_forms, empirically_contingent).
narrative_ontology:cs_axiom('9019b685-2bea-4558-b709-4d81fd30321e', foundational, extinct_form_recoverable_from_textual_symbols).
narrative_ontology:cs_axiom_status(extinct_form_recoverable_from_textual_symbols, holdable).
narrative_ontology:cs_axiom_grounding('9019b685-2bea-4558-b709-4d81fd30321e', extinct_form_recoverable_from_textual_symbols, instrumental).
narrative_ontology:cs_reference_frame('9019b685-2bea-4558-b709-4d81fd30321e', classical_canon_normative_terminus).
narrative_ontology:cs_drift_state('9019b685-2bea-4558-b709-4d81fd30321e', contemporary_philological_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9019b685-2bea-4558-b709-4d81fd30321e', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_philology_establishment).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_text_editors).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_manuscript_scholars).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, latin_language_students).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, neo_latin_composers).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, ecclesiastical_latin_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, neo_latin_composers).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, ecclesiastical_latin_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Professors, examiners, and curriculum committees in classics faculties define which forms count as correct Latin, train teachers in the grammatical-reconstruction method, and certify competence through examinations keyed to ancient authors. Careers, journals, and learned societies are organized around the ancient canon; abandoning the standard would mean dismantling the expertise these scholars personally embody. Deference and resources flow to them as guardians of the classical inheritance.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_philology_establishment, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, classical_philology_establishment, beneficiary).

% Scholars producing critical editions of ancient authors supply the reference texts against which correctness is measured. Each new edition refreshes the standard and confirms the editor's authority, and their volumes are cited wherever correctness is disputed. Their position depends on the ancient corpus remaining the sole touchstone; a shift to practice-based legitimacy would devalue the editorial enterprise.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_text_editors, beneficiary,
    organized, biographical, constrained, global).

% Researchers of medieval manuscripts work on materials the standard pre-classifies as deviant from correct Latin. To publish in mainstream venues they must frame their sources as objects of correction or antiquarian curiosity rather than as bearers of legitimate usage; posts, grants, and journal space sit disproportionately with colleagues of the ancient corpus. Leaving would mean exiting the discipline that trains, employs, and credentials them.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_manuscript_scholars, payer,
    organized, generational, constrained, global).

% Learners in schools and universities acquire Latin through memorized paradigms and composed exercises checked against ancient usage, a route that consumes years before any fluent use is possible. While enrolled they cannot obtain credentials by any other path; most abandon the language after certification, and the minority who continue inherit and reproduce the same standard they were taught under.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, latin_language_students, payer,
    powerless, immediate, trapped, global).

% Writers who compose new Latin for scholarship, liturgy, or personal practice are judged by ancient usage and corrected for barbarisms whenever their phrasing follows later transmission. Many embrace the discipline willingly, because purity of Ciceronian style purchases standing inside the small community of active Latinists; the same rule that disciplines them supplies the ideal they aspire to, and their sense of who they are is bound up with pursuing it.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, neo_latin_composers, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, neo_latin_composers, beneficiary).

% The Church's drafting offices produce encyclicals, liturgical texts, and diplomatic documents in an unbroken writing practice that reaches back across the ancient-medieval divide. Its living usage is measured against ancient texts and periodically corrected toward them, yet the fixed standard also gives its documents a stability and universality that no living dialect could supply, and the office plans in centuries, not careers.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, ecclesiastical_latin_authority, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, ecclesiastical_latin_authority, beneficiary).

% Speakers of French, Spanish, Italian, and the other daughter languages descend from the very transmission the standard declares corrupt. They hold no seat in the correctness dispute — their languages left the arena centuries ago — yet the reading's genealogy implicitly ranks their entire linguistic ancestry as deviation. They would contest the framing, but they stand outside the classics conversation where the standard is set.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, romance_vernacular_communities, excluded,
    organized, generational, constrained, continental).

% Historians of the language and of classical scholarship trace how the standard arose, whom it served, and how its claims fared against accumulating evidence. They hold no stake in which forms are correct, and both camps cite their work: the establishment for the grandeur of the recovery project, its critics for the record of stigmatization and marginalization.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, historiographers_of_latin, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__discontinuity_reading, classical_philology_establishment).
narrative_ontology:fixing_cost_class(correct_latin__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors the definition of correct Latin to a fixed, publicly inspectable corpus of ancient texts, so that correctness can be verified identically at any place and time without access to a living speech community; supports stable critical editions, cross-border scholarly citation, and reproducible examination standards.
% TRANSFER_FUNCTION: Moves linguistic authority, posts, grant funding, and curricular hours toward specialists trained on the ancient corpus and away from scholars and practitioners of transmitted forms; moves students' years of preparation and fees into grammatical reconstruction; moves prestige from medieval manuscripts to classical ones.
% ABSENT_VOICES: Romance-vernacular speech communities, whose ancestry the reading implicitly ranks as deviation, and the medieval authorial tradition itself, which survives only through advocates. Living-Latin educators who would argue for practice-based legitimacy also lack a seat. All of them stand outside classics faculties — in modern-language departments, historical institutes, and independent Latinist networks — with no vote on curriculum committees or editorial boards.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, curricula, examinations, critical-editing priorities, and career lines in several disciplines would rearrange within a generation; medieval Latin would be reclassified as a legitimate variety rather than a deviation; neo-Latin practice would reorganize around living norms; and the credential premium attached to Classical mastery would migrate or evaporate.
% FOUNDING_PROBLEM: Late-medieval Latin had fragmented into regional scholastic jargons, degrading scholarly communication across borders; the humanists built the discontinuity standard to restore a stable, eloquent, trans-European Latin by reanchoring it to the recovered texts of antiquity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by histories of Neo-Latin and of classical scholarship, which document the retreat of Latin as a working language after roughly 1700 and the standard's migration into credentialing; by sociolinguistic surveys of Latin's communicative decline; and by medievalists' published objections to the corruption framing. The classics establishment attests continued liveness by pointing to residual niches — ecclesiastical drafting, taxonomic nomenclature — but no source outside the beneficiary set attests that the original fragmentation problem still exists.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62: the standard transfers years of learner effort, marginalizes an entire scholarly field's sources, and concentrates authority in the ancient-corpus professions, while retaining a thin live verification function (fixed-text checking for nomenclature, epigraphy, ecclesiastical drafting) that keeps it from scoring as pure extraction. Suppression is authored at 0.58 as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine. Suppression here is mixed structural and internalized: curricular gates and editorial gatekeeping are structural, but after enforcement machinery decayed (see the suppression_requirement series peaking near t=300 and falling to 0.40 by t=500), compliance persists through professional identity fusion — hence the scalar exceeds the series endpoint, and the ambiguity is routed to the suppression_structural_vs_internalized omega. Theater_ratio is authored at 0.55 and rising across the series: composed Ciceronian exercises, prize examinations, and correctness disputes in ever-smaller circles increasingly substitute for the communicative function the standard once served. Accessibility_collapse is 0.55: within the reading's own framework, accepting the rupture premise collapses alternatives almost completely (if transmitted usage is corrupt, textual reconstruction is the only road), but across readings the continuity and hybrid alternatives remain live, which caps collapse. Resistance is 0.6: Erasmus's Ciceronianus, the persistent medievalist objection, the living-Latin movement, and the historical-linguistic evidence for transmissional continuity all press against the standard and have never been silenced. The claim/metric pair is deliberately unreconciled: I claim tangled_rope because a genuine coordination function and asymmetric extraction coexist in one enforced structure; the engine computes each seat independently.
 *
 * PERSPECTIVAL GAP:
 *   From the establishment seat the arrangement computes as near-rope: a hard-won recovery of a civilizational patrimony, a verifiable standard, a discipline the seat itself embodies — exit is unthinkable because the standard and the scholar are the same thing. From the student seat it computes as near-snare: years of compulsory reconstruction for a credential, with no alternative path and no use at the end for most. From the medievalist seat it is a boundary-drawing machine that pre-stigmatizes the object of study. Same structure, different computed types per seat; the engine derives this divergence from power, exit, and directional position, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The establishment and the editors sit near the beneficiary pole: the standard subsidizes their authority, and the establishment's identity_locked exit amplifies its investment in persistence. Students and medievalists sit near the target pole — students trapped (no credential path around the standard), medievalists constrained (one discipline, one job market) — so effective extraction lands on them near full strength. Neo-Latin composers and the ecclesiastical authority are genuinely dual-positioned: each pays corrections on living usage while collecting stability or standing from the fixed norm, placing them mid-range; the group-level derivation from the beneficiaries/victims arrays cannot see this duality, so it is carried in secondary_role declarations and here rather than in overrides (overrides are keyed by power atom and would misfire across agents sharing an atom). Romance-vernacular communities are excluded rather than coordinated: their absence from the derivation is itself the datum that the standard's unanimity was produced by keeping dissenting seats out of the room.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmented scholastic Latin degrading trans-European communication — is dead: vernaculars absorbed the communicative load, and no one outside the beneficiary set attests the problem still exists, while the arrangement demonstrably still organizes curricula, careers, and identities (disappearance_verdict: world_rearranges). The dead-problem x rearranges-world mismatch is exactly the capture/zombie signature the R5 consumer flags, and I author it honestly rather than flattering the origin. The classification prevents mislabeling in both directions: calling the whole arrangement a snare would erase the thin live verification function that still serves nomenclature, epigraphy, and ecclesiastical drafting; calling it a rope would erase the concentrated capture (gain_flow names the establishment seat) and the pre-stigmatization of medieval scholarship. If the establishment ever withers as a capturing seat — if capture diffuses and only inertial performance remains — the residue would re-read as piton; the rising theater series is the leading indicator of that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (discontinuity_reading) of the kernel correct_latin; what would the sibling readings change structurally if adopted?',
    'Track adoption patterns in curricula, editorial practice, and examination standards: adoption of continuity_reading would restore medieval forms to the legitimate usage set and dissolve the stigmatization of transmitted practice; adoption of hybrid_reading would retain transmitted forms as presumptively valid subject to local textual correction.',
    'Under continuity_reading the victim set collapses (nothing is pre-classified as corrupt) and epsilon falls sharply; under hybrid_reading the victim set narrows to uncorrected usage and epsilon lands between this reading and continuity. The disagreement is located entirely in where legitimacy attaches: ancient text only, continuous practice only, or practice-with-textual-correction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the correct_latin kernel; siblings are separate constraints.').

omega_variable(
    rupture_thesis_empirical_status,
    'Is the declared rupture between Classical and medieval Latin a linguistically real break, or a constructed boundary imposed on a substantially continuous transmission?',
    'Diachronic corpora and manuscript transmission studies tracing morphosyntactic and lexical continuity from late antiquity through the medieval period; the historical-linguistic literature already leans strongly toward continuity.',
    'If the rupture dissolves, the reading''s foundational axiom loses its empirical ground, the stigmatization of medieval usage stands exposed as pure boundary-drawing, and the arrangement drifts from tangled_rope toward snare; if a genuine break existed, part of the enforcement cost is the price of a real distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_thesis_empirical_status, empirical, 'Whether the Classical/medieval rupture is real or constructed.').

omega_variable(
    reconstruction_sufficiency,
    'Can command of an extinct form actually be reoccupied from textual symbols alone, or does grammatical reconstruction yield only analytic knowledge that no amount of examination performance converts into legitimate use?',
    'Comparative outcomes of grammar-reconstruction programs versus immersive living-Latin programs on production fluency, comprehension speed, and retention.',
    'If reconstruction is insufficient by design, the standard demands an unfalsifiable competence and the gap between certified and usable command is pure extracted effort; if sufficient, a large share of the learning-cost burden is the genuine price of the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_sufficiency, empirical, 'Whether textual reconstruction delivers the competence the standard certifies.').

omega_variable(
    persistence_via_capture_or_function,
    'Does the arrangement persist because the fixed-text verification function still coordinates anything, or purely through credential capture by the disciplines organized around it?',
    'Counterfactual audit of what breaks if the standard were withdrawn tomorrow: residual users (ecclesiastical drafting, taxonomic nomenclature, epigraphy) versus credential-dependent populations (examinations, curricula, career lines).',
    'If residual function dominates, the tangled_rope reading holds and reform should preserve the verification core; if capture dominates, the arrangement is a snare wearing a philological coat and the correct remedy is withdrawal rather than reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_via_capture_or_function, empirical, 'Live coordination function versus credential capture as the persistence mechanism.').

omega_variable(
    suppression_structural_vs_internalized,
    'Now that external enforcement machinery has decayed (see the falling suppression_requirement series), is the remaining suppression structural (curricular gates, editorial gatekeeping) or internalized (professional identity fusion with the classical canon)?',
    'Post-reform cohort behavior: whether scholars trained after Latin requirements were dropped still police correctness with the same vigor, and whether defection rates rise when identity-affirming rewards are removed.',
    'If internalized, the scalar suppression understates effective lock-in — targets carry the standard with them after every external barrier is removed, and removal of enforcement will not by itself dissolve the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression after enforcement decay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__discontinuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(corr_tr_t100, correct_latin__discontinuity_reading, theater_ratio, 100, 0.17).
narrative_ontology:measurement(corr_tr_t200, correct_latin__discontinuity_reading, theater_ratio, 200, 0.24).
narrative_ontology:measurement(corr_tr_t300, correct_latin__discontinuity_reading, theater_ratio, 300, 0.32).
narrative_ontology:measurement(corr_tr_t400, correct_latin__discontinuity_reading, theater_ratio, 400, 0.44).
narrative_ontology:measurement(corr_tr_t500, correct_latin__discontinuity_reading, theater_ratio, 500, 0.55).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__discontinuity_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(corr_be_t100, correct_latin__discontinuity_reading, base_extractiveness, 100, 0.41).
narrative_ontology:measurement(corr_be_t200, correct_latin__discontinuity_reading, base_extractiveness, 200, 0.47).
narrative_ontology:measurement(corr_be_t300, correct_latin__discontinuity_reading, base_extractiveness, 300, 0.53).
narrative_ontology:measurement(corr_be_t400, correct_latin__discontinuity_reading, base_extractiveness, 400, 0.58).
narrative_ontology:measurement(corr_be_t500, correct_latin__discontinuity_reading, base_extractiveness, 500, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__discontinuity_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(corr_su_t100, correct_latin__discontinuity_reading, suppression_requirement, 100, 0.36).
narrative_ontology:measurement(corr_su_t200, correct_latin__discontinuity_reading, suppression_requirement, 200, 0.46).
narrative_ontology:measurement(corr_su_t300, correct_latin__discontinuity_reading, suppression_requirement, 300, 0.58).
narrative_ontology:measurement(corr_su_t400, correct_latin__discontinuity_reading, suppression_requirement, 400, 0.52).
narrative_ontology:measurement(corr_su_t500, correct_latin__discontinuity_reading, suppression_requirement, 500, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints over one kernel: the continuity reading (low epsilon — transmitted practice is validated, no victim set), this discontinuity reading (moderate-high epsilon — transmission stigmatized, costly reconstruction mandated, concentrated capture), and the hybrid reading (intermediate epsilon — transmitted forms presumptively valid, targeted correction). The discontinuity reading is downstream of the humanist recovery of the ancient corpus and upstream of curricular, editorial, and examination enforcement; it links to both siblings because each contests the same legitimacy boundary, and where that boundary is drawn determines who pays. Measuring 'correctness of Latin' against different observables (living practice versus ancient text versus corrected transmission) yields different epsilons — which is precisely why these are three constraints, not one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
