% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__whole_language_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__whole_language_reading
 *   human_readable: Whole-Language Immersion Instructional Regime
 *   domain: education/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates ONE reading — whole_language_reading — of the
 *   contested literacy_acquisition_kernel. The arrangement under contest is
 *   the instructional regime in which early reading grows out of immersion in
 *   meaningful connected text, phonics knowledge is expected to develop
 *   incidentally through exposure and context, and explicit decoding
 *   instruction is treated as unnecessary at best and damaging to reading
 *   motivation at worst. The regime is coordinated through teacher
 *   preparation, district adoptions, leveled-text publishing, and
 *   professional culture. Its genuine coordination function is real: shared
 *   authentic texts build vocabulary, comprehension, and motivation for most
 *   children, and the anti-drill founding complaint was substantively
 *   grounded. Its extraction is real too: children without print-rich homes
 *   and children with dyslexia pay the price of missing explicit instruction
 *   during the developmental window when it matters most, and the bill
 *   compounds. Per the decomposition discipline, the sibling readings
 *   (phonics_reading, balanced_literacy_reading, structured_literacy_reading)
 *   are separate constraints with separate epsilon values in separate files;
 *   this file does not average across them. KEY AGENTS (by structural
 *   relationship): - holistic_literacy_educators: Primary agenda-setter and
 *   identity beneficiary (organized/identity_locked) — professional judgment
 *   and identity preserved by the regime - teacher_preparation_faculties:
 *   Institutional beneficiary (institutional/constrained) — programs and
 *   reputations anchored to the framework - leveled_text_publishers: Material
 *   beneficiary (institutional/arbitrage) — captures program revenue, pivots
 *   when markets shift - students_without_home_literacy_support: Primary
 *   target (powerless/trapped) — bears the missed-instruction cost -
 *   dyslexic_students: Primary target (powerless/trapped) — the method fails
 *   them categorically - affluent_print_rich_students: Incidental beneficiary
 *   with minor cost exposure (moderate/constrained) -
 *   parents_of_struggling_readers: Excluded voice turned organized challenger
 *   (organized/constrained) - district_literacy_directors: Administrative
 *   agenda-setter (institutional/mobile) - reading_scientists: Analytical
 *   observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.6).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.43).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.49).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.43).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.49).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole-Language Immersion Instructional Regime").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "education/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, 'cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4').
narrative_ontology:cs_kernel_codification('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', distributed).
narrative_ontology:cs_authority_grounding('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', lineage).
narrative_ontology:cs_interpretation_layer_present('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4').
narrative_ontology:cs_reading_relation('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_reading_relation('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', foundational, decoding_skills_induce_from_meaningful_exposure).
narrative_ontology:cs_axiom_status(decoding_skills_induce_from_meaningful_exposure, holdable).
narrative_ontology:cs_axiom_grounding('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', decoding_skills_induce_from_meaningful_exposure, empirically_contingent).
narrative_ontology:cs_axiom('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', secondary, explicit_decoding_instruction_harms_motivation).
narrative_ontology:cs_axiom_status(explicit_decoding_instruction_harms_motivation, holdable).
narrative_ontology:cs_axiom_grounding('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', explicit_decoding_instruction_harms_motivation, empirically_contingent).
narrative_ontology:cs_reference_frame('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', natural_meaning_acquisition_framework).
narrative_ontology:cs_drift_state('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cbc1cd33-a4e6-4e7d-9bcc-f0ca2acb3ea4', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, holistic_literacy_educators).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teacher_preparation_faculties).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, leveled_text_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, affluent_print_rich_students).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, dyslexic_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, affluent_print_rich_students).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classroom teachers, literacy coaches, and curriculum coordinators trained in whole-language methods. They design daily instruction around read-alouds, shared big-book reading, writing workshop, and leveled texts, and treat explicit phonics drills as contrary to what good literacy teaching is. Their professional identity, peer standing, and accumulated lesson libraries are built on the method; adopting systematic decoding instruction would mean publicly confronting years of prior practice and paying for retraining.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, holistic_literacy_educators, agenda_setter,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, holistic_literacy_educators, beneficiary).

% Education-school faculties whose courses, syllabi, textbooks, and scholarly reputations rest on constructivist literacy theory. They accredit each cohort of new teachers into the framework. Revising wholesale would unsettle degree programs, accreditation alignments, and published legacies accumulated over decades.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teacher_preparation_faculties, beneficiary,
    institutional, generational, constrained, continental).

% Commercial publishers selling leveled readers, big books, running-record kits, and guided-reading programs aligned to the immersion philosophy. Revenue tracks district adoptions of the approach. When markets shift, product lines can pivot to decodable texts; several major houses have already done so.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, leveled_text_publishers, beneficiary,
    institutional, biographical, arbitrage, global).

% Children arriving at school without print-rich homes, regular read-aloud routines, or letter-name familiarity. The immersion approach presumes background knowledge they do not have, and they receive no systematic decoding instruction during the narrow K-2 window in which most children map sounds to letters. They fall behind classmates, cannot leave the assigned classroom, and cannot buy tutoring; the gap compounds across every subsequent subject that requires reading.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support, payer,
    powerless, immediate, trapped, national).

% Students with phonological processing differences who cannot induce grapheme-phoneme correspondences from exposure alone, however rich. Under the immersion regime they are labeled unmotivated or slow, are coached to guess from pictures and context with strategies that fail them, and typically receive accurate help only after years of private evaluation and remediation their families fund.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, dyslexic_students, payer,
    powerless, biographical, trapped, global).

% Children from households with many books, literate adults, and paid enrichment. Household resources substitute for whatever the classroom does not supply, so the regime costs most of them little and delivers genuine goods: rich stories, wide vocabulary, positive associations with reading. A minority among them harbor undiagnosed dyslexia that the environment conceals until late.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, affluent_print_rich_students, beneficiary,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, affluent_print_rich_students, payer).

% Families who discover the gap through their own child's struggle, then find no seat in curriculum-adoption or philosophy decisions. They absorb private tutoring bills, evaluation costs, and years of advocacy. Organized coalitions eventually forced legislative remedies in many states; leaving the public system was never a realistic option for most.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers, payer).

% Administrators who select curricula, commission professional development, and defend the adopted philosophy before school boards. Career mobility lets them move between districts when instructional fashions shift, so the costs of a wrong adoption land more on classrooms than on them.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, district_literacy_directors, agenda_setter,
    institutional, biographical, mobile, regional).

% Cognitive psychologists and education researchers producing eye-movement studies, meta-analyses, and longitudinal cohorts on how reading is actually acquired. They hold no material stake in classroom adoption; their findings were for decades dismissed inside the profession as reductionist laboratory work.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, reading_scientists, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes early-literacy instruction around authentic connected text: shared read-alouds, independent reading, writing workshop, and discussion replace fragmented skill drills, building vocabulary, comprehension strategy, and positive reading motivation across a whole classroom at once.
% TRANSFER_FUNCTION: Moves instructional time and attention from explicit decoding practice toward immersive text engagement. The decoding gap that results lands on the students least able to purchase replacement instruction at home; meanwhile professional autonomy, disciplinary identity, and curriculum-program revenue flow to educators, preparation faculties, and publishers.
% ABSENT_VOICES: Struggling five-to-eight-year-olds cannot testify to the instruction they needed until the deficit is already compounded. Parents of struggling readers sat outside every curriculum committee for decades. Reading scientists' evidence entered professional conversations slowly and was reframed as ideology. All three were outside the rooms where adoption and philosophy were decided.
% DISAPPEARANCE_RATIONALE: If the immersion regime vanished overnight, teacher preparation, district adoptions, classroom schedules, and publishing catalogs would reorganize around explicit sequential decoding instruction in the K-2 block, with connected-text work retained but sequenced after basic code mastery. The professional identities anchored to facilitative literacy teaching would be the last element to rearrange.
% FOUNDING_PROBLEM: Mid-twentieth-century reading instruction was dominated by decontextualized basal drills that produced children who could call words without understanding them and came to hate reading. Whole language was built to restore meaning, motivation, and authentic literacy experience.
% FOUNDING_PROBLEM_CORROBORATION: Education historians document the joylessness of the basal era independently of whole-language partisans, and cognitive scientists concede the motivational and comprehension critique had substance. No party outside the benefiting coalition attests that abandoning explicit decoding instruction was the necessary cure; reading scientists and parent advocates actively dispute that step while granting the founding complaint.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.60: the harm is concentrated rather than universal — most middle-class children would learn to read under almost any humane regime, so aggregate damage understates per-seat severity, but for the trapped seats the loss is large and irreversible within the childhood window. Suppression 0.43 reflects institutional-ideological enforcement rather than physical coercion: philosophy statements, adoption lists, blocked phonics materials, and professional ridicule of 'drill and kill'; alternatives never disappeared (decodable texts stayed in print, tutoring markets operated, the research literature stayed public), hence accessibility_collapse 0.30. Resistance 0.72 is among the highest this apparatus records for a pedagogical arrangement: the reading wars, parent litigation, Decoding-Dyslexia-style organizing, investigative journalism, and a wave of state legislation mandating evidence-based instruction. Theater 0.49: read-alouds and writing workshop are functional activity, but the load-bearing doctrinal claim — that skilled readers sample and guess from context, the 'psycholinguistic guessing game' — is contradicted by four decades of eye-movement evidence showing skilled readers decode nearly every word; maintaining that claim in the face of the record is performative. The three measurement series share one grid ({1967, 1979, 1991, 2000, 2010, 2018, 2023}); every tracked metric carries an authored value at every point, so no end-state substitution contaminates early rows. Extractiveness climbs with adoption scale (California's statewide adoption circa 1987-1995 is the inflection), peaks, and dips slightly after state legislation began forcing blended retreat. Suppression_requirement traces an enforcement ratchet — districts policing philosophy hardest precisely when counterevidence peaked — followed by decay as legislatures broke the ratchet; the story deliberately tracks enforcement-capacity change, which is why the series is authored despite the static-scalar default. Claimed_type tangled_rope is asserted independently of these metrics; the engine computes per-seat types and owns any divergence.
 *
 * PERSPECTIVAL GAP:
 *   From the holistic_literacy_educator seat the regime computes as close to a pure good: it preserves professional judgment, honors children's meaning-making, and protects motivation — the extraction is invisible because its payers are in other buildings and other years. From the trapped student seat the same structure operates as the absence of the one thing school uniquely owed them. Among same-level actors, teachers diverge sharply on exit: the identity-locked veteran cannot revise without self-repudiation, while the district director exits by changing jobs. Publishers sit nearest the arbitrage pole — they monetized the philosophy and can monetize its successor, which is why commercial behavior tracks adoption cycles rather than defending the doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d downward: holistic educators (identity subsidy, enforced by their own administration), preparation faculties (prestige and enrollment), publishers (revenue, with arbitrage-grade exit placing them nearest the full-beneficiary end), and affluent print-rich students (net subsidized, minor indirect cost). Victim declarations drive d upward: students without home literacy support and dyslexic students are simultaneously full targets and maximally trapped — compulsory attendance, no choice of classroom, no ability to purchase the missing instruction, and a biological window that closes — so effective extraction amplifies to near the full-target ceiling for those seats. Affluent students carry a mild payer overlay through their unidentified-dyslexic minority. Reading scientists and the excluded parents sit outside the d arithmetic as designed (observer/excluded), though the parents' organized pressure is the visible mechanism behind the post-2018 suppression decay.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two symmetrical misreadings. A pure-rope reading would credit the regime's genuine coordination value — authentic text, motivation, vocabulary — and erase the concentrated, compounding harm to trapped students; a pure-snare reading would invert the error, erasing the fact that the founding complaint (joyless drill producing word-callers) was real and that the regime's text-centered practices deliver real goods to most participants. Mandatrophy is declared resolved because the regime's distinctive mandate — the claim that explicit decoding instruction is unnecessary or harmful — has been superseded by converging evidence and, decisively, by statute in a majority of US states, while pockets of the arrangement persist through institutional inertia, identity investment, and legacy curricula. What survives is increasingly maintained theatrically rather than argued for, which is why the theater series peaks just before the enforcement decay begins.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_omega,
    'This constraint is the whole_language_reading of literacy_acquisition_kernel. What structurally changes if a sibling reading governs the same classrooms?',
    'Compare compiled sibling files (phonics_reading, balanced_literacy_reading, structured_literacy_reading) seat-for-seat: beneficiary/victim sets, directionality profiles, and per-seat computed types under identical structural inputs.',
    'Under phonics_reading or structured_literacy_reading governance, extraction migrates from trapped students to teacher autonomy (mandated retraining, scripted fidelity requirements), and the victim set shrinks to compliance-burdened educators; the tangled_rope asymmetry reverses direction rather than disappearing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_omega, conceptual, 'Committer-frame routing: one kernel, four readings, four constraints; this omega carries the cross-reading structural delta.').

omega_variable(
    matthew_effect_compounding,
    'How much of the regime''s total harm arrives through compounding rather than initial shortfall — do early decoding gaps for disadvantaged entrants widen through grades 3-8 as text demands rise?',
    'Longitudinal cohorts linking K-2 instructional regime to later reading outcomes stratified by home-literacy variables, isolating regime effect from selection.',
    'If compounding is strong, effective extraction on the trapped seats exceeds what the static measure suggests and the case for treating the arrangement''s cost profile as dominated by its victims strengthens further; if weak, the harm is front-loaded and partially recoverable through late remediation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(matthew_effect_compounding, empirical, 'Whether missed early instruction compounds (Matthew effect) or is recoverable later.').

omega_variable(
    motivation_harm_claim_status,
    'Does explicit, systematic decoding instruction actually depress reading motivation and enjoyment, as the reading''s second clause asserts?',
    'Randomized and quasi-experimental comparisons of affective outcomes (engagement, voluntary reading, attitude inventories) between explicit-code and immersion classrooms, controlling for baseline motivation.',
    'If the harm claim is unsupported, the clause functions as cover for identity preservation rather than child protection, the arrangement loses its strongest moral defense, and its persistence reads as inertia plus capture; if supported for some populations, part of the measured extraction is the price of avoiding that harm and the coordination case strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(motivation_harm_claim_status, empirical, 'Status of the motivation-harm premise that distinguishes this reading from its siblings.').

omega_variable(
    identity_fusion_depth,
    'Is the educator-side resistance to revision primarily structural (retraining costs, schedule and adoption constraints) or internalized (professional identity fused with the method, such that explicit phonics feels like bad teaching even after barriers fall)?',
    'Post-legislation trajectory in states that mandated evidence-based instruction: if classroom practice converges quickly once materials and PD are provided, resistance was structural; if practice reverts or evades despite provision, the internalized component dominates.',
    'If substantially internalized, the constraint''s effective suppression is higher than the structural measure indicates — adherents carry it past removal of external enforcement — and decay of the enforcement ratchet overstates the regime''s actual retreat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_depth, empirical, 'Structural versus internalized mechanism behind the regime''s persistence among practitioners.').

omega_variable(
    cs_framing_underdetermination,
    'Is the commitment system here the instructional regime itself, or the legitimacy conjecture layered above it (''reading is acquired naturally, like spoken language'')? The obvious framing names the classroom arrangement; the deeper framing names the Smith-Goodman natural-learning conjecture the arrangement exists to honor.',
    'Test both framings against the drift record: if classification and foreclosure computations differ between the regime-framing and the conjecture-framing, the kernel is the conjecture and the regime is its institutional shadow; signals guiding the choice include which entity absorbs falsification (the conjecture absorbed it; the regime merely retreated).',
    'Under the conjecture-framing, the commitment-system pattern locates at the theoretical layer with the classroom regime as downstream enforcement, shifting where authority erosion registers and potentially recomputing which sibling readings are foreclosed versus merely outcompeted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of the kernel (regime vs. legitimating conjecture) and the classification consequences of choosing between them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 1967, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1967, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(lite_tr_t1979, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1979, 0.26).
narrative_ontology:measurement(lite_tr_t1991, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1991, 0.36).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2010, 0.5).
narrative_ontology:measurement(lite_tr_t2018, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2018, 0.53).
narrative_ontology:measurement(lite_tr_t2023, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2023, 0.49).

% Extraction over time
narrative_ontology:measurement(lite_be_t1967, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1967, 0.32).
narrative_ontology:measurement(lite_be_t1979, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1979, 0.44).
narrative_ontology:measurement(lite_be_t1991, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1991, 0.54).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement(lite_be_t2018, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(lite_be_t2023, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2023, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1967, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1967, 0.22).
narrative_ontology:measurement(lite_su_t1979, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1979, 0.33).
narrative_ontology:measurement(lite_su_t1991, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1991, 0.48).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2000, 0.56).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(lite_su_t2018, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2018, 0.52).
narrative_ontology:measurement(lite_su_t2023, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2023, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how reading should be taught' decomposes under epsilon-invariance into four structurally distinct arrangements — one per kernel reading. This member (whole_language_reading) carries the highest extraction on students and the lowest on teacher autonomy; phonics_reading and structured_literacy_reading invert that profile (extraction concentrated on teacher autonomy via mandated retraining and scripted programs, relief for trapped students); balanced_literacy_reading sits between and inherits contamination risk from whichever sibling dominates its local adoption. The upstream members cite the same empirical record with opposite valence, which is why the family is linked rather than merged: merging would require a single epsilon that no seat's observation supports.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
