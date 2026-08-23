% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__phonics_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Mandated Systematic Grapheme-Phoneme Instruction as Foundational Reading Skill (Phonics Reading)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   A legislative and administrative wave codifies the claim that reading
 *   acquisition requires explicit, systematically sequenced instruction in
 *   grapheme-phoneme correspondence as the foundational skill:
 *   approved-curriculum lists, mandated screening, prohibitions on
 *   cueing-based methods, and recertification of teacher preparation. The
 *   regime solves a real instructional problem — complete code coverage for
 *   children whom unguided discovery fails — while concentrating procurement
 *   on approved publishers, narrowing teacher discretion to implementation,
 *   and reclassifying an entire pedagogical tradition as error. The claim and
 *   the metrics are authored independently: claimed_type is my structural
 *   read of the arrangement (genuine coordination plus asymmetric extraction
 *   held by active enforcement), while the metrics describe the regime's
 *   actual operation; the movement's own settled-science rhetoric is routed
 *   to an omega rather than into the claim.
 *
 * KEY AGENTS:
 *   - struggling_readers: primary subsidized seat (powerless/trapped) — receive the decoding instruction the arrangement guarantees; cannot choose or refuse it
 *   - phonics_curriculum_publishers: receipt-capturing beneficiary (institutional/mobile) — approved-list procurement concentrates the regime's gains on their order books
 *   - classroom_teachers: primary payer seat (moderate/constrained) — discretion and retraining burden flow from them into the mandated sequence
 *   - whole_language_trained_educators: payer seat with locked exit (moderate/identity_locked) — professional identity fused with the displaced pedagogy
 *   - state_education_authorities: agenda setter (institutional/mobile) — codifies and enforces the requirement
 *   - literacy_policy_analysts: analytical observer (analytical/analytical) — sees the full structure, holds no procurement stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.58).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.6).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Mandated Systematic Grapheme-Phoneme Instruction as Foundational Reading Skill (Phonics Reading)").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, 'cca3f37a-b688-4dfd-8e8c-7a28d40d30dc').
narrative_ontology:cs_kernel_codification('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', distributed).
narrative_ontology:cs_authority_grounding('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', expertise).
narrative_ontology:cs_interpretation_layer_present('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc').
narrative_ontology:cs_reading_relation('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', foundational, explicit_systematic_gpc_instruction_necessary).
narrative_ontology:cs_axiom_status(explicit_systematic_gpc_instruction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', explicit_systematic_gpc_instruction_necessary, empirically_contingent).
narrative_ontology:cs_axiom('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', foundational, decoding_primacy_over_contextual_guessing).
narrative_ontology:cs_axiom_status(decoding_primacy_over_contextual_guessing, holdable).
narrative_ontology:cs_axiom_grounding('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', decoding_primacy_over_contextual_guessing, empirically_contingent).
narrative_ontology:cs_axiom('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', secondary, teacher_discretion_subordinate_to_validated_sequence).
narrative_ontology:cs_axiom_status(teacher_discretion_subordinate_to_validated_sequence, holdable).
narrative_ontology:cs_axiom_grounding('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', teacher_discretion_subordinate_to_validated_sequence, instrumental).
narrative_ontology:cs_reference_frame('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', systematic_code_first_instruction_standard).
narrative_ontology:cs_drift_state('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', contemporary_mandate_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('cca3f37a-b688-4dfd-8e8c-7a28d40d30dc', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, reading_science_research_community).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, literacy_advocacy_organizations).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, whole_language_trained_educators).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, school_districts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, typical_progress_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, school_districts).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, teacher_preparation_programs).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, typical_progress_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teacher_preparation_programs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children who do not acquire reliable decoding from print exposure alone — disproportionately children without home literacy advantage and children with dyslexia. Under this regime they receive explicit, sequenced instruction in grapheme-phoneme correspondences from their first year of schooling, with screening to catch anyone falling behind. They cannot choose their school's method or decline instruction; what they receive depends entirely on what adults mandate. The instruction gives them the decoding skill that unguided exposure was failing to produce.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, national).

% Children who would likely crack the alphabetic code under a range of instructional approaches. They receive the same sequenced correspondence instruction as everyone else: they gain a secure decoding base but spend instructional time and text-exposure opportunity on material they largely did not need spelled out. They cannot opt into a faster or literature-centered track; the sequence is uniform across the classroom.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, typical_progress_readers, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, typical_progress_readers, payer).

% Commercial publishers of systematic, scripted phonics programs. State approval lists and mandated adoptions concentrate sales onto programs matching the codified scope-and-sequence; districts must buy from the list. They collect the procurement receipts the mandate regime generates, invest in aligning products to each state's codified sequence, and can shift product lines if requirements change.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, phonics_curriculum_publishers, beneficiary,
    institutional, biographical, mobile, national).

% Cognitive psychologists, education researchers, and dyslexia scientists whose research program established the case for explicit code instruction. Mandates validate the program, drive funding, consulting, and professional-development demand, and convert findings into procurement criteria. Standing is bound to the framework's continued authority; dissenters within the field report a colder reception for work questioning the strength or scope of the effect.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, reading_science_research_community, beneficiary,
    organized, generational, constrained, global).

% Parent-led and professional advocacy organizations that campaigned for science-of-reading legislation. The mandate wave is their legislative product; they collect agenda power, funding, and a durable policy role monitoring implementation and shaping approved lists, and they organize testimony in every statehouse considering a bill.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, literacy_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Legislatures and education agencies that codify the requirement: approved-curriculum lists, mandated screening, prohibitions on cueing-based methods, and recertification rules for preparation programs. They set the scope-and-sequence requirements publishers must match, respond to advocacy pressure and assessment outcomes, and bear the political cost of implementation failures.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, state_education_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Working teachers who must deliver the mandated sequence with prescribed routines and pacing. Their discretion over method, sequence, and materials narrows to implementation choices; many must complete retraining on the code itself, which their own schooling never taught them. Unions give them voice in negotiations, but the statutory sequence binds regardless; their practical exits are compliance, transferring to unregulated settings, or leaving teaching.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Educators whose degrees, certifications, published work, and professional self-concept were built on whole-language and balanced-literacy pedagogy. The regime reclassifies their expertise as a documented error: their methods are prohibited in a growing list of states, their preparation programs must be recertified, and continued practice means retraining into the framework that displaced them. Leaving the framework means dismantling a career's professional identity; staying without believing means performing fidelity.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_trained_educators, payer,
    moderate, biographical, identity_locked, national).

% District administrators who must procure from the approved list, retrain staff, re-schedule intervention blocks, and report screening data on state timelines. They bear the up-front costs of the transition; their students' outcomes improve where implementation is faithful. They cannot decline the mandate, and approved-list rules remove their historical discretion to assemble curricula from competing vendors.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, school_districts, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, school_districts, beneficiary).

% University education schools that must restructure coursework around systematic code instruction to keep state approval and keep their graduates licensure-eligible. Faculty trained in whole-language traditions must retool or recuse from reading-methods courses. Programs also collect tuition for the new retraining pipelines, so compliance carries a revenue offset.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teacher_preparation_programs, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, teacher_preparation_programs, beneficiary).

% Major publishers of balanced-literacy and leveled-reading curricula. State approval machinery delists their flagship products and districts cancel contracts as mandates take effect. They would compete for the procurement on price and design if admitted to the lists; their exclusion is maintained by the same codified requirements that define the approved market.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, balanced_literacy_publishers, excluded,
    powerful, biographical, mobile, national).

% Independent researchers, methodologists, and policy analysts — including critics of the phonics consensus — who examine effect sizes, implementation fidelity, and procurement economics. They hold no procurement stake and no mandate authority; their leverage is publication and testimony, and both sides of the instructional contest cite or contest their work.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, literacy_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__phonics_reading, phonics_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine instructional-coordination problem: English orthography is an opaque alphabetic code that a large minority of children do not fully induce from exposure alone. A shared, explicit scope-and-sequence of grapheme-phoneme correspondences ensures complete code coverage, prevents instructional gaps across classrooms and grade transitions, and gives every teacher — including those never taught the code themselves — a usable, validated sequence.
% TRANSFER_FUNCTION: Moves instructional authority (discretion over method, sequence, and materials) from classroom teachers and districts to state-codified program designs and their publishers; moves procurement funds from district budgets to approved-list vendors; moves retraining costs onto the existing educator workforce and preparation programs; delivers decoding skill to students, disproportionately to those who would not otherwise acquire it.
% ABSENT_VOICES: Whole-language and balanced-literacy practitioners and theorists are progressively excluded from curriculum approval, preparation-program recertification, and legislative hearings framed as settled science; dissenting reading researchers report a colder reception in venues dominated by the phonics consensus; balanced-literacy publishers are delisted rather than consulted; and children — the nominal beneficiaries — hold no seat anywhere in the approval process.
% DISAPPEARANCE_RATIONALE: If the mandate regime vanished overnight, instructional practice would re-fragment along the preparation traditions of whoever staffs each classroom; struggling readers' acquisition would again depend on their teacher's training luck; approved-list procurement would dissolve and publishers would compete for voluntary adoption; the advocacy infrastructure and the retraining industry would lose their policy anchor. The coordination function would partially survive as voluntary practice where teachers have internalized the code, but the guarantee — that every child receives complete code instruction regardless of school or teacher — would disappear.
% FOUNDING_PROBLEM: Wide-scale reading failure: for decades a large fraction of children failed to read proficiently under whole-language and balanced-literacy regimes that left grapheme-phoneme instruction to incidental discovery, with failure concentrated among children without home-literary advantage. The founding problem was to guarantee that every child receives explicit, complete code instruction.
% FOUNDING_PROBLEM_CORROBORATION: NAEP, PIRLS, and PISA proficiency series corroborate persistent reading failure independent of the advocacy movement; clinical and epidemiological dyslexia-prevalence literature attests the problem from outside the benefiting parties; district-level outcome records corroborate both the persistence of the problem and the disputed size of the instructional effect. No corroboration comes from within the beneficiary set alone.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 because the regime's costs are real and asymmetric — narrowed teacher discretion, mandated procurement, retraining burdens, delisted alternatives — while its largest benefit flows to a class (struggling readers) that collects no procurement receipts. Suppression at 0.60 is structural, not internalized: approved lists, method prohibitions, and certification gates do the coercive work; a dedicated omega covers the internalized residue in educator identity lock. Theater at 0.25 reflects a mostly functional instructional core with a growing compliance layer (phonics-washed repackagings, screening-check ritual). Accessibility collapse at 0.50: alternatives persist (private, home, unlisted curricula) but the approval machinery steadily closes the public-school route to them. Resistance at 0.60: teacher pushback, dissenting researchers, and holdout jurisdictions are real and organized. The temporal series run on one shared grid (t = 0, 5, 10, 15, 20, 25 for all three metrics) and are monotonic rather than oscillatory — an enforcement ratchet, not a cycle; suppression_requirement is authored because the story specifically tracks enforcement machinery being built (lists, bans, recertification), not merely extraction shifting. Suppression is authored as a raw structural property; the engine alone scales extractiveness by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is fidelity: a legislature translating replicated findings into a guarantee for children. From the classroom-teacher seat the same structure is scripted labor: the sequence arrives as pacing charts and prescribed routines. From the whole-language educator's seat it is dispossession: the arrangement declares a career's expertise a documented error. From the struggling reader's seat it is the instruction that unlocks print. Same codified sequence; four different constraints. Per-seat classifications computed from the power and exit atoms should diverge exactly along this line, with the payer seats — and especially the identity-locked one — computing the most extractive experience of the arrangement. Note also the same-level contrast: classroom_teachers and whole_language_trained_educators hold the same nominal power atom but differ in exit options (constrained versus identity_locked), differentiated not by global standing but by pedagogical tradition.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers sit at the beneficiary end: the arrangement subsidizes them with skill they would not otherwise acquire, and their trapped exit makes the subsidy total. Publishers sit near the beneficiary end and are simultaneously the receipt seat — mandated procurement lands the regime's gains on them, which is why gain_flow names them; receipt and benefit are distinct facts and both hold here. Teachers and whole-language-trained educators sit at the target end: the arrangement takes discretion, method autonomy, and (for the second group) professional standing, and their constrained or identity-locked exits push their effective extraction toward the full-target end. Districts sit mid-range: they pay mandated costs but their students collect part of the benefit. Authorities sit near-symmetric: they spend enforcement effort rather than collect its product. The engine derives these positions from the beneficiary/victim declarations and exit atoms; no directionality override was needed because the role/exit pairs differentiate the seats cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass reading failure under discovery-oriented instruction — is live and corroborated from outside the benefiting parties, so no mandatrophy is declared. The classification work the type does here is double-edged: reading the arrangement as pure extraction (the whole-language seat's temptation) would erase the genuine coordination function — complete code coverage that struggling readers demonstrably need; reading it as pure coordination (the movement's own framing) would excuse procurement capture and the narrowing of professional discretion. The tangled-rope structure holds both: the coordination function is real, and the extraction riding on it requires separate justification. The mandatrophy trajectory to watch: if the necessity-scope omega resolves differentially — only a minority of readers needing systematic instruction — the universal mandate's enforcement would outlive its warrant and drift toward screening compliance without instructional necessity for most children.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settled_science_vs_constructed_mandate,
    'Is the phonics prescription a settled fact about how humans acquire reading (a natural law of instruction) or a constructed policy arrangement whose settled-science framing concentrates benefits on identifiable agents — approved-list publishers, the research-advocacy complex, retraining providers?',
    'Adversarial re-analysis of the intervention literature (the NRP/Ehri meta-analytic tradition against the Bowers 2020 and Wyse & Bradbury critiques): robustness of systematic-phonics effects across designs, populations, and comparison conditions, cross-checked against the interest alignment of the claim''s institutional carriers.',
    'If natural law, mandate enforcement is fidelity to cognitive science and the measured extraction is coordination cost; if constructed, the regime owes justification for procurement capture and discretion-narrowing independent of the science, and false-summit machinery applies to the movement''s mountain rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settled_science_vs_constructed_mandate, empirical, 'Natural-law versus constructed-arrangement status of the instructional necessity claim.').

omega_variable(
    necessity_scope_ambiguity,
    'Does the necessity claim hold universally — every reader needs systematic sequenced grapheme-phoneme instruction — or differentially, with a struggling minority needing it and typical readers acquiring decoding under a range of methods?',
    'Moderator analyses of instructional trials by baseline decoding risk; differential-response studies tracking whether effects for typical readers approach zero.',
    'A differential resolution shrinks the coordination function to targeted intervention, converts universal-mandate instruction time for typical readers into opportunity cost, and weakens this reading''s foreclosure of the whole-language sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_scope_ambiguity, empirical, 'Universal versus differential scope of the instructional necessity claim.').

omega_variable(
    kernel_reading_epsilon_divergence,
    'This story is one reading of the reading_acquisition_mechanism kernel; what structural delta would the sibling readings author over the same mandate regime, and where exactly do the readings'' ε assessments part ways?',
    'Cross-reading comparison of the sibling stories: the whole-language reading authors high ε for the mandate as suppression of meaning-based pedagogy; the balanced-literacy reading authors intermediate ε as displacement of integration; this reading authors moderate ε as procurement capture plus narrowed discretion. The parting of ways sits in the necessity claim''s causal scope and in whether mandate enforcement is legitimacy or suppression.',
    'Adopting a sibling''s structural delta re-partitions this regime''s victim and beneficiary sets (teachers as coerced labor versus as fidelity implementers) and shifts per-seat classifications across the whole constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_epsilon_divergence, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings would re-partition the same regime''s ε.').

omega_variable(
    educator_identity_lock_mechanism,
    'Is whole-language-trained educators'' locked exit structural (certification gates, retraining costs, labor-market position) or internalized (professional self-concept fused with the displaced pedagogy)?',
    'Post-retraining trajectory study: educators completing structured-literacy certification who report restored efficacy versus those complying performatively or leaving the profession; persistence of resistance after mandate repeal would indicate internalization.',
    'If internalized, the coercive force outlasts the mandates and repeal would not quickly release the arrangement; if structural, repeal re-opens method choice almost immediately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(educator_identity_lock_mechanism, empirical, 'Structural versus internalized mechanism of educator exit-lock.').

omega_variable(
    procurement_capture_vs_program_cost,
    'What share of approved-program pricing and adoption reflects genuine program development and validation cost, versus rents collected because the approved list removes district choice?',
    'Cost-structure comparison of approved curricula against comparable unmandated materials; procurement pricing before and after states open or close approved lists.',
    'A high rent share confirms the publisher seat as the regime''s capturer and strengthens the extraction reading; a low share reframes publisher gain as compensation for producing a coordination good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procurement_capture_vs_program_cost, empirical, 'Rent versus cost composition of mandated procurement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phonics_reading_tr_t0, reading_acquisition_mechanism__phonics_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(phonics_reading_tr_t5, reading_acquisition_mechanism__phonics_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(phonics_reading_tr_t10, reading_acquisition_mechanism__phonics_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(phonics_reading_tr_t15, reading_acquisition_mechanism__phonics_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(phonics_reading_tr_t20, reading_acquisition_mechanism__phonics_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(phonics_reading_tr_t25, reading_acquisition_mechanism__phonics_reading, theater_ratio, 25, 0.25).

% Extraction over time
narrative_ontology:measurement(phonics_reading_be_t0, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(phonics_reading_be_t5, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 5, 0.39).
narrative_ontology:measurement(phonics_reading_be_t10, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(phonics_reading_be_t15, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement(phonics_reading_be_t20, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(phonics_reading_be_t25, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(phonics_reading_su_t0, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(phonics_reading_su_t5, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement(phonics_reading_su_t10, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(phonics_reading_su_t15, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(phonics_reading_su_t20, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(phonics_reading_su_t25, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 25, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial 'reading wars' label conflates three structurally distinct claims about one kernel (how reading acquisition works). This file is the phonics reading only: the necessity-and-primacy claim operating as a mandate regime, with its own ε, beneficiaries, and victims. The whole-language reading (implicit emergence; authors high ε for the phonics mandate as suppression of meaning-based pedagogy) and the balanced-literacy reading (integration; intermediate ε) are separate stories linked through these network edges. The phonics reading's necessity premise forecloses the whole-language premise within any single framework, while coexisting with the balanced-literacy premise; the mandate wave exerts structural pressure on the balanced-literacy sibling without resolving the dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
