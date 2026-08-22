% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: literacy_acquisition_kernel__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Regime
 *   domain: educational/psychological/pedagogical
 *
 * SUMMARY:
 *   This story instantiates one reading of the literacy_acquisition_kernel:
 *   the whole-language arrangement, in which reading acquisition is held to
 *   emerge from meaningful engagement with connected text, phonics skill to
 *   develop naturally through exposure and context, and explicit decoding
 *   instruction to be unnecessary and potentially harmful to motivation. As
 *   institutionalized from the mid-1970s through the 2010s — in
 *   teacher-education methods courses, state curriculum frameworks
 *   (California's 1987 literature-based framework was the emblematic
 *   mandate), adopted basals and leveled-text programs, Reading Recovery, and
 *   professional-development networks — the arrangement solved a real
 *   instructional problem (the comprehension and motivation failures of
 *   drill-dominated mid-century basals) while imposing its steepest costs on
 *   the students least able to compensate: children without print-rich homes,
 *   taught to guess from pictures, first letters, and context rather than
 *   decode, and dyslexic children, who require exactly the explicit
 *   instruction the arrangement excluded. The epsilon referent is the
 *   whole-language arrangement itself, assessed by the reading's own lights:
 *   the movement declared every child a natural reader, and the arrangement's
 *   failure to deliver reading to children without home substitutes is
 *   measured against that declared commitment. Constraint-family note
 *   (epsilon decomposition): the sibling readings author different
 *   arrangements with different epsilon — the phonics reading authors the
 *   drill-era basals, whose paying seat is the motivated reader crushed by
 *   decontextualized drill; the structured-literacy reading authors the
 *   Orton-Gillingham tradition; the balanced-literacy reading authors the
 *   hybrid that absorbed this arrangement's practices after 2000. This file
 *   authors only the meaning-first arrangement, whose costs concentrate on
 *   the unsupported-student seat while the teacher seat experiences near-zero
 *   cost (professional judgment preserved). KEY AGENTS (by structural
 *   relationship): - elementary_classroom_teachers: primary beneficiary
 *   (organized/identity_locked) — autonomy and professional identity
 *   preserved; also bears blame and retraining costs -
 *   teacher_education_faculties: agenda setter (institutional/constrained) —
 *   reproduces the framework through methods courses and accreditation -
 *   literacy_curriculum_publishers: secondary beneficiary
 *   (institutional/arbitrage) — revenue tracks the adopted framework; receipt
 *   seat for the regime's monetary proceeds -
 *   students_without_home_literacy_support: primary target
 *   (powerless/trapped) — taught to guess rather than decode; failure
 *   recorded as their deficit - dyslexic_students: primary target
 *   (powerless/trapped) — denied the explicit instruction their reading
 *   development requires - parents_of_struggling_readers: payer and excluded
 *   voice (moderate/constrained) — absorbs private remediation costs;
 *   objections dismissed - reading_scientists: excluded
 *   (organized/analytical) — evidence barred from adoption decisions for
 *   decades - state_education_agencies: agenda setter
 *   (institutional/constrained) — adopted the frameworks, later legislated
 *   against them - print_rich_home_students: incidental beneficiary
 *   (moderate/constrained) — home supplies what the classroom omits -
 *   literacy_policy_analysts: analytical observer — sees the full
 *   reproduction loop
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.62).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.45).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Acquisition Regime").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational/psychological/pedagogical").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, 'd3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3').
narrative_ontology:cs_kernel_codification('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', formalized).
narrative_ontology:cs_authority_grounding('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', practice).
narrative_ontology:cs_interpretation_layer_present('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3').
narrative_ontology:cs_reading_relation('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', literacy_acquisition_kernel__balanced_literacy_reading, forecloses).
narrative_ontology:cs_axiom('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', foundational, reading_acquisition_is_natural).
narrative_ontology:cs_axiom_status(reading_acquisition_is_natural, holdable).
narrative_ontology:cs_axiom_grounding('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', reading_acquisition_is_natural, empirically_contingent).
narrative_ontology:cs_axiom('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', foundational, explicit_decoding_instruction_harmful).
narrative_ontology:cs_axiom_status(explicit_decoding_instruction_harmful, holdable).
narrative_ontology:cs_axiom_grounding('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', explicit_decoding_instruction_harmful, empirically_contingent).
narrative_ontology:cs_axiom('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', secondary, practitioner_knowledge_over_lab_evidence).
narrative_ontology:cs_axiom_status(practitioner_knowledge_over_lab_evidence, holdable).
narrative_ontology:cs_axiom_grounding('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', practitioner_knowledge_over_lab_evidence, conventional).
narrative_ontology:cs_reference_frame('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', meaning_first_natural_learning).
narrative_ontology:cs_drift_state('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', post_science_of_reading_legislation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('d3c65ccc-1f8d-46ae-ad1a-ca34f0dbfff3', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, elementary_classroom_teachers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teacher_education_faculties).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, literacy_curriculum_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, print_rich_home_students).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, dyslexic_students).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, elementary_classroom_teachers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, emergent_literacy_hypothesis).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, psycholinguistic_guessing_theory).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, three_cueing_system).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, natural_learning_theory).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, constructivist_learning_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Plan literacy blocks around read-alouds, shared big books, guided reading groups, and independent choice reading; select texts by interest and level rather than by phonetic sequence. The arrangement preserves their judgment over how reading is taught and anchors a professional identity as facilitators of natural readers rather than deliverers of scripted skills sequences. They also absorb downstream costs: when children fail to decode, teachers face the blame, and mid-career retraining mandates now require unlearning practices their careers were built on.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, elementary_classroom_teachers, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, elementary_classroom_teachers, payer).

% Train the workforce: methods courses teach the meaning-first framework, cueing strategies, and leveled-text practice, and accreditation closes the loop that reproduces the framework each year. Their curricula, faculty lines, and textbook adoptions are built around it; revising means overhauling courses, admitting decades of error to alumni and districts, and re-credentialing the profession.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teacher_education_faculties, agenda_setter,
    institutional, generational, constrained, national).

% Supply the adopted materials — leveled book sets, big books, author-led professional-development institutes and workshop franchises — and their revenue tracks whichever framework districts adopt. During the regime's ascendancy their catalogs, conference presence, and author contracts were built on it; retooling is expensive, so the arrangement's persistence protected their product lines, and they pivot when adoption markets shift.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, literacy_curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, continental).

% Arrive at school having been read to daily, with alphabet knowledge, vocabulary, and print routines already in place; immersion in meaningful text is enough for them to crack the code, and they experience the classroom as story-rich and motivating. If progress stalls, their families can purchase tutoring that substitutes for what the school does not teach.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, print_rich_home_students, beneficiary,
    moderate, biographical, constrained, local).

% Arrive without alphabet knowledge, book-handling routines, or daily read-alouds, and are taught to guess words from pictures, first letters, and context instead of sounding them out. Progress depends on background the school does not build; when guessing fails they are flagged as slow or unmotivated and the deficit is recorded as theirs. They cannot leave the classroom, choose a different method, or wait until adulthood to remediate cheaply.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support, payer,
    powerless, biographical, trapped, local).

% Need explicit, systematic, cumulative phoneme-grapheme instruction to build the decoding circuitry that does not assemble incidentally for them; the classroom's rejection of explicit instruction leaves them guessing, failing, and internalizing reading failure as personal defect, typically identified late or never, with effective remediation available mainly to families who can pay private clinicians.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Watch a child guess and stall, are told to give it time, and eventually purchase private assessment and tutoring out of pocket. When they question classroom method they are dismissed as anxious or as pushing drill-and-kill, and their evidence — the child himself — is read as the exception that proves the method sound.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers, excluded).

% Produce the convergent evidence — panel meta-analyses, eye-tracking studies, neuroimaging, longitudinal instructional trials — showing that decoding is assembled rather than absorbed and that explicit instruction is the active ingredient for the children the arrangement fails. Their findings were barred from adoption decisions as laboratory artifacts, their motives questioned as reductionist, and their policy influence arrived only decades later through legislation.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, reading_scientists, excluded,
    organized, generational, analytical, continental).

% Adopt curriculum frameworks, certify instructional materials, and fund early-reading interventions. They mandated literature-based frameworks at the regime's peak, reversed course after their own state's reading scores collapsed, and now legislate evidence-based instruction and ban cueing practices — the seat that pays the political cost of the outcomes and holds the legal power to end the arrangement.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, state_education_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Track adoption patterns, NAEP cohorts, state legislative waves, and publisher pivots. They can see the full loop — ed schools training teachers, districts adopting materials, outcomes feeding back into legislation — and hold no stake in any framework's success.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, literacy_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__whole_language_reading, literacy_curriculum_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orients early-literacy instruction around meaningful connected text: read-alouds, shared big books, guided reading, independent choice reading, and writing — building vocabulary, comprehension, and motivation through use rather than decomposition, and giving teachers a coherent professional framework that preserves classroom judgment. It also solved a real historical problem: the joyless, drill-heavy skills sequences that produced children who could call words but not make sense of them.
% TRANSFER_FUNCTION: Moves instructional time from explicit decoding instruction to text engagement; moves instructional authority from curriculum designers and researchers to classroom teachers and the teacher-education complex; and moves the burden of acquiring decoding onto the home — families with print-rich environments supply what the school does not teach, while families without such environments cannot.
% ABSENT_VOICES: Reading scientists were structurally absent from adoption decisions — their convergent evidence was dismissed as laboratory artifact and their advocacy as reductionism. Parents of struggling readers were present but voiceless: objections reframed as anxiety, the child's failure read as the child's deficit. Dyslexic children themselves had no seat at all; the arrangement recorded its own failure as their lateness.
% DISAPPEARANCE_RATIONALE: The workforce, materials market, and training pipeline are built around the arrangement: if it vanished overnight, early-literacy classrooms would reorganize around explicit phonics routines within a few school years (as they are doing under state mandates), publishers would pivot catalogs, methods courses would retool, and the leveled-text and cueing infrastructure would lose its institutional host. Schooling is not neutral to this arrangement's absence.
% FOUNDING_PROBLEM: Mid-century skills-based instruction — phonics-heavy, decontextualized drill on basals — produced children who could decode words but comprehended poorly and disliked reading; the movement was built to restore meaning, motivation, and the teacher's professional authority over method.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the movement: Jeanne Chall's 1967 Learning to Read: The Great Debate, commissioned independently of the movement, documented the comprehension costs of skills-dominated instruction; teacher memoirs and district records of the drill era attest the motivation failure; and structured-literacy proponents — this reading's opponents — concede that meaning and motivation matter. No source outside the movement corroborates the stronger founding claim that explicit decoding instruction is unnecessary or harmful; that claim is attested only by the movement's own authorities and their institutional descendants.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.62 (interval end) because the arrangement's costs are severe and concentrated: for the unsupported-student and dyslexic seats it withholds the active ingredient of reading acquisition precisely where no home substitute exists, while imposing near-zero cost on the teacher seat, whose judgment and identity the arrangement preserves. Suppression is 0.45 and falling: enforcement ran through teacher-education gatekeeping, adoption committees, and professional-development culture that framed explicit instruction as drill-and-kill, and is now being dismantled by statute in a majority of US states while ed-school reproduction persists. Suppression is authored as a raw structural property, unscaled; extractiveness is the quantity the engine scales by directionality and scope. Theater is 0.30: read-alouds, shared reading, and writing are genuinely functional; the performative share grew as the regime became defensive — cueing mini-lessons and authentic-assessment rituals maintained in the face of accumulating disconfirmation. The temporal series runs on one shared grid and shows a two-wave pattern rather than smooth drift: institutional boom (California's 1987 mandate), local crash and reversal (California 1995-96, absorbed nationally as the 0.66-to-0.68 plateau as balanced-literacy re-accumulation offset the reversal), and legislative correction (Mississippi 2013 onward, cueing bans 2019-2025) pulling extractiveness and enforcement down at interval end. Claim and metrics are independent: the claimed type is what the structure shows — a genuine coordination function (meaning-first engagement, teacher professionalism) fused with asymmetric extraction through the same arrangement — while the metrics describe the operation as the record shows it. The victim seats are individually powerless and trapped; their coalition route ran through parents, reading scientists, and legislators, which is the route that finally moved it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats the arrangement is professional liberation: a coherent framework that trusts teachers' knowledge of children over laboratory abstractions, and classrooms that are story-rich and humane. From the trapped paying seats the same arrangement is the denial of instruction: a child who guesses and stalls is told the method is sound and the child is not ready. Same-level divergence: elementary teachers and reading scientists hold the same nominal power (organized professionals) and sit at opposite structural positions — teachers inside the arrangement with identity-fused exit, scientists outside it with analytical exit and no adoption leverage for three decades. The identity lock on the teacher seat is professional and epistemic at once: careers built on the method, self-concept as facilitator of natural readers, and the conviction that classroom observation outranks experimental evidence; where the identity frame has broken under mandate (Mississippi-style retraining), practice has changed quickly — which is itself evidence about the lock's composition. The engine computes per-seat classifications from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: elementary_classroom_teachers (autonomy and professional identity preserved — the arrangement's declared core benefit), teacher_education_faculties (the reproduction loop runs through their methods courses), literacy_curriculum_publishers (catalogs and professional-development revenue tracked the adopted framework), print_rich_home_students (home supplies what the classroom omits, and they receive the arrangement's genuine benefits). Victims: students_without_home_literacy_support and dyslexic_students (bear the full cost — no decoding instruction, no substitute at home, no exit), parents_of_struggling_readers (absorb private remediation costs and dismissed objections). Directionality follows the declarations: teacher and faculty seats near the beneficiary end (teachers damped by identity_locked exit, which fuses them to the arrangement), the publisher seat near the beneficiary end with arbitrage exit, the student seats near the full-target end (trapped, powerless), the parent seat high-target. No directionality overrides are authored: the beneficiary and victim declarations plus exit options already place every seat correctly. On the receipt surface, the gains land demonstrably on the publisher seat (regime-contingent catalog and institute revenue), which is why gain_flow names it rather than asserting diffuse receipt; the fixing cost for the seat that could act (state agencies) is prohibitive relative to diffuse future-student benefits — workforce retraining, materials replacement, ed-school overhaul, and professional resistance — which is why correction took legislative waves across decades.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — drill-dominated basals producing word-callers with poor comprehension and children who hated reading — was real and independently documented; its live residue (meaning, motivation, and rich text matter) was absorbed by the balanced and structured-literacy siblings rather than solved by this arrangement's exclusivity claim. The mandate as authored here — explicit decoding instruction is unnecessary and harmful — has outlived its evidentiary basis, which is why founding_problem_status is contested and the mismatch consumer should read this story closely: the arrangement persists in ed-school reproduction and residual classroom practice while its founding claim is dead in the literature and dying in law. The classification work the type does: calling the arrangement a snare would erase the genuine coordination function (text engagement, teacher professionalism) and misdirect reform toward banning literature rather than adding explicit instruction; calling it a rope would erase the costs concentrated on children without home substitutes. Tangled rope holds both, and the receipt surface (publisher capture, prohibitive fixing cost) explains the decades-long persistence against accumulating disconfirmation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the whole_language_reading instantiation of the literacy_acquisition_kernel; what structural elements change under the sibling readings (phonics_reading, structured_literacy_reading, balanced_literacy_reading), and where exactly is the disagreement located?',
    'Author the sibling stories and compare victim and beneficiary sets and epsilon: the phonics and structured-literacy readings move dyslexic and unsupported students from the paying seats into served seats and relocate the open question to drill-era motivation costs; the balanced-literacy reading splits the difference with its own mixed seat structure.',
    'If a sibling reading is adopted as the operative framework, this constraint''s victim set (students without home literacy support, dyslexic students) converts largely into beneficiaries, its beneficiary set (teacher professional identity, publisher revenue) contracts, and its classification shifts toward rope or scaffold. The disagreement is located in one structural element — whether decoding must be explicitly taught — from which every seat asymmetry in this story follows.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of the literacy-acquisition kernel; sibling readings would restructure the victim and beneficiary sets.').

omega_variable(
    natural_acquisition_empirical_status,
    'Is reading acquisition actually a natural process that emerges from exposure to meaningful text (the reading''s foundational axiom), or a biologically secondary skill that must be assembled through explicit instruction?',
    'Convergent evidence already largely resolves it: cross-linguistic neuroimaging of the visual word-form area, instructional randomized trials meta-analyzed by the National Reading Panel, and population natural experiments (California 1987-1996, Mississippi from 2013, England''s phonics screening check). Residual uncertainty sits at the boundary: how much oral-language and print exposure substitutes for instruction at the margins.',
    'If acquisition is natural, the arrangement''s costs on unsupported students are misattributed — their failure would be home-driven under any regime — and the arrangement sits closer to rope. If acquisition is assembled, the cost is structural: the arrangement withholds the active ingredient precisely from students who cannot substitute for it at home, and the tangled_rope reading is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_acquisition_empirical_status, empirical, 'Whether the reading''s natural-learning axiom is empirically true or false.').

omega_variable(
    home_vs_instruction_attribution,
    'How much of the measured outcome gap for students without print-rich homes is caused by the instructional arrangement itself, versus background disadvantage that would depress reading under any pedagogy?',
    'Within-SES natural experiments where the instructional regime changed and demographics held: Mississippi''s post-2013 gains, California''s post-1996 reversal, England''s 2006 national phonics rollout — comparing disadvantaged-cohort trajectories before and after regime change.',
    'A large regime effect confirms the cost is causal and concentrated on the paying student seat; a null effect would relocate the harm to background inequality and downgrade this constraint''s epsilon substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(home_vs_instruction_attribution, empirical, 'Causal share of instructional regime versus background disadvantage in the student seat''s outcomes.').

omega_variable(
    teacher_identity_lock_persistence,
    'Does the arrangement persist through sincere conviction (the reading is genuinely believed) or through identity-protective cognition (evidence is discounted because admitting error threatens professional identity)?',
    'Practice-change data under mandate: post-legislation surveys and classroom observation of whether retrained teachers adopt explicit routines; persistence of cueing practices in classrooms where materials have already been replaced; districts that changed without mandates.',
    'If identity-protective, suppression is substantially internalized and the arrangement outlives its institutional enforcement — repeal does not end it. If conviction-based, evidence-aligned retraining converts practice quickly and the residual regime decays on schedule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_identity_lock_persistence, empirical, 'Conviction versus identity-protective cognition as the persistence mechanism.').

omega_variable(
    motivation_cost_of_explicit_instruction,
    'Is the motivation harm the reading attributes to explicit decoding instruction real and material, or small and transient?',
    'Randomized and longitudinal studies measuring decoding outcomes alongside reading interest and self-concept under explicit versus meaning-first conditions; classroom motivation measures in states mid-transition.',
    'If the motivation cost is material and persistent, part of the arrangement''s coordination story survives and the balance shifts toward rope; if small or transient, the harm premise collapses and the arrangement''s coordination claim reduces to generic text engagement available under any regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(motivation_cost_of_explicit_instruction, empirical, 'Whether explicit instruction carries the motivation cost the reading claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(whole_language_reading_tr_t0, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(whole_language_reading_tr_t10, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(whole_language_reading_tr_t20, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(whole_language_reading_tr_t30, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(whole_language_reading_tr_t40, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(whole_language_reading_tr_t50, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(whole_language_reading_be_t0, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(whole_language_reading_be_t10, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(whole_language_reading_be_t20, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(whole_language_reading_be_t30, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(whole_language_reading_be_t40, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(whole_language_reading_be_t50, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(whole_language_reading_su_t0, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(whole_language_reading_su_t10, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(whole_language_reading_su_t20, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(whole_language_reading_su_t30, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(whole_language_reading_su_t40, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(whole_language_reading_su_t50, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, structured_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the reading wars' conflates four structurally distinct arrangements of one kernel (literacy_acquisition_kernel), decomposed per the epsilon-invariance principle into separate stories: whole_language_reading (this file; meaning-first, epsilon 0.62 concentrated on unsupported students), phonics_reading (decoding-first basals; different victim seat — the motivated reader under drill), structured_literacy_reading (Orton-Gillingham universalism), and balanced_literacy_reading (the post-2000 hybrid that absorbed this arrangement's practices). The upstream empirical question (whether decoding must be explicitly taught) drives every downstream seat asymmetry; each family member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
