% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__structured_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Universal Mandate (Orton-Gillingham Reading)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the structured_literacy_reading of the
 *   literacy_acquisition_kernel: the claim that reading acquisition requires
 *   explicit, systematic, cumulative instruction across five components —
 *   phonological awareness, phonics, fluency, vocabulary, and comprehension —
 *   designed originally for students with dyslexia in the Orton-Gillingham
 *   tradition and now asserted to apply universally. The standing arrangement
 *   under contest (the epsilon referent) is the current mandate apparatus:
 *   state science-of-reading laws requiring teacher training in structured
 *   literacy, approved-curriculum adoptions, universal screening mandates,
 *   and statutory bans on competing approaches. The claim and metrics are
 *   authored independently: the claimed type is tangled_rope — genuine
 *   coordination function plus asymmetric extraction — while the metrics
 *   describe the arrangement's actual operation at interval end. Family note
 *   (epsilon decomposition): the sibling readings are separate constraints
 *   linked via network.affects_constraints. Phonics_reading carries a
 *   narrower epsilon referent (decoding-first sequencing, no certification
 *   economy); whole_language_reading's referent is the immersion claim now
 *   under statutory prohibition; balanced_literacy_reading's is the
 *   complementarity claim being reorganized under mandate. This reading's
 *   epsilon covers the full mandate apparatus because the certification and
 *   curriculum economy rides on its universal-extension claim specifically.
 *   The contest over whether this is a fourth reading or a phonics variant is
 *   carried as omega reading_kernel_variant_status, not adjudicated here.
 *
 * KEY AGENTS:
 *   - students_with_learning_disabilities: Primary beneficiary (powerless/trapped) — the intervention's designed target; largest measured gains; no exit from compulsory instruction
 *   - general_education_teachers: Primary payer (organized/constrained) — bear mandated training hours, program implementation, and scripted-fidelity autonomy loss
 *   - structured_literacy_certification_bodies: Secondary beneficiary (institutional/arbitrage) — accredit training and certify practitioners; each mandate expands their fee base
 *   - proprietary_curriculum_publishers: Secondary beneficiary (institutional/arbitrage) — hold multi-year district contracts for mandated programs; largest single capturer of the directed spending
 *   - state_legislatures: Agenda setter (institutional/arbitrage) — enact the training mandates, curriculum approvals, and approach bans; collect political credit
 *   - local_school_districts: Payer (organized/constrained) — bear training, materials, and compliance costs under state law
 *   - dyslexia_advocacy_organizations: Beneficiary and agenda setter (organized/mobile) — mobilized the legislative wave; gain legitimacy, membership, and funding from the mandates they lobbied for
 *   - colleges_of_education: Payer (institutional/constrained) — forced overhaul of teacher-preparation curricula built on the approaches the statutes displace
 *   - reading_research_community: Analytical observer — holds the evidence base; attests the intervention core while contesting the universal extension
 *   - typically_developing_readers: Beneficiary with payer secondary role (powerless/trapped) — receive the universal instruction; small early-grades gains, contested opportunity cost
 *   - struggling_readers: Beneficiary (powerless/trapped) — non-dyslexic struggling readers; large gains from explicit instruction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.65).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Universal Mandate (Orton-Gillingham Reading)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, '91b80288-914d-43e7-bd24-4efa0abe8565').
narrative_ontology:cs_kernel_codification('91b80288-914d-43e7-bd24-4efa0abe8565', formalized).
narrative_ontology:cs_authority_grounding('91b80288-914d-43e7-bd24-4efa0abe8565', lineage).
narrative_ontology:cs_interpretation_layer_present('91b80288-914d-43e7-bd24-4efa0abe8565').
narrative_ontology:cs_reading_relation('91b80288-914d-43e7-bd24-4efa0abe8565', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('91b80288-914d-43e7-bd24-4efa0abe8565', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('91b80288-914d-43e7-bd24-4efa0abe8565', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('91b80288-914d-43e7-bd24-4efa0abe8565', foundational, explicit_systematic_instruction_universally_required).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_universally_required, holdable).
narrative_ontology:cs_axiom_grounding('91b80288-914d-43e7-bd24-4efa0abe8565', explicit_systematic_instruction_universally_required, empirically_contingent).
narrative_ontology:cs_axiom('91b80288-914d-43e7-bd24-4efa0abe8565', foundational, intervention_evidence_generalizes_to_all_readers).
narrative_ontology:cs_axiom_status(intervention_evidence_generalizes_to_all_readers, holdable).
narrative_ontology:cs_axiom_grounding('91b80288-914d-43e7-bd24-4efa0abe8565', intervention_evidence_generalizes_to_all_readers, empirically_contingent).
narrative_ontology:cs_axiom('91b80288-914d-43e7-bd24-4efa0abe8565', secondary, multisensory_encoding_necessary_for_decoding).
narrative_ontology:cs_axiom_status(multisensory_encoding_necessary_for_decoding, holdable).
narrative_ontology:cs_axiom_grounding('91b80288-914d-43e7-bd24-4efa0abe8565', multisensory_encoding_necessary_for_decoding, empirically_contingent).
narrative_ontology:cs_reference_frame('91b80288-914d-43e7-bd24-4efa0abe8565', certified_specialist_clinical_sequence).
narrative_ontology:cs_drift_state('91b80288-914d-43e7-bd24-4efa0abe8565', contemporary_universal_mandate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('91b80288-914d-43e7-bd24-4efa0abe8565', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, typically_developing_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_certification_bodies).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, proprietary_curriculum_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_advocacy_organizations).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, local_school_districts).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, colleges_of_education).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, typically_developing_readers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, science_of_reading_research_program).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__structured_literacy_reading, orthographic_mapping_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are the population the five-component sequence was designed for. Under immersion-based instruction most did not acquire decoding and were left with undiagnosed failure; under explicit systematic instruction their measured outcomes improve dramatically. They attend compulsory schooling, cannot choose their instructional method, and experience the mandate as the difference between literacy and long-term academic failure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_learning_disabilities, beneficiary,
    powerless, biographical, trapped, national).

% Read below grade level without a dyslexia diagnosis — the largest at-risk group in early-grade classrooms. Explicit systematic instruction produces large measured gains for them in the early grades; like all students they have no choice over the method used in their classroom.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, national).

% Learn to read under most competent methods. They receive the same five-component sequence as everyone else; the evidence shows small positive effects in kindergarten and first grade, and the opportunity cost of instructional time spent on decoding routines they did not need is the contested part of their position. They cannot opt out.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, typically_developing_readers, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, typically_developing_readers, payer).

% Must complete mandated structured-literacy training — from state-required awareness modules to 30-60+ hour coursework, with certification-track programs running far longer — frequently on their own time or unpaid professional-development days. They then deliver scripted lessons under fidelity monitoring, with pacing, language, and materials fixed by the adopted program. Professional judgment about method and pacing is displaced by the script; leaving the profession is the only exit from the mandate.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    organized, biographical, constrained, national).

% Must adopt state-approved curricula, purchase teacher training seats and program licenses, and document implementation for state compliance monitoring. Multi-year program contracts and training cohorts make mid-course correction expensive; compliance is legally required and locally funded.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, local_school_districts, payer,
    organized, biographical, constrained, regional).

% Accredit training programs and certify practitioners under standards they maintain (IMSLEC, AOGPE, IDA accreditation). Every new training mandate expands the population required to pass through their accredited providers; they collect accreditation and certification fees and control who counts as qualified. They can revise standards and rebrand — as with the coining of the term 'structured literacy' itself — without losing their position.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, structured_literacy_certification_bodies, beneficiary,
    institutional, generational, arbitrage, global).

% Sell the mandated programs — scripted teacher guides, student materials, decodable text series, and companion training — under multi-year district adoption contracts. Mandate states draw from approved lists on which their products sit, and fidelity requirements bind districts to their materials and training. They are the largest single recipient of the spending the mandates direct.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, proprietary_curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, national).

% Organized the parent-advocacy wave (Decoding Dyslexia and successor coalitions) that produced the state laws. The mandates they lobbied for now supply their legitimacy, membership growth, conference revenue, and a seat in standards-setting. They both collect from the arrangement and continue to set its agenda.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_advocacy_organizations, agenda_setter).

% Enact the training mandates, screening requirements, approved-curriculum lists, and statutory bans on cueing-based approaches. The laws are politically popular and bipartisan; legislators collect visible credit for fixing reading at low electoral cost while the compliance burden lands on districts and teachers. They can amend or repeal at will.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, state_legislatures, agenda_setter,
    institutional, immediate, arbitrage, national).

% Teacher-preparation faculties built on the approaches the statutes displace must overhaul coursework, retrain instructor staff, and align programs with structured-literacy standards to keep state approval. Some pivot and capture new training revenue; others bear the cost as displaced incumbents. Accreditation and state program approval depend on compliance.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, colleges_of_education, payer,
    institutional, generational, constrained, national).

% Produces and adjudicates the evidence: meta-analyses attesting large effects of explicit instruction for struggling readers, contested moderation analyses over effects for typical readers and older students, and active debate over what the evidence licenses mandating. It holds no enforcement power but supplies the warrant both sides cite.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, reading_research_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__structured_literacy_reading, proprietary_curriculum_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__structured_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem in reading instruction: before standardization, classroom methods varied widely and children who did not spontaneously acquire decoding — disproportionately dyslexic students — failed without diagnosis or targeted help. A shared explicit sequence (phonological awareness, phonics, fluency, vocabulary, comprehension) delivered by trained instructors makes reliable reading acquisition reproducible across classrooms instead of dependent on individual teacher philosophy.
% TRANSFER_FUNCTION: Moves mandated training hours and professional-development time from general education teachers to certification providers; moves district curriculum budgets to proprietary program publishers under multi-year adoption contracts; moves pedagogical discretion from classroom teachers to program scripts and fidelity monitors; and moves instructional time within the school day toward the five-component sequence for all students.
% ABSENT_VOICES: Teachers who implement the programs have little seat in adoption decisions — adoption flows from legislatures through state education agencies to districts, with teachers as implementers rather than parties. Multilingual/ELL educators object that a mandate designed around monolingual English learners with dyslexia is applied to students at very different starting points. Developmental and motivation researchers who documented the affective costs of early scripted regimes were largely absent from the legislative hearings that produced the approach bans.
% DISAPPEARANCE_RATIONALE: Districts would revert to methodological heterogeneity within a few budget cycles; certification revenue streams and curriculum contracts would lapse; dyslexic students would again depend on whether their individual teacher happened to teach decoding explicitly, and the pre-mandate failure rates would re-emerge; the legislative apparatus (screening mandates, training requirements, three-cueing bans) would require active repeal rather than fading on its own.
% FOUNDING_PROBLEM: Students with dyslexia were failing under whole-language and balanced-literacy instruction that assumed reading develops through immersion in meaningful text. Samuel Orton's 1920s-30s clinical work and the Gillingham-Stillman manual (1936/1940) built a structured, sequential, multisensory remediation so that students who could not acquire decoding incidentally could be taught it directly.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the NICHD National Reading Panel (2000) and subsequent meta-analytic literature in cognitive psychology attest that explicit, systematic phonics instruction substantially benefits struggling and at-risk readers; the pediatric and neuropsychological literature attests to dyslexia's prevalence and its response to structured intervention. The same outside literature does not clearly corroborate the universal-mandate extension across all grade bands and reader profiles — balanced-literacy and motivation researchers actively dispute that breadth — so the founding problem's liveness is corroborated while the arrangement's current scope is not.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) bundles a genuinely valuable instructional core with an extraction superstructure: the five-component sequence reliably produces decoding — its value is largest for dyslexic students, whose failure rates under immersion approaches were catastrophic — while the mandate apparatus (certified-training requirements, proprietary program adoption, fidelity monitoring) transfers teacher time and district budgets to certification and publishing seats at rates decoupled from instructional marginal cost. Suppression (0.65) is a raw structural property, unscaled by power or scope in the engine's arithmetic: it reflects legislatively banned alternatives (three-cueing prohibitions in mandate states), licensure-linked training requirements, and approved-curriculum lists rather than participant preference; a smaller internalized component (fidelity norms teachers carry as professional identity) is noted but the dominant mechanism is statutory. Theater (0.42) is moderate: multisensory rituals, accreditation hour requirements, and box-ticking fidelity checks coexist with real instructional function inherited from a 1930s clinical lineage. Accessibility collapse (0.58): alternatives are being actively collapsed by statute in mandate states but persist in private tutoring, home instruction, and non-mandate jurisdictions. Resistance (0.6): organized teacher pushback against scripted fidelity regimes, researcher contestation of the universality extension, and ELL educator objections. The measurement series share one grid (t=0..30 at steps of 6; t=0 approximates 1994, t=30 approximates 2024) and are deliberately non-monotonic: Reading First (t=6-12) was the first extraction ratchet, its collapse after the 2006-07 implementation findings (t=18) the first relaxation, and the post-2019 science-of-reading legislative wave (t=24-30) the second and larger ratchet.
 *
 * PERSPECTIVAL GAP:
 *   From the dyslexic-student seat the arrangement computes as near-pure coordination — it is the difference between literacy and lifelong academic failure — while from the general-teacher seat the same structure computes as enforced extraction (uncompensated training hours, scripted autonomy loss, fidelity surveillance) and from the certification and publisher seats as a revenue mandate. These are not disagreements about facts but different structural positions within one arrangement; the engine computes per-seat types from power, exit, and directionality, and wide divergence across seats is the expected signature of a tangled rope. The colleges_of_education seat is internally split: incumbent balanced-literacy faculties experience displacement while pivoting programs capture new training revenue from the same mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (dyslexic students, struggling readers, typically developing readers, certification bodies, publishers, advocacy organizations) derive directionality near the beneficiary pole; declared victims (general education teachers, districts, colleges of education) derive directionality near the target pole. Trapped exit (all student seats) and constrained exit (teachers, districts, colleges) hold targets near the full-target end; arbitrage exit (publishers, certification bodies, legislatures) holds beneficiaries near zero. Typically developing readers are the least certain seat: declared beneficiaries on the reading's own evidence (small positive early-grades effects) while carrying a contested opportunity cost, so their derived directionality likely understates their burden slightly. No directionality overrides are authored: the override mechanism keys on power atoms, and both atom classes that would need correction are internally heterogeneous — the powerless atom contains dyslexic students (d near 0) and typically developing readers (d near 0.4), and the institutional atom contains certification bodies (d near 0.05) and legislatures (d near 0.3) — so any per-atom override would misfire across seats. The agenda-setter seat collects political credit from the mandates it writes; the canonical fallback for an undeclared institutional seat likely overstates that seat's extraction burden, which is recorded here rather than forced through an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two symmetrical errors. Reading the arrangement as pure rope (the advocacy and legislative framing) erases the certification rents, the publisher lock-in, and the uncompensated teacher burden that ride on the same mandate. Reading it as pure snare (the teacher-autonomy framing) erases the documented intervention value that makes the coordination function real — dyslexic students' outcomes are the strongest evidence in the file. The founding problem (dyslexia remediation) is live, so this is not classic mandatrophy; the drift question is scope. The arrangement has expanded from remediation of a diagnosed minority to a universal mandate, and the extraction superstructure grew with the scope. If mandates were proportioned back to the evidence — targeted intervention for struggling readers, voluntary depth of certification — the residue would compute closer to rope; omega universality_evidence_gap is the instrument that would date that transition, and omega certification_rent_capture distinguishes the coordination cost of training from the rent component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_variant_status,
    'This story instantiates the structured_literacy_reading of the literacy_acquisition_kernel: is that reading a structurally distinct fourth reading, or a variant of phonics_reading with an added certification economy — and where exactly do the readings'' structural claims diverge?',
    'Axiom-set comparison across the family: if the universal-applicability claim, the five-component architecture, and the certification regime are all entailed by phonics_reading''s decoding-first axiom, this story collapses into a phonics variant and its epsilon should merge with phonics_reading''s; if they are independent normative additions, the reading is distinct. The disagreement is located in scope (remediation-targeted vs. universal) and in the delivery apparatus (curriculum sequencing vs. certified-practitioner mandate).',
    'If a variant, the family reduces to three readings and the certification-extraction surface should be reattributed to phonics_reading; if distinct, the training-mandate extraction belongs to this reading alone and phonics_reading''s epsilon stays low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_variant_status, conceptual, 'Whether this reading is independent of phonics_reading or a variant with an appended certification economy.').

omega_variable(
    universality_evidence_gap,
    'Does the evidence base for explicit systematic instruction extend from struggling readers (strong effects) to all students (contested), such that the universal mandate tracks the evidence or exceeds it?',
    'Meta-analytic moderation analysis: effect sizes for explicit phonics instruction by reader profile (typical, at-risk, dyslexic) and grade band. If effects for typical readers beyond the early grades are near zero, universal mandates exceed the evidence.',
    'If the evidence is intervention-specific, the universal mandate''s burden on general classrooms is unjustified by the constraint''s own empirical warrant and the arrangement drifts toward pure extraction; if effects are general, the mandate is coordination and the typically_developing_readers seat''s payer side dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_evidence_gap, empirical, 'Whether universal application is evidence-supported or an overextension of dyslexia-intervention findings.').

omega_variable(
    certification_rent_capture,
    'Do the teacher-training mandates primarily serve instructional quality or the certification and curriculum economy (training providers, accreditation bodies, program publishers)?',
    'Cost-benefit audit of mandated training: compare student outcomes for teachers certified through full OG-track coursework versus compressed state modules versus no certification, controlling for program fidelity; independently trace fee and contract flows to accreditation bodies and publishers.',
    'If compressed modules produce equivalent outcomes, the certification-hour requirements function as rent collection and the constraint''s extractiveness is understated; if full certification is outcome-necessary, the teacher burden is coordination cost and the rope component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_rent_capture, empirical, 'Whether certification requirements track efficacy or capture.').

omega_variable(
    lineage_theater_share,
    'What share of Orton-Gillingham-tradition practice (multisensory rituals, accreditation hours, scripted fidelity requirements) is functional instruction versus lineage performance inherited from the tradition''s 1930s clinical origins?',
    'Component-deletion studies isolating multisensory and ritual elements from the explicit-sequencing core; accreditation outcome audits comparing accredited and non-accredited providers.',
    'A high theater share would raise the inertial, performance-maintained component inside the arrangement and support streamlining mandates to the functional core; a low share supports the tradition''s integrated design claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lineage_theater_share, empirical, 'Functional versus lineage-performative share of OG practice elements.').

omega_variable(
    teacher_autonomy_tradeoff,
    'Is the loss of teacher pedagogical autonomy a genuine cost borne by teachers and students, or does curricular standardization itself benefit students by eliminating ineffective instructional variation?',
    'Comparative outcomes across districts with high-fidelity scripted implementation versus adapted implementation, plus teacher-retention and instructional-quality measures under each regime.',
    'If standardization benefits students, the teacher seat''s burden claim weakens and the arrangement moves toward rope; if autonomy loss degrades outcomes or retention, the burden is confirmed as borne cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(teacher_autonomy_tradeoff, conceptual, 'Whether constrained teacher autonomy is a cost or a coordination benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(lite_tr_t0, observed).
narrative_ontology:measurement(lite_tr_t6, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(lite_tr_t6, observed).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(lite_tr_t12, observed).
narrative_ontology:measurement(lite_tr_t18, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement_basis(lite_tr_t18, observed).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(lite_tr_t24, observed).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(lite_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(lite_be_t0, observed).
narrative_ontology:measurement(lite_be_t6, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement_basis(lite_be_t6, observed).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(lite_be_t12, observed).
narrative_ontology:measurement(lite_be_t18, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement_basis(lite_be_t18, observed).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement_basis(lite_be_t24, observed).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(lite_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(lite_su_t0, observed).
narrative_ontology:measurement(lite_su_t6, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 6, 0.3).
narrative_ontology:measurement_basis(lite_su_t6, observed).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement_basis(lite_su_t12, observed).
narrative_ontology:measurement(lite_su_t18, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 18, 0.35).
narrative_ontology:measurement_basis(lite_su_t18, observed).
narrative_ontology:measurement(lite_su_t24, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement_basis(lite_su_t24, observed).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(lite_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the literacy_acquisition_kernel decomposes into four readings with distinct epsilon referents, per the epsilon-invariance principle — one label ('how reading is acquired') covered four structurally different claims. This reading's epsilon covers the full mandate apparatus (training mandates, certification economy, curriculum lock-in, approach bans) over the five-component universal claim; phonics_reading's covers the narrower decoding-first sequencing claim without the certification economy; whole_language_reading's covers the immersion claim now under statutory prohibition; balanced_literacy_reading's covers the complementarity claim being reorganized under mandate. Upstream/downstream structure: phonics_reading's evidence base is cited as warrant for this reading's universal extension, while this reading's legislative success supplies the enforcement environment that constrains the other two siblings' practice space. The variant-status contest (fourth reading vs. phonics variant) is carried as omega reading_kernel_variant_status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
