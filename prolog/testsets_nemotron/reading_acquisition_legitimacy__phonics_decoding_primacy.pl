% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Systematic Phonics as Legitimate Decoding Instruction
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint instantiates the phonics_decoding_primacy reading of the
 *   contested kernel 'reading_acquisition_legitimacy.' It asserts that
 *   reading acquisition is fundamentally decoding, and that legitimate
 *   instruction must make the alphabetic principle explicit through
 *   systematic, sequential phonics with decodable texts. The constraint
 *   emerged from cognitive science consensus (NRP 2000, subsequent
 *   replication) and has been codified in state policy (Reading First, state
 *   dyslexia laws, 'science of reading' legislation). It coordinates
 *   instruction by providing a shared scope-and-sequence, common assessments,
 *   and early identification of struggling readers. Extraction occurs through
 *   commercial program mandates, fidelity requirements that exceed linguistic
 *   necessity, and the displacement of teacher knowledge by scripted
 *   curricula. The constraint actively suppresses alternative pedagogies
 *   (whole language, balanced literacy) through policy mandates and funding
 *   conditions.
 *
 * KEY AGENTS:
 *   - early_readers: Primary beneficiaries (powerless/biographical/trapped) — receive explicit decoding instruction that prevents reading failure
 *   - struggling_readers: Primary beneficiaries (powerless/biographical/trapped) — identified early via decoding assessments, receive targeted intervention
 *   - dyslexic_learners: Primary beneficiaries (powerless/biographical/trapped) — systematic phonics is necessary (not just beneficial) for their reading acquisition
 *   - teachers_implementing_structured_literacy: Secondary beneficiaries (moderate/biographical/constrained) — gain coherent framework but lose autonomy to fidelity mandates
 *   - teachers_constrained_by_fidelity_requirements: Victims (moderate/biographical/constrained) — bear cost of scripted programs, pacing mandates, commercial program compliance
 *   - schools_under_curriculum_mandates: Victims (organized/biographical/constrained) — bear financial and operational costs of mandated programs, assessments, and professional development
 *   - commercial_program_publishers: Beneficiaries (institutional/generational/arbitrage) — capture revenue from mandated adoptions, control fidelity definitions
 *   - literacy_researchers: Observers (analytical/civilizational/analytical) — study outcomes, debate mechanisms, inform policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.28).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.42).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.28).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Systematic Phonics as Legitimate Decoding Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, 'a70c4b73-a112-4f43-931d-3f1f00395bd1').
narrative_ontology:cs_kernel_codification('a70c4b73-a112-4f43-931d-3f1f00395bd1', formalized).
narrative_ontology:cs_authority_grounding('a70c4b73-a112-4f43-931d-3f1f00395bd1', expertise).
narrative_ontology:cs_interpretation_layer_present('a70c4b73-a112-4f43-931d-3f1f00395bd1').
narrative_ontology:cs_reading_relation('a70c4b73-a112-4f43-931d-3f1f00395bd1', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('a70c4b73-a112-4f43-931d-3f1f00395bd1', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('a70c4b73-a112-4f43-931d-3f1f00395bd1', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('a70c4b73-a112-4f43-931d-3f1f00395bd1', foundational, alphabetic_principle_requires_explicit_instruction).
narrative_ontology:cs_axiom_status(alphabetic_principle_requires_explicit_instruction, holdable).
narrative_ontology:cs_axiom_grounding('a70c4b73-a112-4f43-931d-3f1f00395bd1', alphabetic_principle_requires_explicit_instruction, empirically_contingent).
narrative_ontology:cs_axiom('a70c4b73-a112-4f43-931d-3f1f00395bd1', foundational, systematic_sequencing_prevents_decoding_gaps).
narrative_ontology:cs_axiom_status(systematic_sequencing_prevents_decoding_gaps, holdable).
narrative_ontology:cs_axiom_grounding('a70c4b73-a112-4f43-931d-3f1f00395bd1', systematic_sequencing_prevents_decoding_gaps, empirically_contingent).
narrative_ontology:cs_axiom('a70c4b73-a112-4f43-931d-3f1f00395bd1', secondary, early_decoding_assessment_enables_preventive_intervention).
narrative_ontology:cs_axiom_status(early_decoding_assessment_enables_preventive_intervention, holdable).
narrative_ontology:cs_axiom_grounding('a70c4b73-a112-4f43-931d-3f1f00395bd1', early_decoding_assessment_enables_preventive_intervention, empirically_contingent).
narrative_ontology:cs_reference_frame('a70c4b73-a112-4f43-931d-3f1f00395bd1', national_reading_panel_2000_consensus).
narrative_ontology:cs_drift_state('a70c4b73-a112-4f43-931d-3f1f00395bd1', state_science_of_reading_legislation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a70c4b73-a112-4f43-931d-3f1f00395bd1', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, early_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, dyslexic_learners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_implementing_structured_literacy).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_constrained_by_fidelity_requirements).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, schools_under_curriculum_mandates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, commercial_program_publishers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_implementing_structured_literacy).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, alphabetic_principle_universality).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, systematic_phonics_effectiveness).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, early_decoding_assessment_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children learning to read who receive explicit systematic phonics instruction. They cannot choose their instructional method; they are trapped in whatever system their school adopts. For those who would otherwise struggle (30-40%), this constraint provides the decoding foundation they need. The alphabetic principle is not discoverable by most children without explicit instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, early_readers, beneficiary,
    powerless, biographical, trapped, national).

% Children who fall behind in reading acquisition. Under this constraint, they are identified early via decoding assessments (DIBELS, Acadience) and receive Tier 2/3 interventions in systematic phonics. Without this constraint, they would likely remain unidentified until later grades when remediation is far less effective. They bear no cost of the constraint; they are pure beneficiaries of its coordination function.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_readers, beneficiary,
    powerless, biographical, trapped, national).

% Children with dyslexia (10-20% of population) for whom systematic phonics is not merely beneficial but necessary. Their neurobiology requires explicit, cumulative, multisensory instruction in the alphabetic principle. The constraint's structure matches their cognitive requirement. Exit is identity-locked: they cannot 'choose' a different reading pathway because their reading pathway IS the constraint. The constraint subsidizes them completely.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, dyslexic_learners, beneficiary,
    powerless, biographical, identity_locked, national).

% Teachers who have adopted structured literacy approaches. They benefit from a coherent, evidence-based framework, common language with colleagues, and clearer assessment data. They pay through fidelity requirements: scripted lessons, pacing mandates, commercial program compliance monitoring. They could leave for balanced literacy districts but face professional identity costs and retooling burden. Their exit is constrained, not trapped.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_implementing_structured_literacy, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_implementing_structured_literacy, payer).

% Teachers mandated to implement commercial phonics programs with strict fidelity. They bear the cost of lost professional autonomy: scripted lessons, prescribed pacing, prohibition on teacher-created materials, compliance observations. Many support systematic phonics but resist commercial program mandates. Exit is constrained: they can change districts (if available), leave teaching, or comply. The constraint extracts their professional judgment and time.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_constrained_by_fidelity_requirements, payer,
    moderate, biographical, constrained, national).

% Districts and schools subject to state 'science of reading' laws requiring approved programs, certified training, and specific assessments. They bear financial costs (program purchases $100K-$1M+, PD, assessment systems), operational costs (scheduling, coaching, monitoring), and opportunity costs (displacing other priorities). Exit is constrained: they can seek waivers (rare), litigate, or comply. State funding is often tied to compliance.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, schools_under_curriculum_mandates, payer,
    organized, biographical, constrained, national).

% Companies (Amplify, HMH, McGraw Hill, 95 Percent Group, etc.) selling comprehensive phonics programs, assessments, and PD. They capture the extraction: state adoption lists, district contracts, recurring revenue from consumables and licenses. They shape fidelity definitions, influence legislation through lobbying, and control the 'approved' curriculum ecosystem. They have arbitrage-grade exit: they can shift products, markets, or acquire competitors. They are beneficiaries of the constraint's enforcement machinery, not its coordination function.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, commercial_program_publishers, beneficiary,
    institutional, generational, arbitrage, national).

% Cognitive scientists, reading researchers, and education scholars who study reading acquisition, instructional effectiveness, and policy implementation. They provide the evidence base (NRP, meta-analyses, RCTS) that legitimates the constraint. They debate mechanisms (phonics vs. morphology, dosage, transfer), monitor outcomes, and critique commercial capture. They neither pay nor collect; they observe and contest the constraint's epistemic warrants.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, literacy_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the alphabetic principle coordination problem: English's deep orthography requires explicit, systematic teaching of grapheme-phoneme correspondences in a logical sequence so that all children — not just those who induce the pattern — can decode. Provides shared scope-and-sequence across classrooms, common assessments for early identification, and a cumulative progression that prevents gaps. Without this coordination, each teacher reinvents decoding instruction, sequences differ, and vulnerable children fall through gaps.
% TRANSFER_FUNCTION: Moves instructional authority and resources from teacher autonomy and whole-language/balanced literacy approaches to commercial program publishers and state mandates. Transfers financial resources (district budgets → publishers), professional discretion (teacher judgment → fidelity rubrics), and assessment control (local measures → mandated screeners). Transfers reading outcomes: from variable/inequitable to more uniform decoding proficiency, especially for vulnerable populations.
% ABSENT_VOICES: Children and families in communities where 'science of reading' mandates have displaced culturally responsive literacy practices, bilingual approaches, and community-based literacy traditions. Indigenous language immersion programs, dual-language educators, and families who value meaning-centered literacy are structurally excluded from policy conversations dominated by decoding metrics. They would object to the narrowing of literacy to decoding fluency but are not in the room where mandates are written.
% DISAPPEARANCE_RATIONALE: If systematic phonics mandates vanished overnight, early reading instruction would immediately fragment: some teachers would continue explicit phonics, others would revert to balanced literacy or whole language, commercial programs would lose guaranteed markets, state dyslexia laws would become unenforceable, and the common assessment infrastructure would dissolve. Decoding outcomes for vulnerable children would likely decline within 2-3 years as early identification lapses. The coordination infrastructure (scope-and-sequence, screeners, intervention protocols) would not persist without the mandate.
% FOUNDING_PROBLEM: The founding problem is the persistent failure of implicit/incidental phonics approaches to teach decoding to a substantial minority of children (30-40%), with catastrophic long-term consequences for those children. The National Reading Panel (2000) synthesized evidence that systematic phonics instruction significantly improves decoding, spelling, and comprehension for all children, with the largest effects for at-risk and dyslexic learners. The problem was not that phonics was unknown, but that it was unsystematically taught, leaving the alphabetic principle opaque to those who could not induce it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by cognitive scientists outside the commercial beneficiary set: Mark Seidenberg (Language at the Speed of Sight), Anne Castles, Kathleen Rastle, Keith Rayner (eye-tracking research), Linnea Ehri (orthographic mapping), David Share (self-teaching hypothesis). Dyslexia advocacy organizations (International Dyslexia Association, Decoding Dyslexia state chapters) — comprised of parents, not publishers — attest the problem remains live. NAEP data (2019, 2022, 2024) showing stagnant/declining reading proficiency corroborates that the problem persists despite two decades of policy attention.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).
:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) reflects modest but real extraction: commercial programs capture revenue, fidelity mandates extract teacher autonomy, and policy enforcement has compliance costs. However, the coordination function is genuine and substantial: the alphabetic principle is a real coordination problem (shared code requires explicit teaching), systematic sequencing prevents gaps, and early identification prevents cascading failure. Suppression (0.42) is moderate: alternative pedagogies are actively marginalized through policy and funding, but not eliminated — balanced literacy persists in many districts. Theater ratio (0.18) is low: the core instructional practices are functional, though fidelity theater exists in compliance monitoring. Accessibility collapse (0.55) is moderate: alternatives exist but are structurally disadvantaged by policy; Resistance (0.48) is moderate: teacher pushback on scripted programs, parent advocacy for alternatives, and academic debate persist. The claimed type 'rope' reflects the reading's self-understanding as genuine coordination; the engine will compute per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From early/struggling/dyslexic readers' seats: the constraint is mountain-like — explicit systematic phonics is necessary infrastructure without which they cannot read. From teachers_constrained_by_fidelity: the constraint is tangled_rope — genuine coordination function (they want to teach decoding well) fused with extractive compliance (commercial scripts, pacing mandates). From commercial_publishers: the constraint is a snare — they extract revenue and control through policy capture while presenting as coordination. From schools: the constraint is scaffold-like — costly transitional infrastructure justified by outcomes, but with no sunset. The engine computes these divergences from power/exit/beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: early_readers, struggling_readers, dyslexic_learners (trapped, identity_locked exit — they cannot exit the need to decode; the constraint subsidizes them). Teachers_implementing_structured_literacy are secondary beneficiaries (constrained exit — they chose this pedagogy but face fidelity mandates). Victims declared: teachers_constrained_by_fidelity (constrained exit — can leave profession but not constraint), schools_under_curriculum_mandates (constrained exit — district policy binds them). Commercial_publishers are beneficiaries not declared in base_properties (they extract, not benefit from coordination) — their extraction is captured in extractiveness metric. Directionality derivation: trapped/identity_locked beneficiaries → d near 0.0 (subsidized); constrained victims → d near 0.8-0.9 (targets); institutional publishers → d near 0.1 (beneficiaries of extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (preventing reading failure through explicit decoding instruction) remains LIVE — dyslexia prevalence, NAEP stagnation, and cognitive science replication sustain it. However, the ORIGINAL Reading First mandate (2002) exhibited mandatrophy: the coordination function was real but the federal enforcement mechanism atrophied into compliance theater and commercial capture. Current state-level 'science of reading' laws are a NEW constraint instantiation, not continuation of the old mandate. The founding problem is corroborated by cognitive scientists (Seidenberg, Castles, Rayner) and dyslexia advocates (IDA, Decoding Dyslexia) — OUTSIDE the commercial beneficiaries. Mandatrophy is PARTIALLY resolved: the core coordination claim is vindicated, but the enforcement layer shows extraction drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the contested kernel ''reading_acquisition_legitimacy''. Does instantiating the phonics_decoding_primacy reading as a clean ε-invariant constraint adequately capture the structural reality, or does the kernel''s multi-reading nature introduce irreducible ambiguity about which extraction and coordination functions are ''real''?',
    'Compare classification outcomes across all kernel readings (phonics_decoding_primacy, whole_language_meaning_primacy, balanced_literacy_integration, structured_literacy_remediation). If classifications diverge substantially despite shared referent, the kernel structure itself is an omega-class ambiguity.',
    'If the kernel frame is essential, treating any single reading as a standalone constraint may misclassify by omitting the competitive pressure from sibling readings that shapes each reading''s actual enforcement and extraction dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether the kernel''s multi-reading structure introduces irreducible ambiguity into single-reading classification').

omega_variable(
    coordination_vs_standardization_boundary,
    'Where does the genuine coordination function of systematic phonics (shared code, predictable progression, early identification) end and extractive standardization (commercial program lock-in, fidelity policing, teacher autonomy reduction) begin?',
    'Track curriculum adoption cycles: when districts adopt new phonics programs, measure changes in (a) student outcomes, (b) teacher discretion, (c) vendor revenue, (d) assessment alignment. A genuine coordination function shows outcome gains with stable or increasing teacher discretion; extractive standardization shows vendor capture and discretion loss without proportional outcome gains.',
    'If extractive standardization dominates, the constraint reclassifies toward tangled_rope or snare. If coordination dominates, rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_standardization_boundary, empirical, 'Boundary between coordination and extractive standardization in systematic phonics mandates').

omega_variable(
    teacher_fidelity_as_extraction_or_coordination,
    'Is high-fidelity implementation requirement a coordination mechanism (ensuring the alphabetic principle is taught completely and in sequence) or an extraction mechanism (commercial program compliance, vendor revenue protection, administrator control)?',
    'Analyze fidelity rubrics: do they measure adherence to linguistic scope-and-sequence (coordination) or adherence to specific commercial program scripts, pacing guides, and proprietary materials (extraction)? Compare outcomes across high-fidelity commercial vs. teacher-developed systematic phonics.',
    'If fidelity requirements primarily serve commercial programs, extraction is higher than measured; if they serve linguistic completeness, extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_fidelity_as_extraction_or_coordination, empirical, 'Whether fidelity mandates coordinate instruction or extract compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(read_tr_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2015, 0.25).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2020, 0.26).
narrative_ontology:measurement(read_be_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement(read_su_t2025, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, information_standard).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.02).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'reading_acquisition_legitimacy' into four structurally distinct readings with different ε values, beneficiary/victim structures, and enforcement dynamics. The phonics_decoding_primacy reading has the strongest empirical vindication (NRP 2000, subsequent meta-analyses) but also the strongest policy enforcement machinery, creating extraction dynamics absent in the whole_language reading (which lacks enforcement) and the balanced_literacy reading (which has diffuse, low-extraction coordination). The structured_literacy_remediation reading shares phonics_decoding_primacy's coordination core but differs in victim structure (designed for vulnerable-first, not universal mandate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__phonics_decoding_primacy, institutional, 0.1).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__phonics_decoding_primacy, moderate, 0.75).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__phonics_decoding_primacy, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
