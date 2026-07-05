% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Systematic Phonics as Foundational Reading Instruction Requirement
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the phonics reading of the contested 'reading
 *   acquisition mechanism' kernel: reading acquisition requires explicit,
 *   systematic instruction in grapheme-phoneme correspondence as a
 *   foundational skill, evidenced by cognitive science (phonological
 *   processing research) and intervention trials, and increasingly codified
 *   into state 'science of reading' statutes. This is generated as a clean,
 *   self-contained constraint with its own stable ε — it does not average
 *   over or describe the whole_language_reading or balanced_literacy_reading
 *   siblings, which are separate constraint files with their own
 *   beneficiary/victim structures and their own ε. The structural delta
 *   specific to this reading: high initial instructional-retraining and
 *   curriculum-adoption cost, low long-term remediation cost (fewer students
 *   requiring intensive reading intervention downstream), narrowed teacher
 *   instructional discretion during the early-literacy years, and
 *   disproportionate benefit to struggling readers and dyslexic students
 *   relative to strong intuitive decoders who would likely succeed under any
 *   method.
 *
 * KEY AGENTS:
 *   - struggling_readers: primary beneficiary (powerless/trapped) — cannot advocate for method, bears cost of wrong pedagogy most acutely
 *   - dyslexic_students: primary beneficiary (powerless/trapped) — strongest evidentiary case for explicit instruction
 *   - teachers_trained_in_whole_language_methods: primary payer (moderate/constrained) — bears retraining and identity cost
 *   - publishers_of_leveled_literacy_programs: secondary payer (organized/mobile) — bears contract and product-line cost, can pivot
 *   - state_education_agencies: agenda_setter (institutional/analytical) — administers mandate, could relax it
 *   - reading_science_researchers: analytical beneficiary (institutional/analytical) — evidence base vindicated, no direct rent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.22).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.38).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Systematic Phonics as Foundational Reading Instruction Requirement").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, '498a7757-9a64-45db-aaf6-0407fbbb14ee').
narrative_ontology:cs_kernel_codification('498a7757-9a64-45db-aaf6-0407fbbb14ee', distributed).
narrative_ontology:cs_authority_grounding('498a7757-9a64-45db-aaf6-0407fbbb14ee', expertise).
narrative_ontology:cs_interpretation_layer_present('498a7757-9a64-45db-aaf6-0407fbbb14ee').
narrative_ontology:cs_reading_relation('498a7757-9a64-45db-aaf6-0407fbbb14ee', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('498a7757-9a64-45db-aaf6-0407fbbb14ee', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('498a7757-9a64-45db-aaf6-0407fbbb14ee', foundational, decoding_requires_explicit_instruction_for_reliable_acquisition).
narrative_ontology:cs_axiom_status(decoding_requires_explicit_instruction_for_reliable_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('498a7757-9a64-45db-aaf6-0407fbbb14ee', decoding_requires_explicit_instruction_for_reliable_acquisition, empirically_contingent).
narrative_ontology:cs_axiom('498a7757-9a64-45db-aaf6-0407fbbb14ee', secondary, implicit_exposure_alone_is_insufficient_for_a_documented_subpopulation).
narrative_ontology:cs_axiom_status(implicit_exposure_alone_is_insufficient_for_a_documented_subpopulation, holdable).
narrative_ontology:cs_axiom_grounding('498a7757-9a64-45db-aaf6-0407fbbb14ee', implicit_exposure_alone_is_insufficient_for_a_documented_subpopulation, empirically_contingent).
narrative_ontology:cs_reference_frame('498a7757-9a64-45db-aaf6-0407fbbb14ee', national_reading_panel_evidence_synthesis).
narrative_ontology:cs_drift_state('498a7757-9a64-45db-aaf6-0407fbbb14ee', post_state_science_of_reading_legislation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('498a7757-9a64-45db-aaf6-0407fbbb14ee', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, dyslexic_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, early_elementary_teachers_using_scope_sequence).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, reading_science_researchers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teachers_trained_in_whole_language_methods).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, publishers_of_leveled_literacy_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, early_elementary_teachers_using_scope_sequence).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, grapheme_phoneme_correspondence_is_foundational).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, orthographic_mapping_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children without strong phonemic-awareness priors from home literacy exposure. Systematic phonics gives them an explicit map from print to sound that they cannot reliably infer from context or exposure alone. Without it, many plateau at guessing-from-pictures strategies that stop working once texts get harder. They have no say in which method their school adopts.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, national).

% Students whose neurological profile makes implicit pattern-extraction from whole-word or context-cueing approaches especially unreliable. Explicit, sequential, cumulative grapheme-phoneme instruction plus decodable text is the intervention with the strongest evidentiary support for this population specifically.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, dyslexic_students, beneficiary,
    powerless, biographical, trapped, national).

% Must learn and deliver a structured, sequenced curriculum rather than choosing texts and activities at their own discretion. This is real professional retraining cost and a narrowing of classroom autonomy, but it also gives them a diagnosable, correctable path when a student stalls, rather than an opaque 'exposure isn't working' dead end.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, early_elementary_teachers_using_scope_sequence, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, early_elementary_teachers_using_scope_sequence, beneficiary).

% Cognitive scientists and reading researchers whose converging behavioral, neuroimaging, and intervention-trial evidence base is vindicated by adoption. They do not collect rents from the classroom, but professional standing and funding streams track the credibility of the phonics evidence base.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, reading_science_researchers, beneficiary,
    institutional, generational, analytical, global).

% Built careers and classroom identities around meaning-first, exposure-driven pedagogy, often over decades. Mandated systematic phonics requires retraining, discarding trusted materials, and in some cases public acknowledgment that a prior practice was less effective for a subset of students. Exit means leaving the profession or the district; staying means retraining under scrutiny.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teachers_trained_in_whole_language_methods, payer,
    moderate, biographical, constrained, regional).

% Companies with large installed contracts selling leveled-reader and balanced-literacy curricula. State-level phonics mandates threaten existing contracts and require costly reformulation of product lines. They can pivot product lines (mobile exit) but lose incumbency advantage and existing licensing revenue.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, publishers_of_leveled_literacy_programs, payer,
    organized, biographical, mobile, national).

% Set curriculum mandates and certification requirements based on the evidence base, often following state legislation ('science of reading' laws). They administer the requirement and could relax it, but the political and evidentiary cost of doing so, given documented literacy outcomes, is currently high.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, state_education_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Parents who observe their children's reading trajectory but have little influence over district curriculum choice beyond advocacy and, for the few with resources, private tutoring or school choice. Their preferences are largely absent from the curriculum-adoption process.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, families_of_early_readers, excluded,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__phonics_reading, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__phonics_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates instructional practice around a decodable, sequenced map from print to sound so that a classroom of children with heterogeneous home-literacy exposure can all acquire a reliable decoding mechanism rather than each child needing to independently infer the code.
% TRANSFER_FUNCTION: Moves instructional time, teacher retraining effort, and curriculum-adoption budgets toward systematic phonics programs and away from incumbent leveled-literacy and whole-language materials and the professional practices built around them.
% ABSENT_VOICES: Families of early readers are rarely direct parties to curriculum-adoption decisions despite bearing the outcome most directly; whole-language-trained teachers' professional-judgment objections are heard in policy debate but are structurally outweighed once state 'science of reading' statutes are enacted.
% DISAPPEARANCE_RATIONALE: If the systematic-phonics requirement were removed, districts would revert to discretionary curriculum choice; without a mandated evidence-aligned floor, struggling readers and dyslexic students would again depend on whichever school or teacher happened to use structured methods, and outcome variance across schools would widen measurably within a few cohorts.
% FOUNDING_PROBLEM: A persistent minority of children (historically estimated at a substantial fraction of a cohort, disproportionately low-income and dyslexic) failed to become fluent decoders under meaning-first and incidental-exposure approaches, plateauing in upper elementary with reading levels that closed off access to content-area learning.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the immediate beneficiary set by cognitive-science research (converging behavioral and neuroimaging studies on phonological processing predating any curriculum-adoption stakes), by longitudinal NAEP and state assessment data showing persistent decoding gaps under non-systematic instruction, and by independent journalistic investigation (e.g. multi-year reporting tracing curriculum choices to outcome gaps) that had no stake in publisher or curriculum-vendor outcomes.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).
:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because the coordination function — a reliable, teachable decoding mechanism for children without strong home-literacy priors — is genuine and well-evidenced, and no party is positioned to extract rents through the requirement itself (curriculum vendors compete for adoption contracts on the merits of implementation, not through suppression of alternatives). Suppression is moderate and rising (0.20 to 0.38) reflecting the real cost imposed on incumbent teachers and publishers as state mandates convert what was professional discretion into a statutory requirement; this is the mechanism's genuine bite, not performative theater. Theater ratio stays low and flat because the instructional core (structured phonics lessons, decodable texts, assessment of decoding fluency) is the actual mechanism at work, not a proxy display.
 *
 * PERSPECTIVAL GAP:
 *   From the state agency and researcher seats, this looks like coordination on settled evidence. From the whole-language-trained teacher's seat, the same requirement looks like enforced professional displacement — years of trusted practice invalidated by statute with real retraining cost and no transition support. The engine should compute these as different seat classifications from the same structural data: the payer seats carry higher effective extraction than the beneficiary seats, even though the underlying ε is authored low and the coordination function is genuine.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers and dyslexic students sit near the beneficiary end: the constraint subsidizes them by providing an explicit, learnable code where implicit exposure previously failed them, and they have no exit (trapped, powerless). Teachers trained in whole-language methods and curriculum publishers sit nearer the target end: they bear the retraining/reformulation cost through the same structural change that benefits the students, though publishers have mobile exit (can pivot product lines) unlike teachers whose professional identity is more constrained. Reading science researchers are structurally validated but do not collect rents from the classroom — hence beneficiary role without financial capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a persistent decoding-failure minority under non-systematic methods) remains live per longitudinal assessment data, which is why founding_problem_status is 'live' rather than 'dead' or 'contested' — this blocks a mandatrophy reading where the requirement persists after its function expired. If future evidence showed the decoding gap had closed durably under alternative methods, this status would need reassessment; currently the corroborating evidence (NAEP trends, independent reporting) supports continued function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Is this constraint a genuine natural-law-adjacent finding about how the human brain acquires the alphabetic code, or a constructed pedagogical policy choice that happens to align with a particular evidence tradition and benefits its associated researchers, textbook publishers, and credentialing bodies?',
    'This is the located point of disagreement among the three kernel readings: phonics_reading holds that explicit systematic instruction is necessary because implicit code-acquisition fails for a documented subpopulation regardless of exposure quality; whole_language_reading holds that decoding is a byproduct of sufficient meaningful engagement and explicit instruction is unnecessary scaffolding; balanced_literacy_reading holds both are necessary in integrated combination. Resolution would require converging causal (not merely correlational) evidence isolating the decoding mechanism from confounded instructional-quality and exposure-volume variables across large, randomized, long-horizon trials — some of which exists (e.g. National Reading Panel meta-analyses) but remains contested on methodological grounds by whole-language proponents.',
    'If the whole_language_reading premise is correct for the general population and phonics benefits are confined to a narrow at-risk subpopulation, this reading''s claimed general-population coordination function shrinks substantially and the constraint''s classification moves toward serving a narrower beneficiary group (dyslexic/struggling readers only) with the broader mandate functioning as an over-generalized, costlier requirement than necessary — a partial false-summit pattern where the ''science'' framing outruns the specific evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, empirical, 'Located kernel disagreement: whether explicit code instruction is necessary in general or only for an at-risk subpopulation, and what that implies for the constraint''s generalized mandate.').

omega_variable(
    publisher_capture_of_evidence_framing,
    'To what extent has the ''science of reading'' branding and associated state mandates been shaped by curriculum-publisher lobbying for a new addressable market (structured-phonics program sales) rather than purely by the underlying cognitive-science evidence?',
    'Trace state legislative history and lobbying disclosures for structured-literacy curriculum vendors relative to the independent academic evidence timeline; compare adoption patterns in jurisdictions with versus without vendor lobbying presence.',
    'If publisher advocacy substantially preceded or shaped the evidentiary consensus rather than following it, some of the measured low extractiveness understates a commercial capture dynamic; if the evidence base clearly predates and is independent of vendor advocacy (as the citation record suggests), the low extractiveness authoring holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(publisher_capture_of_evidence_framing, conceptual, 'Whether structured-phonics curriculum vendors substantially shaped, versus merely followed, the evidentiary consensus now codified into policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__phonics_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(read_tr_t4, reading_acquisition_mechanism__phonics_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_mechanism__phonics_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_mechanism__phonics_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_mechanism__phonics_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__phonics_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(read_be_t4, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 4, 0.17).
narrative_ontology:measurement(read_be_t8, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 8, 0.19).
narrative_ontology:measurement(read_be_t12, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 12, 0.2).
narrative_ontology:measurement(read_be_t16, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 16, 0.21).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 20, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(read_su_t4, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(read_su_t8, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(read_su_t12, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(read_su_t16, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__phonics_reading, 0.08).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the reading_acquisition_mechanism kernel. whole_language_reading and balanced_literacy_reading are separate constraint files with independently authored ε, beneficiary/victim structures, and classifications. This file's network edges point to both siblings because adoption of the phonics reading in a jurisdiction structurally displaces resources and legitimacy from both alternative readings simultaneously (a single state curriculum adoption decision selects among all three, not a pairwise contest).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__phonics_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
