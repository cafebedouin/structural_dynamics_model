% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__phonics_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Phonics-First Reading Instruction Mandate (Decoding Precedes Comprehension)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the phonics-first reading of the literacy
 *   acquisition kernel: reading acquisition requires explicit, systematic
 *   phoneme-grapheme instruction before connected-text exposure, and decoding
 *   precedes and enables comprehension. This is one of four structurally
 *   distinct readings of the same underlying kernel (how children acquire
 *   reading), each authored as its own constraint per the ε-invariance
 *   principle. Under this reading, the coordination function (closing the
 *   decoding-failure gap, especially for phonologically weak students) is
 *   real and well-evidenced, but the mandate as implemented in many
 *   jurisdictions has been captured by scripted-fidelity requirements that
 *   extract from teacher professional judgment beyond what the founding
 *   evidence base specifically supports. The claim (tangled_rope) and the
 *   metrics (moderate-high extractiveness, moderate suppression, rising
 *   theater ratio) are authored independently and are expected to diverge
 *   somewhat from a pure phonics-science reading, which would show lower
 *   extractiveness absent the scripted-fidelity apparatus.
 *
 * KEY AGENTS:
 *   - students_with_weak_phonological_awareness: primary beneficiary (powerless/trapped) — the population the coordination function most clearly serves
 *   - classroom_teachers_professional_judgment: primary target (moderate/constrained) — bears the extraction as loss of pedagogical discretion
 *   - curriculum_publishers_of_scripted_phonics_programs: secondary beneficiary (organized/arbitrage) — captures procurement rents from mandate specificity
 *   - reading_science_researchers: agenda_setter (institutional/analytical) — supplies the evidentiary warrant the mandate cites
 *   - state_education_departments: agenda_setter (institutional/arbitrage) — writes and enforces compliance
 *   - advanced_early_readers_held_to_scripted_pace: secondary victim (powerless/trapped) — pays a differentiation cost the coordination function does not price in
 *   - whole_language_and_balanced_literacy_practitioners: excluded — displaced without consultation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.62).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phonics-First Reading Instruction Mandate (Decoding Precedes Comprehension)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, 'f1ffadcf-d519-419f-a46e-d2f691d1c81e').
narrative_ontology:cs_kernel_codification('f1ffadcf-d519-419f-a46e-d2f691d1c81e', formalized).
narrative_ontology:cs_authority_grounding('f1ffadcf-d519-419f-a46e-d2f691d1c81e', expertise).
narrative_ontology:cs_interpretation_layer_present('f1ffadcf-d519-419f-a46e-d2f691d1c81e').
narrative_ontology:cs_reading_relation('f1ffadcf-d519-419f-a46e-d2f691d1c81e', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('f1ffadcf-d519-419f-a46e-d2f691d1c81e', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('f1ffadcf-d519-419f-a46e-d2f691d1c81e', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('f1ffadcf-d519-419f-a46e-d2f691d1c81e', foundational, decoding_is_prerequisite_gate_for_comprehension).
narrative_ontology:cs_axiom_status(decoding_is_prerequisite_gate_for_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('f1ffadcf-d519-419f-a46e-d2f691d1c81e', decoding_is_prerequisite_gate_for_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('f1ffadcf-d519-419f-a46e-d2f691d1c81e', foundational, explicit_instruction_required_before_connected_text).
narrative_ontology:cs_axiom_status(explicit_instruction_required_before_connected_text, holdable).
narrative_ontology:cs_axiom_grounding('f1ffadcf-d519-419f-a46e-d2f691d1c81e', explicit_instruction_required_before_connected_text, empirically_contingent).
narrative_ontology:cs_reference_frame('f1ffadcf-d519-419f-a46e-d2f691d1c81e', national_reading_panel_evidentiary_consensus).
narrative_ontology:cs_drift_state('f1ffadcf-d519-419f-a46e-d2f691d1c81e', post_science_of_reading_legislation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f1ffadcf-d519-419f-a46e-d2f691d1c81e', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_of_scripted_phonics_programs).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers_professional_judgment).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, advanced_early_readers_held_to_scripted_pace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot self-teach grapheme-phoneme correspondence through context guessing alone; without explicit, systematic decoding instruction they plateau or fail to acquire the alphabetic principle, with cascading effects on later comprehension and self-concept as readers. Systematic phonics before connected-text exposure closes their most common failure pathway.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, national).

% Sell scripted, sequenced phonics curricula and the accompanying assessments, training, and coaching contracts that districts adopt to comply with phonics mandates. Legislative mandates requiring 'the science of reading' create a captive procurement market; publishers lobby for continued mandate specificity.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_of_scripted_phonics_programs, beneficiary,
    organized, generational, arbitrage, national).

% Required to deliver scripted phonics lessons in fixed sequence and pacing regardless of what their day-to-day observation of individual students tells them about readiness, interest, or need for enrichment versus remediation. Deviating from the script during observation windows can trigger corrective action; leaving the profession or the district is the primary exit, not adapting the constraint from within.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, classroom_teachers_professional_judgment, payer,
    moderate, biographical, constrained, regional).

% Already decode fluently and are ready for connected text and meaning-focused work, but must proceed through the same fixed phoneme-grapheme sequence as struggling peers because the program is scripted for fidelity, not differentiated by mastery. Their exit options are effectively nonexistent within the assigned classroom.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, advanced_early_readers_held_to_scripted_pace, payer,
    powerless, biographical, trapped, local).

% Produce and cite the converging cognitive-science evidence (National Reading Panel, meta-analyses of decoding instruction) that grounds legislative phonics mandates. Their findings are the kernel's evidentiary basis and they benefit professionally and institutionally from the mandate's continued citation of their work, though they do not administer classrooms directly.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_science_researchers, agenda_setter,
    institutional, generational, analytical, national).

% Write and enforce 'science of reading' legislation mandating explicit phonics sequencing, approve curricula, and audit district compliance. They can revise the mandate's specificity and enforcement intensity but currently have strong political incentive to maintain visible, auditable compliance.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, state_education_departments, agenda_setter,
    institutional, generational, arbitrage, regional).

% Hold professional training and pedagogical commitments built around meaning-first or balanced approaches; under phonics-mandate legislation their prior training is treated as invalidated and their classroom practice is displaced. They are rarely consulted in mandate design despite bearing its retraining costs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, whole_language_and_balanced_literacy_practitioners, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates instructional sequencing across a school system so that every student receives systematic exposure to phoneme-grapheme correspondence before being expected to derive meaning from connected text, closing the most common and best-documented failure pathway in early reading acquisition (weak decoding skills masked or compensated for by guessing strategies until comprehension collapses in later grades).
% TRANSFER_FUNCTION: Moves instructional-design authority and day-to-day pacing discretion from individual classroom teachers to curriculum publishers and state-mandated scripted programs; moves procurement dollars from districts to publishers of phonics-sequenced materials and their associated training/assessment products; moves reduced decoding-failure risk to previously underserved students, particularly those with weak phonological awareness.
% ABSENT_VOICES: Whole-language and balanced-literacy trained teachers, and researchers within those traditions, are structurally excluded from mandate design once 'science of reading' legislation codifies phonics-first sequencing as the compliance standard; their objections about motivation, meaning-making, and differentiated pacing are treated as settled rather than live.
% DISAPPEARANCE_RATIONALE: Reading-science proponents argue that removing the mandate would rearrange the world substantially — decoding-failure rates would rise again as districts drift back to context-guessing approaches, particularly harming students with weak phonological awareness who lack home-based scaffolding. Teachers and balanced-literacy advocates argue the underlying skill-building would largely continue informally through professional judgment and that only the compliance apparatus (scripted fidelity monitoring, publisher contracts) would vanish, leaving actual student outcomes roughly unchanged.
% FOUNDING_PROBLEM: Large-scale, well-replicated cognitive-science findings (converging across the National Reading Panel review and subsequent meta-analyses) established that a substantial fraction of children do not spontaneously induce the alphabetic principle from context and meaning cues alone, and that systematic phonics instruction reduces reading failure rates, especially for children with weak phonological awareness who had previously been mislabeled as unteachable or slow under meaning-first approaches.
% FOUNDING_PROBLEM_CORROBORATION: Independent cognitive-science researchers outside the curriculum-publishing industry (university-based reading scientists, replication studies across multiple countries) corroborate that the decoding-failure problem was real and substantial. However, teachers' unions and independent literacy scholars outside the phonics-publishing ecosystem corroborate a *different* claim: that the problem was never 'no explicit phonics' but rather 'no systematic phonics at all', and that the current scripted-fidelity mandate over-solves the original problem by also displacing teacher judgment the original research never addressed.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, contested).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high) because the reading's coordination function is genuine and well-evidenced for the target population (weak phonological awareness students) — this keeps ε well below a pure snare reading — but the scripted-fidelity enforcement layer that has grown around the mandate extracts substantially from teacher judgment and from advanced students' differentiated pacing, which the founding cognitive-science evidence never specifically required. Suppression (0.62) is higher than extraction because compliance monitoring (walkthrough audits, fidelity checklists, corrective action for deviation) is an active, escalating enforcement mechanism independent of scope. Theater ratio rises over the interval (0.10 to 0.28) as districts increasingly invest in visible compliance documentation (fidelity logs, walkthrough forms) that measures adherence to script rather than student decoding outcomes directly — a Goodhart-style substitution of proxy for function.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with weak phonological awareness sit near the beneficiary end: the constraint's core function (systematic decoding instruction before connected text) directly closes their dominant failure mode, and they have no independent path to acquire this skill without it (trapped, but the constraint subsidizes rather than extracts). Curriculum publishers are structural beneficiaries via captured procurement. Teachers are targets: moderate power, constrained exit (leaving the district or profession, not adapting the constraint), and the extraction (loss of pacing/method discretion) runs directly through the same structure that produces the coordination benefit — this is precisely the tangled-rope signature. Advanced early readers held to uniform pacing are a secondary victim group the coordination story does not price in.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (systematic explanation of the alphabetic principle for children who do not induce it from context) remains largely live per independent cognitive-science corroboration — this blocks a pure mandatrophy/piton verdict. But the enforcement layer built on top of that founding evidence (scripted fidelity monitoring, walkthrough audits, corrective action for pedagogical deviation) is not clearly warranted by the same evidence base, and its persistence looks more like institutional risk-aversion and publisher-contract lock-in than a response to a live coordination need. The tangled_rope classification captures this: genuine coordination function plus enforcement-driven extraction riding on the same structure, rather than either pure Mountain (natural, unchallengeable) or pure Snare (no coordination function at all).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sequencing_versus_integration_ambiguity,
    'Does the phonics_reading premise (decoding must precede connected-text exposure) represent a distinct pedagogical claim from structured_literacy_reading''s integrated multi-component sequence, or is ''phonics_reading'' simply an underspecified subset of structured literacy that collapses into it once phonological awareness and fluency components are added?',
    'Comparative curriculum analysis: identify phonics-first programs that explicitly sequence decoding before ANY connected text exposure (strict reading) versus programs that integrate phonics instruction alongside decodable connected text from early sessions (weaker reading closer to structured_literacy_reading or balanced_literacy_reading).',
    'If the strict sequencing claim is rarely implemented in pure form, this reading may describe a smaller and more contested population of actual programs than the mandate language suggests, which would lower confidence in the extraction attribution to ''phonics mandates'' broadly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sequencing_versus_integration_ambiguity, conceptual, 'Whether phonics_reading is a genuinely distinct kernel reading or an underspecified variant of structured_literacy_reading.').

omega_variable(
    mandate_versus_founding_evidence_gap,
    'Is the scripted-fidelity enforcement apparatus (walkthrough audits, corrective action for pedagogical deviation) actually supported by the cognitive-science evidence base the mandate cites, or is it an administrative layer added independently that has attached itself to the mandate''s legitimacy without independent evidentiary warrant?',
    'Trace the citation chain from state ''science of reading'' legislation back to the specific studies cited; determine whether any cited study evaluated scripted-fidelity monitoring itself (versus evaluating systematic phonics content) as a variable affecting student outcomes.',
    'If the enforcement layer has no independent evidentiary support, that widens the tangled-rope''s extraction component (teacher-judgment displacement is ''excess'' extraction beyond what closing the founding problem required) — this bears directly on the mandatrophy_analysis finding that not-live enforcement components ride on a still-live core function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_versus_founding_evidence_gap, empirical, 'Whether the compliance-monitoring apparatus is independently evidence-supported or a captured add-on.').

omega_variable(
    beneficiary_population_boundary,
    'What proportion of the general student population genuinely requires explicit systematic phonics-before-text sequencing (versus benefiting equally from balanced or integrated approaches), and does mandating the strict sequence for ALL students constitute extraction from students who would have acquired decoding through less rigid approaches?',
    'Meta-analytic subgroup analysis comparing outcomes for students with strong versus weak phonological awareness under strict phonics-first sequencing versus balanced/integrated approaches.',
    'If the benefit is concentrated in a phonologically-weak subgroup while advanced early readers experience net cost from uniform pacing, this supports treating universal mandate application (versus targeted intervention) as the extractive component, narrowing the coordination-function justification to a subset of the mandated population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_population_boundary, empirical, 'Whether the coordination benefit generalizes to the full mandated population or is concentrated in a subgroup.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lite_tr_t4, literacy_acquisition_kernel__phonics_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__phonics_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__phonics_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__phonics_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lite_be_t4, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lite_su_t4, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of literacy_acquisition_kernel, each authored as a separate story with its own ε per the ε-invariance principle: phonics_reading (this story, ε=0.58, tangled_rope — coordination via systematic decoding instruction, extraction via scripted-fidelity enforcement on teacher judgment), whole_language_reading (expected low enforcement/extraction on teachers but high extraction on students with weak phonological awareness who fail to acquire decoding without explicit instruction), balanced_literacy_reading (expected lower extraction on both fronts — hybrid, no rigid sequencing mandate), and structured_literacy_reading (expected higher θ formalization, Orton-Gillingham lineage, tighter cs_structure kernel_codification). All four are linked bidirectionally so contamination/purity analysis can propagate across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
