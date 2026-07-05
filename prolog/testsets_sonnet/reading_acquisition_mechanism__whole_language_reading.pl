% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__whole_language_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Doctrine
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the whole-language reading of the contested
 *   reading-acquisition kernel: the claim that decoding skill emerges
 *   implicitly from meaningful exposure to authentic texts, without need for
 *   a systematic, explicit grapheme-phoneme instructional sequence. It is
 *   written as its own constraint with its own stable epsilon, distinct from
 *   the phonics reading (explicit systematic instruction as foundational) and
 *   the balanced-literacy reading (integration of both). The three readings
 *   are siblings in one kernel family, not three measurements of one
 *   constraint — each has a different beneficiary/victim structure and a
 *   different persistence mechanism, so each gets its own file per the
 *   ε-invariance principle. Structurally: the approach imposes low upfront
 *   instructional-design cost (no systematic sequence to build or scaffold)
 *   but produces a rising tail of remediation cost concentrated on children
 *   whose home environments or neurology do not supply the implicit exposure
 *   the theory assumes, while granting substantial autonomy to classroom
 *   teachers and curriculum developers who are insulated from downstream
 *   outcomes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.58).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.52).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Doctrine").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, 'a2baf6c7-929b-4e8b-95e0-bbd52747c685').
narrative_ontology:cs_kernel_codification('a2baf6c7-929b-4e8b-95e0-bbd52747c685', distributed).
narrative_ontology:cs_authority_grounding('a2baf6c7-929b-4e8b-95e0-bbd52747c685', practice).
narrative_ontology:cs_interpretation_layer_present('a2baf6c7-929b-4e8b-95e0-bbd52747c685').
narrative_ontology:cs_reading_relation('a2baf6c7-929b-4e8b-95e0-bbd52747c685', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2baf6c7-929b-4e8b-95e0-bbd52747c685', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('a2baf6c7-929b-4e8b-95e0-bbd52747c685', foundational, decoding_emerges_implicitly_from_meaningful_exposure).
narrative_ontology:cs_axiom_status(decoding_emerges_implicitly_from_meaningful_exposure, holdable).
narrative_ontology:cs_axiom_grounding('a2baf6c7-929b-4e8b-95e0-bbd52747c685', decoding_emerges_implicitly_from_meaningful_exposure, empirically_contingent).
narrative_ontology:cs_axiom('a2baf6c7-929b-4e8b-95e0-bbd52747c685', secondary, explicit_systematic_sequence_unnecessary_for_foundational_literacy).
narrative_ontology:cs_axiom_status(explicit_systematic_sequence_unnecessary_for_foundational_literacy, holdable).
narrative_ontology:cs_axiom_grounding('a2baf6c7-929b-4e8b-95e0-bbd52747c685', explicit_systematic_sequence_unnecessary_for_foundational_literacy, empirically_contingent).
narrative_ontology:cs_reference_frame('a2baf6c7-929b-4e8b-95e0-bbd52747c685', meaning_centered_literature_immersion).
narrative_ontology:cs_drift_state('a2baf6c7-929b-4e8b-95e0-bbd52747c685', post_national_literacy_score_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a2baf6c7-929b-4e8b-95e0-bbd52747c685', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, teacher_colleges_and_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_seeking_autonomy).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, fluent_readers_from_print_rich_homes).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers_without_phonemic_awareness).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, children_from_low_print_exposure_households).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, students_with_dyslexia_and_related_disorders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_seeking_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design teacher-preparation curricula and instructional materials around meaning-first, exposure-based literacy theory. Train successive cohorts of teachers in this model, sell leveled-reader systems and professional development aligned to it, and defend the model in professional literature. Bear little direct accountability when downstream reading scores fall, since remediation is diagnosed and funded elsewhere.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, teacher_colleges_and_curriculum_publishers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, teacher_colleges_and_curriculum_publishers, beneficiary).

% Trained under the model, they find it requires less rigid sequencing and grants latitude to build curriculum around chosen texts and classroom judgment rather than scripted phonics drills. Some later confront struggling readers they were not trained to diagnose or remediate, absorbing personal professional strain and self-doubt when the approach does not work for a portion of their students.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_seeking_autonomy, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, classroom_teachers_seeking_autonomy, payer).

% Enter school already primed by dense home literacy exposure; for these children implicit exposure-based instruction is sufficient scaffolding on top of skills largely acquired outside the classroom. Their reading trajectories look like confirmation that the method works.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, fluent_readers_from_print_rich_homes, beneficiary,
    moderate, biographical, mobile, local).

% Lack the implicit pattern-extraction capacity the method assumes; without explicit grapheme-phoneme instruction they do not spontaneously decode. They fall behind, are often referred to remediation years later once the gap has compounded, and have no say in the instructional model applied to them.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers_without_phonemic_awareness, payer,
    powerless, biographical, trapped, local).

% Enter school without the incidental exposure to text and phonological patterning that the method quietly assumes as background knowledge. The gap between them and print-rich peers widens under an approach that treats decoding as something that will simply emerge.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, children_from_low_print_exposure_households, payer,
    powerless, biographical, trapped, local).

% Have a neurological profile that makes implicit decoding acquisition especially unlikely without direct, systematic, explicit instruction. Under exposure-based pedagogy their difficulty is frequently misread as a motivation or exposure problem rather than a disorder requiring targeted intervention, delaying diagnosis and remediation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, students_with_dyslexia_and_related_disorders, payer,
    powerless, biographical, trapped, local).

% Cognitive scientists and reading researchers whose converging experimental evidence on phonological processing and decoding acquisition sits outside the teacher-training and curriculum-publishing pipeline that adopted whole language pedagogy. Their findings are cited in policy fights but were largely absent from the professional consensus that installed this model in schools of education.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, reading_science_researchers, excluded,
    organized, generational, analytical, national).

% Set curriculum standards and increasingly mandate 'evidence-based reading instruction' following documented literacy score declines, reviewing testimony, longitudinal outcome data, and cost data on remediation programs to decide whether to require structural change.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, state_education_agencies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__whole_language_reading, teacher_colleges_and_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates classroom practice and teacher preparation around a shared theory that treats reading as acquired the way oral language is acquired — through rich, motivating, meaning-centered engagement with real texts — reducing the need for scripted, sequenced skill drills and standardizing what 'good literacy teaching' looks like across a teacher-training pipeline.
% TRANSFER_FUNCTION: Moves instructional-design cost from curriculum developers and teacher trainers (low upfront cost: no systematic phonics sequence to build, sequence, or assess) onto struggling readers and their families later, in the form of remediation cost, delayed diagnosis, and compounded academic disadvantage that must be paid for by special education systems, tutoring markets, and the affected children's own trajectories.
% ABSENT_VOICES: Reading science researchers whose experimental findings on phonological awareness and decoding were not centrally incorporated into the teacher-education pipeline that adopted this model; parents of children who fell behind under the approach and later sought outside diagnosis; the children themselves, who have no voice in curriculum adoption decisions made years before their own reading difficulties surface.
% DISAPPEARANCE_RATIONALE: If exposure-based, meaning-first pedagogy vanished from teacher training and classroom practice overnight, curriculum publishers would need to rebuild materials around systematic phonics sequences, teacher-training programs would need to retool coursework, and struggling readers currently misdiagnosed as 'not yet ready' would instead be identified early for explicit decoding intervention — a substantial institutional and financial rearrangement, not a null event.
% FOUNDING_PROBLEM: Mid-20th-century reading instruction (heavy phonics drilling divorced from meaningful text) was criticized as tedious, decontextualized, and disconnected from children's motivation to read for meaning; whole language pedagogy was built to solve the problem of joyless, meaning-starved reading instruction by centering authentic literature and treating decoding as a byproduct of engaged reading.
% FOUNDING_PROBLEM_CORROBORATION: Teacher colleges and literacy curriculum publishers attest the founding problem (joyless, meaning-disconnected drilling) remains a live risk and that meaning-centered engagement is still necessary. Cognitive scientists and reading researchers, plus state agencies reviewing national literacy score data, attest instead that the empirical premise underlying the solution — that decoding emerges implicitly from exposure for most learners — was never well supported and that the founding problem has been effectively supplanted by a different, worse problem: large cohorts of non-decoding readers. This corroboration comes from outside the beneficiary set (independent reading-science literature and state assessment data), not from the teacher-training institutions that adopted the model.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.32) reflecting genuine coordination value in early adoption — a real alternative to joyless rote phonics drilling existed and had a legitimate motivational case — but rises over the interval (to 0.58) as longitudinal outcome data accumulated showing a persistent subgroup of non-decoding readers whose difficulty the model's own theory could not diagnose. Theater ratio rises in parallel (0.20 to 0.44) as the professional discourse increasingly defends the approach rhetorically ('the reading wars,' balanced framing claims) even as its core mechanism claim faces mounting disconfirmation. Suppression is moderate rather than severe (0.52) because the constraint operates through institutional path-dependency in teacher training and publisher lock-in rather than through overt coercion — no one is legally barred from teaching phonics, but the credentialing and materials pipeline makes deviation costly for an individual teacher.
 *
 * DIRECTIONALITY LOGIC:
 *   Teacher colleges and curriculum publishers sit at the beneficiary end: they built and sell the pipeline and bear no direct cost when outcomes diverge by subgroup. Classroom teachers are secondary beneficiaries of professional autonomy but partly become payers when they must personally absorb the strain of students who are not thriving under a model they were trained to trust. Fluent readers from print-rich homes are beneficiaries almost by accident — the model's implicit-exposure assumption happens to be satisfied by their home environment, so the constraint's actual mechanism does no work for them; they would likely learn to read under nearly any pedagogy. Struggling readers, low-print-exposure children, and students with dyslexia are the structural targets: they are the population for whom the model's core empirical assumption (implicit acquisition from exposure) is false, and they bear the compounding cost of delayed diagnosis with no exit — a child cannot select their own first-grade pedagogy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — joyless, decontextualized phonics drilling disconnected from motivation to read for meaning — was arguably real and worth solving. That gives the tangled_rope claim its coordination leg: there was a genuine problem, and meaning-centered engagement is not worthless. But the founding problem's status is now contested rather than clearly live, per the six-questions genealogy: reading science corroboration external to the beneficiary set indicates the empirical premise underlying the solution (implicit decoding acquisition from exposure) was not well supported even at adoption, and the accumulating remediation cost is not merely residual friction but a structural byproduct of applying the model universally regardless of a child's need for explicit instruction. Classifying this as tangled_rope rather than snare respects that a coordination function existed and continues to exist for a subpopulation (print-rich, higher phonological-awareness children); classifying it as tangled_rope rather than rope respects that the model requires active institutional enforcement (through teacher credentialing and curriculum adoption) to persist against the countervailing weight of reading science, and that it has clear, identifiable victims who bear a cost the beneficiaries do not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_acquisition_empirical_status,
    'Is the claim that decoding skill emerges implicitly from exposure to authentic texts empirically supported for the general child population, or only for a subpopulation with strong pre-existing phonological awareness and print exposure?',
    'Meta-analytic reading-science literature comparing longitudinal decoding outcomes across matched cohorts taught with exposure-based versus explicit phonics instruction, controlling for home literacy environment and phonological processing baseline.',
    'If implicit acquisition holds only for an advantaged subpopulation, the constraint''s coordination claim collapses to that subpopulation and the extraction on the remainder is structural rather than incidental — strengthening the tangled_rope reading toward snare for the affected group. If implicit acquisition is genuinely general, the extraction measured here may instead reflect implementation failure rather than a false mechanism claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_acquisition_empirical_status, empirical, 'Whether the implicit-acquisition mechanism is population-general or subpopulation-specific.').

omega_variable(
    kernel_reading_relationship_ambiguity,
    'Does this reading forecloses, coexist with, or merely influence the phonics_reading and balanced_literacy_reading siblings, given that balanced_literacy explicitly incorporates elements this reading treats as unnecessary?',
    'Analyze whether a single teacher-training framework could simultaneously hold whole_language_reading''s core premise (decoding emerges implicitly, explicit instruction is not foundational) and balanced_literacy_reading''s core premise (both explicit phonics and authentic exposure are jointly required) without internal contradiction.',
    'If whole_language''s premise and balanced_literacy''s premise cannot be held in the same framework, the relationship should be forecloses rather than coexists_with, which would change how contamination propagation analysis treats the family. This story treats the relationship as coexists_with/influences because different institutional actors (different states, different districts) hold each reading as live policy without a single arbitrating framework, but this is a conceptual judgment call documented here rather than resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship_ambiguity, conceptual, 'Whether whole_language and balanced_literacy are logically compatible within one framework or mutually foreclosing.').

omega_variable(
    remediation_cost_attribution,
    'How much of the observed downstream remediation cost (special education referrals, tutoring markets, grade retention) should be attributed causally to the whole-language mechanism specifically, versus other confounding factors (poverty, school funding, teacher quality variance)?',
    'Natural experiments from jurisdictions that mandated a switch to systematic phonics instruction (e.g. state-level ''right to read'' legislation), tracking pre/post remediation referral rates with demographic controls held constant.',
    'A strong causal attribution would sharpen the victim declarations and justify a higher extractiveness score; a weak attribution would suggest this story currently overstates the mechanism''s causal role relative to socioeconomic confounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_cost_attribution, empirical, 'Causal weight of the pedagogy itself versus confounding socioeconomic factors in downstream remediation cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(read_be_t8, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(read_be_t16, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(read_be_t32, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(read_su_t8, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(read_su_t16, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(read_su_t32, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__whole_language_reading, 0.1).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial 'reading wars' debate into structurally distinct constraints sharing a kernel (reading_acquisition_mechanism). whole_language_reading claims decoding emerges implicitly from authentic-text exposure with no need for systematic sequence; phonics_reading claims explicit systematic grapheme-phoneme instruction is foundational and necessary; balanced_literacy_reading claims both are jointly required. Each has a distinct beneficiary/victim structure and a distinct epsilon; they are linked here rather than merged because measuring 'the reading acquisition debate' as a single constraint would conflate three different extraction profiles and three different victim populations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
