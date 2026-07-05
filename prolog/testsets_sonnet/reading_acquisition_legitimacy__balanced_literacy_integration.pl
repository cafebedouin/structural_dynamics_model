% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__balanced_literacy_integration, []).

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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration — Mixed Phonics/Meaning-Making Reading Instruction
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Balanced literacy emerged as a professional compromise between two
 *   competing reading-instruction camps: phonics-first approaches that treat
 *   reading as decoding, made explicit through systematic instruction, and
 *   whole-language approaches that treat reading as meaning-making, acquired
 *   through immersion in authentic text. The balanced model asserts that
 *   legitimate instruction requires both components, mixing decodable texts
 *   with authentic literature and toggling teachers between direct
 *   instruction and facilitation. This story evaluates the
 *   balanced-integration reading of the reading_acquisition_legitimacy kernel
 *   specifically — not the phonics-primacy or whole-language-primacy
 *   readings, which are separate constraints with their own ε values and
 *   stakeholder structures. The balanced reading's central empirical
 *   vulnerability is that it may serve strong incidental learners well while
 *   under-serving struggling decoders and children from low-print-exposure
 *   homes, who need concentrated systematic instruction the mixed model
 *   dilutes.
 *
 * KEY AGENTS:
 *   - curriculum_publishers_balanced_literacy_lines: beneficiary (organized/arbitrage) — profits from continued hybrid-materials adoption
 *   - district_administrators_avoiding_pedagogy_wars: agenda_setter (institutional/constrained) — mandates the framework to avoid political exposure
 *   - teacher_colleges_maintaining_existing_syllabi: beneficiary/agenda_setter (institutional/constrained) — insulates existing training pipelines from overhaul
 *   - elementary_classroom_teachers: payer/agenda_setter (moderate/constrained) — absorbs the cost of running two instructional systems without diagnostic training
 *   - struggling_decoders_in_mixed_classrooms: payer (powerless/trapped) — bears the primary extraction, receiving diluted rather than concentrated phonics instruction
 *   - children_from_low_print_exposure_homes: payer (powerless/trapped) — disproportionately dependent on explicit instruction the mixed model under-delivers
 *   - reading_researchers_science_of_reading: excluded (organized/analytical) — evidence base is cited selectively but not given implementation authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.42).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.38).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration — Mixed Phonics/Meaning-Making Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, '8f583af1-453b-4710-b056-3951b33b81dc').
narrative_ontology:cs_kernel_codification('8f583af1-453b-4710-b056-3951b33b81dc', distributed).
narrative_ontology:cs_authority_grounding('8f583af1-453b-4710-b056-3951b33b81dc', practice).
narrative_ontology:cs_interpretation_layer_present('8f583af1-453b-4710-b056-3951b33b81dc').
narrative_ontology:cs_reading_relation('8f583af1-453b-4710-b056-3951b33b81dc', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('8f583af1-453b-4710-b056-3951b33b81dc', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('8f583af1-453b-4710-b056-3951b33b81dc', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('8f583af1-453b-4710-b056-3951b33b81dc', foundational, decoding_and_meaning_making_are_independently_necessary_and_concurrent).
narrative_ontology:cs_axiom_status(decoding_and_meaning_making_are_independently_necessary_and_concurrent, holdable).
narrative_ontology:cs_axiom_grounding('8f583af1-453b-4710-b056-3951b33b81dc', decoding_and_meaning_making_are_independently_necessary_and_concurrent, empirically_contingent).
narrative_ontology:cs_axiom('8f583af1-453b-4710-b056-3951b33b81dc', secondary, professional_synthesis_is_preferable_to_partisan_resolution).
narrative_ontology:cs_axiom_status(professional_synthesis_is_preferable_to_partisan_resolution, holdable).
narrative_ontology:cs_axiom_grounding('8f583af1-453b-4710-b056-3951b33b81dc', professional_synthesis_is_preferable_to_partisan_resolution, instrumental).
narrative_ontology:cs_reference_frame('8f583af1-453b-4710-b056-3951b33b81dc', reading_wars_professional_truce_1990s).
narrative_ontology:cs_drift_state('8f583af1-453b-4710-b056-3951b33b81dc', post_science_of_reading_consensus_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8f583af1-453b-4710-b056-3951b33b81dc', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_publishers_balanced_literacy_lines).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, district_administrators_avoiding_pedagogy_wars).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_colleges_maintaining_existing_syllabi).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, students_with_strong_incidental_decoding_skills).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_decoders_in_mixed_classrooms).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, children_from_low_print_exposure_homes).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, teachers_pressured_to_toggle_without_diagnostic_training).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, elementary_classroom_teachers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, reading_is_multicomponential).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, compromise_pedagogy_is_professionally_defensible).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell basal reading programs marketed as combining 'the best of both worlds' — decodable readers alongside leveled authentic texts. Revenue depends on districts continuing to purchase mixed-model materials rather than committing fully to either a structured-literacy scope-and-sequence or an immersive trade-book model, either of which would require different (and often cheaper, less proprietary) materials.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_publishers_balanced_literacy_lines, beneficiary,
    organized, generational, arbitrage, national).

% Mandate the balanced approach in curriculum guides and teacher evaluations, citing it as a professionally neutral middle path that avoids taking a side in a polarized public debate between phonics advocates and whole-language advocates. Administer training, monitor compliance, and could in principle move fully to structured literacy but bear reputational and political cost for appearing to concede either side of the debate was wrong.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, district_administrators_avoiding_pedagogy_wars, agenda_setter,
    institutional, biographical, constrained, regional).

% Have trained generations of teachers in balanced/whole-language-descended methods and would face costly faculty retraining and credential-program overhaul if the field moved decisively toward structured literacy. Continue certifying new teachers under frameworks that treat 'balance' as the settled, professionally mature position, insulating existing coursework from revision.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_colleges_maintaining_existing_syllabi, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_colleges_maintaining_existing_syllabi, agenda_setter).

% Are told to toggle between explicit phonics instruction and facilitated meaning-making guided reading, often without diagnostic training to know which mode a given struggling reader needs at a given moment. Absorb the cognitive and time cost of running two instructional systems in one classroom, and are blamed individually when struggling readers fall further behind under the mixed approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, elementary_classroom_teachers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, elementary_classroom_teachers, agenda_setter).

% Pick up the alphabetic code quickly regardless of instructional method, largely due to home literacy exposure prior to school. For these children, the mixed model's authentic-literature component adds genuine motivational and comprehension value, and the model looks, from their seat, like exactly the right balance.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, students_with_strong_incidental_decoding_skills, beneficiary,
    powerless, biographical, mobile, local).

% Cannot induct the alphabetic code from exposure to authentic text the way strong incidental learners can, and need dense, systematic, cumulative phonics instruction to establish decoding. In a toggling classroom they receive diluted, inconsistent explicit instruction interspersed with meaning-making activities they cannot yet access because they cannot decode the words in front of them. They have no exit from the classroom assignment their district makes for them.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_decoders_in_mixed_classrooms, payer,
    powerless, biographical, trapped, local).

% Enter school with less pre-existing exposure to book language, vocabulary, and print concepts than peers from high-print-exposure homes, and depend disproportionately on what school actually teaches explicitly. A balanced model that assumes some incidental uptake of decoding through authentic-text immersion under-serves exactly the children who most need instruction, not immersion, to acquire the code.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, children_from_low_print_exposure_homes, payer,
    powerless, biographical, trapped, local).

% Publish converging cognitive-science evidence that decoding acquisition benefits from systematic, explicit, sequenced phonics instruction for the majority of learners and especially for at-risk readers, and argue balanced literacy's 'meaning-making primacy for early decoding' component lacks empirical support. Their findings inform some district policy debates but are frequently filtered through compromise language that preserves existing balanced programs rather than replacing them outright.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, reading_researchers_science_of_reading, excluded,
    organized, generational, analytical, national).

% Track state-level reading proficiency data, review curriculum adoption lists, and produce comparative analyses of instructional approaches. They can recommend policy shifts and sometimes mandate structured-literacy components in law, but have limited authority over district-level classroom implementation and teacher-preparation programs.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, state_reading_policy_analysts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides teachers and districts a single instructional framework that avoids fully committing to either extreme of a genuinely contested pedagogical debate, allowing curriculum adoption, teacher training, and classroom practice to proceed under one coherent (if internally mixed) banner rather than fracturing into competing incompatible systems within the same school.
% TRANSFER_FUNCTION: Moves instructional time, curricular coherence, and diagnostic clarity away from struggling decoders (who need concentrated systematic phonics) toward a diffused mixed model that serves already-advantaged incidental learners well and serves at-risk learners poorly, while moving purchasing dollars toward publishers of hybrid materials and insulating teacher-preparation institutions from the cost of overhauling their programs.
% ABSENT_VOICES: Reading scientists whose research most directly implicates the balanced model's weakest component (assuming decoding can partly emerge from meaning exposure) are cited selectively in policy documents but rarely given authority to specify classroom-level diagnostic protocols; parents of struggling readers, who bear the consequence of delayed identification, are rarely present in curriculum-adoption decisions at all.
% DISAPPEARANCE_RATIONALE: If the balanced-literacy framework disappeared overnight, districts would be forced to choose an instructional model with a clearer internal logic — either fully systematic structured literacy or fully immersive whole language — publishers would need to redesign or discontinue hybrid product lines, teacher colleges would face pressure to revise syllabi around whichever model prevailed, and struggling decoders would either receive more concentrated phonics instruction (likely benefiting them) or lose it entirely (likely harming them), depending on which replacement model won out. The arrangement's disappearance would force exactly the reckoning it currently defers.
% FOUNDING_PROBLEM: The 1980s-2000s 'reading wars' pitted phonics-first and whole-language camps against each other with no resolution in sight; balanced literacy was built to give districts, publishers, and teacher-training institutions a professionally respectable synthesis that could end the public conflict and let curriculum adoption proceed without appearing to take a doctrinaire side.
% FOUNDING_PROBLEM_CORROBORATION: Reading science researchers outside the balanced-literacy publishing and teacher-training establishment (independent of curriculum vendors and colleges of education) attest that the founding conflict has been substantially resolved by converging cognitive-science evidence favoring systematic phonics for foundational decoding, and that balanced literacy persists less as a live synthesis and more as an institutionally convenient truce that avoids the retraining and materials costs a clear resolution would require. District administrators and teacher colleges — the parties most benefiting from the truce's continuation — dispute this and describe balanced literacy as the professionally mature, ongoing consensus position; no fully independent third party outside either camp corroborates the founding problem as fully live in its original 1980s form.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).
:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than severe, because the coordination function is genuine: balanced literacy did resolve an institutionally destructive public conflict and does provide real motivational value through authentic literature for children who can already decode. But extraction is rising over the measured interval as the science-of-reading evidence base has hardened against the model's weakest premise (that decoding can partly emerge from meaning exposure) while institutional adoption has not correspondingly narrowed. Theater ratio is comparatively high and rising (0.46) because much of the continued 'balance' framing in curriculum documents functions increasingly as institutional cover for avoiding costly teacher retraining and materials replacement, rather than as an evidence-driven instructional design choice. Suppression is moderate: no one is coercively barred from choosing structured literacy, but professional-consensus framing in teacher colleges and district guidance makes deviation carry real career and compliance friction for individual teachers.
 *
 * DIRECTIONALITY LOGIC:
 *   Curriculum publishers and teacher colleges sit near the beneficiary end: the mixed model preserves existing revenue lines and existing training investments without requiring costly redesign. District administrators are agenda-setters who could shift policy but bear political cost for doing so, giving them a genuine (if constrained) capacity to change the arrangement — distinguishing them from the payer seats who have no such capacity. Struggling decoders and children from low-print-exposure homes sit at the target end: trapped in classroom assignments they cannot choose, dependent on exactly the systematic instruction the balanced model dilutes in favor of toggling. Students who decode easily regardless of method are genuine beneficiaries — the authentic-literature component adds real value for them, which is why this reading is not a pure snare: there is a real population for whom the coordination story is also the truth.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is load-bearing here: the balanced model is not pure extraction because it genuinely served an original coordination function (ending a professionally destructive binary conflict) and continues to deliver real value to strong incidental learners. But it is not a pure rope either, because the coordination benefit is asymmetrically distributed — it favors already-advantaged learners while imposing a diagnostic and instructional cost specifically on the most vulnerable readers, and its persistence increasingly depends on institutional inertia (teacher-college syllabi, publisher product lines) rather than on continued evidentiary support. The founding_problem_status is authored as 'contested' rather than 'dead' precisely because reasonable professional disagreement about the balanced model's validity persists even as independent researchers increasingly regard its core premise as empirically undermined for a specific, identifiable subpopulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_as_synthesis_or_truce,
    'Is the balanced-literacy reading a genuine empirical synthesis (both components are independently necessary and mutually reinforcing) or an institutional truce that persists because no party wants to bear the cost of a clear resolution?',
    'Longitudinal comparison of reading outcomes for at-risk subpopulations under balanced instruction versus under structured-literacy remediation with matched authentic-literature exposure added afterward; if outcomes converge once systematic phonics is front-loaded and authentic literature is layered on later rather than concurrently, the ''balance'' framing is doing less independent work than claimed.',
    'If balance is a genuine synthesis, this reading''s coordination function is real and its extraction is closer to the coordination-cost floor. If it is an institutional truce, the tangled_rope classification understates the extraction and the story is closer to a snare riding on residual professional-consensus language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_as_synthesis_or_truce, empirical, 'Whether balanced literacy is an independently justified synthesis or a cost-avoiding institutional compromise.').

omega_variable(
    which_reading_kernel_disagreement_located_in,
    'Where exactly is the disagreement between this reading and its siblings located — is it about WHAT reading is (a definitional/conceptual dispute) or about WHEN each component should be introduced and in what sequence (a sequencing dispute)?',
    'Structured literacy_remediation and phonics_decoding_primacy readings could in principle be compatible with balanced_literacy_integration if the dispute is purely about sequencing (phonics first, then more balance later) rather than about whether meaning-making exposure is independently necessary during initial decoding acquisition. Compare the four readings'' authored axioms directly.',
    'If the disagreement is purely sequencing, the readings coexist_with each other and could in principle be synthesized into a time-phased protocol. If the disagreement is about the independent necessity of concurrent meaning-making exposure during decoding acquisition, the readings are more sharply opposed and this reading''s core premise is what the empirical evidence increasingly targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_kernel_disagreement_located_in, conceptual, 'Whether the kernel dispute is about sequencing or about the independent necessity of concurrent meaning-making instruction.').

omega_variable(
    beneficiary_population_size_uncertainty,
    'What proportion of the general student population are ''strong incidental decoders'' for whom the balanced model genuinely works well, versus ''struggling decoders'' for whom it under-serves?',
    'Population-level screening data (e.g., dyslexia-simulator studies, early literacy screening assessment results across large districts) estimating the proportion of children who need explicit systematic phonics instruction to acquire decoding versus those who acquire it with minimal explicit instruction.',
    'If the struggling-decoder population is a small minority, the tangled_rope classification''s extraction magnitude is modest and localized. If it approaches or exceeds a third of the population (as some science-of-reading advocates argue), the extraction is far more structurally significant than the ''balance serves most students'' framing suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_population_size_uncertainty, empirical, 'The size of the population for whom balanced literacy''s coordination story fails to hold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.2).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 8, 0.27).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 16, 0.33).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 24, 0.38).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 32, 0.43).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 40, 0.46).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 16, 0.34).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(read_be_t32, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(read_be_t40, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(read_su_t32, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(read_su_t40, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__balanced_literacy_integration, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the reading_acquisition_legitimacy kernel. balanced_literacy_integration (this story) claims a middle synthesis; phonics_decoding_primacy and structured_literacy_remediation both treat systematic explicit instruction as primary (the latter foregrounding the most vulnerable learners specifically); whole_language_meaning_primacy treats decoding as an emergent byproduct of meaning immersion. Each reading has a distinct ε, distinct beneficiary/victim structure, and distinct claimed_type — they are not the same constraint viewed from different angles but four structurally distinct claims about what legitimate reading instruction requires, linked via network edges rather than folded into one multi-valued story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
