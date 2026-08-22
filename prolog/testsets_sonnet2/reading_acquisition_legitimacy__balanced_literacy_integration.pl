% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Balanced Literacy Integration Reading (Reading Acquisition Legitimacy Kernel)
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Balanced literacy emerged as the negotiated peace treaty of the reading
 *   wars, holding that legitimate instruction must include both explicit
 *   phonics and authentic literature exposure, with teachers toggling between
 *   direct instruction and facilitation. This story authors that specific
 *   reading of the reading-acquisition-legitimacy kernel: a mixed structure
 *   combining decodable and authentic texts, where struggling readers receive
 *   both phonics intervention and guided reading. The coordination function
 *   is real — it ends a costly pedagogical war and lets districts adopt a
 *   single defensible framework — but the same structure that coordinates
 *   districts, publishers, and credentialing bodies around one label
 *   systematically underserves the population (struggling decoders, dyslexic
 *   students, ELLs) whose needs are least compatible with a 50/50 time-split
 *   default. As national reading science consensus (the 'science of reading'
 *   movement) has hardened around the primacy of systematic phonics for
 *   foundational decoding, the balanced label has increasingly functioned as
 *   a way to preserve meaning-first practices (leveled texts, three-cueing,
 *   guided reading time) from full evidentiary displacement — hence the
 *   rising theater_ratio: an increasing share of 'balance' is
 *   professional-development performance and framework-labeling rather than
 *   diagnostically targeted instruction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.42).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.38).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration Reading (Reading Acquisition Legitimacy Kernel)").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, 'aec1f02f-121b-4bfc-bcd4-29d3615e06b2').
narrative_ontology:cs_kernel_codification('aec1f02f-121b-4bfc-bcd4-29d3615e06b2', distributed).
narrative_ontology:cs_authority_grounding('aec1f02f-121b-4bfc-bcd4-29d3615e06b2', practice).
narrative_ontology:cs_interpretation_layer_present('aec1f02f-121b-4bfc-bcd4-29d3615e06b2').
narrative_ontology:cs_reading_relation('aec1f02f-121b-4bfc-bcd4-29d3615e06b2', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('aec1f02f-121b-4bfc-bcd4-29d3615e06b2', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_axiom('aec1f02f-121b-4bfc-bcd4-29d3615e06b2', foundational, decoding_and_meaning_making_are_co_equal_components).
narrative_ontology:cs_axiom_status(decoding_and_meaning_making_are_co_equal_components, holdable).
narrative_ontology:cs_axiom_grounding('aec1f02f-121b-4bfc-bcd4-29d3615e06b2', decoding_and_meaning_making_are_co_equal_components, empirically_contingent).
narrative_ontology:cs_axiom('aec1f02f-121b-4bfc-bcd4-29d3615e06b2', secondary, reading_wars_resolved_by_synthesis_not_adjudication).
narrative_ontology:cs_axiom_status(reading_wars_resolved_by_synthesis_not_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('aec1f02f-121b-4bfc-bcd4-29d3615e06b2', reading_wars_resolved_by_synthesis_not_adjudication, conventional).
narrative_ontology:cs_reference_frame('aec1f02f-121b-4bfc-bcd4-29d3615e06b2', reading_wars_negotiated_synthesis).
narrative_ontology:cs_drift_state('aec1f02f-121b-4bfc-bcd4-29d3615e06b2', post_science_of_reading_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aec1f02f-121b-4bfc-bcd4-29d3615e06b2', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_consultants_and_coaches).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, schools_of_education_balanced_faculty).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, average_and_above_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers_with_decoding_deficits).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers_under_dual_mandate).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, english_language_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopts balanced literacy frameworks and mandates that teachers use both phonics blocks and guided reading/leveled-text workshops. Sets curriculum, procures materials, evaluates teachers against a mixed-methods rubric, and can revise the mandate when political or empirical pressure mounts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, district_curriculum_offices, agenda_setter,
    institutional, generational, arbitrage, regional).

% Sell leveled-reader libraries, guided-reading kits, and companion phonics supplements as an integrated package. Revenue depends on districts maintaining the dual-component mandate; can rebrand offerings if research consensus shifts without existential risk to the business.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_curriculum_publishers, beneficiary,
    organized, biographical, arbitrage, national).

% Deliver professional development on 'balancing' phonics and authentic text exposure, interpret ambiguous mandate language for schools, and are retained precisely because the balance is not self-executing. Their livelihood depends on the mandate remaining contestable enough to require ongoing coaching.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_consultants_and_coaches, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_consultants_and_coaches, agenda_setter).

% Must deliver both a systematic phonics scope-and-sequence AND a workshop/guided-reading rotation within the same instructional day, often without adequate time, training, or diagnostic tools to know which component a given child needs more of. Absorbs the cost of reconciling two pedagogies the mandate declares compatible but does not operationally integrate.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers_under_dual_mandate, payer,
    moderate, immediate, constrained, local).

% Receive phonics instruction diluted by time spent on meaning-focused workshop activities that presuppose decoding skill they do not yet have; guided reading with leveled texts often permits picture-cueing and context-guessing strategies that mask rather than remediate decoding gaps. Cannot select their own instructional model and depend entirely on which teacher, school, or year they land in for how much explicit phonics they actually receive.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers_with_decoding_deficits, payer,
    powerless, biographical, trapped, local).

% Require intensive, systematic, cumulative phonics instruction with minimal ambiguity; the balanced model's default allocation of instructional time to authentic-text immersion is actively counter-indicated for this population, and identification often lags because early struggle is attributed to normal developmental variation within the balanced framework's expectations.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Face compounded decoding and vocabulary/background-knowledge gaps; the balanced model's assumption that meaning-making will scaffold decoding presumes English oral language proficiency these students are still building, so the 'meaning' half of the balance is less available to them than to native speakers.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, english_language_learners, payer,
    powerless, biographical, trapped, local).

% Have sufficient decoding automaticity that the balanced model's emphasis on authentic literature and self-selected reading serves them well; they benefit from the enrichment and motivation components without being harmed by the reduced systematicity of the phonics component, since their decoding was never the bottleneck.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, average_and_above_readers, beneficiary,
    moderate, biographical, mobile, local).

% Have built certification programs, tenure cases, and decades of published scholarship around balanced literacy as the professionally respectable middle path between 'reductive' phonics-only and 'irresponsible' whole-language approaches. Their institutional identity and credentialing authority are invested in the balance framing being correct.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, schools_of_education_balanced_faculty, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, schools_of_education_balanced_faculty, agenda_setter).

% Cognitive scientists studying decoding acquisition (the 'science of reading' evidence base) argue the balanced model's core premise — that systematic phonics and unstructured meaning-exposure are equally weighted, mutually reinforcing components — is not supported by convergent evidence on how the reading brain acquires the alphabetic code. They publish and testify but have limited direct control over district curriculum adoption cycles.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, reading_science_researchers, excluded,
    organized, generational, analytical, national).

% Observe their children not progressing and seek explanation or private tutoring/remediation, often only discovering the systematic-phonics deficit outside the school system. Have limited institutional standing to challenge curriculum adoption directly except through school board advocacy or litigation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers, excluded,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, professionally defensible instructional stance that lets districts adopt one curriculum, train teachers on one framework, and avoid choosing between two politically and academically contested camps — genuinely coordinates purchasing, training, and evaluation around one label.
% TRANSFER_FUNCTION: Moves instructional time, curricular budget, and professional legitimacy toward publishers, consultants, and balanced-literacy-credentialed faculty; moves remediation cost and lost developmental time onto struggling readers, dyslexic students, and ELLs whose decoding needs are underserved by the mandate's default time allocation, and onto teachers who must reconcile incompatible pedagogical logics without adequate diagnostic support.
% ABSENT_VOICES: Reading science researchers and parents of struggling readers are structurally outside the curriculum-adoption conversation, which runs through district offices, publishers, and schools-of-education faculty; their evidence and lived outcomes surface mainly after adoption, through test scores, private tutoring markets, or advocacy campaigns, not through the adoption process itself.
% DISAPPEARANCE_RATIONALE: If the balanced-literacy mandate disappeared overnight, districts would have to choose or construct an alternative framework; curriculum publishers built around the dual-package model would need to restructure offerings, consultants whose expertise is 'balancing' would need new positioning, schools of education would face pressure to revise certification coursework, and instructional time currently split between two components would be reallocated — likely toward whichever single framework research or political pressure favored.
% FOUNDING_PROBLEM: The reading wars of the 1980s-90s pitted phonics-only instruction (criticized as joyless, decontextualized drilling that produced fluent decoders who disliked reading and struggled with comprehension) against whole-language instruction (criticized as failing systematic decoders); balanced literacy was built to end the wars by claiming both camps were partially right and packaging both components into one framework.
% FOUNDING_PROBLEM_CORROBORATION: District curriculum offices and schools of education attest the balance framework resolved the reading wars and remains the responsible middle path. Independent cognitive science researchers (outside the balanced-literacy beneficiary set) and structured-literacy advocacy organizations attest that national and state reading assessment data show the founding problem was not resolved but re-packaged — decoding deficits persist at scale, particularly among low-income, ELL, and dyslexic populations — and that the balance framing has functioned to shield the whole-language-descended meaning-making component from the evidentiary challenge systematic phonics research poses.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42) rather than high because balanced literacy genuinely does provide phonics instruction to all students, unlike a pure whole-language regime — but it siphons instructional time and institutional legitimacy away from the systematic, diagnostic intensity that decoding-deficit populations require, while curriculum publishers and consultancy markets around 'balance' both profit from and help sustain that diffusion. Suppression is moderate and rising (0.22→0.38) reflecting increasing institutional resistance (certification requirements, PD mandates, curriculum adoption cycles) to fully displacing the meaning-making component even as evidence mounts against equal-weighting. Theater ratio rises sharply (0.25→0.48) because a growing share of 'balanced' practice has become defensive relabeling of unchanged three-cueing and leveled-reading practices under phonics-friendly branding, rather than genuine integration.
 *
 * DIRECTIONALITY LOGIC:
 *   District curriculum offices, publishers, consultants, and schools-of-education faculty sit near the beneficiary end: the balanced framing gives them a stable, non-falsifiable-in-practice product to sell, teach, and defend, and 'balance' is elastic enough to absorb criticism from either side without requiring wholesale restructuring. Average-and-above readers are incidental beneficiaries — their decoding was never the bottleneck, so the meaning-making component is pure upside. Struggling readers, dyslexic students, and ELLs sit at the target end: trapped exit options (they cannot choose their school's curriculum), and the structural harm is a foreseeable, documented consequence of time-allocation choices the framework makes on their behalf without diagnostic triage. Classroom teachers are payers in a different register — they absorb the coordination cost of reconciling two pedagogies within one class period without being either the source or beneficiary of the mandate.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure Snare (there is genuine coordination value — ending the reading wars, giving districts one adoptable framework, providing phonics to some degree to all students) or as pure Rope (it requires active enforcement via mandated curriculum adoption, credentialing requirements, and PD cycles, and it has identifiable victims whose harm is not incidental but a structural consequence of the framework's default time-allocation). Tangled Rope captures both: coordination is real, extraction is real, and the same mechanism produces both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_as_genuine_synthesis_or_strategic_ambiguity,
    'Is balanced literacy a genuine empirical synthesis of decoding and meaning-making research, or a strategically ambiguous label that lets whole-language-descended practices survive under phonics-friendly branding without being falsifiable against the science-of-reading evidence base?',
    'Comparative outcome studies tracking decoding automaticity and comprehension in balanced-literacy classrooms with fidelity-audited implementation versus structured-literacy classrooms, controlling for instructional time actually spent on systematic phonics versus three-cueing/leveled-text practice.',
    'If balance is genuine synthesis with fidelity, extraction is lower and the tangled_rope classification would weight toward the rope pole; if balance functions primarily as strategic ambiguity preserving contested practices, the classification should weight further toward snare, particularly for the struggling-reader and dyslexic-student populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_as_genuine_synthesis_or_strategic_ambiguity, empirical, 'Whether the balanced label reflects genuine integration or strategic ambiguity shielding contested practice.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (balanced_literacy_integration) of the reading_acquisition_legitimacy kernel, alongside phonics_decoding_primacy, structured_literacy_remediation, and whole_language_meaning_primacy. Which reading a district adopts is itself a contested, non-neutral choice — what determines which reading a given school system treats as authoritative?',
    'Track curriculum adoption decisions against the institutional actors involved (state legislation mandating science-of-reading approaches vs. district-level curriculum offices with balanced-literacy-trained leadership vs. schools-of-education certification requirements) to identify which lever actually moves reading policy.',
    'If state legislative mandates (increasingly favoring phonics_decoding_primacy or structured_literacy_remediation readings) override district-level balanced-literacy adoption, this constraint''s real-world instantiation may be narrowing even as its institutional entrenchment (publisher contracts, certification programs) persists — a piton-adjacent trajectory worth tracking in later revisions of this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'How kernel-reading selection is actually determined across institutional levels, and what that implies for this reading''s future trajectory.').

omega_variable(
    diagnostic_triage_feasibility,
    'Could the balanced model be restructured so that diagnostic assessment determines each student''s actual phonics-versus-meaning-making time allocation, rather than applying a uniform default split to all students regardless of decoding status?',
    'Pilot programs comparing uniform-balance classrooms to diagnostically-triaged classrooms (where students below a decoding-automaticity threshold receive intensive systematic phonics while others receive a more meaning-weighted mix) on reading outcomes across ability levels.',
    'If diagnostic triage is feasible and effective, the coordination function could be preserved while sharply reducing the extraction from struggling readers, dyslexic students, and ELLs — suggesting the extraction is a fixable implementation defect rather than an inherent feature of balancing per se.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_triage_feasibility, preference, 'Whether uniform time-allocation is an inherent feature of the balanced approach or a correctable implementation choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.25).
narrative_ontology:measurement(read_tr_t6, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 6, 0.32).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 12, 0.38).
narrative_ontology:measurement(read_tr_t18, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 18, 0.42).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 24, 0.46).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(read_be_t6, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(read_be_t18, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 18, 0.39).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(read_su_t6, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 6, 0.26).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(read_su_t18, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 18, 0.33).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__balanced_literacy_integration, 0.1).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the reading_acquisition_legitimacy kernel (balanced_literacy_integration, phonics_decoding_primacy, whole_language_meaning_primacy, structured_literacy_remediation). Each reading is authored as a separate constraint story with its own epsilon, beneficiary/victim structure, and claimed_type, per the ε-invariance principle: measuring 'legitimate reading instruction' by decoding-outcome fidelity versus by comprehension/engagement outcomes versus by vulnerable-learner remediation outcomes yields structurally different constraints, not one constraint viewed three ways. This file's ε (0.42) and tangled_rope claim describe only the balanced-integration reading's own operation and beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
