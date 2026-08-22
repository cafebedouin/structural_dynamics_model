% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__balanced_literacy_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading of Reading Acquisition Mechanism
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   Balanced literacy positions itself as the reasonable synthesis in the
 *   reading wars: teach explicit phonics AND immerse children in authentic
 *   literature, in integrated practice, so that neither camp's insight is
 *   lost. This story evaluates that specific reading of the
 *   reading-acquisition kernel — not the phonics-only reading and not the
 *   whole-language reading, which are separate constraints. The
 *   distinguishing empirical problem with the balanced-literacy reading is
 *   implementation drift: because the label does not specify a required
 *   proportion or sequencing of phonics instruction, and because publisher
 *   programs and teacher-training pipelines carry strong institutional
 *   inertia toward meaning-first methods (three-cueing, context-guessing,
 *   leveled texts), the 'integration' frequently collapses toward
 *   whole-language in classroom-time allocation even where phonics is
 *   nominally present on paper.
 *
 * KEY AGENTS:
 *   - balanced_literacy_publishers: Primary beneficiary (organized/arbitrage) — sells the compromise as a coherent program suite
 *   - district_curriculum_administrators: Beneficiary/agenda_setter (institutional/constrained) — adopts and defends the framework institutionally
 *   - teacher_college_faculty: Beneficiary (institutional/identity_locked) — professional identity built on the meaning-first tradition the label preserves
 *   - struggling_decoders and dyslexic_students: Primary targets (powerless/trapped) — bear the cost of diluted systematic phonics
 *   - independent_reading_researchers: Analytical observer — documents the gap between the stated and delivered proportions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.52).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.4).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Reading of Reading Acquisition Mechanism").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, '2f5fd1f2-5a17-43ff-8750-573ec5a218ce').
narrative_ontology:cs_kernel_codification('2f5fd1f2-5a17-43ff-8750-573ec5a218ce', distributed).
narrative_ontology:cs_authority_grounding('2f5fd1f2-5a17-43ff-8750-573ec5a218ce', practice).
narrative_ontology:cs_interpretation_layer_present('2f5fd1f2-5a17-43ff-8750-573ec5a218ce').
narrative_ontology:cs_reading_relation('2f5fd1f2-5a17-43ff-8750-573ec5a218ce', reading_acquisition_mechanism__phonics_reading, influences).
narrative_ontology:cs_reading_relation('2f5fd1f2-5a17-43ff-8750-573ec5a218ce', reading_acquisition_mechanism__whole_language_reading, influences).
narrative_ontology:cs_axiom('2f5fd1f2-5a17-43ff-8750-573ec5a218ce', foundational, integration_of_code_and_meaning_is_necessary_and_sufficient).
narrative_ontology:cs_axiom_status(integration_of_code_and_meaning_is_necessary_and_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('2f5fd1f2-5a17-43ff-8750-573ec5a218ce', integration_of_code_and_meaning_is_necessary_and_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('2f5fd1f2-5a17-43ff-8750-573ec5a218ce', secondary, proportion_and_sequencing_of_components_need_not_be_specified).
narrative_ontology:cs_axiom_status(proportion_and_sequencing_of_components_need_not_be_specified, overridden).
narrative_ontology:cs_axiom_grounding('2f5fd1f2-5a17-43ff-8750-573ec5a218ce', proportion_and_sequencing_of_components_need_not_be_specified, instrumental).
narrative_ontology:cs_reference_frame('2f5fd1f2-5a17-43ff-8750-573ec5a218ce', reading_wars_synthesis_compromise).
narrative_ontology:cs_drift_state('2f5fd1f2-5a17-43ff-8750-573ec5a218ce', post_science_of_reading_movement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2f5fd1f2-5a17-43ff-8750-573ec5a218ce', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, district_curriculum_administrators).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_college_faculty).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_decoders).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, low_income_district_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce leveled-reader systems, guided-reading kits, and professional development packages sold as the 'balanced' compromise between phonics-only and whole-language extremes. Revenue depends on districts adopting comprehensive program suites rather than standalone systematic phonics curricula. Can pivot marketing language when political winds shift toward 'science of reading' without changing underlying pedagogy much.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_publishers, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_publishers, agenda_setter).

% Select the reading program board-wide and defend the choice publicly. Balanced literacy framing lets them claim to have 'both' phonics and authentic literature without committing to the more rigid, harder-to-market systematic phonics scope-and-sequence. Face reputational cost if they admit a prior adoption failed.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, district_curriculum_administrators, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, district_curriculum_administrators, agenda_setter).

% Built careers and academic reputations on whole-language-descended, meaning-first literacy theory. Balanced literacy lets them retain the intellectual framework and course content they teach while nominally incorporating phonics, avoiding wholesale repudiation of decades of published scholarship and training materials.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_college_faculty, beneficiary,
    institutional, generational, identity_locked, national).

% Implement whatever program the district adopts, often with contradictory training: told to teach systematic phonics on Monday and use three-cueing/guessing strategies from context and pictures on Tuesday. Bear the confusion and workload of reconciling incompatible methods inside one classroom, with little say over curriculum choice.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, beneficiary).

% Children without strong context-guessing skills or high phonemic awareness who need explicit, systematic, cumulative phonics instruction to decode. In practice-diluted balanced literacy classrooms, phonics instruction is inconsistent and often crowded out by leveled-text guessing strategies, leaving them without either firm decoding skills or the print-rich immersion that would help stronger students.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_decoders, payer,
    powerless, biographical, trapped, local).

% Require the most explicit, structured, and intensive phonics instruction of any subgroup. When balanced literacy collapses toward its whole-language component in actual classroom time allocation, these students lose the one intervention shown to close their gap, and are instead pushed toward compensatory guessing strategies that mask rather than remediate the deficit.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Lack the home literacy environment (parental read-aloud time, book access, informal phonemic exposure) that lets more advantaged peers survive an under-systematic phonics program on ambient exposure alone. Depend most heavily on the school being the sole source of explicit decoding instruction — exactly the component most likely to be diluted under implementation drift.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, low_income_district_students, payer,
    powerless, generational, trapped, regional).

% Cognitive scientists, reading researchers, and parent advocacy groups citing decades of converging evidence (simple view of reading, National Reading Panel, meta-analyses) that systematic phonics is necessary and that three-cueing/guessing strategies actively impede decoding automaticity. Frequently locked out of curriculum-adoption committees dominated by literacy faculty and publisher relationships.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, science_of_reading_advocates, excluded,
    organized, biographical, constrained, national).

% Conduct longitudinal and experimental studies on decoding acquisition across program types, largely outside the commercial curriculum ecosystem. Document the frequent gap between balanced literacy's stated integration and its observed classroom-time allocation, which skews toward meaning-first strategies.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, independent_reading_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its ideal form, balanced literacy solves a genuine problem: neither pure decoding drills nor pure immersion in authentic text alone reliably produces fluent, motivated readers; combining explicit code instruction with rich text exposure could plausibly serve both the mechanics and the motivation/comprehension sides of reading acquisition.
% TRANSFER_FUNCTION: Moves instructional time, teacher training investment, and curriculum-adoption budgets toward publisher program suites and toward literacy faculty's preferred meaning-first frameworks, and moves decoding-skill outcomes away from the students who most need systematic phonics — the promised integration is frequently not delivered in the proportions the label implies.
% ABSENT_VOICES: Science-of-reading researchers and parents of struggling readers are frequently excluded from curriculum-adoption committees, which are often composed of administrators and literacy-faculty-trained coordinators with institutional and career investment in the status quo framework.
% DISAPPEARANCE_RATIONALE: Publishers and literacy faculty would argue that abandoning the balanced-literacy label collapses a genuine, defensible integration; science-of-reading advocates and many classroom teachers would argue nothing of instructional value disappears because the label rarely constrains actual classroom time allocation now, and removing it would simply force honest naming of whichever component (usually meaning-first) is actually being taught.
% FOUNDING_PROBLEM: The 1980s-90s 'reading wars' produced two entrenched camps (phonics-first and whole-language) each with partisans and each with real evidentiary gaps in practice; balanced literacy was proposed as a synthesis that could end the wars by claiming to honor both traditions' valid insights.
% FOUNDING_PROBLEM_CORROBORATION: Independent reading researchers and science-of-reading advocacy organizations (outside the publisher/literacy-faculty benefiting group) attest that the 'synthesis' is frequently nominal — classroom observation studies and time-allocation audits from outside the balanced-literacy publishing ecosystem report that systematic phonics is often under-implemented relative to its stated share, meaning the founding problem (ending the wars via genuine integration) is not solved so much as relabeled.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, contested).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) rather than severe because the coordination function is genuine when balanced literacy is actually delivered as specified — the extraction arises specifically from implementation drift, not from the integrated-practice premise itself being false. Theater ratio is high and rising (0.30 to 0.58) because an increasing share of what is marketed and defended as 'balanced' instruction is, on time-allocation audits, actually meaning-first strategy dressed in phonics-compliant language for adoption committees and accountability reporting. Suppression is moderate (0.40) — there is no coercive suppression of alternatives in the legal sense, but curriculum-adoption gatekeeping by literacy faculty and administrators functions as a softer suppression of the systematic-phonics-first alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers, district administrators, and literacy faculty are declared beneficiaries because the balanced-literacy label protects their existing revenue, institutional choices, and professional identity respectively — the derivation gives them low d. Struggling decoders, dyslexic students, and low-income district students are declared victims with trapped exit options and powerless power atoms — the derivation gives them high d, amplified by their inability to select an alternative program or supplement instruction privately. Classroom teachers sit in between: they benefit from having a defensible, adoptable framework to teach under, but pay the cost of reconciling contradictory methods with no say in the underlying choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending the phonics-vs-whole-language wars via genuine synthesis) is contested rather than dead: for districts that implement balanced literacy with a real, well-sequenced phonics component, the founding problem is substantially live and being addressed. The mandatrophy risk is that the LABEL survives and is defended long after the delivered PRACTICE has drifted toward whole-language, at which point the constraint is doing extraction (protecting publisher and faculty positions) under cover of a coordination story it no longer performs. Classifying this as tangled_rope rather than snare or rope preserves that distinction: the coordination function is real in principle and in some implementations, and the extraction is real and asymmetric in practice — collapsing either into the other would misclassify the many districts where implementation fidelity is genuinely high alongside the many where it is not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_vs_label,
    'Does the extraction measured here reflect a structural flaw in the balanced-literacy reading itself, or a fidelity-of-implementation problem separable from the reading''s core claim?',
    'Classroom-level time-allocation audits comparing high-fidelity balanced-literacy implementations (verified systematic phonics scope-and-sequence delivered as specified) against low-fidelity ones, tracking decoding outcomes for struggling readers in each.',
    'If high-fidelity implementations show outcomes comparable to systematic phonics programs, the extraction is an implementation artifact and this reading is closer to a genuine rope than a tangled rope. If even high-fidelity implementations underperform, the balanced-literacy reading''s synthesis premise itself is structurally weaker than claimed, strengthening the case for the phonics_reading as sole necessary condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_vs_label, empirical, 'Whether extraction traces to implementation drift or to the reading''s structural claim.').

omega_variable(
    committer_structure_ambiguity,
    'Is the reading-acquisition kernel genuinely under one committer framework with three coexisting readings, or does the balanced-literacy reading function mainly as an institutional compromise position adopted BECAUSE it avoids forecloding either sibling — i.e., is ''balance'' itself doing political rather than pedagogical work?',
    'Trace curriculum-adoption committee deliberations and publisher marketing history to determine whether balanced literacy was adopted for evidentiary reasons (synthesizing genuinely complementary mechanisms) or for institutional-conflict-avoidance reasons (splitting the difference between entrenched camps without resolving the underlying empirical question).',
    'If adopted primarily for conflict-avoidance, the balanced-literacy reading''s coordination function is weaker than authored here and the constraint tilts further toward snare; if adopted on genuine synthesis grounds with drift as the separate failure mode, tangled_rope with an implementation-fidelity omega (above) is the accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_ambiguity, conceptual, 'Whether the balanced-literacy reading''s synthesis premise is evidentiary or institutionally strategic.').

omega_variable(
    beneficiary_capture_of_evidence_base,
    'To what extent does the balanced-literacy publishing and teacher-education ecosystem shape which studies get funded, cited, and adopted into policy, versus the independent reading-research evidence base cited by science-of-reading advocates?',
    'Funding-source and citation-network analysis of literacy efficacy studies used in curriculum-adoption decisions, cross-referenced against publisher and teacher-college-affiliated authorship.',
    'High capture would strengthen the case that teacher_college_faculty and balanced_literacy_publishers function as an entrenched beneficiary coalition resistant to correction regardless of outcome data; low capture would suggest the persistence of implementation drift is inertia rather than active interest-protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_evidence_base, empirical, 'Whether the evidence base informing adoption decisions is captured by beneficiary institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(read_tr_t6, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement(read_tr_t18, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 18, 0.5).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(read_be_t6, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(read_be_t12, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(read_be_t18, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 18, 0.49).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(read_be_t30, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(read_su_t6, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 6, 0.32).
narrative_ontology:measurement(read_su_t12, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(read_su_t18, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 18, 0.38).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(read_su_t30, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__balanced_literacy_reading, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_reading).

% DUAL FORMULATION NOTE:
% These three stories decompose the natural-language 'reading wars' / balanced literacy debate per the ε-invariance principle: phonics_reading models the claim that systematic phonics is foundational and necessary (contested but with strong convergent evidence — lower ε, closer to rope/mountain-adjacent depending on framing); whole_language_reading models the claim that decoding emerges implicitly from authentic text exposure (weakly supported empirically, higher ε where implemented as a stand-alone method); balanced_literacy_reading (this story) models the compromise claim, whose defining structural feature is that its actual delivered ε depends heavily on implementation fidelity, and whose institutional beneficiaries have incentive to claim fidelity is high regardless of classroom reality. All three share the same underlying kernel (what mechanism produces reading acquisition) but are structurally distinct constraints with different beneficiary/victim sets and different ε — they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
