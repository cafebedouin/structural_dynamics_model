% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading of the Literacy Acquisition Kernel
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   Balanced literacy positions itself as the synthesis reading of the
 *   literacy acquisition kernel: phonics and meaningful text engagement are
 *   complementary, not opposed, and the correct instructional design blends
 *   both. This reading emerged historically as an exit from the
 *   phonics-vs-whole-language reading wars. Its structural risk is that
 *   'balance' is under-specified — it names a real coordination problem (both
 *   components matter) without a fixed, falsifiable answer to how much
 *   systematic phonics time is required, which leaves room for the label to
 *   be applied to programs that are whole-language descended in substance.
 *   The rising theater_ratio reflects the observed pattern: balance-branded
 *   curricula proliferate and PD consulting around 'balance' grows, while
 *   independent evidence on classroom-level phonics fidelity under balanced
 *   programs remains thin.
 *
 * KEY AGENTS:
 *   - balanced_literacy_curriculum_publishers: beneficiary/agenda_setter — sells the synthesis label as product line
 *   - schools_of_education_balanced_faculty: agenda_setter/beneficiary — institutional legitimacy tied to teaching balance as already-resolved synthesis
 *   - classroom_teachers_under_conflicting_mandates: payer — absorbs coordination cost of two historically rival pedagogies
 *   - struggling_readers_undiagnosed_dyslexia: payer — bears the cost if balance under-delivers systematic phonics
 *   - reading_researchers_cognitive_science: excluded — evidence cited selectively, not consulted on implementation fidelity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.48).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.42).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading of the Literacy Acquisition Kernel").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, 'efbbcd98-1f72-4d8e-b6bd-49630587279b').
narrative_ontology:cs_kernel_codification('efbbcd98-1f72-4d8e-b6bd-49630587279b', distributed).
narrative_ontology:cs_authority_grounding('efbbcd98-1f72-4d8e-b6bd-49630587279b', distributed).
narrative_ontology:cs_reading_relation('efbbcd98-1f72-4d8e-b6bd-49630587279b', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('efbbcd98-1f72-4d8e-b6bd-49630587279b', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_reading_relation('efbbcd98-1f72-4d8e-b6bd-49630587279b', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_axiom('efbbcd98-1f72-4d8e-b6bd-49630587279b', foundational, phonics_and_meaning_engagement_are_complementary_not_sequential).
narrative_ontology:cs_axiom_status(phonics_and_meaning_engagement_are_complementary_not_sequential, holdable).
narrative_ontology:cs_axiom_grounding('efbbcd98-1f72-4d8e-b6bd-49630587279b', phonics_and_meaning_engagement_are_complementary_not_sequential, empirically_contingent).
narrative_ontology:cs_axiom('efbbcd98-1f72-4d8e-b6bd-49630587279b', secondary, instructional_proportion_should_be_teacher_judged_not_fixed_dosage).
narrative_ontology:cs_axiom_status(instructional_proportion_should_be_teacher_judged_not_fixed_dosage, holdable).
narrative_ontology:cs_axiom_grounding('efbbcd98-1f72-4d8e-b6bd-49630587279b', instructional_proportion_should_be_teacher_judged_not_fixed_dosage, instrumental).
narrative_ontology:cs_reference_frame('efbbcd98-1f72-4d8e-b6bd-49630587279b', post_reading_wars_synthesis_consensus).
narrative_ontology:cs_drift_state('efbbcd98-1f72-4d8e-b6bd-49630587279b', post_science_of_reading_legislative_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('efbbcd98-1f72-4d8e-b6bd-49630587279b', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, balanced_literacy_curriculum_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, schools_of_education_balanced_faculty).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, professional_development_consultants).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers_undiagnosed_dyslexia).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers_under_conflicting_mandates).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, low_income_district_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and sell leveled-text kits, guided-reading materials, and 'balanced' curricula marketed as the synthesis position. Revenue depends on periodic adoption cycles; when districts churn between reading wars positions, publishers sell new materials either way, so the balanced label is commercially safe regardless of the underlying evidence dispute.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, balanced_literacy_curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, balanced_literacy_curriculum_publishers, agenda_setter).

% Trained several generations of teachers in whole-language-descended, meaning-first methods and now teach 'balance' as a face-saving middle path. Their institutional legitimacy and tenure lines are built on the pedagogy they already teach; abandoning it for a structured-literacy overhaul would mean admitting decades of teacher preparation under-served decoding instruction.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, schools_of_education_balanced_faculty, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, schools_of_education_balanced_faculty, beneficiary).

% Sell workshops helping districts 'balance' their literacy blocks. Every new state mandate or curriculum controversy creates fresh consulting demand; the ambiguity of what balance concretely requires in classroom minutes is a feature of their market, not a bug to be resolved.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, professional_development_consultants, beneficiary,
    organized, biographical, arbitrage, national).

% Told to deliver both systematic phonics blocks and rich meaning-focused reading time within a fixed instructional day, often with materials that were not designed together. Absorb the coordination cost of reconciling two pedagogies that were developed as rivals, with little planning time and career risk if their students' scores lag either camp's benchmark.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers_under_conflicting_mandates, payer,
    moderate, biographical, constrained, local).

% Need explicit, systematic, cumulative decoding instruction most urgently, and are the population for whom a diluted 'balance' that under-delivers structured phonics time is most costly. Cannot select their own instructional method and often are identified as struggling only after the balanced approach has already run its course for a year or more.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers_undiagnosed_dyslexia, payer,
    powerless, biographical, trapped, local).

% Attend districts with the least resources to properly implement both pillars of a balanced approach simultaneously — insufficient decodable-text inventory, insufficient PD hours, insufficient small-group time — so 'balance' in practice tilts toward whichever pillar is cheaper to deliver at scale, usually the meaning-focused, less-materials-intensive side.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, low_income_district_students, payer,
    powerless, biographical, trapped, regional).

% Set literacy standards and reading-legislation requirements, commissioning reviews and testifying in curriculum-adoption hearings. Increasingly cite structured-literacy legislation ('reading laws') as evidence that balanced framings underdeliver on phonics fidelity, without always having independent classroom-implementation data.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, state_education_agencies, observer,
    institutional, generational, analytical, national).

% Publish converging evidence (simple view of reading, meta-analyses of phonics effect sizes) that is frequently cited by all camps but rarely drives curriculum-adoption decisions directly; their findings are invoked selectively by publishers and consultants to brand competing products as 'balanced' or 'science of reading' aligned without independent verification of classroom fidelity.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, reading_researchers_cognitive_science, excluded,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__balanced_literacy_reading, diffuse).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Names a real coordination problem: reading instruction genuinely requires both decoding skill and meaningful engagement with connected text, and a purely phonics-only or purely meaning-only program that ignores the other component leaves gaps. A synthesis framing that gets both pillars right, well-sequenced and well-resourced, is a coherent solution to a real pedagogical problem.
% TRANSFER_FUNCTION: Moves curriculum-adoption budgets and professional-development spending from districts to publishers and consultants under the 'balance' label; moves instructional time and teacher planning effort into reconciling two historically rival methodologies; moves risk of under-delivered systematic phonics onto the students least able to compensate for it, especially those with reading difficulties or fewer out-of-school literacy supports.
% ABSENT_VOICES: Reading researchers whose meta-analytic work most directly bears on how much explicit phonics time is actually needed are cited selectively rather than consulted on implementation fidelity; parents of struggling readers are rarely present in curriculum-adoption decisions until after a diagnosis has already cost a child a year or more of appropriate instruction.
% DISAPPEARANCE_RATIONALE: If the balanced-literacy framing disappeared overnight, curriculum adoption would have to choose openly between a structured-literacy-descended systematic-phonics-first sequence and something closer to the older whole-language emphasis — the 'balance' label currently lets districts avoid that choice by claiming to have already made it. Publishers would need to rebrand or discontinue balance-branded product lines and PD consultants who monetize the balance gap would lose a market.
% FOUNDING_PROBLEM: The reading wars of the 1980s-1990s pitted phonics-first and whole-language camps against each other in ways that felt politically and pedagogically unproductive; balanced literacy was built to offer educators and policymakers an exit from that binary — a framing under which both camps' insights could be honored without anyone having to concede the fight.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive scientists working from the simple view of reading and National Reading Panel-descended meta-analyses attest that the 1990s binary was a genuine false dichotomy in principle, corroborating that a real synthesis problem exists. However, independent literacy researchers and state legislators behind the 2013-present 'reading laws' movement attest that balanced literacy as actually implemented in most districts under-delivers systematic, cumulative phonics instruction relative to what the science-of-reading synthesis would require — i.e., that the label solved the political problem without solving the instructional one. No corroboration exists from outside the publisher/PD-consultant/ed-school complex that 'balance' in practice, as opposed to in principle, reliably delivers the systematic phonics component.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) rather than high because the coordination function is genuine — reading acquisition really does require both decoding and meaning-making, and a well-implemented balance is a coherent instructional design, not a pure cover story. But extraction is non-trivial because the label's vagueness has commercial value independent of implementation quality: publishers and consultants profit from the ambiguity of what 'balance' concretely requires, and the theater_ratio's rise over the interval (0.30 to 0.55) tracks growing rebranding activity (balance-labeled materials proliferating) relative to growth in verified fidelity outcomes. Suppression (0.42) is moderate: no one is coercively barred from adopting structured literacy instead, but the balance framing does dominate teacher-preparation pipelines and state curriculum-adoption defaults, narrowing what a new teacher or a resource-constrained district encounters as the default option.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (publishers, ed-school faculty), balanced literacy is a legitimate scientific synthesis that resolved an unproductive binary. From the payer seats (teachers reconciling incompatible materials, struggling readers who needed more systematic phonics time than they got), the same arrangement operates as extraction: cost absorbed without corresponding delivery of the decoding-instruction component the synthesis promised. The engine's per-seat computation should reflect this asymmetry from the declared power/exit data, not from any story-level adjudication of which side is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers and PD consultants sit near the beneficiary end: they collect adoption and workshop revenue regardless of implementation fidelity, and their exit options are arbitrage-grade (pivot to whichever label sells). Ed-school faculty are beneficiaries but identity-locked rather than mobile — their exit is blocked by professional identity investment in decades of teacher preparation built on meaning-first assumptions, not by market mobility. Classroom teachers are moderate-power payers with constrained exit — they can advocate but cannot unilaterally redesign the instructional day. Struggling readers and low-income district students are powerless, trapped payers bearing the sharpest cost if the phonics component the label promises is under-delivered in the setting they specifically depend on.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (an unproductive phonics-vs-whole-language binary) was genuinely live in the 1980s-1990s and cognitive science has since converged on a synthesis view (the simple view of reading) that does resolve the binary IN PRINCIPLE. The mandatrophy risk is that the balance label persists as institutional and commercial infrastructure even where implementation has drifted back toward its whole-language ancestry — the corpus classification should not treat 'balance' as automatically vindicated by the fact that a genuine synthesis exists in the research literature; the coordination function and the implementation fidelity are separable questions, and this story's tangled_rope claim holds them apart rather than assuming the label's success from its premise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_vs_whole_language_rebrand,
    'Is balanced literacy a structurally distinct third reading of the literacy acquisition kernel, or is it whole-language pedagogy with a phonics-acknowledging label attached for political and commercial cover?',
    'Classroom-level implementation audits measuring actual minutes of systematic, cumulative phonics instruction delivered under balance-branded curricula, compared against structured_literacy''s explicit dosage benchmarks and against pre-balance whole-language classrooms; convergence with structured_literacy dosage would support genuine synthesis, convergence with legacy whole-language minutes would support rebrand.',
    'If rebrand, this reading''s coordination-function claim collapses and the constraint moves toward snare (the coordination story is cover for the same extraction whole_language_reading already represents); if genuine synthesis, the tangled_rope classification holds with the extraction concentrated in commercial and institutional overhead rather than in the pedagogy itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_whole_language_rebrand, empirical, 'Whether balanced literacy is structurally distinct from whole language or a relabeling.').

omega_variable(
    balance_underspecification_as_feature_or_bug,
    'Is the lack of a fixed, falsifiable dosage requirement for systematic phonics within ''balance'' an oversight that could be corrected, or is under-specification structurally load-bearing for the label''s commercial and political function?',
    'Track whether balanced-literacy advocacy organizations and publishers adopt a specific minimum phonics-minutes standard when pressed by state reading legislation, versus continuing to resist quantification.',
    'If the field resists quantification even under legislative pressure, that resistance itself is evidence the ambiguity is functionally protective of the commercial and institutional beneficiaries named in this story, strengthening the tangled_rope reading over a benign-ambiguity account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_underspecification_as_feature_or_bug, conceptual, 'Whether balance''s lack of a fixed phonics-dosage standard is incidental or structurally protective of its beneficiaries.').

omega_variable(
    beneficiary_school_of_education_natural_law_framing_check,
    'Do schools of education present balanced literacy''s validity as an empirically settled scientific consensus (a natural-law-adjacent framing) in ways that occlude the contested implementation-fidelity question documented above?',
    'Review teacher-preparation program materials and accreditation standards for language that frames balance as scientifically settled versus language that flags the ongoing science-of-reading legislative contest.',
    'If ed-school framing treats balance as settled fact rather than contested synthesis, that supports treating the constraint''s persistence as partly reliant on suppressing the ongoing evidentiary dispute rather than resolving it — raising suppression and moving the classification away from pure rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_school_of_education_natural_law_framing_check, conceptual, 'Whether ed-school framing of balance as settled science occludes the contested implementation question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 25, 0.54).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 30, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(literacy_acquisition_kernel__balanced_literacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__balanced_literacy_reading, 0.1).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the literacy_acquisition_kernel. balanced_literacy_reading claims complementary synthesis of phonics and meaning-engagement; phonics_reading claims decoding-precedence without conceding balance's blended proportionality; structured_literacy_reading claims an explicit cumulative multi-component sequence with a fixed scope-and-sequence balance never commits to; whole_language_reading denies explicit phonics instruction is necessary at all. Each reading is authored as its own ε-invariant constraint with its own beneficiary/victim structure; this story's ε (0.48) reflects moderate extraction concentrated in commercial/institutional overhead around an ambiguous label, structurally distinct from phonics_reading's and structured_literacy_reading's likely lower ε (evidence-convergent, less brand-contested) and whole_language_reading's likely higher ε (evidence-divergent, direct victim harm to students denied explicit decoding instruction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
