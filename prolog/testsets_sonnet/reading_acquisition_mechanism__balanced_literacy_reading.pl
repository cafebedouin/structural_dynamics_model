% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Balanced Literacy Reading Instruction Framework
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the balanced literacy reading of the contested
 *   reading-acquisition kernel: the claim that reading acquisition requires
 *   BOTH explicit phonics AND authentic literature exposure in genuinely
 *   integrated practice. This is distinct from the phonics_reading (which
 *   locates the causal foundation in systematic grapheme-phoneme instruction
 *   alone) and the whole_language_reading (which claims decoding emerges
 *   implicitly from meaningful text exposure). Balanced literacy presents
 *   itself as the synthesis that resolves the reading wars, but its
 *   structural delta is that implementation fidelity varies enormously across
 *   districts and frequently collapses toward whole-language practice —
 *   retaining cueing-system prompts (picture, context, and syntax cues to
 *   guess words) under a phonics-labeled veneer — because the 'integration'
 *   is underspecified and easier to satisfy nominally than substantively. The
 *   metrics here describe the compromise framework's actual operation as
 *   implemented at scale, not the idealized synthesis its proponents
 *   describe.
 *
 * KEY AGENTS:
 *   - curriculum_publishers: Primary beneficiary (institutional/arbitrage) — sells the integrated product line
 *   - balanced_literacy_consultants: Agenda-setter (organized/mobile) — administers fidelity standards
 *   - schools_of_education_faculty: Beneficiary/co-agenda-setter (institutional/identity_locked) — pedagogical lineage preservation
 *   - classroom_teachers: Payer (moderate/constrained) — absorbs implementation contradiction
 *   - struggling_readers: Primary victim (powerless/trapped) — bears decoding-deficit cost
 *   - children_from_low_literacy_households: Compounding victim (powerless/trapped) — no supplemental exposure
 *   - cognitive_science_reading_researchers: Analytical observer (analytical/analytical) — documents implementation-outcome gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.52).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.38).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.34).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Reading Instruction Framework").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, '63ef810c-c645-4b3c-947d-8e27bd225f9a').
narrative_ontology:cs_kernel_codification('63ef810c-c645-4b3c-947d-8e27bd225f9a', distributed).
narrative_ontology:cs_authority_grounding('63ef810c-c645-4b3c-947d-8e27bd225f9a', practice).
narrative_ontology:cs_interpretation_layer_present('63ef810c-c645-4b3c-947d-8e27bd225f9a').
narrative_ontology:cs_reading_relation('63ef810c-c645-4b3c-947d-8e27bd225f9a', reading_acquisition_mechanism__phonics_reading, influences).
narrative_ontology:cs_reading_relation('63ef810c-c645-4b3c-947d-8e27bd225f9a', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('63ef810c-c645-4b3c-947d-8e27bd225f9a', foundational, decoding_and_meaning_making_are_co_foundational).
narrative_ontology:cs_axiom_status(decoding_and_meaning_making_are_co_foundational, holdable).
narrative_ontology:cs_axiom_grounding('63ef810c-c645-4b3c-947d-8e27bd225f9a', decoding_and_meaning_making_are_co_foundational, empirically_contingent).
narrative_ontology:cs_axiom('63ef810c-c645-4b3c-947d-8e27bd225f9a', secondary, integration_requires_no_single_dominant_method).
narrative_ontology:cs_axiom_status(integration_requires_no_single_dominant_method, holdable).
narrative_ontology:cs_axiom_grounding('63ef810c-c645-4b3c-947d-8e27bd225f9a', integration_requires_no_single_dominant_method, instrumental).
narrative_ontology:cs_reference_frame('63ef810c-c645-4b3c-947d-8e27bd225f9a', reading_wars_resolution_synthesis).
narrative_ontology:cs_drift_state('63ef810c-c645-4b3c-947d-8e27bd225f9a', post_science_of_reading_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('63ef810c-c645-4b3c-947d-8e27bd225f9a', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_consultants).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, schools_of_education_faculty).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, children_from_low_literacy_households).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, integrated_instruction_superiority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell integrated 'balanced literacy' curriculum packages, leveled-reader libraries, and companion professional development materials to districts. The compromise framing lets them market a large, ever-renewing product line (guided reading levels, three-cueing prompts, mini-lesson kits) that a purely phonics-first or purely whole-language curriculum would not require. They can pivot branding toward 'science of reading' language without changing the underlying product mix.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, national).

% Design and deliver the district-level professional development that operationalizes the 'both/and' doctrine in classrooms. Their livelihood depends on the framework requiring ongoing expert mediation to reconcile phonics scope-and-sequence with authentic-text immersion; if either pure approach won cleanly, less specialized facilitation would be needed. They set which practices count as sufficiently 'integrated' fidelity.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_consultants, agenda_setter,
    organized, biographical, mobile, national).

% Trained cohorts of teachers for decades in whole-language-descended, meaning-first pedagogy; the balanced literacy label lets many retain the bulk of that training and self-concept as literacy educators while nominally adding phonics components. Their professional identity and tenure cases are built on this pedagogical lineage, making wholesale abandonment of the framework a threat to institutional legitimacy, not just a curriculum change.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, schools_of_education_faculty, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, schools_of_education_faculty, agenda_setter).

% Are handed conflicting mandates: implement systematic phonics scope-and-sequence AND maintain rich authentic-text immersion, guided reading groups, and three-cueing prompting, often without additional instructional time or coherent training. When students fail to read, they absorb blame for 'implementation fidelity' failures even when the underlying framework's internal tensions make faithful joint implementation nearly impossible within a school day.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, regional).

% Depend on systematic, cumulative phonics instruction to build decoding automaticity; in variably-implemented balanced literacy classrooms they are frequently taught to guess words from pictures and context (residual whole-language cueing) rather than decode, delaying or preventing the acquisition of the phonics foundation the framework nominally requires. They have no ability to choose their instructional approach or diagnose why they are falling behind.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Lack home-based supplemental phonics exposure that print-rich, higher-resourced households can informally provide, making them disproportionately dependent on the school's explicit instruction actually being systematic. When implementation collapses toward whole-language practice, these children bear the largest downstream literacy gap because school is their only structured source of decoding instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, children_from_low_literacy_households, payer,
    powerless, generational, trapped, national).

% Observe their children struggling to decode and are told the school uses a 'balanced' evidence-based approach, but have little visibility into whether phonics instruction is actually systematic or vestigial. Few have the pedagogical expertise to audit classroom practice, and their concerns are frequently deflected as impatience rather than treated as evidence of implementation failure.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, parents_and_caregivers, excluded,
    powerless, biographical, constrained, local).

% Study the actual mechanisms of skilled reading acquisition (phonological awareness, orthographic mapping, comprehension) independent of any pedagogical brand. Their meta-analyses increasingly document that self-labeled 'balanced literacy' classrooms often retain cueing-system practices inconsistent with the evidence on decoding, without the label itself being audited against classroom observation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, cognitive_science_reading_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to resolve the genuine tension between two real needs of reading acquisition — automatized decoding skill and motivated engagement with meaningful text — by requiring both explicit phonics and authentic literature exposure within a single integrated instructional program, rather than forcing schools to choose one pedagogy exclusively.
% TRANSFER_FUNCTION: Moves instructional time, curriculum-purchasing budgets, and professional-development dollars toward vendors and consultants who can supply 'integrated' materials and fidelity coaching, while moving the cost of ambiguous implementation onto teachers (who must reconcile conflicting mandates without adequate time) and onto struggling readers (who bear the consequence when the phonics half of the integration is under-delivered).
% ABSENT_VOICES: Parents of struggling readers rarely have the pedagogical literacy to distinguish genuine integrated fidelity from watered-down whole-language dressed in phonics vocabulary, and are structurally outside the curriculum-adoption conversation, which happens between district administrators, consultants, and publishers.
% DISAPPEARANCE_RATIONALE: If the balanced literacy framework disappeared overnight, districts would have to choose explicitly between systematic phonics-first programs and literature-immersion programs; publishers and consultants built around the 'integrated' compromise product line would need to re-brand or exit, and schools of education would face pressure to revise teacher-preparation curricula. Whether the world materially 'rearranges' for children is contested: proponents argue genuine integration is pedagogically necessary and its disappearance would harm engagement and comprehension outcomes; critics argue removing the ambiguous middle position would simply force overdue clarity toward systematic phonics and improve outcomes for the children currently most harmed.
% FOUNDING_PROBLEM: The 'reading wars' of the 1980s-1990s left phonics-only and whole-language-only camps in unresolved conflict; balanced literacy was built to end the conflict by claiming both were right and could be synthesized, restoring peace among competing pedagogical factions and unifying curriculum markets.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science reading researchers, working from outside both the publishing and teacher-education establishments, corroborate via large-scale reading outcome data (e.g. NAEP trends, national literacy inquiries in several English-speaking countries) that the 'synthesis' as commonly implemented frequently fails to deliver systematic phonics in practice, suggesting the founding compromise is administratively alive but functionally often dead at the classroom level. Curriculum publishers and schools of education, who benefit from the framework's continuation, largely attest the founding problem remains live and the synthesis is sound; no source entirely outside stakeholders with a professional stake in the framework's continuation attests unqualified success.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, contested).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.52) rather than extreme because the coordination function is genuine: a synthesis of decoding skill-building and meaningful engagement IS what the cognitive science on reading actually supports in principle, so the framework is not pure extraction dressed as coordination. But suppression (0.38) and, more sharply, theater_ratio (0.61 and rising) capture the structural delta named in this reading: as the label 'balanced literacy' persisted for decades, an increasing share of what districts labeled 'integration' became performative — nominal phonics components layered onto substantively unchanged whole-language practice — while the underlying decoding-skill deficits accumulated in exactly the population (struggling readers, low-literacy-household children) least able to compensate outside school. Accessibility_collapse is moderate-low (0.34) because, unlike a true mountain, alternative approaches (phonics-first programs) remain visible, documented, and increasingly adopted by 'science of reading' legislative reform movements in several US states — the alternative has not been suppressed so much as institutionally out-competed within teacher-preparation pipelines.
 *
 * DIRECTIONALITY LOGIC:
 *   Curriculum publishers and consultants sit near the beneficiary end: the integrated framework's ambiguity is a feature for them, sustaining ongoing demand for their specific expertise and materials in a way neither a pure phonics program nor a pure whole-language program would. Schools of education faculty are beneficiaries with an identity-lock complication: their exit options are constrained not by market power but by professional and institutional identity fused to the pedagogical tradition. Classroom teachers are payers with constrained exit — they cannot unilaterally redesign curriculum but bear the daily cost of reconciling contradictory mandates. Struggling readers and low-literacy-household children are the clearest targets: trapped exit options (no ability to select their instructional environment), powerless, and bearing the compounding cost of variable implementation fidelity.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is load-bearing here: unlike a pure extraction mechanism, balanced literacy is built on a real coordination problem (the reading wars genuinely needed resolution, and cognitive science genuinely supports both decoding automaticity and meaningful engagement as necessary, not merely one). Classifying it as a pure snare would erase the genuine pedagogical insight that motivated the synthesis. Classifying it as a rope would erase the asymmetric extraction — publishers and identity-locked faculty benefit from ongoing ambiguity while powerless struggling readers pay the cost of implementation drift. The tangled_rope frame holds both: real coordination function, real extraction, and (per R5) a founding problem whose status is contested rather than cleanly live or dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compromise_as_synthesis_vs_cover,
    'Is ''balanced literacy'' a genuine pedagogical synthesis that happens to suffer from implementation variance, or is the ''both/and'' framing itself a structural cover story that allows publishers, consultants, and teacher-education faculty to avoid the accountability that a single well-defined, falsifiable approach would impose?',
    'Compare reading outcomes across districts with high-fidelity documented balanced literacy implementation (verified via classroom observation protocols measuring actual phonics systematicity) against phonics-first and against low-fidelity ''balanced literacy in name only'' districts. If high-fidelity balanced literacy matches or exceeds phonics-first outcomes, the synthesis reading is vindicated; if high-fidelity implementation is rare and outcomes track observed phonics systematicity regardless of label, the label functions primarily as institutional cover.',
    'If cover story: this constraint''s true type shifts toward snare, with the coordination function largely rhetorical. If genuine synthesis with implementation drift: tangled_rope holds, with the drift itself being the object requiring fixing (training and accountability infrastructure) rather than the underlying theory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_as_synthesis_vs_cover, empirical, 'Whether balanced literacy''s ambiguity is genuine synthesis-difficulty or institutional cover for continued whole-language practice.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Where exactly does ''balanced literacy done with genuine phonics systematicity'' become indistinguishable from the phonics_reading, and where does ''balanced literacy done with insufficient phonics'' become indistinguishable from the whole_language_reading? Is balanced_literacy_reading a structurally distinct third position, or is it better modeled as a continuum between the two sibling readings that collapses to one pole in practice?',
    'Structural audit of a representative sample of self-identified ''balanced literacy'' curricula against explicit phonics scope-and-sequence benchmarks (e.g. degree of systematic, cumulative, decodable-text alignment) versus degree of three-cueing system retention, to determine whether a genuinely distinct middle category is empirically populated or whether observed implementations cluster at the whole-language pole.',
    'If implementations cluster at the whole-language pole, this constraint''s ε and victim profile would converge toward the whole_language_reading''s, undermining the claim that balanced_literacy_reading is a stable, independently classifiable third constraint rather than a rhetorical relabeling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Whether balanced literacy is a structurally distinct reading or a rhetorical midpoint that collapses toward whole language in practice.').

omega_variable(
    teacher_preparation_lock_in_mechanism,
    'Is the identity-lock experienced by schools of education faculty and long-tenured teachers a matter of professional identity fusion (genuinely difficult to unwind without institutional crisis) or primarily an economic lock-in (retraining and materials costs) that could be resolved with sufficient investment?',
    'Track outcomes and institutional resistance in jurisdictions that have mandated ''science of reading'' legislative reform requiring teacher retraining — measure whether resistance persists after retraining is fully funded and completed, which would indicate identity-fusion rather than pure resource constraint.',
    'If identity-fusion dominates, remediation requires generational turnover in teacher-education faculty rather than policy/funding fixes alone, materially raising the fixing_cost classification''s justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(teacher_preparation_lock_in_mechanism, empirical, 'Whether faculty resistance to reform is identity-based or resource-based.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(read_tr_t0, observed).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement_basis(read_tr_t8, observed).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 16, 0.46).
narrative_ontology:measurement_basis(read_tr_t16, observed).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 24, 0.53).
narrative_ontology:measurement_basis(read_tr_t24, observed).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 32, 0.58).
narrative_ontology:measurement_basis(read_tr_t32, observed).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 40, 0.61).
narrative_ontology:measurement_basis(read_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(read_be_t0, observed).
narrative_ontology:measurement(read_be_t8, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement_basis(read_be_t8, observed).
narrative_ontology:measurement(read_be_t16, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement_basis(read_be_t16, observed).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement_basis(read_be_t24, observed).
narrative_ontology:measurement(read_be_t32, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement_basis(read_be_t32, observed).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(read_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(read_su_t0, observed).
narrative_ontology:measurement(read_su_t8, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement_basis(read_su_t8, observed).
narrative_ontology:measurement(read_su_t16, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement_basis(read_su_t16, observed).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 24, 0.33).
narrative_ontology:measurement_basis(read_su_t24, observed).
narrative_ontology:measurement(read_su_t32, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement_basis(read_su_t32, observed).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(read_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__balanced_literacy_reading, 0.08).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__whole_language_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the colloquial 'reading wars' / 'science of reading' debate into structurally distinct constraints per the ε-invariance principle. phonics_reading claims systematic grapheme-phoneme instruction is the causal foundation (lower theater_ratio, more contested empirical status but less institutional drift). whole_language_reading claims decoding emerges implicitly from authentic engagement (distinct beneficiary/victim structure centered on the whole-language teacher-education tradition). balanced_literacy_reading (this story) claims both are jointly necessary, but is distinguished from its siblings by chronic implementation-fidelity variance and a documented tendency to collapse toward the whole_language_reading's practice profile despite nominal phonics inclusion. The three do not share one ε — each is a separately measured constraint linked here for contamination-propagation and drift analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
