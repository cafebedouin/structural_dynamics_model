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
 *   domain: educational psychology / literacy pedagogy / cognitive science
 *
 * SUMMARY:
 *   This story instantiates the 'balanced literacy' reading of the literacy
 *   acquisition kernel: the claim that systematic phonics instruction and
 *   meaningful text engagement are complementary rather than competing, and
 *   that instructional 'balance' resolves the phonics-versus-whole-language
 *   dispute. This is generated as its own ε-invariant constraint,
 *   structurally distinct from the phonics_reading, whole_language_reading,
 *   and structured_literacy_reading siblings, which are separate constraint
 *   stories. The central authoring tension is that 'balance' functions
 *   institutionally as a purchasing and adoption category (curriculum
 *   programs branded 'balanced') more than as a stable operational construct
 *   — outcome data increasingly suggests that programs claiming the balanced
 *   label vary enormously in how much systematic phonics they actually
 *   deliver, with many defaulting toward embedded/incidental phonics
 *   inherited from whole-language practice.
 *
 * KEY AGENTS:
 *   - district_administrators: agenda-setter adopting curricula under the balanced label (institutional/arbitrage)
 *   - curriculum_publishers: primary beneficiary of repeated adoption cycles (organized/arbitrage)
 *   - teacher_training_consultancies: beneficiary certifying the approach (organized/arbitrage)
 *   - classroom_teachers: payer implementing ambiguous guidance (moderate/constrained)
 *   - struggling_readers and dyslexic_students: payers bearing outcome risk (powerless/trapped)
 *   - reading_researchers: analytical observer
 *   - structured_literacy_advocates: excluded voice arguing balance is whole-language rebrand
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.52).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.44).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading of the Literacy Acquisition Kernel").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational psychology / literacy pedagogy / cognitive science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, '20de4500-5f58-4817-b3c1-2caf5514b915').
narrative_ontology:cs_kernel_codification('20de4500-5f58-4817-b3c1-2caf5514b915', distributed).
narrative_ontology:cs_authority_grounding('20de4500-5f58-4817-b3c1-2caf5514b915', distributed).
narrative_ontology:cs_reading_relation('20de4500-5f58-4817-b3c1-2caf5514b915', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('20de4500-5f58-4817-b3c1-2caf5514b915', literacy_acquisition_kernel__whole_language_reading, influences).
narrative_ontology:cs_reading_relation('20de4500-5f58-4817-b3c1-2caf5514b915', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('20de4500-5f58-4817-b3c1-2caf5514b915', foundational, phonics_and_meaning_are_complementary_not_sequential).
narrative_ontology:cs_axiom_status(phonics_and_meaning_are_complementary_not_sequential, holdable).
narrative_ontology:cs_axiom_grounding('20de4500-5f58-4817-b3c1-2caf5514b915', phonics_and_meaning_are_complementary_not_sequential, empirically_contingent).
narrative_ontology:cs_axiom('20de4500-5f58-4817-b3c1-2caf5514b915', secondary, instructional_balance_can_be_locally_determined_without_fixed_sequence).
narrative_ontology:cs_axiom_status(instructional_balance_can_be_locally_determined_without_fixed_sequence, holdable).
narrative_ontology:cs_axiom_grounding('20de4500-5f58-4817-b3c1-2caf5514b915', instructional_balance_can_be_locally_determined_without_fixed_sequence, instrumental).
narrative_ontology:cs_reference_frame('20de4500-5f58-4817-b3c1-2caf5514b915', reading_wars_polarization_era).
narrative_ontology:cs_drift_state('20de4500-5f58-4817-b3c1-2caf5514b915', post_science_of_reading_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('20de4500-5f58-4817-b3c1-2caf5514b915', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, district_administrators).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, teacher_training_consultancies).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, dyslexic_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopts balanced literacy curricula and professional development packages, presenting the approach as the reasonable middle ground between 'reading wars' factions. Can switch curriculum vendors and pedagogical framing every few years without personally bearing classroom-level consequences; career incentives favor being seen as adopting a moderate, defensible consensus position.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, district_administrators, agenda_setter,
    institutional, biographical, arbitrage, regional).

% Sells 'balanced' literacy programs, leveled reading materials, and companion assessment suites to districts. Benefits from the appearance of scientific synthesis without needing rigorous outcome validation; each cycle of pedagogical debate creates fresh purchasing cycles as districts re-adopt updated 'balanced' materials.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers, beneficiary,
    organized, generational, arbitrage, national).

% Runs professional development workshops certifying teachers in balanced literacy methods. Revenue depends on balanced literacy remaining the institutionally endorsed synthesis; has structural incentive to frame the phonics-vs-meaning debate as resolved by their approach rather than genuinely settled by evidence.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, teacher_training_consultancies, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, teacher_training_consultancies, agenda_setter).

% Implements whatever balance of phonics and text-immersion the adopted curriculum specifies, often with vague or contradictory guidance about how much systematic phonics is 'enough.' Bears the day-to-day burden of reconciling ambiguous pedagogical mandates with visible student struggle; cannot easily override district curriculum choices without risking professional standing.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Receives instruction under whatever balance a given curriculum instantiates, which in practice often defaults toward incidental or embedded phonics rather than the systematic, cumulative sequences shown to matter most for weaker decoders. Has no ability to select a different instructional approach; consequences (reading below grade level) compound silently for years before intervention.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Requires the most explicit, systematic, and cumulative phonics instruction of any subgroup; balanced literacy's embedded or incidental phonics component is frequently insufficient for this population regardless of how the balance is nominally struck. Depends entirely on whether the specific balanced program happens to include enough explicit decoding instruction, a matter of implementation variance rather than guaranteed design.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Studies outcome data across programs claiming the balanced label, noting that 'balance' is not a standardized construct — some balanced programs are phonics-heavy, others are whole-language-heavy with token phonics add-ons. Can publish findings but has no authority to compel district adoption decisions.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, reading_researchers, observer,
    analytical, generational, analytical, global).

% Argues that 'balance' is frequently a rebranding of whole-language practice that avoids naming systematic, cumulative phonics as a floor requirement, and that the term's vagueness is precisely what lets weak implementations claim compliance. Present in academic and advocacy venues but rarely at the district curriculum-adoption table, where publisher relationships and administrator preference dominate.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides districts and teachers a shared vocabulary and adoption framework that claims to reconcile decades of polarized 'reading wars' debate, allowing curriculum decisions to be made without resolving the underlying empirical dispute about how much and what kind of phonics instruction is necessary.
% TRANSFER_FUNCTION: Moves purchasing decisions and instructional time toward curricula and professional development branded as 'balanced,' and moves the burden of reconciling ambiguous instructional guidance onto classroom teachers; moves outcome risk onto struggling readers and dyslexic students whose need for systematic phonics may not be met by a given program's particular balance.
% ABSENT_VOICES: Structured literacy advocates and cognitive scientists studying reading acquisition in dyslexic populations argue 'balance' as commonly implemented under-delivers systematic phonics; they publish and advocate but are largely absent from district-level curriculum adoption committees, which are populated by administrators and publisher representatives.
% DISAPPEARANCE_RATIONALE: Publishers and administrators would argue the field reverts to unproductive polarization without a 'balanced' consensus frame; structured literacy advocates would argue nothing of instructional substance changes because the label was never operationally consistent, and outcomes would improve if replaced by an explicit systematic-phonics floor.
% FOUNDING_PROBLEM: The 'reading wars' of the 1980s-1990s produced two entrenched, empirically-informed but practically incompatible instructional camps (phonics-first vs. whole language); balanced literacy was proposed as a synthesis that would let districts avoid ideological warfare while claiming to honor evidence from both traditions.
% FOUNDING_PROBLEM_CORROBORATION: Publishers and teacher-training consultancies attest balance is a genuine, evidence-informed synthesis actively serving classrooms today. Independent reading researchers and the National Reading Panel-adjacent literature, along with structured literacy practitioners outside the balanced-literacy commercial ecosystem, attest that 'balance' as commonly implemented in adopted programs frequently under-delivers systematic phonics relative to what dyslexia and reading-science research establishes as necessary — corroboration from outside the beneficiary set is mixed-to-critical, not confirmatory.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, contested).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52) and theater_ratio (0.58) are both moderate-to-elevated because 'balanced literacy' as commercially and institutionally implemented functions substantially as a branding and purchasing category — the theater_ratio trajectory rises from 0.30 to 0.58 over the interval as the term becomes increasingly a compliance/marketing label decoupled from a standardized instructional definition, tracking the same pattern documented in the source material's note that this reading is 'contested whether genuine third synthesis or whole_language rebrand.' Suppression (0.44) is moderate: no one is coerced into using balanced programs at the level of individual teachers refusing methods, but the institutional endorsement chain (administrators, accreditors, publishers) makes deviation professionally costly. Accessibility_collapse (0.40) is moderate-low because alternative approaches (structured literacy, explicit phonics-first) remain visible and increasingly adopted in some jurisdictions via 'science of reading' legislation — the collapse is partial, not complete. Resistance (0.62) is comparatively high, reflecting the vigorous structured-literacy and science-of-reading advocacy movement actively contesting balanced literacy's evidentiary claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Curriculum publishers and teacher-training consultancies sit near the beneficiary end: they profit from adoption cycles regardless of whether the underlying pedagogical synthesis is empirically sound, and their exit options (arbitrage — they can pivot branding as evidence shifts) protect them from downside. District administrators are also positioned near beneficiary/agenda-setter because adopting a 'balanced' consensus position is institutionally safe even if outcomes lag. Classroom teachers are payers with constrained exit — they implement whatever the adopted curriculum specifies and bear the on-the-ground burden of reconciling ambiguous guidance with visible student difficulty, but cannot easily refuse district mandates. Struggling readers and especially dyslexic students are the clearest targets: trapped exit options, powerless structural position, and the highest sensitivity to whether a given 'balanced' program happens to deliver adequate systematic phonics — a matter of implementation lottery rather than guaranteed design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending reading-wars polarization so districts could make curriculum decisions without adjudicating a live scientific dispute) is contested as live vs. dead: publishers and consultancies maintain the reading-wars synthesis problem is still live and balanced literacy still solves it; independent reading science increasingly treats the core empirical question as substantially resolved in favor of systematic, explicit phonics as a floor requirement for most learners, which would make balanced literacy's 'diplomatic synthesis' function largely obsolete — persisting now primarily as an adoption/purchasing category (piton-adjacent theater) rather than a genuine unresolved-tension bridge. The tangled_rope classification reflects a genuine coordination function (districts do need SOME shared framework to make curriculum decisions) coexisting with asymmetric extraction (publishers and consultancies profit from ambiguity that lets weak implementations claim the balanced label while struggling and dyslexic readers bear the cost of under-delivered systematic phonics).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_vs_whole_language_rebrand,
    'Is balanced literacy a structurally distinct third reading that genuinely integrates systematic phonics with meaningful text engagement, or is it whole_language_reading rebranded with phonics-friendly rhetoric while implementation defaults toward incidental/embedded phonics?',
    'Content analysis of widely-adopted ''balanced literacy'' curricula measuring actual instructional time and sequence structure devoted to systematic, cumulative phonics versus embedded/incidental phonics-in-context, cross-referenced against outcome data (e.g., DIBELS, NAEP subgroup performance) for students taught under balanced programs versus structured literacy programs.',
    'If balanced programs systematically under-deliver explicit, cumulative phonics relative to structured_literacy_reading''s standard, this reading functions largely as a rebrand and the tangled_rope classification''s extraction component (publishers profiting from ambiguity at the cost of struggling/dyslexic readers) is strongly corroborated. If balanced programs reliably deliver adequate systematic phonics alongside rich text engagement, the reading is a genuine synthesis and the rope/coordination component dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_whole_language_rebrand, empirical, 'Whether balanced literacy is a genuine synthesis reading or a whole-language rebrand — the central kernel-contest question for this reading.').

omega_variable(
    beneficiary_naturalness_ambiguity,
    'Is the ''balance'' framing a natural pedagogical insight (skills genuinely complement each other, as cognitive science on reading acquisition broadly supports) or a constructed institutional category shaped substantially by publisher and consultancy incentives to sustain adoption cycles?',
    'Trace the historical emergence of ''balanced literacy'' as a term through publisher marketing materials and district adoption documents versus its emergence in peer-reviewed reading science literature; compare timing and framing.',
    'If the term originated primarily as a marketing/adoption category rather than a research consensus term, the coordination function claimed by administrators and publishers is weaker than presented, strengthening the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, conceptual, 'Whether the balance concept is a natural cognitive-science finding or a constructed adoption category.').

omega_variable(
    victim_identity_ambiguity,
    'Does balanced literacy, as actually implemented across districts, primarily fail struggling and dyslexic readers (victim set as declared), or does implementation variance mean some balanced programs serve these populations adequately while others do not — making ''victim'' a program-specific rather than reading-specific fact?',
    'Disaggregated outcome studies by specific curriculum brand and phonics-instruction-time within programs self-labeled as balanced literacy.',
    'If harm is concentrated in specific under-implemented programs rather than inherent to the balanced reading itself, the extraction component may be a fidelity-of-implementation problem rather than a structural feature of this reading — which would push the classification toward rope (with the tangled_rope profile reflecting implementation failure rather than design).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_identity_ambiguity, empirical, 'Whether harm to struggling/dyslexic readers is inherent to the reading or an artifact of implementation variance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(lite_tr_t4, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 16, 0.53).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(lite_tr_t24, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(lite_be_t4, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(lite_be_t24, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 24, 0.52).

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
% This story is one of four sibling readings of literacy_acquisition_kernel. phonics_reading and structured_literacy_reading claim decoding-first sequencing with systematic, explicit, cumulative instruction (structured_literacy_reading extending this to a full multi-component model originating in dyslexia intervention). whole_language_reading claims phonics is unnecessary and potentially harmful, emerging naturally from text exposure. balanced_literacy_reading (this story) claims synthesis: both are necessary and complementary. The kernel contest here is unusually asymmetric: this reading's own commentary raises the possibility (in genuine_synthesis_vs_whole_language_rebrand) that it collapses into whole_language_reading under implementation scrutiny, making the influences edge to whole_language_reading bidirectional in practice even though only one direction is declared per this reading's authoring scope. ε for this reading (0.52) sits between the two poles by design, reflecting genuine synthesis attempt plus institutional churn incentive, and must not be reconciled with the siblings' ε values — each is a separate constraint with its own stable ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
