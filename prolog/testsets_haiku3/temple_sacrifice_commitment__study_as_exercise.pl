% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Temple Sacrifice Commitment: Study as Exercise
 *   domain: religious/legal/commitment_system
 *
 * SUMMARY:
 *   After the destruction of the Second Temple (70 CE), material sacrifice
 *   became impossible: no Temple building, no priesthood, no sacrificial
 *   animals. Yet the covenant commitment to the divine command remained
 *   binding. This constraint instantiates ONE reading of how that commitment
 *   is occupied in the absence of material conditions: the study and
 *   intellectual engagement with sacrifice law constitutes itself a
 *   performance of the divine command. Study is not a substitute for
 *   sacrifice (a sibling reading's framing), nor is it mere archival
 *   preservation of a defunct practice (another sibling reading). Rather,
 *   intellectual engagement WITH the commanded practice IS the performance of
 *   the covenant in the Temple's absence. This reading has structured
 *   halakhic practice and textual transmission for nearly 2000 years.
 *
 * KEY AGENTS:
 *   - Studying community: practitioners and scholars engaged in sacrifice law transmission; identity-locked into the commitment structure; the constraint benefits them by permitting covenant fidelity through study.
 *   - Textual tradition: the corpus of Mishnaic and Talmudic sacrifice law; not an agent but the medium of performance.
 *   - Replacement theology voices: excluded from the framework; they interpret the sacrificial system as ended; their alternative framing is structurally external to this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Temple Sacrifice Commitment: Study as Exercise").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious/legal/commitment_system").

domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '227fcda6-f061-4d72-936b-4c89e3d52e05').
narrative_ontology:cs_kernel_codification('227fcda6-f061-4d72-936b-4c89e3d52e05', fixed_text).
narrative_ontology:cs_authority_grounding('227fcda6-f061-4d72-936b-4c89e3d52e05', lineage).
narrative_ontology:cs_interpretation_layer_present('227fcda6-f061-4d72-936b-4c89e3d52e05').
narrative_ontology:cs_reading_relation('227fcda6-f061-4d72-936b-4c89e3d52e05', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('227fcda6-f061-4d72-936b-4c89e3d52e05', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('227fcda6-f061-4d72-936b-4c89e3d52e05', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('227fcda6-f061-4d72-936b-4c89e3d52e05', foundational, study_constitutes_performance).
narrative_ontology:cs_axiom_status(study_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('227fcda6-f061-4d72-936b-4c89e3d52e05', study_constitutes_performance, deontological).
narrative_ontology:cs_axiom('227fcda6-f061-4d72-936b-4c89e3d52e05', foundational, intellectual_engagement_occupies_commitment).
narrative_ontology:cs_axiom_status(intellectual_engagement_occupies_commitment, holdable).
narrative_ontology:cs_axiom_grounding('227fcda6-f061-4d72-936b-4c89e3d52e05', intellectual_engagement_occupies_commitment, deontological).
narrative_ontology:cs_reference_frame('227fcda6-f061-4d72-936b-4c89e3d52e05', temple_sacrifice_commitment_post_destruction).
narrative_ontology:cs_drift_state('227fcda6-f061-4d72-936b-4c89e3d52e05', contemporary_study_transmission, gap(stable, minor, true)).
narrative_ontology:cs_created_at('227fcda6-f061-4d72-936b-4c89e3d52e05', '2026-06-13T14:32:00Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, intellectual_engagement_as_performance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practitioners and scholars engaged in the study and transmission of sacrifice law. Their engagement occupies the commitment to the divine command in the absence of material sacrifice. The study itself constitutes performance of the covenant — intellectual engagement is the exercise of the commitment. No extractive mechanism operates; the community benefits from maintaining fidelity through study, but does not collect rents or bear asymmetric costs.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, generational, identity_locked, universal).

% The corpus of sacrifice law and its interpretive lineage. Not an agent; included to name what the constraint preserves and transmits. The tradition itself carries the commitment structure; engagement with it is the mechanism of its exercise.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, textual_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, textual_tradition).

% Those outside the covenant community who might view the sacrificial system as obsolete or merely historical. They have no voice in the framework that holds study as performance because they do not inhabit the commitment structure. Their skepticism is structurally external to the constraint.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, non_studying_observers, excluded,
    powerless, biographical, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the covenant commitment in perpetuity by exercising it through intellectual engagement when material instantiation is impossible or suspended. Study performs the divine command in the absence of the material conditions (Temple, priesthood, sacrificial animals) required for literal sacrifice.
% TRANSFER_FUNCTION: No transfer operates. Study is an intrinsic exercise of commitment, not a mechanism that moves goods, status, or obligation from one seat to another. The studying community maintains the covenant through engagement; the exercise is its own end.
% ABSENT_VOICES: Those who interpret the sacrificial system as definitively ended (replacement theology, functional obsolescence doctrine) are excluded by the framework's axiom. Voices arguing that study cannot substitute for material performance — a sibling reading — remain live but are structurally foreclosed by this reading's core claim.
% DISAPPEARANCE_RATIONALE: If this constraint — the claim that study performs the divine command — vanished overnight, the studying community would continue studying, and a sibling reading (performance_only) would immediately occupy the space. The disappearance is a change of framing, not a change in what actually happens. Material conditions remain absent; the constraint is the interpretation that study occupies the commitment despite that absence.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), the material conditions for sacrificial performance ceased: no Temple building, no priesthood, no sacrificial animals. Yet the covenant commitment to the divine command persists. How can the covenant be exercised when the commanded material practice is impossible?
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic sources (Berakhot 26b, Megillah 31b) attest that the problem persists and that study of sacrifice law constitutes performance of the command in the Temple's absence. Legal philosophers and historians outside the tradition document the post-70 hermeneutical shift that embedded study-as-performance into the halakhic framework. The founding problem remains live because the Temple remains absent and the commitment remains binding.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_unchanged).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint authors as Mountain: extractiveness is zero (no one collects from study; no mechanism extracts value); suppression is zero (the constraint arises from material impossibility, not coercion); theater is zero (study is genuine engagement with textual meaning, not performative theater masking extraction). Accessibility of alternatives is very high (0.95): the sibling readings (performance_only, hybrid_preparatory, symbolic_transformation) are all structurally available and have been advocated by different communities and periods; none is uniquely forced. Resistance is minimal (0.05) because the studying community has affirmed this reading for centuries; the constraint is embraced rather than resisted by those it governs. The beneficiaries array names the studying community because they are identifiably lifted by the arrangement (covenant fidelity is now possible when material performance is not), satisfying the FSM-candidate gate. The beneficiary presence, combined with emerges_naturally=true and the claim of mountain-type, triggers the false-summit evaluation: the omega variables below document the irreducible ambiguity — is this constraint a necessary consequence of destroyed-temple reality, or a constructed reading that serves the interpretive community's interests?
 *
 * PERSPECTIVAL GAP:
 *   The studying community experiences this constraint as liberatory: study permits covenant fidelity when material sacrifice is impossible. An external observer (non-studying voices) might experience it as a constructed replacement that redefines away the problem rather than solving it. The constraint's status as 'necessary consequence' vs. 'useful construction' is exactly what the omegas dispute. From the internal seat (studying community), the constraint emerges naturally; from an external seat (one that denies study performs sacrifice), it is a non-obvious reframing. The engine computes directionality from the structural data: the studying community is the beneficiary (d near 0.0), but the zero-extractiveness profile and lack of victims place the computed type below the tangled-rope gate.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no payers in this constraint — no agent bears costs that another collects. The studying community occupies the covenant commitment through study, which is intrinsically valuable to them (identity-locked exit means they remain committed regardless of external incentives). No extraction mechanism operates. This is the key structural difference from a sibling reading (performance_only): the performance_only reading would split into beneficiaries (those who maintain covenant memory) and implicit victims (those whose labor is consumed in maintaining what they view as a defunct practice). This reading forecloses that split by axiomatically holding that study IS performance, not a substitute for it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (how to exercise the covenant when material performance is impossible) was live at its origin (70 CE) and remains live (Temple is still absent 1956 years later). The commitment has not decayed; it has been reinterpreted to accommodate new conditions. No mandatrophy is present — the constraint's function has not atrophied. Unlike a piton (where the primary function has withered and only performance remains), this constraint's function (covenant exercise via study) has been clarified and reinforced across 2000 years of textual transmission.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_consequence_vs_constructed_reading,
    'Is the equation of study with sacrifice performance a necessary consequence of destroyed-temple reality, or a constructed reading that the halakhic community chose to embed?',
    'Historical analysis of post-70 hermeneutical decision points: were alternatives genuinely unavailable, or were they debated and rejected? Examine sources like Tosafot and medieval commentaries for evidence of suppressed alternatives or explicit choices.',
    'If necessary consequence, the constraint is a genuine mountain (reality-forced). If constructed choice, the constraint is a false summit — a beneficiary-serving reframing disguised as natural law. The ambiguity is irreducible because the textual sources embed interpretive commitment alongside historical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_consequence_vs_constructed_reading, conceptual, 'Whether study-as-performance is necessitated by material conditions or is an authorized reinterpretation.').

omega_variable(
    study_performance_axiom_universality,
    'Does the axiom ''study is performance of the divine command'' hold universally across all commandments, or is it specific to sacrifice law?',
    'Survey halakhic sources on other commandments that became impossible (Temple ritual in exile, sacrificial service after Rabbinic period). If study-as-performance is asserted uniformly, the axiom is universal; if sacrifice law receives special treatment, it is a local accommodation.',
    'Universal application strengthens the constraint as a genuine mountain (applies wherever conditions are impossible). Local application suggests the constraint is tailored to preserve a specific tradition and might be recategorized as snare (if it extracts coherence from the studying community at the cost of logical consistency elsewhere).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_performance_axiom_universality, empirical, 'Whether the study-as-performance equation applies beyond sacrifice law or is locally specific.').

omega_variable(
    post_messianic_reversal_risk,
    'If the Temple were rebuilt and material sacrifice became possible again, would the study-as-performance axiom be displaced, or would it remain as an enduring refinement of the commitment?',
    'Examine sources on the messianic age and the restoration of sacrifice. If the consensus holds that material sacrifice would resume and study would return to preparatory role, the current constraint is conditional on Temple absence. If the consensus holds that study-as-performance would persist even after restoration, the constraint is structural rather than conditional.',
    'If conditional, the constraint is a temporary accommodation (scaffold-like in nature, awaiting messianic change) rather than a permanent mountain. If structural, it is a genuine reinterpretation of the covenant itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_messianic_reversal_risk, conceptual, 'Whether the study-as-performance axiom is conditional on Temple absence or enduring.').

omega_variable(
    beneficiary_interest_circularity,
    'Does the studying community benefit from the study-as-performance axiom because it is true, or does it affirm the axiom because it benefits from being able to maintain covenant commitment without material sacrifice?',
    'Examine whether dissenting voices from within the studying community (those who held performance_only or symbolic_transformation readings) were suppressed or remained as live options. If suppressed, the beneficiary interest may have driven axiom adoption. If live, the axiom was chosen despite alternatives.',
    'If the axiom was chosen despite alternatives, it is a robust mountain. If alternatives were suppressed to favor the beneficiary''s interest, the constraint may be a false summit warranting FSM reclassification to snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_interest_circularity, preference, 'Whether the axiom was chosen on its merits or selected because it served beneficiary interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 70, 0.0).
narrative_ontology:measurement_basis(temp_tr_t70, observed).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 500, 0.0).
narrative_ontology:measurement_basis(temp_tr_t500, observed).
narrative_ontology:measurement(temp_tr_t1100, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1100, 0.0).
narrative_ontology:measurement_basis(temp_tr_t1100, observed).
narrative_ontology:measurement(temp_tr_t1700, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1700, 0.0).
narrative_ontology:measurement_basis(temp_tr_t1700, observed).
narrative_ontology:measurement(temp_tr_t1950, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1950, 0.0).
narrative_ontology:measurement_basis(temp_tr_t1950, observed).
narrative_ontology:measurement(temp_tr_t2026, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 2026, 0.0).
narrative_ontology:measurement_basis(temp_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 70, 0.0).
narrative_ontology:measurement_basis(temp_be_t70, observed).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 500, 0.0).
narrative_ontology:measurement_basis(temp_be_t500, observed).
narrative_ontology:measurement(temp_be_t1100, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1100, 0.0).
narrative_ontology:measurement_basis(temp_be_t1100, observed).
narrative_ontology:measurement(temp_be_t1700, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1700, 0.0).
narrative_ontology:measurement_basis(temp_be_t1700, observed).
narrative_ontology:measurement(temp_be_t1950, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1950, 0.0).
narrative_ontology:measurement_basis(temp_be_t1950, observed).
narrative_ontology:measurement(temp_be_t2026, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 2026, 0.0).
narrative_ontology:measurement_basis(temp_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t70, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 70, 0.0).
narrative_ontology:measurement_basis(temp_su_t70, observed).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 500, 0.0).
narrative_ontology:measurement_basis(temp_su_t500, observed).
narrative_ontology:measurement(temp_su_t1100, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1100, 0.0).
narrative_ontology:measurement_basis(temp_su_t1100, observed).
narrative_ontology:measurement(temp_su_t1700, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1700, 0.0).
narrative_ontology:measurement_basis(temp_su_t1700, observed).
narrative_ontology:measurement(temp_su_t1950, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1950, 0.0).
narrative_ontology:measurement_basis(temp_su_t1950, observed).
narrative_ontology:measurement(temp_su_t2026, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 2026, 0.0).
narrative_ontology:measurement_basis(temp_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, attachment_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__study_as_exercise, 0.0).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the temple_sacrifice_commitment kernel. The kernel persists; the readings decompose into separate constraints with different structures. study_as_exercise authors as Mountain (zero extractiveness, no victims, beneficiary is the studying community maintaining fidelity through study). Sibling readings have divergent structures: performance_only would be Snare (victims bear labor of defunct practice); hybrid_preparatory would be Scaffold (temporary accommodation); symbolic_transformation would be Rope (genuine coordination). Each reading is complete and ε-invariant; the family is linked via network.affects_constraints and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
