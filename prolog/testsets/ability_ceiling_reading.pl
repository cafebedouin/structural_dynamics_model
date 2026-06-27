% ============================================================================
% CONSTRAINT STORY: ability_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ability_ceiling_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ability_ceiling_reading
 *   human_readable: Innate Cognitive Capacity Ceiling (Ability-Ceiling Reading)
 *   domain: educational_psychology/learning_theory/epistemology
 *
 * SUMMARY:
 *   The ability-ceiling reading interprets learning difficulty as revealing
 *   fixed cognitive capacity limits—some minds are structurally unsuited to
 *   certain domains. This reading is one of three structurally distinct
 *   interpretations of the same observable (persistent learning difficulty).
 *   The ability-ceiling reading treats struggle as diagnostic of immutable
 *   capacity; the prerequisite-debt reading treats it as recoverable
 *   knowledge gaps; the access-barrier reading treats it as institutional
 *   exclusion. These are not three perspectives on one constraint—they are
 *   three constraints with different ε values, different beneficiary/victim
 *   structures, and different persistence mechanisms. This story instantiates
 *   ONLY the ability-ceiling reading.
 *
 * KEY AGENTS:
 *   - meritocratic_sorting_institutions: agenda_setter (institutional/mobile) — operate tracking and admissions gates legitimated by ability-ceiling assumptions
 *   - high_performing_learners: beneficiary (moderate/mobile) — receive enriched resources when performance gaps are naturalized as capacity differences
 *   - learners_below_threshold: payer (powerless/identity_locked) — tracked into reduced-opportunity paths when struggle is read as capacity limit
 *   - late_bloomers: payer (powerless/identity_locked) — excluded by premature sorting before delayed mastery becomes visible
 *   - growth_mindset_educators: excluded (moderate/constrained) — marginalized for treating difficulty as pedagogically actionable
 *   - learning_scientists: observer (institutional/analytical) — study neuroplasticity and skill malleability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ability_ceiling_reading, 0.68).
domain_priors:suppression_score(ability_ceiling_reading, 0.72).
domain_priors:theater_ratio(ability_ceiling_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ability_ceiling_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ability_ceiling_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ability_ceiling_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ability_ceiling_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(ability_ceiling_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ability_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(ability_ceiling_reading, "Innate Cognitive Capacity Ceiling (Ability-Ceiling Reading)").
narrative_ontology:topic_domain(ability_ceiling_reading, "educational_psychology/learning_theory/epistemology").

domain_priors:requires_active_enforcement(ability_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ability_ceiling_reading, '8a05c85d-4fa0-4575-aa04-68239d1fa092').
narrative_ontology:cs_kernel_codification('8a05c85d-4fa0-4575-aa04-68239d1fa092', distributed).
narrative_ontology:cs_authority_grounding('8a05c85d-4fa0-4575-aa04-68239d1fa092', expertise).
narrative_ontology:cs_interpretation_layer_present('8a05c85d-4fa0-4575-aa04-68239d1fa092').
narrative_ontology:cs_reading_relation('8a05c85d-4fa0-4575-aa04-68239d1fa092', learning_difficulty_substrate__prerequisite_debt_reading, influences).
narrative_ontology:cs_reading_relation('8a05c85d-4fa0-4575-aa04-68239d1fa092', learning_difficulty_substrate__access_barrier_reading, coexists_with).
narrative_ontology:cs_axiom('8a05c85d-4fa0-4575-aa04-68239d1fa092', foundational, cognitive_capacity_immutability).
narrative_ontology:cs_axiom_status(cognitive_capacity_immutability, holdable).
narrative_ontology:cs_axiom_grounding('8a05c85d-4fa0-4575-aa04-68239d1fa092', cognitive_capacity_immutability, empirically_contingent).
narrative_ontology:cs_axiom('8a05c85d-4fa0-4575-aa04-68239d1fa092', secondary, early_performance_diagnostic_validity).
narrative_ontology:cs_axiom_status(early_performance_diagnostic_validity, holdable).
narrative_ontology:cs_axiom_grounding('8a05c85d-4fa0-4575-aa04-68239d1fa092', early_performance_diagnostic_validity, empirically_contingent).
narrative_ontology:cs_reference_frame('8a05c85d-4fa0-4575-aa04-68239d1fa092', psychometric_capacity_measurement).
narrative_ontology:cs_drift_state('8a05c85d-4fa0-4575-aa04-68239d1fa092', post_neuroplasticity_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8a05c85d-4fa0-4575-aa04-68239d1fa092', '').
narrative_ontology:cs_kernel_id(ability_ceiling_reading, learning_difficulty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ability_ceiling_reading, meritocratic_sorting_institutions).
narrative_ontology:constraint_beneficiary(ability_ceiling_reading, high_performing_learners).
narrative_ontology:constraint_victim(ability_ceiling_reading, learners_below_threshold).
narrative_ontology:constraint_victim(ability_ceiling_reading, late_bloomers).
narrative_ontology:constraint_vindicates(ability_ceiling_reading, fixed_intelligence_doctrine).
narrative_ontology:constraint_vindicates(ability_ceiling_reading, early_identification_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate tracking systems, admissions gates, and ability grouping that allocate educational resources and opportunities based on early performance signals. Justify these mechanisms as efficient matching of learners to appropriate challenge levels. The ability-ceiling reading legitimates early sorting by framing performance gaps as reflecting stable underlying capacity rather than remediable preparation gaps.
narrative_ontology:constraint_stakeholder(ability_ceiling_reading, meritocratic_sorting_institutions, agenda_setter,
    institutional, generational, mobile, national).

% Receive enriched instruction, advanced placement, and institutional validation when early performance is interpreted as revealing high innate capacity. The ability-ceiling framing protects their access to accelerated tracks by naturalizing the performance gap—if difficulty reflects capacity limits rather than preparation debt, then differential resource allocation is matching rather than compounding advantage.
narrative_ontology:constraint_stakeholder(ability_ceiling_reading, high_performing_learners, beneficiary,
    moderate, biographical, mobile, local).

% Experience early struggle as diagnostic of fixed limitation. Are tracked into remedial or vocational paths with reduced access to advanced content. The ability-ceiling reading forecloses the hypothesis that their difficulty reflects recoverable preparation gaps or pedagogical mismatch—the constraint operates by converting struggle into identity ('not a math person') and institutional placement into destiny. Exit requires rejecting the internalized capacity verdict, which the educational system continuously re-administers through performance feedback.
narrative_ontology:constraint_stakeholder(ability_ceiling_reading, learners_below_threshold, payer,
    powerless, biographical, identity_locked, local).

% Show delayed mastery trajectories that the ability-ceiling reading cannot accommodate—their eventual success falsifies the early diagnostic but only after institutional sorting has already occurred. The constraint extracts from them by closing advanced pathways before their capacity becomes visible, then attributes their exclusion to the earlier (mis)measurement rather than to the measurement's premature finality.
narrative_ontology:constraint_stakeholder(ability_ceiling_reading, late_bloomers, payer,
    powerless, biographical, identity_locked, local).

% Advocate for pedagogical approaches that treat difficulty as information about instruction rather than capacity. Are marginalized in institutional contexts where ability-ceiling assumptions structure resource allocation and tracking decisions. Their exclusion is maintained by framing their position as wishful thinking that denies real cognitive differences.
narrative_ontology:constraint_stakeholder(ability_ceiling_reading, growth_mindset_educators, excluded,
    moderate, biographical, constrained, local).

% Study learning trajectories, neuroplasticity, and the malleability of cognitive skills. Accumulate evidence that most learning difficulty reflects remediable factors (prior knowledge gaps, working memory load, metacognitive skill) rather than fixed capacity limits. Their research challenges the ability-ceiling reading but competes with institutional inertia and the beneficiaries' interest in maintaining early sorting mechanisms.
narrative_ontology:constraint_stakeholder(ability_ceiling_reading, learning_scientists, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for allocating differentiated instruction and educational resources by treating performance signals as revealing underlying capacity, enabling institutions to sort learners into tracks without continuously re-evaluating placement.
% TRANSFER_FUNCTION: Moves educational opportunity and institutional validation from learners who struggle early to learners who perform well early, justified by the claim that the performance gap reflects immutable cognitive differences rather than recoverable preparation gaps.
% ABSENT_VOICES: Late bloomers and learners whose difficulty reflects pedagogical mismatch or recoverable knowledge gaps are structurally excluded from the diagnostic conversation—the ability-ceiling reading forecloses the hypothesis that their struggle is informative about instruction rather than capacity, so their testimony about eventual mastery is dismissed as anecdotal exception rather than systematic falsification.
% DISAPPEARANCE_RATIONALE: If the ability-ceiling reading vanished overnight, tracking systems would lose their naturalization—institutions would face pressure to treat early struggle as diagnostic of instructional need rather than capacity limit, resource allocation would shift toward remediation and re-teaching rather than sorting, and the identity-lock on learners below threshold would break as difficulty became pedagogically actionable rather than biographically determinative.
% FOUNDING_PROBLEM: Early 20th century mass education required efficient mechanisms for sorting large numbers of learners into differentiated tracks with limited diagnostic resources; psychometric testing provided a scalable sorting technology, and the ability-ceiling reading legitimated the resulting placements by framing test performance as revealing stable underlying capacity.
% FOUNDING_PROBLEM_CORROBORATION: Learning scientists and cognitive psychologists outside the sorting institutions attest that the founding problem is dead: neuroplasticity research, growth mindset interventions, and mastery-based pedagogies demonstrate that most learning difficulty is remediable with appropriate instruction. The ability-ceiling reading persists not because the diagnostic problem remains unsolved but because the sorting function it enables continues to serve institutional and beneficiary interests. Legislative testimony from education researchers and meta-analyses of intervention studies support the shifted-function reading.
narrative_ontology:disappearance_verdict(ability_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(ability_ceiling_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ability_ceiling_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-27',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(ability_ceiling_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ability_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ability_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ability_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68) because the ability-ceiling reading converts early performance signals into durable institutional placements that concentrate resources on high performers while foreclosing remediation for struggling learners—the transfer is justified by naturalizing the performance gap rather than by demonstrating that the gap is immutable. Suppression is high (0.72) because the constraint operates partly through internalized identity ('not a math person') that persists even after external barriers are removed—learners below threshold carry the capacity verdict with them. Theater ratio is moderate (0.41): diagnostic testing and ability grouping perform real sorting functions, but a growing share of the apparatus defends the naturalization claim against accumulating evidence of malleability. Accessibility collapse is high (0.78) because once the ability-ceiling reading is institutionally adopted, alternative interpretations of difficulty (as preparation debt or pedagogical mismatch) are foreclosed by the tracking system's structure. Resistance is moderate-high (0.58) because growth mindset educators and learning scientists actively contest the reading, but their resistance meets institutional inertia and beneficiary interest in maintaining early sorting.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and payer seats should compute differently: from the institutional position, ability-ceiling assumptions enable efficient resource allocation and legitimate tracking; from the powerless/identity_locked learner position, the same assumptions operate as enforced extraction that converts early struggle into biographical destiny. The engine computes this divergence from the structural data—the claimed type (tangled_rope) reflects the institutional coordination function, while the metrics describe substantially extractive operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Meritocratic sorting institutions are structural beneficiaries (the ability-ceiling reading legitimates their sorting mechanisms—d near beneficiary end). High-performing learners are beneficiaries (receive enriched resources justified by naturalized performance gaps—d near beneficiary end). Learners below threshold and late bloomers are targets (bear the extraction through reduced opportunity and identity-lock—d near target end, amplified by identity_locked exit). Growth mindset educators are excluded rather than coordinated. Learning scientists are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The ability-ceiling reading exhibits mandatrophy: it was built to solve an early 20th century mass-education sorting problem (efficient differentiation with limited diagnostic resources), but neuroplasticity research and mastery-based pedagogy have demonstrated that most learning difficulty is remediable. The founding problem is dead (learning scientists attest this from outside the benefiting institutions), yet the constraint persists because the sorting function it enables continues to serve institutional efficiency and high-performer advantage. The R5 mismatch (founding_problem_status: dead + disappearance_verdict: world_rearranges) flags this as a zombie constraint maintained by beneficiary interest rather than by the problem it claims to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is persistent learning difficulty evidence of fixed cognitive capacity limits (ability-ceiling reading), recoverable preparation gaps (prerequisite-debt reading), or institutional exclusion (access-barrier reading)?',
    'Longitudinal studies tracking learners who receive intensive remediation vs. learners who are tracked into reduced-opportunity paths: if remediation closes performance gaps, the ability-ceiling reading is falsified; if gaps persist despite remediation, the reading is corroborated. Natural experiments from jurisdictions that eliminate early tracking provide additional evidence.',
    'If the ability-ceiling reading is falsified, the constraint''s extraction becomes visible as unjustified sorting rather than efficient matching—institutional legitimacy for tracking systems collapses and resource allocation shifts toward remediation. If corroborated, the constraint''s coordination function is vindicated and the measured extraction represents the unavoidable cost of matching learners to appropriate challenge levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Which reading of the learning_difficulty_substrate kernel is structurally accurate.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (institutional tracking barriers) or internalized (identity-lock that persists after barriers are removed)?',
    'Post-intervention suppression trajectory: if learners who receive growth mindset interventions and remediation continue to avoid challenging domains even after demonstrating mastery, the suppression is partly internalized; if they re-engage with previously avoided domains, the suppression was structural.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest—learners carry the capacity verdict with them after institutional barriers are removed, and remediation requires identity reconstruction in addition to skill-building. If structural, removing tracking barriers is sufficient to restore access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized identity-lock.').

omega_variable(
    neuroplasticity_vs_sorting_efficiency,
    'Does the accumulating evidence of cognitive malleability and skill remediability falsify the ability-ceiling reading, or does institutional efficiency in resource allocation justify maintaining ability-based sorting even if capacity is partly malleable?',
    'Policy analysis comparing educational outcomes in systems that maintain ability tracking vs. systems that eliminate it: if tracked systems produce better aggregate outcomes despite foreclosing late bloomers, the efficiency justification holds; if untracked systems produce equal or better outcomes, the ability-ceiling reading is maintained for beneficiary interest rather than aggregate welfare.',
    'If neuroplasticity evidence falsifies the ability-ceiling reading, the constraint is reclassified as pure extraction (snare) rather than tangled_rope—the coordination story is cover for sorting that benefits high performers at the expense of struggling learners. If efficiency justifies sorting despite malleability, the constraint remains tangled_rope with genuine coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(neuroplasticity_vs_sorting_efficiency, preference, 'Whether institutional efficiency justifies ability-based sorting given evidence of cognitive malleability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ability_ceiling_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abil_tr_t0, ability_ceiling_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(abil_tr_t8, ability_ceiling_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(abil_tr_t16, ability_ceiling_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(abil_tr_t24, ability_ceiling_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(abil_tr_t32, ability_ceiling_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(abil_tr_t40, ability_ceiling_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(abil_be_t0, ability_ceiling_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(abil_be_t8, ability_ceiling_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(abil_be_t16, ability_ceiling_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(abil_be_t24, ability_ceiling_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(abil_be_t32, ability_ceiling_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(abil_be_t40, ability_ceiling_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(abil_su_t0, ability_ceiling_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(abil_su_t8, ability_ceiling_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(abil_su_t16, ability_ceiling_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(abil_su_t24, ability_ceiling_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(abil_su_t32, ability_ceiling_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(abil_su_t40, ability_ceiling_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ability_ceiling_reading, prerequisite_debt_reading).
narrative_ontology:affects_constraint(ability_ceiling_reading, access_barrier_reading).

% DUAL FORMULATION NOTE:
% The learning_difficulty_substrate kernel decomposes into three constraint stories with different ε values: ability_ceiling_reading (this story, ε=0.68, benefits sorting institutions), prerequisite_debt_reading (ε~0.35, benefits remediation educators), and access_barrier_reading (ε~0.72, benefits equity reformers). The ability-ceiling reading influences both siblings by creating structural conditions (early tracking, identity-lock) that make alternative interpretations harder to implement even when pedagogically or institutionally available. All three readings are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
