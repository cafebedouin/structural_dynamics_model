% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Passover Survival Competence Transmission (D5 Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   The commemorative ritual of Passover, read through the D5
 *   survival-competence lens, functions as a distributed mechanism for
 *   transmitting embodied adaptive capacity across catastrophic
 *   discontinuities. This reading construes the ritual not merely as memorial
 *   obligation but as practical rehearsal for institutional transformation
 *   under duress. The constraint operates through annual performance
 *   obligations that bind identity-locked participants while generating
 *   coordination benefits of decentralized continuity. It is one reading of
 *   the contested catastrophe_memory_function kernel, competing with
 *   mourning-practice and hybrid-transformation readings. The claim of
 *   tangled_rope captures both the genuine coordination function (survival
 *   knowledge transmission) and the asymmetric extraction (gendered ritual
 *   labor, identity-locked obligation, authority consolidation).
 *
 * KEY AGENTS:
 *   - tradition_authorities: agenda_setter (institutional/global/constrained) â maintain interpretive control and textual authority
 *   - observant_community: dual beneficiary/payer (organized/global/identity_locked) â receives competence and cohesion, bears performance costs
 *   - ritual_labor_bearers: primary target (powerless/national/identity_locked) â extracted domestic labor rendered socially invisible
 *   - marginalized_obligants: secondary target (powerless/local/identity_locked) â bears identity costs without receiving adaptive benefit
 *   - assimilated_outsiders: excluded (moderate/national/mobile) â no longer in the conversation
 *   - ritual_theorists: observer (analytical/global/analytical) â sees the full structural arc
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.5).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Passover Survival Competence Transmission (D5 Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, 'a65850be-454e-478f-a007-ea60e9253c3a').
narrative_ontology:cs_kernel_codification('a65850be-454e-478f-a007-ea60e9253c3a', fixed_text).
narrative_ontology:cs_authority_grounding('a65850be-454e-478f-a007-ea60e9253c3a', lineage).
narrative_ontology:cs_interpretation_layer_present('a65850be-454e-478f-a007-ea60e9253c3a').
narrative_ontology:cs_reading_relation('a65850be-454e-478f-a007-ea60e9253c3a', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('a65850be-454e-478f-a007-ea60e9253c3a', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('a65850be-454e-478f-a007-ea60e9253c3a', foundational, survival_competence_transmission).
narrative_ontology:cs_axiom_status(survival_competence_transmission, holdable).
narrative_ontology:cs_axiom_grounding('a65850be-454e-478f-a007-ea60e9253c3a', survival_competence_transmission, empirically_contingent).
narrative_ontology:cs_axiom('a65850be-454e-478f-a007-ea60e9253c3a', foundational, decentralized_continuity_imperative).
narrative_ontology:cs_axiom_status(decentralized_continuity_imperative, holdable).
narrative_ontology:cs_axiom_grounding('a65850be-454e-478f-a007-ea60e9253c3a', decentralized_continuity_imperative, instrumental).
narrative_ontology:cs_reference_frame('a65850be-454e-478f-a007-ea60e9253c3a', embodied_adaptive_transmission).
narrative_ontology:cs_drift_state('a65850be-454e-478f-a007-ea60e9253c3a', contemporary_diaspora, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a65850be-454e-478f-a007-ea60e9253c3a', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, tradition_authorities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, observant_community).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, ritual_labor_bearers).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, marginalized_obligants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, observant_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the textual and practical framework of the commemorative ritual. They define correct performance and the adaptive lessons encoded in the narrative. Their authority and institutional role depend on the ritual's continuity across generations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, tradition_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Perform the ritual annually, transmitting narrative and practices to children. They receive social cohesion, identity continuity, and encoded survival knowledge, but bear significant time, material, and cognitive costs of preparation and participation. Exit without family or communal rupture is difficult.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, observant_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, observant_community, payer).

% Perform the hidden labor that makes the ritual possibleâfood preparation, household arrangement, cleaningâwithout receiving proportional recognition or authority in the ritual's public performance. Their labor is structurally necessary but socially invisible within the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_labor_bearers, payer,
    powerless, biographical, identity_locked, national).

% Attend and perform the ritual to maintain family peace and social standing, but do not experience the transmission of adaptive capacity. They pay the costs of participation without receiving the claimed coordination benefit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, marginalized_obligants, payer,
    powerless, immediate, identity_locked, local).

% No longer participate in the ritual and do not receive the transmitted competence. They are excluded from the community's decentralized continuity network and may experience family rupture from their exit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, assimilated_outsiders, excluded,
    moderate, biographical, mobile, national).

% Analyze the ritual's functional role in cultural transmission without being bound by its obligations. They document the survival-competence hypothesis and compare it with other commemorative practices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__survival_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmitting practical adaptive capacity across generations through embodied ritual rehearsal, ensuring that survival knowledgeâresource mobilization, rapid adaptation, decentralized decision-makingâpersists despite institutional disruption or catastrophe.
% TRANSFER_FUNCTION: Moves time, labor, and cognitive attention from ritual participantsâparticularly marginalized obligants and domestic laborersâto the communal stock of survival competence and institutional continuity, while consolidating interpretive authority in tradition authorities.
% ABSENT_VOICES: Secular community members who reject ritual obligation but might value survival competence; women who perform ritual labor but are excluded from interpretive authority in traditional frameworks; anthropologists who question whether the ritual transmits genuine adaptive capacity or only group identity.
% DISAPPEARANCE_RATIONALE: If the commemorative ritual vanished overnight, the mechanism for transmitting embodied survival competence would collapse; the community would lose a primary vehicle for intergenerational adaptation training, and decentralized continuity would fragment into individualized, non-coordinated memory practices. Social arrangements around catastrophe preparedness would reorganize.
% FOUNDING_PROBLEM: How to preserve practical survival knowledge and maintain communal continuity after catastrophic disruptionâexile, persecution, disasterâwhen centralized institutions may fail.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists attest that diasporic communities have survived centuries of dispersion; however, sociologists note that modern assimilated communities have largely abandoned full ritual observance while maintaining group identity through other means, contesting whether the ritual itself is the necessary vehicle. Corroboration from outside the benefiting parties: academic fields of ritual studies and sociology of religion.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.5, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50) reflects the substantial but partial asymmetry between coordination benefit and labor/identity costs. Suppression (0.60) captures active social enforcement of participation norms and the collapse of recognized alternatives within the community. Theater_ratio (0.45) acknowledges that while the survival-competence function is partly genuine, an increasing share of modern performance is identity-maintenance theater rather than adaptive rehearsal. Accessibility_collapse (0.70) registers that within the observant community, alternatives to this specific ritual are socially unrecognized. Resistance (0.30) reflects gradual assimilation rather than organized opposition. The temporal series shows metric substitution over the interval: as secularization reduced the genuine survival-competence payoff in stable liberal states, theater_ratio rose and suppression intensified to maintain participation.
 *
 * PERSPECTIVAL GAP:
 *   From the tradition-authority seat, the constraint appears as ropeâa necessary coordination mechanism preserving civilizationally valuable knowledge. From the ritual-labor-bearer seat, it reads as snareâan enforced obligation extracting invisible labor under cover of sacred necessity. The tangled_rope classification captures this divergence without adjudicating it: both the coordination and extraction are structurally real, encoded in the same ritual performance.
 *
 * DIRECTIONALITY LOGIC:
 *   Tradition authorities sit near the beneficiary pole: they gain institutional continuity and interpretive authority from the ritual's persistence. The observant community sits near symmetric: they genuinely receive survival-competence benefits (intergenerational cohesion, narrative frameworks) but pay substantial time and identity costs. Ritual labor bearers and marginalized obligants sit at the target end: they bear disproportionate costs (invisible labor, forced participation) relative to benefits extracted by the constraint. Assimilated outsiders have exited and thus reverse the directionality vector entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreserving survival knowledge across catastropheâremains contested. Modern assimilated communities maintain group identity without full ritual observance, suggesting the arrangement persists beyond its original survival function for some seats. The R5 genealogy flags this as contested obsolescence: the problem is live for traditionalist communities but arguably dead for liberal diaspora communities. This prevents mislabeling the ritual as pure rope (ignoring the identity-locked extraction) or pure snare (ignoring the genuine coordination benefit). The metrics show rising theater and extraction over the interval, consistent with a coordination mechanism whose founding function has partially atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the commemorative ritual primarily a mechanism for survival competence transmission, or does its core function lie in mourning and boundary maintenance?',
    'Comparative ethnographic analysis of ritual participants'' stated purposes versus observable adaptive outcomes; structural comparison with sibling readings of the same kernel.',
    'If survival competence is secondary to mourning, the constraint''s coordination function weakens and its identity-maintenance extraction strengthens, pushing computed classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel reading ambiguity between survival competence and mourning practice').

omega_variable(
    decentralized_enforcement_ambiguity,
    'Does decentralized ritual maintenance constitute active enforcement, or is the constraint self-enforcing through identity coordination?',
    'Measure exit costs for participants in communities with varying degrees of centralized religious authority; compare coercion profiles across reform, conservative, and orthodox communities.',
    'If enforcement is purely decentralized identity pressure rather than active institutional enforcement, the constraint may function more as rope than tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_enforcement_ambiguity, empirical, 'Ambiguity about whether ritual persistence requires active enforcement or identity self-enforcement').

omega_variable(
    gendered_labor_extraction_visibility,
    'To what extent does the ritual''s survival competence depend on extracted domestic labor that is structurally invisible in traditional framings?',
    'Ethnographic accounting of labor hours and authority distribution within ritual preparation; comparison across gender-egalitarian and traditionalist communities.',
    'If survival competence cannot be produced without this asymmetric labor extraction, the tangled rope classification is confirmed; if egalitarian communities achieve equivalent competence transmission, extraction is contingent rather than necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_labor_extraction_visibility, empirical, 'Whether gendered ritual labor is structurally necessary or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__survival_competence_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__survival_competence_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_function__survival_competence_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__survival_competence_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__survival_competence_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 50, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_function kernel, decomposed from the colloquial label 'ritual preserves catastrophe memory' per the Îµ-invariance principle. The survival_competence_reading isolates the D5 adaptive-transmission claim, distinct from the D1/D4 mourning-practice claim and the hybrid claim. Each reading carries a distinct Îµ and stakeholder surface.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
