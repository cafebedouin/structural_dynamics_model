% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Honor Settlement Legitimacy â Contraction Reading
 *   domain: historical/sociological
 *
 * SUMMARY:
 *   This constraint story captures the contraction reading of the
 *   honor_settlement_legitimacy kernel. Under this reading, the cultural
 *   framework of honor itself exited the normative possibility space during
 *   the long transition to modernity, rendering the duel not merely illegal
 *   or imprudent but cognitively unthinkable as a legitimate mode of dispute
 *   settlement. The constraint is modeled as a mountain: a background
 *   cultural limit that operates without active enforcement, extraction, or
 *   identifiable beneficiaries.
 *
 * KEY AGENTS:
 *   - aristocratic_honor_practitioners (extinct as a social formation)
 *   - modern_bourgeois_society (diffuse bearer of the new framework)
 *   - state_monopoly_on_violence (adjacent constraint, not a party to this one)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.03).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.05).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Honor Settlement Legitimacy â Contraction Reading").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical/sociological").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '00c2a10b-9c37-40b4-bc17-9f444f502f71').
narrative_ontology:cs_kernel_codification('00c2a10b-9c37-40b4-bc17-9f444f502f71', implicit).
narrative_ontology:cs_authority_grounding('00c2a10b-9c37-40b4-bc17-9f444f502f71', diffuse_epistemic).
narrative_ontology:cs_reading_relation('00c2a10b-9c37-40b4-bc17-9f444f502f71', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('00c2a10b-9c37-40b4-bc17-9f444f502f71', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('00c2a10b-9c37-40b4-bc17-9f444f502f71', foundational, honor_culture_exited_normative_space).
narrative_ontology:cs_axiom_status(honor_culture_exited_normative_space, holdable).
narrative_ontology:cs_axiom_grounding('00c2a10b-9c37-40b4-bc17-9f444f502f71', honor_culture_exited_normative_space, empirically_contingent).
narrative_ontology:cs_axiom('00c2a10b-9c37-40b4-bc17-9f444f502f71', foundational, dueling_cognitively_unthinkable).
narrative_ontology:cs_axiom_status(dueling_cognitively_unthinkable, holdable).
narrative_ontology:cs_axiom_grounding('00c2a10b-9c37-40b4-bc17-9f444f502f71', dueling_cognitively_unthinkable, empirically_contingent).
narrative_ontology:cs_reference_frame('00c2a10b-9c37-40b4-bc17-9f444f502f71', aristocratic_honor_hegemony).
narrative_ontology:cs_drift_state('00c2a10b-9c37-40b4-bc17-9f444f502f71', industrial_modernity, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('00c2a10b-9c37-40b4-bc17-9f444f502f71', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates interpersonal lethal violence as a mechanism for status and dispute resolution by removing it from the normative possibility space, rendering coordination around honor unnecessary.
% TRANSFER_FUNCTION: No allocative transfer; the constraint operates as a diffuse cultural condition rather than a mechanism moving resources between seated agents.
% ABSENT_VOICES: Residual honor-culture adherents and aristocratic traditionalists who would defend dueling as a legitimate institution are structurally absent from modern normative discourse; their exclusion is a feature of the framework contraction rather than an active suppression campaign.
% DISAPPEARANCE_RATIONALE: If the cognitive unthinkability of dueling vanished overnight, modern legal and medical institutions already prohibit and treat its analogues; the specific cultural incapacity to imagine duelist legitimacy is a background feature whose disappearance would be absorbed by existing enforcement structures without requiring immediate social rearrangement.
% FOUNDING_PROBLEM: Chronic interpersonal lethal violence and status contestation through dueling, which destabilized elite social order and impeded the consolidation of state monopoly over legitimate violence.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists (e.g., Elias), historians of violence, and state legal archives document the decline; the corroboration comes from outside the extinct honor-culture beneficiary set. No live beneficiary attests the founding problem's persistence.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.03, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.03) because the unthinkability of dueling does not extract resources from any seated agent; it functions as a cognitive boundary. Suppression is near-zero (0.05) because the constraint persists without active enforcementâdueling is abandoned not from fear of punishment but from incapacity to conceive it. Accessibility collapse is very high (0.92) because the alternative (honor combat) has become cognitively inaccessible within modern normative space. Resistance is negligible (0.04) because the framework is fully internalized; only fringe historical re-enactors or marginalized subcultures mount symbolic resistance, and they do not target this constraint directly.
 *
 * PERSPECTIVAL GAP:
 *   A residual honor-culture seat would perceive the modern prohibition as a coercive snare or tyrannical rope; from the modern structural seat, the same arrangement appears as a naturalized mountain because no agent need enforce it. The divergence is seated in the framework itself, not in a disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim is declared because the constraint operates as a diffuse background condition. Directionality is uniform and near-neutral: the framework does not structurally subsidize or target any specific agent within the modern order. The state's monopoly on violence is a separate constraint with its own extraction profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâchronic lethal honor violenceâhas been solved. The constraint persists not as a mandate but as a transformed cultural baseline. Mandatrophy does not apply because there is no active mandate that has outlived its function; the function has been sublated into the cultural framework itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalized_culture_or_constructed_hegemony,
    'Is the cognitive unthinkability of dueling a genuine emergent cultural limit (mountain), or does it conceal constructed interests (state monopoly, bourgeois class project) that benefit from rendering honor violence illegitimate?',
    'Cross-cultural and cross-class historical comparison: verify whether dueling abandonment correlates with state formation and bourgeois ascendancy, or whether similar abandonment occurs in societies lacking those specific power configurations.',
    'If the unthinkability is constructed to serve identifiable interests, the constraint is a false summit (tangled_rope or snare) rather than a mountain, and should be reclassified with beneficiaries and victims declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalized_culture_or_constructed_hegemony, conceptual, 'Whether the constraint is a naturalized cultural mountain or a constructed hegemonic false summit').

omega_variable(
    cognitive_unthinkability_operationalization,
    'Can ''cognitive unthinkability'' be operationalized independently of legal prohibition and social sanction, or is it a post-hoc interpretive construct read onto heterogeneous historical behaviors?',
    'Historical discourse analysis measuring the shift from dueling being discussed as risky or imprudent to being discussed as absurd, barbaric, or unthinkable, controlling for legal status.',
    'If unthinkability collapses into prohibition-plus-sanction, the constraint may be a scaffold that ossified, or a rope maintained by diffuse coordination, rather than a true mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_unthinkability_operationalization, empirical, 'Whether cognitive unthinkability is a verifiable historical condition or an interpretive overlay').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel honor_settlement_legitimacy. The contraction reading claims honor culture exited the normative possibility space; the drop reading claims fringe persistence; the composite reading claims overdetermined decline with contraction edge. They are decomposed per the Îµ-invariance principle because their Îµ values and structural claims differ significantly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
