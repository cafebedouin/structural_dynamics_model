% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Dueling Disappearance Mechanism â Overdetermined Composite Reading
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   Between roughly 1830 and 1900 in the United States, the social practice
 *   of dueling was eliminated not by any single cause but by the simultaneous
 *   convergence of legal prohibition, institutional modernization, cultural
 *   shift toward dignity norms, and the traumatic aftereffects of the Civil
 *   War. This constraint story instantiates the overdetermined-composite
 *   reading of the dueling disappearance mechanism kernel, treating the
 *   elimination as a hybrid coordination-extraction regime: it coordinated
 *   the transition to state and commercial dispute resolution while
 *   extracting the constitutive practice of honor-culture elites. The
 *   claim/metric independence is preserved by authoring a tangled_rope claim
 *   while acknowledging that the non-separability of causal pathways makes
 *   any single Îµ inherently contestable.
 *
 * KEY AGENTS:
 *   - State legal apparatus (institutional/agenda_setter): enforces prohibition and claims monopoly violence
 *   - Bourgeois institutions (institutional/beneficiary): absorb disputes through courts, banks, and press
 *   - Commercial middle class (moderate/beneficiary): gains stable commerce and formal dispute access
 *   - Reformist culture elites (powerful/agenda_setter): displace honor culture with dignity norms
 *   - Traditional honor elites (powerful/payer): lose constitutive dispute mechanism and status practice
 *   - Historical analyst (analytical/observer): evaluates the composite mechanism from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.58).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.75).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Dueling Disappearance Mechanism â Overdetermined Composite Reading").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, 'dddc85f8-ed43-40a3-8c1e-3687b3d9edc4').
narrative_ontology:cs_kernel_codification('dddc85f8-ed43-40a3-8c1e-3687b3d9edc4', distributed).
narrative_ontology:cs_authority_grounding('dddc85f8-ed43-40a3-8c1e-3687b3d9edc4', expertise).
narrative_ontology:cs_reading_relation('dddc85f8-ed43-40a3-8c1e-3687b3d9edc4', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('dddc85f8-ed43-40a3-8c1e-3687b3d9edc4', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('dddc85f8-ed43-40a3-8c1e-3687b3d9edc4', foundational, plural_causation_over_monocausal).
narrative_ontology:cs_axiom_status(plural_causation_over_monocausal, holdable).
narrative_ontology:cs_axiom_grounding('dddc85f8-ed43-40a3-8c1e-3687b3d9edc4', plural_causation_over_monocausal, conventional).
narrative_ontology:cs_axiom('dddc85f8-ed43-40a3-8c1e-3687b3d9edc4', foundational, no_single_mechanism_dominated).
narrative_ontology:cs_axiom_status(no_single_mechanism_dominated, holdable).
narrative_ontology:cs_axiom_grounding('dddc85f8-ed43-40a3-8c1e-3687b3d9edc4', no_single_mechanism_dominated, empirically_contingent).
narrative_ontology:cs_reference_frame('dddc85f8-ed43-40a3-8c1e-3687b3d9edc4', overdetermined_causal_plurality).
narrative_ontology:cs_drift_state('dddc85f8-ed43-40a3-8c1e-3687b3d9edc4', contemporary_historical_synthesis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dddc85f8-ed43-40a3-8c1e-3687b3d9edc4', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, commercial_middle_class).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, reformist_culture_elites).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_honor_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces anti-dueling statutes and claims monopoly on legitimate violence. Benefits from expanded sovereignty and the elimination of elite self-help as a competing authority.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_apparatus, beneficiary).

% Courts, commercial banks, and the print press provide institutional substitutes for dueling. They absorb disputes formerly settled by personal combat, expanding jurisdiction, revenue, and social influence.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_institutions, beneficiary,
    institutional, generational, mobile, national).

% Benefits from reduced violent disruption to commerce and from expanded access to formal dispute resolution previously dominated by honor-elite personal combat.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, commercial_middle_class, beneficiary,
    moderate, biographical, mobile, national).

% Promotes dignity-culture norms through education, religious discourse, and literature. Gains cultural authority as honor-culture practices recede and emotional self-regulation becomes the dominant elite virtue.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, reformist_culture_elites, agenda_setter,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, reformist_culture_elites, beneficiary).

% Bear the loss of dueling as a constitutive mechanism for dispute resolution, status confirmation, and masculine identity. Their social position was historically fused with honor-culture practices; exit requires abandoning a core component of elite selfhood.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_honor_elites, payer,
    powerful, biographical, identity_locked, regional).

% Observes the composite mechanism from outside the historical period, evaluating whether the convergence of legal, institutional, cultural, and traumatic forces is better modeled as overdetermination or as separable causes.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, historical_analyst, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__overdetermined_composite_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transition from honor-based violent self-help to modern state-mediated and commercial dispute resolution, stabilizing elite behavior during state formation and market expansion.
% TRANSFER_FUNCTION: Moves the right to resolve disputes and restore honor from individual armed elites to state courts, commercial institutions, and print-cultural reputation systems; simultaneously moves status capital from honor-based to dignity-based registers.
% ABSENT_VOICES: Lower-class communities excluded from both dueling culture and its reform; women as objects of honor disputes but never subjects in the mechanism; free Black Americans and enslaved people for whom dueling was structurally irrelevant but whose simultaneous exclusion from the new legal alternatives constituted a parallel constraint.
% DISAPPEARANCE_RATIONALE: If the composite suppression regime vanished overnight during its active period, dueling would resume as a dispute mechanism, state monopoly on violence would fracture, commercial dispute resolution would lose jurisdiction, and the cultural framework would revert toward honor-based self-help â the entire social order of the late nineteenth century would reorganize around the return of personal combat.
% FOUNDING_PROBLEM: Elite violence through personal combat was destabilizing public order, interrupting commercial relations, killing influential young men, and threatening the state's claim to a monopoly on legitimate force.
% FOUNDING_PROBLEM_CORROBORATION: State legislators and reformers attested the problem from within the benefiting parties. European travelers and diplomatic observers recorded the destabilizing effects from outside the American elite. Later historians debate whether the problem was genuinely acute or constructed to justify expanding state and commercial jurisdiction.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects substantial but not total extraction: the honor elite lost a practice central to their identity, but the coordination benefit to state formation and commerce was real. Suppression (0.75) is high because the constraint persisted only through active legal enforcement, institutional substitution, and cultural stigmatization. Theater ratio (0.42) rises over time as dueling became rare and anti-dueling enforcement increasingly targeted a vanished practice. Accessibility collapse (0.80) is high because once the composite regime was in place, dueling became practically unthinkable as an alternative. Resistance (0.60) reflects persistent elite pushback, especially in the antebellum and postbellum South, before final normalization.
 *
 * PERSPECTIVAL GAP:
 *   From the state and bourgeois-institutional seats, the arrangement is genuine coordination that solved a collective-action problem of elite violence and stabilized commercial relations. From the traditional honor-elite seat, the same structure is extraction that destroyed a legitimate and identity-constitutive social practice through convergent suppression. The commercial middle class experiences a mixed seat: coordination benefit from reduced violence, diffuse cost from living under expanded state surveillance and commercial dependency.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal apparatus and bourgeois institutions are clear beneficiaries (low d): they collect monopoly authority and expanded jurisdiction. The commercial middle class and reformist elites are secondary beneficiaries (moderate-low d). The traditional honor elites are the primary targets (high d): they bear the cost of practice elimination under convergent suppression. Because their exit is identity_locked rather than merely constrained, their effective extraction is amplified. The historical analyst sits at analytical exit with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy mislabeling because it carries a genuine coordination function â the suppression of lethal elite violence and the provision of institutional alternatives â alongside clear asymmetric extraction from honor-culture elites. It is not a snare because the coordination is not cover; the courts, banks, and dignity culture actually operated as substitutes. It is not a piton because the practice genuinely transformed rather than decayed in place. The founding problem (elite violence) was contested but not fabricated post hoc, and the mechanism did not outlive its function; rather, it succeeded and dissolved into modern institutional arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_pathway_nonseparability,
    'Can the independent causal contributions of legal prohibition, institutional modernization, cultural shift, and Civil War trauma be separated empirically, or do they form a non-decomposable composite?',
    'Counterfactual historical analysis or structural equation modeling of regional variation in mechanism timing; in practice, no pure natural experiment is available.',
    'If non-separable, the constraint''s extractiveness is irreducibly distributed across mechanisms and no single Îµ is definable for the composite, supporting the authored non-separability claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_pathway_nonseparability, conceptual, 'Whether the four causal pathways can be isolated or form a composite whole').

omega_variable(
    victim_mechanism_dependency,
    'Which social group bore the primary cost of dueling''s suppression depends on which mechanism dominated: legal prohibition (cost to would-be duelists), institutional substitution (cost to honor elites losing jurisdiction), cultural shift (cost to identity-fused honor practitioners), or war trauma (cost to postbellum Southern gentry)?',
    'Regional and temporal disaggregation of resistance patterns, social stratification data, and memoir evidence from 1830-1900.',
    'Different victim identification changes the directionality profile and seat classification; if costs were borne diffusely across mechanisms, the extraction is more tangled-rope than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_mechanism_dependency, empirical, 'Ambiguity of victim seat depending on dominant suppression mechanism').

omega_variable(
    kernel_reading_sibling_boundary,
    'Does the overdetermined reading subsume its monocausal siblings as partial truths, or does it represent a mutually exclusive historiographic framework?',
    'Analysis of whether historians can consistently hold the overdetermined reading while granting causal sufficiency to a single mechanism; logical examination of the independence claim.',
    'Determines whether the kernel is a family of coexisting readings or a competitive replacement structure; affects whether coexists_with or influences is the correct relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_boundary, conceptual, 'Boundary between overdetermined and monocausal readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dueling_od_tr_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dueling_od_tr_t14, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 14, 0.28).
narrative_ontology:measurement(dueling_od_tr_t28, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 28, 0.4).
narrative_ontology:measurement(dueling_od_tr_t42, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 42, 0.52).
narrative_ontology:measurement(dueling_od_tr_t56, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 56, 0.62).
narrative_ontology:measurement(dueling_od_tr_t70, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 70, 0.68).

% Extraction over time
narrative_ontology:measurement(dueling_od_be_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(dueling_od_be_t14, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 14, 0.42).
narrative_ontology:measurement(dueling_od_be_t28, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 28, 0.55).
narrative_ontology:measurement(dueling_od_be_t42, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 42, 0.68).
narrative_ontology:measurement(dueling_od_be_t56, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 56, 0.76).
narrative_ontology:measurement(dueling_od_be_t70, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 70, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(dueling_od_su_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dueling_od_su_t14, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 14, 0.55).
narrative_ontology:measurement(dueling_od_su_t28, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 28, 0.68).
narrative_ontology:measurement(dueling_od_su_t42, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 42, 0.8).
narrative_ontology:measurement(dueling_od_su_t56, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 56, 0.86).
narrative_ontology:measurement(dueling_od_su_t70, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 70, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% This constraint is the overdetermined-composite reading of the dueling disappearance mechanism kernel, decomposed from the contraction and institutional-displacement readings because each instantiates a structurally distinct explanatory claim with different beneficiary structures and Îµ profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
