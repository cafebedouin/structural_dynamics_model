% ============================================================================
% CONSTRAINT STORY: interactionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_interactionist_reading, []).

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
 *   constraint_id: interactionist_reading
 *   human_readable: Interactionist Reading of Moral Causation
 *   domain: moral_psychology/philosophy_of_action/social_psychology
 *
 * SUMMARY:
 *   The interactionist reading of moral causation holds that moral action
 *   emerges from the interaction of person and situation: character exists as
 *   a real but context-sensitive disposition, neither fully stable across all
 *   contexts nor fully determined by immediate circumstances. This reading
 *   arose in response to the person-situation debate in moral psychology,
 *   where dispositional accounts (character as stable trait) and situational
 *   accounts (behavior as environmentally determined) each explained part of
 *   the empirical variance but neither captured the full pattern. The
 *   interactionist framework coordinates research, education, and policy
 *   around dual intervention targets—character fortification and situational
 *   design—while resisting collapse to either pole. It is one of three live
 *   readings of the moral causation kernel; the dispositional and situational
 *   readings remain active competitors.
 *
 * KEY AGENTS:
 *   - intervention_designers: organized/mobile — design dual-track programs targeting character and situation; benefit from theoretical legitimation
 *   - moral_educators: organized/mobile — teach character formation while acknowledging situational influence; framework validates their work against situationist critique
 *   - institutional_reformers: organized/mobile — redesign structures to reduce moral failure; framework supports structural reform without absolving individual responsibility
 *   - dispositional_theorists: analytical — hold stable character traits as primary; see interactionist reading as diluting responsibility
 *   - situational_theorists: analytical — hold situational forces as determinative; see interactionist reading as salvaging unsupported character concept
 *   - empirical_psychologists: analytical — measure person-situation variance; provide empirical base but remain normatively agnostic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(interactionist_reading, 0.42).
domain_priors:suppression_score(interactionist_reading, 0.38).
domain_priors:theater_ratio(interactionist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(interactionist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(interactionist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(interactionist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(interactionist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(interactionist_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(interactionist_reading, rope).
narrative_ontology:human_readable(interactionist_reading, "Interactionist Reading of Moral Causation").
narrative_ontology:topic_domain(interactionist_reading, "moral_psychology/philosophy_of_action/social_psychology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(interactionist_reading, 'd83de150-30f7-4627-82b4-3ef8c9d9e8b3').
narrative_ontology:cs_kernel_codification('d83de150-30f7-4627-82b4-3ef8c9d9e8b3', distributed).
narrative_ontology:cs_authority_grounding('d83de150-30f7-4627-82b4-3ef8c9d9e8b3', expertise).
narrative_ontology:cs_interpretation_layer_present('d83de150-30f7-4627-82b4-3ef8c9d9e8b3').
narrative_ontology:cs_reading_relation('d83de150-30f7-4627-82b4-3ef8c9d9e8b3', moral_causation_locus__dispositional_reading, coexists_with).
narrative_ontology:cs_reading_relation('d83de150-30f7-4627-82b4-3ef8c9d9e8b3', moral_causation_locus__situational_reading, coexists_with).
narrative_ontology:cs_axiom('d83de150-30f7-4627-82b4-3ef8c9d9e8b3', foundational, interaction_irreducibility).
narrative_ontology:cs_axiom_status(interaction_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('d83de150-30f7-4627-82b4-3ef8c9d9e8b3', interaction_irreducibility, empirically_contingent).
narrative_ontology:cs_axiom('d83de150-30f7-4627-82b4-3ef8c9d9e8b3', secondary, character_context_sensitivity).
narrative_ontology:cs_axiom_status(character_context_sensitivity, holdable).
narrative_ontology:cs_axiom_grounding('d83de150-30f7-4627-82b4-3ef8c9d9e8b3', character_context_sensitivity, empirically_contingent).
narrative_ontology:cs_reference_frame('d83de150-30f7-4627-82b4-3ef8c9d9e8b3', variance_decomposition_empiricism).
narrative_ontology:cs_drift_state('d83de150-30f7-4627-82b4-3ef8c9d9e8b3', contemporary_meta_analytic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d83de150-30f7-4627-82b4-3ef8c9d9e8b3', '').
narrative_ontology:cs_kernel_id(interactionist_reading, moral_causation_locus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(interactionist_reading, intervention_designers).
narrative_ontology:constraint_beneficiary(interactionist_reading, moral_educators).
narrative_ontology:constraint_beneficiary(interactionist_reading, institutional_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design interventions targeting both character development and situational architecture. The interactionist framework legitimates dual-track programs (virtue cultivation plus institutional safeguards) and provides theoretical grounding for their funding and implementation.
narrative_ontology:constraint_stakeholder(interactionist_reading, intervention_designers, beneficiary,
    organized, biographical, mobile, global).

% Teach character formation while acknowledging situational influence. The framework validates their work against situationist critiques that character education is futile, while preventing complacency that character alone suffices.
narrative_ontology:constraint_stakeholder(interactionist_reading, moral_educators, beneficiary,
    organized, generational, mobile, global).

% Redesign organizational structures to reduce moral failure. The interactionist reading supports structural reform without absolving individual responsibility, enabling policy changes that address systemic factors while maintaining accountability norms.
narrative_ontology:constraint_stakeholder(interactionist_reading, institutional_reformers, beneficiary,
    organized, generational, mobile, national).

% Hold that stable character traits are the primary causal factor in moral action. They see the interactionist reading as diluting personal responsibility and overweighting transient situational factors that virtuous agents should resist.
narrative_ontology:constraint_stakeholder(interactionist_reading, dispositional_theorists, observer,
    analytical, generational, analytical, global).

% Hold that situational forces overwhelmingly determine behavior and stable character is largely illusory. They see the interactionist reading as salvaging an empirically unsupported concept of character to preserve traditional moral frameworks.
narrative_ontology:constraint_stakeholder(interactionist_reading, situational_theorists, observer,
    analytical, generational, analytical, global).

% Measure person-situation variance in moral behavior across contexts. They provide the empirical base for interaction effects but remain agnostic on normative implications, treating the reading as one interpretive frame among several for the same data.
narrative_ontology:constraint_stakeholder(interactionist_reading, empirical_psychologists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates moral psychology research, educational practice, and institutional design around a framework that attributes causal weight to both character and situation, enabling interventions at multiple levels without collapsing to either pole.
% TRANSFER_FUNCTION: Moves legitimacy and resources toward dual-track interventions (character formation plus situational design) and away from single-factor approaches that target only disposition or only environment.
% ABSENT_VOICES: Radical situationists who deny character stability entirely and strict dispositionalists who treat situational influence as moral weakness are structurally marginalized in policy conversations shaped by this reading, though both remain live positions in academic discourse.
% DISAPPEARANCE_RATIONALE: If the interactionist reading vanished, intervention design would polarize toward either pure character education (dispositional pole) or pure structural reform (situational pole), eliminating the theoretical warrant for integrated programs that address both. Funding and institutional support would reallocate along the polarized axis.
% FOUNDING_PROBLEM: The person-situation debate in moral psychology created a theoretical impasse: dispositional accounts could not explain context-sensitivity of moral behavior; situational accounts could not explain cross-situational consistency or moral development. Neither pole alone matched the empirical pattern of partial stability and partial malleability.
% FOUNDING_PROBLEM_CORROBORATION: Empirical psychologists outside the normative debate attest the founding problem remains live: variance decomposition studies consistently show significant main effects for both person and situation plus interaction terms, with no single factor accounting for majority variance. Meta-analyses from neutral methodological reviews corroborate the irreducibility of the interaction structure.
narrative_ontology:disappearance_verdict(interactionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(interactionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(interactionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(interactionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(interactionist_reading_tests).
:- end_tests(interactionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the reading channels resources toward integrated interventions and marginalizes single-factor approaches, but the constraint is not purely extractive—it solves a real coordination problem (the empirical pattern of partial stability) and does not suppress alternatives as illegitimate, only as incomplete. Suppression is moderate-low (0.38): the reading does not foreclose dispositional or situational work, but it does shift funding and legitimacy toward interaction-focused programs. Theater ratio is low (0.28): the framework's dual-track structure genuinely matches the empirical variance pattern; most enforcement activity serves the coordination function rather than defending the reading against evidence. Accessibility collapse is moderate (0.48): alternative single-factor readings remain empirically and normatively live; the interactionist reading does not render them unthinkable. Resistance is moderate (0.52): dispositional and situational theorists actively contest the reading's necessity, arguing their own frameworks can accommodate interaction effects without elevating interaction to primary status.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (intervention designers, educators, reformers), the interactionist reading operates as genuine coordination: it solves the theoretical impasse and enables integrated practice. From the dispositional and situational observer seats, the same reading operates as a compromise framework that dilutes their respective positions—dispositionalists see it as conceding too much to situational determinism; situationists see it as salvaging character concepts the data do not support. The engine computes this divergence from the structural data; the claimed type (rope) reflects the beneficiary-seat experience while the metrics capture the moderate extraction and suppression the reading imposes on the polarized alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervention designers, moral educators, and institutional reformers are beneficiaries: the reading legitimates their dual-track work and provides theoretical grounding for funding. They experience low directionality (d near beneficiary end) because the constraint enables rather than extracts from their practice. Dispositional and situational theorists are observers rather than victims: the reading does not extract from them (they lose no resources) but it does compete for theoretical dominance and shifts the center of gravity in policy conversations. Empirical psychologists are neutral observers: they provide the data the reading interprets but do not depend on any particular normative interpretation for their work.
 *
 * MANDATROPHY ANALYSIS:
 *   The interactionist reading does not exhibit mandatrophy: the founding problem (irreducibility of person-situation interaction in empirical variance) remains live, and the reading's dual-track intervention structure continues to match that empirical pattern. The modest rise in extractiveness over the interval reflects increasing institutionalization (more programs formally adopt the framework, reducing space for single-factor approaches) rather than functional atrophy. The constraint coordinates around a real and persistent empirical structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interaction_term_primacy,
    'Are person-situation interaction effects genuinely primary (irreducible to main effects), or are they a statistical artifact of measurement error and unmeasured third variables?',
    'Longitudinal studies with high-fidelity person and situation measurement, testing whether interaction variance persists after controlling for measurement error and known confounds. If interaction terms collapse under refined measurement, the interactionist reading loses its empirical warrant.',
    'If interaction effects are artifacts, the reading is empirically unsupported and extractiveness would be reclassified upward (the framework would be channeling resources toward a non-existent causal structure). If interaction effects are robust, the reading''s coordination function is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interaction_term_primacy, empirical, 'Whether interaction terms reflect real causal structure or measurement noise.').

omega_variable(
    character_concept_salvage,
    'Is the interactionist reading''s retention of ''character'' a genuine theoretical necessity, or a rhetorical move to preserve traditional moral frameworks against situationist critique?',
    'Conceptual analysis of whether the interactionist framework''s predictions differ from a pure situational account that includes person-as-history-of-situations. If the frameworks are empirically equivalent, the character concept is doing normative rather than explanatory work.',
    'If character is empirically dispensable, the reading''s extraction from situational approaches is unjustified—it would be a normatively motivated compromise rather than an empirically forced one. If character is empirically necessary, the reading''s coordination function is structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(character_concept_salvage, conceptual, 'Whether character as a theoretical posit is empirically necessary or normatively motivated.').

omega_variable(
    intervention_target_separability,
    'Are character-focused and situation-focused interventions genuinely independent targets, or does effective character formation require situational support such that the dual-track structure collapses to a single integrated intervention?',
    'Experimental comparison of character-only, situation-only, and integrated interventions. If integrated interventions show no additive benefit over situation-only interventions, character work is epiphenomenal. If they show additive benefit, the dual-track structure is vindicated.',
    'If intervention targets are not separable, the interactionist reading''s dual-track legitimation is overstated—it would be coordinating around a distinction that does not carve intervention space at the joints. If targets are separable, the reading''s structure matches the causal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_target_separability, empirical, 'Whether character and situation are independent intervention targets or inseparable components.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(interactionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inte_tr_t0, interactionist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(inte_tr_t10, interactionist_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(inte_tr_t20, interactionist_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(inte_tr_t30, interactionist_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(inte_tr_t40, interactionist_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(inte_be_t0, interactionist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inte_be_t10, interactionist_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(inte_be_t20, interactionist_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(inte_be_t30, interactionist_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(inte_be_t40, interactionist_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(inte_su_t0, interactionist_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(inte_su_t10, interactionist_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(inte_su_t20, interactionist_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(inte_su_t30, interactionist_reading, suppression_requirement, 30, 0.37).
narrative_ontology:measurement(inte_su_t40, interactionist_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(interactionist_reading, dispositional_reading).
narrative_ontology:affects_constraint(interactionist_reading, situational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the moral_causation_locus kernel. The dispositional_reading holds character traits as stable and primary; the situational_reading holds situational forces as determinative and character as illusory. The interactionist_reading (this constraint) holds interaction effects as primary and character as real but context-sensitive. All three readings interpret the same empirical base (variance in moral behavior across persons and situations) but attribute different causal primacy. The readings coexist as competing frameworks; none forecloses the others, but each influences resource allocation and legitimacy in intervention design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
