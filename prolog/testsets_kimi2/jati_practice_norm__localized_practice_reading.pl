% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati as Localized Coordination Norm (Practice Reading)
 *   domain: social/anthropological/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the localized_practice_reading of the
 *   jati_practice_norm kernel. It treats jati boundaries as decentralized
 *   coordination mechanisms that operate through continuous local
 *   renegotiation rather than fixed scriptural derivation or colonial
 *   administrative reification. The empirical proliferation to 3000+
 *   categories is read as evidence of weak enforcement and strong local
 *   agency, supporting a rope classification where coordination function
 *   dominates over coercion. This is distinguished from the
 *   orthodox_textual_reading (fixed varna framework) and the
 *   colonial_census_reading (external administrative stabilization).
 *
 * KEY AGENTS:
 *   - local_lineages (beneficiary/moderate/constrained): coordinate marriage and economic life through fluid jati boundaries
 *   - occupational_groups (beneficiary/moderate/constrained): maintain economic specialization and credit networks
 *   - village_councils (agenda_setter/moderate/constrained): weakly adjudicate local disputes without rigid enforcement capacity
 *   - social_anthropologists (observer/analytical): document empirical proliferation and fluidity of local practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.18).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.15).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati as Localized Coordination Norm (Practice Reading)").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social/anthropological/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '07d90671-4dad-44ba-a198-29cd77fc37e5').
narrative_ontology:cs_kernel_codification('07d90671-4dad-44ba-a198-29cd77fc37e5', distributed).
narrative_ontology:cs_authority_grounding('07d90671-4dad-44ba-a198-29cd77fc37e5', practice).
narrative_ontology:cs_interpretation_layer_present('07d90671-4dad-44ba-a198-29cd77fc37e5').
narrative_ontology:cs_reading_relation('07d90671-4dad-44ba-a198-29cd77fc37e5', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('07d90671-4dad-44ba-a198-29cd77fc37e5', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('07d90671-4dad-44ba-a198-29cd77fc37e5', foundational, local_practice_as_legitimate_source).
narrative_ontology:cs_axiom_status(local_practice_as_legitimate_source, holdable).
narrative_ontology:cs_axiom_grounding('07d90671-4dad-44ba-a198-29cd77fc37e5', local_practice_as_legitimate_source, conventional).
narrative_ontology:cs_axiom('07d90671-4dad-44ba-a198-29cd77fc37e5', foundational, jati_boundaries_derive_from_practice_not_text).
narrative_ontology:cs_axiom_status(jati_boundaries_derive_from_practice_not_text, holdable).
narrative_ontology:cs_axiom_grounding('07d90671-4dad-44ba-a198-29cd77fc37e5', jati_boundaries_derive_from_practice_not_text, empirically_contingent).
narrative_ontology:cs_reference_frame('07d90671-4dad-44ba-a198-29cd77fc37e5', local_endogamous_coordination).
narrative_ontology:cs_drift_state('07d90671-4dad-44ba-a198-29cd77fc37e5', contemporary_postcolonial, gap(stable, minor, false)).
narrative_ontology:cs_created_at('07d90671-4dad-44ba-a198-29cd77fc37e5', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_lineages).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, occupational_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use jati categories to coordinate marriage alliances, social support, and ritual exchange within locally understood boundaries. They participate in the ongoing reinterpretation of inclusion and exclusion through everyday practice and local dispute resolution, experiencing the arrangement as flexible coordination rather than rigid hierarchy.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_lineages, beneficiary,
    moderate, biographical, constrained, regional).

% Maintain economic specialization and informal credit networks through jati-based trust relationships. The norms facilitate economic coordination and reduce transaction costs in local markets without relying on centralized enforcement or formal contract institutions.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, occupational_groups, beneficiary,
    moderate, biographical, constrained, regional).

% Occasionally adjudicate disputes about marriage compatibility or boundary violations, but lack the capacity or incentive to enforce rigid categorical schemes across the community. Their authority is situational and subject to ongoing local acceptance rather than top-down mandate.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, village_councils, agenda_setter,
    moderate, biographical, constrained, local).

% Document the empirical proliferation of jati categories to more than 3000 distinct labels, observing that local practice exceeds and contradicts fixed textual or administrative schemes. They record marriage and occupational data showing fluid boundaries and weak centralized enforcement.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, social_anthropologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__localized_practice_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates marriage alliances, occupational specialization, and informal credit networks in localized contexts where centralized legal or state institutions are absent or weak. Provides a decentralized mechanism for social boundary maintenance that adapts to local conditions through continuous renegotiation rather than fixed rules.
% TRANSFER_FUNCTION: Moves social trust, marriage opportunities, and economic coordination from unbounded individual choice into structured local circuits. No centralized extraction occurs; the transfer is the coordination surplus itself, distributed among participants.
% ABSENT_VOICES: Colonial administrators and nationalist modernizers who prefer fixed, enumerable categories for governance; orthodox textual scholars who assert a stable scriptural varna derivation; and individuals seeking complete individual autonomy outside group boundaries. These voices are excluded because the localized practice reading foregrounds empirical practitioner agency over external classificatory schemes.
% DISAPPEARANCE_RATIONALE: If the localized jati coordination norms disappeared, marriage patterns would destabilize in contexts where they currently structure alliance networks, occupational credit systems would face higher transaction costs, and local social support mechanisms would require alternative institutional backing. The rearrangement would be unevenâstrongest in regions where state and market institutions remain weak.
% FOUNDING_PROBLEM: How to coordinate marriage, economic specialization, and social trust in localized settings without centralized legal or state institutions to enforce contracts, verify identities, or adjudicate disputes.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographers and social historians from outside the benefiting communities document the ongoing coordination function of jati networks in marriage and economic life. Anti-caste reformers acknowledge the historical absence of robust alternative institutions in many local contexts while disputing the normative legitimacy of the jati solution.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint moves social trust and coordination rather than extracting centralized rents. Suppression is low (0.15) because persistence depends on local participation and renegotiation, not on excluding alternatives or coercing compliance. Theater ratio is minimal (0.10) because enforcement is genuinely weak and performative maintenance is absent. Accessibility collapse is moderate-low (0.30): alternatives exist (state institutions, market networks, religious conversion) but are socially costly in local contexts where jati coordination is functional. Resistance is low (0.20) because participants experience the arrangement as locally beneficial coordination rather than external imposition.
 *
 * PERSPECTIVAL GAP:
 *   The observer seat (anthropologists documenting proliferation) and the beneficiary seats (local practitioners) largely converge in perceiving low extraction and weak enforcement. Divergence would appear if analyzed from the orthodox textual seat or colonial administrative seat, which are modeled as sibling readings rather than perspectives within this constraint. Within this reading, the primary gap is between local participants who experience coordination and external reformers who see hierarchyâthis is handled at the kernel level by the distinct readings rather than within this constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   All seated agents within this constraint experience low directionality: local_lineages, occupational_groups, and village_councils all sit near the beneficiary end because the constraint coordinates rather than extracts. There is no identified victim seat within this reading; the engine should compute all directionalities below the symmetric threshold. The absence of victims is structurally constitutive of the rope claim.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy mislabeling by distinguishing the coordination function (marriage and economic trust networks) from any extraction that might occur in other readings of the same kernel. The constraint would be misclassified as a snare if analyzed from the colonial reading (which identifies external administrative extraction) or the orthodox reading (which identifies ritual hierarchy). By isolating the localized practice as its own constraint with its own epsilon, the framework preserves the rope classification for the genuine coordination component without denying that other structurally distinct constraints (sibling readings) may be more extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is jati best understood as localized coordination norms, colonial administrative reification, or fixed scriptural derivation?',
    'Comparative historical and ethnographic analysis tracking the same communities across pre-colonial, colonial, and post-colonial periods to determine the relative weight of local practice versus external reification versus textual ideology.',
    'If colonial reification dominates, the constraint is a snare or tangled rope with high extraction; if textual derivation dominates, it is a commitment system with deontological grounding; if local practice dominates, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Uncertainty about which kernel reading captures the structural reality of jati').

omega_variable(
    proliferation_vs_consolidation,
    'Does the empirical proliferation of 3000+ jati categories represent genuine continuous local renegotiation, or superficial differentiation masking underlying consolidation?',
    'Network analysis of marriage patterns and occupational boundaries across regions to distinguish autonomous local variation from branching within stable structural parameters.',
    'If superficial, the coordination function is weaker than claimed and extraction or hierarchy may be embedded at a deeper level; if genuine, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_vs_consolidation, empirical, 'Whether category proliferation indicates real local agency or masks structural consolidation').

omega_variable(
    coercion_localization,
    'Is the apparent weak enforcement of jati boundaries a property of the norms themselves, or has enforcement been displaced to adjacent institutions such as the state or market that now perform the suppressive function?',
    'Mapping of marriage and occupation outcomes against state policy and market integration to detect displaced enforcement operating through parallel institutions.',
    'If suppression is displaced rather than absent, the constraint may be a tangled rope with enforcement performed by adjacent institutions rather than the local norms themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_localization, empirical, 'Whether weak local enforcement reflects genuine rope dynamics or displaced coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jati_tr_t10, jati_practice_norm__localized_practice_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__localized_practice_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(jati_tr_t30, jati_practice_norm__localized_practice_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__localized_practice_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(jati_tr_t50, jati_practice_norm__localized_practice_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(jati_be_t10, jati_practice_norm__localized_practice_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__localized_practice_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(jati_be_t30, jati_practice_norm__localized_practice_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__localized_practice_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(jati_be_t50, jati_practice_norm__localized_practice_reading, base_extractiveness, 50, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jati_practice_norm__localized_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, colonial_census_reading).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel decomposes into three structurally distinct constraints because the label 'jati' conflates empirical local practice (low epsilon, rope), colonial administrative reification (high extraction, snare or tangled rope), and scriptural textual claims (commitment system, mountain or false summit candidate). Each reading has different beneficiaries, victim sets, and coordination versus extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
