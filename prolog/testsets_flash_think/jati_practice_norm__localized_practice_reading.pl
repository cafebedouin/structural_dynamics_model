% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundaries as Localized Practice Norms
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'localized_practice_reading' of
 *   the 'jati_practice_norm' kernel. It describes jati boundaries as dynamic,
 *   continuously renegotiated coordination norms within local communities,
 *   leading to a proliferation of categories (3000+). This reading emphasizes
 *   the adaptive and fluid nature of these social structures, contrasting
 *   with interpretations that view them as fixed by text or administrative
 *   decree. The low extractiveness and suppression reflect the continuous,
 *   bottom-up adaptation rather than top-down enforcement.
 *
 * KEY AGENTS:
 *   - local_jati_groups: Primary beneficiary (moderate/constrained) — define and benefit from norms
 *   - community_leaders: Agenda setter (organized/mobile) — facilitate renegotiation
 *   - individual_members: Payer (powerless/identity_locked) — adhere to norms, bear social costs
 *   - anthropological_observers: Analytical observer (analytical/analytical) — study the dynamics
 *   - orthodox_religious_authorities: Excluded (institutional/arbitrage) — advocate for fixed textual definitions, but are ignored
 *   - state_census_bureau: Excluded (institutional/analytical) — attempt to reify categories, but local practice resists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.15).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.2).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundaries as Localized Practice Norms").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social_anthropology/religious_studies/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '83e8a244-d7f3-49f3-bbfa-4824a7275f05').
narrative_ontology:cs_kernel_codification('83e8a244-d7f3-49f3-bbfa-4824a7275f05', distributed).
narrative_ontology:cs_authority_grounding('83e8a244-d7f3-49f3-bbfa-4824a7275f05', practice).
narrative_ontology:cs_interpretation_layer_present('83e8a244-d7f3-49f3-bbfa-4824a7275f05').
narrative_ontology:cs_reading_relation('83e8a244-d7f3-49f3-bbfa-4824a7275f05', jati_practice_norm__orthodox_textual_reading, forecloses).
narrative_ontology:cs_reading_relation('83e8a244-d7f3-49f3-bbfa-4824a7275f05', jati_practice_norm__colonial_census_reading, forecloses).
narrative_ontology:cs_axiom('83e8a244-d7f3-49f3-bbfa-4824a7275f05', foundational, jati_boundaries_are_locally_negotiated).
narrative_ontology:cs_axiom_status(jati_boundaries_are_locally_negotiated, holdable).
narrative_ontology:cs_axiom_grounding('83e8a244-d7f3-49f3-bbfa-4824a7275f05', jati_boundaries_are_locally_negotiated, conventional).
narrative_ontology:cs_axiom('83e8a244-d7f3-49f3-bbfa-4824a7275f05', foundational, jati_categories_are_fluid_and_proliferating).
narrative_ontology:cs_axiom_status(jati_categories_are_fluid_and_proliferating, holdable).
narrative_ontology:cs_axiom_grounding('83e8a244-d7f3-49f3-bbfa-4824a7275f05', jati_categories_are_fluid_and_proliferating, empirically_contingent).
narrative_ontology:cs_reference_frame('83e8a244-d7f3-49f3-bbfa-4824a7275f05', dynamic_local_consensus).
narrative_ontology:cs_drift_state('83e8a244-d7f3-49f3-bbfa-4824a7275f05', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('83e8a244-d7f3-49f3-bbfa-4824a7275f05', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_jati_groups).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, community_leaders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, individual_members).
narrative_ontology:constraint_vindicates(jati_practice_norm__localized_practice_reading, social_cohesion_theory).
narrative_ontology:constraint_vindicates(jati_practice_norm__localized_practice_reading, local_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups define their own boundaries and norms through ongoing social interaction and negotiation. They benefit from the social cohesion, mutual support, and clear roles these norms provide within their local context.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_jati_groups, beneficiary,
    moderate, biographical, constrained, local).

% Leaders within local communities facilitate the continuous renegotiation of jati boundaries and norms. They uphold local traditions and mediate disputes, ensuring the adaptive function of the constraint. Their authority is derived from their role in maintaining social order and cohesion.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, community_leaders, agenda_setter,
    organized, biographical, mobile, local).

% Individual members adhere to the locally negotiated jati norms, bearing the social costs of deviation (e.g., exclusion from marriage networks or social support). They also benefit from the strong group identity and social structure these norms provide, making exit difficult due to identity fusion.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, individual_members, payer,
    powerless, immediate, identity_locked, local).

% Academics who study the dynamic and fluid nature of jati boundaries, documenting their proliferation and local adaptation. They analyze the social function and economic implications of these norms without direct participation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, anthropological_observers, observer,
    analytical, generational, analytical, global).

% Religious authorities who would argue for fixed, scriptural definitions of varna and jati, viewing local deviations as ritual pollution. Their influence is largely absent from the actual local practice and renegotiation processes described by this reading.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, orthodox_religious_authorities, excluded,
    institutional, civilizational, arbitrage, national).

% Government agencies that attempt to categorize and fix jati identities for administrative purposes (e.g., census, affirmative action). Their efforts to reify categories are often resisted or ignored by local, fluid practices, making them largely excluded from the actual operation of this constraint.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, state_census_bureau, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__localized_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(jati_practice_norm__localized_practice_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible framework for social organization, marriage networks, occupational specialization, and mutual support within local communities, adapting to changing social and economic circumstances.
% TRANSFER_FUNCTION: Transfers social status, occupational roles, and marriage eligibility within local groups, from those who adhere to those who deviate, ensuring social cohesion and order.
% ABSENT_VOICES: Orthodox religious authorities and state census bureaus are largely absent from the actual local renegotiation. They would advocate for fixed, external definitions of jati, but their attempts to impose these are resisted or ignored by local practice.
% DISAPPEARANCE_RATIONALE: If jati boundaries and their continuous renegotiation vanished overnight, local social structures, marriage patterns, and traditional occupational networks would collapse, leading to significant social disorganization and a need for new, potentially less adaptive, forms of community organization.
% FOUNDING_PROBLEM: The need for flexible social organization, division of labor, and mutual support in diverse, often agrarian, communities, allowing for local adaptation and identity formation in response to ecological and economic changes.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies and local community narratives consistently corroborate the ongoing need for these social organizing principles, even as their specific forms evolve. This corroboration comes from independent academic research and the lived experiences documented by community members, not solely from those who benefit from the system.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.15) and suppression (0.20) reflect the nature of these norms as continuously renegotiated and locally adapted. The proliferation of thousands of distinct jati categories across India indicates weak, decentralized enforcement and a strong emphasis on local autonomy, rather than a coercive, extractive system. Theater ratio is low (0.10) because the norms are genuinely functional and adaptive, with little performative maintenance for a non-existent central authority. The metrics are consistent with a Rope, where coordination benefits outweigh minimal costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of local jati groups and community leaders, these norms are essential, adaptive coordination mechanisms. Individual members experience the constraint as a necessary part of their social identity, with costs for deviation. External observers, such as orthodox religious authorities or state census bureaus, might perceive these fluid boundaries as a deviation from 'true' or 'official' categories, but their perspectives are largely excluded from the actual operation of this localized practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Local jati groups and community leaders are beneficiaries, actively shaping and benefiting from the coordination. Individual members are payers, bearing the social costs of adherence but also benefiting from identity and community. Orthodox religious authorities and state census bureaus are structurally excluded, as their attempts to impose fixed definitions are not part of this localized, fluid system. Anthropological observers maintain an analytical distance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''localized_practice_reading'' of the ''jati_practice_norm'' kernel?',
    'Comparison with alternative readings (orthodox_textual_reading, colonial_census_reading) to confirm distinct structural properties and underlying axioms.',
    'If misidentified, the classification would incorrectly attribute properties of other readings to this one, leading to an inaccurate assessment of extractiveness and coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint as one specific reading of the jati practice norm.').

omega_variable(
    impact_of_orthodox_textual_reading,
    'What would be the structural impact on this constraint if the ''orthodox_textual_reading'' of jati norms were to gain dominant influence in local practice?',
    'Empirical observation of communities where scriptural authority is strictly enforced, or historical analysis of periods of religious revivalism and their impact on local jati fluidity.',
    'If the orthodox reading became dominant, extractiveness and suppression would likely increase significantly, as deviation from fixed scriptural norms would be met with ritual pollution and social exclusion, transforming this Rope into a Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_orthodox_textual_reading, empirical, 'Impact of fixed scriptural definitions on local jati practice.').

omega_variable(
    impact_of_colonial_census_reading,
    'What would be the structural impact on this constraint if the ''colonial_census_reading'' (reified, administratively fixed categories) were to fully reassert itself in local practice?',
    'Analysis of the effects of state-led categorization drives on local social fluidity, or counterfactual modeling of administrative enforcement of fixed jati lists.',
    'If the colonial census reading were to reassert itself, the fluidity and renegotiation characteristic of this reading would diminish, leading to increased suppression and potentially extractiveness as administrative categories are used for resource allocation or political control, shifting the constraint towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_colonial_census_reading, empirical, 'Impact of administratively fixed categories on local jati practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(jati_tr_t10, jati_practice_norm__localized_practice_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__localized_practice_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(jati_tr_t30, jati_practice_norm__localized_practice_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__localized_practice_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(jati_tr_t50, jati_practice_norm__localized_practice_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(jati_be_t10, jati_practice_norm__localized_practice_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__localized_practice_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(jati_be_t30, jati_practice_norm__localized_practice_reading, base_extractiveness, 30, 0.13).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__localized_practice_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement(jati_be_t50, jati_practice_norm__localized_practice_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__localized_practice_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(jati_su_t10, jati_practice_norm__localized_practice_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__localized_practice_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement(jati_su_t30, jati_practice_norm__localized_practice_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement(jati_su_t40, jati_practice_norm__localized_practice_reading, suppression_requirement, 40, 0.19).
narrative_ontology:measurement(jati_su_t50, jati_practice_norm__localized_practice_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'jati_practice_norm' kernel. Each reading (localized_practice, orthodox_textual, colonial_census) represents a different structural interpretation of jati boundaries, with distinct ε values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
