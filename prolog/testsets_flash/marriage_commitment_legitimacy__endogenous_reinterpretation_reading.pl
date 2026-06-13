% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Marriage Commitment Legitimacy (Endogenous Reinterpretation Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'endogenous reinterpretation' reading of
 *   the Manifesto, a pivotal document in the history of a religious
 *   institution. This reading posits that the Manifesto, which reversed a
 *   long-standing practice, was a genuine prophetic revelation from God,
 *   commanded to preserve the Church for higher purposes. Federal pressure is
 *   acknowledged as a catalyst but not the primary cause. The
 *   reinterpretation maintains theological continuity by reframing the prior
 *   practice as a temporary stage in a larger divine plan, with monogamy
 *   representing a new covenant stage. This reading emphasizes the agency of
 *   the church leadership and the ongoing nature of divine guidance.
 *
 * KEY AGENTS:
 *   - church_leadership: Agenda setter (institutional/generational) — interprets and promulgates revelation, enforces new practice.
 *   - devout_members: Payer/Beneficiary (moderate/biographical) — adheres to new practice, benefits from perceived divine favor and church preservation.
 *   - dissenting_members: Excluded (powerless/biographical) — struggles with the reinterpretation, may leave or be excommunicated.
 *   - prophetic_succession_doctrine: Beneficiary (analytical/civilizational) — its legitimacy is reinforced by the reinterpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.15).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.25).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Marriage Commitment Legitimacy (Endogenous Reinterpretation Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '4b0f0d1a-42d6-414a-b794-73ddf9e291ac').
narrative_ontology:cs_kernel_codification('4b0f0d1a-42d6-414a-b794-73ddf9e291ac', fixed_text).
narrative_ontology:cs_authority_grounding('4b0f0d1a-42d6-414a-b794-73ddf9e291ac', lineage).
narrative_ontology:cs_interpretation_layer_present('4b0f0d1a-42d6-414a-b794-73ddf9e291ac').
narrative_ontology:cs_reading_relation('4b0f0d1a-42d6-414a-b794-73ddf9e291ac', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b0f0d1a-42d6-414a-b794-73ddf9e291ac', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('4b0f0d1a-42d6-414a-b794-73ddf9e291ac', foundational, prophetic_revelation_is_supreme).
narrative_ontology:cs_axiom_status(prophetic_revelation_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('4b0f0d1a-42d6-414a-b794-73ddf9e291ac', prophetic_revelation_is_supreme, theological).
narrative_ontology:cs_axiom('4b0f0d1a-42d6-414a-b794-73ddf9e291ac', foundational, divine_commands_can_evolve_for_higher_purposes).
narrative_ontology:cs_axiom_status(divine_commands_can_evolve_for_higher_purposes, holdable).
narrative_ontology:cs_axiom_grounding('4b0f0d1a-42d6-414a-b794-73ddf9e291ac', divine_commands_can_evolve_for_higher_purposes, theological).
narrative_ontology:cs_reference_frame('4b0f0d1a-42d6-414a-b794-73ddf9e291ac', unbroken_prophetic_guidance).
narrative_ontology:cs_drift_state('4b0f0d1a-42d6-414a-b794-73ddf9e291ac', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4b0f0d1a-42d6-414a-b794-73ddf9e291ac', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, devout_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, devout_members).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_guidance_of_church).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_revelation_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for interpreting and promulgating divine revelation, including the Manifesto. They enforce the new practice and articulate the theological justification for the change, preserving the institution's prophetic authority and continuity. Their identity is fused with the church's mission.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Adhere to the new practice as commanded by prophetic authority. They experience the cost of changing deeply held traditions but benefit from the perceived divine favor and the preservation of their religious community. Their identity is deeply tied to their membership.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, devout_members, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, devout_members, beneficiary).

% Struggle with the reinterpretation, feeling that it contradicts prior divine commands or core theological principles. They face social pressure to conform and may choose to leave the church or be excommunicated, bearing significant personal and social costs.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dissenting_members, excluded,
    powerless, biographical, constrained, local).

% The theological principle that divine guidance continues through a living prophet. Its legitimacy is reinforced by this reading, as the Manifesto is presented as a demonstration of ongoing revelation and the prophet's authority to guide the church through changing times.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective identity and practice of the religious community around a new, divinely sanctioned understanding of marriage, ensuring institutional survival and perceived divine favor amidst external pressures.
% TRANSFER_FUNCTION: Transfers adherence to a new practice (monogamy) from individual members to the collective, in exchange for the perceived preservation of the church and its prophetic authority. It also transfers interpretive authority from historical precedent to current prophetic revelation.
% ABSENT_VOICES: Dissenting members who believe the reinterpretation contradicts prior, unchangeable divine commands are marginalized or leave the community. Their voices, if present and empowered, would challenge the legitimacy of the reinterpretation as a genuine prophetic act.
% DISAPPEARANCE_RATIONALE: If the endogenous reinterpretation of the Manifesto vanished, the church's current marriage practice would lose its primary theological justification. This would lead to widespread doctrinal confusion, potential schism, and a crisis of prophetic authority, fundamentally reorganizing the institution's structure and identity.
% FOUNDING_PROBLEM: The problem of reconciling a divinely commanded practice (plural marriage) with escalating federal legal and political pressure that threatened the church's existence and autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Church leadership attests that the problem of preserving the church's divine mission in a hostile world remains live, and the Manifesto's reinterpretation was a necessary, divinely-guided solution. Independent historians and sociologists, while acknowledging the federal pressure, also document internal theological discussions and the leadership's framing of the event as a spiritual imperative, corroborating the 'live' status from a broader perspective.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because, from this reading's perspective, the change is divinely mandated for the collective good, not for institutional rent-seeking. Suppression (0.25) is present but moderate, reflecting the internal pressure to conform to prophetic authority rather than overt coercion. Theater ratio (0.1) is low, as the reinterpretation is seen as a sincere theological adjustment, not a performance. Accessibility collapse (0.8) is high because, within this framework, the divine command leaves little room for alternative interpretations or practices. Resistance (0.1) is low, as most devout members accept the prophetic authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership and devout members, this is a necessary, divinely-guided adaptation (Rope). From the perspective of dissenting members, it might feel more like a Snare, where their deeply held beliefs are suppressed for institutional survival. The engine's classification will reflect the structural position of each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership benefits from maintaining prophetic succession and institutional legitimacy (low d). Devout members are beneficiaries of the church's preservation but pay by conforming to the new practice (d near symmetric). Dissenting members are targets, as their prior commitments are invalidated (high d). The prophetic succession doctrine itself is a beneficiary, as its authority is reinforced.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a Snare by emphasizing the theological justification and the perceived divine mandate, rather than focusing solely on the coercive aspects of institutional change. It frames the 'mandate' as evolving under divine guidance, thus avoiding a 'mandatrophy' diagnosis from this specific perspective. The founding problem (preserving the church) is seen as live, and the reinterpretation is the solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_causation,
    'Was the Manifesto primarily an endogenous prophetic revelation, or an exogenous capitulation to federal pressure?',
    'Historical analysis of internal church records, prophetic statements, and federal correspondence immediately preceding the Manifesto''s issuance, seeking evidence of independent theological development versus direct coercive threats.',
    'If primarily exogenous, the constraint shifts towards a Snare or Tangled Rope, with federal government as the primary agenda_setter and the church leadership as a constrained payer. If endogenous, the Rope classification holds, emphasizing the church''s agency and the divine authority''s role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_causation, empirical, 'Ambiguity regarding the primary cause of the Manifesto''s issuance.').

omega_variable(
    theological_continuity_vs_discontinuity,
    'Does the reinterpretation of marriage doctrine genuinely preserve theological continuity, or does it represent a fundamental discontinuity masked by reinterpretation?',
    'Comparative theological analysis of pre- and post-Manifesto doctrinal texts, focusing on the logical coherence and consistency of the ''higher purposes'' argument with prior revelations. Examination of dissenting theological voices within the church.',
    'If discontinuity is dominant, the constraint''s legitimacy erodes, potentially shifting it towards a Piton (theatrical maintenance of a broken mandate) or a Snare (coercive enforcement of a new, ungrounded doctrine). If continuity is robust, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_continuity_vs_discontinuity, conceptual, 'Ambiguity regarding the theological coherence of the reinterpretation.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''marriage_commitment_legitimacy'' kernel. This reading, ''endogenous_reinterpretation_reading'', asserts the Manifesto as genuine prophetic revelation for higher purposes. How would the classification change if the ''exogenous_override_reading'' (federal coercion) or ''hybrid_pragmatic_reading'' (strategic adaptation) were adopted?',
    'Adopting the ''exogenous_override_reading'' would increase extractiveness and suppression, shifting towards a Snare. Adopting the ''hybrid_pragmatic_reading'' would increase theater_ratio and potentially extractiveness, shifting towards a Tangled Rope or Piton.',
    'The classification of this constraint is highly dependent on which reading of the kernel is accepted, demonstrating the kernel''s contested nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is a specific reading of the marriage_commitment_legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_legitimacy' kernel. This 'endogenous_reinterpretation_reading' emphasizes divine revelation and theological continuity, contrasting with the 'exogenous_override_reading' (federal coercion) and 'hybrid_pragmatic_reading' (strategic adaptation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
