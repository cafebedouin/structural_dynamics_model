% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: UN Security Council P5 Veto — Coordination Reading
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint story represents the coordination_reading of the Article
 *   27 veto power kernel. It interprets the P5 veto as a necessary unanimity
 *   gate that solves a collective-action problem: without it, any Security
 *   Council resolution could compel a nuclear-armed great power into military
 *   confrontation it rejects, creating unacceptable escalation risk. The
 *   veto's extraction (ε ≈ 0.03) derives from the small but real cost of
 *   maintaining the veto mechanism (diplomatic friction, blocked resolutions
 *   on non-existential issues) — not from rents captured by the P5. The
 *   beneficiary is the international system as a whole, including non-P5
 *   states, because great-power war avoidance is a global public good. This
 *   reading coexists with the oligopoly_reading (which sees the veto as
 *   extracting authority rents) and the sovereignty_reading (which sees it as
 *   instantiating Westphalian non-consent). The coordination_reading does not
 *   foreclose either sibling; it occupies a distinct normative claim about
 *   the veto's structural function.
 *
 * KEY AGENTS:
 *   - international_system: Primary beneficiary (analytical/universal) — receives the global public good of great-power war avoidance
 *   - p5_states: Institutional agenda_setters (institutional/global) — hold the veto power, administer the constraint, benefit from stability
 *   - non_p5_states: Beneficiaries (organized/regional_to_global) — receive war-prevention benefit without holding veto; exit constrained by UN membership
 *   - nuclear_non_p5_states: Excluded (powerful/regional) — nuclear-armed but without veto; their confrontation risk is not directly gated
 *   - security_council_institution: Agenda_setter (institutional/global) — administers the veto mechanism, sets procedural rules
 *   - analytical_observer: Observer (analytical/universal) — assesses the constraint's structural function across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.03).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.05).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "UN Security Council P5 Veto — Coordination Reading").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, 'af36804f-91e6-43a8-954b-737539c51708').
narrative_ontology:cs_kernel_codification('af36804f-91e6-43a8-954b-737539c51708', formalized).
narrative_ontology:cs_authority_grounding('af36804f-91e6-43a8-954b-737539c51708', lineage).
narrative_ontology:cs_interpretation_layer_present('af36804f-91e6-43a8-954b-737539c51708').
narrative_ontology:cs_reading_relation('af36804f-91e6-43a8-954b-737539c51708', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('af36804f-91e6-43a8-954b-737539c51708', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('af36804f-91e6-43a8-954b-737539c51708', foundational, great_power_war_prevention_requires_unanimity_gate).
narrative_ontology:cs_axiom_status(great_power_war_prevention_requires_unanimity_gate, holdable).
narrative_ontology:cs_axiom_grounding('af36804f-91e6-43a8-954b-737539c51708', great_power_war_prevention_requires_unanimity_gate, empirically_contingent).
narrative_ontology:cs_axiom('af36804f-91e6-43a8-954b-737539c51708', foundational, veto_cost_is_coordination_overhead_not_rent).
narrative_ontology:cs_axiom_status(veto_cost_is_coordination_overhead_not_rent, holdable).
narrative_ontology:cs_axiom_grounding('af36804f-91e6-43a8-954b-737539c51708', veto_cost_is_coordination_overhead_not_rent, empirically_contingent).
narrative_ontology:cs_reference_frame('af36804f-91e6-43a8-954b-737539c51708', charter_unanimity_gate_1945).
narrative_ontology:cs_drift_state('af36804f-91e6-43a8-954b-737539c51708', contemporary_multipolar_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('af36804f-91e6-43a8-954b-737539c51708', '2026-08-03T14:22:11Z').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_p5_states).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, great_power_war_prevention_via_unanimity_gate).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, collective_action_failure_averted_by_veto).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The global system of states receives the public good of great-power war avoidance. The veto prevents any SC resolution from compelling a nuclear great power into rejected confrontation, which would risk nuclear escalation. This benefit is non-excludable and non-rivalrous. The international system has no exit from this constraint — it is the constituency the constraint serves.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_system, beneficiary,
    analytical, civilizational, analytical, universal).

% The five permanent Security Council members (US, UK, France, Russia, China) hold the veto power. They administer the constraint by casting or threatening vetoes. They benefit directly from the war-prevention function (avoiding unwanted confrontation) and from the institutional authority the veto confers. Their exit option is arbitrage-grade: they could withdraw from the UN or block Charter amendment, but the veto itself is their primary leverage.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__coordination_reading, p5_states, beneficiary).

% The 188+ non-permanent UN member states receive the global public good of great-power war avoidance without holding veto power. They participate in the coordination mechanism through UN membership, which constrains exit (withdrawal is legally possible but politically and practically costly). They benefit from the stability the veto provides but have no direct control over its exercise.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_states, beneficiary,
    organized, generational, constrained, global).

% Nuclear-armed states outside the P5 (India, Pakistan, North Korea, potentially Israel, Iran) face great-power confrontation risk without a veto gate. They are excluded from the veto mechanism but affected by its systemic effects. Their exit from the UN system is constrained; they would object to the P5 monopoly on veto power if present in the coordination calculus.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, nuclear_non_p5_states, excluded,
    powerful, biographical, constrained, regional).

% The Security Council as an institution administers the veto mechanism through its procedural rules (Article 27). It sets the agenda, manages veto use, and is the forum where the coordination function is exercised. It has no independent exit — it is the institutional embodiment of the constraint.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, security_council_institution, agenda_setter,
    institutional, generational, analytical, global).

% The analytical seat assessing the constraint's structural function across all readings. Sees the full coordination-extraction landscape, the kernel contest, and the seat divergences. Neither collects nor pays; exit is analytical (always available).
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great-power war by requiring unanimity among nuclear-armed P5 states before the Security Council can authorize military action that could compel a nuclear state into confrontation it rejects — solving the collective-action problem where any single great power's rejection of a military mandate would otherwise create escalation risk.
% TRANSFER_FUNCTION: Moves diplomatic friction and blocked resolutions (the cost of maintaining the unanimity gate) from all UN member states collectively, in exchange for the global public good of avoided great-power nuclear confrontation. No net transfer from victims to beneficiaries — the cost is the coordination overhead itself.
% ABSENT_VOICES: Nuclear-armed non-P5 states (India, Pakistan, North Korea, Israel, Iran) are structurally excluded from the veto mechanism but bear confrontation risk without a corresponding gate. They would argue for either veto expansion or alternative coordination mechanisms. Future generations (who inherit the institutional design) are also absent — they would bear any long-term degradation of the veto's war-prevention function.
% DISAPPEARANCE_RATIONALE: If the P5 veto disappeared overnight, the Security Council could pass binding resolutions compelling nuclear-armed great powers into military confrontations they reject. This would create immediate escalation risk in multiple active flashpoints (Ukraine, Taiwan, Korea, Middle East). The international system would reorganize around ad hoc great-power management (summits, bilateral hotlines, spheres of influence) — the UN-centered collective security architecture would collapse.
% FOUNDING_PROBLEM: The UN Charter's founders (1945) designed the P5 veto to prevent the League of Nations' failure: without a great-power unanimity gate, collective security mechanisms would either be ignored by great powers or compel them into unwanted wars, causing system collapse. The veto was the price of great-power participation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the UN Charter drafting history (San Francisco Conference records), by Cold War crisis management literature (e.g., Cuban Missile Crisis veto use preventing escalation), and by contemporary nuclear deterrence scholars outside the P5 beneficiary set (e.g., Scott Sagan, Nina Tannenwald). The oligopoly_reading contests this status, arguing the founding problem is dead and the veto persists as rent extraction; this reading holds the problem is live because nuclear confrontation risk persists.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.03, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is extremely low (0.03) because the veto's operational cost is the diplomatic friction of blocked resolutions — not a transfer from victims to beneficiaries. Suppression is minimal (0.05) because the veto is a procedural gate, not an active coercive apparatus; states comply because the alternative (great-power war) is worse, not because they are forced. Theater ratio is low (0.08) because the veto's war-prevention function is genuine and actively used (e.g., Cold War crisis management), not performative. Accessibility collapse is very high (0.92) because the unanimity gate is structurally hard to bypass — any reform requires P5 consent, creating a self-entrenching coordination equilibrium. Resistance is near-zero (0.02) because no state seriously contests the veto's existence as a war-prevention mechanism; contestation targets specific veto uses, not the mechanism itself. The claimed type is rope: a pure coordination mechanism with minimal extraction, genuine collective-action function, and no victim class.
 *
 * PERSPECTIVAL GAP:
 *   The coordination_reading sees the veto as a rope from every seat: even non-P5 states benefit from the global public good of great-power war avoidance, and their constrained exit (UN membership) is voluntary participation in the coordination mechanism. The oligopoly_reading would compute tangled_rope or snare for non-P5 seats (extraction of authority rents by P5). The sovereignty_reading computes rope from the P5 seat (sovereignty protection) but may compute differently for non-P5 states. The engine will compute per-seat classifications from the structural data authored here; the divergence between readings is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries (international_system, p5_states, non_p5_states) receive the coordination benefit: avoided great-power war. No victim class is declared because this reading asserts no asymmetric extraction — the veto's cost (blocked resolutions) is symmetrically distributed across all members and is the price of the coordination function. P5 states are both agenda_setters (they hold and exercise the veto) and beneficiaries (they avoid being compelled into unwanted confrontation). Non-P5 states are beneficiaries with constrained exit (UN membership is the coordination commitment). The analytical observer seat sees the full structure. Directionality d is near 0.5 (symmetric) for all beneficiary seats because costs ≈ benefits; the international_system seat is analytical (d not applicable).
 *
 * MANDATROPHY ANALYSIS:
 *   The veto's founding problem (preventing great-power war via unanimity gate) remains live — nuclear confrontation risk persists. The mandate has not atrophied because the coordination function is continuously exercised (vetoes cast on Syria, Ukraine, Israel-Palestine resolutions demonstrate active use). However, the oligopoly_reading argues mandatrophy has occurred: the veto now primarily protects P5 geopolitical interests rather than preventing nuclear war. This reading rejects that characterization; the mandate is live and the mechanism remains functionally necessary. The theater_ratio at 0.08 reflects minimal performative maintenance — the veto is not a degraded institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the P5 veto a genuine coordination mechanism (this reading) or an oligopolistic extraction device (oligopoly_reading)?',
    'Compare counterfactual great-power conflict rates in institutional designs with and without a unanimity gate; measure whether veto use correlates with preventing nuclear confrontation or with protecting narrow P5 interests.',
    'If oligopoly_reading is structurally dominant, this constraint reclassifies from rope to tangled_rope or snare with P5 as beneficiaries and non-P5 states as victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Commitment to this reading vs. the oligopoly_reading of the same kernel (article_27_veto_power).').

omega_variable(
    coordination_necessity,
    'Is a P5 unanimity gate actually necessary to prevent great-power war, or would alternative mechanisms (e.g., qualified majority with nuclear-use thresholds) achieve the same coordination with less suppression?',
    'Historical analysis of near-miss crises (Cuban Missile Crisis, 1983 Soviet nuclear false alarm) and institutional design experiments; formal modeling of veto vs. alternative gates.',
    'If alternatives exist, the veto''s suppression component is structural overkill — the constraint extracts unnecessary compliance cost, shifting toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity, empirical, 'Whether the veto''s coordination function is uniquely satisfied by the current unanimity rule.').

omega_variable(
    nuclear_state_coverage,
    'Does the veto''s war-prevention logic depend on the P5 being the only nuclear states, or does it degrade as nuclear proliferation expands?',
    'Track veto use patterns and great-power confrontation dynamics as nuclear club expands (India, Pakistan, North Korea, potential Iran); assess whether the coordination function holds without veto power for new nuclear states.',
    'If the coordination logic requires veto for all nuclear-armed states, the current P5-only structure becomes a partial coordination mechanism with extractive exclusion — tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_state_coverage, empirical, 'Whether the veto''s war-prevention coordination scales with the actual nuclear landscape.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 1945, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art27_coord_tr_t1945, article_27_veto_power__coordination_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(art27_coord_tr_t1960, article_27_veto_power__coordination_reading, theater_ratio, 1960, 0.06).
narrative_ontology:measurement(art27_coord_tr_t1975, article_27_veto_power__coordination_reading, theater_ratio, 1975, 0.07).
narrative_ontology:measurement(art27_coord_tr_t1990, article_27_veto_power__coordination_reading, theater_ratio, 1990, 0.075).
narrative_ontology:measurement(art27_coord_tr_t2005, article_27_veto_power__coordination_reading, theater_ratio, 2005, 0.08).
narrative_ontology:measurement(art27_coord_tr_t2020, article_27_veto_power__coordination_reading, theater_ratio, 2020, 0.08).

% Extraction over time
narrative_ontology:measurement(art27_coord_be_t1945, article_27_veto_power__coordination_reading, base_extractiveness, 1945, 0.02).
narrative_ontology:measurement(art27_coord_be_t1960, article_27_veto_power__coordination_reading, base_extractiveness, 1960, 0.025).
narrative_ontology:measurement(art27_coord_be_t1975, article_27_veto_power__coordination_reading, base_extractiveness, 1975, 0.028).
narrative_ontology:measurement(art27_coord_be_t1990, article_27_veto_power__coordination_reading, base_extractiveness, 1990, 0.03).
narrative_ontology:measurement(art27_coord_be_t2005, article_27_veto_power__coordination_reading, base_extractiveness, 2005, 0.03).
narrative_ontology:measurement(art27_coord_be_t2020, article_27_veto_power__coordination_reading, base_extractiveness, 2020, 0.03).

% Suppression requirement over time
narrative_ontology:measurement(art27_coord_su_t1945, article_27_veto_power__coordination_reading, suppression_requirement, 1945, 0.04).
narrative_ontology:measurement(art27_coord_su_t1960, article_27_veto_power__coordination_reading, suppression_requirement, 1960, 0.045).
narrative_ontology:measurement(art27_coord_su_t1975, article_27_veto_power__coordination_reading, suppression_requirement, 1975, 0.048).
narrative_ontology:measurement(art27_coord_su_t1990, article_27_veto_power__coordination_reading, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(art27_coord_su_t2005, article_27_veto_power__coordination_reading, suppression_requirement, 2005, 0.05).
narrative_ontology:measurement(art27_coord_su_t2020, article_27_veto_power__coordination_reading, suppression_requirement, 2020, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__coordination_reading, 0.1).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, un_charter_amendment_procedure).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, great_power_conflict_management).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, nuclear_nonproliferation_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the article_27_veto_power kernel. The coordination_reading asserts rope classification with ε≈0.03 from coordination overhead; oligopoly_reading asserts tangled_rope/snare with P5 as beneficiaries extracting authority rents; sovereignty_reading asserts rope/mountain from P5 seat (sovereignty protection) but ambiguous for non-P5. All three stories share the kernel_id and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
