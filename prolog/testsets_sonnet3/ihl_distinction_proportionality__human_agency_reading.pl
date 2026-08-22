% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: Human-Agency Reading of IHL Distinction/Proportionality (Meaningful Human Control Requirement)
 *   domain: international_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the human-agency reading of the contested
 *   IHL distinction/proportionality kernel: that the law's obligations of
 *   distinction and proportionality are satisfied only when a human makes the
 *   final lethal-force decision, because moral judgment of this kind is held
 *   to be irreducible and non-delegable to machines under Martens Clause
 *   humanity principles. This is one of three live readings of the same
 *   kernel text and practice. The categorical_prohibition_reading holds
 *   autonomous killing wrong per se, independent of any performance showing —
 *   a stronger, dignity-based claim that would foreclose even a perfectly
 *   performing autonomous system. The outcomes_based_reading holds the
 *   opposite: that IHL is technology-neutral and satisfied by demonstrated
 *   performance parity, regardless of who or what makes the decision. This
 *   story authors ONLY the human-agency reading as its own constraint, with
 *   its own ε, beneficiaries, and victims — it does not average across
 *   readings or describe the contest inside its own metrics.
 *
 * KEY AGENTS:
 *   - icrc_and_ihl_interpretive_authorities: primary beneficiary and agenda-setter (institutional/arbitrage) — maintains centrality by anchoring the legal standard on human decision-presence, which only interpretive bodies like itself are positioned to adjudicate
 *   - military_operational_commanders: primary payer (powerful/constrained) — bears the operational tempo and personnel-risk cost of keeping a human in the loop
 *   - autonomous_systems_developers: secondary payer (moderate/constrained) — forecloses fielding fully autonomous targeting regardless of demonstrated accuracy
 *   - civilian_populations_in_conflict_zones: nominal beneficiary but also excluded (powerless/trapped) — the protected class with no voice in the standard-setting process and untested actual protective benefit
 *   - outcomes_based_reading_advocates: excluded (organized/constrained) — present in deliberation but structurally disadvantaged against the entrenched human-agency default
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.68).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.72).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "Human-Agency Reading of IHL Distinction/Proportionality (Meaningful Human Control Requirement)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, 'e79ca0b8-74a1-466a-88db-c65499aad6e2').
narrative_ontology:cs_kernel_codification('e79ca0b8-74a1-466a-88db-c65499aad6e2', distributed).
narrative_ontology:cs_authority_grounding('e79ca0b8-74a1-466a-88db-c65499aad6e2', lineage).
narrative_ontology:cs_interpretation_layer_present('e79ca0b8-74a1-466a-88db-c65499aad6e2').
narrative_ontology:cs_reading_relation('e79ca0b8-74a1-466a-88db-c65499aad6e2', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('e79ca0b8-74a1-466a-88db-c65499aad6e2', ihl_distinction_proportionality__outcomes_based_reading, influences).
narrative_ontology:cs_axiom('e79ca0b8-74a1-466a-88db-c65499aad6e2', foundational, human_moral_judgment_irreducible_at_point_of_force).
narrative_ontology:cs_axiom_status(human_moral_judgment_irreducible_at_point_of_force, holdable).
narrative_ontology:cs_axiom_grounding('e79ca0b8-74a1-466a-88db-c65499aad6e2', human_moral_judgment_irreducible_at_point_of_force, deontological).
narrative_ontology:cs_axiom('e79ca0b8-74a1-466a-88db-c65499aad6e2', secondary, performance_parity_insufficient_absent_human_decision_maker).
narrative_ontology:cs_axiom_status(performance_parity_insufficient_absent_human_decision_maker, holdable).
narrative_ontology:cs_axiom_grounding('e79ca0b8-74a1-466a-88db-c65499aad6e2', performance_parity_insufficient_absent_human_decision_maker, instrumental).
narrative_ontology:cs_reference_frame('e79ca0b8-74a1-466a-88db-c65499aad6e2', geneva_conventions_additional_protocol_i_targeting_framework).
narrative_ontology:cs_drift_state('e79ca0b8-74a1-466a-88db-c65499aad6e2', post_ccw_gge_autonomous_weapons_debate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e79ca0b8-74a1-466a-88db-c65499aad6e2', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, human_rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, states_with_manned_force_advantage).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_commanders).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_systems_developers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, states_seeking_autonomous_capability_parity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, irreducibility_of_moral_judgment_thesis).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, martens_clause_humanity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues authoritative commentary and convenes state-party consultations (CCW GGE) interpreting distinction/proportionality as requiring a human decision-maker at the point of force. Its institutional relevance and convening authority are directly tied to being the body that adjudicates what 'meaningful human control' requires; it faces no binding vote and cannot be bypassed by states that want a different answer without years of treaty renegotiation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities, beneficiary).

% Must keep a human in or near the targeting loop even where sensor fusion and reaction-time advantages favor full automation, accepting slower engagement cycles and personnel risk to satisfy the human-judgment requirement. Cannot exit the obligation without their state incurring reputational and legal exposure before allies and international bodies.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_operational_commanders, payer,
    powerful, immediate, constrained, national).

% Build targeting and weapons-release systems whose fully autonomous configurations are foreclosed as unlawful regardless of demonstrated accuracy, forcing continued investment in human-interface architectures and slower fielding timelines. Their exit is limited to relabeling systems as human-supervised or selling into jurisdictions with looser enforcement.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_systems_developers, payer,
    moderate, biographical, constrained, global).

% Face a normative regime that locks in the operational tempo and manpower requirements of states with large trained-personnel pools, while their own strategic advantage would come from full autonomy at machine speed. They can defect from the norm at reputational cost but cannot easily rewrite it given ICRC/GGE consensus-building dynamics.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, states_seeking_autonomous_capability_parity, payer,
    institutional, generational, constrained, global).

% Possess large, well-trained human operator corps and mature human-in-the-loop targeting doctrine; a norm mandating human judgment at the point of force preserves the relative value of this existing capability against rivals investing heavily in full autonomy.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, states_with_manned_force_advantage, beneficiary,
    institutional, generational, arbitrage, global).

% Are the intended protected class of distinction/proportionality obligations generally; whether a human decision-maker at the point of force actually reduces harm to them versus a well-validated autonomous system is contested and untested at scale. They have no seat in CCW deliberations and no ability to independently verify which regime better protects them.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict_zones, excluded).

% Argue that IHL compliance should be measured by demonstrated performance, not by the presence of a human decision-maker; they are present in CCW discussions but structurally disadvantaged because the human-agency reading currently anchors the working consensus text and burden of persuasion runs against them.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, outcomes_based_reading_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, icrc_and_ihl_interpretive_authorities).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legible, auditable rule — 'a human must make the final lethal-force decision' — that can be verified through doctrine, training records, and after-action review, avoiding the much harder problem of certifying an autonomous system's real-world distinction/proportionality performance across unbounded combat scenarios before deployment.
% TRANSFER_FUNCTION: Moves operational tempo, personnel risk, and fielding speed away from states and firms pursuing full autonomy and toward states and institutions whose comparative advantage rests on trained human operators and interpretive convening power over the legal standard itself.
% ABSENT_VOICES: Civilian populations who would bear the actual battlefield consequences of either regime have no representation in the CCW/ICRC process. Autonomous-systems engineers who believe validated statistical performance already exceeds human operators in specific engagement profiles are present but structurally on the back foot against an entrenched interpretive default.
% DISAPPEARANCE_RATIONALE: If the human-agency reading were displaced, several states would immediately pursue certification pathways for supervised or full autonomy in target engagement, doctrine and procurement budgets would shift substantially, and the ICRC/CCW process would lose its primary current lever for shaping the legal status of lethal autonomous weapons.
% FOUNDING_PROBLEM: Early debates over lethal autonomous weapons systems (LAWS) confronted a genuine gap: no established legal or technical framework existed for verifying that a machine could reliably apply the contextual, case-by-case judgment distinction and proportionality assessments were understood to require, and there was reasonable fear that speed-of-engagement pressures would erode battlefield restraint entirely.
% FOUNDING_PROBLEM_CORROBORATION: Some independent legal scholars and roboticists outside the ICRC/advocacy coalition (e.g., researchers publishing on validated autonomous system performance in structured environments) attest that the technical gap motivating the human-judgment requirement has narrowed for certain engagement classes, while military ethicists within allied defense establishments corroborate that the underlying judgment problem remains live for the majority of real combat scenarios involving ambiguous civilian presence.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε=0.68) reflects that the human-agency reading imposes a real, rising cost on operational efficiency and capability development without a settled empirical showing that human presence at the point of force actually reduces civilian harm relative to a validated autonomous alternative — the coordination benefit (legible auditability) is real but the extraction from foreclosed-autonomy states and developers is substantial and growing as autonomous validation techniques mature elsewhere. Suppression (0.72) is high because the reading's persistence depends on active diplomatic and normative pressure (CCW consensus-building, export-control alignment, doctrine mandates) rather than on unanimous technical agreement — dissenting states and firms are managed rather than persuaded. Theater ratio stays low-moderate (0.22, rising slowly) because the underlying verification function (auditable human decision records) remains largely functional rather than performative, though its share of purely symbolic compliance activity (training checklists, doctrine language) is increasing as autonomous validation techniques improve elsewhere and the human-presence requirement becomes harder to justify on pure performance grounds.
 *
 * DIRECTIONALITY LOGIC:
 *   ICRC/IHL interpretive authorities sit at the beneficiary end: their institutional relevance is structurally tied to being the adjudicating body for what 'human judgment' requires, and they face effectively arbitrage-level exit (they set terms, are not subject to them). States with manned-force advantage similarly benefit — the norm preserves the comparative value of an existing capability. Military commanders and autonomous-systems developers sit near the target end: they bear the direct operational and commercial costs of a rule that forecloses their most efficient configurations, with only constrained exit (defection at legal/reputational cost). Civilian populations are the intended beneficiary class in principle but carry a directionality closer to excluded/trapped in practice, since they have no voice in the standard's formation and no ability to verify whether it serves them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the absence of any verification framework for autonomous distinction/proportionality judgment — was genuinely live when the human-agency reading first hardened into doctrine. It remains partially live: most real combat scenarios still involve ambiguous civilian presence that untested autonomous systems have not been shown to handle reliably. But for narrower, more structured engagement profiles, validated autonomous performance is beginning to close the gap the reading was built to manage. This produces a contested founding-problem status rather than a clean live/dead call — exactly the case where classification should not default to either 'coordination that solved a real problem' or 'pure extraction dressed as principle.' The tangled_rope classification captures this: a genuine coordination function (legible, auditable compliance standard) coexists with asymmetric extraction (interpretive authorities and manned-force-advantaged states benefit; operational commanders, developers, and capability-seeking states pay), sustained by active enforcement (CCW consensus process, doctrine mandates) rather than by unanimous technical consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_judgment_necessity_vs_institutional_centrality,
    'Is the requirement of human decision-presence at the point of lethal force a genuinely necessary safeguard for distinction/proportionality compliance, or is it substantially explained by the interpretive authority it confers on the bodies (ICRC, CCW process) whose institutional relevance depends on being the arbiters of what ''human judgment'' requires?',
    'Controlled comparative studies of civilian-harm outcomes between human-in-the-loop and validated-autonomous engagement in matched structured scenarios; independent audit of whether ICRC/CCW institutional positioning shifts in proportion to the norm''s durability.',
    'If human presence shows no measurable protective advantage over validated autonomous performance in tested scenarios, the coordination justification weakens substantially and the extraction reading strengthens; if human presence shows a robust protective margin, the coordination function is vindicated and the tangled_rope classification should shift toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_judgment_necessity_vs_institutional_centrality, empirical, 'Whether the human-judgment requirement is safety-necessary or primarily institution-preserving.').

omega_variable(
    kernel_reading_sibling_delta,
    'This constraint is one reading (human_agency_reading) of the ihl_distinction_proportionality kernel. The sibling categorical_prohibition_reading would foreclose even a demonstrably superior autonomous system on dignity grounds alone (a stronger, non-performance-contingent claim); the sibling outcomes_based_reading would authorize full autonomy given performance parity (a weaker, performance-contingent claim). Where exactly does the disagreement sit — is it about what counts as evidence, or about whether evidence is relevant at all?',
    'Track CCW GGE working text evolution: language requiring ''meaningful human control'' (this reading) versus language invoking ''human dignity'' categorically (categorical reading) versus language keyed to demonstrated IHL-compliance metrics (outcomes reading) signals which premise is gaining ground.',
    'If CCW consensus text drifts toward performance-metric language, this reading is being displaced toward the outcomes_based_reading; if it drifts toward dignity/humanity language, toward the categorical_prohibition_reading. The human_agency_reading is a stable midpoint whose durability depends on neither sibling premise winning outright.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Locating where this reading''s core premise diverges from its two sibling readings within the same kernel.').

omega_variable(
    operational_efficiency_victim_measurement,
    'How large is the actual operational cost the human-agency reading imposes on military effectiveness, versus a cost that would be incurred anyway due to independent technical immaturity of autonomous targeting systems?',
    'Comparative procurement and deployment timeline analysis for supervised-autonomy versus fully-autonomous systems, controlling for underlying technical readiness independent of legal constraint.',
    'If the operational cost is mostly attributable to technical immaturity rather than the legal requirement itself, the victim declaration for military_operational_commanders and capability-seeking states should be discounted; if the legal requirement is binding well past the point of technical readiness, the victim declaration is strongly supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_efficiency_victim_measurement, empirical, 'Disentangling legal-constraint cost from independent technical-readiness cost for military operational stakeholders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ihl__tr_t4, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(ihl__tr_t8, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(ihl__tr_t16, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(ihl__tr_t24, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ihl__be_t4, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(ihl__be_t8, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(ihl__be_t16, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ihl__be_t24, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ihl__su_t4, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(ihl__su_t8, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(ihl__su_t16, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(ihl__su_t24, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__human_agency_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposed from the colloquial 'IHL and lethal autonomous weapons' debate, per the ε-invariance principle. The categorical_prohibition_reading authors a higher ε and a starker beneficiary/victim asymmetry (dignity claim admits no performance-based rebuttal, so victims include even demonstrably safer autonomous systems). The outcomes_based_reading authors a substantially lower ε (coordination function dominant, extraction minimal, victims largely absent) because it removes the interpretive-authority rent this reading preserves. All three share the same underlying kernel text and practice (ihl_distinction_proportionality) but diverge sharply in beneficiary structure, ε, and classification — exactly the case the ε-invariance principle requires decomposing rather than averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
