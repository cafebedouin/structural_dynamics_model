% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Legitimacy via Security Necessity and Defensive Territorial Control (1967 Borders Plus Strategic Depth)
 *   domain: political/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story captures the 'security necessity reading' of
 *   territorial legitimacy in Israel/Palestine. It asserts that Israeli
 *   control beyond the 1967 lines — including settlements, military zones,
 *   and the blockade of Gaza — is legitimate because it provides
 *   indispensable strategic depth. The reading conditions Palestinian
 *   sovereignty on demilitarization and treats the 1967 lines as militarily
 *   indefensible. The claim/metric gap is deliberate: the constraint is
 *   CLAIMED as tangled_rope (coordination + extraction) while the authored
 *   metrics describe a heavily extractive, actively enforced regime with
 *   rising theater — the engine measures that divergence; do not reconcile
 *   the claim to the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.75).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.8).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Legitimacy via Security Necessity and Defensive Territorial Control (1967 Borders Plus Strategic Depth)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, 'e1eefcf3-04ce-44dd-956f-9e7c880e7801').
narrative_ontology:cs_kernel_codification('e1eefcf3-04ce-44dd-956f-9e7c880e7801', distributed).
narrative_ontology:cs_authority_grounding('e1eefcf3-04ce-44dd-956f-9e7c880e7801', extraction).
narrative_ontology:cs_interpretation_layer_present('e1eefcf3-04ce-44dd-956f-9e7c880e7801').
narrative_ontology:cs_reading_relation('e1eefcf3-04ce-44dd-956f-9e7c880e7801', territorial_legitimacy__partition_reading, influences).
narrative_ontology:cs_reading_relation('e1eefcf3-04ce-44dd-956f-9e7c880e7801', territorial_legitimacy__indigenous_continuity_reading, influences).
narrative_ontology:cs_axiom('e1eefcf3-04ce-44dd-956f-9e7c880e7801', foundational, security_necessity_justifies_territorial_control).
narrative_ontology:cs_axiom_status(security_necessity_justifies_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('e1eefcf3-04ce-44dd-956f-9e7c880e7801', security_necessity_justifies_territorial_control, instrumental).
narrative_ontology:cs_axiom('e1eefcf3-04ce-44dd-956f-9e7c880e7801', foundational, palestinian_sovereignty_conditional_on_demilitarization).
narrative_ontology:cs_axiom_status(palestinian_sovereignty_conditional_on_demilitarization, holdable).
narrative_ontology:cs_axiom_grounding('e1eefcf3-04ce-44dd-956f-9e7c880e7801', palestinian_sovereignty_conditional_on_demilitarization, instrumental).
narrative_ontology:cs_reference_frame('e1eefcf3-04ce-44dd-956f-9e7c880e7801', strategic_depth_legitimacy).
narrative_ontology:cs_drift_state('e1eefcf3-04ce-44dd-956f-9e7c880e7801', post_oslo_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e1eefcf3-04ce-44dd-956f-9e7c880e7801', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_government).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_settlers).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_security_establishment).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinians_west_bank).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinians_gaza).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_authority).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, state_right_to_defensive_depth).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, territorial_control_as_security_prerequisite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the territorial control regime through military orders, settlement policy, and diplomatic framing. Collects strategic depth, land, water resources, and security coordination. Justifies the regime as existential necessity. Can pivot to alternative frameworks (e.g., disengagement) but at high political cost.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive subsidized housing, infrastructure, and ideological fulfillment in West Bank settlements. Their presence is framed as security contribution. Exit would mean abandoning homes and communities; political mobilization makes exit costly but possible.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_settlers, beneficiary,
    organized, biographical, constrained, local).

% Gains operational control over territory, intelligence access, and buffer zones. The constraint legitimizes their institutional mandate and resource allocation. They can advocate policy shifts but are structurally invested in the control architecture.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_security_establishment, beneficiary,
    institutional, generational, arbitrage, national).

% Bear land expropriation, movement restrictions, resource diversion, and limited autonomy under military rule. The constraint conditions their sovereignty on demilitarization and accepts settlements as permanent. Exit is physically blocked (permits, checkpoints) and politically unrealized (no sovereign state).
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinians_west_bank, payer,
    powerless, biographical, trapped, local).

% Subject to blockade, periodic military operations, and separation from West Bank polity. The security necessity reading treats Gaza as a distinct security threat requiring containment. Exit is nearly impossible (border closures, permit regime).
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinians_gaza, payer,
    powerless, biographical, trapped, local).

% Denied right of return under this reading; their claims are treated as incompatible with security necessity. They have no voice in the arrangements that determine their fate. Exit from refugee status requires a political solution this reading rejects.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_refugees, excluded,
    powerless, generational, trapped, regional).

% Exercises limited municipal autonomy in Areas A/B under Oslo, but remains subordinate to Israeli security control and settlement expansion. Collects donor aid and some governance authority. Could theoretically dissolve itself, but that would collapse the only Palestinian institutional structure.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_authority, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, palestinian_authority, agenda_setter).

% Issues resolutions (UNSC 242, 2334), funds humanitarian aid, and mediates peace processes. Lacks enforcement will to alter facts on the ground. Their analytical seat sees the full structure but their institutional power to change it is minimal.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified security architecture for Israel by consolidating territorial control over strategic high ground, water aquifers, and border approaches, replacing the vulnerable 1967 lines with a defensible envelope.
% TRANSFER_FUNCTION: Moves land, water, mineral resources, and demographic control from Palestinian collective ownership to Israeli state/settler control; moves security risk from Israeli population centers to Palestinian areas; moves political agency from Palestinian self-determination to Israeli discretionary permission.
% ABSENT_VOICES: Palestinian refugees and diaspora (denied representation in any negotiation); Israeli peace camp and human rights organizations (marginalized in domestic politics); Global South states advocating decolonial frameworks (excluded from great-power mediation formats).
% DISAPPEARANCE_RATIONALE: If the security necessity justification vanished overnight, the legal basis for settlements, military occupation, and blockade would collapse. The territorial arrangement would revert to 1967 lines as the default legal baseline, triggering a reorganization of sovereignty, resource allocation, and security guarantees — likely through intense negotiation or conflict.
% FOUNDING_PROBLEM: The perceived existential vulnerability of Israel within the 1967 armistice lines after the 1967 war, requiring strategic depth to absorb attack and enable defense.
% FOUNDING_PROBLEM_CORROBORATION: Israeli security establishment and right-wing parties attest the problem is live, citing Iran-backed militias and Hamas. Palestinian leadership, ICJ advisory opinion (2024), and majority of UN member states attest the founding problem is resolved by peace treaties (Egypt, Jordan) and security cooperation, rendering continued occupation extractive. Independent security analysts (e.g., INSS, former IDF chiefs) are split: some argue technology and treaties obsolete territorial depth; others insist geography remains decisive.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the constraint transfers land, water, and autonomy from Palestinians to Israeli control at a rate far exceeding any security coordination benefit. Suppression is very high (0.8) because the regime's persistence depends on active military enforcement, permit systems, and physical barriers that prevent exit and suppress alternatives. Theater ratio (0.4) reflects that genuine security coordination exists (intelligence sharing, counter-terrorism) but a growing share of enforcement serves settlement expansion and demographic engineering rather than immediate threat response. The measurement series run on one shared time grid (0,9,18,27,36,45,57) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli security establishment's seat, the constraint is a genuine coordination mechanism that solves a collective-action problem (defense) with side payments to settlers. From the Palestinian seats, the same structure operates as a snare: the coordination story is cover for extraction, and exit is structurally blocked. The engine will compute this divergence from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli government and security establishment are structural beneficiaries (d near 0.0): they collect strategic depth, resources, and institutional mandate. Settlers are beneficiaries with constrained exit (d ~0.2). Palestinian Authority is a payer with limited agenda-setting power (d ~0.6). Palestinians in West Bank, Gaza, and refugees are full targets (d near 1.0): trapped, identity-locked to land, bearing the full extractive weight. International community is analytical observer (d=0.5). The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1967 vulnerability) has partially resolved via peace treaties, technology, and deterrence, but the arrangement has expanded rather than contracted. The mandate has outlived its original function — settlement growth continues where security rationale is thinnest (deep West Bank). This is mandatrophy: the coordination function atrophied while the extraction function intensified. The constraint persists because the agenda-setters (Israeli government) face prohibitive fixing cost (political suicide, coalition collapse) while payers (Palestinians) lack power to force change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_necessity_natural_vs_constructed,
    'Is the claimed security necessity a genuine geophysical/strategic invariant (mountain-like) or a constructed justification that expands with settlement growth?',
    'Counterfactual analysis: if settlements were removed but military presence retained, would security degrade measurably? Compare 2005 Gaza disengagement (security did not collapse) with 2000 Lebanon withdrawal (security improved).',
    'If natural invariant, the constraint trends toward mountain/rope; if constructed, the high extractiveness and suppression reveal a snare/tangled_rope using security as cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_necessity_natural_vs_constructed, conceptual, 'Whether the security rationale is an irreducible strategic fact or a malleable political claim.').

omega_variable(
    suppression_mechanism_west_bank,
    'Is the suppression of Palestinian agency primarily structural (checkpoints, permits, Area C control) or internalized (Palestinian Authority security cooperation, economic dependency, normalized limited autonomy)?',
    'Track resistance levels after periods of reduced structural pressure (e.g., Oslo years vs. post-Second Intifada). If resistance persists despite lowered structural barriers, internalized suppression is significant.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint''s persistence relies on Palestinian complicity, not just Israeli force. This affects theta_coupling and piton detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_west_bank, empirical, 'Structural vs. internalized suppression in the West Bank.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__security_necessity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(terr_tr_t9, territorial_legitimacy__security_necessity_reading, theater_ratio, 9, 0.25).
narrative_ontology:measurement(terr_tr_t18, territorial_legitimacy__security_necessity_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(terr_tr_t27, territorial_legitimacy__security_necessity_reading, theater_ratio, 27, 0.35).
narrative_ontology:measurement(terr_tr_t36, territorial_legitimacy__security_necessity_reading, theater_ratio, 36, 0.38).
narrative_ontology:measurement(terr_tr_t45, territorial_legitimacy__security_necessity_reading, theater_ratio, 45, 0.39).
narrative_ontology:measurement(terr_tr_t57, territorial_legitimacy__security_necessity_reading, theater_ratio, 57, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__security_necessity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(terr_be_t9, territorial_legitimacy__security_necessity_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(terr_be_t18, territorial_legitimacy__security_necessity_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(terr_be_t27, territorial_legitimacy__security_necessity_reading, base_extractiveness, 27, 0.66).
narrative_ontology:measurement(terr_be_t36, territorial_legitimacy__security_necessity_reading, base_extractiveness, 36, 0.7).
narrative_ontology:measurement(terr_be_t45, territorial_legitimacy__security_necessity_reading, base_extractiveness, 45, 0.73).
narrative_ontology:measurement(terr_be_t57, territorial_legitimacy__security_necessity_reading, base_extractiveness, 57, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__security_necessity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(terr_su_t9, territorial_legitimacy__security_necessity_reading, suppression_requirement, 9, 0.7).
narrative_ontology:measurement(terr_su_t18, territorial_legitimacy__security_necessity_reading, suppression_requirement, 18, 0.75).
narrative_ontology:measurement(terr_su_t27, territorial_legitimacy__security_necessity_reading, suppression_requirement, 27, 0.78).
narrative_ontology:measurement(terr_su_t36, territorial_legitimacy__security_necessity_reading, suppression_requirement, 36, 0.8).
narrative_ontology:measurement(terr_su_t45, territorial_legitimacy__security_necessity_reading, suppression_requirement, 45, 0.8).
narrative_ontology:measurement(terr_su_t57, territorial_legitimacy__security_necessity_reading, suppression_requirement, 57, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__security_necessity_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings form a constraint family decomposing the 'territorial legitimacy' label. The security_necessity_reading has substantially higher ε (0.75) than the partition_reading would claim (near 0 for 1948 lines) or indigenous_continuity_reading (near 0 for pre-1948 continuity). They are linked because each is cited to undermine the others: security necessity is invoked to reject partition borders; partition legality is invoked to reject settlements; indigenous continuity is invoked to reject both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, institutional, 0.1).
constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, organized, 0.2).
constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
