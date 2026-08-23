% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty / Responsibility to Protect
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the conditional_sovereignty reading of
 *   the westphalian_sovereignty kernel. The reading holds that sovereignty is
 *   not absolute but entails a responsibility to protect populations; when a
 *   state commits systematic human rights violations, it forfeits its
 *   immunity from external intervention. This reading emerged from the 1990s
 *   humanitarian intervention debates, was codified in the 2001 ICISS report
 *   and 2005 UN World Summit Outcome Document (R2P), and has been invoked in
 *   Libya (2011), Côte d'Ivoire (2011), and referenced in Syria, Myanmar, and
 *   Ukraine contexts. The constraint operates as a snare from the perspective
 *   of target states: the coordination story (protecting populations) is real
 *   but the enforcement machinery extracts sovereignty selectively,
 *   suppressing alternatives (regional solutions, diplomatic pressure) and
 *   concentrating costs on geopolitically disfavored states while powerful
 *   states remain exempt. The claimed_type is snare because the extraction is
 *   asymmetric and enforcement-dependent, though the coordination function is
 *   genuinely believed by advocates.
 *
 * KEY AGENTS:
 *   - international_intervention_advocates: Primary beneficiary (institutional/arbitrage) — human rights NGOs, R2P norm entrepreneurs, Western states championing the doctrine; they gain legitimating framework for intervention
 *   - target_sovereign_states: Primary victim (powerful/constrained) — states accused of systematic violations; they bear sovereignty costs, face intervention, sanctions, ICC referral; exit is constrained by great power politics
 *   - non_interventionist_states: Secondary victim (institutional/constrained) — China, Russia, Global South states opposing R2P; they bear normative pressure and precedent risk; exit is constrained by UNSC veto dynamics
 *   - at_risk_populations: Beneficiary (powerless/trapped) — civilian populations in crisis zones; they receive protection when intervention occurs but have no agency in triggering it; exit is trapped
 *   - un_security_council: Agenda setter (institutional/arbitrage) — authorizes or blocks interventions; P5 members hold veto power; they administer the constraint's enforcement gate
 *   - regional_organizations: Excluded (organized/constrained) — AU, ASEAN, OAS; they propose alternative mechanisms but are structurally marginalized by UNSC primacy; exit is constrained
 *   - international_legal_scholars: Observer (analytical/analytical) — interpret and contest the doctrine's legal status; analytical exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.38).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.62).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty / Responsibility to Protect").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, 'cb7a416c-7259-473d-98c7-6f39e3fb065b').
narrative_ontology:cs_kernel_codification('cb7a416c-7259-473d-98c7-6f39e3fb065b', fixed_text).
narrative_ontology:cs_authority_grounding('cb7a416c-7259-473d-98c7-6f39e3fb065b', lineage).
narrative_ontology:cs_interpretation_layer_present('cb7a416c-7259-473d-98c7-6f39e3fb065b').
narrative_ontology:cs_reading_relation('cb7a416c-7259-473d-98c7-6f39e3fb065b', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('cb7a416c-7259-473d-98c7-6f39e3fb065b', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('cb7a416c-7259-473d-98c7-6f39e3fb065b', foundational, sovereignty_entails_responsibility_to_protect).
narrative_ontology:cs_axiom_status(sovereignty_entails_responsibility_to_protect, holdable).
narrative_ontology:cs_axiom_grounding('cb7a416c-7259-473d-98c7-6f39e3fb065b', sovereignty_entails_responsibility_to_protect, deontological).
narrative_ontology:cs_axiom('cb7a416c-7259-473d-98c7-6f39e3fb065b', foundational, systematic_violations_trigger_legitimate_intervention).
narrative_ontology:cs_axiom_status(systematic_violations_trigger_legitimate_intervention, holdable).
narrative_ontology:cs_axiom_grounding('cb7a416c-7259-473d-98c7-6f39e3fb065b', systematic_violations_trigger_legitimate_intervention, conventional).
narrative_ontology:cs_reference_frame('cb7a416c-7259-473d-98c7-6f39e3fb065b', conditional_westphalian_order).
narrative_ontology:cs_drift_state('cb7a416c-7259-473d-98c7-6f39e3fb065b', contemporary_r2p_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cb7a416c-7259-473d-98c7-6f39e3fb065b', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, international_intervention_advocates).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, at_risk_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, target_sovereign_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, non_interventionist_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, un_security_council_p5_western).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, un_security_council_p5_non_western).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, human_rights_primacy_over_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Human rights NGOs, R2P norm entrepreneurs, and Western states championing the doctrine. They gain a legitimating framework for intervention, institutional relevance, and funding streams. They can shift advocacy to other norms if R2P loses credibility (arbitrage exit). They administer the narrative but not the enforcement gate.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_intervention_advocates, beneficiary,
    institutional, generational, arbitrage, global).

% Civilian populations in crisis zones facing systematic violations. They receive protection when intervention occurs but have no agency in triggering it and bear collateral costs (displacement, infrastructure damage). They cannot exit the condition of vulnerability; their fate depends on others' decisions.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, at_risk_populations, beneficiary,
    powerless, immediate, trapped, local).

% States accused of systematic human rights violations (e.g., Libya 2011, Sudan, Myanmar, Syria). They bear sovereignty costs: military intervention, sanctions, ICC referrals, diplomatic isolation. Exit is constrained — they cannot leave the international system, and compliance demands may threaten regime survival. Great power alignment modulates but does not eliminate exposure.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, target_sovereign_states, payer,
    powerful, biographical, constrained, national).

% China, Russia, and Global South states opposing R2P as sovereignty erosion. They bear normative pressure: precedents set against them, diplomatic capital spent blocking interventions, institutional legitimacy costs. Their UNSC veto gives partial exit (blocking enforcement) but they cannot escape the framework's normative gravity. Exit is constrained by great power competition.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, non_interventionist_states, payer,
    institutional, generational, constrained, global).

% US, UK, France — they authorize interventions, shape mandate language, and benefit from selective application (intervening in adversary states, exempting allies). They control the enforcement gate and can opt out via veto or non-participation. Their exit is arbitrage-grade: they can pivot to other frameworks (counterterrorism, WPS) if R2P becomes inconvenient.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, un_security_council_p5_western, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, un_security_council_p5_western, beneficiary).

% China, Russia — they authorize or (mostly) block interventions, bear precedent risk for their own domestic policies (Xinjiang, Chechnya, Ukraine), and spend diplomatic capital maintaining the non-intervention norm. Their veto gives enforcement control but not normative exit — the framework's legitimacy erodes their sovereignty claims. Exit is constrained: they cannot leave the UNSC or the R2P discourse.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, un_security_council_p5_non_western, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, un_security_council_p5_non_western, payer).

% AU, ASEAN, OAS, EU — they propose alternative mechanisms (African Standby Force, ASEAN Way, OAS democracy clause) but are structurally marginalized by UNSC primacy in Chapter VII authorization. They would object to exclusive UNSC gatekeeping but lack authorization power. Exit is constrained: they operate within the UN framework but cannot replace it.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, regional_organizations, excluded,
    organized, biographical, constrained, regional).

% Scholars interpreting and contesting R2P's legal status: whether it is customary law, emerging norm, or political concept. They have analytical exit — they can change frameworks — but their interpretations shape the constraint's legitimacy over civilizational time.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine coordination failure of the 1990s: the international community's inability to organize timely, legitimate response to mass atrocities (Rwanda, Srebrenica). R2P provides a shared framework (three pillars) for prevention, reaction, and rebuilding that did not previously exist.
% TRANSFER_FUNCTION: Moves sovereignty rights (non-interference, territorial integrity, domestic jurisdiction) from target states to the international community (as represented by UNSC) when systematic violation thresholds are met. The transfer is conditional, not continuous — sovereignty is held in trust, forfeited upon breach.
% ABSENT_VOICES: Victims of interventions gone wrong (Libya post-2011 chaos, Kosovo displacement) who would object to the doctrine's application but are not represented in norm-setting forums. Populations in non-intervened crises (Syria, Yemen, Ethiopia) who would demand the doctrine's application but lack advocacy access. Indigenous sovereignty movements that reject the Westphalian kernel entirely — they are excluded from the sovereignty conversation altogether.
% DISAPPEARANCE_RATIONALE: If the conditional sovereignty constraint vanished overnight, the UNSC would lose its primary normative framework for authorizing humanitarian intervention. Interventions would revert to ad hoc coalitions of the willing (Kosovo 1999 model) or unilateral action. The R2P pillar structure (prevention, reaction, rebuilding) would dissolve. At-risk populations would lose the only internationally agreed trigger for protective action. Target states would regain absolute sovereignty de jure but not de facto (great power intervention would continue). The normative architecture of post-1990s humanitarian action would collapse.
% FOUNDING_PROBLEM: The international community's paralysis before mass atrocities in the 1990s — specifically Rwanda (1994) and Srebrenica (1995) — where the UN had peacekeepers on the ground but no mandate, political will, or legal framework to prevent genocide. The Westphalian non-interference norm blocked action; the humanitarian imperative demanded it. The constraint was built to resolve this contradiction.
% FOUNDING_PROBLEM_CORROBORATION: The ICISS commission (2001) and UN World Summit (2005) attest the problem is live — mass atrocities recur (Darfur, Syria, Myanmar, Xinjiang, Ukraine). Target states and non-interventionist states attest the problem is substantially solved or mischaracterized — they argue the constraint now serves geopolitical selection, not atrocity prevention (Russian/Chinese UNSC statements, NAM declarations, African Union Ezulwini Consensus). Independent commissions (e.g., 2014 R2P review) corroborate both: atrocities persist AND selectivity undermines legitimacy.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate: the constraint extracts sovereignty rights from target states when thresholds are met, but does not extract continuously — only when triggered. Suppression (0.62) is high because the constraint's persistence depends on active enforcement (UNSC resolutions, ICC, sanctions regimes) and on suppressing alternative frameworks (non-intervention, regional solutions). Theater_ratio (0.41) is moderate: the R2P pillar structure (prevent, react, rebuild) performs coordination, but Pillar 3 (react) dominates operational reality while Pillars 1-2 are under-resourced. Accessibility_collapse (0.58) reflects that once the R2P framework is accepted, alternatives (strict non-intervention) become diplomatically costly. Resistance (0.71) is high: target states, non-interventionist states, and regional bodies actively contest the doctrine's legitimacy and application. The measurement series (2001-2024) shows extraction rising from post-9/11 humanitarian intervention discourse through R2P codification (2005) to Libya (2011) peak and post-Libya plateau with Syria non-intervention creating credibility gap.
 *
 * PERSPECTIVAL GAP:
 *   From the intervention_advocate seat (beneficiary/arbitrage), the constraint is a rope: it solves a genuine coordination failure (international community's inability to stop mass atrocities) with minimal coercive overhead when UNSC agrees. From the target_state seat (victim/constrained), it is a snare: the coordination story covers selective extraction of sovereignty by powerful states; alternatives are suppressed; enforcement is the constraint's real function. From the non_interventionist_state seat (victim/constrained), it is a piton: a degraded norm that persists theatrically (R2P invoked ritually) but extracts little from powerful violators. The engine computes this seat divergence from the structural data — the authored claim (snare) reflects the target_state seat's experience as structurally dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervention advocates are structural beneficiaries: they gain a legitimating framework, funding, institutional relevance (d ≈ 0.15). At-risk populations are incidental beneficiaries: they receive protection when intervention occurs but cannot trigger it and bear collateral costs (d ≈ 0.35). Target states are full targets: they bear sovereignty loss, intervention, sanctions, legal jeopardy; exit is constrained by great power politics (d ≈ 0.85). Non-interventionist states are secondary targets: they bear normative erosion and precedent risk; their veto power gives partial exit but they cannot escape the framework (d ≈ 0.65). UNSC P5 are agenda_setters with arbitrage exit: they control enforcement, benefit from selective application, can opt out via veto (d ≈ 0.10 for Western P5, 0.75 for China/Russia — hence directionality_overrides needed). Regional organizations are excluded: they would offer alternatives but are structurally locked out of authorization (d ≈ 0.60).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1990s: Rwanda, Srebrenica — international paralysis before mass atrocities) is LIVE but CONTESTED. The arrangement persists because the coordination failure it was built to solve recurs (Syria, Myanmar, Ethiopia, Sudan), but the extraction pattern has shifted: the constraint now primarily disciplines weaker states while stronger violators operate with impunity. This is not mandatrophy (the problem hasn't disappeared) but mandate_capture: the enforcement machinery serves geopolitical selection rather than atrocity prevention. The classification prevents mislabeling by showing the coordination function (pillars 1-2) is real but operationally subordinate to the extraction function (pillar 3 selectivity) — a tangled_rope dynamic that the snare claim captures from the target seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_conditional_vs_absolute,
    'Does the conditional_sovereignty reading logically foreclose the absolute_sovereignty reading within a single commitment framework, or do they coexist as competing frameworks held by different parties?',
    'Analyze whether any state or international body can simultaneously maintain that sovereignty is both absolute and conditional on human rights compliance without contradiction.',
    'If forecloses, the two readings cannot be held by the same actor; if coexists_with, they represent a live structural contest in international law with no internal resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_conditional_vs_absolute, conceptual, 'Logical relationship between conditional and absolute sovereignty readings of the Westphalian kernel').

omega_variable(
    kernel_reading_conditional_vs_graduated,
    'Does the conditional_sovereignty reading (binary threshold: systematic violations trigger intervention) structurally influence the graduated_sovereignty reading (spectrum of sovereignty based on capacity/legitimacy), or do they operate independently?',
    'Track whether R2P-style threshold language in UN resolutions and state practice creates downstream pressure toward graduated assessments of state capacity.',
    'If influences, the conditional reading''s institutionalization reshapes the operating environment for graduated sovereignty claims without resolving the dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_conditional_vs_graduated, empirical, 'Structural pressure from threshold-based intervention norm on spectrum-based sovereignty claims').

omega_variable(
    intervention_selectivity_bias,
    'Is the measured extraction (ε=0.38) driven by genuine coordination failure (inability to protect populations) or by selective enforcement that extracts sovereignty from disfavored states while exempting powerful ones?',
    'Compare intervention authorization rates against objective violation severity metrics across geopolitical alignments; regression controlling for strategic interest.',
    'If selective enforcement dominates, the constraint operates as a snare with geopolitical extraction; if genuine coordination failure dominates, it is a tangled_rope with real but imperfect coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_selectivity_bias, empirical, 'Whether the constraint''s extractiveness reflects coordination failure or geopolitical selectivity').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by target states structural (military intervention, sanctions, ICC referral) or internalized (self-censorship, preemptive compliance, sovereignty performance)?',
    'Post-intervention trajectory analysis: if suppression persists after external pressure lifts, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests; the constraint reshapes state identity beyond coercive moments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for conditional sovereignty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 0, 23).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wscs_tr_t0, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(wscs_tr_t0, observed).
narrative_ontology:measurement(wscs_tr_t6, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(wscs_tr_t6, observed).
narrative_ontology:measurement(wscs_tr_t12, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(wscs_tr_t12, observed).
narrative_ontology:measurement(wscs_tr_t18, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 18, 0.41).
narrative_ontology:measurement_basis(wscs_tr_t18, observed).
narrative_ontology:measurement(wscs_tr_t23, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 23, 0.41).
narrative_ontology:measurement_basis(wscs_tr_t23, observed).

% Extraction over time
narrative_ontology:measurement(wscs_be_t0, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(wscs_be_t0, observed).
narrative_ontology:measurement(wscs_be_t6, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 6, 0.28).
narrative_ontology:measurement_basis(wscs_be_t6, observed).
narrative_ontology:measurement(wscs_be_t12, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 12, 0.35).
narrative_ontology:measurement_basis(wscs_be_t12, observed).
narrative_ontology:measurement(wscs_be_t18, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 18, 0.38).
narrative_ontology:measurement_basis(wscs_be_t18, observed).
narrative_ontology:measurement(wscs_be_t23, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 23, 0.38).
narrative_ontology:measurement_basis(wscs_be_t23, observed).

% Suppression requirement over time
narrative_ontology:measurement(wscs_su_t0, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(wscs_su_t0, observed).
narrative_ontology:measurement(wscs_su_t6, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 6, 0.48).
narrative_ontology:measurement_basis(wscs_su_t6, observed).
narrative_ontology:measurement(wscs_su_t12, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 12, 0.56).
narrative_ontology:measurement_basis(wscs_su_t12, observed).
narrative_ontology:measurement(wscs_su_t18, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 18, 0.62).
narrative_ontology:measurement_basis(wscs_su_t18, observed).
narrative_ontology:measurement(wscs_su_t23, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 23, 0.62).
narrative_ontology:measurement_basis(wscs_su_t23, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__conditional_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, icc_complementarity).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, unsc_veto_power).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, regional_intervention_mechanisms).

% DUAL FORMULATION NOTE:
% Part of the westphalian_sovereignty constraint family. This reading (conditional_sovereignty) differs from absolute_sovereignty in ε (0.38 vs ~0.05) and victim structure (target states vs. none). It differs from graduated_sovereignty in threshold structure (binary trigger vs. continuous spectrum). All three share the kernel but instantiate distinct constraints with distinct metrics and stakeholder surfaces.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__conditional_sovereignty, institutional, 0.1).
constraint_indexing:directionality_override(westphalian_sovereignty__conditional_sovereignty, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
