% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity Rule — Veto Trap Reading
 *   domain: institutional/political_economy
 *
 * SUMMARY:
 *   The EU Council unanimity rule requires all member states to agree on
 *   decisions in sensitive domains (taxation, foreign policy, social
 *   security, enlargement). The veto_trap_reading interprets this rule as a
 *   structural vulnerability: the credible threat of veto gives minority
 *   states systematic leverage to extract concessions from the majority. This
 *   is not a bug but a feature of the rule's incentive structure — any state
 *   can convert its formal equality into disproportionate policy influence by
 *   threatening to block. The extraction is real (concessions, opt-outs,
 *   budget rebates), the coordination function is real (preventing coercion),
 *   and they are fused in a single rule that requires active enforcement (the
 *   Council's procedural discipline). The claimed type 'tangled_rope'
 *   reflects this dual character; the metrics describe the extraction-heavy
 *   equilibrium that emerged post-enlargement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.82).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.78).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity Rule — Veto Trap Reading").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, '38c0b3ab-c99f-45eb-85b1-d84353d3520e').
narrative_ontology:cs_kernel_codification('38c0b3ab-c99f-45eb-85b1-d84353d3520e', formalized).
narrative_ontology:cs_authority_grounding('38c0b3ab-c99f-45eb-85b1-d84353d3520e', lineage).
narrative_ontology:cs_interpretation_layer_present('38c0b3ab-c99f-45eb-85b1-d84353d3520e').
narrative_ontology:cs_reading_relation('38c0b3ab-c99f-45eb-85b1-d84353d3520e', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('38c0b3ab-c99f-45eb-85b1-d84353d3520e', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('38c0b3ab-c99f-45eb-85b1-d84353d3520e', foundational, unanimity_enables_minoritarian_extraction).
narrative_ontology:cs_axiom_status(unanimity_enables_minoritarian_extraction, holdable).
narrative_ontology:cs_axiom_grounding('38c0b3ab-c99f-45eb-85b1-d84353d3520e', unanimity_enables_minoritarian_extraction, empirically_contingent).
narrative_ontology:cs_axiom('38c0b3ab-c99f-45eb-85b1-d84353d3520e', secondary, veto_threat_is_credible_commitment).
narrative_ontology:cs_axiom_status(veto_threat_is_credible_commitment, holdable).
narrative_ontology:cs_axiom_grounding('38c0b3ab-c99f-45eb-85b1-d84353d3520e', veto_threat_is_credible_commitment, instrumental).
narrative_ontology:cs_reference_frame('38c0b3ab-c99f-45eb-85b1-d84353d3520e', unanimity_as_neutral_sovereignty_shield).
narrative_ontology:cs_drift_state('38c0b3ab-c99f-45eb-85b1-d84353d3520e', post_2004_enlargement_equilibrium, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('38c0b3ab-c99f-45eb-85b1-d84353d3520e', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, minority_coalition).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, majority_coalition).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, commission_agenda).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:constraint_vindicates(eu_council_unanimity__veto_trap_reading, sovereign_equality_doctrine).
narrative_ontology:constraint_vindicates(eu_council_unanimity__veto_trap_reading, consensus_legitimacy_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A member state that credibly threatens veto to extract concessions, opt-outs, or side-payments from the majority coalition. Gains policy concessions, budget rebates, or institutional carve-outs. Bears cost of diplomatic isolation and reputational damage when blocking is perceived as obstructionist. Exit from the unanimity constraint means treaty change or leaving the Union — both politically costly.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_member_state, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, blocking_member_state, payer).

% A small group of states coordinating veto threats to amplify extraction. Pools blocking power to extract larger concession packages than any single state could. Gains collective policy influence disproportionate to population/economic weight. Bears coordination costs and risk of majority retaliation through enhanced cooperation mechanisms.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, minority_coalition, beneficiary,
    organized, biographical, constrained, continental).

% The large coalition of states that must concede to blocking states to achieve any collective action under unanimity. Pays through policy dilution, budget transfers, opt-outs for blockers, and delayed legislation. Cannot exit the constraint without unanimous treaty reform — which requires the blockers' consent. Organizes enhanced cooperation and qualified-majority workarounds where treaties permit.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, majority_coalition, payer,
    institutional, generational, constrained, continental).

% The European Commission proposes legislation and manages the agenda. Under unanimity, it must anticipate veto threats and pre-negotiate concessions, weakening proposals before formal submission. Gains agenda control by being the only actor that can frame compromise packages. Could push for treaty change to qualified majority but lacks independent power to enforce it.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, commission_agenda, agenda_setter,
    institutional, generational, mobile, continental).

% Co-legislator under ordinary legislative procedure but excluded from unanimity domains (taxation, foreign policy, enlargement, social security). Would object to veto extraction that bypasses parliamentary scrutiny. Its absence from unanimity domains is structural — treaty design — not procedural oversight.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, european_parliament, excluded,
    organized, biographical, constrained, continental).

% Bear the policy consequences of diluted legislation and delayed action (climate, tax justice, rule of law). No formal voice in Council veto dynamics. Exit from EU-level policy effects requires national political change or emigration — both high-cost.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, citizens_civil_society, excluded,
    powerless, biographical, trapped, continental).

% Observes the structural pattern: unanimity rule creates a credible threat point that minority states exploit systematically. Sees the coordination function (preventing majority coercion) and the extraction function (concession extraction) as empirically distinguishable but institutionally fused.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents majoritarian coercion of sovereign states in domains where exit is impossible (taxation, foreign policy, constitutional change) by requiring affirmative consent for collective action.
% TRANSFER_FUNCTION: Moves policy concessions, budget resources, legislative opt-outs, and institutional carve-outs from the majority coalition to blocking minority states through credible veto threats.
% ABSENT_VOICES: European Parliament (excluded from unanimity domains by treaty), citizens and civil society (no formal Council role), smaller states that neither block nor lead but accept diluted outcomes. They would object to systematic minority extraction but are structurally excluded from the veto bargaining.
% DISAPPEARANCE_RATIONALE: If unanimity vanished overnight in its current domains, qualified majority voting would become default. Majority coalition would pass legislation without minority concessions. Blocking states would lose extraction leverage. Commission would propose bolder initiatives. Treaty change would be required to restore unanimity — a major institutional rearrangement.
% FOUNDING_PROBLEM: Post-Maastricht (1993) and successive enlargements created a Union where sovereign states feared majority coercion in core sovereignty domains (tax, foreign policy, border control). Unanimity was the institutional guarantee that no state would be bound against its will in these domains.
% FOUNDING_PROBLEM_CORROBORATION: Founding treaties (Maastricht, Amsterdam, Nice, Lisbon) and declarations by member state governments attest the sovereignty protection rationale. Independent institutional analyses (e.g., European University Institute, CEPS) and parliamentary reports document the shift from protective shield to extraction mechanism post-2004 enlargement. The Commission's own better-regulation evaluations acknowledge veto-driven dilution.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because veto threats systematically transfer value: the British rebate, Polish opt-outs on charter of fundamental rights, Hungarian vetoes on Ukraine aid and rule-of-law conditionality all follow the pattern. Suppression (0.78) is high because the majority cannot bypass blockers without treaty change — which requires unanimity. Theater ratio (0.45) reflects that the sovereignty-protection framing is genuinely believed by some actors but increasingly performative as a cover for extraction. Accessibility collapse (0.72) is high because qualified majority alternatives exist in treaties but are politically inaccessible in unanimity domains. Resistance (0.58) is moderate: majority states resist through enhanced cooperation and political pressure but cannot structurally escape.
 *
 * PERSPECTIVAL GAP:
 *   From the blocking state's seat, unanimity is a legitimate sovereignty shield (rope/tangled_rope with low personal extraction). From the majority coalition's seat, it is an extraction mechanism (snare/tangled_rope with high extraction). From the analytical seat, the structural fusion of coordination and extraction is visible — the rule cannot be separated into 'good coordination part' and 'bad extraction part' because the veto threat IS the coordination mechanism. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Blocking states (beneficiaries) sit near d=0.15 — they collect concessions, control the agenda's feasible set. Minority coalition similarly. Majority coalition (payers) sit near d=0.85 — they pay concessions, accept dilution, cannot exit. Commission (agenda_setter) sits near d=0.45 — gains agenda control but loses policy ambition. Parliament and citizens (excluded) sit at d≈0.9 and d≈0.95 respectively — bear costs with zero voice. The derivation from beneficiary/victim declarations + power + exit produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereignty protection against majoritarian coercion) is contested: it persists for some states (neutral countries, net contributors) but has mutated into extraction leverage for others. The constraint is not a pure mandatrophy case — the coordination function remains live for some parties — but the extraction function has grown to dominate the equilibrium. This is why tangled_rope, not piton: active enforcement maintains it, beneficiaries actively defend it, and the coordination function is not fully atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the veto_trap_reading a distinct constraint from the sovereignty_guarantor_reading and diplomatic_capital_reading, or a different observational lens on the same constraint?',
    'Apply the ε-invariance test: if measuring extraction via veto-concession patterns yields high ε (0.82) while measuring via sovereignty-protection incidents yields low ε, they are distinct constraints. The veto_trap_reading instantiates the high-ε constraint; the sovereignty_guarantor_reading instantiates the low-ε constraint.',
    'If distinct, each reading gets its own constraint story, classification, and stakeholders. If same constraint, the framework must model observer-dependent classification — which it rejects by design (ε is intrinsic).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three kernel readings are structurally distinct constraints or observer perspectives on one constraint.').

omega_variable(
    extraction_coordination_separability,
    'Can the coordination function (sovereignty protection) be institutionally separated from the extraction function (veto-for-concessions) without losing the former?',
    'Counterfactual analysis: qualified majority with reinforced subsidiarity review vs. current unanimity. If sovereignty violations remain rare under QMV+subsidiarity, the functions are separable and the extraction is removable overhead.',
    'If separable, the constraint is a snare with a coordination cover story. If inseparable, it is a genuine tangled_rope where extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_coordination_separability, empirical, 'Whether the coordination and extraction components of unanimity are structurally separable.').

omega_variable(
    suppression_mechanism,
    'Is the suppression of majority preferences structural (treaty requirement of unanimity) or internalized (majority states self-censor proposals anticipating veto)?',
    'Compare formal veto counts vs. pre-negotiated withdrawal of proposals. If most suppression occurs at pre-proposal stage, internalized suppression dominates.',
    'If internalized, effective suppression exceeds the formal veto count — the constraint''s reach extends beyond its formal invocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism in the veto trap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1993, eu_council_unanimity__veto_trap_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(eu_c_tr_t1999, eu_council_unanimity__veto_trap_reading, theater_ratio, 1999, 0.3).
narrative_ontology:measurement(eu_c_tr_t2004, eu_council_unanimity__veto_trap_reading, theater_ratio, 2004, 0.38).
narrative_ontology:measurement(eu_c_tr_t2009, eu_council_unanimity__veto_trap_reading, theater_ratio, 2009, 0.42).
narrative_ontology:measurement(eu_c_tr_t2014, eu_council_unanimity__veto_trap_reading, theater_ratio, 2014, 0.44).
narrative_ontology:measurement(eu_c_tr_t2020, eu_council_unanimity__veto_trap_reading, theater_ratio, 2020, 0.45).
narrative_ontology:measurement(eu_c_tr_t2024, eu_council_unanimity__veto_trap_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1993, 0.45).
narrative_ontology:measurement(eu_c_be_t1999, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1999, 0.52).
narrative_ontology:measurement(eu_c_be_t2004, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2004, 0.65).
narrative_ontology:measurement(eu_c_be_t2009, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2009, 0.71).
narrative_ontology:measurement(eu_c_be_t2014, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2014, 0.76).
narrative_ontology:measurement(eu_c_be_t2020, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(eu_c_be_t2024, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1993, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(eu_c_su_t1999, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1999, 0.6).
narrative_ontology:measurement(eu_c_su_t2004, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2004, 0.68).
narrative_ontology:measurement(eu_c_su_t2009, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2009, 0.72).
narrative_ontology:measurement(eu_c_su_t2014, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2014, 0.75).
narrative_ontology:measurement(eu_c_su_t2020, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(eu_c_su_t2024, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__veto_trap_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_enhanced_cooperation).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_qualified_majority_expansion).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_rule_of_law_conditionality).

% DUAL FORMULATION NOTE:
% Part of eu_council_unanimity constraint family. This reading (veto_trap) has high ε (0.82) because it measures extraction via concession patterns. sovereignty_guarantor_reading has low ε (~0.15) measuring sovereignty violations prevented. diplomatic_capital_reading has moderate ε (~0.35) measuring negotiation investment. They are linked because veto_trap extraction degrades the legitimacy that diplomatic_capital claims, and sovereignty_guarantor framing is the political cover that sustains veto_trap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
