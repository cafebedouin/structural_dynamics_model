% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity as Veto Trap (Minoritarian Extraction Reading)
 *   domain: political/institutional
 *
 * SUMMARY:
 *   This constraint story instantiates the veto_trap_reading of the contested
 *   eu_council_unanimity kernel. Under EU Treaty rules, Council decisions in
 *   sensitive policy areas require unanimity among member states. The veto
 *   trap reading treats this not as legitimate sovereignty protection or
 *   consensus-building diplomacy, but as a structural vulnerability that
 *   empowers blocking minorities to extract concessions, opt-outs, and
 *   side-payments from coalition majorities through credible threats. The
 *   reading holds that the coordination narratives surrounding unanimity
 *   obscure a systematic transfer of value from majority preference to
 *   minority position.
 *
 * KEY AGENTS:
 *   - veto_wielding_minority: Beneficiary (powerful, constrained exit) â extracts concessions by threatening to block legislation
 *   - coalition_majority: Primary target (powerful, constrained exit) â bears extraction through policy dilution and side-payments
 *   - european_commission: Agenda-setter (institutional, constrained) â proposes legislation that gets held hostage by veto threats
 *   - eu_citizens: Excluded (powerless, trapped) â bear diffuse costs of suboptimal policy but are outside the bargaining room
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.82).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.75).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, snare).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity as Veto Trap (Minoritarian Extraction Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "political/institutional").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, 'c57b0230-78e9-4fe4-9233-b621f2591464').
narrative_ontology:cs_kernel_codification('c57b0230-78e9-4fe4-9233-b621f2591464', formalized).
narrative_ontology:cs_authority_grounding('c57b0230-78e9-4fe4-9233-b621f2591464', lineage).
narrative_ontology:cs_interpretation_layer_present('c57b0230-78e9-4fe4-9233-b621f2591464').
narrative_ontology:cs_reading_relation('c57b0230-78e9-4fe4-9233-b621f2591464', eu_council_unanimity__sovereignty_guarantor_reading, forecloses).
narrative_ontology:cs_reading_relation('c57b0230-78e9-4fe4-9233-b621f2591464', eu_council_unanimity__diplomatic_capital_reading, influences).
narrative_ontology:cs_axiom('c57b0230-78e9-4fe4-9233-b621f2591464', foundational, veto_as_extraction_mechanism).
narrative_ontology:cs_axiom_status(veto_as_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('c57b0230-78e9-4fe4-9233-b621f2591464', veto_as_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('c57b0230-78e9-4fe4-9233-b621f2591464', foundational, consensus_narrative_obscures_rent).
narrative_ontology:cs_axiom_status(consensus_narrative_obscures_rent, holdable).
narrative_ontology:cs_axiom_grounding('c57b0230-78e9-4fe4-9233-b621f2591464', consensus_narrative_obscures_rent, conventional).
narrative_ontology:cs_reference_frame('c57b0230-78e9-4fe4-9233-b621f2591464', minoritarian_extraction_regime).
narrative_ontology:cs_drift_state('c57b0230-78e9-4fe4-9233-b621f2591464', contemporary_eu_reform_pressure, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c57b0230-78e9-4fe4-9233-b621f2591464', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, veto_wielding_minority).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Member state or small coalition that can block Council legislation in unanimity-governed areas. Uses the credible threat of veto to extract national opt-outs, side-payments, or policy dilution from the majority. Benefits disproportionately from a rule that amplifies its negative power into positive leverage.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, veto_wielding_minority, beneficiary,
    powerful, generational, constrained, continental).

% Broad coalition of member states seeking policy advancement in areas requiring unanimity. Bears the cost of the constraint through conceded opt-outs, financial side-payments, and legislative dilution necessary to buy off a blocking minority. Cannot bypass the rule without treaty reform or threatening EU exit, both prohibitively costly.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority, payer,
    powerful, generational, constrained, continental).

% Proposes legislation and seeks to advance integration. Its agenda is regularly held hostage by veto threats, forcing acceptance of watered-down compromises or abandonment of initiatives. Lacks unilateral authority to override Council unanimity and is structurally subordinate to member state bargaining.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, european_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Subject to policy outcomes of Council negotiations but excluded from the closed-door bargaining where unanimity is traded for concessions. Bear diffuse costs of suboptimal, delayed, or diluted policy when minority vetoes block majority-preferred solutions. Individual exit via emigration is costly and collective voice is weak in Council proceedings.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_citizens, excluded,
    powerless, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, veto_wielding_minority).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Requires all member states to consent before adopting legislation in designated policy areas, nominally ensuring no state is bound against its will in sovereignty-sensitive domains.
% TRANSFER_FUNCTION: Moves policy concessions, financial side-payments, opt-outs, and legislative dilution from the coalition majority to the blocking minority in exchange for lifting the veto threat.
% ABSENT_VOICES: EU citizens and organized civil society groups are excluded from Council negotiations; they bear the cost of suboptimal or delayed policy but are not in the room. Future generations and non-EU states affected by EU policy externalities also have no voice.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement disappeared, coalition majorities could adopt preferred policies without buying off blocking minorities through concessions and opt-outs; the structural leverage enabling minoritarian extraction would vanish, and EU legislative dynamics in sensitive areas would shift to majority coalition-building.
% FOUNDING_PROBLEM: Post-war European integration required reconciling deep sovereignty concerns with collective action; small member states feared being systematically outvoted by larger coalitions on issues touching core national interests.
% FOUNDING_PROBLEM_CORROBORATION: Historians and integration scholars outside the benefiting parties attest that the original problem was genuine but has been substantially transformed by enlargement and policy scope expansion; political scientists document systematic exploitation of the veto for national extraction. Minority beneficiary states deny the transformation and insist the founding problem remains fully live.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.82) is high because the historical record shows repeated instances of legislation being blocked or diluted until majority coalitions pay off holdouts. Suppression (0.75) is high because alternatives to unanimity â treaty reform, passerelle clauses, or leaving the EU â are structurally blocked by the same veto logic or prohibitive cost. Theater ratio (0.45) is moderate: Council negotiations perform elaborate consensus-building rituals that mask the underlying extraction. Accessibility collapse (0.70) reflects that once the unanimity rule is in place, meaningful alternatives for the majority collapse; resistance (0.55) is moderate because majority states chafe under the constraint but remain bound by treaty loyalty and exit costs. The metrics are authored independently of the claimed snare type.
 *
 * PERSPECTIVAL GAP:
 *   The veto-wielding minority and the coalition majority occupy the same nominal institutional plane (member states in the Council) but experience radically different directionalities. The minority sees the veto as sovereign right and legitimate defense; the majority experiences the same rule as coercive extraction. The Commission experiences it as institutional paralysis. These divergences are structurally determined by who can credibly threaten to block and who has something to lose from deadlock.
 *
 * DIRECTIONALITY LOGIC:
 *   The veto-wielding minority is declared beneficiary (d near the beneficiary end): the constraint subsidizes their bargaining position by amplifying a single state's negative power into leverage over the majority. The coalition majority is declared victim/payer (d near the target end): the constraint extracts from them via conceded opt-outs and diluted policy. The European Commission sits between â agenda-setter but also payer in terms of institutional ambition thwarted. Citizens are excluded and bear diffuse costs with no voice. No directionality override is needed because the beneficiary/victim structure cleanly maps to the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The veto trap reading prevents mislabeling by insisting that the presence of a coordination narrative ('sovereignty protection,' 'consensus building') does not establish coordination function. The mandatrophy test asks whether the founding problem â protecting small states from majoritarian override â is still live. The reading holds it is contested or dead: the contemporary pattern shows vetoes used across all state sizes to extract narrow national rents rather than protect existential sovereignty. Without this genealogy, the constraint would be misread as a Rope (consensus mechanism) or Mountain (inevitable feature of confederal design). The divergence between claimed type (snare) and the sovereignty-guarantor reading's implied type (rope/mountain) is exactly the cross-reading measurement the corpus exists to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_intentionality_or_structure,
    'Do blocking states intentionally wield vetoes to extract concessions, or does the extraction emerge structurally from sincere divergence of national interest under unanimity rules?',
    'Game-theoretic modeling of Council bargaining combined with archival analysis of negotiating mandates: intentional extraction reveals consistent demand patterns beyond sincere policy disagreement.',
    'If intentional, the constraint is more clearly a snare with identifiable agenda-setter-like beneficiaries; if structural, the extraction may be an emergent property of institutional design without malign intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_intentionality_or_structure, empirical, 'Intentional strategy versus emergent structural extraction').

omega_variable(
    kernel_reading_validity,
    'Which reading of the unanimity kernel â sovereignty guarantor, diplomatic capital, or veto trap â most accurately captures its structural operation in the contemporary EU?',
    'Comparative analysis across policy areas and time: if extraction correlates with policy salience and reversibility rather than sovereignty sensitivity, the veto trap reading gains support; if vetoes track existential sovereignty concerns, the sovereignty guarantor reading is vindicated.',
    'Resolution would reclassify the constraint across the full spectrum from rope/diplomatic-capital to snare/veto-trap, with significant implications for treaty reform advocacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Indeterminacy across competing kernel readings').

omega_variable(
    unanimity_reform_path,
    'Can the veto trap dynamic be dissolved without treaty reform, through enhanced cooperation, passerelle clauses, or political will?',
    'Tracking legislative output and judicial doctrine in areas where alternative decision-making routes exist.',
    'If bypass routes prove viable, accessibility_collapse is lower than measured and the constraint''s snare classification may weaken; if they are systematically blocked, extraction is locked in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_reform_path, empirical, 'Availability of non-treaty reform paths').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(eu_c_tr_t12, eu_council_unanimity__veto_trap_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(eu_c_tr_t24, eu_council_unanimity__veto_trap_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(eu_c_tr_t36, eu_council_unanimity__veto_trap_reading, theater_ratio, 36, 0.36).
narrative_ontology:measurement(eu_c_tr_t48, eu_council_unanimity__veto_trap_reading, theater_ratio, 48, 0.41).
narrative_ontology:measurement(eu_c_tr_t60, eu_council_unanimity__veto_trap_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(eu_c_be_t12, eu_council_unanimity__veto_trap_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(eu_c_be_t24, eu_council_unanimity__veto_trap_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(eu_c_be_t36, eu_council_unanimity__veto_trap_reading, base_extractiveness, 36, 0.62).
narrative_ontology:measurement(eu_c_be_t48, eu_council_unanimity__veto_trap_reading, base_extractiveness, 48, 0.74).
narrative_ontology:measurement(eu_c_be_t60, eu_council_unanimity__veto_trap_reading, base_extractiveness, 60, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__veto_trap_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(eu_c_su_t12, eu_council_unanimity__veto_trap_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(eu_c_su_t24, eu_council_unanimity__veto_trap_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(eu_c_su_t36, eu_council_unanimity__veto_trap_reading, suppression_requirement, 36, 0.67).
narrative_ontology:measurement(eu_c_su_t48, eu_council_unanimity__veto_trap_reading, suppression_requirement, 48, 0.72).
narrative_ontology:measurement(eu_c_su_t60, eu_council_unanimity__veto_trap_reading, suppression_requirement, 60, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
