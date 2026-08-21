% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity: Veto Trap Reading
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'veto trap' reading of the EU
 *   Council's unanimity rule. In this reading, the rule, originally intended
 *   to protect national sovereignty, has become a structural vulnerability
 *   that enables minoritarian extraction. A single member state can credibly
 *   threaten to block collective action, forcing concessions from the
 *   majority on unrelated issues, thereby systematically transferring value
 *   from the collective to the blocking minority. This is a Snare because the
 *   coordination story (consensus-building) is cover for an extractive
 *   mechanism that depends on suppressing the majority's will.
 *
 * KEY AGENTS:
 *   - blocking_member_state: Primary beneficiary/agenda_setter (powerful/arbitrage) — extracts concessions
 *   - coalition_majority_member_states: Primary payer (organized/constrained) — bears costs of diluted policy
 *   - eu_citizens: Secondary payer (powerless/trapped) — bears costs of policy paralysis
 *   - eu_institutions: Observer (institutional/analytical) — observes the dynamic but is constrained
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.85).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.75).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, snare).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity: Veto Trap Reading").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, 'b8a9b5aa-d074-4b4f-b362-f8c9e78c9110').
narrative_ontology:cs_kernel_codification('b8a9b5aa-d074-4b4f-b362-f8c9e78c9110', formalized).
narrative_ontology:cs_authority_grounding('b8a9b5aa-d074-4b4f-b362-f8c9e78c9110', lineage).
narrative_ontology:cs_interpretation_layer_present('b8a9b5aa-d074-4b4f-b362-f8c9e78c9110').
narrative_ontology:cs_reading_relation('b8a9b5aa-d074-4b4f-b362-f8c9e78c9110', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8a9b5aa-d074-4b4f-b362-f8c9e78c9110', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('b8a9b5aa-d074-4b4f-b362-f8c9e78c9110', foundational, unanimity_as_extraction_lever).
narrative_ontology:cs_axiom_status(unanimity_as_extraction_lever, holdable).
narrative_ontology:cs_axiom_grounding('b8a9b5aa-d074-4b4f-b362-f8c9e78c9110', unanimity_as_extraction_lever, empirically_contingent).
narrative_ontology:cs_reference_frame('b8a9b5aa-d074-4b4f-b362-f8c9e78c9110', post_maastricht_integration_era).
narrative_ontology:cs_drift_state('b8a9b5aa-d074-4b4f-b362-f8c9e78c9110', contemporary_multi_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b8a9b5aa-d074-4b4f-b362-f8c9e78c9110', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A single member state that uses its unanimity veto power to block collective action, not to protect its sovereignty, but to extract concessions or opt-outs on unrelated issues, or to prevent policies that would benefit other states without direct cost to itself. Benefits from the leverage the veto provides.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_member_state, agenda_setter,
    powerful, biographical, arbitrage, national).

% A group of member states that support a proposed EU policy but are forced to make concessions to a blocking state to achieve unanimity. They bear the cost of diluted policy outcomes or side payments, and their collective will is suppressed by the minority's leverage.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority_member_states, payer,
    organized, biographical, constrained, continental).

% Citizens across the EU who are denied the benefits of collective action due to minoritarian blocking. They experience the costs of policy paralysis or suboptimal outcomes, with no direct recourse to overcome the veto trap.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_citizens, payer,
    powerless, generational, trapped, continental).

% The European Commission, Parliament, and Council Secretariat, which propose and facilitate policies. They observe the paralysis and extraction caused by the unanimity rule, but their power to overcome it is limited by the member states' treaty-enshrined veto.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_institutions, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The unanimity rule is nominally intended to ensure all member states are genuinely committed to collective decisions, fostering deeper integration and legitimacy by requiring full consent.
% TRANSFER_FUNCTION: Systematically transfers policy influence and material concessions from the majority of member states to a minority blocking state, in exchange for its consent to collective action.
% ABSENT_VOICES: EU citizens, whose collective interests are often diluted or ignored when minoritarian blocking prioritizes narrow national gains over broader European welfare. Their voice is mediated through national governments, which are themselves subject to the veto trap.
% DISAPPEARANCE_RATIONALE: If the unanimity rule vanished overnight, the EU's decision-making process would fundamentally change. Policies currently blocked or diluted would pass, shifting power dynamics significantly from individual states to the collective, and potentially accelerating integration. The current system of minoritarian extraction would collapse.
% FOUNDING_PROBLEM: The unanimity rule was established to protect the sovereign interests of individual member states, ensuring no state could be coerced into collective action against its fundamental national interest, particularly in sensitive areas like foreign policy or taxation.
% FOUNDING_PROBLEM_CORROBORATION: Blocking member states and some national governments attest the founding problem (protection of sovereignty) is still live. However, a majority of member states, EU institutions, and independent political analysts attest that the rule is now primarily used for minoritarian extraction, indicating the founding problem is largely 'dead' in practice, or at least 'contested' in its application, as evidenced by numerous instances of vetoes for unrelated concessions.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the veto power allows a minority to systematically extract concessions from the majority, often on issues unrelated to the original policy. Suppression is also high (0.75) as the majority's policy preferences are effectively suppressed by the credible threat of a single state's veto. The theater ratio is low (0.1) because the blocking is a direct, functional exercise of power, not merely performative maintenance. The increasing trend in extractiveness and suppression reflects the growing frequency and strategic use of veto threats over time, particularly as the EU has expanded and tackled more sensitive policy areas.
 *
 * PERSPECTIVAL GAP:
 *   The blocking member state perceives the unanimity rule as a legitimate tool for defending national interests and maximizing its bargaining position (a 'sovereignty guarantor' or 'diplomatic capital' reading). The coalition majority, however, experiences it as an extractive 'veto trap' that undermines collective action and transfers value to the minority. The engine's classification as Snare reflects the structural reality of extraction from the majority's perspective, despite the blocking state's self-justification.
 *
 * DIRECTIONALITY LOGIC:
 *   The blocking_member_state is a full beneficiary (d=0.0) as it directly gains from the leverage of the veto. The coalition_majority_member_states and eu_citizens are full targets (d=1.0) as they bear the costs of policy paralysis and concessions. EU institutions are analytical observers (d=0.5) as they are structurally neutral to the extraction but affected by the paralysis.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling minoritarian extraction as legitimate coordination. While the unanimity rule has a genuine coordination function (ensuring consent), its current application, as described in this reading, has atrophied into an extractive mechanism. The 'veto trap' reading highlights how the mandate to protect sovereignty has been co-opted to enable rent-seeking, rather than serving its original purpose. The engine's Snare classification captures this shift, distinguishing it from a genuine Rope or Scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_extraction_motive,
    'Is a given veto exercised primarily to protect a core national sovereign interest, or to extract unrelated concessions?',
    'Detailed analysis of the blocking state''s stated reasons versus the actual policy outcomes and side deals, corroborated by diplomatic leaks or independent investigative journalism. If concessions are granted on unrelated issues, it points to extraction.',
    'If primarily for sovereignty, the constraint might be reclassified closer to a Rope or even a Mountain (from that state''s perspective). If primarily for extraction, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_extraction_motive, empirical, 'Distinguishing genuine sovereignty protection from strategic extraction.').

omega_variable(
    unanimity_rule_framing,
    'Is the EU Council''s unanimity rule fundamentally a ''sovereignty guarantor'' (protecting national interests), a ''diplomatic capital'' mechanism (forcing consensus), or a ''veto trap'' (enabling minoritarian extraction)?',
    'This is a conceptual omega. Resolution depends on which normative framework is adopted and which empirical evidence (e.g., frequency of vetoes, nature of concessions) is prioritized. No single empirical test can resolve the framing itself.',
    'The classification of the constraint would shift dramatically based on the adopted framing: from Mountain/Rope (sovereignty/diplomatic capital) to Snare (veto trap).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unanimity_rule_framing, conceptual, 'The fundamental framing of the unanimity rule''s function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1993, 0.4).
narrative_ontology:measurement(eu_c_be_t2000, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(eu_c_be_t2007, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2007, 0.7).
narrative_ontology:measurement(eu_c_be_t2014, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2014, 0.78).
narrative_ontology:measurement(eu_c_be_t2020, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2020, 0.82).
narrative_ontology:measurement(eu_c_be_t2024, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1993, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1993, 0.3).
narrative_ontology:measurement(eu_c_su_t2000, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(eu_c_su_t2007, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2007, 0.6).
narrative_ontology:measurement(eu_c_su_t2014, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2014, 0.68).
narrative_ontology:measurement(eu_c_su_t2020, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(eu_c_su_t2024, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'eu_council_unanimity' kernel. This 'veto_trap_reading' emphasizes minoritarian extraction, while 'sovereignty_guarantor_reading' focuses on protection of national interests, and 'diplomatic_capital_reading' on consensus-building. All three are distinct constraints derived from the same underlying rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
