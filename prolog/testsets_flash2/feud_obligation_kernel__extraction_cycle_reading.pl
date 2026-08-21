% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Extraction Cycle
 *   domain: legal_anthropology/medieval_history/political_systems
 *
 * SUMMARY:
 *   This constraint describes blood-feud obligations as a destructive cycle
 *   of extraction, depleting resources and lives, and hindering the
 *   development of stable political systems. It is one reading of the
 *   'feud_obligation_kernel', focusing on the negative societal impacts and
 *   the way royal authority leverages the chaos for its own consolidation.
 *   The reading emphasizes the high costs borne by participants and
 *   non-participants alike, and the active suppression of alternatives to
 *   kinship-based enforcement.
 *
 * KEY AGENTS:
 *   - feud_families: Primary victims (identity_locked/moderate) — bear direct costs of violence
 *   - peasantry: Collateral victims (trapped/powerless) — suffer indirect costs, no exit
 *   - local_merchants: Economic victims (constrained/moderate) — suffer trade disruption
 *   - royal_authority: Primary beneficiary/agenda_setter (arbitrage/institutional) — profits from legitimizing monopoly on violence
 *   - mercenary_bands: Secondary beneficiaries (mobile/organized) — profit from perpetuating conflict
 *   - ecclesiastical_institutions: Excluded (analytical/institutional) — preach against feuds but lack enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.9).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.75).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '84b879c9-10d8-4572-87eb-494cc8972f4b').
narrative_ontology:cs_kernel_codification('84b879c9-10d8-4572-87eb-494cc8972f4b', implicit).
narrative_ontology:cs_authority_grounding('84b879c9-10d8-4572-87eb-494cc8972f4b', practice).
narrative_ontology:cs_reading_relation('84b879c9-10d8-4572-87eb-494cc8972f4b', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('84b879c9-10d8-4572-87eb-494cc8972f4b', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('84b879c9-10d8-4572-87eb-494cc8972f4b', foundational, violence_depletes_productive_capacity).
narrative_ontology:cs_axiom_status(violence_depletes_productive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('84b879c9-10d8-4572-87eb-494cc8972f4b', violence_depletes_productive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('84b879c9-10d8-4572-87eb-494cc8972f4b', foundational, centralized_justice_is_net_beneficial).
narrative_ontology:cs_axiom_status(centralized_justice_is_net_beneficial, holdable).
narrative_ontology:cs_axiom_grounding('84b879c9-10d8-4572-87eb-494cc8972f4b', centralized_justice_is_net_beneficial, instrumental).
narrative_ontology:cs_reference_frame('84b879c9-10d8-4572-87eb-494cc8972f4b', pre_state_anarchy).
narrative_ontology:cs_drift_state('84b879c9-10d8-4572-87eb-494cc8972f4b', rise_of_royal_authority, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('84b879c9-10d8-4572-87eb-494cc8972f4b', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, mercenary_bands).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feud_families).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, peasantry).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, local_merchants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by kinship and honor to avenge wrongs, leading to cycles of violence, loss of life, and destruction of property. Their identity is deeply intertwined with the obligation, making exit unthinkable without profound social cost.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_families, payer,
    moderate, generational, identity_locked, local).

% Suffer collateral damage from feuds, including destruction of crops, livestock, and homes. They are often conscripted or caught in the crossfire, with no means to escape the violence or influence its course.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% Experience disruption to trade routes, loss of goods, and reduced market access due to ongoing feuds. They bear economic costs but are dependent on the local power structures, limiting their ability to exit or resist.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, local_merchants, payer,
    moderate, biographical, constrained, regional).

% Benefits from the chaos of feuds by presenting itself as the sole legitimate arbiter of justice and enforcer of peace. This legitimizes its expansion of power, consolidation of territory, and collection of taxes for 'protection' and 'justice'.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_authority, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, royal_authority, agenda_setter).

% Profit directly from the ongoing violence, being hired by feuding families or royal authorities. They have a vested interest in the perpetuation of conflict, as it ensures their employment.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, mercenary_bands, beneficiary,
    organized, immediate, mobile, regional).

% Preach against feuds as violations of divine law and attempt to impose 'Peace of God' movements, but their authority is often insufficient to halt the cycles of violence, especially when secular powers benefit from them.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_institutions, excluded,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__extraction_cycle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the absence of centralized justice, feuds provide a mechanism for families to seek retribution for wrongs, acting as a form of self-help justice.
% TRANSFER_FUNCTION: Transfers lives, property, and productive capacity from feuding families and the general populace to the cycle of violence itself, indirectly benefiting those who profit from instability or offer 'protection'.
% ABSENT_VOICES: The peasantry and local merchants, who bear the brunt of the violence and economic disruption, would advocate for peace and centralized justice. Ecclesiastical institutions also preach against feuds but lack the coercive power to enforce their will.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished overnight, the immediate cessation of violence would allow for increased agricultural output, safer trade, and a reduction in mortality. Royal authority would lose a key justification for its expansion, and the social fabric would reorganize around new forms of conflict resolution and governance.
% FOUNDING_PROBLEM: The absence of a centralized, legitimate authority capable of enforcing justice and deterring crime, leading individuals and families to resort to self-help mechanisms for redress.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and legal codes from the period show that while feuds were a response to a lack of central authority, their destructive nature often led to calls for royal intervention. Royal charters and legal reforms from outside the feuding families consistently framed feuds as a problem to be solved by state power, not a legitimate form of justice.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) because the cycle of violence leads to significant loss of life, property, and productive capacity, with little net benefit to the participants. Suppression (0.75) is high due to the strong social and identity-based pressures to participate in feuds, coupled with the lack of effective alternative justice mechanisms. Theater ratio is low (0.1) as the destructive function is quite direct and not primarily performative; any 'justice' achieved is outweighed by the costs. Accessibility collapse is high (0.8) because the social structure and lack of state power make it very difficult to opt out of the feud system. Resistance is high (0.9) from those caught in the cycle, but largely ineffective due to the systemic nature of the constraint.
 *
 * PERSPECTIVAL GAP:
 *   Feud families, while victims, may perceive the obligation as a necessary, albeit costly, means of maintaining honor and seeking justice in a stateless society. Royal authority, however, views it as a barbaric practice that justifies its own expansion of power. The engine's per-seat classification will reflect this divergence, with feud families experiencing it as a snare (high extraction, identity-locked) and royal authority as a rope (beneficiary, legitimizing its role).
 *
 * DIRECTIONALITY LOGIC:
 *   Feud families are identity_locked targets (high d) due to honor codes and kinship ties. Peasantry and local merchants are trapped/constrained targets (high d) due to their vulnerability and lack of exit. Royal authority is a beneficiary (low d) as it gains legitimacy and resources by 'solving' the feud problem. Mercenary bands are also beneficiaries (low d) as they profit from the conflict itself. Ecclesiastical institutions are excluded, their attempts at pacification largely ineffective against the structural forces.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading classifies the feud as a snare, emphasizing its destructive and extractive nature. This prevents mislabeling it as a 'coordination mechanism' (as the 'stateless_coordination_reading' might) by highlighting the net negative sum and the identifiable victims. The persistence of feuds, despite their costs, is explained by the lack of alternatives and the benefits accrued by external actors (royal authority, mercenaries) who have an interest in its continuation, rather than by a genuine coordination function for the participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    net_social_welfare_calculation,
    'What is the true net social welfare impact of blood-feud obligations, accounting for both the ''justice'' provided and the costs of violence?',
    'Detailed historical-economic modeling comparing regions with and without strong feud traditions, or before and after state pacification, using metrics like mortality rates, agricultural output, and trade volume.',
    'If net welfare is positive, the ''stateless_coordination_reading'' gains empirical support; if strongly negative, this ''extraction_cycle_reading'' is further validated. This would shift the classification from snare to tangled_rope or even rope if the coordination benefits were found to outweigh extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_social_welfare_calculation, empirical, 'Quantifying the overall societal impact of feuds beyond individual costs.').

omega_variable(
    legitimacy_of_royal_intervention,
    'To what extent was royal authority''s intervention against feuds genuinely driven by a desire for peace and justice, versus a strategic move to consolidate power and extract resources?',
    'Analysis of royal decrees, legal reforms, and tax records, cross-referenced with independent accounts of local conditions and the actual enforcement mechanisms employed.',
    'If intervention was primarily self-serving, the ''royal_authority'' seat''s directionality would shift further towards beneficiary/extractor. If genuinely altruistic, it would support a more ''scaffold''-like interpretation of early state-building efforts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_royal_intervention, conceptual, 'Distinguishing genuine pacification from power consolidation by royal authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternatives to feuds structural (lack of state capacity) or internalized (honor codes, identity fusion)?',
    'Comparative analysis of communities where state capacity was introduced: if feuds persist despite external enforcement, internalized suppression is dominant. If they rapidly decline, structural suppression was key.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the identity-locked feud families carry the suppression with them after exit. If structural, state-building efforts are more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for feud participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 80, 0.11).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 60, 0.88).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 80, 0.89).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 100, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 80, 0.73).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, royal_taxation_legitimacy).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, territorial_consolidation_process).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feud_obligation_kernel'. This 'extraction_cycle_reading' focuses on the destructive and extractive aspects, contrasting with the 'stateless_coordination_reading' and the 'christianized_pacification_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
