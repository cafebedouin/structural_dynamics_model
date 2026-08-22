% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_sovereignty_maximalist, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: Sovereignty Maximalist Reading of RBIO Practice Norms
 *   domain: international_relations/international_law
 *
 * SUMMARY:
 *   The sovereignty maximalist reading asserts that state sovereignty is
 *   absolute and legitimate RBIO norms exist only to protect that sovereignty
 *   against external interference. Humanitarian exceptions are characterized
 *   as pretexts for regime change by hegemonic powers. Under this reading,
 *   there is no legitimate intervention authority except self-defense;
 *   conditionality is acceptable only when the target state can exit without
 *   cost. Beneficiaries are authoritarian regimes and non-aligned middle
 *   powers; victims are populations trapped under repressive governments with
 *   no external recourse. This constraint story instantiates ONE reading of
 *   the contested rbio_practice_norm_complex kernel; sibling readings
 *   (liberal_institutional_reading, hegemonic_extraction_reading) represent
 *   alternative structural interpretations of the same RBIO text and
 *   practice.
 *
 * KEY AGENTS:
 *   - Authoritarian regimes: institutional beneficiaries; invoke sovereignty to shield repression
 *   - Non-aligned middle powers: organized beneficiaries; use sovereignty maximalism to resist great-power pressure
 *   - Populations under repressive governments: powerless victims; trapped domestically, foreclosed externally
 *   - International humanitarian advocates: organized payers; delegitimized as pretextual regime changers
 *   - Liberal institutional advocates (wealthy democracies): powerful payers; contest the absolute sovereignty claim
 *   - P5 veto holders: institutional agenda setters; maintain discretionary enforcement
 *   - International courts and monitoring bodies: excluded; delegitimized by the sovereignty maximalist frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.68).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, snare).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "Sovereignty Maximalist Reading of RBIO Practice Norms").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '45e4c857-94d5-4682-a1c2-1f10c306dc8f').
narrative_ontology:cs_kernel_codification('45e4c857-94d5-4682-a1c2-1f10c306dc8f', fixed_text).
narrative_ontology:cs_authority_grounding('45e4c857-94d5-4682-a1c2-1f10c306dc8f', extraction).
narrative_ontology:cs_interpretation_layer_present('45e4c857-94d5-4682-a1c2-1f10c306dc8f').
narrative_ontology:cs_reading_relation('45e4c857-94d5-4682-a1c2-1f10c306dc8f', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('45e4c857-94d5-4682-a1c2-1f10c306dc8f', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('45e4c857-94d5-4682-a1c2-1f10c306dc8f', foundational, state_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('45e4c857-94d5-4682-a1c2-1f10c306dc8f', state_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('45e4c857-94d5-4682-a1c2-1f10c306dc8f', foundational, humanitarian_intervention_is_pretextual_regime_change).
narrative_ontology:cs_axiom_status(humanitarian_intervention_is_pretextual_regime_change, holdable).
narrative_ontology:cs_axiom_grounding('45e4c857-94d5-4682-a1c2-1f10c306dc8f', humanitarian_intervention_is_pretextual_regime_change, empirically_contingent).
narrative_ontology:cs_reference_frame('45e4c857-94d5-4682-a1c2-1f10c306dc8f', westphalian_sovereignty_principle).
narrative_ontology:cs_drift_state('45e4c857-94d5-4682-a1c2-1f10c306dc8f', contemporary_selective_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('45e4c857-94d5-4682-a1c2-1f10c306dc8f', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, non_aligned_middle_powers).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_humanitarian_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, p5_veto_holders).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutional_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the sovereignty maximalist reading to shield domestic repression from external criticism, investigation, or intervention. They frame internal governance as a matter of exclusive state prerogative and reject humanitarian conditionality as a cover for regime change. They benefit from a norm that treats sovereignty as absolute and foreclose accountability beyond borders.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, global).

% Use the sovereignty maximalist framing to resist external pressure on their own governance or geopolitical alignment. They benefit from a norm that legitimizes rejection of any intervention authority and reduces the asymmetry that larger powers might otherwise impose through humanitarian or institutional mechanisms.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, non_aligned_middle_powers, beneficiary,
    organized, generational, constrained, global).

% Bear the costs of the sovereignty maximalist reading: their suffering — torture, disappearance, systematic oppression — is coded as a matter of exclusive internal sovereignty and thus beyond the scope of international humanitarian norm invocation. They have no exit: domestic authorities control coercive force; external allies are foreclosed by the absolute sovereignty claim. Their only recourse (flight, asylum) remains individual and offers no system-level relief.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments, payer,
    powerless, biographical, trapped, local).

% Advocate for humanitarian intervention, monitoring, and conditionality as legitimate under RBIO norms. Under the sovereignty maximalist reading, their work is characterized as pretextual regime change; they operate in a constrained space where moral claims to intervene are delegitimized as hegemonic interference. Their advocacy carries reputational risk and limited leverage.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_humanitarian_advocates, payer,
    organized, biographical, constrained, global).

% Represent states (typically wealthy democracies) that advance conditional support, human rights review, and humanitarian intervention authority under the auspices of universal RBIO norms. They contest the sovereignty maximalist reading by proposing that legitimate state interests include ensuring RBIO compliance. They have exit options: they can unilaterally decouple from RBIO regimes (sanctions, non-recognition) but face legitimacy costs for appearing to impose their model.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutional_advocates, payer,
    powerful, generational, mobile, global).

% Control the binding interpretation of RBIO norms via Security Council veto. Under the sovereignty maximalist reading, they maintain discretion to invoke sovereignty protection against their own rivals while instrumentally invoking humanitarian exceptions against adversaries. They benefit from a norm that is absolute in principle (shielding them from intervention) but flexible in enforcement (allowing selective intervention against non-aligned targets).
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, p5_veto_holders, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, p5_veto_holders, beneficiary).

% International Criminal Court, UN Human Rights Council, and independent monitoring bodies that would investigate and adjudicate violations. Under the sovereignty maximalist reading, their authority is delegitimized as external interference; states opt out or block their mandates. They are structurally excluded from the constraint's operation and would argue for revisable sovereignty and conditional immunity (personal immunity vs. state immunity questions).
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_courts_and_monitoring, excluded,
    organized, generational, constrained, global).

% Academic and professional consensus on what RBIO norms permit. The sovereignty maximalist reading contests the liberal institutional reading's claim that RBIO norms ARE universal and consent-based; instead, it asserts they are a hegemonic project retroactively cloaked in universalism. This is a non-agent entity kept for narrative completeness.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, scholarly_consensus_on_rbio, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(rbio_practice_norm_complex__sovereignty_maximalist_reading, scholarly_consensus_on_rbio).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rule against external coercive interference in domestic governance, thereby enabling states of all regime types to plan and conduct policy without fear of forcible external override. Solves the collective-action problem of preventing stronger states from colonizing weaker ones through humanitarian or institutional pretexts.
% TRANSFER_FUNCTION: Transfers legitimacy from humanitarian and universal human rights claims to state sovereignty claims. It reallocates authority: from external accountability bodies and universal norms to domestic state discretion. Under this reading, intervention authority flows only to the target state (self-help), not to international institutions or coalitions.
% ABSENT_VOICES: Populations under repressive governments are structurally excluded — they have no seat at the state-to-state negotiating table and cannot formally contest the sovereignty maximalist reading. International human rights bodies (ICC, UN HR Council) are also excluded: their legitimacy is precisely what the sovereignty maximalist reading rejects. Scholars and practitioners advancing the liberal institutional reading are marginalized as hegemonic agents.
% DISAPPEARANCE_RATIONALE: If the sovereignty maximalist norm disappeared, the world would NOT rearrange immediately — the Westphalian state system would persist. BUT humanitarian intervention authority and international accountability mechanisms would activate; domestic repression would face external consequences (sanctions, investigation, intervention) that would reshape incentives for authoritarian regimes. Some parties (sovereigntist states) contest this: they argue removing the norm would enable hegemonic powers to colonize smaller states without restraint. The contestation is structural, not ephemeral.
% FOUNDING_PROBLEM: European colonialism and great-power interference in the 19th and early 20th centuries demonstrated that absent a non-intervention norm, stronger states would dominate weaker ones through military conquest or institutional manipulation. The Westphalian principle (cuius regio, eius religio) evolved into an absolute sovereignty norm: one state's internal affairs are beyond another state's authority to adjudicate.
% FOUNDING_PROBLEM_CORROBORATION: Sovereigntist states and non-aligned powers attest the founding problem is live: great-power intervention remains a persistent threat, and humanitarian conditionality is instrumentalized to justify regime change against geopolitical rivals (Libya 2011, Iraq 2003 interventions). Liberal institutional advocates and humanitarian monitors attest the founding problem is substantially solved — they argue the RBIO system HAS constrained great-power unilateralism, and the issue now is selective enforcement (capacity problem, not legitimacy problem). Academic historians document colonialism and great-power intervention; there is external corroboration of the founding problem, but deep disagreement on whether the sovereignty maximalist solution is structurally sound or a cover for authoritarianism.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the sovereignty maximalist reading enables authoritarian regimes to extract compliance and legitimacy from their own populations and from international actors without accountability. Suppression is also high (0.72) because maintaining the absolute sovereignty claim requires actively excluding external monitors, courts, and humanitarian advocates — their delegitimization is built into the reading's structure. Theater is moderate (0.41) and rising: the reading was originally grounded in genuine anti-colonialism (1945–1968, theater_ratio ~0.12–0.18) but has increasingly become performative maintenance — sovereignty is invoked as a shield against accountability for actions that contradict the founding problem (preventing great-power colonialism). The post-Cold War era (1991 onward) shows a sharp rise in both extractiveness and theater: the Cold War's competing bloc structure had somewhat constrained unilateral great-power interventions, but the unipolar moment (1991–2003) exposed the selective enforcement of sovereignty — hegemonic powers intervene against non-aligned targets (Iraq, Libya) while invoking sovereignty for allies. The measurement series are authored on one shared time grid spanning 1945–2026, with transition points marking shifts in the constraint's enforcement pattern (1968: decolonization consolidation; 1991: Cold War end; 2003: Iraq invasion; 2015: Syrian crisis and enforcement plateau).
 *
 * PERSPECTIVAL GAP:
 *   From the authoritarian regime seat and the P5 veto holder seat, the sovereignty maximalist reading is experienced as a genuinely protective coordination mechanism: it prevents external interference and preserves the state's capacity to manage its own affairs. From the population seat (powerless, trapped), the same reading is experienced as extraction-enabling: it forecloses external recourse and legitimizes domestic repression. From the liberal institutional advocate seat (powerful), the reading is experienced as selectively enforced — applied against adversaries but not allies, thus instrumentalizing sovereignty to serve hegemonic interests. The engine computes per-seat classifications from the structural data: a powerful agent with mobile exit options will compute a different d (lower effective extraction) than a powerless agent with trapped exit; the same constraint maps to different types from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes: role=beneficiary, power=institutional, exit=arbitrage (they can defect to alternate alliance systems or non-aligned positions). Directionality low (d~0.15–0.25), because they actively benefit and can exit while maintaining state status. Non-aligned middle powers: role=beneficiary, power=organized, exit=constrained (they depend on RBIO legitimacy and cannot fully exit without legitimacy cost). Directionality moderate (d~0.35–0.45). Populations under repressive governments: role=payer, power=powerless, exit=trapped (they cannot exit their state's territory, cannot invoke external humanitarian authority, cannot organize transnational resistance). Directionality very high (d~0.85–0.95) — they are the primary targets of the constraint's extraction. International humanitarian advocates: role=payer, power=organized, exit=constrained (they can create alternative advocacy networks but cannot override the constraint's delegitimization). Directionality moderate-high (d~0.55–0.70). Liberal institutional advocates: role=payer, power=powerful, exit=mobile (they can sanction, recognize alternative governments, organize coalitions). Directionality moderate (d~0.40–0.55). P5 veto holders: role=agenda_setter (they enforce the sovereignty maximalist reading selectively), secondary role=beneficiary (they benefit from a norm that shields them from intervention). Directionality low (d~0.10–0.20), because they set enforcement terms and collect the legitimacy gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty maximalist reading exhibits mandatrophy: its founding problem (preventing great-power colonialism) is substantially solved by the existing RBIO architecture, yet the reading persists and has become increasingly extractive. The post-1991 era shows a plateau in suppression (0.72 from 2003 onward) and rising theater (0.41 by 2026): enforcement has become performative maintenance rather than functional prevention. The measurement series capture this drift — extractiveness and theater rise through 2003 (the Iraq invasion, where sovereignty was invoked to justify non-intervention in some cases and overridden for geopolitical reasons in others), then plateau. The beneficiaries (authoritarian regimes, P5 holders) could modify or exit the reading, but the cost to fix is prohibitive for the payee seats (international advocates depend on RBIO legitimacy, populations have no agency). The constraint persists because it is now mostly theater: the appearance of sovereign protection masks selective enforcement that serves hegemonic interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_vs_conditional_sovereignty,
    'Is state sovereignty truly absolute, or is it conditionally suspended when internal atrocities reach a threshold that triggers humanitarian intervention authority?',
    'Comparative legal analysis of state practice: do states actually treat sovereignty as absolute in cases of genocide, mass atrocity, systematic torture? Or do they invoke R2P (Responsibility to Protect) and humanitarian intervention precisely when sovereignty claims conflict with human rights norms? If the latter, sovereignty is conditionally suspended in practice, contradicting the sovereignty maximalist reading''s core premise.',
    'If sovereignty is revealed to be conditional rather than absolute in state practice, the reading''s foundational claim is undermined. This would reclassify the constraint from snare (extractive, shielding repression) to tangled_rope (extraction layered onto genuine coordination of non-intervention, but with negotiated exceptions) or conversely to hegemonic_extraction if interventions are selective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_vs_conditional_sovereignty, empirical, 'Whether state sovereignty is structurally absolute or conditionally suspended in practice.').

omega_variable(
    humanitarian_exception_legitimacy,
    'Are humanitarian exceptions to sovereignty always pretextual regime change, or are some grounded in genuine atrocity thresholds that cross into universal human rights?',
    'Pattern analysis of humanitarian intervention cases: cases where intervention occurred (Kosovo, Rwanda intervention debate, Syria non-intervention) vs. cases where humanitarian norms would justify intervention but sovereignty maximalism blocked it. Do interventions cluster around geopolitical interest or around actual atrocity severity? If the latter, humanitarian exceptions are not purely pretextual; if the former, the sovereignty maximalist reading''s claim that they ARE pretextual holds.',
    'If humanitarian exceptions are not purely pretextual, then the sovereignty maximalist reading mischaracterizes the constraint''s function — it is not pure extraction, but hybrid extraction-coordination (tangled_rope). If they are purely pretextual, the snare classification holds and extraction is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_exception_legitimacy, empirical, 'Whether humanitarian intervention exceptions are structurally grounded or instrumentally selective.').

omega_variable(
    kernel_reading_contestation,
    'Which reading of the rbio_practice_norm_complex kernel most accurately describes state practice: the sovereignty maximalist reading, the liberal institutional reading, or the hegemonic extraction reading?',
    'Each reading proposes a different structural interpretation of RBIO. Sovereignty maximalism says RBIO norms ARE legitimate when they protect absolute state sovereignty. Liberal institutionalism says RBIO norms ARE legitimate when they are consent-based and revisable. Hegemonic extraction says RBIO norms ARE formally legitimate but operationally frozen by P5 veto. These are not empirical hypotheses about RBIO''s *origin* (that is history) but structural hypotheses about what legitimizes RBIO *now*. Resolution requires genealogical analysis: which reading''s framing of legitimacy is most coherent with how states ACTUALLY justify their RBIO conduct? This is a conceptual question, not empirical, but empirical evidence (state justifications, voting patterns, treaty negotiations) informs the framing choice.',
    'This is the fundamental contest this constraint story is PART OF. If the liberal institutional reading is correct, then this sovereignty maximalist reading is a misleading deviation that obscures the actual RBIO structure. If the hegemonic extraction reading is correct, then both the sovereignty maximalist and liberal institutional readings are ideological covers for the same underlying extraction. The engine will compute all three readings as separate constraints and let the corpus''s aggregate evidence adjudicate the most coherent frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which structural interpretation of the RBIO kernel is most coherent with actual state practice.').

omega_variable(
    populator_exit_mechanism_ambiguity,
    'Under the sovereignty maximalist reading, can populations escape repressive regimes through asylum and refugee status, or does the constraint functionally foreclose that exit as well?',
    'Observe whether asylum systems in liberal states accept refugees from sovereignty maximalist-protected repressive regimes, or whether those states invoke sovereignty to deny entry. If liberal states accept refugees, the reading is less absolutely extractive (populations have a costly but real exit). If liberal states invoke sovereignty reciprocally to deny asylum, the reading forecloses exit entirely.',
    'If exit is available through asylum (though costly), the d for populations shifts downward and the constraint''s classification may shift from snare toward tangled_rope (extraction with partial exit). If exit is foreclosed (asylum denied on sovereignty grounds), the snare classification and very high d hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(populator_exit_mechanism_ambiguity, empirical, 'Whether populations can exit repressive regimes through asylum or are functionally trapped by sovereignty principles reciprocally applied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(rbio_tr_t1968, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1968, 0.18).
narrative_ontology:measurement(rbio_tr_t1991, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1991, 0.28).
narrative_ontology:measurement(rbio_tr_t2003, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2003, 0.38).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(rbio_tr_t2026, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(rbio_be_t1968, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement(rbio_be_t1991, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1991, 0.55).
narrative_ontology:measurement(rbio_be_t2003, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2003, 0.64).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(rbio_be_t2026, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1945, 0.48).
narrative_ontology:measurement(rbio_su_t1968, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1968, 0.56).
narrative_ontology:measurement(rbio_su_t1991, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1991, 0.64).
narrative_ontology:measurement(rbio_su_t2003, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2003, 0.72).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(rbio_su_t2026, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.18).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% The rbio_practice_norm_complex kernel decomposes into three structurally distinct constraint stories, one per reading. The sovereignty_maximalist_reading asserts that RBIO norms are legitimate WHEN they protect absolute state sovereignty; this is logically incompatible with the liberal_institutional_reading's assertion that RBIO norms are legitimate because they are consent-based and revisable (which implies sovereignty can be conditionally suspended). The hegemonic_extraction_reading asserts that both readings are ideological covers for frozen hegemonic project. These three constraints share the same kernel text (RBIO practice) but propose different ε values and beneficiary structures because each reading instantiates a different structural interpretation. The sibling readings are linked via network.affects_constraints to enable the corpus to analyze how different readings of the same kernel produce different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, powerless, 0.92).
constraint_indexing:directionality_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
