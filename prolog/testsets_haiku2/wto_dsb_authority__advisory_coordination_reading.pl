% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__advisory_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__advisory_coordination_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: wto_dsb_authority__advisory_coordination_reading
 *   human_readable: WTO DSB Advisory Coordination (Sovereignty-Preserving Reading)
 *   domain: international_law/institutional_legitimacy
 *
 * SUMMARY:
 *   Under this reading, WTO Dispute Settlement Body panels function as
 *   neutral expert advisors rather than binding judges. Member states
 *   commission panels to author reasoned legal opinions on contested trade
 *   measures, use those opinions to strengthen their bilateral negotiating
 *   positions, and retain unilateral discretion over whether to comply with
 *   any settlement. This reading treats DSB rulings as inputs to negotiation,
 *   not judicial orders. Sovereignty is preserved because no state surrenders
 *   policy-making authority to the DSB—the panel can diagnose a legal
 *   violation, but compliance depends on negotiated settlement or fear of
 *   bilateral retaliation, not institutional authority. The constraint
 *   operates through coordination (shared professional standards for
 *   interpreting trade law) rather than coercion (mandatory compliance). The
 *   claim/metric gap is intentional: this reading is claimed as rope (genuine
 *   coordination with minimal coercive overhead) while the authored metrics
 *   reflect moderate extractiveness rising slowly over 31 years and modest
 *   theater growth—the engine will measure whether the constraint remains
 *   coordinative or has drifted toward extraction.
 *
 * KEY AGENTS:
 *   - WTO DSB panels (institutional agenda-setter; author expert opinions; enforce nothing)
 *   - Complainant member states (beneficiaries; use DSB opinions to improve negotiating leverage; retain exit via non-compliance + retaliation)
 *   - Respondent member states (payers through constraints on policy space during dispute; retain exit via acceptance or rejection)
 *   - WTO membership collective (institutional beneficiary; maintains consensus-based governance by preserving sovereign discretion)
 *   - Enforcement authority claimants (excluded under this reading; would be present in binding_referee_reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, 0.31).
domain_priors:suppression_score(wto_dsb_authority__advisory_coordination_reading, 0.18).
domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__advisory_coordination_reading, rope).
narrative_ontology:human_readable(wto_dsb_authority__advisory_coordination_reading, "WTO DSB Advisory Coordination (Sovereignty-Preserving Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__advisory_coordination_reading, "international_law/institutional_legitimacy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__advisory_coordination_reading, '669e8291-825f-43cd-8435-fbe9b1c02559').
narrative_ontology:cs_kernel_codification('669e8291-825f-43cd-8435-fbe9b1c02559', fixed_text).
narrative_ontology:cs_authority_grounding('669e8291-825f-43cd-8435-fbe9b1c02559', lineage).
narrative_ontology:cs_interpretation_layer_present('669e8291-825f-43cd-8435-fbe9b1c02559').
narrative_ontology:cs_reading_relation('669e8291-825f-43cd-8435-fbe9b1c02559', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('669e8291-825f-43cd-8435-fbe9b1c02559', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('669e8291-825f-43cd-8435-fbe9b1c02559', foundational, states_retain_ultimate_policy_discretion).
narrative_ontology:cs_axiom_status(states_retain_ultimate_policy_discretion, holdable).
narrative_ontology:cs_axiom_grounding('669e8291-825f-43cd-8435-fbe9b1c02559', states_retain_ultimate_policy_discretion, deontological).
narrative_ontology:cs_axiom('669e8291-825f-43cd-8435-fbe9b1c02559', foundational, dsb_authority_is_advisory_not_judicial).
narrative_ontology:cs_axiom_status(dsb_authority_is_advisory_not_judicial, holdable).
narrative_ontology:cs_axiom_grounding('669e8291-825f-43cd-8435-fbe9b1c02559', dsb_authority_is_advisory_not_judicial, conventional).
narrative_ontology:cs_reference_frame('669e8291-825f-43cd-8435-fbe9b1c02559', advisory_coordination_authority).
narrative_ontology:cs_drift_state('669e8291-825f-43cd-8435-fbe9b1c02559', contemporary_2026, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('669e8291-825f-43cd-8435-fbe9b1c02559', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, negotiating_member_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, dispute_resolution_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, complainant_member_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, wto_membership_collective).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, complainant_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, respondent_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Composed of expert trade lawyers selected by parties and WTO Secretariat. Author written opinions explaining legal interpretations of WTO agreements, identify facts, and propose reasoned guidance to support bilateral negotiation. Their authority is treated as expert advice, not judicial decree. Opinions inform settlement but do not compel outcomes.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, wto_dsb_panels, agenda_setter,
    institutional, generational, analytical, global).

% Initiate disputes when they believe their trading rights are harmed. Receive reasoned analysis from expert panels that strengthens their negotiating position relative to the respondent. Under this reading they retain the option to accept or reject panel guidance and pursue alternative remedies (retaliation, regional negotiation, or exit). They pay through participation costs and accept some constraint on policy space while dispute is active.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, complainant_member_states, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, complainant_member_states, payer).

% Defend their policies or practices under challenge. Receive panel opinions that may expose legal vulnerability but do not mandate immediate change. They retain unilateral discretion to modify behavior, negotiate a settlement, or refuse to comply and face bilateral consequences (retaliation authorized by the complaining state). The panel opinion is leverage in negotiation, not an order.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, respondent_member_states, payer,
    organized, generational, mobile, global).

% All WTO members collectively benefit from a predictable, neutral dispute resolution mechanism that reduces uncertainty and facilitates trade without centralizing enforcement power in a supranational court. The advisory framing preserves consensus-based WTO governance and state sovereignty while providing dispute clarity.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, wto_membership_collective, beneficiary,
    institutional, generational, constrained, global).

% Under alternative readings (binding_referee_reading), there would be a supranational WTO enforcement authority with compulsory jurisdiction. This reading's framework excludes that institutional structure by design—no centralized WTO court with mandatory compliance authority. Such a body would demand sovereignty surrender that this reading's constituent states refuse.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, enforcement_authority_claimants, excluded,
    institutional, generational, trapped, global).

% Benefit indirectly from reduced trade uncertainty when disputes are resolved through expert guidance and negotiation, but have no direct access to the DSB process. Their interests are represented through member state delegation. They observe whether disputes get resolved in ways that protect market access.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, private_traders_export_sectors, observer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__advisory_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(wto_dsb_authority__advisory_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides neutral, expert legal analysis of whether contested trade measures comply with WTO agreements, facilitating bilateral negotiation by reducing information asymmetry and legal ambiguity. The coordination problem solved is: without DSB analysis, states negotiate trade disputes with no shared professional standard for interpreting treaty obligations, leading to deadlock or raw power bargaining.
% TRANSFER_FUNCTION: Transfers decision-making authority over policy outcomes from panels to negotiating states themselves. What flows is legitimized legal reasoning (expert opinion), which both strengthens the complaining state's negotiating position and constrains the respondent state's claimed discretion—but neither state is forced to comply.
% ABSENT_VOICES: Supranational enforcement constituencies (those who would argue for a binding judicial authority) are structurally excluded from this reading's framing. Under the binding_referee_reading, a centralized WTO enforcement apparatus would exist and would demand sovereignty surrender; this reading forecloses that institutional innovation by design. Private trade constituencies have no direct voice but are represented through member state preferences.
% DISAPPEARANCE_RATIONALE: If the DSB advisory function disappeared, member states would lose a trusted neutral mechanism for interpreting their mutual commitments. Trade disputes would devolve to bilateral power bargaining or regional dispute forums, settlement would become slower and less predictable, and consensus-based WTO governance would lack the expert input it currently relies on to legitimate negotiated outcomes. The trading system would reorganize around regional blocs or direct negotiations without the WTO's coordinating role.
% FOUNDING_PROBLEM: The Uruguay Round created a rules-based trade system with written agreements (GATT, GATS, TRIPs, etc.) but no agreed mechanism for interpreting how those rules apply to new trade disputes. States needed a neutral way to assess legal claims without giving up sovereignty to a supranational court.
% FOUNDING_PROBLEM_CORROBORATION: WTO member states in the Appellate Body reform process (2017-present) continue to assert that dispute resolution must remain advisory and state-driven; the foundational tension between binding authority and sovereignty persists. Legal scholars outside the benefiting parties (academic commentary on DSB legitimacy, ICJ advisory opinions on institutional independence) confirm the founding problem remains live—the need for dispute clarity without centralized enforcement.
narrative_ontology:disappearance_verdict(wto_dsb_authority__advisory_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__advisory_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__advisory_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__advisory_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__advisory_coordination_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__advisory_coordination_reading_tests).
:- end_tests(wto_dsb_authority__advisory_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.18 at 1995) because the advisory framing imposes minimal coercive obligation on any single state—each retains unilateral exit via non-compliance and bilateral retaliation negotiation. It rises slowly to 0.31 by 2026 as jurisprudence accumulates and compliance expectations harden (a soft normative pressure, not a structural constraint change). Theater is low and rises gradually (0.08→0.22) because the DSB maintains a relatively stable expert-advisory function; the rising ratio reflects creeping normalization of compliance (parties increasingly stage acceptance of panel findings) but does not signal functional atrophy. Suppression is minimal (0.08→0.18) because the constraint operates through expert legitimacy and bilateral power dynamics, not coercion—states are not barred from refusing compliance; they face only retaliation and reputation costs. The measurement series tracks one shared time grid across all three metrics, showing the advisory-coordination reading as a low-extractiveness, low-suppression baseline that has drifted slightly upward as states informally treat DSB opinions as increasingly dispositive (soft drift, not structural change). All measurements are observed through 2023; 2026 projections assume continuation of the current soft-hardening trend.
 *
 * PERSPECTIVAL GAP:
 *   The DSB panels and WTO membership see this arrangement as genuine coordination that reduces dispute uncertainty while preserving state sovereignty. Respondent states experiencing unfavorable panel opinions see extraction—they face negotiating pressure grounded in the panel's authority, even though they retain formal exit. Complainant states see beneficiary status—the panel's opinion validates their legal position. The engine computes these divergent perceptions from the stakeholder power atoms and exit options: powerful organized states with mobile exit (complainants with leverage) perceive lower extraction than weaker respondents with constrained exit. The advisory framing is what keeps extraction low—if compliance expectations hardened enough to transform the reading into binding_referee_reading, extraction and suppression would jump sharply upward for respondent states.
 *
 * DIRECTIONALITY LOGIC:
 *   WTO DSB panels are the structural agenda-setter: they define the terms on which disputes are analyzed and initiate the reasoning that shapes negotiation. They extract no direct benefit but wield institutional authority. Complainant states are near-beneficiary (d ≈ 0.25): they benefit from expert validation of their claims and use it in negotiation; they pay through participation costs and accept some constraint on their own policy discretion (they could not initiate the same dispute-narrative without DSB authority). Respondent states are near-payer (d ≈ 0.70): they bear constraint on their policy choices during dispute and face reputational/bilateral pressure to comply, but retain exit via non-compliance and can negotiate alternatives. WTO membership is institutional-beneficiary (d ≈ 0.15): genuine coordination benefit, negligible extraction cost. The derived directionality chain follows from beneficiary/victim declarations + exit options: beneficiaries get low d (coordination benefit outweighs constraint); victims/payers get high d (extraction pressure despite formal exit). No overrides are needed—the structural derivation captures the advisory reading's fundamental asymmetry: it is coordination that distributes benefits asymmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is NOT mandatrophic. The founding problem (need for neutral dispute resolution without sovereignty surrender) remains live, settlement patterns confirm the advisory framing is still operative (states negotiate settlements rather than accept orders), and the DSB retains a genuine coordination function. However, the rising theater_ratio (0.08→0.22) and slow extractiveness creep (0.18→0.31) signal a soft drift toward binding_referee_reading—states are increasingly treating DSB opinions as de facto binding, and the gap between formal sovereignty and actual compliance expectation is narrowing. This is a slow-motion reading-transition, not mandatrophy. If the drift continued to theater_ratio > 0.5 and extractiveness > 0.65, the constraint would be better classified under binding_referee_reading; at current trajectory, that reclassification is 15-20 years away (if current trends hold).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    advisory_vs_binding_drift,
    'Is the DSB''s authority genuinely advisory (states retain unilateral discretion) or has it drifted toward de facto binding through normalization of compliance?',
    'Empirical observation of settlement behavior: do states negotiate settlements after unfavorable panel opinions (advisory signature) or accept panel conclusions without negotiation (binding signature)? Coded analysis of 50+ major disputes post-2010 to measure negotiation intensity and compliance rates absent explicit Dispute Settlement Understanding amendments.',
    'If drift is confirmed (settlement without negotiation, compliance > 90% after panel opinion), reclassify to binding_referee_reading; extracted states would then perceive extraction closer to 0.65, suppression > 0.40. This reading would be empirically falsified by the data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(advisory_vs_binding_drift, empirical, 'Whether the advisory reading''s formal sovereignty preservation matches actual practice or has been eroded by normalization.').

omega_variable(
    sovereignty_retention_credibility,
    'Do weaker member states actually retain meaningful exit from DSB-guided settlements, or is their exit theoretical (formal right with prohibitive retaliation cost)?',
    'Case study of smallest/weakest WTO members facing unfavorable panel findings: Can they reject settlement without facing disproportionate retaliation? Comparative analysis of retaliation authorization rates for states that accept vs. reject panel guidance.',
    'If exit is theoretical (retaliation prohibitively costly for weak states), the reading''s claim of preserved sovereignty is false for a subset of stakeholders—constrained-exit states would face effective binding authority despite the formal advisory framing. This would support reclassification of weak-state experience to snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_retention_credibility, empirical, 'Whether formal sovereignty preservation in the advisory reading is equally credible for all member state power levels.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the DSB''s growing normative authority (rising theater_ratio) represent deepened expert legitimacy (coordinative function) or accumulated institutional power extraction?',
    'Analyze panel reasoning patterns: do panels increasingly invoke precedent and jurisprudential development (extending authority through technical legitimacy) or defer to member state directives (preserving advisory posture)? Interview DSB panelists and member state counsel on whether they experience the panels'' reasoning as advisory or prescriptive.',
    'If panels are extending authority through technical reasoning (judicial drift), the constraint is beginning the transition from advisory_coordination_reading to judicial_activism_reading. If panels remain deferential to member directives, the advisory reading is stable despite rising theater. This question gates the reading-transition timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the DSB''s measured rising theater reflects coordinative deepening or extractive creep.').

omega_variable(
    kernel_reading_contest_structure,
    'Does the binding_referee_reading genuinely foreclose this advisory_coordination_reading, or do these readings coexist as live positions held by different member state coalitions?',
    'Survey member state jurisprudential positions and treaty interpretation documents post-2015: how many explicitly endorse binding authority? How many explicitly defend advisory-plus-negotiation authority? Is the split clean between states, or do individual states hold both positions in different contexts?',
    'If readings coexist (different states hold different interpretations of DSB authority simultaneously, neither ruling out the other within each state''s framework), the relation is coexists_with. If one reading''s core premise logically contradicts the other such that a state cannot consistently hold both within any framework, the relation is forecloses. This question determines the cs_structure.reading_relations atom for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'The structural relationship between this reading and binding_referee_reading at the kernel level.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__advisory_coordination_reading, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement_basis(wto__tr_t1995, observed).
narrative_ontology:measurement(wto__tr_t2002, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2002, 0.12).
narrative_ontology:measurement_basis(wto__tr_t2002, observed).
narrative_ontology:measurement(wto__tr_t2009, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2009, 0.16).
narrative_ontology:measurement_basis(wto__tr_t2009, observed).
narrative_ontology:measurement(wto__tr_t2016, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement_basis(wto__tr_t2016, observed).
narrative_ontology:measurement(wto__tr_t2023, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2023, 0.22).
narrative_ontology:measurement_basis(wto__tr_t2023, observed).
narrative_ontology:measurement(wto__tr_t2026, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(wto__tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement_basis(wto__be_t1995, observed).
narrative_ontology:measurement(wto__be_t2002, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2002, 0.24).
narrative_ontology:measurement_basis(wto__be_t2002, observed).
narrative_ontology:measurement(wto__be_t2009, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2009, 0.27).
narrative_ontology:measurement_basis(wto__be_t2009, observed).
narrative_ontology:measurement(wto__be_t2016, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2016, 0.29).
narrative_ontology:measurement_basis(wto__be_t2016, observed).
narrative_ontology:measurement(wto__be_t2023, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2023, 0.31).
narrative_ontology:measurement_basis(wto__be_t2023, observed).
narrative_ontology:measurement(wto__be_t2026, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2026, 0.31).
narrative_ontology:measurement_basis(wto__be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 1995, 0.08).
narrative_ontology:measurement_basis(wto__su_t1995, observed).
narrative_ontology:measurement(wto__su_t2002, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2002, 0.1).
narrative_ontology:measurement_basis(wto__su_t2002, observed).
narrative_ontology:measurement(wto__su_t2009, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2009, 0.14).
narrative_ontology:measurement_basis(wto__su_t2009, observed).
narrative_ontology:measurement(wto__su_t2016, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2016, 0.16).
narrative_ontology:measurement_basis(wto__su_t2016, observed).
narrative_ontology:measurement(wto__su_t2023, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2023, 0.18).
narrative_ontology:measurement_basis(wto__su_t2023, observed).
narrative_ontology:measurement(wto__su_t2026, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2026, 0.18).
narrative_ontology:measurement_basis(wto__su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__advisory_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__advisory_coordination_reading, 0.08).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% The WTO DSB authority kernel decomposes into three constraints, each instantiating a different reading of the same authoritative kernel (the Dispute Settlement Understanding). advisory_coordination_reading (this constraint) treats DSB panels as expert advisors providing reasoned negotiation input while member states retain policy discretion; binding_referee_reading treats panels as issuing binding legal determinations grounded in treaty law; judicial_activism_reading treats panel authority as illegitimate institutional overreach. Each reading has different ε (low for advisory, high for binding), different victim/beneficiary structure (advisory: respondent states are soft-payers; binding: respondent states are hard-payers; activist: panels are illegitimate extractors), and different cs_structure axioms (advisory: states_retain_ultimate_discretion; binding: treaty_supremacy_binds_policy; activist: no_legitimate_supranational_authority). The three readings coexist as live positions held by different member state coalitions and different legal schools. This constraint affects binding_referee_reading through soft drift (normalization of compliance erodes the advisory reading's empirical foundation) and influences judicial_activism_reading (if panels extend authority beyond treaty text, activist critique gains purchase against both advisory and binding readings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
