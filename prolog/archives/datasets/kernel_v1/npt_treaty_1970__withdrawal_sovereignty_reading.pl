% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right as Sovereignty Exercise (1970)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   Article X of the Nuclear Non-Proliferation Treaty (1970) grants all state
 *   parties the right to withdraw 'if it decides that extraordinary events,
 *   related to the subject matter of this Treaty, have jeopardized the
 *   supreme interests of its country.' This constraint instantiates the
 *   'withdrawal_sovereignty_reading'—the interpretation that the withdrawal
 *   right is a legitimate exercise of state sovereignty and that treaty
 *   obligations are contingent on the security environment remaining stable.
 *   This reading directly contests the 'oligopoly_enforcement_reading' (which
 *   treats withdrawal rights as enforcement mechanism sustaining NWS
 *   asymmetry) and the 'reciprocal_disarmament_reading' (which frames
 *   withdrawal threats as triggers for collective disarmament acceleration).
 *   The sovereignty reading transforms the NPT from a binding permanent
 *   commitment into a conditioned, revocable contract—states retain the right
 *   to exit if 'extraordinary events' make continued participation costly.
 *   This produces a structural shift: regime stability itself enters the
 *   victim set (credible exit threats undermine compliance incentives),
 *   threshold states gain option value from withdrawal-threat credibility,
 *   and the treaty's binding force becomes dependent on security-environment
 *   exogeneity rather than on legal form. The extractiveness trajectory shows
 *   steady accumulation (0.35 → 0.58) as withdrawal threats become more
 *   credible (Iran post-2002, North Korea post-2006, Ukraine post-2014
 *   context). Theater ratio similarly rises (0.42 → 0.68) as the mechanism's
 *   ability to coordinate expectations via the withdrawal right itself
 *   diminishes and is replaced by security-environment determination of
 *   actual exit probability.
 *
 * KEY AGENTS:
 *   - Threshold states (South Korea, Egypt, Iran, Argentina, Brazil): Primary beneficiaries (powerful/arbitrage) — gain option value and bargaining leverage from withdrawal-threat credibility; extract side benefits (security guarantees, technology transfer) by credibly threatening exit
 *   - Regime stability norm: Primary victim (powerless/trapped) — cannot organize or exit; bears full cost of each withdrawal threat erosion; no coordination benefit accrues to regime stability from Article X
 *   - Nonnuclear weapon states (non-threshold): Secondary victims (moderate/constrained) — compliance incentives undermined by credible exit threats from threshold states; foreswore weapons in exchange for permanent binding commitments that are now contingent
 *   - Nuclear weapon states: Bifurcated agent (institutional/constrained) — quasi-beneficiary (arsenal stabilization via NNWS restraint) and quasi-victim (disarmament obligations + obligation to respect treaty permanence); cannot easily exit but can reinterpret obligations
 *   - Treaty review process coalition: Organized agent (organized/mobile) — attempts to rebalance constraint via review conferences; frames Article X as contingent on NWS disarmament progress (scaffold logic with implicit sunset)
 *   - Treaty text mechanism: Institutional degradation (institutional/arbitrage) — formal right persists as ceremonial; actual exit probability determined by security environment rather than treaty structure; piton classification reflects dormancy and high theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.58).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.62).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right as Sovereignty Exercise (1970)").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, 'npt-sovereignty-reading-v1').
narrative_ontology:cs_kernel_codification('npt-sovereignty-reading-v1', formalized).
narrative_ontology:cs_authority_grounding('npt-sovereignty-reading-v1', lineage).
narrative_ontology:cs_interpretation_layer_present('npt-sovereignty-reading-v1').
narrative_ontology:cs_reading_relation('npt-sovereignty-reading-v1', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('npt-sovereignty-reading-v1', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('npt-sovereignty-reading-v1', foundational, withdrawal_right_as_inalienable_sovereignty).
narrative_ontology:cs_axiom_status(withdrawal_right_as_inalienable_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('npt-sovereignty-reading-v1', withdrawal_right_as_inalienable_sovereignty, deontological).
narrative_ontology:cs_axiom('npt-sovereignty-reading-v1', secondary, extraordinary_events_standard_as_flexible).
narrative_ontology:cs_axiom_status(extraordinary_events_standard_as_flexible, holdable).
narrative_ontology:cs_axiom_grounding('npt-sovereignty-reading-v1', extraordinary_events_standard_as_flexible, empirically_contingent).
narrative_ontology:cs_reference_frame('npt-sovereignty-reading-v1', permanent_binding_commitment_framework).
narrative_ontology:cs_drift_state('npt-sovereignty-reading-v1', post_cold_war_security_fragmentation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('npt-sovereignty-reading-v1', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, withdrawal_threat_exercisers).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, nonnuclear_weapon_states_compliance_incentive).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, collective_security_architecture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIME STABILITY NORM (SNARE) — The NPT's legitimacy depends on perceived permanence and mutual binding force. Each withdrawal threat (North Korea 2002–2006, Iran threats post-JCPOA collapse) erodes the norm's credibility globally. The norm cannot exit this constraint; it bears the full cost of each defection attempt. No coordination benefit accrues to regime stability from Article X's existence — the article's purpose is exclusively the sovereignty right, which directly undermines regime binding force.
constraint_indexing:constraint_classification(npt_treaty_1970__withdrawal_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NONNUCLEAR WEAPON STATES (SNARE) — NNWS ratified the NPT in exchange for binding assurance that NWS would pursue disarmament and that access to peaceful nuclear technology would not be blocked. Article X withdrawal threats—especially by threshold states positioned to weaponize—directly undermine NNWS compliance incentives. Why foreswear weapons if threshold states retain exit rights? The constraint extracts compliance compliance from NNWS through the threat that their restraint may be unilaterally voided by others. Exit cost is extremely high (withdrawal violates international law, triggers sanctions, isolates state) but not impossible; thus 'constrained' rather than 'trapped.' Extraction is severe because the legitimacy of their own restraint is contingent on the permanence they believed they were purchasing.
constraint_indexing:constraint_classification(npt_treaty_1970__withdrawal_sovereignty_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THRESHOLD STATES (ROPE) — From the threshold state perspective, Article X is pure coordination: it enables communication of intent ('we retain the right to withdraw if security environment changes') without requiring actual withdrawal. The threat itself is the coordination function—it signals resolve, creates bargaining leverage, and enables negotiation of side benefits (security guarantees, technology transfer, sanctions relief) in exchange for forgoing the right. For states like South Korea, Egypt, and historically Iran pre-JCPOA, the withdrawal threat was their primary bargaining asset. No extraction flows to this agent; the constraint is experienced as empowerment. Exit options are high ('arbitrage')—they can withdraw, renegotiate, or remain conditionally. Net benefit accrues to this agent.
constraint_indexing:constraint_classification(npt_treaty_1970__withdrawal_sovereignty_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NUCLEAR WEAPON STATES (TANGLED ROPE) — NWS experience the constraint as simultaneously coordination and extraction. The coordination function: Article X's permanence (coupled with disarmament obligations) stabilizes their own arsenals against preemptive pressure from NNWS. The extraction function: NWS benefit from the credible threat of NNWS restraint, which Article X enables through exit-threat signaling. NWS are quasi-beneficiaries (stability) and quasi-victims (obligations) at once. They cannot easily exit the treaty (withdrawal would destroy their own legitimacy as responsible stewards) but can reinterpret or depreciate their disarmament obligations. Extractiveness is moderate-high because they hold both the norm-maintenance burden and the asymmetric arsenal.
constraint_indexing:constraint_classification(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REVIEW PROCESS COALITION (SCAFFOLD) — Treaty review conferences (every 5 years) represent an organized attempt to rebalance the constraint. The coalition (NNWS, NGOs, activist states) frames Article X not as permanent but as contingent on NWS disarmament progress. The scaffold logic: if disarmament obligations are met, withdrawal threats become less credible and regime stability is strengthened. If disarmament stalls, the sunset implicit in Article X becomes operative—the treaty loses legitimacy and states exit, forcing renegotiation. The constraint is experienced as temporary precisely because its permanence depends on unmet NWS commitments. The coalition has agency (they convene reviews, propose modifications) and sees an exit path (collective renegotiation or treaty dissolution if NWS fail to disarm).
constraint_indexing:constraint_classification(npt_treaty_1970__withdrawal_sovereignty_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TREATY TEXT MECHANISM (PITON) — Article X exists as written in 1970, codified and referenced, but its actual coordination function has atrophied. Withdrawal threats are now credible only when coupled with independent security crises (Iran hostage crisis, North Korean isolation, Russian invasion of Crimea). Article X alone cannot generate credible exit threats; the real driver is external security environment. The treaty text persists as ceremonial—withdrawal-right discourse continues, but the mechanism's power to coordinate expectations has degraded into theater. Legal scholars and diplomats continue to discuss the 'right' to withdraw while knowing that exercise of the right is determined by factors orthogonal to the treaty. Theater ratio is high (0.68) because the formal structure (the right exists, is permanent, is sacrosanct) is maintained while its material function (signaling resolve, enabling exit) is performed by security-environment exogeneity instead.
constraint_indexing:constraint_classification(npt_treaty_1970__withdrawal_sovereignty_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the right of exit from binding treaties is a fundamental principle of state sovereignty itself—a natural law of international law that cannot be overridden by any treaty text. States cannot voluntarily bind themselves in perpetuity without violating their own sovereignty (reductio: enslaving oneself is void). Therefore, Article X is not a 'right' the treaty grants; it is the consequence of an immutable feature of sovereignty. This perspective sees the constraint as mountain—inevitable, unchangeable, following from the nature of state relations. However, the structural data (multiple beneficiaries, clear victims, active enforcement requirements) contradicts the mountain classification. The engine will detect this as a false summit: the naturalizing frame ('sovereignty is immutable') obscures the constructed institutional arrangement ('exit threat credibility is an engineered feature of the regime').
constraint_indexing:constraint_classification(npt_treaty_1970__withdrawal_sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(npt_treaty_1970__withdrawal_sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(npt_treaty_1970__withdrawal_sovereignty_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, TR),
    TR >= 0.70.

:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The withdrawal right structure creates measurable extraction: threshold states gain bargaining leverage (extracting security guarantees, technology, sanctions relief) by credibly signaling exit intent; NNWS lose compliance incentive (their restraint becomes contingent on others' compliance); regime stability bears the full cost of eroded credibility. The rise from 0.35 to 0.58 reflects increasing credibility of withdrawal threats as security crises (Iranian Revolution, Korean isolation, post-Cold War asymmetry) make exit rational for threshold states. The value is not higher (not 0.70+) because much of the extraction is legitimate sovereignty rent—threshold states genuinely have heightened security concerns, and exit rights appropriately account for this. Suppression (0.62): Moderate-high. NNWS face significant barriers to exit (international stigma, sanctions, technology denial) but withdrawal is not impossible; threshold states face lower barriers (security justification is credible). Theater ratio (0.68): Moderately high. The formal withdrawal right is invoked rhetorically and legally, but actual withdrawal probability is determined by independent security events (hostage crises, missile tests, regional wars) rather than by the treaty structure itself. Legal scholars debate whether North Korea's 2003 withdrawal was justified under Article X extraordinary events language; the answer is empirically determined by reference to external security facts, not by the treaty text. This creates the theater dynamic: the mechanism (the right, the language, the review process) is maintained ceremonially while its actual coordination function is performed by security-environment exogeneity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates full perspectival divergence. The threshold state perspective (rope) sees pure sovereignty exercise and bargaining empowerment. The NNWS perspective (snare) sees their own compliance incentives eroded. The regime stability perspective (snare) sees an existential threat to binding force. The NWS perspective (tangled_rope) sees necessary asymmetry protection coupled with awkward disarmament obligations. The review process coalition (scaffold) sees a temporary problem solvable through disarmament acceleration. The treaty text mechanism (piton) sees ceremonial form decoupled from security-determined function. The analytical observer (mountain) risks naturalizing contingent institutional arrangements as immutable features of state sovereignty. The gap reveals that Article X is not a simple allocation of rights and duties but a site of fundamental contestation over what 'binding' means: Does it mean permanent (NWS preferred), or does it mean 'binding conditional on extraordinary-events parameters' (sovereignty reading)? Does it mean 'reciprocal disarmament obligation enforces binding force' (reciprocal reading)? Or does it mean 'NWS maintain permanent asymmetry via controlled exit threat' (oligopoly reading)? Each reading produces a different effective constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary threshold states: high power (powerful), high exit options (arbitrage). The derivation chain yields low d → low/negative effective extraction chi. These agents experience the constraint as empowerment. Victim regime stability: zero agency (powerless), zero exit (trapped). Derives d ≈ 1.0 → maximum f(d) → maximum experienced extraction chi. The norm's victim status is structural and irreversible within the current treaty frame. Victim NNWS (non-threshold): moderate power, constrained exit (high cost but possible). Derives moderate d → moderate f(d) → moderate experienced extraction chi. The directionality reflects that their compliance incentives are undermined but not eliminated; they retain some agency (can still threaten withdrawal, can renegotiate, can pursue dual-use civilian programs). NWS bifurcation: institutional power, constrained exit (cannot withdraw without destroying legitimacy). The derivation produces moderate-to-high d reflecting their dual status as quasi-beneficiary and quasi-victim. The scope modifier σ(S) is global (1.2 × baseline), amplifying effective extraction because the verification burden and institutional surveillance requirements are planetwide.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING INSTANTIATION: The mandatrophy is resolved by recognizing that this is one reading of a contested kernel, not an error in classification. The NPT text simultaneously supports three structurally coherent readings with different extraction mechanics. The sovereignty reading (this story) produces tangled_rope because genuine coordination exists (states signal intent, negotiate side benefits, stabilize expectations through withdrawal-threat communication) alongside asymmetric extraction (threshold states extract option value; regime stability bears the cost). The coordination function is real: without Article X, the entire NPT would collapse into a pure snare (NNWS permanently foreswearing weapons while NWS retain them indefinitely). Article X enables the negotiation that produced the treaty in the first place. But the extraction is also real: the withdrawal-right credibility directly undermines NNWS compliance incentives by making their restraint contingent on threshold-state forbearance. The engine's classification as tangled_rope is correct for this reading. If the oligopoly or reciprocal readings were instantiated in separate files, they would show different extraction mechanics and different victim sets—but all three would remain tangled_rope or snare-range constraints, not mountains. The sovereignty reading is not 'the' right interpretation; it is one reading among coexisting alternatives, differentiated by axiom (withdrawal_right_as_sovereignty vs. withdrawal_threat_as_enforcement_mechanism vs. withdrawal_threat_as_disarmament_trigger).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_environment_exogeneity,
    'Is Article X withdrawal credibility determined by the treaty structure itself, or by independent security environment shocks that make withdrawal rational regardless of the treaty?',
    'Historical analysis of withdrawal threats: which were preceded by treaty violations vs independent security crises (Iran: hostage crisis; North Korea: isolation + missile tests; Ukraine: post-Soviet security anxiety). Counterfactual: would withdrawal threats be made if security environment were stable?',
    'If exogenous security drives threats: Article X is piton (mechanism dormant, theater high). If treaty structure drives threats: Article X is tangled_rope (mechanism active, extraction genuine). The classification hinges on the causal mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_environment_exogeneity, empirical, 'Whether withdrawal threats are driven by treaty structure or security environment').

omega_variable(
    regime_stability_as_victim,
    'Does the existence of Article X as a withdrawal-right mechanism actually destabilize the regime in practice, or is the destabilization only potential and largely theoretical?',
    'Empirical audit: count withdrawal threats (actual, threatened, implied) over time; correlate with regime weakness indicators (NNWS non-compliance, new weapons programs, defections). Measure: does each withdrawal threat measurably reduce NNWS compliance incentive (measured via new enrichment starts, reprocessing facility construction, NPT withdrawal bills introduced in legislatures)?',
    'If measured destabilization is significant (>3 NNWS new programs per withdrawal threat): regime_stability is genuinely victimized (snare classification confirmed). If destabilization is minimal or theoretical: the victim status of regime stability is aspirational, and the constraint is more rope than snare from the global/organized perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_stability_as_victim, empirical, 'Whether regime destabilization from Article X is empirical or theoretical').

omega_variable(
    disarmament_obligation_status,
    'Are NWS disarmament obligations (Article VI) binding commitments, or unenforceable political commitments?',
    'Legal analysis: case law from ICJ, UNSC, and treaty interpretation jurisprudence on Article VI enforceability. Empirical: has any NNWS ever invoked Article VI breach to justify withdrawal (serious legal threat), or only as rhetorical complaint?',
    'If Article VI is binding: scaffold''s review-process logic is structural (disarmament progress measurably triggers regime stability). If Article VI is unenforceable: scaffold is aspirational theater, and the constraint remains tangled_rope or snare from NNWS perspective (no credible disarmament exit ramp exists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disarmament_obligation_status, empirical, 'Enforceability status of NWS disarmament obligations').

omega_variable(
    kernel_reading_identity,
    'This constraint instantiates the ''withdrawal_sovereignty_reading'' of the NPT kernel. Sibling readings (oligopoly_enforcement_reading, reciprocal_disarmament_reading) produce different victim sets and extraction mechanisms. Does the NPT framework accommodate all three readings simultaneously, or does one reading logically foreclose the others?',
    'Constitutional interpretation: can a treaty text legitimately support readings where (a) withdrawal right is sovereignty-protecting (this reading), (b) withdrawal right is oligopoly enforcement mechanism (NWS cartel maintaining asymmetry), and (c) withdrawal threat is trigger for collective disarmament acceleration (reciprocal reading)? Or does commitment to one reading require rejecting another?',
    'If all three readings coexist: the NPT is a distributed kernel with no single authoritative interpretation (affects cs_structure.authority_grounding). If one reading forecloses another: the relationship shifts from coexists_with to forecloses in reading_relations. The reading_relations entries in cs_structure encode the actual logical structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Logical relationship between withdrawal_sovereignty and sibling NPT readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 1970, 1995).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_withdrawal_tr_t0, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(npt_withdrawal_tr_t10, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(npt_withdrawal_tr_t25, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 25, 0.68).

% Extraction over time
narrative_ontology:measurement(npt_withdrawal_be_t0, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(npt_withdrawal_be_t10, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(npt_withdrawal_be_t25, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt_withdrawal_su_t0, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(npt_withdrawal_su_t10, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(npt_withdrawal_su_t25, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, iran_jcpoa_withdrawal_credibility).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, north_korea_npt_exit_2003).

% DUAL FORMULATION NOTE:
% The NPT kernel admits at least three structurally distinct constraint readings: (1) withdrawal_sovereignty_reading (this file) — extractiveness 0.58, tangled_rope, sovereignty exercise frame; (2) oligopoly_enforcement_reading — likely extractiveness 0.45-0.55, tangled_rope, NWS cartel frame; (3) reciprocal_disarmament_reading — likely extractiveness 0.35-0.45, rope or scaffold, disarmament-trigger frame. Each reading has different beneficiaries (threshold states vs NWS vs NNWS coalition) and different victims. The three stories form a constraint family linked by kernel identity, not by empirical decomposition per ε-invariance principle. They are readings of the same text, producing different classifications from different interpretive frames.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__withdrawal_sovereignty_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
