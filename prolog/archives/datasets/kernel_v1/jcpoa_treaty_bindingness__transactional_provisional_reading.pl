% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__transactional_provisional_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA Treaty Bindingness: Transactional-Provisional Reading (Unilateral Withdrawal)
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   The Joint Comprehensive Plan of Action (JCPOA), signed in 2015 by Iran,
 *   the P5+1 states (US, UK, France, Russia, China) plus Germany and the EU,
 *   represents a contested international legal structure whose bindingness
 *   pivots on how its signatories interpret the treaty's termination clauses
 *   and dispute resolution mechanisms. This constraint models ONE specific
 *   reading: the transactional-provisional interpretation, which treats the
 *   JCPOA as a conditional, reciprocal arrangement that either party can exit
 *   unilaterally if they determine the other has acted in bad faith. Under
 *   this reading, the agreement is not a permanent multilateral institution
 *   but a transactional coordination mechanism with built-in exit ramps. This
 *   reading emphasizes the explicit language of Article 36 (dispute
 *   resolution) and the snapback clauses (automatic sanctions reinstatement),
 *   interpreting them as enabling unilateral withdrawal rather than as
 *   procedural constraints on withdrawal. The transactional reading creates
 *   specific structural consequences: withdrawing states face lower
 *   diplomatic costs (withdrawal is treaty-lawful), non-withdrawing
 *   signatories face extraction without proportional agency, and Iran faces
 *   the Snare of having invested compliance that can be unilaterally
 *   nullified. The measurements track how theater and suppression increased
 *   from 2015 to 2020, as the dispute resolution machinery became
 *   increasingly performative (states engaged in procedural compliance while
 *   pursuing contradictory substantive goals) and suppression mechanisms
 *   (sanctions reimposition, secondary sanctions threats, compliance
 *   inspection intensification) intensified in response to interpretive
 *   disputes.
 *
 * KEY AGENTS:
 *   - United States (+ Israel as strategic partner): Primary withdrawing state (powerful/arbitrage) — benefits from unilateral exit option, can reimpose sanctions to coerce other signatories and isolate Iran
 *   - European Signatories (France, UK, Germany, EU): Non-withdrawing institutional actors (organized/mobile) — face extraction through secondary sanctions pressure but have exit alternatives (INSTEX, blocking regulations)
 *   - Iran: Compliance investor and victim (moderate/constrained) — made verifiable nuclear concessions in exchange for sanctions relief; withdrawal nullifies investment without proportional compensation
 *   - Russia and China: Institutional signatories (organized/constrained) — experience the constraint as coordination with extraction; maintain the agreement partly due to geopolitical alignment with Iran
 *   - IAEA and NPT institutions: Enforcement architects (institutional/constrained) — maintain verification machinery despite reduced enforcement authority when great powers diverge
 *   - Domestic opposition coalitions (US hawks, Israeli security establishment, Iranian hard-liners): Organized opponents (organized/constrained) — benefit from transactional reading's validation of unilateral exit
 *   - Analytical observer (treaty law perspective): Civilizational context (analytical/analytical) — risks naturalizing the contested reading as inherent to treaty law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.58).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.62).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA Treaty Bindingness: Transactional-Provisional Reading (Unilateral Withdrawal)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, 'ebeb848e-5d91-4fd1-8840-fac41c172eb4').
narrative_ontology:cs_kernel_codification('ebeb848e-5d91-4fd1-8840-fac41c172eb4', fixed_text).
narrative_ontology:cs_authority_grounding('ebeb848e-5d91-4fd1-8840-fac41c172eb4', extraction).
narrative_ontology:cs_interpretation_layer_present('ebeb848e-5d91-4fd1-8840-fac41c172eb4').
narrative_ontology:cs_reading_relation('ebeb848e-5d91-4fd1-8840-fac41c172eb4', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('ebeb848e-5d91-4fd1-8840-fac41c172eb4', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('ebeb848e-5d91-4fd1-8840-fac41c172eb4', foundational, unilateral_exit_treaty_lawful_under_bad_faith_determination).
narrative_ontology:cs_axiom_status(unilateral_exit_treaty_lawful_under_bad_faith_determination, holdable).
narrative_ontology:cs_axiom_grounding('ebeb848e-5d91-4fd1-8840-fac41c172eb4', unilateral_exit_treaty_lawful_under_bad_faith_determination, conventional).
narrative_ontology:cs_axiom('ebeb848e-5d91-4fd1-8840-fac41c172eb4', foundational, self_judging_bad_faith_determination).
narrative_ontology:cs_axiom_status(self_judging_bad_faith_determination, holdable).
narrative_ontology:cs_axiom_grounding('ebeb848e-5d91-4fd1-8840-fac41c172eb4', self_judging_bad_faith_determination, deontological).
narrative_ontology:cs_reference_frame('ebeb848e-5d91-4fd1-8840-fac41c172eb4', reciprocal_treaty_agreement_with_exit_clauses).
narrative_ontology:cs_drift_state('ebeb848e-5d91-4fd1-8840-fac41c172eb4', unilateral_withdrawal_and_snapback_dispute_2018_2020, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ebeb848e-5d91-4fd1-8840-fac41c172eb4', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, withdrawing_state_coalition).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_opposition_to_deal).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_agreement_stability).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, non_withdrawing_signatories).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_compliance_investments).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MULTILATERAL AGREEMENT STABILITY (SNARE) — Non-withdrawing signatories face extraction without exit. Once a single powerful state unilaterally withdraws and reimpose sanctions, other signatories cannot maintain the deal's economic benefits for Iran. Trapped in the consequences of another's exit decision. Maximum extraction, no agency.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__transactional_provisional_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IRAN — COMPLIANCE INVESTOR (SNARE) — Made verifiable concessions (centrifuge removal, enrichment limits, inspections) in exchange for sanctions relief. Unilateral withdrawal by a single signatory nullifies the deal's value without option to re-escalate immediately (constrained by international law norms on nuclear escalation). Significant extraction: invested compliance with no guarantee of preserved benefit.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__transactional_provisional_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WITHDRAWING STATE COALITION (TANGLED ROPE) — Powerful states have both exit option (unilateral withdrawal clause) and a coordination function (the deal does coordinate sanctions relief for non-signatories and nuclear oversight). But they also extract: they can reimpose sanctions unilaterally, using their market access and dollar-system gatekeeping power to coerce other signatories. Arbitrage option allows exit at will; beneficiary of the coercive extraction mechanism.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EUROPEAN SIGNATORIES (ROPE) — See the JCPOA as a coordination mechanism: nuclear oversight, sanctions predictability, trade stability. Have mobile exit options (can invoke dispute resolution, issue blocking regulations to shield their firms from secondary sanctions, negotiate INSTEX workarounds). Experience the constraint as coordination with manageable extraction — not beneficiaries, but not powerless either. Low extraction, moderate theater.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__transactional_provisional_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: IAEA AND NON-PROLIFERATION INSTITUTIONS (SCAFFOLD) — See the transactional reading as a temporary architecture. The deal is a coordination mechanism with a built-in sunset: dispute resolution clauses, snapback triggers, and the explicit transactional framing mean the agreement is contingent on continued reciprocal compliance. If Iran violates or a signatory withdraws invoking bad faith, the entire framework collapses. Theater is moderate — the institutional machinery (inspections, reports) remains functional even during dispute. But the constraint is fundamentally temporary.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__transactional_provisional_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: UN SECURITY COUNCIL ENFORCEMENT (PITON) — The JCPOA was designed to supersede and replace previous Security Council resolutions through multilateral consensus. Under the transactional reading, this architecture is largely theatrical: enforcement depends on great-power alignment, and unilateral withdrawal mechanisms bypass consensus entirely. The institutions persist (IAEA still inspects, Security Council still meets) but lack actual enforcement power when a permanent member's interests diverge. Theater ratio high because the institutional machinery is maintained despite reduced functional enforcement capacity.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__transactional_provisional_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: DOMESTIC OPPOSITION COALITION (TANGLED ROPE) — Groups opposing the deal (some security hawks, sanctions advocates, ideological factions) experience the transactional reading as liberation: it confirms that unilateral exit is available and bad-faith clauses favor withdrawal. They benefit from the reading's framing (their preferred interpretation of the treaty's bindingness is upheld) but are also constrained by international law norms and other signatories' retaliation options. Moderate extraction because exit comes with diplomatic and economic costs; moderate coordination because the coalition does align other states' incentives (some agree withdrawal is justified).
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this reading reflects an inherent property of treaty law itself: all treaties are ultimately voluntary commitments between sovereign states. No supranational authority can enforce them; withdrawal is always technically possible; bindingness is always conditional on continued consent. Under this view, the transactional reading is not a contingent institutional choice but a description of how international law fundamentally works. However, this naturalizes what is actually a contested reading of the JCPOA's specific architecture and text.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__transactional_provisional_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__transactional_provisional_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__transactional_provisional_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, TR),
    TR >= 0.70.

:- end_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The transactional reading enables a withdrawing state to extract significant concessions (verifiable Iranian nuclear steps) while maintaining exit options and the ability to reimpose sanctions unilaterally. The extractiveness is not maximal (0.70+) because other signatories have countermeasures (INSTEX, blocking regulations, diplomatic retaliation), and because Iran's compliance concessions create some baseline coordination benefit. The upward trajectory from 0.35 to 0.58 reflects how the interpretation consolidated as a dominant framing during the 2018-2020 dispute period. Suppression (0.62): Moderate-high. Significant barriers prevent non-withdrawing signatories from maintaining the agreement's benefits for Iran: they lack the market access, financial system integration, and coercive capacity to offset sanctions pressure. But suppression is not total because Europe did create workarounds (INSTEX) and some states maintained trade relations. The trajectory reflects intensifying sanctions pressure and secondary sanctions threats. Theater ratio (0.68): Moderate-high. The dispute resolution machinery became increasingly performative: states engaged in Annex IV compliance procedures, IAEA inspection reports continued, but the underlying disagreement about bindingness was not resolved through these mechanisms. Unilateral withdrawal circumvented dispute resolution entirely, revealing the theater. The increase reflects how procedural compliance detached from substantive implementation over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the transactional reading (unilateral exit is lawful, bad-faith determination is self-judging, withdrawal faces minimal treaty-legal constraints) and the binding reading (treaty creates binding obligations, exit requires multilateral consensus or adjudication, unilateral withdrawal is a breach). From the withdrawing state's perspective, the constraint is manageable (low extraction, clear exit). From the non-withdrawing signatories' perspective, it is extraction (trapped in consequences of another's unilateral choice). From Iran's perspective, it is the Snare (invested compliance with no protection). From the institutional perspective, it is a temporary architecture already breaking down. From the analytical view, it appears as an immutable feature of treaty law — but only if the transactional reading is naturalized as the correct interpretation rather than recognized as a contested choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The transactional reading produces high directionality variation across agents. Withdrawing states with market gatekeeping power (d ≈ 0.15, full beneficiaries with arbitrage options, f(d) ≈ -0.01) experience negative effective extraction — the constraint subsidizes them. Non-withdrawing states without alternative enforcement mechanisms (d ≈ 0.80, victims with constrained options, f(d) ≈ 1.25) experience high effective extraction. Iran as a compliance investor (d ≈ 0.92, full target/victim with trapped exit, f(d) ≈ 1.38) experiences maximum extraction — made concessions with no guarantee of preserved benefit. The European signatories (d ≈ 0.60, mixed position with mobile exit, f(d) ≈ 0.85) experience moderate extraction. The scope modifier (global = 1.2) amplifies these values because the constraint operates through global financial and sanctions systems. The perspectival gap arises entirely from directionality variation: the same constraint is Rope for beneficiaries, Snare for trapped victims, Tangled Rope for mixed-position states, Scaffold for institutions with sunset logic, Piton for enforcement machinery, and naturalized as Mountain from the analytical context.
 *
 * MANDATROPHY ANALYSIS:
 *   The transactional reading resolves the mandatrophy by clarifying that the JCPOA's bindingness is structurally indeterminate at the treaty level — it depends on how signatories interpret Article 36 and the snapback clauses. The reading does NOT claim that unilateral withdrawal is costless (it faces diplomatic/economic retaliation); it claims that withdrawal is treaty-lawful under a plausible textual interpretation. The constraint's classification as Tangled Rope reflects this: genuine coordination (nuclear oversight, sanctions predictability) is embedded alongside asymmetric extraction (beneficiaries can exit unilaterally, victims cannot). The mandatrophy is resolved by noting that the transactional reading is not 'the' correct interpretation but a live position within international law, held by withdrawing states and their allies, opposed by non-withdrawing signatories but not foreclosed by the treaty text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bad_faith_determination_locus,
    'Who has the authority to determine whether a state has acted in bad faith? Is it unilateral (each state decides for itself), multilateral (consensus required), or subject to third-party adjudication?',
    'Textual analysis of JCPOA Article 36 and Annex IV dispute resolution clauses; examination of state practice post-withdrawal; ICJ advisory opinions on treaty termination in bad faith.',
    'If unilateral: withdrawing state has effective veto on agreement continuation (Snare from other parties'' perspective). If multilateral or adjudicated: withdrawal requires external validation (Tangled Rope or Rope). This is the core interpretive crux between the transactional and binding readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bad_faith_determination_locus, conceptual, 'Who determines bad faith in treaty termination').

omega_variable(
    snapback_sanctions_bindingness,
    'Are UN Security Council snapback sanctions (automatic reinstatement of previous resolutions) legally binding on non-withdrawing signatories, or are they a coordination mechanism that can be blocked by Council members?',
    'Analysis of Security Council procedure, P5 veto power, and institutional practice during 2018-2020 snapback dispute.',
    'If truly automatic: transactional reading is correct — unilateral withdrawal triggers automatic consequences. If blockable: snapback is contingent on consensus, strengthening the binding reading (withdrawal has reputational/institutional costs but not automatic legal consequences).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(snapback_sanctions_bindingness, empirical, 'Whether snapback sanctions are automatic or consensus-dependent').

omega_variable(
    iranian_compliance_status_ambiguity,
    'Did Iranian compliance with JCPOA nuclear restrictions remain verifiable and substantial throughout the period, or were there material violations that justified invoking bad-faith clauses?',
    'IAEA technical reports and inspector assessments; independent forensic analysis of uranium enrichment and centrifuge operations; timeline correlation with claimed violations.',
    'If Iran maintained compliance: withdrawal invoked bad faith falsely, strengthening the binding reading and Snare classification for Iran. If Iran violated: withdrawal is justified under the transactional reading, shifting Iran to constrained (they violated first) rather than trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iranian_compliance_status_ambiguity, empirical, 'Iranian compliance with JCPOA nuclear restrictions').

omega_variable(
    reading_kernel_foreclosure,
    'Does the transactional-provisional reading logically foreclose the binding-multilateral reading, or can both be held simultaneously by different legal frameworks (treaty textualism vs. pacta sunt servanda doctrine)?',
    'Comparative constitutional law analysis of how different states incorporate international treaty law; examination of Vienna Convention on the Law of Treaties articles 60-65 (material breach and suspension/termination) vs. articles on good faith interpretation.',
    'If foreclosed: only one reading can be legally coherent (one framework wins). If coexistent: both readings are live options within different interpretive traditions, and the contest is genuinely unresolvable by law alone (political/structural resolution required).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Whether readings logically foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_txn_theater_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(jcpoa_txn_theater_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 3, 0.55).
narrative_ontology:measurement(jcpoa_txn_theater_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(jcpoa_txn_extractiveness_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jcpoa_txn_extractiveness_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(jcpoa_txn_extractiveness_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_txn_suppression_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jcpoa_txn_suppression_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 3, 0.54).
narrative_ontology:measurement(jcpoa_txn_suppression_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, iran_nuclear_sanctions_secondary_enforcement).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, european_sanctions_blocking_regulations).

% DUAL FORMULATION NOTE:
% The JCPOA bindingness kernel decomposes into three structurally distinct constraint stories: (1) transactional-provisional (this story, ε=0.58) — unilateral exit lawful, bad faith self-judged, high extraction; (2) binding-multilateral (ε=0.32) — exit requires consensus, dispute resolution mandatory, coordination mechanism preserved; (3) graduated-compliance (ε=0.45) — violations partial, remediation available, intermediate enforcement. Each story has different ε values because they treat the observable (treaty text, practice history, institutional machinery) differently. The ε differences reflect genuine interpretive disputes, not measurement ambiguity. All three readings are live in international law discourse; none is foreclosed by the text alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__transactional_provisional_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
