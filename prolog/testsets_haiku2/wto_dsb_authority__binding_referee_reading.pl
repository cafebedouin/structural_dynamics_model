% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO Dispute Settlement Body Authority (Binding Referee Reading)
 *   domain: international_law/trade_governance
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body (DSB) issues rulings that member states
 *   are obligated to comply with or face authorized retaliation. This is the
 *   binding-referee reading of the contested WTO DSB authority kernel. Under
 *   this reading, member states have surrendered policy discretion within
 *   WTO-covered domains (trade in goods, services, intellectual property,
 *   investment) in exchange for market-access commitments and binding dispute
 *   resolution. Non-compliance with DSB rulings is a treaty violation, not a
 *   policy choice. This reading emphasizes institutional stability, the rule
 *   of law in trade, and the credible commitment problem solved by a binding
 *   referee. It is contested by two sibling readings: the
 *   advisory-coordination reading (which holds that panels provide expert
 *   opinions but states retain final policy choice) and the judicial-activism
 *   reading (which holds that panels have exceeded their mandate and created
 *   obligations not in the founding treaties). The binding-referee reading is
 *   the de facto operational reading of the WTO system as currently
 *   practiced, though its legitimacy is heavily contested by scholars and
 *   some member governments, particularly regarding interpretive drift in
 *   recent decades.
 *
 * KEY AGENTS:
 *   - WTO Secretariat and Panel System: institutional agenda-setter, administers dispute resolution and interprets WTO agreements
 *   - Major Trading Economies (US, EU, China): powerful beneficiaries, have litigation advantage and can absorb retaliation costs
 *   - Smaller Trading Economies: constrained payers, lack litigation resources and bear compliance costs they had limited voice in shaping
 *   - Domestic Constituencies: powerless payers with identity-locked exit, bear policy constraints they cannot appeal through their legislatures
 *   - WTO Member Governments: collectively set DSB mandate but operationally constrained by unanimity and asymmetric win/loss distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.68).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.72).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO Dispute Settlement Body Authority (Binding Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, 'd2bb6e6e-20a3-4126-a00b-d02840e87385').
narrative_ontology:cs_kernel_codification('d2bb6e6e-20a3-4126-a00b-d02840e87385', fixed_text).
narrative_ontology:cs_authority_grounding('d2bb6e6e-20a3-4126-a00b-d02840e87385', lineage).
narrative_ontology:cs_interpretation_layer_present('d2bb6e6e-20a3-4126-a00b-d02840e87385').
narrative_ontology:cs_reading_relation('d2bb6e6e-20a3-4126-a00b-d02840e87385', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('d2bb6e6e-20a3-4126-a00b-d02840e87385', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('d2bb6e6e-20a3-4126-a00b-d02840e87385', foundational, member_states_surrendered_policy_discretion).
narrative_ontology:cs_axiom_status(member_states_surrendered_policy_discretion, holdable).
narrative_ontology:cs_axiom_grounding('d2bb6e6e-20a3-4126-a00b-d02840e87385', member_states_surrendered_policy_discretion, conventional).
narrative_ontology:cs_axiom('d2bb6e6e-20a3-4126-a00b-d02840e87385', foundational, dsb_rulings_are_binding_not_advisory).
narrative_ontology:cs_axiom_status(dsb_rulings_are_binding_not_advisory, holdable).
narrative_ontology:cs_axiom_grounding('d2bb6e6e-20a3-4126-a00b-d02840e87385', dsb_rulings_are_binding_not_advisory, empirically_contingent).
narrative_ontology:cs_reference_frame('d2bb6e6e-20a3-4126-a00b-d02840e87385', treaty_mandated_binding_dispute_resolution).
narrative_ontology:cs_drift_state('d2bb6e6e-20a3-4126-a00b-d02840e87385', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d2bb6e6e-20a3-4126-a00b-d02840e87385', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, major_trading_economies).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_secretariat_and_panel_system).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, smaller_trading_economies).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_policy_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_member_governments).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, major_trading_economies).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_constituencies).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, wto_member_governments).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, treaty_supremacy_doctrine).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, rule_of_law_in_trade).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the DSB, appoints panelists, interprets WTO agreements in rulings that member states must comply with or face authorized retaliation. Controls the interpretive process and dispute resolution calendar. Cannot exit the system without dissolution; has institutional incentive to maintain authority through consistent panel-strengthening jurisprudence.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_secretariat_and_panel_system, agenda_setter,
    institutional, generational, trapped, global).

% Gain market access commitments and binding dispute resolution when others breach. Can afford to litigate extensively, absorb retaliation costs, and invoke DSB rulings selectively. Pay compliance costs when they lose cases, but asymmetric litigation advantage means they win on balance. Can negotiate bilateral waivers or withdrawal threats give them negotiating leverage.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, major_trading_economies, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, major_trading_economies, payer).

% Bound by the same DSB rulings but lack litigation resources to defend policy space or mount their own cases. Face compliance costs and retaliation threats; cannot afford extended panel procedures or expert economists for factual disputes. Policy space is constrained by DSB rulings they had limited voice in shaping.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, smaller_trading_economies, payer,
    moderate, biographical, constrained, global).

% Bear the costs of policy constraints imposed by DSB rulings: public health programs curtailed by intellectual-property enforcement, labor protections constrained by market-access commitments, environmental regulations invalidated as non-tariff barriers. Cannot appeal to their own legislatures once their government has accepted the DSB ruling as binding. Their policy preferences are locked out by the prior sovereignty trade-off.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_constituencies, payer,
    powerless, biographical, identity_locked, national).

% Collectively set the DSB's mandate and can theoretically modify or dissolve it; operationally constrained by unanimity requirement and winner/loser asymmetries. Governments benefit from market access for their export sectors but pay via constrained policy autonomy. Larger economies capture more benefit; smaller ones bear disproportionate costs.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_member_governments, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, wto_member_governments, payer).

% Regional trade agreements and other dispute-resolution mechanisms (investment arbitration, bilateral agreements) exist in parallel but are structurally subordinated to WTO law where treaties overlap. Cannot compete with DSB authority; their rulings defer to WTO interpretations. Excluded from the consensus process that shapes DSB jurisprudence.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, non_wto_governance_regimes, excluded,
    institutional, generational, trapped, global).

% Observe DSB rulings and compliance patterns to assess accession costs. Currently outside the constraint; their position depends on whether they join. Can choose not to join or negotiate exceptional carve-outs, but accession pressures and market-access incentives drive participation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_accession_candidates, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__binding_referee_reading, wto_secretariat_and_panel_system).
narrative_ontology:fixing_cost_class(wto_dsb_authority__binding_referee_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a binding dispute-resolution mechanism to enforce treaty commitments and market-access schedules across 164+ member states without bilateral renegotiation. Solves the problem of treaty breach and the credibility problem of commitment: members know rulings are enforceable via authorized retaliation, which makes compliance incentives real rather than voluntary.
% TRANSFER_FUNCTION: Transfers policy discretion from member states to the DSB panel system. Member states surrender the right to unilaterally interpret WTO agreements in their own territory and accept binding rulings that constrain domestic regulation. The transfer is from policy autonomy (at national/domestic level) to dispute-resolution authority (at supranational level). Secondary transfer: compliance costs flow from non-complying states to complying states via retaliation authorizations and market-access constraints.
% ABSENT_VOICES: Domestic constituencies in member states that lose DSB cases are structurally excluded: they do not participate in panel procedures, cannot appeal to their legislatures to contest the ruling, and have no institutional voice. NGOs, subnational governments, and affected worker/consumer groups observe but do not participate in the dispute process. Non-member states and non-parties to covered agreements have no standing.
% DISAPPEARANCE_RATIONALE: If the DSB's binding authority vanished overnight, member states would revert to bilateral dispute negotiation or regional dispute forums. Market-access commitments would become unenforceable declarations rather than binding law. Governments that previously accepted policy constraints due to DSB compliance would renegotiate or defect. Trade predictability would collapse; protectionist regressions would likely accelerate. The entire architecture of post-1995 trade governance depends on DSB bindingness.
% FOUNDING_PROBLEM: Pre-WTO dispute resolution was bilateral, slow, subject to power asymmetry, and often led to tit-for-tat retaliation spirals. The GATT had no binding dispute mechanism; disagreements festered or escalated to trade wars. The founding problem was credibly committing to trade liberalization despite incentives to backslide: a binding referee that makes breach costly and negotiation mandatory.
% FOUNDING_PROBLEM_CORROBORATION: Member states' own legal arguments and WTO Appellate Body jurisprudence repeatedly invoke the need for predictable dispute resolution. However, the corroboration is internal to the WTO system — the Appellate Body speaks only to defend its own authority. External corroboration from academic trade scholars and non-WTO trade regimes (e.g., USMCA dispute provisions) supports the founding problem as live. However, governments and scholars critical of DSB activism (judicial-activism-reading adherents) contest whether the current DSB rules are solving the founding problem or have drifted into creating new obligations beyond the original design.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 at the interval end (2026) because the constraint transfers policy discretion from member states to the DSB panel system, and this transfer is permanent and binding for any state party to the WTO. The trajectory from 0.42 (1995) to 0.68 (2026) reflects the accumulation of DSB jurisprudence that has clarified and extended the scope of binding obligations beyond the founding treaties' text (this is the judicial-activism-reading contestation documented in omegas). Suppression is high (0.72) because member states must comply or face authorized retaliation; the retaliation authorization makes non-compliance costly and constrains policy alternatives. Theater ratio is moderate (0.28) because the DSB conducts genuine legal analysis and produces written rulings with factual and legal reasoning, but the constraint's core function is enforcing compliance via threat, not educating or persuading. Accessibility collapse is high (0.78) because once a member state joins the WTO, exiting or opting out of DSB rulings is extremely costly (economic retaliation, loss of market access); the constraint is effectively permanent. Resistance is moderate-high (0.61) because member governments have contested DSB authority repeatedly (Appellate Body reform efforts, India's defense of policy space, the US withdrawal of support for the Appellate Body in 2016-2020, alternative dispute mechanisms); this reflects genuine pushback against the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (WTO Secretariat/panel system) experiences the constraint as legitimate dispute resolution and treaty enforcement; they see bindingness as necessary for credibility and compliance. Member governments experience it as a negotiated trade-off: they surrendered discretion in exchange for market access, which they largely accepted as legitimate at accession. However, smaller trading economies increasingly experience the constraint as extractive: they pay compliance costs and bear policy constraints they had limited voice in shaping, while major economies capture the benefits of binding enforcement (e.g., US bringing cases against competitors' regulations). Domestic constituencies in losing-member states experience the constraint as imposed policy, not negotiated coordination; they have no voice in DSB proceedings and cannot appeal to their legislatures. The engine will compute these divergences from the authored power/exit/role data: powerful agenda-setters with arbitrage options (major economies) compute a different effective extraction than powerless payers with identity-locked exit (domestic constituencies). The constraint's type will vary by seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from beneficiary/victim + exit structure. The WTO Secretariat/panel system sits as agenda-setter with institutional power and cannot exit (trapped exit) — it benefits from maintaining authority and has institutional incentives to expand it. Major trading economies are powerful beneficiaries (arbitrage exit) — they can litigate, win more often, and enforce rulings in their favor. Smaller trading economies are constrained payers with moderate power — they pay compliance costs and have limited litigation resources (constrained exit). Domestic constituencies are powerless payers with identity-locked exit — they cannot leave the WTO system and cannot appeal DSB rulings through their legislatures; their policy preferences are locked out once their government accepts the ruling. This structure produces directionality values ranging from near-0.0 (beneficiary end, for agenda-setter and major economies) to near-1.0 (target end, for powerless domestic constituencies). The asymmetry is structural: the same constraint allocates benefits to some seats and costs to others.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (credible commitment to trade liberalization via binding dispute resolution) remains live and operationally solved — the DSB does provide binding rulings and member states do comply with most of them. However, the mandatrophy risk lies in the expansive interpretation of panel authority. If panels have created obligations beyond the founding treaties' text (as the judicial-activism reading contends), then the constraint may have drifted from solving the founding problem into enforcing obligations members did not negotiate. The measurement series shows rising theater ratio (0.12 to 0.28) and steady rise in extractiveness (0.42 to 0.68), suggesting the constraint's enforcement infrastructure has hardened over time and increasingly performs institutional self-preservation rather than treaty implementation. This does not prove mandatrophy — the extracted value may be legitimate dispute-resolution cost — but it raises mandatrophy alerts. The constraint's tangled-rope classification (coordination + enforcement extraction) is defensible: genuine coordination problem solved, but asymmetric extraction because major economies structurally capture more benefit. Mandatrophy would emerge if the extraction outgrew the coordination function and panels prioritized institutional authority over treaty fidelity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_advisory_framing,
    'Is the DSB''s authority grounded in explicit treaty language mandating binding rulings, or is bindingness an institutional practice that member states have accepted post-hoc?',
    'Historical analysis of the Uruguay Round negotiating record and founding text; surveys of member-state legal positions and domestic ratification instruments; analysis of whether member states have ever formally contested bindingness vs. merely contested specific rulings.',
    'If bindingness is explicit in treaty text and founding intent, the constraint is a transparently negotiated sovereignty trade-off (supports binding-referee reading). If bindingness emerged from practice and unchallenged institutional assertion, the constraint may reflect institutional capture or judicial activism (supports judicial-activism-reading). The foundational legitimacy of the entire DSB system depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_vs_advisory_framing, empirical, 'Whether DSB bindingness derives from explicit founding agreement or institutional practice.').

omega_variable(
    interpretive_mandate_drift,
    'Has DSB panel jurisprudence systematically interpreted WTO agreements to extend beyond the letter and founding intent of the treaties, thereby creating new compliance obligations not explicitly negotiated?',
    'Comparative legal analysis of founding agreements vs. landmark DSB rulings (e.g., Appellate Body jurisprudence on non-discrimination, precaution, sanitary standards). Survey of member-state complaints about panel overreach. Analysis of whether panel interpretations track the negotiated text or depart from it systematically.',
    'Evidence of systematic drift toward judicial legislation would support the judicial-activism reading and undermine the legitimacy of the binding-referee reading. Evidence of faithful interpretation within textual bounds would support bindingness as legitimate treaty enforcement. This is the core contestation between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_mandate_drift, empirical, 'Whether DSB panels have drifted beyond their founding mandate through interpretive expansion.').

omega_variable(
    power_asymmetry_in_binding_authority,
    'Does the binding authority of DSB rulings distribute equally across member states, or do major trading economies structurally advantage themselves through litigation resources, panelist selection, and rule-shaping?',
    'Quantitative analysis of dispute-win rates by country size and resources. Analysis of who initiates disputes and against whom. Study of whether major economies'' policy preferences appear in panel rulings more often than smaller economies''. Investigation of repeat-player advantage in dispute forums.',
    'If binding authority is asymmetrically captured by major economies, the constraint functions as extractive for smaller members (tangled-rope reading supported). If authority distributes equally, bindingness is more defensible as neutral coordination. The measurement of this asymmetry is central to whether the constraint warrants mandatrophy analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(power_asymmetry_in_binding_authority, empirical, 'Whether DSB binding authority is asymmetrically captured by major trading economies.').

omega_variable(
    sovereignty_surrender_voluntariness,
    'Did member states genuinely consent to surrender policy discretion when they ratified the WTO, or were they coerced by market-access incentives and structural power asymmetries?',
    'Analysis of accession negotiations: did all states face identical pressure, or did smaller states have constraints that limited their negotiating position? Study of whether states understood DSB bindingness at ratification time or discovered it post-hoc. Analysis of exit costs: could a state credibly withdraw from the WTO without catastrophic trade costs?',
    'Genuine informed consent supports the tangled-rope reading (coordination + extraction, both known). Coerced or ill-understood surrender of discretion moves the constraint toward snare. The distribution of understanding and consent across the membership matters: some large states may have genuinely consented while smaller states faced constrained choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_surrender_voluntariness, conceptual, 'Whether surrender of policy discretion was genuinely voluntary or structurally coerced.').

omega_variable(
    kernel_reading_contest_location,
    'Which structural element is the site of disagreement between binding-referee, advisory-coordination, and judicial-activism readings?',
    'Empirical resolution of the specific sub-disagreements above (binding-vs-advisory framing, interpretive-mandate-drift, consent-voluntariness) would move the constraint toward one reading. However, the committer structure itself may not be resolvable: if major states genuinely believe the binding-referee reading while smaller states believe they face judicial activism, the kernel remains contested and no single factual resolution settles it.',
    'This omega names the irreducible contestation in the kernel itself: the DSB''s legitimacy is read differently by different readings'' adherents, and no single fact may settle which reading is structurally true. Commitment-system classification depends on acknowledging this irreducible ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'The binding-referee reading inhabits a contested kernel where three coherent readings coexist with different structural conclusions about DSB authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__binding_referee_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement_basis(wto__tr_t1995, observed).
narrative_ontology:measurement(wto__tr_t2002, wto_dsb_authority__binding_referee_reading, theater_ratio, 2002, 0.16).
narrative_ontology:measurement_basis(wto__tr_t2002, observed).
narrative_ontology:measurement(wto__tr_t2009, wto_dsb_authority__binding_referee_reading, theater_ratio, 2009, 0.22).
narrative_ontology:measurement_basis(wto__tr_t2009, observed).
narrative_ontology:measurement(wto__tr_t2016, wto_dsb_authority__binding_referee_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement_basis(wto__tr_t2016, observed).
narrative_ontology:measurement(wto__tr_t2022, wto_dsb_authority__binding_referee_reading, theater_ratio, 2022, 0.28).
narrative_ontology:measurement_basis(wto__tr_t2022, observed).
narrative_ontology:measurement(wto__tr_t2026, wto_dsb_authority__binding_referee_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(wto__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement_basis(wto__be_t1995, observed).
narrative_ontology:measurement(wto__be_t2002, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2002, 0.51).
narrative_ontology:measurement_basis(wto__be_t2002, observed).
narrative_ontology:measurement(wto__be_t2009, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2009, 0.58).
narrative_ontology:measurement_basis(wto__be_t2009, observed).
narrative_ontology:measurement(wto__be_t2016, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2016, 0.64).
narrative_ontology:measurement_basis(wto__be_t2016, observed).
narrative_ontology:measurement(wto__be_t2022, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2022, 0.67).
narrative_ontology:measurement_basis(wto__be_t2022, observed).
narrative_ontology:measurement(wto__be_t2026, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(wto__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement_basis(wto__su_t1995, observed).
narrative_ontology:measurement(wto__su_t2002, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2002, 0.56).
narrative_ontology:measurement_basis(wto__su_t2002, observed).
narrative_ontology:measurement(wto__su_t2009, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2009, 0.62).
narrative_ontology:measurement_basis(wto__su_t2009, observed).
narrative_ontology:measurement(wto__su_t2016, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement_basis(wto__su_t2016, observed).
narrative_ontology:measurement(wto__su_t2022, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2022, 0.71).
narrative_ontology:measurement_basis(wto__su_t2022, observed).
narrative_ontology:measurement(wto__su_t2026, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(wto__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__binding_referee_reading, 0.12).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, intellectual_property_enforcement_via_trips).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, sanitary_and_phytosanitary_standards_constraint).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, regulatory_harmonization_pressure).

% DUAL FORMULATION NOTE:
% The wto_dsb_authority kernel has three constraint-level readings: binding-referee (this story), advisory-coordination, and judicial-activism. Each reading has its own ε (binding-referee: high extraction, 0.68; advisory-coordination: lower extraction, ~0.35; judicial-activism: high extraction but classified differently, ~0.72). They share the same institution (WTO DSB) but instantiate different constraints because they read the institution's authority differently. The three stories form a constraint family linked by network.affects_constraints. Factual resolution of empirical omegas (founding-intent, interpretive-drift, power-asymmetry, consent-voluntariness) could move the entire family's classification or resolve the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, powerful, 0.25).
constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, moderate, 0.68).
constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
