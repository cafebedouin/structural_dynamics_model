% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility as Structural Trilemma (Triffin Reading)
 *   domain: economic/international_political_economy
 *
 * SUMMARY:
 *   The Bretton Woods system (1944–1973) anchored the post-WWII international
 *   monetary order in dollar-gold convertibility at $35/oz and fixed exchange
 *   rates. This reading of the constraint identifies convertibility itself as
 *   a structural contradiction that cannot be resolved by policy adjustment
 *   or institutional design — it is a trilemma made manifest: the U.S. cannot
 *   simultaneously (1) maintain a fixed gold parity, (2) provide sufficient
 *   dollar liquidity for growing world trade, and (3) sustain its own
 *   monetary sovereignty and full-employment policy. As U.S. inflation and
 *   deficits grew in the 1960s, gold reserves depleted, and creditor nations
 *   faced a choice between holding depreciating reserves or converting them
 *   to gold (accelerating the depletion). The constraint extracts from both
 *   the U.S. (which must subordinate domestic policy to external redemption
 *   pressure) and from creditor nations (whose reserve holdings depreciate
 *   without compensating adjustment). The beneficiary is the post-Bretton
 *   Woods floating-rate regime that resolves the trilemma by allowing
 *   exchange rates to float, decoupling monetary policy from gold redemption.
 *   This reading opposes a 'strict convertibility' reading (which treats the
 *   obligation as binding and enforceable) and a 'policy flexibility' reading
 *   (which treats convertibility as subordinate to domestic stability). The
 *   Triffin reading asserts that neither policy adjustment nor rule
 *   interpretation can resolve the underlying impossibility — the system's
 *   collapse is structural, not contingent on policy mistakes.
 *
 * KEY AGENTS:
 *   - United States Treasury and Federal Reserve: The hegemon locked into an impossible commitment. Their identity as the postwar monetary authority makes exit (abandoning convertibility) experienced as loss of hegemony, even though hegemony becomes unsustainable under the constraint. Identity-locked exit.
 *   - Gold-standard creditor nations (France, Germany, Switzerland, others): Rational actors in a coordination trap. Converting dollars to gold is individually rational but collectively destabilizing. Constrained by the need for international trade settlement to remain possible.
 *   - Bretton Woods institutional architects: IMF and central bank governors defending rules they created but whose physics no longer permit enforcement. Trapped by institutional legitimacy and lack of amendment authority.
 *   - Triffin analytical seat: Economist Robert Triffin and followers who diagnosed the trilemma as structural. Their position is that the constraint is NOT a policy problem but a design problem requiring systemic revision.
 *   - Developing economies and post-colonial states: Excluded from Bretton Woods design but forced to peg to the dollar. Bear adjustment costs of the system's collapse without having created or managed it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.81).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.62).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility as Structural Trilemma (Triffin Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "economic/international_political_economy").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, 'eb91d74c-1bb2-4d73-9ec2-4e4c77a10243').
narrative_ontology:cs_kernel_codification('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243', fixed_text).
narrative_ontology:cs_authority_grounding('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243', lineage).
narrative_ontology:cs_interpretation_layer_present('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243').
narrative_ontology:cs_reading_relation('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_axiom('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243', foundational, trilemma_logical_necessity).
narrative_ontology:cs_axiom_status(trilemma_logical_necessity, holdable).
narrative_ontology:cs_axiom_grounding('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243', trilemma_logical_necessity, empirically_contingent).
narrative_ontology:cs_axiom('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243', foundational, structural_revision_required).
narrative_ontology:cs_axiom_status(structural_revision_required, holdable).
narrative_ontology:cs_axiom_grounding('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243', structural_revision_required, deontological).
narrative_ontology:cs_reference_frame('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243', trilemma_equilibrium_impossible).
narrative_ontology:cs_drift_state('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243', post_1965_empirical_validation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('eb91d74c-1bb2-4d73-9ec2-4e4c77a10243', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, floating_rate_post_bretton_woods_regime).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_monetary_authority).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, gold_standard_creditor_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, multinational_exporters).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury_and_fed).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, central_banks_of_creditor_states).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, impossible_trilemma_hypothesis).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, reserve_currency_contradiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The U.S. monetary authority committed to redeem dollars in gold at $35/oz under Bretton Woods. This commitment constrains domestic monetary expansion needed for full employment, military spending, and welfare commitments. The authority must simultaneously issue sufficient dollars for world liquidity (a role that requires deficit spending) and defend gold reserves (which shrink with deficit spending). The identity-lock arises from the U.S. commitment to hegemonic monetary authority — abandoning convertibility is experienced as strategic defeat, even though adherence to it becomes economically impossible. The U.S. both sets the formal rules (the agenda-setter role, through IMF Articles and Federal Reserve policy) and is constrained by those rules (the payer role, as gold flows out and inflation pressures mount).
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury_and_fed, payer,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury_and_fed, agenda_setter).

% France, Germany, Switzerland, Netherlands, Belgium, and others that export manufactures and accumulate dollar reserves under the assumption of gold redemption at fixed parity. As U.S. inflation erodes the real value of reserves without corresponding exchange-rate adjustment, they face an impossible choice: hold depreciating dollars (and lose wealth), convert to gold (and accelerate reserve depletion, destabilizing the system), or devalue their own currencies against the dollar (which is politically costly and competitively damaging). They cannot unilaterally exit without fragmenting the international trade system they depend on. They must coordinate a breach of Bretton Woods rules, but coordination is expensive and creates the prisoner's dilemma: early converters protect their reserves but accelerate system collapse for later converters.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, gold_standard_creditor_nations, payer,
    organized, biographical, constrained, global).

% Not an actor but a regime structure: the post-1973 system of floating exchange rates that emerges after the Bretton Woods breakdown. It resolves the Triffin trilemma by allowing the dollar to depreciate against gold and other currencies, freeing monetary policy from redemption constraint, and allowing interest rates to float. The constraint's collapse vindicates this floating-rate order, which becomes the beneficiary in the sense that it is the regime whose legitimacy the constraint's failure demonstrates.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, floating_rate_post_bretton_woods_regime, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, floating_rate_post_bretton_woods_regime).

% The IMF, World Bank, and central-bank governors (primarily U.S., UK, Canadian, and Swiss leadership) who negotiated and wrote the Bretton Woods Articles in 1944 and administered them through the 1960s-70s. They are trapped because they cannot easily revise the system's founding documents without admitting design failure. Their legitimacy depends on the rules they created. They defend the system through elaborate policy theater (London Gold Pool, currency swap networks, IMF special programs) while the underlying contradiction persists. They cannot enforce a rule whose physics has been broken by inflation and capital flows.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, bretton_woods_institutional_architects, agenda_setter,
    institutional, generational, trapped, global).

% Charged with supervising the convertibility regime under Article IV. Faces a structural bind: defending the formal rule while watching the economic reality make it untenable. Cannot unilaterally revise the system's founding charter but also cannot enforce rules that physics no longer permits. By the late 1960s, the IMF's role shifts to managing the system's orderly breakdown rather than enforcing its rules — a role transition that goes unannounced and unacknowledged in official documents.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% Bank of France, Bundesbank, Swiss National Bank, and others. Accumulate dollar reserves and, starting in the 1960s, convert them to gold at the official rate ($35/oz). Their individual decisions to convert are rational (buying a depreciating asset for an appreciating one), but collectively these conversions accelerate the system's collapse. Each faces identical incentive misalignment: convert early to preserve value, but collective early conversion kills the system everyone depends on. This is the structural prisoner's dilemma the Triffin reading diagnoses.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, central_banks_of_creditor_states, payer,
    organized, biographical, constrained, national).

% Large firms that benefit from the fixed-parity, capital-controls regime: they can plan production and pricing on fixed exchange rates; they can move capital across borders at official rates even when black markets price differently. This coordination benefit evaporates when rates float and exchange-rate risk enters business planning. These actors have high exit options (they can operate in any currency zone) and low extraction cost (they benefit from the fixed system), so their directionality is low (near beneficiary end).
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, multinational_exporters, beneficiary,
    powerful, biographical, mobile, global).

% India, Egypt, Indonesia, Yugoslavia, and others pegged to sterling or dollar at fixed rates but had no seat in Bretton Woods design and no voice in its amendment. They relied on Bretton Woods stability for development planning. When the system collapses, they bear the adjustment costs (currency crashes, debt revaluation, capital flight, terms-of-trade shocks) without having designed or managed the regime. Their exclusion from the constraint's governance is structural suppression.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, developing_economies_and_post_colonial_states, excluded,
    powerless, generational, trapped, global).

% The economist Robert Triffin and the tradition of institutional economics he represents. Identified the logical contradiction at the core of the system: the U.S. cannot simultaneously be a sovereign state constrained by gold convertibility AND provide the growing dollar liquidity the world economy requires. This reading embodies the analytical case that the contradiction is NOT a policy choice but a structural design flaw that physics (reserve depletion, inflation dynamics, capital flows) will inevitably break. The analytical seat perceives the constraint as a mountain-like inevitability given the trilemma's logic.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, robert_triffin_analytical_seat, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__triffin_structural_reading, floating_rate_post_bretton_woods_regime).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__triffin_structural_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods convertibility solves the post-WWII coordination problem of establishing fixed exchange rates, capital mobility (within permitted channels), and stable international settlement: parties commit to fixed parities and gold redemption at the center, allowing trade and investment on predictable pricing. In principle, this solves the 1920s instability (floating rates, competitive devaluations, trade collapse) by locking in mutual credibility.
% TRANSFER_FUNCTION: Extracts from the U.S. (seigniorage loss, monetary-policy constraint, forced adjustment) and from creditor nations (depreciation of dollar reserves, interest-rate constraints, inability to revalue without breaking the fixed system). The extraction is the cost of the attempted coordination; it flows to the eventual beneficiary (floating-rate regime) that only crystallizes after collapse.
% ABSENT_VOICES: Developing economies and colonial/post-colonial states are excluded from Bretton Woods decision-making but forced to peg to the dollar or sterling. They would argue (and do, in the 1960s-70s) that the system's instability is imposed on them by design flaws they had no part in creating. The constraint's collapse happens TO them, not FOR them; they bear adjustment costs without consent.
% DISAPPEARANCE_RATIONALE: If convertibility had never been imposed, or had been abandoned early without the painful 1960s-70s stasis, the international monetary system would have moved directly to floating rates or an alternative reserve arrangement (gold standard proper, or a genuinely multi-currency basket). The constraint's persistence from 1944 to 1973 forced a two-decade detour through an impossible position, compressing all adjustment into a sharp break.
% FOUNDING_PROBLEM: Post-WWII international trade required exchange-rate stability, settlement mechanisms, and credible commitment by the reserve-issuer (U.S.) not to inflate the currency. The founding problem was: how to prevent return to 1930s competitive devaluation and trade collapse while allowing the U.S. to maintain its domestic economy?
% FOUNDING_PROBLEM_CORROBORATION: By 1965 (Robert Triffin's testimony to Congress), the founding problem's condition had changed fundamentally: the U.S. was no longer a surplus economy defending against others' inflation, but a deficit economy whose own spending was exporting inflation. The founding problem (preventing devaluation races) was solved; the system's persistence created a new problem (the Triffin dilemma: the solution became the pathology). This status shift is attested by International Monetary Fund staff analysis, OECD economic reviews, and later historical scholarship outside the U.S. policy establishment — parties without stake in defending the defunct system.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.25 (1944, initial commitment seems costless) to 0.81 (1973, full conflict manifest) as the contradiction develops. At t0 the U.S. has large gold reserves and manageable external deficits; convertibility feels like a credible commitment with low cost. By 1960 (Triffin's diagnosis moment), gold is flowing out, inflation is rising, and the cost becomes visible. By 1973, the constraint forces impossible choices: honor gold convertibility and accept domestic recession, or abandon it and break the system's legal foundation. Theater ratio rises as suppression mechanisms proliferate (London Gold Pool, currency swap networks, capital controls) to artificially maintain the fixed parity while the underlying economics move against it — more machinery is needed to defend what market forces oppose. Suppression requirement plateaus around 0.62 because the constraint's enforcement depends ultimately on U.S. military/geopolitical power (NATO, alliances), not on economic incentives. The coercion grid shows structural-level accessibility collapse falling from 0.92 to 0.15: at t0 alternatives to Bretton Woods seem impossible (all major economies are physically destroyed or isolated); by t1 floating rates appear (and are implemented) as the only viable alternative. Organizational-level suppression rises as central banks must coordinate to defend fixed parities against market pressure. Class-level resistance rises as exporters, workers, and developing states experience inflation and constraint. This is cyclical constraint dynamics: the contradiction itself drives resistance, which then forces policy response (Nixon Shock, December 1971), which then crystallizes the shift to floating rates (January 1973). The measurement series model this arc.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. policy seat, convertibility is an unfunded mandate that creates impossible choices but cannot be abandoned without strategic loss. From the creditor-nation seat, the constraint is a trap set by the U.S. to extract seigniorage; converting to gold (the only escape) accelerates collapse and imposes shared pain. From the Triffin analytical seat, both parties are victims of a design flaw; the conversation should not be about policy adjustment but about systems change. The engine computes different type classifications from each seat: U.S. may classify as snare (forced extraction), creditors as tangled_rope (coordination with asymmetric cost), analysts as observing a mountain-like inevitability (the trilemma is physics). The metrics authored here (high extractiveness, rising theater, persistent suppression) reflect the Triffin seat's reading: the constraint extracts from all parties until structural revision occurs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the U.S. approaches 1.0 (full target): it bears the constraint's costs (gold loss, inflation, policy constraint) without benefit, and exit (abandoning convertibility) is presented as abandonment of hegemony. However, the U.S. does receive pre-seigniorage benefit early in the period (the ability to run deficits because dollars are accepted as reserves). Directionality starts near 0.6 (symmetric) and drifts toward 0.85 by 1973. For creditor nations, directionality is 0.8 (near target): they accumulate depreciating reserves, cannot revalue without breaking the fixed system, and must choose between holding losses or destabilizing conversions. For developing economies (excluded), directionality would be higher (0.9+) if they were authored as payers — but they are excluded, so they do not feed the beneficiary/victim derivation; their exclusion is structural suppression. The constraint's single articulated beneficiary is the floating-rate regime (not an agent, but a structure), which emerges AFTER the constraint collapses. This is the logical closure the Triffin reading requires: the constraint extracts from all living agents, but the regime that resolves it is the regime that succeeds the constraint's destruction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing 1930s competitive devaluation and providing stable settlement for postwar reconstruction — is dead by 1965. The constraint persists for eight more years not because it solves the founding problem but because (1) institutional inertia (Bretton Woods articles cannot be easily amended), (2) U.S. geopolitical interest in maintaining hegemonic position, and (3) creditors' fear of the chaos of system redesign. The constraint becomes the problem it was designed to solve: instead of preventing devaluation, it forces eventual sharp break; instead of ensuring stability, it creates two decades of latent crisis. Mandatrophy is present and the Triffin reading declares it: the founding problem is dead (the constraint is still needed to prevent 1930s-style races, but no one fears that anymore). The constraint's persistence past its functional death is the mechanism that forces eventual collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seigniorage_boundary,
    'How much of the measured extractiveness flows from U.S. seigniorage (the benefit of issuing a reserve currency) versus the cost of honoring redemption commitments? Is the U.S. net extractor or net target?',
    'Accounting decomposition: measure the flow of seigniorage (goods/services imported without matching payment) against the cost of gold loss and policy constraint. Compare U.S. balance-of-payments accounting across Bretton Woods and post-Bretton Woods periods.',
    'If seigniorage exceeds constraint cost, the U.S. is a net beneficiary whose identity-lock is about preserving a profitable arrangement. If constraint cost exceeds seigniorage, the U.S. is a net victim identity-locked into a deteriorating position. Classification directionality (d) depends on this decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_boundary, empirical, 'Whether U.S. seigniorage gain exceeds convertibility cost.').

omega_variable(
    trilemma_necessity,
    'Is the trilemma a logical necessity of the fixed-parity design, or could policy coordination have resolved it without systemic revision?',
    'Counterfactual analysis: model the system with aggressive revaluation of creditor currencies (higher D-mark, franc, etc.), or with capital control modifications, or with SDR-based reserve architecture. Assess whether these adjustments could have sustained fixed parities and convertibility long-term.',
    'If the trilemma is logical necessity, the Triffin reading is correct and the system''s collapse was inevitable. If the trilemma could have been resolved with better policy coordination, the policy-flexible reading is correct and the constraint''s persistence depends on contingent political choices (Nixon administration refusal to accept U.S. adjustment).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trilemma_necessity, conceptual, 'Whether the Triffin trilemma is a logical impossibility or a contingent coordination failure.').

omega_variable(
    suppression_mechanism_internalization,
    'The high suppression requirement (0.62) is maintained through what combination of structural coercion (capital controls, legal limits on gold conversion) and internalized commitment (institutional actors'' self-restraint in defending the system)? Does suppression persist after collapse, affecting the floating-rate system''s design?',
    'Post-1973 analysis of capital account liberalization, the rise of eurodollar markets (which escaped suppression), and the persistence of central-bank coordination norms after the system''s legal collapse. If suppression was primarily structural, liberalization would be rapid; if internalized, coordination would persist.',
    'If suppression is primarily internalized (commitment to defending Bretton Woods becomes institutional identity), then exit from the constraint carries hidden costs even after formal collapse — central banks may resist floating rates or gold revaluation even when rules permit. This would affect the floating-rate system''s stability and the institutional evolution of post-Bretton Woods architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression in the convertibility constraint.').

omega_variable(
    kernel_committer_disagreement,
    'Is the kernel (Article IV convertibility commitment) a fixed legal text that different parties interpret differently, or is the kernel itself contested — i.e., do different parties understand what was AGREED in 1944?',
    'Textual and historical analysis of Bretton Woods negotiating records, U.S. vs. UK vs. creditor-nation interpretations of Article IV, and differences in domestic ratification debates. Assess whether the disagreement is about interpretation of settled law or about what was settled.',
    'If parties disagreed about what was agreed in 1944 (some thought convertibility was absolute, others conditional, others temporary), then the constraint is less a shared commitment than a misunderstanding. The trilemma reading becomes a diagnosis of that misunderstanding rather than a structural contradiction of the agreement''s terms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_disagreement, conceptual, 'Whether Article IV was a settled commitment differently interpreted, or a misunderstood commitment differently remembered.').

omega_variable(
    beneficiary_regime_identity,
    'Is the post-Bretton Woods floating-rate regime genuinely the beneficiary of convertibility''s collapse, or does it inherit the same constraints (now manifested in currency competition and monetary-policy coordination challenges)?',
    'Comparative analysis: do floating-rate systems resolve the trilemma or merely transform it? Measure the autonomy of monetary policy, exchange-rate stability, and capital mobility in post-1973 period against Bretton Woods period. Assess whether floating rates are more stable or whether instability is merely visible rather than suppressed.',
    'If floating rates are genuinely more stable and autonomous, the constraint''s collapse is vindication of the Triffin diagnosis. If instability merely shifts form (exchange-rate volatility replaces reserve depletion), the constraint may be a feature of any international monetary system rather than a design flaw of Bretton Woods specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_regime_identity, empirical, 'Whether floating-rate regime resolves or merely transforms the Triffin trilemma.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1944, tn=1973
narrative_ontology:measurement(doll_grid_01, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(class), 1944, 0.72).
narrative_ontology:measurement(doll_grid_02, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(class), 1973, 0.48).
narrative_ontology:measurement(doll_grid_03, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(individual), 1944, 0.65).
narrative_ontology:measurement(doll_grid_04, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(individual), 1973, 0.62).
narrative_ontology:measurement(doll_grid_05, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(organizational), 1944, 0.88).
narrative_ontology:measurement(doll_grid_06, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(organizational), 1973, 0.22).
narrative_ontology:measurement(doll_grid_07, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(structural), 1944, 0.92).
narrative_ontology:measurement(doll_grid_08, dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse(structural), 1973, 0.15).
narrative_ontology:measurement(doll_grid_09, dollar_gold_convertibility__triffin_structural_reading, resistance(class), 1944, 0.18).
narrative_ontology:measurement(doll_grid_10, dollar_gold_convertibility__triffin_structural_reading, resistance(class), 1973, 0.72).
narrative_ontology:measurement(doll_grid_11, dollar_gold_convertibility__triffin_structural_reading, resistance(individual), 1944, 0.22).
narrative_ontology:measurement(doll_grid_12, dollar_gold_convertibility__triffin_structural_reading, resistance(individual), 1973, 0.68).
narrative_ontology:measurement(doll_grid_13, dollar_gold_convertibility__triffin_structural_reading, resistance(organizational), 1944, 0.12).
narrative_ontology:measurement(doll_grid_14, dollar_gold_convertibility__triffin_structural_reading, resistance(organizational), 1973, 0.88).
narrative_ontology:measurement(doll_grid_15, dollar_gold_convertibility__triffin_structural_reading, resistance(structural), 1944, 0.08).
narrative_ontology:measurement(doll_grid_16, dollar_gold_convertibility__triffin_structural_reading, resistance(structural), 1973, 0.92).
narrative_ontology:measurement(doll_grid_17, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(class), 1944, 0.08).
narrative_ontology:measurement(doll_grid_18, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(class), 1973, 0.68).
narrative_ontology:measurement(doll_grid_19, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(individual), 1944, 0.05).
narrative_ontology:measurement(doll_grid_20, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(individual), 1973, 0.45).
narrative_ontology:measurement(doll_grid_21, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(organizational), 1944, 0.12).
narrative_ontology:measurement(doll_grid_22, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(organizational), 1973, 0.81).
narrative_ontology:measurement(doll_grid_23, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(structural), 1944, 0.15).
narrative_ontology:measurement(doll_grid_24, dollar_gold_convertibility__triffin_structural_reading, stakes_inflation(structural), 1973, 0.85).
narrative_ontology:measurement(doll_grid_25, dollar_gold_convertibility__triffin_structural_reading, suppression(class), 1944, 0.12).
narrative_ontology:measurement(doll_grid_26, dollar_gold_convertibility__triffin_structural_reading, suppression(class), 1973, 0.58).
narrative_ontology:measurement(doll_grid_27, dollar_gold_convertibility__triffin_structural_reading, suppression(individual), 1944, 0.08).
narrative_ontology:measurement(doll_grid_28, dollar_gold_convertibility__triffin_structural_reading, suppression(individual), 1973, 0.42).
narrative_ontology:measurement(doll_grid_29, dollar_gold_convertibility__triffin_structural_reading, suppression(organizational), 1944, 0.18).
narrative_ontology:measurement(doll_grid_30, dollar_gold_convertibility__triffin_structural_reading, suppression(organizational), 1973, 0.65).
narrative_ontology:measurement(doll_grid_31, dollar_gold_convertibility__triffin_structural_reading, suppression(structural), 1944, 0.2).
narrative_ontology:measurement(doll_grid_32, dollar_gold_convertibility__triffin_structural_reading, suppression(structural), 1973, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__triffin_structural_reading, 0.18).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, bretton_woods_capital_control_regime).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_rate_system).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the dollar-gold-convertibility kernel. The TRIFFIN_STRUCTURAL_READING diagnoses the constraint as a logical impossibility (the trilemma) requiring systemic revision. STRICT_CONVERTIBILITY_READING treats it as a binding legal obligation. POLICY_FLEXIBLE_READING treats it as subordinate to domestic stability. All three readings share the same formal object (Article IV, $35/oz, fixed parities) but differ in whether the constraint is a contractual obligation, a policy tool, or a structural design flaw. The ε values differ substantially: strict reading keeps ε low (the obligation is real and constrains policy); policy-flexible reading modulates ε based on whether policy adjustment is successful; Triffin reading keeps ε high throughout because the trilemma cannot be solved by policy or negotiation. Network edges show that Triffin influences both sibling readings (changes the terms of debate about what 'flexible' or 'strict' convertibility actually means in the face of the trilemma) but does not foreclose them (both are still live positions held by different policy communities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__triffin_structural_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
