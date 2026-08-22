% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Overdetermined Collapse Reading of the Bretton Woods Transition (Triffin Bind as Structural Law)
 *   domain: economic/monetary_political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the transition_causality kernel:
 *   the claim that the end of the Bretton Woods par-value system was
 *   structurally inevitable, driven by multiple reinforcing contradictions —
 *   chiefly the Triffin bind (supplying world liquidity required US deficits
 *   that destroyed confidence in dollar-gold convertibility), compounded by
 *   US fiscal expansion, the one-way speculative wager fixed parities handed
 *   to mobile capital, and the refusal of surplus countries to revalue. Per
 *   the epsilon-referent rule, extractiveness is authored for the STANDING
 *   ARRANGEMENT UNDER CONTEST — the fixed-rate dollar-gold exchange standard
 *   as it actually operated — assessed by this reading's own lights: an
 *   architecture that promised stability while extracting adjustment from
 *   deficit seats, devaluation risk from surplus seats, and ultimately its
 *   own anchor from the hegemon. Sibling readings (contingent_choice_reading,
 *   hybrid_trigger_reading) are separate constraint stories with their own
 *   epsilon and victim structures; they are linked, not averaged, here. KEY
 *   AGENTS (by structural relationship): - us_monetary_authorities: hegemonic
 *   agenda-setter and trapped beneficiary (institutional/trapped) — issues
 *   the reserve asset, collects seigniorage, bears the unhonorable pledge -
 *   deficit_country_governments: primary payers (organized/constrained) —
 *   bear deflationary adjustment - surplus_central_banks: payer-beneficiaries
 *   (organized/constrained) — accumulate doubtful dollar claims -
 *   internationally_active_exporters: beneficiaries (powerful/mobile) —
 *   harvest parity stability - private_speculative_capital: arbitraging
 *   beneficiary (powerful/arbitrage) — monetizes the one-way bet -
 *   imf_bureaucracy: administrative agenda-setter (organized/identity_locked)
 *   — its mandate is fused with the regime it polices -
 *   developing_commodity_economies: excluded payers (powerless/trapped) -
 *   monetary_economists: analytical observers (analytical/analytical) —
 *   diagnose the bind from outside.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.7).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.62).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Overdetermined Collapse Reading of the Bretton Woods Transition (Triffin Bind as Structural Law)").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "economic/monetary_political_economy/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, 'aa74d6cb-d933-4532-8198-71a000337deb').
narrative_ontology:cs_kernel_codification('aa74d6cb-d933-4532-8198-71a000337deb', distributed).
narrative_ontology:cs_authority_grounding('aa74d6cb-d933-4532-8198-71a000337deb', expertise).
narrative_ontology:cs_interpretation_layer_present('aa74d6cb-d933-4532-8198-71a000337deb').
narrative_ontology:cs_reading_relation('aa74d6cb-d933-4532-8198-71a000337deb', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('aa74d6cb-d933-4532-8198-71a000337deb', transition_causality__hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom('aa74d6cb-d933-4532-8198-71a000337deb', foundational, reinforcing_contradictions_make_collapse_unavoidable).
narrative_ontology:cs_axiom_status(reinforcing_contradictions_make_collapse_unavoidable, holdable).
narrative_ontology:cs_axiom_grounding('aa74d6cb-d933-4532-8198-71a000337deb', reinforcing_contradictions_make_collapse_unavoidable, empirically_contingent).
narrative_ontology:cs_axiom('aa74d6cb-d933-4532-8198-71a000337deb', secondary, policy_sequence_cannot_preserve_gold_parity_regime).
narrative_ontology:cs_axiom_status(policy_sequence_cannot_preserve_gold_parity_regime, holdable).
narrative_ontology:cs_axiom_grounding('aa74d6cb-d933-4532-8198-71a000337deb', policy_sequence_cannot_preserve_gold_parity_regime, empirically_contingent).
narrative_ontology:cs_reference_frame('aa74d6cb-d933-4532-8198-71a000337deb', structural_overdetermination_frame).
narrative_ontology:cs_drift_state('aa74d6cb-d933-4532-8198-71a000337deb', post_revisionist_monetary_history_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aa74d6cb-d933-4532-8198-71a000337deb', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, us_monetary_authorities).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, internationally_active_exporters).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, private_speculative_capital).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, deficit_country_governments).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, surplus_central_banks).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, developing_commodity_economies).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, us_monetary_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, surplus_central_bands_placeholder).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, surplus_central_bands_placeholder).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, triffin_dilemma_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the world's reserve currency under a gold-convertibility pledge priced at $35/oz. Supplies international liquidity by running external deficits, which simultaneously erodes the metallic backing behind every outstanding dollar claim. Collects seigniorage — real goods for paper — while its gold stock drains from roughly $24 billion in 1949 toward the legal floor. Its exits inside the regime are nil short of demolishing the pledge itself: deflate domestically (politically barred), devalue (breaks the system it anchors), or suspend convertibility (ends the regime). In August 1971 it took the third road.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, us_monetary_authorities, agenda_setter,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, us_monetary_authorities, beneficiary).

% Run persistent external deficits under fixed parities — the United Kingdom repeatedly, Italy and others episodically. Adjustment lands on them through deflationary budgets, wage restraint, and import surcharges, frequently attached to Fund credit conditions. Devaluing means crisis and stigma; holding the parity means unemployment at home. Their leverage over the system's rules amounts to bloc votes they rarely win.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, deficit_country_governments, payer,
    organized, biographical, constrained, regional).

% Placeholder removed at compile time.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, surplus_central_bands_placeholder, payer,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, surplus_central_bands_placeholder, beneficiary).

% Contract, invest, and price across borders under parities that rarely move, converting exchange-rate risk into a planning constant. They lobby for stability and against realignment, and bear almost none of the adjustment cost when deficit countries are squeezed.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, internationally_active_exporters, beneficiary,
    powerful, biographical, mobile, global).

% Reads the widening gap between dollar liabilities and gold cover and moves funds ahead of parity changes. Fixed parities hand it an asymmetric wager: attacking a peg costs little when it holds and pays enormously when it breaks. Eurodollar balances and private gold buying give it reach that no capital control fully closes.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, private_speculative_capital, beneficiary,
    powerful, immediate, arbitrage, global).

% Administers the parity grid, extends conditional credit to deficit members, and certifies adjustment programs. Its mandate, staffing, and authority exist only inside the par-value system it polices; a generalized float dissolves its core function. It therefore defends the architecture as vigorously as any member while absorbing the legitimacy costs of conditionality.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, imf_bureaucracy, agenda_setter,
    organized, generational, identity_locked, global).

% Have no seat where parities and liquidity rules are set — the G10 and the Fund board decide. They inherit the consequences: credit on austerity terms, commodity price swings transmitted through reserve-country policies, and quota shares that cap their access to liquidity.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, developing_commodity_economies, payer,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, developing_commodity_economies, excluded).

% Diagnose the architecture from outside its administration. Triffin's 1960 testimony maps the liquidity-confidence bind before the acute crises arrive; Rueff and the French school demand restoration of convertibility; Friedman's case for floating supplies the exit design. They command analysis and argument, not enforcement.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__overdetermined_collapse_reading, us_monetary_authorities).
narrative_ontology:fixing_cost_class(transition_causality__overdetermined_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a single settlement frame for postwar world trade: fixed but adjustable parities, a common vehicle and reserve asset, and pooled credit for temporary external disequilibria — replacing the interwar pattern of competitive devaluation, inconvertible currencies, and discriminatory bilateral clearing.
% TRANSFER_FUNCTION: Moves seigniorage and real resources to the reserve-issuing country; moves adjustment burdens — deflation, wage restraint, unemployment — to deficit countries; moves devaluation risk onto surplus central banks accumulating dollar claims; and periodically transfers large arbitrage gains to speculative capital at parity junctures.
% ABSENT_VOICES: Labor in adjusting economies, and the developing world generally, had no seat where parities and liquidity rules were made; their interests entered only afterward, as the aftermath of conditionality. Domestic electorates in the reserve country learned of the convertibility pledge's abandonment after it happened.
% DISAPPEARANCE_RATIONALE: If the par-value system vanished overnight at any point after 1950, trade contracting, reserve holding, and Fund lending would all have been rebuilt around some successor arrangement within months — the 1971-73 scramble (Smithsonian realignment, the snake, generalized floating) shows exactly this rearrangement occurring under duress once the anchor was pulled.
% FOUNDING_PROBLEM: Interwar monetary disorder: competitive devaluations, beggar-thy-neighbor trade policy, inconvertible currencies, and a world liquidity shortage that choked multilateral trade reconstruction.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Triffin's 1960 diagnosis showed the founding scarcity had inverted into a dollar glut; Rueff's public campaign and the G10's own 1965 review acknowledged the inversion. The US authorities attested the opposite — that the system remained sound and the problem live — until August 1971; that disagreement between beneficiaries and independent diagnosticians is itself the signal.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is mountain because this reading's structural verdict is that the bind admitted zero degrees of freedom for every index inside the architecture: once dollar liabilities outran gold cover, no policy sequence preserved both liquidity and convertibility. The metrics describe the regime's actual operation, which grew steadily more extractive and more theatrical as the bind worked itself out: extractiveness climbs from 0.30 at signing to 0.70 at the window's closure; theater_ratio climbs from 0.08 to 0.55 as parity defense degenerates from routine operations into the Gold Pool's managed retreat (1968) and the Smithsonian realignment announced as historic and abandoned within fourteen months (1971). The suppression_requirement series is authored deliberately: the story specifically tracks enforcement-capacity change — capital controls, the Interest Equalization Tax, voluntary credit restraint, Gold Pool escalation building to 0.68 by 1971, then decaying to 0.62 as the enforcement object ceased to exist. All three series share one time grid (seven points, 1944-1973) so no metric row borrows another's end-state. Accessibility_collapse is high (0.85) because, once the bind was understood, every within-architecture alternative vanished — only exiting the architecture itself remained, which is the content of the inevitability claim. Resistance is moderate (0.40): the French convertibility campaign, speculative attacks, and academic dissent were real and sustained, and all of it failed to alter the bind — which is precisely this reading's point.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute sharply differently. Deficit-country payers and surplus-dollar-accumulating payers sit near the full-target end: extraction amplified by constrained exit. Exporters and speculators sit near the beneficiary end, the speculators closest of all because arbitrage-grade exit lets them collect at every juncture. The anomalous seat is the hegemon: nominally the arrangement's chief beneficiary and its agenda-setter, yet structurally captive — the derivation chain would read its declared beneficiary role and drive directionality toward subsidy, which is why an override pins it near symmetric. The IMF seat is identity-locked: its organizational self-concept is constituted by administering parities, so exit is unthinkable without self-dissolution; break that identity frame and it becomes a mobile observer of floating-rate governance. Observers see the full convergence of pathways that participants each saw only from their own corner.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map to directionality as follows. us_monetary_authorities appears in BOTH arrays — the bind's defining fact is that its host was also its casualty — and the override (institutional, d=0.42) pins the net relationship slightly beneficiary-side of symmetric: seigniorage collected against an unhonorable pledge, gold hemorrhaged, privilege demolished by its own hand. Deficit_country_governments (payer, constrained) derive near-full-target directionality; developing_commodity_economies (payer, powerless, trapped) derive maximal target directionality with no coalition lever exercised inside the interval. Surplus_central_banks (payer with beneficiary secondary, constrained) derive high-but-not-maximal target directionality. Internationally_active_exporters (beneficiary, mobile) and private_speculative_capital (beneficiary, arbitrage) derive low directionality, the speculators lowest. Imf_bureaucracy derives mild beneficiary-side directionality through mandate rents. Monetary_economists carry the analytical atom and no extraction exposure. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the global scope's verification discount.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar chaos and world liquidity shortage — died around 1958-60: current-account convertibility was restored, reconstruction complete, and the scarcity inverted into a dollar glut, exactly as Triffin testified. The regime then persisted thirteen more years on institutional inertia and increasingly theatrical parity defense, which is why founding_problem_status is dead while disappearance_verdict is world_rearranges — the mismatch flags the 1958-71 zombie decade for capture/zombie cross-check. The classification prevents two symmetrical mislabelings: reading the early genuine coordination (the rope-phase reconstruction settlement) backward as pure extraction, and reading the terminal theater forward as proof the whole arrangement was always performance. This story's mountain claim attaches to the bind, not to the regime's coordination merit: the bind is what converted coordination into trap, and its operation vindicates the triffin_dilemma_thesis recorded under vindicated_propositions — a proposition that collects no rents and is deliberately kept out of the beneficiary set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_bind,
    'Is the Triffin bind a genuine structural law of any reserve-currency-under-hard-anchor architecture, or a constructed constraint whose ''inevitability'' framing served the arrangement''s identifiable beneficiaries?',
    'Comparative-design analysis: establish whether symmetric-adjustment and multi-reserve designs (Keynes Clearing Union, the CRU plan, SDR substitution accounts) were technically coherent and were rejected on distributional grounds rather than impossibility; archival record of the 1962-69 reform negotiations.',
    'If the bind is constructed, the mountain claim fails and the story enters false-summit territory — the inevitability doctrine functions as legitimation for incumbent beneficiaries, and the engine''s false-summit signature should override toward hybrid coordination/extraction semantics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_bind, conceptual, 'Whether the bind is natural law or a constructed arrangement whose costs were laundered as inevitability.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is the overdetermined_collapse_reading of kernel transition_causality; what would the sibling readings (contingent_choice_reading, hybrid_trigger_reading) change structurally, and where exactly is the disagreement located?',
    'Locate the disputed element — whether the causal structure admits policy degrees of freedom (contingent) or required contingent trigger events to actualize accumulated contradictions (hybrid) — via counterfactual process-tracing of the 1965-71 decision points.',
    'Adoption of a sibling reading dissolves this story''s mountain claim: any counterfactual viability above zero converts the bind into a choice-space constraint with rope/scaffold semantics and a different victim set; this file''s epsilon and victim structure are valid only under this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel, with the disagreement located in the existence of policy degrees of freedom.').

omega_variable(
    adjustment_asymmetry_magnitude,
    'How asymmetric was the adjustment burden between deficit and surplus seats, and did the reserve-issuing seat escape adjustment entirely?',
    'Panel measurement of output and unemployment cost per unit of external imbalance corrected, 1958-1971, disaggregated by country role in the regime.',
    'High asymmetry amplifies effective extraction on the deficit seats and pushes their per-seat classifications toward pure extraction; rough symmetry supports coordination-heavy readings and softens the victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjustment_asymmetry_magnitude, empirical, 'Magnitude of the adjustment asymmetry across seats.').

omega_variable(
    counterfactual_reform_feasibility,
    'Was a cooperative reform path — wider bands, SDR substitution, US acceptance of adjustment discipline — actually available in 1965-68, or was the veto structure sufficient to make collapse necessary?',
    'Archival negotiation analysis of G10 ministerial meetings and Gold Pool records, testing whether any reform proposal commanded a winning coalition before August 1971.',
    'A feasible-but-rejected path falsifies strict inevitability and shifts the story toward hybrid-trigger semantics; demonstrated absence of any winning coalition confirms the mountain claim and the near-zero counterfactual viability this reading asserts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_reform_feasibility, empirical, 'Feasibility of the reform counterfactual this reading denies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__overdetermined_collapse_reading, theater_ratio, 1944, 0.08).
narrative_ontology:measurement_basis(tran_tr_t1944, observed).
narrative_ontology:measurement(tran_tr_t1950, transition_causality__overdetermined_collapse_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement_basis(tran_tr_t1950, observed).
narrative_ontology:measurement(tran_tr_t1958, transition_causality__overdetermined_collapse_reading, theater_ratio, 1958, 0.16).
narrative_ontology:measurement_basis(tran_tr_t1958, observed).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__overdetermined_collapse_reading, theater_ratio, 1965, 0.26).
narrative_ontology:measurement_basis(tran_tr_t1965, observed).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__overdetermined_collapse_reading, theater_ratio, 1968, 0.38).
narrative_ontology:measurement_basis(tran_tr_t1968, observed).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__overdetermined_collapse_reading, theater_ratio, 1971, 0.55).
narrative_ontology:measurement_basis(tran_tr_t1971, observed).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__overdetermined_collapse_reading, theater_ratio, 1973, 0.55).
narrative_ontology:measurement_basis(tran_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1944, 0.3).
narrative_ontology:measurement_basis(tran_be_t1944, observed).
narrative_ontology:measurement(tran_be_t1950, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1950, 0.34).
narrative_ontology:measurement_basis(tran_be_t1950, observed).
narrative_ontology:measurement(tran_be_t1958, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1958, 0.44).
narrative_ontology:measurement_basis(tran_be_t1958, observed).
narrative_ontology:measurement(tran_be_t1965, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1965, 0.56).
narrative_ontology:measurement_basis(tran_be_t1965, observed).
narrative_ontology:measurement(tran_be_t1968, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1968, 0.63).
narrative_ontology:measurement_basis(tran_be_t1968, observed).
narrative_ontology:measurement(tran_be_t1971, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1971, 0.7).
narrative_ontology:measurement_basis(tran_be_t1971, observed).
narrative_ontology:measurement(tran_be_t1973, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1973, 0.7).
narrative_ontology:measurement_basis(tran_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1944, 0.35).
narrative_ontology:measurement_basis(tran_su_t1944, observed).
narrative_ontology:measurement(tran_su_t1950, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1950, 0.36).
narrative_ontology:measurement_basis(tran_su_t1950, observed).
narrative_ontology:measurement(tran_su_t1958, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1958, 0.42).
narrative_ontology:measurement_basis(tran_su_t1958, observed).
narrative_ontology:measurement(tran_su_t1965, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1965, 0.52).
narrative_ontology:measurement_basis(tran_su_t1965, observed).
narrative_ontology:measurement(tran_su_t1968, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement_basis(tran_su_t1968, observed).
narrative_ontology:measurement(tran_su_t1971, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1971, 0.68).
narrative_ontology:measurement_basis(tran_su_t1971, observed).
narrative_ontology:measurement(tran_su_t1973, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1973, 0.62).
narrative_ontology:measurement_basis(tran_su_t1973, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, resource_allocation).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'why did Bretton Woods end?' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints. This file instantiates the overdetermination claim — the par-value arrangement as a trap with near-zero counterfactual viability, high epsilon, and a victim set spanning every constrained actor including the hegemon. transition_causality__contingent_choice_reading instantiates a choice-space constraint (different epsilon, policy-error victim emphasis); transition_causality__hybrid_trigger_reading instantiates accumulation-plus-trigger (epsilon between the siblings, trigger-dependent victim activation). The structural-evidence corpus organized under this reading is cited by both siblings, so this story carries influence edges to each; the siblings are separate files with their own claimed types and are never averaged into this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__overdetermined_collapse_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
