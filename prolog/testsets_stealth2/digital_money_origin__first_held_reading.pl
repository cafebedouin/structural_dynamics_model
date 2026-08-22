% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: First-Held Threshold: Individually Held Non-Physical Monetary Instruments as Practical Stores of Value
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the first_held_reading of the
 *   digital_money_origin kernel: the claim that digital money emerged when
 *   individuals first held non-physical monetary instruments as practical
 *   stores of value. Under this reading the origin window opens in the
 *   mid-1990s (stored-value cards, the Mondex trial) and consolidates through
 *   wallet balances, mobile money, and early crypto holdings by the late
 *   2000s. The standing arrangement under contest, and the epsilon referent,
 *   is the holding-gate regime of that era: participation in digital money
 *   requires clearing implementation barriers (connectivity, devices, banking
 *   relationships, identity documentation), and network effects concentrate
 *   benefits among early adopters while exclusion costs fall on the
 *   unconnected. Assessed by this reading's own lights, which take holding
 *   rather than conception or recognition as constitutive, those barriers and
 *   network effects are part of the phenomenon, not incidental to it. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   while the authored metrics describe moderately extractive, actively
 *   enforced operation whose intensity grew across the interval; the engine
 *   measures the divergence and the per-seat classifications.
 *
 * KEY AGENTS:
 *   - digital_payment_operators: Agenda setter (institutional/arbitrage) — runs the rails, sets participation terms, collects fees, float, and data
 *   - early_adopting_holders: Primary beneficiary (moderate/mobile) — captures network-effect surplus, sets usage norms
 *   - unbanked_offline_populations: Primary target (powerless/trapped) — bears exclusion costs as commerce digitizes
 *   - accepting_merchants: Dual-positioned payer/beneficiary (organized/constrained) — pays interchange, gains customer reach
 *   - monetary_authorities: Late-arriving observer (institutional/analytical) — tracks and supervises after the holding fact
 *   - monetary_historians: Analytical observer (analytical/analytical) — adjudicates the periodization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.66).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.6).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "First-Held Threshold: Individually Held Non-Physical Monetary Instruments as Practical Stores of Value").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, 'ff4bf621-6558-4a7c-81bd-710ca6c4a46c').
narrative_ontology:cs_kernel_codification('ff4bf621-6558-4a7c-81bd-710ca6c4a46c', distributed).
narrative_ontology:cs_authority_grounding('ff4bf621-6558-4a7c-81bd-710ca6c4a46c', distributed).
narrative_ontology:cs_reading_relation('ff4bf621-6558-4a7c-81bd-710ca6c4a46c', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff4bf621-6558-4a7c-81bd-710ca6c4a46c', digital_money_origin__regulatory_recognition_reading, coexists_with).
narrative_ontology:cs_axiom('ff4bf621-6558-4a7c-81bd-710ca6c4a46c', foundational, practical_holding_constitutes_monetary_emergence).
narrative_ontology:cs_axiom_status(practical_holding_constitutes_monetary_emergence, holdable).
narrative_ontology:cs_axiom_grounding('ff4bf621-6558-4a7c-81bd-710ca6c4a46c', practical_holding_constitutes_monetary_emergence, conventional).
narrative_ontology:cs_axiom('ff4bf621-6558-4a7c-81bd-710ca6c4a46c', secondary, implementation_barriers_are_constitutive_of_the_phenomenon).
narrative_ontology:cs_axiom_status(implementation_barriers_are_constitutive_of_the_phenomenon, holdable).
narrative_ontology:cs_axiom_grounding('ff4bf621-6558-4a7c-81bd-710ca6c4a46c', implementation_barriers_are_constitutive_of_the_phenomenon, empirically_contingent).
narrative_ontology:cs_reference_frame('ff4bf621-6558-4a7c-81bd-710ca6c4a46c', holder_practice_constitutes_emergence).
narrative_ontology:cs_drift_state('ff4bf621-6558-4a7c-81bd-710ca6c4a46c', contemporary_multicausal_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ff4bf621-6558-4a7c-81bd-710ca6c4a46c', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopting_holders).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, digital_payment_operators).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_offline_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, accepting_merchants).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, accepting_merchants).
narrative_ontology:constraint_vindicates(digital_money_origin__first_held_reading, practical_holding_constitutes_monetary_reality).
narrative_ontology:constraint_vindicates(digital_money_origin__first_held_reading, market_led_innovation_periodization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the card networks, e-wallet platforms, and mobile-money rails on which individually held digital balances live. Set participation terms (KYC thresholds, fee schedules, float rules, merchant acceptance standards) and enforce them through contract and technical gating. Collect interchange and wallet fees, earn yield on parked balances, and accumulate transaction data. Exit is arbitrage-grade: they can reprice, re-scope, or relocate services across jurisdictions.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, digital_payment_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Connected, banked individuals who adopted non-physical instruments early: stored-value cards, wallet balances, mobile money, early crypto. They capture the network-effect surplus in wider acceptance, referral incentives, fee waivers, and outsized influence over usage norms. Holding is cheap for them because the prerequisite infrastructure (device, connectivity, bank linkage, identity documents) is already in place, and they can move between competing wallets at low cost.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopting_holders, beneficiary,
    moderate, biographical, mobile, global).

% People without the infrastructure the holding gate presupposes: no bank relationship, thin identity documentation, intermittent connectivity, or no compatible device. As wages, remittances, government payments, and merchant acceptance migrate onto digital rails, they pay in longer queues, cash-handling premiums, exclusion from online pricing, and shrinking usability of the cash they hold. Exit would require acquiring the very infrastructure the gate demands.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_offline_populations, payer,
    powerless, immediate, trapped, regional).

% Businesses that accept digital instruments at the point of sale. They pay interchange and gateway fees on every transaction and absorb chargeback and compliance overhead, but gain access to customers who no longer carry cash and to remote demand they could not otherwise reach. Once a critical mass of holders exists, declining acceptance is commercially costly, so their exit is constrained even where fees rankle.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, accepting_merchants, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, accepting_merchants, beneficiary).

% Central banks and statistics agencies watch individually held digital balances migrate out of their monetary aggregates, track stability implications, and eventually extend supervision to wallet providers and e-money issuers. In this reading's account their formal recognition arrives after the holding fact; their seat is observational and supervisory rather than constitutive.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_authorities, observer,
    institutional, generational, analytical, national).

% Economic historians and payment-systems scholars who reconstruct when non-physical instruments first functioned as practical stores of value, weighing archival evidence of holding behavior against scheme launches and regulatory milestones. Their periodization choices determine which constraint set organizes the field's datasets.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__first_held_reading, digital_payment_operators).
narrative_ontology:fixing_cost_class(digital_money_origin__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of moving and storing value across distance without physical bearer instruments: a person can park purchasing power in a non-physical balance and transmit it remotely, instantly, and in small denominations, which cash cannot do and branch banking does slowly. The periodization convention itself also coordinates: anchoring the origin at first holding gives economists and historians a common dataset boundary.
% TRANSFER_FUNCTION: Moves purchasing power from holders' pockets and bank branches into operator-managed digital balances; moves fees and float income from users and merchants to network operators; moves early-mover advantages (acceptance breadth, referral rewards, norm-setting influence) to early adopters; and moves exclusion costs (cash-handling premiums, lost access to online commerce and pricing) onto those without infrastructure.
% ABSENT_VOICES: Unbanked and offline populations are absent from the venues where digital money's history is written: industry retrospectives, innovation-prize juries, and much of the academic adoption literature. Cash-reliant informal traders and cash-preferring older users would object that holding capacity is being treated as the measure of monetary reality while their exclusion is footnoted as lagging adoption. They are outside the room because the forums are convened by operators, adopters, and regulators, not by the excluded.
% DISAPPEARANCE_RATIONALE: If individually holdable digital value vanished overnight, e-commerce checkout, gig-work payout, remittance corridors, and app-store billing would revert to physical or bank-intermediated forms; wage disbursement, aid delivery, and merchant acceptance would reorganize around cash logistics; and the operators' revenue base would evaporate. The arrangements built on top of the holding capability depend on it.
% FOUNDING_PROBLEM: Physical bearer money moves and stores value across distance badly: transport and theft risk, no remote payment channel open to individuals, denominational friction for small transactions, and no way for a person to park purchasing power anywhere but their pocket or a bank branch counter.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: central-bank payment-systems surveys document the retail cross-distance settlement gap the instruments addressed; development-economics remittance-cost studies attest the transfer problem from the senders' side; and unbanked respondents' testimony in financial-inclusion research attests both the problem and the access barrier from the excluded seat. Industry white papers also attest the problem but sit inside the beneficiary set and are not relied on here.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.66 at interval end because the holding gate converts a monetary capability into an access-conditioned service: the unconnected bear exclusion costs that grow as commerce digitizes, and network effects raise late-entry costs, while operators collect fees and float decoupled from marginal service cost. Suppression is 0.60 and structural rather than coercive: no one forces the unconnected to stay out, but KYC thresholds, device and connectivity costs, and documentation requirements are real gates, and the enforcement machinery (compliance regimes, network rules) built steadily across the interval, hence the rising suppression_requirement series. Theater is 0.40: the first-held criterion wears neutral-empiricism clothing while centering consumer-adoption narratives that flatter market-led innovation stories, yet it also tracks something real, since an instrument nobody holds is not practically money. Accessibility_collapse is 0.50: cash remained a workable alternative throughout most of the interval, so opting out stayed possible, but it grew progressively costlier as acceptance thinned. Resistance is 0.52: cash-preference movements, privacy advocates, cash-only businesses, and unbanked advocacy mounted real but uncoordinated pushback; coalition potential among the excluded exists (financial-inclusion campaigns, community currencies) but never consolidated into systemic leverage. The measurement series run on one shared time grid (seven points, all three metrics at every point) showing a monotonic ratchet rather than cyclical oscillation: exclusion costs compound with adoption, so no intermittent-reinforcement mechanism is posited.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats compute differently from the same structure. From the operator seat the arrangement is infrastructure provision priced at what the market bears, built and maintained at real cost; from the unconnected seat the same structure operates as a gate that converts a public monetary capability into a private, access-conditioned one. Early adopters experience a meritocracy of adoption; the excluded experience inherited inequality, since connectivity and documentation correlate with income and geography. The sharpest same-level contrast sits between two nominally identical adults in a monetized economy who occupy opposite seats purely on infrastructure access: equal civic standing, divergent power atoms, divergent exits. Merchants illustrate intra-level divergence too: same nominal commercial standing, but fee burden versus customer reach nets out differently by size and sector.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Operators (agenda_setter, institutional, arbitrage) sit near the beneficiary end; early_adopting_holders (beneficiary, mobile exit) sit nearest it, since arbitrage-grade wallet-switching damps their exposure further; unbanked_offline_populations (payer, trapped) sit near the full-target end, amplified by the regional scope at which verification of their situation is weakest. One override is authored: accepting_merchants hold the organized power atom alone but carry no base-level beneficiary or victim declaration, because their position is genuinely mixed (fees paid against reach gained); the canonical fallback for organized would misplace them, so d is pinned at 0.55. Monetary authorities and historians are analytical-seat observers and take the analytical handling. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling in both directions. A pure-snare reading would erase the genuine coordination surplus: remote value transfer solved a real problem, corroborated by remittance-cost and payment-systems literature from outside the beneficiary set. A pure-rope reading would erase the access-gated asymmetry: the same rails that coordinate exchange also price participation behind infrastructure walls and concentrate network rents. On the genealogy side, the founding problem (secure remote holding and transfer of value by individuals) is live, so no mandatrophy is declared: the arrangement has not outlived its function, even though specific early schemes (Mondex above all) died while their function migrated to successors. The theater_ratio trajectory is watched rather than diagnosed: if the coordination story continues decoupling from the enforcement story, the drift path runs toward snare, not piton, because a concentrated capturer (the operators) exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_constituting_element,
    'Which structural element of digital money''s history constitutes its emergence: technical and institutional conceivability (became_thinkable_reading), first practical individual holding (this reading), or formal regulatory incorporation (regulatory_recognition_reading)?',
    'Comparative analysis across the three sibling stories in the digital_money_origin family: if the constraint sets (beneficiaries, victims, enforcement machinery, origin dates) differ materially by reading, the kernel is genuinely indexical and no single origin date is correct.',
    'This file resolves the ambiguity FOR THIS READING ONLY: holding is constitutive, so implementation barriers and network effects enter the constraint set and access exclusion enters the victim structure. Adopting a sibling dissolves those elements and installs a different set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(origin_constituting_element, conceptual, 'Location of the kernel disagreement: the constituting element of digital money''s emergence.').

omega_variable(
    sibling_structural_delta,
    'What structurally changes if a sibling reading is adopted instead of this one?',
    'Read the sibling files directly: became_thinkable_reading shifts beneficiaries toward early theorists and protocol designers, dates origin earlier, and removes infrastructure-access victims; regulatory_recognition_reading shifts the agenda-setter seat to monetary authorities and makes compliance and statistical incorporation the operative constraints.',
    'Under became_thinkable, this story''s victim declaration (unbanked_offline_populations) has no counterpart and epsilon drops toward coordination cost; under regulatory_recognition, the payer seat migrates to non-compliant issuers and the enforcement story inverts from market gating to statutory gating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_structural_delta, conceptual, 'Committer structure: what each sibling reading would change in beneficiary, victim, and agenda-setter configuration.').

omega_variable(
    first_held_threshold_event,
    'Which historical instrument first satisfies ''individual holds a non-physical instrument as a practical store of value'': prepaid phone cards (early 1990s), the Mondex trial (1995), PayPal balances (1999-2000), M-Pesa e-float (2007), or early bitcoin holdings (2009-2010)? And does ''practical'' require convertibility, durability, or scale?',
    'Archival reconstruction of the earliest stored-value instruments with documented individual holding behavior, operationalized as convertibility plus persistence plus holder discretion over when to spend.',
    'An earlier threshold pulls the origin toward the became_thinkable era and shrinks the implementation-barrier constraint set; a later threshold (M-Pesa/bitcoin) strengthens the access-exclusion victim structure and raises measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_held_threshold_event, empirical, 'Boundary ambiguity in the reading''s own threshold event.').

omega_variable(
    store_of_value_function_boundary,
    'Did early holders actually use non-physical instruments as stores of value (balance held across time) or only as transmission media (instant spend-through), and does the reading''s claim survive if typical holding periods were negligible?',
    'Transaction-level data on holding durations in early schemes: PayPal float residency times, M-Pesa e-float balance persistence, prepaid card dormancy statistics.',
    'If holding was transient, the store-of-value half of the claim weakens and the reading collapses toward a pure payments-adoption story, changing the victim structure from savings exclusion to transmission exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(store_of_value_function_boundary, empirical, 'Whether the store-of-value function was genuinely exercised by first holders.').

omega_variable(
    network_effect_rent_attribution,
    'How much of the advantage accruing to early adopters and operators is genuine network coordination surplus versus rent manufactured by proprietary closure of interoperable rails?',
    'Compare welfare and fee trajectories in interoperable versus closed mobile-money deployments (for example interoperability mandates in East African markets) against matched closed systems.',
    'If rents are manufactured by closure, effective extraction rises and the classification drifts toward snare; if the surplus is genuine coordination value, the rope-side share of the tangled rope is larger than the metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_rent_attribution, empirical, 'Attribution of early-adopter and operator gains between coordination surplus and artificial scarcity.').

omega_variable(
    exclusion_counterfactual_welfare,
    'Do those without infrastructure access bear net harm from the holding-gate arrangement, or is their position unchanged, with cash decline driven by separate forces such as merchant digitization and state de-cashing policy?',
    'Difference-in-differences on cash-infrastructure withdrawal (ATM and branch closures, cash-acceptance attrition) across regions with varying digital penetration, controlling for policy shocks.',
    'If harm is net, the victim declaration stands and extraction is real; if the unconnected are merely unaffected bystanders, the victim structure weakens and the constraint looks closer to a rope with distributional noise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_counterfactual_welfare, empirical, 'Counterfactual welfare of the excluded under the holding-gate arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1995, digital_money_origin__first_held_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement_basis(digi_tr_t1995, observed).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__first_held_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(digi_tr_t2000, observed).
narrative_ontology:measurement(digi_tr_t2005, digital_money_origin__first_held_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement_basis(digi_tr_t2005, observed).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__first_held_reading, theater_ratio, 2010, 0.31).
narrative_ontology:measurement_basis(digi_tr_t2010, observed).
narrative_ontology:measurement(digi_tr_t2015, digital_money_origin__first_held_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement_basis(digi_tr_t2015, observed).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__first_held_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement_basis(digi_tr_t2020, observed).
narrative_ontology:measurement(digi_tr_t2025, digital_money_origin__first_held_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(digi_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1995, digital_money_origin__first_held_reading, base_extractiveness, 1995, 0.34).
narrative_ontology:measurement_basis(digi_be_t1995, observed).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__first_held_reading, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement_basis(digi_be_t2000, observed).
narrative_ontology:measurement(digi_be_t2005, digital_money_origin__first_held_reading, base_extractiveness, 2005, 0.47).
narrative_ontology:measurement_basis(digi_be_t2005, observed).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__first_held_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement_basis(digi_be_t2010, observed).
narrative_ontology:measurement(digi_be_t2015, digital_money_origin__first_held_reading, base_extractiveness, 2015, 0.59).
narrative_ontology:measurement_basis(digi_be_t2015, observed).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__first_held_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement_basis(digi_be_t2020, observed).
narrative_ontology:measurement(digi_be_t2025, digital_money_origin__first_held_reading, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement_basis(digi_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1995, digital_money_origin__first_held_reading, suppression_requirement, 1995, 0.24).
narrative_ontology:measurement_basis(digi_su_t1995, observed).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__first_held_reading, suppression_requirement, 2000, 0.31).
narrative_ontology:measurement_basis(digi_su_t2000, observed).
narrative_ontology:measurement(digi_su_t2005, digital_money_origin__first_held_reading, suppression_requirement, 2005, 0.39).
narrative_ontology:measurement_basis(digi_su_t2005, observed).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__first_held_reading, suppression_requirement, 2010, 0.46).
narrative_ontology:measurement_basis(digi_su_t2010, observed).
narrative_ontology:measurement(digi_su_t2015, digital_money_origin__first_held_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement_basis(digi_su_t2015, observed).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__first_held_reading, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement_basis(digi_su_t2020, observed).
narrative_ontology:measurement(digi_su_t2025, digital_money_origin__first_held_reading, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement_basis(digi_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'when did digital money emerge' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle into three stories sharing the digital_money_origin kernel. Became_thinkable_reading is upstream (conception precedes and enables holding); this first_held_reading is midstream (adoption creates the facts); regulatory_recognition_reading is downstream (recognition follows and codifies adoption). This reading influences the regulatory sibling by generating the adoption facts that authorities later incorporate, and is enabled by the conception sibling's prior technical work. Each member carries its own epsilon, beneficiary/victim structure, and classification; no member adjudicates the others internally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__first_held_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
