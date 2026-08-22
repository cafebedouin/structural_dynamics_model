% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Digital Money Origin — First Practical Holding Reading (Access-Gated Adoption Structure)
 *   domain: monetary history/technology studies/institutional economics
 *
 * SUMMARY:
 *   This story instantiates the first_held_reading of the
 *   digital_money_origin kernel: digital money is dated to the first
 *   practical holding of non-physical instruments as working stores of value
 *   (roughly 1990s electronic stored value through mobile money; interval t=0
 *   approximates 1990, t=35 approximates 2025). Under this reading the
 *   operative constraint set is the access gate — holding requires device,
 *   connectivity, documentation, and account standing — maintained by
 *   platform requirements and hardened by successive compliance regimes. The
 *   gate channels network-effect gains to early-connected holders while
 *   imposing exclusion costs on the unconnected. Family note: the colloquial
 *   question 'when did digital money emerge' decomposes into three
 *   structurally distinct claims — thinkability (upstream sibling), first
 *   holding (this file), regulatory recognition (downstream sibling) — each
 *   with its own epsilon, beneficiary structure, and victims, linked via
 *   network.affects_constraints. Claim/metric independence is preserved:
 *   claimed_type states my structural belief; the metrics describe observed
 *   operation and are not tuned to any predicted engine output.
 *
 * KEY AGENTS:
 *   - digital_payment_platform_operators: agenda-setter and collector (institutional/arbitrage) — runs the rails, sets account and fee terms, receives the fee and float flows
 *   - early_adopters_with_access: primary beneficiary (organized/mobile) — captures compounding network-effect gains
 *   - infrastructure_excluded_populations: primary target (powerless/trapped) — bears exclusion costs as cash acceptance declines
 *   - cash_dependent_small_merchants: dual-positioned bearer (moderate/constrained) — pays dual-stack costs, gains partially from digital reach
 *   - community_savings_groups: excluded voice (powerless/trapped) — informal arrangements that do not map onto account-based requirements
 *   - central_bank_payment_authorities: analytical observer (institutional/analytical) — monitors, licenses, and can mandate basic tiers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.62).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.61).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Origin — First Practical Holding Reading (Access-Gated Adoption Structure)").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary history/technology studies/institutional economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, 'b639dc68-758e-4a70-829c-16a40e85ae3c').
narrative_ontology:cs_kernel_codification('b639dc68-758e-4a70-829c-16a40e85ae3c', distributed).
narrative_ontology:cs_authority_grounding('b639dc68-758e-4a70-829c-16a40e85ae3c', distributed).
narrative_ontology:cs_reading_relation('b639dc68-758e-4a70-829c-16a40e85ae3c', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('b639dc68-758e-4a70-829c-16a40e85ae3c', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('b639dc68-758e-4a70-829c-16a40e85ae3c', foundational, material_holding_constitutes_monetary_existence).
narrative_ontology:cs_axiom_status(material_holding_constitutes_monetary_existence, holdable).
narrative_ontology:cs_axiom_grounding('b639dc68-758e-4a70-829c-16a40e85ae3c', material_holding_constitutes_monetary_existence, empirically_contingent).
narrative_ontology:cs_axiom('b639dc68-758e-4a70-829c-16a40e85ae3c', secondary, first_holders_capture_compounding_network_rents).
narrative_ontology:cs_axiom_status(first_holders_capture_compounding_network_rents, holdable).
narrative_ontology:cs_axiom_grounding('b639dc68-758e-4a70-829c-16a40e85ae3c', first_holders_capture_compounding_network_rents, empirically_contingent).
narrative_ontology:cs_reference_frame('b639dc68-758e-4a70-829c-16a40e85ae3c', materialist_use_periodization).
narrative_ontology:cs_drift_state('b639dc68-758e-4a70-829c-16a40e85ae3c', contemporary_fintech_historiography, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('b639dc68-758e-4a70-829c-16a40e85ae3c', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters_with_access).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, digital_payment_platform_operators).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, infrastructure_excluded_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, cash_dependent_small_merchants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, cash_dependent_small_merchants).
narrative_ontology:constraint_vindicates(digital_money_origin__first_held_reading, network_externalities_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the ledgers, apps, and card networks through which digital balances are held and moved. Set account-opening requirements, identity-documentation thresholds, and per-transaction fees; decide which merchants and agents connect to the rails. Income arrives as fees, float, and foreign-exchange spreads on volume. Exposure to any single jurisdiction's rules is limited by multi-country operations and the ability to reprice or relocate services.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, digital_payment_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, digital_payment_platform_operators, beneficiary).

% Urban, banked, device-owning households and businesses that adopted digital balances early. They transact at falling marginal cost, build reputational and credit histories inside the systems, and gain most as more counterparties join. Leaving a mature platform means forfeiting accumulated history and accepting slower, costlier settlement, so they stay and accumulate further advantage.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters_with_access, beneficiary,
    organized, biographical, mobile, global).

% Households without reliable electricity, smartphones, connectivity, identity documents, or nearby agents. They continue to hold value in cash and livestock, paying transport and theft risks; when forced through digital rails they pay agent fees and intermediary markups. Opening an account requires documents and minimum balances they often cannot meet, and cash acceptance around them is shrinking as merchants and landlords move online.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, infrastructure_excluded_populations, payer,
    powerless, immediate, trapped, regional).

% Market traders and corner shops serving mixed customer bases. They maintain two payment stacks — cash drawers with security costs and digital terminals with fixed fees — and absorb chargebacks and connectivity outages. Digital sales channels widen their reach, but fee schedules and settlement delays are set by parties they cannot negotiate with.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, cash_dependent_small_merchants, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, cash_dependent_small_merchants, beneficiary).

% Rotating savings and credit associations, village banks, and informal treasurers whose members pool cash weekly. Their rules, meeting rhythms, and social enforcement predate digital rails and do not map onto individual-account documentation requirements. When digitized wholesale they lose the visibility and flexibility that made them work; when ignored they watch formal products priced for people unlike their members.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, community_savings_groups, excluded,
    powerless, generational, trapped, regional).

% Monitor payment-system stability, publish inclusion statistics, license issuers, and set anti-money-laundering expectations. They can mandate basic-account tiers and cap fees, and several have piloted offline-capable public payment instruments. Their statistical frameworks and legal definitions are revised on legislative cycles that trail market practice.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, central_bank_payment_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__first_held_reading, digital_payment_platform_operators).
narrative_ontology:fixing_cost_class(digital_money_origin__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates exchange and value storage across distance and time without physical transfer: a shared record of claims replaces cash-in-hand, letting strangers settle remotely and households store value without vault or courier risk.
% TRANSFER_FUNCTION: Moves transaction fees, float income, and network-effect rents toward platform operators and early-connected holders; moves exclusion costs — cash-handling risk, intermediary premiums, shrinking acceptance — onto households and merchants without infrastructure access.
% ABSENT_VOICES: Unbanked and unconnected households, informal savings groups, and cash-dependent micro-merchants were absent when account requirements, documentation thresholds, and merchant fee schedules were standardized; their objections surface only retrospectively through inclusion statistics and remittance-cost studies.
% DISAPPEARANCE_RATIONALE: If the access-gated structure vanished overnight — universal frictionless holding — platform fee income would compress, early-adopter positional advantages would evaporate, remittance corridors would reprice, and remaining cash logistics would lose their subsidy; the digital economy's inclusion politics would reorganize around usage rather than access.
% FOUNDING_PROBLEM: Physical value handling was costly and dangerous: storing and moving cash exposed holders to theft, loss, and distance limits, and remote settlement required slow, expensive intermediaries.
% FOUNDING_PROBLEM_CORROBORATION: Central-bank payment-cost studies, World Bank Global Findex surveys, and merchant-association cash-handling cost audits attest the founding problem from outside the platform-operator and early-adopter beneficiary set; no corroborating source attests the problem is fully solved, because the excluded populations themselves document its persistence.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.62: fees on captive corridors are decoupled from marginal processing cost, and exclusion converts non-participation into payable intermediary markups; genuine utility to connected users caps it below pure-extraction levels. Suppression 0.61: documentation thresholds, de-risking account closures, merchant cash refusal, and branch/ATM withdrawal keep the cash alternative costly — structural, not total, since cash remains lawful. Theater 0.40: financial-inclusion rhetoric outruns delivered access, rising as inclusion marketing grew and easing slightly as basic-tier mandates landed. Accessibility_collapse 0.48: alternatives persist but degrade as acceptance shrinks. Resistance 0.45: cash-preference campaigns, privacy litigation, and merchant fee revolts, with some successful fee caps. All three series share one time grid (t=0..35) so every metric is authored at every examined point. The suppression_requirement series is authored deliberately: enforcement capacity visibly changed over the interval — the post-2001 AML ratchet, post-2015 de-risking, pandemic-era acceleration, then partial proportionality rebalancing — so this is a tracked enforcement trajectory, not a static picture. The trajectory is drift with one inflection, not a cycle; no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The operator seat experiences the arrangement as infrastructure it built and prices — coordination-forward, with fees as the cost of maintaining trust and uptime. Early adopters experience earned convenience that compounds. The excluded households and cash-only merchants experience the same rails as a toll gate they cannot afford to pass, with the gate tightening as acceptance networks shrink. These seats will compute differently not because they disagree about facts but because exit options (arbitrage versus trapped) and directional position differ; the engine derives the divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: platform operators (agenda_setter plus beneficiary, arbitrage exit) sit near the beneficiary pole; early adopters (beneficiary, mobile exit) sit low but above the operators since they pay fees; infrastructure-excluded populations (victim, trapped, place-bound) sit near the full-target pole, amplified by their inability to route around the gate; community savings groups sit at the target pole but generate no extraction flow, contributing exclusion pressure rather than receipts. One override is declared: cash_dependent_small_merchants are listed among victims because their net position is negative, but the derivation from the victim declaration alone would overshoot — their secondary benefit (digital sales reach) and constrained-but-real exit place them nearer symmetric, hence moderate power atom overridden to d=0.55.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope guards against two opposite mislabels. Reading the arrangement as pure rope would erase the documented exclusion costs, fee asymmetries, and compliance-driven closure of alternatives; reading it as snare would erase the real coordination surplus — remote settlement, loss-resistant storage — that even many excluded households actively seek. The founding problem remains live (solved for the connected, unsolved for the excluded), so no mandatrophy resolution is declared; the status=live x verdict=world_rearranges pairing is consistent with a functioning hybrid rather than a zombie mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of kernel digital_money_origin (reading: first_held_reading). Which periodization criterion should fix the origin, and how would adopting a sibling reading restructure the constraint?',
    'Comparative classification across the three reading-stories in the family; per-seat engine outputs under each criterion reveal which structural features are criterion-relative and which are invariant.',
    'Adopting became_thinkable_reading moves the origin earlier, shrinks the early-adopter cohort, and shifts beneficiaries toward the institutions that made the concept actionable; adopting regulatory_recognition_reading moves the origin later, installs monetary authorities as agenda-setters, and replaces access-barrier victims with formal-recognition laggards.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer frame: this story is the first-held reading of the digital-money-origin kernel; sibling readings instantiate different constraints.').

omega_variable(
    practical_holding_boundary,
    'What counts as ''held as a practical store of value''? Stored-value transit cards, prepaid mobile airtime, in-game balances, and bank ledger deposits are boundary cases that move the origin date by decades.',
    'Historiographic adjudication on functional criteria: unit persistence across sessions, convertibility at par, use in arm''s-length exchange independent of the issuing venue.',
    'Broader boundaries push the origin earlier, shrink the early-adopter rent window, and thin the victim set; narrower boundaries do the reverse and concentrate both beneficiary gains and exclusion costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_holding_boundary, conceptual, 'Category-boundary indeterminacy in the ''practical holding'' criterion.').

omega_variable(
    network_rent_attribution,
    'How much of early adopters'' measured gain is coordination surplus that latecomers could eventually share, versus positional rent that depends on latecomers'' continued exclusion?',
    'Quasi-experimental comparison of staggered mobile-money rollouts across regions; difference-in-differences on household welfare and transaction-cost trajectories.',
    'A high positional-rent share supports the asymmetric-extraction component and tighter interoperability mandates; a high surplus share supports a coordination-first reading with lower effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_rent_attribution, empirical, 'Decomposition of early-adopter gains into surplus versus positional rent.').

omega_variable(
    exclusion_cost_magnitude,
    'Do infrastructure-excluded households bear net costs from the digital shift, or do they self-select out of services they do not value?',
    'Panel data linking cash-acceptance decline to household transaction costs; remittance-price comparisons across corridors before and after digitization.',
    'Net-harm findings raise effective extraction on the trapped seat and strengthen the victim declarations; self-selection findings lower it and soften the extraction-side reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_cost_magnitude, empirical, 'Whether exclusion imposes net harm or reflects revealed preference.').

omega_variable(
    cash_suppression_source,
    'Is the declining viability of cash structural (merchant refusal, branch and ATM withdrawal, fee schedules) or adaptive (holder preference following demonstrated convenience)?',
    'Natural experiments where jurisdictions guaranteed cash acceptance or mandated ATM presence: if cash use recovers when structural barriers lift, the suppression was structural.',
    'Structural findings raise the suppression measure and support an enforcement-maintained reading; adaptive findings lower it and reweight the arrangement toward voluntary adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cash_suppression_source, empirical, 'Structural versus adaptive source of the cash alternative''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(first_held_origin_tr_t0, digital_money_origin__first_held_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(first_held_origin_tr_t0, observed).
narrative_ontology:measurement(first_held_origin_tr_t5, digital_money_origin__first_held_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(first_held_origin_tr_t5, observed).
narrative_ontology:measurement(first_held_origin_tr_t10, digital_money_origin__first_held_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(first_held_origin_tr_t10, observed).
narrative_ontology:measurement(first_held_origin_tr_t15, digital_money_origin__first_held_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(first_held_origin_tr_t15, observed).
narrative_ontology:measurement(first_held_origin_tr_t20, digital_money_origin__first_held_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(first_held_origin_tr_t20, observed).
narrative_ontology:measurement(first_held_origin_tr_t25, digital_money_origin__first_held_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(first_held_origin_tr_t25, observed).
narrative_ontology:measurement(first_held_origin_tr_t30, digital_money_origin__first_held_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(first_held_origin_tr_t30, observed).
narrative_ontology:measurement(first_held_origin_tr_t35, digital_money_origin__first_held_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement_basis(first_held_origin_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(first_held_origin_be_t0, digital_money_origin__first_held_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(first_held_origin_be_t0, observed).
narrative_ontology:measurement(first_held_origin_be_t5, digital_money_origin__first_held_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement_basis(first_held_origin_be_t5, observed).
narrative_ontology:measurement(first_held_origin_be_t10, digital_money_origin__first_held_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(first_held_origin_be_t10, observed).
narrative_ontology:measurement(first_held_origin_be_t15, digital_money_origin__first_held_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement_basis(first_held_origin_be_t15, observed).
narrative_ontology:measurement(first_held_origin_be_t20, digital_money_origin__first_held_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(first_held_origin_be_t20, observed).
narrative_ontology:measurement(first_held_origin_be_t25, digital_money_origin__first_held_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(first_held_origin_be_t25, observed).
narrative_ontology:measurement(first_held_origin_be_t30, digital_money_origin__first_held_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(first_held_origin_be_t30, observed).
narrative_ontology:measurement(first_held_origin_be_t35, digital_money_origin__first_held_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(first_held_origin_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(first_held_origin_su_t0, digital_money_origin__first_held_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(first_held_origin_su_t0, observed).
narrative_ontology:measurement(first_held_origin_su_t5, digital_money_origin__first_held_reading, suppression_requirement, 5, 0.24).
narrative_ontology:measurement_basis(first_held_origin_su_t5, observed).
narrative_ontology:measurement(first_held_origin_su_t10, digital_money_origin__first_held_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(first_held_origin_su_t10, observed).
narrative_ontology:measurement(first_held_origin_su_t15, digital_money_origin__first_held_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(first_held_origin_su_t15, observed).
narrative_ontology:measurement(first_held_origin_su_t20, digital_money_origin__first_held_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(first_held_origin_su_t20, observed).
narrative_ontology:measurement(first_held_origin_su_t25, digital_money_origin__first_held_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(first_held_origin_su_t25, observed).
narrative_ontology:measurement(first_held_origin_su_t30, digital_money_origin__first_held_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement_basis(first_held_origin_su_t30, observed).
narrative_ontology:measurement(first_held_origin_su_t35, digital_money_origin__first_held_reading, suppression_requirement, 35, 0.61).
narrative_ontology:measurement_basis(first_held_origin_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel digital_money_origin. The colloquial label 'when did digital money emerge' conflates three structurally distinct claims: conceivability (became_thinkable_reading — earliest date, institution-facing beneficiaries), first practical holding (this file — intermediate date, access-gated beneficiary/victim structure), and formal recognition (regulatory_recognition_reading — latest date, authority-centered structure). Each member carries its own epsilon, beneficiaries, and victims; this upstream practice-fact reading influences the downstream recognition reading because recognition responds to holding that has already occurred. All three files cross-link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__first_held_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
