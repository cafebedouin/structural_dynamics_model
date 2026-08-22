% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods External Discipline Regime (Sovereignty-Defense Reading)
 *   domain: economic/international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_defense reading of the Bretton
 *   Woods treaty kernel: the Articles of Agreement and their operating
 *   machinery (the dollar-gold anchor, adjustable parities, IMF
 *   conditionality) are assessed as a standing arrangement in which external
 *   monetary discipline is imposed on all members except the reserve issuer.
 *   The coordination function is real — fixed parities, Fund liquidity, and
 *   the suppression of competitive devaluation underwrote the postwar trade
 *   expansion — but the adjustment burden is asymmetric: non-reserve states
 *   deflate and submit to creditor-set conditions while the anchor issuer
 *   finances deficits in its own currency, and the gold anchor, presented as
 *   a symmetric stabilizer, operates increasingly as performance before the
 *   issuer suspends it unilaterally in 1971. KEY AGENTS (by structural
 *   relationship): - us_reserve_issuer: Agenda-setter and primary beneficiary
 *   (institutional/arbitrage) — issues the anchor currency, dominates Fund
 *   governance, exits by rule change in 1971 - non_reserve_currency_states:
 *   Primary targets (moderate/constrained) — defend parities, deflate on
 *   demand, adjust only with approval - imf_conditionality_recipients: Acute
 *   targets (moderate/trapped) — accept policy conditions for Fund credit -
 *   surplus_creditor_states: Secondary beneficiaries (powerful/mobile) —
 *   export platform and reserve accumulation; import issuer inflation late in
 *   the interval - us_multinational_corporations: Beneficiaries
 *   (organized/mobile) — acquire abroad with peg-sustained dollars -
 *   deficit_state_workforces: Bearing parties with no seat
 *   (powerless/trapped) — absorb the deflation -
 *   symmetric_adjustment_advocates: Excluded voices (organized/trapped) —
 *   bancor and Triffin reforms never adopted -
 *   imf_conditionality_administrators: Enforcement organ
 *   (institutional/constrained) — administers surveillance and standbys under
 *   creditor-weighted voting - monetary_economists: Analytical observers
 *   (analytical/analytical) — diagnose the asymmetry (Triffin) without votes
 *   Family note: the label 'Bretton Woods' decomposes into three readings of
 *   one treaty kernel; this story's epsilon (0.72 at interval end) is
 *   authored over the sovereignty-defense referent — the standing discipline
 *   arrangement as it operated — and differs from the sibling stories'
 *   epsilon by construction, since each reading has a different victim set
 *   and beneficiary structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.72).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.58).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods External Discipline Regime (Sovereignty-Defense Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "economic/international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, 'b75af746-9596-4710-ab0a-f8fabbb1457c').
narrative_ontology:cs_kernel_codification('b75af746-9596-4710-ab0a-f8fabbb1457c', formalized).
narrative_ontology:cs_authority_grounding('b75af746-9596-4710-ab0a-f8fabbb1457c', extraction).
narrative_ontology:cs_interpretation_layer_present('b75af746-9596-4710-ab0a-f8fabbb1457c').
narrative_ontology:cs_reading_relation('b75af746-9596-4710-ab0a-f8fabbb1457c', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, influences).
narrative_ontology:cs_reading_relation('b75af746-9596-4710-ab0a-f8fabbb1457c', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_axiom('b75af746-9596-4710-ab0a-f8fabbb1457c', foundational, national_monetary_sovereignty_priority).
narrative_ontology:cs_axiom_status(national_monetary_sovereignty_priority, holdable).
narrative_ontology:cs_axiom_grounding('b75af746-9596-4710-ab0a-f8fabbb1457c', national_monetary_sovereignty_priority, deontological).
narrative_ontology:cs_axiom('b75af746-9596-4710-ab0a-f8fabbb1457c', secondary, issuer_discipline_symmetry_required).
narrative_ontology:cs_axiom_status(issuer_discipline_symmetry_required, holdable).
narrative_ontology:cs_axiom_grounding('b75af746-9596-4710-ab0a-f8fabbb1457c', issuer_discipline_symmetry_required, instrumental).
narrative_ontology:cs_reference_frame('b75af746-9596-4710-ab0a-f8fabbb1457c', symmetric_sovereignty_within_parities).
narrative_ontology:cs_drift_state('b75af746-9596-4710-ab0a-f8fabbb1457c', post_nixon_suspension, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b75af746-9596-4710-ab0a-f8fabbb1457c', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, us_reserve_issuer).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, surplus_creditor_states).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, us_multinational_corporations).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, imf_conditionality_recipients).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, deficit_state_workforces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, surplus_creditor_states).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, dollar_standard_viability).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, imf_conditionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the reserve currency to which every other parity is anchored and converts dollars to gold for official holders at thirty-five dollars an ounce. Its treasury and central bank set the anchor's operating rules, hold effective veto over Fund decisions through quota weight, and decide in practice whether the anchor holds at all. Its external deficits are financed by its own liabilities held as reserves abroad, and in 1971 it suspends gold convertibility by televised announcement. Its exit from the arrangement is a change to the rules everyone else lives under.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, us_reserve_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, us_reserve_issuer, beneficiary).

% Declare and defend parities against the dollar, finance external deficits from reserves, and must obtain Fund approval before changing parity. When reserves run low they deflate domestic demand or borrow under conditions. Their external accounts are subject to approval and discipline; the anchor issuer's is not. Leaving means floating alone, losing access to Fund credit, and disrupting the trade finance their exporters rely on.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    moderate, biographical, constrained, national).

% Draw Fund credit in external crises — Britain in 1947 and 1967, and the recurring standby borrowers among smaller deficit economies — and accept negotiated policy conditions: credit restraint, budget cuts, and scheduled liberalization steps. Once a standby is drawn, abandoning it mid-crisis means losing the credit line and the confidence it signals to bondholders, so the conditions are honored even where they are contested.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf_conditionality_recipients, payer,
    moderate, biographical, trapped, national).

% Run persistent surpluses under parities that favor their exporters, accumulate dollar reserves, and enjoy stable exchange rates for trade. Late in the interval they import the anchor issuer's inflation through the peg and hold growing claims whose gold value depends on a convertibility promise the issuer struggles to keep; they revalue under protest (Germany in 1969 and 1971) or accumulate further.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, surplus_creditor_states, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, surplus_creditor_states, payer).

% Acquire foreign firms and finance foreign operations with dollars the peg structure holds above what a floating rate would set, and move profits through the same umbrella. They benefit from the liquidity and confidence the system provides and lobby actively in United States payments policy, including the interest equalization tax debates of the 1960s.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, us_multinational_corporations, beneficiary,
    organized, biographical, mobile, global).

% Bear the domestic side of parity defense and Fund conditions: wage restraint, hiring freezes, and unemployment when governments deflate to hold a parity or satisfy a standby. They hold no seat in parity decisions or Fund negotiations; their governments negotiate on their behalf and present the resulting austerity as unavoidable.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, deficit_state_workforces, payer,
    powerless, biographical, trapped, national).

% Proposed symmetric adjustment at the design stage — Keynes's clearing union with a bancor reserve and automatic penalties on creditor and debtor alike — and later reform programs in the Triffin and Bellagio tradition: a synthetic reserve asset, wider bands, a revised gold price. Outvoted at Bretton Woods in 1944 and outmaneuvered in the 1960s reform debates; their proposals are on the record and never adopted.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, symmetric_adjustment_advocates, excluded,
    organized, generational, trapped, global).

% The Fund's staff and Executive Board run surveillance, approve parity changes, and negotiate standby conditions with borrowing members. Voting weight follows quotas, which concentrate effective control among creditors, with the anchor issuer holding veto power over major decisions. Its leverage is the liquidity only it can provide deficit members; its standing rests on the Articles it administers, which it interprets but cannot formally revise without member consent.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf_conditionality_administrators, agenda_setter,
    institutional, generational, constrained, global).

% Diagnose the system's mechanics from outside the negotiating rooms: Triffin's congressional testimony on the reserve dilemma, the Bellagio Group's reform papers, and the academic literature on adjustment asymmetry. They can name the structure and its arithmetic precisely but command no votes; their influence runs through the reform debates they inform.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__sovereignty_defense, us_reserve_issuer).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__sovereignty_defense, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common external monetary framework for the non-communist trading world: fixed-but-adjustable parities anchored to the dollar, official dollar-gold convertibility, and a Fund liquidity backstop, solving the interwar problems of competitive devaluation, unstable exchange rates, and trade collapse without restoring the classical gold standard's automatic deflationary discipline.
% TRANSFER_FUNCTION: Moves external adjustment costs and reserve seigniorage from deficit member states toward the reserve issuer: non-anchor states deflate, devalue only with approval, and accept conditions for credit, while the anchor issuer finances its deficits in its own currency, which others must hold as reserves; late in the interval it also moves the anchor's collapse costs onto reserve holders by suspending convertibility.
% ABSENT_VOICES: Advocates of symmetric adjustment were excluded at both ends of the interval — the bancor clearing-union faction at design, the Triffin and Bellagio reformers later. Borrowing governments had voice only before a creditor-weighted board; workforces bearing deflation had no seat at all. The broad assent to the Articles in 1944 arose in a room from which the symmetric-adjustment position had already been negotiated out.
% DISAPPEARANCE_RATIONALE: If the parity system and its Fund machinery vanished overnight, exchange rates would float and trade finance would reorganize around currency risk within months, the anchor issuer would lose the channel that finances its deficits with its own liabilities, deficit states would lose their crisis backstop, and gold and reserve markets would reprice — the postwar trading order was organized around these parities and would not survive their silent removal.
% FOUNDING_PROBLEM: The interwar monetary chaos: competitive devaluation, exchange control, collapsed trade volumes, and a classical gold standard whose adjustment discipline fell almost entirely on deficit countries — the arrangement was built to keep exchange rates stable for trade while subordinating external discipline to national policy autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Conference records and both delegations' papers corroborate that interwar chaos was the named founding problem, and the post-collapse economic-history literature — written outside the benefiting parties — corroborates that the founding problem was substantially solved by the mid-1950s while the arrangement persisted. Triffin's 1959-1960 congressional testimony and the Bellagio Group papers corroborate from outside the beneficiary set that the adjustment asymmetry was structural rather than incidental. United States Treasury statements of the era are the principal voice attesting the problem remained live, and they are not disinterested.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects an adjustment burden decoupled from contribution: the anchor issuer's deficits are financed by liabilities others must hold, non-anchor states deflate and submit to conditions, and the interval ends with the issuer suspending the anchor unilaterally and leaving holders with the loss. Suppression (0.58) is authored at the arrangement's mature-phase characteristic — parity obligations enforceable through Fund approval, conditionality negotiated before a creditor-weighted board, governance the issuer can veto — because the standing arrangement this story is about is the operating regime, not its wreckage; the terminal decay is carried by the measurement series instead. Theater ratio (0.48) tracks the anchor's decline into performance: the London Gold Pool's defense of thirty-five dollars an ounce and the 'temporary' framing of the 1971 suspension were increasingly ceremonial, while Fund lending remained functionally real, keeping the ratio well below piton territory. Accessibility collapse (0.5): floating (Canada, 1950-62) and staying outside the Fund were real but costly alternatives, so alternatives are degraded, not eliminated. Resistance (0.55): the French gold conversions and de Gaulle's 1965 challenge, sterling's years-long resistance to devaluation, the German revaluation fights, and the Triffin critique were sustained and public, yet no member overturned the structure from inside — the system ended by the agenda-setter's own act. The three measurement series share one time grid (1944, 1949, 1953, 1958, 1962, 1967, 1971) so every metric is authored at every examined point. The suppression_requirement series is a deliberate rise-then-fall and is authored because enforcement-capacity change is central to this story: conditionality machinery ratchets up through the 1950s and 1960s, then the anchor's own enforcement collapses in 1968-71. Claim and metrics are independent authored facts: tangled_rope is claimed from structure (a real coordination function carrying an asymmetric, actively enforced burden); the metrics describe operation as the historical record shows it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute differently from the same Articles. From the issuer's position the arrangement is a coordination order it designed, financed in its early decades, and can exit at will — its exit option is a rule change, exercised in 1971. From the non-anchor payer seats the same structure is a discipline regime they did not set and cannot amend, with adjustment approval held by a board they cannot outvote. Surplus creditors hold a third seat: a good bargain that sours as imported inflation accumulates and their claims' anchor weakens. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The reserve issuer sits near the beneficiary pole: it collects seigniorage, externalizes adjustment, and its arbitrage-grade exit (rule change) pushes it further toward the beneficiary end. Non-reserve states and conditionality recipients sit near the target pole, the latter amplified by trapped exit once a standby is drawn. Workforces are targets with no exit at all. Surplus creditors are declared beneficiaries with mobile exit, which derives a low directionality; their late-interval position (imported inflation, weakening anchor) makes them less than pure beneficiaries, but no override is authored because the schema's overrides are power-atom-scoped and the issuer and the Fund administrators share the institutional atom with opposite structural relationships — a power-atom override here would misfire across both seats. Fund administrators carry no beneficiary or victim declaration; their seat is the enforcement organ, structurally near-symmetric, and the canonical fallback governs. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the arrangement's global scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar monetary chaos — was substantially solved by the mid-1950s, and the arrangement persisted for another two decades while the asymmetry accumulated; the base_extractiveness series shows that accumulation (0.28 to 0.72). The tangled_rope classification is what prevents both mislabelings: a pure-snare reading would erase why more than thirty states joined and stayed (the coordination function was real — stable parities and Fund liquidity underwrote the trade expansion), and a pure-rope reading would erase who paid for the stability and who could opt out of paying. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): the problem the arrangement was built for is disputed as live precisely because the arrangement's disappearance would resurrect a version of it — the signature of a coordination structure whose mandate has drifted rather than died.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading of the bretton_woods_treaty_substrate kernel; the Articles instantiate a different constraint under each sibling reading — what changes structurally if a sibling is instantiated instead?',
    'Author the sibling stories (keynesian_embedded_liberalism, neoliberal_convertibility) with their own beneficiary/victim sets and epsilon, then compare the three classifications against the same historical record.',
    'Under keynesian_embedded_liberalism the victim set shifts toward capital owners and beneficiaries toward national governments seeking policy space; under neoliberal_convertibility victims shift toward states and beneficiaries toward capital markets. This reading''s structure — non-reserve states as victims, the reserve issuer as beneficiary via exorbitant privilege, the anchor as performance — exists only under sovereignty_defense; the disagreement is located in which binding cost the treaty machinery imposes on whom.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which constraint the Bretton Woods Articles instantiate is reading-indexed; this file fixes the sovereignty_defense instantiation.').

omega_variable(
    exorbitant_privilege_magnitude,
    'How much of the system''s adjustment burden did the anchor issuer actually externalize, and how much did it bear in the early decades (dollar-shortage-era liquidity provision, aid outflows)?',
    'Country-level balance-of-payments and reserve-accumulation accounting across the interval, with a floating-rate counterfactual for the issuer''s financing costs.',
    'If early-period extraction is near zero, the constraint is a rope that degraded into a tangled rope and epsilon is accumulated rather than intrinsic; if the asymmetry is present from adoption, intrinsic epsilon is high from the first measurement and the drift story is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exorbitant_privilege_magnitude, empirical, 'Magnitude and timing of the issuer''s externalized adjustment burden.').

omega_variable(
    sovereignty_preserved_or_substituted,
    'Did the arrangement preserve national monetary sovereignty for non-reserve states relative to the classical gold standard, or substitute discretionary creditor discipline for automatic gold discipline under a new name?',
    'Compare policy-autonomy outcomes across regimes: domestic deflation episodes, parity-change approval rates, and conditionality incidence for members under Bretton Woods against gold-standard-era and floating-era baselines.',
    'If sovereignty was genuinely preserved, the coordination claim is real and the rope component is strong; if discipline was merely re-labeled and re-routed through creditor discretion, the coordination story is cover and the classification leans toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_preserved_or_substituted, empirical, 'Whether the sovereignty-preservation coordination claim survives comparison with adjacent regimes.').

omega_variable(
    gold_anchor_stabilizer_status,
    'Was the dollar-gold anchor ever a symmetric stabilizer, or was it asymmetric from adoption given the bancor rejection and the issuer''s veto over revision?',
    'Archival study of the 1944 negotiations and the 1956-1968 reform debates, assessing whether any symmetric design was ratifiable given the issuer''s domestic constraints.',
    'If symmetric design was infeasible from the start, the asymmetry is a founding condition rather than a degradation, and the reading''s verdict that the anchor became a snare applies to the whole interval rather than its second half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_anchor_stabilizer_status, conceptual, 'Whether the anchor''s asymmetry was intrinsic to the design or accumulated in operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bretton_woods_sovdef_tr_t0, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(bretton_woods_sovdef_tr_t0, observed).
narrative_ontology:measurement(bretton_woods_sovdef_tr_t5, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(bretton_woods_sovdef_tr_t5, observed).
narrative_ontology:measurement(bretton_woods_sovdef_tr_t9, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 9, 0.16).
narrative_ontology:measurement_basis(bretton_woods_sovdef_tr_t9, observed).
narrative_ontology:measurement(bretton_woods_sovdef_tr_t14, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 14, 0.22).
narrative_ontology:measurement_basis(bretton_woods_sovdef_tr_t14, observed).
narrative_ontology:measurement(bretton_woods_sovdef_tr_t18, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 18, 0.3).
narrative_ontology:measurement_basis(bretton_woods_sovdef_tr_t18, observed).
narrative_ontology:measurement(bretton_woods_sovdef_tr_t23, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 23, 0.38).
narrative_ontology:measurement_basis(bretton_woods_sovdef_tr_t23, observed).
narrative_ontology:measurement(bretton_woods_sovdef_tr_t27, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 27, 0.48).
narrative_ontology:measurement_basis(bretton_woods_sovdef_tr_t27, observed).

% Extraction over time
narrative_ontology:measurement(bretton_woods_sovdef_be_t0, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(bretton_woods_sovdef_be_t0, observed).
narrative_ontology:measurement(bretton_woods_sovdef_be_t5, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 5, 0.33).
narrative_ontology:measurement_basis(bretton_woods_sovdef_be_t5, observed).
narrative_ontology:measurement(bretton_woods_sovdef_be_t9, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 9, 0.4).
narrative_ontology:measurement_basis(bretton_woods_sovdef_be_t9, observed).
narrative_ontology:measurement(bretton_woods_sovdef_be_t14, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 14, 0.48).
narrative_ontology:measurement_basis(bretton_woods_sovdef_be_t14, observed).
narrative_ontology:measurement(bretton_woods_sovdef_be_t18, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 18, 0.55).
narrative_ontology:measurement_basis(bretton_woods_sovdef_be_t18, observed).
narrative_ontology:measurement(bretton_woods_sovdef_be_t23, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 23, 0.64).
narrative_ontology:measurement_basis(bretton_woods_sovdef_be_t23, observed).
narrative_ontology:measurement(bretton_woods_sovdef_be_t27, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 27, 0.72).
narrative_ontology:measurement_basis(bretton_woods_sovdef_be_t27, observed).

% Suppression requirement over time
narrative_ontology:measurement(bretton_woods_sovdef_su_t0, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(bretton_woods_sovdef_su_t0, observed).
narrative_ontology:measurement(bretton_woods_sovdef_su_t5, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(bretton_woods_sovdef_su_t5, observed).
narrative_ontology:measurement(bretton_woods_sovdef_su_t9, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 9, 0.55).
narrative_ontology:measurement_basis(bretton_woods_sovdef_su_t9, observed).
narrative_ontology:measurement(bretton_woods_sovdef_su_t14, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 14, 0.62).
narrative_ontology:measurement_basis(bretton_woods_sovdef_su_t14, observed).
narrative_ontology:measurement(bretton_woods_sovdef_su_t18, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 18, 0.65).
narrative_ontology:measurement_basis(bretton_woods_sovdef_su_t18, observed).
narrative_ontology:measurement(bretton_woods_sovdef_su_t23, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 23, 0.6).
narrative_ontology:measurement_basis(bretton_woods_sovdef_su_t23, observed).
narrative_ontology:measurement(bretton_woods_sovdef_su_t27, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 27, 0.4).
narrative_ontology:measurement_basis(bretton_woods_sovdef_su_t27, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, resource_allocation).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% The natural-language label 'the Bretton Woods system' covers at least three structurally distinct claims, which the epsilon-invariance principle requires be authored as separate stories of one treaty kernel: this sovereignty_defense reading (external discipline imposed asymmetrically; epsilon high and rising; non-reserve states as victims, the reserve issuer as beneficiary), the keynesian_embedded_liberalism reading (capital controls protecting domestic policy space; a different victim set), and the neoliberal_convertibility reading (intervention constrained to enable capital markets; again a different victim set). The readings share the Articles as referent but differ in which binding cost constitutes the constraint; their epsilon values differ by construction and each is stable within its own story. The downstream pressure runs from this reading into the embedded-liberalism sibling: documenting the adjustment asymmetry weakens the claim that the arrangement protected all members' policy space equally, without logically eliminating that reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
