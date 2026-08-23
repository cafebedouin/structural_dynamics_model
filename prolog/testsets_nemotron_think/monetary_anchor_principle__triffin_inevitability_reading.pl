% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__triffin_inevitability_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma: Gold Standard Reserve Currency Impossibility
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   The Triffin dilemma reading of the monetary anchor principle holds that a
 *   reserve currency issuer operating under a gold standard faces a
 *   structural contradiction: to supply global liquidity, it must run
 *   persistent balance-of-payments deficits, which inevitably drain its gold
 *   reserves until convertibility becomes unsustainable. This is not a policy
 *   failure but a mathematical necessity — the gold stock grows at ~2%
 *   annually while world trade and liquidity demand grow faster. The Bretton
 *   Woods system (1944–1971) instantiated this contradiction; its collapse
 *   was the dilemma's mechanical resolution. The constraint claims Mountain
 *   type: a physical/logical limit with no policy escape. Epsilon is very low
 *   (0.08) because the dilemma extracts nothing — it is a structural limit
 *   that destroys the arrangement containing it. The victim is the Bretton
 *   Woods framework itself, which could not survive the contradiction. No
 *   beneficiary exists; the system fails all participants.
 *
 * KEY AGENTS:
 *   - us_treasury_federal_reserve: Reserve currency issuer (institutional/constrained) — forced by dilemma to choose between domestic policy and gold convertibility
 *   - other_central_banks: Dollar reserve holders (organized/constrained) — accumulate dollar claims they cannot redeem without triggering collapse
 *   - bretton_woods_framework: The institutional arrangement (payer) — bears the terminal cost of the contradiction
 *   - imf_institutional_body: System administrator (institutional/constrained) — manages the regime but cannot alter the arithmetic
 *   - analytical_observer: Sees full structure (analytical/analytical) — identifies the dilemma as mathematical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.08).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.15).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma: Gold Standard Reserve Currency Impossibility").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '263a5b3b-1246-4d9b-9f86-f6379aa35ee9').
narrative_ontology:cs_kernel_codification('263a5b3b-1246-4d9b-9f86-f6379aa35ee9', formalized).
narrative_ontology:cs_authority_grounding('263a5b3b-1246-4d9b-9f86-f6379aa35ee9', lineage).
narrative_ontology:cs_interpretation_layer_present('263a5b3b-1246-4d9b-9f86-f6379aa35ee9').
narrative_ontology:cs_reading_relation('263a5b3b-1246-4d9b-9f86-f6379aa35ee9', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('263a5b3b-1246-4d9b-9f86-f6379aa35ee9', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('263a5b3b-1246-4d9b-9f86-f6379aa35ee9', foundational, triffin_dilemma_as_mathematical_necessity).
narrative_ontology:cs_axiom_status(triffin_dilemma_as_mathematical_necessity, holdable).
narrative_ontology:cs_axiom_grounding('263a5b3b-1246-4d9b-9f86-f6379aa35ee9', triffin_dilemma_as_mathematical_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('263a5b3b-1246-4d9b-9f86-f6379aa35ee9', bretton_woods_gold_exchange_standard).
narrative_ontology:cs_drift_state('263a5b3b-1246-4d9b-9f86-f6379aa35ee9', late_1960s_gold_drain, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('263a5b3b-1246-4d9b-9f86-f6379aa35ee9', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__triffin_inevitability_reading, private_financial_actors).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, us_treasury_federal_reserve).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, other_central_banks).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, triffin_dilemma).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, gold_standard_impossibility_theorem).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, reserve_currency_liquidity_tradeoff).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the reserve currency issuer, the US must run deficits to supply global dollar liquidity. This generates seigniorage benefits but structurally guarantees gold reserve depletion. The reserve currency role is identity-locked — abandoning it would mean relinquishing the exorbitant privilege and global monetary leadership. No exit exists that preserves the role; the dilemma is built into the role itself.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, us_treasury_federal_reserve, payer,
    institutional, generational, identity_locked, global).

% Hold dollar reserves as the foundation of their monetary systems. As US gold coverage declines, their reserves become claims on a shrinking gold stock. They can convert dollars to gold (accelerating collapse), hold dollars (accepting depreciation), or diversify (limited by dollar dominance). Their exit is constrained by the network effects of the dollar system.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, other_central_banks, payer,
    organized, biographical, constrained, global).

% The institutional arrangement itself — the fixed exchange rate system, gold convertibility at $35/oz, IMF surveillance. It bears the terminal cost of the Triffin contradiction: when US gold coverage falls below a credible threshold, the framework cannot survive. It has no agency to exit; it is the structure that the dilemma destroys.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_framework, payer,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_framework).

% Administers the Bretton Woods rules: approves parity changes, provides standby credits, monitors compliance. It manages the system but cannot alter the Triffin arithmetic. Its authority derives from the framework's legitimacy; as the dilemma intensifies, its policy tools (conditionality, surveillance) become irrelevant to the core contradiction.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, imf_institutional_body, agenda_setter,
    institutional, generational, constrained, global).

% Operate the Eurodollar market and offshore dollar creation, which partially bypasses the Triffin constraint by creating dollar liquidity outside the gold-backed system. They benefit from the system's expansion phase and have high exit mobility — they can shift booking centers and instruments. Their activity both relieves and masks the dilemma.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, private_financial_actors, beneficiary,
    powerful, biographical, mobile, global).

% Identifies the Triffin dilemma as a structural feature of any reserve currency system with a metallic anchor. Sees the Bretton Woods collapse not as a policy failure but as the working out of a mathematical necessity. The constraint appears as a Mountain from this seat — zero extraction, total accessibility collapse, no resistance.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system coordinated global trade and reconstruction by providing a stable dollar-gold anchor and an institutional framework (IMF) for managing imbalances. The Triffin dilemma is not a coordination function — it is the structural contradiction that made the coordination mechanism self-terminating.
% TRANSFER_FUNCTION: The dilemma transfers nothing — it is a stock-flow inconsistency. The gold standard regime transferred seigniorage to the US and adjustment costs to surplus countries, but the dilemma itself is the recognition that this transfer cannot continue indefinitely. The 'transfer' is the inevitable default on gold convertibility.
% ABSENT_VOICES: The populations of Global South countries whose development was constrained by the system's deflationary bias and IMF conditionality. They were not represented in the 1944 design and had no voice in the 1971 transition. The Triffin reading treats them as absent from the structural logic, but the overdetermined composite reading would include their experience as part of the system's illegitimacy.
% DISAPPEARANCE_RATIONALE: If the Triffin dilemma were recognized as a structural limit from the start (1944), the monetary anchor principle would have been designed differently — perhaps with a supranational reserve asset (Keynes's bancor), symmetric adjustment obligations, or no metallic anchor. The world monetary system would have been arranged around a viable coordination mechanism rather than one with a built-in expiration date.
% FOUNDING_PROBLEM: The Bretton Woods system was built to solve the interwar monetary chaos: competitive devaluations, trade collapse, and the absence of a stable international payment system after the gold standard's first failure. The founding problem was creating a credible anchor for fixed exchange rates that would support postwar reconstruction and trade liberalization.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Eichengreen, Kindleberger) and the IMF's own retrospective analyses confirm the reconstruction-era problem was solved by the late 1960s. The Triffin dilemma itself — identified by Triffin in 1960 — is the corroborating evidence that the system's success (global liquidity provision) created its failure condition. No beneficiary of the system disputes that the founding problem was resolved; they dispute whether the system could have been reformed rather than abandoned.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, ExtMetricName, E),
    domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.08) because the Triffin dilemma is not an extraction mechanism — it is a structural limit that terminates the arrangement. Suppression is low (0.15) because the dilemma itself does not coerce; the gold standard regime's enforcement machinery (capital controls, IMF conditionality) suppresses alternatives, but the dilemma is the reason those suppressions ultimately fail. Theater ratio is minimal (0.05) — there is no performative maintenance of the dilemma; it is a cold arithmetic fact. Accessibility collapse is extreme (0.92) — once the arithmetic is understood, no alternative gold-standard configuration escapes it (capital controls delay but do not resolve the stock-flow mismatch). Resistance is near-zero (0.08) — the constraint is not opposed; it is discovered. The measurement series tracks the dilemma's accumulating pressure over the Bretton Woods lifespan (1944–1971), with extractiveness rising only as the gold drain becomes visible.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer seat sees a Mountain (mathematical necessity). The US issuer seat experiences it as a trap with no exit (identity_locked by reserve currency role). Other central banks experience it as a slow-motion default on their reserves. The IMF experiences it as institutional obsolescence. The engine computes these as different effective extractions from the same base epsilon because directionality differs: the issuer is both beneficiary (seigniorage) and target (gold drain), other central banks are pure targets, the framework is the terminal victim.
 *
 * DIRECTIONALITY LOGIC:
 *   The US as reserve currency issuer has a dual position: it gains seigniorage (beneficiary) but faces inevitable gold exhaustion (target). The net directionality is near-symmetric (d ≈ 0.5) because the seigniorage is the mechanism that creates the drain. Other central banks are pure targets (d → 1.0) — they hold dollar claims that become unredeemable. The Bretton Woods framework as an institutional arrangement is the terminal victim (d → 1.0) — it ceases to exist. The IMF is a constrained administrator (d ≈ 0.7) — it manages a system it cannot fix. The analytical observer has d = 0.5 (analytical fallback). No agent has arbitrage-grade exit; the reserve currency role is identity-locked for the issuer.
 *
 * MANDATROPHY ANALYSIS:
 *   The Bretton Woods system was built to solve the interwar monetary chaos (founding problem: stable exchange rates for trade reconstruction). By the late 1960s, that problem was substantially solved (European recovery complete), but the system persisted because the Triffin dilemma had no exit. The mandate did not atrophy — the constraint itself made the arrangement impossible. This is not mandatrophy (a function outliving its purpose) but structural suicide (a function destroying its own container). The classification prevents mislabeling this as a Snare (extraction) or Piton (inertial persistence): it is a Mountain that the system walked into.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the monetary_anchor_principle kernel admit multiple structurally distinct readings, or is the Triffin inevitability reading the only structurally valid decomposition?',
    'Compare the epsilon values and beneficiary/victim structures across the three declared readings. If epsilon differs materially across readings for the same referent, the kernel decomposes into distinct constraints.',
    'If the kernel decomposes, each reading is a separate constraint story linked by network.affects_constraints. If not, the readings are perspectival variants of one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel''s contested readings map to one constraint or a constraint family.').

omega_variable(
    triffin_mechanism_uniqueness,
    'Is the Triffin dilemma (gold reserve scarcity vs. global liquidity demand) a logically necessary contradiction, or does it depend on contingent policy choices (e.g., sterilization, capital controls, SDR creation)?',
    'Historical counterfactual analysis: could the Bretton Woods system have persisted indefinitely with different policy configurations that relaxed the gold convertibility constraint while maintaining the anchor?',
    'If contingent, the constraint is not a Mountain but a Snare or Tangled Rope maintained by policy choices. If logically necessary, Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_mechanism_uniqueness, empirical, 'Whether the Triffin contradiction is mathematical necessity or policy-contingent.').

omega_variable(
    victim_identification_ambiguity,
    'Is the Bretton Woods institutional framework the proper victim, or are the victims the national economies and populations that experienced the transition costs?',
    'Trace the distributional consequences of the 1971 transition: who bore adjustment costs, who gained seigniorage, and whether the framework itself was an agent or a set of rules.',
    'If victims are national populations rather than the framework, the stakeholder structure changes and effective extraction may be non-zero for specific agent seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_identification_ambiguity, conceptual, 'Whether the victim is the institutional arrangement or the people governed by it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_tr_t0, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_tr_t5, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 5, 0.03).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_tr_t10, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_tr_t15, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 15, 0.04).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_tr_t20, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_tr_t25, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_tr_t27, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 27, 0.05).

% Extraction over time
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_be_t0, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_be_t5, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 5, 0.03).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_be_t10, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_be_t15, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_be_t20, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_be_t25, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 25, 0.07).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_be_t27, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 27, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_su_t0, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_su_t5, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_su_t10, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_su_t15, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_su_t20, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_su_t25, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement(monetary_anchor_principle__triffin_inevitability_reading_su_t27, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 27, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__triffin_inevitability_reading, 0.15).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_system).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, gold_standard_regime).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, dollar_reserve_currency_status).

% DUAL FORMULATION NOTE:
% The monetary_anchor_principle kernel decomposes into three constraint stories: this Triffin inevitability reading (Mountain, epsilon ~0.08), the punctuated_swap_reading (Snare or Scaffold, epsilon higher — a policy choice with beneficiaries), and the overdetermined_composite_reading (Tangled Rope, epsilon moderate — coordination function with extraction). They are linked because the Triffin mechanism is cited as evidence by the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__triffin_inevitability_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
