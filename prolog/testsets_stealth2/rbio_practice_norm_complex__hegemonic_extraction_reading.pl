% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: Rules-Based International Order as Frozen Hegemonic Project (Hegemonic Extraction Reading)
 *   domain: international relations/international law/political economy
 *
 * SUMMARY:
 *   This story instantiates the hegemonic_extraction_reading of the kernel
 *   rbio_practice_norm_complex: the postwar rules-based order as a formally
 *   universal, consent-based arrangement whose revision channels are
 *   practically sealed by the P5 veto and institutional path dependency, and
 *   whose enforcement selects by enforcer interest. On this reading the
 *   standing arrangement under contest — the referent for epsilon — is an
 *   order that delivers real coordination goods (trade predictability,
 *   dispute fora, contract enforceability) while transferring policy autonomy
 *   and wealth from Global South debtor states and their populations to US
 *   and European capital, with conditionality functioning as contract signed
 *   under duress. The claim and the metrics are independent authored facts:
 *   claimed_type records the structure this reading believes true (hybrid
 *   coordination plus asymmetric extraction, actively enforced), while the
 *   metric values record the operation this reading observes. Sibling
 *   readings of the same kernel — liberal_institutional_reading and
 *   sovereignty_maximalist_reading — are separate constraint files with their
 *   own epsilon, victim sets, and classifications; nothing about them is
 *   averaged into this one.
 *
 * KEY AGENTS:
 *   - us_european_capital: Primary beneficiary (institutional/arbitrage) — collects enforceable rights, market access, and clearing privileges; exit is frictionless
 *   - p5_permanent_states: Agenda setter and veto-rent collector (institutional/arbitrage) — administers, selectively enforces, and seals revision
 *   - imf_world_bank_conditionality_administrators: Enforcement arm (institutional/identity_locked) — designs and polices adjustment conditionality; professionally fused with the paradigm
 *   - global_south_debtor_states: Primary target (organized/constrained) — signs under distress, votes without deciding, exit penalized
 *   - structural_adjustment_populations: Diffuse target (powerless/trapped) — bears austerity with no procedural seat
 *   - g77_reform_coalition: Excluded reformer (organized/constrained) — majoritarian proposals die in sealed channels
 *   - brics_parallel_institutions: Excluded alternative-builder (institutional/mobile) — accumulating outside options at the margins
 *   - critical_intl_law_scholarship: Analytical observer (analytical/analytical) — documents asymmetry, holds no enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.72).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "Rules-Based International Order as Frozen Hegemonic Project (Hegemonic Extraction Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international relations/international law/political economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, '67d24ff5-7774-419e-8e5a-af6a542e9a0e').
narrative_ontology:cs_kernel_codification('67d24ff5-7774-419e-8e5a-af6a542e9a0e', fixed_text).
narrative_ontology:cs_authority_grounding('67d24ff5-7774-419e-8e5a-af6a542e9a0e', extraction).
narrative_ontology:cs_interpretation_layer_present('67d24ff5-7774-419e-8e5a-af6a542e9a0e').
narrative_ontology:cs_reading_relation('67d24ff5-7774-419e-8e5a-af6a542e9a0e', rbio_practice_norm_complex__liberal_institutional_reading, influences).
narrative_ontology:cs_reading_relation('67d24ff5-7774-419e-8e5a-af6a542e9a0e', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('67d24ff5-7774-419e-8e5a-af6a542e9a0e', foundational, selectivity_reveals_extractive_intent).
narrative_ontology:cs_axiom_status(selectivity_reveals_extractive_intent, holdable).
narrative_ontology:cs_axiom_grounding('67d24ff5-7774-419e-8e5a-af6a542e9a0e', selectivity_reveals_extractive_intent, empirically_contingent).
narrative_ontology:cs_axiom('67d24ff5-7774-419e-8e5a-af6a542e9a0e', foundational, conditionality_is_coerced_contract).
narrative_ontology:cs_axiom_status(conditionality_is_coerced_contract, holdable).
narrative_ontology:cs_axiom_grounding('67d24ff5-7774-419e-8e5a-af6a542e9a0e', conditionality_is_coerced_contract, empirically_contingent).
narrative_ontology:cs_reference_frame('67d24ff5-7774-419e-8e5a-af6a542e9a0e', hegemonic_founders_compact).
narrative_ontology:cs_drift_state('67d24ff5-7774-419e-8e5a-af6a542e9a0e', contemporary_multipolarity, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('67d24ff5-7774-419e-8e5a-af6a542e9a0e', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_debtor_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds enforceable property and contract rights across jurisdictions, dollar-clearing privileges, and preferential access to Southern markets opened by loan conditionality. Collects returns from the order's stability without administering it day to day. Exit is trivially available: capital relocates to whichever jurisdiction honors the rules, so the order must compete for its presence rather than the reverse.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Hold veto power over Security Council action and over Charter amendment, which seals the formal revision channels the order advertises. Authorize sanctions and force selectively, decide referrals and exemptions, and collect veto rents in the form of immunity from rules applied to others. They occupy the rule-making position itself, so exit from the arrangement is meaningless; reshaping it is likewise blocked by their own mutual vetoes.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_states, beneficiary).

% Design and police loan conditionality: fiscal austerity, privatization schedules, capital-account liberalization, tied to balance-of-payments support for states in distress. Careers, peer standing, and institutional doctrine are built inside the technocratic adjustment paradigm; questioning the paradigm means leaving the profession rather than reforming it. They execute agreements the borrowing states sign but did not write.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, imf_world_bank_conditionality_administrators, agenda_setter,
    institutional, generational, identity_locked, global).

% Sign standby agreements and structural adjustment programs under debt distress, accepting policy conditions they did not author and cannot veto. Formal membership carries Assembly votes weighted far below their populations and no equivalent lever over Council or fund decisions. Default, autarky, or parallel institutions carry severe financing and market-access penalties, though alternative lenders have begun to widen the outside option since the mid-2010s.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_debtor_states, payer,
    organized, biographical, constrained, global).

% Bear austerity directly: subsidy removal, public-sector wage cuts, user fees for health and education, unemployment following privatization. They were never party to the loan agreements executed in their name and have no procedural seat anywhere in the chain from creditor board to national implementation. Individual exit means migration, which destination states restrict.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations, payer,
    powerless, immediate, trapped, regional).

% Passes majoritarian reform resolutions in the General Assembly: Council expansion, voting reform, sovereign debt arbitration frameworks. Every proposal requires Security Council concurrence or great-power assent to take effect, so the coalition speaks where it cannot decide. It recurs every session with substantially the same agenda and substantially the same outcome.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, g77_reform_coalition, excluded,
    organized, generational, constrained, global).

% Build alternative financing outside Bretton Woods governance: the New Development Bank, the AIIB, currency swap lines, local-currency settlement pilots. They seek renegotiated terms rather than exit from the interstate system, and are treated as outsiders at the core rule-setting tables while accumulating the capacity to make that treatment costlier.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, brics_parallel_institutions, excluded,
    institutional, generational, mobile, continental).

% Documents enforcement asymmetries, traces conditionality outcomes, and theorizes the order's legitimacy structure from outside any governing seat. Holds no enforcement capacity; influence runs through citation, advisory opinions, and the slow socialization of elites who rotate through the institutions being described.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, critical_intl_law_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__hegemonic_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides common rules and adjudication for interstate commerce and security among accepting states: trade and shipping rules, contract enforceability, border settlement forums, and dispute panels replace bilateral power bargaining with predictable procedure. This function is real and is what the order's self-description cites.
% TRANSFER_FUNCTION: Moves wealth and policy autonomy from Global South debtor states and their populations toward Northern creditors and capital holders: debt-service flows under adjustment conditionality, asymmetric trade and intellectual-property terms, and enforcement (sanctions, force, referrals) distributed by enforcer interest rather than violation rate.
% ABSENT_VOICES: Populations living under adjustment programs were never signatories and have no seat in the chain from creditor board to national implementation. Colonized peoples were absent from the founding conferences where the order's architecture was fixed. The G77 majority speaks in the Assembly but decides nowhere; BRICS financiers sit outside the core tables. Each would object that consent was never theirs to give.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, trade would reorganize around bilateral power bargains, creditor claims would lose their enforcement backstop, dozens of states would face instantaneous financing crises, and security disputes would lose their authorized-intervention channel. Nearly every existing commercial contract, reserve position, and alliance structure presupposes it.
% FOUNDING_PROBLEM: Preventing a recurrence of great-power war and interwar economic collapse: collective security against aggression, open trade against beggar-thy-neighbor spirals, managed exchange rates against competitive devaluation.
% FOUNDING_PROBLEM_CORROBORATION: No attestation comes from the beneficiary set alone. G77 and Non-Aligned Movement ministerial communiques attest that the security branch retains value while the economic-governance branch now operates as extraction; dependency-theory scholarship from Southern academies and default-episode histories (Argentina, Zambia, Sri Lanka) corroborate the conditionality critique from outside the institutions; even Northern realist scholars concede enforcement follows enforcer interest. The parties dispute which branches of the founding problem remain live.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the transfer is large and continuous — debt service under imposed policy, asymmetric trade and IP terms, enforcement distributed by interest — but capped below snare territory because the order also delivers genuine public goods its own critics use: dispute panels, trade predictability, a currency of last resort. Suppression (0.72) reflects dependence on active machinery rather than participant preference: conditionality leverage, the sanctions-authorization monopoly, financial surveillance, and the veto seal on revision; it sits below 1.0 because exit is costly rather than impossible and BRICS-era alternatives are widening it. Theater_ratio (0.55) prices the legitimation layer: Charter amendment procedures, Assembly votes, and rule-of-law rhetoric perform universality while the veto makes revision practically unreachable — over half of the order's visible activity now maintains the appearance of openness rather than the substance. Accessibility_collapse (0.45) is moderate: alternatives persist and multiply (New Development Bank, AIIB, swap lines, regional adjudication), so understanding the constraint does not close the option space the way a natural law would. Resistance (0.6) is sustained and organized: the G77/NIEO lineage, debt-default episodes, forum-shopping, de-dollarization experiments. The temporal series run on one shared eight-point grid (1944-2026) so every tracked metric is authored at every examined time point; the trajectories show extraction and enforcement machinery accumulating together, with theater accelerating after 1990 as formal-universality language detached from practice.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural data is what forces the divergence. From the p5 and administrator seats, the arrangement is the coordination achievement they personally operate: rules, fora, predictability, crisis management — extraction is invisible as leadership overhead. From the debtor-state seats, the identical structure presents as sealed revision, selective enforcement, and contracts signed under duress. From the capital seat, the order is background stability that prices into every return. The adjustment populations experience none of the coordination goods directly and all of the austerity directly. The engine computes these per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. us_european_capital sits near the beneficiary pole: subsidized by enforceable rights and opened markets, with arbitrage-grade exit damping any residual exposure. p5_permanent_states derive low directionality from their veto rents despite bearing order-maintenance costs. The conditionality administrators sit near symmetric in cash terms but are identity-locked into the frame, which pins their effective position to the arrangement they administer. global_south_debtor_states derive high directionality as declared victims with constrained exit, which amplifies effective extraction; structural_adjustment_populations sit nearest the full-target end — full bearers, trapped, no procedural voice. The excluded coalitions carry high directionality without even the voice that formal membership grants debtors. No directionality_overrides are needed: the beneficiary/victim declarations plus the differentiated exit atoms already separate every seat the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem splits: war-prevention through collective security remains arguably live in a nuclear era, while the economic-governance branch's original mandate (managed openness preventing collapse) has drifted into maintenance of the transfer structure itself. This is why the story claims tangled_rope rather than snare: calling the order a snare erases the real coordination function even this reading concedes ('formally revisable', functioning dispute settlement), and calling it a rope erases the asymmetric extraction the reading exists to document. The R5 interview records the founding problem as contested rather than dead, so the mismatch consumer finds no clean zombie signature — the arrangement demonstrably rearranges the world (Q5) while the parties dispute whether its founding problems persist. mandatrophy is therefore unresolved-by-design here: the honest state is a live coordination shell hosting a contested extraction core, which is precisely the tangled-rope structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story is one reading of kernel rbio_practice_norm_complex. How would the sibling readings restructure the constraint: would liberal_institutional_reading collapse epsilon toward the coordination floor (victims dissolving into capacity shortfalls), and would sovereignty_maximalist_reading change the victim set to any externally targeted state regardless of alignment?',
    'Compile the sibling stories and compare per-seat classifications, epsilon values, and engine-computed foreclosure relations across the family; the disagreement is located in selectivity valence, consent status, and victim-set composition.',
    'Classification is reading-indexed: resolving the kernel contest would not refine this story but would redistribute its structural claims across the sibling files. Cross-reading comparison measures the kernel''s contest rather than adjudicating it inside any one file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one of three readings of the RBIO kernel; sibling readings instantiate different constraints with different epsilon and victim sets.').

omega_variable(
    selectivity_intent_vs_capacity,
    'Does enforcement selectivity track enforcer interest (this reading''s extractive-intent claim) or enforcement capacity and violation salience (the liberal reading''s capacity claim)?',
    'Panel analysis of violations crossed with enforcement actions across power tiers, controlling for violation frequency, visibility, and geographic reach; interest-driven selectivity survives the controls, capacity-driven selectivity does not.',
    'If capacity explains selectivity, epsilon falls toward coordination cost and this reading loses its central warrant; if interest explains it after controls, the extraction claim is confirmed and the liberal sibling''s epsilon is the one under strain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_intent_vs_capacity, empirical, 'Whether the enforcement asymmetry is intentional extraction or incidental capacity.').

omega_variable(
    consent_status_under_distress,
    'Is conditionality consent genuine — states choosing adjustment over available alternatives — or coerced contract, signed under debt distress with no realistic outside option?',
    'Audit of refused-program episodes and the outside options actually available at signing; natural experiments where alternative creditors existed (post-2015 parallel lenders) show whether terms and acceptance rates shift when exit widens.',
    'A coerced-contract finding supports high epsilon and the declared victim structure; a genuine-consent finding converts part of the measured extraction into priced services and moves the story toward the liberal sibling''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_status_under_distress, empirical, 'Whether adjustment agreements satisfy the conditions of voluntary contract.').

omega_variable(
    freeze_design_vs_path_dependency,
    'Is the practical un-amendability of the order a deliberately maintained extractive design (the veto as a rent-producing instrument) or emergent institutional inertia that no party actively profits from defending?',
    'Trace the historical record of amendment and reform attempts: did P5 actors expend resources to defeat specific proposals (design), or merely decline to act while proposals lapsed (inertia)? Compare Charter-amendment history with fund-quota reform history.',
    'Design evidence supports tangled_rope with concentrated gain flow in the named seats; pure-inertia evidence pushes toward piton dynamics with diffuse gains and theatrical maintenance of the revision facade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freeze_design_vs_path_dependency, conceptual, 'Whether the revision freeze is engineered or accreted.').

omega_variable(
    beneficiary_granularity,
    'Does the extraction accrue to US and European capital specifically, or diffuse across order participants — including Southern elites who implement adjustment, middle-income beneficiaries of market access, and generalized price effects?',
    'Incidence analysis of adjustment-period transfers: creditor balance-sheet gains versus domestic elite capture in implementing states versus economy-wide price effects; follow who actually holds the enlarged claims.',
    'Concentrated accrual confirms the named gain-flow seat and the hegemony-specific framing; demonstrated diffusion would move the receipt surface toward diffuse and soften the reading''s beneficiary identification without necessarily lowering epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_granularity, empirical, 'Where the extracted value actually lands within and beyond the declared beneficiary set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 1944, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_heg_extract_tr_t1944, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1944, 0.14).
narrative_ontology:measurement(rbio_heg_extract_tr_t1960, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(rbio_heg_extract_tr_t1975, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1975, 0.26).
narrative_ontology:measurement(rbio_heg_extract_tr_t1990, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1990, 0.36).
narrative_ontology:measurement(rbio_heg_extract_tr_t2000, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2000, 0.47).
narrative_ontology:measurement(rbio_heg_extract_tr_t2008, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2008, 0.51).
narrative_ontology:measurement(rbio_heg_extract_tr_t2016, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2016, 0.54).
narrative_ontology:measurement(rbio_heg_extract_tr_t2026, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2026, 0.55).

% Extraction over time
narrative_ontology:measurement(rbio_heg_extract_be_t1944, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1944, 0.46).
narrative_ontology:measurement(rbio_heg_extract_be_t1960, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1960, 0.53).
narrative_ontology:measurement(rbio_heg_extract_be_t1975, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(rbio_heg_extract_be_t1990, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1990, 0.71).
narrative_ontology:measurement(rbio_heg_extract_be_t2000, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(rbio_heg_extract_be_t2008, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2008, 0.76).
narrative_ontology:measurement(rbio_heg_extract_be_t2016, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2016, 0.77).
narrative_ontology:measurement(rbio_heg_extract_be_t2026, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(rbio_heg_extract_su_t1944, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1944, 0.34).
narrative_ontology:measurement(rbio_heg_extract_su_t1960, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1960, 0.42).
narrative_ontology:measurement(rbio_heg_extract_su_t1975, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(rbio_heg_extract_su_t1990, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1990, 0.66).
narrative_ontology:measurement(rbio_heg_extract_su_t2000, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2000, 0.67).
narrative_ontology:measurement(rbio_heg_extract_su_t2008, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2008, 0.69).
narrative_ontology:measurement(rbio_heg_extract_su_t2016, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(rbio_heg_extract_su_t2026, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'rules-based international order'. The label covers at least three structurally distinct claims with different epsilon and different victim sets: the liberal institutional reading (official self-description; negligible extraction, capacity-limited enforcement), this hegemonic extraction reading (substantially extractive; Southern debtors and adjustment populations as victims), and the sovereignty maximalist reading (extraction framed as interference; victim set defined by external targeting rather than North/South position). The liberal reading is upstream: its legitimacy claims are the resource this reading documents as selectively deployed, and each documented selectivity case erodes the upstream reading's evidentiary base without logically eliminating it. All three files link one another via affects_constraints; epsilon is invariant within each file because each instantiates one reading of the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
