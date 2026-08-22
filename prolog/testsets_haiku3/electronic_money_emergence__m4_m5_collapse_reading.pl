% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

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
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: M4/M5 Statistical Distinction as Electronic Money Category Constructor
 *   domain: economic/institutional
 *
 * SUMMARY:
 *   In the 1970s-1980s, electronic payment systems and credit money grew so
 *   rapidly that historical monetary aggregates (M1: currency + demand
 *   deposits) no longer captured the functional money supply. Central banks
 *   responded by creating new aggregates: M4 (narrower, including only highly
 *   liquid near-monies) and M5 (broader, including all liquid assets). This
 *   reading argues that the M4/M5 distinction did not discover a pre-existing
 *   category of electronic money but retroactively CREATED that category
 *   through the act of statistical distinction. Electronic money did not
 *   emerge in nature and then get measured; rather, the measurement
 *   convention, by establishing a boundary between M4 and M5, constructed the
 *   category that was then read backward as an emergence event. The
 *   constraint is the measurement regime itself — the institutional practice
 *   of maintaining and publishing these distinctions — which extracts
 *   legitimacy and policy authority from those who would contest it.
 *
 * KEY AGENTS:
 *   - Central banks: agenda-setters who define and publish the M4/M5 categories; their measurement authority frames electronic money as an objective monetary phenomenon
 *   - Monetary economists: beneficiaries who depend on M4/M5 categories as the authoritative framework for research and policy recommendation
 *   - Heterodox theorists: payers who are excluded from mainstream policy influence because their frameworks contradict the orthodox categories; identity-locked in alternative monetary traditions
 *   - Alternative currency communities: powerless payers whose innovations are classified out of existence; trapped in informal, unrecognized monetary spaces
 *   - Financial historians: excluded observers who document that M4/M5 is recent institutional convention, not monetary discovery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.68).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.71).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "M4/M5 Statistical Distinction as Electronic Money Category Constructor").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic/institutional").

domain_priors:requires_active_enforcement(electronic_money_emergence__m4_m5_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, 'c10aa76a-d462-4d7b-9106-02cd5ad28f82').
narrative_ontology:cs_kernel_codification('c10aa76a-d462-4d7b-9106-02cd5ad28f82', formalized).
narrative_ontology:cs_authority_grounding('c10aa76a-d462-4d7b-9106-02cd5ad28f82', extraction).
narrative_ontology:cs_interpretation_layer_present('c10aa76a-d462-4d7b-9106-02cd5ad28f82').
narrative_ontology:cs_reading_relation('c10aa76a-d462-4d7b-9106-02cd5ad28f82', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('c10aa76a-d462-4d7b-9106-02cd5ad28f82', electronic_money_emergence__first_held_reading, influences).
narrative_ontology:cs_axiom('c10aa76a-d462-4d7b-9106-02cd5ad28f82', foundational, measurement_creates_category).
narrative_ontology:cs_axiom_status(measurement_creates_category, holdable).
narrative_ontology:cs_axiom_grounding('c10aa76a-d462-4d7b-9106-02cd5ad28f82', measurement_creates_category, deontological).
narrative_ontology:cs_axiom('c10aa76a-d462-4d7b-9106-02cd5ad28f82', secondary, no_pre_institutional_emergence).
narrative_ontology:cs_axiom_status(no_pre_institutional_emergence, holdable).
narrative_ontology:cs_axiom_grounding('c10aa76a-d462-4d7b-9106-02cd5ad28f82', no_pre_institutional_emergence, empirically_contingent).
narrative_ontology:cs_reference_frame('c10aa76a-d462-4d7b-9106-02cd5ad28f82', electronic_money_as_institutional_artifact).
narrative_ontology:cs_drift_state('c10aa76a-d462-4d7b-9106-02cd5ad28f82', contemporary_cryptocurrency_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c10aa76a-d462-4d7b-9106-02cd5ad28f82', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, central_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, monetary_economists).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, financial_regulators).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, heterodox_monetary_theorists).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, alternative_currency_communities).
narrative_ontology:constraint_vindicates(electronic_money_emergence__m4_m5_collapse_reading, fiat_currency_stability_doctrine).
narrative_ontology:constraint_vindicates(electronic_money_emergence__m4_m5_collapse_reading, central_bank_measurement_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and publish the M4/M5 monetary aggregates. Their measurement conventions classify what counts as 'electronic money' for policy purposes. They administer the statistical system and adjust definitions when institutional changes require it. Benefit from the measurement authority that allows them to claim they understand and can manage the money supply.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_banks, agenda_setter,
    institutional, generational, arbitrage, national).

% Use the official M4/M5 categories as the authoritative framework for research and policy advice. Their models, papers, and consulting work depend on these categories being treated as real monetary phenomena rather than measurement conventions. They benefit from the institutional stability the categories provide and from the prestige of alignment with central bank definitions.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_economists, beneficiary,
    organized, biographical, constrained, global).

% Use the M4/M5 framework to justify regulatory interventions in money supply, interest rates, and financial stability. The categories allow them to frame policy as responding to objective monetary phenomena rather than discretionary institutional choices. Benefit from the appearance that they are measuring and responding to real monetary emergence rather than managing a constructed classification system.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, financial_regulators, beneficiary,
    institutional, generational, constrained, national).

% Argue that the M4/M5 distinction misclassifies monetary phenomena and obscures the true nature of credit, endogenous money creation, and post-Keynesian dynamics. They incur the cost of being excluded from mainstream policy influence, unpublished in leading journals that assume M4/M5 validity, and unable to secure research funding for alternative monetary frameworks. Their intellectual identity is fused with the rejection of orthodox monetary categories.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, heterodox_monetary_theorists, payer,
    moderate, biographical, identity_locked, global).

% Operate local currencies, mutual credit systems, or cryptocurrency protocols that the M4/M5 framework does not recognize as legitimate money. They bear the cost of regulatory exclusion and institutional invisibility. Their monetary innovations are classified out of existence by the central bank's measurement definitions, preventing mainstream adoption and policy legitimacy.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, alternative_currency_communities, payer,
    powerless, biographical, trapped, local).

% Document the contingent, constructed history of monetary categorization. Would argue that the M4/M5 distinction is a recent institutional artifact (post-1980s), not a discovery of preexisting monetary reality. They are excluded from policy conversations and their historical research is treated as decorative rather than constitutive of monetary understanding.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, financial_historians, excluded,
    moderate, biographical, constrained, global).

% Build and operate the technological infrastructure that the M4/M5 categories claim to measure: electronic transfer systems, settlement networks, deposit accounts, and digital wallets. They see the actual flows and infrastructure; they neither benefit from nor are harmed by the classification itself, though their technology enables the measurement categories to function as if they were real.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, payment_system_operators, observer,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__m4_m5_collapse_reading, central_banks).
narrative_ontology:fixing_cost_class(electronic_money_emergence__m4_m5_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a standardized, universally applicable language for discussing the size and composition of money supplies in the economy, enabling central banks, regulators, and economists to communicate and coordinate policy around a shared set of categories.
% TRANSFER_FUNCTION: Moves interpretive authority from heterodox theorists and alternative currency communities to mainstream central banks and orthodox economists. The M4/M5 distinction transfers legitimacy (what counts as 'real' money) and policy influence (who gets to define monetary reality) from decentralized, contested frameworks to a single, officially published measurement regime.
% ABSENT_VOICES: Financial historians, heterodox economists, cryptocurrency developers, and post-Keynesian monetary theorists are not present in the conversations where M4/M5 definitions are set. They would attest that the distinction is a measurement artifact, not a discovery of monetary emergence, but they are excluded from the central bank committees that define the categories.
% DISAPPEARANCE_RATIONALE: If the M4/M5 distinction disappeared, central banks would lose a key measurement framework for justifying policy interventions, but the actual financial infrastructure and electronic money itself would not change. Alternatively, without the distinction, the recognition of electronic money's true emergence (as a gradual technological and institutional process) might become visible, changing the narrative about monetary control. Some would say the world rearranges (monetary policy becomes incoherent without the categories); others would say it would reveal that the world never actually depended on the M4/M5 categories, only on the pretense that it did.
% FOUNDING_PROBLEM: The rapid growth of electronic payment systems and credit money in the 1970s-1980s created a technical problem for monetary measurement: how to count dematerialized currency, digital transfers, and lines of credit that looked and functioned like money but did not fit the historical definitions of M1 (currency in circulation). Central banks needed new aggregates to describe what was happening in the financial system.
% FOUNDING_PROBLEM_CORROBORATION: Central banks themselves attest that the original problem (technical measurement of electronic flows) was solved by the 1990s and the distinction now persists as standard practice. Historians of finance (Blinder, Goodhart, Skidelsky) and payment systems engineers attest that the technical problem was solved, the infrastructure stabilized, and the measurement categories now reflect institutional convention rather than discovered monetary reality. Heterodox economists attest the founding problem is a cover story: the real problem was not measurement but the need to legitimize credit-driven money creation in the 1980s deregulation era.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, contested).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.22 to 0.68 over the interval because the measurement regime's authority deepens: in 1975 it was a technical fix for a real problem; by 2025 it is an entrenched institutional convention that excludes alternative frameworks. Theater ratio rises from 0.25 to 0.62 because the regime's function shifts: early measurement was genuinely technical, but as the infrastructure stabilized (1990s onward), the M4/M5 distinction increasingly functions to legitimize central bank authority and orthodox monetary theory rather than to measure new phenomena. Suppression requirement rises from 0.35 to 0.71 because maintaining the distinction requires active institutional effort: central banks must publish revised definitions when new financial instruments challenge the boundaries; regulators must exclude alternative currencies from legitimacy; economists must defend the categories against heterodox critique. The constraint persists not because it solves a coordination problem better than alternatives (it doesn't), but because central banks can, and heterodox economists have no institutional power to dislodge it. All three metrics are authored on a shared time grid (every metric at every time point) and show the classic piton signature: the founding coordination function (measuring electronic flows) was solved by the 1990s, but the measurement regime persists as theater and extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the central bank and orthodox economist seats, M4/M5 is a discovered category that enables policy management and scientific understanding — they compute the constraint as coordination or genuine rope. From the heterodox and alternative currency seats, the same M4/M5 regime is an enforced classification that suppresses their frameworks — they compute it as pure extraction or snare. The engine should produce this divergence naturally from the structural data: the beneficiary seats (institutional power, mobile exit) see coordination; the payer seats (moderate/powerless power, identity-locked exit) see extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks hold institutional power and arbitrage-grade exit (they can change the definitions whenever they choose); they benefit from the measurement authority the categories confer, so their d is near the beneficiary end (~0.2). Monetary economists have organized power and constrained exit (they can contest definitions only through peer review and publication, where the orthodox categories dominate the conversation); they benefit from alignment with central bank categories, so their d is moderate-low (~0.3). Heterodox theorists have moderate power and are identity-locked (their entire intellectual tradition depends on rejecting orthodox categories; exiting would mean ceasing to be a heterodox theorist); they bear the cost of exclusion and delegitimacy, so their d is near the target end (~0.8). Alternative currency communities have powerless institutional position and are trapped (no regulatory pathway to legitimacy, no way out except surrender of their project); they bear the cost of classification invisibility, so their d approaches full target (~0.95). The directionality derivation should produce these values from the declared beneficiary/victim + exit relationships without manual override.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's classification as piton (not snare or tangled rope) depends on recognizing that the founding problem is dead. The M4/M5 distinction was built to solve a real measurement problem in the 1970s-1980s: electronic money was growing faster than traditional aggregates could capture. That problem is solved — financial infrastructure and monetary accounting are now mature enough to measure electronic flows without difficulty. The constraint persists not because the problem remains live but because central banks choose to maintain it (institutional inertia, prestige, ease). There is no beneficiary concentrated enough to defend the constraint against determined opposition — economists and regulators benefit, but diffusely, and could adapt to alternative frameworks without losing power. The constraint's persistence is theatrical: the M4/M5 regime performs the role of objective monetary measurement while actually stabilizing institutional authority. A snare reading would require that the constraint persist because someone captures concentrated rents from maintaining it; a tangled rope reading would require genuine coordination alongside extraction. Neither fits. The piton diagnosis is correct: the constraint is mostly performance, function-atrophied, persisting by institutional habit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_ontology_ambiguity,
    'Does the M4/M5 distinction measure a pre-existing monetary distinction, or does it create that distinction through the act of institutional measurement?',
    'Comparative history of alternative monetary measurement regimes: if different countries or historical periods that measured electronic money differently (or not at all) experienced the same underlying monetary phenomena, then M4/M5 is a measurement convention, not a discovered category. If M4/M5-absent systems fail to recognize essential monetary properties, then M4/M5 measures real distinctions.',
    'If measurement creates the category, the constraint is a piton (institutional theater maintaining a dead function). If measurement discovers pre-existing distinctions, the constraint is coordination supporting genuine monetary understanding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_ontology_ambiguity, conceptual, 'Whether electronic money exists prior to or only after M4/M5 measurement').

omega_variable(
    functional_emergence_vs_classificatory_emergence,
    'Did electronic money emerge functionally (new payment methods actually worked differently) or only classificatorily (existing payment methods got reclassified by measurement)?',
    'Technical history of payment systems: if electronic transfers and digital accounting created genuinely new monetary capabilities (speed, reach, scriptability) unavailable before, emergence is functional. If electronic systems merely substituted for paper-based systems without changing what money does, emergence is classificatory only.',
    'Functional emergence supports the became_thinkable or first_held readings. Classificatory emergence supports the m4_m5_collapse reading that the distinction is measurement artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_emergence_vs_classificatory_emergence, empirical, 'Whether electronic money brought new capabilities or only new names for existing ones').

omega_variable(
    institutional_power_concentration,
    'Would heterodox or alternative currency frameworks gain institutional acceptance if the M4/M5 regime were abandoned, or does the regime''s dominance reflect deeper structural power differences?',
    'Scenario: regulatory mandate requiring central banks to publish three equally-weighted competing monetary aggregates (orthodox, post-Keynesian, and institutional credit). If heterodox frameworks gain credibility and policy influence, then the regime''s power is contingent; if they remain marginal, the regime''s dominance reflects power independent of measurement convention.',
    'If contingent, the regime is pure suppression (classification ruling out alternatives). If structural, the regime is theater masking deeper institutional imbalances, and piton diagnosis is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_power_concentration, preference, 'Whether the M4/M5 regime''s dominance is because it measures reality or because central banks have power to enforce classification').

omega_variable(
    cryptocurrency_reclassification,
    'As cryptocurrency systems mature and develop electronic payment, settlement, and store-of-value functions, will the M4/M5 regime reclassify cryptocurrencies as money, or will it continue to exclude them?',
    'Observe central bank policy over the next 5-10 years: if they expand M4/M5 to include stable cryptocurrencies or CBDCs (central bank digital currencies) with full parity, the regime is adaptive and semi-functional. If they maintain exclusion despite functional equivalence, the regime is pure gatekeeping (suppression).',
    'Inclusion would suggest the regime is responsive measurement. Exclusion would support the piton reading: the regime survives by controlling what counts as legitimate money, not by discovering monetary reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cryptocurrency_reclassification, empirical, 'Whether the M4/M5 regime will adapt to new forms of electronic money or maintain exclusionary gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1975, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement_basis(elec_tr_t1975, observed).
narrative_ontology:measurement(elec_tr_t1985, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement_basis(elec_tr_t1985, observed).
narrative_ontology:measurement(elec_tr_t1995, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 1995, 0.48).
narrative_ontology:measurement_basis(elec_tr_t1995, observed).
narrative_ontology:measurement(elec_tr_t2005, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2005, 0.56).
narrative_ontology:measurement_basis(elec_tr_t2005, observed).
narrative_ontology:measurement(elec_tr_t2015, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2015, 0.6).
narrative_ontology:measurement_basis(elec_tr_t2015, observed).
narrative_ontology:measurement(elec_tr_t2025, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 2025, 0.62).
narrative_ontology:measurement_basis(elec_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(elec_be_t1975, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1975, 0.22).
narrative_ontology:measurement_basis(elec_be_t1975, observed).
narrative_ontology:measurement(elec_be_t1985, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement_basis(elec_be_t1985, observed).
narrative_ontology:measurement(elec_be_t1995, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement_basis(elec_be_t1995, observed).
narrative_ontology:measurement(elec_be_t2005, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement_basis(elec_be_t2005, observed).
narrative_ontology:measurement(elec_be_t2015, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement_basis(elec_be_t2015, observed).
narrative_ontology:measurement(elec_be_t2025, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(elec_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1975, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement_basis(elec_su_t1975, observed).
narrative_ontology:measurement(elec_su_t1985, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement_basis(elec_su_t1985, observed).
narrative_ontology:measurement(elec_su_t1995, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement_basis(elec_su_t1995, observed).
narrative_ontology:measurement(elec_su_t2005, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement_basis(elec_su_t2005, observed).
narrative_ontology:measurement(elec_su_t2015, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement_basis(elec_su_t2015, observed).
narrative_ontology:measurement(elec_su_t2025, electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(elec_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__m4_m5_collapse_reading, 0.05).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, central_bank_measurement_authority).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, monetary_policy_effectiveness_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the electronic_money_emergence kernel. The m4_m5_collapse_reading argues that the M4/M5 statistical distinction created the category of electronic money retroactively, making the 'emergence' a measurement artifact rather than a historical discovery. Sibling readings (became_thinkable_reading, first_held_reading) locate emergence at different moments and treat measurement differently. All three stories share the same kernel but instantiate different constraints with different ε values, beneficiary structures, and types. The m4_m5_collapse_reading is a piton (institutional theater maintaining a dead function); sibling readings may classify differently based on their respective referents and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electronic_money_emergence__m4_m5_collapse_reading, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
