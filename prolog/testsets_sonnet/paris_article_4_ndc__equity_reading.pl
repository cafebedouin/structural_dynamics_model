% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Article 4 NDC Interpretation — CBDR-RC Equity Reading
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   The Paris Agreement's Article 4 requires each party to submit Nationally
 *   Determined Contributions, but leaves open how far the treaty's
 *   differentiation language — rooted in the UNFCCC's Common But
 *   Differentiated Responsibilities and Respective Capabilities (CBDR-RC)
 *   principle — structures the interpretation of those NDCs. This story
 *   instantiates the EQUITY READING: NDCs must be interpreted through CBDR-RC
 *   as requiring structural, categorical distinctions between developed and
 *   developing states in obligation depth, reporting rigor, timeline, and
 *   finance/technology transfer entitlement. This is a genuinely distinct
 *   constraint from the sovereigntist reading (NDCs as purely self-determined
 *   pledges with no binding differentiation logic) and the supranational
 *   reading (NDCs as binding, ratcheting, uniformly-accountable commitments)
 *   — each reading produces a different beneficiary/victim structure and a
 *   different epsilon. The equity reading's coordination function (securing
 *   near-universal participation across radically unequal capacities) is
 *   real; its extraction (developed-state fiscal and competitiveness costs,
 *   contested category boundaries for large emerging emitters) is also real
 *   and requires active multilateral enforcement (negotiated guidance
 *   documents, transparency framework rules, coalition veto power) to hold.
 *
 * KEY AGENTS:
 *   - developing_state_coalitions: Primary beneficiary and co-agenda-setter (organized/constrained) — secures differentiated, lighter-touch obligations and transfer entitlements
 *   - climate_vulnerable_small_island_states: Beneficiary with no independent leverage (powerless/trapped) — depends on the coalition structure for voice
 *   - developed_state_governments: Primary payer and co-agenda-setter (institutional/constrained) — accepts differentiated, heavier obligations and finance transfer commitments
 *   - developed_state_taxpayers: Diffuse payer with no negotiating seat (powerless/trapped) — funds the transfers without direct voice
 *   - rapidly_industrializing_emerging_economies: Excluded/unresolved category (powerful/constrained) — occupies a structural position the binary developed/developing frame does not cleanly capture
 *   - unfccc_secretariat: Analytical/administrative observer (institutional/analytical) — implements whatever differentiation the parties negotiate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.48).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.42).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Article 4 NDC Interpretation — CBDR-RC Equity Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '8dfc3de8-2a76-4017-a36a-273ab3888de8').
narrative_ontology:cs_kernel_codification('8dfc3de8-2a76-4017-a36a-273ab3888de8', fixed_text).
narrative_ontology:cs_authority_grounding('8dfc3de8-2a76-4017-a36a-273ab3888de8', distributed).
narrative_ontology:cs_reading_relation('8dfc3de8-2a76-4017-a36a-273ab3888de8', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8dfc3de8-2a76-4017-a36a-273ab3888de8', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_axiom('8dfc3de8-2a76-4017-a36a-273ab3888de8', foundational, historical_responsibility_grounds_differentiated_obligation).
narrative_ontology:cs_axiom_status(historical_responsibility_grounds_differentiated_obligation, holdable).
narrative_ontology:cs_axiom_grounding('8dfc3de8-2a76-4017-a36a-273ab3888de8', historical_responsibility_grounds_differentiated_obligation, deontological).
narrative_ontology:cs_axiom('8dfc3de8-2a76-4017-a36a-273ab3888de8', secondary, capacity_asymmetry_justifies_categorical_not_gradual_distinction).
narrative_ontology:cs_axiom_status(capacity_asymmetry_justifies_categorical_not_gradual_distinction, holdable).
narrative_ontology:cs_axiom_grounding('8dfc3de8-2a76-4017-a36a-273ab3888de8', capacity_asymmetry_justifies_categorical_not_gradual_distinction, conventional).
narrative_ontology:cs_reference_frame('8dfc3de8-2a76-4017-a36a-273ab3888de8', unfccc_1992_annex_differentiation).
narrative_ontology:cs_drift_state('8dfc3de8-2a76-4017-a36a-273ab3888de8', post_paris_ratchet_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8dfc3de8-2a76-4017-a36a-273ab3888de8', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_state_coalitions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, climate_vulnerable_small_island_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_negotiating_blocs).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_taxpayers).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_energy_intensive_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_governments).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, common_but_differentiated_responsibilities_doctrine).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, historical_responsibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiating blocs (G77+China, LMDCs, AOSIS) invoke CBDR-RC to secure differentiated obligations: their own NDCs are read as aspirational and conditional on finance and technology transfer, while developed states are read as bound to deeper cuts and transfer commitments. They co-author the interpretive text at COP sessions and hold effective veto power over any supranational enforcement mechanism that would flatten the developed/developing distinction.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_state_coalitions, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, developing_state_coalitions, agenda_setter).

% Face existential physical risk from warming they did negligible historical amount to cause. Depend on the equity reading to keep pressure on developed states for adaptation finance and loss-and-damage transfers. They have no exit from the physical exposure and limited leverage individually, but gain voice through coalition alignment with the larger developing bloc.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, climate_vulnerable_small_island_states, beneficiary,
    powerless, civilizational, trapped, global).

% Technical and legal delegations that draft and defend the CBDR-RC interpretive apparatus inside UNFCCC bodies (transparency framework negotiations, global stocktake modalities). They administer the differentiation architecture — self-selected reporting tiers, flexibility provisions for developing states — and can block supranational ratchet mechanisms that would erase the distinction.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_negotiating_blocs, agenda_setter,
    organized, generational, constrained, global).

% Accept binding-flavored NDC targets and finance/technology transfer obligations under the equity reading's interpretive pressure, while retaining formal sovereignty to set their own NDC content under Paris's bottom-up architecture. They negotiate the finance commitments (e.g., the $100bn/year goals) that the equity reading treats as owed, not discretionary, and bear reputational and diplomatic costs for underdelivery.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_governments, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, developed_state_governments, agenda_setter).

% Fund the climate finance transfers and domestic decarbonization costs that the equity reading assigns to developed states as differentiated obligation. They have no direct voice in the CBDR-RC interpretive negotiation and bear the fiscal cost through taxation and energy price effects, with no meaningful exit from national tax jurisdiction.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_taxpayers, payer,
    powerless, biographical, trapped, national).

% Face steeper domestic compliance costs and carbon pricing than counterpart industries in developing states operating under differentiated flexibility, creating competitiveness pressure. Can lobby domestically and relocate some operations (carbon leakage), but cannot exit the underlying treaty architecture or avoid the differentiated cost structure while remaining in a developed-state jurisdiction.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_energy_intensive_industries, payer,
    powerful, biographical, constrained, national).

% Administers the enhanced transparency framework and compiles national reports under whatever differentiation rules the parties agree to. Has some interpretive discretion in drafting guidance documents but cannot unilaterally impose a reading against a blocking coalition of either developed or developing parties.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, unfccc_secretariat, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, unfccc_secretariat, agenda_setter).

% States like China, India, and other large emerging emitters sit awkwardly inside the binary developed/developing frame the equity reading depends on — they claim developing-state flexibility while now emitting at scales comparable to or exceeding developed states. Their position is not formally renegotiated in the interpretive text; developed-state governments increasingly object to this in bilateral fora, but the objection has not been incorporated into the CBDR-RC interpretive apparatus itself.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, rapidly_industrializing_emerging_economies, excluded,
    powerful, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that a single undifferentiated global emissions obligation would be unworkable and unjust given vastly different historical emissions, current capacity, and development needs across ~195 parties — differentiation lets the treaty secure near-universal participation instead of collapsing under a one-size-fits-all mandate.
% TRANSFER_FUNCTION: Moves finance, technology, and capacity-building resources from developed-state treasuries and industry to developing-state governments and adaptation/mitigation projects, while moving compliance flexibility (self-selected reporting rigor, longer timelines, conditional targets) from a uniform baseline to developing states specifically.
% ABSENT_VOICES: Rapidly industrializing large emitters occupying an intermediate structural position are not given a distinct interpretive category; developed-state domestic constituencies bearing the fiscal cost (taxpayers, energy-intensive labor) have no seat in the UNFCCC negotiating rooms where the differentiation is drawn.
% DISAPPEARANCE_RATIONALE: If the CBDR-RC equity reading were abandoned in favor of a flat, undifferentiated obligation, the negotiating coalition that has sustained near-universal Paris participation would fracture: developing states have repeatedly signaled that formal equality of obligation without historical-responsibility differentiation is a red line, and several finance and loss-and-damage mechanisms are drafted explicitly on the differentiated premise. Its removal would trigger renegotiation of the entire NDC architecture.
% FOUNDING_PROBLEM: The 1992 UNFCCC and subsequently Paris needed a formula reconciling near-universal participation with radically unequal historical emissions and present development capacity, after the binding/differentiated Kyoto model had driven major emitters (notably the US) out of the regime entirely.
% FOUNDING_PROBLEM_CORROBORATION: Developing-state blocs and vulnerable states attest the founding problem — unequal historical responsibility and capacity — remains fully live and is in fact intensifying as loss-and-damage costs mount. Independent voices outside both benefiting coalitions — IPCC synthesis reports and OECD climate finance trackers — corroborate that historical-emissions asymmetry is empirically real, but also document that several 'developing' parties invoking the differentiation now have per-capita and absolute emissions exceeding some 'developed' parties, which several developed-state governments and independent policy analysts (e.g., Center for Global Development) argue means the founding problem's factual predicate has partially eroded for a subset of large emerging emitters without the interpretive category being updated.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) and rising slowly before flattening: the equity reading imposes real fiscal and competitiveness costs on developed-state payers, but the transfer obligations (finance goals, technology mechanisms) have historically been under-delivered relative to their nominal scale, which caps how extractive the constraint actually becomes in practice — hence the leveling-off after 2023 rather than continued acceleration. Suppression is moderate (0.42): the constraint is enforced through negotiated text, coalition veto power, and diplomatic reputational cost rather than hard legal sanction, since Paris's architecture is fundamentally bottom-up and non-punitive. Theater ratio is moderate-elevated (0.4) reflecting that a substantial share of the differentiation apparatus (self-selected reporting flexibility, aspirational finance goals routinely missed) functions more as negotiated symbolic accommodation than as enforced material transfer — this is a real coordination structure with a meaningful theatrical component, not a pure sham. Accessibility collapse is moderate-low (0.35) because alternative interpretive framings (sovereigntist, supranational) remain live and contested at every COP — this reading has not foreclosed its rivals within the treaty text itself. Resistance is substantial (0.6), reflecting persistent developed-state objection to open-ended differentiation, especially regarding large emerging emitters.
 *
 * DIRECTIONALITY LOGIC:
 *   Developing-state coalitions and vulnerable states are declared beneficiaries: the equity reading structurally reduces their compliance burden and creates entitlement claims on developed-state resources, so their derived directionality sits near the beneficiary end. Developed-state governments and their taxpayers/industries are declared victims/payers: the reading assigns them deeper, more binding-flavored obligations and finance transfer duties, so their derived directionality sits nearer the target end — amplified for taxpayers (powerless, trapped, no direct voice) and somewhat dampened for governments (institutional, retains some negotiating leverage as co-agenda-setter). Rapidly industrializing emerging economies are deliberately NOT placed in either beneficiary or victim category structurally — they are marked excluded, since the interpretive apparatus has not been updated to give them a distinct category despite their genuinely intermediate position; this is itself a source of the constraint's contestation rather than a settled directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (radically unequal historical responsibility and capacity undermining a flat universal obligation) remains substantially live per most corroborating sources, which argues against treating this as pure mandatrophy — the coordination function that justified differentiation in 1992 has not simply evaporated. However, the founding_problem_status is marked contested rather than live outright, because the empirical predicate has partially eroded for a specific subset of parties (large emerging emitters) without the interpretive category being revised to track that erosion. This is the diagnostic case the framework is built to catch: a genuine coordination structure that has NOT become obsolete in general, but has drifted out of alignment with the facts for part of its covered population — a partial-mandatrophy signature rather than a clean resolved/unresolved binary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    developed_developing_binary_erosion,
    'Does the binary developed/developing categorical distinction the equity reading depends on still track real differences in historical responsibility and current capacity, or has it become materially misaligned for a subset of large emerging emitters?',
    'Comparative analysis of per-capita and cumulative historical emissions, GDP per capita, and technological capacity across UNFCCC Annex/non-Annex classifications versus current emissions data; tracking whether any renegotiation of the categorical boundary occurs in future COP text.',
    'If the binary is found to be substantially misaligned for major emitters, the equity reading''s coordination justification weakens for that subset while remaining intact for genuinely lower-capacity developing states — suggesting the constraint may need to be further decomposed by emitter tier rather than by a single developed/developing binary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developed_developing_binary_erosion, empirical, 'Whether the developed/developing category still matches material reality for all covered parties.').

omega_variable(
    finance_obligation_enforceability,
    'Are the developed-state finance and technology transfer obligations under the equity reading genuinely binding in any enforceable sense, or are they aspirational commitments whose non-delivery carries no material consequence?',
    'Track actual disbursed climate finance against pledged targets (e.g., the $100bn/year goal) over multiple reporting cycles, and observe whether any developed state faces material diplomatic, legal, or reputational consequence for shortfall.',
    'If obligations are consistently unenforced, the constraint''s true extractiveness is lower than nominal transfer commitments suggest, and theater_ratio should be weighted more heavily than base_extractiveness in classification; if enforcement mechanisms strengthen, extractiveness for developed-state payers would rise correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_obligation_enforceability, empirical, 'Whether the finance/transfer component of CBDR-RC differentiation is materially enforced or largely symbolic.').

omega_variable(
    kernel_reading_contest_resolution,
    'Within the paris_article_4_ndc kernel, which reading (equity, sovereigntist, or supranational) will dominant state practice and dispute-resolution bodies converge on as the treaty regime matures, and does convergence foreclose the others?',
    'Track ICJ advisory opinions, COP decision text evolution, and state practice patterns (particularly whether major emitters explicitly invoke CBDR-RC language in domestic NDC submissions) over the coming decade.',
    'If state practice converges strongly toward the supranational reading, the equity reading''s coalition veto power would erode and this constraint''s beneficiary structure would weaken; if the sovereigntist reading dominates instead, the equity reading''s transfer obligations would lose their interpretive force entirely. Convergence toward any one reading does not necessarily forecloses the others within the treaty''s ambiguous text, since Paris was deliberately drafted with interpretive flexibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, conceptual, 'Which kernel reading state practice will ultimately favor, and whether that constitutes foreclosure or continued coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__equity_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(pari_tr_t2015, observed).
narrative_ontology:measurement(pari_tr_t2019, paris_article_4_ndc__equity_reading, theater_ratio, 2019, 0.33).
narrative_ontology:measurement_basis(pari_tr_t2019, observed).
narrative_ontology:measurement(pari_tr_t2023, paris_article_4_ndc__equity_reading, theater_ratio, 2023, 0.38).
narrative_ontology:measurement_basis(pari_tr_t2023, observed).
narrative_ontology:measurement(pari_tr_t2027, paris_article_4_ndc__equity_reading, theater_ratio, 2027, 0.4).
narrative_ontology:measurement_basis(pari_tr_t2027, projected).
narrative_ontology:measurement(pari_tr_t2031, paris_article_4_ndc__equity_reading, theater_ratio, 2031, 0.41).
narrative_ontology:measurement_basis(pari_tr_t2031, projected).
narrative_ontology:measurement(pari_tr_t2035, paris_article_4_ndc__equity_reading, theater_ratio, 2035, 0.4).
narrative_ontology:measurement_basis(pari_tr_t2035, projected).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__equity_reading, base_extractiveness, 2015, 0.32).
narrative_ontology:measurement_basis(pari_be_t2015, observed).
narrative_ontology:measurement(pari_be_t2019, paris_article_4_ndc__equity_reading, base_extractiveness, 2019, 0.38).
narrative_ontology:measurement_basis(pari_be_t2019, observed).
narrative_ontology:measurement(pari_be_t2023, paris_article_4_ndc__equity_reading, base_extractiveness, 2023, 0.45).
narrative_ontology:measurement_basis(pari_be_t2023, observed).
narrative_ontology:measurement(pari_be_t2027, paris_article_4_ndc__equity_reading, base_extractiveness, 2027, 0.49).
narrative_ontology:measurement_basis(pari_be_t2027, projected).
narrative_ontology:measurement(pari_be_t2031, paris_article_4_ndc__equity_reading, base_extractiveness, 2031, 0.47).
narrative_ontology:measurement_basis(pari_be_t2031, projected).
narrative_ontology:measurement(pari_be_t2035, paris_article_4_ndc__equity_reading, base_extractiveness, 2035, 0.48).
narrative_ontology:measurement_basis(pari_be_t2035, projected).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__equity_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement_basis(pari_su_t2015, observed).
narrative_ontology:measurement(pari_su_t2019, paris_article_4_ndc__equity_reading, suppression_requirement, 2019, 0.34).
narrative_ontology:measurement_basis(pari_su_t2019, observed).
narrative_ontology:measurement(pari_su_t2023, paris_article_4_ndc__equity_reading, suppression_requirement, 2023, 0.39).
narrative_ontology:measurement_basis(pari_su_t2023, observed).
narrative_ontology:measurement(pari_su_t2027, paris_article_4_ndc__equity_reading, suppression_requirement, 2027, 0.41).
narrative_ontology:measurement_basis(pari_su_t2027, projected).
narrative_ontology:measurement(pari_su_t2031, paris_article_4_ndc__equity_reading, suppression_requirement, 2031, 0.42).
narrative_ontology:measurement_basis(pari_su_t2031, projected).
narrative_ontology:measurement(pari_su_t2035, paris_article_4_ndc__equity_reading, suppression_requirement, 2035, 0.42).
narrative_ontology:measurement_basis(pari_su_t2035, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__equity_reading, 0.12).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, unfccc_loss_and_damage_fund_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, green_climate_fund_disbursement_governance).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the paris_article_4_ndc kernel decomposed under the epsilon-invariance principle: equity_reading (this story, moderate epsilon ~0.48, tangled_rope, asymmetric transfer obligations), sovereigntist_reading (lower epsilon expected, rope-leaning, no mandatory differentiation logic), and supranational_reading (potentially higher epsilon for non-compliant states, tangled_rope or snare-leaning, uniform ratchet accountability). Each reading has its own beneficiary/victim structure and its own claimed_type; they are linked here rather than merged because measuring 'the NDC interpretive constraint' under different observables (self-determination vs. differentiated obligation vs. binding ratchet) produces materially different epsilon values — the hallmark of a kernel requiring decomposition rather than a single constraint with an ambiguous observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
