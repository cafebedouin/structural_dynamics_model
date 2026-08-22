% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: Embedded Liberalism Reading: Trade Framework with Bounded Domestic Policy Space
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This story instantiates the embedded-liberalism reading of the
 *   NAFTA-jurisdictional-boundary kernel: trade agreement text operates as an
 *   enforceable framework for market access that simultaneously protects a
 *   bounded zone of domestic policy space, with environmental, labor, and
 *   health measures compatible with trade obligations when non-discriminatory
 *   and defensible as pursuing legitimate objectives. The framework
 *   coordinates genuine cross-border exchange — predictable treatment and a
 *   rules-based dispute channel — while transferring real costs to the
 *   regulatory side of the boundary: agencies must litigate to defend
 *   measures inside the protected zone, taxpayers fund the defense and any
 *   awards, and the anticipated cost of challenge chills some regulation
 *   before it is adopted. The claim/metrics split is deliberate: the
 *   constraint is CLAIMED as tangled_rope — genuine coordination function
 *   plus asymmetric, actively enforced cost transfer — while the metrics
 *   describe moderately extractive operation; the engine computes per-seat
 *   classifications and any divergence between the claim and computed seats
 *   is the datum, not an error. Sibling readings of the same kernel text are
 *   separate constraints with different epsilon: capital_supremacy_reading
 *   reads the same text as supreme law (higher epsilon, wider victim set);
 *   sovereignty_primacy_reading reads it as subordinate coordination (lower
 *   epsilon, unenforceable market access). This reading's epsilon of 0.58 is
 *   stable only under this reading's exception scope.
 *
 * KEY AGENTS:
 *   - export_firms_investors: Primary beneficiary (powerful/mobile) — collects adjudicated market access and awards; forum-shopping mobility
 *   - domestic_regulatory_agencies: Primary target (institutional/trapped) — defends environmental and labor measures inside the legitimate-objectives boundary at their own expense
 *   - trade_ministries_state_parties: Agenda-setter (institutional/constrained) — negotiates and administers the framework, also litigates defenses as secondary payer
 *   - taxpayers_defending_states: Secondary target (moderate/trapped) — bears award and fee exposure through the budget with no procedural seat
 *   - environmental_labor_advocates: Excluded voice (organized/constrained) — would contest the boundary's placement but holds no standing
 *   - dispute_tribunal_arbitrators: Boundary adjudicators (institutional/arbitrage) — set the effective scope of legitimate objectives case by case, paid per sitting
 *   - trade_law_practitioners: Secondary beneficiary (organized/mobile) — collects fee flows that scale with dispute volume
 *   - trade_law_scholars: Analytical observer — maps the gap between balance language and docket outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.58).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.5).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "Embedded Liberalism Reading: Trade Framework with Bounded Domestic Policy Space").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, '567bbb41-1385-4cf4-b8f8-dfe05ead3457').
narrative_ontology:cs_kernel_codification('567bbb41-1385-4cf4-b8f8-dfe05ead3457', fixed_text).
narrative_ontology:cs_authority_grounding('567bbb41-1385-4cf4-b8f8-dfe05ead3457', lineage).
narrative_ontology:cs_interpretation_layer_present('567bbb41-1385-4cf4-b8f8-dfe05ead3457').
narrative_ontology:cs_reading_relation('567bbb41-1385-4cf4-b8f8-dfe05ead3457', nafta_jurisdictional_boundary__capital_supremacy_reading, influences).
narrative_ontology:cs_reading_relation('567bbb41-1385-4cf4-b8f8-dfe05ead3457', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('567bbb41-1385-4cf4-b8f8-dfe05ead3457', foundational, nondiscrimination_secures_regulatory_policy_space).
narrative_ontology:cs_axiom_status(nondiscrimination_secures_regulatory_policy_space, holdable).
narrative_ontology:cs_axiom_grounding('567bbb41-1385-4cf4-b8f8-dfe05ead3457', nondiscrimination_secures_regulatory_policy_space, conventional).
narrative_ontology:cs_axiom('567bbb41-1385-4cf4-b8f8-dfe05ead3457', secondary, proportionality_deference_bounds_tribunal_review).
narrative_ontology:cs_axiom_status(proportionality_deference_bounds_tribunal_review, holdable).
narrative_ontology:cs_axiom_grounding('567bbb41-1385-4cf4-b8f8-dfe05ead3457', proportionality_deference_bounds_tribunal_review, conventional).
narrative_ontology:cs_reference_frame('567bbb41-1385-4cf4-b8f8-dfe05ead3457', embedded_liberalism_equilibrium).
narrative_ontology:cs_drift_state('567bbb41-1385-4cf4-b8f8-dfe05ead3457', contemporary_isds_backlash_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('567bbb41-1385-4cf4-b8f8-dfe05ead3457', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_firms_investors).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_law_practitioners).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, taxpayers_defending_states).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_labor_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, dispute_tribunal_arbitrators).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_ministries_state_parties).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, embedded_liberalism_compromise_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, nondiscrimination_exception_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell goods and deploy capital across the covered jurisdictions. They hold standing to bring claims when host-state measures impede market access or investment value, and they receive adjudicated awards and predictable treatment in return. Their operations span many jurisdictions, so they can route activity around any single host state's rules and can choose which forum and which agreement's protections to invoke.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_firms_investors, beneficiary,
    powerful, biographical, mobile, global).

% Counsel claimant firms and defending states in dispute proceedings and advise on treaty-compliant regulatory design. Their fee income scales with dispute volume and with the complexity of the legitimate-objectives boundary they help litigate. They serve whichever side retains them and move between claimant and defense work as demand shifts.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_law_practitioners, beneficiary,
    organized, biographical, mobile, continental).

% Draft and enforce environmental, labor, and health measures within their territory. When a covered measure is challenged, they must defend it in dispute proceedings within the legitimate-objectives boundary, at their own expense, under deadlines and review standards they did not set. They cannot decline jurisdiction: every measure they adopt inside the covered sectors is exposed to challenge for as long as the agreement binds their state.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, payer,
    institutional, generational, trapped, national).

% Ultimately fund the defense costs, tribunal fees, and any awards or settlements their state pays out. They have no procedural role in the disputes that generate these liabilities and no direct channel to contest where the boundary sits; their exposure arrives through the state budget after outcomes are decided.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, taxpayers_defending_states, payer,
    moderate, biographical, trapped, national).

% Negotiate the agreements, administer the exception clauses, appoint or approve adjudicators, and decide whether to comply with, negotiate away, or terminate dispute provisions. They also argue the defenses when their state is respondent, so they carry the framework's defense costs alongside their stewardship of it. Renegotiation or withdrawal is available to them but carries diplomatic and market-access costs they weigh against domestic pressure.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_ministries_state_parties, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_ministries_state_parties, payer).

% Sit on the panels and tribunals that decide where the boundary between trade obligations and protected domestic policy space actually falls in each case. They are appointed case by case from a small professional pool, are paid per sitting, and their doctrinal writings shape which future measures are considered challengeable. They hold no continuing office and rotate across forums.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, dispute_tribunal_arbitrators, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, dispute_tribunal_arbitrators, beneficiary).

% Campaign for stronger environmental and labor protection and for explicit carve-outs from challenge. They have no standing in investor-state or state-state proceedings, no seat in most renegotiations beyond observer pressure, and no formal channel to contest awards that chill the measures they pursue. Their leverage runs through domestic politics and public campaigns directed at the ministries.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_labor_advocates, excluded,
    organized, biographical, constrained, national).

% Map the case law, measure defense success rates, and document the gap between the text's balance language and the docket's outcomes. They publish critiques and reform proposals that feed into renegotiations and tribunal reasoning but hold no decision rights over the boundary.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_firms_investors).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__embedded_liberalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, enforceable framework for cross-border market access: exporters and investors get predictable treatment across jurisdictions, and state parties get a rules-based channel for regulatory disputes that would otherwise escalate into unilateral retaliation. The framework also coordinates expectations about which domestic measures are challengeable and which fall inside protected policy space.
% TRANSFER_FUNCTION: Moves adjudicated market access and monetary awards from state parties to exporting firms and investors; moves the cost of defending domestic regulation (litigation fees, tribunal costs, settlement exposure) from those firms to regulatory agencies and taxpayers; moves a share of regulatory decision-making from domestic agencies into adjudicative review.
% ABSENT_VOICES: Environmental, labor, and public-health constituencies whose regulations are chilled or challenged have no standing in investor-state or state-state dispute processes; subnational governments whose measures are challenged are represented only indirectly by the state party. Only investors and state parties hold seats, so the boundary's placement is set without the voices with the strongest stake in the policy-space half of the bargain.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, market access would revert to bilateral bargaining and retaliation cycles, investment protections would collapse to host-state domestic law, and the litigation industry organized around the boundary would dissolve; trade flows, investment siting, and regulatory drafting practice would all rearrange around the new uncertainty.
% FOUNDING_PROBLEM: The postwar embedded-liberalism problem: how to secure open markets and predictable cross-border treatment without stripping states of the domestic regulatory capacity (environmental, labor, health) that domestic coalitions demand — the compromise Ruggie described as legitimizing the multilateral trading order.
% FOUNDING_PROBLEM_CORROBORATION: Trade historians and international-political-economy scholars attest the founding bargain and its design; tribunal jurisprudence applying proportionality-style deference to legitimate-objectives defenses attests that the policy-space half is textually and doctrinally real; civil-society litigation critiques, several states' ISDS withdrawals and the USMCA's narrowing of investor-state access attest that the balance is disputed in operation. Corroboration from outside the beneficiary set exists for both the bargain's reality and its contested tilt — no single outside source attests the balance currently holds.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the reading's design caps what can be taken — regulators keep defensive authority inside the legitimate-objectives boundary — but invoking the defense is priced, and the price falls on parties who cannot exit: defense costs, tribunal fees, awards, and the anticipatory chilling of measures never adopted. Suppression 0.50: ratified obligations bind states and exit (withdrawal, wholesale termination) carries diplomatic and market-access costs, but the framework prohibits no measure outright — it prices and adjudicates them, so suppression is moderate rather than high. Theater_ratio 0.30: the policy-space language and exception clauses do real work (defenses win often enough that the protected zone is not fictional), but a rising share of framework activity is performative balance — communiqués and side agreements affirming regulatory autonomy while docket practice tilts toward claimant leverage. Accessibility_collapse 0.45: alternatives remain live — per-agreement carve-outs, ISDS terminations, the USMCA's narrowing of investor-state access, withdrawal itself — so the framework does not foreclose exit routes the way a natural limit would. Resistance 0.55: sustained civil-society campaigns, state pushback, and the backlash wave of terminations and renegotiations meet the framework continuously. The three tracked metrics share one six-point grid (T=0..30, roughly 1994–2024): extraction and enforcement rise through the claim-proliferation era (Ethyl, Metalclad, the Philip Morris line) then plateau and ease slightly as carve-outs and terminations mature, while theater peaks mid-interval when balance rhetoric is loudest relative to docket outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The same treaty text reads as enforceable property from the investor seat — the exceptions are hurdles to clear — and as a cost schedule from the regulator seat — the protected zone must be purchased through litigation each time it is invoked. The ministry seat experiences both at once: it administers the bargain, negotiates the exceptions, and then argues the defenses when challenged, so its position is genuinely dual. Taxpayers and advocates sit outside the procedure entirely: liabilities arrive post-hoc through budgets and chill arrives pre-hoc through anticipated challenge, with no procedural moment at which either can contest the boundary's placement. Coalition potential among the payer seats is real — advocates plus subnational governments have moved several states to terminate or narrow provisions — but it runs through domestic politics, not the dispute system. The engine should compute divergent per-seat types from this asymmetry: beneficiary seats near coordination, trapped payer seats near extraction, with the ministries straddling.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to low directionality for export firms and investors (they collect access and awards, and their multi-jurisdictional footprint gives them arbitrage-grade mobility) and for trade-law practitioners (fee flows scale with dispute volume). The victim declarations map to high directionality for domestic regulatory agencies (trapped: every covered measure they adopt is exposed and they cannot decline jurisdiction) and for taxpayers (trapped: liability arrives through the budget with no procedural seat). Trade ministries sit near-symmetric: they administer and benefit from the framework's stability while absorbing defense costs as secondary payers, so the derivation should place them mid-range rather than at either end. Tribunal arbitrators collect per-dispute fees — a moderately beneficiary-side position — yet adjudicate the boundary case by case; their structural interest in dispute volume is real but bounded by appointment politics and professional norms. Advocates are excluded rather than coordinated: their exclusion from standing is part of what the framework's procedure maintains, and no override is needed because the role and exit declarations already capture these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling open markets with domestic regulatory capacity — is live but contested, so no mandatrophy resolution is declared: the framework has not outlived its function, though the balance point has drifted and is now actively renegotiated. The tangled_rope claim guards against two symmetric mislabels. Reading the framework as pure coordination ignores that its costs fall on trapped, procedurally unrepresented parties and are actively enforced by a dispute machinery whose professionals collect from volume. Reading it as pure extraction ignores the genuine coordination delivered — measurable trade-flow predictability and a retaliation-substituting dispute channel — and the exceptions' real, sometimes-winning protection of domestic measures. The classification keeps both halves on the table and lets the per-seat computation register the divergence an aggregate label would flatten.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_boundary_scope,
    'Which reading of the nafta_jurisdictional_boundary kernel governs the boundary in operation — this embedded_liberalism_reading, capital_supremacy_reading, or sovereignty_primacy_reading?',
    'Track the distribution of dispute outcomes over time: systematic failure of legitimate-objectives defenses and expansion of claimant-protective doctrines converges practice toward capital supremacy; systematic state non-compliance or wholesale termination of dispute provisions converges toward sovereignty primacy; a stable mixed record with winning defenses sustains this reading.',
    'Under capital supremacy the victim set widens to all regulated domestic activity, epsilon rises and the structure shifts toward pure extraction; under sovereignty primacy the enforcement function atrophies and epsilon falls toward pure coordination. This story''s epsilon of 0.58 and its tangled structure hold only under this reading''s exception scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_boundary_scope, conceptual, 'Kernel-reading contest: which reading of the jurisdictional boundary governs in operation.').

omega_variable(
    regulatory_chill_observation_gap,
    'How much of the framework''s cost transfer is realized (awards, fees, defense costs) versus latent (regulations never adopted because of anticipated challenge)?',
    'Compare regulatory adoption rates in covered versus uncovered sectors and states, controlling for issue salience and political capacity; survey regulators on measures shelved in anticipation of challenge.',
    'If latent chill dominates, the effective burden on the trapped payer seats is materially higher than the litigation-cost measure suggests and their effective extraction approaches the full-target end; if realized costs dominate, the current epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_observation_gap, empirical, 'Realized versus latent (chill) split in the cost transfer.').

omega_variable(
    exception_defense_sincerity,
    'Do the legitimate-objectives exceptions function as genuine policy-space protection, or as a costly screening lottery that mostly ratifies tribunal discretion?',
    'Win-rate analysis of exception and necessity defenses across the case population since proportionality analysis matured, disaggregated by measure type and respondent state capacity.',
    'If defenses rarely succeed, the policy-space half of this reading is performance — epsilon rises and the reading drifts toward capital supremacy in operation; if defenses routinely succeed, the balance holds and the performative share of framework activity falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exception_defense_sincerity, empirical, 'Whether the exceptions mechanism protects policy space or performs protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_embedded_liberalism_tr_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_tr_t0, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_tr_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_tr_t6, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_tr_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_tr_t12, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_tr_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_tr_t18, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_tr_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_tr_t24, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_tr_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(nafta_embedded_liberalism_be_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_be_t0, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_be_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_be_t6, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_be_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_be_t12, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_be_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_be_t18, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_be_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_be_t24, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_be_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(nafta_embedded_liberalism_su_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_su_t0, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_su_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_su_t6, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_su_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_su_t12, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_su_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_su_t18, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_su_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_su_t24, observed).
narrative_ontology:measurement(nafta_embedded_liberalism_su_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(nafta_embedded_liberalism_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, resource_allocation).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% The nafta_jurisdictional_boundary kernel decomposes into three readings with distinct, stable epsilon values over the same treaty text: capital_supremacy_reading (treaty as supreme law; high epsilon, victim set includes all regulated domestic activity), this embedded_liberalism_reading (balanced framework; moderate epsilon via litigation costs and regulatory chill), and sovereignty_primacy_reading (treaty as subordinate coordination; low epsilon, but market access unenforceable). They are linked rather than merged because measuring the boundary through different exception scopes yields different epsilon values — one story per reading, per the epsilon-invariance principle. This reading sits intermediate and feeds both siblings: its exceptions jurisprudence is the evidence base sovereignty advocates cite when carving back, and the claimant leverage it permits is the evidence base supremacy advocates cite for harmonization pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
