% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__market_access_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: wto_treaty_framework__market_access_reading
 *   human_readable: WTO Treaty Framework — Market Access / Non-Discrimination Reading
 *   domain: international_trade_law/development_economics
 *
 * SUMMARY:
 *   This constraint captures the market-access reading of the WTO treaty
 *   framework kernel: trade liberalization is a symmetric, universal
 *   obligation binding all members equally; non-discrimination (MFN, national
 *   treatment) and guaranteed market access are the treaty's primary purpose;
 *   and Special and Differential Treatment (S&D) provisions are transitional
 *   accommodations to be phased out as members develop, not permanent
 *   structural rights. Under this reading, tariff bindings, subsidy
 *   disciplines, and local-content prohibitions apply with increasing force
 *   to developing members over time, and dispute settlement jurisprudence has
 *   generally treated S&D language as aspirational or time-limited rather
 *   than judicially enforceable on equal footing with market-access
 *   obligations. This is a distinct constraint from the developmental_reading
 *   sibling, which treats S&D as a permanent structural commitment
 *   recognizing asymmetric starting conditions — the two readings produce
 *   different victim sets, different epsilon trajectories, and different
 *   verdicts on the same treaty text, which is why they are authored as
 *   separate constraints linked through network.affects_constraints rather
 *   than as one story with a measurement parameter.
 *
 * KEY AGENTS:
 *   - multinational_corporations: primary beneficiary (institutional/arbitrage) — captures guaranteed market access without bearing reciprocal developmental cost
 *   - advanced_economy_exporters: primary beneficiary (powerful/mobile) — locks in competitive position achieved during their own protected development
 *   - wto_secretariat_and_dispute_settlement_body: agenda_setter (institutional/analytical) — administers and enforces the symmetric-obligation interpretation through binding jurisprudence
 *   - infant_industries: primary target (powerless/trapped) — loses the policy tools used historically by every currently-advanced economy
 *   - smallholder_agricultural_producers: primary target (powerless/trapped) — faces subsidized competition while domestic protective capacity is constrained
 *   - low_income_country_treasuries: secondary target (moderate/constrained) — loses tariff revenue and industrial-policy latitude
 *   - developing_country_trade_negotiators: excluded voice (organized/constrained) — advances the developmental_reading position but loses ground in dispute settlement jurisprudence
 *   - trade_economists_and_development_scholars: analytical observer (analytical/analytical) — documents the historical industrial-policy record without adjudicating the interpretive contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, 0.71).
domain_priors:suppression_score(wto_treaty_framework__market_access_reading, 0.68).
domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Treaty Framework — Market Access / Non-Discrimination Reading").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international_trade_law/development_economics").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, 'fe162e18-b326-4d1f-85d0-d512eb9713e5').
narrative_ontology:cs_kernel_codification('fe162e18-b326-4d1f-85d0-d512eb9713e5', formalized).
narrative_ontology:cs_authority_grounding('fe162e18-b326-4d1f-85d0-d512eb9713e5', lineage).
narrative_ontology:cs_interpretation_layer_present('fe162e18-b326-4d1f-85d0-d512eb9713e5').
narrative_ontology:cs_reading_relation('fe162e18-b326-4d1f-85d0-d512eb9713e5', wto_treaty_framework__developmental_reading, coexists_with).
narrative_ontology:cs_axiom('fe162e18-b326-4d1f-85d0-d512eb9713e5', foundational, liberalization_obligation_is_symmetric_and_universal).
narrative_ontology:cs_axiom_status(liberalization_obligation_is_symmetric_and_universal, holdable).
narrative_ontology:cs_axiom_grounding('fe162e18-b326-4d1f-85d0-d512eb9713e5', liberalization_obligation_is_symmetric_and_universal, conventional).
narrative_ontology:cs_axiom('fe162e18-b326-4d1f-85d0-d512eb9713e5', foundational, sd_provisions_are_time_limited_transitional_accommodation).
narrative_ontology:cs_axiom_status(sd_provisions_are_time_limited_transitional_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('fe162e18-b326-4d1f-85d0-d512eb9713e5', sd_provisions_are_time_limited_transitional_accommodation, conventional).
narrative_ontology:cs_reference_frame('fe162e18-b326-4d1f-85d0-d512eb9713e5', uruguay_round_single_undertaking).
narrative_ontology:cs_drift_state('fe162e18-b326-4d1f-85d0-d512eb9713e5', post_doha_stalemate_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe162e18-b326-4d1f-85d0-d512eb9713e5', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, advanced_economy_exporters).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, established_market_incumbents).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, infant_industries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, smallholder_agricultural_producers).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, low_income_country_treasuries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_country_trade_negotiators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate across many jurisdictions and benefit directly from tariff bindings, national treatment, and market access commitments that guarantee entry into member markets on non-discriminatory terms. Can relocate production and shift supply chains to exploit whichever member's commitments are most favorable; face essentially no binding cost from the non-discrimination obligation because they already meet its terms by virtue of scale.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Gain guaranteed access to developing-country markets on the same terms as domestic producers, without needing reciprocal accommodation for their own already-mature industrial base. The symmetric-obligation framing treats their historical protection period as closed history rather than an ongoing entitlement, locking in the current competitive gap.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, advanced_economy_exporters, beneficiary,
    powerful, generational, mobile, global).

% Administers and enforces the non-discrimination and market-access obligations through binding dispute settlement; treats S&D provisions as best-endeavor exceptions rather than enforceable rights, and treats tariff bindings, subsidy disciplines, and local-content prohibitions as the treaty's core, judicially enforceable obligations. Sets the interpretive default that liberalization is the baseline and departures require justification.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, wto_secretariat_and_dispute_settlement_body, agenda_setter,
    institutional, civilizational, analytical, global).

% Nascent manufacturing and technology sectors in developing economies that would historically have used tariffs, local-content requirements, and subsidies to build scale before facing international competition. Under this reading those tools are treated as trade-distorting deviations subject to challenge and phase-out, so these industries face mature multinational competitors on nominally equal terms while still building basic capacity. Exit means either never developing the sector or absorbing losses through early exposure.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, infant_industries, payer,
    powerless, biographical, trapped, national).

% Compete against heavily subsidized agricultural imports from wealthy members whose domestic support programs are treated as legacy commitments while their own governments' capacity to protect food security sectors is constrained by market-access and non-discrimination rules. Cannot relocate; livelihoods are tied to land and local markets that are now open to subsidized competition.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, smallholder_agricultural_producers, payer,
    powerless, biographical, trapped, national).

% Lose tariff revenue that historically funded a significant share of public budgets, and are constrained from using industrial policy tools (local content rules, directed subsidies) that wealthier members used during their own development. Face dispute settlement exposure if they attempt policies that resemble discrimination against foreign goods, even when framed as developmental necessity. Can negotiate transition periods but these are time-limited under this reading, not permanent accommodations.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, low_income_country_treasuries, payer,
    moderate, generational, constrained, national).

% Argue in negotiating rounds that S&D provisions should be permanent structural accommodations reflecting historically asymmetric starting conditions, not temporary transitional exceptions to a universal liberalization norm. Their position is formally heard in negotiating rounds but structurally loses ground in dispute settlement, where the treaty text's market-access and non-discrimination articles are treated as the operative obligations and S&D language is read as aspirational or time-bound.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developing_country_trade_negotiators, excluded,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, developing_country_trade_negotiators, payer).

% Study the empirical record of industrial policy in currently-advanced economies (tariff protection, subsidies, forced technology transfer during their own development) and compare it with the policy space available to developing members under current treaty interpretation. Their findings feed both readings' arguments but do not themselves adjudicate the dispute settlement body's interpretive practice.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, trade_economists_and_development_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:fixing_cost_class(wto_treaty_framework__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a collective-action problem: without binding non-discrimination and market-access commitments, individual members face incentives to erect protectionist barriers that collectively shrink global trade and invite retaliatory spirals. A rules-based, reciprocally enforceable system of market access lowers transaction costs and predictability for all traders.
% TRANSFER_FUNCTION: Moves policy space and protective capacity from developing-economy industrial and agricultural sectors to established multinational and advanced-economy producers, by treating current market-access levels as the obligatory baseline and treating protective deviations from that baseline as concessions requiring justification and eventual phase-out, regardless of the recipient's stage of development.
% ABSENT_VOICES: Developing-country negotiators raise the developmental-reading position formally in rounds (Doha Development Agenda language, S&D reform proposals) but that position has not translated into binding, permanently codified accommodation in dispute settlement jurisprudence; infant industries and smallholder producers who bear the sharpest costs have no direct seat at treaty negotiation at all — they are represented, imperfectly, by national negotiators balancing many constituencies.
% DISAPPEARANCE_RATIONALE: If the market-access reading's enforcement machinery disappeared overnight — if S&D provisions were treated as permanently binding equal-status commitments rather than transitional exceptions — developing-country members would regain substantial latitude to use tariffs, local-content rules, and directed subsidies for industrial development; multinational corporations and advanced-economy exporters would lose guaranteed, litigable access to protected markets; global trade volumes might contract in the short run while developmental policy space expanded.
% FOUNDING_PROBLEM: The postwar and Uruguay Round trading system was built to prevent a return to 1930s-style beggar-thy-neighbor protectionism and tariff wars that deepened the Depression and fed into global conflict, by locking in reciprocal, predictable, non-discriminatory market access among trading partners.
% FOUNDING_PROBLEM_CORROBORATION: Trade economists outside the WTO Secretariat and independent of major exporting interests (including scholars who have studied the historical industrial-policy record of Britain, the US, Germany, Japan, and South Korea during their own development) corroborate that the beggar-thy-neighbor problem was real and that reciprocal market access solved it among comparably-developed economies — but the same scholars, working independently of both the Secretariat and developing-country delegations, document that applying the symmetric-obligation reading to economies at vastly different development stages recreates a different problem the founding framework never addressed: locking in a competitive hierarchy under the banner of the very principle meant to prevent hierarchical exploitation.
narrative_ontology:disappearance_verdict(wto_treaty_framework__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__market_access_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_treaty_framework__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__market_access_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__market_access_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_treaty_framework__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71 and rising because, under this reading's own operative jurisprudence, the treaty's core obligations (market access, non-discrimination, tariff disciplines) compress steadily against developing members' capacity to protect infant industries and food-security sectors, while S&D provisions have not hardened into enforceable equal-status rights over three decades — the gap between the treaty's stated coordination promise (predictable, reciprocal trade) and its lived effect on asymmetric parties (compressed policy space) has widened, not narrowed, since 1995. Suppression (0.68) reflects the binding force of the Dispute Settlement Body: a developing member that deploys tariffs or local-content rules resembling discrimination faces litigable, enforceable challenge, while a wealthy member's legacy agricultural subsidies face comparatively weaker discipline. Theater ratio (0.32) is moderate: real coordination function exists (predictable market access genuinely reduces trade-war risk) but a growing share of the S&D architecture (technical assistance programs, transition-period extensions) functions more as diplomatic cover for the underlying asymmetric bargain than as a substantive remedy.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational corporations and advanced-economy exporters sit near the full-beneficiary end of directionality: they already meet non-discrimination and market-access standards by virtue of existing scale and market position, so the obligation imposes negligible marginal cost on them while guaranteeing them litigable access to markets they would otherwise have to negotiate bilaterally. Infant industries and smallholder producers sit near the full-target end: they are trapped (national scope, no meaningful exit — abandoning the sector or absorbing exposure are the only options) and the constraint's operative jurisprudence extracts the specific policy tools (tariffs, local content, subsidies) that would let them build competitive scale before facing that access. Low-income country treasuries sit closer to the target end but with somewhat more institutional latitude (moderate power, constrained rather than trapped exit) because sovereign governments retain some negotiating leverage in trade rounds even as dispute settlement forecloses unilateral deviation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — beggar-thy-neighbor protectionism and retaliatory tariff spirals among comparably-developed trading partners — is genuinely live among peer economies and the coordination function there is real (this is why the constraint is authored as tangled_rope, not snare: there is a genuine coordination function, not merely an extraction story with coordination as cover). But the market-access reading applies the same symmetric-obligation logic to economies at vastly different development stages, where the founding problem was never really about protecting an infant industry's first decade — it was about mature economies avoiding mutual escalation. Treating S&D as a temporary exception rather than a permanent structural accommodation extends a solution designed for peer competition onto asymmetric relationships, which is exactly the mandatrophy pattern: a mandate (prevent trade wars among peers) outliving its original scope and being applied to a population (developing economies at early industrial stages) for whom the founding problem was never the operative one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sd_provisions_transitional_or_structural,
    'Are Special and Differential Treatment provisions properly read as temporary transitional exceptions to a universal liberalization norm, or as permanent structural accommodations recognizing that trading partners begin from asymmetric historical starting conditions?',
    'This is the precise textual and jurisprudential question dividing the market_access_reading from the developmental_reading. Resolution would require either a binding Ministerial Conference decision fixing S&D''s legal status permanently, or a sustained line of Appellate Body/dispute panel jurisprudence treating S&D claims as equally enforceable to market-access claims (as opposed to the current practice of treating them as best-endeavor language subject to negotiated, time-limited extensions).',
    'If S&D is authoritatively read as permanent and structural, this constraint''s beneficiary/victim structure inverts substantially — the compression of developing-country policy space would itself become the treaty violation, and epsilon would fall sharply as the ''exception'' becomes the operative baseline for asymmetric members. This is precisely the sibling constraint (developmental_reading), authored separately rather than as a parameter of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sd_provisions_transitional_or_structural, conceptual, 'Whether S&D is transitional exception or permanent structural right — the kernel''s central contested premise.').

omega_variable(
    historical_symmetry_of_starting_conditions,
    'Is it defensible to treat market-access and non-discrimination as symmetric obligations when currently-advanced economies used the very tools now prohibited (tariffs, local content requirements, directed subsidies, forced technology transfer) during their own development?',
    'Comparative economic-historical analysis of industrial policy in Britain, the US, Germany, Japan, and South Korea during their respective development periods, weighed against the counterfactual trajectory of currently-developing economies under present treaty constraints. Chang (2002), Reinert, and similar historical-institutionalist trade scholarship bear directly on this.',
    'If the historical record shows advanced economies systematically used now-prohibited tools to reach their current position, the symmetric-obligation framing is harder to defend as anything other than a ladder-kicking-away move that locks in an achieved competitive hierarchy under a formally neutral rule — strengthening the case that this reading''s coordination story is substantially extraction-driven despite the genuine coordination function it also serves among peer economies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_symmetry_of_starting_conditions, empirical, 'Whether the symmetric-obligation premise is defensible given the documented historical industrial-policy record of currently-advanced economies.').

omega_variable(
    dispute_settlement_asymmetric_application,
    'Does WTO dispute settlement jurisprudence apply market-access and subsidy disciplines more rigorously against developing-member industrial policy than against advanced-member agricultural subsidies and non-tariff barriers?',
    'Systematic empirical review of Dispute Settlement Body case outcomes and remedy severity, sorted by respondent development status and by the sector/policy type challenged (industrial policy vs. agricultural support vs. non-tariff technical barriers).',
    'A demonstrated asymmetry in enforcement rigor would corroborate the suppression score authored here (0.68) as a lived jurisprudential pattern rather than a purely textual reading; an even-handed enforcement record would weaken this reading''s claim to be substantially extractive relative to the developmental reading''s characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispute_settlement_asymmetric_application, empirical, 'Whether enforcement practice, not just treaty text, differentially burdens developing-member policy tools.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_treaty_framework__market_access_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(wto__tr_t2001, wto_treaty_framework__market_access_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(wto__tr_t2008, wto_treaty_framework__market_access_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(wto__tr_t2013, wto_treaty_framework__market_access_reading, theater_ratio, 2013, 0.28).
narrative_ontology:measurement(wto__tr_t2018, wto_treaty_framework__market_access_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(wto__tr_t2025, wto_treaty_framework__market_access_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_treaty_framework__market_access_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(wto__be_t2001, wto_treaty_framework__market_access_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(wto__be_t2008, wto_treaty_framework__market_access_reading, base_extractiveness, 2008, 0.63).
narrative_ontology:measurement(wto__be_t2013, wto_treaty_framework__market_access_reading, base_extractiveness, 2013, 0.66).
narrative_ontology:measurement(wto__be_t2018, wto_treaty_framework__market_access_reading, base_extractiveness, 2018, 0.69).
narrative_ontology:measurement(wto__be_t2025, wto_treaty_framework__market_access_reading, base_extractiveness, 2025, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_treaty_framework__market_access_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(wto__su_t2001, wto_treaty_framework__market_access_reading, suppression_requirement, 2001, 0.53).
narrative_ontology:measurement(wto__su_t2008, wto_treaty_framework__market_access_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(wto__su_t2013, wto_treaty_framework__market_access_reading, suppression_requirement, 2013, 0.62).
narrative_ontology:measurement(wto__su_t2018, wto_treaty_framework__market_access_reading, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(wto__su_t2025, wto_treaty_framework__market_access_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__market_access_reading, 0.1).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, developmental_reading).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_treaty_framework__developmental_reading).

% DUAL FORMULATION NOTE:
% This constraint and wto_treaty_framework__developmental_reading are two readings of a single contested kernel (wto_treaty_framework): the WTO treaty text and its S&D provisions. They are not the same constraint viewed from different angles — they instantiate different beneficiary/victim structures, different epsilon trajectories, and different classifications from the same underlying legal text, because they disagree about what that text obligates (S&D as decaying exception vs. S&D as permanent structural right). Per the epsilon-invariance principle, each reading is authored as its own story with its own metrics; they are linked here rather than merged into one parameterized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
