% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   This story instantiates the market-access reading of the WTO treaty
 *   framework kernel: trade liberalization as a symmetric universal
 *   obligation binding all members equally, with non-discrimination and
 *   market access as the treaty's primary purpose, and
 *   special-and-differential (S&D) provisions treated as temporary
 *   transitional exceptions on a phase-out trajectory rather than permanent
 *   structural accommodations. This is a distinct constraint from the sibling
 *   developmental_reading (constraint_id: developmental_reading, not authored
 *   here), which treats S&D as a permanent, equal-status treaty commitment
 *   recognizing asymmetric starting conditions. Under this reading's own
 *   lights, the treaty's operative case law and negotiating baseline
 *   substantially favor the symmetric-obligation framing, producing high
 *   extraction on infant-industry tariff regimes, subsidies, and
 *   local-content requirements as these are progressively read down as
 *   violations rather than protected policy space.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, 0.71).
domain_priors:suppression_score(wto_treaty_framework__market_access_reading, 0.62).
domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Treaty Framework — Market Access / Non-Discrimination Reading").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international_trade_law/development_economics").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, 'f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9').
narrative_ontology:cs_kernel_codification('f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9', fixed_text).
narrative_ontology:cs_authority_grounding('f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9', extraction).
narrative_ontology:cs_interpretation_layer_present('f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9').
narrative_ontology:cs_reading_relation('f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9', wto_treaty_framework__developmental_reading, coexists_with).
narrative_ontology:cs_axiom('f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9', foundational, liberalization_obligation_is_symmetric_and_universal).
narrative_ontology:cs_axiom_status(liberalization_obligation_is_symmetric_and_universal, holdable).
narrative_ontology:cs_axiom_grounding('f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9', liberalization_obligation_is_symmetric_and_universal, conventional).
narrative_ontology:cs_axiom('f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9', foundational, sd_provisions_are_transitional_derogations_not_rights).
narrative_ontology:cs_axiom_status(sd_provisions_are_transitional_derogations_not_rights, holdable).
narrative_ontology:cs_axiom_grounding('f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9', sd_provisions_are_transitional_derogations_not_rights, conventional).
narrative_ontology:cs_reference_frame('f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9', gatt_uruguay_round_symmetric_liberalization_bargain).
narrative_ontology:cs_drift_state('f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9', post_doha_round_stalemate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9fefa6f-d0ed-44e6-b98d-48760d9ee1f9', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_manufacturing_corporations).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, advanced_economy_exporters).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, global_trade_law_bar).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, infant_industries_in_developing_economies).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, smallholder_agricultural_producers).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, domestic_industrial_policy_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate cross-border supply chains that depend on predictable, low tariff access and enforceable non-discrimination rules across member markets. Benefit directly when a member state's local-content requirement or infant-industry tariff is successfully challenged and removed under the market-access reading of the treaty. Can relocate production across jurisdictions to arbitrage remaining policy differences.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_manufacturing_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Gain guaranteed market access to developing-economy markets under the non-discrimination principle, without symmetric obligation to compensate for historical industrialization advantages already banked before the disciplines applied to them. Litigate through dispute settlement when developing-country measures are read as market-access violations.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, advanced_economy_exporters, beneficiary,
    powerful, generational, mobile, global).

% Trade lawyers, panelists, and dispute-settlement specialists whose careers and case volume depend on the market-access reading remaining the operative interpretive frame — every S&D exception litigated as a narrow, temporary carve-out generates billable interpretive work and reinforces the reading's centrality.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, global_trade_law_bar, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, global_trade_law_bar, agenda_setter).

% Negotiate and enforce treaty text asserting symmetric obligations as the baseline and S&D as a temporary derogation to be phased out. Sponsor dispute-settlement actions against developing-country tariff and subsidy regimes framed as market-access violations. Retain policy space they used during their own industrialization by having exited that phase before the disciplines existed.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developed_country_trade_ministries, agenda_setter,
    institutional, generational, arbitrage, global).

% Nascent manufacturing and technology sectors that require tariff protection, local-content requirements, or subsidy support to reach competitive scale — the same tools advanced economies used historically. Under the market-access reading these tools are treated as violations subject to phase-out schedules regardless of whether the sector has actually matured. Cannot exit the treaty system without losing all market access.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, infant_industries_in_developing_economies, payer,
    powerless, biographical, trapped, national).

% Face import competition from subsidized agricultural exports once tariff bindings are reduced under the symmetric-obligation reading, while facing continued (WTO-compliant) domestic support subsidies in exporting countries. Have no meaningful voice in dispute settlement and cannot relocate or diversify quickly.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, smallholder_agricultural_producers, payer,
    powerless, immediate, trapped, local).

% The policy toolkit itself — tariff schedules, subsidy authority, local-content mandates — is progressively foreclosed as each S&D exception is treated as a sunset item rather than a standing entitlement, narrowing what future governments in developing economies can do regardless of which party holds office.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, domestic_industrial_policy_capacity, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(wto_treaty_framework__market_access_reading, domestic_industrial_policy_capacity).

% Developing-country trade negotiators, UNCTAD economists, and South-South coalitions who argue S&D should be permanent structural accommodation and technology transfer a core obligation, not a temporary derogation. Present at negotiating rounds but structurally outvoted or out-resourced in dispute-settlement litigation capacity, and their reading has never become the treaty's operative interpretive baseline.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developmental_reading_advocates, excluded,
    organized, generational, constrained, global).

% Adjudicates disputes between the readings by applying treaty text; its accumulated case law has, in practice, tended to treat S&D provisions as narrowly construed exceptions to a symmetric-obligation baseline rather than as coequal structural commitments, which is itself part of what fixes the market-access reading as dominant.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, wto_dispute_settlement_body, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__market_access_reading, multinational_manufacturing_corporations).
narrative_ontology:fixing_cost_class(wto_treaty_framework__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes predictable, rules-based market access and non-discriminatory treatment across member economies, reducing the transaction costs and arbitrariness that would otherwise attend bilateral trade relations — real coordination value for firms and consumers seeking stable cross-border commerce.
% TRANSFER_FUNCTION: Moves policy space and infant-industry protection capacity from developing-economy governments and their nascent industrial sectors to established multinational exporters and manufacturers, via progressively narrowed tariff bindings, phased-out S&D exceptions, and dispute-settlement rulings that treat industrial-policy tools as temporary derogations rather than standing rights.
% ABSENT_VOICES: Smallholder producers and infant-industry workers have no direct standing in WTO dispute settlement; their interests are represented, if at all, by national trade ministries whose negotiating capacity and legal resources are asymmetric to those of advanced-economy counterparts and multinational corporate legal teams.
% DISAPPEARANCE_RATIONALE: If the market-access reading's operative dominance disappeared and the developmental reading became the baseline instead, tariff schedules, subsidy disciplines, and S&D timelines would be renegotiated as permanent structural accommodations; developing economies would regain standing policy space currently treated as a sunset item, and the dispute-settlement case law built on the symmetric-obligation premise would need to be substantially revisited.
% FOUNDING_PROBLEM: Post-war and post-colonial trade architecture needed a mechanism to prevent beggar-thy-neighbor protectionism and to provide predictable market access after decades of trade-destroying tariff wars — the coordination problem was genuine and shared across rich and poor economies alike.
% FOUNDING_PROBLEM_CORROBORATION: Advanced-economy trade ministries and the global trade law bar attest the symmetric-obligation framing is the treaty's settled purpose and S&D was always intended as transitional. UNCTAD economists, developmental-reading advocates, and independent economic historians of industrial policy (documenting how currently advanced economies used tariffs and subsidies during their own industrialization) attest from outside the beneficiary set that the founding problem included accommodating asymmetric starting conditions as a permanent feature, not a temporary exception — this corroboration is external to the market-access reading's own beneficiaries.
narrative_ontology:disappearance_verdict(wto_treaty_framework__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__market_access_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction (0.71 by 2025) reflects the accumulating effect of dispute-settlement rulings and negotiating-round outcomes that treat developing-economy industrial policy tools as narrow, sunsetting exceptions rather than standing rights — the same tools advanced economies used without equivalent constraint during their own industrialization. Suppression (0.62) is structural: the treaty's binding dispute-settlement mechanism and cross-retaliation authority make non-compliance costly and exit effectively impossible without losing market access altogether. Theater ratio is comparatively low (0.28) because the market-access function is genuinely operative — real tariff reduction and real non-discrimination enforcement occur — this is not a hollowed-out shell, it is a functioning but asymmetrically extractive coordination structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational manufacturers, advanced-economy exporters, and the trade law bar sit near the beneficiary end: they collect market access, litigation business, and supply-chain predictability without bearing the phase-out costs. Infant industries, smallholder producers, and the abstract policy-capacity entity sit near the full-target end: trapped exit (leaving the WTO forfeits market access entirely), powerless bargaining position, and the constraint's core enforcement machinery (dispute settlement, cross-retaliation) is aimed structurally at their protective measures.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents this reading from being mislabeled as pure extraction (a snare) or as pure coordination (a rope): there is a genuine, non-fabricated coordination function (predictable market access reduces real transaction costs for cross-border trade) that coexists with asymmetric extraction (infant industries pay through the same non-discrimination machinery that benefits established exporters). Collapsing this into a snare would erase the real coordination benefit; collapsing it into a rope would erase the asymmetric cost structure documented in the phase-out treatment of S&D. The founding-problem mismatch check (status=contested, verdict=world_rearranges) flags exactly this tension for downstream review rather than resolving it by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sd_provisions_temporary_or_structural,
    'Are special-and-differential provisions correctly read as temporary transitional exceptions to a symmetric baseline obligation, or as permanent structural accommodations recognizing asymmetric starting conditions that are coequal with the non-discrimination principle?',
    'This is the committer-level disagreement between the market_access_reading and developmental_reading instantiations of the wto_treaty_framework kernel. Resolution would require either a binding reinterpretation by the WTO Ministerial Conference/Appellate mechanism settling the treaty''s operative purpose, or a sustained shift in dispute-settlement case law precedent toward treating S&D as a standing right rather than a sunset item. No such resolution currently exists; both readings remain live and are held by different negotiating coalitions.',
    'If the developmental reading were adopted as the operative baseline, infant industries and domestic industrial policy capacity would exit the victim set, S&D phase-out schedules would be abandoned in favor of permanent accommodation, and this constraint''s extraction score would fall substantially — this is precisely why the two readings are authored as separate constraints rather than reconciled into one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sd_provisions_temporary_or_structural, conceptual, 'Committer-level disagreement: which reading of the WTO kernel is the treaty''s true operative purpose.').

omega_variable(
    historical_asymmetry_grounding,
    'Does the historical fact that currently-advanced economies used tariffs, subsidies, and local-content requirements extensively during their own industrialization (largely before equivalent disciplines existed) ground a normative claim that developing economies are owed equivalent policy space now, or is this historical asymmetry irrelevant to the treaty''s forward-looking symmetric-obligation design?',
    'Economic-historical scholarship on industrial policy in currently-advanced economies (documented extensively by economic historians such as Chang, Reinert, and others) versus contemporary trade-theoretic arguments for universal liberalization as welfare-maximizing regardless of historical sequencing. This is fundamentally a normative/value question about what treaty obligations should track, not a purely empirical one.',
    'If historical asymmetry is held to ground a valid claim to differentiated treatment, the market-access reading''s treatment of infant-industry protection as illegitimate rent-seeking is undermined; if historical asymmetry is held irrelevant, the market-access reading''s symmetric-obligation framing is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_asymmetry_grounding, preference, 'Whether historical industrialization asymmetry grounds differentiated treaty obligations.').

omega_variable(
    coordination_extraction_separability_trade,
    'Is the market-access coordination function (predictable cross-border trade, reduced transaction costs) separable from the extraction on infant-industry policy space, or does the coordination function structurally require suppressing that policy space to function?',
    'Comparative analysis of trade regimes that permit greater industrial-policy flexibility (e.g., regional trade agreements with broader S&D carve-outs) versus WTO-disciplined regimes, examining whether market access and predictability degrade meaningfully when infant-industry protections are permitted.',
    'If separable, the extraction on developing-economy policy space is avoidable rent-seeking riding on a genuine coordination function; if inseparable, some of the measured extraction reflects an actual coordination cost rather than pure capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability_trade, empirical, 'Whether market-access coordination requires suppressing industrial policy space to function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_treaty_framework__market_access_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(wto__tr_t2001, wto_treaty_framework__market_access_reading, theater_ratio, 2001, 0.16).
narrative_ontology:measurement(wto__tr_t2007, wto_treaty_framework__market_access_reading, theater_ratio, 2007, 0.19).
narrative_ontology:measurement(wto__tr_t2013, wto_treaty_framework__market_access_reading, theater_ratio, 2013, 0.23).
narrative_ontology:measurement(wto__tr_t2019, wto_treaty_framework__market_access_reading, theater_ratio, 2019, 0.26).
narrative_ontology:measurement(wto__tr_t2025, wto_treaty_framework__market_access_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_treaty_framework__market_access_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(wto__be_t2001, wto_treaty_framework__market_access_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(wto__be_t2007, wto_treaty_framework__market_access_reading, base_extractiveness, 2007, 0.63).
narrative_ontology:measurement(wto__be_t2013, wto_treaty_framework__market_access_reading, base_extractiveness, 2013, 0.67).
narrative_ontology:measurement(wto__be_t2019, wto_treaty_framework__market_access_reading, base_extractiveness, 2019, 0.69).
narrative_ontology:measurement(wto__be_t2025, wto_treaty_framework__market_access_reading, base_extractiveness, 2025, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_treaty_framework__market_access_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(wto__su_t2001, wto_treaty_framework__market_access_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(wto__su_t2007, wto_treaty_framework__market_access_reading, suppression_requirement, 2007, 0.55).
narrative_ontology:measurement(wto__su_t2013, wto_treaty_framework__market_access_reading, suppression_requirement, 2013, 0.58).
narrative_ontology:measurement(wto__su_t2019, wto_treaty_framework__market_access_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(wto__su_t2025, wto_treaty_framework__market_access_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__market_access_reading, 0.1).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, developmental_reading).

% DUAL FORMULATION NOTE:
% This constraint and developmental_reading are the two readings of the wto_treaty_framework kernel. Both share the same treaty text and dispute-settlement institution but diverge on which provision (non-discrimination/market-access vs. S&D/technology-transfer) is the treaty's primary normative commitment and which is the exception. This reading (market_access_reading) authors substantially higher ε because it treats industrial-policy foreclosure as an intended and legitimate feature of a symmetric-obligation design; developmental_reading authors substantially lower ε for the same underlying treaty operation because it treats S&D as the coequal commitment that should prevent that foreclosure from occurring. Per the ε-invariance principle, these are two separate constraint stories, not one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
