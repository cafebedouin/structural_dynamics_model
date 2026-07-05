% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   domain: international_trade_law/development_economics/political_economy
 *
 * SUMMARY:
 *   This story instantiates the market-access reading of the WTO treaty
 *   kernel: trade liberalization is the treaty's primary, symmetric
 *   obligation; non-discrimination and market access constitute the treaty's
 *   core purpose; and Special and Differential Treatment (S&D) provisions are
 *   transitional exceptions with expiring schedules, not permanent structural
 *   accommodations. Under this reading, tariff bindings, subsidy disciplines,
 *   and prohibitions on local content requirements bind all members on an
 *   ostensibly equal basis regardless of differing levels of industrial
 *   development. The coordination function (preventing discriminatory
 *   bilateral tariff wars) is genuine and historically grounded, but the
 *   symmetric-obligation frame concentrates gains with actors who already
 *   possess scale and mobility while compressing the policy space infant
 *   industries and developing-country treasuries would need to reach
 *   competitive parity. The sibling reading — developmental_reading — treats
 *   S&D as permanent structural accommodation and technology transfer as a
 *   core commitment; that is a different constraint, authored separately,
 *   with its own beneficiary/victim structure and its own epsilon.
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
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Treaty Framework — Market Access / Non-Discrimination Reading").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international_trade_law/development_economics/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, 'efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4').
narrative_ontology:cs_kernel_codification('efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4', formalized).
narrative_ontology:cs_authority_grounding('efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4', extraction).
narrative_ontology:cs_interpretation_layer_present('efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4').
narrative_ontology:cs_reading_relation('efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4', wto_treaty_framework__developmental_reading, coexists_with).
narrative_ontology:cs_axiom('efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4', foundational, obligations_bind_symmetrically_regardless_of_development_status).
narrative_ontology:cs_axiom_status(obligations_bind_symmetrically_regardless_of_development_status, holdable).
narrative_ontology:cs_axiom_grounding('efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4', obligations_bind_symmetrically_regardless_of_development_status, conventional).
narrative_ontology:cs_axiom('efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4', foundational, sd_provisions_are_transitional_not_structural).
narrative_ontology:cs_axiom_status(sd_provisions_are_transitional_not_structural, holdable).
narrative_ontology:cs_axiom_grounding('efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4', sd_provisions_are_transitional_not_structural, instrumental).
narrative_ontology:cs_reference_frame('efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4', gatt_reciprocal_bargained_concessions).
narrative_ontology:cs_drift_state('efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4', post_doha_round_stalemate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('efaf3bd1-a3bb-4d2d-995f-c9f8a3c723a4', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_exporters).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, advanced_economy_governments).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, global_capital_intensive_industries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, infant_industries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, smallholder_agricultural_producers).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, least_developed_country_treasuries).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, non_discrimination_as_treaty_telos).
narrative_ontology:constraint_vindicates(wto_treaty_framework__market_access_reading, market_access_as_primary_purpose).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and enforce the most-favored-nation and national-treatment disciplines through the dispute settlement system, treating S&D as a time-limited concession rather than a structural feature. They retain the fiscal and regulatory capacity to absorb transition costs domestically and use the binding rules to lock in market access for their own exporters abroad.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, advanced_economy_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, advanced_economy_governments, beneficiary).

% Gain predictable, litigable market access across member states because tariff bindings and non-discrimination rules are treated as the treaty's core purpose rather than as negotiable accommodations. They can relocate production and shift supply chains to exploit whichever jurisdiction offers the most liberalized terms, insulated from any single country's policy reversal.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_exporters, beneficiary,
    organized, biographical, arbitrage, global).

% Benefit from disciplines that treat subsidies and local content requirements as presumptively illegitimate distortions rather than legitimate development tools, since they already possess scale and capital access that infant competitors lack; the symmetric-obligation framing forecloses the policy space that would let challengers catch up.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, global_capital_intensive_industries, beneficiary,
    organized, generational, mobile, global).

% Nascent domestic manufacturers who would need tariff protection, subsidies, or local content mandates to reach competitive scale, but under this reading such measures are treated as time-limited transitional exceptions subject to phase-out schedules and dispute challenge. They cannot exit the treaty framework without their government incurring retaliation or losing market access for other sectors.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, infant_industries, payer,
    powerless, biographical, trapped, national).

% Compete against subsidized agricultural imports from wealthier members whose domestic support programs survive negotiation while the producers' own governments face pressure to liberalize under the symmetric-obligation reading. They have no individual capacity to relocate, diversify export markets, or absorb price shocks from sudden market opening.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, smallholder_agricultural_producers, payer,
    powerless, biographical, trapped, national).

% Lose tariff revenue that historically funded a large share of public spending as bound rates fall under the universal liberalization schedule, while S&D transition periods expire before administrative and industrial capacity has caught up. Formal withdrawal from WTO membership would cut off market access and aid conditionality tied to trade-policy compliance, so exit is theoretically available but practically foreclosed.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, least_developed_country_treasuries, payer,
    moderate, generational, constrained, national).

% Adjudicates whether S&D measures and industrial policy instruments comply with the non-discrimination and market-access disciplines, treating those disciplines as the treaty's operative baseline and S&D as an interpretively narrow exception. Its rulings compound over time, narrowing the space available for future development policy regardless of the panel's individual composition.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, wto_dispute_settlement_body, observer).

% Argue in negotiating rounds that S&D should be a permanent structural accommodation reflecting asymmetric starting conditions, not a sunset clause, but lack the negotiating leverage of major markets and are frequently outvoted or sidelined in consensus-based rulemaking dominated by larger economies' priorities.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developing_country_negotiators, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__market_access_reading, multinational_exporters).
narrative_ontology:fixing_cost_class(wto_treaty_framework__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single predictable rule set for cross-border trade — binding tariff schedules, non-discrimination commitments, and a dispute mechanism — so that firms and governments do not have to renegotiate market access bilaterally with every trading partner.
% TRANSFER_FUNCTION: Moves policy space and fiscal capacity from developing-country governments and their nascent industries to firms and governments already possessing scale, by treating universal liberalization as the treaty's baseline purpose and confining protective measures to an expiring exception category.
% ABSENT_VOICES: Infant industries and smallholder producers have no seat at treaty negotiations or dispute panels; their interests are represented only secondhand through government negotiators who face asymmetric bargaining power relative to advanced-economy blocs and must trade concessions across unrelated sectors to preserve any protective measure at all.
% DISAPPEARANCE_RATIONALE: If the market-access reading's disciplines vanished overnight, tariff bindings and non-discrimination obligations would no longer constrain domestic industrial policy, and governments would regain latitude to deploy subsidies, local content rules, and protective tariffs without dispute-panel exposure — supply chains built on locked-in market access would face renegotiation, and multinational exporters would lose the litigable predictability the framework currently guarantees them.
% FOUNDING_PROBLEM: The postwar trading system was built to prevent the beggar-thy-neighbor tariff wars and discriminatory bilateral deals that had fragmented world trade and deepened the Depression, by committing members to reciprocal, non-discriminatory market opening.
% FOUNDING_PROBLEM_CORROBORATION: Advanced-economy trade ministries and export-industry associations attest the founding problem remains live — protectionist relapse is an ongoing risk requiring binding universal disciplines. UNCTAD economists, developing-country negotiating blocs, and independent trade economists outside the beneficiary set attest that the original problem (preventing discriminatory bilateral fragmentation) has been substantially solved, and that the current framework's insistence on symmetric obligation now functions primarily to foreclose industrial-policy tools that the framework's own historical beneficiaries used during their development.
narrative_ontology:disappearance_verdict(wto_treaty_framework__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__market_access_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__market_access_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.71 at interval end) and rising over the 1995-2025 interval because the treaty's binding disciplines have progressively narrowed the industrial-policy toolkit available to developing members as S&D transition periods expire, while the dispute settlement body's accumulated jurisprudence has interpreted exceptions narrowly. Suppression sits at 0.62 — moderate-high — reflecting that formal withdrawal is legally available to any member but practically foreclosed by the market-access and aid-conditionality consequences of exit. Theater ratio is comparatively low (0.28) because the enforcement machinery (panels, appellate mechanisms, retaliation authorization) performs a real function; the constraint is not primarily performative, it is actively extractive.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (advanced-economy governments, dispute settlement body), this reading of the treaty appears as principled universal coordination — a level playing field that treats all members alike. From the payer seats (infant industries, smallholder producers, LDC treasuries), the identical rule structure operates as an asymmetric extraction mechanism: formally symmetric obligations applied to structurally asymmetric starting conditions reproduce and compound the initial gap. The engine's per-seat computation is expected to diverge sharply between these two groups of stakeholders precisely because the same textual rule maps to different lived structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational exporters and capital-intensive industries sit near the full-beneficiary end: they already hold the mobility and scale the symmetric-obligation framing rewards, and their exit options are arbitrage-grade. Advanced-economy governments occupy a dual beneficiary/agenda-setter position — they administer the disciplines and disproportionately shape how exceptions are interpreted. Infant industries and smallholder producers sit near the full-target end: trapped exit, powerless bargaining position, and direct exposure to the liberalization schedule's costs. Least-developed country treasuries occupy an intermediate but still target-leaning position — formally sovereign, practically constrained by market-access and conditionality linkages.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — discriminatory bilateral tariff wars fragmenting world trade — is substantially solved; multilateral binding tariff schedules have functioned as intended for that narrow purpose since 1947-1995. But the market-access reading extends the same universal-symmetric logic to a domain (industrial development policy) where the original coordination rationale does not straightforwardly apply, since infant industries face a collective-action problem (needing temporary protection to reach scale) that is structurally different from the beggar-thy-neighbor tariff-war problem the GATT/WTO system was built to solve. Treating S&D as merely transitional, rather than recognizing an ongoing coordination need distinct from the original founding problem, is the mechanism by which coordination language continues to justify what is, for the affected group, an extractive constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symmetric_obligation_vs_structural_asymmetry,
    'Does treating liberalization obligations as formally symmetric across members with radically different industrial starting conditions constitute genuine equal treatment, or does formal symmetry applied to substantive asymmetry function as a mechanism of extraction?',
    'Comparative historical analysis of currently-advanced economies'' own use of tariffs, subsidies, and local content requirements during their industrialization periods, set against the policy space available to developing members under current WTO disciplines.',
    'If historical asymmetry in policy-space usage is confirmed, the market-access reading''s claim to be neutral coordination is substantially undermined, supporting reclassification toward a more clearly extractive structure; if usage patterns were comparable, the symmetric-obligation framing is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetric_obligation_vs_structural_asymmetry, conceptual, 'Whether formal symmetry under asymmetric conditions is neutral coordination or extraction.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the WTO treaty framework''s actual textual and negotiating history closer to the market-access reading (S&D as transitional exception) or the developmental_reading (S&D as permanent structural accommodation), or is the treaty text itself genuinely underdetermined between these readings?',
    'Close textual and negotiating-history analysis of the original GATT Part IV and Doha Development Agenda commitments, cross-checked against how S&D provisions have actually been treated in dispute settlement jurisprudence over time.',
    'If dispute settlement jurisprudence has consistently narrowed S&D over time despite ambiguous founding text, that trajectory itself is evidence the market-access reading has become dominant in practice regardless of the text''s original intent — which would mean this story''s classification reflects an institutionally settled rather than merely textually asserted reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the market-access and developmental readings are equally supported by treaty text/history or whether one has become institutionally dominant.').

omega_variable(
    infant_industry_protection_efficacy,
    'Would extended or permanent S&D protections actually enable infant industries to reach competitive scale, or would they simply entrench inefficient domestic producers indefinitely without ever achieving the development goal?',
    'Comparative case studies of countries that used extended protection successfully (East Asian industrializers) versus countries where extended protection failed to produce competitive industries, controlling for complementary policies (education, infrastructure, macroeconomic stability).',
    'If protection reliably fails without complementary conditions, the developmental_reading''s victim-set framing for market-access disciplines is weakened; if protection is a necessary (if insufficient) condition frequently present in successful cases, the market-access reading''s compression of policy space is more clearly extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(infant_industry_protection_efficacy, empirical, 'Whether S&D protections causally enable development or merely entrench inefficiency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_treaty_framework__market_access_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(wto__tr_t2000, wto_treaty_framework__market_access_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(wto__tr_t2005, wto_treaty_framework__market_access_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(wto__tr_t2010, wto_treaty_framework__market_access_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(wto__tr_t2015, wto_treaty_framework__market_access_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(wto__tr_t2020, wto_treaty_framework__market_access_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(wto__tr_t2025, wto_treaty_framework__market_access_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_treaty_framework__market_access_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(wto__be_t2000, wto_treaty_framework__market_access_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(wto__be_t2005, wto_treaty_framework__market_access_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(wto__be_t2010, wto_treaty_framework__market_access_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(wto__be_t2015, wto_treaty_framework__market_access_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(wto__be_t2020, wto_treaty_framework__market_access_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(wto__be_t2025, wto_treaty_framework__market_access_reading, base_extractiveness, 2025, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_treaty_framework__market_access_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(wto__su_t2000, wto_treaty_framework__market_access_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(wto__su_t2005, wto_treaty_framework__market_access_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(wto__su_t2010, wto_treaty_framework__market_access_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(wto__su_t2015, wto_treaty_framework__market_access_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(wto__su_t2020, wto_treaty_framework__market_access_reading, suppression_requirement, 2020, 0.61).
narrative_ontology:measurement(wto__su_t2025, wto_treaty_framework__market_access_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__market_access_reading, 0.1).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, developmental_reading).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_dispute_settlement_jurisprudence).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, structural_adjustment_conditionality).

% DUAL FORMULATION NOTE:
% This constraint and developmental_reading are sibling readings of the single wto_treaty_framework kernel. Both readings share the same treaty text and dispute-settlement institution but instantiate structurally different constraints: market_access_reading treats S&D as a sunsetting exception and produces a high-epsilon, tangled-rope structure with infant industries and LDC treasuries as victims and multinational exporters as beneficiaries; developmental_reading treats S&D as a permanent structural commitment and would produce a substantially different beneficiary/victim structure and epsilon. They are linked here rather than merged because the ε-invariance principle requires each reading to carry its own stable ε — attempting to average or hedge across the readings would violate that principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
