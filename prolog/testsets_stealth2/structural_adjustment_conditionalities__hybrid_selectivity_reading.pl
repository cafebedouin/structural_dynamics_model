% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__hybrid_selectivity_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Structural Adjustment Conditionalities — Hybrid Selectivity Reading
 *   domain: economic/political/international
 *
 * SUMMARY:
 *   Since the debt crisis of the early 1980s, crisis lending to sovereign
 *   states has carried policy conditions: fiscal consolidation, subsidy
 *   removal, privatization, trade liberalization, and structural benchmarks
 *   tying each tranche to verified performance. This story authors the
 *   hybrid_selectivity_reading of that arrangement: the discipline is real,
 *   the coordination function is real, and the enforcement is allocated by
 *   geopolitical position — enforced harshly on states without leverage,
 *   waived repeatedly for states whose alignment, basing rights, or conflict
 *   geography major shareholders value. The epsilon referent is the standing
 *   conditionality regime as this reading sees it: genuinely coordinative in
 *   function, selectively extractive in incidence. Time points index years
 *   after 1980 (t=0 is the eve of the Mexican default; t=44 is 2024). See
 *   kernel_context for the reading structure; the sibling readings are
 *   separate constraints, not averaged here.
 *
 * KEY AGENTS:
 *   - imf_executive_board_and_management: agenda-setter (institutional/arbitrage) — designs programs, sets benchmarks, decides waivers case by case
 *   - us_treasury_and_g7_shareholders: primary beneficiary with agenda-setting power (institutional/arbitrage) — quota dominance and informal veto steer enforcement toward allies
 *   - geopolitically_strategic_debtors: beneficiary (moderate/constrained) — repeat borrowers whose strategic value purchases forbearance
 *   - core_private_creditors: beneficiary (powerful/arbitrage) — adjustment channels foreign exchange to debt service and shields them from restructuring losses
 *   - nonstrategic_program_countries: primary target (moderate/trapped) — bear full-force conditionality with no alternative financing once markets close
 *   - program_country_households: primary target (powerless/trapped) — absorb austerity they never negotiated and cannot exit
 *   - program_country_labor_and_civil_society: excluded voice (organized/trapped) — would renegotiate distributional terms; consulted only after core targets are fixed
 *   - china_and_nonwestern_creditors: excluded voice (institutional/arbitrage) — lend outside the framework that defines comparability
 *   - academic_economists_and_policy_analysts: analytical observer (analytical/analytical) — produce the matched-comparison evidence any adjudication of selectivity would rest on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.71).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.76).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Structural Adjustment Conditionalities — Hybrid Selectivity Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "economic/political/international").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'de009113-74c1-42b1-af72-6ac3c6aa9500').
narrative_ontology:cs_kernel_codification('de009113-74c1-42b1-af72-6ac3c6aa9500', formalized).
narrative_ontology:cs_authority_grounding('de009113-74c1-42b1-af72-6ac3c6aa9500', extraction).
narrative_ontology:cs_interpretation_layer_present('de009113-74c1-42b1-af72-6ac3c6aa9500').
narrative_ontology:cs_reading_relation('de009113-74c1-42b1-af72-6ac3c6aa9500', structural_adjustment_conditionalities__creditor_coordination_reading, influences).
narrative_ontology:cs_reading_relation('de009113-74c1-42b1-af72-6ac3c6aa9500', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('de009113-74c1-42b1-af72-6ac3c6aa9500', foundational, enforcement_tracks_geopolitical_alignment).
narrative_ontology:cs_axiom_status(enforcement_tracks_geopolitical_alignment, holdable).
narrative_ontology:cs_axiom_grounding('de009113-74c1-42b1-af72-6ac3c6aa9500', enforcement_tracks_geopolitical_alignment, empirically_contingent).
narrative_ontology:cs_axiom('de009113-74c1-42b1-af72-6ac3c6aa9500', secondary, coordination_costs_fall_on_unaligned_debtors).
narrative_ontology:cs_axiom_status(coordination_costs_fall_on_unaligned_debtors, holdable).
narrative_ontology:cs_axiom_grounding('de009113-74c1-42b1-af72-6ac3c6aa9500', coordination_costs_fall_on_unaligned_debtors, empirically_contingent).
narrative_ontology:cs_reference_frame('de009113-74c1-42b1-af72-6ac3c6aa9500', economically_triggered_uniform_discipline).
narrative_ontology:cs_drift_state('de009113-74c1-42b1-af72-6ac3c6aa9500', contemporary_case_record_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('de009113-74c1-42b1-af72-6ac3c6aa9500', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_private_creditors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, us_treasury_and_g7_shareholders).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, nonstrategic_program_countries).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, program_country_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, program_country_labor_and_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Approves loan programs, sets performance criteria and structural benchmarks, conducts reviews, and decides case by case whether missed targets trigger suspension or waiver. Staff design programs under quota-weighted board guidance; management publicly frames conditions as country-owned. Waiver deliberations happen behind closed doors, and the same institution applies markedly different patience to different borrowers.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, imf_executive_board_and_management, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold dominant quota shares and, by long-standing convention, an effective veto over major decisions. Treasury departments steer program design informally, most visibly where security interests are engaged: allied governments repeatedly receive financing and waivers despite chronic target misses, while the same shareholders press harder terms on unaligned borrowers. The arrangement converts their financial exposure into alliance-management leverage.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, us_treasury_and_g7_shareholders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, us_treasury_and_g7_shareholders, agenda_setter).

% Host bases, anchor alliances, or border conflicts that major powers value. They enter programs repeatedly, miss targets routinely, and keep receiving disbursements through waivers and augmentations. Financing access persists for them where economically similar unaligned states lose it; the price is policy oversight and a security patronage they cannot easily replace.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtors, beneficiary,
    moderate, biographical, constrained, regional).

% Hold sovereign bonds and syndicated loans to program countries. Adjustment programs channel scarce foreign exchange toward debt service and shield them from restructuring losses; they price official-program presence as implicit protection and can sell exposure before programs turn. They concede little in exchange for the continuity the programs secure.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_private_creditors, beneficiary,
    powerful, immediate, arbitrage, global).

% States without geopolitical leverage that meet the arrangement's full force: deep fiscal consolidation, privatization under deadline, cross-conditionality binding every tranche to every target. Once markets close and reserves run down, the program is the only financing path; declining it means default, import collapse, and exclusion from the creditworthiness signal the program confers.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, nonstrategic_program_countries, payer,
    moderate, biographical, trapped, national).

% Absorb the adjustment directly: removed subsidies, frozen wages, user fees for health and education, public-sector layoffs. They did not negotiate the program, cannot vote on its terms before boards approve them, and cannot exit the currency, the tax net, or the country at any meaningful scale.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, program_country_households, payer,
    powerless, immediate, trapped, national).

% Organize strikes and protests against adjustment packages and would renegotiate distributional terms if admitted to program design. Consultation mechanisms exist on paper, but sequencing and core macro targets are settled before participation begins; they bear costs their exclusion keeps off the negotiating table, and their protests register as implementation risk rather than as voice.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, program_country_labor_and_civil_society, excluded,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, program_country_labor_and_civil_society, payer).

% Lend outside the Paris Club and historically outside official-program geometry. Their rise hands some distressed states a second menu and complicates debt treatments; established institutions want their terms harmonized into the existing architecture, and they resist absorption on terms they did not write.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, china_and_nonwestern_creditors, excluded,
    institutional, generational, arbitrage, global).

% Study program outcomes, waiver patterns, and distributional effects across four decades of arrangements. They produce the matched comparisons and panel evidence that any serious adjudication of the selectivity question would rest on, and they disagree sharply among themselves about what the record shows.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, academic_economists_and_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_private_creditors).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides emergency external financing when private capital exits, and solves the creditor collective-action problem: a senior official lender imposing one negotiated fiscal framework stops individual creditors from racing for exit and gives all of them a common expectation of continued debt service. Programs also supply political cover under which finance ministries implement adjustments no coalition could pass unprompted.
% TRANSFER_FUNCTION: Moves foreign exchange and fiscal space from program-country budgets and households toward external debt service; moves policy discretion from elected domestic institutions to program negotiators; and allocates the severity of both transfers by geopolitical alignment rather than by economic need.
% ABSENT_VOICES: Program-country legislatures rarely ratify terms before boards approve them; affected households and labor movements are consulted after core targets are fixed; non-Western creditors sit outside the framework that defines comparability of treatment. Each group would contest distributional terms and enforcement symmetry, and their absence is what lets unanimity at the board stand in for consent.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would force simultaneous disorderly defaults across program countries, close the last standing financing channel for balance-of-payments crises, and throw crisis resolution onto ad hoc creditor committees and great-power bilateral lending. Sovereign crisis management would rebuild around whichever powers filled the vacuum, with the terms of discipline set by whoever held the replacement leverage.
% FOUNDING_PROBLEM: Recurrent balance-of-payments crises and sovereign defaults threatened the postwar trading system; the arrangement was built to lend into crisis while correcting the policies that produced the external gap, returning countries to voluntary market access.
% FOUNDING_PROBLEM_CORROBORATION: Program-country central banks and finance ministries — outside the beneficiary set — attest that acute financing gaps are real and that emergency liquidity remains necessary. Economic historians corroborate the original Bretton Woods mandate from the archival record. Heterodox and postcolonial economists, also outside the beneficiary set, attest that the founding problem has been overtaken by a dependency-reproducing dynamic the original mandate never contemplated. No attestation settles the status; the standing dispute among non-beneficiary witnesses is itself the finding.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71: high, because adjustment transfers are large, sustained, and decoupled from creditor concession, but below the extraction reading's estimate because the liquidity and creditor-coordination functions are genuine and debtor governments themselves seek programs. Suppression is 0.76: persistence depends on closing alternatives — market closure triggers program entry, cross-conditionality links every tranche to every target, and Fund presence signals creditworthiness to private lenders — not on participant preference. Theater ratio is 0.40 and rising across the interval: ownership language, participatory PRSP processes, and results-based frameworks perform inclusivity while core macro targets are settled beforehand; the underlying fiscal operations remain real, so theater stays below majority. Accessibility_collapse is 0.58: bond markets, non-Western creditors, and default exist as alternatives but collapse exactly when the arrangement binds. Resistance is 0.62: four decades of austerity riots, electoral turnover against incumbent reformers, selective defaults, and standing reform coalitions. The measurement series share one grid (t = 0, 8, 16, 24, 32, 38, 44). The mid-interval dip in suppression_requirement (t=24) reflects the HIPC/PRSP softening and the commodity-boom window when middle-income countries regained market access; the subsequent re-hardening tracks the post-2008 return of frontier borrowing and pandemic-era programs. The oscillation is driven by global liquidity cycles, not intermittent reinforcement — the two-tier structure itself never closes. Coalition note: the payer seats' obvious escape is coordinated default or a debtor bloc; the architecture anticipates this — sequential treatment, cross-conditionality, and the reputational penalty of default keep coalition formation expensive, which is why trapped exit rather than organized refusal characterizes the target seats.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the board seat the arrangement is crisis infrastructure it administers under a mandate it reads as even-handed. From the strategic-debtor seat it is purchasable forbearance — an insurance policy priced in alignment. From the non-strategic payer seat it is uncompensated discipline applied with a rigor its sponsors' allies never face. From the household seat it is austerity authored remotely, ratified nowhere it can vote, and escaped nowhere it can go. The engine derives these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for geopolitically_strategic_debtors, core_private_creditors, and us_treasury_and_g7_shareholders; the G7 seat's secondary agenda_setter role and arbitrage exit keep it near the beneficiary pole despite its enforcement labor. Victim declarations drive high directionality for nonstrategic_program_countries and program_country_households, amplified by trapped exit — trapped targets sit nearer the full-target end than mobile ones. The board seat derives near-symmetric with a mild tilt toward institutional self-perpetuation: it bears administrative cost and collects continuity. No directionality_overrides are needed: the same-power institutional seats (G7 treasuries, the board, non-Western creditors) already separate on role and exit, so the structural derivation distinguishes what an override would otherwise have to patch. China_and_nonwestern_creditors sit outside the derivation as an excluded alternative-architecture actor, not a seat inside this arrangement's gain structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what keeps both halves of this arrangement visible. Authoring it as pure coordination would erase the victims and license the waiver asymmetry as benign flexibility; authoring it as pure extraction would erase the liquidity function and predict abolition that program governments themselves consistently refuse. Declaring beneficiaries preserves the coordination gate, declaring victims plus active enforcement registers the asymmetric extraction, and the two together force the classification to price the coupling. The founding problem (balance-of-payments crisis management) is contested rather than dead — acute financing gaps remain real even as critics document a dependency dynamic the founders never contemplated — so mandatrophy_resolved is deliberately not declared, and the arrangement cannot be filed as a piton performing a vanished function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the hybrid_selectivity_reading of the kernel structural_adjustment_conditionalities. Would instantiating the creditor_coordination_reading or the debtor_extraction_reading instead change the constraint''s epsilon, victim set, and computed classification?',
    'Author the sibling stories as separate files and compare engine-computed classifications. The disagreement is located in one structural element: whether selective enforcement is incidental imperfection in a coordination device (coordination reading), the universal design principle of an extractive instrument (extraction reading), or the constitutive allocation logic of the arrangement itself (this reading).',
    'The coordination reading would move epsilon toward roughly 0.25 and shrink the victim set toward empty; the extraction reading would move epsilon toward roughly 0.85 and generalize victims to all program countries regardless of alignment. This reading fixes victims by geopolitical position and holds epsilon at 0.71.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one of three readings of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    waiver_determinants_identifiability,
    'Can variance in waiver decisions, program interruptions, and enforcement severity be statistically attributed to geopolitical alignment (UN voting coincidence, basing agreements, alliance ties) net of economic fundamentals (debt ratios, reserves, growth)?',
    'Panel analysis across the full population of Fund arrangements regressing completion and waiver outcomes on alignment measures with fundamentals controls; matched-pair case studies of economically similar aligned and unaligned program countries.',
    'A robust alignment coefficient confirms selectivity as constitutive rather than anecdotal, anchoring the victim set in geopolitical position; a null result would push the arrangement toward uniform-application readings and redistribute measured extraction toward economic circumstance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waiver_determinants_identifiability, empirical, 'Whether enforcement selectivity tracks geopolitics above and beyond economic fundamentals.').

omega_variable(
    counterfactual_enforcement_severity,
    'How much of the enforcement severity borne by non-strategic debtors reflects solvency necessity that any lender would impose, versus discretionary harshness that strategically valuable peers are spared?',
    'Matched comparisons of adjustment depth, prior-action counts, and waiver frequency between strategic and non-strategic borrowers conditioned on comparable debt-service ratios and reserve positions.',
    'Large matched-pair gaps attribute the differential to the arrangement itself rather than to debtor circumstances, raising the extraction share properly chargeable to the constraint; small gaps would locate most measured severity in genuine insolvency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_enforcement_severity, conceptual, 'Separating necessary creditor discipline from discretionary, alignment-indexed harshness.').

omega_variable(
    strategic_debtor_net_position,
    'Are geopolitically strategic debtors net beneficiaries of the arrangement, or does continued waiver-backed financing merely defer and compound their eventual adjustment burden?',
    'Longitudinal trajectories of debt-service ratios, cumulative program years, and ultimate adjustment depth for repeat strategic borrowers against counterfactual market-access estimates.',
    'If strategic debtors'' burdens are deferred rather than dissolved, the beneficiary declaration narrows and the arrangement reads closer to universal extraction with timing variance; if their burdens genuinely dissipate, the selectivity asymmetry is confirmed as a durable two-tier structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_debtor_net_position, empirical, 'Whether waiver recipients are durably subsidized or merely postponing the same extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacond_hybrid_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sacond_hybrid_tr_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(sacond_hybrid_tr_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(sacond_hybrid_tr_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(sacond_hybrid_tr_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(sacond_hybrid_tr_t38, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 38, 0.4).
narrative_ontology:measurement(sacond_hybrid_tr_t44, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 44, 0.4).

% Extraction over time
narrative_ontology:measurement(sacond_hybrid_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(sacond_hybrid_be_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(sacond_hybrid_be_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(sacond_hybrid_be_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(sacond_hybrid_be_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(sacond_hybrid_be_t38, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 38, 0.7).
narrative_ontology:measurement(sacond_hybrid_be_t44, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 44, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(sacond_hybrid_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.74).
narrative_ontology:measurement(sacond_hybrid_su_t8, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(sacond_hybrid_su_t16, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 16, 0.77).
narrative_ontology:measurement(sacond_hybrid_su_t24, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(sacond_hybrid_su_t32, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(sacond_hybrid_su_t38, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 38, 0.74).
narrative_ontology:measurement(sacond_hybrid_su_t44, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 44, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__debtor_extraction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'IMF conditionality' decomposes into three structurally distinct constraints, one per reading of the kernel: coordination-as-designed, extraction-as-experienced, and selectivity-as-operated (this file). Their epsilon values differ widely and must not be reconciled into one number. The coordination reading is upstream — its doctrine supplies the legitimating frame that the other two measure practice against — and this reading documents the doctrine-incidence gap. Each sibling file mirrors this note and links back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
