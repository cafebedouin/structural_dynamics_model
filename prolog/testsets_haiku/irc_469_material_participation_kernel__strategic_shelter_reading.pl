% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strategic_shelter_reading, []).

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
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: IRC 469 Material Participation Permissive Threshold (Strategic Shelter Reading)
 *   domain: tax/regulatory
 *
 * SUMMARY:
 *   This constraint embodies the strategic shelter reading of IRC 469
 *   material participation rules: a high-income investor or syndication
 *   operator can achieve 'material participation' status in real estate
 *   holdings through permissive hour-counting methodologies and favorable
 *   grouping elections, unlocking passive loss deductions that offset
 *   ordinary income. The reading is instantiated in Treasury regulations, IRS
 *   safe harbor guidance, and litigation positions that recognize these
 *   structuring techniques. The consequence is systematic tax sheltering
 *   available primarily to those with capital, legal infrastructure, and
 *   sophistication to exploit the permissiveness — a coordination mechanism
 *   (aggregating labor across holdings) that extracts tax burden toward
 *   less-connected populations. The claim/metric gap is deliberate: this
 *   reading is CLAIMED as tangled_rope (legitimate coordination with
 *   asymmetric participation rules) while the authored metrics describe
 *   increasing extractiveness (0.38→0.68 over 40 years) and rising theater
 *   ratio (0.15→0.41), suggesting a constraint whose coordination function is
 *   eroding as it serves increasingly as pure extraction machinery.
 *
 * KEY AGENTS:
 *   - high_income_passive_investors (powerless→powerful trajectory; arbitrage exit)
 *   - real_estate_syndication_operators (organized agenda-setters; mobile exit)
 *   - internal_revenue_service (institutional interpreter; enforces permissive reading)
 *   - salaried_wage_earners (moderate power; constrained exit; bear distributional cost)
 *   - active_business_owners (organized payers; constrained exit)
 *   - congress (institutional observer; formally sovereign but deferred)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.52).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC 469 Material Participation Permissive Threshold (Strategic Shelter Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax/regulatory").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '4421033e-4723-4f4b-a059-e9f0839c3f87').
narrative_ontology:cs_kernel_codification('4421033e-4723-4f4b-a059-e9f0839c3f87', fixed_text).
narrative_ontology:cs_authority_grounding('4421033e-4723-4f4b-a059-e9f0839c3f87', extraction).
narrative_ontology:cs_interpretation_layer_present('4421033e-4723-4f4b-a059-e9f0839c3f87').
narrative_ontology:cs_reading_relation('4421033e-4723-4f4b-a059-e9f0839c3f87', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('4421033e-4723-4f4b-a059-e9f0839c3f87', foundational, material_participation_aggregable_via_grouping_elections).
narrative_ontology:cs_axiom_status(material_participation_aggregable_via_grouping_elections, holdable).
narrative_ontology:cs_axiom_grounding('4421033e-4723-4f4b-a059-e9f0839c3f87', material_participation_aggregable_via_grouping_elections, empirically_contingent).
narrative_ontology:cs_axiom('4421033e-4723-4f4b-a059-e9f0839c3f87', secondary, investor_tax_neutrality_norm).
narrative_ontology:cs_axiom_status(investor_tax_neutrality_norm, holdable).
narrative_ontology:cs_axiom_grounding('4421033e-4723-4f4b-a059-e9f0839c3f87', investor_tax_neutrality_norm, instrumental).
narrative_ontology:cs_reference_frame('4421033e-4723-4f4b-a059-e9f0839c3f87', legislatively_intended_material_participation_gate).
narrative_ontology:cs_drift_state('4421033e-4723-4f4b-a059-e9f0839c3f87', contemporary_post_permissive_guidance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4421033e-4723-4f4b-a059-e9f0839c3f87', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_passive_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_syndication_operators).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, salaried_wage_earners).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, active_business_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% High-income individuals and partnerships that structure real estate holdings to claim material participation through permissive hour-counting and grouping elections. They benefit by deducting passive losses against ordinary income, lowering effective tax rates on wealth preservation. Exit is available at arbitrage cost: relocating holdings or changing investment structure.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_passive_investors, beneficiary,
    powerful, biographical, arbitrage, national).

% Syndication platforms, real estate partnerships, and investment advisors that structure deals explicitly to enable clients to qualify as material participants. They set the qualification criteria by documenting and aggregating hours; their revenue and reputation depend on delivering tax-shelter arrangements that survive audit. They benefit by charging fees for tax-favorable structuring.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_syndication_operators, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, real_estate_syndication_operators, beneficiary).

% Administers the passive loss rules and interprets materiality thresholds. The strategic shelter reading embeds permissiveness into the agency's own pronouncements (safe harbor tests, grouping election allowances). The IRS enforces this reading through audit guidance and litigation positions that recognize aggressive structuring.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, internal_revenue_service, agenda_setter,
    institutional, generational, constrained, national).

% Cannot claim passive loss deductions on wage income; their tax burden rises as a proportion of revenue relative to sophisticated investors who shelter gains through material participation deductions. They lack the capital base and legal infrastructure to exploit permissive qualification standards.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, salaried_wage_earners, payer,
    moderate, biographical, constrained, national).

% Cannot use passive loss deductions to offset business income; the permissive material participation rule allows passive investors to shield portfolio income while active operators must report and pay tax on operating results. The rule creates competitive disadvantage in effective tax rate.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, active_business_owners, payer,
    organized, biographical, constrained, national).

% Tax fairness advocates, labor unions, and progressive policy organizations oppose the permissive reading and would argue for tighter materiality standards if included in rulemaking. They are excluded from substantive input into Treasury guidance and are heard only through legislative testimony.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, progressive_advocacy_groups, excluded,
    moderate, biographical, constrained, national).

% Enacted IRC 469 as part of the Tax Reform Act of 1986 to prevent tax sheltering through passive losses. Congress remains formally sovereign over the rule but defers to Treasury interpretation and regulatory guidance in practice; legislative correction requires supermajority consensus.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, congress, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_passive_investors).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strategic_shelter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables investors to aggregate holdings across multiple properties and time periods to establish material participation status, solving the coordination problem of demonstrating sufficient labor involvement in real estate across a portfolio.
% TRANSFER_FUNCTION: Transfers tax deductions (foregone government revenue) from wage earners and active business operators to high-income passive investors. The mechanism: permissive hour-counting and grouping elections allow passive loss deductions that directly reduce the investor's taxable income, shifting tax burden to others.
% ABSENT_VOICES: Tax-disadvantaged workers and small business owners have no seat in Treasury regulatory development; progressive tax policy advocates are heard in testimony but excluded from rulemaking authority. Labor representatives and state tax administrators, who bear the distributional consequence, are not parties to the IRS guidance process.
% DISAPPEARANCE_RATIONALE: If the permissive material participation reading were eliminated and replaced with strict documentation requirements, the effective tax rates on high-income passive investment portfolios would rise substantially, capital flows into syndicated real estate would shift, and a major asset class structuring strategy would collapse. Wealth preservation pathways for high-income households would reorganize.
% FOUNDING_PROBLEM: Tax Reform Act of 1986 sought to prevent 'abusive tax shelters' by limiting passive loss deductions; the problem was legitimate tax avoidance through loss-only partnerships. Congress intended materiality to be a substantial gate.
% FOUNDING_PROBLEM_CORROBORATION: Congressional testimony from 1986 and legislative history documents intent to tighten shelter access. The Treasury Office of Tax Analysis and the Treasury Inspector General for Tax Administration have periodically attested that the permissive reading has re-enabled sheltering. Conversely, the IRS and tax bar attest that permissive interpretation is necessary for legitimate real estate investors. Academic tax economists disagree sharply on whether the original problem persists.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval because hour-counting techniques became more standardized and aggressive post-2000; syndication platforms developed explicit safe harbor documentation packages; IRS guidance hardened around permissiveness rather than tightening. By year 40, the constraint's primary function is transferring tax deductions to high-income portfolios. Suppression is moderate (0.52 at end) because the rule is technically legal and formal; the suppression is not personal coercion but rather the constraint's structure prevents alternatives (wage earners cannot use it, active operators cannot use it, and tight documentation requirements mean working-class real estate owners cannot afford compliance). Theater ratio rises through year 20 as the coordination function (aggregating labor documentation) became decoupled from actual labor involvement; by year 25 it plateaus (the rule's legitimacy narrative is now purely performative). Measurement series share one time grid: every metric is authored at every examined point. The interval spans 1986 (Tax Reform Act enactment) through ~2026 (present).
 *
 * PERSPECTIVAL GAP:
 *   From the high-income investor's seat, this is genuine coordination (aggregating holdings for labor thresholds is difficult and requires infrastructure). From the IRS's seat, the permissive reading is a legitimate interpretation of statutory language and administrative necessity (strict interpretation would exclude legitimate investors). From the wage earner's and active operator's seats, this is pure extraction — they cannot use the deduction and bear the tax burden shift. From Congress's formal seat, this reading is a departure from the 1986 intent. The engine computes these divergences from the power/exit/beneficiary/victim structural data — the authored claim does not predict which seat sees what.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income investors are structural beneficiaries: they access deductions not available to others (d near 0.0, full beneficiary). Syndication operators are beneficiaries who also set terms (d near 0.15, beneficiary-with-agenda-setter mixture). Salaried wage earners are structural targets: they cannot access the same deductions, tax burden shifts toward them (d near 0.95, near-full target). Active business owners are constrained payers (d near 0.85). IRS is institutional arbiter that enforces the permissive reading, holding it stable against legislative intent (d near 0.5, symmetric in institutional posture). Directionality overrides not needed: the structural data drives the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was legitimate: the 1986 Act did prevent abusive loss shelters for a period. By the 1990s, permissive IRS guidance re-enabled sheltering; the founding problem (abusive shelters) is contested (some argue it persists, others argue it was solved). Mandatrophy has partially resolved: the problem the rule was built to address is no longer the rule's function. The constraint now primarily transfers tax burden. The theater ratio rise (0.15→0.41) confirms erosion — coordination narrative maintenance with declining coordination substance. A strict reading would close the shelter; a permissive reading (this one) maintains it. The engine's per-seat computation will show the salaried wage earner and active operator seats computing as snare (pure extraction they cannot exit), while the high-income investor seat computes as rope (beneficial coordination). This divergence is the mandatrophy signal: a constraint whose coordination story is one reading and whose extraction story is another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hour_counting_legitimacy,
    'Are the hour-counting methodologies (grouping elections, aggregation across properties, inclusion of planning/consulting hours) structurally necessary to capture legitimate real estate work, or do they primarily enable tax avoidance by inflating participatory labor?',
    'Comparative audit analysis: measure the distribution of claimed hours against actual contemporaneous time records (where discoverable); compare claimed hours in syndicated properties against claimed hours in self-managed properties; econometric analysis of whether aggressive hour-counting correlates with passive loss claims that concentrate in high-income households.',
    'If hour-counting is primarily legitimate, the permissive reading is genuine coordination cost, and extractiveness should be recharacterized as lower. If hour-counting is primarily opportunistic inflation, the constraint is pure extraction dressed as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hour_counting_legitimacy, empirical, 'Whether hour-counting methodologies serve legitimate administrative necessity or primarily enable avoidance.').

omega_variable(
    foundational_problem_persistence,
    'Does the 1986 Tax Reform Act''s foundational problem (abusive tax shelters enabled by passive loss deductions) persist as a live threat, or has it been solved by other regulatory mechanisms (listed property rules, alternative minimum tax, broader anti-shelter doctrines)?',
    'Treasury IG audit reports on shelter prevalence pre- and post-2000; IRS litigation statistics on passive loss disallowance rates; GAO analysis of passive loss deduction concentration and tax gap contribution.',
    'If the problem persists, mandatrophy is not resolved and the permissive reading is a failure to implement legislative intent. If the problem is solved, the permissive reading''s function is no longer gateekeeping and it operates primarily as a wealth preservation mechanism — a coordinate identification of erosion toward pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_problem_persistence, empirical, 'Whether the foundational shelter-prevention problem the 1986 Act addressed remains live.').

omega_variable(
    statutory_ambiguity_vs_reading_choice,
    'Does IRC 469 genuinely permit the permissive reading''s interpretation of material participation, or does the statute''s language and structure preclude the permissive reading as a valid interpretation within statutory bounds?',
    'Linguistic analysis by tax law scholars; Supreme Court textualism applied to the statute''s operative language; comparison of legislative history against Treasury interpretations.',
    'If the statute permits both readings, the committer frame correctly models two readings of one kernel. If the statute forecloses the permissive reading, then the strategic shelter reading is not a valid statutory interpretation but a regulatory override — a different kind of constraint (regulatory capture) riding on statutory text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_ambiguity_vs_reading_choice, conceptual, 'Whether the permissive reading is a valid statutory interpretation or a regulatory override of statutory intent.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.52 at end of interval) purely structural (legal/institutional barriers that constrain who can access the deduction), or does it include internalized suppression (salaried workers and small operators have accepted the narrative that they ''don''t qualify'' and stopped seeking alternatives)?',
    'Survey of wage earners and small business owners on perceived eligibility and interest in material participation claims; analysis of pre- vs. post-regulatory-guidance litigation rates; comparison of substantive participation claims by population group and education level.',
    'If suppression is primarily structural, it persists as written rule. If internalized, the constraint''s effective suppression is higher than the rule suggests — targets carry the barrier with them even if the rule were liberalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of alternative participation claims is structural barrier or internalized narrative acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(irc__tr_t5, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(irc__tr_t10, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(irc__tr_t15, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(irc__tr_t20, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(irc__tr_t25, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(irc__tr_t35, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(irc__tr_t40, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(irc__be_t5, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(irc__be_t10, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(irc__be_t15, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(irc__be_t20, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(irc__be_t25, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(irc__be_t35, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(irc__be_t40, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(irc__su_t5, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(irc__su_t10, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(irc__su_t15, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(irc__su_t20, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(irc__su_t25, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(irc__su_t35, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement(irc__su_t40, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strategic_shelter_reading, 0.18).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel__strict_gatekeeper_reading).

% DUAL FORMULATION NOTE:
% Two readings of IRC 469 material participation kernel instantiate distinct constraints with opposite directionality structures. Strategic shelter reading enables broad passive loss access via permissive hour-counting; strict gatekeeper reading restricts access via tight documentation and narrow aggregation. Sibling constraints share statutory referent (IRC 469) but diverge on materiality measurement and proof burden. The strategic reading examines the constraint through the beneficiary seat (high-income investor); the strict reading examines it through the excluded/payer seats (wage earners, active operators). Extractiveness values are reading-indexed over the same standing arrangement (the permissive interpretation framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
