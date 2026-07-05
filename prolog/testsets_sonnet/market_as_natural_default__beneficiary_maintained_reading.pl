% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market-as-Natural-Default (Beneficiary-Maintained Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This story instantiates the beneficiary-maintained reading of the
 *   market_as_natural_default kernel: the claim that market allocation's
 *   status as economic 'common sense' is not a residue of forgotten
 *   alternatives but an actively engineered and continuously re-defended
 *   closure, sustained by identifiable beneficiaries (finance sector
 *   incumbents, large corporate shareholders, and the think tanks they fund)
 *   through PR, curriculum influence, and institutional capture. This is
 *   deliberately distinct from the sibling readings: the
 *   lapsed_alternative_reading holds that market dominance resulted from
 *   historical forgetting rather than active suppression (a much
 *   lower-extraction, near-mountain story), and the hybrid_amnesia_reading
 *   holds that an initial lapse created conditions later exploited by
 *   beneficiaries (a mixed-origin story). This story does not describe those
 *   readings' mechanics — it is the clean, single reading where the closure
 *   was, from the outset and continuously, actively engineered and
 *   maintained, and where alternatives are suppressed rather than merely
 *   forgotten.
 *
 * KEY AGENTS:
 *   - finance_sector_incumbents: primary beneficiary and agenda-setter (institutional/arbitrage) — funds and directs the naturalization apparatus
 *   - large_corporate_shareholders: beneficiary (powerful/arbitrage) — collects returns from the presumption
 *   - market_ideology_think_tanks: agenda-setter (organized/constrained) — produces and circulates the narrative, institutionally dependent on its plausibility
 *   - displaced_cooperative_economies, unorganized_wage_labor, public_sector_alternatives_constituencies: payers (powerless-to-moderate/trapped-to-constrained) — bear the burden-of-proof asymmetry
 *   - heterodox_economists: excluded (moderate/constrained) — would contest the framing if given equal platform
 *   - economic_historians_outside_finance_funding: analytical observer — corroborates the active-maintenance claim from outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.47).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.62).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market-as-Natural-Default (Beneficiary-Maintained Reading)").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, 'ccaa452b-c657-47b4-8134-438379c156d6').
narrative_ontology:cs_kernel_codification('ccaa452b-c657-47b4-8134-438379c156d6', distributed).
narrative_ontology:cs_authority_grounding('ccaa452b-c657-47b4-8134-438379c156d6', extraction).
narrative_ontology:cs_interpretation_layer_present('ccaa452b-c657-47b4-8134-438379c156d6').
narrative_ontology:cs_reading_relation('ccaa452b-c657-47b4-8134-438379c156d6', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccaa452b-c657-47b4-8134-438379c156d6', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('ccaa452b-c657-47b4-8134-438379c156d6', foundational, naturalization_requires_continuous_active_investment).
narrative_ontology:cs_axiom_status(naturalization_requires_continuous_active_investment, holdable).
narrative_ontology:cs_axiom_grounding('ccaa452b-c657-47b4-8134-438379c156d6', naturalization_requires_continuous_active_investment, empirically_contingent).
narrative_ontology:cs_axiom('ccaa452b-c657-47b4-8134-438379c156d6', foundational, alternatives_are_suppressed_not_forgotten).
narrative_ontology:cs_axiom_status(alternatives_are_suppressed_not_forgotten, holdable).
narrative_ontology:cs_axiom_grounding('ccaa452b-c657-47b4-8134-438379c156d6', alternatives_are_suppressed_not_forgotten, empirically_contingent).
narrative_ontology:cs_reference_frame('ccaa452b-c657-47b4-8134-438379c156d6', postwar_contested_allocation_era).
narrative_ontology:cs_drift_state('ccaa452b-c657-47b4-8134-438379c156d6', contemporary_neoliberal_consensus_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ccaa452b-c657-47b4-8134-438379c156d6', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, finance_sector_incumbents).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, large_corporate_shareholders).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, market_ideology_think_tanks).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, displaced_cooperative_economies).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, unorganized_wage_labor).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, public_sector_alternatives_constituencies).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, market_allocation_is_the_default_state_of_exchange).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund think tanks, business schools, and financial press that present market allocation as the default, pre-political state of exchange to which all economies naturally revert absent distortion. Sits on the boards and donor rolls of the institutions that produce this framing, and benefits directly when regulatory or public alternatives are foreclosed as 'unnatural intervention.' Can relocate capital across jurisdictions if any single naturalization narrative loses traction.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, finance_sector_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, finance_sector_incumbents, beneficiary).

% Collect the returns that flow from treating market allocation of labor, capital, and public goods as inevitable rather than chosen; benefit from public discourse that treats alternatives (cooperatives, public utilities, sectoral bargaining) as historical curiosities or failed experiments rather than live options. Diversified enough to be insulated from any single market's volatility.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, large_corporate_shareholders, beneficiary,
    powerful, generational, arbitrage, global).

% Produce and circulate the naturalization narrative — white papers, op-eds, textbook chapters — that frame market allocation as pre-institutional common sense. Funded by finance and corporate donors; their institutional survival depends on the narrative's continued plausibility, which gives them an active maintenance incentive distinct from the beneficiaries who fund them.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, market_ideology_think_tanks, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, market_ideology_think_tanks, beneficiary).

% Historical and contemporary cooperative, mutualist, and communal allocation systems that are actively marginalized in policy discourse, financing access, and legal recognition — not merely forgotten but out-competed by narrative and by law shaped in response to the narrative. Their exit is trapped: reviving these forms requires overcoming both capital access barriers and the reputational cost of being framed as economically naive.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, displaced_cooperative_economies, payer,
    powerless, generational, trapped, regional).

% Bears the downstream cost of policy that treats labor-market outcomes as natural rather than the product of specific, defensible institutional choices — wage suppression, weak bargaining power, and the foreclosure of sectoral or codetermination alternatives are legitimated as market outcomes rather than contestable arrangements. Cannot individually exit a labor market shaped this way.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, unorganized_wage_labor, payer,
    powerless, biographical, trapped, national).

% Advocates and beneficiaries of public utility, public banking, or municipal-ownership models who find their proposals treated as ideologically suspect departures from a 'natural' baseline rather than as one allocation mechanism among several. Have some organizational capacity (unions, municipal coalitions) but face a media and academic environment shaped by the naturalization narrative's funders.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, public_sector_alternatives_constituencies, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, public_sector_alternatives_constituencies, excluded).

% Economic historians and institutionalist economists who document that market allocation is one historically contingent arrangement among many, actively maintained through specific enclosures, legal enforcement, and PR investment. Publish in lower-prestige venues, face tenure and funding headwinds tied to a discipline substantially shaped by the naturalization narrative's institutional footprint. Would object loudly to the 'natural default' framing if given equal platform access.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, heterodox_economists, excluded,
    moderate, biographical, constrained, national).

% Independent scholars documenting the specific enclosure acts, legal interventions, and public-relations campaigns (e.g., mid-20th-century business-funded economic education programs) through which market allocation was actively naturalized and re-naturalized after periods of contestation. Not funded by the beneficiary class; their work is the primary corroborating evidence for this reading's active-defense claim.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, economic_historians_outside_finance_funding, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__beneficiary_maintained_reading, finance_sector_incumbents).
narrative_ontology:fixing_cost_class(market_as_natural_default__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, low-negotiation-cost baseline for economic expectation-setting: market allocation as 'default' reduces the transaction cost of justifying any particular market arrangement, since deviations bear the burden of proof rather than the arrangement itself.
% TRANSFER_FUNCTION: Moves the burden of justification from market outcomes (which are treated as needing no defense) onto alternative arrangements (which must overcome a presumption of unnaturalness); this asymmetric burden channels policy attention, financing, and legal protection toward market-based allocation and away from cooperative, public, and sectoral-bargaining alternatives, transferring bargaining power and rents to those already positioned to benefit from market allocation.
% ABSENT_VOICES: Heterodox economists and historians of political economy who can document the specific, datable acts of enclosure, deregulation, and funded public-relations campaigns that produced and re-produced the 'natural market' framing are systematically underrepresented in mainstream economic education, financial press, and policy advisory bodies funded by the beneficiary class.
% DISAPPEARANCE_RATIONALE: If the active maintenance apparatus (funded think tanks, curriculum influence, media framing) disappeared overnight, the presumption favoring market allocation would lose its institutional reproduction mechanism within a generation; cooperative, public, and sectoral alternatives would no longer bear an automatic burden of proof, and policy debates would need to justify market allocation on its merits case-by-case rather than by default. Capital would very likely fund new institutional maintenance to restore the presumption, which is itself evidence the presumption is actively defended rather than a passive residue.
% FOUNDING_PROBLEM: In the mid-20th century, market allocation faced serious ideological and institutional competition (social democratic planning, cooperative movements, postwar mixed economies); the naturalization project was built to resolve genuine uncertainty about which allocation system would dominate by making market allocation appear pre-political and inevitable rather than one contested option among several.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside finance-sector funding (e.g., documentation of business-funded economic education campaigns, corporate-funded chair endowments in economics departments, and coordinated media strategy from the 1940s onward) corroborate that the naturalization narrative required, and continues to require, active institutional investment rather than persisting as inert historical residue; this corroboration comes from scholars whose funding is independent of the beneficiary class named above.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.47, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored in the moderate-to-high band (0.47 at interval end, within the 0.35-0.55 delta specified for this reading) because the mechanism transfers real bargaining power and policy attention toward market allocation without being total expropriation — some genuine coordination value exists in a shared allocation baseline. Suppression (0.62) exceeds extraction because the active maintenance apparatus (funding, curriculum capture, media framing) is a raw structural fact about how alternatives are kept off the table, independent of how much is actually extracted through the resulting policy tilt. Theater ratio rises across the interval (0.30 to 0.58) because as academic and public awareness of the historical contingency has grown, an increasing share of the naturalization apparatus's activity has shifted from substantive argument to reputational and procedural defense — the accessibility_collapse (0.60) and resistance (0.55) values reflect that alternatives are structurally difficult to access but not eliminated, and resistance is real and organized rather than negligible, consistent with an actively contested tangled rope rather than a settled natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Finance sector incumbents and large corporate shareholders sit near the full-beneficiary end: they fund the apparatus and collect the downstream rents from the burden-of-proof asymmetry, and their global arbitrage-grade exit options mean they are never trapped by any single jurisdiction's contestation of the narrative. Market ideology think tanks are a secondary beneficiary-agenda-setter hybrid — they benefit from institutional survival tied to the narrative's plausibility, distinct from but dependent on the primary funders. Displaced cooperative economies, unorganized wage labor, and public-sector-alternative constituencies sit near the full-target end: trapped or constrained, bearing the asymmetric justificatory burden with no comparable capacity to fund a counter-narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem is authored as live rather than dead: the ideological competition the naturalization project was built to resolve (social democratic and cooperative alternatives) remains a genuine live policy alternative in many jurisdictions, which is precisely why active, continuous maintenance investment (rather than one-time historical closure) is required — a genuinely dead founding problem would not need re-funding each generation. This distinguishes the tangled_rope reading from a piton: a piton persists through inertia with no concentrated beneficiary; here concentrated beneficiaries (finance, corporate shareholders) actively reinvest in maintenance, which is the defining structural feature this reading asserts against the lapsed_alternative_reading's forgetting account.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_defense_vs_passive_residue,
    'Is the observed persistence of market-naturalization framing better explained by continuous active investment from identifiable beneficiaries, or by inertia from a closure that occurred once and simply was never revisited?',
    'Track funding flows to economic-education and think-tank infrastructure over time: if funding levels track periods of ideological contestation (rising when alternatives gain political traction, falling when the presumption is secure), that supports active defense; flat or declining funding independent of contestation would support a lapsed/residue account.',
    'If active defense is confirmed, this reading (tangled_rope with identifiable beneficiaries) is the structurally accurate one and the sibling lapsed_alternative_reading should be understood as describing a different or earlier phase; if funding is flat/declining, the hybrid_amnesia_reading better fits the current data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_defense_vs_passive_residue, empirical, 'Whether beneficiary funding tracks contestation cycles (active defense) or is flat/inertial (passive residue).').

omega_variable(
    coordination_value_of_shared_baseline,
    'How much of the extraction measured here is inseparable from a genuine coordination benefit (a shared allocation baseline reduces negotiation costs for everyone), versus how much is purely captured rent with no coordination counterpart?',
    'Compare transaction costs and policy volatility in jurisdictions with strong versus weak naturalization narratives, controlling for underlying market structure, to isolate the coordination-value component from the rent-capture component.',
    'A larger coordination-value component would push this reading''s classification toward a less extractive tangled_rope or even a rope with beneficiary capture at the margins; a smaller component would push toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_value_of_shared_baseline, conceptual, 'Whether the naturalization narrative''s coordination function is real or is cover story for pure rent capture.').

omega_variable(
    reading_framing_underdetermination,
    'Is the beneficiary-maintained framing the correct primary lens, or does the hybrid_amnesia_reading better capture the historical sequence (an initial passive lapse later exploited, rather than continuous active engineering from the start)?',
    'Detailed historical periodization of specific naturalization campaigns (e.g., mid-20th-century business-funded economic education, post-1970s deregulation PR) to determine whether beneficiary funding preceded or followed the initial narrative dominance.',
    'If beneficiary funding preceded and produced the initial dominance, this reading is structurally correct from the outset; if dominance arose first through unrelated historical forgetting and beneficiaries only later invested in maintaining it, the hybrid_amnesia_reading is the more accurate account of origin, though this reading would remain accurate for the contemporary maintenance phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether active engineering or a hybrid lapse-then-capture sequence better characterizes the historical record this reading claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mark_tr_t14, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 14, 0.38).
narrative_ontology:measurement(mark_tr_t28, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 28, 0.45).
narrative_ontology:measurement(mark_tr_t42, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 42, 0.5).
narrative_ontology:measurement(mark_tr_t56, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 56, 0.55).
narrative_ontology:measurement(mark_tr_t70, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 70, 0.58).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mark_be_t14, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 14, 0.33).
narrative_ontology:measurement(mark_be_t28, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 28, 0.38).
narrative_ontology:measurement(mark_be_t42, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 42, 0.42).
narrative_ontology:measurement(mark_be_t56, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 56, 0.45).
narrative_ontology:measurement(mark_be_t70, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 70, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mark_su_t14, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 14, 0.47).
narrative_ontology:measurement(mark_su_t28, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 28, 0.52).
narrative_ontology:measurement(mark_su_t42, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 42, 0.56).
narrative_ontology:measurement(mark_su_t56, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 56, 0.6).
narrative_ontology:measurement(mark_su_t70, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 70, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__beneficiary_maintained_reading, 0.1).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the market_as_natural_default kernel. beneficiary_maintained_reading (this story) claims active, continuous, beneficiary-funded engineering of market naturalization with moderate-to-high extractiveness (0.47) and identifiable beneficiaries/victims. lapsed_alternative_reading claims the dominance resulted from historical forgetting rather than active suppression — expected near-mountain, low extraction, minimal beneficiary structure. hybrid_amnesia_reading claims an initial passive lapse created conditions later exploited by beneficiaries — expected intermediate extraction and a mixed origin story. All three share the same underlying kernel (why market allocation holds default status) but instantiate structurally distinct claims with distinct epsilon values, per the ε-invariance principle; they are linked here via network edges rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
