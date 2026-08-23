% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Broad Effects Test Reading of the Federal Commerce Power
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   Since the 1937 constitutional settlement, the operative reading of
 *   Article I, Section 8, Clause 3 treats commerce as any economic activity
 *   with substantial aggregate effects on interstate commerce, treats
 *   regulation as including prohibition and comprehensive control, and
 *   therefore extends federal power deep into intrastate life. The canonical
 *   demonstrations are the 1942 wheat-quota case (home-consumed grain
 *   aggregated into national supply), the 1964 public-accommodations statutes
 *   (discriminatory exclusion framed as a commerce burden), and the 2005
 *   reaffirmation that even locally grown medical cannabis falls within reach
 *   once a regulated national market exists. This story instantiates the
 *   broad_effects_test reading of the commerce_clause_scope kernel and of
 *   that reading alone: epsilon here indexes the standing broad-effects
 *   arrangement itself as it has actually operated from 1937 to the present,
 *   not the arrangement any rival reading would install. The sibling
 *   readings, narrow_originalist and intermediate_channels, are separate
 *   constraint files with their own epsilon values and victim structures,
 *   linked through the network and through cs_structure.reading_relations;
 *   the disagreement between readings lives in what 'commerce' and 'regulate'
 *   mean, and this file does not average over it.
 *
 * KEY AGENTS:
 *   - - united_states_congress: Agenda setter (institutional/arbitrage) - writes statutes under the broad rationale; its only ceiling is political
 *   - - united_states_supreme_court: Boundary administrator (institutional/constrained) - adjudicates the doctrine's reach; bound by its own precedent
 *   - - federal_regulatory_agencies: Primary beneficiary-administrator (institutional/arbitrage) - jurisdiction and budget expand with each successful reach
 *   - - state_governments: Principal payer (powerful/trapped) - lose police-power autonomy wherever economic effects are claimed
 *   - - intrastate_agricultural_producers: Payer with offsetting receipts (moderate/constrained) - aggregated into federal schemes yet paid by farm programs
 *   - - national_civil_rights_organizations: Beneficiary (organized/mobile) - commerce-based enforcement is their core asset
 *   - - uniform_rule_seeking_national_firms: Beneficiary (powerful/arbitrage) - purchase uniformity, displace divergent state rules
 *   - - local_municipal_economies: Payer (moderate/trapped) - local choices overridden by federally mandated standards
 *   - - noncommercial_conduct_participants: Contingent payer (powerless/trapped) - exposed whenever jurisdictional hooks attach
 *   - - federalism_limited_government_advocates: Excluded voice (organized/mobile) - argues the narrow reading without a standing seat
 *   - - constitutional_law_academy: Analytical observer (organized/analytical) - documents drift between text, history, and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.63).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.66).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.63).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Broad Effects Test Reading of the Federal Commerce Power").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, '7b03131c-7d47-43e3-875a-d853647ccee4').
narrative_ontology:cs_kernel_codification('7b03131c-7d47-43e3-875a-d853647ccee4', fixed_text).
narrative_ontology:cs_authority_grounding('7b03131c-7d47-43e3-875a-d853647ccee4', lineage).
narrative_ontology:cs_interpretation_layer_present('7b03131c-7d47-43e3-875a-d853647ccee4').
narrative_ontology:cs_reading_relation('7b03131c-7d47-43e3-875a-d853647ccee4', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('7b03131c-7d47-43e3-875a-d853647ccee4', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('7b03131c-7d47-43e3-875a-d853647ccee4', foundational, aggregate_economic_effect_suffices_for_federal_reach).
narrative_ontology:cs_axiom_status(aggregate_economic_effect_suffices_for_federal_reach, holdable).
narrative_ontology:cs_axiom_grounding('7b03131c-7d47-43e3-875a-d853647ccee4', aggregate_economic_effect_suffices_for_federal_reach, empirically_contingent).
narrative_ontology:cs_axiom('7b03131c-7d47-43e3-875a-d853647ccee4', foundational, commerce_power_encompasses_prohibition_and_comprehensive_control).
narrative_ontology:cs_axiom_status(commerce_power_encompasses_prohibition_and_comprehensive_control, holdable).
narrative_ontology:cs_axiom_grounding('7b03131c-7d47-43e3-875a-d853647ccee4', commerce_power_encompasses_prohibition_and_comprehensive_control, conventional).
narrative_ontology:cs_reference_frame('7b03131c-7d47-43e3-875a-d853647ccee4', commerce_as_integrated_national_economy).
narrative_ontology:cs_drift_state('7b03131c-7d47-43e3-875a-d853647ccee4', contemporary_post_lopez_raich_era, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('7b03131c-7d47-43e3-875a-d853647ccee4', '2026-08-20T00:00:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_civil_rights_organizations).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, uniform_rule_seeking_national_firms).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, intrastate_agricultural_producers).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_municipal_economies).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, noncommercial_conduct_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, intrastate_agricultural_producers).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, aggregate_effects_jurisprudence).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, national_market_uniformity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes the statutes that operationalize the broad reading: since 1937 it can reach virtually any economic activity by asserting aggregate national effects, and it selects that rationale whenever national uniformity or national program-building is wanted. Its ceiling is political rather than legal: it may decline to regulate a domain, reroute authority through the spending power, or amend statutes, and it answers to voters rather than to states.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, united_states_congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicates how far aggregate-effect reasoning reaches. Upheld the aggregation principle for a home-consumed wheat crop in 1942, sustained commerce-based public-accommodations enforcement in 1964, narrowed reach for non-economic activity in 1995 and 2000, then reaffirmed aggregation for economic activity in 2005. It cannot step outside the constitutional order it polices; its room for maneuver is bounded by its own accumulated precedent.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, united_states_supreme_court, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, united_states_supreme_court, observer).

% Administer programs built on the broad rationale: environmental, food and drug, labor, transportation, and financial regulation. Each successful assertion of aggregate-effects authority enlarges their jurisdiction, budget, and staffing. They select among overlapping statutory bases opportunistically and rarely face a binding outer limit on economic subjects; their principal costs are political controversy and litigation over particular rules.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, agenda_setter).

% Retain formal sovereignty and a large reserve of police powers, but whenever a regulated activity carries any claimed national economic footprint, federal statutes displace their choices. They cannot leave the union, cannot opt out of federal preemption, and their reserved-powers shield has repeatedly failed once economic effects were alleged. Their remaining leverage is litigation, which succeeds episodically, and negotiation through congressional delegations.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    powerful, generational, trapped, regional).

% Farmers and small producers whose output is consumed locally or sold regionally. Individually their transactions look negligible next to the national market, yet the aggregation principle folds their combined activity into federal acreage allotments, marketing quotas, and conservation conditionality; the canonical case penalized a farmer for wheat grown to feed his own livestock. The same body of authority also delivers substantial federal farm-program payments to them, which offsets a significant share of the burden they carry.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, intrastate_agricultural_producers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, intrastate_agricultural_producers, beneficiary).

% National advocacy organizations that converted the commerce power into the enforcement backbone for public-accommodations and employment nondiscrimination after remedies tied to state action proved inadequate against private exclusion. Uniform federal coverage is their core asset: they operate across every jurisdiction and lose little when any single state resists.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_civil_rights_organizations, beneficiary,
    organized, generational, mobile, national).

% Multistate and multinational enterprises that prefer one national regulatory standard to fifty divergent ones. Compliance economics favor uniformity; they lobby for federal rules that displace stricter or merely different state regimes, and they can relocate operations or structure transactions to arbitrage jurisdictions when rules bind unevenly.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, uniform_rule_seeking_national_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Cities and county economies whose zoning, licensing, procurement, and development decisions are overridden when federally mandated environmental, transportation, or housing standards arrive under aggregate-effects statutes. They hold no direct standing with federal agencies and depend on state delegations and national associations to carry their objections.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_municipal_economies, payer,
    moderate, generational, trapped, local).

% Individuals whose conduct is not economic: carrying a firearm near a school, cultivating plants for personal medical use, enduring gender-based violence. When a statute attaches a jurisdictional hook tying their conduct to some economic nexus, federal enforcement reaches them; absent such a hook, the 1995 and 2000 carve-outs shield them. Their exposure is contingent, case-by-case, and they possess no collective voice in the doctrine's administration.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, noncommercial_conduct_participants, payer,
    powerless, biographical, trapped, national).

% Scholars, state-rights officeholders, and limited-government movements pressing the narrow trade-crossing-lines reading. They publish, file amicus briefs, and contest judicial nominations, but hold no standing seat in the operative doctrine; their arguments surface chiefly as episodic narrowing attempts that the settled practice then absorbs.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federalism_limited_government_advocates, excluded,
    organized, biographical, mobile, national).

% Constitutional law scholars and commentators who track the doctrine's movement, document the distance between text, founding history, and practice, and supply the analytical frames each branch borrows. They collect no rents from the arrangement and bear none of its compliance burdens.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_law_academy, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_scope__broad_effects_test, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single national market: it prevents states from taxing, barring, or discriminating against sister-state goods, secures uniform commercial rules for multistate exchange, and supplies a vehicle for spillover problems, including airsheds, river basins, discriminatory exclusion from markets, and nationwide product hazards, that no single state can resolve alone.
% TRANSFER_FUNCTION: Moves regulatory authority from fifty state capitols to Washington; moves compliance obligations and penalty exposure onto intrastate producers and local economies; moves program dollars back outward to favored producers and grantees; and concentrates discretionary policy-setting in federal agencies.
% ABSENT_VOICES: State-rights advocates participate only marginally and episodically. Structurally absent are the residents of policy regimes that preemption prevented from ever forming: states that might have experimented with different labor, environmental, or safety standards never adopted them, so no constituency for those untried regimes ever materialized. Also absent are future cohorts, who inherit the consolidated jurisdiction without ever having chosen it.
% DISAPPEARANCE_RATIONALE: If the broad reading vanished overnight, plenary intrastate economic authority would revert to the states; statutes resting solely on the aggregate-effects rationale would lapse or scramble for other constitutional anchors; whole federal programs would shed their authorization; the trade-barrier incentives that plagued the Articles period would slowly re-emerge; national-market integration gains would erode while state experimentation and regulatory diversity revived.
% FOUNDING_PROBLEM: Under the Articles of Confederation, states erected tariffs and discriminatory duties on sister-state goods and vessels, flooded neighbors with depreciated paper money that disrupted their commerce, and fought over navigation rights; the Philadelphia Convention treated a general federal power over commerce among the states as the remedy. Later renewals of the founding problem included Depression-era national economic collapse and the racially motivated exclusion of Black travelers from interstate lodging and dining markets.
% FOUNDING_PROBLEM_CORROBORATION: Madison's Federalist No. 42 and the Convention's delegate records attest the founding trade-war problem; economic historians document the fragmentation costs of the confederation period; and the congressional findings behind the 1964 Civil Rights Act documented discriminatory exclusion as a burden on interstate commerce, drawing on testimony from affected travelers and businesses. These attestations come from historical scholarship, economists, and contemporaneous state officials' own admissions, seats outside the arrangement's beneficiary set. No version of the genealogy rests solely on the benefiting parties' self-report.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independently authored. I claim tangled_rope because the arrangement structurally possesses BOTH halves: a genuine, corroborated coordination function (one national market; the founding trade-war problem is real and still live) AND asymmetric extraction through the same structure (states and intrastate actors pay in autonomy and compliance wherever an aggregate effect can be alleged, while federal institutions collect jurisdiction, budget, and discretion). Descriptively: epsilon 0.63 is high but capped below snare-grade because the coordination benefit is real, diffuse, and reaches nearly every market participant; suppression 0.66 is a raw structural property, deliberately unscaled, reflecting that federal preemption forecloses state alternatives outright rather than raising their price; theater_ratio 0.27 reflects the periodic 'our federalism' and Tenth Amendment rhetoric that performs restraint without binding it, peaking around the 1995-2000 narrowing opinions whose practical effect the 2005 decision largely reversed; accessibility_collapse 0.52 marks partial collapse, since preemption closes state paths but Lopez/Morrison/NFIB demonstrate the doctrine is not limitless and Congress retains a real choice not to act; resistance 0.48 marks real but episodic opposition, concentrated in litigation moments rather than continuous mobilization. All three tracked metric series run on one shared nine-point time grid (1937, 1942, 1955, 1964, 1976, 1995, 2000, 2005, 2025) with every metric authored at every point; suppression_requirement is tracked because the story specifically traces enforcement-capacity build-out (New Deal and Great Society agency construction) followed by a mature plateau, not merely static suppression. The extractiveness series shows accumulation through 1964, a partial dip at the 1995-2000 narrowing, and recovery thereafter: the arrangement metabolizes its challengers.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. State governments, trapped and facing preemption, sit near the full-target end and experience something close to pure extraction: the coordination story buys them nothing they can refuse, since their consent was never sought and their exit is barred. Intrastate producers occupy a split position: the same authority that quotas them also pays them, so their experienced extraction is materially discounted. Federal agencies, national civil-rights organizations, and uniformity-seeking national firms sit near the beneficiary end and experience the identical structure as indispensable coordination they helped build. The Court occupies a hybrid seat: it collects interpretive centrality but pays legitimacy costs whenever the gap between text and practice draws fire. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place federal_regulatory_agencies, national_civil_rights_organizations, and uniform_rule_seeking_national_firms near the low-d end; their exit profiles (arbitrage, mobile) push them further toward subsidy-side readings. Victim declarations place state_governments and local_municipal_economies near the high-d end, amplified by trapped exit: preemption removes the alternatives that would otherwise dampen effective extraction. Intrastate_agricultural_producers carry a payer role with a beneficiary secondary role; the derivation reads both, tempering what would otherwise be near-full-target extraction, which matches the substantive fact that farm-program payments flow back through the same authority. Noncommercial_conduct_participants are victims with contingent exposure: their d is high but gated on whether a jurisdictional hook attaches. Congress collects authority and pays only political costs, sitting well below symmetric; the Court collects interpretive centrality and pays episodic legitimacy costs, sitting moderately beneficiary-side. Suppression enters the computation raw: it is a structural property of the constraint, scaled by nothing, while extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and externally corroborated (trade fragmentation under the Articles; renewed national emergencies; market-closing discrimination), so this is not a mandate outliving its function, and no mandatrophy resolution is declared. The classification work here is preventive in both directions: reading the arrangement as pure extraction (ignoring the coordination function) would mislabel a working national-market mechanism as a snare and license dismantling programs that solve real collective-action problems; reading it as pure coordination (ignoring the extraction) would launder the subsumption of state police powers and the sweeping-in of local producers as mere overhead. The tangled_rope claim forces both halves to be accounted for simultaneously, and the temporal series shows why the hybrid is stable rather than transitional: extraction accumulated for decades, was partially checked, and then re-accumulated, with no sunset mechanism anywhere in the structure. The leveled grid sharpens the point: suppression and stakes rose at the structural and organizational levels while class-level resistance declined, because the arrangement purchased producer acquiescence with subsidy flows, an accommodation dynamic rather than a decay dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_structural_delta,
    'How would this constraint''s victim set, epsilon, and computed classification change under the sibling readings of commerce_clause_scope, narrow_originalist and intermediate_channels?',
    'Compile the sibling files and compare computed per-seat classifications, victim sets, and epsilon. The disagreement is located in the semantic content of ''commerce'' and ''regulate'' and in whether aggregation over intrastate activity is admitted at all.',
    'Under narrow_originalist the victim set shrinks toward nil and epsilon falls toward coordination-floor territory, yielding a rope-like profile; under intermediate_channels victims shrink to aggregation-of-economic-activity cases only, with epsilon landing between this file and the narrow reading. This file''s numbers are valid only for the broad reading''s arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_structural_delta, conceptual, 'Committer-frame routing: this story is one reading of a three-way-contested kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    aggregation_marginal_reality,
    'Does aggregate-effects reach track the real marginal consequences of individual intrastate acts, or is aggregation a jurisdictional ratchet that is indifferent to marginal size?',
    'Marginal-contribution studies of representative cases: the share of home-consumed output in relevant national markets, elasticity estimates linking localized acts to national aggregates, and replication of the canonical wheat-farming arithmetic under modern conditions.',
    'If marginal contributions are negligible, the broad reach is definitional rather than consequential, strengthening the narrow reading''s critique; if materially nonzero, the broad rationale retains empirical footing and part of the measured extraction is the price of genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_marginal_reality, empirical, 'Whether the aggregation doctrine''s empirical premise holds for the cases it actually reaches.').

omega_variable(
    state_reserved_domain_extent,
    'How much genuinely autonomous regulatory space do state governments retain once preemption under aggregate-effects statutes is inventoried domain by domain?',
    'Systematic audit of preempted versus retained police-power fields across the fifty states, weighted by fiscal and social significance of each field.',
    'A large residual domain would soften the state-victim declaration and lower the state seat''s effective extraction; a thin residual would confirm near-total subsumption wherever economic effects are claimable, pushing the state seat toward the full-target end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_reserved_domain_extent, empirical, 'Size of the surviving state autonomy zone beneath the broad reading.').

omega_variable(
    noneconomic_carveout_durability,
    'Will the non-economic activity carve-outs survive attenuation pressure, or will they be reabsorbed the way economic aggregation absorbed home production?',
    'Longitudinal tracking of applications for jurisdictional-hook laundering, statutes attaching nominal economic elements to non-economic conduct, and of the courts'' willingness to credit attenuated causal chains after the 2005 reaffirmation.',
    'Erosion of the carve-outs widens the victim set toward universal economic-life coverage and raises epsilon; durable carve-outs bound the victim set and hold the classification nearer the middle of the hybrid band.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(noneconomic_carveout_durability, empirical, 'Durability of the principal doctrinal brake on the broad reading.').

omega_variable(
    uniformity_vs_experimentation_weighting,
    'How should the welfare gains from national uniformity be weighed against the foreclosed value of state-level experimentation?',
    'No purely empirical resolution exists; deliberative and policy-process valuation, with sensitivity analysis across weighting schemes.',
    'Heavy uniformity weighting pushes the net assessment toward coordination and softens extraction judgments; heavy experimentation weighting pushes toward extraction and hardens them; per-seat classifications shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniformity_vs_experimentation_weighting, preference, 'Normative weighting underlying the beneficiary/victim ledger; irreducibly value-laden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1937, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccs_broad_effects_tr_t1937, commerce_clause_scope__broad_effects_test, theater_ratio, 1937, 0.15).
narrative_ontology:measurement_basis(ccs_broad_effects_tr_t1937, observed).
narrative_ontology:measurement(ccs_broad_effects_tr_t1942, commerce_clause_scope__broad_effects_test, theater_ratio, 1942, 0.14).
narrative_ontology:measurement_basis(ccs_broad_effects_tr_t1942, observed).
narrative_ontology:measurement(ccs_broad_effects_tr_t1955, commerce_clause_scope__broad_effects_test, theater_ratio, 1955, 0.18).
narrative_ontology:measurement_basis(ccs_broad_effects_tr_t1955, observed).
narrative_ontology:measurement(ccs_broad_effects_tr_t1964, commerce_clause_scope__broad_effects_test, theater_ratio, 1964, 0.2).
narrative_ontology:measurement_basis(ccs_broad_effects_tr_t1964, observed).
narrative_ontology:measurement(ccs_broad_effects_tr_t1976, commerce_clause_scope__broad_effects_test, theater_ratio, 1976, 0.24).
narrative_ontology:measurement_basis(ccs_broad_effects_tr_t1976, observed).
narrative_ontology:measurement(ccs_broad_effects_tr_t1995, commerce_clause_scope__broad_effects_test, theater_ratio, 1995, 0.3).
narrative_ontology:measurement_basis(ccs_broad_effects_tr_t1995, observed).
narrative_ontology:measurement(ccs_broad_effects_tr_t2000, commerce_clause_scope__broad_effects_test, theater_ratio, 2000, 0.32).
narrative_ontology:measurement_basis(ccs_broad_effects_tr_t2000, observed).
narrative_ontology:measurement(ccs_broad_effects_tr_t2005, commerce_clause_scope__broad_effects_test, theater_ratio, 2005, 0.28).
narrative_ontology:measurement_basis(ccs_broad_effects_tr_t2005, observed).
narrative_ontology:measurement(ccs_broad_effects_tr_t2025, commerce_clause_scope__broad_effects_test, theater_ratio, 2025, 0.27).
narrative_ontology:measurement_basis(ccs_broad_effects_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(ccs_broad_effects_be_t1937, commerce_clause_scope__broad_effects_test, base_extractiveness, 1937, 0.45).
narrative_ontology:measurement_basis(ccs_broad_effects_be_t1937, observed).
narrative_ontology:measurement(ccs_broad_effects_be_t1942, commerce_clause_scope__broad_effects_test, base_extractiveness, 1942, 0.55).
narrative_ontology:measurement_basis(ccs_broad_effects_be_t1942, observed).
narrative_ontology:measurement(ccs_broad_effects_be_t1955, commerce_clause_scope__broad_effects_test, base_extractiveness, 1955, 0.6).
narrative_ontology:measurement_basis(ccs_broad_effects_be_t1955, observed).
narrative_ontology:measurement(ccs_broad_effects_be_t1964, commerce_clause_scope__broad_effects_test, base_extractiveness, 1964, 0.66).
narrative_ontology:measurement_basis(ccs_broad_effects_be_t1964, observed).
narrative_ontology:measurement(ccs_broad_effects_be_t1976, commerce_clause_scope__broad_effects_test, base_extractiveness, 1976, 0.64).
narrative_ontology:measurement_basis(ccs_broad_effects_be_t1976, observed).
narrative_ontology:measurement(ccs_broad_effects_be_t1995, commerce_clause_scope__broad_effects_test, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement_basis(ccs_broad_effects_be_t1995, observed).
narrative_ontology:measurement(ccs_broad_effects_be_t2000, commerce_clause_scope__broad_effects_test, base_extractiveness, 2000, 0.57).
narrative_ontology:measurement_basis(ccs_broad_effects_be_t2000, observed).
narrative_ontology:measurement(ccs_broad_effects_be_t2005, commerce_clause_scope__broad_effects_test, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(ccs_broad_effects_be_t2005, observed).
narrative_ontology:measurement(ccs_broad_effects_be_t2025, commerce_clause_scope__broad_effects_test, base_extractiveness, 2025, 0.63).
narrative_ontology:measurement_basis(ccs_broad_effects_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(ccs_broad_effects_su_t1937, commerce_clause_scope__broad_effects_test, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement_basis(ccs_broad_effects_su_t1937, observed).
narrative_ontology:measurement(ccs_broad_effects_su_t1942, commerce_clause_scope__broad_effects_test, suppression_requirement, 1942, 0.5).
narrative_ontology:measurement_basis(ccs_broad_effects_su_t1942, observed).
narrative_ontology:measurement(ccs_broad_effects_su_t1955, commerce_clause_scope__broad_effects_test, suppression_requirement, 1955, 0.56).
narrative_ontology:measurement_basis(ccs_broad_effects_su_t1955, observed).
narrative_ontology:measurement(ccs_broad_effects_su_t1964, commerce_clause_scope__broad_effects_test, suppression_requirement, 1964, 0.62).
narrative_ontology:measurement_basis(ccs_broad_effects_su_t1964, observed).
narrative_ontology:measurement(ccs_broad_effects_su_t1976, commerce_clause_scope__broad_effects_test, suppression_requirement, 1976, 0.66).
narrative_ontology:measurement_basis(ccs_broad_effects_su_t1976, observed).
narrative_ontology:measurement(ccs_broad_effects_su_t1995, commerce_clause_scope__broad_effects_test, suppression_requirement, 1995, 0.64).
narrative_ontology:measurement_basis(ccs_broad_effects_su_t1995, observed).
narrative_ontology:measurement(ccs_broad_effects_su_t2000, commerce_clause_scope__broad_effects_test, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement_basis(ccs_broad_effects_su_t2000, observed).
narrative_ontology:measurement(ccs_broad_effects_su_t2005, commerce_clause_scope__broad_effects_test, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement_basis(ccs_broad_effects_su_t2005, observed).
narrative_ontology:measurement(ccs_broad_effects_su_t2025, commerce_clause_scope__broad_effects_test, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(ccs_broad_effects_su_t2025, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1937, tn=2025
narrative_ontology:measurement(ccs_broad_effects_grid_01, commerce_clause_scope__broad_effects_test, accessibility_collapse(class), 1937, 0.35).
narrative_ontology:measurement(ccs_broad_effects_grid_02, commerce_clause_scope__broad_effects_test, accessibility_collapse(class), 2025, 0.52).
narrative_ontology:measurement(ccs_broad_effects_grid_03, commerce_clause_scope__broad_effects_test, accessibility_collapse(individual), 1937, 0.3).
narrative_ontology:measurement(ccs_broad_effects_grid_04, commerce_clause_scope__broad_effects_test, accessibility_collapse(individual), 2025, 0.48).
narrative_ontology:measurement(ccs_broad_effects_grid_05, commerce_clause_scope__broad_effects_test, accessibility_collapse(organizational), 1937, 0.38).
narrative_ontology:measurement(ccs_broad_effects_grid_06, commerce_clause_scope__broad_effects_test, accessibility_collapse(organizational), 2025, 0.58).
narrative_ontology:measurement(ccs_broad_effects_grid_07, commerce_clause_scope__broad_effects_test, accessibility_collapse(structural), 1937, 0.3).
narrative_ontology:measurement(ccs_broad_effects_grid_08, commerce_clause_scope__broad_effects_test, accessibility_collapse(structural), 2025, 0.55).
narrative_ontology:measurement(ccs_broad_effects_grid_09, commerce_clause_scope__broad_effects_test, resistance(class), 1937, 0.32).
narrative_ontology:measurement(ccs_broad_effects_grid_10, commerce_clause_scope__broad_effects_test, resistance(class), 2025, 0.26).
narrative_ontology:measurement(ccs_broad_effects_grid_11, commerce_clause_scope__broad_effects_test, resistance(individual), 1937, 0.18).
narrative_ontology:measurement(ccs_broad_effects_grid_12, commerce_clause_scope__broad_effects_test, resistance(individual), 2025, 0.18).
narrative_ontology:measurement(ccs_broad_effects_grid_13, commerce_clause_scope__broad_effects_test, resistance(organizational), 1937, 0.5).
narrative_ontology:measurement(ccs_broad_effects_grid_14, commerce_clause_scope__broad_effects_test, resistance(organizational), 2025, 0.44).
narrative_ontology:measurement(ccs_broad_effects_grid_15, commerce_clause_scope__broad_effects_test, resistance(structural), 1937, 0.42).
narrative_ontology:measurement(ccs_broad_effects_grid_16, commerce_clause_scope__broad_effects_test, resistance(structural), 2025, 0.34).
narrative_ontology:measurement(ccs_broad_effects_grid_17, commerce_clause_scope__broad_effects_test, stakes_inflation(class), 1937, 0.35).
narrative_ontology:measurement(ccs_broad_effects_grid_18, commerce_clause_scope__broad_effects_test, stakes_inflation(class), 2025, 0.55).
narrative_ontology:measurement(ccs_broad_effects_grid_19, commerce_clause_scope__broad_effects_test, stakes_inflation(individual), 1937, 0.28).
narrative_ontology:measurement(ccs_broad_effects_grid_20, commerce_clause_scope__broad_effects_test, stakes_inflation(individual), 2025, 0.48).
narrative_ontology:measurement(ccs_broad_effects_grid_21, commerce_clause_scope__broad_effects_test, stakes_inflation(organizational), 1937, 0.3).
narrative_ontology:measurement(ccs_broad_effects_grid_22, commerce_clause_scope__broad_effects_test, stakes_inflation(organizational), 2025, 0.55).
narrative_ontology:measurement(ccs_broad_effects_grid_23, commerce_clause_scope__broad_effects_test, stakes_inflation(structural), 1937, 0.25).
narrative_ontology:measurement(ccs_broad_effects_grid_24, commerce_clause_scope__broad_effects_test, stakes_inflation(structural), 2025, 0.45).
narrative_ontology:measurement(ccs_broad_effects_grid_25, commerce_clause_scope__broad_effects_test, suppression(class), 1937, 0.32).
narrative_ontology:measurement(ccs_broad_effects_grid_26, commerce_clause_scope__broad_effects_test, suppression(class), 2025, 0.52).
narrative_ontology:measurement(ccs_broad_effects_grid_27, commerce_clause_scope__broad_effects_test, suppression(individual), 1937, 0.3).
narrative_ontology:measurement(ccs_broad_effects_grid_28, commerce_clause_scope__broad_effects_test, suppression(individual), 2025, 0.5).
narrative_ontology:measurement(ccs_broad_effects_grid_29, commerce_clause_scope__broad_effects_test, suppression(organizational), 1937, 0.33).
narrative_ontology:measurement(ccs_broad_effects_grid_30, commerce_clause_scope__broad_effects_test, suppression(organizational), 2025, 0.58).
narrative_ontology:measurement(ccs_broad_effects_grid_31, commerce_clause_scope__broad_effects_test, suppression(structural), 1937, 0.28).
narrative_ontology:measurement(ccs_broad_effects_grid_32, commerce_clause_scope__broad_effects_test, suppression(structural), 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, resource_allocation).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, intermediate_channels).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Commerce Clause' bundles three structurally distinct claims with disjoint epsilon values and victim sets, decomposed per the epsilon-invariance principle into a three-file family: this file (broad_effects_test, the standing post-1937 arrangement), narrow_originalist (a facilitation-only trade regime with a nearly empty victim set), and intermediate_channels (channels-plus-effects with limiting principles, victims confined to aggregation-of-economic-activity cases). Citation flow runs from narrow/originalist scholarship downstream into the limiting-principles movement that produced intermediate_channels; this reading's post-1937 operation created the structural pressure (perceived overreach) that shaped intermediate_channels' limiting principles (influences edge), while this reading and narrow_originalist contradict at the level of what 'regulate' means and cannot coexist within any single coherent framework (forecloses edge). Each file carries its own beneficiaries, victims, and epsilon; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
