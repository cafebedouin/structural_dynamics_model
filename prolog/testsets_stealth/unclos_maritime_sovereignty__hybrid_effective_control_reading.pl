% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: Graduated Feature-Based Maritime Sovereignty (Hybrid Effective-Control Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   A graduated maritime-sovereignty arrangement governs how ocean features
 *   generate jurisdictional space. Naturally formed features above high tide
 *   anchor full territorial seas and exclusive economic zones; artificially
 *   built structures receive only a 500-meter safety perimeter for navigation
 *   safety. The arrangement's second tier, however, lets an artificial
 *   structure's footprint ripen into a territorial claim when the building
 *   state maintains effective control long enough and no challenger
 *   successfully interrupts the clock. In operation this converts dredging
 *   capacity, garrison logistics, and naval reach into durable entitlement,
 *   while the burden of interruption falls on whichever claimant is weakest
 *   near the feature. The regime is claimed here as tangled_rope: the first
 *   tier performs genuine, widely relied-upon coordination, and the
 *   time-conditioned tier rides on top of it as an asymmetric transfer
 *   mechanism. Claim and metrics are authored independently; the engine
 *   computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - construction_capable_regional_powers: agenda-setting builder ([institutional]/[arbitrage]) — sets construction pace and administers the maturation clock
 *   - - militarily_weaker_claimants: primary target ([moderate]/[trapped]) — bears silent forfeiture as challenge lapses
 *   - - established_natural_feature_occupiers: dual-positioned holder ([moderate]/[constrained]) — collects full zones from natural holdings while losing margin to neighbors' builds
 *   - - adjacent_coastal_states: secondary target ([moderate]/[constrained]) — EEZ margins erode feature by feature
 *   - - artisanal_fishing_communities: diffuse cost-bearer ([powerless]/[trapped]) — displaced from traditional grounds by expanding perimeters
 *   - - global_shipping_interests: coordination beneficiary ([organized]/[mobile]) — plans around defined perimeters and lane boundaries
 *   - - distant_water_naval_powers: external balancer ([institutional]/[arbitrage]) — collects navigational predictability, pays in patrol tempo
 *   - - international_maritime_tribunals: analytical observer ([institutional]/[analytical]) — defines feature status; binds only consenting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.62).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.64).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "Graduated Feature-Based Maritime Sovereignty (Hybrid Effective-Control Reading)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '5d3ddfbe-596f-470a-9767-7f739317dbb1').
narrative_ontology:cs_kernel_codification('5d3ddfbe-596f-470a-9767-7f739317dbb1', fixed_text).
narrative_ontology:cs_authority_grounding('5d3ddfbe-596f-470a-9767-7f739317dbb1', distributed).
narrative_ontology:cs_reading_relation('5d3ddfbe-596f-470a-9767-7f739317dbb1', unclos_maritime_sovereignty__unclos_strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('5d3ddfbe-596f-470a-9767-7f739317dbb1', unclos_maritime_sovereignty__unclos_expansive_construction_reading, forecloses).
narrative_ontology:cs_axiom('5d3ddfbe-596f-470a-9767-7f739317dbb1', foundational, natural_artificial_graduated_sovereignty).
narrative_ontology:cs_axiom_status(natural_artificial_graduated_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5d3ddfbe-596f-470a-9767-7f739317dbb1', natural_artificial_graduated_sovereignty, conventional).
narrative_ontology:cs_axiom('5d3ddfbe-596f-470a-9767-7f739317dbb1', foundational, prescriptive_maturation_absent_challenge).
narrative_ontology:cs_axiom_status(prescriptive_maturation_absent_challenge, holdable).
narrative_ontology:cs_axiom_grounding('5d3ddfbe-596f-470a-9767-7f739317dbb1', prescriptive_maturation_absent_challenge, conventional).
narrative_ontology:cs_axiom('5d3ddfbe-596f-470a-9767-7f739317dbb1', secondary, safety_zone_navigation_priority).
narrative_ontology:cs_axiom_status(safety_zone_navigation_priority, holdable).
narrative_ontology:cs_axiom_grounding('5d3ddfbe-596f-470a-9767-7f739317dbb1', safety_zone_navigation_priority, instrumental).
narrative_ontology:cs_reference_frame('5d3ddfbe-596f-470a-9767-7f739317dbb1', graduated_feature_sovereignty_baseline).
narrative_ontology:cs_drift_state('5d3ddfbe-596f-470a-9767-7f739317dbb1', post_south_china_sea_arbitration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5d3ddfbe-596f-470a-9767-7f739317dbb1', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, established_natural_feature_occupiers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, global_shipping_interests).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, adjacent_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, artisanal_fishing_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, distant_water_naval_powers).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, established_natural_feature_occupiers).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, distant_water_naval_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates dredging fleets, garrisons, and coast guards; decides which features to build on, when to move from a safety perimeter to civilian administration and military deployment, and how fast to escalate. Its construction pace sets the tempo every other party responds to. It can alternate between arguing law before tribunals and presenting completed facts on the water, and it can decline the jurisdiction of any forum that rules against it.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers, agenda_setter,
    institutional, generational, arbitrage, regional).

% Asserts entitlements to features and waters it cannot physically police. Each year without a successful interruption consolidates the neighbor's position; mounting an interruption risks a confrontation it would lose, while staying silent forfeits the claim. Diplomatic protest registers in the record but does not stop the clock under the reading's time-conditioned logic. Leaving the dispute entirely means abandoning territory framed domestically as non-negotiable.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants, payer,
    moderate, generational, trapped, regional).

% Holds naturally formed islands and reefs and draws full territorial-sea and exclusive-economic-zone entitlements from them under the reading's first tier. At the same time it watches stronger neighbors build artificial structures nearby whose perimeters and eventual claims press toward its holdings. Contesting every structure exceeds its patrol budget, so it rations objections and loses margin between protests.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, established_natural_feature_occupiers, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, established_natural_feature_occupiers, payer).

% Sits outside the immediate feature dispute but holds coasts whose 200-nautical-mile entitlements shrink as artificial structures near their shores accumulate control. Fisheries access and seabed prospects erode feature by feature. Its recourse is protest notes and coalition diplomacy, both slower than dredging.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, adjacent_coastal_states, payer,
    moderate, generational, constrained, regional).

% Works traditional grounds that fall inside expanding security perimeters around fortified features. Patrols and exclusion radii push crews farther from historical fishing banks; shifting grounds means longer voyages at higher fuel cost or leaving the trade. These communities have no seat in any forum where feature status is argued.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, artisanal_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Relies on predictable lane boundaries and clearly marked installation perimeters; the 500-meter rule keeps platforms off main routes and gives insurers a computable risk surface. Pays indirectly through rerouting and premium increases when perimeters expand around fortified features, and can shift trades elsewhere if a corridor closes.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, global_shipping_interests, beneficiary,
    organized, biographical, mobile, global).

% Collects from the stable baseline the reading preserves — open lanes, defined perimeters, predictable transit for commerce and carrier groups alike. Pays in operational tempo: regular challenge patrols are the main instrument keeping maturation clocks from running on features whose absorption would close approaches it depends on.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, distant_water_naval_powers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, distant_water_naval_powers, payer).

% Adjudicates feature status and zone entitlement when parties accept jurisdiction; its rulings define what counts as a natural island, a rock, or an artificial structure. It commands no fleet, so its determinations bind only those who consent to be bound — and the largest builder in the most consequential dispute has declined its jurisdiction.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_maritime_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_regional_powers).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__hybrid_effective_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides graduated, predictable rules for maritime space: naturally formed features anchor full territorial-sea and exclusive-economic-zone regimes that shipping, insurers, and coastal administrations can plan around; artificial installations receive a defined 500-meter safety perimeter that keeps platforms off navigation routes; and the time-conditioned pathway offers states a way to consolidate contested positions without open war, substituting patience for bombardment in frozen sovereignty disputes.
% TRANSFER_FUNCTION: Moves maritime space — fishing grounds, hydrocarbon access, sea-lane control, strategic depth — from militarily weaker claimants and adjacent coastal states toward states able to build and garrison features, converting construction expenditure and naval presence into durable entitlement as challenge lapses.
% ABSENT_VOICES: Weaker claimants object, but their objections register as protest notes that the time-conditioned logic discounts unless backed by physical presence; artisanal fishing communities displaced by expanding perimeters have no seat anywhere the terms are argued; and the strongest builder removed itself from the adjudicative room entirely by rejecting tribunal jurisdiction, so the terms harden in forums it does not enter.
% DISAPPEARANCE_RATIONALE: If the graduated arrangement vanished overnight, maritime boundaries would revert to raw positional contest: built features would lose their legal-horizon payoff logic, claimant states would re-militarize disputed waters to defend holdings by presence alone, shipping would lose defined perimeters and reprice corridor risk, and every frozen occupation would become an active standoff.
% FOUNDING_PROBLEM: Mid-twentieth-century offshore resource claims collided with freedom of navigation. UNCLOS III built the zoning regime but left artificial structures and long-running occupations under-specified; the hybrid reading filled that gap — giving installations a defined perimeter while offering states a path to consolidate contested positions short of armed seizure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Annex VII tribunal jurisprudence (including the 2016 South China Sea award on feature status), ITLOS advisory practice, shipping-industry bodies (ICS/BIMCO) attesting the value of navigational certainty, the academic international-law literature on prescription and artificial islands, and the diplomatic protest record of coastal states — all attest both that the underlying problem remains live and that the time-conditioned tier now functions as a conversion mechanism for construction capacity.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is intermediate (0.62 at interval end): the natural-feature tier is near-benign coordination that shipping and natural-feature holders genuinely rely on, while the time-conditioned tier transfers zones steadily from those who cannot police waters to those who can build in them. Suppression (0.64) reflects the coercive maintenance the arrangement requires — coast-guard interdiction, exclusion radii, militia harassment of challengers — and is authored as a raw structural property, unscaled; the engine scales only extractiveness, by directionality and scope. Theater (0.32) captures the growing share of 'research station' and 'civil facility' framing over what functions as garrison infrastructure, while the safety-perimeter function remains real. Accessibility collapse is moderate (0.48): adjudication, coalition diplomacy, and challenge patrols exist as alternatives, but none restores zones already consolidated, and understanding the clock creates no exit from it. Resistance (0.61) is real and sustained — arbitration, protest coalitions, rival occupation, freedom-of-navigation operations. The three measurement series run on one shared time grid (points 0–30, every metric at every point) so no end-state value is injected into earlier rows; the rising base_extractiveness series models rent accumulation layered onto coordination as construction scaled after 2010, and the rising suppression_requirement series is authored deliberately because the interval tracks a visible build-up of enforcement machinery (coast-guard law expansions, militia integration, hardened perimeters), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter's position — arbitrage exit, generational horizon — the arrangement is order-building it conducts: a patient, lawful-seeming substitute for war that it can litigate or ignore as convenient. From the trapped payer seats the identical structure operates as slow-motion expropriation: rights decay silently unless defended at escalation risk. Established natural-feature occupiers straddle the divide, defending the first tier that entitles them while losing ground to the second. Distant naval powers experience the arrangement as a bargain worth patrolling for. The tribunal seat sees doctrinal integrity at stake with no fleet behind its rulings. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Construction-capable regional powers sit near the beneficiary end: they wrote the operating tempo, collect the consolidating zones, and hold arbitrage-grade exit between legal argument and completed fact. Global shipping interests sit nearest the beneficiary pole — mobile, paying only diffuse indirect costs for a perimeter system they actively use. Distant naval powers are dual-positioned: subsidized by the stable baseline, taxed in patrol tempo, landing mid-low. Militarily weaker claimants and adjacent coastal states sit near the target end — trapped or constrained, bearing the transfer directly, with no exit that preserves their entitlements. Artisanal fishing communities bear the costs most concretely and hold the least power: full-target directionality at negligible counterweight. Established natural-feature occupiers are genuinely mixed — beneficiaries of the natural tier, partial targets of the maturation tier — and the derivation should place them mid-scale rather than at either pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetric errors. Reading the whole arrangement as pure coordination misses the conversion mechanism — the clause that turns construction budgets into title while discounting protest — and would certify a transfer as a public good. Reading it as pure extraction misses the genuine zoning and safety function that shipping, insurers, and natural-feature holders defend and would mispredict their behavior. On lifecycle placement: the founding problem is live (corroborated outside the benefiting parties), no sunset exists, the arrangement is administered actively rather than maintained theatrically, and the gains accrue to a named concentrated seat — so neither the piton cell (diffuse gains, prohibitive fix, atrophied function) nor the scaffold cell (transitional mandate) fits. The status-live x world_rearranges combination raises no obsolescence flag; the arrangement's mandate has not outlived its function, but its second tier has drifted well past the gap-filling role the founding problem defined, which the practice-drift declaration records.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the hybrid_effective_control_reading of the unclos_maritime_sovereignty kernel; how would adoption of the strict_geographic_reading or the expansive_construction_reading change the structure?',
    'Observe which reading future Annex VII awards, ITLOS jurisprudence, and crystallizing state practice endorse; a categorical feature-status rule adopted by tribunals or a rival custom consolidated among builders would resolve the contest.',
    'Under the strict sibling the maturation clause disappears — construction never alters status — and measured extraction falls toward pure coordination cost; under the expansive sibling construction immediately generates waters, the natural/artificial tier collapses, and the burden on weaker claimants rises sharply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the UNCLOS feature-status kernel; sibling readings move epsilon in opposite directions.').

omega_variable(
    prescription_threshold_indeterminacy,
    'How long is ''prolonged,'' and what quantum of continuous state activity suffices for an artificial feature''s control to mature into a territorial claim?',
    'Tribunal treatment of prescription periods in maritime settings, or negotiated codification of a maturity threshold; absent either, track how long existing built features have gone un-reversed in practice.',
    'A short, clear threshold accelerates forfeiture for weaker claimants and raises effective extraction; a long or undefined threshold preserves ambiguity that the construction-capable party can litigate indefinitely — the indeterminacy itself favors the strong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prescription_threshold_indeterminacy, conceptual, 'No agreed metric exists for the maturation clock the reading''s second tier runs on.').

omega_variable(
    challenge_adequacy_standard,
    'Do diplomatic protests and arbitral filings interrupt the maturation clock, or does only sustained physical interdiction count as challenge?',
    'Compare outcomes across features met with protest-only responses versus features met with persistent physical presence; observe whether any tribunal has credited protest alone as defeating acquiescence.',
    'If protest suffices, weaker claimants can preserve their positions cheaply and the arrangement stays moderately burdensome; if physical challenge is required, preserving a claim demands risking armed encounter, and the maturation clause operates as a transfer mechanism priced in escalation risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(challenge_adequacy_standard, empirical, 'The operative definition of ''challenge'' determines who bears the cost of stopping the clock.').

omega_variable(
    article_121_rock_limitation,
    'How many of the natural features anchoring the reading''s first tier are legally islands rather than rocks incapable of sustaining habitation or economic life — and does the natural tier survive that limitation?',
    'Feature-by-feature adjudication or independent survey of habitation and economic-life evidence on occupied natural features.',
    'If most natural features are legally rocks, the reading''s coordination tier shrinks to a narrow band, its time-conditioned tier dominates the structure, and the graduated bargain reads as transfer dressed in coordination''s clothing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_121_rock_limitation, empirical, 'The natural/artificial boundary the graduation rests on is empirically thinner than the reading assumes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(uncl_tr_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(uncl_tr_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(uncl_tr_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(uncl_tr_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(uncl_tr_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(uncl_be_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(uncl_be_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(uncl_be_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(uncl_be_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(uncl_su_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 5, 0.51).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(uncl_su_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(uncl_su_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(uncl_su_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 30, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_expansive_construction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'UNCLOS island regime' decomposes into three structurally distinct claims about artificial features: categorical immutability (strict_geographic_reading), immediate generation of waters by construction (expansive_construction_reading), and time-conditioned maturation (this file). Each carries its own epsilon, beneficiary structure, and classification; they form one constraint family linked via affects_constraints. This reading sits between its siblings: it preserves the strict reading's natural-feature tier, admits a prescription pathway the strict reading denies, and conditions on time-and-acquiescence what the expansive reading grants immediately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
