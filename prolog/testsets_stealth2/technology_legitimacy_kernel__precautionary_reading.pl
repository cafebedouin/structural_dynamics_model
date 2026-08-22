% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__precautionary_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__precautionary_reading
 *   human_readable: Precautionary Reversibility Criterion for Climate-Technology Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the technology_legitimacy_kernel:
 *   the precautionary_reading, under which a technology is legitimate for
 *   climate mitigation if and only if its worst-case failure modes and legacy
 *   costs are bounded and reversible within a generation. Per the
 *   epsilon-invariance discipline, the sibling readings
 *   (reliability_primacy_reading, velocity_primacy_reading) are separate
 *   constraints in separate files; nothing about their content is folded into
 *   this story's metrics or structure. The standing arrangement under contest
 *   — the thing this story is about — is the operative gatekeeping regime
 *   that applies the reversibility screen through taxonomy rules,
 *   green-finance eligibility, and portfolio mandates. Its coordination
 *   function is genuine (it is the only present institution that prices
 *   century-scale legacies at the point of deployment decision); its
 *   extraction is also real (the same screen forecloses an entire low-carbon
 *   technology family from the mitigation project and channels the freed
 *   capital to the families that pass). The claim and the metrics are
 *   authored independently: I claim tangled_rope because both components are
 *   structurally present and enforcement is load-bearing; the metrics
 *   describe how the regime actually operates, and the engine computes
 *   per-seat types from the structural data.
 *
 * KEY AGENTS:
 *   - green_taxonomy_regulators: agenda setter (institutional/arbitrage) — administers the screen, commissions assessments, controls eligibility
 *   - renewable_energy_industries: primary beneficiary (powerful/mobile) — passes the screen by construction, collects the legitimacy premium
 *   - climate_advocacy_organizations: beneficiary (organized/identity_locked) — supplies the screen's moral vocabulary; identity fused with the frame
 *   - nuclear_industry_operators: primary target (organized/trapped) — foreclosed from green finance and mandates despite low-carbon output
 *   - electricity_ratepayers: diffuse cost bearer (moderate/constrained) — absorbs system-cost consequences in tariffs
 *   - repository_host_communities: standing burden (powerless/trapped) — host the irreversible stock the screen cites but never remediates
 *   - future_generations: declared shielded party (powerless/trapped) — protected prospectively, seated nowhere, represented only by proxies
 *   - developing_nation_energy_planners: excluded voice (moderate/constrained) — finance conditioned on screens they never helped draft
 *   - integrated_assessment_bodies: analytical observer (institutional/analytical) — models portfolios under rival criteria, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.6).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.62).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Precautionary Reversibility Criterion for Climate-Technology Legitimacy").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, '1d47f399-d39b-486c-9d25-2264d74f6231').
narrative_ontology:cs_kernel_codification('1d47f399-d39b-486c-9d25-2264d74f6231', formalized).
narrative_ontology:cs_authority_grounding('1d47f399-d39b-486c-9d25-2264d74f6231', expertise).
narrative_ontology:cs_interpretation_layer_present('1d47f399-d39b-486c-9d25-2264d74f6231').
narrative_ontology:cs_reading_relation('1d47f399-d39b-486c-9d25-2264d74f6231', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d47f399-d39b-486c-9d25-2264d74f6231', technology_legitimacy_kernel__velocity_primacy_reading, influences).
narrative_ontology:cs_axiom('1d47f399-d39b-486c-9d25-2264d74f6231', foundational, irreversible_legacy_disqualifies).
narrative_ontology:cs_axiom_status(irreversible_legacy_disqualifies, holdable).
narrative_ontology:cs_axiom_grounding('1d47f399-d39b-486c-9d25-2264d74f6231', irreversible_legacy_disqualifies, deontological).
narrative_ontology:cs_axiom('1d47f399-d39b-486c-9d25-2264d74f6231', secondary, proxies_may_decide_for_absent_parties).
narrative_ontology:cs_axiom_status(proxies_may_decide_for_absent_parties, holdable).
narrative_ontology:cs_axiom_grounding('1d47f399-d39b-486c-9d25-2264d74f6231', proxies_may_decide_for_absent_parties, conventional).
narrative_ontology:cs_reference_frame('1d47f399-d39b-486c-9d25-2264d74f6231', intergenerational_trusteeship_standard).
narrative_ontology:cs_drift_state('1d47f399-d39b-486c-9d25-2264d74f6231', contemporary_taxonomy_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1d47f399-d39b-486c-9d25-2264d74f6231', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewable_energy_industries).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, climate_advocacy_organizations).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, future_generations).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, electricity_ratepayers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, repository_host_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain official sustainability classification schemes and apply the bounded-and-reversible-within-a-generation test to candidate technologies. They commission worst-case assessments, decide which generation sources qualify for green labels, subsidized finance, and portfolio mandates, and can revise the criteria — though revision carries coalition, legal, and credibility costs. They collect administrative authority from operating the screen.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, green_taxonomy_regulators, agenda_setter,
    institutional, generational, arbitrage, continental).

% Sell generation whose decommissioning is physically straightforward and increasingly contractually bonded, so they pass the reversibility screen by construction. They receive the resulting legitimacy premium: preferential finance, faster permitting, and eligibility for green procurement. They would remain commercially viable under rival criteria, but the largest share of mitigation capital flows to them under this one.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewable_energy_industries, beneficiary,
    powerful, generational, mobile, global).

% Campaign for adoption of the reversibility screen, staff consultations, litigate its application, and supply the moral vocabulary in which it is defended. Their membership, funding, and agenda-setting influence flow from the screen's operation, and their organizational self-concept is constituted by the precautionary frame — abandoning it would dissolve the organization's purpose rather than relocate it.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, climate_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, global).

% Operate low-carbon dispatchable plants that fail the screen on waste legacy and accident tails regardless of individual plant records. Their assets, licenses, and workforce are reactor-specific and cannot be converted to the favored technology family; without green designation their capital costs rise sharply and portfolio mandates close. Their available moves are litigation, lobbying for carve-outs, and absorbing foreclosure.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_operators, payer,
    organized, generational, trapped, global).

% Fund the power system through bills and taxes. In jurisdictions applying the screen strictly, the set of eligible firm low-carbon options shrinks, so system builds lean on storage-heavy or gas-bridged configurations whose costs land in tariffs. They did not choose the screen, cannot vote on taxonomy technical committees, and can exit only by moving or self-supplying.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, electricity_ratepayers, payer,
    moderate, biographical, constrained, national).

% Live beside the existing irreversible stock — waste repositories, accident-exclusion zones, contaminated sites — that the screen cites as its evidentiary anchor. The screen's operation directs no remediation resources to them; their burden persists as the standing proof that irreversible legacies are real. They cannot relocate the legacy and face steep personal costs of leaving.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, repository_host_communities, payer,
    powerless, generational, trapped, regional).

% Inherit whatever legacies pass the gate and whatever climate trajectory the chosen portfolio produces. The screen's protections accrue to them prospectively: bounded waste, bonded decommissioning, reversible land use. They hold no seat in any proceeding that sets or applies the screen; their interests enter only through proxies who speak for them.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, future_generations, excluded).

% Plan buildouts in regions where dense, firm, low-carbon capacity is scarce and import dependence is acute. They find concessional climate finance increasingly conditioned on screens authored mainly by wealthy post-industrial states whose risk preferences and grid conditions differ from their own. They are not represented in the technical committees where eligibility tests are drafted.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, developing_nation_energy_planners, excluded,
    moderate, biographical, constrained, national).

% Model mitigation portfolios under alternative eligibility criteria and publish scenario comparisons showing cost, reliability, and legacy outcomes of each. They enforce nothing and collect nothing from the screen's operation; their analyses are cited by every side of the eligibility dispute.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, integrated_assessment_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__precautionary_reading, renewable_energy_industries).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__precautionary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: deployment decisions systematically discount irreversible, century-scale legacy costs because the parties who would bear them hold no seat in present decisions. The screen gives capital allocators, permittees, and procurement officers a shared ex ante test so that competition for the cheapest kilowatt-hour does not silently purchase unbounded legacies.
% TRANSFER_FUNCTION: Moves legitimacy — and through it finance access, permitting speed, and portfolio eligibility — toward technologies with bonded, reversible decommissioning and away from technologies with unbounded worst-case tails. Concretely it shifts mitigation capital toward renewable generation and away from nuclear, while the present-day system-cost consequences of that shift land on ratepayers.
% ABSENT_VOICES: Developing-nation energy planners, future generations themselves, and ordinary ratepayers are outside the taxonomy and standard-setting rooms where the test is drafted and applied. The broad agreement among seated parties partly reflects who was invited: the parties most exposed to the screen's costs (foreclosed industries, finance-denied planners, tariff-bound households) had no drafting seat.
% DISAPPEARANCE_RATIONALE: If the screen vanished overnight, green finance definitions would need immediate replacement or collapse, nuclear projects would regain eligibility and financing, capital allocation across mitigation portfolios would reshuffle within quarters, and the advocacy coalitions and technical committees built around the criterion would lose their coordinating frame. Arrangements visibly depend on it.
% FOUNDING_PROBLEM: Technological lock-in to irreversible harms: the recognition, sharpened by nuclear waste accumulation and fossil infrastructure inertia, that some energy choices create burdens outlasting the institutions that chose them, and that unaided markets will not price century-scale liabilities because no present counterparty owes them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: national waste-management agency inventories document growing legacy stocks, actuarial reviews document decommissioning liabilities exceeding original estimates, and insurers' retreat from construction risk attests that the underlying hazard is priced as real by parties with no stake in the screen's survival.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__precautionary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.60: the screen redirects the mitigation investment pool and forecloses a rival family's market access, but a substantial share of its cost-imposition is the protection it exists to provide, so it does not reach snare-range values. Suppression is 0.62 and is authored as a RAW structural property — it is not scaled by power or scope anywhere in the pipeline; only extractiveness is scaled (by directionality and spatial scope) in the engine's computation. Suppression here is structural, not internalized: denial of green designation, portfolio mandates, and procurement rules, with no cognitive-lock component distinct from the financial barriers. Theater is 0.30: reversibility assessments for paradigm cases are frequently decided before the assessment runs, but the gating has real finance consequences, so performance is a minority share of activity. Accessibility_collapse is 0.45 — deliberately low for a construct: once the screen is understood, alternatives persist (case-by-case liability bonding, insurance-backed deployment, and two live sibling criteria), so the space of governance ideas does not collapse. Resistance is 0.60: sustained contestation by nuclear-capable states, industry litigation, and counter-declarations at international climate summits. All three tracked metrics run on ONE shared time grid (T=0,4,8,12,16,20) so every metric is authored at every examined point; the trajectories show monotonic hardening — extraction and enforcement infrastructure rose together as advisory principle became binding finance gate — with no oscillation, so no cyclical analysis is required.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the finding. From the agenda-setter and advocacy seats the arrangement is protective coordination they built and staff: the screen is the only mechanism giving the unrepresented a veto, and its costs are the price of trusteeship. From the nuclear-operator seat the same structure is enforced foreclosure — a legitimacy tribunal whose test their product cannot pass regardless of operating record, administered by coalitions that benefit from its verdicts. From the ratepayer and host-community seats it is a burden-allocation device: costs arrive in tariffs and in unremediated legacy stock while the protection accrues elsewhere. The engine derives these divergent per-seat classifications from the power, exit, and directional data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable industries sit nearest the beneficiary pole: declared beneficiary with mobile exit, meaning they retain viability under rival regimes and thus hold arbitrage-grade insulation — the derivation places their d near 0.0. Advocacy organizations are beneficiaries with identity_locked exit: they collect influence and membership from the screen's operation and cannot conceptually exit, which anchors them near the beneficiary end while making their position inertial. Future generations are declared beneficiaries (the screen's protections accrue to them) with trapped exit and no seat — the derivation assigns low d, but the proxy-representation omega flags that this low d is asserted on behalf of a party that never confirmed it. Nuclear operators sit near the full-target pole: declared victim, trapped exit (reactor-specific assets), high d amplified by their inability to arbitrage the regime. Ratepayers carry moderately high d: diffuse, compulsory cost-bearing with constrained exit. Repository host communities carry high d: trapped beside the burden that anchors the regime's moral force, receiving nothing from its operation. Taxonomy regulators are not declared beneficiary or victim; their d comes from the canonical fallback for the institutional power atom, adjusted by their arbitration-grade control of the criteria — commentary notes this seat's mild beneficiary tilt (authority collection) as a candidate for a future override if corpus comparison shows systematic misfit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — markets do not price century-scale irreversible liabilities — remains live, corroborated by waste-agency inventories, actuarial reviews, and insurer behavior from outside the benefiting parties, so this is not a resolved mandate kept alive by inertia; the mandatrophy-resolved flag is accordingly not declared. Classification as tangled_rope prevents the two symmetric misreadings: reading the screen as pure rope (as its proponents do) erases the documented receipts — foreclosed market access and redirected capital accruing to identifiable seats — while reading it as pure snare (as its opponents do) erases the genuine coordination function that no rival institution currently performs. The theater_ratio trajectory matters diagnostically: it is rising but still minority-share, consistent with a hybrid whose coordination core is intact while procedural performance accumulates around it. If the decidability omega resolves against the screen — if the test proves framing-dominated — the theater share should be re-authored upward and the classification revisited toward the snare boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the technology_legitimacy_kernel — the precautionary_reading, which makes bounded-and-reversible-within-a-generation the sole legitimacy test. Sibling readings (reliability_primacy_reading, velocity_primacy_reading) instantiate different constraints from the same kernel with different beneficiary/victim structures: under reliability primacy nuclear enters the legitimate set and intermittent renewables lose standing; under velocity primacy the test is deployability within the carbon budget and legacy reversibility drops out entirely. Where is the disagreement located?',
    'The disagreement is located in the decision criterion itself: which single property (reversibility, dispatchability, deployability velocity) a technology must possess to count as legitimate. Resolution would require either a lexical framework ordering the properties or an explicit multi-criterion scheme — both are framework-level choices no dataset settles.',
    'Switching readings reassigns renewables and nuclear between the beneficiary and victim sets and moves epsilon materially; classifications computed from this file are valid only for the precautionary seat and must not be averaged across the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    reversibility_decidability,
    'Is ''bounded and reversible within a generation'' an empirically decidable property of a technology, or does its application depend on framing choices (what counts as the worst case, how accident tails are weighted, what counts as one generation) that predetermine outcomes for paradigm cases?',
    'Compare assessment outcomes across independent technical bodies applying the test to the same candidate technologies under divergent framing assumptions; convergence would indicate decidability, persistent divergence would indicate framing dominance.',
    'If the test is effectively undecidable, it functions as an authoritative labeling device wielded by whoever staffs the committee — theater rises, the coordination claim weakens, and the arrangement drifts toward capture by the labeling authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_decidability, conceptual, 'Whether the screen''s core predicate is a test or a framing-dependent label.').

omega_variable(
    proxy_representation_fidelity,
    'Do the present actors who invoke future generations (advocacy organizations, taxonomy committees) accurately transmit those interests, or does the proxy structure filter them through present-coalition preferences?',
    'Compare the screen''s actual outputs against what the affected future cohorts would rationally accept behind a veil of temporal ignorance — e.g., whether bonded-decommissioning requirements bind at levels actuaries judge sufficient, or at levels convenient to present buildout schedules.',
    'If filtered, part of the protection credited to the declared beneficiary (future generations) actually subsidizes present actors, and the measured extraction is misattributed across seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proxy_representation_fidelity, empirical, 'Whether proxy advocacy for absent parties tracks the absent parties'' interests.').

omega_variable(
    selective_scrutiny_asymmetry,
    'Does the screen''s enforcement apply worst-case and legacy scrutiny uniformly across the energy portfolio — fossil lock-in fails reversibility far more severely than nuclear — or selectively, with rigorous application concentrated on the excluded competitor?',
    'Audit eligibility decisions and finance flows: compare the depth of legacy assessment applied to nuclear applications versus to gas-bridge extensions and fossil-adjacent transitions that received transitional green designations.',
    'Uniform application strengthens the coordination-function claim; selective application indicates the screen operates partly as a competitive instrument against a rival technology family, supporting the asymmetric-extraction component and drift toward harder classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_scrutiny_asymmetry, empirical, 'Whether the reversibility test is applied evenhandedly or aimed at one technology family.').

omega_variable(
    foreclosure_opportunity_cost,
    'What is the mitigation and system cost of foreclosing firm low-carbon options under this screen — slower decarbonization, higher tariffs, extended fossil bridges — and who bears it on what timeline?',
    'Integrated assessment modeling comparing screened and unscreened portfolios under identical carbon constraints, disaggregated by cost-bearing cohort and decade.',
    'Large modeled costs borne by ratepayer and future-generation seats raise their effective extraction above what the headline metric suggests; negligible costs would support the reading''s own claim that the screen''s price is small.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_opportunity_cost, empirical, 'Magnitude and incidence of the opportunity cost imposed by technological foreclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tlk_precautionary_tr_t0, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(tlk_precautionary_tr_t4, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(tlk_precautionary_tr_t8, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(tlk_precautionary_tr_t12, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(tlk_precautionary_tr_t16, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(tlk_precautionary_tr_t20, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(tlk_precautionary_be_t0, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tlk_precautionary_be_t4, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(tlk_precautionary_be_t8, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(tlk_precautionary_be_t12, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(tlk_precautionary_be_t16, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(tlk_precautionary_be_t20, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(tlk_precautionary_su_t0, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(tlk_precautionary_su_t4, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(tlk_precautionary_su_t8, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(tlk_precautionary_su_t12, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(tlk_precautionary_su_t16, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(tlk_precautionary_su_t20, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, velocity_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'legitimate climate technology' covers three structurally distinct claims with materially different epsilon values — the precautionary screen (this file), the reliability gate, and the velocity gate. They are not one constraint viewed from three angles: their victim sets differ (nuclear is victim under precaution, beneficiary under reliability; renewables invert likewise), their failure modes differ, and their enforcing institutions differ. Upstream/downstream structure: the precautionary reading INFLUENCES the velocity reading by shrinking the option set over which deployment-speed claims are computed (foreclosure changes feasibility arithmetic), while the reliability reading is routinely cited as evidence AGAINST precautionary exclusions in eligibility disputes. Each family member links the others in its affects_constraints array; cross-reading classification averaging is prohibited.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
