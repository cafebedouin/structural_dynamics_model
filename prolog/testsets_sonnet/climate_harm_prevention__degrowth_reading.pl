% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Degrowth Reading of Climate Harm Prevention: Planned Contraction as Legitimate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth reading of the contested
 *   climate_harm_prevention kernel: the claim that legitimate climate
 *   response requires planned economic contraction in the Global North
 *   because mitigation within a growth framework is physically and
 *   politically impossible. It is one of three sibling readings
 *   (mitigation_priority, adaptation_priority) of the same underlying kernel
 *   — the shared commitment to preventing climate harm. This reading is
 *   authored as a clean, self-contained constraint: it does not average
 *   across the siblings' claims or hedge its own epsilon against theirs. Its
 *   beneficiary set (Global South communities, future generations) and victim
 *   set (Global North working/middle classes, carbon-intensive-sector
 *   workers) are specific to this reading's rejection of growth as a fixed
 *   constraint boundary, and differ structurally from what the
 *   mitigation_priority reading would declare (which keeps growth-sector
 *   capital and consumers largely intact and shifts cost toward
 *   decarbonization-technology investment instead).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.58).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.62).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Degrowth Reading of Climate Harm Prevention: Planned Contraction as Legitimate Response").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '8350f04d-2455-4afc-8874-5f51cb048f97').
narrative_ontology:cs_kernel_codification('8350f04d-2455-4afc-8874-5f51cb048f97', distributed).
narrative_ontology:cs_authority_grounding('8350f04d-2455-4afc-8874-5f51cb048f97', distributed).
narrative_ontology:cs_reading_relation('8350f04d-2455-4afc-8874-5f51cb048f97', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('8350f04d-2455-4afc-8874-5f51cb048f97', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_axiom('8350f04d-2455-4afc-8874-5f51cb048f97', foundational, growth_is_not_a_fixed_constraint_boundary).
narrative_ontology:cs_axiom_status(growth_is_not_a_fixed_constraint_boundary, holdable).
narrative_ontology:cs_axiom_grounding('8350f04d-2455-4afc-8874-5f51cb048f97', growth_is_not_a_fixed_constraint_boundary, empirically_contingent).
narrative_ontology:cs_axiom('8350f04d-2455-4afc-8874-5f51cb048f97', foundational, differentiated_historical_responsibility_requires_present_contraction).
narrative_ontology:cs_axiom_status(differentiated_historical_responsibility_requires_present_contraction, holdable).
narrative_ontology:cs_axiom_grounding('8350f04d-2455-4afc-8874-5f51cb048f97', differentiated_historical_responsibility_requires_present_contraction, deontological).
narrative_ontology:cs_reference_frame('8350f04d-2455-4afc-8874-5f51cb048f97', post_paris_growth_consistent_mitigation_consensus).
narrative_ontology:cs_drift_state('8350f04d-2455-4afc-8874-5f51cb048f97', post_ipcc_ar6_synthesis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8350f04d-2455-4afc-8874-5f51cb048f97', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_communities).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, ecosystem_stability_claimants).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_working_and_middle_classes).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, carbon_intensive_industry_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_consumer_economy).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, growth_is_not_a_fixed_constraint_boundary).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, differentiated_historical_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academics, activists, and a minority bloc within Global North governments who argue mitigation targets are unreachable inside a growth-committed economy, and who push for binding planned contraction policies (reduced material throughput, work-time reduction, wealth caps) as the only physically coherent path. They administer the framing through publications, policy proposals, and advocacy, but hold no direct enforcement power over national budgets or industrial policy.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_policy_coalition, agenda_setter,
    organized, generational, mobile, global).

% Bear disproportionate climate harm despite minimal historical emissions. Under this reading, Global North contraction directly reduces the harm trajectory they face and frees atmospheric and resource budget for their own development. They have no seat in Global North domestic politics that decides whether contraction happens, and cannot compel it themselves.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_communities, beneficiary,
    powerless, generational, trapped, global).

% Inherit whatever emissions budget and ecological base current policy leaves them. This reading treats them as the primary intended beneficiary of contraction, though they cannot advocate, vote, or bargain in present-day policy processes — their interest is asserted on their behalf by the degrowth coalition and climate scientists.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Would bear the direct cost of planned contraction: reduced consumption, restructured or eliminated jobs in growth-dependent sectors, constrained material living standards. They did not individually set historical emissions policy at anything like the scale of capital owners or states, yet planned contraction as commonly proposed falls on their consumption and employment first. Exit is limited to emigration or political resistance; most cannot simply opt out of the national economy they are embedded in.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_working_and_middle_classes, payer,
    moderate, biographical, constrained, national).

% Employed in fossil fuel extraction, heavy manufacturing, aviation, and logistics sectors slated for the fastest contraction under this reading. Their livelihoods and regional economies are structurally tied to exactly the activity the constraint requires shrinking, with no comparable alternative employment base readily available in most contraction timelines proposed.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, carbon_intensive_industry_workers, payer,
    powerless, biographical, trapped, national).

% Hold the actual enforcement capacity — fiscal policy, monetary policy, industrial regulation — that would be required to implement planned contraction at scale, but are structurally committed to growth as the metric of legitimacy (GDP targets, employment mandates, debt-servicing requirements tied to growth expectations). They are largely absent from seriously operationalizing this reading; their institutional design excludes contraction as an admissible policy object even where officials privately concede the physics.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_states_and_central_banks, excluded,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, global_north_states_and_central_banks, agenda_setter).

% Own the capital stock whose valuation depends on continued growth and continued fossil-intensive throughput in many sectors. Not formally part of the contraction debate's stated stakeholder set, but their objection — that a legitimate response cannot include planned contraction — sets the practical limits on which policies reach a legislature. Global mobility of capital lets them relocate or diversify away from jurisdictions that adopt contraction policy, which pressures those jurisdictions not to adopt it.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, growth_dependent_capital_owners, excluded,
    powerful, biographical, arbitrage, global).

% Hold the sibling reading that emissions reduction can succeed within a growth framework via technological transition. They are excluded from this constraint's own frame by definition (this reading asserts their premise is physically/politically impossible), but they dominate the actual institutional venues — UNFCCC processes, national climate plans — where climate policy legitimacy is currently adjudicated.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, mitigation_priority_reading_adherents, excluded,
    institutional, generational, analytical, global).

% Produce the emissions-budget and warming-trajectory analyses that both this reading and its siblings draw on. They do not adjudicate between degrowth, mitigation, or adaptation framings — their physical modeling is compatible with more than one policy reading, which is part of what keeps the kernel contested rather than settled.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, climate_scientists_and_ipcc_working_groups, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a claim about the physical carbon budget with a policy prescription: since remaining budget is finite and Global North historical/per-capita emissions are disproportionate, the reading proposes to allocate the burden of staying within budget by contracting Global North material throughput rather than assuming technology substitutes fast enough at growth-consistent rates.
% TRANSFER_FUNCTION: Moves consumption capacity, employment stability, and near-term material comfort from Global North working and middle classes (and especially carbon-intensive-sector workers) toward an expanded emissions budget and improved climate trajectory for Global South communities and future generations.
% ABSENT_VOICES: Global North states and central banks are structurally excluded from operationalizing the reading even when they might privately accept its physics, because growth is embedded as a legitimacy metric in their own institutional design. Growth-dependent capital owners are excluded from the stated stakeholder debate but effectively veto through capital mobility. Future generations and Global South communities, the reading's primary beneficiaries, have no direct voice in the political processes that would decide whether contraction is adopted.
% DISAPPEARANCE_RATIONALE: If the degrowth reading disappeared from the policy conversation overnight, the mitigation_priority and adaptation_priority readings would absorb the discursive space, and no near-term material policy would change (no government has adopted planned contraction as stated policy), so in that narrow sense the world is largely unchanged. But the reading's proponents argue this apparent non-effect is exactly the harm: its absence from serious consideration is what keeps growth-framework mitigation as the only legitimated option, which they hold to be physically inadequate — so whether the world 'rearranges' depends on whether you credit the reading's own causal claim about the growth framework's adequacy, which is precisely what is contested.
% FOUNDING_PROBLEM: Standard emissions-reduction pathways assume continued GDP growth in high-income economies decoupled from throughput and emissions at a rate consistent with 1.5-2C budgets; the degrowth reading was built on the claim that no empirically observed decoupling has occurred at anywhere near the required rate, making growth-consistent mitigation a physically incoherent premise rather than a merely difficult one.
% FOUNDING_PROBLEM_CORROBORATION: Some ecological economists and ex-IPCC contributors outside the core degrowth advocacy network attest that absolute decoupling at required rates has not been empirically observed in any high-income economy to date, corroborating the founding problem's empirical premise. Mainstream economic and mitigation-priority institutions (IEA, most national climate agencies) dispute that this establishes physical impossibility rather than a technology-and-policy-pace problem, and treat the founding problem as substantially addressable within growth, making its status genuinely contested rather than settled in either direction.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, contested).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58 at interval end) and rising: the reading requires an increasingly large transfer of consumption capacity and employment security from Global North payers as the emissions-budget argument sharpens with each IPCC cycle. Suppression sits at 0.62 because the reading's political viability depends on overcoming institutional structures (growth-linked fiscal and monetary policy, capital mobility) that are actively organized against contraction — this is not merely persuasion-resistant, it is structurally excluded from most policy venues. Theater ratio (0.4) reflects that a meaningful share of 'degrowth-adjacent' policy activity (green growth rebranding, voluntary corporate net-zero pledges) performs adjacency to the reading's diagnosis while not implementing planned contraction, which the reading's own proponents flag as co-optation. Accessibility collapse is comparatively low (0.35): unlike a mountain, real policy alternatives (mitigation_priority, adaptation_priority) remain fully available and are in fact institutionally dominant, so alternatives have not collapsed — the reading struggles for adoption, not for the absence of competing options. Resistance is high (0.85), consistent with the exclusion of the reading from institutions with implementation power.
 *
 * PERSPECTIVAL GAP:
 *   From the degrowth coalition's own seat, this is coordination: a physically necessary reallocation to prevent catastrophic harm. From the Global North working/middle-class payer seat, the same structure presents as an imposed cost whose scale and timeline were not offered for their consent and whose benefits accrue mostly to people outside their polity or not yet born. The engine should compute these as different seat-level types from the same structural data — that divergence is the point of authoring beneficiary/victim/excluded roles explicitly rather than adjudicating a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South communities and future generations are declared beneficiaries because the reading's entire logic is that contraction preserves emissions budget and ecological stability for them — they sit near the full-beneficiary end of directionality despite having essentially no power to enact the constraint themselves; the derivation should reflect benefit-without-agency rather than benefit-with-control. Global North working/middle classes and carbon-intensive-industry workers are victims because planned contraction as typically specified falls on their consumption and employment, producing high derived directionality toward the target end, amplified by trapped/constrained exit options. Global North states and growth-dependent capital owners are marked excluded rather than beneficiary or payer because the reading structurally cannot get a hearing inside their institutional logic — their absence from serious operationalization is itself part of what the reading calls the founding problem.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is deliberate: this reading genuinely solves a coordination problem — it proposes a coherent, budget-consistent allocation of a finite atmospheric resource across historically unequal contributors — while simultaneously requiring an enforced, asymmetric transfer from a specific present-day population (Global North workers) to non-present or non-domestic beneficiaries. Classifying it as pure snare would erase the real coordination logic (finite carbon budget, differentiated historical responsibility); classifying it as rope would erase the real, asymmetric cost falling on populations who did not individually author the historical emissions and who have limited exit. The mandatrophy question is whether the reading's founding problem (growth-consistent decoupling not observed at required rates) remains live — it is authored as contested, not resolved, precisely because empirical decoupling rates are still disputed data, not settled fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_empirical_ambiguity,
    'Has any high-income economy achieved absolute decoupling of GDP growth from material throughput and emissions at a rate consistent with 1.5-2C budgets, or is decoupling at required rates empirically unobserved?',
    'Longitudinal cross-national data on GDP, material footprint, and territorial-plus-consumption emissions over multi-decade windows, adjudicated against required IPCC-consistent reduction rates.',
    'If sufficient decoupling is empirically demonstrated somewhere at required rates, the degrowth reading''s founding problem weakens substantially in favor of mitigation_priority; if it remains unobserved after further data, the degrowth reading''s core premise strengthens and its exclusion from policy venues becomes harder to justify on empirical grounds alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_empirical_ambiguity, empirical, 'Whether growth-consistent decoupling at required climate rates is physically achievable or empirically refuted.').

omega_variable(
    kernel_reading_selection,
    'This constraint is one of three declared readings (degrowth_reading, mitigation_priority, adaptation_priority) of the shared climate_harm_prevention kernel. Which reading a given policy actor adopts is itself a contested, non-arbitrary choice — what determines which reading a given institution treats as legitimate?',
    'Trace institutional adoption patterns: which reading dominates UNFCCC text, national climate law, and multilateral finance conditionality, and what structural features (growth-legitimacy dependence, capital mobility exposure, historical-emissions accounting norms) predict adoption.',
    'If institutional adoption tracks growth-dependence of the adopting institution rather than the empirical merits of each reading''s founding claim, that supports treating mitigation_priority''s institutional dominance as partly a product of institutional self-preservation rather than superior physical grounding — strengthening the degrowth reading''s claim that its exclusion is structural rather than evidentiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'What determines which kernel reading a given institution adopts as legitimate, and whether that determination is evidentiary or structural.').

omega_variable(
    contraction_burden_distribution_within_global_north,
    'Within Global North populations, is planned contraction''s cost distributed toward carbon-intensive-sector workers and general consumers (who bear disproportionate adjustment cost) or toward capital owners and high-consumption elites (whose share of emissions and wealth is also disproportionate)?',
    'Compare degrowth policy proposals (wealth caps, luxury consumption taxes, universal basic services, sectoral just-transition funds) against actually enacted contraction-adjacent policy to see which burden distribution has in practice been implemented.',
    'If actual implementation concentrates cost on workers rather than high-consumption capital owners, the victim declaration in this story (working/middle classes, industry workers) is validated as the operative victim set even under the reading''s own stated aims; if wealth-targeted mechanisms dominate in practice, the victim set should shift toward capital owners and the current authoring would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_burden_distribution_within_global_north, empirical, 'Whether planned contraction as actually implemented falls on labor or on capital within the Global North.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__degrowth_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(clim_tr_t6, climate_harm_prevention__degrowth_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(clim_tr_t12, climate_harm_prevention__degrowth_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(clim_tr_t18, climate_harm_prevention__degrowth_reading, theater_ratio, 18, 0.37).
narrative_ontology:measurement(clim_tr_t24, climate_harm_prevention__degrowth_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__degrowth_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__degrowth_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t6, climate_harm_prevention__degrowth_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(clim_be_t12, climate_harm_prevention__degrowth_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(clim_be_t18, climate_harm_prevention__degrowth_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(clim_be_t24, climate_harm_prevention__degrowth_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__degrowth_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__degrowth_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t6, climate_harm_prevention__degrowth_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(clim_su_t12, climate_harm_prevention__degrowth_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(clim_su_t18, climate_harm_prevention__degrowth_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(clim_su_t24, climate_harm_prevention__degrowth_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__degrowth_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_harm_prevention kernel. mitigation_priority claims emissions reduction succeeds within a growth framework via technological transition — this degrowth reading forecloses that premise directly (its core claim is that growth-consistent mitigation is physically impossible, which mitigation_priority's core claim denies; both cannot be simultaneously true within one framework, though different institutional actors hold each). adaptation_priority accepts a higher warming trajectory and prioritizes resilience; this reading influences but does not foreclose it, since adaptation_priority is compatible with either mitigation or degrowth as the accompanying emissions strategy and mainly shifts resource priority downstream. epsilon differs substantially across the three: this reading's extraction (0.58, rising) falls on a domestically-bounded present population for the benefit of a globally-distributed and temporally-future population, structurally distinct from mitigation_priority's extraction (borne more by growth-sector capital reallocation and technology-transition costs spread across consumers) and adaptation_priority's extraction (borne by populations left exposed to higher realized climate damages under an accepted higher-warming trajectory).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
