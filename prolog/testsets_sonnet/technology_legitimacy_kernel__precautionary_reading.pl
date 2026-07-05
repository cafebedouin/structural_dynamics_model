% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Precautionary Reversibility Standard for Climate-Mitigation Technology Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel governing what
 *   counts as a 'legitimate' climate-mitigation technology. The precautionary
 *   reading holds that legitimacy requires bounded, generationally-reversible
 *   worst-case failure modes and legacy costs. Under this criterion,
 *   renewables (rapidly decommissionable, no multi-millennial waste stream)
 *   qualify as legitimate; nuclear (long-lived waste, low-probability
 *   high-consequence accident tail) does not, regardless of its low
 *   routine-operation carbon intensity or dispatchable baseload value. The
 *   reading has become operationally load-bearing: it shapes green finance
 *   taxonomies (notably contested EU and multilateral development bank
 *   sustainable-finance criteria), investment screens, and NGO advocacy
 *   positions. Two sibling readings of the same kernel —
 *   reliability_primacy_reading (legitimacy = dispatchable baseload
 *   capability) and velocity_primacy_reading (legitimacy = deployability
 *   within the remaining carbon budget) — are NOT part of this constraint;
 *   they are separate stories with their own ε, beneficiary/victim sets, and
 *   stakeholders, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - renewables_industry: primary beneficiary (organized/mobile) — gains taxonomy and finance advantage
 *   - precaution_aligned_environmental_ngos: agenda_setter (organized/constrained) — authors and administers the standard
 *   - nuclear_industry_workers and nuclear_dependent_grid_regions: primary targets (moderate/constrained-trapped) — bear exclusion costs
 *   - fossil_incumbents_via_delayed_replacement: dual position (powerful/arbitrage) — incidentally advantaged by nuclear's exclusion while structurally opposed to mitigation generally
 *   - future_generations_under_this_reading: the reading's stated normative beneficiary, non-agent, represented only by proxy
 *   - reliability_primacy_advocates: excluded — hold a sibling reading, absent from this reading's drafting process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.58).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.52).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Precautionary Reversibility Standard for Climate-Mitigation Technology Legitimacy").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, '2f8d9104-f9b7-46e0-aa34-01682c532c0f').
narrative_ontology:cs_kernel_codification('2f8d9104-f9b7-46e0-aa34-01682c532c0f', distributed).
narrative_ontology:cs_authority_grounding('2f8d9104-f9b7-46e0-aa34-01682c532c0f', distributed).
narrative_ontology:cs_reading_relation('2f8d9104-f9b7-46e0-aa34-01682c532c0f', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f8d9104-f9b7-46e0-aa34-01682c532c0f', technology_legitimacy_kernel__velocity_primacy_reading, influences).
narrative_ontology:cs_axiom('2f8d9104-f9b7-46e0-aa34-01682c532c0f', foundational, irreversible_legacy_cost_disqualifies_legitimacy).
narrative_ontology:cs_axiom_status(irreversible_legacy_cost_disqualifies_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2f8d9104-f9b7-46e0-aa34-01682c532c0f', irreversible_legacy_cost_disqualifies_legitimacy, deontological).
narrative_ontology:cs_axiom('2f8d9104-f9b7-46e0-aa34-01682c532c0f', secondary, generational_horizon_bounds_acceptable_risk_transfer).
narrative_ontology:cs_axiom_status(generational_horizon_bounds_acceptable_risk_transfer, holdable).
narrative_ontology:cs_axiom_grounding('2f8d9104-f9b7-46e0-aa34-01682c532c0f', generational_horizon_bounds_acceptable_risk_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('2f8d9104-f9b7-46e0-aa34-01682c532c0f', post_fukushima_precautionary_consensus).
narrative_ontology:cs_drift_state('2f8d9104-f9b7-46e0-aa34-01682c532c0f', contemporary_energy_security_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2f8d9104-f9b7-46e0-aa34-01682c532c0f', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewables_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, storage_and_grid_flexibility_vendors).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, precaution_aligned_environmental_ngos).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, future_generations_under_this_reading).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_workers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_dependent_grid_regions).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, fossil_incumbents_via_delayed_replacement).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, communities_facing_prolonged_fossil_exposure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, fossil_incumbents_via_delayed_replacement).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, reversibility_as_legitimacy_criterion).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, generational_bounding_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar, wind, and storage developers benefit directly from a legitimacy test keyed to decommissioning reversibility: panels and turbines can be dismantled and sites restored within a human lifetime, so this reading admits them as legitimate mitigation technology by construction. They actively fund advocacy for the reversibility framing in policy and finance-taxonomy debates.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewables_industry, beneficiary,
    organized, generational, mobile, global).

% Battery and demand-response firms gain market share as the reversibility standard excludes baseload nuclear and locks in a grid architecture built around variable renewables plus flexibility assets. Their commercial position is strengthened by this reading regardless of its truth.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, storage_and_grid_flexibility_vendors, beneficiary,
    organized, generational, mobile, global).

% Advocacy organizations authored and promote the reversibility-within-a-generation test in green finance taxonomies, investment screens, and multilateral climate finance criteria. They administer the standard's application to specific technologies (notably excluding nuclear) and defend it against reliability- and velocity-primacy challengers.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, precaution_aligned_environmental_ngos, agenda_setter,
    organized, civilizational, constrained, global).

% Engineers, operators, and construction workers in nuclear supply chains bear career and investment risk when this standard excludes nuclear from green taxonomies and mitigation finance eligibility, regardless of the technology's actual carbon performance. Their exit is retraining into other energy sectors, which is costly and slow.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_workers, payer,
    moderate, biographical, constrained, national).

% Regions whose grids rely on existing nuclear baseload face financing and political pressure to retire plants early or forgo new builds because the plants fail this legitimacy test, even where no lower-carbon dispatchable replacement exists at comparable scale. They cannot relocate their grid infrastructure.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_dependent_grid_regions, payer,
    moderate, biographical, trapped, regional).

% Where nuclear exclusion slows dispatchable low-carbon buildout, gas and coal incumbents retain market share longer than they otherwise would as backup capacity. They are structurally both harmed by the broader mitigation push and incidentally advantaged by this specific reading's nuclear exclusion — they have the resources to exploit whichever framing serves them.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, fossil_incumbents_via_delayed_replacement, payer,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, fossil_incumbents_via_delayed_replacement, beneficiary).

% Populations near fossil plants that remain online longer because nuclear replacement was foreclosed by this legitimacy test bear continued air pollution and localized climate burden. They did not participate in setting the reversibility criterion and have no standing in the taxonomy debates that determine it.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, communities_facing_prolonged_fossil_exposure, payer,
    powerless, biographical, trapped, regional).

% The reading's normative center of gravity: people not yet born are protected from irreversible legacy costs (nuclear waste requiring multi-millennial stewardship, low-probability high-consequence accidents) by excluding technologies whose worst case cannot be bounded within roughly one generation. They cannot advocate for themselves; the standard exists to represent their interests by proxy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations_under_this_reading, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(technology_legitimacy_kernel__precautionary_reading, future_generations_under_this_reading).

% Grid engineers, utility operators, and nuclear advocates who hold that dispatchable baseload capability is the legitimacy criterion are present in adjacent policy debates but are not the authors of this reading's taxonomy language; their framework is a sibling reading of the same kernel, structurally excluded from this reading's own criteria.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, reliability_primacy_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__precautionary_reading, diffuse).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__precautionary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides investors, regulators, and multilateral climate finance bodies a single bright-line screen for excluding technologies whose failure modes could impose costs on people who never consented to the risk — solving the genuine problem of evaluating mitigation technologies under deep uncertainty about tail outcomes.
% TRANSFER_FUNCTION: Moves capital, policy priority, and legitimacy standing away from technologies with long-tailed legacy costs (principally nuclear) and toward technologies with rapidly reversible footprints (principally renewables plus storage), regardless of each technology's near-term carbon-displacement performance.
% ABSENT_VOICES: Reliability-primacy advocates and communities in nuclear-dependent or fossil-exposed regions are structurally outside the taxonomy-drafting rooms where this reading's criteria are set; future generations, the reading's stated beneficiary, have no seat and are represented only by proxy advocacy.
% DISAPPEARANCE_RATIONALE: If the reversibility-within-a-generation test vanished from green taxonomies and finance criteria overnight, nuclear projects would re-enter mitigation-eligible financing pools, renewables' relative taxonomy advantage would narrow, and capital allocation across the sector would shift measurably within a single financing cycle — the standard is doing real allocative work, not merely describing a settled fact.
% FOUNDING_PROBLEM: Climate finance and technology policy needed a way to admit or exclude candidate mitigation technologies under conditions where the relevant failure modes (nuclear accidents, long-lived waste, but also, symmetrically, climate tipping points from delay) are low-probability, high-consequence, and not resolvable by ordinary cost-benefit analysis within normal political time horizons.
% FOUNDING_PROBLEM_CORROBORATION: Independent risk-analysis literature (e.g., comparative mortality and land-use studies) corroborates that nuclear's worst-case tail is real but statistically much smaller than fossil-fuel status-quo harms, which cuts against the precautionary reading's exclusion; multilateral development bank staff and grid engineers outside the environmental-NGO coalition attest that the reversibility criterion, as applied, forecloses a dispatchable low-carbon option without a like-for-like replacement, suggesting the standard's practical effect diverges from its stated protective aim.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects that the standard transfers real capital and policy standing away from a technology class (nuclear) whose measured harm profile is contested rather than settled — the exclusion functions as extraction from nuclear-adjacent workers and regions even where the precautionary logic is sound in the abstract. Suppression (0.52) is moderate: the standard operates mainly through finance-taxonomy exclusion and reputational framing rather than direct coercion, but it does foreclose financing pathways for excluded technologies, which is a real barrier, not mere persuasion. Theater ratio (0.28) is kept low-moderate because the reversibility criterion does correspond to a genuine, non-trivial technical distinction (decommissioning timelines differ by orders of magnitude between solar/wind and nuclear); this is not pure performance. Accessibility collapse (0.40) is moderate — the reading has NOT fully foreclosed the debate; reliability- and velocity-primacy framings remain live and contested in policy venues, so alternatives have not collapsed. Resistance (0.70) is high because nuclear advocates, some grid engineers, and increasingly some climate scientists actively contest the reading's practical effect of delaying dispatchable decarbonization.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewables and storage vendors sit near the beneficiary end: the standard was substantially shaped with their technology profile as the template case, and they gain taxonomy and financing advantage with no offsetting cost imposed on them by the criterion. Nuclear workers and nuclear-dependent regions sit near the target end: they bear career, investment, and grid-continuity costs from an exclusion whose empirical basis (comparative risk profile) is contested outside the coalition that set the standard. Fossil incumbents are structurally ambiguous — harmed by the overall mitigation push but incidentally advantaged by this specific reading's effect of slowing nuclear's re-entry into competition for baseload replacement; this is captured with a dual role rather than a directionality override, since the derivation from beneficiary/victim data alone would not resolve the ambiguity cleanly. Future generations are declared as beneficiary but flagged as non-agent (agent: false) because they cannot participate in the constraint's operation or exert exit pressure; their inclusion is the reading's normative justification, not a party with directionality-bearing power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — evaluating mitigation technologies under deep tail-risk uncertainty — remains partially live: genuine uncertainty about very-long-horizon nuclear waste stewardship and accident tails has not been resolved. But the founding_problem_status is authored as contested rather than dead or live because independent risk analysis (outside the NGO coalition that authored the standard) suggests the practical operation of the standard has drifted from protecting future generations toward foreclosing a specific, empirically comparatively low-risk dispatchable technology at a moment when delaying its deployment plausibly increases near-term fossil exposure — a different and less flattering function than the founding narrative claims. This is exactly the kind of mismatch (a coordination story riding on a partly-displaced function) that the tangled_rope classification and the R5 corroboration requirement are built to surface, rather than accepting the precautionary framing's self-description at face value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_definition_ambiguity,
    'Is ''reversible within a generation'' a well-defined technical threshold, or does its vagueness allow the standard''s administrators discretion to include or exclude technologies based on prior commitments rather than principled risk assessment?',
    'Compare how the standard has been applied across specific cases (e.g., small modular reactors with shorter waste half-lives vs. legacy light-water reactors) — if application is consistent with a clear operational definition, the standard is principled; if application varies with which technology is being evaluated in ways not explained by the stated criterion, discretion is doing unacknowledged work.',
    'If the definition is genuinely operationalizable and consistently applied, this reading functions closer to a rope (real coordination around a real risk criterion). If discretion dominates, the coordination story is cover for a pre-existing preference for renewables, and the classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_definition_ambiguity, conceptual, 'Whether the generational-reversibility threshold is a principled criterion or a discretionary lever.').

omega_variable(
    sibling_reading_empirical_convergence,
    'Under realistic decarbonization pathways, do the precautionary, reliability-primacy, and velocity-primacy readings actually recommend different technology portfolios, or do they converge once transmission, storage, and demand-side factors are modeled jointly?',
    'Integrated assessment modeling comparing technology portfolios optimized separately under each reading''s legitimacy constraint against modeled 2050 outcomes; empirical divergence or convergence of the resulting portfolios.',
    'If the readings converge on similar portfolios despite different justificatory frameworks, the kernel contest is largely rhetorical rather than allocative. If they diverge substantially (as the exclusion of nuclear under this reading suggests), the choice of reading has first-order consequences for actual emissions trajectories and legacy risk distribution — the stakes of the committer contest are real, not merely a difference of frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_convergence, empirical, 'Whether choosing among kernel readings materially changes technology portfolios or is largely a framing dispute.').

omega_variable(
    future_generations_representation_validity,
    'Does proxy advocacy for future generations (via environmental NGOs and precautionary-framework advocates) accurately represent the interests of people who do not yet exist, or does it substitute the advocates'' present-day risk preferences for an unknowable future preference structure?',
    'No direct empirical resolution is possible (future generations cannot be surveyed); the question can only be triangulated via intergenerational ethics literature and historical track record of similar proxy-representation claims (e.g., past environmental precaution claims that were later validated or invalidated by outcomes).',
    'If proxy representation is judged legitimate, the beneficiary declaration for future_generations_under_this_reading is well-grounded and the standard''s protective claim holds real normative weight. If proxy representation is judged to substitute present advocates'' preferences, the standard''s stated justification is weaker than its practical effect of allocating financing away from nuclear.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_representation_validity, preference, 'Whether proxy representation of unborn future generations is a valid basis for excluding a present-day technology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(tech_tr_t24, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(tech_be_t24, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(tech_su_t24, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__velocity_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposed from the natural-language 'technology legitimacy for climate mitigation' concept, per the ε-invariance principle. Each reading is a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification: precautionary_reading (this story, tangled_rope, renewables-favoring, nuclear-excluding), reliability_primacy_reading (expected beneficiary set includes nuclear and other dispatchable baseload technologies), and velocity_primacy_reading (expected beneficiary set favors whichever technology deploys fastest at scale, which may or may not include nuclear depending on build-time assumptions). All three are linked bidirectionally via affects_constraints because policy actors invoke each reading strategically against the others in the same taxonomy and financing debates — a shift in which reading dominates a jurisdiction's green finance rules directly changes capital availability for the technologies excluded or included under the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
