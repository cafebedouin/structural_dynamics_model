% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Climate Response as Mandated Global North Degrowth and Redistribution
 *   domain: climate policy/political economy/intergenerational justice
 *
 * SUMMARY:
 *   This story instantiates the degrowth reading of the contested
 *   climate-response-imperative kernel: the claim that avoiding catastrophic
 *   climate outcomes requires the Global North to undertake structural
 *   economic transformation — reduced material consumption, redistribution of
 *   wealth and working time, and post-growth institutional design — as a
 *   precondition for both mitigation and adaptation, rather than relying on
 *   technological substitution (the mitigation-priority reading) or treating
 *   resilience-building as primary (the adaptation-priority reading). Under
 *   this reading, present-day Global North working and middle classes, and
 *   especially carbon-intensive industry workers, enter the victim set
 *   directly: they bear the consumption reduction, working-time
 *   restructuring, and asset redistribution the transformation requires.
 *   Future generations and Global South populations are the beneficiaries,
 *   receiving a preserved carbon budget, redistributed adaptation finance,
 *   and reduced physical exposure. This reading explicitly rejects reliance
 *   on unproven large-scale carbon dioxide removal (CDR) as a substitute for
 *   consumption reduction, which is the key axis distinguishing it from the
 *   mitigation-priority reading's technology-and-market framing.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiary (powerless/trapped, civilizational horizon) — inherits climate outcome but has no present voice
 *   - global_south_populations: Beneficiary (powerless/trapped, global scope) — bears physical exposure, receives redistributed resources under this reading
 *   - global_north_working_and_middle_classes: Primary payer (moderate/constrained) — bears consumption reduction and redistribution
 *   - carbon_intensive_industry_workers: Acute payer (powerless/trapped, regional) — bears direct livelihood loss
 *   - post_growth_policy_coalition: Agenda-setter (organized/mobile) — authors and advocates the prescription without itself bearing its costs
 *   - fossil_fuel_and_growth_lobby: Excluded but institutionally powerful — resists the transition via arbitrage-grade exit
 *   - climate_scientists_and_ipcc_bodies: Analytical observer — evidence base shared across all three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.62).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Climate Response as Mandated Global North Degrowth and Redistribution").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate policy/political economy/intergenerational justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, '84e36f14-db90-4591-ad54-092c58bccaf1').
narrative_ontology:cs_kernel_codification('84e36f14-db90-4591-ad54-092c58bccaf1', distributed).
narrative_ontology:cs_authority_grounding('84e36f14-db90-4591-ad54-092c58bccaf1', distributed).
narrative_ontology:cs_reading_relation('84e36f14-db90-4591-ad54-092c58bccaf1', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('84e36f14-db90-4591-ad54-092c58bccaf1', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_axiom('84e36f14-db90-4591-ad54-092c58bccaf1', foundational, consumption_reduction_is_necessary_not_optional).
narrative_ontology:cs_axiom_status(consumption_reduction_is_necessary_not_optional, holdable).
narrative_ontology:cs_axiom_grounding('84e36f14-db90-4591-ad54-092c58bccaf1', consumption_reduction_is_necessary_not_optional, empirically_contingent).
narrative_ontology:cs_axiom('84e36f14-db90-4591-ad54-092c58bccaf1', foundational, technological_substitution_alone_is_insufficient).
narrative_ontology:cs_axiom_status(technological_substitution_alone_is_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('84e36f14-db90-4591-ad54-092c58bccaf1', technological_substitution_alone_is_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('84e36f14-db90-4591-ad54-092c58bccaf1', secondary, intergenerational_and_north_south_redistribution_is_a_precondition_of_justice).
narrative_ontology:cs_axiom_status(intergenerational_and_north_south_redistribution_is_a_precondition_of_justice, holdable).
narrative_ontology:cs_axiom_grounding('84e36f14-db90-4591-ad54-092c58bccaf1', intergenerational_and_north_south_redistribution_is_a_precondition_of_justice, deontological).
narrative_ontology:cs_reference_frame('84e36f14-db90-4591-ad54-092c58bccaf1', growth_oriented_industrial_baseline).
narrative_ontology:cs_drift_state('84e36f14-db90-4591-ad54-092c58bccaf1', post_paris_agreement_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('84e36f14-db90-4591-ad54-092c58bccaf1', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, ecological_systems).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_working_and_middle_classes).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, carbon_intensive_industry_workers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_consumer_economy_incumbents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, fossil_fuel_and_growth_lobby).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have no vote, no market position, and no capacity to bargain in present-day decisions, yet inherit whatever climate stability or instability the current transformation choice produces. Under this reading, they are the primary constituency the degrowth transition is undertaken for, though they cannot currently ratify or contest the arrangement.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Bear disproportionate physical exposure to climate impacts they did not primarily cause. Under this reading, Global North consumption reduction and redistribution are read as the mechanism that both slows the physical hazard they face and transfers resources and fiscal space toward their adaptation. They have limited direct enforcement power over Global North policy and depend on international negotiation and solidarity movements to advance the claim.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Would experience reduced consumption levels, altered working-time norms, and redistribution of income and assets under a post-growth transition. They have some democratic voice through elections and labor organizing but cannot easily exit the national economic system that would be restructured around them; relocation to avoid the transition is not a realistic option for most.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_working_and_middle_classes, payer,
    moderate, biographical, constrained, national).

% Employed in fossil-fuel extraction, heavy manufacturing, and adjacent supply chains concentrated in specific regions. A structural transformation away from growth-oriented, carbon-intensive production directly threatens their livelihoods and community economic bases, and retraining or relocation is often unavailable or inadequate. They are the most acutely exposed payers in the near term.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, carbon_intensive_industry_workers, payer,
    powerless, biographical, trapped, regional).

% Firms, shareholders, and sectors whose business models depend on continuous consumption growth (retail, advertising, consumer finance, aviation, fast fashion) would see demand and valuations structurally reduced. They have organized lobbying capacity to resist the transition and are largely excluded from having authored its terms, though they are not powerless.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_consumer_economy_incumbents, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__degrowth_reading, global_north_consumer_economy_incumbents, excluded).

% Academics, NGOs, some political parties, and international bodies advocating for degrowth/post-growth frameworks. They set the intellectual and policy agenda for this reading, publish the models and proposals, and lobby for structural reforms (four-day week, wealth caps, universal basic services) but do not themselves bear the consumption reduction or hold enforcement power to implement it unilaterally.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, post_growth_policy_coalition, agenda_setter,
    organized, generational, mobile, national).

% Would lose the most under a genuinely enforced degrowth transition and is structurally positioned to resist it via capital mobility, political donation, and control over energy infrastructure. Excluded from the degrowth coalition's own framing as a legitimate voice, though they retain outsized structural power and can arbitrage across jurisdictions to avoid the transition's reach.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, fossil_fuel_and_growth_lobby, excluded,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__degrowth_reading, fossil_fuel_and_growth_lobby, beneficiary).

% Produce the physical and carbon-budget evidence used by all three readings of the kernel to justify their prescriptions. They do not adjudicate between mitigation-priority, adaptation-priority, and degrowth framings, and their emissions pathways are compatible with more than one of these institutional readings.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_scientists_and_ipcc_bodies, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a rapid, economy-wide reduction in Global North material and energy throughput so that the remaining global carbon budget is not consumed by continued Global North growth, while freeing fiscal and physical resources for redistribution toward adaptation and mitigation capacity elsewhere.
% TRANSFER_FUNCTION: Moves consumption capacity, working time, and accumulated wealth from Global North households, workers, and consumption-dependent firms toward global carbon-budget headroom, Global South adaptation finance, and future generations' inherited climate stability.
% ABSENT_VOICES: Global North working and middle classes and carbon-intensive industry workers are named as payers but are largely absent from the coalition that authored the degrowth prescription; future generations and Global South communities are named beneficiaries but have no present enforcement voice either. The fossil fuel and growth lobby is excluded from legitimacy in the coalition's own framing despite holding real structural power to resist.
% DISAPPEARANCE_RATIONALE: If the degrowth prescription vanished as a policy program, mitigation-priority and adaptation-priority readings would continue to operate and claim the same physical evidence base — the parties dispute whether the world would meaningfully rearrange (degrowth advocates say catastrophic overshoot becomes far more likely; mitigation-priority advocates say technological substitution proceeds regardless) or stay on roughly the same trajectory (since no jurisdiction has yet implemented degrowth policy at scale, its removal from discourse may be closer to status quo than disappearance of an operating arrangement).
% FOUNDING_PROBLEM: Post-WWII industrial growth in the Global North, powered overwhelmingly by fossil fuels, produced the majority of cumulative historical emissions now driving climate destabilization, while continued growth-oriented consumption patterns are read as structurally incompatible with remaining within a safe carbon budget.
% FOUNDING_PROBLEM_CORROBORATION: IPCC working group reports and independent carbon-budget accounting (outside the degrowth policy coalition itself) corroborate that cumulative historical Global North emissions dominate the remaining budget and that continued growth in material throughput is difficult to reconcile with 1.5-2C pathways without significant efficiency or consumption change; however, these same bodies do not corroborate degrowth specifically as the necessary mechanism — mitigation-priority readings cite the same IPCC evidence for a technology-substitution pathway instead.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, contested).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 and rising over the interval because, under this reading, the transformation's costs increasingly fall on a fairly specific and identifiable set of present-day Global North payers (workers, consumer-dependent firms) while its benefits accrue to parties (future generations, Global South) who cannot presently contest or ratify the arrangement — a structurally asymmetric transfer even though a genuine coordination problem (finite remaining carbon budget) underlies it. Suppression is authored as substantial and rising (0.62 by 2024) because implementing degrowth at the pace and scale the reading requires would demand active state enforcement against a well-organized growth-dependent economic base — carbon taxes, consumption caps, and wealth redistribution do not self-execute against resistance. Theater ratio starts high (0.7) and falls only moderately (0.55) because most enacted 'green growth' and voluntary corporate sustainability measures to date are, from this reading's perspective, substitutes for the actual structural transformation rather than instances of it — a persistent gap between rhetorical climate commitment and the redistributive restructuring this reading claims is necessary. Accessibility collapse is moderate (0.4): alternative pathways (technological substitution, adaptation-only) remain live and contested, unlike a natural law where alternatives genuinely vanish. Resistance is high (0.78) reflecting the organized political and economic opposition the transformation would face from consumption-dependent industry and affected workers.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and Global South populations are declared beneficiaries and sit near the full-beneficiary end of directionality despite bearing zero authorial or enforcement power — their benefit is structural rather than agentive, a feature the derivation chain must be read carefully against (they benefit from the arrangement's operation, not from any capacity they exercise). Global North working/middle classes and carbon-intensive industry workers are declared victims/payers and sit near the target end, amplified further by their constrained-to-trapped exit options: a national worker cannot simply relocate out of a jurisdiction restructuring its whole economy. The post-growth policy coalition, despite authoring the prescription, is NOT itself a payer in the primary sense — it is analytically and often materially insulated from the consumption reduction it advocates, which is a structural feature of advocacy-versus-implementation gap common to transformative political programs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cumulative Global North historical emissions and finite remaining carbon budget) is corroborated as live by evidence external to the degrowth coalition itself (IPCC accounting), which prevents this reading from being dismissed as a self-serving academic or activist construct. However, the SPECIFIC mechanism this reading prescribes — degrowth as necessary, not merely sufficient — is not independently corroborated by the same evidence base; the IPCC bodies remain agnostic between the mitigation-priority and degrowth mechanisms. This is precisely the divergence the classification exists to surface: the underlying coordination problem is real (Mountain-adjacent: the physical carbon budget is close to a natural constraint), but the specific institutional prescription bundled with it carries a redistributive transfer function that is contestable and enforced, not natural — hence tangled_rope rather than mountain or pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_vs_sufficiency_of_degrowth,
    'Is structural degrowth genuinely NECESSARY for staying within the remaining carbon budget, or is it one SUFFICIENT pathway among several (including the mitigation-priority reading''s technology-substitution route)?',
    'Comparative integrated assessment modeling under multiple pathway assumptions (high-CDR-reliance vs. no-CDR-reliance vs. degrowth) evaluated against updated carbon budget estimates and observed decoupling rates between GDP and emissions in Global North economies.',
    'If degrowth is not strictly necessary — if sufficient decoupling is achievable — the redistributive transfer this reading imposes on present Global North payers loses its coordination justification and the classification shifts toward snare (extraction dressed as necessity); if genuinely necessary, the tangled_rope classification''s coordination component is well-grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_vs_sufficiency_of_degrowth, empirical, 'Whether degrowth is a necessary or merely sufficient mechanism relative to sibling readings.').

omega_variable(
    beneficiary_agency_asymmetry,
    'Given that future generations and Global South populations cannot currently ratify, contest, or renegotiate the terms of this transformation, is their status as ''beneficiary'' meaningfully different from a proxy claim made on their behalf by the present-day policy coalition?',
    'Examine whether Global South governments and civil society organizations, when consulted directly (as opposed to represented by Global North degrowth advocates), endorse the specific redistributive mechanism proposed, versus alternative demands (e.g., unconditional climate finance, technology transfer without consumption mandates).',
    'If Global South voices substantially diverge from the degrowth coalition''s framing of their interests, the beneficiary declaration is partly a representational construct rather than a directly corroborated structural fact, which would attenuate the coordination claim without eliminating it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_agency_asymmetry, conceptual, 'Whether declared beneficiaries without present agency are adequately represented by the reading''s own coalition.').

omega_variable(
    reading_boundary_disagreement_location,
    'Where exactly does the degrowth reading''s premise diverge from the mitigation-priority reading''s premise — is it a disagreement about physical feasibility of technological substitution, or a disagreement about the moral/political acceptability of continued growth even if technically feasible?',
    'Decompose degrowth advocacy literature and mitigation-priority technical literature to identify whether cited disagreements are primarily about decoupling feasibility (empirical) or about growth''s intrinsic desirability (normative/preference).',
    'If the disagreement is primarily empirical (decoupling feasibility), the two readings could in principle converge on new evidence, weakening the case that they are structurally distinct constraints rather than a single contested empirical claim; if primarily normative, the ε-invariance decomposition into separate constraint stories is clearly warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_disagreement_location, conceptual, 'Locating whether the mitigation-priority/degrowth split is empirical or normative in nature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_response_imperative__degrowth_reading, theater_ratio, 1990, 0.7).
narrative_ontology:measurement(clim_tr_t1997, climate_response_imperative__degrowth_reading, theater_ratio, 1997, 0.68).
narrative_ontology:measurement(clim_tr_t2005, climate_response_imperative__degrowth_reading, theater_ratio, 2005, 0.65).
narrative_ontology:measurement(clim_tr_t2012, climate_response_imperative__degrowth_reading, theater_ratio, 2012, 0.6).
narrative_ontology:measurement(clim_tr_t2018, climate_response_imperative__degrowth_reading, theater_ratio, 2018, 0.58).
narrative_ontology:measurement(clim_tr_t2024, climate_response_imperative__degrowth_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_response_imperative__degrowth_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(clim_be_t1997, climate_response_imperative__degrowth_reading, base_extractiveness, 1997, 0.42).
narrative_ontology:measurement(clim_be_t2005, climate_response_imperative__degrowth_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(clim_be_t2012, climate_response_imperative__degrowth_reading, base_extractiveness, 2012, 0.55).
narrative_ontology:measurement(clim_be_t2018, climate_response_imperative__degrowth_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(clim_be_t2024, climate_response_imperative__degrowth_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_response_imperative__degrowth_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(clim_su_t1997, climate_response_imperative__degrowth_reading, suppression_requirement, 1997, 0.35).
narrative_ontology:measurement(clim_su_t2005, climate_response_imperative__degrowth_reading, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement(clim_su_t2012, climate_response_imperative__degrowth_reading, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement(clim_su_t2018, climate_response_imperative__degrowth_reading, suppression_requirement, 2018, 0.57).
narrative_ontology:measurement(clim_su_t2024, climate_response_imperative__degrowth_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__degrowth_reading, 0.12).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the climate_response_imperative kernel. mitigation_priority_reading shares the same physical evidence base but locates the coordination mechanism in technological substitution and carbon markets rather than consumption reduction, producing a different (lower-suppression, more diffuse-beneficiary) structural profile. adaptation_priority_reading treats resilience-building in exposed regions as primary and mitigation as aspirational, producing yet another victim/beneficiary structure (adaptation-finance recipients as direct beneficiaries rather than future generations globally). All three share the underlying carbon-budget physical constraint but diverge on mechanism, victim set, and reliance on unproven technology — per the ε-invariance principle, each is authored as a separate constraint story with its own ε rather than as one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
