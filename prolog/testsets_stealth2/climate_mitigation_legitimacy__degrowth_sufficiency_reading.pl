% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Sufficiency-First Decarbonization Gate (Degrowth Reading)
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   The degrowth/sufficiency reading of climate mitigation legitimacy holds
 *   that decarbonization runs through demand reduction, rendering large-scale
 *   generation expansion unnecessary. Operationally the claim acts as a
 *   legitimacy gate in climate policy: funding calls, advisory seats, and
 *   discourse standing are conditioned on sufficiency alignment, and
 *   generation-expansion proposals are coded as unnecessary before evaluation
 *   on merit. The arrangement has a genuine collective-action core — rebound
 *   effects and absolute throughput ceilings are empirically documented
 *   problems that unilateral restraint cannot solve — but its burdens land
 *   asymmetrically: on energy-poor populations whose demand is not yet met,
 *   on Global South industrializers whose development paths require demand
 *   growth, and, per this reading's characteristic structural signature, on
 *   BOTH nuclear and renewable developers as growth-dependent industries
 *   whose capital deployment the norm delegitimizes. Institutional gains
 *   accrue to the advocacy ecosystem that maintains the claim; fossil
 *   incumbents harvest delay opportunistically without designing it. This
 *   file instantiates ONLY the degrowth_sufficiency_reading of the kernel;
 *   the baseload-necessity, renewable-primacy, and portfolio-pragmatism
 *   readings are separate constraints in separate files, linked via
 *   network.affects_constraints. The claim/metric gap is deliberate: the
 *   reading CLAIMS its arrangement as necessary justice while the authored
 *   metrics describe its actual mixed operation — the engine measures the
 *   divergence. KEY AGENTS (by structural relationship): -
 *   degrowth_advocacy_networks: Primary beneficiary and agenda-setter
 *   (organized/identity_locked) — collects grants, chairs, and agenda power
 *   from the claim's salience - fossil_fuel_incumbents: Incidental
 *   beneficiary (powerful/arbitrage) — harvests delayed clean-buildout
 *   competition without designing the norm - nuclear_energy_developers:
 *   Primary target (institutional/trapped) — growth-dependent fleet expansion
 *   delegitimized - utility_scale_renewable_developers: Primary target
 *   (institutional/constrained) — climate-aligned buildout marked unnecessary
 *   - energy_poor_households: Target with least voice (powerless/trapped) —
 *   unmet demand caught by universal restraint prescriptions -
 *   global_south_industrializers: Organized resister (organized/constrained)
 *   — development pathways framed as the problem - general_energy_consumers:
 *   Excluded seat (moderate/trapped) — their demand is the object; never
 *   seated - climate_policy_analysts: Analytical observer
 *   (analytical/analytical) — sees the full scenario space
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Sufficiency-First Decarbonization Gate (Degrowth Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '34f6f747-1fc6-4522-a739-1388fd621ffe').
narrative_ontology:cs_kernel_codification('34f6f747-1fc6-4522-a739-1388fd621ffe', distributed).
narrative_ontology:cs_authority_grounding('34f6f747-1fc6-4522-a739-1388fd621ffe', distributed).
narrative_ontology:cs_reading_relation('34f6f747-1fc6-4522-a739-1388fd621ffe', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('34f6f747-1fc6-4522-a739-1388fd621ffe', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('34f6f747-1fc6-4522-a739-1388fd621ffe', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('34f6f747-1fc6-4522-a739-1388fd621ffe', foundational, aggregate_demand_reduction_necessary).
narrative_ontology:cs_axiom_status(aggregate_demand_reduction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('34f6f747-1fc6-4522-a739-1388fd621ffe', aggregate_demand_reduction_necessary, empirically_contingent).
narrative_ontology:cs_axiom('34f6f747-1fc6-4522-a739-1388fd621ffe', foundational, sufficiency_ethics_precede_supply_optimization).
narrative_ontology:cs_axiom_status(sufficiency_ethics_precede_supply_optimization, holdable).
narrative_ontology:cs_axiom_grounding('34f6f747-1fc6-4522-a739-1388fd621ffe', sufficiency_ethics_precede_supply_optimization, deontological).
narrative_ontology:cs_reference_frame('34f6f747-1fc6-4522-a739-1388fd621ffe', sufficiency_first_steady_state).
narrative_ontology:cs_drift_state('34f6f747-1fc6-4522-a739-1388fd621ffe', contemporary_expansion_consensus, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('34f6f747-1fc6-4522-a739-1388fd621ffe', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocacy_networks).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, utility_scale_renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_poor_households).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_south_industrializers).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, jevons_paradox_rebound_thesis).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, absolute_decoupling_skepticism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run research programs, campaigns, and advisory seats inside UN, EU, and municipal climate processes. Grants, professorships, conference circuits, and agenda invitations flow to them in proportion to the salience of the demand-reduction framing, and they police the framing's boundaries within movement spaces. Leaving the position would mean dissolving careers, networks, and a worldview built around the claim.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocacy_networks, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocacy_networks, agenda_setter).

% Sell fuels whose demand the norm ultimately seeks to shrink, yet every year in which restraint rhetoric slows clean-buildout permits, finance, and grid connections preserves their margin. They did not design the norm but fund adjacent delay advocacy and amplify scarcity arguments. Their gain is contingent: if the norm ever fully bound, their market would collapse, so they profit from its rhetorical life rather than its success.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).

% Finance and build reactors whose business case assumes a growing clean-fleet market. Under the norm their capital deployment is coded as unnecessary before it is evaluated on merit, raising financing costs and political risk. Sunk certification, workforce, and site commitments make any pivot away from fleet growth a write-off of decades of accumulated capability.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_energy_developers, payer,
    institutional, generational, trapped, global).

% Develop wind, solar, storage, and transmission whose pipelines assume expanding electricity demand. A climate-aligned norm marking buildout as unnecessary lands on them with particular force: their projects clear emissions tests yet fail the sufficiency test. They can shift crews between technologies but cannot abandon buildout without abandoning their business.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, utility_scale_renewable_developers, payer,
    institutional, biographical, constrained, global).

% Consume little energy and seek more of it: heating, cooling, cooking, mobility. Universal restraint prescriptions reach them through prices and rationing-by-framing before basic access is secured. They hold no seats in the fora where the norm is articulated and depend on intermediaries to voice objection.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_poor_households, payer,
    powerless, immediate, trapped, global).

% Pursue industrialization strategies that require rising energy demand. The norm frames their aspiration as the planetary problem and arrives attached to green-finance conditionality. They contest it collectively through negotiating blocs and equity doctrine but command less leverage than OECD treasuries and multilateral lenders.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_south_industrializers, payer,
    organized, generational, constrained, continental).

% Use energy services daily; their aggregate demand is the object the norm would reshape. They are described by advocates and defended by industry associations but are rarely seated in deliberation. Their consent is presumed rather than solicited, and they cannot exit energy service to escape the debate.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, general_energy_consumers, excluded,
    moderate, immediate, trapped, global).

% Model scenario spaces, compare pathway costs, and publish integrated assessments spanning all readings of the mitigation-legitimacy contest. They collect nothing from any reading and can move between frameworks; their reports are cited by every seat.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocacy_networks).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__degrowth_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates aggregate restraint that no actor can achieve unilaterally: individual households, firms, and states cannot cut consumption alone without losing competitiveness or welfare, so a shared sufficiency norm synchronizes demand reduction, counters rebound (efficiency gains being re-spent into more consumption), and keeps absolute energy and material throughput within ecological ceilings.
% TRANSFER_FUNCTION: Moves perceived legitimacy and investment flow away from large-scale generation expansion toward demand-side management and sufficiency programming; concentrates agenda-setting authority and associated funding in advocacy institutions; distributes ecological headroom as a diffuse good; and imposes foregone energy access and foregone buildout opportunity on energy-poor populations, Global South industrializers, and growth-dependent generation industries.
% ABSENT_VOICES: General energy consumers are described-for rather than seated; energy-poor households appear only through intermediary advocates; grid reliability engineers testify only adversarially when summoned by expansionist opponents; future generations are invoked rhetorically but hold no seat anywhere in the deliberation.
% DISAPPEARANCE_RATIONALE: If the norm vanished overnight, sufficiency programming loses its funding rationale and the advocacy ecosystem reorganizes around adaptation or distribution; expansion debates lose their restraining pole and buildout approvals accelerate; energy-poor constituencies lose an ally-frame; the physical energy system shifts little immediately since the norm never commanded majority policy, but the legitimacy economy of climate policy visibly rearranges.
% FOUNDING_PROBLEM: Efficiency-and-substitution alone were not cutting absolute emissions and material throughput fast enough: rebound effects consumed efficiency gains (Jevons dynamics), and aggregate consumption trajectories overshot ecological ceilings — the concern crystallized in the 1970s limits-to-growth moment and was renewed by planetary-boundaries science.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: IPCC Working Group III demand-side assessments and Earth-system (planetary boundaries) literature attest that absolute throughput and emissions overshoot persist despite efficiency gains, and updated limits-to-growth modelling corroborates the trajectory concern. None of these sources, however, attests the strong form that generation expansion is unnecessary — they support the founding problem, not this reading's exclusive remedy.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 at interval end: the norm transfers real costs — foregone energy access, suppressed buildout opportunity, financing penalties on growth-dependent clean industries — onto identifiable groups while its headline goods (ecological headroom, avoided lock-in) are diffuse. Suppression is 0.58 and is a raw structural property, unscaled by power or scope: the norm's coercive force is mostly discursive and budgetary (legitimacy gatekeeping, funding conditionality, movement orthodoxy) rather than statutory, so alternatives remain visible and pursued. Theater is 0.45: a large share of activity is manifestos, conferences, and symbolic local experiments relative to implemented absolute demand reduction, but the agenda-setting function performed is real. Accessibility_collapse is low (0.38) because understanding the claim does not close the expansion option — states keep building; the claim competes rather than forecloses. Resistance is high (0.75): the energy-industrial complex, most governments, modeling institutions, and consumer preferences push back continuously. The temporal series run on one shared grid (T=0..30, mapping roughly 1995-2025) with all three metrics authored at every point; the suppression_requirement series is authored because the story specifically tracks enforcement-capacity change — the norm's maintenance effort intensified as movement professionalization met expansionist counter-mobilization after the energy-security shocks of the early 2020s. Endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the advocacy seat the arrangement is overdue justice: taking consumption headroom from overconsumers is the point, so extraction registers as intended function. From the renewable-developer seat the same structure is bitterly ironic — a climate-legitimacy norm taxing the climate industry — and from the nuclear seat it is prejudicial gatekeeping that raises capital costs without adjudicating merit. From the energy-poor and Global South seats it is distant affluence rationing before universal access. The analyst seat sees a four-way contest none of the participants fully models. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Advocacy networks derive a deep-beneficiary directionality from the beneficiary declaration plus identity_locked exit (they cannot leave the frame that subsidizes them). Nuclear and renewable developers derive near-full-target directionality: they bear the transfer (delegitimized capital deployment) with trapped or constrained exit. Energy-poor households and Global South industrializers derive high target directionality from the victim declarations, amplified by trapped/constrained exit. General consumers sit near-symmetric in raw flows but are excluded from the conversation that sets the terms. One override is authored: fossil_fuel_incumbents (power_atom: powerful, d_value 0.25). Pure beneficiary derivation would place them near d~0.08, but their gain is contingent on the norm remaining rhetorical — full success destroys their market — and the norm's demand logic ultimately targets their product; their structural position is a mixed, incidental one nearer the middle than a designed beneficiary's, so the override lifts d from the derived deep-beneficiary value to 0.25.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — absolute throughput overshoot despite efficiency gains — is still live, so the arrangement is not mandatrophy-resolved and no sunset applies: the claim presents as a permanent principle, not a transition device, which blocks scaffold misclassification. The tangled_rope structure guards against both standard errors: a pure-snare reading would erase the genuine coordination core (planetary boundaries and rebound are real collective-action problems the norm coordinates against), while a pure-rope reading would erase the asymmetric burden (energy-poor and Global South pay without proportionate benefit) and the concentrated institutional gains flowing to the advocacy ecosystem. Piton misclassification fails because enforcement is intensifying, not atrophying — theater is rising but remains below functional dominance, and the maintainers still bear real costs of defending the claim against a mobilized opposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the climate_mitigation_legitimacy kernel; the baseload_necessity, renewable_primacy, and portfolio_pragmatism readings instantiate different constraints with different victim sets and different epsilon. Where does the contest actually bind?',
    'Track adoption patterns among policy elites, modeling institutions, and finance: which reading''s victim set and remedy set governs actual capital allocation and scenario architecture over time.',
    'Switching readings swaps the victim set (growth-dependent generation industries versus demand-side constituencies versus each other) and flips epsilon and classification wholesale; this story''s values are valid only under the degrowth reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a four-reading kernel; sibling readings are separate constraints.').

omega_variable(
    rebound_evidence_strength,
    'Is the empirical core — that rebound and Jevons dynamics nullify efficiency-led decarbonization strongly enough to make demand reduction NECESSARY rather than merely helpful — actually supported at macro scale?',
    'Macro-level elasticity and economy-wide rebound studies across decoupling episodes; comparison of absolute consumption trajectories in high-efficiency economies.',
    'If economy-wide rebound is modest, the necessity claim weakens to a preference, the coordination core shrinks, and the arrangement drifts toward pure advocacy positioning with higher effective extraction relative to function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebound_evidence_strength, empirical, 'Whether rebound empirics carry the strong necessity claim.').

omega_variable(
    burden_incidence_equity,
    'Do the arrangement''s costs actually land on affluent overconsumption (as the reading intends and defends) or regressively on energy-poor households and Global South development?',
    'Distributional incidence analysis of enacted sufficiency policies (carbon pricing pass-through, mobility restrictions, retrofit mandates) by income decile and region.',
    'Regressive incidence would sharpen the extraction asymmetry and push the computed classification toward the snare end; progressive incidence would validate the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_incidence_equity, empirical, 'Whether the burden falls where the reading says it should.').

omega_variable(
    fossil_delay_capture,
    'Is fossil incumbents'' amplification of restraint rhetoric opportunistic parallel benefit, or active capture — funding and discourse operations that keep the norm rhetorical precisely so it never binds their demand?',
    'Trace funding flows and coordinated messaging between fossil interests and sufficiency-adjacent delay advocacy; compare restraint-rhetoric uptake in jurisdictions with and without fossil lobbying presence.',
    'Confirmed capture would raise effective extraction sharply and reposition the fossil seat from incidental beneficiary toward co-agenda-setter; mere parallelism leaves the current structure intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_delay_capture, empirical, 'Whether the fossil beneficiary seat is captured or coincidental.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (funding gatekeeping, advisory-seat exclusion, conditionality) or internalized (movement-space orthodoxy that persists as self-policing after external barriers are removed)?',
    'Post-exit suppression trajectory: track researchers and campaigners who leave the advocacy ecosystem — if orthodox enforcement persists in their new positions absent external incentive, the internalized share is substantial.',
    'If largely internalized, the arrangement''s suppressive force travels with its carriers and outlasts any single funding regime, raising durable suppression above the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism split.').

omega_variable(
    sufficiency_floor_definition,
    'What counts as ''enough'' — the floor beneath which demand reduction may not reach? The answer determines who is an overconsumer and therefore who the arrangement may legitimately burden.',
    'Not resolvable by data alone: it depends on weighting subsistence, dignity, and ecological headroom against one another; deliberative and doctrinal processes (equity frameworks, capability thresholds) are the relevant arenas.',
    'A high floor protects energy-poor and Global South seats from the victim set; a low floor universalizes the burden and converts the arrangement''s equity defense into cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_floor_definition, preference, 'Value-laden definition of the sufficiency floor governing burden allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(degrowth_sufficiency_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(degrowth_sufficiency_tr_t0, observed).
narrative_ontology:measurement(degrowth_sufficiency_tr_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(degrowth_sufficiency_tr_t5, observed).
narrative_ontology:measurement(degrowth_sufficiency_tr_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(degrowth_sufficiency_tr_t10, observed).
narrative_ontology:measurement(degrowth_sufficiency_tr_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(degrowth_sufficiency_tr_t15, observed).
narrative_ontology:measurement(degrowth_sufficiency_tr_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(degrowth_sufficiency_tr_t20, observed).
narrative_ontology:measurement(degrowth_sufficiency_tr_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(degrowth_sufficiency_tr_t25, observed).
narrative_ontology:measurement(degrowth_sufficiency_tr_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(degrowth_sufficiency_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(degrowth_sufficiency_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(degrowth_sufficiency_be_t0, observed).
narrative_ontology:measurement(degrowth_sufficiency_be_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement_basis(degrowth_sufficiency_be_t5, observed).
narrative_ontology:measurement(degrowth_sufficiency_be_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(degrowth_sufficiency_be_t10, observed).
narrative_ontology:measurement(degrowth_sufficiency_be_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(degrowth_sufficiency_be_t15, observed).
narrative_ontology:measurement(degrowth_sufficiency_be_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(degrowth_sufficiency_be_t20, observed).
narrative_ontology:measurement(degrowth_sufficiency_be_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(degrowth_sufficiency_be_t25, observed).
narrative_ontology:measurement(degrowth_sufficiency_be_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(degrowth_sufficiency_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(degrowth_sufficiency_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(degrowth_sufficiency_su_t0, observed).
narrative_ontology:measurement(degrowth_sufficiency_su_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 5, 0.43).
narrative_ontology:measurement_basis(degrowth_sufficiency_su_t5, observed).
narrative_ontology:measurement(degrowth_sufficiency_su_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement_basis(degrowth_sufficiency_su_t10, observed).
narrative_ontology:measurement(degrowth_sufficiency_su_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(degrowth_sufficiency_su_t15, observed).
narrative_ontology:measurement(degrowth_sufficiency_su_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(degrowth_sufficiency_su_t20, observed).
narrative_ontology:measurement(degrowth_sufficiency_su_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(degrowth_sufficiency_su_t25, observed).
narrative_ontology:measurement(degrowth_sufficiency_su_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(degrowth_sufficiency_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how to decarbonize legitimately' decomposes into four structurally distinct readings of the climate_mitigation_legitimacy kernel, each with its own epsilon, beneficiary/victim structure, and classification. This member (degrowth_sufficiency_reading) uniquely places BOTH nuclear and renewable industries in its victim set as growth-dependent, privileges energy-system downsizing, and minimizes new capital deployment; the baseload_necessity sibling reverses the necessity premise entirely, and the renewable_primacy and portfolio_pragmatism siblings relocate the victim set. The upstream members (higher empirical confidence: grid physics, cost curves) influence the downstream contested member; this reading influences its siblings by shifting legitimacy conditions and green-finance conditionality without resolving the contest. Per the epsilon-invariance principle, no single story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
