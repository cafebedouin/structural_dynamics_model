% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Planned Economic Contraction as Legitimate Climate Response (Degrowth Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth reading of climate harm prevention asserts that legitimate
 *   mitigation requires planned economic contraction in wealthy economies
 *   because emissions reductions necessary to prevent catastrophic warming
 *   are physically incompatible with maintained growth trajectories. This
 *   reading directly contests the mitigation-priority reading (which
 *   maintains growth-compatible transition is possible) and creates
 *   structural pressure on the adaptation-priority reading (by insisting
 *   contraction is feasible and necessary rather than accepting high warming
 *   as inevitable). The degrowth reading positions Global South populations
 *   and future generations as primary beneficiaries of North contraction, and
 *   present Global North consumption as the structural victim of the
 *   transition costs. This is a kernel reading instantiating one normative
 *   claim about what 'legitimate' climate response entails; sibling readings
 *   propose different claims grounded in different assessments of
 *   technological feasibility, equity, and political realism.
 *
 * KEY AGENTS:
 *   - Global South populations: powerless, face disproportionate climate damages, structurally excluded from wealthy-nation policy institutions
 *   - Future generations: powerless, civilizational time horizon, trapped in the climate regime set by present emissions choices
 *   - Global North present consumption: organized, biographical horizon, constrained exit — subject to contraction costs
 *   - Incumbent growth coalition: institutional power, currently agenda-setting within growth framework, both payer (threatened business model) and enforcer (controlling policy)
 *   - Degrowth advocacy movements: organized advocates, ideological clarity, limited institutional power
 *   - Labor and transition workers: organized, split interests (disruption vs. transition opportunity)
 *   - Development justice coalition: organized, moral authority, weaker institutional position
 *   - Climate science: observer role, provides biophysical constraints all readings must accommodate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.82).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.76).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Planned Economic Contraction as Legitimate Climate Response (Degrowth Reading)").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '0437e374-cb27-4438-8d79-8a7011c138d6').
narrative_ontology:cs_kernel_codification('0437e374-cb27-4438-8d79-8a7011c138d6', distributed).
narrative_ontology:cs_authority_grounding('0437e374-cb27-4438-8d79-8a7011c138d6', distributed).
narrative_ontology:cs_reading_relation('0437e374-cb27-4438-8d79-8a7011c138d6', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('0437e374-cb27-4438-8d79-8a7011c138d6', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_axiom('0437e374-cb27-4438-8d79-8a7011c138d6', foundational, decoupling_insufficient_for_necessary_reductions).
narrative_ontology:cs_axiom_status(decoupling_insufficient_for_necessary_reductions, holdable).
narrative_ontology:cs_axiom_grounding('0437e374-cb27-4438-8d79-8a7011c138d6', decoupling_insufficient_for_necessary_reductions, empirically_contingent).
narrative_ontology:cs_axiom('0437e374-cb27-4438-8d79-8a7011c138d6', foundational, north_contraction_prerequisite_to_legitimate_response).
narrative_ontology:cs_axiom_status(north_contraction_prerequisite_to_legitimate_response, holdable).
narrative_ontology:cs_axiom_grounding('0437e374-cb27-4438-8d79-8a7011c138d6', north_contraction_prerequisite_to_legitimate_response, deontological).
narrative_ontology:cs_reference_frame('0437e374-cb27-4438-8d79-8a7011c138d6', growth_dependent_wealthy_economy).
narrative_ontology:cs_drift_state('0437e374-cb27-4438-8d79-8a7011c138d6', post_climate_tipping_point_recognition_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0437e374-cb27-4438-8d79-8a7011c138d6', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_present_consumption).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, degrowth_advocacy_movements).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, transition_workers_and_labor).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, development_justice_coalition).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, incumbent_growth_coalition).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, transition_workers_and_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face disproportionate climate impacts (flooding, drought, heat stress, agricultural collapse) driven by historical and ongoing Global North emissions. Under degrowth reading, they gain from immediate North contraction that reduces future warming and frees ecological/economic space for development priority. Structurally absent from the governance institutions that would implement contraction; cannot exit the physical constraint of climate change.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Inherit the climate regime determined by present-era mitigation trajectories and cumulative emissions. Under degrowth reading, they benefit from present contraction in North that reduces warming damage to their starting conditions. Have no current voice in the constraint-setting institutions; their interests are represented through proxies (environmental advocates, institutions with centurial mandates) if at all.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Absorbs the immediate cost of planned contraction: reduced material throughput, constrained consumption growth, restructured labor markets, shifted investment away from growth-linked assets. This represents individuals, corporations, labor unions, and state treasuries in wealthy economies dependent on growth-path asset appreciation and consumption expansion. Their exit option is political rejection of the constraint (which the enforcement frame must suppress), not geographic or economic exit from the world system.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_present_consumption, payer,
    organized, biographical, constrained, global).

% Includes fossil fuel producers, growth-dependent financial institutions, consumer goods manufacturers, and state apparatus oriented to GDP expansion. They formally set climate policy agendas in most wealthy nations and international bodies. Under degrowth reading they are both payers (their core business model is threatened) and agenda-setters (they currently control which framings are 'legitimate'). Their arbitrage option is capital flight to jurisdictions not implementing contraction, which the constraint must prevent via coordination across sovereign states (currently absent).
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, incumbent_growth_coalition, payer,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, incumbent_growth_coalition, agenda_setter).

% Argue the degrowth framing is the only physically defensible mitigation path and advocate for policy implementation. Benefit from the constraint's adoption insofar as it validates their analysis and creates constituencies for systemic transition. Possess ideological clarity and moral urgency but lack institutional power to unilaterally enforce the arrangement; must build political coalitions with labor movements, development justice advocates, and post-carbon industries.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_advocacy_movements, beneficiary,
    organized, generational, mobile, regional).

% Face immediate income disruption from contraction of fossil and growth-dependent sectors (coal mining, automotive, fast fashion, aviation, energy-intensive manufacturing). Also stand to benefit from intentional transition pathways (public employment, skill retraining, post-carbon infrastructure jobs) if the contraction is managed with labor negotiating power. Their exit is geographically constrained; their material interests are genuinely split.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, transition_workers_and_labor, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, transition_workers_and_labor, beneficiary).

% Argues that North contraction is restitution for colonial extraction and present climate colonialism, and that South development space is prerequisite to climate stability. Views degrowth reading as the only framework that centers South equity. Possess moral authority and South-based constituencies but weaker institutional power in wealthy-nation policy spaces where enforcement would occur.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, development_justice_coalition, beneficiary,
    organized, generational, mobile, global).

% Provide the physical constraints and empirical baselines (carbon budget, tipping points, ecosystem viability thresholds) that frame all three climate readings as responses to the same physical problem. Do not advocate for degrowth specifically, but their data constrains what mitigation framings are physically tenable. Can be captured into different readings' rhetorical service (baseline denial for adaptation-priority, technological optimism for mitigation-priority, biophysical realism for degrowth), but cannot avoid the constraint that warming trajectories are functions of cumulative emissions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, climate_science_and_ecologists, observer,
    institutional, civilizational, analytical, global).

% Currently set climate policy within growth-framework constraints; dependent on growth-linked tax revenue, electoral cycles tied to consumption expectations, and capital mobility. Under degrowth reading would need to implement contraction coordination (emissions budgets, consumption caps, investment reallocation) against capital flight and domestic political opposition. Possess enforcement capacity but lack coordination mechanisms to prevent arbitrage across jurisdictions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, wealthy_nation_governments, agenda_setter,
    institutional, biographical, mobile, national).

% Framings that would reject degrowth's legitimacy premise: that growth-compatible mitigation is not merely politically constrained but physically impossible. Include techno-optimism (efficiency gains decouple emissions from growth), adaptation-sufficiency (accept higher warming and focus on resilience), and market-mechanism framings (carbon pricing without contraction). These are structurally excluded by the degrowth reading's definition of 'legitimate response.'
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, excluded_alternatives_framings, excluded,
    analytical, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a transition away from growth-dependent production and consumption in wealthy economies toward reduced material throughput, renewable energy infrastructure, and meeting material needs within planetary boundaries. Solves the genuine coordination problem that individual North actors cannot unilaterally decouple growth-dependency without competitive disadvantage and capital flight; requires synchronized policy across wealthy nations and international enforcement to prevent arbitrage.
% TRANSFER_FUNCTION: Transfers ecological carrying capacity from present Global North consumption to future generations and present Global South development. Moves resources (capital, labor, energy, manufactured goods) away from luxury consumption, speculative investment, and planned obsolescence toward meeting basic needs, transition infrastructure, and ecosystem restoration. Absorbs growth-path asset losses in wealthy economies and constraints present consumption expansion.
% ABSENT_VOICES: Populations in wealthy economies not yet politicized into degrowth constituencies (middle-income Global North workers who fear contraction without transition protection); Global South populations whose own development aspirations differ from the degrowth frame (who may prioritize rapid industrialization or technology adoption over contraction of the North); workers in fossil and growth-dependent sectors who have not secured transition agreements; future generations themselves, represented only through proxy advocacy.
% DISAPPEARANCE_RATIONALE: If planned contraction as a legitimate climate response framework vanished, wealthy economies would continue growth-trajectory policy (current state), emissions would remain on high warming paths, and Global South climate impacts would intensify. The economic organization of the North would stay locked in growth-dependency, the political legitimacy of degrowth advocacy would evaporate (no institutional backing), and the distribution of climate costs would remain radically unequal. The disappearance of the constraint would mean the persistence of high-emissions growth trajectories and the foreclosure of the South's development space.
% FOUNDING_PROBLEM: Physical climate science establishes that remaining carbon budget for 1.5°C or even 2°C warming is exhausted by midcentury if wealthy economies maintain growth trajectories; technological mitigation without contraction cannot achieve necessary emissions reductions in required timeframes. Political economy of growth-dependent states (fossil fuel lobbying, asset-appreciation expectations, labor dependency on growth-linked employment) prevents genuine contraction from being legitimated within growth-framework policy. Therefore, the founding problem is: how to achieve necessary emissions reductions when growth-framework mitigation is physically inadequate and politically neutered?
% FOUNDING_PROBLEM_CORROBORATION: Climate science consensus on remaining carbon budget and tipping-point proximity attests to the physical constraint. IPCC synthesis reports, emissions gap analyses, and peer-reviewed biophysical modeling corroborate that 1.5°C and 2°C pathways require rapid emissions reductions incompatible with business-as-usual growth. Political economists and climate justice researchers outside degrowth advocacy (including mainstream institutions like World Bank, IMF, and academic climate programs) increasingly document that growth-framework mitigation has not delivered emissions reductions at required pace or scale. The founding problem's status is contested because wealthy-nation policy actors and some climate economists deny the physical/political incompatibility premise, maintaining that technological transition and market mechanisms can solve within growth. But the material evidence for the founding problem (observational climate data, emissions trajectory tracking, failed prior mitigation targets) is not in dispute; what is contested is whether degrowth is the legitimate response.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82 at interval end) because the constraint moves resources from Global North present consumption (the victim set, structurally most organized politically) to Global South and future (the beneficiary set, structurally powerless in current governance). The transfer is structurally asymmetric and requires suppressing the preferences of the politically powerful incumbent coalition that currently benefits from growth trajectories. Suppression is high (0.76) because the constraint cannot persist through participant choice alone — it requires overriding the growth-dependent financial, state, and corporate interests that dominate policy in wealthy nations. Theater ratio rises from low (0.22 at t=0, before any implementation) toward moderate (0.48 by t=50) as initial policy adoption creates performative commitments ('net zero by 2050' targets) that are decoupled from actual contraction (genuine material throughput reduction). The measurement series tracks the constraint as it moves from theoretical advocacy phase (low suppression needed, low theater) toward implementation phase (high suppression needed to maintain against incumbent coalition resistance, rising theater as policy commitments outpace material change). All metrics authored on one shared time grid (t=0,10,20,30,40,50) so temporal analysis has complete data.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (wealthy-nation governments, incumbent growth coalition) experiences this constraint as threatening and illegitimate precisely because it requires abandoning their primary organizing principle (growth). From their seat the constraint appears as an extractive imposition by ideological actors (degrowth advocates) lacking material consequences for their preferences. The beneficiary seats (Global South, future generations) experience it as the only legitimate response to genuine physical constraints and historical injustice; from their seat the constraint is not extraction but restitution and survival necessity. The constraint's type divergence arises from this perspectival split: the agenda-setter seat computes it as snare (they lose, it is enforced against their preference), the beneficiary seats compute it as tangled-rope (genuine coordination function — preventing climate catastrophe — coupled with asymmetric distribution of costs). The engine's per-seat classification captures this divergence; the single story's ε (0.82) reflects that the constraint is being authored from a reading whose beneficiaries are the Global South and future-generation seats, and whose extraction targets the wealthy-nation growth coalition.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South populations and future generations are beneficiaries (d near 0.0): they gain the primary benefit (reduced warming, preserved carrying capacity) and are already bearing costs of climate change under the status quo; the constraint improves their position. Global North present consumption is the victim (d near 1.0): they bear the direct cost (reduced material throughput, growth-path asset loss, consumption constraints) and have been beneficiaries of the prior arrangement (cheap energy, growth-dependent employment, consumption abundance). The incumbent growth coalition is ambiguously positioned — they are both payer (threatened business model) and current agenda-setter (they set the frame that degrowth must contest). Their d is high as targets of the constraint but complicated by their institutional power; their exit option is arbitrage (moving capital to non-contracting jurisdictions), which the constraint must suppress through international coordination. Development justice advocates and labor movements are organized beneficiaries/payers respectively, with moderate power and constrained but not trapped exit. The climate science seat is analytical (d=0.5, observer position): they anchor the physical constraints all readings must accommodate but do not preferentially benefit or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (emissions reductions at required pace are genuinely not delivered by growth-framework mitigation, verified by observation). The disappearance verdict is world_rearranges (the constraint's absence means high-emissions trajectories persist and Southern development space is foreclosed). This alignment (live founding problem + world_rearranges verdict) indicates the constraint is not mandatrophic — it is performing its intended function (enabling necessary emissions reductions by coordinating North contraction). However, theater ratio rises significantly over the interval (0.22 to 0.48), indicating performative commitment-inflation: wealthy-nation governments adopt 'net zero' targets and green finance rhetoric while maintaining material throughput growth and failing to implement actual contraction. This rising theater is a warning signal of incipient mandatrophy: the performative layer is divorcing from material function. If theater reaches 0.65-0.75, the constraint will begin to show mandatrophic character (the theatrical commitment persists while the founding problem remains unaddressed because material contraction is not implemented). The current trajectory has the constraint as a functional tangled-rope (genuine coordination function coupled with extraction) with rising theatrical maintenance costs; if theater continues rising without material change, it would migrate toward piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_contested,
    'Can wealthy economies achieve necessary emissions reductions (2-3% annually) while maintaining or growing material throughput and GDP?',
    'Multi-decadal empirical observation of wealthy-economy emissions trajectories post-policy adoption; engineering assessments of renewable energy deployment rates required for full decoupling; material flow analysis tracking whether growth-decoupling is compositional (shift to services) or genuine (absolute throughput reduction without growth offset).',
    'If decoupling is feasible at required scale/speed, the mitigation_priority reading''s core premise holds and degrowth is not physically necessary (though still possibly preferable on equity grounds). If decoupling is not feasible, degrowth reading''s core premise is vindicated and growth-framework mitigation is physically illusory. This is the empirical linchpin that determines whether the readings coexist or one forecloses the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_feasibility_contested, empirical, 'Whether material decoupling is feasible at required scale — empirical question determining viability of growth-compatible mitigation').

omega_variable(
    political_coordination_feasibility,
    'Can international coordination enforce North contraction (preventing capital flight, speculative arbitrage, policy breakdown) without centralized world authority or coercive supranational power?',
    'Observation of actual policy coordination responses to climate targets (current: minimal; Paris Agreement has no enforcement mechanism); emergence of coordinated carbon border mechanisms, capital controls, or supra-state enforcement apparatus; historical comparison to prior coordination problems requiring enforcement (Montreal Protocol, nuclear non-proliferation).',
    'If coordination is infeasible without supranational coercion (currently absent), the degrowth constraint cannot be enforced and remains a normative claim without material grip (theater without function — piton trajectory). If coordination proves feasible through state-coalition mechanisms (as Montreal Protocol did), enforcement becomes structurally possible and degrowth moves from utopian to implementable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_coordination_feasibility, empirical, 'Whether coordinated North contraction can be enforced without global centralized authority').

omega_variable(
    growth_framework_boundary_contestation,
    'Is ''growth'' a constraint boundary defining what counts as legitimate climate response, or is it a policy lever independent of climate necessity?',
    'Normative/conceptual: this is not empirically resolvable. Different reading traditions place growth at different positions: mitigation_priority treats it as separable (growth + emissions reduction possible), degrowth treats it as foundational to the problem (growth drives emissions), adaptation_priority brackets it (focuses on resilience regardless). Resolution depends on which reading''s axioms are accepted, not on additional data.',
    'This ambiguity is the core committer-frame uncertainty: different readings of the climate_harm_prevention kernel disagree on what the problem is fundamentally about. If growth is the problem, degrowth''s extraction (imposing contraction) is reframed as treatment. If growth is not the problem, degrowth''s extraction is unnecessary and regressive. No empirical observation can settle this because it is not empirical — it is axiomatic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(growth_framework_boundary_contestation, conceptual, 'Definitional contest: whether growth is a constraint boundary or a policy lever').

omega_variable(
    intergenerational_beneficiary_standing,
    'Do future generations have standing as beneficiaries in present-era policy constraints, or only present actors?',
    'Philosophical/normative: different moral theories assign different standing to non-present entities. This is not empirically resolvable but conceptually tractable via philosophical analysis of futurity, moral patiency, and intergenerational justice. Some traditions (precautionary principle, long-termism, indigenous governance models) grant standing; others (presentist utilitarianism, libertarian frameworks) deny it.',
    'If future generations have moral standing as beneficiaries, degrowth reading''s beneficiary set is structurally justified and the constraint operates in service of genuine coordination (present contraction for future survival). If they lack standing, degrowth reading''s beneficiary set is phantom (no actual agent in future can collect or ratify the benefit), and the constraint becomes extraction from present North for abstract future, which reframes it as snare (imposition without coherent beneficiary). This determines whether the tangled-rope classification holds or collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_beneficiary_standing, preference, 'Whether future generations have moral standing in present climate policy — depends on ethical framework adopted').

omega_variable(
    south_development_equity_framing,
    'Is the degrowth reading''s positioning of Global South as primary beneficiary empirically accurate, or does South contraction-via-North''s-contraction actually foreclose South development pathways?',
    'Development economics and political economy analysis: examine whether North contraction frees South development space (as degrowth claims) or merely reduces North demand for South exports, eliminating a growth channel without providing alternative development paths. Track what empirical changes in capital flows, technology transfer, and South agency actually emerge if North contraction occurs.',
    'If North contraction enables South development, degrowth''s beneficiary claim is vindicated and South coordination is win-win (both get what they need — South gets emissions space, North gets forced transition). If North contraction merely impoverishes both (demand destruction without development alternative), the constraint becomes globally extractive (piton or snare for both North and South), and the beneficiary framing is false.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(south_development_equity_framing, empirical, 'Whether North contraction frees or forecloses South development space — depends on actual economic mechanisms of transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__degrowth_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__degrowth_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(clim_tr_t10, projected).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__degrowth_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(clim_tr_t20, projected).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__degrowth_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t40, climate_harm_prevention__degrowth_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(clim_tr_t40, projected).
narrative_ontology:measurement(clim_tr_t50, climate_harm_prevention__degrowth_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement_basis(clim_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__degrowth_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(clim_be_t0, projected).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__degrowth_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(clim_be_t10, projected).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__degrowth_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement_basis(clim_be_t20, projected).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__degrowth_reading, base_extractiveness, 30, 0.79).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t40, climate_harm_prevention__degrowth_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(clim_be_t40, projected).
narrative_ontology:measurement(clim_be_t50, climate_harm_prevention__degrowth_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement_basis(clim_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__degrowth_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__degrowth_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(clim_su_t10, projected).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__degrowth_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(clim_su_t20, projected).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__degrowth_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t40, climate_harm_prevention__degrowth_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement_basis(clim_su_t40, projected).
narrative_ontology:measurement(clim_su_t50, climate_harm_prevention__degrowth_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement_basis(clim_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).

% DUAL FORMULATION NOTE:
% The climate_harm_prevention kernel has three structurally distinct readings: mitigation_priority (growth-compatible emissions reduction via technology), adaptation_priority (accept higher warming, build resilience), and degrowth_reading (contract North to achieve necessary emissions reductions). Each reading instantiates a different constraint with different ε, beneficiary/victim structure, and type classification. The readings coexist in contested public discourse and influence each other through political legitimacy competition. Degrowth reading constrains mitigation_priority (argues decoupling is impossible) and influences adaptation_priority (by insisting contraction is feasible, makes pure adaptation less tenable). All three are linked by network.affects_constraints to represent the kernel's internal structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__degrowth_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
