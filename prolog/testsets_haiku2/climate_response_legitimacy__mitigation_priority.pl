% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Legitimacy Commitment
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority reading of climate legitimacy establishes that
 *   the appropriate response to atmospheric CO2 accumulation is technological
 *   emissions reduction combined with carbon pricing, framed as capable of
 *   decoupling growth from emissions and therefore compatible with preserving
 *   wealthy-nation living standards and incumbent economic structures. This
 *   reading contests two sibling readings: adaptation-priority (which accepts
 *   high warming and prioritizes resilience over mitigation) and
 *   degrowth-transformation (which argues decoupling is infeasible and
 *   requires deliberate economic contraction). The mitigation-priority
 *   reading's core claim is that technological solutions + market mechanisms
 *   = climate safety without material sacrifice. The structural delta is that
 *   future generations and developing nations enter the victim set if
 *   decoupling fails (renewable capacity lags, CDR proves inadequate, carbon
 *   lock-in continues), while wealthy-nation workers, fossil fuel regions,
 *   and adaptation practitioners enter the payer set. This reading is CLAIMED
 *   as tangled_rope because it coordinates action on real climate risk while
 *   simultaneously extracting benefit for wealthy nations and energy capital
 *   through the institutionalization of a particular legitimacy frame that
 *   forecloses competing responses.
 *
 * KEY AGENTS:
 *   - Incumbent energy producers (institutional power) — preserve asset value by ensuring mitigation-priority framing remains credible
 *   - Wealthy nations' current electorates (institutional power) — benefit from climate action that promises no growth sacrifice
 *   - Carbon tech entrepreneurs (organized power) — capture deployment capital and subsidies under the commitment
 *   - Future generations in high-warming scenarios (powerless) — enter victim set if decoupling fails
 *   - Developing nations (moderate power) — constrained to expensive decarbonization while wealthy nations preserve growth
 *   - Fossil fuel workers (moderate power) — bear direct transition costs and job losses
 *   - Degrowth and adaptation advocates (excluded) — systematically sidelined from policy conversations that define legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.71).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Climate Legitimacy Commitment").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, 'ec5f5594-a3cb-4267-b364-0f01038a1c0e').
narrative_ontology:cs_kernel_codification('ec5f5594-a3cb-4267-b364-0f01038a1c0e', formalized).
narrative_ontology:cs_authority_grounding('ec5f5594-a3cb-4267-b364-0f01038a1c0e', expertise).
narrative_ontology:cs_interpretation_layer_present('ec5f5594-a3cb-4267-b364-0f01038a1c0e').
narrative_ontology:cs_reading_relation('ec5f5594-a3cb-4267-b364-0f01038a1c0e', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('ec5f5594-a3cb-4267-b364-0f01038a1c0e', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('ec5f5594-a3cb-4267-b364-0f01038a1c0e', foundational, technological_decoupling_sufficient).
narrative_ontology:cs_axiom_status(technological_decoupling_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('ec5f5594-a3cb-4267-b364-0f01038a1c0e', technological_decoupling_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('ec5f5594-a3cb-4267-b364-0f01038a1c0e', foundational, market_mechanisms_allocate_optimally).
narrative_ontology:cs_axiom_status(market_mechanisms_allocate_optimally, holdable).
narrative_ontology:cs_axiom_grounding('ec5f5594-a3cb-4267-b364-0f01038a1c0e', market_mechanisms_allocate_optimally, conventional).
narrative_ontology:cs_reference_frame('ec5f5594-a3cb-4267-b364-0f01038a1c0e', technological_decoupling_legitimacy_frame).
narrative_ontology:cs_drift_state('ec5f5594-a3cb-4267-b364-0f01038a1c0e', post_paris_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ec5f5594-a3cb-4267-b364-0f01038a1c0e', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, incumbent_energy_producers).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, wealthy_nations_current_generation).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, carbon_tech_entrepreneurs).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations_high_warming_scenario).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, developing_nations_constrained_growth).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, workers_fossil_fuel_transition).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, technological_decoupling_possible).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, market_mechanisms_solve_coordination).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, growth_compatible_with_climate_safety).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of climate legitimacy by funding think tanks, underwriting policy analysis, and lobbying for carbon pricing (which they can trade within) over alternative framings. Collects extraction through avoided divestment timelines, asset preservation, carbon market participation, and maintained access to cheap capital. Their exit from this commitment would require accepting stranded-asset losses or competing in a degrowth-oriented economy; both are more costly than defending the mitigation-priority frame.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, incumbent_energy_producers, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, incumbent_energy_producers, beneficiary).

% Wealthy-nation electorates benefit from the mitigation-priority commitment because it promises climate action without material sacrifice — the growth trajectory, employment in tech sectors, and consumption patterns are preserved. They bear transition costs (energy price signals, infrastructure buildout) but these are treated as investments in growth, not losses. Their exit from this commitment would require electoral majorities accepting degrowth or accepting higher warming; both face strong domestic political resistance.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, wealthy_nations_current_generation, beneficiary,
    institutional, biographical, constrained, national).

% Capture substantial capital flows under the mitigation-priority framing: venture capital, government procurement, subsidies, renewable deployment mandates, carbon offset demand. Their business models are entirely dependent on the commitment's persistence — batteries, solar, wind, grid tech, carbon capture startups all exist because the commitment generates demand for these solutions. They have exit options (other industrial sectors) but benefit enough to defend the current frame.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_tech_entrepreneurs, beneficiary,
    organized, biographical, mobile, global).

% If decoupling fails (renewable deployment lags required scales, CDR proves inadequate, carbon lock-in persists), future generations inherit a climate trajectory beyond 2°C warming with cascading ecological and social damage. They cannot negotiate the terms of the commitment, cannot vote on whether they accept the risk, and cannot exit — their existence and wellbeing depend on present decisions made without their consent. The commitment transfers all downside risk to them while preserving growth for present generations.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations_high_warming_scenario, payer,
    powerless, civilizational, trapped, universal).

% Developing nations are told to build renewable-only infrastructure immediately, despite having lower per-capita emissions and lower industrial base. They face climate finance conditionality that ties money to decarbonization-first policies, blocking pathways to growth through cheap energy infrastructure. They cannot exit by choosing cheap fossil fuels (climate finance is cut off); cannot exit by choosing growth-first (the commitment forbids it). Their growth model is narrowed while wealthy nations' growth is preserved.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, developing_nations_constrained_growth, payer,
    moderate, generational, constrained, regional).

% Coal miners, oil refinery workers, gas plant operators, and supply-chain workers are told their livelihoods must be sacrificed for the climate response. Retraining programs exist but often lead to lower wages or different work. Geographic displacement is common. They have constrained exit options (skill-specific jobs, regional economies built on fossil extraction), face direct job losses, and their voice is admitted to transitional-justice discussions but not determinative in policy design. The commitment treats their livelihoods as acceptable losses.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, workers_fossil_fuel_transition, payer,
    moderate, biographical, constrained, regional).

% Scientific bodies measure climate outcomes and communicate uncertainty. Under the mitigation-priority commitment, they face pressure to maintain optimism about decoupling feasibility to keep political support for climate action alive, while their model ensembles show wide uncertainty bands around whether current policies suffice. They observe outcomes and communicate risk, but their epistemic autonomy is compromised by political stakes — optimism bias in communication is rewarded, honest uncertainty is treated as defection.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_scientists, observer,
    institutional, generational, analytical, global).

% Climate adaptation practitioners (coastal defense architects, drought-resistant agriculture developers, early-warning system builders, climate-refuge planners) are structurally sidelined by the mitigation-priority framing, which treats adaptation as secondary and subordinate. Their resource access is rationed; policy influence is suppressed. They would argue that given warming already locked in, adaptation capacity is morally urgent and should be co-equal with mitigation — their voice is not in the room where legitimacy is defined.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, adaptation_practitioners, excluded,
    organized, generational, constrained, global).

% Economists and policy analysts arguing decoupling is infeasible and climate response requires deliberate economic contraction are systematically excluded from policy conversations that admit only mitigation-vs-adaptation as legitimate options. Their theoretical arguments (decoupling historically does not keep pace with growth, renewable scale-up faces hard physical limits, carbon pricing is captured by incumbent interests) are dismissed as politically non-viable rather than empirically evaluated. Their entry to the conversation would fundamentally reframe what counts as legitimate climate response.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, degrowth_economists, excluded,
    organized, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__mitigation_priority, incumbent_energy_producers).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global emissions reduction effort by establishing a legitimacy framework that offers wealthy nations and energy producers a pathway to climate response that preserves growth, jobs, and consumption patterns. It solves the collective-action problem of persuading high-emitting nations to incur transition costs by promising that technological solutions will do the heavy lifting — avoiding the need to coordinate consumption reductions or structural economic shifts that would face stronger domestic political resistance.
% TRANSFER_FUNCTION: Transfers the risk of decoupling failure from wealthy nations and present generations to future generations and developing nations. It also transfers capital and policy attention from adaptive infrastructure to renewable technology deployment, benefiting entrepreneurs and energy companies. It transfers jobs and livelihoods from fossil fuel workers to (promised but uncertain) clean technology employment.
% ABSENT_VOICES: Degrowth economists, adaptation practitioners focused on climate resilience, community-based climate justice organizations, and indigenous peoples whose land is used for renewable infrastructure without consent are structurally excluded from the policy conversations that define this commitment. Fossil fuel workers have a seat at transitional-justice discussions but not at the table defining whether decoupling is feasible.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority commitment disappeared, the entire architecture of climate policy would reorganize: carbon pricing might collapse, renewable subsidies would be reframed as industrial policy rather than climate necessity, and debate would reopen on whether adaptation and degrowth pathways were co-equal legitimate responses. The political economy of energy would shift; capital would redistribute from tech entrepreneurs toward either adaptation infrastructure or growth-preservation strategies. The constraint's disappearance would trigger fundamental disagreement about what climate legitimacy means.
% FOUNDING_PROBLEM: Atmospheric CO2 accumulation creates warming risk; global emissions continue to rise despite scientific warnings; wealthy nations and energy producers face both climate risk and pressure to act, but large-scale material contraction is politically intolerable to electorates, so a framing that promises climate safety without growth sacrifice became politically necessary.
% FOUNDING_PROBLEM_CORROBORATION: Wealthy-nation governments and energy producers attest the founding problem requires mitigation-priority response. Climate scientists attest CO2 accumulation is real and warming is occurring. Degrowth economists and adaptation researchers attest that the founding problem cannot be solved by decoupling alone — that it requires either deliberate contraction or acceptance of higher warming, with attendant adaptation needs — this corroboration comes from outside the beneficiary set and directly contests the adequacy of the mitigation-priority framing.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.68 endpoint) reflects the constraint's mechanism: it extracts from future generations by transferring decoupling-failure risk to them, extracts from developing nations by constraining their growth pathways, extracts from fossil fuel workers by redefining their livelihoods as acceptable losses. The suppression score (0.71) is high because the commitment's persistence depends on actively suppressing alternative framings (degrowth, adaptation-priority) as illegitimate policy options — the enforcement apparatus includes funding hierarchies, journal gatekeeping, and policy exclusion rules that keep alternatives off the negotiation table. Theater ratio (0.52 endpoint) reflects that roughly half of enforcement activity is performative: climate summits where pledges are made without credible enforcement, corporate net-zero commitments that rely on unverified offsets, renewable deployment announcements that mask continued fossil investment. The measurement trajectory shows extractiveness and theater both rising over 50 years as the gap between decoupling rhetoric and emissions outcomes widens — the commitment becomes more theatrically important as it becomes less functionally adequate. This is the classic Goodhart/piton pattern: as the original function (emissions actually decoupling) fails to materialize, enforcement shifts to maintaining the legitimacy frame itself (theater rises, suppression of alternatives rises). The shared time grid ensures every metric is authored at all six time points so temporal analysis has a complete picture. Basis tags distinguish observed recent history from projected future: early measurements (0-30) are grounded in documented policy trajectories and deployment data; later measurements (40-50) are author projections about how the commitment's theater and extraction likely evolve as decoupling targets recede.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (incumbent energy producers, wealthy-nation governments), the mitigation-priority commitment solves a real collective-action problem: it created a legitimacy frame that persuaded high-emitting nations to incur transition costs, kept global climate negotiations from fragmenting into competing blocs, and enabled capital mobilization for renewable technology. From the payer seats (future generations, developing nations, fossil fuel workers), the same structure operates as an exclusionary mechanism: it institutionalized one reading of climate legitimacy while suppressing competing readings that would impose costs on the beneficiary seats. The commitment ENABLED action by making a particular action politically feasible for wealthy nations; the same mechanism SUPPRESSED action by defining other responses as illegitimate. This double movement — enabling one thing by suppressing others — is characteristic of tangled_rope: genuine coordination on one axis (mobilizing decarbonization action), genuine extraction on another axis (who bears the cost and who decides what counts as legitimate response).
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent energy producers and wealthy-nation electorates occupy beneficiary seats (d near 0.1-0.3): they collect from the commitment — preserved asset values, continued growth, job stability — and have exit options (they could pivot to degrowth or adaptation framing, but choose not to because it costs them more). Carbon tech entrepreneurs occupy beneficiary seats with even higher mobility (d near 0.2): they can exit into other industrial sectors but benefit enough from the current commitment to stay. Future generations and developing nations occupy full-target seats (d near 0.9-1.0): they bear costs they did not consent to, cannot negotiate terms, and have no exit. Fossil fuel workers occupy partial-target seats (d near 0.7-0.8): they bear direct costs (job loss), face constrained exit (geographic, skill-specific), and their voice is admitted but not determinative in policy. Scientists occupy observer seats (d = 0.5 or analytical): they serve observational/measurement functions but their epistemic interests (honest uncertainty communication) are in tension with the commitment's political need for optimism, pulling them toward accommodation. The per-seat directionality divergence is deliberate and structural: the same constraint looks like coordination to the beneficiary seats (we solved the collective-action problem of motivating wealthy nations to act on climate) and like enforced extraction to the target seats (we transferred the cost of your climate action to our future and our growth model).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (CO2 accumulation, rising temperatures, energy transition needed) is live and real. The founding function (mobilizing global emissions reduction through a legitimacy frame that preserves growth and incumbent interests) was effective for 15-20 years in generating renewable deployment and keeping international climate agreements alive. But the constraint's success at its original function is not what persists — what persists is the legitimacy frame itself, now maintained theatrically even as the original function (actual decoupling) fails to materialize. By t=30-40, the measurement data show extractiveness plateauing and theater rising: the constraint is no longer primarily doing emissions reduction (that function partially failed) and is now primarily maintaining the political legitimacy of the wealthy nations' climate response. This is mandatrophy: the founding function atrophies, but the constraint persists because suppressing alternatives (degrowth, adaptation-priority) has become the primary agenda-setter interest, not solving the original coordination problem. The constraint does not need to be abandoned because it still serves extraction and legitimacy-frame maintenance, even though it fails to deliver on its founding promise of adequate decoupling. Classification as tangled_rope (not snare) is appropriate because real coordination did occur and real emissions reduction did happen, but the asymmetry (who bears transition costs, who captures energy capital, whose growth is preserved) makes it tangled — the rope frays when you pull on whose interests are actually being served.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_boundary,
    'Is absolute decoupling of emissions from growth in wealthy nations feasible at the required scales and timescales to meet climate targets without adaptation or degrowth?',
    'Empirical test: compare realized decoupling rates (consumption-adjusted, embodied-carbon included) against required decoupling rates for 1.5-2°C scenarios out to 2050. If realized rates persistently lag required rates, decoupling is not sufficient. Monitor renewable capacity deployment, grid modernization pace, and carbon capture deployment against model-required scales.',
    'If decoupling fails to materialize (empirically most likely outcome), future generations enter the victim set as promised and their function shifts from ''contingent on successful decoupling'' to ''bearing brunt of insufficient action.'' The commitment''s legitimacy frame collapses. If decoupling succeeds (lower confidence), the extraction to future generations evaporates and the constraint approaches pure rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_feasibility_boundary, empirical, 'Whether absolute decoupling of emissions from growth can occur at required scales without adaptation or degrowth.').

omega_variable(
    carbon_pricing_capture_dynamics,
    'Will carbon pricing mechanisms remain outside incumbent energy-producer influence, or will they be captured and defanged as they are implemented?',
    'Track carbon price levels over time relative to price required for 1.5°C pathway. Monitor policy changes that weaken mechanisms (offsets, exemptions, leakage). Compare incumbent producer behavior under different pricing regimes (high-price scenarios that would force divestment vs. low-price scenarios that allow continued investment).',
    'High capture risk means carbon pricing becomes another extraction mechanism for incumbent interests (they trade offsets, avoid divestment, profit from price volatility) rather than a decoupling driver. Low capture means the commitment''s market-mechanism component functions as designed. Capture degree directly modulates whether decoupling suffices or whether future generations must bear larger costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_pricing_capture_dynamics, empirical, 'Whether carbon pricing mechanisms can resist regulatory capture by incumbent energy producers.').

omega_variable(
    alternative_reading_suppression_mechanism,
    'Is the suppression of adaptation-priority and degrowth-priority readings a structural feature of the commitment, or a contingent political choice that could be reversed?',
    'Institutional analysis: if a change in political alignment (e.g., degrowth parties gaining electoral power) can reinstate suppressed readings as legitimate policy options, suppression is contingent. If the commitment''s internal logic structurally forecloses alternatives (e.g., through capital allocation to mitigation-only infrastructure), suppression is structural.',
    'If suppression is contingent, the commitment is more vulnerable to political overthrow and alternative framings could emerge. If suppression is structural, the commitment is more resilient but also more extractive (it actively maintains the exclusion of alternatives as part of normal operation). Structural suppression makes the constraint closer to snare than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_suppression_mechanism, conceptual, 'Whether suppression of alternative climate-response readings is structural to the mitigation-priority commitment or contingent on political power.').

omega_variable(
    future_generation_consent_impossibility,
    'Does the transfer of decoupling-failure risk to future generations violate a moral principle that intergenerational commitment should not transfer unconsented-to risk?',
    'Philosophical/normative analysis with empirical input: if future-generation climate models show divergence scenarios (high-warming branch if decoupling fails), and if future generations have no voice in defining the commitment''s terms, does the commitment violate intergenerational justice? Different philosophical frameworks (utilitarian, rights-based, capability-approach) may reach different verdicts.',
    'If the principle is accepted, the commitment''s legitimacy is compromised by its intergenerational structure — it may be reclassified as snare (pure extraction from the voiceless) rather than tangled_rope. If the principle is rejected (future generations are necessarily governed by present decisions), the commitment retains legitimacy under this axis, though not others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generation_consent_impossibility, preference, 'Whether transferring decoupling-failure risk to future generations violates intergenerational justice principles.').

omega_variable(
    reading_container_foreclosure,
    'Does the mitigation-priority reading foreclose the adaptation-priority reading within a single commitment framework, or do they coexist as different policy emphases?',
    'Structural analysis: if acknowledging adaptation urgency (accepting some high-warming scenarios as inevitable) contradicts the claim that decoupling is sufficient, the readings are logically incompatible. If adaptation can be treated as co-equal with mitigation within a single framework, they coexist.',
    'If they are foreclosing, the mitigation-priority reading is in direct logical conflict with adaptation-priority. If they coexist, they compete politically but not logically. This determines whether the reading_relations edge is ''forecloses'' or ''coexists_with'' — a structural claim, not a preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_container_foreclosure, conceptual, 'Whether the mitigation-priority and adaptation-priority readings logically foreclose each other or coexist within the same commitment framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__mitigation_priority, theater_ratio, 10, 0.44).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__mitigation_priority, theater_ratio, 20, 0.49).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__mitigation_priority, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(clim_tr_t30, observed).
narrative_ontology:measurement(clim_tr_t40, climate_response_legitimacy__mitigation_priority, theater_ratio, 40, 0.54).
narrative_ontology:measurement_basis(clim_tr_t40, projected).
narrative_ontology:measurement(clim_tr_t50, climate_response_legitimacy__mitigation_priority, theater_ratio, 50, 0.56).
narrative_ontology:measurement_basis(clim_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(clim_be_t0, projected).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__mitigation_priority, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(clim_be_t10, projected).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__mitigation_priority, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(clim_be_t20, projected).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__mitigation_priority, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t40, climate_response_legitimacy__mitigation_priority, base_extractiveness, 40, 0.69).
narrative_ontology:measurement_basis(clim_be_t40, projected).
narrative_ontology:measurement(clim_be_t50, climate_response_legitimacy__mitigation_priority, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(clim_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__mitigation_priority, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__mitigation_priority, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__mitigation_priority, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(clim_su_t30, observed).
narrative_ontology:measurement(clim_su_t40, climate_response_legitimacy__mitigation_priority, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(clim_su_t40, projected).
narrative_ontology:measurement(clim_su_t50, climate_response_legitimacy__mitigation_priority, suppression_requirement, 50, 0.73).
narrative_ontology:measurement_basis(clim_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__mitigation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, carbon_pricing_policy__international).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, renewable_technology_deployment__scale_requirements).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, fossil_fuel_divestment__timeline_deferral).

% DUAL FORMULATION NOTE:
% The climate_response_legitimacy kernel decomposes into three structurally distinct constraints: mitigation_priority (this story — technological solutions + market mechanisms), adaptation_priority (accept warming, prioritize resilience infrastructure), and degrowth_transformation (require structural economic change). Each reading instantiates a different ε value, different victim/beneficiary structure, different classification. The three stories are linked by network.affects_constraints edges showing how each reading creates downstream pressure on the others' operating environments without logically foreclosing them (each remains a live position held by different parties). This decomposition follows ε-invariance: a single 'climate response legitimacy' story cannot have a stable ε because different readings measure different things (whether decoupling works, whether warming is acceptable, whether growth is compatible with emissions reduction). Three ε values, three constraint stories, one kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__mitigation_priority, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
