% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Climate Response Obligation — Degrowth Reading (Throughput Contraction; Sufficiency over Efficiency)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth reading of the
 *   climate_response_obligation kernel: the obligation to respond to climate
 *   disruption, read as a requirement to reduce absolute material throughput
 *   within planetary boundaries, with sufficiency prioritized over
 *   efficiency. Per the kernel-reading epsilon-referent rule, base
 *   extractiveness is authored over the STANDING arrangement this reading
 *   contests — the growth-dependent climate-response status quo
 *   (efficiency-first decoupling inside expanding throughput) — as the
 *   degrowth reading assesses it: highly extractive, with capital
 *   accumulation itself as the mechanism that transfers material space from
 *   the biosphere, the Global South, and future generations to present
 *   Northern consumption, while the official 'response' fails to reduce
 *   absolute flows. The emitted constraint (the throughput-contraction
 *   obligation itself) is claimed as tangled_rope: it solves a genuine
 *   commons problem (a real biophysical budget) while concentrating costs on
 *   identifiable seats (affluent Northern consumption, fossil-intensive
 *   capital, the green-growth technology coalition) and requiring binding
 *   enforcement. Claim and metrics are authored independently; the engine
 *   computes per-seat classifications from the structural data, and
 *   divergence between the reading's self-description (rope) and the computed
 *   type is exactly the measurement this story exists to take.
 *
 * KEY AGENTS:
 *   - future_generations: primary beneficiary (powerless/trapped) — inherits the boundary state; present only through proxy advocates
 *   - planetary_ecological_systems: beneficiary substrate (non-agent; powerless, universal scope) — registers its condition only through lagged physical feedback
 *   - global_south_developing_populations: conditional beneficiary and payer (organized/constrained) — development space preserved only if the North-first sequencing holds
 *   - low_income_northern_households: shielded beneficiary (powerless/trapped) — the sufficiency floor protects them first
 *   - affluent_global_north_households: primary target (powerful/arbitrage) — contraction aims at their consumption; resources buy partial arbitrage only
 *   - fossil_intensive_capital: target (institutional/arbitrage) — stranded assets; arbitrage runs through political channels
 *   - green_technology_industries: secondary target (powerful/constrained) — the decoupling premise their growth model rests on is what this reading rejects
 *   - degrowth_scholarly_movement: agenda setter (moderate/identity_locked) — defines the framework's terms; professionally fused with its core claims
 *   - national_governments: agenda setter and cost bearer (institutional/constrained) — sole enactment authority; bears concentrated electoral backlash for diffuse, future benefits
 *   - ipcc_style_assessment_bodies: analytical observer (analytical/analytical) — quantifies transgression and decoupling performance; enforces nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.82).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.65).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Climate Response Obligation — Degrowth Reading (Throughput Contraction; Sufficiency over Efficiency)").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, 'e7361780-9e56-4734-86d7-3daae2c27b30').
narrative_ontology:cs_kernel_codification('e7361780-9e56-4734-86d7-3daae2c27b30', distributed).
narrative_ontology:cs_authority_grounding('e7361780-9e56-4734-86d7-3daae2c27b30', distributed).
narrative_ontology:cs_reading_relation('e7361780-9e56-4734-86d7-3daae2c27b30', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('e7361780-9e56-4734-86d7-3daae2c27b30', climate_response_obligation__adaptation_priority, forecloses).
narrative_ontology:cs_axiom('e7361780-9e56-4734-86d7-3daae2c27b30', foundational, sufficiency_precedes_efficiency).
narrative_ontology:cs_axiom_status(sufficiency_precedes_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('e7361780-9e56-4734-86d7-3daae2c27b30', sufficiency_precedes_efficiency, empirically_contingent).
narrative_ontology:cs_axiom('e7361780-9e56-4734-86d7-3daae2c27b30', foundational, north_first_contraction_justice).
narrative_ontology:cs_axiom_status(north_first_contraction_justice, holdable).
narrative_ontology:cs_axiom_grounding('e7361780-9e56-4734-86d7-3daae2c27b30', north_first_contraction_justice, deontological).
narrative_ontology:cs_reference_frame('e7361780-9e56-4734-86d7-3daae2c27b30', within_boundary_sufficiency_economy).
narrative_ontology:cs_drift_state('e7361780-9e56-4734-86d7-3daae2c27b30', contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('e7361780-9e56-4734-86d7-3daae2c27b30', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_ecological_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_developing_populations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, low_income_northern_households).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, affluent_global_north_households).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, fossil_intensive_capital).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, green_technology_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_developing_populations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, national_governments).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, steady_state_economics_doctrine).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, contraction_and_convergence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The biophysical substrate of the arrangement: climate stability, biodiversity, nitrogen and phosphorus cycles, freshwater and land systems. Material throughput reduction lowers the pressure on these systems directly. It holds no seat in any allocation decision and registers its condition only through lagged physical feedback — boundary transgressions and tipping events that arrive after the fact and cannot be appealed.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_ecological_systems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_ecological_systems).

% People not yet born who inherit the boundary state the present generation leaves. Every unit of throughput the present consumes is a unit they cannot; contraction now enlarges their remaining material and climatic space. They cannot vote, litigate, or exit the planet's climate system, and are present only through proxy advocates and constitutional experiments.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% The majority of humanity, most of it below any plausible sufficiency floor. The arrangement's justice condition — the North contracts first and hardest — preserves their development space and reduces climate harm they did little to cause. But throughput ceilings bind them too: their aspiration to material expansion is capped at convergence, and if the North-first sequencing fails in practice they bear the ceilings without having received the floor. Exit from the climate system is unavailable; diplomatic coordination (G77, climate-vulnerable country blocs) is their main lever.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_developing_populations, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, global_south_developing_populations, payer).

% Households in wealthy countries whose consumption already sits at subsistence-to-modest levels. Sufficiency floors and cap-and-dividend designs route resources toward them, and their energy and transport costs are the first shielded under contraction policies. They cannot exit the pricing systems they live inside and have the least capacity to absorb transition costs — which is why the floor is their protection.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, low_income_northern_households, beneficiary,
    powerless, biographical, trapped, national).

% The top consumption deciles in wealthy countries, whose lifestyles account for a disproportionate share of material footprint. The arrangement aims contraction at their consumption specifically: frequent flying, large housing, meat-intensive diets, high-turnover goods. Their resources buy partial arbitrage — offsets, jurisdiction shopping, luxury carve-outs, political donations against caps — but the obligation is aimed at consumption that cannot fully relocate, since footprint follows them across borders under consumption-based accounting.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, affluent_global_north_households, payer,
    powerful, biographical, arbitrage, global).

% Corporations and asset holders whose balance sheets are sunk in throughput-expanding infrastructure: extraction, refining, cement, bulk shipping, combustion vehicle chains. Contraction strands reserves and devalues long-lived assets on schedules faster than depreciation. Their arbitrage runs through political channels — delay, capture of design processes, litigation, rebranding — because the physical asset base cannot relocate.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, fossil_intensive_capital, payer,
    institutional, biographical, arbitrage, global).

% Renewables, efficiency, and electrification sectors whose growth model rests on the decoupling premise: that clean supply can expand fast enough to power continued growth. The arrangement's sufficiency-first framing subordinates them — demand reduction shrinks the addressable market their valuations assume, and their political coalition with growth liberalism weakens. Their asset base and workforce are sunk in the transition buildout, limiting relocation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, green_technology_industries, payer,
    powerful, biographical, constrained, global).

% Post-growth economists, ecological economists, and movement organizations who define the arrangement's terms: boundary metrics, sufficiency floors, contraction schedules, North-first sequencing. They hold conferences, journals, and pilot projects but no state power. Their professional identities, careers, and community standing are built on the framework's core claims; abandoning throughput reduction as the obligation's core would dissolve their position, so exit from the framework is effectively unavailable to them.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_scholarly_movement, agenda_setter,
    moderate, generational, identity_locked, global).

% The only actors with legal authority to enact binding caps, rationing, and border adjustments. Enactment exposes them to concentrated electoral backlash from the households and sectors that pay — the paradigm case being fuel-tax protests — while the benefits of enactment are diffuse, future, and largely foreign. Treaty commitments and trade exposure limit unilateral exit; their electoral cycles run far shorter than the arrangement's time horizon.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, national_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, national_governments, payer).

% International scientific assessment bodies and Earth-system research networks that quantify boundary transgressions, material footprints, and decoupling performance. Their findings are the arrangement's empirical substrate — both its necessity case and its falsification conditions. They take no position on the obligation's distribution and can enforce nothing.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, ipcc_style_assessment_bodies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared biophysical budget: material throughput — energy, materials, land, water, absorption capacity — is finite and common, and uncoordinated expansion overdraws it for everyone. The arrangement solves the commons problem by setting aggregate caps and allocating contraction so the budget holds, with a sufficiency floor guaranteeing the underconsuming are not asked to contract below decent living standards.
% TRANSFER_FUNCTION: Moves consumption space: throughput entitlements shift from the top consumption deciles of wealthy countries and from throughput-expanding capital toward a global sufficiency floor, toward the Global South's development space, and toward the future — present overconsumption is converted into material room for the underconsuming and the not-yet-born.
% ABSENT_VOICES: Future generations are absent and represented only by proxy; the biosphere is absent entirely, its feedback arriving only as disaster. Within the present conversation, Global South development advocates object that ceilings without an enforced North-first sequence freeze existing inequality; growth-dependent labor movements object that contraction destroys livelihoods before sufficiency institutions exist; and efficiency-technology coalitions object that demand reduction forecloses the buildout their transition model requires.
% DISAPPEARANCE_RATIONALE: If the throughput-contraction obligation vanished overnight, the climate response contest would reorganize around the sibling readings: technology-led decarbonization within growth would reclaim the agenda, adaptation investment would scale as harms land, and material throughput would keep expanding under decoupling assumptions. The specific burden structure — Northern consumption contracts first, sufficiency floor for the underconsuming, capital accumulation itself treated as the thing to shrink — would disappear with it, and the Global South's conditional development space would revert to a bargaining chip.
% FOUNDING_PROBLEM: The arrangement was built to solve a failure the efficiency-first response could not: absolute material throughput keeps rising while economies decouple only relatively, and planetary boundaries — climate, biodiversity, nitrogen, land — are transgressed. The founding problem is how to contract total material flows to within biophysical limits justly, protecting both the underconsuming and the not-yet-born.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and the Earth-system planetary-boundary literature corroborate the founding problem from outside any benefiting party: transgressions are measured, and demand-side reduction now appears even in mainstream mitigation pathways (including IEA net-zero work) as necessary. What no one outside the degrowth coalition corroborates is the strong claim that sufficiency-over-efficiency is the uniquely required form of response — mitigation-priority and adaptation-priority parties attest the problem while rejecting this reading's prescription, which is precisely the contest the kernel organizes.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.82) is authored over the standing growth-dependent arrangement per the kernel-reading referent rule — never over this reading's endorsed alternative, which would drive every advocacy reading to epsilon near zero. Suppression (0.65) is the emitted constraint's own structural enforcement requirement — binding caps, entitlement systems, border adjustments against defection — authored raw and unscaled; only extractiveness is scaled by directionality and scope downstream. Theater (0.35) reflects an arrangement whose current operation is largely discursive: sufficiency language mainstreams faster than throughput falls (wellbeing budgets and beyond-GDP initiatives coexist with record material footprints). Accessibility collapse (0.6) is conditional: within the reading's premises (boundaries binding, decoupling insufficient at required rates) the efficiency-only alternative collapses; but the premise is itself the contest, so alternatives persist for every seat that rejects the premise. Resistance (0.78) is the arrangement's best-documented property — fuel-tax backlashes, growth-labor coalitions, Southern equity objections, green-growth counter-framing. All three measurement series share one time grid (interval 0-15, approximately 2009-2025, the planetary-boundaries-era degrowth discourse): the reading's assessed extraction of the status quo rises as decoupling fails to materialize, theater rises as rhetoric mainstreams, and the enforcement intensity that actual contraction would require hardens as payer-side resistance organizes.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the agenda-setter seats (movement, governments) the arrangement is a necessary commons solution awaiting institutionalization; from the payer seats (affluent households, fossil capital, green technology) it is a confiscation of consumption space and asset value aimed at them specifically; from the beneficiary seats (future generations, South, low-income North) it is the first framework that names them as principals rather than externalities. Same-level divergence: affluent_global_north_households and green_technology_industries share a power atom but sit in opposite exit structures — household arbitrage is individual and consumptive (offsets, footprint relocation), while industry constraint is sunk and structural — so equal nominal power yields different leverage. Inter-institutional: national_governments and fossil_intensive_capital share the institutional atom but sit on opposite sides of the enforcement relation, and both claim the assessment bodies' analytical authority for their own framing. Coalition dynamics: the payer seats could form a blocking coalition (and partially have — growth-labor alliances against contraction policy), which is the structural source of the high resistance score. Identity-lock: the movement's fusion is professional-ideological — careers, journals, and community standing are constituted by the framework's core claims; if the frame broke (decoupling proven feasible at required rates), the agenda-setter seat would dissolve rather than migrate, which is why the decoupling dispute is the story's load-bearing omega.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive d. future_generations and low_income_northern_households derive near-beneficiary d (powerless, trapped, declared beneficiaries); planetary_ecological_systems is excluded from derivation as a non-agent (agent: false) — it benefits but cannot hold a directionality. Four overrides correct derivations that would land wrong. (1) powerful -> 0.78: victim status plus arbitrage-grade exit would damp affluent_global_north_households toward the beneficiary end, but their arbitrage is intra-arrangement (offsets, footprint relocation) and cannot remove them from an obligation aimed at consumption itself; green_technology_industries shares the atom as a co-targeted payer whose growth premise this reading forecloses. (2) organized -> 0.4: global_south_developing_populations is declared beneficiary with constrained exit, which would derive near-beneficiary d, but the arrangement binds their future throughput conditionally on a North-first sequence that is design-dependent — the override encodes the conditional burden. (3) institutional -> 0.7: the derivation cannot split same-atom seats; fossil_intensive_capital deserves roughly 0.85 (its assets are the enforcement object) and national_governments roughly 0.55 (agenda-setter bearing concentrated political costs); the override sits between, weighted to the story's target-side institutional mass. (4) moderate -> 0.45: degrowth_scholarly_movement is an agenda-setter with no canonical structural data; near-symmetric d reflects bearing the arrangement's political costs while collecting scholarly capital from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions of mislabeling. Reading the arrangement as pure extraction erases its genuine coordination function — the biophysical budget is real, and uncoordinated expansion overdraws it for everyone, including the payers' descendants; the tangled_rope structure keeps the commons problem on the books alongside the asymmetric burden. Reading it as pure coordination (the reading's own preferred self-description) erases the identifiable seats that pay through the same structure that coordinates everyone else — which is why the claim is authored tangled_rope rather than rope despite the movement's rope self-understanding. Mandatrophy: the founding problem is live (transgression is measured and worsening), so no mandatrophy is declared. The real atrophy risk is tracked by the theater series: if binding instruments never arrive, the arrangement degenerates into sufficiency rhetoric maintained performatively by institutions that adopt its language without its obligations — at which point the piton signature (theater dominant, diffuse gains, no seat both able and willing to fix it) becomes accurate. The mismatch consumer should watch founding_problem_status against the theater trajectory: status=live with rising theater is the pre-atrophy state, and a flip to status=dead while the discourse persists would date the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the climate_response_obligation kernel — what would the sibling readings (mitigation_priority, adaptation_priority) change structurally if adopted as the operative constraint instead?',
    'Comparative classification across the three reading-stories: mitigation_priority shifts the victim set toward present-and-future climate-harm bearers while retaining growth-compatible consumption and efficiency-led means; adaptation_priority shifts beneficiaries toward present resilience-builders and victims toward future generations and the Global South; this reading uniquely places affluent Northern consumption patterns and capital accumulation itself inside the extraction structure and makes sufficiency a requirement rather than a co-benefit.',
    'If a sibling reading is adopted, the victim set, burden distribution, and enforcement object all change; this reading''s specific obligations (absolute throughput contraction, sufficiency floor, North-first sequencing) drop out, and its beneficiary structure reverts to the siblings'' distributions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story instantiates the degrowth reading of the climate response obligation kernel; the siblings are separate constraints, not parts of this one.').

omega_variable(
    decoupling_empirical_dispute,
    'Is absolute decoupling of material throughput and emissions from economic output achievable globally at the rates required to stay within planetary boundaries — the empirical premise on which this reading''s necessity claim stands or falls?',
    'Multi-decade observation of consumption-based material footprints and emissions intensity against GDP in developed economies; natural experiments from absolute-cap jurisdictions; integrated assessment model comparisons of demand-side versus efficiency-only pathways.',
    'If decoupling at required rates is achievable, the contraction obligation''s necessity claim collapses toward the mitigation reading (efficiency-led decarbonization suffices) and the Northern-consumption victim set loses its justification; if not achievable, this constraint is forced and both sibling readings are structurally insufficient as responses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_empirical_dispute, empirical, 'The load-bearing empirical dispute underneath the reading: feasibility of absolute decoupling at required rates.').

omega_variable(
    north_first_sequencing_enforceability,
    'Can the North-first contraction sequencing — Global South development space preserved until convergence — be enforced in practice, or does the arrangement as specified freeze existing global inequality under a sufficiency ceiling?',
    'Design analysis and track record of candidate enforcement instruments (border carbon adjustments, cap-and-share, conditionality of technology and finance transfer); analysis of Global South negotiating positions on common-but-differentiated responsibilities.',
    'If unenforceable, global_south_developing_populations shifts from conditional beneficiary to structural victim and the arrangement''s justice premise fails, pushing the burden distribution toward the snare end; if enforceable, the conditional-beneficiary designation holds and the tangled structure is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_first_sequencing_enforceability, preference, 'Whether the arrangement''s intragenerational justice condition is design-stable or aspirational cover.').

omega_variable(
    sufficiency_floor_definition,
    'Where does the sufficiency floor sit — the consumption level below which no one is asked to contract? The victim/beneficiary boundary of this arrangement depends on an underdetermined parameter.',
    'Convergence on a defensible floor via decent-living-standards and wellbeing-needs research, participatory specification, and sensitivity analysis of victim-set membership across candidate floor levels.',
    'A high floor shrinks the victim set to a thin global affluence layer and the arrangement reads as targeted restitution; a low floor sweeps Global South aspiration and Northern working classes into the victim set and the burden distribution reads as broadly extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sufficiency_floor_definition, conceptual, 'The floor parameter underdetermines the arrangement''s victim set and therefore its per-seat classifications.').

omega_variable(
    future_generations_representation,
    'Is the future-generations beneficiary seat a genuine structural principal, or a rhetorical proxy through which present advocacy coalitions — including the movement that authors this arrangement — advance present interests?',
    'Institutional design evidence: do proxy mechanisms (ombudspersons for future generations, constitutional environmental rights, litigation standing) change outcomes in the direction the seat''s interests predict, independent of the proxy''s present-day sponsors?',
    'If proxy, part of the declared beneficiary structure is cover, the effective beneficiary set shrinks to present low-consumption populations, and directionality for the seat should be discounted; if genuine, the intergenerational transfer is real and the seat''s near-beneficiary directionality stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_representation, conceptual, 'Authenticity of the intergenerational beneficiary seat.').

omega_variable(
    degrowth_theater_trajectory,
    'Is the rising theater ratio a transient mainstreaming artifact or the arrangement''s terminal state — sufficiency rhetoric substituting permanently for throughput reduction?',
    'Track the gap between sufficiency-adjacent official discourse and consumption-based material footprint in adopting jurisdictions; if discourse rises while footprints flatline, the theater is terminal.',
    'If terminal, the arrangement degrades toward theatrical maintenance of obligations never made operative (piton-side drift with no capturer); if transient, theater falls as binding instruments arrive and the tangled_rope structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_theater_trajectory, empirical, 'Whether sufficiency rhetoric is becoming a substitute for contraction rather than a precursor to it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__degrowth_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t3, climate_response_obligation__degrowth_reading, theater_ratio, 3, 0.19).
narrative_ontology:measurement_basis(clim_tr_t3, observed).
narrative_ontology:measurement(clim_tr_t6, climate_response_obligation__degrowth_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(clim_tr_t6, observed).
narrative_ontology:measurement(clim_tr_t9, climate_response_obligation__degrowth_reading, theater_ratio, 9, 0.28).
narrative_ontology:measurement_basis(clim_tr_t9, observed).
narrative_ontology:measurement(clim_tr_t12, climate_response_obligation__degrowth_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement_basis(clim_tr_t12, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_obligation__degrowth_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(clim_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__degrowth_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t3, climate_response_obligation__degrowth_reading, base_extractiveness, 3, 0.72).
narrative_ontology:measurement_basis(clim_be_t3, observed).
narrative_ontology:measurement(clim_be_t6, climate_response_obligation__degrowth_reading, base_extractiveness, 6, 0.76).
narrative_ontology:measurement_basis(clim_be_t6, observed).
narrative_ontology:measurement(clim_be_t9, climate_response_obligation__degrowth_reading, base_extractiveness, 9, 0.79).
narrative_ontology:measurement_basis(clim_be_t9, observed).
narrative_ontology:measurement(clim_be_t12, climate_response_obligation__degrowth_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement_basis(clim_be_t12, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_obligation__degrowth_reading, base_extractiveness, 15, 0.82).
narrative_ontology:measurement_basis(clim_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__degrowth_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t3, climate_response_obligation__degrowth_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement_basis(clim_su_t3, observed).
narrative_ontology:measurement(clim_su_t6, climate_response_obligation__degrowth_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(clim_su_t6, observed).
narrative_ontology:measurement(clim_su_t9, climate_response_obligation__degrowth_reading, suppression_requirement, 9, 0.6).
narrative_ontology:measurement_basis(clim_su_t9, observed).
narrative_ontology:measurement(clim_su_t12, climate_response_obligation__degrowth_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement_basis(clim_su_t12, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_obligation__degrowth_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(clim_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'climate response obligation' covers three structurally distinct constraints emitted by one kernel under three readings. They share the empirical substrate (assessment-body findings on transgression and decoupling performance) but differ in epsilon, victim set, and enforcement object: this reading uniquely places affluent Northern consumption patterns and capital accumulation itself inside the extraction structure and makes sufficiency a requirement rather than a co-benefit. Family members are linked via affects_constraints; each reading's file documents the same decomposition from its own seat. The upstream empirical claims (boundary transgression, decoupling shortfall) are cited by this reading as evidence for its downstream normative structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, powerful, 0.78).
constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, organized, 0.4).
constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, institutional, 0.7).
constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
