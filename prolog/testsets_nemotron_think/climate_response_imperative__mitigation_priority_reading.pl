% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Climate Response as Mitigation-First via Innovation and Markets
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The mitigation-priority reading of the climate response imperative frames
 *   climate action as primarily an emissions reduction challenge solvable
 *   through technological innovation (renewables, batteries, CDR, hydrogen)
 *   and market mechanisms (carbon pricing, trading, Article 6). Adaptation is
 *   treated as residual — necessary only where mitigation falls short. This
 *   reading dominates the UNFCCC architecture, IPCC scenario space, and
 *   Global North climate finance. Its structural logic transfers the cost of
 *   mitigation failure (insufficient CDR deployment, temperature overshoot)
 *   to future generations and vulnerable regions as deferred adaptation
 *   burden, while directing financial returns to Global North innovation
 *   sectors and carbon market intermediaries. The reading presents itself as
 *   pragmatic coordination; its operation extracts asymmetrically.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.72).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.65).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Climate Response as Mitigation-First via Innovation and Markets").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '0a5bd949-163a-4ad6-a4fc-b7b08aaed95c').
narrative_ontology:cs_kernel_codification('0a5bd949-163a-4ad6-a4fc-b7b08aaed95c', distributed).
narrative_ontology:cs_authority_grounding('0a5bd949-163a-4ad6-a4fc-b7b08aaed95c', distributed).
narrative_ontology:cs_reading_relation('0a5bd949-163a-4ad6-a4fc-b7b08aaed95c', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a5bd949-163a-4ad6-a4fc-b7b08aaed95c', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('0a5bd949-163a-4ad6-a4fc-b7b08aaed95c', foundational, technological_innovation_sufficiency).
narrative_ontology:cs_axiom_status(technological_innovation_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('0a5bd949-163a-4ad6-a4fc-b7b08aaed95c', technological_innovation_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('0a5bd949-163a-4ad6-a4fc-b7b08aaed95c', secondary, market_mechanisms_efficiency).
narrative_ontology:cs_axiom_status(market_mechanisms_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('0a5bd949-163a-4ad6-a4fc-b7b08aaed95c', market_mechanisms_efficiency, conventional).
narrative_ontology:cs_reference_frame('0a5bd949-163a-4ad6-a4fc-b7b08aaed95c', kyoto_paris_architecture).
narrative_ontology:cs_drift_state('0a5bd949-163a-4ad6-a4fc-b7b08aaed95c', post_paris_implementation_gap, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0a5bd949-163a-4ad6-a4fc-b7b08aaed95c', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, fossil_fuel_companies_with_ccs_investments).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, vulnerable_regions_global_south).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, indigenous_communities).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, adaptation_underfunded_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, unfccc_parties_global_south).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, technological_optimism_doctrine).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, market_efficiency_in_climate_policy).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, green_growth_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive massive public subsidies, tax credits, and carbon market revenues for renewable energy, battery, carbon capture, and hydrogen technologies. Their business models depend on the mitigation-priority framing directing climate finance toward technology deployment rather than demand reduction or adaptation finance. They can pivot across sectors and geographies if policy signals shift.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, beneficiary,
    powerful, biographical, arbitrage, global).

% Banks, verification bodies, registries, and brokers that extract fees from every ton of carbon traded under Article 6 and voluntary markets. Their revenue scales with the mitigation-priority architecture's reliance on offsetting and trading rather than direct regulation. They lobby for market expansion and oppose non-market approaches.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Major oil and gas companies that have invested heavily in carbon capture and storage (CCS) and blue hydrogen. They shape the mitigation agenda through lobbying, IPCC engagement, and national net-zero strategies that treat CCS as essential. Their stranded asset risk is managed by the mitigation-priority framing's reliance on technological carbon removal rather than fossil fuel phaseout.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, fossil_fuel_companies_with_ccs_investments, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, fossil_fuel_companies_with_ccs_investments, beneficiary).

% Bear the deferred costs of inadequate adaptation and the risk of CDR non-delivery. They have no voice in current negotiations, no exit from the climate system, and no mechanism to hold present actors accountable. The mitigation-priority reading's bet on future CDR deployment transfers the burden of overshoot to them.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_imperative__mitigation_priority_reading, future_generations).

% Small island states, least developed countries, and climate-vulnerable regions that face existential losses while adaptation finance remains a fraction of promised amounts. They are structurally dependent on Global North mitigation ambition and finance flows they cannot control. Their negotiating power in UNFCCC is real but asymmetrical.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, vulnerable_regions_global_south, payer,
    moderate, biographical, constrained, regional).

% Experience both direct climate impacts and displacement from mitigation projects (renewable energy mega-projects, carbon offset plantations, conservation enclosures). Their territorial rights and knowledge systems are recognized in rhetoric but overridden in practice. Exit is identity-locked: their relationship to land is constitutive, not optional.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, indigenous_communities, payer,
    powerless, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, indigenous_communities, excluded).

% Coastal, agricultural, and urban poor communities in both Global South and North that face escalating climate risks while adaptation finance is captured by mitigation-priority instruments. They have no mobility, no political leverage, and no access to the innovation economy's benefits.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, adaptation_underfunded_communities, payer,
    powerless, immediate, trapped, local).

% Transnational networks demanding reparative justice, adaptation finance, and structural transformation. They are present at COPs but excluded from the core mitigation-priority negotiation track (Article 6, technology mechanism, net-zero accounting). Their exclusion is structural: the architecture treats justice as a side-event, not a design constraint.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_justice_movements, excluded,
    organized, biographical, constrained, global).

% Produces the assessment reports that legitimize the mitigation-priority pathway (e.g., AR6 WGIII scenarios heavily reliant on CDR). Individual scientists dissent, but the institutional process converges on pathways that preserve the innovation-market framing. Their authority derives from expertise, but their scenario architecture encodes the reading's axioms.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, ipcc_scientific_community, observer,
    analytical, generational, analytical, universal).

% Wealthy country negotiators who designed the Kyoto-Paris architecture around market mechanisms, technology transfer, and net-zero accounting. They control the agenda, the finance flows, and the scenario space. Their domestic politics constrain ambition but their structural position is agenda-setting.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, unfccc_parties_global_north, agenda_setter,
    institutional, biographical, arbitrage, global).

% Developing country negotiators who must operate within the mitigation-priority framework to access any climate finance. They advocate for adaptation and loss-and-damage within a system that treats these as residual. Their coalition power (G77+China, AOSIS, LDCs, African Group) is real but constrained by the architecture's design.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, unfccc_parties_global_south, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, unfccc_parties_global_south, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating global emissions reduction through technology deployment incentives and market-based carbon pricing rather than direct regulation, rationing, or structural economic transformation. Solves the collective action problem of who reduces how much by substituting price signals and innovation subsidies for negotiated burden-sharing.
% TRANSFER_FUNCTION: Transfers adaptation burden and climate risk from present Global North emitters to future generations and vulnerable regions via deferred adaptation finance and CDR reliance; transfers financial returns and policy rents to innovation sectors, carbon market intermediaries, and fossil fuel incumbents with CCS investments.
% ABSENT_VOICES: Future generations (temporally excluded by definition), indigenous knowledge holders (epistemically excluded from scenario architecture), frontline communities in sacrifice zones for mitigation projects (territorially excluded), and the majority of Global South populations whose adaptation needs are deprioritized in finance allocation.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority architecture vanished overnight, the UNFCCC would lose its central organizing logic: NDCs, Article 6, technology mechanism, and net-zero accounting would collapse. Climate policy would reorganize around either adaptation-first bilateralism, degrowth/post-growth frameworks, or chaotic uncoordinated national responses. The $100B+ annual climate finance architecture is built on this constraint.
% FOUNDING_PROBLEM: Achieving global emissions reduction without disrupting Global North economic growth, requiring structural redistribution of consumption, or confronting the political power of fossil fuel incumbents. The Kyoto Protocol's market mechanisms were the founding institutional solution.
% FOUNDING_PROBLEM_CORROBORATION: The Global North (UNFCCC Annex I parties, OECD, IPCC WGIII scenario architecture) attests the problem is live: innovation and markets remain the only scalable path. The Global South negotiating blocs (AOSIS, LDC Group, African Group, LMDC) attest the founding problem was always a Northern framing that externalized costs; climate justice scholarship (e.g., Roberts & Parks, Klinsky, Newell et al.) corroborates that the mitigation-priority architecture was designed to avoid addressing historical responsibility and consumption inequality.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__mitigation_priority_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint's core bet on unproven CDR at scale transfers overshoot risk to victims who cannot consent or exit. Suppression (0.65) is substantial because the architecture actively marginalizes adaptation finance, loss-and-damage, and demand-side measures through scenario design, finance rules, and negotiation agenda control. Theater (0.48) is rising as net-zero pledges proliferate while emissions and CDR gaps widen — performative compliance substitutes for delivery. Accessibility collapse (0.55) reflects that alternative framings (adaptation-priority, degrowth) exist but are structurally excluded from the scenario architecture and finance flows. Resistance (0.58) comes from climate justice movements, Global South negotiators, and impacted communities but is contained within the architecture's side-events.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (Global North parties, innovation sectors), the constraint appears as genuine coordination: a working architecture channeling trillions into decarbonization. From the payer seats (future generations, vulnerable regions, indigenous communities), the same structure operates as extraction: their survival is collateral in a bet on technologies that may never scale. The engine computes this divergence from the declared power/exit/role structure — the claimed_type (tangled_rope) captures the dual nature; the metrics describe its current operating point.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North innovation sectors, carbon market intermediaries, and fossil fuel companies with CCS are structural beneficiaries (d ~ 0.15-0.25): they collect rents, shape rules, and have arbitrage-grade exit. Future generations are full targets (d = 1.0): trapped, identity-locked to the climate system, zero exit. Vulnerable Global South regions and indigenous communities are high-target (d ~ 0.75-0.85): constrained exit, identity-locked to territory, dependent on finance they don't control. Climate justice movements are excluded (not in the directionality computation). IPCC scientific community is analytical (d = 0.5): observes full structure but institutional incentives align with the reading. UNFCCC Global North parties are agenda-setters with beneficiary-directionality; Global South parties are payers with constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (emissions reduction without growth disruption) was live in 1997. By 2025, the emissions gap, CDR non-delivery, and adaptation finance shortfall constitute strong evidence the founding problem is dead or superseded — the architecture fails its own metric. Yet the constraint persists and intensifies (rising extractiveness, theater, suppression). This is mandatrophy: the mandate (mitigation-first via markets/innovation) has outlived its function but persists because beneficiaries capture the rents and payers lack exit. The founding_problem_status = contested reflects the structural dispute: beneficiaries declare it live; victims and corroborators declare it dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is this constraint a distinct structural entity or merely a perspective on the shared kernel climate_response_imperative?',
    'Compare ε-invariance across readings: if mitigation_priority_reading, adaptation_priority_reading, and degrowth_reading yield materially different ε, beneficiary/victim sets, and coordination functions when assessed against the same standing arrangement, they are distinct constraints linked by network.affects_constraints, not perspectives on one constraint.',
    'If they are distinct constraints, each gets its own classification and the kernel is a family label only. If they are perspectives, the framework must model perspective-relative classification (which it does not).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Commitment-system frame: this story instantiates one reading of a contested kernel; sibling readings are other constraint stories.').

omega_variable(
    cdr_feasibility_at_scale,
    'Will carbon dioxide removal technologies (BECCS, DACCS, enhanced weathering) deploy at the gigaton-scale assumed by mitigation-priority scenarios (IPCC AR6 C1-C3 pathways)?',
    'Empirical tracking of CDR deployment vs. scenario milestones; engineering-economic analysis of energy, land, and water constraints; monitoring of permanent storage verification.',
    'If CDR fails at scale, the mitigation-priority reading''s extractiveness is structurally higher than authored (overshoot becomes locked-in, adaptation burden on victims becomes catastrophic). If CDR succeeds, the reading''s coordination function is partially vindicated and extractiveness decreases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cdr_feasibility_at_scale, empirical, 'The central empirical bet of the mitigation-priority reading; its failure mode is the primary extraction mechanism.').

omega_variable(
    adaptation_cost_compensation_possibility,
    'Can the deferred adaptation costs transferred to vulnerable regions and future generations ever be fairly compensated within the mitigation-priority architecture?',
    'Track adaptation finance flows vs. needs (UNEP Adaptation Gap Report); assess loss-and-damage fund operationalization; evaluate whether carbon market revenues reach adaptation at scale.',
    'If compensation is structurally impossible (time lag, non-identity problem, political economy of finance), the reading''s extraction is non-rectifiable — a snare feature. If compensation mechanisms can be built, the tangled_rope classification holds with potential for rebalancing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_cost_compensation_possibility, conceptual, 'Whether the asymmetric extraction has a rectification pathway or is structurally irreversible.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of adaptation-priority and degrowth framings structural (institutional rules, finance architecture, scenario design) or internalized (cognitive capture of Global South elites, NGO professionalization, epistemic narrowing of climate economics)?',
    'Compare suppression of alternative framings in UNFCCC formal agenda vs. side-events; analyze citation networks in IPCC reports; track career incentives in climate policy academia.',
    'If internalized, suppression persists even if formal rules change — the constraint''s effective suppression is higher than structural measures suggest. If purely structural, rule changes (e.g., adaptation goal operationalization) could reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the climate policy epistemology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 1997, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_priority_tr_t1997, climate_response_imperative__mitigation_priority_reading, theater_ratio, 1997, 0.2).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2005, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2015, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2021, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2021, 0.44).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2025, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2025, 0.46).
narrative_ontology:measurement(climate_mitigation_priority_tr_t2030, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2030, 0.48).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_priority_be_t1997, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 1997, 0.35).
narrative_ontology:measurement(climate_mitigation_priority_be_t2005, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(climate_mitigation_priority_be_t2015, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(climate_mitigation_priority_be_t2021, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2021, 0.66).
narrative_ontology:measurement(climate_mitigation_priority_be_t2025, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2025, 0.7).
narrative_ontology:measurement(climate_mitigation_priority_be_t2030, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2030, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_priority_su_t1997, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 1997, 0.4).
narrative_ontology:measurement(climate_mitigation_priority_su_t2005, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(climate_mitigation_priority_su_t2015, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(climate_mitigation_priority_su_t2021, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(climate_mitigation_priority_su_t2025, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2025, 0.63).
narrative_ontology:measurement(climate_mitigation_priority_su_t2030, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2030, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__mitigation_priority_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint, adaptation_priority_reading, and degrowth_reading form the climate_response_imperative constraint family. They share the kernel 'climate response is imperative' but decompose into structurally distinct constraints with different ε, beneficiary/victim sets, and coordination functions. mitigation_priority_reading has highest extractiveness (CDR reliance, deferred adaptation); adaptation_priority_reading has lower extractiveness but higher suppression (requires massive North-to-South finance transfers); degrowth_reading has lowest extractiveness but highest accessibility_collapse (demands structural transformation). The upstream constraint (mitigation_priority_reading) influences downstream siblings by setting the scenario architecture and finance rules they must contest within.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__mitigation_priority_reading, institutional, 0.2).
constraint_indexing:directionality_override(climate_response_imperative__mitigation_priority_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
