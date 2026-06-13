% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Reading: Nuclear-Centric Decarbonization Mandate
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel: the
 *   claim that reliable decarbonization requires large-scale dispatchable
 *   baseload power that renewables cannot provide. This reading — the
 *   'baseload necessity reading' — shapes global climate policy, grid
 *   architecture, and infrastructure investment. Nuclear power operators and
 *   grid planners benefit from this framing by securing capital, regulatory
 *   priority, and technical authority. Renewable energy developers and
 *   distributed generation advocates pay the cost: their technologies are
 *   classified as 'supplements' rather than primary solutions, grid rules are
 *   designed around baseload assumptions, and alternative decarbonization
 *   pathways (renewable-primary, demand reduction) are filtered out of
 *   official policy. The constraint is CLAIMED as Tangled Rope (genuine
 *   coordination function: solving real grid stability challenges) while the
 *   metrics describe substantially extractive, actively enforced operation
 *   that depends on suppressing technical evidence from alternative readings.
 *   This divergence is the deliberate signal the corpus exists to measure.
 *
 * KEY AGENTS:
 *   - nuclear_power_operators: institutional beneficiary, controls policy agenda through technical authority
 *   - grid_planners_with_dispatchability_mandates: agenda-setting institutional actor, enforces assumptions through grid codes and interconnection rules
 *   - renewable_energy_developers: payer, classified as inadequate, face interconnection barriers and grid rules designed for baseload
 *   - carbon_intensive_industry: powerful beneficiary, benefits from narrative that keeps electricity demand intact rather than forcing industrial process change
 *   - distributed_generation_advocates: excluded from planning, voice suppressed in official decarbonization forums
 *   - energy_modeling_institutions: agenda-setter, embeds baseload assumptions in modeling frameworks treated as technical discovery
 *   - research_institutions_studying_renewable_feasibility: observer, findings that renewable grids work are treated as anomalies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.72).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Reading: Nuclear-Centric Decarbonization Mandate").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, '9f458540-aac2-45e1-8088-b513a3f67d5e').
narrative_ontology:cs_kernel_codification('9f458540-aac2-45e1-8088-b513a3f67d5e', distributed).
narrative_ontology:cs_authority_grounding('9f458540-aac2-45e1-8088-b513a3f67d5e', extraction).
narrative_ontology:cs_reading_relation('9f458540-aac2-45e1-8088-b513a3f67d5e', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('9f458540-aac2-45e1-8088-b513a3f67d5e', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f458540-aac2-45e1-8088-b513a3f67d5e', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('9f458540-aac2-45e1-8088-b513a3f67d5e', foundational, dispatchable_baseload_necessary_for_grid_reliability).
narrative_ontology:cs_axiom_status(dispatchable_baseload_necessary_for_grid_reliability, holdable).
narrative_ontology:cs_axiom_grounding('9f458540-aac2-45e1-8088-b513a3f67d5e', dispatchable_baseload_necessary_for_grid_reliability, empirically_contingent).
narrative_ontology:cs_axiom('9f458540-aac2-45e1-8088-b513a3f67d5e', foundational, renewable_generation_insufficient_without_baseload_backing).
narrative_ontology:cs_axiom_status(renewable_generation_insufficient_without_baseload_backing, holdable).
narrative_ontology:cs_axiom_grounding('9f458540-aac2-45e1-8088-b513a3f67d5e', renewable_generation_insufficient_without_baseload_backing, empirically_contingent).
narrative_ontology:cs_reference_frame('9f458540-aac2-45e1-8088-b513a3f67d5e', grid_stability_requires_centralized_dispatchable_generation).
narrative_ontology:cs_drift_state('9f458540-aac2-45e1-8088-b513a3f67d5e', contemporary_high_renewable_penetration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9f458540-aac2-45e1-8088-b513a3f67d5e', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, carbon_intensive_industry_seeking_legitimacy).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, grid_planners_with_dispatchability_mandates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_generation_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, decentralized_energy_transition_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_consumers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_consumers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, grid_storage_technology_developers).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__baseload_necessity_reading, thermodynamic_grid_stability_requirement).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__baseload_necessity_reading, dispatchable_baseload_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate multi-billion-dollar power plants with 60+ year lifespans. Benefit from regulatory frameworks and technical standards that treat nuclear as the only viable decarbonization solution. Capital costs are recovered through guaranteed load factors, long-term power purchase agreements, and grid priority. Fund research and policy advocacy that positions baseload as essential. Their alternative would be competing on pure cost and efficiency metrics with renewables, which they cannot win — competitive exit is foreclosed by economics, making reliance on the baseload necessity reading strategic.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_operators, agenda_setter).

% Design electrical grid architecture, capacity planning, and interconnection standards on the assumption that dispatchable baseload is non-negotiable. This assumption is embedded in grid codes, modeling frameworks, and 20+ year infrastructure plans. Professional identity and institutional authority rest on managing a 'stable' grid, which is defined by baseload dispatch. Exit means re-engineering decades of prior work, re-validating models, and admitting that past analyses may have been incomplete. Constrained by path dependence and institutional inertia.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_planners_with_dispatchability_mandates, agenda_setter,
    institutional, biographical, constrained, regional).

% Steel, cement, chemicals, and mining industries use massive amounts of industrial electricity. Benefit from a decarbonization narrative that keeps large-scale electricity demand intact (via electrification of heat and processes) rather than forcing demand reduction or process transformation. The baseload necessity framing legitimizes continued high-consumption pathways. Their alternative would be facing pressure to restructure industrial processes, improve efficiency, or shift to lower-energy production methods — all costly and competitively threatening.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, carbon_intensive_industry_seeking_legitimacy, beneficiary,
    powerful, biographical, arbitrage, global).

% Develop, finance, and operate solar and wind farms. Pay the cost of technologies being classified as insufficient for reliable decarbonization. Face interconnection delays (grid rules prioritize baseload-supporting infrastructure), curtailment rules designed for baseload-centric systems, and policy uncertainty about renewable viability. Cannot exit the electricity market (it is their only market). Limited options: invest in storage to meet grid operator requirements, litigate for better interconnection terms, or fund research disputing the baseload necessity claim. Identity locked: most have committed careers to renewable expansion.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers, payer,
    organized, biographical, constrained, national).

% Advocate for rooftop solar, community microgrids, local storage, and demand-side management as decarbonization infrastructure. Structurally excluded from grid planning forums where baseload necessity is assumed. Their alternatives (local generation, peer-to-peer energy sharing, microgrids) are treated as supplements to centralized generation rather than core infrastructure. Grid rules, interconnection standards, and utility rate structures all favor central dispatch. Constrained exit: can operate at margins of the grid but cannot influence grid architecture itself.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_generation_advocates, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_generation_advocates, excluded).

% Research and advocate for demand reduction, efficiency improvements, and distributed renewable plus storage as primary decarbonization paths. Systematically excluded from energy modeling, grid planning, and official climate policy documents. Their findings that renewable-primary grids work (Denmark, Costa Rica) are treated as anomalies rather than proof. Policy research funding flows to baseload-supporting narratives. Constrained exit: can publish research and speak at alternative forums, but official policy channels remain closed.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, decentralized_energy_transition_proponents, excluded,
    moderate, generational, constrained, regional).

% Develop and maintain grid planning software and energy modeling frameworks (PLEXOS, NEMO, IRENA models) used by utilities, grid operators, and governments. Embed baseload necessity assumptions into the code and parameter sets. These technical assumptions are treated as discovered facts rather than design choices. Exit is constrained: changing foundational assumptions requires re-validating decades of prior models and policy recommendations, which would damage institutional credibility. Financial incentive to maintain existing model architecture and parameterization.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, energy_modeling_institutions, agenda_setter,
    institutional, biographical, constrained, global).

% Many major climate advocacy organizations accept the baseload necessity premise as axiomatic because it produces policy urgency and funding authorization. Benefit from a decarbonization narrative that treats nuclear expansion as essential, which legitimizes rapid action and large-scale capital deployment. Mobile exit: could endorse renewable-primary or degrowth narratives, but institutional alignment and funding relationships with energy-sector actors create path dependence. Most have consciously chosen to accept baseload necessity framing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_advocates, beneficiary,
    organized, biographical, mobile, global).

% Benefit from reliable, dispatchable electricity supply that baseload generation is designed to provide. Also bear costs through higher electricity rates (nuclear capital costs) and constrained alternatives (renewables may be delayed or limited by grid rules). Cannot exit the grid or choose their own electricity generation mix. Options are limited to switching providers within the same regulatory jurisdiction or installing rooftop solar (where allowed).
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_consumers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_consumers, payer).

% Develop battery, hydrogen, thermal, and mechanical storage technologies that could enable renewable-primary grids. Partly excluded from infrastructure planning because storage is treated as a supplement to baseload rather than as a core grid component. Grid planning models systematically undervalue storage contributions and assume storage cannot replace baseload. Progress in storage technology is constrained by policy frameworks that do not contemplate storage as a replacement. Competing for limited capital and policy attention against baseload narratives.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_storage_technology_developers, excluded,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, grid_storage_technology_developers, payer).

% Conduct empirical research on grid stability, renewable penetration feasibility, and storage economics. Their findings that renewable-primary or high-renewable-penetration grids achieve stability (Denmark, Uruguay, Costa Rica, high-renewable regions in Texas) are documented in peer-reviewed literature. These findings have limited institutional impact on policy because the baseload necessity reading is already embedded in regulations, procurement rules, and model assumptions. Can see the full structure from analytical distance but cannot modify grid-planning decisions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, research_institutions_studying_renewable_feasibility, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_operators).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__baseload_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures electrical grid reliability by providing dispatchable power that can meet demand spikes, cover seasonal variation, and maintain voltage and frequency stability. Solves the technical challenge of integrating large amounts of variable renewable supply into a grid that must serve both continuous base load and unpredictable peaks without interruption.
% TRANSFER_FUNCTION: Moves public trust, regulatory priority, and capital investment from renewable infrastructure (solar, wind, storage) toward nuclear plant construction and operation. Transfers technical authority to grid planners and energy modeling institutions that assume baseload necessity. Moves electricity cost increases to consumers and renewable developers (higher interconnection barriers, grid-use charges) to nuclear operators (via guaranteed load factors and priority dispatch).
% ABSENT_VOICES: Distributed generation advocates, energy storage technology developers, researchers documenting high-renewable grid feasibility, and advocates for demand reduction are structurally excluded from official energy policy and grid planning forums. They would argue that renewable-plus-storage can achieve grid stability at lower cost and faster deployment. Their empirical findings are filtered out of binding policy documents and energy models.
% DISAPPEARANCE_RATIONALE: If this reading disappeared — if energy policy and grid planning no longer assumed baseload necessity — capital investment would shift dramatically from nuclear to renewable and storage infrastructure. Grid architecture would re-optimize for variable renewable supply plus storage, demand flexibility, and distributed generation rather than centralized dispatchable generation. Interconnection rules would favor distributed resources. Energy modeling would treat baseload as one optional design choice rather than a requirement. The economic case for nuclear would collapse without regulatory mandate. Decarbonization timelines would shift as alternative infrastructure deployments accelerated.
% FOUNDING_PROBLEM: Early integration of renewables onto existing grids (2010s) revealed technical challenges: high solar and wind penetration caused frequency instability, voltage sags, and difficulty meeting peak demand without conventional generation backup. This was interpreted as proving that renewables CANNOT be the primary decarbonization source without baseload backing.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear operators and grid planners cite the early technical challenges as evidence the problem persists. Researchers studying modern high-renewable grids (Denmark operating 80%+ wind, Uruguay and Costa Rica achieving 99%+ renewable years, California running renewable-primary days) document that grid stability IS achievable at high renewable penetration through improved grid codes, fast-response storage, and demand automation. Independent economic analyses show battery storage costs have declined below competing with baseload nuclear on a levelized basis. The founding problem was real; whether it proves baseload is the ONLY solution is where readings diverge. No corroborating source outside nuclear and grid-planning institutions supports that baseload remains necessary; outside testimony is unanimous that alternatives have become viable.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 reflects asymmetric flows: nuclear operators and grid planners extract regulatory authority and capital commitment from the baseload necessity premise; renewable developers bear the cost of being classified as insufficient. Suppression at 0.72 captures the active work required to maintain the reading despite contrary evidence: grid codes designed to exclude rival payment/dispatch mechanisms, research findings about renewable feasibility filtered out of official modeling, interconnection rules that favor centralized generation. Theater ratio at 0.41 reflects partial decoupling between function and form: the real coordination problem (grid stability) is genuine, but a growing share of enforcement machinery defends nuclear capital recovery and excludes alternatives rather than solving stability per se. Accessibility collapse at 0.62 is moderate: alternatives exist and are visible (Denmark, Costa Rica operate high-renewable grids; battery costs are public), but grid lock-in and institutional inertia make exiting the baseload framework expensive. Resistance at 0.71 is substantial: renewable developers, storage companies, climate advocates pushing for faster deployment, and researchers documenting renewable viability all push back against the reading's claims. The coercion grid shows escalating stakes and suppression at structural and organizational levels (grid operators, utilities, modeling institutions) while individual agents retain somewhat higher resistance. At t=40, extractiveness plateaus near 0.68 because the reading is mature and its institutional embedding has stabilized; further extraction would require visible suppression of increasingly credible alternatives, which becomes harder as empirical evidence accumulates.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (renewable developers, distributed advocates) and the agenda-setter seats (nuclear operators, grid planners) experience this constraint as functionally opposite categories from the same structural position. A renewable developer sees a Snare (pure extraction, no benefit, suppressed alternatives, trapped exit). A grid planner sees a Rope (genuine coordination, mutual benefit). The engine will compute per-seat types from the authored structural data; the divergence is the measurement. The reading itself asserts that it is Tangled Rope (genuine coordination + asymmetric extraction), which may be how nuclear operators frame it, but the victims' perception (Snare) is not authoritatively overridden by the reading's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators benefit structurally (capital recovery, regulatory mandate, policy authority); they have strong exit options (could endorse alternative decarbonization, could compete on pure cost) but choose to defend the baseload necessity reading because it is the only pathway preserving their business model. Directionality for institutional_powerful = ~0.15 (beneficiary end). Grid planners benefit from the regulatory authority and technical control the baseload assumption grants them; they have constrained exit (changing assumptions requires re-engineering their professional frameworks) and are partly captured by the institutions that employ them. Directionality for institutional_biographical = ~0.65 (moderate target). Renewable developers are trapped: they have the technology to deliver decarbonization but are pre-screened as inadequate by grid modeling and policy. Exit is constrained (can't exit the market), and the cost of fighting the assumption (research funding, policy advocacy) is high. Directionality for organized_biographical = ~0.85 (target end). Distributed generation advocates are excluded entirely, denied participation in the official process where their voice could dispute the assumption. Directionality for moderate_biographical excluded = ~0.80 (target end). The readings in cs_structure.axioms reflect this: the baseload necessity axiom is 'foundational' and 'holdable' for the institutional seats defending it, but that axiom depends on treating renewables as fundamentally limited — a claim the reading maintains by suppressing contrary evidence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem for this constraint is real and historical: early high-renewable grids did face stability challenges, and dispatchable generation was (and is) a genuine technical solution. The founding problem status is 'contested' because the empirical answer to 'are renewables sufficient?' has shifted: modern evidence from Denmark (80%+ wind), Costa Rica (99% renewable years), and California (70%+ renewable days) shows that the founding problem — grid stability at high renewable penetration — IS solvable without mandatory baseload. The constraint persists because its institutional embedding now outpaces the empirical ground. Mandatrophy is present but unresolved: the reading still claims to solve the founding problem (grid stability) but the problem is arguably solved by alternatives. The reading is defended now primarily on capital protection (avoiding stranded nuclear assets) and institutional authority maintenance (grid planners' professional identity), not because the stability problem remains unsolved. A clean mandatrophy resolution would require either: (a) acknowledging the founding problem is substantially solved and letting the reading dissolve, or (b) reframing the problem to something baseload solves that alternatives cannot (e.g., 'investor confidence,' 'large capital projects,' 'geopolitical fuel security'). The reading does not cleanly do either; instead it holds both by treating empirical falsification from alternative jurisdictions as exceptions rather than proof.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseload_technical_necessity_vs_economic_choice,
    'Is the requirement for dispatchable baseload power a technical necessity of physics and grid stability, or a design choice reflecting current infrastructure and institutional preferences?',
    'Empirical comparison across jurisdictions: high-renewable grids (Denmark, Uruguay, Costa Rica, parts of California) that achieve stability without baseload dispatchability. Analysis of grid stability metrics (frequency deviation, voltage stability, blackout frequency) in baseload-free vs. baseload-dependent systems at equivalent reliability targets. Technology roadmaps for storage, demand response, and smart grid integration that would enable baseload-free operation at scale.',
    'If baseload is a choice, the constraint reclassifies from ''natural law'' to ''Tangled Rope with false summit risk'' — the technical vindication dissolves and the extraction becomes visible. If baseload is truly necessary, the extraction declines as coordination cost rather than overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_technical_necessity_vs_economic_choice, empirical, 'Whether grid stability requires centralized dispatchable generation or is achievable with distributed renewables plus storage.').

omega_variable(
    reading_kernel_contest,
    'This reading (baseload necessity) is one of four competing readings of the ''climate_mitigation_legitimacy'' kernel. What determines which reading becomes institutionalized as the legitimate technical/policy framework?',
    'Historical analysis: prior decarbonization mandates (EU Renewable Energy Directive, IRA in the US) show how readings shift with empirical evidence, cost trajectories, and institutional capture. Observe which reading is embedded in grid codes, long-term procurement rules, and climate legislation. Track whether research findings about renewable feasibility change institutional commitments or are reinterpreted to preserve the baseload reading.',
    'This uncertainty is the core feature of a kernel contest: all four readings are structurally coherent claims, but only one becomes THE legitimate framework in any jurisdiction. The baseload reading''s persistence depends on suppressing or reinterpreting evidence from alternative readings and alternative geographies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Why the baseload-necessity reading, rather than renewable-primary, portfolio, or degrowth readings, becomes THE official decarbonization framework.').

omega_variable(
    suppression_mechanism_centralization_vs_internalization,
    'Is the measured suppression of renewable-primary pathways primarily structural (regulatory barriers, grid code lock-in, interconnection rules designed for baseload) or internalized (energy planners and utilities genuinely believe baseload is necessary, not just defending institutional turf)?',
    'Post-regulatory-change trajectory: if suppression is structural, it declines quickly after grid codes change to enable high-renewable penetration; if internalized, suppression of alternatives persists even after structural barriers are removed (planners continue to argue against renewable-primary pathways despite having removed the formal barriers).',
    'If structural, the constraint can be modified by changing regulations; if internalized, the constraint persists through professional socialization and identity lock-in of grid planners and energy modelers, requiring deeper institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_centralization_vs_internalization, empirical, 'Is the suppression of renewable alternatives maintained by external rules or by internalized professional beliefs?').

omega_variable(
    cost_trajectory_and_baseload_viability,
    'The economics of baseload generation (nuclear capital costs) and renewable generation (battery storage costs) are both declining, but at different rates. At what cost point does a renewable-plus-storage grid become cheaper than a nuclear-based system, and does the reading adjust its claims as that crossover approaches?',
    'Comparative cost modeling: Lazard LCOE reports, NREL Annual Technology Baseline, and peer-reviewed analyses showing levelized cost of energy for nuclear, renewables, and hybrid systems. Track whether the ''baseload necessity'' reading is defended on technical grounds (stability) or cost grounds (affordability). If cost projections change the economic answer, does the reading shift to pure technical claims?',
    'If the reading''s force depends on cost advantage, and costs cross, the economic justification collapses and only technical necessity claims remain. If technical claims can be maintained regardless of cost, the reading persists as a pure institutional commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_trajectory_and_baseload_viability, empirical, 'Whether baseload necessity is defended on technical or economic grounds, and whether cost shifts change the reading''s viability.').

omega_variable(
    grid_modeling_assumption_embedding,
    'The baseload necessity reading is maintained partly through modeling assumptions embedded in energy planning software and frameworks (PLEXOS, NEMO, IRENA models). These assumptions treat dispatchability as a hard constraint rather than as an optimization variable. If these models were reformulated to treat dispatchability as a design choice rather than a requirement, would the model outputs shift to renewable-primary pathways?',
    'Comparative modeling: run standard grid planning scenarios (e.g., 100% decarbonization, meeting peak demand) with baseload-as-hard-requirement vs. baseload-as-optional, holding all other variables fixed. Compare model outputs for capacity mixes, total system cost, and feasibility verdict.',
    'If model outputs shift substantially when the assumption is relaxed, the baseload necessity reading is shown to be an artifact of modeling choices, not technical discovery. The suppression of alternatives is partly maintained by the technical apparatus itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grid_modeling_assumption_embedding, empirical, 'Whether grid modeling assumptions embed the baseload necessity reading rather than discovering it from first principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 5, 0.34).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, observed).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_tr_t30, observed).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(clim_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(clim_be_t25, observed).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, observed).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(clim_su_t25, observed).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(clim_su_t30, observed).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(clim_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(clim_grid_01, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(clim_grid_02, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(class), 40, 0.65).
narrative_ontology:measurement(clim_grid_03, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(individual), 0, 0.48).
narrative_ontology:measurement(clim_grid_04, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(individual), 40, 0.56).
narrative_ontology:measurement(clim_grid_05, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(clim_grid_06, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(organizational), 40, 0.61).
narrative_ontology:measurement(clim_grid_07, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(clim_grid_08, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(structural), 40, 0.74).
narrative_ontology:measurement(clim_grid_09, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(clim_grid_10, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(class), 40, 0.74).
narrative_ontology:measurement(clim_grid_11, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(clim_grid_12, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(individual), 40, 0.62).
narrative_ontology:measurement(clim_grid_13, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(organizational), 0, 0.64).
narrative_ontology:measurement(clim_grid_14, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(organizational), 40, 0.71).
narrative_ontology:measurement(clim_grid_15, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(structural), 0, 0.68).
narrative_ontology:measurement(clim_grid_16, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(structural), 40, 0.71).
narrative_ontology:measurement(clim_grid_17, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(class), 0, 0.62).
narrative_ontology:measurement(clim_grid_18, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(class), 40, 0.68).
narrative_ontology:measurement(clim_grid_19, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(individual), 0, 0.44).
narrative_ontology:measurement(clim_grid_20, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(individual), 40, 0.48).
narrative_ontology:measurement(clim_grid_21, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(clim_grid_22, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(organizational), 40, 0.62).
narrative_ontology:measurement(clim_grid_23, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(structural), 0, 0.71).
narrative_ontology:measurement(clim_grid_24, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(structural), 40, 0.76).
narrative_ontology:measurement(clim_grid_25, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(clim_grid_26, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(class), 40, 0.72).
narrative_ontology:measurement(clim_grid_27, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(individual), 0, 0.52).
narrative_ontology:measurement(clim_grid_28, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(individual), 40, 0.54).
narrative_ontology:measurement(clim_grid_29, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(organizational), 0, 0.61).
narrative_ontology:measurement(clim_grid_30, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(organizational), 40, 0.65).
narrative_ontology:measurement(clim_grid_31, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(structural), 0, 0.74).
narrative_ontology:measurement(clim_grid_32, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(structural), 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__baseload_necessity_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_capital_recovery_guarantee).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, grid_interconnection_standard_baseload_preference).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, electricity_price_stability_extraction).

% DUAL FORMULATION NOTE:
% The 'climate_mitigation_legitimacy' kernel decomposes into four structurally distinct constraint stories, each instantiating a different reading of what decarbonization requires. This reading (baseload necessity) claims that dispatchable nuclear is NECESSARY and renewable-only pathways are INSUFFICIENT. The sibling readings claim: renewable_primacy (renewables + storage are SUFFICIENT), portfolio_pragmatism (all options are optimal, technology-neutral), and degrowth (demand reduction is PRIMARY). These are not the same constraint viewed from different angles — they have different ε values (baseload necessity assumes high extraction from renewable exclusion; renewable primacy assumes low extraction), different beneficiary/victim structures (baseload benefits nuclear operators; renewable primacy benefits solar/wind developers), and incompatible institutional implications. Each reading constrains what decarbonization options appear feasible in grid planning and policy. The readings coexist in public discourse but are embedded in different jurisdictions, research institutions, and policy regimes — no single framework holds all four as equally valid. Linking them via 'affects_constraints' enables the engine to track which reading becomes institutionalized and what happens to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__baseload_necessity_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
