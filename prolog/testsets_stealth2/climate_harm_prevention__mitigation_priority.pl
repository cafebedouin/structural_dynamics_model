% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__mitigation_priority, []).

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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Mitigation-Priority Reading of Climate Harm Prevention
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   Since the Rio framework convention, the legitimate center of climate
 *   response has been occupied by a specific arrangement: emission reduction
 *   framed as prevention of future harm, delivered through technological
 *   substitution — renewables, efficiency, electrification, carbon accounting
 *   — while explicitly preserving economic growth. This story authors THAT
 *   arrangement as a single epsilon-invariant constraint, from the
 *   mitigation_priority seat of the climate_harm_prevention kernel. The rival
 *   readings (adaptation-first resilience building; planned Northern
 *   contraction) are separate constraints in separate files, linked through
 *   the network section; they are not folded into this one. The epsilon
 *   referent is the standing mitigation-priority arrangement as it actually
 *   operates — treaties, subsidy regimes, carbon markets, budget hierarchies
 *   — assessed by this reading's own lights, which endorse mitigation as the
 *   correct priority; the score therefore reflects extraction undeniable even
 *   to a sympathizer, not a hostile audit. Claim and metrics are authored
 *   independently: I claim tangled_rope because the structure genuinely
 *   coordinates against a real commons problem while simultaneously charging
 *   identifiable present-day groups for benefits that are deferred, diffuse,
 *   and unenforceable by their intended recipients, all held in place by
 *   active enforcement machinery. The metrics describe that operation without
 *   being tuned to any predicted verdict.
 *
 * KEY AGENTS:
 *   - future_generations: declared primary beneficiary (powerless/trapped) — receives avoided harm only if present actors deliver; cannot collect or enforce
 *   - renewable_energy_industries: present-day material beneficiary (organized/mobile) — collects subsidy and mandate rents; advocates tightening
 *   - carbon_finance_intermediaries: present-day procedural beneficiary (organized/arbitrage) — monetizes the accounting apparatus itself
 *   - carbon_intensive_workers: primary present-day cost bearer (moderate/constrained) — displacement concentrated in anchored regions
 *   - present_day_climate_vulnerable_communities: double cost bearer (powerless/trapped) — current damages plus deferred adaptation
 *   - global_south_development_states: dual-positioned (organized/constrained) — bears development-space costs, receives below-scale finance
 *   - fossil_fuel_incumbents: nominal cost bearer with arbitrage-grade deflection (institutional/arbitrage)
 *   - climate_policy_establishment: agenda setter (institutional/identity_locked) — administers the frame its members professionally are
 *   - degrowth_climate_justice_movements: excluded voice (organized/constrained) — contests the growth premise from outside the rooms
 *   - integrated_assessment_economists: analytical observer (analytical/analytical) — defines feasibility inside the frame's assumptions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.52).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Mitigation-Priority Reading of Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, '8b379ac0-5537-45f5-93cc-979510c217ae').
narrative_ontology:cs_kernel_codification('8b379ac0-5537-45f5-93cc-979510c217ae', fixed_text).
narrative_ontology:cs_authority_grounding('8b379ac0-5537-45f5-93cc-979510c217ae', expertise).
narrative_ontology:cs_interpretation_layer_present('8b379ac0-5537-45f5-93cc-979510c217ae').
narrative_ontology:cs_reading_relation('8b379ac0-5537-45f5-93cc-979510c217ae', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('8b379ac0-5537-45f5-93cc-979510c217ae', climate_harm_prevention__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('8b379ac0-5537-45f5-93cc-979510c217ae', foundational, growth_compatible_decarbonization_possible).
narrative_ontology:cs_axiom_status(growth_compatible_decarbonization_possible, holdable).
narrative_ontology:cs_axiom_grounding('8b379ac0-5537-45f5-93cc-979510c217ae', growth_compatible_decarbonization_possible, empirically_contingent).
narrative_ontology:cs_axiom('8b379ac0-5537-45f5-93cc-979510c217ae', foundational, emissions_reduction_primary_duty_to_future).
narrative_ontology:cs_axiom_status(emissions_reduction_primary_duty_to_future, holdable).
narrative_ontology:cs_axiom_grounding('8b379ac0-5537-45f5-93cc-979510c217ae', emissions_reduction_primary_duty_to_future, deontological).
narrative_ontology:cs_axiom('8b379ac0-5537-45f5-93cc-979510c217ae', secondary, technological_substitution_over_behavioral_restraint).
narrative_ontology:cs_axiom_status(technological_substitution_over_behavioral_restraint, holdable).
narrative_ontology:cs_axiom_grounding('8b379ac0-5537-45f5-93cc-979510c217ae', technological_substitution_over_behavioral_restraint, instrumental).
narrative_ontology:cs_reference_frame('8b379ac0-5537-45f5-93cc-979510c217ae', growth_compatible_transition_sufficiency).
narrative_ontology:cs_drift_state('8b379ac0-5537-45f5-93cc-979510c217ae', post_first_global_stocktake, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8b379ac0-5537-45f5-93cc-979510c217ae', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, renewable_energy_industries).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, carbon_finance_intermediaries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, present_day_climate_vulnerable_communities).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, global_south_development_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, global_south_development_states).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are the declared primary recipients of the arrangement's benefit: avoided warming harm if emission cuts arrive in time. They cannot vote, contract, litigate on their own behalf, or withhold participation; their claim is carried proxy-fashion by advocacy organizations, youth plaintiffs using guardianship and constitutional doctrines, and ombudsman-style institutions. Whether the value nominally reserved for them actually arrives depends entirely on the performance of present-day actors.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Manufacture and deploy the technologies the transition relies on: turbines, panels, storage, grids, electrolyzers. They receive feed-in tariffs, tax credits, auction-guaranteed revenues, and mandated market share funded by taxpayers and ratepayers. They lobby for tightening decarbonization mandates, which expands their addressable market, and can shift capital between jurisdictions offering the richest support schemes.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, renewable_energy_industries, beneficiary,
    organized, biographical, mobile, global).

% Design, verify, trade, and advise on carbon credits, offset portfolios, disclosure frameworks, and transition-finance products. Their revenue scales with the complexity and volume of the accounting apparatus rather than with tons actually abated. When a standard or registry loses credibility they rebrand under a successor scheme, carrying client relationships across the boundary.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_finance_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Coal miners, oil and gas workers, combustion-engine plant employees, and dependent trades bear displacement, wage loss, pension erosion, and community decline as production shifts. Just-transition funds compensate a fraction of losses and arrive slowly relative to plant closures. Their skills and mortgages are anchored to specific regions and facilities, so relocation is costly and partial; their political voice has grown as closures accelerate.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_workers, payer,
    moderate, immediate, constrained, regional).

% Communities already flooded, scorched, or storm-struck pay twice under the arrangement: they absorb current damages while adaptation, early-warning, and loss-and-damage funding is subordinated to mitigation spending in budget hierarchies, and they carry indirect costs where mitigation raises near-term energy and food prices. Migration is limited by poverty and border regimes; their claims enter negotiations mainly through bloc representatives.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, present_day_climate_vulnerable_communities, payer,
    powerless, immediate, trapped, global).

% Negotiate as a bloc (G77-plus-China) inside the treaty system. They accept mitigation conditionalities and forgo some cheap fossil development paths, while receiving climate finance that arrives below promised scale and partly as loans. They gain access to technology transfer and leapfrogging investment, so their position is genuinely dual: they bear development-space costs now against benefits that depend on donor follow-through.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, global_south_development_states, payer,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__mitigation_priority, global_south_development_states, beneficiary).

% Major producers and heavy industry face stranded-asset risk, carbon pricing exposure, and disclosure mandates. In practice they deflect much of this: they fund delay campaigns, capture permitting processes, acquire transition assets, and reprice outputs to consumers. Their net burden under the arrangement is heavily mediated by their lobbying success, which is why they are described here as cost-bearing rather than listed among the groups the arrangement systematically charges.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_incumbents, payer,
    institutional, generational, arbitrage, global).

% Environment ministries, the UNFCCC secretariat, COP presidencies, climate-finance banks, and accredited NGO networks set the negotiating agenda, define what counts as a credible national pledge, and administer compliance review. Careers, budgets, and organizational purpose have been built on the mitigation frame for three decades; entertaining a rival framing of the mandate would dissolve the professional identities and institutional charters the staff inhabit.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_policy_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Academics, activist networks, and Global South scholars arguing that the growth framework makes sufficient decarbonization impossible and that sufficiency, redistribution, and present-day repair must lead. They publish, protest, and litigate at the margins but hold no seats in negotiation rooms; official processes admit them as observers with speaking rights outside decision sessions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_climate_justice_movements, excluded,
    organized, generational, constrained, global).

% Build the cost-benefit and pathway models that define feasibility inside the arrangement: discount rates, carbon-price trajectories, technology-learning curves. Their modeling choices determine which futures count as attainable and which demands count as unrealistic, giving them quiet agenda-shaping power. They analyze the structure from outside enforcement but inside its assumptions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, integrated_assessment_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__mitigation_priority, renewable_energy_industries).
narrative_ontology:fixing_cost_class(climate_harm_prevention__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action against a genuine atmospheric commons problem: emissions anywhere impose costs everywhere with long lag, so meaningful reduction requires synchronized commitments, shared monitoring and accounting, technology diffusion, and burden-sharing rules that no single actor can supply alone.
% TRANSFER_FUNCTION: Moves present resources — fiscal transfers, consumer price increases, displaced labor income, and deferred adaptation spending — away from present-generation carbon-intensive producers, their workforce, and currently exposed populations, toward avoided future harm and toward present-day low-carbon industry and carbon-accounting intermediaries.
% ABSENT_VOICES: Future generations are absent by definition and speak only through proxy litigants and advocates. Present-day climate-vulnerable communities enter mainly through bloc diplomacy that subordinates their adaptation claims. Degrowth and climate-justice voices are confined to observer status. The non-human systems bearing the harm have no seat at all.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority arrangement vanished overnight, treaty machinery, carbon markets, renewable mandates, and climate-finance flows would unwind; trillions in committed investment would reprice; adaptation-first and contractionist framings would compete for the vacated legitimacy space; and present-day vulnerable populations would immediately contest the budget hierarchy that currently defers their claims.
% FOUNDING_PROBLEM: By the late 1980s the scientific community had established that cumulative greenhouse-gas emissions would produce long-lived, potentially irreversible harm falling mostly on people not yet born, while the costs of prevention fell immediately on identifiable industries and consumers. The founding problem was how to organize early, collective emission reductions across sovereign states when costs are present and concentrated and benefits are distant and diffuse.
% FOUNDING_PROBLEM_CORROBORATION: The underlying problem is corroborated from outside the benefiting parties: IPCC assessment cycles, national science academies, insurance and reinsurance loss data, and central-bank climate-risk analyses all attest that cumulative-emission harm remains live. Note the distinction: these sources corroborate the founding problem, not the claim that this particular arrangement solves it best — adaptation-first and degrowth proponents corroborate the problem while disputing the solution, and no source outside the benefiting parties attests that the growth-compatible framing specifically is vindicated.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58: the coordination core is real and large, but the cost incidence is concentrated and present (workers, exposed communities, Southern development space) while the headline benefit is deferred to parties who cannot collect, and a growing share of fiscal flow lands as above-competitive returns in transition industries and accounting services. Suppression at 0.52 reflects structural rather than violent exclusion: rival framings are not banned but are defunded, model-defined as infeasible, and kept outside decision rooms; enforcement runs through treaty review, carbon accounting gatekeeping, and funding conditionality. Theater at 0.42 captures pledge inflation, weak-offset quality, and summit ritual — activity that performs mitigation without delivering it. Accessibility collapse at 0.45: alternatives remain thinkable and partially practiced (adaptation finance exists; degrowth scholarship grows), so understanding the arrangement does not close the option space the way a natural law would. Resistance at 0.6: fossil incumbency, climate-justice mobilization, Southern equity demands, and worker backlash all actively contest the arrangement. The temporal series share one grid (six points, all three metrics at every point); they smooth the visible COP-cycle oscillation — pledge spikes before summits, delivery troughs after — which is a reporting artifact of the cycle rather than a separate extraction mechanism, though intermittent-reinforcement dynamics around pledge cycles are worth watching. Coalition note: the payer seats are not natural allies (workers vs. Southern states vs. justice movements), but just-transition framing has repeatedly attempted worker-South coalitions; their fragmentation is itself maintained by the arrangement's allocation of compensable grievances separately to each group. Identity-lock note: the establishment seat is bound by institutional identity fusion — three decades of careers, charters, and budgets constitute the mitigation frame, so exit equals professional self-dissolution; if that frame broke, the administrative machinery would not merely shrink, it would lose its organizing purpose.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the establishment seat the arrangement is a hard-won civilizational achievement it personally embodies; from the worker and vulnerable-community seats it is a queue in which their claims are permanently second; from the renewables and carbon-finance seats it is a expanding market; from the Southern-bloc seat it is a promise scaled below its invoice. The primary beneficiary seat is stranger still: future generations experience nothing at all — neither benefit nor cost — unless present actors perform, making them a beneficiary whose position is wholly derivative. The engine computes these divergences from power, exit, and role data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality for future_generations (nominal full beneficiary, damped further by zero collection capacity), renewable_energy_industries (material beneficiary with mobile capital), and carbon_finance_intermediaries (beneficiary with arbitrage-grade mobility between standards). Victim declarations map to high directionality for carbon_intensive_workers (constrained exit amplifies), present_day_climate_vulnerable_communities (trapped, double-charged), and global_south_development_states (payer with a genuine secondary beneficiary position that moderates but does not reverse their bearing of costs). The establishment sits mid-range: it collects legitimacy and budgets rather than the extraction itself. Fossil incumbents are deliberately NOT listed in the victims array despite bearing real regulatory costs: their arbitrage-grade deflection means the arrangement's net charge on them is small and contested, and listing them would misstate the structural incidence. No directionality overrides were needed: role, power, and exit data differentiate every seat the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — organizing early collective emission cuts under present-cost/future-benefit asymmetry — remains live: global emissions have not declined on the required trajectory, so the arrangement has not outlived its function and mandatrophy is not resolved. The mismatch consumer should read founding_problem_status=live against disappearance_verdict=world_rearranges and find no zombie flag. The drift to watch is Goodhart-shaped rather than obsolescence-shaped: theater_ratio has risen monotonically across the interval (0.15 to 0.42) as pledge performance and accounting volume grow faster than delivered abatement. If delivery collapsed while the ceremonial layer kept expanding, the arrangement would slide toward piton dynamics — administered by an establishment that could change it but bears less of its cost than the diffuse payers — and the classification should be revisited then. The tangled_rope claim is what prevents mislabeling in both directions: reading the arrangement as pure coordination ignores the measurable adaptation-deferral and rent-capture components; reading it as pure extraction erases the genuine commons problem that would reassert itself instantly if enforcement vanished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is mitigation_priority the correct instantiation of the climate_harm_prevention kernel, or do the adaptation_priority and degrowth_reading siblings instantiate the kernel better — and where exactly does the disagreement bite?',
    'Comparative outcome tracking across jurisdictions and decades: realized warming trajectories, resilience investment returns, and welfare outcomes under mitigation-dominant versus adaptation-forward versus contractionist policy mixes; plus philosophical adjudication of the intergenerational discount question the readings actually disagree on.',
    'If adaptation_priority prevails, the victim set inverts — present vulnerable communities become primary beneficiaries and future generations'' claim is discounted; if degrowth_reading prevails, this constraint''s foundational possibility axiom fails and the growth-framework protection of present consumption becomes the arrangement''s central extraction. The disagreement is located in burden timing and in the growth-sufficiency premise, not in the reality of climate harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which sibling reading of the climate_harm_prevention kernel this arrangement competes against, and on what structural element the contest turns.').

omega_variable(
    growth_compatibility_assumption,
    'Can decarbonization actually proceed fast enough within continued economic growth to prevent dangerous harm, or does the growth framework embed physically or politically impossible assumptions?',
    'Decoupling-rate measurement against required carbon budgets: absolute territorial and consumption-based decoupling in major economies, material-throughput limits on transition buildout, and historical precedent for sustained double-digit clean-energy deployment growth.',
    'If growth-compatible decarbonization is infeasible at the required rate, this reading''s foundational premise fails, the degrowth sibling''s core claim is vindicated, and the arrangement''s protection of present consumption patterns becomes its dominant feature rather than a background assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_compatibility_assumption, empirical, 'Whether the growth-framework premise of this reading survives contact with required decarbonization rates.').

omega_variable(
    future_beneficiary_enforceability,
    'Is the primary beneficiary''s position structurally real — do reserved benefits actually convert into avoided harm for future people — or is future_generations a legitimating figurehead while present-day industry seats collect the transfers?',
    'Trace the conversion chain: delivered-abatement accounting separated from pledge accounting, litigation outcomes for guardian/constitutional climate cases, and whether realized atmospheric concentrations track the quantities the transfers were nominally purchasing.',
    'If conversion fails systematically, the beneficiary declaration is nominal, the arrangement''s coordination claim weakens toward cover, and the computed extraction concentrates entirely on the present-day payer seats with no offsetting deferred benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_beneficiary_enforceability, conceptual, 'Whether the declared primary beneficiary can actually receive what the arrangement reserves for it.').

omega_variable(
    adaptation_deferral_extraction_share,
    'How much of the measured extraction consists of active deferral of adaptation and loss-and-damage spending (a transfer away from present vulnerable populations) versus ordinary transition cost-bearing?',
    'Budget-share analysis of climate finance flows: mitigation-versus-adaptation allocation ratios over time against assessed need, and counterfactual costing of adaptation gaps in exposed regions.',
    'A large deferral share would identify a specific, nameable extraction channel running through the budget hierarchy — strengthening the asymmetric-extraction component of the classification; a small share would locate most extraction in subsidy rents and labor displacement instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_deferral_extraction_share, empirical, 'Size of the adaptation-deferral component within the arrangement''s total extraction.').

omega_variable(
    green_transfer_rent_share,
    'What fraction of fiscal transfers to transition industries is genuine coordination cost — building infrastructure that would not otherwise exist — versus above-competitive rent captured by incumbent suppliers and intermediaries?',
    'Competitive-procurement audits of renewable support schemes, margin analysis in auction and credit markets, and comparison of delivered cost curves against subsidized cost curves across jurisdictions with differing scheme designs.',
    'A high rent share would confirm concentrated receipt of the arrangement''s transfers in the industry seats and sharpen the extraction asymmetry; a low share would recast most transfers as legitimate buildout cost and soften the extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(green_transfer_rent_share, empirical, 'Rent-versus-cost composition of the transition subsidy flows.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_harm_prevention__mitigation_priority, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(clim_tr_t2001, climate_harm_prevention__mitigation_priority, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(clim_tr_t2010, climate_harm_prevention__mitigation_priority, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(clim_tr_t2015, climate_harm_prevention__mitigation_priority, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__mitigation_priority, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clim_tr_t2025, climate_harm_prevention__mitigation_priority, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_harm_prevention__mitigation_priority, base_extractiveness, 1992, 0.38).
narrative_ontology:measurement(clim_be_t2001, climate_harm_prevention__mitigation_priority, base_extractiveness, 2001, 0.44).
narrative_ontology:measurement(clim_be_t2010, climate_harm_prevention__mitigation_priority, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(clim_be_t2015, climate_harm_prevention__mitigation_priority, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__mitigation_priority, base_extractiveness, 2020, 0.57).
narrative_ontology:measurement(clim_be_t2025, climate_harm_prevention__mitigation_priority, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_harm_prevention__mitigation_priority, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement(clim_su_t2001, climate_harm_prevention__mitigation_priority, suppression_requirement, 2001, 0.44).
narrative_ontology:measurement(clim_su_t2010, climate_harm_prevention__mitigation_priority, suppression_requirement, 2010, 0.47).
narrative_ontology:measurement(clim_su_t2015, climate_harm_prevention__mitigation_priority, suppression_requirement, 2015, 0.49).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__mitigation_priority, suppression_requirement, 2020, 0.51).
narrative_ontology:measurement(clim_su_t2025, climate_harm_prevention__mitigation_priority, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'legitimate climate response' covers three structurally distinct arrangements that share one kernel (climate_harm_prevention) but diverge on burden timing, beneficiary primacy, and the growth-sufficiency premise. This file instantiates mitigation_priority (epsilon 0.58; victims are present-day cost bearers; assumes growth-compatible decarbonization). The adaptation_priority sibling accepts a higher warming trajectory and re-centers present-day resilience (different victim set: future generations bear deferred harm). The degrowth_reading sibling rejects the growth framework outright (different coordination claim: sufficiency and contraction replace technological substitution). The siblings are linked here because mitigation primacy structurally shapes both: it starves adaptation of first-claim resources and defines the growth frame the contractionist reading attacks. Each story carries its own epsilon, stakeholders, and classification; none averages across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
