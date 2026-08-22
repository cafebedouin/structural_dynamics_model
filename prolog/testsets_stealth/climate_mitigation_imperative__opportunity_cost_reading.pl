% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__opportunity_cost_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Speed-First Mitigation Allocation Rule (Opportunity-Cost Reading)
 *   domain: energy/climate/governance
 *
 * SUMMARY:
 *   A widely adopted allocation rule directs climate-mitigation capital,
 *   procurement priority, and campaign legitimacy toward whichever low-carbon
 *   technologies deliver the most avoided emissions per dollar per year,
 *   fastest. Under this rule, generation options with heavy upfront capital
 *   and decade-scale build times are scored as net-negative uses of a
 *   shrinking carbon budget: money and years spent on them are treated as
 *   tonnes forgone elsewhere. The rule operates through development-bank
 *   screening policies, green-taxonomy design, national procurement
 *   sequencing, and movement norms, and it must be actively maintained —
 *   lender policies renewed, taxonomies defended, planning scenarios
 *   rewritten — because capital otherwise migrates back toward excluded
 *   options when electricity systems tighten. Time mapping for the interval:
 *   t=0 corresponds to approximately 2015 (Paris entry into force), t=10 to
 *   2025.
 *
 * KEY AGENTS:
 *   - - renewable_energy_developers: Primary beneficiary (institutional/mobile) — receives redirected capital, procurement preference, and political goodwill
 *   - - grid_storage_manufacturers: Secondary beneficiary (institutional/mobile) — order books scale with accelerated renewable build-out
 *   - - climate_advocacy_coalitions: Beneficiary with agenda-setting reach (organized/identity_locked) — the rule disciplines their messaging, funding, and coalition boundaries
 *   - - multilateral_lenders: Agenda setter (institutional/arbitrage) — administers the exclusion through screening policy without concentrating balance-sheet risk
 *   - - national_energy_ministries: Agenda setter (institutional/constrained) — sequences procurement and planning scenarios; reversal is politically costly
 *   - - nuclear_vendor_utilities: Primary target (institutional/constrained) — bears capital denial, taxonomy exclusion, and stretched licensing
 *   - - nuclear_skilled_workforce: Target (moderate/trapped) — carries the employment and pipeline-break costs
 *   - - plant_host_communities: Target (powerless/trapped) — bear regional economic loss with no seat in the deciding forums
 *   - - gas_generation_interests: Excluded actor (powerful/arbitrage) — collects the gap-hours the sequencing leaves behind
 *   - - energy_systems_analysts: Analytical observer (analytical/analytical) — external check on whether the speed arithmetic still holds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.52).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Speed-First Mitigation Allocation Rule (Opportunity-Cost Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy/climate/governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '2626f9c2-9144-4a74-a5cf-967694a81f85').
narrative_ontology:cs_kernel_codification('2626f9c2-9144-4a74-a5cf-967694a81f85', distributed).
narrative_ontology:cs_authority_grounding('2626f9c2-9144-4a74-a5cf-967694a81f85', expertise).
narrative_ontology:cs_interpretation_layer_present('2626f9c2-9144-4a74-a5cf-967694a81f85').
narrative_ontology:cs_reading_relation('2626f9c2-9144-4a74-a5cf-967694a81f85', climate_mitigation_imperative__portfolio_optimization_reading, influences).
narrative_ontology:cs_reading_relation('2626f9c2-9144-4a74-a5cf-967694a81f85', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('2626f9c2-9144-4a74-a5cf-967694a81f85', foundational, deployment_speed_dominates_selection).
narrative_ontology:cs_axiom_status(deployment_speed_dominates_selection, holdable).
narrative_ontology:cs_axiom_grounding('2626f9c2-9144-4a74-a5cf-967694a81f85', deployment_speed_dominates_selection, empirically_contingent).
narrative_ontology:cs_axiom('2626f9c2-9144-4a74-a5cf-967694a81f85', secondary, capital_scarcity_makes_per_dollar_yield_binding).
narrative_ontology:cs_axiom_status(capital_scarcity_makes_per_dollar_yield_binding, holdable).
narrative_ontology:cs_axiom_grounding('2626f9c2-9144-4a74-a5cf-967694a81f85', capital_scarcity_makes_per_dollar_yield_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('2626f9c2-9144-4a74-a5cf-967694a81f85', speed_first_abatement_triage).
narrative_ontology:cs_drift_state('2626f9c2-9144-4a74-a5cf-967694a81f85', post_cop28_nuclear_pledge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2626f9c2-9144-4a74-a5cf-967694a81f85', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, grid_storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, climate_advocacy_coalitions).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_vendor_utilities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_skilled_workforce).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, plant_host_communities).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, cumulative_carbon_budget_arithmetic).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, levelized_cost_of_electricity_ranking).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__opportunity_cost_reading, marginal_abatement_cost_curve_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and operate wind, solar, and storage projects worldwide. The allocation rule channels concessional finance, procurement preference, and political goodwill toward their product category, and project pipelines expand accordingly. Their skills deploy anywhere, but their business case inside climate finance depends on the prioritization staying in place.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers, beneficiary,
    institutional, biographical, mobile, global).

% Manufacture batteries and flexibility equipment whose economics improve as variable renewables dominate build-outs. Order books scale directly with the pace of renewable deployment that the speed criterion accelerates.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, grid_storage_manufacturers, beneficiary,
    institutional, biographical, mobile, global).

% Campaign networks whose post-Paris strategy crystallized around speed-and-cost arguments. The rule gives their fundraising, messaging, and coalition discipline a concrete target. Members who publicly break with the exclusion of long-build generation find speaking slots, funder access, and allyship harder to come by; leaving the position would forfeit standing built over decades.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_advocacy_coalitions, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, climate_advocacy_coalitions, agenda_setter).

% Public development banks and climate funds that screen generation investments. Several maintain de facto or explicit policies against new reactor lending while scaling renewable credit lines; portfolio officers apply the speed test project by project. Their capital is mobile across sectors and countries, letting them hold the line without concentrating balance-sheet risk in any single technology.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, multilateral_lenders, agenda_setter,
    institutional, generational, arbitrage, global).

% Set procurement auctions, permitting queues, and capacity-market design. Ministries persuaded by the speed criterion sequence renewables first and leave reactors out of planning scenarios or relegate them to post-2035 placeholders. Reversing course means reopening integrated resource plans and absorbing criticism from governing coalitions and campaign partners.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, national_energy_ministries, agenda_setter,
    institutional, generational, constrained, national).

% Developers and operators of reactor technology: state-owned incumbents, vendor-engineering firms, and utilities holding licenses for potential builds. They face closed doors in climate finance, exclusion from green taxonomies in major jurisdictions, and licensing environments that stretch timelines further. Assets and skills are long-lived and technology-specific, so exiting the sector means writing down decades of accumulated capability.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_vendor_utilities, payer,
    institutional, generational, constrained, continental).

% Welders, engineers, and tradespeople whose expertise is reactor-specific. Employment tracks a thin order book; when a national program is cancelled the workforce disperses and the apprenticeship pipeline breaks for a generation. Relocation abroad is realistic for a mobile minority; most are tied to plant regions by family and housing.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_skilled_workforce, payer,
    moderate, biographical, trapped, regional).

% Towns hosting existing stations or promised new ones, where jobs, tax base, and school enrollment ride on plant operations and construction. When portfolio rules steer investment elsewhere, replacement industries arrive slowly or not at all, and these towns hold no seat in the finance committees making the call.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, plant_host_communities, payer,
    powerless, generational, trapped, local).

% Fossil generator owners and fuel suppliers, absent from climate-finance conversations entirely. Whenever firm low-carbon capacity is deferred, their plants run the gap hours and their revenue streams lengthen. They lobby neither for nor against the rule; they collect what the sequencing leaves behind.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, gas_generation_interests, excluded,
    powerful, biographical, arbitrage, global).

% Independent modelers and grid engineers publishing capacity-expansion studies, firm-power cost analyses, and deployment-timeline audits. They hold no capital and take no institutional side; their work is the main external check on whether the speed arithmetic still holds as technology costs move.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, energy_systems_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__opportunity_cost_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the capital-scarcity triage problem: with finite mitigation finance and a closing carbon budget, it provides lenders, ministries, and campaign networks a shared decision rule that ranks technologies by avoided tonnes per dollar per year and directs capital to the front of that ranking.
% TRANSFER_FUNCTION: Moves mitigation capital, permitting priority, and political legitimacy away from long-duration, capital-intensive generation toward fast-deploying renewables and storage; within campaign networks, it moves standing and funder access toward members who adhere to the speed criterion.
% ABSENT_VOICES: Nuclear host communities and the reactor-specific skilled trades have no seat in portfolio deliberations that determine their regions' economic futures. Fossil generator interests are absent yet absorb the gap-hours the sequencing creates. Populations exposed to the consequences of getting the sequencing wrong are represented by no one in the room.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, screening policies at development banks would lose their organizing test, national procurement sequences would be reopened, campaign coalitions would lose the allocation logic that disciplines their messaging and funding, and capital currently routed by speed-ranking would renegotiate its destination across every low-carbon technology simultaneously.
% FOUNDING_PROBLEM: Post-Paris urgency: cumulative carbon budgets reward speed, and planners feared that waiting for perfect firm power would burn the remaining budget while deploying nothing.
% FOUNDING_PROBLEM_CORROBORATION: IPCC and IEA scenario literature corroborates the speed-weighting core from outside the benefiting parties; nuclear engineering bodies and independent grid analysts dispute the exclusion corollary, citing standardized fleet builds completed inside a decade.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-substantial (0.58 at interval end): the rule does not seize existing assets, but it denies a class of actors access to the capital, taxonomies, and planning scenarios that determine whether their technology exists in 2040, and the denial is decoupled from any harm the technology's operation causes. Suppression (0.52) is a mix of structural and normative mechanisms — roughly 60% structural (lender screens, taxonomy design, licensing environments) and 40% internalized (movement norms that make deviation professionally and socially expensive within climate institutions); the internalized share is why suppression persists even where formal policies soften. Theater ratio (0.30) is rising: early-period activity was dominated by genuine cost and deployment analysis, while a growing share of current activity defends the exclusion rhetorically as its empirical premises age. Accessibility collapse is low-moderate (0.40) because alternatives remain genuinely open — efficiency, demand flexibility, geothermal, hydro, and other firm-clean options stay investable; the rule narrows the option space without collapsing it. Resistance (0.62) is real and organized: industry advocacy, national reactor programs, and international pledges press against the rule continuously, which is why suppression_requirement rose across the interval before easing slightly as counter-mobilization forced partial accommodations. All three tracked series run on one shared time grid (t = 0, 2, 4, 6, 8, 10) so every metric is authored at every examined point; the dynamics are monotonic-with-plateau rather than cyclical, driven by enforcement hardening against rising resistance rather than by oscillating external shocks.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute very different arrangements from the same structure. From the agenda-setter seats (lenders, ministries), the rule is prudent triage they administer and can defend line-by-line from published cost data. From the target seats (vendor utilities, skilled workforce, host communities), the same structure is a coordinated denial of livelihood and regional future enforced by actors who bear none of its costs. From the beneficiary seats, it is merely the correction of a historical distortion that favored the incumbent technology. The advocacy-coalition seat is the sharpest divergence: identity fusion with the exclusion makes the rule constitutive of member standing, so its holders experience the constraint as self-expression rather than imposition. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for renewable developers, storage manufacturers, and advocacy coalitions — the rule subsidizes them, and their exit options (mobile capital, portable skills) push them toward the beneficiary end. Victim declarations drive high directionality for vendor utilities, the skilled workforce, and host communities; trapped and constrained exit keeps them near the full-target end, since they cannot redeploy reactor-specific assets or region-bound labor. Agenda setters sit intermediate: lenders hold arbitrage-grade exit (their d is damped by mobility even while they administer the exclusion), while ministries are more bound to national planning commitments. Gas interests are excluded from the conversation yet incidentally subsidized by the sequencing — a qualitative asymmetry noted here rather than handled with a per-atom override, since an override keyed to their power level would misapply to unrelated agents.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem — allocating scarce mitigation capital under a closing carbon budget — remains live, so the rule has not outlived its function. The classification prevents two opposite mislabelings. Calling the arrangement pure coordination erases the identifiable actors who bear its asymmetric costs without consent; calling it pure extraction erases the genuine triage function that would persist even under a neutral allocator, since speed-weighted capital ranking solves a real collective-action problem. The forward risk is inertial drift: if storage costs and alternative firm-clean options mature until the speed premium collapses, the rule's analytic core would hollow out while its enforcement machinery persisted — the rising theater_ratio series is the early symptom to watch, and the fossil-leakage omega tests whether the rule is already failing by its own metric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the climate_mitigation_imperative kernel — the opportunity_cost_reading, which scores options by carbon-per-dollar-per-year. Sibling readings instantiate structurally different constraints: the portfolio_optimization_reading treats all low-carbon sources as complements and removes nuclear from the victim set (reliability value offsets cost), while the systems_transition_reading makes centralized generation the target regardless of technology and relocates the victim set to governance form rather than capital cost. Where is the disagreement located?',
    'The disagreement sits in the selection criterion itself: speed-weighted yield versus portfolio reliability versus governance form. Resolution requires the parties to converge on what the mitigation imperative optimizes for — an empirical question (which criterion minimizes realized warming) entangled with a conceptual one (whether the imperative is a triage problem or a transformation problem).',
    'Under the portfolio reading, this constraint''s victim set shrinks or dissolves and its extraction measure falls sharply; under the systems reading, the victim set expands to include centralized renewables as well. The computed type could move from tangled_rope toward rope (if the exclusion dissolves) or toward snare (if the target set generalizes).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Kernel-membership omega: this story is one reading of a contested imperative; sibling readings change the victim set and the metric of merit.').

omega_variable(
    nuclear_timeline_learning_curve,
    'Is the disqualifying timeline premise empirically robust — do standardized fleet construction programs compress build times enough to flip the opportunity-cost arithmetic?',
    'Compare completed fleet builds (multi-unit programs delivered inside a decade under serial standardization) against first-of-a-kind projects; if median delivery times for standardized designs fall below the threshold at which cumulative avoided carbon dominates alternatives, the premise fails.',
    'If timelines compress, the constraint loses its coordination justification and its extraction becomes uncovered — pushing classification toward snare; if they do not, the exclusion retains its empirical warrant and the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_timeline_learning_curve, empirical, 'Whether reactor build-time distributions invalidate the speed-disqualification premise.').

omega_variable(
    counterfactual_capital_substitution,
    'Does capital denied to long-build generation actually substitute into faster alternatives at the margin, or was the counterfactual rhetorical — would the money have sat idle or leaked to non-mitigation uses absent the rule?',
    'Trace marginal deployment responses to exogenous shifts in available mitigation finance: if renewable build rates track available capital closely, substitution is real and the rule''s transfer function operates as described; if build rates are permitting- or interconnection-limited instead, the binding constraint lies elsewhere.',
    'If substitution is weak, the rule''s measured extraction buys little coordination benefit — the tangled_rope reading degrades toward snare; if strong, the extraction purchases genuine additional avoided emissions and the hybrid classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_capital_substitution, empirical, 'Whether the capital-transfer counterfactual underlying the opportunity-cost claim is real.').

omega_variable(
    fossil_bridge_leakage,
    'Did deferring firm low-carbon capacity extend fossil generation through the transition gap — making the rule net-harmful by its own carbon-per-dollar metric?',
    'Decompose dispatch data in jurisdictions that followed the sequencing strictly: attribute gap-hour generation to fossil units and compare realized cumulative emissions against counterfactual portfolios including firm clean capacity; grid-operator reliability interventions during tight periods provide natural experiments.',
    'If leakage is material, the constraint fails its own vindicated proposition (cumulative carbon budget arithmetic) — the strongest possible internal refutation, converting the coordination story into cover and supporting reclassification toward snare; if immaterial, the rule survives its sharpest internal critique.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_bridge_leakage, empirical, 'Whether the sequencing extended fossil operation enough to negate its own speed advantage.').

omega_variable(
    movement_identity_enforcement,
    'Is the enforcement carried by campaign networks materially structural (funder access, platform control) or internalized (identity fusion in which the exclusion constitutes member standing)?',
    'Observe the standing and funding trajectories of members who publicly deviate: if deviation costs persist even where formal policies have softened, the mechanism is internalized; if costs track specific institutional gatekeepers, it is structural.',
    'If internalized, the constraint''s effective suppression outlasts any formal-policy reversal — dismantling lender screens would not dismantle the rule, and lifecycle decay models would overpredict how quickly the constraint dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(movement_identity_enforcement, conceptual, 'Structural versus internalized split in the movement-level enforcement mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmi_oc_read_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(cmi_oc_read_tr_t2, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 2, 0.17).
narrative_ontology:measurement(cmi_oc_read_tr_t4, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(cmi_oc_read_tr_t6, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(cmi_oc_read_tr_t8, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(cmi_oc_read_tr_t10, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(cmi_oc_read_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cmi_oc_read_be_t2, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(cmi_oc_read_be_t4, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(cmi_oc_read_be_t6, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 6, 0.57).
narrative_ontology:measurement(cmi_oc_read_be_t8, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(cmi_oc_read_be_t10, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cmi_oc_read_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement(cmi_oc_read_su_t2, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 2, 0.41).
narrative_ontology:measurement(cmi_oc_read_su_t4, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(cmi_oc_read_su_t6, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(cmi_oc_read_su_t8, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(cmi_oc_read_su_t10, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, systems_transition_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition: the colloquial label 'the climate mitigation imperative' covers three structurally distinct claims with distinct epsilon values, beneficiary/victim sets, and failure modes, and is therefore authored as three linked stories. This story (opportunity_cost_reading) holds epsilon for the speed-first allocation arrangement as this reading assesses it; portfolio_optimization_reading instantiates a complementarity-maximizing constraint in which nuclear sits outside the victim set; systems_transition_reading instantiates a governance-form constraint in which centralization itself is the target. The opportunity-cost reading exerts downstream influence on the portfolio reading by starving it of the capital its premise assumes, and coexists with the systems reading as rival live positions held by different factions. Per the epsilon-invariance principle, no single story averages across these readings; each carries its own stable epsilon over the standing arrangement it contests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
