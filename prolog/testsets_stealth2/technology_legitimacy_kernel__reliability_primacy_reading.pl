% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Dispatchability-Only Climate Technology Legitimacy Gate (Reliability-Primacy Reading)
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   A technology is legitimate for climate mitigation, under this constraint,
 *   if and only if it provides dispatchable, baseload-capable generation to
 *   ensure grid stability. The criterion operates as a legitimacy gate on
 *   climate policy: it decides which technologies are eligible for subsidy,
 *   portfolio-standard compliance value, capacity-market revenue, and
 *   standing as a real climate solution. As written it is carbon-blind (the
 *   test is dispatchability alone), so it admits the gas fleet alongside
 *   nuclear and hydro, while wind and solar qualify only by purchasing the
 *   qualification good of storage. Enforcement runs through
 *   capacity-accreditation methodologies, resource-adequacy proceedings,
 *   clean-firm procurement mandates, and reliability must-run designations;
 *   the criterion's authority rests on the practitioner tradition of
 *   power-systems operations, and an interpretive substructure (accreditation
 *   manuals, NERC standards, integrated-resource-plan modeling conventions)
 *   absorbs drift without revising the kernel. This story instantiates one
 *   reading of the technology-legitimacy kernel; the reading's committer
 *   structure is recorded in kernel_context and the omega variables. Claim
 *   and metrics are independent authored facts: I claim tangled_rope because
 *   the gate coordinates a genuine adequacy problem while extracting
 *   asymmetrically, and the metrics describe its observed operation.
 *
 * KEY AGENTS:
 *   - regional_transmission_operators: agenda setter (institutional/constrained) — administers the accreditation and adequacy machinery that operationalizes the criterion
 *   - incumbent_dispatchable_utilities: primary beneficiary and co-agenda-setter (institutional/constrained) — collects capacity revenues and rate-base returns on the dispatchable fleet the criterion protects
 *   - nuclear_industry: paradigm beneficiary (organized/identity_locked) — supplies the criterion's paradigm case; its identity is fused with the baseload value proposition
 *   - gas_generation_operators: opportunistic beneficiary (powerful/mobile) — admitted by the criterion's carbon-blindness; capital mobile across CCS and hydrogen pivots
 *   - storage_manufacturers: derivative beneficiary (organized/mobile) — sells the qualification good the gate makes mandatory
 *   - intermittent_renewable_developers: primary target (organized/constrained) — pays the storage qualification cost or forfeits legitimacy
 *   - ratepayers: diffuse target (powerless/trapped) — bears the retail reliability premium and stranded-cost recovery
 *   - climate_policy_advocates: excluded voice (organized/mobile) — contests the criterion publicly but lacks standing in its technical venues
 *   - grid_engineering_researchers: analytical observer (analytical/analytical) — documents the flexibility literature that contests the baseload premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.72).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.62).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Dispatchability-Only Climate Technology Legitimacy Gate (Reliability-Primacy Reading)").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, 'ec481484-415a-42e5-a295-04a1da8abb29').
narrative_ontology:cs_kernel_codification('ec481484-415a-42e5-a295-04a1da8abb29', distributed).
narrative_ontology:cs_authority_grounding('ec481484-415a-42e5-a295-04a1da8abb29', practice).
narrative_ontology:cs_interpretation_layer_present('ec481484-415a-42e5-a295-04a1da8abb29').
narrative_ontology:cs_reading_relation('ec481484-415a-42e5-a295-04a1da8abb29', technology_legitimacy_kernel__velocity_primacy_reading, influences).
narrative_ontology:cs_reading_relation('ec481484-415a-42e5-a295-04a1da8abb29', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('ec481484-415a-42e5-a295-04a1da8abb29', foundational, grid_stability_requires_dispatchable_baseload).
narrative_ontology:cs_axiom_status(grid_stability_requires_dispatchable_baseload, holdable).
narrative_ontology:cs_axiom_grounding('ec481484-415a-42e5-a295-04a1da8abb29', grid_stability_requires_dispatchable_baseload, empirically_contingent).
narrative_ontology:cs_axiom('ec481484-415a-42e5-a295-04a1da8abb29', secondary, firm_capacity_preconditions_transition).
narrative_ontology:cs_axiom_status(firm_capacity_preconditions_transition, holdable).
narrative_ontology:cs_axiom_grounding('ec481484-415a-42e5-a295-04a1da8abb29', firm_capacity_preconditions_transition, instrumental).
narrative_ontology:cs_reference_frame('ec481484-415a-42e5-a295-04a1da8abb29', dispatchable_baseload_sufficiency).
narrative_ontology:cs_drift_state('ec481484-415a-42e5-a295-04a1da8abb29', contemporary_high_renewables_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ec481484-415a-42e5-a295-04a1da8abb29', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, incumbent_dispatchable_utilities).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, large_hydro_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, gas_generation_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, storage_manufacturers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, grid_reliability_engineering_doctrine).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, resource_adequacy_planning_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the capacity markets, resource-adequacy assessments, and accreditation methodologies through which the dispatchability criterion is operationalized. They set how much firm capacity each technology is credited with, administer reliability must-run designations, and publish the reliability assessments that anchor the criterion's authority. They collect administrative fees and planning authority, not the capacity revenues themselves; their stake is institutional, since the firm-capacity framework is the core of their planning mandate and rebuilding it around flexibility would be a multi-year institutional project.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, regional_transmission_operators, agenda_setter,
    institutional, generational, constrained, continental).

% Own and operate the dispatchable fleet of nuclear, gas, hydro, and coal units and earn regulated returns and capacity payments on it. They file the integrated resource plans and reliability arguments that keep the criterion in force, and their rate base is collateral of the criterion: if legitimacy shifted to intermittent-plus-storage portfolios, their firm assets would strand. They cannot exit the arrangement without writing down the assets the criterion protects.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, incumbent_dispatchable_utilities, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, incumbent_dispatchable_utilities, agenda_setter).

% Supplies the paradigm case of the criterion: high-capacity-factor, always-on generation. It collects legitimacy, production tax credits, and portfolio carve-outs under the reliability framing. Its public identity, financing model, and workforce pipeline are all built on the baseload value proposition; a legitimacy criterion indexed to deployment speed or legacy-cost bounding would strip that proposition, and the industry has no alternative frame to exit into.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry, beneficiary,
    organized, generational, identity_locked, global).

% Operate dispatchable, reservoir-backed hydro fleets that qualify under the criterion by construction and collect capacity value and clean-energy designation for assets built decades ago. They bear little of the criterion's cost and support firm-capacity constructs that credit their storage capability.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, large_hydro_operators, beneficiary,
    institutional, generational, constrained, regional).

% Operate dispatchable gas fleets that qualify under the criterion as written, which contains no carbon condition, so dispatchability alone admits them. The reliability framing is their principal channel into climate policy: it funds CCS retrofit subsidies, justifies capacity payments, and resists retirements. Their capital is mobile across hydrogen blending, CCS retrofits, and peaker economics, so their position under the criterion is opportunistic rather than fused.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, gas_generation_operators, beneficiary,
    powerful, biographical, mobile, continental).

% Sell the qualification good: under this criterion, intermittent generation becomes legitimate only by purchasing enough storage to behave dispatchably. Every tightening of the criterion expands their addressable market. They profit from the gate without setting it, and their exit is unconstrained because their product sells into any portfolio standard, in whichever direction the criterion moves.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, storage_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Develop wind and solar that the criterion disqualifies absent storage. They pay the qualification cost of storage buildout priced as a reliability tax, or forfeit subsidy eligibility, portfolio-standard compliance value, and capacity-market revenues. Their volume lets them contest the criterion in public, but their standing in the accreditation and resource-adequacy venues where it is enforced is thin.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers, payer,
    organized, biographical, constrained, continental).

% Pay the retail bill that funds the reliability premium: capacity payments to firm generators, over-built firm margins, and stranded-cost recovery when baseload assets lose value in a high-renewables grid. They cannot exit the grid, their attention is diffuse, and their organized advocacy is outnumbered in the technical proceedings where the premium is set.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers, payer,
    powerless, biographical, trapped, continental).

% Contest the criterion in public discourse, arguing it carbon-locks the fleet and slows the transition, but hold little standing in the resource-adequacy dockets, accreditation technical conferences, and integrated-resource-plan proceedings where the criterion is actually administered. They would admit renewables-plus-storage and flexibility as legitimate and reindex legitimacy to emissions outcomes.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_policy_advocates, excluded,
    organized, generational, mobile, continental).

% Study whether the dispatchable-baseload premise matches observed grid operation. Their literature increasingly frames reliability as a flexibility and ramping problem rather than a baseload problem, and documents high-renewables systems maintaining adequacy without new firm capacity. They collect nothing and pay nothing; their publications are the principal external check on the criterion's technical premise.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_engineering_researchers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__reliability_primacy_reading, incumbent_dispatchable_utilities).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__reliability_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real coordination problem in decarbonization planning: portfolios retiring synchronous fossil generation must maintain frequency stability and resource adequacy, and a shared criterion for which technologies count as firm capacity lets planners, regulators, and investors coordinate procurement, accreditation, and portfolio composition instead of each jurisdiction improvising its own adequacy standard.
% TRANSFER_FUNCTION: Moves policy legitimacy, subsidy eligibility, portfolio-standard compliance value, and capacity-market revenues toward dispatchable generators (nuclear, hydro, geothermal, gas), and moves qualification costs (mandatory storage buildout for intermittent renewables) and the retail reliability premium (capacity payments, stranded-cost recovery) onto renewable developers and ratepayers.
% ABSENT_VOICES: Climate policy advocates and demand-side flexibility specialists would object that the criterion carbon-locks the fleet and ignores the flexibility literature, and they are largely absent, without formal standing, from the resource-adequacy dockets, capacity-accreditation technical conferences, and integrated-resource-plan proceedings where the criterion is operationalized. The unanimity of the technical venues partly reflects who is seated there.
% DISAPPEARANCE_RATIONALE: If the criterion vanished overnight, capacity-accreditation methodologies, clean-firm procurement mandates, and the reliability arguments underwriting gas-life extensions and nuclear carve-outs would lose their shared standard; portfolio standards, subsidy design, and resource-plan modeling conventions would reorganize around a competing legitimacy criterion, and the revenue adequacy of the dispatchable fleet would be immediately in question.
% FOUNDING_PROBLEM: Twentieth-century grids were architected around synchronous, dispatchable generation, and early decarbonization policy needed a decision rule for which technologies could be trusted to replace retiring firm capacity without breaking the lights. The reliability-primacy reading was built to solve that resource-adequacy problem: admit as legitimate mitigation exactly the technologies that keep the grid stable.
% FOUNDING_PROBLEM_CORROBORATION: The underlying resource-adequacy problem is corroborated from outside the beneficiary set: NERC reliability assessments and system-operator loss-of-load studies document rising adequacy risk as firm capacity retires, and independent grid-engineering literature confirms reliability is a real constraint on transition portfolios. But the same external literature contests the reading's specific premise that adequacy requires dispatchable baseload rather than flexibility, so the founding problem is corroborated while the founding solution's framing is disputed by the strongest independent witnesses.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.72: the gate channels subsidy eligibility, portfolio compliance value, and capacity revenues toward the incumbent dispatchable fleet, imposes a priced qualification (storage) on its principal rivals, and, through carbon-blindness, extends legitimacy to gas generation that no emissions criterion would admit. Suppression is 0.62, authored as a raw unscaled structural property, and is structural rather than internalized: exclusion operates through venue standing, accreditation penalties, and must-run designations, not through targets' beliefs; ratepayer acquiescence reflects diffuse attention, not identity fusion. Theater is 0.44 and rising: the reliability concern is real, but a growing share of the criterion's invocation is rhetorical, with 'baseload' deployed against renewables in venues where the underlying engineering frame has shifted to flexibility. Accessibility_collapse is 0.5: renewables-plus-storage, demand response, transmission, and flexibility remain fully visible alternatives; the criterion does not make them unthinkable, it prices and disqualifies them. Resistance is 0.65: the renewables industry, climate advocates, and parts of the grid-engineering literature contest the criterion actively, with standing in public discourse though not in technical venues. The measurement series run on one shared grid (points 0-24 observed, roughly 2000-2024; point 30 projected to roughly 2030), and enforcement hardening is the traced dynamic, hence the suppression_requirement series.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter and beneficiary seats (system operators practicing reliability daily, utilities whose rate base is firm capacity, an industry whose identity is the baseload proposition) the criterion is not a gate at all but the engineering truth their institutions exist to uphold; the extraction they collect reads as payment for a service, firm capacity, they genuinely provide. From the payer seats, the same structure is an exclusionary device that prices their technology out of legitimacy and bills the difference to captive customers. The observer seat sees a criterion whose technical premise is migrating under it: adequacy metrics such as loss-of-load probability and marginal ELCC increasingly measure flexibility contribution rather than baseload status. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates them. Ratepayers, though powerless individually, are the seat where coalition potential is least exhausted: retail advocacy is thin, but the premium is legible on bills.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (utilities, nuclear, hydro, gas, storage) derive low directionality because the gate subsidizes them, with exit modulating the degree: nuclear's identity lock pins it near full beneficiary since it cannot reframe its value proposition, gas's mobile capital lets it arbitrage the criterion opportunistically, and storage sits nearest the pure-beneficiary end because its product sells in whichever direction the gate moves. Declared victims derive high directionality: renewable developers are constrained (they can pay to qualify or shrink) and ratepayers are trapped (no exit from the grid), so the reliability premium and qualification cost land at near-full target. Regional transmission operators are not declared beneficiaries and derive near-symmetric directionality: they administer the machinery and collect authority and fees, not the rents. No directionality overrides were needed; the beneficiary and victim declarations plus exit atoms differentiate the seats without them.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what keeps both failure modes visible. Reading the gate as a snare would erase the genuine coordination function (resource adequacy is corroborated by witnesses outside the beneficiary set, and a portfolio that ignores it fails publicly) and would hand the coalition a standing refutation, blackouts, it does not otherwise earn. Reading it as a rope would erase the asymmetric extraction: carbon-blind fossil admission, the priced qualification on rivals, and a retail premium whose incidence falls on trapped payers. The mandatrophy question turns on the founding problem's status: resource adequacy is live, but whether the baseload framing solves it is contested by the flexibility literature. If the engineering premise fully migrates to flexibility and the criterion survives on institutional inertia and rhetorical maintenance alone, the constraint drifts toward piton: enforcement decays, theater dominates, and no party profits enough to defend it. The baseload-versus-flexibility and storage-cost omegas are the tripwires for that drift, and the theater_ratio series (0.15 rising toward 0.44) is its leading indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the reliability-primacy reading of the technology_legitimacy_kernel; what structurally changes if the velocity-primacy sibling reading is adopted instead?',
    'Comparative classification of the sibling constraint files: adopt the velocity reading''s criterion (deployability at scale within the remaining carbon budget) and recompute the beneficiary and victim sets, holding this file''s referent fixed.',
    'Under the velocity reading, this reading''s beneficiary set largely inverts: nuclear and gas exit on construction timelines, renewables-plus-storage enters, and the extraction story relocates from reliability premiums to deployment delay. The two readings cannot be merged into one constraint without violating epsilon-invariance; each is a separate file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality: this story is one reading of the technology-legitimacy kernel; the velocity sibling would swap the beneficiary and victim sets.').

omega_variable(
    precautionary_sibling_delta,
    'What structurally changes under the precautionary sibling reading, and where exactly is the disagreement between the readings located?',
    'Classify the precautionary sibling (legitimacy iff worst-case failure modes and legacy costs are bounded and reversible within a generation) and diff its structural sets against this reading''s: the disagreement is located in the cost ledger, operational reliability costs for this reading versus legacy liability (waste, emissions, decommissioning) for the sibling.',
    'The precautionary reading evicts both nuclear (unbounded waste legacy) and unabated gas (unbounded emissions legacy) from the beneficiary set, shrinking it toward hydro, geothermal, and renewables-plus-storage. The readings coexist across factions because each polices a different cost ledger, but a single party holding both must conjoin the criteria.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(precautionary_sibling_delta, conceptual, 'Sibling delta: the precautionary reading relocates the criterion from operational stability to legacy-cost bounding, evicting nuclear and gas from the beneficiary set.').

omega_variable(
    carbon_blindness_of_criterion,
    'Is the operative reading carbon-blind as written (pure dispatchability admits unabated gas), or does the coalition implicitly assume a low-carbon dispatchable set?',
    'Textual analysis of the criterion''s operational carriers, including clean-firm procurement statutes, capacity-accreditation rules, and reliability testimony, for whether a carbon condition is ever attached, and whether gas operators in fact collect legitimacy rents under the reading.',
    'If carbon-blind, gas_generation_operators is a full beneficiary and the extraction story includes fossil legitimization; if an implicit carbon condition binds in practice, gas drops toward the excluded side and the beneficiary set contracts to nuclear, hydro, and storage-qualified resources.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_blindness_of_criterion, empirical, 'Whether the dispatchability criterion carries an implicit carbon condition in operational use.').

omega_variable(
    baseload_vs_flexibility_premise,
    'Is the criterion''s technical premise that grid stability requires dispatchable baseload still the operative engineering frame, or has flexibility superseded it?',
    'Power-systems literature and adequacy-metric evolution: if resource adequacy is demonstrably maintainable at high renewable shares without new baseload (loss-of-load studies, marginal-ELCC accreditation, operating records of high-renewables systems), the baseload premise is overridden operationally.',
    'If flexibility supersedes baseload, the constraint''s coordination function atrophies while the gate persists institutionally, which is piton drift: enforcement decays, theater dominates, and no party profits enough to defend the criterion. If baseload remains load-bearing, the coordination claim stands and the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_vs_flexibility_premise, empirical, 'Whether the baseload premise is engineering-live or a legacy frame the criterion persists on.').

omega_variable(
    storage_cost_qualification_dynamics,
    'Does the falling cost of storage collapse the qualification cost the gate imposes on renewables, dulling the constraint''s extractive edge?',
    'Storage cost-curve tracking against the effective qualification requirement (hours of storage demanded for accreditation parity): if qualification approaches costlessness, the gate''s differential burden on renewables vanishes.',
    'Collapsing qualification cost drifts the constraint toward rope (coordination without extraction); frozen or rising qualification cost, for example accreditation rules ratcheting storage requirements faster than costs fall, drifts it toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_cost_qualification_dynamics, empirical, 'Whether storage economics erode or entrench the gate''s differential burden on intermittent renewables.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t6, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(tech_tr_t6, observed).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(tech_tr_t12, observed).
narrative_ontology:measurement(tech_tr_t18, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement_basis(tech_tr_t18, observed).
narrative_ontology:measurement(tech_tr_t24, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(tech_tr_t24, observed).
narrative_ontology:measurement(tech_tr_t30, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(tech_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t6, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(tech_be_t6, observed).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(tech_be_t12, observed).
narrative_ontology:measurement(tech_be_t18, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 18, 0.64).
narrative_ontology:measurement_basis(tech_be_t18, observed).
narrative_ontology:measurement(tech_be_t24, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement_basis(tech_be_t24, observed).
narrative_ontology:measurement(tech_be_t30, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(tech_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t6, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement_basis(tech_su_t6, observed).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(tech_su_t12, observed).
narrative_ontology:measurement(tech_su_t18, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(tech_su_t18, observed).
narrative_ontology:measurement(tech_su_t24, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement_basis(tech_su_t24, observed).
narrative_ontology:measurement(tech_su_t30, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(tech_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'which climate technologies are legitimate for mitigation?' decomposes into three structurally distinct constraints, one per reading of the technology_legitimacy_kernel, because each reading fixes a different epsilon referent and a different beneficiary and victim set. This file is the reliability-primacy member; the velocity-primacy and precautionary members are separate stories linked here. The upstream member is this one: its engineering premise is the historically established frame, and its capacity-market and accreditation machinery creates downstream structural pressure on the velocity sibling's operating environment. Per epsilon-invariance, no single story may hedge across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
