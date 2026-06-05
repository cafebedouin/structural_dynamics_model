% ============================================================================
% CONSTRAINT STORY: sotu_1963_kennedy_tax_base_broadening_structural_reform
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1963_kennedy_tax_base_broadening_structural_reform, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1963_kennedy_tax_base_broadening_structural_reform
 *   human_readable: Kennedy Tax Rate Cuts Coupled with Structural Base-Broadening Reform (1963-1964)
 *   domain: regulatory/fiscal_policy
 *
 * SUMMARY:
 *   In his 1963 State of the Union address, President Kennedy proposed a
 *   comprehensive tax reform coupling significant rate reductions with
 *   mandatory structural changes designed to close loopholes, eliminate
 *   'special preferences,' and remove 'unnecessary hardships' — generating
 *   $3.5 billion in offsetting revenue while broadening the tax base. The
 *   constraint operates as an equity mechanism that makes tax reduction
 *   fiscally defensible by tying it to base-broadening reform, preventing the
 *   tax system's structural erosion. Kennedy frames the reform as resolving
 *   an implicit contract violation: ordinary wage earners pay taxes on all
 *   income while holders of preferential positions shield portions of theirs.
 *   The coupling reestablishes horizontal equity (similar income should bear
 *   similar tax burden) while reducing vertical rates. This creates a tangled
 *   rope structure: genuine coordination function (broadening the base does
 *   eliminate distortions and simplify administration) alongside asymmetric
 *   extraction (preference-dependent agents lose shelter options). The
 *   constraint's theater ratio (0.55) reflects moderate performative content
 *   — the 'special preferences' language carries significant rhetorical
 *   weight beyond the technical base-broadening provisions, and the rate cuts
 *   serve as political incentive for congressional action on less popular
 *   elements.
 *
 * KEY AGENTS:
 *   - Ordinary Wage Earners: Primary beneficiary (moderate/constrained) — gain from rate reduction and relief from subsidizing preferential positions, but constrained by inability to restructure income or access new shelters
 *   - Tax Shelter Holders: Primary victim (powerless/trapped) — lose sheltered positions with no exit; cannot maintain preferences while accepting rate cuts; cannot avoid the system
 *   - Preferential Deduction Beneficiaries: Victim cohort (institutional/constrained) — corporations and high-income individuals with special-interest deductions; face efficiency losses from closure
 *   - Tax Authority: Institutional beneficiary (institutional/arbitrage) — experiences pure coordination: closing loopholes simplifies administration, broadens revenue base, and increases structural integrity; can modulate enforcement
 *   - Reform-Oriented Business Coalition: Organized intermediate (organized/constrained) — small businesses and efficiency-oriented firms supporting cleaner tax code; see temporary enforcement cost but durable simplification benefit
 *   - Tax Preference Legacy System: Institutional actor (institutional/arbitrage) — accumulated deductions and exclusions from prior policy eras that persist through inertia rather than active function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the coupling as fiscal law rather than political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1963_kennedy_tax_base_broadening_structural_reform, 0.52).
domain_priors:suppression_score(sotu_1963_kennedy_tax_base_broadening_structural_reform, 0.48).
domain_priors:theater_ratio(sotu_1963_kennedy_tax_base_broadening_structural_reform, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1963_kennedy_tax_base_broadening_structural_reform, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1963_kennedy_tax_base_broadening_structural_reform, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1963_kennedy_tax_base_broadening_structural_reform, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1963_kennedy_tax_base_broadening_structural_reform, tangled_rope).
narrative_ontology:human_readable(sotu_1963_kennedy_tax_base_broadening_structural_reform, "Kennedy Tax Rate Cuts Coupled with Structural Base-Broadening Reform (1963-1964)").
narrative_ontology:topic_domain(sotu_1963_kennedy_tax_base_broadening_structural_reform, "regulatory/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1963_kennedy_tax_base_broadening_structural_reform).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1963_kennedy_tax_base_broadening_structural_reform, ordinary_wage_earners).
narrative_ontology:constraint_beneficiary(sotu_1963_kennedy_tax_base_broadening_structural_reform, equitable_tax_system_integrity).
narrative_ontology:constraint_victim(sotu_1963_kennedy_tax_base_broadening_structural_reform, tax_shelter_holders).
narrative_ontology:constraint_victim(sotu_1963_kennedy_tax_base_broadening_structural_reform, preferential_deduction_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAX SHELTER HOLDER (SNARE) — Trapped by the policy coupling: cannot retain preferential treatment while rate cuts proceed; cannot exit without accepting higher effective tax burden. The reform extracts preferential positions without exit options. Maximum experienced extraction from this agent's structural position.
constraint_indexing:constraint_classification(sotu_1963_kennedy_tax_base_broadening_structural_reform, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORDINARY WAGE EARNER (TANGLED ROPE) — Benefits from rate reduction but constrained by inability to avoid the system; experiences genuine coordination (fair taxation without subsidizing preferential treatment) alongside extraction (losing shelter options that peers previously enjoyed). Mixed structural position.
constraint_indexing:constraint_classification(sotu_1963_kennedy_tax_base_broadening_structural_reform, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TAX AUTHORITY (ROPE) — Experiences the constraint as pure coordination: closing loopholes and broadening the base solves the fundamental tax system problem (revenue leakage and preference distortion) while enabling rate reduction. Net coordination benefit — the authority gains both structural integrity and simplified administration. Arbitrage exit: can modulate enforcement intensity.
constraint_indexing:constraint_classification(sotu_1963_kennedy_tax_base_broadening_structural_reform, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM-ORIENTED BUSINESS COALITION (SCAFFOLD) — Organized actors supporting base broadening see this as temporary enforcement of structural change with a sunset: once preferential provisions are eliminated and a cleaner base is established, ongoing maintenance requires less intervention. The coalition experiences extraction during transition but sees an exit path as tax administration normalizes around the broader base.
constraint_indexing:constraint_classification(sotu_1963_kennedy_tax_base_broadening_structural_reform, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TAX PREFERENCE LEGACY SYSTEM (PITON) — The pre-1963 system of accumulated tax preferences is largely theatrical: each preference was justified by historical policy rationales (capital formation, rural development, regional equity) but by 1963 persists through institutional inertia rather than active function. The Kennedy reform targets this theater — the preferences maintain political legitimacy without delivering economic benefit. Theater ratio high because the preferences' stated purpose has decoupled from their effect.
constraint_indexing:constraint_classification(sotu_1963_kennedy_tax_base_broadening_structural_reform, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the coupling of rate reduction to base broadening appears as an inescapable structural law: any durable tax reduction must be paired with revenue replacement, which requires closing preferences — this is presented as inherent to fiscal arithmetic. However, this naturalizes what is actually a political choice (whether to offset the revenue loss at all, or to accept deficits). The constraint is classified as mountain only from this analytical distance.
constraint_indexing:constraint_classification(sotu_1963_kennedy_tax_base_broadening_structural_reform, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1963_kennedy_tax_base_broadening_structural_reform_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1963_kennedy_tax_base_broadening_structural_reform, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1963_kennedy_tax_base_broadening_structural_reform, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1963_kennedy_tax_base_broadening_structural_reform, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1963_kennedy_tax_base_broadening_structural_reform, TR),
    TR >= 0.70.

:- end_tests(sotu_1963_kennedy_tax_base_broadening_structural_reform_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The reform extracts preferential positions from shelter holders and forces-broadens the tax base, but the extraction is constrained by the legitimate coordination goal (closing distortions, improving horizontal equity) and the rate reduction partially compensates ordinary earners. The value reflects that while preference holders experience significant loss, this is offset by genuine structural benefits. Initial value (0.28) represents pre-reform shelter-dependent system with lower perceived extraction because preferences are normalized. Value increases sharply at t=2 (0.38) as enforcement mechanisms activate post-passage, then reaches peak (0.52) as full base-broadening provisions take effect. Value declines slightly at t=10 (0.48) as the system stabilizes and actors adapt to the new tax base configuration — some of the perceived extraction dissipates as the new equilibrium becomes normal. Suppression (0.48): Moderate. Preference-dependent agents face significant barriers to exit: cannot restructure income without tax consequences, cannot access eliminated shelters, face compliance requirements for new base provisions. But suppression is not total — high-income taxpayers retain substantial arbitrage capacity (legal tax planning, timing strategies, relocation options), and the policy includes transition provisions. Theater ratio (0.55–0.68): Moderate, with interesting trajectory. Initial theater (0.62) is high because Kennedy's 'special preferences' rhetoric carries more weight than the technical provisions, framing the reform as moral correction. Theater declines as implementation proceeds (0.58 at t=2, 0.55 at t=5) because the actual provisions replace the performative language — administration focuses on technical base definition. Theater rises again at t=10 (0.68) as new preferences begin emerging as shelters close (indicating the cycle restarting — the constraints are partially theatrical because the underlying preference-seeking behavior persists).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence across institutional actors. Tax shelter holders see snare (0.85–0.95): they experience pure extraction with no coordination benefit and no exit. Ordinary wage earners see tangled rope (0.45–0.55): they gain from rate cuts but lose some deduction options — mixed coordination and extraction. The tax authority sees rope (0.15–0.25): the constraint solves their core coordination problem (base definition and revenue integrity) with net positive effect. The reform coalition sees scaffold (0.45–0.50 transitional, declining): they experience enforcement burden during transition but perceive durability of the cleaner base as the sunset — temporary extraction yielding to structural simplification. The legacy preference system is piton (0.68): accumulated provisions persist through institutional inertia despite eroding policy function; the reform explicitly targets this theater. The analytical observer sees mountain (0.72): from civilizational distance, the coupling appears as fiscal law (any durable rate cut must pair with revenue offset). The perspectival gap reveals that 'preference elimination' is not symmetrical extraction — it differentially impacts agents by their structural dependence on shelters, and the rate reduction differentially benefits agents by their income source and access to remaining deductions. The constraint's legitimacy depends on which perspective dominates the political frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Kennedy's framing couples rate reduction to base broadening through the equity principle: ordinary earners pay on all income; preference holders should not shield portions. This reframes base broadening from 'increasing taxes' to 'eliminating unfair subsidies' — a crucial rhetorical move. The directionality computation reflects this frame: beneficiaries are 'ordinary wage earners' and 'equitable tax system integrity' (abstract collective good); victims are 'tax shelter holders' and 'preferential deduction beneficiaries' (specific groups losing advantages). The extraction is concentrated on those with shelter dependence (high d), while distributed across broader earner groups (moderate d) via rate cuts. Tax authority experiences negative d (institutional beneficiary) because base broadening solves its structural problem. The coupling is critical to directionality: without rate cuts, the base broadening would appear as pure extraction (snare). With rate cuts, it appears as mixed (tangled rope). The rate cuts are the legitimation mechanism that makes the extraction acceptable.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY via structural coupling: The constraint avoids the mandatrophy (labeled as tangled rope with clear coordination + extraction properties) by explicitly embedding both functions in the policy design. The coordination function is base broadening (solves tax system distortion and administrative complexity). The extraction function is preference elimination (concentrates costs on preference-dependent agents). Kennedy's innovation is coupling these such that the extraction is framed as correcting an injustice (ordinary earners subsidizing preferences) rather than as pure redistribution. This is mandatrophy-resolving because it prevents mislabeling as 'just a rate cut' (which would miss the extraction) or 'just closing loopholes' (which would miss the coordination). The constraint is genuinely tangled: real coordination benefit (base broadening improves tax system function) plus real asymmetric extraction (preference holders lose positions). The rate reduction is the mechanism that makes the extraction tolerable — it creates net beneficiaries (ordinary earners with rate cuts exceeding lost deductions) as well as net losers (preference holders). Without the rate reduction, the constraint would be pure snare (extraction without compensation). The theater ratio (moderate, declining, then rising) reflects the transition: initially performative language dominates, then implementation replaces theater with technical change, then new theater emerges as actors adapt by creating new shelters. The long-term trajectory (theater rising at t=10 despite base broadening 'success') suggests the underlying preference-seeking behavior is structural — the constraint extracts from specific shelter vehicles but cannot prevent preference creation entirely. This is captured in omega_shelter_elimination_vs_replacement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preference_elimination_sufficiency,
    'Do the identified $3.5 billion in base-broadening provisions actually eliminate ''special preferences'' or merely redistribute who benefits from them?',
    'Post-implementation analysis: track whether closed provisions are replaced by new preferences; measure actual revenue generation vs. projected $3.5 billion offset; compare effective tax rates across income levels pre- and post-reform',
    'If preferences genuinely eliminated: reform succeeds as base broadening, snare classification holds. If new preferences emerge: reform is partial, and the constraint reclassifies toward tangled_rope or piton (theater increases as preferences migrate to new vehicles).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preference_elimination_sufficiency, empirical, 'Whether base-broadening provisions eliminate or redirect tax preferences').

omega_variable(
    rate_cut_necessity_coupling,
    'Is the rate reduction structurally necessary for the base-broadening reform to succeed, or is it a political sweetener that could be decoupled from the structural changes?',
    'Counterfactual analysis: would Congress have enacted the same base-broadening provisions without rate cuts? Historical comparison to other tax reforms with/without rate coupling. Political economy analysis of coalition formation.',
    'If rate cuts are structurally necessary: the coupling is a genuine coordination mechanism (tangled rope legitimacy confirmed). If rate cuts are political incentives: the coupling is an extraction mechanism disguised as coordination (snare risk increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rate_cut_necessity_coupling, preference, 'Whether rate cuts are structurally necessary or politically contingent').

omega_variable(
    shelter_elimination_vs_replacement,
    'After 1964, do eliminated shelters remain eliminated, or do new shelters emerge within 5-10 years as beneficiaries adapt?',
    'Longitudinal tax code analysis: track emergence of new deductions, exclusions, and preferences post-1964; measure effective tax rate dispersion across income levels; identify sectors adopting alternative shelter vehicles (corporate structure changes, executive compensation vehicles, etc.)',
    'If shelters remain eliminated: reform is durable and snare classification is accurate. If new shelters emerge: the constraint''s extractiveness increases over time (measurement drift), and piton classification becomes relevant (the reform was theatrical, theater_ratio rising).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shelter_elimination_vs_replacement, empirical, 'Whether eliminated shelters remain closed or are replaced by new vehicles').

omega_variable(
    ordinary_earner_burden_shift,
    'Do ordinary wage earners experience a net benefit from the rate cuts, or do the base-broadening provisions extract more from them than rate reductions return?',
    'Distributional analysis: compute effective tax rate changes by income quintile; track shifts in statutory rates vs. base-broadening provisions'' impact on standard deductions, personal exemptions, and deduction limitations for middle-income earners',
    'If wage earners net positive: tangled_rope classification holds (genuine mixed benefit). If wage earners net negative: extraction from this group increases, and the constraint reclassifies toward snare (beneficiaries are only highest-income shelter holders).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordinary_earner_burden_shift, empirical, 'Whether ordinary wage earners achieve net tax benefit from the reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1963_kennedy_tax_base_broadening_structural_reform, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kenn_tr_t0, sotu_1963_kennedy_tax_base_broadening_structural_reform, theater_ratio, 0, 0.62).
narrative_ontology:measurement(kenn_tr_t2, sotu_1963_kennedy_tax_base_broadening_structural_reform, theater_ratio, 2, 0.58).
narrative_ontology:measurement(kenn_tr_t5, sotu_1963_kennedy_tax_base_broadening_structural_reform, theater_ratio, 5, 0.55).
narrative_ontology:measurement(kenn_tr_t10, sotu_1963_kennedy_tax_base_broadening_structural_reform, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(kenn_be_t0, sotu_1963_kennedy_tax_base_broadening_structural_reform, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(kenn_be_t2, sotu_1963_kennedy_tax_base_broadening_structural_reform, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(kenn_be_t5, sotu_1963_kennedy_tax_base_broadening_structural_reform, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(kenn_be_t10, sotu_1963_kennedy_tax_base_broadening_structural_reform, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1963_kennedy_tax_base_broadening_structural_reform, resource_allocation).
narrative_ontology:affects_constraint(sotu_1963_kennedy_tax_base_broadening_structural_reform, capital_gains_preference_structure).
narrative_ontology:affects_constraint(sotu_1963_kennedy_tax_base_broadening_structural_reform, corporate_income_taxation_framework).
narrative_ontology:affects_constraint(sotu_1963_kennedy_tax_base_broadening_structural_reform, personal_exemption_erosion_cycle).

% DUAL FORMULATION NOTE:
% The Kennedy tax reform decomposes into multiple constraint stories: the rate reduction itself (coordination, rope) is structurally distinct from base-broadening provisions (extraction, snare). This story models the coupled constraint that ties rate reduction to base broadening, creating the tangled rope hybrid. Upstream constraints include capital gains preference and corporate sheltering structures that the reform targets. Downstream constraints include the erosion of the new base as new preferences emerge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1963_kennedy_tax_base_broadening_structural_reform, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
