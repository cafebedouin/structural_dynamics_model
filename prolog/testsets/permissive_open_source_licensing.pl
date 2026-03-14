% ============================================================================
% CONSTRAINT STORY: permissive_open_source_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_open_source_licensing, []).

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
 *   constraint_id: permissive_open_source_licensing
 *   human_readable: Permissive Open Source Licensing
 *   domain: software/intellectual_property/governance
 *
 * SUMMARY:
 *   Permissive open source licensing (MIT, Apache 2.0, BSD) represents a
 *   structural coordination mechanism that has evolved into an extraction
 *   constraint as corporate adoption has scaled. The constraint exhibits
 *   hybrid properties: it genuinely solves coordination problems (reducing
 *   legal friction, enabling code reuse) while simultaneously extracting
 *   value from original contributors to corporate beneficiaries. The theater
 *   ratio (0.65) reflects increasing performative framing around 'open source
 *   community' and 'shared values' masking asymmetric value flows. Individual
 *   contributors and small maintainers experience suppression through
 *   legitimacy narratives that frame extraction as inevitable ('code wants to
 *   be free') and ethical ('open source is freedom'). The constraint shows
 *   signs of degradation (increasing theater ratio) and accumulation (rising
 *   extractiveness) over the measured interval, with organized alternatives
 *   (GPL, Commons Clause) emerging as escape pathways. However, the 'too big
 *   to fork' problem and identity lock around open source ideology limit the
 *   practical effectiveness of these alternatives.
 *
 * KEY AGENTS:
 *   - Original Contributors: Primary victim (powerless/trapped) — individual developers whose labor is extracted without compensation or attribution obligation once code is licensed permissively
 *   - Small Maintainers: Secondary victim (moderate/constrained) — projects that become critical infrastructure but lack resources to manage security, maintenance, or governance; face unpaid labor and liability
 *   - Corporate Adopters: Primary beneficiary (institutional/arbitrage) — technology companies extracting value through free integration of mature libraries without contribution obligation or licensing friction
 *   - Downstream Derivative Creators: Secondary beneficiary (powerful/mobile) — mid-tier companies building proprietary products on permissive-licensed foundations with full relicensing freedom
 *   - Open Source Licensing Norm: Institutional actor (institutional/arbitrage) — the cultural system maintaining legitimacy of permissive licensing through 'freedom' rhetoric despite sustainability crises
 *   - Alternative Licensing Coalition: Organized agents (organized/constrained) — GPL advocates, Commons Clause promoters, dual-licensing movements attempting to migrate ecosystems to contribution-equity models
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing permissive licensing as inevitable property of 'how code works' rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_open_source_licensing, 0.38).
domain_priors:suppression_score(permissive_open_source_licensing, 0.48).
domain_priors:theater_ratio(permissive_open_source_licensing, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_open_source_licensing, extractiveness, 0.38).
narrative_ontology:constraint_metric(permissive_open_source_licensing, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(permissive_open_source_licensing, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_open_source_licensing, tangled_rope).
narrative_ontology:human_readable(permissive_open_source_licensing, "Permissive Open Source Licensing").
narrative_ontology:topic_domain(permissive_open_source_licensing, "software/intellectual_property/governance").

domain_priors:requires_active_enforcement(permissive_open_source_licensing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_open_source_licensing, corporate_adopters).
narrative_ontology:constraint_beneficiary(permissive_open_source_licensing, derivative_producers).
narrative_ontology:constraint_victim(permissive_open_source_licensing, original_contributors).
narrative_ontology:constraint_victim(permissive_open_source_licensing, commons_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINAL CONTRIBUTOR (SNARE) — Individual developers who contribute to permissive-licensed projects (MIT, Apache 2.0, BSD) have no exit once code enters the commons. Their labor produces value extracted by corporations for proprietary products with zero attribution obligation or contribution obligation. Trapped by the license terms they agreed to; cannot recover contributions. Maximum extraction with suppression through legitimacy framing ('open source is freedom').
constraint_indexing:constraint_classification(permissive_open_source_licensing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL MAINTAINER (TANGLED ROPE) — Maintainers of popular permissive-licensed libraries face coordination benefits (community feedback, issue triage, code contributions) alongside extraction (unpaid labor, dependency management burden, security liability without compensation). Cannot exit without abandoning project reputation; constrained by ecosystem dependence. Genuine coordination function mixed with asymmetric extraction.
constraint_indexing:constraint_classification(permissive_open_source_licensing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CORPORATE ADOPTER (ROPE) — Large technology companies experience permissive licensing as pure coordination: they can freely integrate, modify, and commercialize code without legal friction. The license solves their coordination problem (reducing licensing friction) while producing net benefit to their organization. Low extraction because they have arbitrage options (proprietary code, other licenses) and benefit from standardization.
constraint_indexing:constraint_classification(permissive_open_source_licensing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOWNSTREAM DERIVATIVE CREATOR (TANGLED ROPE) — Mid-tier companies building on permissive-licensed code benefit from reduced licensing constraints but are themselves constrained by competing upstream dependencies and maintenance burden they inherit. Can relicense their own derivatives freely but cannot change upstream terms. Mixed coordination (access to foundational code) and extraction (unpaid dependency management, security cascades).
constraint_indexing:constraint_classification(permissive_open_source_licensing, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LICENSING NORM SYSTEM (PITON) — The open source movement as institutional practice has largely become performative: the language of 'free software freedom' persists as normative covering while extraction flows from contributors to corporations continue. The norm is maintained through cultural inertia ('open source is good') despite documented sustainability crises. Theater ratio high because community governance rituals mask asymmetric value extraction.
constraint_indexing:constraint_classification(permissive_open_source_licensing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE LICENSING MOVEMENT (SCAFFOLD) — Organized efforts (copyleft licenses like GPL, Commons Clause, dual licensing, SSPL, Prosperity Public License) represent emerging recognition that permissive licensing creates unsustainable extraction. These alternatives have sunset logic: they are temporary frameworks while cultural/institutional norms shift to require contribution equity. Low effective extraction from this perspective because organized agents see exit pathways (license migration, ecosystem forking).
constraint_indexing:constraint_classification(permissive_open_source_licensing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, permissive licensing appears as an immutable property of how open source coordinates: the license terms are fixed contracts, corporations naturally optimize within legal frameworks, and value flows inevitably follow incentive gradients. This perspective risks naturalizing what are actually contingent institutional choices (the choice to use permissive rather than copyleft licenses, the choice to forgo contribution requirements).
constraint_indexing:constraint_classification(permissive_open_source_licensing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_open_source_licensing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(permissive_open_source_licensing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(permissive_open_source_licensing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(permissive_open_source_licensing, TR),
    TR >= 0.70.

:- end_tests(permissive_open_source_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Permissive licensing extracts contributor labor to corporate benefit, but the extraction is moderate rather than severe because: (1) significant genuine coordination benefits exist (reduced legal friction, code reuse), (2) some contributors volunteer explicitly for visibility/portfolio building, (3) corporations do contribute back in some cases (though measurement shows <5% return flow in most ecosystems). The moderate value reflects that the constraint has real coordination function alongside extraction. Suppression (0.48): Moderate-high. Contributors face multiple barriers to exit: psychological (identity lock to open source ideology), technical (once code is in the wild, contributors cannot control its use), and economic (small maintainers lack resources to fork or migrate). However, suppression is not total because copyleft alternatives exist and some projects successfully migrate. Theater ratio (0.65): Increasing. The open source community governance rituals (community councils, contributor agreements, roadmap discussions) are increasingly performative: they create appearance of shared decision-making while corporate priorities dominate through resource concentration and technical expertise. The interval shows theater increasing from 0.40 to 0.65, indicating degradation of authentic coordination function. Claimed type (Tangled Rope): The constraint exhibits both genuine coordination (license solves real legal/technical problems) and asymmetric extraction (value flows predominantly to corporations). The active enforcement (requirement to include license text, prohibition on sublicensing restrictions) is what maintains the extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The gap is structural and irreconcilable within the current institutional arrangement. Contributors and corporations occupy genuinely different positions relative to permissive licensing: what solves corporations' coordination problems directly enables extraction from contributors. This is not a disagreement about facts or values — it is a structural incompatibility. The constraint persists because corporations have institutional power to set defaults (GitHub recommends MIT; frameworks default to Apache 2.0) and because contributors are identity-locked to open source ideology, preventing collective action. The alternative licensing coalition's scaffold perspective is real but faces structural barriers: the 'too big to fork' threshold means once a library becomes critical infrastructure, license migration is technically impossible (all downstream projects would need to accept new terms, which they will not do).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from beneficiary/victim declarations and exit options. Original contributors (powerless, trapped, victim) compute high d → high f(d) → high experienced extraction. Corporate adopters (institutional, arbitrage, beneficiary) compute low d → negative f(d) → negative experienced extraction. Small maintainers (moderate, constrained, victim + beneficiary mix) compute mid-range d, producing moderate χ. The Tangled Rope classification requires active enforcement (license text requirement, prohibition on relicensing restrictions) which maintains the asymmetry. The scaffold perspective's lower chi derives from organized power and perceived exit options, reducing d. The piton perspective's theater_ratio gate captures that the open source norm system has become largely performative in maintaining legitimacy despite documented extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that classification depends entirely on the observer's structural position and time horizon. The original contributor sees permanent extraction (Snare at biographical horizon). The corporate adopter sees coordination (Rope at immediate horizon). The alternative licensing coalition sees temporary constraint with exit pathways (Scaffold at generational horizon). The piton perspective sees ritualized performance (theater ratio increasing from 0.40 to 0.65). No single type is 'correct' — the presheaf over the observation site describes the full structure. The natural law perspective (Mountain) is a false summit: permissive licensing is not inevitable or unchangeable, it is a contingent institutional choice made visible by comparing with copyleft alternatives that produce different extraction patterns. The mandatrophy resolution shows that the constraint's persistence despite awareness of extraction derives from a combination of: (1) identity lock in the contributor population (open source ideology preventing perception of mechanism), (2) institutional power concentration (corporations set defaults), (3) technical irreversibility ('too big to fork' barrier), and (4) performative legitimacy (theater masking extraction through 'freedom' rhetoric).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contribution_sustainability_threshold,
    'What proportion of contribution must flow back to original projects for permissive licensing to remain self-sustaining rather than extractive?',
    'Empirical analysis of contribution ratios in major ecosystems (Linux kernel, Python, Node.js); comparison of permissive vs copyleft ecosystems; longitudinal tracking of maintainer burnout rates',
    'If threshold < 10% return flow: most permissive ecosystems are already extractive (Snare). If threshold > 30%: permissive licensing can be sustainable (Rope). Current data suggests < 5% return flow in many ecosystems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contribution_sustainability_threshold, empirical, 'Sustainability threshold for permissive licensing contribution flows').

omega_variable(
    corporate_dependency_irreversibility,
    'Once a permissive-licensed library becomes critical infrastructure in corporate stacks, can the original contributors ever reassert control or change license terms?',
    'Historical case analysis (OpenSSL, jQuery, etc.); examination of attempted license migrations; measurement of ''too big to fork'' threshold',
    'If irreversible: contributors are permanently trapped once code scales (Snare). If reversible: exit options improve over time (Tangled Rope or Rope). Empirical evidence suggests high irreversibility once 10K+ projects depend.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corporate_dependency_irreversibility, empirical, 'Whether contributor control is irreversibly lost at scale').

omega_variable(
    alternative_license_adoption_feasibility,
    'Can organized coalitions successfully migrate open source ecosystems away from permissive licenses to contribution-equity models (GPL variants, Commons Clause, dual licensing)?',
    'Tracking license migration attempts; analysis of fork success rates; measurement of corporate resistance and adoption curves for alternative licenses',
    'If feasible: scaffold perspective is structural (exit pathways real). If infeasible: alternative licenses are purely aspirational (scaffold is performative, constraint remains Snare/Tangled Rope). Current adoption of alternatives shows modest but growing penetration (5-15% of new projects).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_license_adoption_feasibility, empirical, 'Feasibility of collective migration to alternative licensing models').

omega_variable(
    open_source_identity_lock,
    'Are contributors locked into permissive licensing by identity fusion with ''open source values'' (universalism, freedom rhetoric) that prevents perception of the extraction mechanism?',
    'Qualitative analysis of contributor narratives; comparison of contribution behavior under permissive vs copyleft models; measurement of awareness of value extraction among contributors',
    'If identity-locked: contributors experience constraint as mountain (unchangeable values) when it is actually contingent institutional choice (Snare/Tangled Rope). If not identity-locked: contributors correctly perceive extraction and could organize opposition. Anecdotal evidence suggests significant identity lock in subcultures of ''pure open source'' philosophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_identity_lock, conceptual, 'Whether contributors are identity-locked to permissive licensing ideology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_open_source_licensing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(posl_tr_t0, permissive_open_source_licensing, theater_ratio, 0, 0.4).
narrative_ontology:measurement(posl_tr_t5, permissive_open_source_licensing, theater_ratio, 5, 0.55).
narrative_ontology:measurement(posl_tr_t10, permissive_open_source_licensing, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(posl_be_t0, permissive_open_source_licensing, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(posl_be_t5, permissive_open_source_licensing, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(posl_be_t10, permissive_open_source_licensing, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_open_source_licensing, information_standard).
narrative_ontology:affects_constraint(permissive_open_source_licensing, software_commons_sustainability).
narrative_ontology:affects_constraint(permissive_open_source_licensing, corporate_open_source_capture).
narrative_ontology:affects_constraint(permissive_open_source_licensing, contributor_attribution_asymmetry).

% DUAL FORMULATION NOTE:
% Permissive licensing constrains multiple distinct phenomena: legal coordination (license solves genuine friction), value extraction (labor asymmetry), and norm maintenance (open source ideology). Upstream constraints concern the technical properties of open source (why code benefits from distributed development); downstream constraints concern the institutional capture of those benefits by corporations. This story models the value extraction constraint itself; related stories model sustainability crisis and attribution asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_open_source_licensing, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
