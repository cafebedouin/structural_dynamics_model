% ============================================================================
% CONSTRAINT STORY: world_factbook_sunset_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_world_factbook_sunset_2026, []).

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
 *   constraint_id: world_factbook_sunset_2026
 *   human_readable: CIA World Factbook Termination
 *   domain: geopolitical/information
 *
 * SUMMARY:
 *   The CIA World Factbook termination in 2026 represents a structural shift
 *   in how the US intelligence community balances public transparency with
 *   classified intelligence monopolization. For 60+ years, the Factbook
 *   served as a coordinating mechanism: it standardized country data
 *   globally, demonstrated US intelligence competence, and provided a
 *   baseline for public policy discourse. But this same public-facing
 *   function increasingly conflicted with executive intelligence
 *   consolidation and classified operation prioritization. Director
 *   Ratcliffe's framing of termination as a return to 'core missions' masks a
 *   deeper extraction: the constraint dissolves the open-source intelligence
 *   ecosystem's dependency on standardized US government data, leaving
 *   researchers, journalists, and smaller governments with fragmented
 *   alternatives. Simultaneously, it consolidates intelligence control within
 *   executive briefing systems, reducing external accountability mechanisms.
 *   The constraint exhibits all six types from different observational
 *   positions. The power asymmetry is stark: the executive chooses
 *   termination unilaterally; the dependent ecosystem cannot negotiate. Yet
 *   the constraint is not permanent — open-data coalitions can rebuild
 *   alternatives, and a future administration could restore the Factbook.
 *   This temporal asymmetry (real-time cost for researchers, reversible at
 *   policy level) is the defining feature of the Tangled Rope classification:
 *   genuine coordination function (intelligence consolidation solves internal
 *   CIA friction) layered with asymmetric extraction (researchers bear
 *   irreversible costs during the shutdown window).
 *
 * KEY AGENTS:
 *   - Executive Intelligence Consolidation (institutional/arbitrage): Primary beneficiary — reduces inter-agency coordination friction, consolidates intelligence focus, reallocates budget to classified operations
 *   - Open-Source Intelligence Ecosystem (powerless/trapped): Primary victim — journalists, researchers, NGOs dependent on standardized Factbook data lose access with no viable immediate alternative
 *   - Academic Research Community (powerless/trapped): Secondary victim — decades of research methodology and curriculum infrastructure depend on Factbook continuity; termination forces costly retraining
 *   - Congressional Oversight Bodies (organized/constrained): Mixed perspective — benefits from simplified executive briefing but loses independent verification tool; exit constrained by executive prerogative
 *   - Open-Data Preservation Coalition (organized/constrained): Positioned to build alternatives — Wikipedia, Internet Archive, Wikidata can absorb Factbook functions but require 2-3 years to mature; scaffold logic with real sunset
 *   - Intelligence Community Institutional Memory (institutional/arbitrage): Sees Factbook as degraded vestigial function; piton classification due to high theater ratio and atrophied primary function
 *   - Analytical Observer (analytical/analytical): Risks naturalizing intelligence monopoly as inherent to state sovereignty (false summit risk)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(world_factbook_sunset_2026, 0.38).
domain_priors:suppression_score(world_factbook_sunset_2026, 0.52).
domain_priors:theater_ratio(world_factbook_sunset_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(world_factbook_sunset_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(world_factbook_sunset_2026, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(world_factbook_sunset_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(world_factbook_sunset_2026, tangled_rope).
narrative_ontology:human_readable(world_factbook_sunset_2026, "CIA World Factbook Termination").
narrative_ontology:topic_domain(world_factbook_sunset_2026, "geopolitical/information").

domain_priors:requires_active_enforcement(world_factbook_sunset_2026).
narrative_ontology:has_sunset_clause(world_factbook_sunset_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(world_factbook_sunset_2026, executive_intelligence_prioritization).
narrative_ontology:constraint_beneficiary(world_factbook_sunset_2026, classified_intelligence_consolidation).
narrative_ontology:constraint_victim(world_factbook_sunset_2026, open_source_intelligence_ecosystem).
narrative_ontology:constraint_victim(world_factbook_sunset_2026, academic_researchers).
narrative_ontology:constraint_victim(world_factbook_sunset_2026, public_policy_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPEN-SOURCE INTELLIGENCE ECOSYSTEM (SNARE) — Cannot exit or substitute the World Factbook's 60-year accumulated intelligence baseline. Journalists, researchers, NGOs, and smaller governments depend on standardized, curated country data. No viable alternative exists with equivalent scope, accessibility, and historical depth. The ecosystem bears the full extraction cost of termination with no option to continue access or negotiate preservation.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC RESEARCH COMMUNITY (SNARE) — Decades of research infrastructure, curricula, and citation networks depend on World Factbook as a standardized reference. Researchers cannot redirect years of methodology overnight. Termination imposes retraining costs, data reconstruction, and reduced comparative research capability. Exit options are severely constrained — alternative sources lack the standardization and historical consistency required for longitudinal studies.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CONGRESSIONAL OVERSIGHT BODIES (TANGLED ROPE) — See dual coordination/extraction: the Factbook enables public accountability by providing standardized data for legislative analysis, but termination also serves executive consolidation of information control. Congress benefits from simplified intelligence briefing (less public access to verify claims) but also loses a tool for independent verification. Exit is constrained by executive prerogative over intelligence budgets.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE INTELLIGENCE CONSOLIDATION (ROPE) — Primary beneficiary. Termination redirects personnel, budget, and intelligence focus to classified operations and executive summary briefings. The constraint solves an internal coordination problem: the World Factbook's public-facing nature created friction between open-source and classified intelligence priorities. Consolidation reduces inter-agency coordination overhead. Executive retains full arbitrage — can reallocate resources or restart the Factbook if priorities shift.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN-DATA PRESERVATION COALITION (SCAFFOLD) — NGOs, libraries, and academic institutions see the termination as a temporary setback with a clear sunset path: preservation via distributed archiving (Internet Archive, academic data repositories, Wikipedia), crowdsourced data collection, and open-source alternatives (Wikidata, Nominatim, statistical databases). The coalition has agency to build alternatives and sees a clear restoration pathway — this constraint is not permanent, only a transition period. Theater ratio is moderate because preservation efforts are functional, not performative.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTELLIGENCE COMMUNITY INSTITUTIONAL MEMORY (PITON) — The World Factbook has operated as a vestigial public-relations function within the CIA for decades — a genuine coordination mechanism in the Cold War era (demonstrating US intelligence expertise, standardizing country data globally) but increasingly performative as classified briefings and proprietary intelligence platforms superseded public Factbook updates. The institutional inertia that kept it alive (bureaucratic presence, congressional expectation) is now insufficient to overcome budget pressure. Classification as piton reflects that the primary function has atrophied and theater (public communication) now exceeds core intelligence output.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INSTITUTIONAL MONOPOLY (MOUNTAIN) — From a civilizational perspective on institutional information control, some degree of intelligence monopolization is inherent to state sovereignty and classified operations. Governments always seek to control information flow; the structural tension between open intelligence (public accountability) and classified intelligence (security) is unavoidable. This perspective risks naturalizing what is actually a policy choice — that the executive CHOSE consolidation. The engine will flag this as a false summit: institutional monopoly is not an immutable law.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(world_factbook_sunset_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(world_factbook_sunset_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(world_factbook_sunset_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(world_factbook_sunset_2026, TR),
    TR >= 0.70.

:- end_tests(world_factbook_sunset_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from the open-source ecosystem by eliminating access to 60 years of standardized data. But extraction is not severe because alternatives exist (albeit fragmented) and the termination window is bounded — the ecosystem can rebuild over 2-3 years. The value reflects intermediate severity: worse than coordination (Rope), better than complete monopoly (Snare). Suppression (0.52): Moderate-high. The ecosystem faces real barriers to substitution: no single alternative matches the Factbook's scope, accessibility, historical depth, or institutional authority. Researchers cannot switch overnight without losing comparative capability. But suppression is not total — open-source alternatives can be built by organized coalitions with resources and motivation. Theater ratio (0.68): Moderately high. The World Factbook has operated with increasing theater for a decade — updates lag, coverage becomes uneven, and the public-facing function diverges from actual intelligence priorities. The termination exposes that theater has consumed much of the platform's actual coordinating function. However, theater is not dominant because the Factbook does perform real coordination: standardizing country data globally is a genuine intelligence function, even if secondary to classified operations. The value (0.68) reflects that performance functions exist but are overshadowed by performative ones.
 *
 * PERSPECTIVAL GAP:
 *   The executive sees Rope (coordination of internal intelligence priorities). The open-source ecosystem sees Snare (trapped extraction). Congressional oversight sees Tangled Rope (mixed coordination and loss of verification tool). The preservation coalition sees Scaffold (temporary with clear sunset). The institutional intelligence community sees Piton (degraded ritual worth ending). The civilizational analytical perspective risks seeing Mountain (inherent information monopoly) but this is a false summit: consolidation is a policy choice, not a law of physics. The perspectival gaps reveal that the constraint's classification depends entirely on whether you control the shutdown decision (executive = Rope) or depend on the resource being terminated (ecosystem = Snare). Time horizon matters: immediate (executive shutdown decision) vs. biographical (researcher career disruption) vs. generational (preservation coalition reconstruction) vs. civilizational (intelligence monopoly norms) all produce different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive Intelligence Consolidation: Institutional power + arbitrage exit → d ≈ 0.05 → low/negative f(d) → experiences constraint as coordinating benefit (Rope). Beneficiary status + exit control → minimal experienced extraction. Open-Source Ecosystem: Powerless + trapped → d ≈ 0.95 → maximum f(d) ≈ 1.42 → experiences constraint as severe extraction (Snare). Victim status + no exit → maximum experienced extraction. Congressional Oversight: Organized + constrained → d ≈ 0.40 → f(d) ≈ 0.40 → experiences mixed extraction (Tangled Rope). Partial beneficiary (simplified intelligence briefing) + constrained exit (executive prerogative) → moderate chi. Preservation Coalition: Organized + constrained but with agency to build alternatives → d ≈ 0.35 → f(d) ≈ 0.30 → experiences constraint as temporary coordination problem (Scaffold). Partial victim status but with exit pathway (building alternatives) → lower chi + sunset logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification resolves the mandatrophy by establishing that the constraint is BOTH a coordination mechanism (internal CIA efficiency) AND an asymmetric extraction (external ecosystem cost). The executive genuinely benefits from consolidation as coordination — reducing inter-agency friction is a real gain. But this coordination value is purchased by eliminating open-source access, which extracting from researchers and journalists. The constraint satisfies all three Tangled Rope gates: (1) requires_active_enforcement = true (executive must actively shut down and prevent public access), (2) beneficiaries = executive intelligence consolidation (genuine), (3) victims = open-source ecosystem (genuine). Without the tangled rope classification, the analyst risks misreading consolidation as pure efficiency (Rope) and missing the extraction from dependent researchers. The mandatrophy prevents that misread by requiring evidence of both coordination AND asymmetric cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    replacement_ecosystem_viability,
    'Can distributed open-source alternatives (Wikidata, academic repositories, proprietary analysis platforms) successfully replace the World Factbook''s coordinating function?',
    '6-12 month tracking of researcher adoption rates for alternative sources; comparative analysis of data standardization, update frequency, and coverage across alternatives; citation network analysis showing dependency migration',
    'If viable: Scaffold classification holds and sunset is real (temporary constraint). If not viable: constraint devolves to permanent Snare for researchers; beneficiaries win permanently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replacement_ecosystem_viability, empirical, 'Viability of open-source Factbook replacements').

omega_variable(
    executive_information_consolidation_intent,
    'Is the Factbook termination primarily a budget efficiency measure or a deliberate information control strategy to reduce public access to standardized country data?',
    'Document analysis of budget justifications vs. intelligence strategy memos; tracking of whether terminated Factbook functions migrate to classified platforms or are genuinely eliminated; observation of subsequent changes to public intelligence disclosure',
    'If efficiency-driven: constraint is neutral/transactional. If control-driven: extraction intent is clarified and the snare perspective is vindicated; beneficiary motivation becomes explicable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_information_consolidation_intent, conceptual, 'Intent behind Factbook termination (efficiency vs. control)').

omega_variable(
    public_intelligence_access_restorability,
    'Can a future administration restore the World Factbook or equivalent, or is 60 years of institutional data too degraded for restoration?',
    'Assessment of data preservation (whether terminated Factbook version is archived with restoration capability); organizational continuity (whether personnel knowledge and editorial processes are retained); cost-benefit analysis of rebuilding vs. restarting',
    'If easily restorable: the sunset is real and temporary (Scaffold logic). If degraded beyond recovery: the termination is functionally permanent (Snare permanent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_intelligence_access_restorability, empirical, 'Restorability of World Factbook after termination').

omega_variable(
    intelligence_monopolization_trade,
    'Does consolidating public intelligence into executive-only platforms increase classified intelligence actionability, or does it reduce external reality-checking and increase analytical bias?',
    'Longitudinal tracking of intelligence accuracy metrics before/after consolidation; comparative analysis of classified intelligence assessment performance vs. open-source baselines; investigation of whether removal of public data access correlates with intelligence failures',
    'If actionability improves: executive consolidation is efficient and justified. If accuracy declines: the constraint extracts in hidden ways (reduced accountability), elevating it from Tangled Rope toward Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intelligence_monopolization_trade, empirical, 'Intelligence actionability impact of public data consolidation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(world_factbook_sunset_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wfb_tr_t0, world_factbook_sunset_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(wfb_tr_t3, world_factbook_sunset_2026, theater_ratio, 3, 0.62).
narrative_ontology:measurement(wfb_tr_t6, world_factbook_sunset_2026, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(wfb_be_t0, world_factbook_sunset_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(wfb_be_t3, world_factbook_sunset_2026, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(wfb_be_t6, world_factbook_sunset_2026, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(world_factbook_sunset_2026, information_standard).
narrative_ontology:affects_constraint(world_factbook_sunset_2026, us_intelligence_classification_regime).
narrative_ontology:affects_constraint(world_factbook_sunset_2026, open_source_intelligence_standardization).

% DUAL FORMULATION NOTE:
% The World Factbook termination is downstream of broader US intelligence consolidation strategy (classified operations prioritization) but represents a distinct structural constraint on the open-source intelligence ecosystem. The upstream constraint (intelligence consolidation imperative) has extractiveness ~0.45 (systemic); the Factbook termination has extractiveness ~0.38 (institutional instantiation). Decomposition recognizes that the general consolidation logic and the specific Factbook decision are structurally related but separable — one could restore the Factbook while maintaining intelligence consolidation priorities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(world_factbook_sunset_2026, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
