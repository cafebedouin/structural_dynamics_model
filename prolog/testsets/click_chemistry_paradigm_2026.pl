% ============================================================================
% CONSTRAINT STORY: click_chemistry_paradigm_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_click_chemistry_paradigm_2026, []).

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
 *   constraint_id: click_chemistry_paradigm_2026
 *   human_readable: Click Chemistry Paradigm
 *   domain: chemical_synthesis/materials_science
 *
 * SUMMARY:
 *   Click chemistry represents a paradigm shift in molecular synthesis that
 *   combines genuine coordination benefits (modular, predictable reactions
 *   solving real synthesis bottlenecks) with significant extractive
 *   mechanisms (displacement of traditional expertise, concentration of
 *   research funding, suppression of alternative methodologies). The
 *   constraint emerged circa 2000-2002 with Sharpless and colleagues
 *   formalizing the concept, and has dominated chemical synthesis discourse
 *   for over two decades. The paradigm exhibits a tangled rope structure: it
 *   genuinely solves coordination problems (accelerating drug discovery,
 *   enabling library synthesis) while simultaneously extracting value from
 *   traditional synthesis expertise and creating institutional lock-in
 *   through funding concentration and curriculum replacement. The rising
 *   theater ratio (0.35 → 0.61 over the interval) reflects increasing
 *   performative emphasis on 'click-compatible' claims in grant proposals and
 *   publications, suggesting that the paradigm is acquiring enforcement
 *   overhead to maintain adoption as initial efficiency gains saturate.
 *   Multiple non-click synthesis pathways remain valuable and irreplaceable,
 *   but institutional pressure to frame all work within click-chemistry
 *   vocabulary creates suppression effects on alternative research
 *   directions.
 *
 * KEY AGENTS:
 *   - Traditional Synthesis Practitioners: Primary victim (powerless/trapped) — expertise devalued; retraining required for career continuation; no exit without status loss
 *   - Catalysis Researchers: Secondary victim (moderate/constrained) — research programs pressured to fit click paradigm; method development funding reduced; exit requires accepting narrower scope
 *   - Pharmaceutical Industry: Primary beneficiary (institutional/arbitrage) — captures acceleration benefits; controls adoption timeline; maximum arbitrage optionality
 *   - Materials Research Groups: Secondary beneficiary (institutional/arbitrage) — benefit from modular synthesis; retain optionality to use traditional methods where needed
 *   - Chemistry Educators: Organized actors (organized/constrained) — face transition overhead; no permanent extraction (scaffold with sunset)
 *   - Traditional Reference Publishing: Institutional actor (institutional/arbitrage) — sees reduced practical utility; persists through inertia (piton classification)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing paradigm adoption as inevitable law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(click_chemistry_paradigm_2026, 0.38).
domain_priors:suppression_score(click_chemistry_paradigm_2026, 0.48).
domain_priors:theater_ratio(click_chemistry_paradigm_2026, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(click_chemistry_paradigm_2026, tangled_rope).
narrative_ontology:human_readable(click_chemistry_paradigm_2026, "Click Chemistry Paradigm").
narrative_ontology:topic_domain(click_chemistry_paradigm_2026, "chemical_synthesis/materials_science").

domain_priors:requires_active_enforcement(click_chemistry_paradigm_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(click_chemistry_paradigm_2026, modular_synthesis_adopters).
narrative_ontology:constraint_beneficiary(click_chemistry_paradigm_2026, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(click_chemistry_paradigm_2026, materials_research_groups).
narrative_ontology:constraint_victim(click_chemistry_paradigm_2026, traditional_synthesis_expertise).
narrative_ontology:constraint_victim(click_chemistry_paradigm_2026, catalysis_research_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL SYNTHESIS EXPERTISE (SNARE) — Decades of accumulated knowledge in multi-step synthesis, protecting group strategies, and iterative optimization become devalued. Early-career chemists trained in the old paradigm face career disruption; their skillset is rapidly displaced by modular click-chemistry protocols. No exit option: retraining in click chemistry requires accepting diminished expertise status. Maximum extraction from this epistemic community.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CATALYSIS RESEARCH COMMUNITY (TANGLED ROPE) — The shift to reliable, modular click reactions reduces funding and publication pressure for catalytic method development. Researchers must adapt their research programs to fit the click-chemistry paradigm or risk marginalization. However, the community benefits from accelerated synthesis downstream — fewer bottleneck reactions free up resources for other problems. Constrained exit: moving to click-focused research requires accepting narrower scientific identity, but staying with traditional catalysis means declining research relevance.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL INDUSTRY (ROPE) — Clear net beneficiary. Click chemistry reduces synthesis time, improves scalability, and enables rapid library synthesis for drug discovery. The constraint appears as pure coordination: accessing the click-chemistry toolkit solves genuine production bottlenecks. Maximum arbitrage position: firms can exit traditional synthesis entirely and adopt click protocols wholesale, extracting value from accelerated discovery cycles.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MATERIALS RESEARCH GROUPS (ROPE) — Access to modular, predictable synthesis pathways enables new materials to be synthesized faster and more reliably. Groups benefit from both the direct efficiency gains and the networking effects: click chemistry creates a shared vocabulary and toolkit that accelerates multi-group collaborations. High arbitrage: groups can selectively adopt click methods where they work best and maintain traditional methods where needed — genuine optionality.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EMERGING SYNTHETIC CHEMISTRY EDUCATORS (SCAFFOLD) — The paradigm shift creates temporary teaching and curriculum overhead: educators must retrain students and rebuild course materials around click chemistry. However, the constraint has a clear sunset: within one generation, students trained natively in click chemistry will enter the workforce, and curriculum transition overhead will disappear. The enforcement overhead (retraining costs, new protocols, equipment investment) is temporary and declining. This is scaffolding for a genuine transition.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL SYNTHESIS TEXTBOOK PUBLISHING (PITON) — Multi-volume references (Fiesers' Reagents, named reactions compendia) that were essential infrastructure are now increasingly theatrical. Publishers maintain them through institutional inertia — they remain canonical references and appear in courses — but practical utility has declined significantly. New graduate students consult them rarely; synthetic chemists look up click reactions online instead. The textbooks persist because institutional momentum and legacy adoption keep them in supply chains, not because they solve current problems. Classic piton signature: high theater, low functionality.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, click chemistry may appear as a natural discovery — the inherent limitations of traditional synthesis (slow, low-yielding, high-waste) are overcome by tapping into the universe's 'spring-loaded' reactions. The appearance is of discovering a law: certain reaction classes (azide-alkyne, thiol-ene) are simply more reliable than others, and this is a feature of chemical bonding itself. However, the structural data contradicts mountain classification: the paradigm's dominance is contingent on institutions adopting it, funding agencies rewarding it, and training programs teaching it — all of which are reversible.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(click_chemistry_paradigm_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(click_chemistry_paradigm_2026, TR),
    TR >= 0.70.

:- end_tests(click_chemistry_paradigm_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high, reflecting the asymmetric knowledge displacement and institutional concentration. The traditional synthesis expertise community loses career value systematically; the paradigm benefits disproportionately accrue to pharmaceutical and materials industry with pre-existing capacity to adopt. However, genuine efficiency gains reduce the pure extraction component — some of the benefit is real. The trajectory (0.22 → 0.38 over 10 years) reflects paradigm maturation: initial efficiency gains plateau, but institutional lock-in and enforcement overhead increase. Suppression (0.48): Moderate. Significant barriers include: (1) funding agencies increasingly prioritizing click-compatible research; (2) journals preferring click methodology visibility; (3) graduate curricula rapidly phasing out traditional synthesis depth; (4) career risk for scientists maintaining non-click focus. But suppression is not total — traditional synthesis expertise remains publishable, funded, and valued in specific domains (natural products, complex asymmetric synthesis). Theater ratio (0.61): Moderate-high and rising. Early click chemistry provided genuine efficiency demonstrations (e.g., rapid library synthesis, scalable peptide ligation). Current usage increasingly exhibits theater: papers claiming 'click-compatible' status for reactions that were never bottlenecks; grant proposals using click framing for research that doesn't benefit from modularity; journal review processes that reward click novelty over methodological rigor. The rising trajectory (0.35 → 0.61) indicates Goodhart drift: the metric (click-chemistry adoptability) is replacing the objective (synthetic efficiency).
 *
 * PERSPECTIVAL GAP:
 *   The traditional synthesis expert sees a snare — displacement without exit, extraction without apparent benefit. The catalysis researcher sees tangled rope — some efficiency benefits from click's modularity, but research identity and funding constrained. The pharmaceutical beneficiary sees rope — pure coordination, no extraction, optionality at every step. The educator sees scaffold — temporary transition overhead with a sunset when click-trained students become the majority. The journal publisher sees piton — ritual persistence without function. The civilizational analyst risks seeing mountain — click reactions as natural law (spring-loaded bonding is inherent to chemistry) — but the structural contingency (institutional adoption, funding concentration, paradigm enforcement) reveals this as naturalization of choice. The perspectival gap is maximal between those with arbitrage optionality (beneficiaries, institutional actors) and those with trapped exit (traditional expertise).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (pharmaceutical industry, materials researchers) occupy institutional power positions with high exit optionality (arbitrage). They can adopt click chemistry selectively, extracting benefits while maintaining traditional methods for irreducible problems. Their d-values derive from beneficiary status plus arbitrage exit, yielding low d and negative effective extraction from their perspective (χ negative). Victims (traditional synthesis experts, displaced researchers) occupy powerless to moderate positions with constrained to trapped exit options. Retraining in click chemistry requires accepting expertise status loss; moving to non-chemistry fields requires career restart. Their d-values derive from victim status plus trapped/constrained exit, yielding high d (0.80-0.95) and strongly positive experienced extraction. The paradigm enforcement (funding concentrations, curriculum changes) is institutional — it persists through organized agents' (universities, funding agencies) actions, not through external coercion. This makes the Tangled Rope classification appropriate: genuine coordination function (efficiency) plus asymmetric extraction (expertise displacement) plus institutional enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the tangled rope classification is not mislabeled pure extraction (snare). The genuine coordination function — click chemistry solves real synthesis bottlenecks and accelerates legitimate research — is documented in pharmaceutical outcomes and materials discovery timelines. Simultaneously, the extractive mechanism — displacement of traditional expertise and suppression of alternative methodologies — is structurally real and unambiguous. The classification is robust because BOTH components are irreducible: removing the coordination function (click chemistry doesn't actually accelerate things) makes the whole paradigm incoherent; removing the extraction (traditional expertise doesn't actually lose value) ignores observable career displacement. The rising theater ratio (0.35 → 0.61) confirms tangled rope integrity: theater increases precisely when efficiency gains saturate and institutional enforcement must work harder to maintain adoption. A pure snare would show constant theater; a pure rope would show declining theater as the coordination problem solves itself. The trajectory pattern matches tangled rope: efficiency up front, enforcement overhead rising, theater increasing as compensation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    click_universality_threshold,
    'What fraction of useful synthetic transformations can be accomplished via click chemistry without sacrificing chemical diversity or product complexity?',
    'Comprehensive analysis of published synthesis routes in major journals; mapping of synthetic problems to click-compatible vs click-incompatible solutions; structural diversity analysis of click vs non-click libraries',
    'If > 80% of synthetic problems have click solutions: click becomes genuinely universal coordination mechanism (Rope from all perspectives). If < 50%: paradigm creates extraction by forcing non-click problems into click frameworks (Snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(click_universality_threshold, empirical, 'What fraction of synthetic problems admit click solutions').

omega_variable(
    traditional_expertise_value_recovery,
    'Do traditional synthesis skills recover value in specialized domains (natural product synthesis, late-stage modification, complex asymmetric synthesis)?',
    'Longitudinal tracking of funding, publication rates, and career outcomes in specialized synthesis subfields; analysis of citation patterns for non-click methods in elite chemistry journals; survey of industrial hiring for specialized synthesis roles',
    'If recovery occurs: traditional expertise is temporarily devalued but structurally needed (Tangled Rope classifies correctly). If no recovery: traditional expertise is permanently displaced (Snare dominates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(traditional_expertise_value_recovery, empirical, 'Whether traditional synthesis expertise recovers value in specialist roles').

omega_variable(
    paradigm_lock_in_mechanism,
    'Is the shift to click chemistry paradigm driven by genuine efficiency gains, or by network effects and funding concentration that lock in the paradigm regardless of alternative quality?',
    'Comparative analysis of click vs non-click methods on identical synthetic targets; analysis of funding agency directives and their temporal correlation with paradigm adoption; interviews with program officers about prioritization criteria',
    'If efficiency-driven: paradigm is coordination (Rope dominates). If lock-in-driven: paradigm is extraction disguised as progress (Snare dominates; Tangled Rope analysis becomes theater detection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradigm_lock_in_mechanism, conceptual, 'Whether paradigm adoption is driven by efficiency or institutional lock-in').

omega_variable(
    sustainability_claims_verification,
    'Do click reactions (particularly azide-alkyne cycloaddition) actually deliver the claimed sustainability benefits (atom economy, waste reduction, safety) at industrial scale, or is this marketing theater?',
    'Life-cycle analysis of click vs traditional synthesis at industrial scale; safety incident data for azide chemistry; waste stream analysis from pharmaceutical manufacturing; cross-validation with sustainability researchers outside chemical industry',
    'If sustainable: paradigm offers genuine coordination benefit (Rope strengthens). If not: sustainability claims are enforcement theater, extracting legitimacy while sustaining environmental costs (suppression gate rises, Tangled Rope deepens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_claims_verification, empirical, 'Whether click chemistry delivers claimed sustainability benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(click_chemistry_paradigm_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(click_tr_t0, click_chemistry_paradigm_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(click_tr_t5, click_chemistry_paradigm_2026, theater_ratio, 5, 0.55).
narrative_ontology:measurement(click_tr_t10, click_chemistry_paradigm_2026, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(click_be_t0, click_chemistry_paradigm_2026, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(click_be_t5, click_chemistry_paradigm_2026, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(click_be_t10, click_chemistry_paradigm_2026, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(click_chemistry_paradigm_2026, information_standard).
narrative_ontology:affects_constraint(click_chemistry_paradigm_2026, pharmaceutical_synthesis_acceleration).
narrative_ontology:affects_constraint(click_chemistry_paradigm_2026, traditional_chemistry_curriculum_degradation).
narrative_ontology:affects_constraint(click_chemistry_paradigm_2026, catalysis_funding_concentration).

% DUAL FORMULATION NOTE:
% Click chemistry paradigm is a constraint family spanning multiple decomposed claims: (1) the structural efficiency of click reactions as a coordination mechanism (high confidence, approaches Rope), (2) the institutional displacement of traditional synthesis expertise (high confidence, Snare for those experts), and (3) the paradigm lock-in via funding and curriculum concentration (medium-high confidence, determines overall Tangled Rope classification). These three are linked causally — the institutional lock-in sustains the paradigm even when efficiency gains plateau. Each story in this family is structurally dependent on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(click_chemistry_paradigm_2026, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
