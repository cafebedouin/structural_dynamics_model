% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: OST Article II Non-Appropriation: International Regime Reading
 *   domain: international_law/space_commons/treaty_interpretation
 *
 * SUMMARY:
 *   The Outer Space Treaty's Article II declares that outer space and
 *   celestial bodies 'are not subject to national appropriation by claim of
 *   sovereignty, by means of use or occupation, or by any other means.' Yet
 *   the treaty provides no binding mechanism for distributing resource rights
 *   or allocating orbital infrastructure. The constraint examined here is the
 *   deferential legal structure in which Article II itself is
 *   non-appropriation language, but its scope and enforcement are deferred to
 *   a future international regime that remains uncodified 55+ years after the
 *   treaty's entry into force. This reading (the international_regime
 *   reading) holds that Article II functions as a temporary scaffold: it
 *   preserves negotiation space by preventing any single party or coalition
 *   from locking in an extraction-permissive or conservation-based regime
 *   unilaterally. The scaffold's sunset is contingent on multilateral regime
 *   negotiation, which has stalled at the distributional stage — major powers
 *   cannot agree on whether future extraction should be permitted (benefiting
 *   industrial powers with launch capability) or prohibited (benefiting all
 *   states equally in a commons preservation model). The constraint creates
 *   regulatory grey zone in which first-movers stake de facto claims while
 *   later entrants face path dependence. This reading is one of three: the
 *   extraction_permissive reading sees Article II as a temporary restraint
 *   pending regime negotiation to permit commercialization; the
 *   commons_conservation reading sees Article II as codifying a permanent
 *   commons principle that regime negotiation should reinforce. The
 *   international_regime reading holds that neither is authoritative absent a
 *   binding multilateral framework, and the uncertainty itself is the
 *   constraint's structure.
 *
 * KEY AGENTS:
 *   - Major Space Powers & Licensed Firms (institutional/arbitrage): US, Russia, China, EU, Japan; operate in grey zone; extract via de facto infrastructure occupation and de facto precedent-setting
 *   - Regime Negotiation Forum (organized/constrained): UNCOPUOS, ITU, multilateral working groups; benefit from uncertainty as it preserves negotiation space; constrained by zero-sum distributional conflict
 *   - Nascent Space Faring States (powerless/trapped): India, South Korea, emerging African programs; face extraction via late entry into grey zone; cannot challenge first-mover de facto claims; depend on regime negotiation outcome they cannot control
 *   - Technical Standards Bodies (institutional/arbitrage): ITU, ISO, IADC; solve genuine coordination problems (frequency allocation, debris tracking); operate on technical grounds without regime closure
 *   - Space Commons (powerless/trapped): Non-appropriation principle itself; trapped beneficiary because it has no enforcement agent; benefits from stalled negotiation but faces sunset as de facto property norms entrench
 *   - Analytical Observer (analytical/analytical): Risks naturalizing Article II ambiguity as immutable law rather than recognizing it as contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.35).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.48).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.35).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Non-Appropriation: International Regime Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_law/space_commons/treaty_interpretation").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, '03130565-2e96-49fc-8f5e-13d7f20a48ed').
narrative_ontology:cs_kernel_codification('03130565-2e96-49fc-8f5e-13d7f20a48ed', formalized).
narrative_ontology:cs_authority_grounding('03130565-2e96-49fc-8f5e-13d7f20a48ed', extraction).
narrative_ontology:cs_reading_relation('03130565-2e96-49fc-8f5e-13d7f20a48ed', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('03130565-2e96-49fc-8f5e-13d7f20a48ed', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_axiom('03130565-2e96-49fc-8f5e-13d7f20a48ed', foundational, appropriation_question_deferred_to_regime).
narrative_ontology:cs_axiom_status(appropriation_question_deferred_to_regime, holdable).
narrative_ontology:cs_axiom_grounding('03130565-2e96-49fc-8f5e-13d7f20a48ed', appropriation_question_deferred_to_regime, conventional).
narrative_ontology:cs_axiom('03130565-2e96-49fc-8f5e-13d7f20a48ed', foundational, neither_reading_canonically_authorized).
narrative_ontology:cs_axiom_status(neither_reading_canonically_authorized, holdable).
narrative_ontology:cs_axiom_grounding('03130565-2e96-49fc-8f5e-13d7f20a48ed', neither_reading_canonically_authorized, conventional).
narrative_ontology:cs_reference_frame('03130565-2e96-49fc-8f5e-13d7f20a48ed', article_ii_ambiguity_as_functional_deferral).
narrative_ontology:cs_drift_state('03130565-2e96-49fc-8f5e-13d7f20a48ed', contemporary_2017, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('03130565-2e96-49fc-8f5e-13d7f20a48ed', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, regime_negotiation_forums).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, technical_standardization_bodies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPACE COMMONS AS STATELESS BENEFICIARY (SCAFFOLD) — The non-appropriation principle itself has no enforcement agent. States, corporations, and individuals operate under temporary regulatory uncertainty (the scaffold). The commons benefits from the regime negotiation being stalled — no single party can lock in extraction rules unilaterally. But the trap is temporal: as first-movers accumulate de facto control (orbital slots, resource extraction infrastructure), the scaffold's sunset approaches. The constraint's function is to delay distribution of property rights long enough that negotiation remains theoretically open.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__international_regime, scaffold,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIME NEGOTIATION COALITION (SCAFFOLD) — Organized states and multilateral bodies (UNCOPUOS, ITU, regional space agencies) experience the constraint as a functional temporary support structure. The legal ambiguity (neither extraction-permissive nor conservation-based reading is authoritative) creates space for incremental norm-building: orbital debris mitigation, frequency coordination, launch licensing. The coalition sees a sunset: once a binding multilateral regime is negotiated, the Article II scaffold's function transfers to the regime itself. Low extraction because the uncertainty actually serves the coalition's interest in delaying zero-sum distribution.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__international_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FIRST-MOVER EXTRACTION REGIME (TANGLED ROPE) — Major space powers (US, Russia, China, EU) and their licensed commercial entities experience Article II as providing both coordination and extraction. The non-appropriation principle genuinely coordinates competition (prevents zero-sum military appropriation spiral). But the legal grey zone simultaneously enables extraction: first-movers stake claims to orbital infrastructure, launch windows, and resource extraction methods (orbital debris, fuel depots, lunar mining) that later entrants cannot easily challenge because no binding regime defines illegality. Extraction runs via de facto property norms, not legal prohibition. The powerful agent has arbitrage — they can operate freely in the grey zone and later influence regime negotiation.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__international_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TECHNICAL STANDARDS BODIES (ROPE) — International technical coordination bodies (ITU for frequency allocation, ISO for space systems, IADC for debris mitigation) experience the constraint as pure coordination. They solve genuine collective action problems: orbital slots must be allocated, communication frequencies must not interfere, debris must be tracked. The uncertainty in Article II actually supports their authority — they can operate on technical grounds without waiting for political regime closure. Low extraction, genuine coordination function, arbitrage exit (they can adapt standards as regime negotiation evolves).
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__international_regime, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NASCENT SPACE FARING STATES (SNARE) — Nations attempting to develop indigenous space capabilities (India, Japan, South Korea, emerging African space programs) face maximum extraction via regulatory grey zone. They lack the de facto infrastructure and diplomatic weight of major powers. They cannot stake orbital claims through de facto occupation because those slots are occupied. They face regime uncertainty when launching (is their launch licensed under extraction-permissive reading or conservation reading?). They have no arbitrage — they must navigate norms set by first-movers. The constraint traps them in a subordinate position: entering the commons late, after major powers have established de facto norms that look uncontroversial because they predate any binding regime.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__international_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: OST TREATY FRAMEWORK AS INSTITUTIONAL PITON (DEGRADED) — The Outer Space Treaty itself (Article II non-appropriation clause) has become a degraded institutional artifact. It performs symbolic legitimacy (all states invoke it) but lacks decisive interpretive authority (neither extraction-permissive nor conservation reading is conclusive). The treaty persists through institutional inertia — withdrawing from it or reinterpreting it explicitly would destabilize the regime negotiation forum. The theater is high (0.65): states invoke the non-appropriation principle while simultaneously staking de facto claims. The treaty continues to be cited because its ambiguity is functional — it preserves negotiation space. If it were reinterpreted definitively (one reading gains authority), the piton would collapse and either a snare or a rope would emerge.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__international_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the non-appropriation principle reflects an immutable property of global commons: territory that no single authority controls is inherently subject to coordination rather than property regimes. This reading sees Article II as codifying a natural law of commons governance — appropriation in the absence of a binding distribution mechanism is inherently contested and unstable. However, this perspective risks false-summit naturalization: the 'immutability' of the commons principle dissolves when major powers have sufficient unilateral capacity to enforce de facto property norms and later demand ex-post ratification through regime negotiation.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__international_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__international_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__international_regime, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, TR),
    TR >= 0.70.

:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate-low. The constraint's primary function in this reading is coordination through deferral — preserving negotiation space rather than extracting resources directly. However, the regulatory grey zone does enable first-movers to accumulate de facto claims that later entrants cannot easily challenge. The extraction is real but not as severe as a snare (0.46+) would indicate because the uncertainty works symmetrically against all parties in the short term, even if asymmetrically over time. The measurement trajectory shows extractiveness rising from 0.15 to 0.35 as de facto property norms accumulate and the window for regime negotiation narrows. Suppression (0.48): Moderate. Barriers to challenging first-mover claims include diplomatic cost, lack of de facto infrastructure, and coordination barriers (nascent space states cannot easily form a coalition against major powers). But suppression is not total — nascent states retain formal legal standing in UNCOPUOS and technical standards bodies. Theater ratio (0.65): Moderate-high. States invoke Article II's non-appropriation principle while simultaneously staking de facto claims via orbital infrastructure occupation, launch licensing, and unilateral resource extraction methods. The legal framework is invoked more for legitimacy than for functional constraint — major powers operate as if extraction-permissive reading were canonical while maintaining rhetorical commitment to non-appropriation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a stark perspectival gap between major-power beneficiaries and late-entrant victims. The major space powers and their licensed firms (powerful/arbitrage) see the scaffold as providing coordination with extraction benefit — they can operate freely in the grey zone and influence regime negotiation. The technical standards bodies (institutional/arbitrage) see pure coordination — frequency allocation, debris tracking, and orbital slot management are genuine collective action problems solved by technical bodies without regime closure. The regime negotiation coalition (organized/constrained) sees the scaffold as a functional temporary support that preserves their bargaining position. But nascent space faring states (powerless/trapped) see the constraint as entrapment: they enter the commons late, after first-movers have occupied key infrastructure, and they face extraction via de facto property norms that predate any binding regime. The analytical observer risks naturalizing the scaffold's ambiguity as an immutable property of commons governance rather than recognizing it as a specific institutional design that benefits some parties and harms others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position: their power level, exit options, and relationship to the extraction flow. Major powers with arbitrage exit experience low d (~0.15) — they can operate freely in the grey zone and influence regime outcomes. Nascent states with trapped exit experience high d (~0.95) — they cannot avoid the regulatory grey zone, cannot exit easily, and cannot shape the regime negotiation that will determine their future constraints. The regime negotiation coalition experiences moderate d (~0.50) — they benefit from the uncertainty that preserves their bargaining space, but they are also constrained by the zero-sum distributional conflict that stalls agreement. Technical standards bodies experience low d (~0.20) — they solve genuine technical problems on non-political grounds and have arbitrage options (they can adapt standards as regime negotiation evolves). The space commons itself experiences high d (~0.90) — as a stateless beneficiary, it has no enforcement agent and is trapped by the scaffold's structure, which preserves negotiation space at the cost of allowing de facto property accumulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint's classification depends critically on the kernel reading chosen. The international_regime reading (this one) produces SCAFFOLD with moderate extractiveness (0.35), reflecting the temporary nature of the regulatory grey zone and the openness of regime negotiation outcomes. The extraction_permissive reading would likely produce TANGLED_ROPE or SNARE (higher ε) because it sees Article II as preventing unilateral appropriation claims while simultaneously enabling commercialization — coordinating industrial competition while extracting from conservation-minded parties. The commons_conservation reading would likely produce ROPE or PITON (lower ε) because it sees Article II as protecting the commons principle regardless of regime negotiation outcomes. The three readings coexist at the political level: major powers favor extraction_permissive; developing nations and environmental coalitions favor commons_conservation; the international regime forum defaults to international_regime (deferral) to preserve negotiation space. The mandatrophy is not resolved by choosing one reading as 'correct' — it is resolved by recognizing that all three are live political positions with different structural consequences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_negotiation_feasibility,
    'Will the stalled multilateral regime negotiation ever conclude in a binding framework, or will the scaffold''s sunset remain indefinite?',
    'Tracking of UNCOPUOS and ITU negotiations; emergence of binding side agreements or protocol amendments; geopolitical shifts in space power distribution that alter negotiation incentives',
    'If regime materializes: scaffold reading confirmed; legal certainty replaces grey zone; extraction mechanisms become explicit. If negotiation remains stalled indefinitely: scaffold reading becomes aspirational — the constraint stabilizes as a piton (theatrical invocation without regime closure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_negotiation_feasibility, empirical, 'Whether multilateral space regime negotiation will conclude in binding framework').

omega_variable(
    de_facto_property_norm_lock_in,
    'At what point does first-mover occupation of orbital infrastructure become so extensive and politically defended that later-entrant firms cannot challenge it even if a binding regime eventually prohibits appropriation?',
    'Historical analysis of lock-in dynamics in analogous commons (EEZ fishing, spectrum allocation, Antarctic treaty); modeling of orbital infrastructure expansion and legal defenses mounted by first-movers',
    'If lock-in threshold is low (< 5-10 years): scaffold reading is operationally equivalent to snare for late-entrants even if regime eventually formalizes. If lock-in threshold is high (> 20 years): scaffold reading provides genuine window for negotiation before irreversible path dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_facto_property_norm_lock_in, empirical, 'Timeline for de facto property norm entrenchment in orbital infrastructure').

omega_variable(
    extraction_permissive_vs_commons_conservation_reading_applicability,
    'Which sibling reading (extraction-permissive or commons-conservation) will gain institutional authority if and when a binding regime is negotiated, and what determines which reading becomes canonical?',
    'Game-theoretic analysis of negotiating state preferences; analysis of geopolitical power distribution and its effect on regime content; examination of precedent from Antarctic Treaty, EEZ regime, and other commons governance cases',
    'If extraction-permissive reading gains authority: scaffold reading''s sunset converts the constraint to a tangled_rope or snare. If commons-conservation reading gains authority: scaffold reading''s sunset converts the constraint to rope or piton. The sibling reading that ''wins'' regime negotiations is not predetermined by the OST text — it is determined by negotiating power and coalition formation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_permissive_vs_commons_conservation_reading_applicability, empirical, 'Which sibling reading will gain authority in binding regime negotiation').

omega_variable(
    article_ii_authority_gap_intentionality,
    'Is the Article II ambiguity (neither reading is authoritative) a deliberate design choice to preserve flexibility, or an unintended consequence of Cold War diplomatic deadlock?',
    'Historical analysis of OST negotiation records and travaux préparatoires; examination of similar deferral clauses in other international treaties; analysis of whether subsequent state practice shows consensus on intent',
    'If intentional: the scaffold reading is the true reading — the OST was designed to defer appropriation questions to a future regime. If unintended: the scaffold reading is a post-hoc rationalization of a drafting failure, and one of the sibling readings (extraction-permissive or conservation) may have been the intended canonical reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_ii_authority_gap_intentionality, conceptual, 'Whether Article II ambiguity is intentional or unintended').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 1967, 2017).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost_intl_tr_t0, ost_article_ii_non_appropriation__international_regime, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ost_intl_tr_t25, ost_article_ii_non_appropriation__international_regime, theater_ratio, 25, 0.58).
narrative_ontology:measurement(ost_intl_tr_t50, ost_article_ii_non_appropriation__international_regime, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(ost_intl_be_t0, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ost_intl_be_t25, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(ost_intl_be_t50, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ost_intl_su_t0, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ost_intl_su_t25, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(ost_intl_su_t50, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, global_infrastructure).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, orbital_slot_allocation_tragedy).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, lunar_resource_extraction_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, satellite_frequency_coordination).

% DUAL FORMULATION NOTE:
% The OST Article II non-appropriation principle decomposes into three structurally distinct constraints corresponding to three readings of the kernel: (1) international_regime (this story) — deferral to future multilateral regime; (2) extraction_permissive — Article II as temporary restraint pending commercialization regime; (3) commons_conservation — Article II as permanent commons principle. Each reading has different ε (0.35, ~0.50, ~0.25 respectively) and different beneficiary/victim structures. The three readings coexist at the political level. Downstream constraints (orbital slot allocation, lunar resources, frequency coordination) are affected by which reading becomes institutionalized in regime negotiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, powerless, 0.92).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
