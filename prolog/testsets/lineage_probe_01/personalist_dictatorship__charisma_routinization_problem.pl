% ============================================================================
% CONSTRAINT STORY: personalist_dictatorship__charisma_routinization_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personalist_dictatorship__charisma_routinization_problem, []).

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
 *   constraint_id: personalist_dictatorship__charisma_routinization_problem
 *   human_readable: Personalist Authority: Charisma Routinization Problem (Weber's Succession Crisis)
 *   domain: political/comparative
 *
 * SUMMARY:
 *   The charisma routinization problem is Max Weber's observation that
 *   personalist authority—lodged in the irreplaceable qualities of a single
 *   leader—cannot be institutionalized without destroying the very legitimacy
 *   that makes it effective. Every personalist regime faces the succession
 *   crisis: authority dies with the man. This reading of the
 *   personalist_dictatorship kernel focuses on the structural incompatibility
 *   between charismatic legitimacy and institutional succession, the
 *   suppression of formal succession mechanisms to maintain the ruler's
 *   irreplaceability claim, and the mortgaging of regime continuity
 *   institutions to one biological lifespan. The constraint exhibits
 *   tangled_rope structure: genuine coordination (unified command, rapid
 *   decision-making, loyalty-based control) coexists with severe extraction
 *   (suppression of succession pathways, institutional subordination to
 *   personal networks, weakening of any institution capable of operating
 *   independently). The suppression requirement and extractiveness rise over
 *   time (0.58→0.72 and 0.52→0.68) as the regime ages: the ruler's mortality
 *   becomes more salient, succession uncertainty increases, and suppression
 *   must intensify to prevent power consolidation by potential heirs or
 *   institutional rivals. Theater_ratio remains moderate (0.55) because some
 *   authentic coordination occurs alongside the performative maintenance of
 *   inherited colonial institutional forms.
 *
 * KEY AGENTS:
 *   - Personalist Ruler: Primary beneficiary (institutional/arbitrage) — irreplaceability claim maintains authority; extractiveness flows toward this agent
 *   - Regime Continuity Institutions (bureaucracy, judiciary, legislature): Primary victims (powerless/trapped) — structurally dependent on regime but rendered obsolete by succession crisis; cannot exit
 *   - Successor Candidates (military officers, party cadres, family members): Secondary victims (powerless/identity_locked) — identity fused with proximity to ruler; compete in absence of formal succession rules
 *   - Military Officer Corps: Mixed position (powerful/constrained) — coordinates defense but suffers coup-proofing fragmentation; constrained by career ties but powerful enough to shape succession
 *   - Opposition and Civil Society: Organized agents (organized/mobile) — face extraction through suppression but benefit from regime instability during succession crises
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a political choice (succession suppression) into an immutable law (Weber's iron law of personalism)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personalist_dictatorship__charisma_routinization_problem, 0.68).
domain_priors:suppression_score(personalist_dictatorship__charisma_routinization_problem, 0.72).
domain_priors:theater_ratio(personalist_dictatorship__charisma_routinization_problem, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personalist_dictatorship__charisma_routinization_problem, extractiveness, 0.68).
narrative_ontology:constraint_metric(personalist_dictatorship__charisma_routinization_problem, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(personalist_dictatorship__charisma_routinization_problem, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personalist_dictatorship__charisma_routinization_problem, tangled_rope).
narrative_ontology:human_readable(personalist_dictatorship__charisma_routinization_problem, "Personalist Authority: Charisma Routinization Problem (Weber's Succession Crisis)").
narrative_ontology:topic_domain(personalist_dictatorship__charisma_routinization_problem, "political/comparative").

domain_priors:requires_active_enforcement(personalist_dictatorship__charisma_routinization_problem).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personalist_dictatorship__charisma_routinization_problem, '0e85433c-2731-4254-97fb-6d87ab34377f').
narrative_ontology:cs_kernel_codification('0e85433c-2731-4254-97fb-6d87ab34377f', implicit).
narrative_ontology:cs_authority_grounding('0e85433c-2731-4254-97fb-6d87ab34377f', extraction).
narrative_ontology:cs_reading_relation('0e85433c-2731-4254-97fb-6d87ab34377f', personalist_dictatorship__coup_proofing_mechanics, coexists_with).
narrative_ontology:cs_reading_relation('0e85433c-2731-4254-97fb-6d87ab34377f', personalist_dictatorship__cult_information_pathology, coexists_with).
narrative_ontology:cs_axiom('0e85433c-2731-4254-97fb-6d87ab34377f', foundational, charisma_structurally_personal).
narrative_ontology:cs_axiom_status(charisma_structurally_personal, holdable).
narrative_ontology:cs_axiom_grounding('0e85433c-2731-4254-97fb-6d87ab34377f', charisma_structurally_personal, empirically_contingent).
narrative_ontology:cs_axiom('0e85433c-2731-4254-97fb-6d87ab34377f', foundational, succession_mechanisms_threaten_irreplaceability).
narrative_ontology:cs_axiom_status(succession_mechanisms_threaten_irreplaceability, holdable).
narrative_ontology:cs_axiom_grounding('0e85433c-2731-4254-97fb-6d87ab34377f', succession_mechanisms_threaten_irreplaceability, empirically_contingent).
narrative_ontology:cs_reference_frame('0e85433c-2731-4254-97fb-6d87ab34377f', charismatic_irreplaceability_authority).
narrative_ontology:cs_drift_state('0e85433c-2731-4254-97fb-6d87ab34377f', late_regime_life_cycle, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0e85433c-2731-4254-97fb-6d87ab34377f', '').
narrative_ontology:cs_kernel_id(personalist_dictatorship__charisma_routinization_problem, personalist_dictatorship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personalist_dictatorship__charisma_routinization_problem, personalist_ruler).
narrative_ontology:constraint_victim(personalist_dictatorship__charisma_routinization_problem, regime_continuity_institutions).
narrative_ontology:constraint_victim(personalist_dictatorship__charisma_routinization_problem, state_administrative_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIME CONTINUITY INSTITUTIONS (SNARE) — State bureaucracy, judiciary, military hierarchy, legislative bodies — all structurally dependent on the personalist authority but bear the risk of obsolescence when the ruler dies. These institutions cannot exit: their existence is contingent on the regime. They cannot propose succession mechanisms without implying the ruler's mortality and challenging his irreplaceability claim. Maximum experienced extraction: the constraint locks continuity institutions into supporting a system whose endpoint is their own invalidation.
constraint_indexing:constraint_classification(personalist_dictatorship__charisma_routinization_problem, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUCCESSOR CANDIDATES / ELITE INSIDERS (SNARE) — Military officers, party cadres, family members positioned as potential heirs are identity-locked into the regime. Their status, power, and self-concept are constituted through proximity to the ruler. No formal succession mechanism exists, so each potential successor's claims depend entirely on the ruler's active endorsement or their capacity to eliminate rivals. Exit would require abandoning the identity fused with regime membership. They are trapped in a competition whose only resolution is the ruler's death or their own elimination.
constraint_indexing:constraint_classification(personalist_dictatorship__charisma_routinization_problem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PERSONALIST RULER (ROPE) — Authority is experienced as pure coordination: the ruler's personal network, patronage system, and decision-making capacity solve the problem of unified command and fast execution during crises. The constraint is perceived as functional and necessary. The ruler has arbitrage (can exit through voluntary retirement, death, or succession handoff). The ruler benefits from the irreplaceability claim — extraction runs toward this agent. From the ruler's perspective, the system solves a real coordination problem: how to prevent institutional fracturing and ensure loyalty in a context of institutional weakness.
constraint_indexing:constraint_classification(personalist_dictatorship__charisma_routinization_problem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE MILITARY OFFICER CORPS (TANGLED ROPE) — Faces constrained exit (career, income, and identity tied to military service) and genuinely coordinates defense and internal security. But the constraint also extracts: coup-proofing mechanisms (unit rotation, salary differentials favoring palace guards, deliberate fragmentation) weaken institutional capacity while enriching those in the ruler's trust. The officer corps both coordinates collective defense AND enables extraction through structural subordination. Extraction is substantial but bounded by the coordination function.
constraint_indexing:constraint_classification(personalist_dictatorship__charisma_routinization_problem, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COLONIAL INSTITUTIONAL LEGACY (PITON) — From a long-horizon view, the personalist regime inherits institutional structures (centralized courts, weak legislatures, loyalty-based administration) from colonial rule. These structures were designed for extractive colonial governance, not for institutional continuity. The personalist ruler maintains these formal structures through theater — holding elections, convening cabinets, publishing constitutions — but the real authority flows through personalist networks. The institutional skeleton persists from inertia and legitimacy claims, not from functional necessity. Theater_ratio is moderate (0.55) because some authentic coordination occurs alongside the performative ritual.
constraint_indexing:constraint_classification(personalist_dictatorship__charisma_routinization_problem, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPPOSITION AND CIVIL SOCIETY (TANGLED ROPE) — Organized opposition movements and civil society groups face extraction (surveillance, restrictions on assembly, legal jeopardy) but also benefit from the personalist regime's structural instability: succession crises create moments of institutional vulnerability where opposition can mobilize. They are mobile (can organize underground, exit to diaspora, or exploit crisis moments). Exit options are higher than for regime insiders. The constraint extracts through suppression but also creates opportunities for challenge — a genuine hybrid structure.
constraint_indexing:constraint_classification(personalist_dictatorship__charisma_routinization_problem, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, this is Weber's genuine observation: charismatic authority is inherently personal and cannot be institutionalized without destroying what makes it charismatic. The constraint appears as an immutable law of political science — personalism and institutional succession are structurally incompatible. However, the structural data contradicts this classification: the constraint has identifiable beneficiaries (the ruler's irreplaceability), identifiable victims (regime continuity institutions), and measurable suppression (succession pathways blocked). The engine will compute this as a false summit, revealing that Weber's observation, while analytically profound, has been naturalized by scholars into an excuse rather than recognized as a contingent institutional choice.
constraint_indexing:constraint_classification(personalist_dictatorship__charisma_routinization_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personalist_dictatorship__charisma_routinization_problem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personalist_dictatorship__charisma_routinization_problem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personalist_dictatorship__charisma_routinization_problem, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(personalist_dictatorship__charisma_routinization_problem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(personalist_dictatorship__charisma_routinization_problem, TR),
    TR >= 0.70.

:- end_tests(personalist_dictatorship__charisma_routinization_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but bounded. The ruler captures disproportionate gains from the coordination function (loyalty networks, fast decision-making, centralized authority), while regime continuity institutions bear the cost of subordination and obsolescence. The 0.68 value reflects that significant extraction occurs—successor candidates are trapped competing for the ruler's favor; institutions are mortgaged to one lifespan—but the constraint also delivers real coordination benefits (unified command in weak-state contexts). Suppression (0.72): High. Formal succession mechanisms are systematically blocked; constitutional succession rules are weakened or rhetoically opposed; military officers are rotated to prevent power consolidation; institutions are fragmented to prevent independent operation. However, suppression is not total (0.90+)—some informal succession pathways exist, and civil society retains mobile options. Theater_ratio (0.55): Moderate. The regime maintains formal institutions (legislatures, judiciaries, civil services) that perform constitutional functions, but real authority flows through personalist networks. The theatrical component increases under succession pressure as regime insiders perform loyalty while positioning for post-succession advantage. The rising measurement trajectory (0.48→0.55) reflects increasing performativity as the regime ages and succession uncertainty rises.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The ruler sees pure coordination (Rope): personalist networks efficiently solve the problem of command and loyalty in weak-state contexts. Regime continuity institutions see pure extraction (Snare): their existence is contingent on the regime, their functions are subordinated to personalist networks, and they bear the risk of obsolescence. Successor candidates see trapped competition (Snare with identity_locked exit): formal succession rules are absent, so power consolidation depends on the ruler's favor, and their identity is fused with regime proximity. The military officer corps sees mixed coordination and extraction (Tangled Rope): they coordinate defense but suffer deliberate fragmentation (coup-proofing). Opposition movements see a temporarily vulnerable target (Tangled Rope with mobile exit): they face suppression but can mobilize during succession crises. The colonial institutional legacy perspective sees degraded formalism (Piton): institutions persist through inertia and legitimacy claims while real power flows through personalist networks. The analytical observer risks seeing an immutable law (Mountain—Weber's observation)—but the structural data reveals this as a false summit naturalizing a political choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The ruler experiences low directionality (d ≈ 0.15–0.20): beneficiary status + arbitrage exit options yield negative effective extraction (χ < 0) from their perspective. They see the constraint as pure coordination. Regime continuity institutions experience high directionality (d ≈ 0.88–0.95): victim status + trapped exit options yield maximum effective extraction (χ > 1.4) from their perspective. Successor candidates experience high directionality with identity_locked exit (d ≈ 0.89): they are victims (competing without rules) with identity fused to the regime, yielding high experienced extraction. The military officer corps experiences moderate-high directionality (d ≈ 0.55–0.65): they are partially beneficiaries (coup-proofing fragmentation enriches palace-loyal units) and partially victims (institutional degradation reduces military capacity), with constrained but not trapped exit. Opposition movements experience moderate directionality (d ≈ 0.60–0.70): they are victims of suppression but mobile enough to exploit crisis moments, and they benefit from regime instability.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the apparent incompatibility between personalism and succession (Weber's iron law) is an institutional choice, not a structural law. The ruler could institutionalize succession (designated heir, constitutional rules, power-sharing arrangements) but chooses not to—doing so would undermine the irreplaceability claim that justifies personalist extraction. The mandatrophy is the tension between the ruler's interest in regime continuity (requires succession mechanisms) and the ruler's interest in maintaining irreplaceability (requires blocking succession mechanisms). Comparative cases show that this is solvable: some personalist regimes (Singapore/Lee Kuan Yew, Morocco/Hassan II, Japan/Meiji) did institutionalize succession within personalist frameworks. Others (Uganda/Amin, Zaire/Mobutu) collapsed at succession. The classification is legitimately tangled_rope: the constraint delivers coordination alongside extraction, and the extraction mechanism (suppression of succession) is a deliberate choice that could be relaxed. The false-summit risk is that scholars citing Weber naturalize this choice into an impossibility, preventing recognition that succession suppression is extractive and solvable, not inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    succession_mechanism_feasibility,
    'Can personalist authority be routinized through designated succession, institutional power-sharing, or constitutional succession rules, or is charismatic authority inherently destroyed by formalization?',
    'Comparative case analysis: regimes that attempted institutionalized succession (Singapore/Lee, Morocco/Hassan, Japan Meiji) vs those that collapsed at succession (Uganda/Amin, Zaire/Mobutu, Haiti/Duvalier). Track whether routinization preserved regime continuity and institutional capacity or whether succession mechanisms became pure theater while real power devolved to military/party factions.',
    'If feasible: routinization is a political choice, not a structural impossibility. Classification shifts from mountain-inflected perspective to tangled_rope across perspectives (constraint is extractive but solvable). If infeasible: personalism is genuinely incompatible with institutional succession, and the constraint is closer to mountain for all perspectives (immutable structural law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(succession_mechanism_feasibility, empirical, 'Whether charismatic authority can survive formalized succession mechanisms').

omega_variable(
    extraction_vs_necessity_trade,
    'Is the suppression of formal succession mechanisms (0.72) a necessary structural feature of personalist authority, or is it an extractive choice made by rulers to maximize their own irreplaceability?',
    'Regime self-presentation analysis: do rulers claim succession mechanisms are impossible (necessity framing) or that they prefer to retain personal control (extraction framing)? Compare rhetoric justifying succession delays across multiple regimes. Examine succession crises where rulers claimed to want designated heirs but institutional constraints prevented it vs cases where rulers actively prevented heirs from consolidating power.',
    'If necessity: suppression is inherent to the personalist form; extractiveness should be reframed as coordination cost, not asymmetric extraction. If extraction: suppression is a deliberate choice to maintain power asymmetry; current 0.68 extractiveness is accurate and may underestimate the intentional blocking of succession paths.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_necessity_trade, empirical, 'Whether succession suppression is structural necessity or deliberate extraction').

omega_variable(
    charisma_versus_loyalty_network,
    'Is the constraint''s core mechanism the ruler''s personal charisma (impossible to transfer) or the loyalty network built on patronage and personality cult (potentially transferable to a designated heir)?',
    'Network mapping during succession crises: trace which institutional and patronage nodes shift to the successor (if succession occurs) vs which collapse. Analyze whether successor authority derives from inherited legitimacy, charismatic claims, or institutional delegation. Compare regimes where charisma was genuinely personal (ruler had mass appeal) vs regimes where ''charisma'' was manufactured through cult infrastructure (successor can inherit the cult machinery).',
    'If personal charisma: the constraint is closer to mountain — routinization is impossible. If loyalty network: the constraint is tangled_rope — extraction is high, but succession is possible if rulers choose institutional paths. Current 0.68 extractiveness assumes loyalty network is the mechanism; true personal charisma would push this toward mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charisma_versus_loyalty_network, empirical, 'Whether constraint mechanism is personal charisma or transferable loyalty network').

omega_variable(
    kernel_reading_contest,
    'Which sibling reading of the personalist_dictatorship kernel is empirically more accurate for explaining regime stability: charisma_routinization_problem (this reading), coup_proofing_mechanics, or cult_information_pathology?',
    'Case study triangulation: identify regimes that exhibited high charisma_routinization_problem signals (ruler mortality crises, succession uncertainty, institutional instability). Measure which of the three mechanisms (routinization failure, coup-proofing fragmentation, information pathology) correlates best with regime outcomes (continuity, collapse, institutional degradation). Test predictions: does the routinization reading predict where and when succession crises occur? Do coup-proofing mechanics explain military weakness better than the routinization lens? Does cult information pathology account for regime decision failures that routinization logic cannot explain?',
    'If this reading dominates: the constraint''s primary damage mechanism is succession uncertainty and institutional atrophy mortgaged to one lifespan. If coup-proofing or cult_information_pathology dominates: the constraint''s real mechanism is elsewhere (military fragmentation or epistemic collapse), and the routinization reading is a secondary effect or rationalization. Classification and extractiveness may shift across readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which sibling reading best explains personalist regime dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personalist_dictatorship__charisma_routinization_problem, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(persauth_tr_t0, personalist_dictatorship__charisma_routinization_problem, theater_ratio, 0, 0.48).
narrative_ontology:measurement(persauth_tr_t10, personalist_dictatorship__charisma_routinization_problem, theater_ratio, 10, 0.52).
narrative_ontology:measurement(persauth_tr_t20, personalist_dictatorship__charisma_routinization_problem, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(persauth_be_t0, personalist_dictatorship__charisma_routinization_problem, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(persauth_be_t10, personalist_dictatorship__charisma_routinization_problem, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(persauth_be_t20, personalist_dictatorship__charisma_routinization_problem, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(persauth_su_t0, personalist_dictatorship__charisma_routinization_problem, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(persauth_su_t10, personalist_dictatorship__charisma_routinization_problem, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(persauth_su_t20, personalist_dictatorship__charisma_routinization_problem, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personalist_dictatorship__charisma_routinization_problem, enforcement_mechanism).
narrative_ontology:affects_constraint(personalist_dictatorship__charisma_routinization_problem, personalist_dictatorship__coup_proofing_mechanics).
narrative_ontology:affects_constraint(personalist_dictatorship__charisma_routinization_problem, personalist_dictatorship__cult_information_pathology).

% DUAL FORMULATION NOTE:
% The personalist_dictatorship kernel decomposes into three distinct constraint stories, each with different mechanisms and ε values. The charisma_routinization_problem reading (this story, ε=0.68) focuses on succession impossibility and institutional subordination. The coup_proofing_mechanics reading (sibling, separate story) would focus on military architecture and institutional fragmentation as primary mechanisms. The cult_information_pathology reading (sibling, separate story) would focus on epistemic collapse as primary mechanism. All three readings describe aspects of personalist rule, but they have different causal claims and predict different empirical patterns. Linking them via network.affects_constraints establishes that they are interpretive alternatives on the same kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
