% ============================================================================
% CONSTRAINT STORY: sotu_1970_nixon_doctrine_burden_sharing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1970_nixon_doctrine_burden_sharing, []).

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
 *   constraint_id: sotu_1970_nixon_doctrine_burden_sharing
 *   human_readable: Nixon Doctrine: Burden Sharing and Allied Defense Responsibility
 *   domain: foreign_policy/alliance_structure
 *
 * SUMMARY:
 *   The Nixon Doctrine, articulated in the 1970 State of the Union and
 *   subsequent elaborations, represents a structural recalibration of Cold
 *   War alliance burden. Rather than U.S. unilateral security provision to
 *   treaty allies (South Korea, South Vietnam, Thailand, and NATO members),
 *   the doctrine mandates that allied nations assume primary responsibility
 *   for their own defense, with U.S. providing military assistance, training,
 *   and nuclear extended deterrence as backup. This constraint has multiple
 *   structural readings depending on observer position: a coordination
 *   mechanism from U.S. and allied military command perspective; a tangled
 *   hybrid of coordination and extraction from allied defense ministry
 *   perspective; pure extraction from the perspective of allied publics
 *   absorbing rearmament costs; degraded institutional performance from the
 *   alliance system perspective as verification mechanisms become theatrical;
 *   and—riskily—an immutable law of alliance economics from the analytical
 *   observer who naturalizes power asymmetries as inherent constraints.
 *
 * KEY AGENTS:
 *   - American Taxpayers: Primary beneficiary (institutional/arbitrage) — benefit from reduced foreign military expenditure and lower deployment costs
 *   - U.S. Military Command: Primary beneficiary (institutional/arbitrage) — achieve strategic influence with lower force commitment and logistical overhead
 *   - Allied Nations (ROK, SVN, Thailand, NATO members): Primary victim (powerless/trapped) — must absorb defense burden without capacity to exit alliance relationship
 *   - Allied Defense Ministries: Secondary victim (moderate/constrained) — experience mixed coordination benefit and extraction cost; have theoretical exit option but geopolitical vulnerability prevents exercise
 *   - Congressional Budget Authority: Beneficiary (institutional/arbitrage) — authority to reallocate defense spending to domestic priorities
 *   - U.S. Congress: Beneficiary (powerful/arbitrage) — political incentive to reduce overseas military expenditure aligns with budget constraints
 *   - Soviet/Warsaw Pact Leadership: Adversarial observer (powerful/constrained) — perceive burden-shifting as both challenge and opportunity; constrained by own alliance burden-sharing requirements
 *   - Analytical Observer: Neutral position (analytical/analytical) — risks naturalizing contingent power asymmetries as immutable structural necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1970_nixon_doctrine_burden_sharing, 0.58).
domain_priors:suppression_score(sotu_1970_nixon_doctrine_burden_sharing, 0.65).
domain_priors:theater_ratio(sotu_1970_nixon_doctrine_burden_sharing, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1970_nixon_doctrine_burden_sharing, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1970_nixon_doctrine_burden_sharing, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1970_nixon_doctrine_burden_sharing, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1970_nixon_doctrine_burden_sharing, tangled_rope).
narrative_ontology:human_readable(sotu_1970_nixon_doctrine_burden_sharing, "Nixon Doctrine: Burden Sharing and Allied Defense Responsibility").
narrative_ontology:topic_domain(sotu_1970_nixon_doctrine_burden_sharing, "foreign_policy/alliance_structure").

domain_priors:requires_active_enforcement(sotu_1970_nixon_doctrine_burden_sharing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1970_nixon_doctrine_burden_sharing, american_taxpayers).
narrative_ontology:constraint_beneficiary(sotu_1970_nixon_doctrine_burden_sharing, us_military_command).
narrative_ontology:constraint_victim(sotu_1970_nixon_doctrine_burden_sharing, allied_defense_budgets).
narrative_ontology:constraint_victim(sotu_1970_nixon_doctrine_burden_sharing, allied_military_personnel).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALLIED NATION BEARING BURDEN (SNARE) — Treaty obligation and geopolitical vulnerability create structural trap. Allied nation cannot exit the alliance without exposing itself to security threat from adversary. Must accept burden-shifting from U.S. despite constrained defense budget and limited military capacity. Suppression is high: formal treaty language, military aid conditionality, and implicit threat of U.S. withdrawal if compliance insufficient. Zero degrees of freedom.
constraint_indexing:constraint_classification(sotu_1970_nixon_doctrine_burden_sharing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED DEFENSE MINISTRY (TANGLED ROPE) — Experiences simultaneous coordination benefit (U.S. training, technology transfer, military cooperation) and extraction (pressure to divert national budget from development to defense, domestic political cost of rearmament). High suppression reflects geopolitical dependency: capacity to exit is theoretically present but practically constrained by security vulnerability and fear of abandonment. Active enforcement through military-to-military pressure and implicit threat of alliance withdrawal.
constraint_indexing:constraint_classification(sotu_1970_nixon_doctrine_burden_sharing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. MILITARY COMMAND (ROPE) — Pure coordination function from institutional U.S. perspective. Burden-shifting reduces deployment costs, logistical overhead, and force commitment while maintaining strategic influence. Achieves alliance coordination with lower extraction cost to the U.S. system. Benefits from technology transfer leverage, forward base access, and allied force multiplication without proportional U.S. resource investment. Exit options are high: U.S. can adjust commitment level or withdraw entirely.
constraint_indexing:constraint_classification(sotu_1970_nixon_doctrine_burden_sharing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: U.S. FISCAL AUTHORITY (ROPE) — Burden-shifting reduces appropriations pressure for foreign military aid and overseas force maintenance. Enables defense spending reallocation to domestic priorities or deficit reduction. Coordinates shared defense responsibility at lower fiscal cost. Suppression is minimal: Congress has full authority to adjust commitment level. Extraction runs strongly toward American taxpayers — they are net beneficiaries of reduced foreign military expenditure.
constraint_indexing:constraint_classification(sotu_1970_nixon_doctrine_burden_sharing, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIANCE THEATER STRUCTURE (PITON) — The multilateral alliance system performs a coordination function (synchronized defense posture, technology standards, intelligence sharing), but the burden-sharing enforcement mechanism has become increasingly theatrical. By the 1970s-80s, the actual verification of allied compliance is weak: declaratory forces (stated capacity) exceed actual deployable forces; readiness standards are enforced through bureaucratic certification rather than real testing; capability gaps are masked by residual U.S. nuclear extended deterrence. The system persists through institutional inertia and Cold War threat perception, but the performative element increases over time as allied capacity gaps grow and U.S. cover remains implicit.
constraint_indexing:constraint_classification(sotu_1970_nixon_doctrine_burden_sharing, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SOVIET ADVERSARY (TANGLED ROPE) — Perceives the Nixon Doctrine as both a coordination challenge (requiring response in allied cost-sharing among Warsaw Pact members) and an extraction opportunity (potential fracturing of Western alliance if burden unequally distributed). Soviet leadership sees their own alliance burden-sharing as coordination requirement; Western burden-sharing appears as potential weakness to exploit. Constrained exit because alliance maintenance is essential to Soviet strategic posture, but geopolitical positioning offers some leverage.
constraint_indexing:constraint_classification(sotu_1970_nixon_doctrine_burden_sharing, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL REALISM (MOUNTAIN) — From a civilizational perspective on international relations, the Nixon Doctrine appears as a natural law of alliance structure: burden must be borne by those who benefit from security; free-riding is inherently unstable; burden-sharing is the only sustainable architecture for extended deterrence. This perspective naturalizes the distribution of costs as inevitable consequence of security economics. However, structural data reveals false summit: the 'natural' burden-sharing reflects power asymmetries and historical path-dependency (U.S. postwar dominance, allied vulnerability), not immutable law.
constraint_indexing:constraint_classification(sotu_1970_nixon_doctrine_burden_sharing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1970_nixon_doctrine_burden_sharing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1970_nixon_doctrine_burden_sharing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1970_nixon_doctrine_burden_sharing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1970_nixon_doctrine_burden_sharing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1970_nixon_doctrine_burden_sharing, TR),
    TR >= 0.70.

:- end_tests(sotu_1970_nixon_doctrine_burden_sharing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. Initial value (0.35) reflects the genuine coordination function of the doctrine—allied nations do benefit from U.S. technology transfer, training, and extended deterrence guarantee. However, extraction component rises as the asymmetry becomes apparent: allied nations must divert development resources to rearmament while U.S. reduces its relative burden. By year 6-9, allied nations face compounding defense burden while U.S. disengages from Vietnam and reduces overseas deployments, shifting the extraction mechanism from active enforcement to structural dependency. The rising trajectory reflects that enforcement relies increasingly on implicit threat (withdrawal of U.S. guarantee) rather than active coordination. Suppression (0.65): High and stable. Suppression operates through multiple mechanisms: formal treaty language (NATO Article 5, bilateral defense treaties) making exit legally and politically costly; implicit threat of U.S. withdrawal if allied burden contribution insufficient; military vulnerability to adversary, making independent exit impossible; and technology/training dependency locking allied militaries into U.S. supply chains. Theater ratio (0.48): Moderate, rising over interval. The doctrine's coordination function is genuine—actual technology transfer, actual training, actual intelligence sharing occur. However, verification of burden-sharing compliance becomes increasingly theatrical: declared force strengths exceed actual deployable capacity; readiness certifications are bureaucratic rather than operationally tested; capability gaps are obscured by residual U.S. nuclear deterrent. Theater rises as the system matures because actual enforcement weakens while compliance theater strengthens (allied nations produce force posture commitments without corresponding capability).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits dramatic perspectival divergence. The powerless allied nation sees a snare: trapped in defensive relationship, forced to bear costs, suppressed by threat of abandonment. The allied defense ministry sees tangled rope: genuine military cooperation benefits coexist with extraction costs—the constraint coordinates defense posture while extracting resources. The U.S. military sees rope: pure coordination mechanism that achieves alliance maintenance at lower cost. The U.S. budget authority sees rope: fiscal benefit of burden-shifting with minimal strategic cost. The alliance system as institutional structure sees piton: the coordination mechanism (force modernization, interoperability, readiness standards) persists through inertia, but enforcement becomes increasingly theatrical as capability gaps grow. The analytical observer risks seeing mountain: natural law of alliance economics that burden must rest on those who benefit, with U.S./allied asymmetry as inevitable structural consequence. The false summit is detectable through beneficiary/victim declarations: if the mountain were genuinely natural law, no identifiable beneficiary class would exist (the constraint would be impersonal). But American taxpayers and military command are clear beneficiaries, revealing the mountain as a false summit—the 'natural law' naturalizes a contingent power relationship established through postwar U.S. dominance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim structure: American taxpayers and U.S. military command benefit from reduced foreign military expenditure and lower deployment obligations. Allied defense budgets and allied military personnel bear extraction costs through rearmament burden and loss of development resources. The beneficiary group has institutional power and arbitrage exit options (U.S. can withdraw commitment or adjust terms). The victim group has powerless/moderate power and trapped/constrained exit options (cannot credibly exit alliance without security vulnerability). The directionality derivation produces high d for victims (near 0.90 for trapped allied nations) and low d for beneficiaries (near 0.15 for institutional U.S. actors with arbitrage options). The sigmoid f(d) applies to produce effective extraction χ, which varies by perspective: U.S. perspective sees low χ (beneficiary position → negative effective extraction), allied perspective sees high χ (victim position with high suppression → high effective extraction), intermediate observers see moderate χ. The constraint's effective extraction is highest from powerless allied nation perspective (trapped + victim status) and lowest from U.S. institutional perspective (beneficiary + arbitrage status).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The Nixon Doctrine constraint demonstrates mandatrophy resolution through institutional analysis. The temptation is to classify the constraint as either pure coordination (rope—everyone benefits from burden-sharing because stable alliances serve mutual interest) or pure extraction (snare—U.S. extracts from allied nations by forcing them to bear defense costs). The resolution reveals that BOTH readings are structurally valid but perspectival. The constraint is genuinely a coordination mechanism—allied nations do need U.S. military support against adversaries, U.S. does benefit from forward bases and alliance interoperability, and burden-sharing creates incentives for all parties to contribute. Simultaneously, the constraint is extractive—U.S. uses threat of withdrawal to force allied nations to absorb costs they would not accept absent security dependence, and the burden-shifting enriches U.S. taxpayers at allied expense. The tangled rope classification captures this duality: the constraint solves a coordination problem (how to maintain alliance at sustainable cost) AND redistributes costs asymmetrically (benefits flow disproportionately to U.S.). The mandatrophy is resolved by recognizing that apparent contradiction—pure coordination vs. pure extraction—dissolves when we specify the structural relationship: from the position of the primary beneficiary (U.S. institutional actor), the constraint is rope. From the position of the primary victim (allied nation), the constraint is snare. The tangled rope is the constraint itself, observable from the intermediate perspective (allied defense ministry or analytical observer). The constraint is neither purely cooperative nor purely coercive; it is a hybrid mechanism that coordinates defense responsibility while extracting disproportionate value from one party. This hybrid nature is not a classification error—it is the accurate description of the constraint's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allied_capacity_versus_commitment,
    'Do allied nations genuinely lack capacity for primary defense responsibility, or is the capacity constraint a product of U.S. dominance that suppresses indigenous military development?',
    'Counterfactual analysis: What defense capability could allied nations achieve if not locked into dependent relationship with U.S. supplier? Comparison of allied defense spending as percentage of national budget before and after Nixon Doctrine implementation.',
    'If genuine capacity constraint: burden-shifting is coordination mechanism (extraction ε < 0.50). If capacity constraint is artificial: burden-shifting is extraction mechanism sustaining dependency (extraction ε > 0.65).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_capacity_versus_commitment, empirical, 'Whether allied capacity constraints are structural or U.S.-induced').

omega_variable(
    exit_cost_ambiguity,
    'What portion of the suppression against allied exit is military (objective threat from adversary) versus political (threat of alliance withdrawal by U.S.)?',
    'Historical case analysis: instances where allied nations reduced defense spending despite U.S. opposition; correlation between U.S. aid level and allied compliance with burden-sharing; cost of unilateral non-compliance vs cost of military vulnerability.',
    'If predominantly military threat: suppression is structural (genuine security need), tangled rope classification appropriate. If predominantly political threat: suppression is relational (extraction instrument), snare classification more appropriate for allied perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_ambiguity, empirical, 'Decomposition of suppression into military vs. political threat').

omega_variable(
    burden_sharing_equity_threshold,
    'What distribution of defense responsibility constitutes ''fair'' burden-sharing versus extractive unfairness?',
    'Comparison of defense burden as percentage of GDP across alliance members; correlation with national capability and threat exposure; analysis of cost asymmetry before and after doctrine implementation.',
    'If threshold < current burden distribution: doctrine is extractive by definition (victims'' lived experience justified). If threshold > current burden distribution: doctrine is still coordination (accepts equity gaps as price of alliance).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(burden_sharing_equity_threshold, preference, 'Threshold for fair versus extractive burden distribution').

omega_variable(
    technological_dependency_lock,
    'Does U.S. military technology and training transfer within the alliance create path-dependent dependency that makes genuine burden-sharing impossible for allied nations?',
    'Analysis of allied military procurement patterns: indigenous capability development versus U.S. equipment reliance; cost of switching to non-U.S. suppliers; technological interoperability requirements that lock allied forces into U.S. supply chains.',
    'If dependency is path-dependent: allied nations are identity_locked into role (cannot exit without dismantling entire military structure), classification shifts to identity-locked trap. If dependency is economic choice: classification remains tangled rope with constrained exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_dependency_lock, empirical, 'Whether alliance technology standards create irreversible dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1970_nixon_doctrine_burden_sharing, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nixon_doctrine_tr_t0, sotu_1970_nixon_doctrine_burden_sharing, theater_ratio, 0, 0.32).
narrative_ontology:measurement(nixon_doctrine_tr_t3, sotu_1970_nixon_doctrine_burden_sharing, theater_ratio, 3, 0.4).
narrative_ontology:measurement(nixon_doctrine_tr_t6, sotu_1970_nixon_doctrine_burden_sharing, theater_ratio, 6, 0.48).
narrative_ontology:measurement(nixon_doctrine_tr_t9, sotu_1970_nixon_doctrine_burden_sharing, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(nixon_doctrine_be_t0, sotu_1970_nixon_doctrine_burden_sharing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nixon_doctrine_be_t3, sotu_1970_nixon_doctrine_burden_sharing, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(nixon_doctrine_be_t6, sotu_1970_nixon_doctrine_burden_sharing, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(nixon_doctrine_be_t9, sotu_1970_nixon_doctrine_burden_sharing, base_extractiveness, 9, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1970_nixon_doctrine_burden_sharing, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1970_nixon_doctrine_burden_sharing, nato_burden_sharing_targets).
narrative_ontology:affects_constraint(sotu_1970_nixon_doctrine_burden_sharing, southeast_asia_military_dependency).
narrative_ontology:affects_constraint(sotu_1970_nixon_doctrine_burden_sharing, us_forward_base_network).

% DUAL FORMULATION NOTE:
% The Nixon Doctrine operates as a unified structural constraint on alliance burden distribution, but its effects decompose into distinct impacts on specific regional alliances (NATO, SEATO, bilateral treaties) with different ε values reflecting different enforcement capacity and allied capacity. This story represents the doctrine's general architecture; network partners represent regional instantiations with higher extractiveness (Southeast Asia dependency) or lower extractiveness (NATO burden-sharing negotiation with peer powers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1970_nixon_doctrine_burden_sharing, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
