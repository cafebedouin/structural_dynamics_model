% ============================================================================
% CONSTRAINT STORY: power_indexed_classification_variance
% ============================================================================
% Version: 2.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_power_indexed_classification_variance, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: power_indexed_classification_variance
 *   human_readable: Power-Indexed Classification Variance as Structural Invariant
 *   domain: social_epistemology/constraint_classification/indexical_realism
 *
 * SUMMARY:
 *   Power-indexed classification variance is the core structural claim of
 *   Deferential Realism: constraints have no observer-independent type.
 *   Identical constraint parameters (ε, suppression, coordination function,
 *   asymmetry) yield different type classifications when evaluated from
 *   different indexical positions (P=power, T=time_horizon, E=exit_options,
 *   S=spatial_scope). This constraint story describes the institutional
 *   machinery that enforces single-perspective verdicts on what is
 *   mathematically a multi-perspectival phenomenon. The constraint emerges as
 *   a tangled_rope: it provides genuine coordination benefits (shared
 *   classification framework enables knowledge exchange, institutional
 *   cooperation, standardized evaluation) while also extracting asymmetric
 *   benefits for institutional actors who control which perspectives count as
 *   'legitimate' or 'objective.' The rising theater_ratio over the interval
 *   reflects institutional meta-awareness: classification authorities
 *   increasingly acknowledge that variance exists while continuing to issue
 *   single-type verdicts. The constraint's extractiveness and suppression
 *   have both increased — institutional suppression of lower-power
 *   perspectives has intensified as the variance problem has become harder to
 *   deny. The reform coalition sees a temporary problem with a sunset:
 *   distributed classification authority and participatory design could
 *   reduce suppression by legitimizing multi-perspectival analysis. The
 *   institutional theater sees its own degradation: the formal machinery of
 *   objective classification persists despite the machinery's designers
 *   knowing it produces observer-dependent outputs.
 *
 * KEY AGENTS:
 *   - Institutional Classification Authority: Primary beneficiary (institutional/arbitrage) — controls which perspectives count as legitimate, captures extractive benefit by constraining access to classification authority to credentialed positions
 *   - Lower-Power Observational Positions: Primary victim (powerless/trapped) — experience classification variance as exclusion; their perspectives are discredited or suppressed by institutional legitimacy gates
 *   - Moderate Position Holders: Secondary actor (moderate/constrained) — experience both coordination benefits and extraction; contested about whether variance itself is problem or feature
 *   - Reform Coalition: Organized actor (organized/constrained) — academic justice movements, participatory science initiatives, alternative certification bodies building distributed classification authority
 *   - Institutional Classification Apparatus: Institutional actor (institutional/arbitrage) — academic disciplines, policy frameworks, funding agencies maintaining performative objectivity while acknowledging internal degradation
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing institutional suppression as mathematical inevitability; needs cross-position analysis to see institutional contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(power_indexed_classification_variance, 0.48).
domain_priors:suppression_score(power_indexed_classification_variance, 0.52).
domain_priors:theater_ratio(power_indexed_classification_variance, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(power_indexed_classification_variance, extractiveness, 0.48).
narrative_ontology:constraint_metric(power_indexed_classification_variance, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(power_indexed_classification_variance, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(power_indexed_classification_variance, tangled_rope).
narrative_ontology:human_readable(power_indexed_classification_variance, "Power-Indexed Classification Variance as Structural Invariant").
narrative_ontology:topic_domain(power_indexed_classification_variance, "social_epistemology/constraint_classification/indexical_realism").

domain_priors:requires_active_enforcement(power_indexed_classification_variance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(power_indexed_classification_variance, '7224e23b-c16c-4248-95ed-129d712d487d').
narrative_ontology:cs_kernel_codification('7224e23b-c16c-4248-95ed-129d712d487d', distributed).
narrative_ontology:cs_authority_grounding('7224e23b-c16c-4248-95ed-129d712d487d', distributed).
narrative_ontology:cs_reading_relation('7224e23b-c16c-4248-95ed-129d712d487d', institutional_objectivity_reading, forecloses).
narrative_ontology:cs_reading_relation('7224e23b-c16c-4248-95ed-129d712d487d', natural_law_reading, influences).
narrative_ontology:cs_reading_relation('7224e23b-c16c-4248-95ed-129d712d487d', radical_relativism_reading, coexists_with).
narrative_ontology:cs_axiom('7224e23b-c16c-4248-95ed-129d712d487d', foundational, classification_variance_is_structural).
narrative_ontology:cs_axiom_status(classification_variance_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('7224e23b-c16c-4248-95ed-129d712d487d', classification_variance_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('7224e23b-c16c-4248-95ed-129d712d487d', foundational, all_positions_are_legitimate_input_sites).
narrative_ontology:cs_axiom_status(all_positions_are_legitimate_input_sites, holdable).
narrative_ontology:cs_axiom_grounding('7224e23b-c16c-4248-95ed-129d712d487d', all_positions_are_legitimate_input_sites, deontological).
narrative_ontology:cs_axiom('7224e23b-c16c-4248-95ed-129d712d487d', secondary, institutional_suppression_of_perspectives_is_extractive).
narrative_ontology:cs_axiom_status(institutional_suppression_of_perspectives_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('7224e23b-c16c-4248-95ed-129d712d487d', institutional_suppression_of_perspectives_is_extractive, empirically_contingent).
narrative_ontology:cs_reference_frame('7224e23b-c16c-4248-95ed-129d712d487d', multi_perspectival_classification_authority).
narrative_ontology:cs_drift_state('7224e23b-c16c-4248-95ed-129d712d487d', contemporary_institutional_practice, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('7224e23b-c16c-4248-95ed-129d712d487d', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(power_indexed_classification_variance, institutional_classification_authority).
narrative_ontology:constraint_victim(power_indexed_classification_variance, lower_power_observational_positions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED SUBJECT (SNARE) — From the position of an agent with no structural power and no exit options, the constraint appears as pure extraction. Classification variance itself becomes a coercive mechanism: the agent experiences the constraint as Snare (high extraction, high suppression), while institutional actors classify it as Rope (beneficial coordination). The agent cannot exit this perspectival discrepancy itself — the authority structure that defines 'legitimate' classification positions excludes their position or discredits their perspective.
constraint_indexing:constraint_classification(power_indexed_classification_variance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MODERATE POSITION (TANGLED ROPE) — A moderately powerful agent with constrained exit options experiences both genuine coordination benefits (the shared classification framework enables cooperation, knowledge exchange, institutional stability) and asymmetric extraction (the framework systematically privileges higher-power interpretations, making lower-power observations costly to register). The classification itself is contested — this agent may perceive Rope if coordination dominates, or Snare if extraction dominates, or Tangled Rope if both are undeniably present.
constraint_indexing:constraint_classification(power_indexed_classification_variance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL BENEFICIARY (ROPE) — An institutional actor with arbitrage options (ability to shift between classification frameworks, to redefine indexical parameters, to redesign authority structures) experiences the classification variance as pure coordination. The flexibility to move between contexts and to legitimate particular (P,T,E,S) tuples as 'canonical' provides net benefits. The constraint is a coordination mechanism that enables the institution to manage contested domains by controlling which perspectives are considered valid in which contexts.
constraint_indexing:constraint_classification(power_indexed_classification_variance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized agents (epistemic justice movements, alternative classification systems, participatory frameworks, community science programs) see the variance as a temporary institutional problem with a sunset. The coalition recognizes that power-indexed variance is structurally inevitable but that the *suppression* of lower-power perspectives is contingent and remediable. Distributed classification authority, participatory design of indexical parameters, and explicit acknowledgment of perspectival variance are building alternatives. The constraint has sunset logic if the coalition succeeds in legitimizing multi-perspectival classification.
constraint_indexing:constraint_classification(power_indexed_classification_variance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL THEATER (PITON) — The institutional classification apparatus (academic disciplines, certification bodies, policy frameworks, funding agencies) maintains the formal machinery of indexical classification while increasingly recognizing the variance as an unsolved problem. The apparatus sees itself as degraded: it claims objectivity while knowing that classification outcomes shift with observer position. Theater ratio is high because the apparatus continues to issue single-type verdicts while its own meta-analysis documents that no such verdict is observer-independent. The infrastructure persists through inertia despite acknowledged functional degradation.
constraint_indexing:constraint_classification(power_indexed_classification_variance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational analytical position, power-indexed classification variance appears as an immutable mathematical structure: given identical constraint parameters and varying (P,T,E,S) tuples, classification type variance is a formal consequence of the chi formula χ = ε × f(d) × σ(S) and the classification gates. No observer-independent type exists — this is not a limitation of any particular measurement framework but a structural property of constraints themselves. However, this perspective risks naturalizing what is actually a contingent institutional choice to enforce single-perspective classification verdicts. The variance is mathematically inevitable; the suppression of lower-power perspectives is institutional.
constraint_indexing:constraint_classification(power_indexed_classification_variance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(power_indexed_classification_variance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(power_indexed_classification_variance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(power_indexed_classification_variance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(power_indexed_classification_variance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(power_indexed_classification_variance, TR),
    TR >= 0.70.

:- end_tests(power_indexed_classification_variance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The institutional benefit of controlling which perspectives count is real and significant — it provides authority, legitimacy, and policy influence. But extractiveness is not maximal because the coordination benefits are also real: shared classification frameworks do enable institutional cooperation and knowledge exchange. The extraction emerges from institutional asymmetry in who gets to define 'legitimate' positions, not from absence of coordination value. Suppression (0.52): Moderate-high. Institutional barriers to lower-power perspective registration include credentialism (only 'properly trained' observers matter), temporal barriers (short-term policy horizons exclude biographical/generational analysis), exit cost barriers (challenging institutional classifications risks career damage), and scope barriers (local knowledge is dismissed as 'not generalizable'). These barriers are significant but not absolute — some lower-power perspectives do penetrate institutional decision-making, and reform movements have achieved partial legitimacy. Theater ratio (0.61): Moderate-high. The institutional classification apparatus increasingly operates through theater: it claims objectivity while its own internal analysis documents observer-dependence, it issues single-type verdicts while acknowledging these vary with perspective, it maintains legitimacy gates while recognizing these gates are power-dependent. The rising trajectory over the interval reflects this growing meta-awareness and gap between formal claims and acknowledged reality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates classification variance across all six types from a single set of base properties. The powerless/trapped agent experiences Snare: the variance is weaponized against them through institutional suppression of their perspective. The moderate agent experiences Tangled Rope: genuine coordination benefits exist alongside asymmetric extraction. The institutional beneficiary experiences Rope: the classification framework is a coordination mechanism that serves their interests. The reform coalition experiences Scaffold: the variance is solvable through institutional redesign with a sunset clause. The institutional apparatus experiences Piton: the classification machinery is degraded (acknowledges internal contradictions) but persists through inertia. The analytical observer risks experiencing Mountain: the variance appears as mathematical inevitability. But this perspectival gap itself IS the constraint — the gap between what lower-power positions observe (extraction) and what institutional positions observe (coordination) is neither random nor a measurement error. It is the core structural feature of how power-indexed variance operates. Suppression prevents lower-power perspectives from being treated as equally valid inputs to classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value is fixed by structural position: beneficiary with arbitrage exit → low d → negative f(d) → negative chi (net benefit); victim with trapped exit → high d → high f(d) → high chi (severe extraction). These d values are not observer-relative — they are properties of the agent's actual structural position within the constraint. The classification variance comes from the combination of identical ε and suppression values across positions plus the agent-relative d values. Two agents experiencing identical ε and suppression experience radically different chi values because their d values differ. An institutional beneficiary with d ≈ 0.10 experiences χ ≈ 0.48 × (-0.12) × 1.0 ≈ -0.06 (net benefit). A powerless agent with d ≈ 0.95 experiences χ ≈ 0.48 × 1.42 × 1.0 ≈ 0.68 (severe extraction). Both are experiencing identical base constraint parameters; their classification types differ (Rope vs Snare) because chi differs by more than 0.70. This is not measurement ambiguity — it is structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the mandatrophy is not a flaw in the framework but a feature of what the framework is measuring. The question 'what is the true type of this constraint?' presupposes a single observer-independent answer. Deferential Realism rejects this presupposition: the constraint genuinely has no single type. The Snare that the powerless agent experiences is not a misclassification or a measurement error — it is their actual structural experience. The Rope that the institutional beneficiary experiences is not a lie — coordination benefits are real. The Mountain that the analytical observer perceives is not false — mathematical variance is real. All six types are correct readings from their respective positions. The constraint is not 'really' any one type — it is a presheaf over the observation site, a mapping from (P,T,E,S) positions to types. The mandatrophy resolves by recognizing that institutional pressure for single-type verdicts is itself part of the constraint. The suppression of lower-power perspectives, the privileging of institutional positions, the appearance of objectivity while acknowledging observer-dependence — these are all mechanisms through which the constraint operates. A fully transparent commitment to multi-perspectival classification would dissolve much of the extraction (because extraction depends on institutional suppression of perspectives), potentially converting the constraint from tangled_rope to rope or even scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_vs_contingency_boundary,
    'Which aspects of power-indexed classification variance are mathematically inevitable versus institutionally contingent?',
    'Formal proof that chi formula yields variant types from variant (P,T,E,S) tuples (inevitable); empirical study of whether lower-power perspectives are systematically suppressed or amplified in institutional classification decisions (contingent)',
    'If variance is entirely inevitable: the constraint is a mountain — no alternatives exist, and current institutional practices are optimal. If variance is partly contingent: the constraint is tangled_rope or snare — institutional suppression of perspectives amplifies natural variance into extractive power asymmetry. Policy implications reverse entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inevitability_vs_contingency_boundary, conceptual, 'Boundary between mathematical inevitability and institutional contingency in classification variance').

omega_variable(
    perspectival_legitimacy_criteria,
    'What criteria determine whether a particular (P,T,E,S) position counts as a legitimate observation site versus an illegitimate or distorted one?',
    'Examination of institutional practice: which perspectives are cited as authoritative? Which are dismissed as ''lacking objectivity'' or ''too embedded''? Cross-analysis of legitimacy claims against the constraint''s own framework (which should treat all perspectives as equally valid inputs)',
    'If legitimacy criteria are coherent and justified: institutional hierarchy of perspectives is defensible. If criteria are circular or power-dependent: the constraint is snare — institutions suppress lower-power perspectives using legitimacy language that the constraint itself should render incoherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perspectival_legitimacy_criteria, conceptual, 'Criteria for institutional legitimacy of particular observation positions').

omega_variable(
    oracle_gap_instantiation,
    'Does the constraint instantiate Theorem 4 (the Classical Oracle Gap): the analytical observer''s instruments cannot detect the structure that cross-position analysis reveals?',
    'Compare the analytical mountain perspective''s conclusions with findings from powerless/trapped perspectives: does institutional analysis see variance as inevitable (mountain) while lower-power analysis sees suppression (snare)? Do these perspectives make incompatible empirical claims, or are they seeing the same phenomenon from different angles?',
    'If oracle gap is real: analytical observer needs cross-position framework to see what their native position prevents them from seeing. Single-position analysis (pure institutional, pure analytical) systematically misses the structure. The analytical mountain perspective is itself identity-locked within institutional legitimacy frames.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oracle_gap_instantiation, empirical, 'Whether the constraint instantiates the classical oracle gap (Theorem 4)').

omega_variable(
    reform_coalition_sunset_feasibility,
    'Are the proposed alternatives (distributed classification authority, participatory design of indexical parameters, explicit multi-perspectival acknowledgment) actually capable of reducing institutional suppression of lower-power perspectives, or do they themselves become new extractive mechanisms?',
    'Longitudinal study of institutions adopting participatory classification: do lower-power perspectives gain decision-making authority in practice? Are their interpretations incorporated into official verdicts? Or do participatory processes become theatrical while institutional authority remains concentrated?',
    'If sunset is feasible: scaffold perspective is real, and the constraint has genuine reform pathway. If participatory processes become theater: the constraint persists as snare or piton, and reform coalition''s sunset clause is illusory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_coalition_sunset_feasibility, empirical, 'Feasibility of institutional reform to reduce perspectival suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(power_indexed_classification_variance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(picv_tr_t0, power_indexed_classification_variance, theater_ratio, 0, 0.38).
narrative_ontology:measurement(picv_tr_t5, power_indexed_classification_variance, theater_ratio, 5, 0.51).
narrative_ontology:measurement(picv_tr_t10, power_indexed_classification_variance, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(picv_be_t0, power_indexed_classification_variance, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(picv_be_t5, power_indexed_classification_variance, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(picv_be_t10, power_indexed_classification_variance, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(picv_su_t0, power_indexed_classification_variance, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(picv_su_t5, power_indexed_classification_variance, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(picv_su_t10, power_indexed_classification_variance, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(power_indexed_classification_variance, information_standard).
narrative_ontology:boltzmann_floor_override(power_indexed_classification_variance, 0.12).
narrative_ontology:affects_constraint(power_indexed_classification_variance, institutional_legitimacy_gatekeeping).
narrative_ontology:affects_constraint(power_indexed_classification_variance, epistemic_justice_framework).
narrative_ontology:affects_constraint(power_indexed_classification_variance, credentialist_barrier_extraction).

% DUAL FORMULATION NOTE:
% Power-indexed classification variance is upstream of three institutional mechanisms: (1) legitimacy gatekeeping (which perspectives institutional authority accepts as valid), (2) epistemic justice problems (how lower-power positions are systematically silenced), and (3) credentialist barriers (structural mechanisms excluding non-credentialed observers). This constraint story describes the variance property itself. The three downstream constraints describe specific institutional mechanisms that suppress lower-power perspectives within the variance landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(power_indexed_classification_variance, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
