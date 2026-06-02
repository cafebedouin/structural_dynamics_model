% ============================================================================
% CONSTRAINT STORY: practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_practice_decline_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: practice_decline_reading
 *   human_readable: Honor Code Substrate Persisting Under Legal Prohibition of Dueling Practice
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint story models a specific reading of the honor satisfaction
 *   substrate: that the normative code persists as internalized commitment
 *   among practitioners while the primary enforcement/satisfaction mechanism
 *   (dueling practice) declines due to exogenous legal prohibition and
 *   institutional barriers. The constraint exhibits properties of Rope
 *   (coordination problem solved by state enforcement + institutional
 *   alternatives) rather than Mountain (natural attrition of honor concepts)
 *   or Snare (pure domination). The reading captures the period of transition
 *   during which agents retain the honor frame but lack sanctioned practice,
 *   forcing relocation of satisfaction into military codes, legal recourse,
 *   and institutional status. Base extractiveness is moderate (0.38) because
 *   the constraint both solves a coordination problem (preventing
 *   honor-violence from destabilizing social order) and extracts from
 *   practitioners (suppresses their preferred satisfaction mechanism).
 *   Suppression is moderate-high (0.62) because legal prohibition and
 *   institutional barriers are real obstacles to practice, yet alternative
 *   satisfaction pathways exist and are accessible to motivated agents.
 *   Theater ratio (0.58) reflects that institutional alternatives (military
 *   honor codes, Southern etiquette, legal processes) maintain honor
 *   discourse performatively while actual satisfaction capacity is reduced
 *   relative to the dueling mechanism.
 *
 * KEY AGENTS:
 *   - Legal State Authority: Primary beneficiary (institutional/arbitrage) — achieves social stability by replacing private violence with institutional mediation; experiences constraint as coordination mechanism
 *   - Duelist Under Prohibition: Primary victim (powerless/trapped) — caught between internalized honor requirement and legal destruction; identity fused with honor satisfaction; maximum experienced extraction
 *   - Military Officer: Secondary agent (moderate/constrained) — benefits from institutional rank alternative while constrained by loss of private satisfaction; mixed experience of coordination and extraction
 *   - Aristocratic Caste: Secondary victim (powerful/mobile) — faces erosion of monopoly on honor satisfaction but retains status through legal/institutional pathways; significant extraction but with exit options
 *   - Honor Code Institution: Structural substrate (institutional/arbitrage) — persists normatively but declines practically; maintained through inertia and theatrical renewal (military codes, Southern codes)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as a successful coordination shift from violent to institutional satisfaction mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(practice_decline_reading, 0.38).
domain_priors:suppression_score(practice_decline_reading, 0.62).
domain_priors:theater_ratio(practice_decline_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(practice_decline_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(practice_decline_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(practice_decline_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(practice_decline_reading, rope).
narrative_ontology:human_readable(practice_decline_reading, "Honor Code Substrate Persisting Under Legal Prohibition of Dueling Practice").
narrative_ontology:topic_domain(practice_decline_reading, "historical_sociology/legal_history/cultural_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(practice_decline_reading, '5d6cd0c4-b517-432e-ad67-4c2d9394ac42').
narrative_ontology:cs_created_at('5d6cd0c4-b517-432e-ad67-4c2d9394ac42', '').
narrative_ontology:cs_kernel_codification('5d6cd0c4-b517-432e-ad67-4c2d9394ac42', fixed_text).
narrative_ontology:cs_authority_grounding('5d6cd0c4-b517-432e-ad67-4c2d9394ac42', lineage).
narrative_ontology:cs_interpretation_layer_present('5d6cd0c4-b517-432e-ad67-4c2d9394ac42').
narrative_ontology:cs_kernel_id(practice_decline_reading, honor_satisfaction_substrate).
narrative_ontology:cs_reading_relation('5d6cd0c4-b517-432e-ad67-4c2d9394ac42', cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d6cd0c4-b517-432e-ad67-4c2d9394ac42', composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('5d6cd0c4-b517-432e-ad67-4c2d9394ac42', foundational, honor_substrate_persistence_under_exogenous_pressure).
narrative_ontology:cs_axiom_status(honor_substrate_persistence_under_exogenous_pressure, holdable).
narrative_ontology:cs_axiom('5d6cd0c4-b517-432e-ad67-4c2d9394ac42', foundational, legal_enforcement_as_primary_decline_driver).
narrative_ontology:cs_axiom_status(legal_enforcement_as_primary_decline_driver, holdable).
narrative_ontology:cs_reference_frame('5d6cd0c4-b517-432e-ad67-4c2d9394ac42', honor_code_functional_in_dueling_practice).
narrative_ontology:cs_drift_state('5d6cd0c4-b517-432e-ad67-4c2d9394ac42', post_legal_prohibition_era, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(practice_decline_reading, legal_state_authority).
narrative_ontology:constraint_beneficiary(practice_decline_reading, institutional_modernization).
narrative_ontology:constraint_victim(practice_decline_reading, honor_satisfaction_practitioners).
narrative_ontology:constraint_victim(practice_decline_reading, feudal_honor_codes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DUELIST (SNARE) — Trapped between internalized honor code requiring satisfaction and legal prohibition making practice fatal to freedom/life. Cannot exit the honor frame without identity dissolution; cannot practice honor without legal destruction. Experiences pure extraction: the constraint forces choice between self-conception and survival.
constraint_indexing:constraint_classification(practice_decline_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MILITARY OFFICER IN TRANSITION (TANGLED ROPE) — Constrained by dual codes (military honor, legal prohibition). Benefits from alternative honor satisfaction mechanisms (promotion, martial prowess, institutional reputation). Experiences mixed coordination and extraction: the constraint both enables institutional military merit AND constrains personal satisfaction pathways.
constraint_indexing:constraint_classification(practice_decline_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGAL STATE AUTHORITY (ROPE) — Institutional beneficiary. Solves coordination problem: replacing private honor violence with legal recourse and institutional rank achieves social order without (theoretically) eliminating honor itself. Experiences the constraint as coordination: legal prohibition + alternative status mechanisms = peaceful satisfaction of honor concerns. Pure coordination function.
constraint_indexing:constraint_classification(practice_decline_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ARISTOCRATIC CASTE (TANGLED ROPE) — Powerful but facing mobility constraint: legal prohibition erodes their monopoly on honor satisfaction while alternative institutional pathways (military rank, political office, social prestige) remain partially available. Experiences extraction (subordination of their satisfaction mechanism to state authority) alongside coordination benefit (preservation of status hierarchy through legal rather than violent means).
constraint_indexing:constraint_classification(practice_decline_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: THE HONOR CODE INSTITUTION (PITON) — The normative substrate of honor persists but its functional enforcement mechanism (dueling practice) has atrophied. The code survives through inertia and theatrical maintenance (Southern 'culture of honor', military honor codes, institutional etiquette) despite the removal of its primary satisfaction mechanism. Theater ratio high because the normative claims persist without corresponding practice capacity.
constraint_indexing:constraint_classification(practice_decline_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From civilizational distance, the constraint is a coordination shift: honor satisfaction mechanisms transition from private violence (dueling) to institutional channels (law, military, bureaucracy). The normative substrate persists; the practice declines because alternative satisfaction pathways are available and enforced. This reading sees the constraint as solving a coordination problem (how to satisfy honor claims without destabilizing social order) rather than as a mountain of human nature or a snare of domination.
constraint_indexing:constraint_classification(practice_decline_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(practice_decline_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(practice_decline_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(practice_decline_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(practice_decline_reading, TR),
    TR >= 0.70.

:- end_tests(practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The legal state achieves substantial coordination benefit by preventing honor violence while maintaining status hierarchy. But practitioners bear significant cost — suppression of their preferred satisfaction mechanism. The extractiveness is not low (Rope threshold) because the suppression is real and substantial, and beneficiaries (state, institutional order) derive genuine advantage from prohibition. The extractiveness is not high (Snare threshold) because functional alternatives exist and are accessible: military honor codes, institutional rank, legal recourse, social prestige. Practitioners face high cost but not impossibility. Suppression (0.62): Moderate-high. Legal prohibition is enforced; duelists face death or imprisonment; institutional barriers to private satisfaction are substantial. But suppression is not total (0.85+) because: (1) enforcement is imperfect (illegal duels continue, especially in peripheral regions); (2) alternative satisfaction pathways are available (military, institutional); (3) agents retain cognitive freedom to honor the code (even if practice is blocked). Theater ratio (0.58): Moderate-high. Institutional alternatives to dueling (military honor codes, Southern etiquette, legal arbitration, bureaucratic rank) maintain honor discourse but with reduced correspondence to actual satisfaction. The code is kept alive through performative invocation — especially in military and Southern contexts — while its primary enforcement mechanism (dueling) is disabled. As time progresses, theater rises (0.35 → 0.58) as the normative substrate persists longer than practice capacity, indicating degradation toward Piton classification at longer timescales.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a perspectival gap between beneficiary and victim. The state authority and institutional modernization see Rope: a coordination mechanism that solves the honor-violence problem while maintaining status differentiation. The duelist sees Snare: trapped between identity requirement and legal prohibition, with no satisfactory exit. The military officer sees Tangled Rope: mixed benefit (institutional rank alternative) and extraction (suppressed private satisfaction). The piton perspective (at civilizational timescale) sees the honor code institution itself degrading — the normative substrate persists but increasingly through theater rather than functional capacity. The analytical observer at civilizational scope sees Rope: a successful transition of satisfaction mechanisms from private (dueling) to public (institutional) channels. The gap is real — different agents experience radically different constraint types — but all perspectives are coherent readings of the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position within the constraint. The state authority (institutional/arbitrage) is a net beneficiary: legal prohibition + institutional alternatives solve their coordination problem while extracting from practitioners. Canonical d for institutional/arbitrage is ~0.00-0.15 (beneficiary range). The duelist (powerless/trapped) is a pure target: suppressed satisfaction mechanism, identity-locked to honor frame, no exit options. Canonical d for powerless/trapped is ~0.95+ (victim range). The military officer (moderate/constrained) is a mixed case: benefits from institutional rank (lowers d) but constrained in satisfaction choice (raises d). Derived d ~0.55-0.65 (middle range). The aristocratic caste (powerful/mobile) has arbitrage options — they can relocate satisfaction to legal/institutional channels — so derived d is lower (~0.40-0.50) despite being victims of suppression. The analytical observer (analytical/analytical) is neutral: canonical d ~0.73 (observer baseline). These directionality values feed the sigmoid f(d) to compute experienced extractiveness (χ) for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The practice_decline_reading resolves mandatrophy by showing that the honor substrate persists as internalized commitment while practice (dueling) declines due to external enforcement and available alternatives. The constraint is Rope (coordination via institutional enforcement + alternative satisfaction channels) not Mountain (natural attrition of honor as human concept) or Snare (pure domination). The reading assumes the normative code has real functional continuity — it shapes behavior, motivates institutional rank-seeking, drives military codes — even after dueling practice is prohibited. This is empirically contestable (see omega variables), but the reading is internally coherent: a coordination mechanism can persist with a modified satisfaction mechanism. The analytical observer's Rope classification suggests the constraint successfully solves a problem (honor violence destabilizing social order) while maintaining the honor substrate in attenuated form. The victim's Snare classification reflects their experience of suppressed preferred mechanism, not the system's actual type. Both readings are valid from their positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_substrate_versus_practice_distinction,
    'Is the persistence of honor code after dueling decline genuine substrate persistence or theatrical maintenance of a dead normative frame?',
    'Longitudinal analysis of: (1) frequency of honor-satisfaction invocations in public discourse vs practice; (2) institutional persistence of honor codes (military, Southern); (3) effectiveness of institutional alternatives in satisfying honor claims; (4) whether descendants of honor-bound groups perceive the code as binding vs vestigial',
    'If genuine substrate: constraint is Rope (coordination shift with persistent function). If theatrical: constraint is Piton (degraded normative frame maintained by inertia). The classification hinges on whether the honor code retains functional salience after practice decline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_substrate_versus_practice_distinction, empirical, 'Whether honor substrate is functionally persistent or theatrically maintained').

omega_variable(
    exogenous_versus_endogenous_decline,
    'Did dueling practice decline primarily due to external legal/institutional enforcement, or would it have declined anyway due to internal evolution of honor concepts toward non-violent satisfaction?',
    'Comparative-historical analysis: (1) societies with strong legal prohibition vs weak enforcement; (2) temporal sequence of legal prohibition vs practice decline; (3) internal philosophical critiques of dueling within honor traditions (Enlightenment, religious reform) vs state-imposed bans; (4) whether prohibition accelerated decline or merely coincided with it',
    'If primarily exogenous (state enforcement): constraint is Rope (coordination via external enforcement). If primarily endogenous (internal evolution): constraint is closer to Mountain (natural cultural evolution) or Piton (vestigial). Degree of exogeneity determines whether the constraint requires active enforcement to persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_versus_endogenous_decline, empirical, 'Exogenous enforcement vs endogenous cultural evolution in dueling decline').

omega_variable(
    alternative_satisfaction_effectiveness,
    'Do legal and institutional mechanisms (courts, military rank, bureaucratic status) genuinely satisfy honor claims, or do they merely suppress the satisfaction mechanism while leaving the underlying claim unresolved?',
    'Ethnographic/historical analysis: (1) comparative satisfaction rates for honor claims under dueling vs legal systems; (2) persistence of honor violence after legal prohibition (illegal duels, feuds, vigilante justice) indicating unmet satisfaction; (3) discourse analysis of whether agents describe institutional alternatives as true satisfaction or mere substitutes; (4) longitudinal tracking of groups with high honor investment in legal vs dueling contexts',
    'If genuinely satisfactory: constraint is Rope (coordination with functional alternative). If merely suppressive: constraint is Snare (extraction via prohibition of satisfaction). The effectiveness of alternatives determines whether the constraint solves a coordination problem or creates one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_satisfaction_effectiveness, empirical, 'Whether institutional alternatives genuinely satisfy honor claims').

omega_variable(
    kernel_reading_disambiguation,
    'Is this constraint-reading one legitimate interpretation of a contested kernel (honor satisfaction substrate) or does it collapse under scrutiny into a different reading altogether?',
    'Philosophical analysis of whether the ''practice decline'' reading coherently sustains the thesis that honor substrate persists while practice declines. Alternative readings (cultural_contraction_reading, composite_overdetermined_reading) may claim substrate also contracts or that decline is over-determined by multiple causes. This omega flags the committer-frame ambiguity itself.',
    'If reading is coherent: proceed with current classification. If reading collapses: constraint may be better analyzed under sibling reading with different ε, beneficiary/victim structure, and perspectives. The kernel identity itself may be under-determined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Committer-frame coherence of practice-decline reading vs sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(practice_decline_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prac_tr_t0, practice_decline_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prac_tr_t3, practice_decline_reading, theater_ratio, 3, 0.46).
narrative_ontology:measurement(prac_tr_t6, practice_decline_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(prac_be_t0, practice_decline_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prac_be_t3, practice_decline_reading, base_extractiveness, 3, 0.31).
narrative_ontology:measurement(prac_be_t6, practice_decline_reading, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(practice_decline_reading, identity_coordination).
narrative_ontology:affects_constraint(practice_decline_reading, cultural_contraction_reading).
narrative_ontology:affects_constraint(practice_decline_reading, composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% The honor satisfaction substrate kernel has three readings: practice_decline_reading (exogenous enforcement primary), cultural_contraction_reading (endogenous cultural evolution primary), composite_overdetermined_reading (both mechanisms). Each reading is a distinct constraint with its own ε, beneficiary/victim structure, perspectives, and classification. This file instantiates the practice_decline reading only. Sibling readings are separate constraint stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
