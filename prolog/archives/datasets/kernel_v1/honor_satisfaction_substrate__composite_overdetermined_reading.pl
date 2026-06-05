% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor Satisfaction Substrate — Composite Overdetermined Decline (Exogenous Suppression + Endogenous Delegitimation)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   The decline of dueling in European societies (roughly 18th–20th
 *   centuries) is commonly attributed to either exogenous legal suppression
 *   OR endogenous cultural transformation — a single causal mechanism. This
 *   constraint story operationalizes the overdetermined reading: dueling's
 *   disappearance resulted from simultaneous, non-independent legal
 *   prohibition AND honor-code delegitimation. The two mechanisms reinforced
 *   each other: legal prohibition made dueling irrational (external barrier),
 *   while cultural transformation from 'honor' to 'dignity' frameworks made
 *   it illegitimate (internal barrier). Critically, these were not additive
 *   independent mechanisms but structurally entangled — legal suppression was
 *   more effective because the honor code was already losing cultural
 *   legitimacy, AND cultural delegitimation was more effective because legal
 *   prohibition provided external enforcement. This composite reading differs
 *   from the practice_decline_reading (which emphasizes legal suppression as
 *   primary) and the cultural_contraction_reading (which emphasizes
 *   honor-to-dignity transformation as primary). The composite reading treats
 *   both as real, causally entangled mechanisms that cannot be separately
 *   evaluated.
 *
 * KEY AGENTS:
 *   - Honor Code Practitioners (Duelists): Powerless/trapped victims. Face simultaneous suppression from legal prohibition and from the delegitimizing shift in cultural norms. Cannot exit through either route.
 *   - Centralizing State Authority: Powerful/mobile beneficiary. Genuine coordination function (suppressing private violence) but extractive outcome (consolidating power, redirecting honor-seeking into state-mediated channels). Benefits from both legal suppression AND cultural transformation working together.
 *   - Legalist Normative Elite: Institutional/arbitrage beneficiary. Benefits from the dignity-culture transition that legitimates rights-based legal order over honor-based feudal order. Sees the transformation as natural progress, risking naturalization of contingent elite preferences.
 *   - Regional Aristocratic Factions: Moderate/constrained. Caught between losing dueling as their conflict-resolution mechanism while gaining protection from rival factions through legal order. Receives some coordination benefit but significant extraction (loss of autonomous status determination).
 *   - Analytical Observer: Analytical/analytical. Risks treating the dignity transition as an inexorable law of cultural evolution, naturalizing what is actually a contingent historical outcome that served particular elites.
 *   - Degraded Honor Code Substrate: Institutional/arbitrage (vestigial). What remains of honor culture persists as theater — formal procedures, codes of conduct, ritual elements — maintained through inertia rather than functional necessity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.55).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.68).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor Satisfaction Substrate — Composite Overdetermined Decline (Exogenous Suppression + Endogenous Delegitimation)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2').
narrative_ontology:cs_kernel_codification('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2', distributed).
narrative_ontology:cs_authority_grounding('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2', lineage).
narrative_ontology:cs_interpretation_layer_present('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2').
narrative_ontology:cs_reading_relation('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2', foundational, dual_mechanism_causal_entanglement).
narrative_ontology:cs_axiom_status(dual_mechanism_causal_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2', dual_mechanism_causal_entanglement, empirically_contingent).
narrative_ontology:cs_axiom('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2', foundational, honor_substrate_persistence_in_redirected_form).
narrative_ontology:cs_axiom_status(honor_substrate_persistence_in_redirected_form, holdable).
narrative_ontology:cs_axiom_grounding('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2', honor_substrate_persistence_in_redirected_form, empirically_contingent).
narrative_ontology:cs_reference_frame('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2', dual_legal_cultural_suppression_framework).
narrative_ontology:cs_drift_state('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2', post_enlightenment_legalist_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4dcc79e0-ee4e-4df0-970b-63ef3bbbc1f2', '2026-02-26T14:32:15Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, centralizing_state_authority).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, bourgeois_legalist_norms).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, honor_code_practitioners).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, coordination_substrate_for_aristocratic_conflict_resolution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DUELIST / HONOR PRACTITIONER (SNARE) — Faces simultaneous suppression from external legal mechanisms (prohibition, criminal liability, social ostracism for violation) AND internal delegitimation (the honor code substrate itself is transforming, making the rational basis for dueling unthinkable). Cannot exit the constraint through either route: legal prohibition closes the external pathway; honor transformation closes the internal legitimacy pathway. Both mechanisms operate simultaneously and reinforce each other. Maximum extraction and suppression — the agent is trapped by redundant mechanisms.
constraint_indexing:constraint_classification(honor_satisfaction_substrate__composite_overdetermined_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CENTRALIZING STATE AUTHORITY (TANGLED ROPE) — Genuine coordination function: suppresses dueling to establish monopoly on legitimate violence and reduce aristocratic factional conflict. This is coordination — the state is solving the real problem of uncontrolled private warfare. But the mechanism is extractive: the state consolidates power, redirects honor satisfaction into state-sanctioned hierarchies (military ranks, titles, bureaucratic precedence), and captures the social prestige that dueling previously generated. Benefits from both mechanisms working together: legal suppression removes the overt practice while cultural transformation redirects honor-seeking into state-mediated channels. Active enforcement required because the honor code would persist without prohibition.
constraint_indexing:constraint_classification(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGALIST NORMATIVE ELITE (ROPE) — Beneficiaries of the cultural transformation from 'honor' to 'dignity' (Pinker's framework). Pure coordination: the shift from shame-based honor cultures to rights-based dignity cultures is a genuine epistemic advancement that solves coordination problems (eliminating the need for private violence, establishing written legal rights as the basis for social standing). This perspective sees no extraction — the transformation is functional improvement in the cultural substrate. This reading risks naturalizing what is actually a contingent cultural shift favored by legalist elites.
constraint_indexing:constraint_classification(honor_satisfaction_substrate__composite_overdetermined_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIONAL ARISTOCRATIC FACTION (TANGLED ROPE) — Constrained by both mechanisms but receives some coordination benefit through legal order that prevents rival factions from uncontrolled violence. The aristocracy loses dueling as a conflict resolution mechanism (extraction) but gains protection from violent elimination by rivals and incorporation into state-mediated hierarchies (coordination). Exit is constrained — they cannot sustain dueling against legal prohibition and delegitimation, but they also cannot exit the honor-satisfaction problem entirely because honor remains culturally salient even as its expression mechanisms are closed. Significant extraction (loss of autonomous conflict resolution) but not total (some gains through incorporation into state).
constraint_indexing:constraint_classification(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the transition from honor to dignity cultures is presented as an inexorable transformation in human moral development (Pinker, Fukuyama frameworks). The constraint appears as an unchangeable feature of cultural evolution itself — societies unavoidably transition from shame-based to rights-based systems as they centralize and professionalize. This perspective naturalizes the contingent historical process, treating the endpoint (dignity culture) as inevitable. However, this reading instantiates the false-summit pattern: the 'natural law' of cultural transformation actually serves the interests of the agents (legalist elites, centralizing states) who benefit from delegitimating honor codes.
constraint_indexing:constraint_classification(honor_satisfaction_substrate__composite_overdetermined_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: DEGRADED HONOR CODE / VESTIGIAL SUBSTRATE (PITON) — The honor code persists as a ghost in the cultural substrate — still invoked as a legitimacy claim, still structuring some social interactions, still generating occasional dueling even after legal prohibition — but stripped of its functional role as a primary mechanism for resolving disputes or establishing social standing. Theater ratio ≥0.70: what remains of honor-based practice is largely performative, maintaining formal grievance procedures and codes of conduct (seconds, formal challenges, ritualized procedures) while the material coordination function has migrated to legal institutions. The code is maintained through institutional inertia and nostalgia rather than because it works.
constraint_indexing:constraint_classification(honor_satisfaction_substrate__composite_overdetermined_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(honor_satisfaction_substrate__composite_overdetermined_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(honor_satisfaction_substrate__composite_overdetermined_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, TR),
    TR >= 0.70.

:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The constraint exhibits significant extraction: the state consolidates power by monopolizing legitimate violence; legalist elites establish rights-based frameworks that replace honor-based status determination; duelists lose access to their primary mechanism for establishing and defending social standing. But extractiveness is not maximal (≥0.66) because genuine coordination benefits exist: legal order does reduce factional violence and chaos, and centralized authority does provide stability that honor-based conflict resolution could not. The dual mechanism (legal suppression + cultural transformation) works together to achieve extraction with plausible coordination benefits. Suppression (0.68): High but not maximal. Rising trajectory over the interval (0.40 → 0.62 → 0.68) reflects enforcement intensification: early in the period, legal prohibition alone could not eliminate dueling; as cultural delegitimation progressed, suppression requirements declined because external prohibition was reinforced by internal normative collapse. Theater ratio (0.48): Moderate. The constraint itself is not highly performative (it's real suppression and real cultural transformation), but the degraded honor code that persists after prohibition exhibits high theater — formal challenges, seconds, ritualized procedures persist as ghosts of function without function.
 *
 * PERSPECTIVAL GAP:
 *   The composite overdetermined reading produces six different classifications from structurally identical base properties. The duelist sees snare (trapped by redundant mechanisms). The state sees tangled_rope (genuine coordination with extraction). The legalist elite sees rope (natural cultural progress). The aristocratic faction sees tangled_rope (mixed coordination benefit and extraction). The analytical observer risks seeing mountain (natural cultural evolution). The degraded honor code itself exhibits piton characteristics (performative theater maintaining inertial form). The perspectival variance demonstrates that the decline is genuinely overdetermined — different observers correctly perceive different mechanisms based on their structural position, and all mechanisms are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from whether the agent benefits or bears costs from the constraint. Beneficiaries (state, legalist elite) experience low or negative directionality (low d → negative chi) because the constraint flows toward them. Victims (duelists, honor practitioners) experience high directionality (high d → high chi) because they bear the suppression cost. The critical insight: in overdetermined suppression, directionality COMPOUNDS because multiple non-independent mechanisms operate simultaneously. The duelist faces both legal prohibition (one extraction route) and cultural delegitimation (another extraction route) — both mechanisms push d toward 1.0. This is not additive (which would violate the chi formula), but it explains why the duelist's experienced extraction is severe: no single escape route exists because both pathways are blocked. The entanglement of mechanisms is the reason this is tangled_rope (beneficiary perspective) rather than pure rope, and snare (victim perspective) rather than constrained mobil.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading resolves mandatrophy by treating overdetermination as a structural feature, not a classification problem. Dueling's disappearance is not ambiguous between 'legal suppression' and 'cultural transformation' — it exhibits BOTH simultaneously and inseparably. The tangled_rope classification (at the state authority and some aristocratic faction perspectives) correctly captures that genuine coordination (suppressing private violence) coexists with extractive asymmetry (state consolidation, elite interest capture, loss of alternative status mechanisms). The snare classification (from duelist perspective) correctly captures that the victim faces irreconcilable suppression from multiple directions. The mountain classification (from analytical observer) correctly identifies the risk of naturalizing contingent outcomes. The mandate is to avoid choosing between mechanisms (which would be false simplification) while accurately measuring the extractiveness that results from their combination. This story accomplishes that by declaring both mechanisms as structural elements and modeling how they reinforce each other through measurements showing rising suppression and extractiveness over the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_endogenous_causal_entanglement,
    'Are legal suppression and honor-code delegitimation causally independent mechanisms, or are they structurally entangled such that each makes the other more effective?',
    'Comparative historical analysis: (a) jurisdictions where legal prohibition was strong but honor culture remained legitimate (did dueling persist despite legal penalty?); (b) jurisdictions where honor culture was already delegitimizing before legal prohibition (did dueling decline without explicit law?); (c) chronological sequencing to determine which mechanism preceded the other and what feedback loops emerged',
    'If independent: dueling''s decline ε can be modeled additively; classify as two separate constraints. If entangled: ε must reflect the mutual reinforcement; composite reading (current story) is correct. If one mechanism is primary and the other derivative: delegate to sibling readings (practice_decline_reading or cultural_contraction_reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_endogenous_causal_entanglement, empirical, 'Causal independence vs. entanglement of legal suppression and cultural delegitimation mechanisms').

omega_variable(
    dignity_culture_as_contingent_vs_inevitable,
    'Did honor-to-dignity transition represent a necessary cultural evolution driven by structural imperatives of modernization, or a contingent historical outcome favored by particular social elites?',
    'Examine societies that underwent legal centralization and state monopoly on violence WITHOUT corresponding delegitimation of honor cultures (Russia, parts of the Ottoman Empire, Japan until Meiji modernization); identify whether absence of dignity-culture transition correlates with other variables (degree of legalism in elite discourse, strength of bourgeois institutions, influence of Enlightenment philosophy)',
    'If necessary evolution: mountain classification of the analytical perspective is justified; cultural transformation is inexorable feature of human development. If contingent: mountain dissolves into false summit; the transformation was a choice made by particular actors to serve their interests (legalists, centralizing states), not a law of nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_culture_as_contingent_vs_inevitable, conceptual, 'Dignity transition as necessary or contingent outcome').

omega_variable(
    honor_satisfaction_substrate_persistence,
    'Does the honor satisfaction substrate genuinely transform/disappear, or is it redirected into alternative institutional channels?',
    'Track manifestations of honor-seeking behavior after dueling prohibition: (a) military rank acquisition and decoration display; (b) bureaucratic precedence and title; (c) professional licensing hierarchies; (d) modern duel-analogs (litigation, public humiliation cycles, reputation damage); evaluate whether these mechanisms satisfy the same underlying honor-satisfaction function with different surfaces',
    'If genuine transformation: the mountain perspective (cultural evolution) has merit; honor substrate is actually transcended. If redirection: the honor code persists in different forms; legal prohibition and cultural legitimacy both serve to channel honor-seeking into state-controlled institutions (extraction mechanism). This supports the current tangled_rope reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(honor_satisfaction_substrate_persistence, empirical, 'Whether honor substrate is transformed or redirected').

omega_variable(
    false_summit_natural_law_status,
    'Is the analytical mountain perspective justified (unavoidable cultural evolution) or a naturalization of contingent elite preferences?',
    'Examine whether the ''dignity transition'' narrative serves identifiable institutional interests (legalists, centralizing states, bourgeois elites); trace the intellectual history of the transition narrative to identify whose framework is being universalized. Compare with counterfactual: would modern societies be fundamentally unstable if honor codes remained culturally salient alongside legal rights frameworks?',
    'If natural law is justified: analytical perspective correctly identifies inevitable structure. If naturalization: analytical perspective triggers false_summit_mountain engine signature; constraint reclassifies to tangled_rope (exogenous legal suppression serving elite interests, dressed in cultural inevitability language).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, conceptual, 'Natural law status of dignity transition: justified or naturalized elite preferences').

omega_variable(
    coordinate_system_ambiguity,
    'From which coordinate system should the decline be measured: the honor code''s own internal logic (where it remains coherent) or the external legalist coordinate system (where it appears incoherent)?',
    'Examine surviving honor-code texts, judicial records, and duelist memoirs: do practitioners themselves report delegitimation (internal coordinate collapse) or external suppression as the primary barrier? Identify whether the code''s coherence persists in private discourse but is suppressed in public practice.',
    'If measured from honor-code coordinate: delegitimation is real and primary; honor substrate has genuinely transformed. If measured from legalist coordinate: suppression is primary; honor substrate persists but is suppressed. The composite reading presumes BOTH are real from DIFFERENT perspectives — an omega documenting that measurement frame is not neutral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_system_ambiguity, conceptual, 'Coordinate system ambiguity in measuring decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_composite_theater_t0, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(honor_composite_theater_t25, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(honor_composite_theater_t50, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(honor_composite_extractiveness_t0, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(honor_composite_extractiveness_t25, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(honor_composite_extractiveness_t50, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(honor_composite_suppression_t0, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(honor_composite_suppression_t25, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(honor_composite_suppression_t50, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint story is the composite_overdetermined_reading of the honor_satisfaction_substrate kernel. The sibling stories (practice_decline_reading and cultural_contraction_reading) represent alternative framings of the same historical phenomenon — dueling's decline — from different causal emphases. The network linking these three stories enables comparative analysis of how different reading positions (exogenous suppression primary vs. endogenous transformation primary vs. entangled dual mechanisms) produce different structural models of the same historical process. Each story has a distinct epsilon value reflecting the mechanism(s) it emphasizes as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
