% ============================================================================
% CONSTRAINT STORY: rights_catalog_facade__external_showcase_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rights_catalog_facade__external_showcase_reading, []).

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
 *   constraint_id: rights_catalog_facade__external_showcase_reading
 *   human_readable: Rights Catalog Facade: External Showcase Reading
 *   domain: legal/doctrinal/political_legitimacy
 *
 * SUMMARY:
 *   The Popular Front era's rights catalog presents a structural paradox: a
 *   text that is maximally quoted internationally and minimally invocable
 *   domestically. The regime derives legitimacy from demonstrating to Western
 *   audiences that socialism delivers on rights that liberal constitutions
 *   merely promise. Simultaneously, the regime suppresses domestic legal
 *   invocation of these same rights, framing such invocation as Western
 *   legalism inconsistent with socialist goals. This creates a constraint
 *   where the catalog functions as international propaganda while extracting
 *   obedience domestically through suppression of invocation. The constraint
 *   is snare-dominant from the perspective of those expecting to invoke the
 *   rights domestically, rope-dominant from the perspective of the propaganda
 *   apparatus and international fellow travelers, and piton from the regime's
 *   constitutional authority (the catalog persists through institutional
 *   inertia despite atrophied domestic function). The theater ratio rises
 *   over the interval as the regime's international circulation of the
 *   catalog increases while its domestic enforcement remains stagnant,
 *   converting the constraint from mixed coordination-extraction to
 *   predominantly performative. The extractiveness increases as suppression
 *   of domestic invocation becomes more systematic, moving from informal
 *   institutional pressure to explicit delegitimization of rights-based
 *   arguments in official ideology.
 *
 * KEY AGENTS:
 *   - Domestic Audience: Powerless/trapped (national scope) — citizens who cannot cite the catalog in legal contexts; bear maximum extraction through suppressed invocation
 *   - Rights Bearers Expecting Invocability: Moderate/constrained (national scope) — activists, lawyers, dissidents who attempt to invoke rights despite costs; face suppression enforcement
 *   - International Propaganda Apparatus: Institutional/arbitrage (global scope) — state media, diplomatic corps, international communist organizations; benefit from international circulation of the catalog
 *   - Fellow Travelers and International Left: Powerful/arbitrage (global scope) — foreign communists, socialist intellectuals, Western sympathizers; benefit from citing the catalog as evidence of socialist superiority
 *   - Dissident Legal Community: Organized/constrained (national scope) — domestic lawyers, reformers, constitutional scholars; experience mixed coordination (catalog names aspirational rights) and extraction (risk of citation)
 *   - Regime's Constitutional Authority: Institutional/arbitrage (civilizational scope) — the state institution maintaining the catalog's formal validity; derives benefit from international credibility while preventing domestic enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rights_catalog_facade__external_showcase_reading, 0.58).
domain_priors:suppression_score(rights_catalog_facade__external_showcase_reading, 0.72).
domain_priors:theater_ratio(rights_catalog_facade__external_showcase_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rights_catalog_facade__external_showcase_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(rights_catalog_facade__external_showcase_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rights_catalog_facade__external_showcase_reading, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rights_catalog_facade__external_showcase_reading, snare).
narrative_ontology:human_readable(rights_catalog_facade__external_showcase_reading, "Rights Catalog Facade: External Showcase Reading").
narrative_ontology:topic_domain(rights_catalog_facade__external_showcase_reading, "legal/doctrinal/political_legitimacy").

domain_priors:requires_active_enforcement(rights_catalog_facade__external_showcase_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rights_catalog_facade__external_showcase_reading, '155910d8-7926-4d18-8a1a-948952e8550a').
narrative_ontology:cs_kernel_codification('155910d8-7926-4d18-8a1a-948952e8550a', formalized).
narrative_ontology:cs_authority_grounding('155910d8-7926-4d18-8a1a-948952e8550a', extraction).
narrative_ontology:cs_interpretation_layer_present('155910d8-7926-4d18-8a1a-948952e8550a').
narrative_ontology:cs_reading_relation('155910d8-7926-4d18-8a1a-948952e8550a', rights_catalog_facade__conditional_clause_reading, coexists_with).
narrative_ontology:cs_reading_relation('155910d8-7926-4d18-8a1a-948952e8550a', rights_catalog_facade__social_rights_substance_reading, coexists_with).
narrative_ontology:cs_axiom('155910d8-7926-4d18-8a1a-948952e8550a', foundational, catalog_written_for_export).
narrative_ontology:cs_axiom_status(catalog_written_for_export, holdable).
narrative_ontology:cs_axiom_grounding('155910d8-7926-4d18-8a1a-948952e8550a', catalog_written_for_export, empirically_contingent).
narrative_ontology:cs_axiom('155910d8-7926-4d18-8a1a-948952e8550a', foundational, domestic_invocation_systematically_suppressed).
narrative_ontology:cs_axiom_status(domestic_invocation_systematically_suppressed, holdable).
narrative_ontology:cs_axiom_grounding('155910d8-7926-4d18-8a1a-948952e8550a', domestic_invocation_systematically_suppressed, empirically_contingent).
narrative_ontology:cs_reference_frame('155910d8-7926-4d18-8a1a-948952e8550a', socialist_superior_rights_delivery).
narrative_ontology:cs_drift_state('155910d8-7926-4d18-8a1a-948952e8550a', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('155910d8-7926-4d18-8a1a-948952e8550a', '').
narrative_ontology:cs_kernel_id(rights_catalog_facade__external_showcase_reading, rights_catalog_facade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rights_catalog_facade__external_showcase_reading, international_propaganda_apparatus).
narrative_ontology:constraint_beneficiary(rights_catalog_facade__external_showcase_reading, fellow_travelers_abroad).
narrative_ontology:constraint_victim(rights_catalog_facade__external_showcase_reading, domestic_audience).
narrative_ontology:constraint_victim(rights_catalog_facade__external_showcase_reading, rights_bearers_expecting_invocability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC AUDIENCE (SNARE) — Faces maximal suppression of invocation. The catalog's rights are quoted in international forums as proof of socialist superiority but are unquotable in domestic legal contexts. Trapped by geographic scope and denied exit from the constraint's extractive mechanism. The catalog is a tool for extracting international legitimacy while suppressing domestic enforcement claims.
constraint_indexing:constraint_classification(rights_catalog_facade__external_showcase_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RIGHTS BEARERS EXPECTING INVOCABILITY (SNARE) — Face constraints on citation of the catalog in domestic legal proceedings. High suppression: attempting to invoke the stated rights in courts or administrative proceedings is framed as legalistic/Western formalism inconsistent with the socialist order. Exit options are constrained, not trapped — some dissident voices attempt invocation despite costs — but the mechanism extracts obedience through the cost of citation.
constraint_indexing:constraint_classification(rights_catalog_facade__external_showcase_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL PROPAGANDA APPARATUS (ROPE) — Experiences the catalog as pure coordination: it solves the communication problem of demonstrating socialist superiority to Western audiences. The apparatus benefits from the catalog's international circulation and derives legitimacy from citing it. Net beneficiary with full arbitrage options — can deploy the catalog abroad while maintaining distance from domestic enforcement demands.
constraint_indexing:constraint_classification(rights_catalog_facade__external_showcase_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FELLOW TRAVELERS AND INTERNATIONAL LEFT (ROPE) — Benefit from citing the catalog as evidence that socialist regimes deliver on rights that liberal constitutions merely promise. The constraint solves their coordination problem: how to defend the regime against Western criticism. They have arbitrage options — cite the catalog abroad, accept it silently regarding domestic enforcement. Net beneficiary.
constraint_indexing:constraint_classification(rights_catalog_facade__external_showcase_reading, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DISSIDENT LEGAL COMMUNITY (TANGLED ROPE) — Experiences both coordination and extraction. Some benefit from the catalog's existence as an aspirational text (coordination function: the catalog names rights that reformers can work toward). But the catalog also extracts from them: attempting to invoke it domestically carries legal and career risk. The constraint is genuinely hybrid — the catalog could enable rights advocacy IF its domestic invocation were permitted. Constrained exit options: they can cite the catalog abroad (like international lawyers), but not at home without cost.
constraint_indexing:constraint_classification(rights_catalog_facade__external_showcase_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGIME'S CONSTITUTIONAL AUTHORITY (PITON) — The regime maintains the catalog as symbolic performance: it persists through institutional inertia and claims of socialist authenticity, but its functional enforcement has atrophied. The regime sees the catalog as simultaneously its greatest asset (internationally) and its greatest institutional inertia (domestically). Theater ratio is very high — the catalog's primary function is presentation to foreign audiences, not domestic legal regulation. The constitutional authority continues to assert the catalog's validity while systematically preventing its domestic invocation.
constraint_indexing:constraint_classification(rights_catalog_facade__external_showcase_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint could be read as a natural law of authoritarian legitimacy: any regime needs international credibility, so it must present rights guarantees to foreign audiences even if it cannot enforce them domestically. However, this is a false summit: the constraint is not a natural law but a constructed institutional arrangement that benefits specific actors (the propaganda apparatus, the regime). The beneficiaries are identifiable, and the mechanism is suppression of domestic invocation — not immutable features of legitimacy.
constraint_indexing:constraint_classification(rights_catalog_facade__external_showcase_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rights_catalog_facade__external_showcase_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rights_catalog_facade__external_showcase_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rights_catalog_facade__external_showcase_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rights_catalog_facade__external_showcase_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rights_catalog_facade__external_showcase_reading, TR),
    TR >= 0.70.

:- end_tests(rights_catalog_facade__external_showcase_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The regime extracts international legitimacy from the catalog while suppressing its domestic invocation, preventing citizens from using their own stated rights. The extractiveness is not maximal (0.72+) because some benefit is genuine coordination: the catalog does describe aspirational social rights (employment, housing, education) that the regime partially delivers. But the primary extractive mechanism is the suppression of invocation paired with international circulation. Suppression (0.72): High and rising. The regime systematically prevents domestic legal citation of the catalog through informal institutional pressure (courts rejecting rights-based arguments), explicit delegitimization (framing rights invocation as Western formalism), and informal sanctions (career costs for lawyers citing the catalog). The suppression requirement increases over the interval as the regime solidifies its ideology that rights invocation is inconsistent with socialism. Theater ratio (0.85): Very high and rising. The catalog's primary contemporary function is international presentation; its domestic legal function has atrophied to near-zero. The theater ratio reflects that the catalog is maintained primarily for its symbolic value abroad, not its operational value domestically. The rise from 0.65 to 0.85 reflects intensifying circulation internationally paired with intensifying suppression domestically — the constraint becomes increasingly performative over time.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals how the same constraint distributes radically different classification across different observers. The domestic audience and rights bearers experience snare: they are trapped or constrained by suppression with no beneficiary status. The international propaganda apparatus and fellow travelers experience rope: they solve their coordination problem (demonstrating socialist superiority) with the catalog as their tool. The dissident legal community experiences tangled_rope: the catalog provides aspirational language (coordination) while carrying invocation costs (extraction). The regime's constitutional authority experiences piton: the catalog is ceremonially maintained but functionally atrophied. The analytical observer at civilizational scale risks false summit (seeing this as a natural law of authoritarian legitimacy) but is corrected by the structural data showing identifiable beneficiaries and suppression mechanisms. The key insight: the constraint is snare from the perspective of those expecting the rights to be invocable, but rope from the perspective of those using the catalog for international propaganda. The same text, opposite classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural relationship to the constraint. The domestic audience are victims (beneficiary: no; bearing costs: yes; d → 0.85) with trapped exit → high f(d) → high experienced extractiveness. The propaganda apparatus are beneficiaries (beneficiary: yes; bearing costs: no; d → 0.10) with arbitrage exit → negative f(d) → low or negative experienced extractiveness. The fellow travelers are beneficiaries (d → 0.15) with powerful/arbitrage status → institutional-level benefit from international credibility. The dissident legal community are mixed: partly victims (suppression cost), partly beneficiary (aspirational framing) → moderate d → moderate experienced extraction. The regime's constitutional authority experiences arbitrage exit and institutional beneficiary status → low d despite maintaining the suppression mechanism, because the regime doesn't experience its own suppression as extraction (it experiences it as enforcement of correct ideology).
 *
 * MANDATROPHY ANALYSIS:
 *   The extractiveness of 0.58 sits at the boundary where the constraint must be classified based on concrete structural analysis, not default assumptions. This is NOT a natural law (mountain) — the suppression is institutional and removable. This is NOT pure coordination (rope) — the suppression and beneficiary asymmetry are core to the mechanism. This IS snare-dominant from the victim perspective (domestic audience, rights bearers) but rope-dominant from the beneficiary perspective (propaganda apparatus, fellow travelers). The mandatrophy is resolved by the perspectival tuple: the snare classification is anchored to the powerless/trapped/domestic perspective; the rope classification is anchored to the institutional/arbitrage/global perspective. Both are correct — they measure different observables at different contexts. The constraint's claimed type (snare) reflects the primary target's experience. The theater ratio (0.85) confirms that the constraint's function is increasingly performative: the catalog is maintained for its symbolic value abroad, not its operational value domestically. This is consistent with snare (extraction of legitimacy) paired with piton (functional atrophy domestically).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    invocability_suppression_mechanism,
    'Is suppression of domestic invocation enforced through explicit legal prohibition, informal institutional pressure, or internalized self-censorship?',
    'Documentation of attempted invocations: court records, administrative proceedings, dissident legal briefs; analysis of outcomes (rejection, delay, retaliation); interviews with domestic lawyers on costs of citation',
    'If explicit legal prohibition: constraint is straightforward snare with transparent coercion. If informal institutional pressure: constraint appears less overtly extractive (theater ratio increases). If internalized self-censorship: suppression is structurally present but appears absent, deepening the facade.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(invocability_suppression_mechanism, empirical, 'Mechanism of suppression of domestic legal invocation').

omega_variable(
    international_audience_uptake,
    'Do international audiences genuinely credit the catalog as evidence of socialist superiority, or do sophisticated foreign observers recognize it as decoupled from domestic enforcement?',
    'Analysis of international press coverage, academic citations, diplomatic usage; comparison of citations pre/post major human rights violations; tracking whether foreign lawyers and policymakers cite the catalog in good faith or as propaganda artifact',
    'If genuinely credited: propaganda apparatus experiences real coordination benefit (rope classification holds). If recognized as facade: propaganda benefit is reduced (constraint approaches pure extraction), and the fellow-travelers'' rope classification weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_audience_uptake, empirical, 'Whether international audiences credit the catalog or recognize it as facade').

omega_variable(
    regime_awareness_of_contradiction,
    'Does the regime explicitly know it is suppressing domestic invocation, or does it rationalize this as ''Western legalism'' inconsistent with socialist goals?',
    'Internal regime documents (if available); regime officials'' public statements distinguishing international from domestic legal frameworks; alignment between official ideology and enforcement patterns',
    'If explicit awareness: regime is consciously deploying the catalog as extractive tool (snare confirmed across regime perspectives). If rationalized: regime may genuinely believe its own framing, reducing intentionality of extraction but not changing structural effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_awareness_of_contradiction, conceptual, 'Whether regime knowingly suppresses invocation or rationalizes it as ideological necessity').

omega_variable(
    reading_foreclosure_condition,
    'Does the external_showcase reading logically foreclose the conditional_clause reading, or can both be held simultaneously?',
    'Logical analysis: If a regime suppresses domestic invocation (showcase reading), does it NECESSARILY include pre-condition clauses (''in conformity with working people''s interests''), or are suppression and explicit pre-conditioning independent mechanisms?',
    'If readings foreclose each other: the constraint has only one interpretation per framework. If coexist: the regime can deploy both suppression AND pre-conditioning as redundant extraction mechanisms, increasing total suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_condition, conceptual, 'Whether external_showcase and conditional_clause readings logically foreclose each other').

omega_variable(
    social_rights_substance_counterfactual,
    'Would domestic enforcement of social guarantees (employment, housing, education) change the classification from snare to tangled_rope or rope?',
    'Counterfactual analysis: assume regime permits domestic invocation of social rights guarantees and enforce them at measurable levels; assess whether genuine coordination function emerges or extraction remains dominant',
    'If coordination emerges: the snare classification depends on suppression; removal of suppression changes the type. If extraction remains: the regime benefits from the guaranteed rights themselves (rents from labor allocation, housing allocation control), not just international credibility, and the snare deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_rights_substance_counterfactual, empirical, 'Whether enforcement of social guarantees would change the constraint type').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rights_catalog_facade__external_showcase_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(righ_tr_t0, rights_catalog_facade__external_showcase_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(righ_tr_t3, rights_catalog_facade__external_showcase_reading, theater_ratio, 3, 0.78).
narrative_ontology:measurement(righ_tr_t6, rights_catalog_facade__external_showcase_reading, theater_ratio, 6, 0.85).

% Extraction over time
narrative_ontology:measurement(righ_be_t0, rights_catalog_facade__external_showcase_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(righ_be_t3, rights_catalog_facade__external_showcase_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(righ_be_t6, rights_catalog_facade__external_showcase_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(righ_su_t0, rights_catalog_facade__external_showcase_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(righ_su_t3, rights_catalog_facade__external_showcase_reading, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(righ_su_t6, rights_catalog_facade__external_showcase_reading, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rights_catalog_facade__external_showcase_reading, information_standard).
narrative_ontology:affects_constraint(rights_catalog_facade__external_showcase_reading, rights_catalog_facade__conditional_clause_reading).
narrative_ontology:affects_constraint(rights_catalog_facade__external_showcase_reading, rights_catalog_facade__social_rights_substance_reading).

% DUAL FORMULATION NOTE:
% The rights_catalog_facade kernel has three distinct readings corresponding to three structurally different constraints. The external_showcase_reading (this file) focuses on suppression of domestic invocation paired with international circulation. The conditional_clause_reading focuses on pre-conditions embedded in the text itself. The social_rights_substance_reading focuses on the actual delivery of social guarantees. These three readings are linked via network.affects_constraints to show they are alternative interpretations of the same kernel. The external_showcase reading emphasizes the FUNCTIONAL suppression mechanism (can't cite domestically), while conditional_clause emphasizes the TEXTUAL pre-conditioning mechanism (can't claim rights beyond 'working people's interests'), and social_rights_substance emphasizes ACTUAL DELIVERY (employment, housing, education are measurably provided). Each reading has its own extractiveness value and its own beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
