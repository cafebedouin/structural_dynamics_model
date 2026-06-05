% ============================================================================
% CONSTRAINT STORY: eighteenth_amendment__enforcement_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eighteenth_amendment__enforcement_collapse_reading, []).

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
 *   constraint_id: eighteenth_amendment__enforcement_collapse_reading
 *   human_readable: Eighteenth Amendment Enforcement Collapse: Volstead's Unobtainable Compliance
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   The Eighteenth Amendment (ratified 1919) declared total prohibition of
 *   alcohol and vested enforcement in the Volstead Act. This constraint
 *   reading instantiates the enforcement-collapse thesis: the declared
 *   suppression target (universal alcohol abstinence) exceeded any achievable
 *   enforcement budget or capacity; juries nullified prosecutions based on
 *   perceived unfairness; the gap between the law's declared authority and
 *   its actual enforcement power taught citizens contempt for law itself.
 *   This is one reading of the Eighteenth Amendment kernel, competing with
 *   the constitutional-overreach reading (Prohibition is the cautionary
 *   lesson about what should not be constitutionalized) and the
 *   organized-crime-externality reading (Prohibition capitalized criminal
 *   enterprise). This reading focuses on the mechanism of credibility
 *   collapse: a law that cannot enforce its declared suppression creates a
 *   structural lesson that laws are negotiable, authority is performative,
 *   and legal compliance is optional. The extractiveness of compliance became
 *   unobtainable because the law declared impossible compliance and then
 *   punished the inevitable non-compliance. The suppression metric
 *   (0.85–0.88) is high because the constraint declares total prohibition,
 *   but the enforceability of that suppression collapsed from 0.70 to 0.88
 *   (increasing impossibility) over the 14-year interval.
 *
 * KEY AGENTS:
 *   - Ordinary Citizens (Drinkers): Powerless/trapped victims — criminalized for widespread social practice; bear extraction risk while compliance is unobtainable
 *   - Local Police: Moderate/constrained actors — mandated to enforce impossible suppression; extract labor from constraint while constraint output (alcohol reduction) fails
 *   - Legal Order (as institutional agent): Organized/constrained — coordinates public health aspiration but extracts credibility from itself through enforcement collapse; teaches contempt for law
 *   - Illicit Alcohol Market: Institutional/arbitrage beneficiary — constraint solves their coordination problem (competition elimination, barrier to entry) without direct extraction cost
 *   - Federal Prohibition Bureaucracy: Institutional/arbitrage — maintains theatrical enforcement activity (raids, arrests) whose real function is to exist, not to achieve prohibition
 *   - Analytical Observer: Civilizational perspective — risks naturalizing the contingent failure as an immutable law of enforcement physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eighteenth_amendment__enforcement_collapse_reading, 0.68).
domain_priors:suppression_score(eighteenth_amendment__enforcement_collapse_reading, 0.85).
domain_priors:theater_ratio(eighteenth_amendment__enforcement_collapse_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eighteenth_amendment__enforcement_collapse_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eighteenth_amendment__enforcement_collapse_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(eighteenth_amendment__enforcement_collapse_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eighteenth_amendment__enforcement_collapse_reading, snare).
narrative_ontology:human_readable(eighteenth_amendment__enforcement_collapse_reading, "Eighteenth Amendment Enforcement Collapse: Volstead's Unobtainable Compliance").
narrative_ontology:topic_domain(eighteenth_amendment__enforcement_collapse_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(eighteenth_amendment__enforcement_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eighteenth_amendment__enforcement_collapse_reading, '14e3b8e8-5068-4374-8ea7-2ba87a602f76').
narrative_ontology:cs_kernel_codification('14e3b8e8-5068-4374-8ea7-2ba87a602f76', formalized).
narrative_ontology:cs_authority_grounding('14e3b8e8-5068-4374-8ea7-2ba87a602f76', lineage).
narrative_ontology:cs_interpretation_layer_present('14e3b8e8-5068-4374-8ea7-2ba87a602f76').
narrative_ontology:cs_reading_relation('14e3b8e8-5068-4374-8ea7-2ba87a602f76', eighteenth_amendment__constitutional_overreach_lesson_reading, coexists_with).
narrative_ontology:cs_reading_relation('14e3b8e8-5068-4374-8ea7-2ba87a602f76', eighteenth_amendment__organized_crime_externality_reading, coexists_with).
narrative_ontology:cs_axiom('14e3b8e8-5068-4374-8ea7-2ba87a602f76', foundational, suppression_target_unobtainable_at_scale).
narrative_ontology:cs_axiom_status(suppression_target_unobtainable_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('14e3b8e8-5068-4374-8ea7-2ba87a602f76', suppression_target_unobtainable_at_scale, empirically_contingent).
narrative_ontology:cs_axiom('14e3b8e8-5068-4374-8ea7-2ba87a602f76', foundational, credibility_loss_teaches_contempt_for_law).
narrative_ontology:cs_axiom_status(credibility_loss_teaches_contempt_for_law, holdable).
narrative_ontology:cs_axiom_grounding('14e3b8e8-5068-4374-8ea7-2ba87a602f76', credibility_loss_teaches_contempt_for_law, instrumental).
narrative_ontology:cs_reference_frame('14e3b8e8-5068-4374-8ea7-2ba87a602f76', eighteenth_amendment_full_enforceability).
narrative_ontology:cs_drift_state('14e3b8e8-5068-4374-8ea7-2ba87a602f76', post_1927_enforcement_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('14e3b8e8-5068-4374-8ea7-2ba87a602f76', '').
narrative_ontology:cs_kernel_id(eighteenth_amendment__enforcement_collapse_reading, eighteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eighteenth_amendment__enforcement_collapse_reading, illicit_alcohol_market).
narrative_ontology:constraint_victim(eighteenth_amendment__enforcement_collapse_reading, legal_order_credibility).
narrative_ontology:constraint_victim(eighteenth_amendment__enforcement_collapse_reading, law_enforcement_capacity).
narrative_ontology:constraint_victim(eighteenth_amendment__enforcement_collapse_reading, jury_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY DRINKER (SNARE) — Trapped by a law that criminalizes widespread social practice. No material exit: drinking is culturally normalized, economically accessible through illicit channels, and compliance is not optional (legal jeopardy). Suppression is maximal — the law creates a binary: comply (abandon social practice) or violate (risk arrest). The drinker bears extraction (criminal risk, blackmail vulnerability, police harassment) while the constraint's function (alcohol prohibition) is unobtainable.
constraint_indexing:constraint_classification(eighteenth_amendment__enforcement_collapse_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL POLICE FORCE (SNARE) — Constrained by a mandate (enforce Volstead) that exceeds any reasonable budget or operational capacity. The constraint extracts enforcement labor while being structurally unobtainable — no amount of police resources could enforce total alcohol prohibition in an urban area where production and distribution are economically rational. Suppression mechanism is coercive (arrest authority) but enforcement target (universal abstinence) is impossible. The police experience extraction (endless, futile enforcement work) with minimal extraction output (the alcohol market thrives).
constraint_indexing:constraint_classification(eighteenth_amendment__enforcement_collapse_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGAL ORDER ITSELF (TANGLED ROPE) — The law coordinates a legitimate public health aspiration (reduce alcohol consumption and its harms) while extracting credibility from the legal system itself. The gap between declared suppression (total prohibition) and achievable suppression (at most 10-20% actual compliance) teaches contempt for law: if the law declares what people widely reject as illegitimate, and the law cannot enforce its own declaration, the law loses authority beyond this constraint. Citizens learn that laws are not binding but negotiable, enforceable only against the weak, and subject to jury nullification. The legal order bears extraction through the asymmetry: it gains no compliance benefit but loses legitimacy in all adjacent domains.
constraint_indexing:constraint_classification(eighteenth_amendment__enforcement_collapse_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ILLICIT ALCOHOL MARKET (ROPE) — Pure coordination: Prohibition creates a market coordination problem (supply and distribution of alcohol) and hands it entirely to criminal organization, which solves it. The market experiences the constraint as a coordination solution (legal competitors are eliminated, barriers to entry are raised by risk and capital requirements), not extraction. The beneficiary's experience is rope — the constraint solves their central coordination problem and imposes minimal direct costs (indirect: police action, violence competition).
constraint_indexing:constraint_classification(eighteenth_amendment__enforcement_collapse_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL PROHIBITION BUREAUCRACY (PITON) — The Volstead Act created a dedicated enforcement apparatus (Bureau of Prohibition, Treasury Department) that persisted through inertia and theatrical activity. The bureau conducted raids, seized operations, and prosecuted cases — generating performance metrics (arrests, confiscations) — while the actual enforcement goal (reducing alcohol consumption) remained unobtainable. Theater ratio is high because the raids and prosecutions are visible, countable activities that demonstrate enforcement effort without producing the declared outcome. The bureaucracy itself becomes the constraint's only real function: it exists to enact the prohibition ritual, not to achieve prohibition.
constraint_indexing:constraint_classification(eighteenth_amendment__enforcement_collapse_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the constraint appears to instantiate a natural law: any law whose suppression target is unobtainable at scale will collapse in enforcement, creating a credibility cascade where citizens learn that the law's authority does not bind their behavior. This perspective risks naturalizing what is actually a contingent institutional failure (Prohibition was politically chosen, its suppression target was declared impossible by empirical observers before ratification, the collapse was foreseeable). The analytical observer may mistake the structural regularity (laws that declare impossible suppression will fail) for an immutable fact rather than a choice.
constraint_indexing:constraint_classification(eighteenth_amendment__enforcement_collapse_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eighteenth_amendment__enforcement_collapse_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eighteenth_amendment__enforcement_collapse_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eighteenth_amendment__enforcement_collapse_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eighteenth_amendment__enforcement_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eighteenth_amendment__enforcement_collapse_reading, TR),
    TR >= 0.70.

:- end_tests(eighteenth_amendment__enforcement_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts in multiple directions — from citizens (criminalization of normal behavior), from police (mandated impossible enforcement), from the legal system itself (credibility loss). The extractiveness is not maximal (0.72+) because the constraint's extractive success is incomplete: it cannot eliminate alcohol consumption, police cannot fully suppress the market, and the legal system retains sufficient legitimacy to eventually repeal the Amendment. The growth from 0.35 to 0.68 reflects that the initial promise of Prohibition was believed to be achievable (extractiveness of compliance seemed possible in 1920); by 1927, the gap between declared and achievable suppression was undeniable, and extractiveness climbed as the constraint taught its lesson of contempt. Suppression (0.85): Very high. Prohibition declares total alcohol abstinence and criminalizes all production, distribution, and consumption. This is the strongest possible suppression declaration. However, the constraint's enforceability collapsed — suppression was declared but not achievable. The suppression metric remains high (not declining) because the law did not weaken; rather, the gap between suppression and enforceability widened. Theater ratio (0.62): Moderate-high. Volstead enforcement was substantially performative — raids were visible, arrests were publicized, the Bureau of Prohibition produced metrics — while actual alcohol consumption remained high or increased. Theater grew from 0.45 to 0.75 as the bureau focused on visible enforcement activity while admitting (internally) that total suppression was impossible. The final theater (0.75) reflects the late-period Bureau's behavior: enforcement theater without enforcement outcome.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why a single indexical context cannot capture Prohibition's constraint structure. The beneficiary's perspective (illicit market) is Rope — they see coordination. The victim's perspective (ordinary drinkers) is Snare — they see extraction and impossibility. The enforcer's perspective (police) is Snare — they see impossible mandates and labor extraction. The institutional perspective (legal order) is Tangled Rope — they coordinate policy while extracting credibility. The ritualist's perspective (prohibition bureaucracy) is Piton — they see theater. The analytical observer risks Mountain — seeing the constraint as an immutable law of enforcement physics. Each perspective is structurally correct within its context; the presheaf over all contexts is the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are determined by each agent's structural position and exit options. Citizens with no exit (trapped) experience maximum d; police with constrained exit experience high d; the legal order experiences moderate-high d (it is partly victim, partly beneficiary of the constraint's coordination function). The illicit market has low d (pure beneficiary with arbitrage capacity). The prohibition bureaucracy has very low d (beneficiary, arbitrage exit available). The analytical observer occupies d ≈ 0.72 (observer position in the chi formula). The engine will compute effective extractiveness χ from these d values: trapped citizens get high χ (extraction felt acutely), constrained police get moderate χ (extraction partially mitigated by agency), the legal order gets moderate-high χ (credibility is extracted while coordination is gained), the illicit market gets negative or very low χ (beneficiary position).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (how can a single law be classified differently by different observers?) by showing that the indexical context determines the classification. The law IS a Snare for the powerless; IS a Tangled Rope for the legal order; IS a Rope for the illicit market. The mandatrophy would arise if we tried to declare a single type that applies everywhere — the constraint forces us to accept perspectival multiplicity. The analytical observer's temptation to call it a Mountain (a law of enforcement physics) is a false summit: the constraint is a choice, not a law of nature. The other sibling readings (constitutional overreach, organized crime externality) each resolve the mandatrophy by selecting different aspects of the constraint structure to privilege — the overreach reading focuses on the constitutional-doctrinal issue (what should be constitutionalized); the crime reading focuses on the economic externality (who benefits). This reading focuses on the enforcement collapse and its epistemological consequence (teaching contempt for law).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_threshold_ambiguity,
    'What compliance rate constitutes enforcement success vs. failure? At what point does the gap between declared suppression and achievable suppression transform enforcement from authority into theater?',
    'Comparison with other failed prohibition regimes (drug war, gambling bans, prostitution statutes) to identify whether there is a critical threshold (20%, 30%, 50% non-compliance) below which citizen respect for the law degrades measurably, or whether the cascade is continuous',
    'If threshold exists: Volstead crossed it deterministically, teaching contempt. If continuous: the contempt teaches depends on pre-existing citizen investment in legal legitimacy. The reading''s extractiveness value depends on this — high threshold = higher ε (more contempt taught per unit non-compliance), low threshold = lower ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_threshold_ambiguity, empirical, 'Compliance threshold determining enforcement success vs. failure').

omega_variable(
    jury_nullification_exogeneity,
    'Did jury nullification of Volstead prosecutions originate from the law''s perceived unfairness (endogenous to the constraint) or from cultural norms about alcohol that pre-existed the Amendment (exogenous, pre-legal)?',
    'Historical analysis of jury instructions, jury deliberation records, and nullification rates across regions with different cultural attitudes toward alcohol; comparison with pre-Volstead nullification rates on alcohol-related charges',
    'If endogenous: the constraint itself taught contempt for law (snare reading confirmed). If exogenous: the nullification reflects pre-legal culture, and the constraint merely collided with it (reframes as a measurement of cultural-legal misalignment, not enforcement collapse teaching contempt). If mixed: extractiveness splits between the constraint''s own effect and the pre-legal component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jury_nullification_exogeneity, empirical, 'Whether jury nullification was taught by Volstead or pre-existed it').

omega_variable(
    credibility_cascade_mechanism,
    'Does legal order credibility degrade through a cascade (loss in Volstead undermines faith in all adjacent law) or through compartmentalization (citizens treat Volstead as a special case, do not generalize to other laws)?',
    'Longitudinal analysis of crime rates, prosecution rates, and regulatory compliance in non-alcohol domains before, during, and after Prohibition; survey data on citizen trust in law across periods; correlation between alcohol-specific nullification and nullification rates in unrelated prosecutions',
    'If cascade: Prohibition teaches contempt for law itself, and extractiveness of compliance is unobtainable across the legal system. If compartmentalized: the damage is contained to alcohol and related enforcement, and other laws retain legitimacy. The reading''s claim about ''teaching contempt for law itself'' depends on cascade, not compartmentalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_cascade_mechanism, empirical, 'Whether legal credibility loss cascades or compartmentalizes').

omega_variable(
    kernel_reading_contest,
    'What is the relationship between this enforcement-collapse reading and its sibling readings of the Eighteenth Amendment kernel?',
    'Structural analysis of how each reading defines the Amendment''s failure: enforcement collapse (this reading) vs. constitutional overreach (sibling 1) vs. organized crime externality (sibling 2). Do these readings agree on what happened but disagree on what it teaches, or do they describe fundamentally different mechanisms?',
    'This is a conceptual/theoretical omega. The reading_relations in cs_structure are assertions about this question. The resolution mechanism is philosophical-historical exegesis, not empirical data gathering.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest: enforcement collapse vs. constitutional overreach vs. organized crime externality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eighteenth_amendment__enforcement_collapse_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vol_theater_1920, eighteenth_amendment__enforcement_collapse_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(vol_theater_1927, eighteenth_amendment__enforcement_collapse_reading, theater_ratio, 7, 0.62).
narrative_ontology:measurement(vol_theater_1933_repeal, eighteenth_amendment__enforcement_collapse_reading, theater_ratio, 14, 0.75).

% Extraction over time
narrative_ontology:measurement(vol_extractiveness_1920, eighteenth_amendment__enforcement_collapse_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vol_extractiveness_1927, eighteenth_amendment__enforcement_collapse_reading, base_extractiveness, 7, 0.68).
narrative_ontology:measurement(vol_extractiveness_1933_repeal, eighteenth_amendment__enforcement_collapse_reading, base_extractiveness, 14, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(vol_suppression_1920, eighteenth_amendment__enforcement_collapse_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vol_suppression_1927, eighteenth_amendment__enforcement_collapse_reading, suppression_requirement, 7, 0.85).
narrative_ontology:measurement(vol_suppression_1933_repeal, eighteenth_amendment__enforcement_collapse_reading, suppression_requirement, 14, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eighteenth_amendment__enforcement_collapse_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eighteenth_amendment__enforcement_collapse_reading, drug_war_enforcement_collapse).
narrative_ontology:affects_constraint(eighteenth_amendment__enforcement_collapse_reading, civil_asset_forfeiture_credibility_extraction).

% DUAL FORMULATION NOTE:
% The Eighteenth Amendment kernel decomposes into three structurally distinct constraint stories: this enforcement-collapse reading (ε≈0.68, focuses on credibility cascade and contempt for law), the constitutional-overreach reading (ε≈0.45, focuses on the doctrinal mistake of constitutionalizing policy), and the organized-crime-externality reading (ε≈0.52, focuses on market-handed-to-criminal-organization). Each reading has different beneficiary/victim sets and different pedagogical consequences. They are linked through the shared kernel (Eighteenth Amendment text) but represent different structural mechanisms and different observers' analytical priorities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
