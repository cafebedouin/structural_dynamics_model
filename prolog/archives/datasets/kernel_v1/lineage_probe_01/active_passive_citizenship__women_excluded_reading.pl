% ============================================================================
% CONSTRAINT STORY: active_passive_citizenship__women_excluded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_active_passive_citizenship_women_excluded_reading, []).

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
 *   constraint_id: active_passive_citizenship__women_excluded_reading
 *   human_readable: Active-Passive Citizenship: Women Excluded by Constitutional Design
 *   domain: political/doctrinal
 *
 * SUMMARY:
 *   The 1791 French Constitution created two categories of citizenship:
 *   active citizens (property-holding males who could vote and hold office)
 *   and passive citizens (all other taxpaying subjects, including women).
 *   This was presented as a refinement of universal rights — distinguishing
 *   those with the political capacity to judge from those with the civic
 *   stake to support government. However, the constitution's treatment of
 *   women reveals that the split was not merely about property or capacity:
 *   women were excluded from BOTH categories by sex. A woman with significant
 *   property could not be an active citizen; a woman with education and stake
 *   could not be a passive citizen in any sense that granted her voice.
 *   Olympe de Gouges's 'Declaration of the Rights of Woman and of the Female
 *   Citizen' (1791) exposed this contradiction by mirroring the
 *   Constitution's own structure while substituting 'woman' for 'man,'
 *   demonstrating that the Constitution's universalism was false and the
 *   exclusion was deliberate. De Gouges was executed in 1793, becoming the
 *   exemplary victim of the constraint she named. Her death marked the
 *   boundary the Revolutionary male leadership would defend: women's
 *   exclusion from civic existence was non-negotiable, even at the cost of
 *   eliminating those who articulated it.
 *
 * KEY AGENTS:
 *   - Women across property classes: Primary victims (powerless/trapped/national) — excluded from both citizenship categories by constitutional design regardless of property or capacity; de Gouges exemplifies the cost of articulating the exclusion.
 *   - Male property-holding leadership: Primary beneficiaries (institutional/arbitrage/national) — consolidated monopoly on political judgment and voting; maintained sole control over the meaning of universal rights.
 *   - Revolutionary male framers: Institutional beneficiaries (institutional/arbitrage/immediate) — designed the constitutional split; explicitly or implicitly chose sex-based exclusion as non-negotiable.
 *   - Olympe de Gouges and feminist activists: Organized victims (organized/constrained/national) — named the exclusion explicitly; articulated women's own rights claims; faced execution for this articulation.
 *   - Women with property: Moderately constrained victims (moderate/constrained/national) — property that might have qualified them under property-logic was negated by sex-based exclusion; experienced the contradiction of being property-holders without civic standing.
 *   - Constitutional authority structure: Institutional maintainer (institutional/arbitrage/national) — preserves the text and conventional interpretation that encodes women's exclusion; authority persists through interpretive tradition after direct enforcement becomes less rhetorically active.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(active_passive_citizenship__women_excluded_reading, 0.68).
domain_priors:suppression_score(active_passive_citizenship__women_excluded_reading, 0.85).
domain_priors:theater_ratio(active_passive_citizenship__women_excluded_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(active_passive_citizenship__women_excluded_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(active_passive_citizenship__women_excluded_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(active_passive_citizenship__women_excluded_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(active_passive_citizenship__women_excluded_reading, snare).
narrative_ontology:human_readable(active_passive_citizenship__women_excluded_reading, "Active-Passive Citizenship: Women Excluded by Constitutional Design").
narrative_ontology:topic_domain(active_passive_citizenship__women_excluded_reading, "political/doctrinal").

domain_priors:requires_active_enforcement(active_passive_citizenship__women_excluded_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(active_passive_citizenship__women_excluded_reading, '0bf10c97-e150-449d-8308-f7537b9170a8').
narrative_ontology:cs_kernel_codification('0bf10c97-e150-449d-8308-f7537b9170a8', formalized).
narrative_ontology:cs_authority_grounding('0bf10c97-e150-449d-8308-f7537b9170a8', lineage).
narrative_ontology:cs_interpretation_layer_present('0bf10c97-e150-449d-8308-f7537b9170a8').
narrative_ontology:cs_reading_relation('0bf10c97-e150-449d-8308-f7537b9170a8', active_passive_citizenship__exclusion_of_the_poor_reading, coexists_with).
narrative_ontology:cs_reading_relation('0bf10c97-e150-449d-8308-f7537b9170a8', active_passive_citizenship__property_franchise_logic_reading, forecloses).
narrative_ontology:cs_axiom('0bf10c97-e150-449d-8308-f7537b9170a8', foundational, women_are_full_civic_agents).
narrative_ontology:cs_axiom_status(women_are_full_civic_agents, holdable).
narrative_ontology:cs_axiom_grounding('0bf10c97-e150-449d-8308-f7537b9170a8', women_are_full_civic_agents, deontological).
narrative_ontology:cs_axiom('0bf10c97-e150-449d-8308-f7537b9170a8', foundational, sex_based_exclusion_violates_universality).
narrative_ontology:cs_axiom_status(sex_based_exclusion_violates_universality, holdable).
narrative_ontology:cs_axiom_grounding('0bf10c97-e150-449d-8308-f7537b9170a8', sex_based_exclusion_violates_universality, deontological).
narrative_ontology:cs_reference_frame('0bf10c97-e150-449d-8308-f7537b9170a8', universal_rights_declaration).
narrative_ontology:cs_drift_state('0bf10c97-e150-449d-8308-f7537b9170a8', terror_enforcement_1793, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('0bf10c97-e150-449d-8308-f7537b9170a8', '').
narrative_ontology:cs_kernel_id(active_passive_citizenship__women_excluded_reading, active_passive_citizenship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(active_passive_citizenship__women_excluded_reading, male_property_holders).
narrative_ontology:constraint_beneficiary(active_passive_citizenship__women_excluded_reading, revolutionary_male_leadership).
narrative_ontology:constraint_victim(active_passive_citizenship__women_excluded_reading, women_across_all_property_classes).
narrative_ontology:constraint_victim(active_passive_citizenship__women_excluded_reading, female_civic_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN CIVIC VICTIMS (SNARE) — Structurally excluded from both active and passive citizenship regardless of property, education, or capacity. No exit option exists within the constitutional framework. Maximum extraction: civic existence as such is denied by sex. This is the reading de Gouges articulated and for which she was executed.
constraint_indexing:constraint_classification(active_passive_citizenship__women_excluded_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MALE PROPERTY HOLDERS (ROPE) — Experience the constitutional split as coordination: separating active and passive citizenship creates a dual membership system that consolidates male political monopoly while maintaining a second class of subjects who can be taxed and governed but not consulted. Beneficiary position with high arbitrage: can interpret the framework, adjust the property threshold, or modify the passive-citizen category without risk of losing structural advantage.
constraint_indexing:constraint_classification(active_passive_citizenship__women_excluded_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FEMINIST ACTIVISTS (TANGLED ROPE) — Organized women articulating their own exclusion (de Gouges, Condorcet, petitioners) experience the constraint as partly coordination (women seeking recognition within the framework's own logic of universal rights) and partly pure extraction (the framework rejects the premise of women's claim). Constrained: can petition and write, but execution demonstrates the cost of sustained pressure. Generate coordination function through explicit articulation of rights claims that expose the constitutional contradiction.
constraint_indexing:constraint_classification(active_passive_citizenship__women_excluded_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WOMEN WITH PROPERTY (SCAFFOLD) — Even wealthy women experience the constraint as structural exclusion, but with a temporal and relational angle: their property could theoretically qualify them under the active-citizen property threshold, yet sex forecloses this path. The scaffold element appears in the generational perspective: the next generation may reinterpret the property criterion to include women, creating a sunset pathway. Theater is moderate because the exclusion is textual and explicit — less performative theater than in systems that claim to include but structurally bar.
constraint_indexing:constraint_classification(active_passive_citizenship__women_excluded_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL AUTHORITY (PITON) — The formal mechanism for maintaining the women's exclusion (textual specificity: 'citizens' construed as male by convention, no explicit mention required) persists through institutional inertia and interpretive tradition. The framers' intent to exclude is clear but no longer articulated after the Terror; the exclusion maintains itself through the authority of the text without active enforcement rhetoric. Theater ratio reflects the shift from explicit exclusion to inherited convention — the constraint is performed as natural rather than actively defended.
constraint_indexing:constraint_classification(active_passive_citizenship__women_excluded_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scope, this perspective risks reading the constitutional exclusion of women as reflecting natural or inevitable limits on political capacity — the framing of women's nature as incompatible with civic judgment. However, the empirical reality (women's own articulation of their exclusion, feminist activism, subsequent electoral inclusion without civilizational collapse) contradicts the mountain classification. This perspective instantiates a false summit: the natural-law framing naturalizes what is a deliberate constitutional choice.
constraint_indexing:constraint_classification(active_passive_citizenship__women_excluded_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(active_passive_citizenship__women_excluded_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(active_passive_citizenship__women_excluded_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(active_passive_citizenship__women_excluded_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(active_passive_citizenship__women_excluded_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(active_passive_citizenship__women_excluded_reading, TR),
    TR >= 0.70.

:- end_tests(active_passive_citizenship__women_excluded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts civic existence itself — women cannot vote, hold office, petition as a civic body, or participate in the public deliberation of matters affecting their lives and property. This is not merely a barrier to office (which would be moderate extraction) but a structural denial of civic agency across both citizenship categories. The measurement trajectory (0.60 → 0.68 → 0.72) shows escalating extraction as the Terror intensifies the enforcement of the constitutional exclusion and executes those who name it (de Gouges). Suppression (0.85): Very high. The exclusion is enforced through legal mechanism (constitutional text), conventional interpretation (citizenship as male by default), social enforcement (ridicule of women's political claims), and violent suppression (execution of the most articulate critic). The trajectory (0.75 → 0.82 → 0.85) reflects increasing enforcement intensity as feminist articulation grows and the male leadership responds with greater repression. Theater ratio (0.55, declining from 0.75): Moderate and declining. The exclusion is initially more theatrical (justified through elaborate arguments about women's nature, capacity, and domestic role) but becomes less theatrical over time as it settles into constitutional authority and convention — the exclusion requires less rhetorical justification once it is codified and the most articulate critics are eliminated. The declining theater reflects the shift from active defense to inherited authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full range of classification across perspectives. The women victims see pure extraction (Snare) — they are excluded from civic existence with no exit option within the constitutional framework. The male beneficiaries see coordination (Rope) — they are solving the legitimate problem of how to distinguish those with political capacity from those with civic stake, while maintaining their monopoly on judgment. Feminist activists see a mixed constraint (Tangled Rope) — they articulate a genuine coordination function (women deserve to participate in the democracy that claims universal rights) alongside the asymmetric extraction (they are executed for this articulation). Female property-holders experience a temporary barrier (Scaffold) — property qualifications create a logical pathway for eventual inclusion across sex lines. The constitutional authority sees a degraded ritual (Piton) — the explicit textual exclusion of women is no longer actively defended after the Terror but persists through convention. The analytical observer risks seeing natural law (Mountain) — women's exclusion as reflecting natural limits on women's political capacity — but this is a false summit: de Gouges and subsequent electoral inclusion of women without civilizational collapse demonstrates the exclusion is constitutional choice, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's position on the extraction flow is determined by structural relationship to the women's-exclusion mechanism. Male property holders benefit from women's exclusion because it consolidates their monopoly on political judgment — they experience low effective extraction (Rope, arbitrage) because they are positioned as beneficiaries. Women are positioned as victims: they experience maximum extraction because they cannot exit the framework and receive zero civic benefit. Feminist activists are organized but constrained — they can articulate claims (hence organized rather than powerless) but face execution for doing so (hence constrained rather than mobile). The constitutional authority benefits from maintaining the text (arbitrage position) but sees its own enforcement mechanism as degraded (piton). The analytical observer can see the full structure (analytical exit option) but risks naturalizing it (false summit trap). Directionality values: male beneficiaries d ≈ 0.10 (beneficiary + arbitrage); women victims d ≈ 0.95 (victim + trapped); feminist activists d ≈ 0.55 (victim + organized); analytical d ≈ 0.72 (observer).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is high-extractiveness (0.68, approaching the 0.70 mandatrophy gate), and it resolves the mandatrophy through explicit committer-frame analysis. The question 'Is the women's exclusion a legitimate constitutional design (coordination) or illegitimate extraction?' cannot be answered from within the Constitution itself — it depends on whether the reading's foundational axiom (women_are_full_civic_agents) is accepted. The male leadership axiom (women_unsuited_for_civic_judgment) contradicts the feminist axiom, and the Constitution's authority enforces the male reading. The mandatrophy resolves not by discovering the 'true' type but by documenting that the same constraint appears as coordination (rope: beneficiary view) and extraction (snare: victim view) depending on axiom acceptance. De Gouges's execution marks the point where the male leadership's rejection of the women's axiom becomes violent enforcement. Mandatrophy resolution: The constraint is a snare to those it excludes and a rope to those who benefit. Both perspectives are structurally accurate. The political question is which axiom should ground the constraint's legitimacy going forward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constitutional_exclusion,
    'Is the exclusion of women from active-passive citizenship a natural law (women''s nature unsuited to political judgment) or a constitutional design choice by male framers?',
    'Historical textual analysis: examine whether the constitutional documents frame women''s exclusion as natural/inevitable vs. explicitly chosen; cross-reference with contemporary feminist responses (de Gouges, Condorcet, petitions) that name exclusion as artificial; comparison with post-1791 constitutional revisions that include women without reported civilizational harm.',
    'If natural: mountain classification confirmed. If constitutional: false-summit signature fires, reclassifying to snare. The historical record strongly supports constitutional choice, not natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constitutional_exclusion, empirical, 'Whether women''s exclusion is natural or constitutionally engineered').

omega_variable(
    extraction_vs_protection_framing,
    'Was the exclusion of women from civic duties experienced by contemporaries as protection (paternalism) or extraction (denial of rights)?',
    'Textual analysis of contemporary women''s own accounts (de Gouges, women''s petitions, memoirs) and male framers'' publicly stated rationales; comparison of rhetoric about women''s exclusion vs. men''s property-based inclusion to identify whether protection or extraction language dominates.',
    'If protection framing dominated: extractiveness may be lower than 0.68 (beneficiaries see themselves as protecting rather than extracting). If extraction framing dominates or women''s own accounts frame as extraction: current 0.68 confirmed. Historical sources indicate extraction framing in women''s accounts and implicit extraction in framers'' logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_protection_framing, empirical, 'Whether exclusion was framed as protection or extraction').

omega_variable(
    reading_foreclosure_between_gender_and_property,
    'Does the property-franchise-logic reading foreclose or coexist with the women-excluded reading?',
    'Examine whether the property threshold was a universal principle applied consistently (coexists: women excluded for sex, poor excluded for property — two separate logics) or whether property logic was invoked selectively to justify women''s exclusion when property could have theoretically qualified them (forecloses: property exclusion makes sex-based exclusion redundant and contradictory).',
    'If forecloses: women-excluded reading and property-franchise reading are mutually exclusive — the property principle cannot consistently exclude both the poor AND wealthy women. If coexists: both exclusions operate as separate structural mechanisms within a single framework. Historical evidence suggests coexistence with friction: wealthy women''s property ownership created a cognitive dissonance the Constitution never resolved, fueling subsequent feminist articulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_between_gender_and_property, conceptual, 'Whether property and sex-based exclusions logically contradict or coexist').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is the suppression of women''s political claim internalized (cognitive, identity-based) vs. structural (legal, enforcement-based)?',
    'Analyze post-1791 enforcement patterns: which legal mechanisms actively suppressed women''s civic organizing vs. which relied on social convention and internalized gender norms? Compare suppression intensity in regions with stronger feminist organizing (Paris) vs. weaker (rural areas) to assess whether enforcement or internalization was primary.',
    'If primarily structural: suppression metric (0.85) is accurate as-is. If partially internalized: suppression may be sustained by women''s own internalized beliefs about unfitness, meaning the constraint''s effective binding is partly cognitive. This would suggest identity_locked exit options for some perspectives, not purely trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized gender norms').

omega_variable(
    kernel_reading_contest_positioning,
    'What is the structural relationship between this reading (women excluded) and the property-franchise-logic reading (property as the basis for citizenship distinction)?',
    'This omega routes the committer-frame structure (Rule 2) through the analytical apparatus. The property-franchise reading claims a universal principle: property stake ensures independent judgment. Does this principle apply to women with property? If yes, the women-excluded reading forecloses it (property principle cannot hold universally while women are excluded). If no, the readings coexist (property is one criterion, sex is another). The Constitution''s text excludes women explicitly; this reading''s authority lies in naming that exclusion.',
    'Classification of the relationship (forecloses, coexists, influences) determines the conceptual coherence of the Constitution''s own framework. A foreclosure finding reveals internal logical contradiction. Coexistence reveals the Constitution as a patchwork of separate exclusionary logics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_positioning, conceptual, 'Structural relationship between women-excluded and property-franchise readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(active_passive_citizenship__women_excluded_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acti_tr_t0, active_passive_citizenship__women_excluded_reading, theater_ratio, 0, 0.75).
narrative_ontology:measurement(acti_tr_t3, active_passive_citizenship__women_excluded_reading, theater_ratio, 3, 0.65).
narrative_ontology:measurement(acti_tr_t6, active_passive_citizenship__women_excluded_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(acti_be_t0, active_passive_citizenship__women_excluded_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(acti_be_t3, active_passive_citizenship__women_excluded_reading, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(acti_be_t6, active_passive_citizenship__women_excluded_reading, base_extractiveness, 6, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(acti_su_t0, active_passive_citizenship__women_excluded_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(acti_su_t3, active_passive_citizenship__women_excluded_reading, suppression_requirement, 3, 0.82).
narrative_ontology:measurement(acti_su_t6, active_passive_citizenship__women_excluded_reading, suppression_requirement, 6, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(active_passive_citizenship__women_excluded_reading, identity_coordination).
narrative_ontology:affects_constraint(active_passive_citizenship__women_excluded_reading, active_passive_citizenship__exclusion_of_the_poor_reading).
narrative_ontology:affects_constraint(active_passive_citizenship__women_excluded_reading, active_passive_citizenship__property_franchise_logic_reading).

% DUAL FORMULATION NOTE:
% The 'active_passive_citizenship' kernel has three structurally distinct readings with different extractiveness values: (1) women_excluded_reading (ε=0.68, this story) — sex-based categorical exclusion; (2) exclusion_of_the_poor_reading (ε=0.62 estimated) — property-based stratification among men; (3) property_franchise_logic_reading (ε=0.40 estimated) — coherent property criterion for distinguishing active/passive citizens. Each reading emphasizes different aspects of the same constitutional text. The women_excluded_reading has higher extractiveness than the property-logic reading because sex-based exclusion is total and cross-property, while property logic at least provides a coherent principle. All three readings are interconnected through the kernel; they cannot all be true simultaneously in a single interpretive framework, and they compete for authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(active_passive_citizenship__women_excluded_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
