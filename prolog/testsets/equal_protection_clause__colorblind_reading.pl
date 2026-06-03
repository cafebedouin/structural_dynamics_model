% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__colorblind_reading, []).

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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Colorblind Reading: Absolute Race Neutrality Doctrine
 *   domain: constitutional_law/civil_rights/educational_policy
 *
 * SUMMARY:
 *   The colorblind reading of the Equal Protection Clause holds that the
 *   Fourteenth Amendment requires absolute race neutrality in state action —
 *   that any racial classification is presumptively unconstitutional
 *   regardless of remedial intent. This constraint is one reading of a
 *   contested constitutional kernel: the meaning and scope of 'equal
 *   protection of the laws.' The reading competes with two sibling
 *   interpretations: the remedial reading, which permits race-conscious
 *   remedies to dismantle systemic discrimination and achieve substantive
 *   equality, and the diversity reading, which permits limited race
 *   consideration as one factor in holistic educational review when diversity
 *   serves compelling institutional interests. The colorblind reading
 *   naturalizes a particular interpretive choice as constitutional law —
 *   elevating formal equality (identical treatment) over substantive equality
 *   (equal outcomes) and treating race-consciousness itself as the
 *   constitutional harm. This creates a striking structural pattern: the
 *   reading provides genuine coordination benefits (clear rules for
 *   compliance, predictability in law) while simultaneously extracting from
 *   those who depend on race-conscious remedies (universities, affirmative
 *   action beneficiaries, remedial advocates). The constraint exhibits all
 *   the hallmarks of a false summit: it appears as neutral interpretation of
 *   constitutional text, but identifiable beneficiaries exist (white and
 *   Asian applicants, formal equality advocates), and the suppression
 *   mechanism (litigation threat, regulatory guidance, doctrine
 *   establishment) is clearly active and enforced. The temporal measurements
 *   show accumulating extraction: as the doctrine becomes established
 *   (Adarand 1995 → Parents Involved 2007 → Students for Fair Admissions
 *   2023), the suppression requirement rises (enforcement cost to
 *   universities adopts race-consciousness rises) and the extractiveness
 *   increases (the gap between stated principle and practical effect widens
 *   as documented racial inequality persists despite colorblind rules).
 *
 * KEY AGENTS:
 *   - Formal Equality Advocates (institutional/arbitrage): Beneficiary. Legal scholars, judges, conservative political coalitions who hold that equal protection requires identical treatment regardless of race. The colorblind reading operationalizes their philosophical framework as binding constitutional doctrine.
 *   - White and Asian Applicants (powerful/arbitrage): Beneficiary. Applicants who benefit from removal of race-conscious consideration while retaining access to other advantage-preserving mechanisms (legacy preferences, development admits, geographic diversity). Advantaged by the reading's naturalization of baseline position.
 *   - Affirmative Action Beneficiaries — Black/Latino Students (powerless/trapped): Victim. Students who have historically relied on race-conscious admissions policies as institutional pathways to higher education in segregated housing and K-12 systems. The colorblind reading transforms the primary tool of remedy into a constitutional violation.
 *   - Race-Conscious Universities (organized/constrained): Victim. Institutions that use race-conscious admissions to achieve demographic diversity, manage social cohesion, and serve their educational missions. The reading suppresses their primary tool through litigation threat and doctrine. Also see coordination function: clearer rules enable some compliance benefits.
 *   - Anti-Subordination/Remedial Advocates (organized/constrained): Victim. Constitutional scholars, civil rights organizations, and justices who view equal protection through the lens of dismantling racial hierarchy and historical discrimination. The colorblind reading forecloses their primary theoretical framework and remedy toolset.
 *   - The State (institutional/constrained): Governmental actor. Benefits from simplified equal protection compliance (fewer exceptions, clearer rules) but loses remedial authority to address documented group-level subordination. Generational constraint: constitutional doctrine is durable once established.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.38).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.62).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Colorblind Reading: Absolute Race Neutrality Doctrine").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/civil_rights/educational_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, 'c88d7746-3336-4907-84f7-2077bc4d48f6').
narrative_ontology:cs_kernel_codification('c88d7746-3336-4907-84f7-2077bc4d48f6', fixed_text).
narrative_ontology:cs_authority_grounding('c88d7746-3336-4907-84f7-2077bc4d48f6', lineage).
narrative_ontology:cs_interpretation_layer_present('c88d7746-3336-4907-84f7-2077bc4d48f6').
narrative_ontology:cs_reading_relation('c88d7746-3336-4907-84f7-2077bc4d48f6', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('c88d7746-3336-4907-84f7-2077bc4d48f6', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('c88d7746-3336-4907-84f7-2077bc4d48f6', foundational, formal_equality_doctrine).
narrative_ontology:cs_axiom_status(formal_equality_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('c88d7746-3336-4907-84f7-2077bc4d48f6', formal_equality_doctrine, deontological).
narrative_ontology:cs_axiom('c88d7746-3336-4907-84f7-2077bc4d48f6', foundational, race_classification_inherent_harm).
narrative_ontology:cs_axiom_status(race_classification_inherent_harm, holdable).
narrative_ontology:cs_axiom_grounding('c88d7746-3336-4907-84f7-2077bc4d48f6', race_classification_inherent_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('c88d7746-3336-4907-84f7-2077bc4d48f6', colorblind_constitutional_baseline).
narrative_ontology:cs_drift_state('c88d7746-3336-4907-84f7-2077bc4d48f6', contemporary_persistent_racial_stratification, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c88d7746-3336-4907-84f7-2077bc4d48f6', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, formal_equality_advocates).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, white_asian_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, race_conscious_universities).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, affirmative_action_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFIRMATIVE ACTION BENEFICIARIES (SNARE) — Trapped. The colorblind reading transforms race-conscious admissions policies into constitutional violations, eliminating the institutional pathway that has historically increased access. Exit is impossible: cannot exit racial identity; cannot exit the constraint's application to educational gatekeeping. Maximum experienced extraction — no alternatives visible from within the trapped position. This reading defines their institutional means of remedy as illegitimate.
constraint_indexing:constraint_classification(equal_protection_clause__colorblind_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RACE-CONSCIOUS UNIVERSITIES (TANGLED ROPE) — Organized but constrained. Universities benefit from the coordination function of affirmative action (serving a diverse student body, managing social cohesion, accessing federal funding tied to diversity metrics). But the colorblind reading suppresses their primary tool for achieving these benefits and exposes them to litigation and enforcement pressure. Constrained exit: can technically adopt race-neutral admissions but lose coordination function; litigation creates high cost to continued race-consciousness. Mixed extraction and coordination function.
constraint_indexing:constraint_classification(equal_protection_clause__colorblind_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WHITE AND ASIAN APPLICANTS IN ADVANTAGED POSITION (ROPE) — Powerful with arbitrage options. The colorblind reading removes one barrier to admission (race-conscious consideration) while leaving intact structural advantages (generational wealth, social capital, access to elite prep schools). Arbitrage: can also benefit from other admissions pathways (legacy preferences, development admits, geographic diversity) or pursue alternative educational routes. This reading experiences the constraint as pure coordination: clarifying the legal rules enables better access prediction and portfolio management. Net beneficiary.
constraint_indexing:constraint_classification(equal_protection_clause__colorblind_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FORMAL EQUALITY ADVOCATES (ROPE) — Institutional beneficiary with arbitrage options. This reading aligns with their core normative commitment: that equal protection means identical legal treatment regardless of race. The constraint's enforcement through judicial doctrine, policy guidance, and litigation threat is the mechanism through which their philosophical framework becomes operative law. They experience this as successful coordination: the legal system is now aligned with their conception of equality. Can arbitrage between judicial venues, regulatory bodies, and litigation strategy.
constraint_indexing:constraint_classification(equal_protection_clause__colorblind_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANTI-SUBORDINATION/REMEDIAL ADVOCATES (TANGLED ROPE) — Organized but generationally constrained. This perspective recognizes that the colorblind reading blocks structural remedies for documented historical discrimination. Exit is constrained: they can litigate alternative theories (disparate impact, constitutional amendment, legislative action) but the colorblind reading's dominance in doctrine makes these pathways costly and slow. This reading provides both a coordination function (establishing formal equality rules) and extracts from their remedial toolkit — suppression of race-conscious remedies that they see as necessary to dismantle subordination.
constraint_indexing:constraint_classification(equal_protection_clause__colorblind_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE STATE (CONSTITUTIONAL AUTHORITY) (TANGLED ROPE) — Institutional actor with constrained options. The colorblind reading benefits the state by simplifying equal protection compliance (fewer exceptions, clearer rules) and aligns with particular political coalitions' preferences. But it also constrains the state's options for addressing documented group-level subordination through targeted remedies. The constraint provides coordination (clear doctrine) and extraction (narrowed remedial authority). Generationally constrained because constitutional doctrine is durable once established; civilizational scope because doctrine shapes centuries of case law.
constraint_indexing:constraint_classification(equal_protection_clause__colorblind_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT CANDIDATE) — From a universal/civilizational analytical position, this reading risks appearing as a natural interpretation of the Fourteenth Amendment's text: 'equal protection of the laws' could mean identical treatment is required, independent of purpose or effect. But this mountain classification is a false summit — the analytical observer must note that the colorblind reading naturalizes a contested interpretive choice (formal equality over substantive equality) and a political allocation (beneficiaries and victims are clearly identifiable). The engine will flag this as an FSM candidate via the beneficiary declarations.
constraint_indexing:constraint_classification(equal_protection_clause__colorblind_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_clause__colorblind_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_clause__colorblind_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The colorblind reading creates identifiable extraction: white and Asian applicants gain admission advantage; formal equality advocates gain doctrinal victory; universities and affirmative action beneficiaries lose autonomy and access. But the extraction is not maximal (snare-level 0.66+) because: (a) the reading has genuine coordination benefits (clear rules reduce uncertainty), (b) alternative remedial pathways remain legally open (albeit at higher cost: litigation, legislative action, university discretion within remaining bounds), and (c) the constitutional text does provide plausible support for the interpretation (not pure fabrication). The reading's strength within constitutional doctrine — it is now binding in Students for Fair Admissions (2023) — suggests substantial real-world extraction, but measured against the tangled_rope gate (requires beneficiaries AND victims AND enforcement), the mixed coordination-extraction hybrid fits. Suppression (0.62): Moderate-high. Universities and remedial advocates face material suppression: litigation costs, regulatory investigation, political pressure, doctrine-based foreclosure of race-conscious tools. The suppression is not total (snare-level 0.60+) because: (a) universities can partially exit via race-neutral socioeconomic preferences, percent-plans, and class-based affirmative action, (b) remedial advocates can pursue constitutional amendment or legislative action (costly but possible), (c) some residual race-conscious tools remain (targeted recruitment, holistic review of lived experience without explicit race data). Theater ratio (0.55): Moderate. The colorblind reading has genuine functional content (it clarifies a legal rule, enables compliance prediction), not pure performance. But there is also theatrical dimension: the framing as neutral/colorblind obscures the beneficiaries and distributional effects — the rule appears universal but benefits specific groups. The theater increases over time (0.48 → 0.55) as the doctrine becomes established and the gap between stated neutrality and observed stratification widens.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps here are dramatic and structural. Affirmative action beneficiaries see a snare (trapped, no exit, pure extraction). Race-conscious universities see tangled rope (mixed coordination and extraction, constrained exit, litigation suppression). White/Asian applicants see rope (coordination, arbitrage options, net benefit). Formal equality advocates see rope (coordination, beneficiary, institutional arbitrage). Remedial advocates see tangled rope (blocked remedial tools, constrained exit, mixed with coordination function). The state sees tangled rope (simplified compliance coordination, but lost remedial authority). The analytical observer risks seeing a mountain (neutral interpretation of constitutional text) but the structural data contradicts this — clear beneficiaries, identifiable victims, active suppression mechanism. The engine will compute this as a false summit. The gap between snake (apparent neutrality) and beneficiary presence is diagnostic: the constraint naturalizes a contingent interpretive choice, presenting it as constitutional inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: beneficiary vs victim, power level, exit options. Formal equality advocates are beneficiaries with institutional power and arbitrage options (can move between judicial venues, advocacy organizations, regulatory bodies) → low d → experienced as coordination (rope). White/Asian applicants are beneficiaries with powerful position and arbitrage options (can pursue alternative educational routes, benefit from other preference mechanisms) → low d → experienced as coordination (rope). Affirmative action beneficiaries are victims with powerless position and trapped exit (cannot exit racial identity, cannot exit educational system without losing opportunity) → high d → experienced as extraction (snare). Race-conscious universities are victims with organized power but constrained exit (can technically exit race-consciousness but lose educational mission function, face litigation and political pressure) → moderate-high d → experienced as mixed (tangled_rope). Remedial advocates are organized victims with constrained exit (can pursue alternative remedies but colorblind doctrine closes the primary pathway) → moderate-high d → experienced as mixed (tangled_rope). The state is institutional actor with constrained exit in long temporal horizon (constitutional doctrine is durable, cannot easily escape established interpretation) → moderate d → experienced as mixed (tangled_rope across institutional and analytical perspectives).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING — CONTESTED EQUAL PROTECTION: This constraint resolves the mandatrophy by making explicit that this is ONE READING of a contested constitutional kernel, not a settled natural law. The mandatrophy — 'is this pure extraction or legitimate coordination?' — is properly resolved by recognizing that the reading provides genuine coordination (clear rules, predictable doctrine) WHILE SIMULTANEOUSLY extracting from specific groups (affirmative action beneficiaries, race-conscious institutions, remedial advocates). The tangled_rope classification captures this exactly: real coordination function (establishing clear equal protection doctrine) paired with asymmetric extraction (distributional consequences that favor formal equality advocates and advantaged applicants while suppressing remedial tools). The false summit candidate marker signals that this reading naturalizes a contingent interpretive choice, making it appear inevitable rather than contested. The mandatrophy is not 'resolve to one type' but 'recognize the reading's position within the kernel contest and measure the extraction from that position.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_vs_substantive_equality,
    'Does equal protection mean identical legal treatment (formal equality) or does it permit differentiated treatment to achieve equal practical outcomes (substantive equality)?',
    'Constitutional interpretation via original text, historical purposes, and systematic jurisprudence across domains (housing, employment, healthcare). Empirical assessment of whether colorblind rules actually produce equal practical effects across racial groups.',
    'If formal equality is the binding constraint: colorblind reading is correct, remedial and diversity readings foreclose. If substantive equality is binding or co-binding: all three readings remain live; colorblind reading is one among competing frameworks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formal_vs_substantive_equality, conceptual, 'Whether equal protection requires formal or substantive equality').

omega_variable(
    constitutional_remedial_authority,
    'Does the Constitution permit states to use race-conscious remedies to address documented historical discrimination, or does the Equal Protection Clause foreclose such remedies on principle?',
    'Historical analysis of constitutional text, Reconstruction legislative intent, Supreme Court doctrinal development from Plessy through Grutter/Fisher. Empirical assessment of whether race-conscious university admissions have historically been necessary to remediate documented educational segregation and discrimination.',
    'If Constitution permits remedial race-consciousness: remedial reading is structurally sound; colorblind reading is one constraint among three. If Constitution forecloses all race-consciousness: colorblind reading dominates; remedial reading is constitutionally indefensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_remedial_authority, empirical, 'Whether Constitution permits race-conscious remedies for historical discrimination').

omega_variable(
    subordination_vs_classification,
    'Is the core evil of race-based discrimination the act of racial classification itself (forbidden by colorblind reading) or the perpetuation of subordination and inequality (the concern of remedial and diversity readings)?',
    'Doctrinal history: Reconstruction Congress intent, cases establishing strict scrutiny (Loving v Virginia — anti-subordination focus — vs Romer v Evans — anti-classification focus). Empirical assessment of whether colorblind rules in fact prevent subordination or whether they can entrench it by foreclosing remedies.',
    'If classification is the evil: colorblind reading is justified; remedial and diversity approaches misconstrue the constitutional problem. If subordination is the evil: colorblind reading may prevent legitimate anti-subordination measures; remedial and diversity readings are constitutionally sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_vs_classification, empirical, 'Whether Equal Protection targets racial classification or subordination').

omega_variable(
    beneficiary_naturalization,
    'Are the identified beneficiaries (white and Asian applicants, formal equality advocates) natural parties to equal protection analysis, or does the colorblind reading benefit them by naturalizing their advantaged position as the neutral baseline?',
    'Structural analysis of who benefits from race-neutrality given existing social stratification. Historical assessment of whether formal equality frameworks have historically protected privileged groups by treating inequality-perpetuating rules as neutral. Empirical measurement of admission rate changes before and after colorblind policies by racial group.',
    'If beneficiaries are natural: colorblind reading distributes protection to the identified parties legitimately. If beneficiaries are artificially elevated by naturalization: the reading is a false summit — it looks like natural law but actually benefits identifiable agents. This would elevate epsilon and trigger false summit reclassification to snare or tangled_rope from more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_naturalization, empirical, 'Whether colorblind reading naturalizes advantaged group position').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_cb_tr_t0, equal_protection_clause__colorblind_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(epc_cb_tr_t15, equal_protection_clause__colorblind_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(epc_cb_tr_t30, equal_protection_clause__colorblind_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(epc_cb_be_t0, equal_protection_clause__colorblind_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(epc_cb_be_t15, equal_protection_clause__colorblind_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(epc_cb_be_t30, equal_protection_clause__colorblind_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(epc_cb_su_t0, equal_protection_clause__colorblind_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(epc_cb_su_t15, equal_protection_clause__colorblind_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(epc_cb_su_t30, equal_protection_clause__colorblind_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, information_standard).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% The Equal Protection Clause kernel decomposes into three structurally distinct constraint stories, each with its own epsilon, beneficiary/victim structure, and classification type. The colorblind reading (this story) has epsilon=0.38, benefits formal equality advocates and white/Asian applicants, and suppresses race-conscious universities. The remedial reading has different epsilon (higher, likely 0.55+), benefits affirmative action beneficiaries and remedial advocates, and suppresses formal-only equality frameworks. The diversity reading occupies middle ground. All three are linked as kernel siblings via network.affects_constraints. The kernel decomposition reflects the ε-invariance principle: the observable used to evaluate the constraint (formal equality vs substantive equality vs diversity accommodation) changes epsilon and beneficiary/victim structure substantially. Three readings, three stories, one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
