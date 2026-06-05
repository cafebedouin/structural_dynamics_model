% ============================================================================
% CONSTRAINT STORY: expression_conscience_amendments__free_exercise_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_free_exercise_clause, []).

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
 *   constraint_id: expression_conscience_amendments__free_exercise_clause
 *   human_readable: Free Exercise Clause: Religious Practice Against Conscience-Burdening Laws
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Free Exercise Clause (Amendment I, part of the Bill of Rights)
 *   protects religious practice against government burden, establishing that
 *   conscience-driven action deserves protection from majoritarian
 *   regulation. This constraint instantiates ONE reading of a contested
 *   constitutional kernel — the broader First Amendment establishment and
 *   freedom of religion/expression text. This reading emphasizes practice
 *   protection and conscience violation prevention. The constraint exhibits
 *   tangled rope structure: it coordinates pluralistic coexistence (enabling
 *   religious minorities to live according to conscience without state
 *   suppression) while also extracting from secular regulatory regimes
 *   (preventing uniform application of neutral laws). The historical
 *   measurements show dramatic shifts: low extractiveness at founding (0.18,
 *   when religious minorities were small and federal authority limited),
 *   increasing through Sherbert-era heightened scrutiny (Sherbert v. Verner,
 *   1963), collapsing under Smith's neutral law doctrine (1990: 0.52, highest
 *   extractiveness), partially restored by RFRA (1993: 0.38), and stabilizing
 *   in contemporary doctrine (2021: 0.35). The suppression metric tells the
 *   story of burden analysis doctrine: founding-era suppression was moderate
 *   (0.40) because religious diversity was limited; suppression fell through
 *   mid-twentieth century as burden doctrine tightened; Smith drastically
 *   raised suppression (0.65) by eliminating heightened scrutiny; RFRA
 *   partially reduced suppression through statutory heightened scrutiny.
 *   Theater ratio tracks the performative content of protection: low at
 *   founding (real constitutional constraint was operational), rising through
 *   twentieth century as burden doctrine became complex, peaking under Smith
 *   (0.65, when the clause was largely ceremonial), and stabilizing post-RFRA
 *   at moderate levels (0.55). This reading is one of five siblings:
 *   assembly_petition_clause (collective political action),
 *   establishment_clause (secular state), free_press_clause (informed public
 *   via institutional press), free_speech_clause (open marketplace of
 *   expression). The kernel contest turns on how the First Amendment's
 *   enumerated protections relate: does Free Exercise function as a
 *   standalone conscience protection, or must it be read in relationship to
 *   Establishment Clause secularization? This story instantiates the
 *   standalone conscience reading.
 *
 * KEY AGENTS:
 *   - Observant Believers (powerful/arbitrage): Primary beneficiary. Gain clarity about when conscience-burdening regulation is forbidden. Can petition for exemptions, litigate, or relocate. Experience the clause as coordinative.
 *   - Minority Faith Communities (moderate/constrained): Secondary beneficiary with high friction costs. Organize for litigation, win exemptions, but face suppression from hostile majorities and high litigation burden. Experience the clause as tangled rope — genuine protection but extraction of litigation resources.
 *   - Unorganized Individual Believers (powerless/trapped): Victims in hostile jurisdictions without community backing. Cannot afford litigation, face suppressive enforcement, lack effective protection mechanism. Experience the clause as snare or non-functional.
 *   - Secular Regulatory Regime (institutional/constrained): Complex position — benefits from clause's coordination (prevents religious conflict) but constrained by clause's extraction (compelling interest test prevents uniform application). Experiences the clause as tangled rope from the regulatory side.
 *   - Constitutional Order (institutional/arbitrage): Generational beneficiary. The clause enables pluralistic democracy and religious stability. Experiences the clause as rope — functional coordination mechanism.
 *   - Analytical Observer (analytical/analytical): Civilizational perspective that risks naturalizing the clause as immutable law while missing its embedded conflicts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expression_conscience_amendments__free_exercise_clause, 0.35).
domain_priors:suppression_score(expression_conscience_amendments__free_exercise_clause, 0.68).
domain_priors:theater_ratio(expression_conscience_amendments__free_exercise_clause, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expression_conscience_amendments__free_exercise_clause, extractiveness, 0.35).
narrative_ontology:constraint_metric(expression_conscience_amendments__free_exercise_clause, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(expression_conscience_amendments__free_exercise_clause, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expression_conscience_amendments__free_exercise_clause, tangled_rope).
narrative_ontology:human_readable(expression_conscience_amendments__free_exercise_clause, "Free Exercise Clause: Religious Practice Against Conscience-Burdening Laws").
narrative_ontology:topic_domain(expression_conscience_amendments__free_exercise_clause, "political/legal/constitutional").

domain_priors:requires_active_enforcement(expression_conscience_amendments__free_exercise_clause).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(expression_conscience_amendments__free_exercise_clause, '3a231b3b-e491-4d5c-bd87-4963a22841f3').
narrative_ontology:cs_kernel_codification('3a231b3b-e491-4d5c-bd87-4963a22841f3', fixed_text).
narrative_ontology:cs_authority_grounding('3a231b3b-e491-4d5c-bd87-4963a22841f3', lineage).
narrative_ontology:cs_interpretation_layer_present('3a231b3b-e491-4d5c-bd87-4963a22841f3').
narrative_ontology:cs_reading_relation('3a231b3b-e491-4d5c-bd87-4963a22841f3', expression_conscience_amendments__establishment_clause, coexists_with).
narrative_ontology:cs_reading_relation('3a231b3b-e491-4d5c-bd87-4963a22841f3', expression_conscience_amendments__assembly_petition_clause, influences).
narrative_ontology:cs_reading_relation('3a231b3b-e491-4d5c-bd87-4963a22841f3', expression_conscience_amendments__free_press_clause, coexists_with).
narrative_ontology:cs_reading_relation('3a231b3b-e491-4d5c-bd87-4963a22841f3', expression_conscience_amendments__free_speech_clause, influences).
narrative_ontology:cs_axiom('3a231b3b-e491-4d5c-bd87-4963a22841f3', foundational, conscience_practice_deserves_heightened_protection).
narrative_ontology:cs_axiom_status(conscience_practice_deserves_heightened_protection, holdable).
narrative_ontology:cs_axiom_grounding('3a231b3b-e491-4d5c-bd87-4963a22841f3', conscience_practice_deserves_heightened_protection, deontological).
narrative_ontology:cs_axiom('3a231b3b-e491-4d5c-bd87-4963a22841f3', secondary, burden_test_is_enforceable_gate).
narrative_ontology:cs_axiom_status(burden_test_is_enforceable_gate, overridden).
narrative_ontology:cs_axiom_grounding('3a231b3b-e491-4d5c-bd87-4963a22841f3', burden_test_is_enforceable_gate, empirically_contingent).
narrative_ontology:cs_reference_frame('3a231b3b-e491-4d5c-bd87-4963a22841f3', conscience_protection_against_majoritarian_suppression).
narrative_ontology:cs_drift_state('3a231b3b-e491-4d5c-bd87-4963a22841f3', contemporary_post_smith_post_rluipa, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('3a231b3b-e491-4d5c-bd87-4963a22841f3', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(expression_conscience_amendments__free_exercise_clause, expression_conscience_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expression_conscience_amendments__free_exercise_clause, observant_believers).
narrative_ontology:constraint_beneficiary(expression_conscience_amendments__free_exercise_clause, minority_faith_communities).
narrative_ontology:constraint_victim(expression_conscience_amendments__free_exercise_clause, uniform_application_interests).
narrative_ontology:constraint_victim(expression_conscience_amendments__free_exercise_clause, secular_regulatory_regimes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVANT BELIEVER (ROPE) — The Free Exercise Clause coordinates the legitimate need for religious conscience to be protected against majoritarian regulation with the broader regulatory framework. From this agent's perspective, the clause is primarily coordinative: it establishes clear rules about when government can and cannot burden practice, enabling religious communities to plan, build institutions, and live according to conscience. The believer experiences this as a functional protection mechanism, not extraction. Arbitrage exit: communities can petition for exemptions, litigate, or relocate to jurisdictions with different regulatory environments.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_exercise_clause, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: MINORITY FAITH COMMUNITY (TANGLED ROPE) — Faces suppression: dominant legislative coalitions can enact laws burdening minority practices (pork restrictions, holiday observances, ritual requirements) with de facto targeting power. The Free Exercise Clause provides genuine coordination (enabling inter-faith coexistence) but also extracts: requires litigation to vindicate protection, faces hostile judiciary in some jurisdictions, bears burden of proving substantial burden and lack of compelling state interest. Exit is constrained by geography, network effects (religious community presence), and cost of litigation. Benefit is real (protected from outright bans) but comes with high friction costs.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_exercise_clause, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNORGANIZED BELIEVER IN HOSTILE JURISDICTION (SNARE) — Individual believer in a jurisdiction with hostile judiciary and no organized community backing cannot effectively use the Free Exercise Clause. Trapped: cannot afford litigation, lacks coalition support, faces suppressive enforcement of neutral laws (vaccination mandates, workplace dress codes, ceremonial bans). The clause exists on paper but does not function to protect this agent. Maximum experienced extraction: conscience-burdening regulation is enforced with no meaningful protection mechanism.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_exercise_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: SECULAR REGULATORY REGIME (TANGLED ROPE) — From the state's regulatory perspective, the Free Exercise Clause both coordinates and extracts. It coordinates: establishes clear limits on majoritarian religious suppression, preventing destabilizing religious conflict. It extracts: prevents uniform application of neutral laws (compelling state interest test forces government to litigate or carve exceptions), creates litigation burden on regulatory agencies, enables regulatory arbitrage (communities seek exemptions). The clause requires active enforcement (judiciary adjudicating substantial burden and compelling interest) but enables coordination of secular and religious life.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_exercise_clause, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL ORDER (ROPE) — At the generational level, the Free Exercise Clause is primarily a coordinative mechanism: it enables pluralistic democracy where government remains secular (Establishment Clause partner) while protecting religious minority practice. This coordination is essential for social stability across diverse belief systems. No net extraction from the constitutional order itself — the clause distributes protection and obligation in a way that sustains the broader system.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_exercise_clause, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAME (MOUNTAIN) — From a civilizational analytical perspective, protecting conscience against majoritarian suppression appears as a natural law: an irreducible requirement of any pluralistic society where belief diversity exists. The principle (conscience protection) appears immutable, requiring no justification beyond itself. However, this framing naturalizes what is actually a contested institutional choice about the weight of conscience relative to uniform regulation. The analytical observer viewing the clause as a mountain risks missing the structural conflicts embedded in its actual operation.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_exercise_clause, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expression_conscience_amendments__free_exercise_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expression_conscience_amendments__free_exercise_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expression_conscience_amendments__free_exercise_clause, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(expression_conscience_amendments__free_exercise_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Free Exercise Clause is fundamentally coordinative — it establishes rules enabling religious practice and secular governance to coexist. However, extractiveness is non-zero because: (1) the clause requires litigation to enforce, creating friction for resource-poor believers; (2) organized communities can extract exemptions that unorganized believers cannot access; (3) the clause prevents uniform application of neutral laws, extracting administrative flexibility from regulatory agencies. The measurement trajectory shows this is not constant: post-Smith (1990), extractiveness spiked to 0.52 because the clause ceased protecting most believers, increasing extraction on those who lacked statutory protections. RFRA (1993) reduced extractiveness back to 0.38 by restoring heightened scrutiny. Contemporary stabilization at 0.35 reflects RLUIPA's narrower domain plus case-by-case burden doctrine. Suppression (0.68): Moderate-high. This measures the structural barriers conscience-driven believers face when government enacts burdening regulation. Suppression includes: majoritarian political power (dominant groups can enact targeting laws), litigation costs (burden falls on believer to litigate), hostile judiciary in some circuits (believer faces uphill burden proof), geographic concentration (believers may have limited exit options from hostile jurisdictions). The measurement trajectory shows suppression rising from founding (0.40, limited religious diversity) through mid-twentieth century decline (Sherbert-era burden doctrine reduced suppression), then spiking under Smith (0.65) when judicial scrutiny vanished. RFRA partially restored scrutiny (0.58), but post-Smith doctrinal skepticism persists (0.68 current). Theater ratio (0.55): Moderate. This measures the performative content of Free Exercise Clause protection. The clause is partly theatrical: the compelling interest test sounds rigorous but is often applied deferentially; burden doctrine uses complex doctrinal language but frequently denies protection; the clause exists in the Constitution but many believers cannot effectively invoke it without organization and resources. Theater was low at founding (0.30) when the clause was a straightforward protection mechanism. Theater rose through doctrine complexity (Sherbert era, 0.40), peaked under Smith (0.65, when the clause was almost entirely ceremonial — it existed but protected almost no one), and stabilized post-RFRA at moderate levels (0.55) reflecting current burden doctrine's mixture of real protection and theatrical denial.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Observant believers (powerful/arbitrage) see Rope — a clear rule establishing when conscience is protected, enabling planning and institution-building with arbitrage exit (petition, litigation, relocation). Minority communities (moderate/constrained) see Tangled Rope — genuine coordination enabling religious coexistence plus extraction of litigation burdens and suppression from hostile majorities. Unorganized believers (powerless/trapped) see Snare — the clause promises protection but they cannot afford to invoke it; they experience suppression without remedy. Secular regulatory regimes (institutional/constrained) see Tangled Rope from the regulatory side — the clause coordinates religious-secular coexistence but prevents uniform law application, forcing litigation and exemptions. The constitutional order (institutional/arbitrage) sees Rope — a functional coordination mechanism enabling pluralism. The analytical observer risks seeing Mountain — an immutable requirement of pluralism — while missing the embedded extraction and the clause's actual operation depending on organization, litigation capacity, and friendly versus hostile judiciary. The perspectival gaps are driven by two factors: (1) organization and litigation resources (organized communities experience the clause as rope; unorganized believers as snare); (2) exit options (those with political power and resources have arbitrage exit; the unorganized are trapped). This is a diagnostic case for how structural position determines classification: the same clause is rope to the powerful, tangled rope to the moderately organized, and snare to the isolated.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for this constraint: Observant believers are declared beneficiaries with powerful/arbitrage exit — they gain clarity about protection and can exit through petition/litigation/relocation. The engine derives d ≈ 0.15 (beneficiary + arbitrage exit), yielding f(d) ≈ -0.01, producing negative χ (they are subsidized by the clause, not extracted from). Minority communities are beneficiaries but with constrained exit — they gain protection but face suppression barriers. The engine derives d ≈ 0.35-0.40 (beneficiary + constrained exit), yielding f(d) ≈ 0.40, producing moderate χ ≈ 0.15 at scope national (0.35 × 0.40 × 1.0). Secular regulatory regimes are victims (uniform application is prevented) with constrained exit — they experience extraction but have enforcement leverage. The engine derives d ≈ 0.60 (victim + constrained exit), yielding f(d) ≈ 0.85, producing elevated χ ≈ 0.30. Unorganized believers are declared victims with trapped exit — the engine derives d ≈ 0.95, f(d) ≈ 1.42, producing χ ≈ 0.50, the highest experienced extractiveness. The perspectives reflect this: beneficiaries with exit capacity see coordination; victims without exit see extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how a clause with genuine coordinative function (enabling religious coexistence) can simultaneously extract from both perspectives. The Free Exercise Clause is NOT a mislabeled snare disguised as rope, nor is it a mislabeled rope that is secretly pure extraction. It is authentically tangled rope: it genuinely coordinates religious-secular coexistence (removing it would destabilize pluralism, a coordination problem) while genuinely extracting (from regulatory regimes through compelling interest tests, from unorganized believers through litigation friction). The mandatrophy dissolves when we recognize that tangled rope is not a boundary case between rope and snare but a distinct type with both coordinative and extractive properties. The measurements show this stability: extractiveness varies (0.18-0.52 across history) but the clause remains coordinative at the generational level even when extractive at the individual level. The perspectival gaps show the same pattern: rope from powerful beneficiaries, tangled rope from moderate agents, snare from powerless victims. The clause is simultaneously all three because it distributes protection and burden asymmetrically — coordination for some (organized communities, regulatory regimes that can litigate) and extraction for others (isolated believers, hostile jurisdictions). Mandatrophy resolution: the constraint is tangled rope from the clausal level because both coordination and extraction are essential to its operation; it cannot be reduced to rope (would lose protection capacity for minorities) or to snare (would lose coordination benefit for pluralism). The perspectival difference is not a misclassification but a structural fact about how the clause operates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    burden_threshold_ambiguity,
    'What constitutes a ''substantial burden'' on religious practice sufficient to trigger Free Exercise Clause protection?',
    'Jurisprudential analysis of burden doctrine across circuits; comparison of doctrine pre-RFRA and post-Smith; empirical study of which burden claims succeed vs fail at different court levels',
    'If threshold is low: more plaintiffs receive protection, clause functions as broader constraint on regulation (more rope-like). If threshold is high: many conscience-burdened believers receive no protection (more snare-like for unorganized believers). This threshold is the primary variable controlling whether the clause coordinates or extracts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_threshold_ambiguity, empirical, 'Burden threshold doctrine determines actual protection scope').

omega_variable(
    compelling_state_interest_collapse,
    'How rigorous is judicial application of the compelling state interest test? Does it genuinely constrain government or function as rubber-stamp ratification of regulatory goals?',
    'Quantitative analysis of compelling interest success rates pre- and post-Smith; analysis of rejected vs accepted interest claims across domains; linguistic analysis of judicial reasoning for interest characterization',
    'If test is rigorous: clause meaningfully protects practice; extractiveness lower. If test is deferential: clause is symbolic; extractiveness higher. The structural question is whether the test gates actual protection or merely theatrically legitimates denials.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_state_interest_collapse, empirical, 'Compelling interest test rigor determines actual protection mechanism').

omega_variable(
    organization_gap_in_protection,
    'Does effective Free Exercise Clause protection depend on having an organized religious community with litigation resources, creating protection for established religions but not isolated believers?',
    'Comparative analysis of protection rates: organized vs unorganized plaintiffs; successful exemptions by religion (controlling for organizational sophistication); correlation between litigation capacity and successful claims',
    'If true: clause creates two-tier protection (organized=rope, unorganized=snare). If false: protection is more uniformly available. This determines whether the clause is a coordination mechanism or an extraction mechanism for the unorganized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organization_gap_in_protection, empirical, 'Whether clause protection requires organizational capacity').

omega_variable(
    kernel_reading_contest,
    'Is the Free Exercise Clause best read as protecting individual conscience against majoritarian suppression (this reading), or should it be read as inseparable from the Establishment Clause''s secular state interest (establishment_clause reading)?',
    'Historical analysis of Founding-era text and intent; doctrine genealogy from Reynolds through Smith to RLUIPA; analysis of cases where free exercise and establishment readings conflict',
    'If individual conscience protection is primary: this reading''s extractiveness is accurate (0.35), clause genuinely protects minority practice. If secular state establishment is primary: clause''s actual effect is to enforce secularization even when it burdens conscience, pushing toward higher extractiveness (0.50+), reclassifying toward snare for religious minorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contest between conscience-protection and secular-state readings of the clause').

omega_variable(
    smith_legacy_operational_burden,
    'Does the Free Exercise Clause, post-Employment Division v. Smith, actually protect minority practice against neutral generally applicable laws, or has the clause been operationally gutted for most regulation?',
    'Empirical study of protection rates for generally applicable laws pre- and post-Smith; analysis of which religious claims succeed post-Smith; comparison to RLUIPA domain where heightened scrutiny was restored',
    'If Smith reversed clause function: extractiveness rises, classification tilts toward snare for unorganized believers. If RLUIPA restoration has reinstated protection: extractiveness stable, clause remains tangled_rope. This is the central empirical question determining actual constraint operation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(smith_legacy_operational_burden, empirical, 'Whether Smith decision gutted Free Exercise Clause protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expression_conscience_amendments__free_exercise_clause, 0, 231).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fec_theater_1791, expression_conscience_amendments__free_exercise_clause, theater_ratio, 0, 0.3).
narrative_ontology:measurement(fec_theater_sherbert, expression_conscience_amendments__free_exercise_clause, theater_ratio, 1963, 0.4).
narrative_ontology:measurement(fec_theater_smith, expression_conscience_amendments__free_exercise_clause, theater_ratio, 1990, 0.65).
narrative_ontology:measurement(fec_theater_rfra, expression_conscience_amendments__free_exercise_clause, theater_ratio, 1993, 0.52).
narrative_ontology:measurement(fec_theater_dobbs_era, expression_conscience_amendments__free_exercise_clause, theater_ratio, 2021, 0.55).

% Extraction over time
narrative_ontology:measurement(fec_extractiveness_1791, expression_conscience_amendments__free_exercise_clause, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fec_extractiveness_sherbert, expression_conscience_amendments__free_exercise_clause, base_extractiveness, 1963, 0.25).
narrative_ontology:measurement(fec_extractiveness_smith, expression_conscience_amendments__free_exercise_clause, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(fec_extractiveness_rfra, expression_conscience_amendments__free_exercise_clause, base_extractiveness, 1993, 0.38).
narrative_ontology:measurement(fec_extractiveness_dobbs_era, expression_conscience_amendments__free_exercise_clause, base_extractiveness, 2021, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(fec_suppression_1791, expression_conscience_amendments__free_exercise_clause, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fec_suppression_sherbert, expression_conscience_amendments__free_exercise_clause, suppression_requirement, 1963, 0.35).
narrative_ontology:measurement(fec_suppression_smith, expression_conscience_amendments__free_exercise_clause, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(fec_suppression_rfra, expression_conscience_amendments__free_exercise_clause, suppression_requirement, 1993, 0.58).
narrative_ontology:measurement(fec_suppression_dobbs_era, expression_conscience_amendments__free_exercise_clause, suppression_requirement, 2021, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expression_conscience_amendments__free_exercise_clause, identity_coordination).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_exercise_clause, establishment_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_exercise_clause, free_speech_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_exercise_clause, assembly_petition_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_exercise_clause, free_press_clause).

% DUAL FORMULATION NOTE:
% The Free Exercise Clause is one reading of the expression_conscience_amendments kernel. The sibling readings (establishment_clause, free_speech_clause, assembly_petition_clause, free_press_clause) are separate constraint stories instantiating different constitutional interpretations of the same First Amendment text. This constraint's extractiveness (0.35) reflects the genuine coordinative function of religious practice protection; sibling constraints will have different ε values reflecting different coordination types. The network link indicates structural interdependence: how the free_exercise reading is adjudicated affects the boundaries within which establishment, speech, and assembly readings operate. A strong free exercise reading (low ε, high coordination) creates structural pressure on establishment reading (narrower room for secular state arguments). Conversely, a strong establishment reading creates structural pressure on free exercise (narrower room for practice protection arguments). See constraint family documentation for full decomposition of the First Amendment kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(expression_conscience_amendments__free_exercise_clause, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
