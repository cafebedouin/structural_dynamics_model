% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__intermediate_scrutiny_tier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__intermediate_scrutiny_tier, []).

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
 *   constraint_id: equal_protection_clause__intermediate_scrutiny_tier
 *   human_readable: Equal Protection Intermediate Scrutiny for Sex-Based Classifications
 *   domain: constitutional_law/equal_protection
 *
 * SUMMARY:
 *   The intermediate scrutiny tier for sex-based classifications is ONE
 *   READING of the Equal Protection Clause kernel. This reading emerged from
 *   the women's rights litigation strategy of the 1970s and achieved
 *   constitutional consolidation in Craig v. Boren (1976), where the Supreme
 *   Court first articulated that sex classifications must serve important
 *   governmental objectives and be substantially related to those objectives.
 *   The reading represents a doctrinal compromise: more protective than
 *   rational basis (which permits any conceivable legitimate interest), but
 *   less absolute than strict scrutiny (which presumes racial classifications
 *   unconstitutional and requires narrow tailoring to compelling interests).
 *   The intermediate scrutiny tier is contested by two sibling readings: the
 *   rational basis tier (which treats sex classifications like economic
 *   classifications, deferring to legislative line-drawing) and the strict
 *   scrutiny tier (which treats sex classifications like race
 *   classifications, presuming them unconstitutional). This constraint story
 *   models ONLY the intermediate scrutiny reading, documenting its structural
 *   properties as a tangled_rope that coordinates sex-equality principles
 *   with residual legislative authority to respond to real differences.
 *
 * KEY AGENTS:
 *   - Sex Discrimination Plaintiffs (organized/arbitrage): Primary beneficiary. Mobilize intermediate scrutiny as a doctrinal tool for challenging sex classifications. Court access and reasonably predictable review standards benefit plaintiff organizations.
 *   - State Legislatures (organized/constrained): Mixed position. Retain authority to use sex classifications when substantially related to important objectives, but face active enforcement burden and scrutiny. Can navigate intermediate scrutiny but with significant legal risk.
 *   - Federal Courts (institutional/constrained): Apply the doctrine as both a coordination mechanism (guidance for rational review) and an enforcement regime (suppressing certain sex classifications). Courts extract doctrinal authority while coordinating judicial-legislative dialogue.
 *   - Real-Differences Rationales (powerful/mobile): Victim perspective. Claims about biological sex differences or social complementarity that once justified categorical sex classifications are now substantially suppressed. Theater persists (courts cite and respond to these claims) but doctrinal force is degraded.
 *   - Sex-Equality Advocacy Movement (organized/constrained): Beneficiary with sunset perspective. Uses intermediate scrutiny as scaffolding toward fuller sex equality; sees the doctrine as a temporary way station toward strict scrutiny treatment.
 *   - Analytical Observer (analytical/analytical): Risk of naturalizing intermediate scrutiny as the inevitable equilibrium of equal protection theory rather than acknowledging it as a constructed doctrinal choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__intermediate_scrutiny_tier, 0.48).
domain_priors:suppression_score(equal_protection_clause__intermediate_scrutiny_tier, 0.52).
domain_priors:theater_ratio(equal_protection_clause__intermediate_scrutiny_tier, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__intermediate_scrutiny_tier, extractiveness, 0.48).
narrative_ontology:constraint_metric(equal_protection_clause__intermediate_scrutiny_tier, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(equal_protection_clause__intermediate_scrutiny_tier, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__intermediate_scrutiny_tier, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__intermediate_scrutiny_tier, "Equal Protection Intermediate Scrutiny for Sex-Based Classifications").
narrative_ontology:topic_domain(equal_protection_clause__intermediate_scrutiny_tier, "constitutional_law/equal_protection").

domain_priors:requires_active_enforcement(equal_protection_clause__intermediate_scrutiny_tier).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__intermediate_scrutiny_tier, '021e08c5-9034-42d0-89bb-5030ce5a1296').
narrative_ontology:cs_kernel_codification('021e08c5-9034-42d0-89bb-5030ce5a1296', formalized).
narrative_ontology:cs_authority_grounding('021e08c5-9034-42d0-89bb-5030ce5a1296', lineage).
narrative_ontology:cs_interpretation_layer_present('021e08c5-9034-42d0-89bb-5030ce5a1296').
narrative_ontology:cs_reading_relation('021e08c5-9034-42d0-89bb-5030ce5a1296', equal_protection_clause__rational_basis_tier, forecloses).
narrative_ontology:cs_reading_relation('021e08c5-9034-42d0-89bb-5030ce5a1296', equal_protection_clause__strict_scrutiny_tier, coexists_with).
narrative_ontology:cs_axiom('021e08c5-9034-42d0-89bb-5030ce5a1296', foundational, sex_not_discrete_insular_minority).
narrative_ontology:cs_axiom_status(sex_not_discrete_insular_minority, holdable).
narrative_ontology:cs_axiom_grounding('021e08c5-9034-42d0-89bb-5030ce5a1296', sex_not_discrete_insular_minority, deontological).
narrative_ontology:cs_axiom('021e08c5-9034-42d0-89bb-5030ce5a1296', foundational, important_objectives_justification_possible).
narrative_ontology:cs_axiom_status(important_objectives_justification_possible, holdable).
narrative_ontology:cs_axiom_grounding('021e08c5-9034-42d0-89bb-5030ce5a1296', important_objectives_justification_possible, empirically_contingent).
narrative_ontology:cs_reference_frame('021e08c5-9034-42d0-89bb-5030ce5a1296', equal_protection_as_heightened_sex_scrutiny).
narrative_ontology:cs_drift_state('021e08c5-9034-42d0-89bb-5030ce5a1296', contemporary_sexual_orientation_and_gender_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('021e08c5-9034-42d0-89bb-5030ce5a1296', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__intermediate_scrutiny_tier, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__intermediate_scrutiny_tier, sex_discrimination_plaintiffs).
narrative_ontology:constraint_beneficiary(equal_protection_clause__intermediate_scrutiny_tier, doctrinal_sex_equality_movement).
narrative_ontology:constraint_victim(equal_protection_clause__intermediate_scrutiny_tier, real_differences_rationales).
narrative_ontology:constraint_victim(equal_protection_clause__intermediate_scrutiny_tier, legislative_line_drawing_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEX-ROLE ENFORCEMENT APPARATUS (SNARE) — States historically relying on sex classifications to enforce traditional gender roles face maximum suppression. The intermediate scrutiny standard eliminates most categorical sex-based statutes as non-narrowly-tailored. Exit is structurally impossible without abandoning the enforcement regime itself. Extraction runs maximal because the standard's core function is to prevent this regime.
constraint_indexing:constraint_classification(equal_protection_clause__intermediate_scrutiny_tier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE LEGISLATIVE BODIES (TANGLED ROPE) — States can still use sex classifications when substantially related to important objectives (e.g., physical capacity in military roles, reproductive difference in family law). Genuine coordination function exists: the standard enables sex-responsive policy without abandoning equal protection. But suppression is significant: lawmakers face active enforcement burden, heightened scrutiny review, and the requirement of exceedingly persuasive justification. Mixed extraction and coordination.
constraint_indexing:constraint_classification(equal_protection_clause__intermediate_scrutiny_tier, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SEX DISCRIMINATION PLAINTIFFS (ROPE) — Primary beneficiaries. The intermediate scrutiny standard provides a coordination mechanism for identifying impermissible sex classifications without the absolute bar of strict scrutiny or the total deference of rational basis. Plaintiffs can mobilize the standard strategically; courts apply a reasonably predictable doctrine. Benefits from the constraint are immediate and clear — access to heightened judicial review. Minimal experienced extraction relative to beneficiary status.
constraint_indexing:constraint_classification(equal_protection_clause__intermediate_scrutiny_tier, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL COURTS (TANGLED ROPE) — Courts apply the intermediate scrutiny standard as a doctrinal coordination mechanism, but also as an enforcement regime extracting authority over legislative sex-based classifications. Genuine coordination function: the standard provides guidance for rational review. But courts also suppress alternative rationales (deference to social science claims about sex differences) and extract doctrinal control. The constraint both coordinates judicial authority and extracts it from legislative bodies.
constraint_indexing:constraint_classification(equal_protection_clause__intermediate_scrutiny_tier, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REAL-DIFFERENCES RATIONALES (PITON) — Social science and biological claims that sex differences justify categorical classifications were once live legal rationales but are now substantially degraded by intermediate scrutiny's exceedingly persuasive justification requirement. The rationales persist in legislative attempts and some judicial reasoning but lack doctrinal force. Theater ratio is high (courts continue to cite and respond to real-differences arguments) but functional suppression is effective. This perspective sees the constraint as a performative gate that appears to engage real-differences reasoning while systematically excluding it.
constraint_indexing:constraint_classification(equal_protection_clause__intermediate_scrutiny_tier, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL RIGHTS VIEW (MOUNTAIN) — From a civilizational perspective, the intermediate scrutiny tier appears as the natural law equilibrium for equal protection: a principled midpoint between the permissiveness of rational basis and the absolutism of strict scrutiny. This perspective sees the doctrine as an immutable constitutional principle — the Platonic form of doctrinal sex equality that emerges naturally from coherent equal protection theory. However, the structural data contradicts this classification. The standard's success at suppressing sex classifications, the active enforcement required, and the beneficiary population suggest this is a constructed doctrine, not a natural law. The engine's false summit detector will classify this as naturalization of a contingent doctrinal choice.
constraint_indexing:constraint_classification(equal_protection_clause__intermediate_scrutiny_tier, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: SEX-EQUALITY ADVOCACY MOVEMENT / SUNSET VIEW (SCAFFOLD) — From the perspective of organizations that mobilized intermediate scrutiny as a strategic compromise, the doctrine functions as temporary scaffolding toward fuller sex-equality recognition. The intermediate tier is seen as a staging ground for eventual strict scrutiny treatment of sex classifications (closing the gap between sex and race). Low theater (the doctrine does real work), genuine beneficiary coordination (plaintiffs win cases), but with sunset logic: the advocacy movement intends this intermediate position as a way station toward higher protection. As social acceptance of sex equality increases, pressure builds for elevation to strict scrutiny — the scaffold becomes unnecessary.
constraint_indexing:constraint_classification(equal_protection_clause__intermediate_scrutiny_tier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__intermediate_scrutiny_tier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_clause__intermediate_scrutiny_tier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_clause__intermediate_scrutiny_tier, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__intermediate_scrutiny_tier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(equal_protection_clause__intermediate_scrutiny_tier, TR),
    TR >= 0.70.

:- end_tests(equal_protection_clause__intermediate_scrutiny_tier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.48): Moderate-high. The intermediate scrutiny tier has substantively suppressed the use of sex classifications for role enforcement (military, government employment, social security benefits). States cannot easily justify categorical sex-based statutes under the 'exceedingly persuasive justification' standard. However, extractiveness is not extreme (snare-level) because the doctrine permits sex classifications when substantially related to important objectives (pregnancy classifications, physical capability requirements, some family law contexts). This middle ground between rational basis (near-total deference, ε ≈ 0.15) and strict scrutiny (presumptive unconstitutionality, ε ≈ 0.75) places intermediate scrutiny in the tangled_rope zone: genuine coordination function (sex-responsive policy remains possible) plus genuine extraction (historical sex-role enforcement is substantially suppressed). The measurement trajectory shows a rise from 0.35 (pre-Craig, when sex classifications faced minimal scrutiny) to 0.52 (peak in 1996, Virginia Military Institute case), stabilizing around 0.48 as doctrine matured. Suppression (0.52): Significant. The 'exceedingly persuasive justification' standard is a high bar that substantially constrains legislative reliance on sex classification. States must produce strong evidence that the classification is substantially related to an important objective — a burden courts enforce vigorously. But suppression is not total: some sex classifications survive (pregnancy-based classifications, physical capacity requirements). Theater Ratio (0.58): Moderate-high. The doctrine maintains theatrical elements: courts cite and respond to real-differences justifications even when rejecting them as insufficiently persuasive (see Justice O'Connor's opinion in Virginia Military Institute, which engages VMI's educational rationales in detail before rejecting them). The exceedingly persuasive justification standard itself is somewhat performative — the standard is articulated clearly but its application is outcome-sensitive, and different circuits apply different thresholds. Theater increased from 0.42 (when scrutiny was nascent and less formalized) to 0.61 (when the doctrine became more standardized but also more ritualized in appellate procedure), stabilizing at 0.58 as practice routinized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival divergence. Sex discrimination plaintiffs see a rope—a coordination mechanism that enables their mobilization. State legislatures see a tangled_rope with significant suppression. The courts applying the doctrine see themselves as coordinating (tangled_rope/institutional), while sex-role enforcers see a snare (complete suppression of their legislative projects). Real-differences rationales see a piton—their arguments are still cited (high theater) but lack doctrinal force (suppressed function). The sex-equality advocacy movement sees a scaffold (temporary compromise toward strict scrutiny). The analytical observer risks seeing a mountain (natural law), but the structural data reveals a false summit: the doctrine's success at suppression, the active enforcement required, and the contingency of its adoption all suggest this is a constructed constraint, not an immutable constitutional principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary-Victim Structure: Sex discrimination plaintiffs and the doctrinal sex-equality movement are primary beneficiaries (low d, negative effective extraction relative to their power level). Sex-role enforcement regimes and real-differences rationales are victims (high d, suppression relative to their prior legislative authority). State legislatures occupy a constrained middle position: they retain some authority to use sex classifications (rope-like coordination benefit) but face significant scrutiny (extraction). Federal courts occupy the extracted-authority position (institutional power, constrained exit) but see themselves as coordinating (medium d). The engine derives d from these structural declarations and applies the sigmoid f(d) to compute experienced extractiveness chi. Beneficiaries with arbitrage options (sex-equality organizations accessing courts) experience low chi. Victims with no exit (sex-role enforcement apparatus) experience maximum chi. Legislators with constrained options experience moderate chi.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exceedingly_persuasive_justification_threshold,
    'What empirical standard distinguishes an ''exceedingly persuasive'' justification for sex classification from a merely ''important'' objective served by ''substantial relationship''?',
    'Corpus analysis of accepted vs rejected justifications in intermediate scrutiny case law; identification of empirical or doctrinal thresholds that consistently predict outcome; comparison with actual acceptance rates across judicial circuits',
    'If threshold is coherent and consistently applied: intermediate scrutiny is a genuine doctrinal tool (snare suppression + rope coordination = tangled_rope). If threshold is indeterminate or outcome-dependent: the constraint is more extractive theater than doctrinal substance (pushes classification toward piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exceedingly_persuasive_justification_threshold, empirical, 'Empirical coherence of ''exceedingly persuasive'' standard in case law').

omega_variable(
    real_differences_sufficiency,
    'When may states invoke biological or social differences between the sexes as satisfying the ''important objective'' requirement of intermediate scrutiny?',
    'Doctrinal survey of holdings: pregnancy classification cases (Michael M. v. Superior Court, Geduldig v. Aiello), physical capability cases (military draft, combat roles), reproductive autonomy cases (abortion restrictions). Mapping of which real-differences rationales survive intermediate scrutiny and which are foreclosed.',
    'If real-differences rationales are accepted for some statuses (e.g., pregnancy, military combat capability): the constraint permits a coordination path for sex-responsive legislation (stronger tangled_rope than snare). If all real-differences rationales are systematically rejected: the constraint approaches strict scrutiny in practical effect (shifts toward snare from legislative perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(real_differences_sufficiency, empirical, 'Doctrinal treatment of biological and social sex differences in intermediate scrutiny').

omega_variable(
    reading_contest_legitimacy,
    'Is the intermediate scrutiny tier a stable reading of the Equal Protection Clause kernel, or is it an unstable compromise between rational basis and strict scrutiny that will eventually collapse into one or the other?',
    'Longitudinal analysis of doctrine: (a) Does intermediate scrutiny stability increase or decrease over time? (b) Are circuit splits emerging suggesting breakdown? (c) Do justices treat intermediate scrutiny as principled doctrine or as ad-hoc balancing? (d) Does elevation-to-strict-scrutiny momentum continue or plateau?',
    'If stable: this reading is a legitimate long-term equilibrium in the kernel''s interpretation space; all three tiers can coexist as permanent readings. If unstable: pressure toward either rational basis (legislative backlash against sex equality) or strict scrutiny (doctrinal convergence) suggests intermediate scrutiny is a temporary historical compromise rather than a sustainable doctrinal settlement. Classification of the constraint itself would shift if the reading itself forecloses or is foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_legitimacy, conceptual, 'Stability and sustainability of intermediate scrutiny as a constitutional doctrine').

omega_variable(
    beneficiary_expansion_limits,
    'Do the doctrinal boundaries of intermediate scrutiny (applicable to sex, now also applied to sexual orientation and gender identity in some circuits) generalize to other classifications, or are there principled limits?',
    'Doctrinal analysis: (a) Bostock expansion (statutory sex discrimination includes sexual orientation and gender identity). (b) Recent constitutional-law circuit developments (whether sexual orientation receives intermediate or strict scrutiny; whether gender identity is protected). (c) Rationales courts offer for inclusion/exclusion (immutability, historical discrimination, relevance to legislative purpose).',
    'If boundaries generalize: the constraint is a reading of a broader equal-protection logic that scales to multiple classifications. If boundaries hold firm to biological sex: the intermediate tier is specific to the sex-discrimination movement''s historical achievement, not a generalizable principle. Different readings of the kernel might emerge as doctrine expands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_expansion_limits, empirical, 'Doctrinal generalizability of intermediate scrutiny to classifications beyond sex').

omega_variable(
    false_summit_kernel_naturalization,
    'Is intermediate scrutiny presented as the natural equilibrium of equal-protection theory (mountain/natural law), or acknowledged as a constructed doctrinal choice among multiple possible readings of the Equal Protection Clause?',
    'Textual analysis of judicial opinions and legal scholarship: (a) Do opinions frame intermediate scrutiny as ''discovered'' or as ''chosen''? (b) Is the doctrine presented with necessity language (''must'') or contingency language (''we adopt'')? (c) How is the doctrine positioned relative to the founding text and history? (d) Do opinions discuss alternative readings or only justify the intermediate tier as inevitable?',
    'If presented as natural law (mountain): high risk of false summit misclassification. The analytical observer perspective instantiates naturalization bias. If acknowledged as constructed choice (tangled_rope/scaffold): the reading is epistemically honest about its own contingency. Affects interpretation of whether the constraint is immutable (mountain false summit) or contestable (tangled_rope + shadow of strict scrutiny tier).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_kernel_naturalization, conceptual, 'Whether intermediate scrutiny is presented as natural law or constructed doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__intermediate_scrutiny_tier, 0, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_int_theater_t0, equal_protection_clause__intermediate_scrutiny_tier, theater_ratio, 0, 0.42).
narrative_ontology:measurement(epc_int_theater_craig, equal_protection_clause__intermediate_scrutiny_tier, theater_ratio, 1976, 0.55).
narrative_ontology:measurement(epc_int_theater_virginia, equal_protection_clause__intermediate_scrutiny_tier, theater_ratio, 1996, 0.61).
narrative_ontology:measurement(epc_int_theater_contemporary, equal_protection_clause__intermediate_scrutiny_tier, theater_ratio, 2020, 0.58).

% Extraction over time
narrative_ontology:measurement(epc_int_extract_t0, equal_protection_clause__intermediate_scrutiny_tier, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(epc_int_extract_craig, equal_protection_clause__intermediate_scrutiny_tier, base_extractiveness, 1976, 0.48).
narrative_ontology:measurement(epc_int_extract_virginia, equal_protection_clause__intermediate_scrutiny_tier, base_extractiveness, 1996, 0.52).
narrative_ontology:measurement(epc_int_extract_contemporary, equal_protection_clause__intermediate_scrutiny_tier, base_extractiveness, 2020, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(epc_int_suppress_t0, equal_protection_clause__intermediate_scrutiny_tier, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(epc_int_suppress_craig, equal_protection_clause__intermediate_scrutiny_tier, suppression_requirement, 1976, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__intermediate_scrutiny_tier, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__intermediate_scrutiny_tier, equal_protection_clause__rational_basis_tier).
narrative_ontology:affects_constraint(equal_protection_clause__intermediate_scrutiny_tier, equal_protection_clause__strict_scrutiny_tier).

% DUAL FORMULATION NOTE:
% The intermediate scrutiny tier is one of three interdependent doctrinal readings of the Equal Protection Clause kernel. The constraint family decomposes as follows: (1) rational_basis_tier (ε ≈ 0.15, Rope/Mountain) — near-total deference to legislative classification authority; (2) intermediate_scrutiny_tier (ε = 0.48, Tangled Rope) — this constraint, the moderate suppression position; (3) strict_scrutiny_tier (ε ≈ 0.75, Snare) — presumptive unconstitutionality of sex classifications. All three readings interpret the same constitutional text but produce radically different extraction structures. The intermediate tier occupies the middle position, influenced by pressure from both directions: strict scrutiny advocates push for elevation; rational basis defenders push for deference. Network linkage captures this interdependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
