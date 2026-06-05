% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Equal Protection Clause — Formal Equality Reading (Race-Blind Doctrine)
 *   domain: constitutional_law/civil_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the formal equality reading of the
 *   Fourteenth Amendment's Equal Protection Clause — the dominant reading in
 *   contemporary U.S. constitutional law, articulated most influentially by
 *   Justice Scalia, Chief Justice Roberts, and the conservative
 *   constitutional majority. The reading holds that equal protection requires
 *   the state to treat individuals without regard to race; explicit racial
 *   classifications are presumptively invidious and permissible only if they
 *   survive strict scrutiny (compelling state interest + narrow tailoring).
 *   This reading stands in direct tension with the anti-caste reading
 *   (constraint_fourteenth_amendment_equal_protection__anti_caste_reading),
 *   which holds that equal protection requires active state dismantling of
 *   racial hierarchy through corrective action. The formal equality reading
 *   produces a hybrid structure: it coordinates a baseline rule against
 *   invidious discrimination (genuine coordination function) while
 *   simultaneously constraining state capacity to address the structural
 *   inequality that persists below the threshold of explicit classification.
 *   The extractiveness value (0.38) reflects that the constraint has both
 *   genuine coordinative elements (establishing a predictable, judicially
 *   administrable standard) and asymmetric effects (groups bearing historical
 *   disadvantage cannot access corrective state action that would remediate
 *   structural inequality). The theater_ratio (0.58) reflects that strict
 *   scrutiny review of racial classifications often performs neutrality while
 *   leaving the causal mechanisms of inequality untouched. The measurements
 *   show increasing theater and extractiveness over the 60-year interval
 *   (from 1964 to 2024), indicating that as the doctrine has matured, its
 *   performative elements have grown relative to its coordinative function,
 *   and its asymmetric effects have accumulated.
 *
 * KEY AGENTS:
 *   - Racialized groups (historically subordinated): Primary victims (powerless/trapped) — bear the structural inequality that formal doctrine presumes to ignore; cannot exit background conditions
 *   - Civil rights enforcement institutions: Secondary victims (organized/constrained) — tasked with enforcing equal protection but forbidden from deploying corrective state action
 *   - Beneficiary groups and status-quo defenders: Primary beneficiaries (institutional/arbitrage) — maintain accumulated advantage while doctrine preserves plausible denial of causation
 *   - Constitutional reformers and anti-caste advocates: Organized agents (powerful/mobile) — challenging the doctrine through litigation and legislative action; see exit routes
 *   - Judiciary: Institutional actor (institutional/constrained) — tasked with enforcing equal protection while bound by formal equality precedent that cordons off structural remediation
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional choice as a natural law of constitutional logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.38).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.42).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Equal Protection Clause — Formal Equality Reading (Race-Blind Doctrine)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/civil_rights/political_philosophy").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, '1624c765-7735-4dfa-8daa-35f610ab2848').
narrative_ontology:cs_kernel_codification('1624c765-7735-4dfa-8daa-35f610ab2848', fixed_text).
narrative_ontology:cs_authority_grounding('1624c765-7735-4dfa-8daa-35f610ab2848', lineage).
narrative_ontology:cs_interpretation_layer_present('1624c765-7735-4dfa-8daa-35f610ab2848').
narrative_ontology:cs_reading_relation('1624c765-7735-4dfa-8daa-35f610ab2848', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('1624c765-7735-4dfa-8daa-35f610ab2848', foundational, state_race_classification_presumptively_invidious).
narrative_ontology:cs_axiom_status(state_race_classification_presumptively_invidious, holdable).
narrative_ontology:cs_axiom_grounding('1624c765-7735-4dfa-8daa-35f610ab2848', state_race_classification_presumptively_invidious, deontological).
narrative_ontology:cs_axiom('1624c765-7735-4dfa-8daa-35f610ab2848', foundational, equality_requires_blindness_to_race).
narrative_ontology:cs_axiom_status(equality_requires_blindness_to_race, holdable).
narrative_ontology:cs_axiom_grounding('1624c765-7735-4dfa-8daa-35f610ab2848', equality_requires_blindness_to_race, deontological).
narrative_ontology:cs_reference_frame('1624c765-7735-4dfa-8daa-35f610ab2848', classification_blindness_regime).
narrative_ontology:cs_drift_state('1624c765-7735-4dfa-8daa-35f610ab2848', contemporary_structural_inequality_persistence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1624c765-7735-4dfa-8daa-35f610ab2848', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, institutional_color_blindness_advocates).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, politically_dominant_groups_status_quo_protected).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, groups_bearing_historical_structural_inequality).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_corrective_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RACIALIZED GROUPS (SNARE) — Structurally locked into the pre-constitutional inequality that formal equality presumes to ignore. Cannot exit the background condition; formal doctrine treats structural inequality as prior to constitutional reach. High experienced extraction — the doctrine itself forecloses remedial state action while preserving the inequality it refuses to see.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__formal_equality_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ENFORCEMENT INSTITUTIONS (TANGLED ROPE) — Constrained by the formal equality doctrine from deploying corrective state action. The constraint coordinates baseline nondiscrimination (genuine function) while simultaneously extracting from enforcement capacity — the institution can monitor but not remedy, coordinate exposure but not correction. Net extraction because the constraint forbids the institution its own remedial mandate.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BENEFICIARY GROUPS & STATUS-QUO DEFENDERS (ROPE) — Groups whose structural advantage is preserved by refusing to see it as a legal problem. The formal equality doctrine enables arbitrage between acknowledging inequality (acknowledged in legislative findings) and refusing remedial state action (justified by race-blindness doctrine). Net benefit — the constraint protects accumulated advantage while maintaining plausible denial.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__formal_equality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORMERS (SCAFFOLD) — See the formal equality doctrine as a temporary holding position that sunset-logic arguments are challenging. The 2009 Shelby County decision that gutted Voting Rights Act preclearance represented a sunset moment; ongoing constitutional challenge seeks to move past race-blindness toward explicit structural remediation. Powerful actors see exit routes; the doctrine is perceived as provisional.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__formal_equality_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL RITUAL OF RACE-NEUTRAL REVIEW (PITON) — The doctrine's actual verification function has atrophied. Courts applying strict scrutiny to racial classifications cannot effectively evaluate whether the classification genuinely remediates injury or merely perpetuates caste hierarchy; the doctrine performs neutrality while leaving structural power arrangements untouched. Maintained through institutional inertia (precedent, jurisdictional deference) rather than functional necessity. High theater — the reviewing court articulates neutrality while the doctrine systematically filters remedial state action out of constitutional reach.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__formal_equality_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL FORM VIEW (MOUNTAIN) — From a purely formal logical perspective, the rule 'equal protection prohibits racial classification absent compelling state interest' is a consistent, derivable doctrine from the principle of non-discrimination. This view treats formal equality as a natural law of constitutional logic — the classifier and the classified are symmetrically positioned under the rule. However, the structural data reveals this as a false summit: the doctrine naturalizes an asymmetric power arrangement (dominant groups maintain accumulated advantage while subordinated groups bear the cost of refusing remedial action).
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__formal_equality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: JUDICIARY (INSTITUTIONAL CONSTRAINED) — The courts are constrained by formal equality doctrine from reviewing the background conditions of inequality while simultaneously tasked with enforcing equal protection. Genuine coordination function: the doctrine stabilizes a predictable legal standard. Real extraction: the judiciary cannot address structural remediation; its review power is cordoned off from the causal mechanisms that produce inequality. Constrained exit — the doctrine is binding precedent that courts cannot easily overcome.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__formal_equality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__formal_equality_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, TR),
    TR >= 0.70.

:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The formal equality doctrine has genuine coordinative content — it establishes a predictable legal rule against invidious discrimination and enables judicial review of explicit classifications. But the extraction is real and asymmetric: the doctrine forecloses corrective state action that would address structural inequality, and this foreclosure asymmetrically benefits groups whose advantage is protected by the presumption that inequality is pre-constitutional background rather than a legal problem. The moderate value reflects the hybrid nature — not pure extraction (Snare) because coordinative elements exist, but not pure coordination (Rope) because the asymmetric effects are substantial and structurally entrenched. Suppression (0.42): Moderate-high. The doctrine suppresses alternatives to formal equality through strict scrutiny review and judicial precedent. It also suppresses the visibility of causal mechanisms — by treating racial inequality as a pre-constitutional background, the doctrine hides the state's role in maintaining that background through facially neutral policies. Exit costs are substantial: courts cannot easily overturn precedent; legislatures cannot easily mandate corrective state action that survives strict scrutiny; civil rights enforcement institutions cannot access remedial mechanisms without constitutional amendment or interpretive reversal. Theater ratio (0.58): Moderate-high. Strict scrutiny review performs the ritual of skeptical examination while often permitting the challenged action to proceed (or denying it for reasons that would justify denial of non-race-conscious action). The doctrine articulates race-blindness as the constitutional ideal while leaving the structural power arrangements that produced inequality untouched. Measurements show increasing theater over time as the doctrine has mature — early formulations (1964-1980s) had slightly more functional content (addressing explicit legal discrimination); later formulations (2000s-2020s) became increasingly focused on symbolic neutrality while structural inequality persisted.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence across structural positions. Racialized groups and civil rights institutions see the constraint as a snare or tangled rope that forecloses remedial action while inequality persists. Beneficiary groups see coordination (rope) — the doctrine provides a predictable rule. Constitutional reformers see a scaffold — the doctrine is being challenged and may sunset. The judiciary sees its own ritual as piton — the strict scrutiny performance persists through inertia. The analytical observer risks misclassifying as mountain (natural law of equal protection) when the structural data reveals a false summit: the doctrine naturalizes a contingent institutional choice that asymmetrically protects accumulated advantage. The kernel contest (formal equality vs anti-caste) is visible as perspectival divergence: the beneficiary group sees formal equality as constitutive of equal protection; the subordinated group sees anti-caste remediation as the constitutive meaning.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) derives from the agent's structural relationship to the constraint. Racialized groups and civil rights institutions are victims — they bear the cost of foreclosed corrective action — producing high d values (0.75-0.95) that yield high chi via the sigmoid f(d). Beneficiary groups are structural beneficiaries whose advantage is preserved — producing low d values (0.15-0.25) that yield negative or low chi. The judiciary occupies a constrained institutional position — unable to override precedent, tasked with enforcing the doctrine they recognize as asymmetric — producing moderate d (0.50-0.60). Constitutional reformers have mobile/arbitrage options — they can pursue alternative doctrinal framings or legislative solutions — producing lower d values (0.35-0.45). The formal equality reading inherently produces asymmetric chi across these positions because the doctrine itself is asymmetric: it coordinates a rule that has non-symmetric effects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_vs_substantive_equality_threshold,
    'What counts as ''formal'' vs ''substantive'' equality — does formal doctrine truly describe the doctrinal rule, or does it misdescribe a rule that is already substantive in effect?',
    'Jurisprudential analysis of what the doctrine actually permits (affirmative action bans, voting rights preclearance restrictions, etc.) — does it permit meaningful substantive remediation or only formal gestures? Empirical tracking of whether groups subject to formal doctrine experience reduction in inequality metrics.',
    'If formal doctrine is truly formal (permits substantive remediation): classification as Rope or Scaffold justified. If formal doctrine forecloses substantive remediation: reclassifies as Snare or Piton from more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_vs_substantive_equality_threshold, empirical, 'Whether formal equality doctrine permits substantive remediation or only formal gestures').

omega_variable(
    competing_kernel_reading_coherence,
    'Can the formal equality reading and the anti-caste reading coexist in a single constitutional framework, or does adoption of one reading logically foreclose the other?',
    'Analysis of constitutional text, historical record, and jurisprudential coherence: Can the Fourteenth Amendment simultaneously prohibit explicit racial classification AND require active dismantling of racial hierarchy? If yes, both readings coexist (different parties holding each); if no, one forecloses the other.',
    'If forecloses: this reading''s core premise (equal protection = race-blindness) directly contradicts anti-caste premise (equal protection = active hierarchy dismantling). If coexists: both readings remain live options in constitutional discourse, reflecting competing jurisprudential traditions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_kernel_reading_coherence, conceptual, 'Whether formal equality and anti-caste readings logically foreclose each other or coexist as live readings').

omega_variable(
    state_corrective_action_as_victim_or_beneficiary,
    'Should state corrective action (affirmative action, targeted enforcement) be classified as a victim of formal equality doctrine or as a beneficiary protected from excess state power?',
    'Normative alignment test: Does the reading treat corrective state action as a harm to be prevented (victim framing) or a power to be limited (beneficiary framing)? Empirical tracking: does the doctrine''s restrictive effect on state corrective action increase or decrease group inequality outcomes?',
    'If corrective action is victim: formal equality doctrine extracts from institutional capacity to remediate. If corrective action is beneficiary (constrained by rule of law limit): formal equality doctrine coordinates legitimate constraints on state power. Affects chi computation through directionality derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_corrective_action_as_victim_or_beneficiary, preference, 'Classification of state corrective action as victim or beneficiary in formal equality framework').

omega_variable(
    natural_law_vs_institutional_arrangement_status,
    'Is the prohibition on explicit racial classification a natural law of democratic legitimacy, or a contingent institutional choice that privileges one theory of equality over competing theories?',
    'Historical analysis: Has formal equality doctrine always been the understood meaning of equal protection, or has its dominance emerged from specific jurisprudential choices (e.g., 1950s-1960s shift toward race-blindness, 1995-present Rehnquist/Roberts court retrenchment)? Comparative constitutional law: do other democracies with equal protection norms adopt formal equality reading?',
    'If natural law: Mountain classification is justified. If institutional choice: Mountain classification is a false summit, reclassifying to Tangled Rope or Snare depending on beneficiary/victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_arrangement_status, empirical, 'Whether formal equality prohibition is a natural law or a contingent institutional choice').

omega_variable(
    strict_scrutiny_verification_function_integrity,
    'Does strict scrutiny review of explicit racial classifications actually serve its stated function of skeptical examination, or does it operate primarily as a performative ritual that validates predetermined outcomes?',
    'Doctrinal analysis of strict scrutiny jurisprudence: What percentage of race-conscious state action survives strict scrutiny? What percentage is struck down? Has this percentage changed over time? Comparative case analysis: do courts apply strict scrutiny consistently across similarly situated cases, or does application vary based on political/ideological alignment?',
    'If strict scrutiny functions: doctrine is coordinative (Rope elements genuine). If performative: doctrine is primarily theater (Piton elements dominant, theater_ratio should be higher).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_scrutiny_verification_function_integrity, empirical, 'Whether strict scrutiny review of racial classifications serves actual verification or performative function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eq_prot_formal_tr_t0, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(eq_prot_formal_tr_t30, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(eq_prot_formal_tr_t60, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(eq_prot_formal_be_t0, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(eq_prot_formal_be_t30, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(eq_prot_formal_be_t60, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(eq_prot_formal_su_t0, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(eq_prot_formal_su_t30, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement(eq_prot_formal_su_t60, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, strict_scrutiny_affirmative_action_gate).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, voting_rights_preclearance_restriction).

% DUAL FORMULATION NOTE:
% The formal equality and anti-caste readings are sibling constraints within the fourteenth_amendment_equal_protection kernel family. Each reading has its own ε, its own perspectives, its own beneficiary/victim structure. The formal equality reading (this file) has ε=0.38 and focuses on the coordinative rule against explicit classification. The anti-caste reading has ε=0.58 and focuses on the mandate for active remediation. These are not the same constraint viewed from different angles — they are structurally distinct interpretations of the same constitutional text, with different empirical status and different downstream effects. Strict scrutiny affirmative action gate and voting rights preclearance restriction are downstream constraints enabled by the formal equality reading — their classification depends on whether one accepts the formal equality framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
