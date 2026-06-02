% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

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
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Equal Protection as Colorblind Commitment (Harlan Reading)
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The colorblind reading of equal protection represents one interpretive
 *   stance on the Fourteenth Amendment's guarantee that no state shall deny
 *   equal protection. This reading, originating in Justice John Marshall
 *   Harlan's dissent in Plessy v. Ferguson (1896), holds that the
 *   Constitution is 'color-blind' and that the state may not classify
 *   citizens on the basis of race under any circumstance — including for
 *   remedial purposes. The constraint instantiates this reading as a
 *   stabilized commitment in constitutional doctrine, producing extraction
 *   (for disadvantaged minorities denied remedy), mixed
 *   coordination-extraction (for affirmative action beneficiaries),
 *   coordination (for institutions maintaining the principle), and
 *   performance ritual (for universities administering programs under legal
 *   siege). The constraint's theater_ratio has risen over the interval
 *   (0.35→0.58) as universities have been forced to generate increasingly
 *   elaborate justifications for race-conscious policies under constitutional
 *   challenge, while the extractiveness of the constraint itself has grown as
 *   the colorblind reading hardens into settled doctrine. This is NOT a
 *   constraint describing affirmative action directly; it describes the
 *   institutional implementation of the colorblind reading and how different
 *   agents experience that reading's force.
 *
 * KEY AGENTS:
 *   - Disadvantaged Racial Minorities: Primary victims (powerless/trapped) — denied access to race-conscious remedial programs; structural inequality persists without institutional remedy
 *   - Applicants Excluded by Race-Conscious Programs: Secondary victims (powerless/trapped) — classified on the basis of race and excluded from opportunity; experience direct harm from the state classification that colorblindness forbids
 *   - Affirmative Action Beneficiaries: Tertiary victims (moderate/constrained) — receive benefits through race-conscious programs, but those programs are continuously under legal threat from colorblind reading; cannot exercise exit (cannot ignore the legal vulnerability)
 *   - Colorblind Constitutional Authority: Primary beneficiary (institutional/arbitrage) — institutions interpreting equal protection colorblindly; gain legitimacy from the principle's simplicity and moral clarity; can shift to alternative readings if political pressure mounts
 *   - Universities and Public Employers: Secondary institutional actors (institutional/arbitrage) — operate affirmative action programs while defending their constitutionality under colorblind reading; increasing theater (justification burden) relative to stable function
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing the colorblind reading as a law of constitutional thought rather than recognizing it as one contested interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.42).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.48).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Equal Protection as Colorblind Commitment (Harlan Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, '61013545-a60f-4f4a-bfbf-22cb39aa5230').
narrative_ontology:cs_kernel_codification('61013545-a60f-4f4a-bfbf-22cb39aa5230', fixed_text).
narrative_ontology:cs_authority_grounding('61013545-a60f-4f4a-bfbf-22cb39aa5230', lineage).
narrative_ontology:cs_interpretation_layer_present('61013545-a60f-4f4a-bfbf-22cb39aa5230').
narrative_ontology:cs_reading_relation('61013545-a60f-4f4a-bfbf-22cb39aa5230', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_reading_relation('61013545-a60f-4f4a-bfbf-22cb39aa5230', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('61013545-a60f-4f4a-bfbf-22cb39aa5230', foundational, race_classification_inherently_harmful).
narrative_ontology:cs_axiom_status(race_classification_inherently_harmful, holdable).
narrative_ontology:cs_axiom_grounding('61013545-a60f-4f4a-bfbf-22cb39aa5230', race_classification_inherently_harmful, deontological).
narrative_ontology:cs_axiom('61013545-a60f-4f4a-bfbf-22cb39aa5230', foundational, colorblindness_as_constitutional_principle).
narrative_ontology:cs_axiom_status(colorblindness_as_constitutional_principle, holdable).
narrative_ontology:cs_axiom_grounding('61013545-a60f-4f4a-bfbf-22cb39aa5230', colorblindness_as_constitutional_principle, deontological).
narrative_ontology:cs_reference_frame('61013545-a60f-4f4a-bfbf-22cb39aa5230', formal_race_neutral_state).
narrative_ontology:cs_drift_state('61013545-a60f-4f4a-bfbf-22cb39aa5230', contemporary_affirmative_action_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('61013545-a60f-4f4a-bfbf-22cb39aa5230', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, colorblind_principle_institutional_authority).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, applicants_excluded_by_race_conscious_programs).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, race_conscious_remedial_programs).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, beneficiaries_of_affirmative_action).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, disadvantaged_racial_minorities_lacking_remedy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISADVANTAGED MINORITY WITHOUT REMEDY (SNARE) — This reading forbids the state from using race-conscious remediation. Historical harms cannot be addressed through race-conscious allocation. The disadvantaged group remains trapped in structural inequality with no legitimate institutional pathway for remedy. Maximum experienced extraction because the colorblind principle actively forecloses remedial tools.
constraint_indexing:constraint_classification(equal_protection_commitment__colorblind_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: APPLICANT DENIED ADMISSION (SNARE) — Individual applicants (Asian Americans, white applicants) experience race-conscious admissions programs as direct harm. The colorblind reading treats their exclusion as a violation of equal protection. Classification itself is the injury, regardless of intent or remedial purpose. No exit option: the constraint operates through the state's classification authority.
constraint_indexing:constraint_classification(equal_protection_commitment__colorblind_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: BENEFICIARY OF REMEDIAL PROGRAM (TANGLED ROPE) — This reading reclassifies affirmative action beneficiaries as victims of the constraint. They experience both coordination (access to opportunity through race-conscious policy) AND extraction (the colorblind reading strips them of that access, treating the remedy as constitutional violation). Constrained exit: the beneficiary cannot simply accept the benefit and ignore the legal vulnerability; the program itself is under continuous legal assault under this reading.
constraint_indexing:constraint_classification(equal_protection_commitment__colorblind_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COLORBLIND CONSTITUTIONAL AUTHORITY (ROPE) — Institutional actors interpreting equal protection through the colorblind lens experience the constraint as coordination: the principle provides clear rule (no racial classification) and minimal administrative burden. Simple categorical rule with high institutional legitimacy. Benefits from the principle's simplicity and symmetrical application. Arbitrage option: can pivot to remedial readings if political pressure mounts, but maintains coordinating function regardless.
constraint_indexing:constraint_classification(equal_protection_commitment__colorblind_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONS ADMINISTERING AFFIRMATIVE ACTION (PITON) — Universities and public employers maintain race-conscious programs while under constitutional siege from the colorblind reading. The constraint persists through institutional inertia despite the theater of justification becoming increasingly elaborate. Institutions must simultaneously operate the programs AND produce legal arguments for their constitutionality — the theater-to-function ratio has risen as litigation pressure mounts. Piton derives from theater gate: high justificatory performance relative to stability of actual practice.
constraint_indexing:constraint_classification(equal_protection_commitment__colorblind_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FORMAL EQUALITY AS NATURAL LAW (MOUNTAIN) — From a civilizational horizon, the colorblind reading appears as an immutable principle of justice: equal protection requires identical treatment regardless of race. Classification itself is the harm, independent of intent or effect. This perspective naturalizes a specific reading as a law of constitutional thought itself. The engine's false summit detector will identify this as potential naturalization of a contested reading, not a law of nature or logic.
constraint_indexing:constraint_classification(equal_protection_commitment__colorblind_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_commitment__colorblind_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_commitment__colorblind_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, TR),
    TR >= 0.70.

:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate-high. The colorblind reading creates asymmetric burden: it forecloses institutional remedies for historical wrongs, trapping disadvantaged groups without legitimate remedy pathways. It prohibits state classification even when classification is the only mechanism to address structural inequality. However, the extractiveness is not as high as a pure snare (0.66+) because the constraint operates through constitutional interpretation (contestable) rather than material coercion (harder to escape). Suppression (0.48): Moderate-high. The reading suppresses alternative approaches (remedial programs, race-conscious allocation) by treating them as constitutional violations. It reduces the freedom to use race-conscious tools, even for remedial purposes. Institutional suppression is high (institutions must comply with constitutional doctrine), but not maximal because litigation and reinterpretation remain available. Theater ratio (0.58): Moderate-high. Universities and employers maintain race-conscious programs while producing increasingly elaborate legal justifications for their constitutionality. The justificatory performance has grown over the interval as colorblind reading hardened; institutions engage in sophisticated rationales (diversity as educational interest, critical mass for educational benefits) that track the boundary of what the colorblind reading permits. This theater-to-function ratio indicates that the actual administrative goal (achieving diverse student bodies and workforces) is increasingly decoupled from the formal legal justification required to defend it under colorblind scrutiny.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a perspectival gap across institutional positions and structural relationships. Disadvantaged minorities see foreclosure of remedy (Snare). Individual applicants excluded by race-conscious programs see direct classification harm (Snare). Affirmative action beneficiaries see mixed coordination (benefit through program) and extraction (vulnerability to constitutional challenge) — Tangled Rope. Institutions maintaining the colorblind principle see coordination (clear rule, institutional legitimacy). Universities defending affirmative action see degraded ritual (Piton) — increasing theater relative to stable function. The analytical observer risks seeing natural law (Mountain) — the principle of colorblindness as constitutive of justice itself — but this naturalizes what is actually a contested institutional reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects the agent's structural position relative to THIS reading's extraction flow. Disadvantaged minorities have d ≈ 0.95 (full victim): they are excluded from remedy by the reading's logic. Applicants excluded by classification have d ≈ 0.90 (primary targets): they directly experience the harm the reading forbids. Affirmative action beneficiaries have d ≈ 0.70 (partial victim): they benefit from programs but those benefits are under legal threat from the reading itself. Institutional authorities interpreting the reading colorblindly have d ≈ 0.10 (beneficiary): they gain simplicity, legitimacy, and clear operational rules. Universities administering affirmative action have d ≈ 0.55 (symmetric): they benefit from the programs' educational outcomes but bear the institutional and litigation burden of defending them under colorblind scrutiny. The analytical observer has d ≈ 0.72 (partial victim of their own framework): their institutional position as unbiased interpreter is compromised by the risk that their analytical stance naturalizes a contested reading as law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's extractiveness (0.42) places it in the tangled_rope zone (0.30 ≤ ε ≤ 0.55, requires coordination function + asymmetric extraction + enforcement). The mandatrophy is resolved by recognizing that the colorblind reading IS a coordination mechanism (provides a clear, administrable constitutional rule) AND creates asymmetric extraction (forecloses remedies for some while simplifying authority for others). The theater_ratio rising over the interval (0.35→0.58) indicates that the coordination function (clarity of rule) is stable while the institutional burden (justifying exceptions) has increased — institutions must perform increasingly elaborate legal reasoning to maintain affirmative action programs under the colorblind reading. The constraint is NOT pure extraction because the colorblind principle genuinely simplifies equal protection doctrine and provides institutional clarity. It is NOT pure coordination because it asymmetrically forecloses remedial tools for disadvantaged groups. It is Tangled Rope: a hybrid that coordinates doctrine while extracting from those denied remedy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_vs_effect_framing,
    'Is equal protection violated by the act of racial classification itself, or by disparate effects and continued structural inequality?',
    'Comparative constitutional law analysis: examine how other jurisdictions and legal traditions frame equal protection (effect-based vs. classification-based). Historical analysis of equal protection doctrine evolution and predictive modeling of which frame produces more stable constitutional equilibrium.',
    'If classification-focused: this reading (colorblind) is correct; affirmative action is unconstitutional. If effect-focused: remedial reading is correct; colorblindness naturalizes persistent inequality. If hybrid: both readings partially right, suggesting tangled_rope rather than mountain or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classification_vs_effect_framing, conceptual, 'Classification-based vs. effect-based framing of equal protection violation').

omega_variable(
    formal_vs_substantive_equality,
    'Does equal protection require formal identical treatment (colorblindness) or substantive equal opportunity (accounting for structural barriers)?',
    'Empirical tracking of outcomes under colorblind vs. remedial regimes; analysis of whether identical formal treatment produces substantively equal opportunity; examination of whether structural barriers persist absent remedial action.',
    'If formal equality sufficient: colorblind reading stands (mountain from analytical perspective, extraction from disadvantaged perspectives). If substantive equality required: colorblind reading forecloses the remedy, creating false summit (institutional beneficiaries naturalizing their authority).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_vs_substantive_equality, empirical, 'Formal versus substantive equality requirement').

omega_variable(
    historical_remedy_window,
    'At what point in time does the historical wrong become legally irrelevant? Does colorblindness from a future date retroactively invalidate remedies for past wrongs?',
    'Constitutional history analysis: precedent treatment of remedy timing; empirical outcomes under statute-of-limitations and laches doctrines in comparable remedial contexts; comparison to international constitutional practice on remedying historical injustice.',
    'If remedy window closed: colorblind reading invalidates affirmative action (victims perspective dominant). If remedy window open: remedial programs legitimate (diversity reading dominant). If remedy window contextual: suggests tangled_rope or scaffold classification (temporary remedial programs with sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_remedy_window, empirical, 'Duration and closure of historical remedy window').

omega_variable(
    kernel_reading_underspecification,
    'Is this constraint the reading of a contested kernel, or is colorblindness itself the uncontested interpretation of equal protection?',
    'Jurisprudential analysis: track competing institutional readings of equal protection from 1880s (Harlan dissent) to present; examine whether colorblind reading is hegemonic or contested within the same institutional authority (Supreme Court); model foreclosure relationships between readings.',
    'If truly contested: kernel-reading frame is correct; remedial and diversity readings are genuinely alternative readings coexisting in the field. If colorblind is hegemonic: this constraint is not a kernel reading but THE institutional reading; other readings are dissents or external critique.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underspecification, conceptual, 'Whether colorblindness is a contested reading or the hegemonic institutional reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eqpc_tr_t0, equal_protection_commitment__colorblind_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eqpc_tr_t30, equal_protection_commitment__colorblind_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(eqpc_tr_t60, equal_protection_commitment__colorblind_reading, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(eqpc_be_t0, equal_protection_commitment__colorblind_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(eqpc_be_t30, equal_protection_commitment__colorblind_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(eqpc_be_t60, equal_protection_commitment__colorblind_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(eqpc_su_t0, equal_protection_commitment__colorblind_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(eqpc_su_t30, equal_protection_commitment__colorblind_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(eqpc_su_t60, equal_protection_commitment__colorblind_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% The colorblind reading is one of three structurally distinct interpretations of the equal_protection_commitment kernel. Each reading generates a separate constraint story with its own ε value, beneficiary/victim structure, and classification pattern. The colorblind reading (this story) has ε=0.42 (Tangled Rope). The remedial reading has different ε (lower or higher depending on empirical effectiveness of remedies) and different victim/beneficiary structure (minorities become beneficiaries if remedies work, perpetrators become victims under remedial interpretation). The diversity reading has its own ε and its own institutional supporters. All three readings coexist in the constitutional field; none logically forecloses another within a unified institutional framework. The network relationship is bidirectional influence: colorblindness challenges constitutionality of remedial programs (affects constraint), but remedial reading challenges colorblindness's denial of remedy (reverse influence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
