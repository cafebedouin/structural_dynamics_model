% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__diversity_reading, []).

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
 *   constraint_id: equal_protection_clause__diversity_reading
 *   human_readable: Equal Protection Clause — Diversity Reading (Limited Race Consideration in Educational Context)
 *   domain: constitutional_law/civil_rights/educational_policy
 *
 * SUMMARY:
 *   The diversity reading of the Equal Protection Clause represents one
 *   interpretation of how race-conscious admissions practices can be
 *   reconciled with formal racial neutrality. This reading emerged from
 *   Regents v. Bakke (1978) and was refined through Gratz v. Bollinger and
 *   Grutter v. Bollinger (2003) before being effectively constrained by
 *   Students for Fair Admissions v. Harvard (2023). The reading permits
 *   universities to consider race as one factor among many when diversity
 *   serves a compelling educational interest, and claims that building
 *   racially diverse student bodies benefits all students through enriched
 *   learning environments. The constraint exhibits a fundamental tension: it
 *   nominally authorizes race-conscious decision-making while simultaneously
 *   imposing suppressions (narrow tailoring, strict scrutiny, prohibition of
 *   quotas) that progressively narrow the window in which such decisions are
 *   legally permissible. The measurement trajectory shows rising
 *   extractiveness and theater ratio over 45 years, reflecting two
 *   developments: (1) increasing empirical contestation about whether
 *   diversity actually serves the claimed educational interest, and (2)
 *   progressive doctrinal narrowing that has transformed the reading from a
 *   framework permitting genuine institutional discretion into a formal
 *   category divorced from actual admissions practice. Universities now
 *   pursue diversity through race-neutral proxies (socioeconomic status,
 *   first-generation status, geography) while maintaining the institutional
 *   language of the diversity justification, suggesting a shift toward
 *   piton-like performativity. This constraint is ONE READING of the
 *   contested equal_protection_clause kernel; the sibling readings
 *   (colorblind_reading, remedial_reading) instantiate different legal
 *   theories grounded in the same constitutional text but generating
 *   conflicting structural consequences.
 *
 * KEY AGENTS:
 *   - Individual Applicants Disadvantaged by Race Consideration: Structural position powerless/trapped — bear direct extraction cost (reduced admissions probability) with no exit option and no voice in mechanism design
 *   - Individual Applicants Advantaged by Race Consideration: Structural position moderate/constrained — benefit from diversity consideration but carry stigma of contested legitimacy; exit to private universities available but costly
 *   - Universities (Institutional Admissions Authority): Structural position institutional/arbitrage — net beneficiary of discretion the reading grants; can frame diversity mandate as part of educational mission; experience the constraint as coordination mechanism solving competing constitutional demands
 *   - Civil Rights Legal Coalition: Structural position organized/constrained — treated diversity reading as temporary scaffold toward substantive equality; could mobilize litigation support but constrained by equal protection doctrine itself
 *   - Judiciary (Courts Interpreting the Constraint): Structural position institutional/analytical — maintain the doctrinal framework through formal authority; increasingly constrain the narrow window through strict scrutiny and narrow tailoring requirements
 *   - Remedial Equality Mandate (Discursive Actor): Structural position powerless/trapped — suppressed by the diversity reading's framework; anti-subordination theory cannot be articulated within the diversity-as-educational-benefit justification; forecloses explicit discussion of systemic discrimination remediation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.22).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.38).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Clause — Diversity Reading (Limited Race Consideration in Educational Context)").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/civil_rights/educational_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, '00c46dbc-6751-4afd-a4ac-0b3e11750e9d').
narrative_ontology:cs_kernel_codification('00c46dbc-6751-4afd-a4ac-0b3e11750e9d', fixed_text).
narrative_ontology:cs_authority_grounding('00c46dbc-6751-4afd-a4ac-0b3e11750e9d', lineage).
narrative_ontology:cs_interpretation_layer_present('00c46dbc-6751-4afd-a4ac-0b3e11750e9d').
narrative_ontology:cs_reading_relation('00c46dbc-6751-4afd-a4ac-0b3e11750e9d', equal_protection_clause__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('00c46dbc-6751-4afd-a4ac-0b3e11750e9d', equal_protection_clause__remedial_reading, influences).
narrative_ontology:cs_axiom('00c46dbc-6751-4afd-a4ac-0b3e11750e9d', foundational, diversity_compelling_interest_justifies_limited_race_consideration).
narrative_ontology:cs_axiom_status(diversity_compelling_interest_justifies_limited_race_consideration, holdable).
narrative_ontology:cs_axiom_grounding('00c46dbc-6751-4afd-a4ac-0b3e11750e9d', diversity_compelling_interest_justifies_limited_race_consideration, instrumental).
narrative_ontology:cs_axiom('00c46dbc-6751-4afd-a4ac-0b3e11750e9d', foundational, formal_neutrality_constraint_prohibits_explicit_group_remediation).
narrative_ontology:cs_axiom_status(formal_neutrality_constraint_prohibits_explicit_group_remediation, holdable).
narrative_ontology:cs_axiom_grounding('00c46dbc-6751-4afd-a4ac-0b3e11750e9d', formal_neutrality_constraint_prohibits_explicit_group_remediation, deontological).
narrative_ontology:cs_reference_frame('00c46dbc-6751-4afd-a4ac-0b3e11750e9d', narrow_tailoring_discretion_window).
narrative_ontology:cs_drift_state('00c46dbc-6751-4afd-a4ac-0b3e11750e9d', post_sffa_2023, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('00c46dbc-6751-4afd-a4ac-0b3e11750e9d', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, universities_institutional_autonomy).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students_diverse_learning_environment).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, applicants_excluded_by_race_consideration).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, remedial_equality_mandate_forecloses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RACIALLY DISADVANTAGED APPLICANTS (SNARE) — Structural position: cannot exit the consideration mechanism; individual applicant has no exit option and faces direct extraction (reduced admissions probability). From their perspective, race consideration functions as a pure extraction mechanism with no coordination benefit. The constraint operates as a snare because the applicant bears a cost imposed by the institution with no offsetting voice in the mechanism. Power: powerless. Time: biographical (college-age window is immediate and unrepeatable). Exit: trapped — cannot opt out of the race-conscious admissions system when applying to a state university. Scope: national (equal protection is national law). Classification: snare (extraction without meaningful coordination function from the target's perspective).
constraint_indexing:constraint_classification(equal_protection_clause__diversity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RACIALLY ADVANTAGED APPLICANTS (TANGLED ROPE) — Structural position: benefit from race consideration but participation is not genuinely voluntary — applicants cannot reject the consideration mechanism without losing its benefit. Exit options are constrained: can apply elsewhere, but national equal protection law applies uniformly across state universities. Benefits: admissions probability increase (coordination function: building diverse cohorts serves all students). Costs: applicants carry the stigma of being potentially selected for race rather than merit, and the legitimacy of their admission is contested. Mixed experience: real coordination benefit (diverse campus) coupled with extractive stigmatization. Power: moderate (affected by policy but not institutional designers). Time: biographical. Exit: constrained (exit to private universities costs significant tuition). Scope: national. Classification: tangled_rope (genuine coordination function — diverse campus benefits all students — coupled with asymmetric extraction in the form of stigmatization and contested legitimacy).
constraint_indexing:constraint_classification(equal_protection_clause__diversity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIVERSITIES INSTITUTIONAL AUTONOMY (ROPE) — Structural position: net beneficiary of the diversity reading. The constraint permits universities to make race-conscious admissions decisions within a narrow window (race as one factor, compelling interest in educational diversity). This grants institutional discretion — universities can achieve student body diversity without facing strict scrutiny liability. Benefits: legitimate institutional autonomy in admissions policy, educational mission flexibility, ability to build diverse cohorts. Coordination function: the diversity reading coordinates between equal protection (formal racial neutrality normatively required) and educational mission (substantive diversity required by competitive higher education landscape). This is pure coordination — universities see the constraint as solving a collective action problem. Power: institutional (can shape policy, have legal capacity). Time: immediate (can change admissions practices at next cycle). Exit: arbitrage (can switch strategies: pursue diversity through other means, accept less diversity, relocate). Scope: national. Classification: rope (pure coordination; minimal extraction; beneficiary position with high exit capacity).
constraint_indexing:constraint_classification(equal_protection_clause__diversity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS LEGAL COALITION (SCAFFOLD) — Structural position: views the diversity reading as a temporary coalition supporting substantive equality without explicitly adopting a remedial anti-subordination framework. Organized actors (NAACP Legal Defense, civil rights nonprofits) treated the diversity reading as a scaffold — a transition mechanism between colorblind doctrine and fuller remedial justice. The reading provides sunset-logic: it is explicitly justified as temporary (narrowly tailored, not permanent entitlement), and its compression into a narrow window suggests eventual termination. Benefits from framing: avoids committing to anti-subordination theory while achieving some diversity gains. Costs: organized actors must accept that the reading does not address systemic discrimination directly. Power: organized (can mobilize litigation and legal advocacy). Time: generational (civil rights struggles operate over decades). Exit: constrained (civil rights organizations cannot exit equal protection litigation landscape, but can shift strategic focus). Scope: national. Classification: scaffold (provides temporary coordination toward diversity without permanent structural redistribution; theater_ratio moderate reflecting partial reliance on performative diversity metrics).
constraint_indexing:constraint_classification(equal_protection_clause__diversity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DIVERSITY JUSTIFICATION AS PITON (INSTITUTIONAL INERTIA VIEW) — Structural position: the diversity reading is now substantially performative, maintained through institutional inertia rather than functional coherence. Post-SFFA v. Harvard (2023), the narrow window for race consideration has collapsed, and the reading persists as a formal legal category divorced from actual practice. Universities now pursue diversity through proxies (socioeconomic status, first-generation status, geographic diversity) while maintaining the institutional ritual of claiming educational benefit. The diversity justification has become theater — the intellectual framework persists in legal writing, judicial precedent, and institutional policy language, but no longer functions to permit the race-conscious decisions it nominally describes. Power: institutional (courts and universities maintain the language). Time: civilizational (doctrine persists across generations of replacement). Exit: arbitrage (institutions can shift to proxy-based diversity; courts can revise doctrine). Scope: national. Theater ratio: 0.65 (the reading is partially performative — claims educational benefit that are real, but the implementation mechanism has largely shifted to non-racial proxies). Classification: piton (doctrine maintained through institutional inertia, reduced functional capacity, theater-dominant).
constraint_indexing:constraint_classification(equal_protection_clause__diversity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Structural position: from a civilizational and universal perspective, the diversity reading instantiates a genuine but severely constrained coordination mechanism. It coordinates between two competing constitutional mandates: (1) equal protection's formal racial neutrality requirement, and (2) educational institutions' functional need for diverse cohorts. The reading produces this coordination through an explicitly narrow window: race is one factor among many, compelling interest is required, narrow tailoring is demanded. This is tangled rope because genuine coordination exists (serving legitimate educational goals, building diverse campuses benefits all students) coupled with asymmetric extraction in the form of the suppression of remedial equality claims. The reading forecloses the anti-subordination framework, preventing explicit acknowledgment of systemic discrimination as a basis for race-conscious remedy. Power: analytical (observes structure). Time: civilizational (watches how the doctrine evolves across generations). Exit: analytical. Scope: universal (the structure of formal equality vs substantive equality is universal). Classification: tangled_rope (coordination of competing mandates + suppression of alternative framings = hybrid structure with both cooperation and extraction).
constraint_indexing:constraint_classification(equal_protection_clause__diversity_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__diversity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_clause__diversity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_clause__diversity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(equal_protection_clause__diversity_reading, TR),
    TR >= 0.70.

:- end_tests(equal_protection_clause__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.22): Low-moderate. The diversity reading generates modest extractiveness because the claimed coordination benefit (diverse learning environments) is real and substantial — building racially diverse student bodies does serve legitimate educational interests and benefits all students through cognitive diversity, perspective-sharing, and reduced prejudice. However, extractiveness is non-zero because the mechanism imposes costs on racially disadvantaged applicants (reduced admissions probability) without their consent or voice, and the legitimacy of those costs is contested (empirical claim that diversity provides educational benefit is disputed in legal and educational literature). The reading does not permit the full race-conscious remedies that anti-subordination theory would justify, so it forecloses a more direct addressing of systemic discrimination. SUPPRESSION (0.38): Moderate-high. The reading suppresses alternative frameworks: (1) colorblind reading cannot coexist with it (they are in direct competition); (2) remedial reading is foreclosed — the framework permits race consideration only for forward-looking educational benefit, not backward-looking remediation of systemic discrimination. Additionally, the narrow tailoring and strict scrutiny requirements increasingly suppress institutional discretion itself — the window for permissible race consideration has progressively narrowed. Suppressions operate both at the doctrinal level (alternative legal theories) and at the institutional level (narrowing what universities can actually do). THEATER RATIO (0.65): Moderate-high and increasing. The reading relies increasingly on performative elements: (1) diversity metrics (how much diversity is enough?) are arbitrary and unmoored to actual measurement of educational benefit; (2) the claim that race-conscious admissions serve compelling educational interest is theoretically justified but empirically contested; (3) post-SFFA, universities maintain the language of diversity justification while shifting implementation to race-neutral proxies, suggesting the doctrinal framework persists as theater after its functional mechanism has been constrained. The measurement trajectory (0.45 → 0.58 → 0.65) reflects increasing divergence between the reading's stated purpose and its actual institutional effects.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates maximum perspectival divergence across the six classification types. The racially disadvantaged applicant experiences snare (pure extraction with no voice in mechanism). The racially advantaged applicant experiences tangled rope (genuine coordination benefit paired with stigmatization extraction). The university experiences rope (coordination solving competing mandates). The civil rights coalition experiences scaffold (temporary support with sunset logic). The judicial/institutional perspective experiences piton (doctrine maintained through inertia, functional capacity declining). The analytical observer experiences tangled rope with increasing piton characteristics (hybrid coordination-extraction at civilizational scale, with rising theater ratio indicating degradation). The perspectival gap reflects that the reading's classification depends entirely on structural position: beneficiary vs. target, institutional vs. individual, temporal horizon (immediate applicant need vs. generational movement vs. civilizational doctrinal evolution). No single type captures the constraint's full structure across all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for the diversity reading run along a spectrum determined by exit options and beneficiary/victim status. Individual applicants disadvantaged by race consideration: d ≈ 0.95 (trapped, victim) — experience maximum extraction from the constraint. Individual applicants advantaged: d ≈ 0.50-0.60 (constrained, mixed beneficiary-victim) — experience moderate extraction due to stigmatization despite admission benefit. Universities: d ≈ 0.05-0.15 (arbitrage, beneficiary) — experience negative effective extraction (benefit from discretion); the sigmoid function produces f(d) ≈ -0.12 to 0.00, yielding institutional benefit. The derived directionality explains why the same structural constraint produces snare (high d → high f(d) → high χ) from the disadvantaged applicant's perspective and rope (low d → negative f(d) → low/negative χ) from the university's perspective. The beneficiary/victim declarations drive the directionality computation — no overrides are necessary because the structural positions are clear.
 *
 * MANDATROPHY ANALYSIS:
 *   The diversity reading avoids mandatrophy at the classification level by being transparent about its hybrid character: it is explicitly a tangled rope, coordinating between competing constitutional mandates (formal racial neutrality vs. substantive diversity need) while suppressing alternative theories (anti-subordination). However, the reading faces a distinct form of mandatrophy at the temporal level: the measurement trajectory shows rising theater ratio (0.45 → 0.65) and rising extractiveness (0.18 → 0.28), indicating a constraint that is becoming increasingly performative and extractive over time. The narrow window is closing post-SFFA, transforming what was nominally a coordinating framework into an increasingly ceremonial doctrine. The mandatrophy here is not classification confusion but functional degradation — the constraint persists as a formal legal category while its actual institutional effect has been progressively constrained. This is precisely the piton signature: doctrine maintained through institutional inertia (courts cite Grutter, universities maintain diversity language) even as the functional mechanism enabling the reading's operative logic has been suppressed. The reading resolves the classification mandatrophy by being a transparent tangled rope, but it instantiates a temporal mandatrophy: what began as genuine institutional coordination (universities could exercise real discretion in race-conscious admissions) has become increasingly theatrical (universities use race-neutral proxies while maintaining diversity rhetoric) as the doctrine's window has narrowed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrow_window_closure,
    'Has the narrow window for race consideration as one factor among many effectively closed post-SFFA, or does the diversity reading retain doctrinal vitality?',
    'Post-SFFA case law analysis; institutional admissions practice documentation; judicial interpretation of whether the diversity reading survives as a live doctrine or has been relegated to historical precedent',
    'If window closed: classification shifts toward piton (doctrine maintained through inertia). If vitality retained: classification remains tangled_rope with reduced suppression. The temporal trajectory of this constraint''s functional capacity is the key uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrow_window_closure, empirical, 'Whether the narrow window for race consideration remains doctrinally viable post-SFFA').

omega_variable(
    remedial_foreclosure_mechanism,
    'Does the diversity reading (limited race consideration for educational benefit) logically foreclose the remedial reading (race-conscious remedies to dismantle systemic discrimination), or can both readings coexist as different legal theories?',
    'Jurisprudential analysis of whether the diversity framework''s ''compelling interest in educational diversity'' can encompass anti-subordination rationales, or whether the framework structurally prevents remedial justifications from being recognized',
    'If forecloses: the reading_relations is ''forecloses'' (rare, strong relation). If coexists: relation is ''coexists_with'' (different factions hold both). The answer determines whether this reading eliminates alternative frameworks or merely competes with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_foreclosure_mechanism, conceptual, 'Whether diversity justification logically forecloses remedial equality theory').

omega_variable(
    stigma_extraction_mechanism,
    'Does the diversity reading impose a real extraction cost on racially advantaged applicants in the form of stigmatization and contested legitimacy, or is this a theoretical concern absent measurable institutional effects?',
    'Longitudinal psychological research on stigma effects for diversity-admitted students; career outcome analysis; student narrative documentation of legitimacy contests',
    'If real: tangled_rope classification is correct (genuine extraction exists). If theoretical: classification shifts toward rope (coordination without significant extraction). The magnitude of extraction depends on whether stigma is structural or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_extraction_mechanism, empirical, 'Whether diversity consideration imposes measurable stigmatization costs').

omega_variable(
    diverse_learning_environment_coordination_reality,
    'Does the claimed coordination benefit of diverse learning environments (all students benefit from diversity) reflect actual measurable educational outcomes, or is this a contested empirical claim functioning as a legitimating narrative?',
    'Educational research synthesis on peer-group diversity effects; student outcome data correlating diversity metrics with academic achievement, civic engagement, or critical thinking measures',
    'If real coordination benefit: tangled_rope and rope classifications are empirically justified. If contested/unproven: classification shifts toward snare or piton (extraction justified by narrative rather than structural coordination). This is the empirical foundation of the reading''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diverse_learning_environment_coordination_reality, empirical, 'Whether diverse learning environments produce measurable educational benefits for all students').

omega_variable(
    kernel_reading_identity,
    'Is this constraint properly identified as ONE READING of the equal_protection_clause kernel, or does the diversity reading represent a substantively different constitutional commitment from the colorblind and remedial readings?',
    'Constitutional jurisprudence analysis: do all three readings ground legitimacy in the same equal protection text and authority, or do they constitute fundamentally distinct constitutional theories? Do they share a common kernel or represent incommensurable interpretive traditions?',
    'If one kernel with multiple readings: the committer-frame analysis applies — reading_relations, axioms, reference_frame, and drift_state should capture the contested space. If distinct kernels: this story should be reframed as a standalone constraint with its own authority grounding, not a reading of a shared kernel. The story assumes kernel identity; this omega documents the assumption''s contestability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether diversity reading shares a kernel with colorblind and remedial readings or constitutes a separate constitutional framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_div_theater_1978, equal_protection_clause__diversity_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(epc_div_theater_2003, equal_protection_clause__diversity_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(epc_div_theater_2023, equal_protection_clause__diversity_reading, theater_ratio, 45, 0.65).

% Extraction over time
narrative_ontology:measurement(epc_div_extractiveness_1978, equal_protection_clause__diversity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(epc_div_extractiveness_2003, equal_protection_clause__diversity_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(epc_div_extractiveness_2023, equal_protection_clause__diversity_reading, base_extractiveness, 45, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(epc_div_suppression_1978, equal_protection_clause__diversity_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(epc_div_suppression_2003, equal_protection_clause__diversity_reading, suppression_requirement, 25, 0.36).
narrative_ontology:measurement(epc_div_suppression_2023, equal_protection_clause__diversity_reading, suppression_requirement, 45, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__diversity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, strict_scrutiny_doctrine).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, narrow_tailoring_requirement).

% DUAL FORMULATION NOTE:
% The diversity reading is one element within the equal_protection_clause kernel family. Each reading (diversity, colorblind, remedial) instantiates a different interpretation of the same constitutional text and generates different structural consequences (different beneficiary/victim sets, different extraction mechanisms). The three readings coexist as live legal theories competing for judicial adoption; they are not decomposed constraints but rather alternative readings of a single kernel. The constraint stories are linked because they share authority grounding (the constitutional text) and because changes in one reading's doctrinal status affect others (e.g., SFFA v. Harvard's narrowing of the diversity reading influences the apparent strength of the colorblind reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
