% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__anti_subordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__anti_subordination_reading, []).

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
 *   constraint_id: equal_protection_clause__anti_subordination_reading
 *   human_readable: Equal Protection Clause: Anti-Subordination Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The anti-subordination reading of the Equal Protection Clause holds that
 *   state action reinforcing social hierarchies along protected class lines
 *   is constitutionally impermissible, regardless of facial neutrality. This
 *   reading emerged as a critical challenge to formalist equal protection
 *   doctrine in the late 20th century, particularly through the work of civil
 *   rights scholars and progressive judicial coalitions. Unlike the
 *   formal-equality reading (which treats equal protection as requiring
 *   colorblind rules) or the substantive-equality reading (which focuses on
 *   material outcomes), the anti-subordination reading treats the
 *   perpetuation of systemic hierarchy itself as the constitutional injury.
 *   This creates a distinctive structural constraint: state administrative
 *   systems must navigate between facially neutral policy requirements
 *   (inherited institutional frameworks) and prohibitions on subordinating
 *   effects (enforced through judicial review). The anti-subordination
 *   reading distributes authority differently—it grants courts and civil
 *   rights advocates power to override democratically enacted neutral
 *   policies when they subordinate protected groups, while constraining state
 *   agencies' policy discretion. This redistribution of authority, combined
 *   with the reading's dependence on doctrine and the contested meaning of
 *   'subordination,' produces tangled-rope dynamics: genuine coordination
 *   function (dismantling systemic subordination) exists alongside
 *   institutional extraction (judicial override of democratic choices, civil
 *   rights professionalization, doctrine-making power).
 *
 * KEY AGENTS:
 *   - Subordinated Communities (powerless/trapped): Primary victims — bear extraction through subordination perpetuated by facially neutral policies; no direct exit mechanism
 *   - Civil Rights Organizations (organized/constrained): Primary beneficiaries from coordination function; also extract through professionalization, institutional identity, and litigation control
 *   - Progressive Judiciary (institutional/arbitrage): Benefits from anti-subordination framing as legitimacy and power; can modulate enforcement to manage political opposition
 *   - State Administrative Apparatus (institutional/constrained): Constrained by subordination liability exposure while gaining coordination framework for equity compliance
 *   - Conservative Legal Tradition (institutional/arbitrage): Primary victim of reading displacement; maintains formal-equality doctrine through institutional inertia (piton dynamics)
 *   - Analytical Observer (analytical/analytical): Risks naturalizing a contingent institutional arrangement (reading's embedded power) as immutable principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__anti_subordination_reading, 0.52).
domain_priors:suppression_score(equal_protection_clause__anti_subordination_reading, 0.58).
domain_priors:theater_ratio(equal_protection_clause__anti_subordination_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__anti_subordination_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_clause__anti_subordination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equal_protection_clause__anti_subordination_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__anti_subordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__anti_subordination_reading, "Equal Protection Clause: Anti-Subordination Reading").
narrative_ontology:topic_domain(equal_protection_clause__anti_subordination_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_clause__anti_subordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__anti_subordination_reading, '154599e3-ce69-4703-a8d1-11e8d70774ac').
narrative_ontology:cs_kernel_codification('154599e3-ce69-4703-a8d1-11e8d70774ac', formalized).
narrative_ontology:cs_authority_grounding('154599e3-ce69-4703-a8d1-11e8d70774ac', lineage).
narrative_ontology:cs_interpretation_layer_present('154599e3-ce69-4703-a8d1-11e8d70774ac').
narrative_ontology:cs_reading_relation('154599e3-ce69-4703-a8d1-11e8d70774ac', equal_protection_clause__formal_equality_reading, coexists_with).
narrative_ontology:cs_reading_relation('154599e3-ce69-4703-a8d1-11e8d70774ac', equal_protection_clause__substantive_equality_reading, influences).
narrative_ontology:cs_axiom('154599e3-ce69-4703-a8d1-11e8d70774ac', foundational, state_perpetuates_subordination_via_neutral_policy).
narrative_ontology:cs_axiom_status(state_perpetuates_subordination_via_neutral_policy, holdable).
narrative_ontology:cs_axiom_grounding('154599e3-ce69-4703-a8d1-11e8d70774ac', state_perpetuates_subordination_via_neutral_policy, empirically_contingent).
narrative_ontology:cs_axiom('154599e3-ce69-4703-a8d1-11e8d70774ac', foundational, courts_have_affirmative_duty_to_dismantle_subordination).
narrative_ontology:cs_axiom_status(courts_have_affirmative_duty_to_dismantle_subordination, holdable).
narrative_ontology:cs_axiom_grounding('154599e3-ce69-4703-a8d1-11e8d70774ac', courts_have_affirmative_duty_to_dismantle_subordination, deontological).
narrative_ontology:cs_reference_frame('154599e3-ce69-4703-a8d1-11e8d70774ac', constitutional_text_as_mandating_equal_protection_norm).
narrative_ontology:cs_drift_state('154599e3-ce69-4703-a8d1-11e8d70774ac', contemporary_progressive_judicial_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('154599e3-ce69-4703-a8d1-11e8d70774ac', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__anti_subordination_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__anti_subordination_reading, subordinated_groups).
narrative_ontology:constraint_beneficiary(equal_protection_clause__anti_subordination_reading, civil_rights_advocates).
narrative_ontology:constraint_beneficiary(equal_protection_clause__anti_subordination_reading, progressive_judiciary).
narrative_ontology:constraint_victim(equal_protection_clause__anti_subordination_reading, facially_neutral_state_policies).
narrative_ontology:constraint_victim(equal_protection_clause__anti_subordination_reading, institutional_inertia).
narrative_ontology:constraint_victim(equal_protection_clause__anti_subordination_reading, conservative_legal_tradition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED COMMUNITIES (SNARE) — Trapped within systems of de facto subordination maintained through facially neutral state policies (lending discrimination, school funding disparity, healthcare access gaps). The anti-subordination reading promises structural remedy but enforcement is patchy; communities bear extraction through delayed equal protection despite the constitutional text. Maximum experienced extraction because exit from subordination is structurally unavailable and remedy is inconsistent.
constraint_indexing:constraint_classification(equal_protection_clause__anti_subordination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ORGANIZATIONS (TANGLED ROPE) — Constrained by litigation resources, political opposition, and hostile judicial climates, but also benefit from the anti-subordination framing as an organizational identity and litigation theory. Genuine coordination function (enforcing equal protection for marginalized groups) exists alongside extraction (careerism within civil rights institutions, funding competition, professionalization that distances organizations from grassroots constituencies). Moderate extraction with some agency.
constraint_indexing:constraint_classification(equal_protection_clause__anti_subordination_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROGRESSIVE JUDICIARY (ROPE) — Experiences the anti-subordination reading as a coordination framework that aligns their institutional authority with civil rights enforcement. The reading enables judicial power to override facially neutral policies; judges gain legitimacy by vindicating subordinated groups' rights. Net beneficiary with arbitrage options — can modulate enforcement stringency to manage political backlash. Low experienced extraction; coordination predominates.
constraint_indexing:constraint_classification(equal_protection_clause__anti_subordination_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ADMINISTRATIVE APPARATUS (TANGLED ROPE) — State agencies must maintain facially neutral policy frameworks (hiring, contracting, funding allocation) while facing exposure to equal protection challenge for subordinating effects. The anti-subordination reading constrains policy flexibility while also enabling coordination around non-subordinating alternatives (affirmative action, targeted remedies). State institutions experience moderate extraction through litigation risk and prescriptive requirements, offset by coordination benefits of formal equity compliance frameworks.
constraint_indexing:constraint_classification(equal_protection_clause__anti_subordination_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, the anti-subordination reading reflects a natural principle of political legitimacy: governance that perpetuates hierarchy along immutable characteristics is inherently illegitimate. This perspective sees equal protection not as a contingent policy choice but as an irreducible requirement of just governance. However, the structural data reveals this as a false summit: the extraction from conservative legal traditions and institutional inertia, combined with the extractive capacity of the anti-subordination reading itself to override facially neutral rules, indicates a contingent institutional arrangement rather than a natural law.
constraint_indexing:constraint_classification(equal_protection_clause__anti_subordination_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: CONSERVATIVE LEGAL TRADITION (PITON) — The formal-equality reading (colorblind constitutionalism) was once the operative framework for equal protection doctrine. The anti-subordination reading has degraded this formalism into a secondary position, maintained through institutional inertia and textual anchoring rather than as a live functional theory. Conservative legal voices invoke formal equality as a ceremonial appeal while conceding ground on subordination effects. High theater ratio reflects that conservative equal-protection arguments increasingly perform rather than function — the real doctrinal work has shifted to anti-subordination and substantive equality frames.
constraint_indexing:constraint_classification(equal_protection_clause__anti_subordination_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__anti_subordination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_clause__anti_subordination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_clause__anti_subordination_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__anti_subordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(equal_protection_clause__anti_subordination_reading, TR),
    TR >= 0.70.

:- end_tests(equal_protection_clause__anti_subordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The anti-subordination reading empowers courts to override facially neutral state policies when they subordinate protected groups. This represents significant extraction from the state administrative apparatus and from conservative legal tradition (institutional victims), distributed to civil rights advocates and subordinated communities (institutional and powerless beneficiaries). However, extraction is not severe (0.66+) because: (1) the reading requires coordination function (actually dismantling subordination), (2) subordinated communities are victims but may genuinely benefit from the coordination, and (3) state agencies can adapt through equity-focused neutral policies rather than pure capitulation. The upward drift (0.28→0.52 over 30 years) reflects institutionalization: the reading has become embedded in case law, administrative guidance, and institutional expectations, increasing extraction costs for actors resisting it. Suppression (0.58): Moderate-high. Multiple mechanisms suppress alternatives to anti-subordination: doctrinal precedent and circuit lock-in reduce judicial discretion to adopt formal-equality; law school curriculum entrenchment socializes lawyers into subordination theory; civil rights institutional capture of the definitional apparatus constrains how 'subordination' can be contested. But suppression is not total—conservative justices continue to invoke formal equality, facially neutral policy-making persists, and political opposition to subordination doctrine remains organized. Theater ratio (0.65): Moderate-high. Significant performative content exists: constitutional rhetoric emphasizing subordination commitment alongside incremental enforcement; state agency equity compliance frameworks that appear to address subordination without fundamental resource reallocation; civil rights litigation that produces doctrinal victories without material remedy for subordinated communities. The upward drift (0.45→0.65) indicates that theater has increased as the gap between subordination doctrine and subordination outcomes has widened—more ceremonial affirmation of anti-subordination principle alongside structural persistence of hierarchies.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how a single constitutional reading can be experienced as mountain (natural principle), rope (coordination mechanism), snare (extraction via subordination persistence), tangled rope (mixed coordination and institutional extraction), or piton (degraded formalism). Subordinated communities experience snare: they are trapped in subordination despite the constitutional text, facing suppression through facially neutral policies and delayed/inconsistent enforcement. Civil rights organizations experience tangled rope: genuine coordination to dismantle subordination exists alongside extraction through organizational professionalism and funding dependence. Progressive judges experience rope: the anti-subordination reading aligns their institutional authority with civil rights enforcement, producing legitimacy without perceived extraction. The state apparatus experiences tangled rope: constrained by subordination liability but also enabled by equity-compliance coordination frameworks. Conservative legal tradition experiences piton: formal equality has been displaced into ceremonial invocation; the operative doctrine is now anti-subordination, maintained through doctrinal precedent rather than functional justification. The analytical observer risks mountain: seeing anti-subordination as a natural principle of just governance, missing the institutional extraction and doctrinal contingency embedded in the reading's authority structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by beneficiary/victim status and exit options. Subordinated communities are victims with trapped exit → high d (0.95) → high f(d) (1.42) → high experienced extraction. Civil rights organizations are beneficiaries with constrained exit (career dependence on civil rights institutions, funding from civil rights foundations) but also coordinate subordination-dismantling → moderate d (0.45) → moderate f(d) (0.45). Progressive judges are beneficiaries with arbitrage exit (can shift doctrinal emphasis) → low d (0.20) → low f(d) (0.02). State apparatus is mixed: victim of doctrinal override (high d component) but also beneficiary of equity-compliance coordination (low d component) → derived d ≈ 0.50 (constrained exit dominates). The piton perspective derives from theater gate (0.65) rather than from extreme chi values. The analytical observer's canonical d (0.73) produces mountain, but the false-summit detector flags this as naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   COORDINATE RESOLUTION: The anti-subordination reading is genuinely hybrid—it coordinates the dismantling of systemic hierarchy while extracting institutional authority from democratic processes and state policy discretion. The mandatrophy dissolves when we recognize that the reading's legitimacy depends on whether the coordination function (addressing subordination) is real and whether the extraction (doctrinal override) is proportionate. If anti-subordination doctrine actually produces material improvements in subordinated-group outcomes, the tangled rope classification holds: the extraction is justified by coordination benefit. If the doctrine produces ceremonial compliance without material remedy (theater ratio at 0.65+ with no corresponding subordination reduction), the constraint converts toward snare as pure extraction. The measurements track this: if extractiveness continues rising while subordination outcomes stagnate, the coordinating function is illusory and the constraint should reclassify upward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretation_boundary,
    'Does the constitutional text (''nor deny to any person equal protection of the laws'') mandate the anti-subordination reading, permit multiple readings, or foreclose it?',
    'Textual analysis of historical ratification debates, original public meaning evidence, subsequent amendment history; comparison with how foundational constitutional clauses have been interpreted across eras',
    'If mandated: anti-subordination reading is natural law status and mountain classification. If permitted: tangled_rope (coordination of subordinated groups + institutional extraction via doctrine) is correct. If foreclosed: sibling formal_equality reading becomes the binding frame and anti-subordination converts to snare (extraction without coordination benefit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_interpretation_boundary, conceptual, 'Whether the Equal Protection text mandates, permits, or forecloses the anti-subordination reading').

omega_variable(
    remedial_enforceability_gap,
    'Can the anti-subordination reading produce enforceable remedies for systemic subordination, or does it function primarily as a legitimacy framing for incrementalist change?',
    'Longitudinal tracking of anti-subordination precedents: proportion that result in actual policy reversal vs. ceremonial reaffirmation; comparison of subordination outcomes before and after key decisions; measurement of compliance costs for state agencies and resource reallocation to subordinated groups',
    'If enforceable: tangled_rope classification confirmed. If primarily legitimacy framing: extractiveness rises to 0.62–0.70 (snare boundary), converting the constraint to snare from most perspectives as the reading provides ideological cover without material remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_enforceability_gap, empirical, 'Whether anti-subordination doctrine produces enforceable remedies or functions primarily as legitimacy framing').

omega_variable(
    subordination_definition_instability,
    'Is ''subordination'' a stable legal category or does the concept shift across time and political contexts, creating ambiguity about what the anti-subordination reading actually constrains?',
    'Doctrinal mapping of how courts define subordination across cases; identification of shifts in which groups are deemed subordinated and which policies are deemed subordinating; comparison of subordination findings across different judicial coalitions with similar factual records',
    'If stable: anti-subordination reading has determinate content. If unstable: the reading functions as a discretionary grant of power to judges to override facially neutral rules (extractiveness rises to 0.65+), converting toward snare classification as judicial discretion becomes the real constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_definition_instability, empirical, 'Stability and determinacy of subordination as a legal category').

omega_variable(
    institutional_capture_risk,
    'Does the anti-subordination reading risk capture by state institutions seeking to override laws and policies through judicial override, converting subordination theory into a tool for institutional aggrandizement rather than subordinated-group protection?',
    'Analysis of state actors (executives, legislatures) invoking subordination doctrine to override civil rights protections or majority-protected entitlements; examination of whether subordination doctrine has been used asymmetrically to protect powerful groups against accountability measures',
    'If low risk: tangled_rope classification holds. If high risk: the reading''s extractiveness increases (0.65+) as the coordination function (subordinated-group protection) is displaced by institutional extraction (state power grab), converting the constraint to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_risk, empirical, 'Risk that anti-subordination doctrine is captured by state institutions for purposes other than protecting subordinated groups').

omega_variable(
    reading_displacement_via_reinterpretation,
    'Can the formal_equality reading be reinstated as the binding frame through judicial reinterpretation, or has the anti-subordination reading become sufficiently embedded in institutional practice that reversal requires explicit repudiation?',
    'Historical tracking of equal protection doctrine: identify reversals of prior readings (e.g., Lochner''s overthrow, de facto desegregation rollback via intent requirement); assess current institutional lock-in via circuit precedent, Restatement adoption, law school curriculum entrenchment, and litigant expectations',
    'If readily reversible: sibling formal_equality reading remains live and coexists_with anti-subordination (reading_relation=coexists_with). If locked in: anti-subordination reading has institutionally foreclosed formal_equality within progressive judicial contexts (reading_relation=forecloses, directional), creating path-dependent doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_displacement_via_reinterpretation, empirical, 'Institutional lock-in and reversibility of anti-subordination reading relative to formal equality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__anti_subordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_anti_sub_tr_t0, equal_protection_clause__anti_subordination_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(epc_anti_sub_tr_t15, equal_protection_clause__anti_subordination_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(epc_anti_sub_tr_t30, equal_protection_clause__anti_subordination_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(epc_anti_sub_be_t0, equal_protection_clause__anti_subordination_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(epc_anti_sub_be_t15, equal_protection_clause__anti_subordination_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(epc_anti_sub_be_t30, equal_protection_clause__anti_subordination_reading, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__anti_subordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__anti_subordination_reading, equal_protection_clause__formal_equality_reading).
narrative_ontology:affects_constraint(equal_protection_clause__anti_subordination_reading, equal_protection_clause__substantive_equality_reading).
narrative_ontology:affects_constraint(equal_protection_clause__anti_subordination_reading, strict_scrutiny_doctrine).
narrative_ontology:affects_constraint(equal_protection_clause__anti_subordination_reading, colorblindness_principle).

% DUAL FORMULATION NOTE:
% The anti-subordination reading is one reading of the contested equal_protection_clause kernel. The formal-equality reading instantiates a different constraint with different ε (likely 0.15-0.25, mountain/rope from different perspectives). The substantive-equality reading instantiates a third constraint with ε in the 0.35-0.45 range. Each reading has its own beneficiary/victim structure, its own authority distribution, and its own perspectival gap. These are not the same constraint viewed from different angles; they are different constraints arising from different interpretations of the same text. Network edges link them via affects_constraints to enable contamination and displacement analysis: the anti-subordination reading's institutional embedding affects the plausibility and enforceability of the formal-equality reading and creates structural pressure toward substantive-equality approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__anti_subordination_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
