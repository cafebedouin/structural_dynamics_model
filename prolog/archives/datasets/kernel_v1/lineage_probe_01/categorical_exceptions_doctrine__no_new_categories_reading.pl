% ============================================================================
% CONSTRAINT STORY: categorical_exceptions_doctrine__no_new_categories_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_categorical_exceptions_no_new_categories, []).

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
 *   constraint_id: categorical_exceptions_doctrine__no_new_categories_reading
 *   human_readable: Categorical Exceptions Doctrine: No New Categories Reading (Stevens/Brown v. Entertainment Merchants)
 *   domain: constitutional_law/free_speech
 *
 * SUMMARY:
 *   In Brown v. Entertainment Merchants Association (2011), Justice Scalia
 *   rejected the state's proposal for a new First Amendment category (violent
 *   video games sold to minors) and held that novel harmful-content
 *   categories cannot be created through cost-benefit analysis of empirical
 *   harm. Instead, new categories would need historical pedigree — a
 *   founding-era or long-established regulatory tradition. This is the
 *   'no-new-categories reading' of the categorical-exceptions doctrine: the
 *   First Amendment's list of unprotected categories (incitement, obscenity,
 *   child sexual abuse material, true threats, fighting words) is closed. The
 *   constraint creates asymmetric extraction: established categories remain
 *   available to regulation; novel harm-based proposals face strict scrutiny
 *   and fail. Justice Stevens had previously urged cost-benefit balancing as
 *   the mechanism; his foreclosure at the categorical boundary is the
 *   reading's core. The constraint exhibits tangled-rope structure: it
 *   provides genuine coordination benefit (predictability, clear judicial
 *   boundaries, reduced arbitrary enforcement) alongside significant
 *   extraction (novel harm discourse suppressed; future regulatory adaptation
 *   barred unless historical analogue exists). The suppression value (0.68)
 *   reflects the doctrinal barrier: proposing new categories requires
 *   overcoming established precedent, strict-scrutiny standards, and judicial
 *   preference for historical closure. The extractiveness (0.52) reflects
 *   that the beneficiary (entertainment industry, expression creators) gains
 *   substantial freedom to produce previously-unregulated content (violent
 *   games, animal-cruelty simulations) without facing new categorical
 *   restrictions, but the extraction is not total because existing categories
 *   (obscenity, incitement) remain available, providing some regulatory
 *   pathway.
 *
 * KEY AGENTS:
 *   - Harm-Based Carve-Out Advocates: Primary victim (powerless/trapped) — cannot propose novel regulatory categories justified by empirical harm without facing strict scrutiny. No exit mechanism.
 *   - Public Health and Child Protection Advocates: Secondary victim (moderate/constrained) — must work within existing categories (obscenity, incitement); foreclosed from proposing category expansion for emerging harms (algorithmic effects, parasocial attachment). Coalition-building constrained.
 *   - Entertainment Industry and Expression Creators: Primary beneficiary (institutional/arbitrage) — unlimited freedom to produce novel disturbing content (violent games, realistic animal cruelty depictions) because no new categories can be created to restrict it. Maximum arbitrage capacity.
 *   - Federal Judiciary: Institutional beneficiary (institutional/arbitrage) — the no-new-categories rule reduces judicial discretion (appears principle-based, avoiding ad hoc balancing) while simultaneously protecting existing beneficiary positions. Doctrine clarity serves institutional interests.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent doctrinal choice (history-only closure) as a logical necessity of First Amendment structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(categorical_exceptions_doctrine__no_new_categories_reading, 0.52).
domain_priors:suppression_score(categorical_exceptions_doctrine__no_new_categories_reading, 0.68).
domain_priors:theater_ratio(categorical_exceptions_doctrine__no_new_categories_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(categorical_exceptions_doctrine__no_new_categories_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(categorical_exceptions_doctrine__no_new_categories_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(categorical_exceptions_doctrine__no_new_categories_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(categorical_exceptions_doctrine__no_new_categories_reading, tangled_rope).
narrative_ontology:human_readable(categorical_exceptions_doctrine__no_new_categories_reading, "Categorical Exceptions Doctrine: No New Categories Reading (Stevens/Brown v. Entertainment Merchants)").
narrative_ontology:topic_domain(categorical_exceptions_doctrine__no_new_categories_reading, "constitutional_law/free_speech").

domain_priors:requires_active_enforcement(categorical_exceptions_doctrine__no_new_categories_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(categorical_exceptions_doctrine__no_new_categories_reading, 'b226b8c2-a737-43c1-86d8-0e423e8a4e51').
narrative_ontology:cs_kernel_codification('b226b8c2-a737-43c1-86d8-0e423e8a4e51', formalized).
narrative_ontology:cs_authority_grounding('b226b8c2-a737-43c1-86d8-0e423e8a4e51', lineage).
narrative_ontology:cs_interpretation_layer_present('b226b8c2-a737-43c1-86d8-0e423e8a4e51').
narrative_ontology:cs_reading_relation('b226b8c2-a737-43c1-86d8-0e423e8a4e51', categorical_exceptions_doctrine__incitement_brandenburg_reading, coexists_with).
narrative_ontology:cs_reading_relation('b226b8c2-a737-43c1-86d8-0e423e8a4e51', categorical_exceptions_doctrine__obscenity_miller_reading, coexists_with).
narrative_ontology:cs_axiom('b226b8c2-a737-43c1-86d8-0e423e8a4e51', foundational, categorical_closure_by_history_only).
narrative_ontology:cs_axiom_status(categorical_closure_by_history_only, holdable).
narrative_ontology:cs_axiom_grounding('b226b8c2-a737-43c1-86d8-0e423e8a4e51', categorical_closure_by_history_only, conventional).
narrative_ontology:cs_axiom('b226b8c2-a737-43c1-86d8-0e423e8a4e51', foundational, judicial_discretion_containment_via_historicity).
narrative_ontology:cs_axiom_status(judicial_discretion_containment_via_historicity, holdable).
narrative_ontology:cs_axiom_grounding('b226b8c2-a737-43c1-86d8-0e423e8a4e51', judicial_discretion_containment_via_historicity, instrumental).
narrative_ontology:cs_reference_frame('b226b8c2-a737-43c1-86d8-0e423e8a4e51', categorical_closure_by_history_establishment).
narrative_ontology:cs_drift_state('b226b8c2-a737-43c1-86d8-0e423e8a4e51', contemporary_post_brown_implementation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b226b8c2-a737-43c1-86d8-0e423e8a4e51', '').
narrative_ontology:cs_kernel_id(categorical_exceptions_doctrine__no_new_categories_reading, categorical_exceptions_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(categorical_exceptions_doctrine__no_new_categories_reading, novel_disturbing_expression_creators).
narrative_ontology:constraint_victim(categorical_exceptions_doctrine__no_new_categories_reading, harm_based_carve_out_proposals).
narrative_ontology:constraint_victim(categorical_exceptions_doctrine__no_new_categories_reading, future_regulatory_adaptation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HARM-BASED CARVE-OUT ADVOCATES (SNARE) — Trapped by the categorical-closure rule: unable to propose new content categories justified by empirical harm without triggering strict scrutiny. Must navigate between abandoning harm-based arguments (conceding epistemic ground) or mounting full constitutional defense (resource-intensive, rarely succeeds). No exit mechanism. Targeted by the constraint's core function: suppressing the very regulatory strategy they would employ.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__no_new_categories_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH AND CHILD PROTECTION ADVOCATES (TANGLED ROPE) — Experience genuine coordination benefit (clear lines reduce arbitrary enforcement; predictability enables organizing around established categories like obscenity). Also bear extraction cost: foreclosed from proposing novel categories (violent media, sexualization of children in non-obscene contexts) even when new empirical evidence emerges. Constrained by both the rule and the coalition-building costs of working within closed categories.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__no_new_categories_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENTERTAINMENT INDUSTRY AND EXPRESSION CREATORS (ROPE) — Primary beneficiary (institutional/arbitrage). The no-new-categories rule enables unlimited production of previously-unregulated content (violent games, realistic animal cruelty simulations) by making category expansion judicially disfavored. Can arbitrage between state legislatures (which cannot create new categories) and federal protection (which bars category addition). Experiences constraint as pure coordination: predictable boundaries reduce regulatory uncertainty and enable long-term planning.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__no_new_categories_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / HISTORY-AS-LAW VIEW (MOUNTAIN) — From civilizational scope, the categorical-closure rule appears as an immutable consequence of First Amendment logic: once you allow judges to invent new categories, you cannot credibly constrain their discretion without importing history as an ordering principle. History becomes the only non-arbitrary anchor. However, this naturalization (Mountain classification) masks a contingent doctrinal choice: Stevens could have adopted cost-benefit balancing (as the Entertainment Merchants Association urged), or adopted harm-prediction criteria, or adopted democratic-process review. The 'history-only' principle is not a logical necessity — it is a institutional power arrangement protecting existing categories.
constraint_indexing:constraint_classification(categorical_exceptions_doctrine__no_new_categories_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(categorical_exceptions_doctrine__no_new_categories_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(categorical_exceptions_doctrine__no_new_categories_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(categorical_exceptions_doctrine__no_new_categories_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(categorical_exceptions_doctrine__no_new_categories_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(categorical_exceptions_doctrine__no_new_categories_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The entertainment industry gains substantial unconstrained production capacity because new harm-based categories cannot be created. The extraction is not total (existing categories remain) and partially justified as coordination (predictable boundaries reduce arbitrary enforcement). The measurement trajectory shows extractiveness rising at t=7 (post-Brown implementation) as the blocked-category consequence became clear, then stabilizing. Theater ratio (0.58): Moderate-high. The 'history-only' principle creates performative justification work: determining which historical pedigrees 'count' requires implicit legitimacy assessments that the doctrine obscures. The categorical closure appears principle-based (preventing judicial discretion) but actually functions to preserve existing-category beneficiaries. Theater rises from t=0 (pre-Brown, when balancing was plausible) to t=7+ (post-Brown, when 'history' becomes the sole acceptable anchor, even as 'which history counts' remains contestable). Suppression (0.68): High. The doctrinal barrier to novel category creation is substantial: proposing a new category requires overcoming established precedent, satisfying strict scrutiny, and persuading courts that no existing category covers the harm. The barrier increased post-Brown as Stevens's closure became binding precedent. This suppression is non-contingent on the proposer's power level — harm-based carve-out advocates face the same doctrinal barrier regardless of resources or political salience.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    history_as_constraint_vs_history_as_narrative,
    'Does the categorical-closure rule enforce history as an objective structural constraint on categories, or does it enforce a particular reading of which historical categories ''count'' as legitimate?',
    'Comparative analysis of how the doctrine treats analogous historical categories: does it accept all categories that have a historical pedigree, or only those that meet implicit legitimacy standards (public concern, viewpoint-neutral application, settled enforcement)? Counterfactual: would a historically-pedigreed but newly-revived category (e.g., seditious libel, if revived from colonial law) gain protection under this reading?',
    'If history is objective constraint: the rule is principle-based and genuinely limits judicial discretion (rope-like stability). If history is selective narrative: the rule is a cover story for preserving existing beneficiary positions while barring new carve-outs (tangled_rope or snare-like extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(history_as_constraint_vs_history_as_narrative, conceptual, 'Whether history functions as objective constraint or selective narrative').

omega_variable(
    cost_benefit_balancing_foreclosure,
    'Why did Stevens foreclose cost-benefit balancing as the mechanism for future category creation? Is this foreclosure logically necessary or doctrinally contingent?',
    'Textual analysis of Stevens''s reasoning in Brown v. EMA (2011); comparison to contemporary doctrinal treatments of balancing in other First Amendment contexts (national security, defamation); examination of whether closed-list approaches exist in domains without historical pedigree anchors (e.g., speech regulation outside First Amendment).',
    'If foreclosure is logically necessary: the reading is stable and defensible as a principle. If contingent: the reading is vulnerable to revisionist cost-benefit proposals and represents an institutional choice to protect established categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_balancing_foreclosure, conceptual, 'Logical necessity vs. doctrinal contingency of foreclosing cost-benefit balancing').

omega_variable(
    future_harm_empiricism_uncapturable,
    'As empirical evidence emerges about novel harms (e.g., neuroplasticity effects of violent media, parasocial attachment effects of algorithmic recommendation), can this evidence credibly fit within existing historical categories, or does the categorical closure systematically exclude novel harm mechanisms?',
    'Longitudinal analysis of whether post-2011 harm evidence has been successfully integrated into existing categories (obscenity, incitement) or forced into novel category proposals that face strict scrutiny. Documentation of cases where plaintiffs attempted to use new empirical data to expand existing categories vs. create new ones.',
    'If new harm evidence can fit existing categories: the categorical closure is adaptive and not extractive. If new harm evidence is systematically foreclosed: the constraint shows extraction signature (novel harm-based proposals suppressed; expression creators benefit from empirical unprovability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_harm_empiricism_uncapturable, empirical, 'Whether new harm empiricism can be integrated into existing historical categories').

omega_variable(
    reading_contingency_on_stevens_institutional_position,
    'Is the no-new-categories reading a principled doctrinal position, or a rationalization of the institutional preferences of the particular justices (especially Stevens) who adopted it? What would the reading look like if authored by harm-reduction advocates rather than by institutional guardians of existing First Amendment doctrine?',
    'Comparative study of how this reading is defended in judicial opinions vs. how it is critiqued in law review literature. Analysis of whether the reading''s beneficiaries (entertainment industry) align with the institutional coalitions (federal judiciary, speech-protective doctrine) that benefit from categorical closure.',
    'If principled: the reading represents a genuine constitutional insight. If institutionally contingent: the reading should be classified as extraction (novel harm discourse suppressed; beneficiary is expression-industry coalition aligned with judicial institutional interests).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contingency_on_stevens_institutional_position, conceptual, 'Whether reading is principled doctrine or institutional preference rationalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(categorical_exceptions_doctrine__no_new_categories_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catexc_tr_t0, categorical_exceptions_doctrine__no_new_categories_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(catexc_tr_t7, categorical_exceptions_doctrine__no_new_categories_reading, theater_ratio, 7, 0.58).
narrative_ontology:measurement(catexc_tr_t14, categorical_exceptions_doctrine__no_new_categories_reading, theater_ratio, 14, 0.58).

% Extraction over time
narrative_ontology:measurement(catexc_be_t0, categorical_exceptions_doctrine__no_new_categories_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(catexc_be_t7, categorical_exceptions_doctrine__no_new_categories_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(catexc_be_t14, categorical_exceptions_doctrine__no_new_categories_reading, base_extractiveness, 14, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(catexc_su_t0, categorical_exceptions_doctrine__no_new_categories_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(catexc_su_t7, categorical_exceptions_doctrine__no_new_categories_reading, suppression_requirement, 7, 0.68).
narrative_ontology:measurement(catexc_su_t14, categorical_exceptions_doctrine__no_new_categories_reading, suppression_requirement, 14, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(categorical_exceptions_doctrine__no_new_categories_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(categorical_exceptions_doctrine__no_new_categories_reading, categorical_exceptions_doctrine__incitement_brandenburg_reading).
narrative_ontology:affects_constraint(categorical_exceptions_doctrine__no_new_categories_reading, categorical_exceptions_doctrine__obscenity_miller_reading).

% DUAL FORMULATION NOTE:
% The no-new-categories reading is one of three structurally distinct constraints within the categorical-exceptions doctrine kernel. The incitement reading (Brandenburg) and obscenity reading (Miller) are sibling readings with different ε values and beneficiary structures. All three readings share the same kernel (First Amendment categorical exceptions) but instantiate different closure mechanisms and extraction patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
