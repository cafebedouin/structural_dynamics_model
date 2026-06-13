% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: Functional Accommodation War Powers Allocation (Context-Dependent Authority)
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   The functional accommodation reading of the war powers kernel asserts
 *   that executive unilateral force deployment is constitutional when
 *   directed at imminent threats but requires congressional authorization for
 *   prolonged campaigns. This reading is the live boundary dispute in
 *   contemporary US constitutional practice. It is claimed as tangled rope
 *   (genuine coordination between branches, each with legitimate authority
 *   within its domain) while authored metrics describe substantial extraction
 *   (executive gains practical force-initiation authority), suppression (the
 *   reading suppresses categorical rules and creates ambiguity where text
 *   appears clear), and theater (the deliberative constraint supposed to
 *   limit war-initiation is retroactively applied, after troops are
 *   deployed). The measurement series shows extractiveness rising from 0.48
 *   to a plateau at 0.62, theater rising from 0.28 to 0.44 and stabilizing,
 *   and suppression rising from 0.62 to a plateau at 0.71, consistent with a
 *   constraint maturing from nascent practice to normalized institutional
 *   operation.
 *
 * KEY AGENTS:
 *   - Executive Commander: Interprets imminent threat broadly; deploys force first; seeks authorization after; controls escalation/de-escalation
 *   - Congress: Retains war declaration and appropriations powers; operates within fait accompli constraint; ratifies executive action or withdraws support at political cost
 *   - Courts: Treat war powers disputes as non-justiciable; decline to enforce constitutional boundary; defer to whichever branch acts first
 *   - Deliberative Democratic Process: Supposed beneficiary of constitutional deliberation requirement; converted to post-hoc ratification mechanism by functional accommodation reading
 *   - Military Personnel & Families: Trapped beneficiaries; gain command clarity but bear cost of contested authorization
 *   - Affected Foreign Populations: Excluded from deliberation; bear cost of force deployment; have no seat in constitutional apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.62).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.71).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "Functional Accommodation War Powers Allocation (Context-Dependent Authority)").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, 'c4b9be55-eaf1-4686-837e-2874ef91459a').
narrative_ontology:cs_kernel_codification('c4b9be55-eaf1-4686-837e-2874ef91459a', fixed_text).
narrative_ontology:cs_authority_grounding('c4b9be55-eaf1-4686-837e-2874ef91459a', extraction).
narrative_ontology:cs_interpretation_layer_present('c4b9be55-eaf1-4686-837e-2874ef91459a').
narrative_ontology:cs_reading_relation('c4b9be55-eaf1-4686-837e-2874ef91459a', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4b9be55-eaf1-4686-837e-2874ef91459a', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('c4b9be55-eaf1-4686-837e-2874ef91459a', foundational, context_dependent_war_authority_allocation).
narrative_ontology:cs_axiom_status(context_dependent_war_authority_allocation, holdable).
narrative_ontology:cs_axiom_grounding('c4b9be55-eaf1-4686-837e-2874ef91459a', context_dependent_war_authority_allocation, instrumental).
narrative_ontology:cs_axiom('c4b9be55-eaf1-4686-837e-2874ef91459a', secondary, modern_threat_speed_requires_executive_flexibility).
narrative_ontology:cs_axiom_status(modern_threat_speed_requires_executive_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('c4b9be55-eaf1-4686-837e-2874ef91459a', modern_threat_speed_requires_executive_flexibility, empirically_contingent).
narrative_ontology:cs_reference_frame('c4b9be55-eaf1-4686-837e-2874ef91459a', constitutional_text_war_declaration_supremacy).
narrative_ontology:cs_drift_state('c4b9be55-eaf1-4686-837e-2874ef91459a', post_cold_war_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c4b9be55-eaf1-4686-837e-2874ef91459a', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_operational_flexibility).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congressional_war_declaration_authority).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, deliberative_constraint_on_force).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.62) because the reading transfers practical war-initiation authority from Congress to the Executive by permitting unilateral action in the gray zone between imminent and prolonged conflicts. Suppression is higher (0.71) because the reading's persistence depends on maintaining ambiguity about where the imminent/prolonged boundary falls—categorical clarity would either permit or prohibit unilateral action, resolving the dispute. Theater is moderate (0.44) because the functional accommodation reading retains genuine deliberative elements (Congress still votes on extended campaigns, still holds appropriations power) alongside the extractive elements (executive deploys first, establishes facts on the ground, reverses the constitutional default from authorization-first to authorization-last). The measurements show extractiveness rising early (0.48 to 0.63 over 40 time units) as the reading becomes normalized institutional practice, then stabilizing, consistent with a constraint maturing into predictable operation. Theater rises from deliberation-heavy (0.28) to performance-heavy (0.44), consistent with the constraint's function shifting from genuine coordination between branches to executive fait accompli followed by legislative ratification.
 *
 * PERSPECTIVAL GAP:
 *   The executive institutional actor and the congressional institutional actor compute different types from the same structural data. From the executive perspective: the functional accommodation reading is genuine coordination that acknowledges the impossibility of legislative deliberation under modern threat conditions—a rope solving a real collective-action problem (speed vs. legitimacy). From the congressional perspective: the reading is tangled rope (asymmetric extraction of war-initiation authority from Congress) or snare (the imminent/prolonged distinction is rhetorical cover for de facto executive primacy). Courts, observing, compute neither type because they decline jurisdiction. Deliberative democracy and military personnel compute the constraint as extractive and suppressive—they lose decision rights and bear consequences. The engine's per-seat computation will surface these divergences from the structural data (executive benefits, Congress pays, courts observe, democracy is both beneficiary and victim). The perspectival gap is not an error; it is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive branch institutional actors have d near 0.2 (beneficiary end): they gain practical force-initiation authority, control operational decisions, face low exit cost for unilateral action. Congressional institutional actors have d near 0.7 (target end): they retain nominal authority but lose effective control, must respond to executive fait accompli, face high political cost for withdrawing appropriations. Courts have d = 0.5 (observational, symmetric position). Deliberative democracy has d near 1.0 (full target): loses the decision rights the constraint is supposed to protect. Military personnel have d near 0.8 (target, trapped): deployed into conflicts whose authorization is contested, unable to refuse, unable to influence the constitutional reading. The functional accommodation reading produces this directionality distribution by creating executive discretion and congressional reactive authority—the executive moves first, Congress responds.
 *
 * MANDATROPHY ANALYSIS:
 *   The functional accommodation reading's mandate is to solve the coordination problem between legislative deliberation speed and executive operational speed under modern threat conditions. The reading has not reached mandatrophy: the founding problem (constitutional ambiguity exposed by modern conditions) remains live and contested. The measurement series shows the reading maturing into stable institutional operation (theater and suppression plateau), not degrading into performance maintenance. However, the reading exhibits a mandatrophy risk: as the imminent/prolonged boundary becomes normalized practice, the mandate (genuine context-dependent allocation) risks becoming theater while the function (executive primacy) becomes the operative constraint. The omega variable on gray-zone suppression addresses this risk: if suppression is internalized consensus (both branches know the boundary is rhetorical), the constraint is transitioning toward piton (attenuated mandate, inertial persistence). The current classification as tangled rope assumes the boundary is genuinely ambiguous and contested; if evidence shows it is strategically maintained theater, reclassification to snare would be warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminent_threat_definition_boundary,
    'What constitutes an ''imminent threat'' that permits unilateral executive action under the functional accommodation reading, and who determines the boundary?',
    'Case law establishing binding thresholds; legislative definition of ''imminent threat'' in authorization statutes; executive branch legal opinions establishing precedent. Examine whether courts review imminence determinations or defer to executive judgment.',
    'A narrow, judicially-reviewable definition of imminence constrains executive unilateralism; a broad, executive-determined definition effectively grants unlimited discretion. The boundary''s location determines whether the functional accommodation is genuine coordination or de facto executive primacy with theater of congressional consultation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminent_threat_definition_boundary, empirical, 'Whether the imminent threat boundary is justiciable and where it is drawn.').

omega_variable(
    gray_zone_suppression_mechanism,
    'Is the gray zone between imminent defense and prolonged campaigns a genuine structural ambiguity, or is it suppressed knowledge that both branches exploit strategically?',
    'Examine legislative and executive framing in classified and unclassified contexts: if both branches privately acknowledge the boundary as arbitrary but publicly frame it as settled law, suppression is internalized consensus; if they genuinely dispute where the line falls, suppression is externalized (the reading itself suppresses categorical clarity). Interview legislative counsel and executive branch lawyers on off-record framing.',
    'If suppression is internalized consensus, the functional accommodation reading is a shared cover story for a de facto executive war power, and the constraint is snare-like (both branches know the boundary is rhetorical but maintain it theatrically). If suppression is externalized structural ambiguity, the constraint is genuinely tangled rope (both branches claim authority within honest uncertainty).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gray_zone_suppression_mechanism, empirical, 'Whether gray-zone ambiguity is genuine or strategically maintained.').

omega_variable(
    reading_versus_alternative_kernel_framings,
    'Is the functional accommodation reading a legitimate constitutional interpretation, or is it a constructed reading that legitimizes practices that would be clearly prohibited under the congressional_primacy_reading?',
    'Examine constitutional text, founding-era framing, and 19th-century practice to establish which reading aligns with original understanding; examine 20th-century statutory law (War Powers Resolution, AUMF mechanisms) to establish whether Congress has intentionally endorsed the functional accommodation framing or rejected it.',
    'If the reading aligns with original understanding, it is a legitimate constitutional accommodation; if it deviates, it is a constructed justification for executive aggrandizement. The impact is on whether the constraint should be classified as tangled rope (both readings legitimate) or snare (one reading falsely presented as natural law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_versus_alternative_kernel_framings, conceptual, 'Whether the functional accommodation reading is a legitimate constitutional interpretation or a constructed post-hoc rationalization.').

omega_variable(
    context_dependency_versus_categorical_authority,
    'Does the functional accommodation reading''s context-dependency (imminent vs. prolonged) represent a genuine structural necessity, or does it create regulatory capture opportunity for whichever branch can define contexts?',
    'Examine historical and contemporary cases: (1) How often does an initiated conflict get classified as imminent defense versus prolonged campaign? (2) Which branch controls the classification? (3) Do courts review the classification? (4) Has the classification ever shifted mid-conflict? Track whether context definitions track actual operational features or political convenience.',
    'If context definitions track real operational features and are subject to review, the accommodation is genuine coordination. If context definitions are strategically chosen by the executive and reviewed only by the executive, the context-dependency becomes a mechanism for suppressing the categorical rule. High extraction and high theater both point toward the latter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(context_dependency_versus_categorical_authority, empirical, 'Whether context-dependent allocation genuinely reflects structural necessity or enables regulatory capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__functional_accommodation_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__functional_accommodation_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__functional_accommodation_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__functional_accommodation_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__functional_accommodation_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(war__tr_t60, war_powers_allocation__functional_accommodation_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(war__tr_t70, war_powers_allocation__functional_accommodation_reading, theater_ratio, 70, 0.44).
narrative_ontology:measurement(war__tr_t80, war_powers_allocation__functional_accommodation_reading, theater_ratio, 80, 0.44).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(war__be_t60, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement(war__be_t70, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 70, 0.62).
narrative_ontology:measurement(war__be_t80, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 80, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(war__su_t60, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(war__su_t70, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 70, 0.71).
narrative_ontology:measurement(war__su_t80, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 80, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__functional_accommodation_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, war_powers_allocation__inherent_executive_reading).

% DUAL FORMULATION NOTE:
% The three readings of the war_powers_allocation kernel form a constraint family representing the spectrum of executive authority interpretation. The functional_accommodation_reading represents the middle position: narrower than inherent_executive (which grants broad unilateral authority) and broader than congressional_primacy (which requires authorization for all wars). The ε-invariance principle applies: each reading instantiates a different constraint with a different structural ε because the reading determines which branch controls war initiation, making the constraints' fundamental structures different. The functional_accommodation_reading's ε (0.62) reflects moderate executive extraction of war authority; the congressional_primacy_reading would have lower ε (executive more constrained); the inherent_executive_reading would have higher ε (executive less constrained). The network edges propagate this structural coupling: changes to congressional primacy or inherent executive authority interpretations create downstream pressure on the functional accommodation reading's legitimacy and operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__functional_accommodation_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
