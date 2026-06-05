% ============================================================================
% CONSTRAINT STORY: colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colorblind_reading, []).

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
 *   constraint_id: colorblind_reading
 *   human_readable: Colorblind Reading of Equal Protection: Categorical Prohibition on Race-Conscious State Action
 *   domain: constitutional_law/civil_rights/education_policy
 *
 * SUMMARY:
 *   The colorblind reading of the Fourteenth Amendment's Equal Protection
 *   Clause establishes that state use of racial classifications is
 *   categorically prohibited regardless of purpose — even remedial purpose
 *   addressing effects of past discrimination. This is ONE READING of the
 *   contested equal protection kernel. The kernel itself is the interpretive
 *   text: 'No State shall... deny to any person within its jurisdiction the
 *   equal protection of the laws.' The colorblind reading instantiates one
 *   normative interpretation of this text: that 'equal protection' mandates
 *   formal identity of treatment, prohibiting all state racial
 *   classification. Sibling readings instantiate alternative interpretations:
 *   the remedial reading emphasizes that equal protection requires
 *   affirmative remedy for effects of past discrimination; the
 *   antisubordination reading emphasizes that equal protection requires
 *   elimination of group subordination rather than mere formal identity.
 *   These are not scientific disagreements about facts — they are
 *   foundational reading choices about what equal protection mandates. The
 *   colorblind reading has emerged as institutional doctrine through Supreme
 *   Court majorities (Adarand Constructors v. Pena, 1995; Parents Involved v.
 *   Seattle School District, 2007; Students for Fair Admissions v.
 *   Harvard/UNC, 2023) but remains contested by academic interpreters,
 *   remedial practitioners, and dissenting justices who hold the remedial and
 *   antisubordination readings.
 *
 * KEY AGENTS:
 *   - Majority Applicants and Beneficiary Groups: Institutional/arbitrage perspective — benefit from formal-equality protection that shields them from race-conscious selection; gain structural advantage because race-correlated benefits (legacy, geographic, athletic preference) remain permitted while race-conscious remedies are prohibited.
 *   - Historically Excluded Groups and Remedial Access Seekers: Powerless/trapped perspective — lose direct remedial pathway; must prove discrimination individually rather than accessing group-based remedy; accumulate effects of past discrimination while prohibition on race-conscious remedy forecloses primary remedy mechanism.
 *   - State Education Systems and Institutional Administrators: Organized/constrained perspective — face conflicting mandates: coordinate equal protection (genuine coordination function) while prohibited from using race-conscious tools to address segregation (asymmetric extraction). Partially benefit from simplified compliance (formal equality easier to defend than effects-based remedies).
 *   - State Remedial Capacity: Abstract institutional actor — loses structural authority to implement race-conscious remedies; prohibited from using diagnosis (race of affected group) to prescribe remedy.
 *   - Judicial/Doctrinal Authority: Institutional/arbitrage perspective — benefits from capturing interpretive authority to define constitutional colorblindness; maintains institutional control over remedy definition and bounds.
 *   - Analytical Observer: Positions across perspectives to examine functional effect of formal doctrine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colorblind_reading, 0.65).
domain_priors:suppression_score(colorblind_reading, 0.58).
domain_priors:theater_ratio(colorblind_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colorblind_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(colorblind_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(colorblind_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colorblind_reading, snare).
narrative_ontology:human_readable(colorblind_reading, "Colorblind Reading of Equal Protection: Categorical Prohibition on Race-Conscious State Action").
narrative_ontology:topic_domain(colorblind_reading, "constitutional_law/civil_rights/education_policy").

domain_priors:requires_active_enforcement(colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(colorblind_reading, '47c9b200-4f0b-46f1-9c22-1769bae468ad').
narrative_ontology:cs_kernel_codification('47c9b200-4f0b-46f1-9c22-1769bae468ad', fixed_text).
narrative_ontology:cs_authority_grounding('47c9b200-4f0b-46f1-9c22-1769bae468ad', lineage).
narrative_ontology:cs_interpretation_layer_present('47c9b200-4f0b-46f1-9c22-1769bae468ad').
narrative_ontology:cs_reading_relation('47c9b200-4f0b-46f1-9c22-1769bae468ad', colorblind_reading__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('47c9b200-4f0b-46f1-9c22-1769bae468ad', colorblind_reading__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('47c9b200-4f0b-46f1-9c22-1769bae468ad', foundational, race_classification_categorically_impermissible).
narrative_ontology:cs_axiom_status(race_classification_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('47c9b200-4f0b-46f1-9c22-1769bae468ad', race_classification_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('47c9b200-4f0b-46f1-9c22-1769bae468ad', foundational, formal_identity_constitutes_equal_protection).
narrative_ontology:cs_axiom_status(formal_identity_constitutes_equal_protection, holdable).
narrative_ontology:cs_axiom_grounding('47c9b200-4f0b-46f1-9c22-1769bae468ad', formal_identity_constitutes_equal_protection, conventional).
narrative_ontology:cs_reference_frame('47c9b200-4f0b-46f1-9c22-1769bae468ad', formal_equality_mandate).
narrative_ontology:cs_drift_state('47c9b200-4f0b-46f1-9c22-1769bae468ad', contemporary_post_2023, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('47c9b200-4f0b-46f1-9c22-1769bae468ad', '').
narrative_ontology:cs_kernel_id(colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(colorblind_reading, majority_applicants).
narrative_ontology:constraint_beneficiary(colorblind_reading, colorblind_doctrine_interpreters).
narrative_ontology:constraint_victim(colorblind_reading, historically_excluded_groups).
narrative_ontology:constraint_victim(colorblind_reading, remedial_access_seekers).
narrative_ontology:constraint_victim(colorblind_reading, state_remedial_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICALLY EXCLUDED GROUPS (SNARE) — Cannot exit the constraint; trapped by formal prohibition on the remedial mechanisms that would directly address effects of past discrimination. Suppression is high: no legitimate pathway to race-conscious remedy, alternative non-race-conscious pathways (socioeconomic preference, legacy preference) operate on different axes. Extracted from by loss of remedial access while bearing accumulated effects of historical discrimination.
constraint_indexing:constraint_classification(colorblind_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BENEFICIARY MAJORITY AND DOCTRINE INTERPRETERS (ROPE) — Benefits from formal equality principle that protects majority applicants from race-conscious selection; also benefits institutional actors (courts, executive actors interpreting constitutional limits narrowly) who capture the interpretive authority to define 'colorblind' across shifting institutional contexts. Net beneficiary: experiences constraint as coordination that protects their interests through neutral framing.
constraint_indexing:constraint_classification(colorblind_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE EDUCATION SYSTEMS (TANGLED ROPE) — Coordinated obligation to provide equal protection (genuine coordination function) embedded within asymmetric constraint on remedial toolkits (extraction). States cannot use race-conscious admissions to address segregation but also cannot ignore segregation — caught between judicial mandate (coordinate equally) and judicial prohibition (do not use race to coordinate). Constrained by doctrine but also benefit from simplified compliance framework (formal equality easier to defend than effects-based remedies).
constraint_indexing:constraint_classification(colorblind_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ORIGINALIST INTERPRETATION AUTHORITY (PITON) — Maintains formal doctrine through performative commitment to 'original public meaning' while the actual doctrine drifts (recent cases carve exceptions for legacy preference, donor preference, athlete recruitment — all race-correlated, all permitted). The colorblind principle is theatrically maintained as categorical while institutional practice permits substantial racial consideration through non-race-labeled mechanisms. Theater ratio (0.48) reflects moderate performativity: the doctrine is genuinely invoked in admissions cases but institutional practice has created workarounds that functionally permit race-conscious resource allocation.
constraint_indexing:constraint_classification(colorblind_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (SNARE) — From civilizational/analytical scope, the colorblind reading produces measurable subordination: historically excluded groups lose remedial access while majority groups retain structurally equivalent benefits (legacy preference, athlete recruitment, geographic diversity weighted toward affluent areas). The constraint operates as extraction wrapped in formal equality. Theater_ratio (0.48) is lower here because the analytical scope recognizes the functional inequality beneath formal language.
constraint_indexing:constraint_classification(colorblind_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 6: FORMAL EQUALITY AS NATURAL LAW (MOUNTAIN) — From a universalist/logical perspective, colorblindness is presented as natural law: formal rule requiring identical treatment is logically prior to considerations of effect. This perspective reads the Fourteenth Amendment clause as establishing an irreducible principle: state may not classify by race because classification by race is categorically impermissible. The reading naturalizes a constructed choice (treating formal identity as the measure of equality) as a logical necessity. The engine will identify this as a false summit: declared beneficiaries (majority_applicants, doctrine_interpreters) reveal that the 'natural law' framing benefits specific parties and naturalizes what is structurally a reading choice among alternatives.
constraint_indexing:constraint_classification(colorblind_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colorblind_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colorblind_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colorblind_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(colorblind_reading, TR),
    TR >= 0.70.

:- end_tests(colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The colorblind reading produces asymmetric effects: majority applicants retain structurally equivalent benefits through race-correlated (but not race-labeled) mechanisms (legacy preference, geographic diversity weighted toward affluent areas, athletic recruitment); historically excluded groups lose the primary remedy mechanism. The constraint extracts from remedial-access seekers by prohibiting the direct remedy while permitting functionally equivalent race-correlated allocations benefiting other groups. Suppression (0.58): Moderate-high. Suppression operates through multiple channels: formal prohibition on race-conscious remedy (structural barrier), institutional capture of alternative-remedy definition (legitimacy barrier — courts control what alternative mechanisms are permissible), and loss of political power to advocate for group-based remedy (organizational barrier — remedial seekers are dispersed across institutions with limited coordinating capacity). Theater ratio (0.48): Moderate. The constraint shows moderate performativity because courts apply colorblind doctrine selectively — race-correlated benefits for majorities are permitted when labeled differently; race-conscious remedies for historically excluded groups are prohibited even when explicitly remedial. The doctrine is not pure performance (it has real functional effects on remedial access) but is performatively maintained (consistent application across race-correlated contexts would look different). Rising theater over time (0.25 in 1978 → 0.48 in 2023) reflects increasing doctrinal tension: as race-correlated alternatives have proliferated (donor preference, legacy, geographic diversity, athletic recruitment), the categorical prohibition on race-conscious remedy appears increasingly selective rather than principle-driven.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries and victims is extreme. Majority applicants experience the constraint as coordination (rope): formal equality protects them from racial competition and permits race-correlated benefits to remain. Historically excluded groups experience the constraint as extraction (snare): they lose direct remedial access while majority advantages persist under different labels. The organized perspective (education systems) experiences tangled rope: genuine coordination function (equal protection obligation) embedded within asymmetric constraint (cannot use race to remedy). The piton perspective (originalist doctrine) shows the theatrical maintenance: colorblind principle invoked categorically while institutional practice permits functionally equivalent race-correlated allocations. The analytical scopes recognize the functional subordination effect: formal identity rule produces substantive inequality.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position. Majority applicants are beneficiaries (declared); their structural position yields low d → low chi. Historically excluded groups are victims (declared); their structural position yields high d → high chi. State systems declare both beneficiaries and victims because they are coordinated (equal protection obligation) while simultaneously subject to asymmetric constraint (cannot use race to coordinate remedially); this produces moderate d and tangled-rope classification. The piton perspective benefits from institutional capture of remedy-definition authority (arbitrage exit options) while performing commitment to colorblind doctrine; this produces moderate d despite beneficiary structural position. The analytical perspectives at civilizational scope recognize the functional extraction beneath formal neutrality; this produces high d for the snare classification and reveals the false summit in the mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   COMMITTER-AXIS CLARIFICATION: The mandate of the Equal Protection Clause is itself contested. The colorblind reading instantiates one interpretation: the mandate is to prohibit state racial classification. The remedial reading instantiates a different interpretation: the mandate is to affirmatively remedy discrimination effects. These are not different views of the same mandate — they are different readings of what the mandate is. The constraint does not suffer from mandatrophy (a mandate outliving its function) but from READING CONTESTATION (multiple normative readings of the same constitutional text producing different functional constraints). The colorblind reading's mandate is coherent and continuously reinforced: the institutional doctrine of prohibiting racial classification is actively maintained and litigated. But the reading's legitimacy is contested by the remedial and antisubordination readings, which claim the Constitution mandates something different. This is not a case where the mandate has atrophied — it is a case where the reading itself is one option among alternatives, and the alternatives are live political and legal positions held by organized constituencies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_vs_substantive_equality,
    'Is formal equality (identical treatment of all races) the only legitimate reading of equal protection, or is substantive equality (addressing effects of discrimination) an equally valid constitutional interpretation?',
    'Historical analysis of textual alternatives: did the Fourteenth Amendment''s drafters foreclose substantive remedies? Comparative constitutional law: how do other democracies resolve this tension? Case law evolution: has the Supreme Court itself recognized substantive equality claims in other domains (gender, disability)?',
    'If formal equality is the only reading: colorblind_reading is mountain-adjacent (constraint derived from constitutional text, not constructed). If substantive equality is coequal: colorblind_reading is a false summit (reading choice among alternatives, not natural law). Classification changes from mountain to snare/tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_vs_substantive_equality, conceptual, 'Whether formal or substantive equality is the constitutionally mandated standard').

omega_variable(
    race_conscious_remedy_necessity,
    'Do race-conscious admissions policies actually function as remedies for effects of past discrimination, or do they primarily function as interest-group transfers benefiting particular institutional actors?',
    'Longitudinal comparison of educational outcomes: do students admitted under race-conscious policies show higher graduation rates, income mobility, or professional outcomes than identical students not admitted? Do institutions that implement race-conscious policies show measurable improvement in segregation metrics? Institutional data on remedial justification: are race-conscious policies explicitly tied to documented discrimination-remediation or primarily justified on diversity grounds?',
    'If race-conscious policies measurably remedy: victim classification (historically_excluded_groups) is correct; suppression is extractive. If policies operate as transfers or status signaling: victim classification may need revision; constraint may be tangled_rope rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(race_conscious_remedy_necessity, empirical, 'Whether race-conscious admissions actually function as remedies or as transfers').

omega_variable(
    alternative_remedy_sufficiency,
    'Can socioeconomic-based, geographic-based, or other non-race-labeled admissions policies produce equivalent or superior remedial effects compared to race-conscious policies?',
    'Controlled comparison studies: institutions switching from race-conscious to class-based or other non-race-labeled criteria; measurement of resulting diversity metrics, segregation outcomes, and representation of historically excluded groups. Analysis of selection criterion correlation: how much of the racial effect in admissions comes directly from race-conscious policy vs. from correlation with other measurable variables?',
    'If alternatives are sufficient: remedial_access_seekers classification as victim is weakened; suppression may be lower than authored (0.58). If alternatives are insufficient: colorblind_reading suppression is validated; constraint is snare. If alternatives are functionally equivalent but not legally permitted: shows extraction mechanism is institutional capture of remedy definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_remedy_sufficiency, empirical, 'Whether non-race-labeled policies can substitute for race-conscious remedies').

omega_variable(
    doctrine_boundary_coherence,
    'Is the colorblind doctrine applied consistently across race-correlated contexts (legacy preference, geographic diversity, athlete recruitment, developmental disability), or is it selectively enforced against race-conscious remedies while permitting functionally equivalent race-correlated allocations through non-race labels?',
    'Doctrinal mapping: do courts apply strict scrutiny equally to all race-correlated admissions criteria or differentially by stated category? Comparative scrutiny analysis: what burden of proof or compelling interest is required for race-conscious vs. race-correlated policies? Case law trends: have courts expanded exceptions to colorblind principle for non-race-labeled criteria?',
    'If selectively enforced: piton classification (performative doctrine) is confirmed and theater_ratio is appropriate. If applied consistently: piton classification weakens; doctrine has higher functional coherence. Affects measurement trajectory: selective enforcement suggests rising theater_ratio over time; consistent enforcement suggests stable theater_ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_boundary_coherence, empirical, 'Whether colorblind doctrine is applied consistently across race-correlated contexts').

omega_variable(
    reading_foreclosure_relationship,
    'Does the colorblind reading''s core axiom (race classification is categorically impermissible) logically foreclose the remedial reading''s core axiom (past discrimination requires race-conscious remedy), or do these represent coexisting normative commitments held by different parties?',
    'Conceptual analysis: is there a single unified framework that could simultaneously hold ''race classification is categorically impermissible'' AND ''past discrimination requires race-conscious remedy''? Or are these genuinely incompatible commitments requiring choice? Historical inquiry: have any major legal traditions held both simultaneously, or does history show them as exclusive alternatives?',
    'If foreclosure: colorblind_reading and remedial_reading cannot coexist in one legal system; one must dominate. If coexistence: readings represent factions within ongoing institutional dispute; both remain live positions. Affects reading_relations classification in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_relationship, conceptual, 'Whether colorblind and remedial readings are logically incompatible or can coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colorblind_reading, 1964, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colorblind_theater_1978, colorblind_reading, theater_ratio, 1978, 0.25).
narrative_ontology:measurement(colorblind_theater_1995, colorblind_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(colorblind_theater_2013, colorblind_reading, theater_ratio, 2013, 0.52).
narrative_ontology:measurement(colorblind_theater_2023, colorblind_reading, theater_ratio, 2023, 0.48).

% Extraction over time
narrative_ontology:measurement(colorblind_extract_1964, colorblind_reading, base_extractiveness, 1964, 0.15).
narrative_ontology:measurement(colorblind_extract_1978, colorblind_reading, base_extractiveness, 1978, 0.35).
narrative_ontology:measurement(colorblind_extract_1995, colorblind_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(colorblind_extract_2013, colorblind_reading, base_extractiveness, 2013, 0.68).
narrative_ontology:measurement(colorblind_extract_2023, colorblind_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(colorblind_suppress_1978, colorblind_reading, suppression_requirement, 1978, 0.42).
narrative_ontology:measurement(colorblind_suppress_1995, colorblind_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(colorblind_suppress_2013, colorblind_reading, suppression_requirement, 2013, 0.62).
narrative_ontology:measurement(colorblind_suppress_2023, colorblind_reading, suppression_requirement, 2023, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colorblind_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(colorblind_reading, 0.12).
narrative_ontology:affects_constraint(colorblind_reading, remedial_reading).
narrative_ontology:affects_constraint(colorblind_reading, antisubordination_reading).
narrative_ontology:affects_constraint(colorblind_reading, equal_protection_kernel).

% DUAL FORMULATION NOTE:
% The colorblind reading, remedial reading, and antisubordination reading are THREE STRUCTURALLY DISTINCT CONSTRAINTS derived from ONE contested kernel (equal_protection_kernel). Each reading produces different ε values, different beneficiary/victim structures, and different classifications because they instantiate different normative interpretations of what 'equal protection' mandates. These are NOT observables of the same constraint with measurement variance — they are genuinely different constraints that share a common textual origin. The family is linked as a presheaf over the equal protection kernel: each reading is a complete constraint story modeling one normative interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(colorblind_reading, analytical, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
