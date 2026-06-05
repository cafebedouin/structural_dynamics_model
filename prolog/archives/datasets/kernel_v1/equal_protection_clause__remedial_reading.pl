% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Clause: Remedial Reading (Anti-Subordination)
 *   domain: constitutional_law/civil_rights/educational_policy
 *
 * SUMMARY:
 *   The remedial reading of the Equal Protection Clause interprets the
 *   clause's core purpose as anti-subordination: the elimination of systemic
 *   discrimination against historically marginalized groups and the
 *   achievement of substantive equality. This reading permits race-conscious
 *   state action — including admissions preferences, targeted resource
 *   allocation, and explicit remedial programs — as constitutional tools for
 *   dismantling the structural effects of past and ongoing discrimination.
 *   The reading treats formal race-neutrality (the colorblind alternative) as
 *   insufficient and even complicit in perpetuating subordination. It locates
 *   the constitutional evil not in state consideration of race (which it sees
 *   as necessary to address race-based harms) but in the failure to remedy
 *   subordination. This constraint story models the remedial reading as ONE
 *   instantiation of the contested equal-protection kernel, generating high
 *   extractiveness due to the reading's conflict with competing
 *   interpretations and the judicial suppression it faces. The extractiveness
 *   trend (0.35 → 0.58 over 40 years) reflects increasing judicial narrowing
 *   of remedial doctrines and political mobilization against race-conscious
 *   policies, which have steadily constrained the reading's institutional
 *   application despite its doctrinal coherence.
 *
 * KEY AGENTS:
 *   - Historically Marginalized Communities (Black, Latino, Indigenous students): Primary victims of systemic discrimination; positioned as primary beneficiaries of remedial action; constrained by ongoing subordination but represented by organized civil rights advocates
 *   - Civil Rights Advocates and Lawyers: Organized institutional actors; primary intellectual beneficiaries of the remedial reading; have arbitrage options (venue shopping, litigation strategy coordination); experience constraint as enabling coordination
 *   - Universities Implementing Race-Conscious Admissions: Institutional beneficiaries; gain legal authorization to pursue diversity; constrained by continuous litigation and political pressure; experience mixed coordination (serving educational mission) and extraction (defending policies)
 *   - White Applicants Denied Admission: Bear direct costs of remedial policies; trapped by selective admissions processes; no exit option or ability to negotiate remedy structure; experience pure extraction
 *   - Conservative Legal Movement and Colorblind Advocates: Structurally mobile institutional actors; see their preferred reading suppressed by the remedial reading's foreclosure of colorblind doctrines; experience suppression despite significant institutional power
 *   - Judicial System: Constrained by constitutional text indeterminacy; must adjudicate competing readings; expends resources on continuous litigation; trapped between textual ambiguity and demand for determinate outcomes
 *   - Analytical Observer: Positioned to see whether the remedial/colorblind tension is an immutable feature of equal protection or a contingent institutional contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.58).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.65).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Clause: Remedial Reading (Anti-Subordination)").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/civil_rights/educational_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, 'cbafcc44-d520-4c03-bc8d-d5ff6ee9762a').
narrative_ontology:cs_kernel_codification('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a', fixed_text).
narrative_ontology:cs_authority_grounding('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a', lineage).
narrative_ontology:cs_interpretation_layer_present('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a').
narrative_ontology:cs_reading_relation('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a', equal_protection_clause__diversity_reading, influences).
narrative_ontology:cs_axiom('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a', foundational, race_consciousness_necessary_remedy).
narrative_ontology:cs_axiom_status(race_consciousness_necessary_remedy, holdable).
narrative_ontology:cs_axiom_grounding('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a', race_consciousness_necessary_remedy, empirically_contingent).
narrative_ontology:cs_axiom('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a', foundational, anti_subordination_core_purpose).
narrative_ontology:cs_axiom_status(anti_subordination_core_purpose, holdable).
narrative_ontology:cs_axiom_grounding('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a', anti_subordination_core_purpose, deontological).
narrative_ontology:cs_reference_frame('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a', substantive_anti_subordination_framework).
narrative_ontology:cs_drift_state('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a', contemporary_post_supreme_court_narrowing, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cbafcc44-d520-4c03-bc8d-d5ff6ee9762a', '2026-02-27T14:32:18Z').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, civil_rights_advocates).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, universities_implementing_remedies).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_marginalized_communities).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, white_applicants_bearing_remedy_costs).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, judicial_suppression_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WHITE APPLICANTS (SNARE) — Trapped by selective admissions processes that weight race-conscious remedies against their individual applications. No exit option from the constraint; cannot restructure the educational market or opt out of selective institutions. Maximal extraction experience: costs imposed without ability to negotiate, exit, or organize collective response. The remedial reading frames their exclusion as a cost of achieving substantive equality, but from this perspective, the constraint extracts individual opportunity cost without consent or reciprocal benefit.
constraint_indexing:constraint_classification(equal_protection_clause__remedial_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HISTORICALLY MARGINALIZED COMMUNITIES (TANGLED ROPE) — Constrained by ongoing systemic discrimination and its intergenerational effects, but also positioned as primary beneficiaries of remedial action. This perspective experiences genuine coordination (the state's duty to remedy systemic wrongs aligns with the communities' interest in overcoming subordination) alongside extraction (remedies are often symbolic, underfunded, or contested, leaving substantive inequality partially unaddressed). High agency through civil rights organizing; constrained by institutional barriers and continued discrimination. The constraint both serves their interests and reproduces limitations on remedial efficacy.
constraint_indexing:constraint_classification(equal_protection_clause__remedial_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIVERSITIES (TANGLED ROPE) — Benefit from having legal authorization to pursue diverse student bodies and implement remedial admissions policies; also constrained by continuous litigation, political pressure, and judicial suppression (see Supreme Court decisions narrowing scope). Experience coordination function (fulfilling educational mission to serve diverse populations) alongside extraction (must expend resources defending policies, face reputational costs from opposition, navigate shifting legal terrain). Constrained agency due to judicial review and political contestation.
constraint_indexing:constraint_classification(equal_protection_clause__remedial_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS ADVOCATES (ROPE) — Organized actors with institutional capacity and legal expertise. Benefit from the remedial reading's legitimacy (it provides doctrinal foundations for their litigation strategy); experience the constraint as enabling coordination (marshaling legal, political, and social movements to advance anti-subordination principles). Have arbitrage options: can shift venue, litigate different fact patterns, coordinate across state and federal levels. Extraction is minimal relative to coordination benefit. This is the reading's primary intellectual beneficiary.
constraint_indexing:constraint_classification(equal_protection_clause__remedial_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: COLORBLIND ADVOCATES (SNARE) — Structurally mobile (can litigate against remedial policies, shift jurisprudence through appointments) but face suppression of their preferred reading within this constraint's framework. The remedial reading forecloses their core premise (that race-consciousness is inherently invidious). From their perspective, the constraint is extractive: it permits subordination of colorblind principles and extracts judicial legitimacy for race-conscious state action. However, their actual institutional power is high and increasing (reflected in recent Supreme Court decisions); the snare classification reflects the temporary suppression of the colorblind position within the remedial reading's logic, not permanent institutional subordination.
constraint_indexing:constraint_classification(equal_protection_clause__remedial_reading, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: JUDICIAL SYSTEM (TANGLED ROPE) — Constrained by the constitutional text ('equal protection') which is formally race-neutral but substantively contested. Must adjudicate competing readings of the same clause. The remedial reading imposes a coordination function (reconciling formal equality with substantive anti-subordination) and enforcement costs (continuous litigation over scope and application). Extraction arises from judicial resources devoted to managing the reading contest and from political pressure to suppress remedial doctrines. The system is trapped between textual indeterminacy and the pressure to produce determinate outcomes.
constraint_indexing:constraint_classification(equal_protection_clause__remedial_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal/civilizational perspective, the tension between formal equality and substantive equality is treated as an immutable feature of any equal protection principle. The remedial reading might appear as responding to an unchangeable dilemma: how to use race-neutral language to address race-based harms. However, the engine will identify this as a false summit: the 'necessity' of race-conscious remedies is contingent on a specific reading of the constitutional kernel, not on natural law. Different readings (colorblind, diversity) reject this 'necessity.' The analytical view risks naturalizing one particular reading's logic.
constraint_indexing:constraint_classification(equal_protection_clause__remedial_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_clause__remedial_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_clause__remedial_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The remedial reading permits broad race-conscious state action, which imposes costs on white applicants barred from certain educational and employment opportunities. This is real extraction — individual opportunity cost imposed by state policy. However, the extraction is justified within the reading's own framework as corrective (remedying prior subordination) rather than exploitative. The justification is internally coherent but contested. The trend from 0.35 to 0.58 reflects increasing pressure from legal challenges and political opposition, which have narrowed the doctrinal space for remedial action. Suppression (0.65): High. The remedial reading faces institutional suppression from multiple sources: the Supreme Court has progressively narrowed permissible remedial scope (Bakke, Fisher, Students for Fair Admissions v Harvard); political opponents have mobilized against race-conscious policies; popular opposition remains substantial. The trend from 0.38 to 0.65 reflects successful institutional challenge to the reading's doctrinal authority. Theater ratio (0.52): Moderate. The remedial reading's implementation involves both genuine remedial policy (real allocations of educational resources toward historically excluded populations) and performative compliance (symbolic gestures toward diversity and inclusion that leave structural inequality partially unaddressed). The relatively stable low-to-moderate theater ratio suggests that implemented remedies mix functional change with theatrical elements but are not primarily performative. Claimed type (Tangled Rope): The constraint exhibits both genuine coordination (the state's duty to remedy aligns with marginalized communities' interests in overcoming subordination) and asymmetric extraction (white applicants and colorblind advocates bear costs). The remedial reading requires active enforcement through litigation, policy implementation, and political mobilization. It is not pure extraction (there is a real coordination function) and not pure coordination (there are real costs imposed unilaterally).
 *
 * PERSPECTIVAL GAP:
 *   The remedial reading generates maximal perspectival divergence. White applicants see pure extraction (Snare); historically marginalized communities see mixed coordination and extraction (Tangled Rope); universities see coordination with institutional costs (Tangled Rope); civil rights advocates see enabling coordination (Rope); conservative legal opponents see suppression of their preferred reading (Snare); the judiciary sees being trapped between competing readings (Tangled Rope); the analytical observer risks naturalizing the remedial/colorblind contest as an immutable feature of equal protection (Mountain/false summit). The perspectival gap reveals that the constraint's classification depends entirely on structural position relative to the remedial reading's costs and benefits. No single type captures the constraint's structure; the presheaf over all perspectives is required.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's position in the extraction flow. White applicants: d ≈ 0.92 (full target of extraction, trapped exit) → high f(d) ≈ 1.38 → high experienced extractiveness. Historically marginalized communities: d ≈ 0.45 (mixed victims of past discrimination, beneficiaries of remedy; constrained exit) → moderate f(d) ≈ 0.55 → moderate chi. Universities: d ≈ 0.50 (both benefit from diversity authorization and constrained by litigation; constrained exit) → symmetric f(d) ≈ 0.65 → moderate chi. Civil rights advocates: d ≈ 0.10 (primary intellectual beneficiaries; organized/arbitrage exit) → low f(d) ≈ -0.02 → minimal/negative chi. Conservative colorblind advocates: d ≈ 0.88 (targeted by foreclosure of their reading; mobile exit despite institutional suppression) → high f(d) ≈ 1.32 → high chi within their framework. The spread in d values (0.10 to 0.92) reflects how differently positioned agents are relative to the remedial reading's extraction flow. No override is needed; the canonical derivation captures the structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading's claimed type (Tangled Rope) is sustained against the mandatrophy through the presence of genuine coordination function alongside asymmetric extraction. The reading's anti-subordination purpose is NOT merely a cover story for extraction — the coordination objective (achieving substantive equality) is real and internally coherent. However, the reading does not escape extraction: the costs imposed on white applicants are extraction, and the suppression of colorblind readings is extraction of doctrinal legitimacy. The constraint resolves mandatrophy by acknowledging that constitutional remedies necessarily embed both coordination (toward substantive equality) and extraction (from those bearing remedy costs). The question 'Is anti-subordination coordination or extraction?' has no single answer: it is coordination for those who accept the remedial goal, extraction for those who reject it or bear its costs. The engine's multivalent classification across perspectives captures this irreducible contestedness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_scope_boundary,
    'How far does the state''s remedial duty extend? Is it limited to addressing identified past discrimination by that specific state, or does it extend to addressing systemic discrimination across generations and institutions?',
    'Historical analysis of implemented remedies and their empirical effects; doctrinal analysis of remedial scope in landmark cases (Bakke, Grutter, Fisher); comparison of narrow vs broad remedial rationales in legislative intent',
    'Narrow scope: constraint classifies as lower extractiveness (remedies are targeted, limited in duration). Broad scope: constraint classifies as higher extractiveness (remedies are expansive, potentially affecting large populations indefinitely). This directly affects whether the remedial reading is sustainable or overreaches.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedial_scope_boundary, conceptual, 'Constitutional scope of remedial duty for systemic discrimination').

omega_variable(
    individual_vs_group_remedy_tension,
    'Can remedial equality be achieved through individual-level race-conscious actions, or does substantive equality require group-level remedies that necessarily burden some individuals?',
    'Empirical analysis of remedial outcomes at individual vs group levels; analysis of whether individual fairness and group-level justice are structurally compatible or in zero-sum tension',
    'If compatible: remedial reading sustains; individual and group-level remedies can coexist. If in tension: remedial reading faces deeper structural contradiction — achieving anti-subordination may require accepting individual unfairness, which erodes the reading''s legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_vs_group_remedy_tension, conceptual, 'Tension between individual fairness and group-level remedial equality').

omega_variable(
    reading_foreclosure_test,
    'Does the remedial reading logically foreclose the colorblind reading, or do they represent competing but coexistent frameworks?',
    'Formal logical analysis: can a single framework hold both that (a) the state has an affirmative duty to use race-conscious remedies (remedial reading) and (b) any state use of race is presumptively unconstitutional (colorblind reading)? Or are these genuinely incompatible premises?',
    'If foreclosed: this reading structurally eliminates colorblind jurisprudence within its own logic. If coexistent: the readings are alternative frameworks held by different institutional actors, and the constraint contest is political, not logical. High impact on how reading_relations should be classified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether remedial and colorblind readings logically foreclose each other').

omega_variable(
    suppression_mechanism_source,
    'Is the measured suppression (0.65) arising from judicial decisions that narrow remedial scope (institutional suppression), or from political and popular opposition to remedial policies (social suppression), or both equally?',
    'Doctrinal analysis of Supreme Court jurisprudence limiting remedial scope (Regents v Bakke, Fisher v UT, Students for Fair Admissions v Harvard); analysis of political organizing against remedial policies; media framing analysis',
    'If primarily judicial: suppression is formal and institutional; the remedial reading''s own framework includes the mechanisms of its suppression. If primarily social: suppression is external; the remedial reading must overcome popular opposition but retains internal coherence. Affects whether suppression is a feature of the constraint or external pressure on it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Source of suppression: judicial doctrinal limitation vs social/political opposition').

omega_variable(
    natural_law_false_summit_test,
    'Is the analytical observer''s mountain classification (treating remedial vs colorblind as responses to immutable equal-protection dilemma) a false summit naturalizing contingent institutional arrangements?',
    'Historical analysis: did equal protection doctrine always require this tension, or is the remedial/colorblind contest a product of specific 20th-century constitutional politics? Cross-national comparison: how do other democracies with substantively equal protection clauses handle the remedial question?',
    'If false summit confirmed: the ''immutable dilemma'' is actually a constructed doctrinal contest. The remedial reading is one institutional choice among others, not a response to natural law. This shifts analytical assessment from ''how to resolve the dilemma'' to ''which reading is politically preferred and why.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_false_summit_test, conceptual, 'Whether the remedial/colorblind tension is natural law or contingent institutional arrangement').

omega_variable(
    kernel_reading_contest_location,
    'Where exactly is the contest between remedial and colorblind readings located? In the text of the clause itself, in its historical intent, in its purposes, or in meta-constitutional principles of legitimacy?',
    'Textual analysis of ''equal protection'' language; originalist analysis of 14th Amendment intent; purposive analysis of anti-subordination vs formal equality aims; jurisprudential analysis of how courts decide between contested readings',
    'If in text: both readings claim support from the same language; indeterminacy is intrinsic. If in historical intent: original meaning may favor one reading (probably colorblind), making remedial reading a doctrinal innovation. If in purposes: both purposes (formal equality and substantive equality) may be simultaneous commitments, making the contest a real structural contradiction. If in meta-constitutional legitimacy: the contest is about which framework courts should adopt, making it explicitly political.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Location of the reading contest within the constitutional kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_remedial_theater_t0, equal_protection_clause__remedial_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(epc_remedial_theater_t20, equal_protection_clause__remedial_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(epc_remedial_theater_t40, equal_protection_clause__remedial_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(epc_remedial_extract_t0, equal_protection_clause__remedial_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(epc_remedial_extract_t20, equal_protection_clause__remedial_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(epc_remedial_extract_t40, equal_protection_clause__remedial_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(epc_remedial_suppress_t0, equal_protection_clause__remedial_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(epc_remedial_suppress_t20, equal_protection_clause__remedial_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(epc_remedial_suppress_t40, equal_protection_clause__remedial_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_clause kernel decomposes into three structurally distinct constraint stories, each modeling a competing reading with different ε values, beneficiary/victim structures, and classifications. The remedial_reading (this constraint, ε=0.58) is the most extractive reading due to the high costs imposed on white applicants and the judicial/political suppression it faces. The colorblind_reading (ε≈0.42) treats race-consciousness itself as the constraint. The diversity_reading (ε≈0.35) treats limited race consideration as low-extraction coordination. All three are linked by network.affects_constraints to signal their interdependence: changes in one reading's institutional power affect the others' scope and salience.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
