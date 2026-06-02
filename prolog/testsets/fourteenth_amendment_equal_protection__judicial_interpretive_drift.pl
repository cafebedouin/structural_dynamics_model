% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__judicial_interpretive_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__judicial_interpretive_drift, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__judicial_interpretive_drift
 *   human_readable: Fourteenth Amendment Equal Protection Clause — Judicial Interpretive Drift Reading
 *   domain: constitutional_law/civil_rights/political_philosophy
 *
 * SUMMARY:
 *   The Fourteenth Amendment's Equal Protection Clause — 'nor shall any State
 *   deny to any person within its jurisdiction the equal protection of the
 *   laws' — is a kernel constitutional commitment that grounds institutional
 *   legitimacy. This constraint instantiates one specific reading of that
 *   kernel: the judicial-interpretive-drift reading, which emphasizes how the
 *   meaning of 'equal protection' has shifted dramatically across time and
 *   across different judges, creating a mechanism where the formal guarantee
 *   of equal protection coexists with practical unpredictability in its
 *   application. Ratified in 1868 to address explicitly racial discrimination
 *   in the immediate post-Civil War context, the clause has been interpreted
 *   as addressing gender discrimination (1970s onward), sexual-orientation
 *   discrimination (2000s onward), algorithmic discrimination (2010s onward),
 *   and structural economic inequality (contested). The drift is not merely
 *   temporal change in legal understanding — it is instantiated through the
 *   three-tier framework of constitutional review (strict scrutiny,
 *   intermediate scrutiny, rational basis), where the tier selected for a
 *   given discrimination claim heavily determines the outcome, and the tier
 *   selection itself has become increasingly fluid and outcome-dependent. The
 *   constraint functions as a tangled rope: it provides genuine coordination
 *   (enables marginalized groups to invoke a focused equal-protection
 *   principle, enables judges to reach decisions without raw political fiat)
 *   while simultaneously extracting from those seeking protection (their
 *   success rate depends on judicial composition, doctrinal drift, and the
 *   unpredictable application of scrutiny tiers). The theater ratio (0.68)
 *   reflects the increasing performative character of the doctrine — equal
 *   protection is invoked to legitimize outcomes that would be difficult to
 *   defend without the label, even as the doctrine's substantive force
 *   attenuates. The measurements show drift over 145 years: extractiveness
 *   rising from 0.25 (immediate post-ratification, when the constraint had
 *   limited scope but when applied had relatively stable meaning) to 0.58
 *   (contemporary, when scope is expansive but meaning is contingent on
 *   judicial composition). The suppression requirement rises as well: what it
 *   takes to suppress equal-protection litigation from succeeding has
 *   increased, partly because the judiciary's capacity to reject claims on
 *   doctrinal grounds has expanded (more sophisticated doctrinal escape
 *   routes became available), partly because marginalized groups'
 *   coordination capacity has not kept pace with doctrinal fragmentation.
 *
 * KEY AGENTS:
 *   - Marginalized Groups Seeking Equal Protection (powerless/trapped): Ratified beneficiary of the formal constraint but victim of its interpretive drift; cannot exit the judicial system; success contingent on judicial composition
 *   - Civil Rights Organizations (moderate/constrained): Benefit from organizational identity and focal point for litigation; bear costs of doctrinal unpredictability; constrained by resource limits and electoral politics affecting judicial composition
 *   - Federal Judiciary (institutional/arbitrage): Primary institutional beneficiary; experiences constraint as coordination mechanism enabling judicial discretion; benefits from interpretive flexibility across changing social contexts
 *   - Political Majorities and Executive Power (powerful/mobile): Benefit from judicial discretion when aligned; extract from it when opposed; mobile exit option through judicial appointments; shift constraint force through electoral control of court composition
 *   - Constitutional Doctrine Reform Movements (organized/constrained): Originalists, living constitutionalists, critical scholars seeking to stabilize or redirect interpretation; constrained by institutional resistance; frame drift as a problem under construction with a coherence pathway
 *   - Historical Rhetoric of Equal Protection (institutional/arbitrage): The inherited legitimacy language that enables contemporary judicial decisions; persists through theater despite atrophied function
 *   - Analytical Observer (analytical/analytical): The civilizational view risking naturalization of drift as inevitable rather than contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__judicial_interpretive_drift, 0.58).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__judicial_interpretive_drift, 0.65).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__judicial_interpretive_drift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__judicial_interpretive_drift, extractiveness, 0.58).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__judicial_interpretive_drift, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__judicial_interpretive_drift, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__judicial_interpretive_drift, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__judicial_interpretive_drift, "Fourteenth Amendment Equal Protection Clause — Judicial Interpretive Drift Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__judicial_interpretive_drift, "constitutional_law/civil_rights/political_philosophy").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__judicial_interpretive_drift).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__judicial_interpretive_drift, '48345322-80cd-4b76-bf75-2c6031821a8b').
narrative_ontology:cs_kernel_codification('48345322-80cd-4b76-bf75-2c6031821a8b', formalized).
narrative_ontology:cs_authority_grounding('48345322-80cd-4b76-bf75-2c6031821a8b', lineage).
narrative_ontology:cs_interpretation_layer_present('48345322-80cd-4b76-bf75-2c6031821a8b').
narrative_ontology:cs_axiom('48345322-80cd-4b76-bf75-2c6031821a8b', foundational, judicial_interpretation_inherently_discretionary).
narrative_ontology:cs_axiom_status(judicial_interpretation_inherently_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('48345322-80cd-4b76-bf75-2c6031821a8b', judicial_interpretation_inherently_discretionary, empirically_contingent).
narrative_ontology:cs_axiom('48345322-80cd-4b76-bf75-2c6031821a8b', foundational, institutional_architecture_determines_canonical_reading).
narrative_ontology:cs_axiom_status(institutional_architecture_determines_canonical_reading, holdable).
narrative_ontology:cs_axiom_grounding('48345322-80cd-4b76-bf75-2c6031821a8b', institutional_architecture_determines_canonical_reading, empirically_contingent).
narrative_ontology:cs_reference_frame('48345322-80cd-4b76-bf75-2c6031821a8b', stable_equal_protection_doctrine).
narrative_ontology:cs_drift_state('48345322-80cd-4b76-bf75-2c6031821a8b', contemporary_judicial_ideology_dependence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('48345322-80cd-4b76-bf75-2c6031821a8b', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__judicial_interpretive_drift, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__judicial_interpretive_drift, institutional_judiciary).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__judicial_interpretive_drift, federal_executive_coordination).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__judicial_interpretive_drift, marginalized_groups_differential_protection).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__judicial_interpretive_drift, constitutional_constraint_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED GROUP (SNARE) — Groups seeking equal protection under a drifting interpretive standard have no exit from the judicial system and face maximal extraction: judicial doctrine shifts beneath their claims (strict scrutiny becomes rational basis depending on the court's composition and doctrinal moment), making successful equal-protection litigation unpredictable and inaccessible. The constraint traps them in a system where equal protection is formally guaranteed but substantively contingent on judicial mood.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__judicial_interpretive_drift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ADVOCATES (TANGLED ROPE) — Legal advocacy organizations benefit from the constraint's existence (it provides a focal point for litigation strategy and organizational identity) while bearing high costs (doctrinal unpredictability makes litigation outcomes contingent on which judges hear the case; constrained by resource limits and electoral politics affecting judicial appointments). Mixed coordination (the constraint enables advocacy) and asymmetric extraction (their success rate depends on forces outside their control).
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__judicial_interpretive_drift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL JUDICIARY (ROPE) — The judiciary benefits from interpretive discretion: the Equal Protection Clause's broad language enables judicial coordination of competing claims without formal rule-making. Judges experience the constraint as pure coordination — the vague standard allows them to resolve disputes flexibly. No victim group is visible from this perspective; the constraint coordinates judicial authority.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__judicial_interpretive_drift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICAL MAJORITIES (TANGLED ROPE) — Powerful actors benefit from judicial discretion when aligned with the judiciary (when appointed judges share the majority's values), but the constraint also extracts from them by limiting their ability to justify discriminatory policies without judicial scrutiny (however weak that scrutiny becomes). Mobile exit option: majorities can shift judicial composition through appointments, changing the constraint's force. Mixed experience: coordination when aligned, extraction when opposed.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__judicial_interpretive_drift, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ORIGINALISTS / REFORM MOVEMENTS (SCAFFOLD) — Organized legal movements (originalism, living constitutionalism, critical legal studies) see interpretive drift as a temporary coherence failure with a sunset: their aim is to anchor interpretation (either to original meaning or to explicitly declared evolving principles), reducing drift. The constraint appears as a problem under construction with an exit pathway — the sunset is the achievement of stable, predictable doctrine. Constrained by institutional resistance and political contingency.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__judicial_interpretive_drift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICAL RHETORIC (PITON) — The Equal Protection Clause's rhetoric (formal equality, color-blindness, neutral principles) persists as institutional theater despite degraded function: it continues to legitimize judicial decisions that would be difficult to defend without the equal-protection label, even as the doctrine itself has become so fluid that almost any outcome can be reached by choosing the appropriate level of scrutiny. The ritual maintains institutional authority while the substantive constraint atrophies.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__judicial_interpretive_drift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, judicial interpretive drift is an inherent feature of constitutional governance: written law always requires interpretation, and interpretation inevitably drifts as contexts change and judges disagree. The constraint appears immutable — a law of political epistemology rather than a contingent institutional arrangement. However, the presence of identifiable beneficiaries (federal judiciary, political majorities) reveals this as a false summit: the 'inevitability' of drift naturalizes the institutional discretion that benefits these actors.
constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__judicial_interpretive_drift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__judicial_interpretive_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__judicial_interpretive_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fourteenth_amendment_equal_protection__judicial_interpretive_drift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__judicial_interpretive_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fourteenth_amendment_equal_protection__judicial_interpretive_drift, TR),
    TR >= 0.70.

:- end_tests(fourteenth_amendment_equal_protection__judicial_interpretive_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits significant extraction: the formal guarantee of equal protection coexists with substantive unpredictability, creating a mechanism where groups must litigate with uncertain success rates. However, extraction is not maximal (as it would be in a snare) because the constraint does provide genuine coordination value — marginalized groups can invoke the equal-protection principle, and judges can reach decisions through doctrine rather than raw force. The 0.58 value reflects a constraint roughly balanced between coordination and extraction, with extraction slightly dominant. The upward trajectory from 0.25 to 0.58 reflects increasing drift: the constraint's meaning has become less stable and more contingent on forces (judicial ideology, political composition) outside the control of those seeking protection. Suppression (0.65): Moderate-high. Substantial barriers to successful equal-protection litigation: the three-tier framework creates doctrinal escape routes (rational basis is nearly insurmountable for government to lose under), the tier selection is outcome-dependent, and judicial composition determines outcomes. Groups cannot easily overcome these barriers, but the barriers are not absolute (some equal-protection claims do succeed). Theater ratio (0.68): Moderate-high. The doctrine increasingly performs a legitimacy function that exceeds its substantive effect: courts invoke equal-protection language to justify outcomes that would be difficult to defend without the label, even as the doctrine's actual constraints on government action have become weaker and more contingent. This rise from 0.35 to 0.68 reflects increasing gap between the ritual of equal-protection review and the substantive meaning of the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how the same legal structure produces radically different classifications depending on the observer's position. Marginalized groups experience it as a snare: they are trapped in a system that formally promises equal protection but delivers unpredictably based on forces outside their control. Civil rights advocates experience it as tangled rope: they benefit from the organizational focal point and legal principle, but face high costs from doctrinal unpredictability. The judiciary experiences it as pure coordination: the vague standard enables them to reach decisions flexibly without formal rule-making. Political majorities experience it as tangled rope: it provides coordination when their judges align with their values, extraction when they oppose. Reform movements experience it as scaffold: they frame drift as a temporary problem under construction with a coherence pathway (fixing doctrine). The historical rhetoric experiences it as piton: the ritual persists through institutional inertia despite degraded function. The civilizational analytical observer risks experiencing it as a mountain — drift as inevitable — when in fact it is a contingent institutional arrangement that benefits those with power over judicial appointments. The false summit reveals itself: the claim that interpretive drift is inherent to constitutional governance naturalizes the institutional discretion that primarily benefits the federal judiciary and political majorities aligned with contemporary judicial composition.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is determined by the agent's structural position. Marginalized groups seeking protection (powerless/trapped) have no exit option and bear full extraction cost, producing high d → high f(d) → high χ (the constraint is experienced as maximally extractive). Civil rights advocates (moderate/constrained) have some resources and some exit options (they can pursue non-litigation strategies), producing moderate d → moderate f(d) → moderate χ (mixed experience). The federal judiciary (institutional/arbitrage) benefits from the constraint's discretion, producing low d from beneficiary status → low f(d) → low or negative χ (they experience it as coordination, not extraction). Political majorities (powerful/mobile) have options (electoral control of appointments), producing moderate-high d but with power dampening the experienced severity. The directionality values converge with time: as doctrinal drift increases, marginalized groups' d value rises (they become more clearly targets), while the judiciary's d value remains low (they continue to benefit). The constraint's asymmetry is structural and temporal: it benefits those with control over meaning-making (the judiciary, political majorities shaping appointments) while extracting from those dependent on stable doctrine (marginalized groups whose success depends on doctrinal ceiling, not floor).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the classification as tangled_rope (coordination + extraction, requiring both beneficiaries and victims) is correct from the system-level view, but perspectival classification reveals the asymmetry: the coordination benefits primarily institutional actors (the judiciary), while the extraction costs are borne primarily by victims seeking equal protection. The constraint is not 'either a rope or a snare' — it is genuinely both, with the distribution of benefits and costs highly asymmetric. The theater component (rising from 0.35 to 0.68) indicates that the coordination function is increasingly performed rather than substantive — the ritual persists while the actual constraint on unequal treatment has weakened. This is the mandatrophy's resolution: the constraint maintains institutional legitimacy (through the equal-protection rhetoric) while its extractive effect has increased (marginal groups face higher barriers to successful litigation as the three-tier framework becomes more fluid). The false summit at the analytical level reveals what was hidden: that treating drift as 'inherent to constitutional law' naturalizes what is actually an institutional arrangement that benefits those with power to shape interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_fixation_ambiguity,
    'What does ''equal protection'' mean at the moment of the Fourteenth Amendment''s ratification (1868), and what constitutes ''equal'' treatment in contemporary contexts the framers did not anticipate (digital discrimination, algorithmic bias)?',
    'Historical analysis of 1868 intent vs. contemporary application cases; examination of whether original-meaning interpretation can accommodate unprecedented discrimination modalities without itself drifting.',
    'If original intent can be applied coherently to contemporary cases: originalism provides a ceiling on drift (mountain-like stability). If original intent is indeterminate or requires interpretation to apply: originalism itself incorporates drift, and the constraint remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_fixation_ambiguity, conceptual, 'Whether original intent provides a stable anchor for equal-protection meaning across centuries').

omega_variable(
    strict_scrutiny_ceiling_collapse,
    'Has strict scrutiny (the highest level of judicial review for suspect classifications) become so permissive that it functions as rational-basis review in practice, collapsing the three-tier framework into a single standard?',
    'Empirical analysis of strict-scrutiny outcomes: success rate of government surviving strict scrutiny in race and gender cases; comparison to rational-basis survival rates across time periods; identification of cases where strict scrutiny was applied but the government prevailed without compelling justification.',
    'If strict scrutiny has collapsed: the three-tier framework is theater, and the constraint is primarily piton (ritualized without function). If strict scrutiny remains a meaningful ceiling: the framework provides some protection, and the constraint is more rope-like (genuine coordination function). Extraction magnitude depends on this empirical pattern.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_scrutiny_ceiling_collapse, empirical, 'Whether strict scrutiny has lost functional meaning in practice').

omega_variable(
    judicial_composition_determination,
    'To what extent do equal-protection outcomes depend on the identity and ideology of the judges hearing the case, versus depending on the legal doctrine itself?',
    'Predictive analysis: build models predicting equal-protection outcomes from judicial composition alone (without reference to doctrine). Compare predictive accuracy to models using doctrine. Measure correlation between judge ideology and equal-protection voting patterns across cases with similar legal questions.',
    'If judges'' ideology predicts outcomes better than doctrine: the constraint is primarily extraction-mechanism (snare from the victim''s perspective), and drift is not incidental but constitutive. If doctrine predicts outcomes better than ideology: the constraint is more rope-like (genuine doctrine-based coordination). This determines whether the constraint''s unpredictability is feature or bug.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_composition_determination, empirical, 'The relative weight of judicial ideology vs. legal doctrine in equal-protection outcomes').

omega_variable(
    kernel_reading_coherence,
    'This constraint instantiates one reading of the Fourteenth Amendment equal-protection kernel — the reading that emphasizes judicial interpretive discretion and its consequences. What are the alternative readings of the same kernel, and how do they differ in their structural relationships to the same base text?',
    'Identification of sibling readings: (a) originalist reading anchored to 1868 meaning; (b) living-constitutionalist reading emphasizing evolving meaning; (c) critical reading emphasizing structural suppression of marginalized groups; (d) nationalist reading emphasizing federal supremacy. Documentation of how each reading structures the same legal text differently and produces different ε values.',
    'If alternative readings have significantly different ε values: the kernel contains multiple constraints. If they converge: the ambiguity is internal to each reading, not a result of reading choice. Determines whether interpretive drift is a property of doctrine or a property of the reading-selection process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coherence, conceptual, 'Identification and structural differentiation of sibling readings of the equal-protection kernel').

omega_variable(
    institutional_extraction_temporal_dynamics,
    'Does judicial interpretive drift serve the long-term institutional interests of the judiciary in maintaining authority and discretion, even when short-term drift disadvantages particular judges or courts?',
    'Historical analysis of judicial support for vague constitutional language vs. support for precise rules; comparison of institutional-coherence rhetoric (judges arguing for judicial flexibility) across ideological lines; examination of whether even originalist judges resist constraints that would limit judicial authority.',
    'If drift systematically benefits institutional judiciary regardless of judges'' stated doctrine: the constraint is an extraction mechanism (tangled_rope or snare from victim perspective). If drift is incidental to other institutional pressures: the constraint is more rope-like. Determines whether the beneficiaries are stable or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_extraction_temporal_dynamics, empirical, 'Whether interpretive drift systematically serves institutional judicial interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__judicial_interpretive_drift, 1868, 2013).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eq_prot_drift_theater_1868, fourteenth_amendment_equal_protection__judicial_interpretive_drift, theater_ratio, 1868, 0.35).
narrative_ontology:measurement(eq_prot_drift_theater_1954, fourteenth_amendment_equal_protection__judicial_interpretive_drift, theater_ratio, 1954, 0.5).
narrative_ontology:measurement(eq_prot_drift_theater_1991, fourteenth_amendment_equal_protection__judicial_interpretive_drift, theater_ratio, 1991, 0.62).
narrative_ontology:measurement(eq_prot_drift_theater_2013, fourteenth_amendment_equal_protection__judicial_interpretive_drift, theater_ratio, 2013, 0.68).

% Extraction over time
narrative_ontology:measurement(eq_prot_drift_extractiveness_1868, fourteenth_amendment_equal_protection__judicial_interpretive_drift, base_extractiveness, 1868, 0.25).
narrative_ontology:measurement(eq_prot_drift_extractiveness_1954, fourteenth_amendment_equal_protection__judicial_interpretive_drift, base_extractiveness, 1954, 0.42).
narrative_ontology:measurement(eq_prot_drift_extractiveness_1991, fourteenth_amendment_equal_protection__judicial_interpretive_drift, base_extractiveness, 1991, 0.55).
narrative_ontology:measurement(eq_prot_drift_extractiveness_2013, fourteenth_amendment_equal_protection__judicial_interpretive_drift, base_extractiveness, 2013, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eq_prot_drift_suppression_1868, fourteenth_amendment_equal_protection__judicial_interpretive_drift, suppression_requirement, 1868, 0.4).
narrative_ontology:measurement(eq_prot_drift_suppression_1954, fourteenth_amendment_equal_protection__judicial_interpretive_drift, suppression_requirement, 1954, 0.58).
narrative_ontology:measurement(eq_prot_drift_suppression_1991, fourteenth_amendment_equal_protection__judicial_interpretive_drift, suppression_requirement, 1991, 0.63).
narrative_ontology:measurement(eq_prot_drift_suppression_2013, fourteenth_amendment_equal_protection__judicial_interpretive_drift, suppression_requirement, 2013, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__judicial_interpretive_drift, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__judicial_interpretive_drift, strict_scrutiny_doctrinal_stability).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__judicial_interpretive_drift, rational_basis_review_floor_collapse).

% DUAL FORMULATION NOTE:
% The Equal Protection Clause is a kernel constitutional commitment. This story analyzes the judicial-interpretive-drift reading, which emphasizes how meaning drifts through time and across judges. Sibling readings (originalist, living-constitutionalist, critical, nationalist) would have different ε values and different perspectival structures. All readings share the same base text (the Amendment) but instantiate different constraints by structuring the interpretation layer differently. This story links to specific doctrinal constraints (strict scrutiny stability, rational basis floor) that are downstream of the reading's instantiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__judicial_interpretive_drift, institutional, 0.15).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__judicial_interpretive_drift, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
