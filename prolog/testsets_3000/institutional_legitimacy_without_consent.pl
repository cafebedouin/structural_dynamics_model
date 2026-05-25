% ============================================================================
% CONSTRAINT STORY: institutional_legitimacy_without_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_legitimacy_without_consent, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_legitimacy_without_consent
 *   human_readable: Institutional Legitimacy Without Explicit Consent
 *   domain: political/institutional
 *
 * SUMMARY:
 *   Institutional legitimacy without explicit consent is a foundational
 *   structural constraint that enables state capacity while suppressing
 *   alternatives to that authority. The constraint operates at the
 *   intersection of political philosophy (the problem of authority),
 *   practical governance (how populations are coordinated), and individual
 *   experience (what citizens experience as their relationship to
 *   institutional power). This story examines how a single structural
 *   phenomenon — the assertion of institutional authority over populations
 *   who did not explicitly consent to that authority — classifies as six
 *   different constraint types depending on the observer's position. For the
 *   powerless subject with no exit, it appears as pure extraction (snare).
 *   For institutional beneficiaries with arbitrage options, it appears as
 *   coordination (rope). For organized reform movements, it appears as a
 *   transitional structure being superseded (scaffold). For the analytical
 *   observer at civilizational scale, it risks appearing as an inevitable law
 *   of governance (mountain), naturalizing what is actually a contingent
 *   institutional choice. The theater ratio increasing from 0.52 to 0.70 over
 *   the interval reflects growing institutional investment in legitimacy
 *   narratives (constitutions, anniversary ceremonies, patriotic education)
 *   as the lived coordination benefits of institutions decline relative to
 *   their coercive apparatus.
 *
 * KEY AGENTS:
 *   - Subject Populations: Primary victims (powerless/trapped) — bear the constraint with no meaningful exit or consent mechanism
 *   - Excluded Constituencies: Secondary victims (powerless/trapped) — groups without franchise, representation, or institutional voice
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — capacity to govern without continuous consent; ability to redirect institutional resources
 *   - Incumbent Power Holders: Secondary beneficiary (institutional/arbitrage) — status preservation through institutional authority legitimacy
 *   - Democratic Reform Movement: Organized agents (organized/constrained) — pushing toward consent-based models; see the current structure as transitional
 *   - Legacy Legitimacy Apparatus: Institutional system (institutional/arbitrage) — the machinery of legitimacy assertion; persisting through theater and inertia
 *   - Captured Officials: Internal agents (institutional/constrained) — subordinated to institutional rules they did not consent to; experience mixed coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_legitimacy_without_consent, 0.58).
domain_priors:suppression_score(institutional_legitimacy_without_consent, 0.65).
domain_priors:theater_ratio(institutional_legitimacy_without_consent, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_legitimacy_without_consent, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_legitimacy_without_consent, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_legitimacy_without_consent, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_legitimacy_without_consent, tangled_rope).
narrative_ontology:human_readable(institutional_legitimacy_without_consent, "Institutional Legitimacy Without Explicit Consent").
narrative_ontology:topic_domain(institutional_legitimacy_without_consent, "political/institutional").

domain_priors:requires_active_enforcement(institutional_legitimacy_without_consent).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_legitimacy_without_consent, institutional_leadership).
narrative_ontology:constraint_beneficiary(institutional_legitimacy_without_consent, incumbent_power_holders).
narrative_ontology:constraint_victim(institutional_legitimacy_without_consent, subject_populations).
narrative_ontology:constraint_victim(institutional_legitimacy_without_consent, excluded_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — Individuals born into institutional jurisdiction have no meaningful exit from governance structures. Legitimacy is asserted over them without their negotiated consent. They cannot choose to opt out of taxation, law, citizenship, or institutional authority. The constraint extracts compliance through suppression of alternative governance options and through internalization of institutional authority as natural or inevitable. Maximum experienced extraction.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL DISSIDENT (TANGLED ROPE) — Citizens in democratic contexts experience mixed signals: institutional legitimacy is partly coordinated (rule of law, public goods provision, collective defense) and partly extractive (coercive taxation, mandatory compliance, subordination to rules they did not consent to). Exit is costly but possible (emigration, exile, civil disobedience with penalties). The constraint provides genuine coordination benefits while simultaneously extracting compliance through legal threat.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL BENEFICIARY (ROPE) — State leadership and institutional power holders experience legitimacy assertion as pure coordination: it solves the collective action problem of how to govern a population without requiring explicit consent from every subject for every action. The institutional perspective sees this as necessary function, not extraction. They have arbitrage options (moving resources between jurisdictions, controlling information flows) and experience the constraint as enabling rather than constraining.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEMOCRATIC REFORM MOVEMENT (SCAFFOLD) — Organized agents pushing for deeper democratic participation, constitutional reform, and consent-based legitimacy see the current arrangement as a temporary structure being superseded. Sunset clause: as communication technologies enable direct participation and as democratic norms mature, the traditional representative model that asserts legitimacy without continuous consent becomes obsolete. The scaffold perspective sees this as a transitional institution — functional enough to enable state capacity, but intentionally being replaced by more participatory forms. Exit path is political reform within institutional structures.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY LEGITIMACY APPARATUS (PITON) — The institutional machinery of legitimacy assertion (constitutions, monuments, ritual ceremonies, public education narratives) persists largely through theater and inertia. The original function — explaining why an institution deserves obedience — has atrophied in societies where legitimacy relies increasingly on service provision and coercive capacity rather than moral authority. The apparatus maintains itself through narrative and symbols but has lost its original coordinating function. Theater ratio is high; actual persuasive power is degraded.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, some form of legitimacy assertion without universal individual consent appears inherent to governance: coordination of large populations always requires some mechanism to motivate compliance, and seeking explicit consent for every action is computationally infeasible. This perspective risks naturalizing what is actually a contingent institutional choice: that legitimacy flows from past constitutional moments and abstract citizenship rather than from explicit ongoing negotiation.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: CAPTURED INSTITUTIONAL ACTOR (TANGLED ROPE) — Officials and administrators within institutions experience legitimacy assertion as both coordinating (enabling state function) and extractive (subordinating them to rules and hierarchies they did not consent to). They have constrained exit options (resignation, transfer, whistleblowing with retaliation risk) and often become identity-locked to institutional roles. The constraint coordinates governance while extracting their compliance through epistemic capture: institutional framings become invisible as alternatives.
constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_legitimacy_without_consent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_legitimacy_without_consent, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_legitimacy_without_consent, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_legitimacy_without_consent, TR),
    TR >= 0.70.

:- end_tests(institutional_legitimacy_without_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts compliance and resources (taxation, labor, obedience to law) without explicit negotiated consent. However, it is not as severe as pure extraction (0.70+) because institutional coordination provides genuine benefits for most subjects (rule of law, security, public goods). The extraction is bundled with coordination, making it a Tangled Rope rather than a pure Snare from most perspectives. Suppression (0.65): High. Suppression operates through multiple mechanisms: legal prohibition on exit (citizenship cannot be unilaterally renounced), economic dependency on institutional provision, socialization into legitimacy narratives, and lack of knowledge about governance alternatives. However, suppression is incomplete — organized exit is possible (emigration, civil disobedience) even though costly. Theater ratio (0.68): Moderate-high. Legitimacy assertion depends heavily on performative elements: constitutional ceremonies, patriotic education, institutional symbolism, and narrative framing as 'the people' consenting through representative mechanisms. The gap between nominal consent (voting) and actual ongoing consent (continuous negotiation) is substantial. As institutional function increasingly relies on coercive capacity rather than moral authority, theater investment increases to maintain the legitimacy narrative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the widest perspectival gap in the corpus. The powerless subject population sees a Snare — they are trapped by institutional authority with no exit and receive no voice in that authority's decisions. The institutional beneficiary sees a Rope — they are solving the coordination problem of governing a large population; legitimacy assertion enables them to coordinate without seeking consent for every action. The democratic reformer sees a Scaffold — the current non-consensual model is being superseded by participatory technologies and democratic norm maturation. The legacy legitimacy apparatus operator sees a Piton — their machinery of constitutional narrative and ceremonial authority persists through inertia and theater, having lost the original persuasive power. The captured official sees a Tangled Rope — subjected to institutional rules they did not consent to, but also enabled by institutional coordination. The civilizational analyst risks seeing a Mountain — viewing legitimacy assertion without consent as inherent to governance — but the structural data reveals this as a false summit, a naturalization of contingent institutional choices. The perspectival gaps are driven by: (1) whether agents benefit from institutional coordination (beneficiaries → coordination frame) or bear extraction costs without benefit (victims → extraction frame), (2) exit options (arbitrage → coordination, trapped → extraction), and (3) power to shape institutional rules (institutional → sees coordination, powerless → sees extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) is determined by their structural position within the legitimacy-assertion flow. Subject populations with no exit and victim status receive d ≈ 0.95 (full targets of extraction). Institutional leadership with arbitrage options and beneficiary status receive d ≈ 0.05-0.15 (full beneficiaries). Moderate populations with constrained exit and mixed beneficiary/victim status receive d ≈ 0.55-0.65 (symmetric). Organized populations with exit paths and agency receive d ≈ 0.40-0.50 (lower target weight due to capacity). These d values feed the sigmoid f(d) to produce experienced extractiveness χ. A powerless trapped subject experiences χ ≈ ε × 1.42 ≈ 0.82, while an institutional arbitrage beneficiary experiences χ ≈ ε × (-0.12) ≈ -0.07 (coordination frame). The directionality derivation reveals why the same structural constraint appears as extraction to some agents and coordination to others — their exit options and benefit distribution determine whether they experience the constraint as targeting them or enabling them.
 *
 * MANDATROPHY ANALYSIS:
 *   Institutional legitimacy without consent resolves the mandatrophy by demonstrating that classification varies legitimately with structural position. There is no single 'correct' type — instead, the constraint's structural properties generate different classifications from different perspectives through the interaction of ε, beneficiary/victim status, exit options, and power. The false summit (mountain perspective naturalizing legitimacy assertion as inherent to governance) is diagnostically important: it reveals how institutional actors naturalize contingent choices as inevitable. The Scaffold perspective is crucial: it shows that the current non-consensual model is not permanent — democratic communication technologies and norm maturation create genuine alternatives. This resolution pattern is characteristic of institutional constraints that lack explicit distributional agreement: they appear immutable from the perspective of those who benefit from the status quo, but transformable from the perspective of those who suffer extraction or who see alternatives. The mandatrophy is resolved by recognizing that both perspectives are structurally accurate — they describe different aspects of the same phenomenon. The question shifts from 'which classification is correct?' to 'how do we navigate between these legitimate but incompatible perspectives?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_threshold_ambiguity,
    'What level and form of consent is sufficient to make institutional legitimacy based on consent rather than assertion?',
    'Comparative analysis of consent mechanisms (constitutional referenda, electoral cycles, ongoing plebiscites, deliberative forums); measurement of effective agency in decision-making vs nominal consent procedures',
    'If low threshold suffices: many existing institutions already meet consent standard and constraint is misclassified as snare. If high threshold required: virtually no institutions achieve true consent-based legitimacy, confirming snare classification for most populations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_threshold_ambiguity, conceptual, 'Threshold of what counts as meaningful consent').

omega_variable(
    alternative_governance_viability,
    'Are there structurally viable governance models that operate with continuous explicit consent rather than asserted legitimacy?',
    'Historical case study of consensus-based governance (Indigenous councils, cooperative structures, direct democracy experiments); analysis of failure modes and scalability limits',
    'If viable at scale: the ''inherent to governance'' framing (mountain perspective) is falsified — alternatives exist. If not viable: constraint may reflect genuine structural necessity rather than extractive choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_governance_viability, empirical, 'Whether continuous-consent governance models are scalable').

omega_variable(
    suppression_internalization_mechanism,
    'Is the suppression of exit options primarily structural (legal prohibition, resource barriers) or internalized (citizens internalize legitimacy narrative and cannot imagine alternatives)?',
    'Survey data on perceived alternatives to institutional authority; analysis of how rapidly exit options emerge when suppression is removed (post-imperial, post-colonial transitions); comparison of populations with different internalization histories',
    'If primarily structural: reducing legal barriers increases exit capacity and shifts constraint toward Rope. If primarily internalized: removing barriers alone does not increase exit; cognitive capture persists and constraint remains Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Structural vs internalized suppression in institutional legitimacy').

omega_variable(
    legitimacy_benefit_distribution,
    'Do all citizens receive genuine benefits from institutional coordination sufficient to justify the suppression of exit options?',
    'Analysis of public goods provision, access to justice, security provision, opportunity, and resource distribution across populations; identification of excluded constituencies receiving extractive costs without coordination benefits',
    'If benefits distributed: many perspectives shift from Snare toward Tangled Rope or Rope (coordination is real). If concentrated: constraint confirms as Snare even for moderate-power populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_benefit_distribution, empirical, 'Whether institutional benefits are distributed across all populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_legitimacy_without_consent, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(instleg_tr_t0, institutional_legitimacy_without_consent, theater_ratio, 0, 0.52).
narrative_ontology:measurement(instleg_tr_t25, institutional_legitimacy_without_consent, theater_ratio, 25, 0.6).
narrative_ontology:measurement(instleg_tr_t50, institutional_legitimacy_without_consent, theater_ratio, 50, 0.68).
narrative_ontology:measurement(instleg_tr_t75, institutional_legitimacy_without_consent, theater_ratio, 75, 0.7).

% Extraction over time
narrative_ontology:measurement(instleg_be_t0, institutional_legitimacy_without_consent, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(instleg_be_t25, institutional_legitimacy_without_consent, base_extractiveness, 25, 0.54).
narrative_ontology:measurement(instleg_be_t50, institutional_legitimacy_without_consent, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(instleg_be_t75, institutional_legitimacy_without_consent, base_extractiveness, 75, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_legitimacy_without_consent, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_legitimacy_without_consent, state_monopoly_on_violence).
narrative_ontology:affects_constraint(institutional_legitimacy_without_consent, representative_democracy_fiction).
narrative_ontology:affects_constraint(institutional_legitimacy_without_consent, taxation_without_continuous_consent).
narrative_ontology:affects_constraint(institutional_legitimacy_without_consent, citizenship_as_status_lock).

% DUAL FORMULATION NOTE:
% Institutional legitimacy without consent is upstream of specific governance mechanisms (taxation, law enforcement, military conscription). Each downstream constraint inherits the structural property of being asserted without continuous consent and adds domain-specific extraction mechanisms. This story addresses the general legitimacy framework; downstream stories address specific institutional domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_legitimacy_without_consent, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
