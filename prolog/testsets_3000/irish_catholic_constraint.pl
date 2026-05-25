% ============================================================================
% CONSTRAINT STORY: irish_catholic_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irish_catholic_constraint, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: irish_catholic_constraint
 *   human_readable: Irish Catholic Identity Constraint: Religious, Cultural, and Economic Integration
 *   domain: social/religious/cultural
 *
 * SUMMARY:
 *   The Irish Catholic constraint represents a historical coordination
 *   mechanism that has undergone significant structural transformation. For
 *   much of Irish history (particularly post-Famine through mid-20th
 *   century), Catholicism functioned as a genuine coordination apparatus: it
 *   provided healthcare, education, social welfare, collective identity, and
 *   political representation in a context of colonial oppression and economic
 *   scarcity. The constraint delivered real coordination benefits and was
 *   experienced as legitimate by most participants. However, this
 *   institutional function has been systematically displaced by the Irish
 *   state, leaving the constraint increasingly dependent on identity fusion
 *   and suppression rather than genuine coordination value. The constraint
 *   now exhibits characteristics of all six DR types depending on
 *   observational position: it appears as an immutable feature of Irish
 *   identity (false mountain), a coordination mechanism to those who benefit
 *   institutionally (rope), a temporary problem being solved by generational
 *   secularization (scaffold), a degraded ritual system (piton), and pure
 *   extraction to those whose reproductive autonomy and sexual identity are
 *   suppressed (snare). The constraint's evolution from functional
 *   coordination to enforced identity-lock represents a key case study in
 *   institutional degradation and the mechanisms by which contingent power
 *   arrangements persist through naturalization.
 *
 * KEY AGENTS:
 *   - Individual Believers: Primary victims (powerless/identity_locked) — identity constituted through Catholic membership; face high suppression of reproductive autonomy, sexual identity, and doctrinal dissent; structurally mobile but cognitively trapped
 *   - Catholic Institutional Hierarchy: Primary beneficiaries (institutional/arbitrage) — bishops, Vatican representatives, seminary networks; experience constraint as pure coordination mechanism providing billions in tithes, property, political influence; have exit options and perceive value
 *   - Community Cohesion Apparatus: Secondary beneficiary (institutional) — parish networks providing mutual aid, lifecycle rituals, social insurance; genuine coordination function but increasingly displaced by state institutions
 *   - Organized Dissident Coalitions: Secondary victims (organized/constrained) — feminist networks, LGBTQ+ Catholic groups, reproductive justice organizations; experience extraction of autonomy but have capacity to coordinate alternatives
 *   - Irish State Secular Institutions: Sunset mechanism (organized/mobile) — state education system, public healthcare, civil marriage law, contraceptive access; building alternative pathways that bypass Catholic constraint; reducing suppression for younger generations
 *   - Secular Irish Nationals: Beneficiaries of sunset transition (organized/mobile) — younger, educated, urban cohorts with access to secular alternatives; face declining suppression; mobile exit options
 *   - Analytical Observer: External perspective (analytical/analytical) — risks naturalizing contingent institutional power as immutable Irish identity feature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irish_catholic_constraint, 0.58).
domain_priors:suppression_score(irish_catholic_constraint, 0.72).
domain_priors:theater_ratio(irish_catholic_constraint, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irish_catholic_constraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(irish_catholic_constraint, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(irish_catholic_constraint, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irish_catholic_constraint, tangled_rope).
narrative_ontology:human_readable(irish_catholic_constraint, "Irish Catholic Identity Constraint: Religious, Cultural, and Economic Integration").
narrative_ontology:topic_domain(irish_catholic_constraint, "social/religious/cultural").

domain_priors:requires_active_enforcement(irish_catholic_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irish_catholic_constraint, catholic_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(irish_catholic_constraint, community_cohesion_apparatus).
narrative_ontology:constraint_victim(irish_catholic_constraint, individual_autonomy).
narrative_ontology:constraint_victim(irish_catholic_constraint, doctrinal_dissenters).
narrative_ontology:constraint_victim(irish_catholic_constraint, women_reproductive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-LOCKED BELIEVER (SNARE) — Individual whose identity is constituted through Catholic membership. Structurally mobile (has income, legal protections, geographic options) but functionally trapped because exit would require abandoning family identity, community belonging, and self-concept. The binding mechanism is cognitive/identity fusion rather than material imprisonment. High extraction experienced: obligatory tithing, reproductive constraints, doctrinal submission, sexual identity suppression. Suppression operates through social shunning, family estrangement, and internalized shame rather than state coercion. The agent cannot perceive mutability from within the identity frame.
constraint_indexing:constraint_classification(irish_catholic_constraint, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: MODERATE PARTICIPANT (TANGLED ROPE) — Agent with some institutional leverage and education (moderate power). Faces high but surmountable costs to exit: career damage in certain social contexts, relationship rupture, loss of community networks. Genuinely benefits from community coordination (mutual aid, lifecycle rituals, social insurance through parish networks). Also bears extraction: reproductive autonomy constraints, mandatory confession cycles, financial obligations. Can perceive exit as costly but possible. Mixed extraction and coordination experienced simultaneously.
constraint_indexing:constraint_classification(irish_catholic_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHURCH HIERARCHY (ROPE) — Institutional actors (bishops, Vatican representatives, seminary networks) experience the constraint as a coordination mechanism: the Catholic identity apparatus coordinates billions in tithes, property holdings, political influence, and social legitimacy. For these actors, the constraint is pure coordination with minimal coercive overhead from their perspective — they have arbitrage options (move between parishes, dioceses, institutional roles) and experience the constraint as providing value. No extraction is perceived by beneficiaries.
constraint_indexing:constraint_classification(irish_catholic_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ORGANIZED DISSIDENT COALITION (TANGLED ROPE) — Collective agents (feminist networks, LGBTQ+ Catholic groups, reproductive justice organizations) with capacity to organize and articulate alternatives. Experience high extraction (reproductive autonomy constraints, sexual identity suppression, women's leadership exclusion) but also have exit pathways through coalition formation and alternative institutions (secular feminist networks, progressive churches, rights organizations). Organized power partially offsets powerlessness through collectivity. Can coordinate to shift norms and reduce suppression.
constraint_indexing:constraint_classification(irish_catholic_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL THEATER (PITON) — From a civilizational timescale, the Catholic identity apparatus in Ireland has degraded from a primary coordination mechanism (pre-1980s: genuine provider of education, healthcare, social stability) to increasingly performative maintenance. Theater ratio (0.68) reflects: confirmation rituals that few internalize fully, marriage vows with high exit rates, confession cycles that have lost explanatory force, institutional sexual abuse concealment theater. The institution maintains itself through inertia, not functional necessity. Modern Ireland provides education, healthcare, and social services through secular institutions. The constraint persists because alternatives haven't fully replaced it organizationally, not because it works.
constraint_indexing:constraint_classification(irish_catholic_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SUNSET COALITION (SCAFFOLD) — Secular institutions (state education system, public healthcare, civil marriage law, contraceptive access) are building alternative pathways that bypass the Catholic constraint apparatus. For agents with mobile exit options (younger generations, educated cohorts, urban populations), suppression has declined substantially post-2000. The constraint's enforcement power has a visible sunset: each generation is less constrained than the previous. This is genuine sunset, not aspirational — Irish secularization metrics show measurable generational decline in religious adherence and institutional compliance.
constraint_indexing:constraint_classification(irish_catholic_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: FALSE SUMMIT (MOUNTAIN MISCLASSIFICATION) — Risk of naturalizing Irish Catholic identity as an immutable feature of Irish ethnicity ('Catholic is what it means to be Irish') rather than as a contingent institutional constraint. This perspective falsely classifies the constraint as a mountain (unchangeable law of cultural identity). However, historical data contradicts this: Irish Catholicism's dominance post-1800 was contingent on British colonial policy (penal laws created incentive for identity fusion; independence created state enforcement apparatus). Modern secularization in Ireland demonstrates the constraint's mutability. The mountain classification is a false summit — the naturalizing framing obscures contingent institutional power.
constraint_indexing:constraint_classification(irish_catholic_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irish_catholic_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irish_catholic_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irish_catholic_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irish_catholic_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irish_catholic_constraint, TR),
    TR >= 0.70.

:- end_tests(irish_catholic_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The constraint extracts reproductive autonomy (contraceptive prohibition, abortion restriction, procreative coercion), sexual identity (LGBTQ+ suppression), doctrinal autonomy (confession cycles, mandatory belief frameworks), and financial obligation (tithing). However, extraction is not maximal (0.72 historical level) because state institutions now provide essential services (education, healthcare) independently, reducing the institutional leverage. The reduction from 0.72 to 0.58 reflects real displacement of the constraint's coordination function. Suppression (0.72): High. Multiple suppression mechanisms operate: family alienation for apostates, career barriers in certain professions, geographic clustering effects (small communities with limited anonymity), internalized shame from confession-based guilt structures, legal prohibition of reproductive options (pre-2018). Suppression operates primarily through social mechanisms (shunning, family pressure) rather than state coercion, but is real and substantial. Theater ratio (0.68): High. Significant performative content in sacramental rituals, confessional cycles, and institutional compliance: many believers report going through motions without internalized conviction. The theater ratio has increased over time as belief adherence has declined but institutional participation norms persisted — agents perform compliance without conviction. Post-2000, theater has declined as younger cohorts reduce performative participation entirely (generational sunset effect). Claimed type (Tangled Rope): The constraint exhibits both genuine coordination (community mutual aid networks, identity belonging) and asymmetric extraction (reproductive autonomy, sexual identity suppression). Active enforcement is required: the hierarchy maintains doctrinal boundaries, family pressure enforces participation norms, and state institutions historically reinforced Catholic educational/healthcare monopolies. Beneficiaries are clear (institutional hierarchy); victims are clear (doctrinal dissenters, women, LGBTQ+ persons). All tangled rope gates are satisfied.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim perspectives is extreme and diagnostic. The institutional hierarchy (Rope) and identity-locked believer (Snare) see radically different constraints occupying the same social space. From the hierarchy's position, the constraint is a successful coordination mechanism generating billions in tithes, property holdings, political influence, and social legitimacy. From the identity-locked believer's position, the constraint is pure extraction: reproductive autonomy is restricted (no contraception, no abortion, procreative coercion), sexual identity is suppressed (LGBTQ+ people are classified as disordered), doctrinal freedom is constrained (mandatory belief frameworks, confession-based guilt cycles), and exit is psychologically impossible (identity fusion makes leaving feel like self-annihilation). The moderate participant (Tangled Rope) experiences both simultaneously: the parish provides genuine mutual aid and community belonging (coordination benefit) while simultaneously constraining reproductive choices and enforcing performative compliance (extraction cost). This gap is not resolvable by saying 'it depends on your position' — the constraint genuinely IS both rope and snare depending on structural location. The analytical observer risks falsely resolving this gap by naturalizing the constraint as an immutable law of cultural identity, which would hide the institutional power dynamics generating the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each agent's structural relationship to the constraint. The institutional hierarchy has d ≈ 0.05 (full beneficiary with arbitrage options) → f(d) ≈ -0.12 (low/negative effective extraction from their perspective, pure coordination value). Identity-locked individuals have d ≈ 0.92 (near-full target with identity_locked exit) → f(d) ≈ 1.34 (maximum experienced extraction). Moderate participants with constrained exit have d ≈ 0.65 (victim with high-cost exit) → f(d) ≈ 1.00 (moderate extraction). Organized coalitions have d ≈ 0.58 (victim with organized capacity to build alternatives) → f(d) ≈ 0.75 (reduced extraction due to agency). Younger generations with mobile exit options have d ≈ 0.45 (weakening victim relationship, increasingly mobile) → f(d) ≈ 0.45 (low extraction). The directionality gradient directly reflects the historical displacement of the constraint's coordination function: as exit options improve (state institutions provide alternatives), d decreases and experienced extractiveness drops. No override is necessary — the structural derivation chain captures the real institutional dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via perspectival plurality: The constraint satisfies all Tangled Rope gates (genuine coordination + asymmetric extraction + active enforcement + beneficiaries + victims) and correctly prevents mislabeling. However, the constraint's full structural reality requires ALL SEVEN perspectives to capture: the institutional coordination (Rope from beneficiary), the pure extraction (Snare from identity-locked), the mixed experience (Tangled Rope from moderate), the organized resistance (Tangled Rope from coalition), the temporary problem (Scaffold from sunset mechanism), the degraded ritual (Piton from civilizational view), and the false naturalization risk (Mountain from analytical observer). The mandatrophy is resolved by recognizing that no single type captures the constraint — the presheaf of types over the observation site IS the answer. The institutional hierarchy genuinely experiences Rope because they receive net benefit and have arbitrage options. The identity-locked individual genuinely experiences Snare because they bear extraction with no exit. These are not competing interpretations of a single type; they are genuine structural divergences. The Tangled Rope classification at the claimed_type level captures the dominant structure (mixed extraction/coordination) while the perspectives reveal the full complexity. This prevents false summit naturalization while maintaining diagnostic precision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_boundary,
    'Is the suppression experienced by believers primarily identity-lock (cognitive/internalized) or constrained exit (external barriers with costs)?',
    'Post-exit cohort studies: measure suppression persistence after institutional exit. If suppression drops sharply post-exit, identify as primarily internalized. If suppression persists (family alienation, economic penalty, social exclusion), identify as primarily external constrained barriers. Distinction via longitudinal well-being tracking.',
    'If identity-locked: classification remains Snare at identity_locked exit. If constrained: reclassify to Snare at constrained exit with lower effective suppression. Affects both base_properties.suppression calibration and exit_options assignment for victim perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_boundary, empirical, 'Identity lock vs external constraint distinction in suppression mechanism').

omega_variable(
    generational_sunset_rate,
    'What is the actual generational decline rate in Irish Catholic institutional compliance and belief adherence? Is it decelerating (approaching floor) or maintaining linear/exponential decay?',
    'Time series analysis of Irish Census religious adherence data (1991, 2002, 2011, 2016, 2022); church attendance surveys; sacrament participation rates across age cohorts; international comparison with other post-Christian societies (France, Spain, Portugal, Italy).',
    'If maintaining linear decline: scaffold sunset is real and structural. If decelerating toward floor: residual constraint may persist at lower intensity indefinitely. Affects theater_ratio trajectory and Piton classification longevity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generational_sunset_rate, empirical, 'Generational decline trajectory in Irish Catholic institutional compliance').

omega_variable(
    extraction_vs_coordination_in_community_provision,
    'Is the Catholic apparatus''s provision of healthcare, education, and social services genuinely coordinated mutual aid or extractive gatekeeping (requiring religious participation as access price)?',
    'Comparison of service availability, quality metrics, and accessibility between Catholic institutional providers and secular state alternatives in Ireland post-2000. Analysis of service access patterns for religious minorities, non-believers, and apostates. Cost structure transparency.',
    'If genuine coordination: beneficiaries classification justified; constraint is Rope from institutional perspective. If extractive gatekeeping: constraint should be reclassified toward higher Snare proportions. Affects base_properties.beneficiaries definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_coordination_in_community_provision, empirical, 'Whether community provision is coordination or extractive gatekeeping').

omega_variable(
    identity_coherence_under_frame_shift,
    'Can an agent maintain Irish national/ethnic identity after exiting Catholic religious identity, or does the identity_locked mechanism make Irish ethnicity and Catholic membership inseparable in the agent''s self-concept?',
    'Qualitative interviews with Irish apostates and secular Irish nationals; longitudinal identity satisfaction surveys; analysis of identity coherence narratives. Cross-cultural comparison with other post-Christian nations (France, Spain) for available identity templates.',
    'If separable: identity_lock is contingent and potentially breakable through alternative identity frames. If inseparable: identity_lock is deeper and more resistant to exit. Affects exit_options classification and omega variable confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coherence_under_frame_shift, conceptual, 'Whether Irish identity and Catholic religious identity are separable in agent self-concept').

omega_variable(
    theater_ratio_vs_suppression_causality,
    'Does high theater_ratio (performative compliance) actually reduce suppression (agents go through the motions without internalizing), or does theater create additional suppression layers (cognitive dissonance, false consciousness, enforced performativity)?',
    'Longitudinal well-being studies correlating theater participation (frequency of performative rituals without belief) with reported autonomy, authenticity, and psychological distress. Analysis of confessional secrecy effects and performative compliance burden.',
    'If theater reduces suppression: suppression estimate should be lowered. If theater increases suppression: theater_ratio is a suppression multiplication factor, not replacement. Affects overall suppression calibration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_vs_suppression_causality, empirical, 'Whether performative theater reduces or amplifies suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irish_catholic_constraint, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icc_tr_t0, irish_catholic_constraint, theater_ratio, 0, 0.45).
narrative_ontology:measurement(icc_tr_t20, irish_catholic_constraint, theater_ratio, 20, 0.62).
narrative_ontology:measurement(icc_tr_t40, irish_catholic_constraint, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(icc_be_t0, irish_catholic_constraint, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(icc_be_t20, irish_catholic_constraint, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(icc_be_t40, irish_catholic_constraint, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irish_catholic_constraint, identity_coordination).
narrative_ontology:boltzmann_floor_override(irish_catholic_constraint, 0.12).
narrative_ontology:affects_constraint(irish_catholic_constraint, reproductive_autonomy_constraint).
narrative_ontology:affects_constraint(irish_catholic_constraint, sexual_identity_suppression).
narrative_ontology:affects_constraint(irish_catholic_constraint, doctrinal_dissent_suppression).
narrative_ontology:affects_constraint(irish_catholic_constraint, irish_state_secularization).

% DUAL FORMULATION NOTE:
% The Irish Catholic constraint decomposes into multiple distinct constraint stories along domain lines: reproductive autonomy constraint (ε≈0.75, Snare), sexual identity suppression (ε≈0.82, Snare), doctrinal dissent suppression (ε≈0.68, Tangled Rope), and community coordination provision (ε≈0.25, Rope). Each has distinct observables, distinct omegas, and distinct measurement trajectories. The unified Irish Catholic constraint story represents the institutional apparatus through which these domain-specific constraints are enforced. Upstream: Irish state secularization (ε≈0.15, Scaffold with sunset) reduces the institutional apparatus's enforcement capacity. Downstream: individual domain constraints inherit the sunset mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irish_catholic_constraint, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
