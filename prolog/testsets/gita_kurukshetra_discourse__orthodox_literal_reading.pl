% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Gita Kurukshetra Discourse (Orthodox Literal Reading): Caste-Based Duty and Righteous Violence
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The Bhagavad Gita's Kurukshetra discourse, read through the orthodox
 *   literal lens, mandates caste-based duty and legitimates righteous
 *   violence by divine authority. This reading interprets Krishna's discourse
 *   to Arjuna as establishing an eternal cosmic hierarchy (the varna system)
 *   in which each person's duty (dharma) is determined by birth caste, and in
 *   which kshatriya warriors may wage war without moral culpability when
 *   acting as duty-bound. The constraint operates through textual authority:
 *   the sacred text, interpreted by Brahmin gatekeepers as requiring literal
 *   acceptance of caste and war authorization, becomes a mechanism for
 *   locking lower castes into subordination and for legitimating violence
 *   against non-combatants and dissenting voices. The constraint exhibits
 *   high extractiveness (0.68) because the benefits flow primarily to the
 *   Brahmin interpretive class (who retain monopoly authority) and the
 *   kshatriya warrior elite (who are freed from moral responsibility for
 *   killing), while the costs fall on lower castes (trapped in divinely
 *   ordained subordination) and non-combatants (whose deaths are reframed as
 *   cosmically acceptable). Suppression is very high (0.72) because the
 *   constraint requires active enforcement: Brahmin interpretive authority
 *   must be defended against reform readings (allegorical, universalist,
 *   Gandhian), caste hierarchy must be policed through social sanction, and
 *   alternative textual interpretations must be suppressed as heterodox.
 *   Theater ratio has risen from 0.35 to 0.55 over the interval as the
 *   constraint's functional coordination role (legitimating warrior duty in a
 *   specific mythological war) has degraded, but its use as a symbol in
 *   political and communal contexts has increased. The constraint is one
 *   reading of a contested kernel: the Gita text itself is the kernel; this
 *   orthodoxy is one reading; Gandhian allegorical reading and universalist
 *   devotional reading are sibling readings coexisting across different
 *   communities and interpretive traditions.
 *
 * KEY AGENTS:
 *   - Brahmin Interpretive Class: Primary beneficiary (institutional/arbitrage) — maintains exclusive authority over scriptural exegesis; legitimated as mediators of cosmic order; controls which readings are 'orthodox'
 *   - Kshatriya Warrior Elite: Secondary beneficiary (powerful/arbitrage) — freed from moral culpability for violence; authorized to act without questioning; structurally dependent on Brahmin interpretive validation
 *   - Lower Castes (Shudra/Dalit): Primary victim (powerless/trapped) — subordination presented as divinely ordained and cosmically necessary; trapped by religious doctrine with no theological exit
 *   - Non-Combatants in War: Collateral victim (powerless/trapped) — deaths legitimated as acceptable within righteous-war doctrine; voiceless in the constraint structure
 *   - Reform and Allegorical Reading Movements: Organized challengers (organized/mobile) — face suppression and gatekeeping but have exit options through alternative publication and moral appeals
 *   - Post-Colonial Hindu Nationalists: Institutional appropriators (powerful/constrained) — attempt to deploy the literal reading to legitimize caste and communal violence; experience degraded function (theater_ratio increase) as they invoke authority structure without institutional machinery to sustain it
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent interpretation as eternal cosmic law; vulnerable to false-summit detection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.68).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.72).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, snare).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Gita Kurukshetra Discourse (Orthodox Literal Reading): Caste-Based Duty and Righteous Violence").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '39a27a0f-01d6-4552-87dc-93bb20c931f2').
narrative_ontology:cs_kernel_codification('39a27a0f-01d6-4552-87dc-93bb20c931f2', fixed_text).
narrative_ontology:cs_authority_grounding('39a27a0f-01d6-4552-87dc-93bb20c931f2', lineage).
narrative_ontology:cs_interpretation_layer_present('39a27a0f-01d6-4552-87dc-93bb20c931f2').
narrative_ontology:cs_reading_relation('39a27a0f-01d6-4552-87dc-93bb20c931f2', gita_gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_reading_relation('39a27a0f-01d6-4552-87dc-93bb20c931f2', gita_universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('39a27a0f-01d6-4552-87dc-93bb20c931f2', foundational, varna_eternally_ordained).
narrative_ontology:cs_axiom_status(varna_eternally_ordained, holdable).
narrative_ontology:cs_axiom_grounding('39a27a0f-01d6-4552-87dc-93bb20c931f2', varna_eternally_ordained, deontological).
narrative_ontology:cs_axiom('39a27a0f-01d6-4552-87dc-93bb20c931f2', foundational, kshatriya_violence_duty_free).
narrative_ontology:cs_axiom_status(kshatriya_violence_duty_free, holdable).
narrative_ontology:cs_axiom_grounding('39a27a0f-01d6-4552-87dc-93bb20c931f2', kshatriya_violence_duty_free, deontological).
narrative_ontology:cs_axiom('39a27a0f-01d6-4552-87dc-93bb20c931f2', secondary, brahmin_interpretive_monopoly_legitimacy).
narrative_ontology:cs_axiom_status(brahmin_interpretive_monopoly_legitimacy, overridden).
narrative_ontology:cs_axiom_grounding('39a27a0f-01d6-4552-87dc-93bb20c931f2', brahmin_interpretive_monopoly_legitimacy, conventional).
narrative_ontology:cs_reference_frame('39a27a0f-01d6-4552-87dc-93bb20c931f2', eternal_varna_cosmos).
narrative_ontology:cs_drift_state('39a27a0f-01d6-4552-87dc-93bb20c931f2', contemporary_post_colonial_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('39a27a0f-01d6-4552-87dc-93bb20c931f2', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_elite).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, varna_hierarchy_structure).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes_shudra_dalit).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, non_combatants_in_war).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, alternative_readings_suppressed).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOWER CASTES (SNARE) — Trapped by religious doctrine claiming their subordination is divinely ordained and their duty is acceptance. No theological exit; their structural position in the varna system is presented as cosmically fixed. This perspective bears maximum extraction: their labor, compliance, and political powerlessness are legitimated as righteous duty, not recognized as oppression.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__orthodox_literal_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-COMBATANTS IN KURUKSHETRA (SNARE) — Those killed in the 'righteous war' authorized by Krishna's discourse have no agency, no exit, and no voice in the text. The constraint permits their deaths as collateral to kshatriya dharma. Maximum extraction and suppression — their deaths are reframed as acceptable within the logic of duty.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__orthodox_literal_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: BRAHMIN INTERPRETIVE CLASS (ROPE) — Benefits from exclusive interpretive authority over the Gita text. The orthodox literal reading locks in Brahmin monopoly on scriptural exegesis and legitimates their superior ritual and intellectual status. They experience this constraint as coordination: preserving the textual authority structure enables their continued role as mediators between cosmic order and human action. High beneficiary status with institutional arbitrage options.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__orthodox_literal_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: KSHATRIYA WARRIOR ELITE (TANGLED ROPE) — Benefit from divine authorization to wage war and kill without moral culpability (the Gita's core discourse to Arjuna). Experience genuine coordination function: the text provides philosophical framework for martial action and duty. But also extraction: the interpretation locks them into a role (warfare as duty) and subordinates them to Brahmin interpretive authority over what constitutes righteous action. Mixed experience: liberation from moral guilt but theological dependency.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-COLONIAL HINDU NATIONALISTS (PITON) — Attempt to deploy the orthodox literal reading to legitimize caste hierarchy and violence against minorities, but the constraint's function has degraded: the authority structure requires Brahmin interpretive gatekeeping that competing Hindu nationalist factions reject; the text's original coordination function (legitimating kshatriya duty in a specific mythological war) does not transfer to modern politics. Theater ratio is high because nationalist invocations perform traditional authority without the institutional machinery (caste-based hierarchy, Brahmin interpretive monopoly, warrior-duty framing) that originally sustained it. The constraint persists through inertia and political utility, not through functional coordination.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__orthodox_literal_reading, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ETERNALIST VIEW (MOUNTAIN) — From this perspective, the Gita's discourse on dharma and duty is an expression of eternal cosmic law (sanatan dharma) — divine truth that cannot be revised, reinterpreted, or rejected without cosmic consequence. The constraint appears immutable because it is grounded in the unchangeable structure of existence itself. This perspective naturalizes what the structural data reveals as a historically contingent interpretive choice, making it vulnerable to false-summit detection.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__orthodox_literal_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: REFORM MOVEMENTS (TANGLED ROPE) — Organizations and scholars advocating allegorical or universalist readings of the Gita experience this constraint as requiring active enforcement: the orthodox literal reading uses institutional authority (textual monopoly, religious gatekeeping, caste sanctions) to suppress alternative readings. Reform movements have exit options (they can publish, organize, appeal to post-colonial egalitarian values) but face significant suppression. They also derive some benefit from the same textual tradition (the Gita's actual philosophical depth and poetic power enable their reinterpretations). Structured as tangled hybrid: coordination function (interpreting sacred text) with asymmetric extraction (orthodox reading enforces silence on alternatives).
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gita_kurukshetra_discourse__orthodox_literal_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gita_kurukshetra_discourse__orthodox_literal_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, TR),
    TR >= 0.70.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): The constraint benefits the Brahmin interpretive class and kshatriya warrior elite while imposing costs on lower castes and non-combatants. The beneficiaries receive institutional authority, moral legitimacy, and freedom from moral accountability; the victims receive divine-mandate subordination and exclusion from interpretive authority. The extraction is not maximal (0.68 rather than 0.80+) because the constraint operates partially through internalized acceptance: lower castes who accept the doctrine as cosmically true experience lower measured suppression than if they were externally coerced. The extraction increases over the interval (0.52→0.68) as post-colonial Hindu nationalism has weaponized the literal reading to legitimize communal violence, expanding its extractive scope beyond the original caste-labor framework into political violence. Suppression (0.72): Very high. The constraint requires active enforcement of interpretive orthodoxy. Brahmin gatekeeping must suppress alternative readings (Gandhian, Ambedkarite, universalist) that preserve the Gita's philosophical power while rejecting caste hierarchy. Caste hierarchy itself must be enforced through social sanction. Violence authorized by the constraint must be reframed as cosmically acceptable rather than morally culpable. The suppression has intensified over the interval (0.58→0.72) as reform movements have gained institutional access (academic publishing, translation into vernacular languages, Dalit theology) — the constraint must work harder to maintain the literal reading's monopoly. Theater ratio (0.55): Moderate-high, increasing over time. The original function of the constraint was coordination: legitimating kshatriya martial duty in the specific mythological Kurukshetra war. That function has largely degraded — modern invokers of the constraint do not command the institutional machinery (caste hierarchy, Brahmin gatekeeping, warrior caste identity) that originally sustained the interpretation. The theater ratio increase (0.35→0.55) reflects rising performativity: nationalist invocations of 'Gita wisdom' and 'righteous war' do the symbolic work without the functional coordination. Post-colonial Hindu nationalists deploy the language of dharma and cosmic order without Brahmin interpretive mediation or organized warrior-duty structures. The rising theater ratio is a piton indicator in the nationalist context (Perspective 5: piton classification), showing how a constraint's function degrades while its symbolic invocation increases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The lower castes see pure snare: divine mandate for subordination with no exit. The warrior elite see tangled rope: freedom from moral guilt (benefit) with theological dependency (extraction). The reform movements see tangled rope with inverse asymmetry: they bear the cost of suppression (extraction) but retain agency and exit options (benefit). The Brahmin class sees rope: the constraint is coordination of textual authority and cosmic order. The post-colonial nationalists see piton: they invoke the authority structure without the institutional machinery, experiencing degraded function and rising theater. The eternalist analytical observer sees mountain: the Gita expresses eternal law. The materialist analytical observer would see snare: the constraint legitimates specific power arrangements. The perspectival gap reveals that the constraint is not a natural law but a contingent interpretation whose classification varies radically by observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the structural position relative to the extraction flow. The Brahmin interpretive class (institutional/arbitrage) has d≈0.05 (beneficiary with exit options) — they benefit maximally and can exit if needed (interpreters can choose alternative readings). The kshatriya warrior elite (powerful/arbitrage) has d≈0.20 (beneficiary with some exit cost) — they benefit from the reading but remain dependent on Brahmin validation and cannot freely abandon their warrior role without identity dissolution. Lower castes (powerless/trapped) have d≈0.95 (total targets with no exit) — they bear maximum extraction and have no theological or institutional exit; accepting the doctrine as cosmically true (identity lock) raises d slightly above pure trapped (0.98) but leaves them structurally immobile. Reform movements (organized/mobile) have d≈0.62 (victims with exit options) — they face suppression but can publish, organize, appeal to post-colonial egalitarianism. Post-colonial nationalists (powerful/constrained) have d≈0.45 (mixed beneficiary-victim) — they benefit from the reading's use as political authority but face escalating suppression from reform movements and international human rights pressure, constraining their deployment. The analytical observer (analytical/analytical) has d≈0.72 (witness to the whole structure) — analytically positioned to see that the constraint benefits specific agents at specific costs, but at risk of naturalizing the structure as eternal law rather than seeing it as contingent interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-LEVEL MANDATROPHY: This constraint instantiates the mandatrophy at the level of textual reading. The Gita kernel is contested across multiple readings (orthodox literal, Gandhian allegorical, universalist devotional). The orthodox literal reading claims to resolve the mandatrophy by presenting the text as unambiguous mandate for caste hierarchy and righteous violence. But this resolution is itself part of the constraint structure — the suppression of alternative readings is what maintains the literal reading's apparent necessity. The genuine mandatrophy resides in the kernel's ambiguity: does the Gita text genuinely mandate caste hierarchy and righteous violence, or does the orthodox reading impose that mandate through interpretive gatekeeping? Resolving this would require comparative textual analysis (omega_1), historiography of the tradition (omega_5), and philosophical analysis of dharma's logical structure (omega_3). Until resolved, the constraint operates as a snare maintained by enforced orthodoxy, not as a natural law or even as a transparent rope. The mandatrophy is partially resolvable through empirical study of the text and its interpretive history; it is partially conceptual (what counts as 'natural' vs 'imposed' reading?).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_allegorical_determinacy,
    'Does the Gita''s text itself privilege literal interpretation of caste-duty and righteous violence, or is literalism itself an interpretive choice imposed by orthodox gatekeeping?',
    'Comparative textual analysis: frequency and contextual weight of passages supporting literal caste hierarchy vs. passages supporting universal dharma and duty transcendent of varna. Cross-cultural hermeneutical analysis of how other Sanskrit texts (Mahabharata frame story, Upanishads) treat varna as contingent vs. eternal.',
    'If literalism is textually privileged: the constraint''s extractiveness is lower (ε→0.55) — the text genuinely mandates hierarchy. If literalism is imposed interpretation: extractiveness is higher (ε→0.78) — the constraint is primarily suppression of alternatives. Current judgment: literalism is an interpretive choice (ε remains at 0.68).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literal_vs_allegorical_determinacy, empirical, 'Whether literal caste hierarchy is textually determined or interpretively imposed').

omega_variable(
    brahmin_interpretive_monopoly_necessity,
    'Is Brahmin interpretive authority structurally necessary to the Gita''s philosophical coherence, or is it a historically accumulated institutional power that could be dispersed without textual loss?',
    'Analysis of the Gita''s actual philosophical arguments (karma, duty, self, brahman) for structural dependence on Brahmin gatekeeping. Comparison with successful non-Brahmin reinterpretations (Dalit theology, Ambedkarite readings, Gandhian reframing) that preserve philosophical depth while rejecting caste hierarchy.',
    'If monopoly is necessary: beneficiary status of Brahmin class (institutional/arbitrage) is justified by coordination function; suppression of alternatives is defensive. If monopoly is contingent: suppression becomes primary extraction mechanism; constraint reclassifies closer to pure snare (ε→0.75, suppression→0.82).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahmin_interpretive_monopoly_necessity, conceptual, 'Whether Brahmin interpretive monopoly is structurally necessary to the text''s coherence').

omega_variable(
    dharma_universalizability,
    'Can the Gita''s core concept of dharma (duty to one''s nature and role) be coherently universalized to transcend caste hierarchy, or does the concept necessarily presuppose a fixed varna system?',
    'Philosophical analysis of dharma''s logical structure: if duty is to one''s svabhava (nature), does nature include caste assignment as eternally binding, or can nature be understood as individual capacity and choice? Historical evidence: non-orthodox interpretations (Ramakrishna Paramahamsa, Sri Aurobindo, Ambedkar) that preserved dharma concept while rejecting caste fixity.',
    'If dharma requires caste fixity: universalist readings are incoherent; the orthodox reading''s suppression of alternatives is defensible. If dharma can be universalized: the constraint''s extraction mechanism is primarily suppression of conceptual alternatives, not textual determination; ε→0.72, theater→0.68.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dharma_universalizability, conceptual, 'Whether dharma concept can coherently transcend caste hierarchy').

omega_variable(
    righteous_war_authorization_scope,
    'Does Krishna''s authorization of righteous violence in the Gita apply only to the specific mythological Kurukshetra war, or does it generate a general doctrine of righteous violence deployable in any conflict?',
    'Textual scope analysis: specificity of Krishna''s discourse to Arjuna''s situation vs. universality claims. Historical analysis of how the constraint has been invoked: has it been limited to the mythological frame or extended to contemporary violence? Evidence from communal violence contexts where the constraint is invoked to justify killings.',
    'If scope is limited to mythology: the constraint''s extractiveness for modern violence claims is lower (through interpretive scoping) — ε→0.55. If scope is general doctrine: the constraint actively legitimates contemporary violence; ε→0.75, suppression→0.80.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(righteous_war_authorization_scope, empirical, 'Whether righteous-war authorization is mythologically specific or universally deployable').

omega_variable(
    reading_as_orthodoxy_vs_tradition,
    'Is the ''orthodox literal reading'' the dominant interpretation across Hindu textual tradition, or is it one historically contingent reading elevated to orthodoxy status by colonial-era and post-colonial institutional gatekeeping?',
    'Historiography of Gita commentary: analysis of pre-colonial interpreters (Shankara, Ramanuja, Madhva, Vallabha) and their positions on caste, violence, dharma. Evidence of whether literal caste hierarchy was universally endorsed or contested within the tradition. Post-colonial emergence of the ''literal orthodoxy'' as political doctrine.',
    'If orthodoxy is genuinely traditional: the constraint reflects stable cross-generational interpretive consensus; authority is distributed across the tradition. If orthodoxy is post-colonial construction: it is a snare maintained by institutional suppression, not organic consensus; ε→0.72, requires_active_enforcement confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_as_orthodoxy_vs_tradition, empirical, 'Whether literal orthodoxy represents traditional consensus or post-colonial gatekeeping').

omega_variable(
    kernel_reading_boundary,
    'Is this constraint the ''reading'' of the kernel (Gita text as sacred authority), or is the reading itself a separate constraint that happens to cite the kernel?',
    'Structural test: separate the constraint into (A) the kernel claim (Gita is divinely authoritative text) and (B) the reading claim (the orthodox literal interpretation is what that text mandates). If (A) and (B) can coherently coexist with alternative readings without logical contradiction, they are separate constraints linked by network dependencies, not reading and kernel.',
    'If they are logically separable: write two constraint stories — one for Gita authority itself (lower ε, mountain candidate), one for the orthodox reading (current ε=0.68, snare). If they are inseparable: the current story correctly models the binding (reading instantiates kernel authority).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the orthodox reading is inseparable from kernel authority or a separate constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_ortho_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gita_ortho_tr_t150, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 150, 0.48).
narrative_ontology:measurement(gita_ortho_tr_t250, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 250, 0.55).

% Extraction over time
narrative_ontology:measurement(gita_ortho_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gita_ortho_be_t150, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 150, 0.61).
narrative_ontology:measurement(gita_ortho_be_t250, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 250, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gita_ortho_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(gita_ortho_su_t150, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 150, 0.68).
narrative_ontology:measurement(gita_ortho_su_t250, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 250, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__orthodox_literal_reading, 0.12).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_universalist_devotional_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, caste_hierarchy_brahminical_legitimation).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, hindu_nationalist_communal_violence_authorization).

% DUAL FORMULATION NOTE:
% The Gita Kurukshetra discourse is one kernel read through three distinct constraint readings: orthodox literal (this file, ε=0.68), Gandhian allegorical (ε≈0.25), universalist devotional (ε≈0.30). Each reading has a distinct extractiveness because they identify different beneficiaries, victims, and mechanisms. The orthodox reading's high extractiveness is not inherent to the Gita text but to the interpretive choice and its enforcement. The network structure shows how the three readings compete for authority and how the orthodox reading's suppression of alternatives is itself part of the extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
