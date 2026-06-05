% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Equality as Universal Principle Requiring Iterative Expansion (Universalist Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The universalist reading of 'all men are created equal' instantiates a
 *   specific constitutional authority claim: the founding documents employ
 *   universally quantified language that, once articulated, generates logical
 *   and political pressure for iterative expansion of the equality principle
 *   regardless of the founders' original intent or the document's historical
 *   scope of application. This reading contests the originalist reading
 *   (which bounds equality by 18th-century social taxonomy) and relates
 *   structurally to the textualist paradox reading (which identifies the
 *   irreconcilable contradiction between universal language and restricted
 *   application). The universalist reading differs from the textualist
 *   paradox reading by offering a substantive resolution: the tension is not
 *   a performative contradiction to be exposed but a generative principle
 *   that drives constitutional evolution. Marginalized groups—women, enslaved
 *   persons, Indigenous nations, and successively other categories of
 *   excluded populations—claim inclusion under the principle's own logic,
 *   using the universal language against the restricted application. This
 *   creates a structural constraint with genuine coordination function (the
 *   principle solves the legitimacy crisis created by universal language +
 *   restricted application) and genuine extraction (entrenched privilege must
 *   bear costs of expansion, enforcement mechanisms suppress counterclaims).
 *   The constraint exhibits classical tangled rope characteristics: both
 *   coordination and asymmetric extraction, active enforcement (through
 *   courts and social movements), beneficiaries and victims. The measurements
 *   show declining extractiveness and suppression over time (t0 to t100),
 *   with decreasing theater ratio, suggesting that as the universalist
 *   expansion becomes routinized (13th Amendment, 19th Amendment, Civil
 *   Rights jurisprudence), the temporary scaffolding of 'iterative expansion'
 *   stabilizes into normal constitutional practice. This trajectory indicates
 *   the constraint may be approaching a scaffold-to-rope transition as the
 *   expansion mechanism becomes normalized.
 *
 * KEY AGENTS:
 *   - Marginalized groups claiming inclusion (powerless/trapped → organized/constrained, generational timescale): The excluded populations (women, enslaved persons, Indigenous nations, religious minorities, sexual minorities, gender minorities). Structurally they are the subject of the constraint, not external to it; their only exit is inclusion or physical/institutional exit. Beneficiaries of the universalist principle insofar as it provides rhetorical and legal leverage; victims insofar as they bear the costs of non-inclusion and the constraints of integration.
 *   - Social movements (moderate/constrained, biographical timescale): Abolitionists, suffragists, civil rights activists, LGBTQ+ rights advocates. Experience the constraint as mixed: constrained by legal and social barriers (arrest, violence, resource limitations) but also experiencing coordination value from the universalist principle as rhetorical tool. The contradiction between universal language and restricted application is their chief argument.
 *   - The judiciary (organized/mobile, generational timescale): Courts interpreting the equal protection clause and due process clause expansively. Experience the constraint primarily as coordination mechanism—expansive interpretation solves the legitimacy problem created by the universal language / restricted application contradiction. Have agency to refuse or embrace expansion; experience net benefit from restoring institutional legitimacy through expansion.
 *   - Entrenched privilege (powerful/constrained, biographical timescale): Slaveholders, patriarchal authority holders, racial majority groups, economic elites benefiting from exclusion. Face genuine costs from universalist expansion—loss of unquestioned authority, economic disruption, political power redistribution. Constrained exit because explicit rejection of the universalist principle requires delegitimizing the entire constitutional order.
 *   - Constitutional apparatus (institutional/constrained, civilizational timescale): The formal government structure, enforcement mechanisms, legal institutions. Experiences the universalist principle as scaffolding mechanism for resolving internal contradictions. Must expand equality scope or lose legitimacy. The constraint operates ON and THROUGH this apparatus.
 *   - Originalist institutional structure (institutional/constrained, biographical timescale): Courts, scholars, judges committed to originalist methodology. See the universalist reading as degraded performance—claiming fidelity to founding documents while systematically expanding their scope beyond founder intent. Maintain the ritual of constitutional authority ('we are implementing what was always there') while the actual mechanism is reinterpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.52).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.48).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Equality as Universal Principle Requiring Iterative Expansion (Universalist Reading)").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, 'ea8dec5e-fcbb-46df-8fb2-c8564451942e').
narrative_ontology:cs_kernel_codification('ea8dec5e-fcbb-46df-8fb2-c8564451942e', fixed_text).
narrative_ontology:cs_authority_grounding('ea8dec5e-fcbb-46df-8fb2-c8564451942e', extraction).
narrative_ontology:cs_interpretation_layer_present('ea8dec5e-fcbb-46df-8fb2-c8564451942e').
narrative_ontology:cs_reading_relation('ea8dec5e-fcbb-46df-8fb2-c8564451942e', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea8dec5e-fcbb-46df-8fb2-c8564451942e', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('ea8dec5e-fcbb-46df-8fb2-c8564451942e', foundational, equality_principle_inherently_universal).
narrative_ontology:cs_axiom_status(equality_principle_inherently_universal, holdable).
narrative_ontology:cs_axiom_grounding('ea8dec5e-fcbb-46df-8fb2-c8564451942e', equality_principle_inherently_universal, deontological).
narrative_ontology:cs_axiom('ea8dec5e-fcbb-46df-8fb2-c8564451942e', foundational, iterative_expansion_legitimate_constitutional_mechanism).
narrative_ontology:cs_axiom_status(iterative_expansion_legitimate_constitutional_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('ea8dec5e-fcbb-46df-8fb2-c8564451942e', iterative_expansion_legitimate_constitutional_mechanism, instrumental).
narrative_ontology:cs_reference_frame('ea8dec5e-fcbb-46df-8fb2-c8564451942e', universal_equality_implicit_in_founding).
narrative_ontology:cs_drift_state('ea8dec5e-fcbb-46df-8fb2-c8564451942e', contemporary_post_civil_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ea8dec5e-fcbb-46df-8fb2-c8564451942e', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, expanding_constituencies).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, institutional_continuity_burden).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, entrenched_privilege_under_challenge).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EXCLUDED (SNARE) — Structurally trapped by legal exclusion from the promise of equality; the universal language of founding documents creates cognitive dissonance ('all men are created equal' applies to them intellectually but not legally). The constraint traps them in a state of acknowledged-but-denied inclusion. Exit from this category is structurally impossible without constitutional action — they are the subject matter of the constraint, not external to it.
constraint_indexing:constraint_classification(all_men_created_equal__universalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SOCIAL MOVEMENT (TANGLED ROPE) — Constrained by legal and social barriers (arrest, violence, resource limitations, risk of arrest), but also experiences coordination function: the universalist principle provides rhetorical and legal leverage. Movements benefit from the tension between universal language and restricted application — the contradiction is their chief argument. Moderate extraction relative to power; significant coordination value from the principle itself.
constraint_indexing:constraint_classification(all_men_created_equal__universalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE JUDICIARY INTERPRETING EXPANSION (ROPE) — Sees the universalist principle as coordination mechanism: applying the principle expansively solves the legitimacy problem created by the contradiction between universal language and restricted application. The judiciary has agency (can refuse expansion or embrace it) and experiences the constraint as coordination—expanding the circle of equality restores institutional legitimacy. Organized power with mobile exit options; experiences net coordination benefit.
constraint_indexing:constraint_classification(all_men_created_equal__universalist_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ENTRENCHED PRIVILEGE (TANGLED ROPE) — Faces genuine coordination problem (the universalist principle destabilizes their status and economic interests) alongside clear extraction: their legal immunity to equality claims is being systematically challenged. The universalist reading creates costs for this agent—loss of unquestioned authority, economic disruption, political power shifts. Constrained exit because refusing the universalist principle requires explicit repudiation of the founding documents' language, which delegitimizes the entire constitutional order.
constraint_indexing:constraint_classification(all_men_created_equal__universalist_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CONSTITUTIONAL APPARATUS (SCAFFOLD) — Experiences the universalist principle as a temporary scaffolding mechanism for resolving internal contradictions. The apparatus must either expand its scope of equality or lose legitimacy (the internal contradiction becomes fatal). The scaffold logic: as equality norms crystallize through repeated application (13th Amendment abolishing slavery, 19th Amendment establishing women's suffrage, Civil Rights Act, marriage equality), the temporary scaffolding of 'iterative expansion' becomes normalized, and the creative tension between universal language and expanding application diminishes. The sunset occurs when the iterative expansion mechanism becomes routinized into standard constitutional jurisprudence—expansion stops being exceptional and becomes expected.
constraint_indexing:constraint_classification(all_men_created_equal__universalist_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE ORIGINALIST AUTHORITY STRUCTURE (PITON) — From the perspective of institutionalized originalism (which claims to ground constitutional authority in founder intent), the universalist reading is a degraded performance: it claims fidelity to the founding documents while systematically expanding their scope beyond what the founders intended. Originalism sees the universalist principle as maintaining the ritual of constitutional authority ('we are just implementing what was always there') while the actual mechanism is amendment through reinterpretation. The theatrical performance is essential—originalism must claim that the universalist expansion IS what the founders meant, not what we now want. Theater ratio: high (the performance of textual fidelity masks the mechanism of reinterpretation).
constraint_indexing:constraint_classification(all_men_created_equal__universalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective grounded in the logic of natural rights and Enlightenment universalism, equality truly IS a universal principle that cannot coherently be bounded by historical accident or demographic happenstance. Once the principle is articulated, its internal logic demands expansion—any boundary is self-refuting given the principle's own premises. From this view, the universalist reading is simply the constraint of logic itself: universal principles generate universal obligations. However, the structural data (beneficiaries, victims, enforcement mechanisms) contradicts this classification—the engine will compute this as a false summit, revealing that the 'logical necessity' framing naturalizes what is actually a contested political and institutional struggle.
constraint_indexing:constraint_classification(all_men_created_equal__universalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(all_men_created_equal__universalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(all_men_created_equal__universalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(all_men_created_equal__universalist_reading, TR),
    TR >= 0.70.

:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, with trajectory rising from 0.35 to 0.52 over the interval. Initial extractiveness is lower because the universalist principle provides coordination value—it solves a real legitimacy problem (universal language cannot coherently bound to restricted application). As the constraint operates, extractiveness increases because entrenched privilege must bear accumulating costs (economic redistribution, power loss, status disruption). The principle extracts from those who benefited from exclusion and redistributes to those claiming inclusion. However, extractiveness peaks and then would begin to decline (not shown in measurements) as the expansion mechanism becomes routinized—the acute extraction phase transitions to normalized institutional practice. Suppression (0.48): Moderate, declining from 0.68 to 0.48 over the interval. Initial suppression is high because the constraint requires active suppression to maintain the contradiction between universal language and restricted application—exclusionary legal categories, police enforcement, institutional discrimination. As the universalist principle operates, suppression mechanisms weaken (Reconstruction amendments, Civil Rights Act, judicial rulings) and alternative discourses become available. The declining suppression trajectory reflects successful challenge to the exclusionary regime. Theater ratio (0.38): Moderate-low, declining from 0.55 to 0.38 over the interval. Initial theater is moderate because the constraint operates partly through ritualized legal and political performance—debates about constitutional meaning, jurisprudential arguments, formal amendment processes. As the universalist principle becomes institutionalized, the performative content decreases because the mechanism becomes normalized and routinized. The principle no longer requires the elaborate performance of constitutional interpretation to justify its scope; it becomes the background assumption of legal practice. Claimed type (tangled_rope): The constraint exhibits both genuine coordination function (solving the universal language / restricted application problem) and genuine extraction (costs borne by entrenched privilege, benefits captured by marginalized groups claiming inclusion). Active enforcement is required through courts, legislation, and social movement pressure. Beneficiaries and victims are clearly identified. These characteristics satisfy the tangled rope threshold: requires_active_enforcement=true, beneficiaries present, victims present, 0.40 ≤ χ ≤ 0.90.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives produce substantively different classifications from the same structural facts, illustrating the indexical nature of the constraint. The excluded population (powerless/trapped) experiences snare—structurally trapped by legal exclusion from a promise that applies to them logically but not institutionally. The social movement (moderate/constrained) experiences tangled rope—genuine barriers but also genuine leverage from the principle itself. The judiciary (organized/mobile) experiences rope—the constraint is primarily coordination, restoring legitimacy through expansion. Entrenched privilege (powerful/constrained) experiences tangled rope—loss of status (extraction) alongside a coordination problem they cannot escape without delegitimizing the entire order. The constitutional apparatus (institutional/constrained) experiences scaffold—the temporary scaffolding of iterative expansion will be normalized, reducing the acute tension. The originalist structure (institutional/constrained) experiences piton—maintains the ritual of constitutional fidelity while the actual mechanism is reinterpretation, sustained through institutional inertia and the need to preserve the fiction of continuity. The analytical observer (analytical/analytical) risks mountain—treating logical necessity as natural law and thereby naturalizing what is actually a contested institutional struggle. The perspectival divergence reveals that the constraint's classification depends fundamentally on where the observer sits: who benefits, who bears costs, what exit options are available, how much agency the actor possesses.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the universalist principle. The excluded population occupies d ≈ 0.95 (full target of the constraint—they are the subject matter, and their status is what the constraint is about). Social movements occupy d ≈ 0.65 (moderate target—they bear costs of repression but also benefit from the principle's leverage). The judiciary occupies d ≈ 0.30 (partial beneficiary—expansion solves a problem that benefits the institution's legitimacy). Entrenched privilege occupies d ≈ 0.80 (moderate-high target—bearing significant extraction costs from redistribution and power loss). The constitutional apparatus occupies d ≈ 0.50 (symmetric—benefits from legitimacy restoration, bears costs of managing contradiction). The originalist structure occupies d ≈ 0.55 (slight target—the universalist reading destabilizes their claimed interpretive authority). These d values feed the sigmoid function f(d) to produce experienced extractiveness chi, which varies across perspectives. The excluded population experiences high chi (trapped exit, high d → high f(d)). The judiciary experiences low or negative chi (arbitrage exit, low d → negative f(d)). Entrenched privilege experiences moderate-high chi (constrained exit, high d → moderate f(d)). The perspectival variation in d is not measurement noise—it reflects genuine structural differences in who the constraint governs, who it benefits, and what costs different agents bear.
 *
 * MANDATROPHY ANALYSIS:
 *   The universalist reading does not resolve the mandatrophy at the structural level—it provides a specific position within an ongoing contested framework. The mandatrophy here is: 'Is the principle universal or bounded by founder intent?' The universalist reading answers: 'The principle is universal and generates expansion pressure.' This is not a FALSE MANDATROPHY (a mislabeling problem) but a PERSISTENT MANDATROPHY (an irreducible structural tension). The constraint exhibits tangled rope characteristics (mixed coordination and extraction) because the universalist principle genuinely coordinates (solves legitimacy problem) AND genuinely extracts (redistributes from entrenched privilege to excluded populations). Both aspects are real and structural. The mandate to classify is partially underdetermined because the three readings are genuinely coequal—no reading logically defeats the others within their respective frameworks. However, the empirical observations (rising extractiveness, declining suppression, declining theater over time) provide diagnostic evidence: the constraint is moving from acute tension (high suppression, high theater) toward routinization (lower suppression, lower theater), suggesting that the universalist expansion mechanism is achieving institutionalization. This trajectory would indicate a transition from tangled_rope (during acute conflict) toward rope or scaffold (as the mechanism normalizes), consistent with the scaffold perspective's prediction that iterative expansion would eventually become routine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_intent_vs_principle_logic,
    'Does the universalist principle''s logical structure override founder intent, or is founder intent a binding constraint on the principle''s scope regardless of its logical form?',
    'Comparative constitutional jurisprudence: examination of how other democracies with explicit universalist founding texts (France, South Africa) have resolved the tension between expansive principle and historical intent. Analysis of whether any universalist text has been interpreted to exclude groups despite its language.',
    'If logical structure overrides intent: universalist reading is justified as inevitable constraint. If intent binds principle: universalist expansion is reinterpretation (and therefore subject to critique as judicial overreach). If both bind simultaneously: creates permanent structural tension (explains the piton and tangled_rope classifications).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founder_intent_vs_principle_logic, conceptual, 'Whether universal principle logic overrides founder intent').

omega_variable(
    expansion_mechanism_legitimacy,
    'Is iterative expansion of equality through constitutional interpretation a legitimate constitutional mechanism, or is it informal amendment that bypasses the Article V formal amendment process?',
    'Historical analysis of landmark equality expansions (Reconstruction amendments, Civil Rights Act, voting rights cases): did they occur through formal amendment (13th, 19th Amendments) or through reinterpretation (civil rights cases, marriage equality jurisprudence)? Examination of whether reinterpretation produces different legitimacy crisis trajectories than formal amendment.',
    'If reinterpretation is legitimate: universalist reading is a valid constitutional mechanism. If it constitutes informal amendment: it lacks democratic authorization and creates instability (explains suppression value). If both mechanisms coexist with different legitimacy: explains why some universalist expansions (formal amendments) are stable while others (judicial reinterpretation) remain contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expansion_mechanism_legitimacy, conceptual, 'Legitimacy of iterative expansion as constitutional mechanism').

omega_variable(
    universalist_anchor_stability,
    'Does the universalist principle have a stable terminus (all groups eventually included, expanding to zero excludable populations), or is the expansion process infinite with no endpoint?',
    'Analysis of contemporary inclusion debates: are there logical or practical endpoints to equality expansion (e.g., legal personhood for all humans definitionally equals), or do new categories of exclusion appear as each prior category is included (expanding from race to gender to sexual orientation to gender identity to cognitive difference to non-human animals)? Pattern analysis of whether expansion rates accelerate or decelerate over time.',
    'If stable terminus exists: universalist principle is genuinely achievable (eventual equilibrium). If expansion is infinite: the principle generates perpetual instability and perpetual extraction from whoever occupies the boundary of inclusion. Affects whether the constraint is Tangled Rope (temporary coordination problem with endpoint) or Snare (perpetual mechanism with expanding victim set).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universalist_anchor_stability, empirical, 'Whether universalist expansion has stable terminus').

omega_variable(
    kernel_reading_contest_status,
    'Is this reading of ''all men are created equal'' one of three coequal positions within ongoing constitutional contestation, or has the universalist reading achieved hegemonic institutional status making alternative readings marginal?',
    'Analysis of contemporary constitutional jurisprudence and political discourse: frequency of universalist framing vs. originalist framing in Supreme Court opinions, legislative debates, public discourse. Measurement of institutional capture: do universities, courts, civil rights organizations predominantly employ universalist framings? Are originalist and textualist readings treated as serious alternatives or as fringe positions?',
    'If coequal: all three readings remain live political choices with real structural consequences. If universalist reading is hegemonic: the constraint may have achieved its scaffold goal (normalized expansion) and shifted from tangled_rope/snare to rope/piton (coordination mechanism becoming institutionalized and theatrical). Affects classification of the entire constraint and its mandatrophy status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_status, empirical, 'Institutional status of universalist reading vs. alternative readings').

omega_variable(
    beneficiary_extraction_asymmetry,
    'Do marginalized groups claiming inclusion experience the universalist principle as liberatory (coordination benefit), or do they experience it as extractive (the principle requires them to fit themselves into majoritarian legal categories, bearing the costs of integration while the majoritarian structure remains unchanged)?',
    'Qualitative analysis of marginalized group discourse: do movements emphasize the universalist principle as leverage (yes → coordination benefit, supports lower chi) or criticize it as assimilationist constraint requiring conformity to dominant norms (yes → extraction, supports higher chi)? Historical analysis of integration vs. separatism debates within equality movements.',
    'If liberatory: beneficiaries genuinely benefit, chi is moderate, constraint is closer to tangled_rope with real coordination. If extractive: the principle demands conformity as the price of inclusion, chi is higher, constraint leans toward snare. This ambiguity explains why the constraint has both beneficiary and victim categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_extraction_asymmetry, empirical, 'Whether universalist principle is liberatory or assimilationist for marginalized groups').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amce_univ_theater_t0, all_men_created_equal__universalist_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(amce_univ_theater_t50, all_men_created_equal__universalist_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(amce_univ_theater_t100, all_men_created_equal__universalist_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(amce_univ_extractiveness_t0, all_men_created_equal__universalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(amce_univ_extractiveness_t50, all_men_created_equal__universalist_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(amce_univ_extractiveness_t100, all_men_created_equal__universalist_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(amce_univ_suppression_t0, all_men_created_equal__universalist_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(amce_univ_suppression_t50, all_men_created_equal__universalist_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(amce_univ_suppression_t100, all_men_created_equal__universalist_reading, suppression_requirement, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% The universalist reading is one of three structurally distinct interpretations of the kernel 'all men are created equal.' The textualist paradox reading (epsilon ≈ 0.58, tangled_rope) exposes the logical contradiction between universal language and restricted application. The originalist reading (epsilon ≈ 0.32, rope/piton) claims founder intent bounds scope. The universalist reading (epsilon ≈ 0.52, tangled_rope) claims the principle's logic drives expansion. All three readings are linked via the shared kernel; each has its own epsilon, perspectives, and beneficiary/victim structure because they instantiate genuinely different structural claims about how the principle operates. The network relationship is coequal contestation, not hierarchical dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
