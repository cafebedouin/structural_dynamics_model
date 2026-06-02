% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed Authority (Liturgical Habituation Reading)
 *   domain: systematic_theology/ecclesiology/christian_doctrine
 *
 * SUMMARY:
 *   The Nicene Creed (325 CE) functions as an identity boundary marker in
 *   liturgical performance, independent of whether participants cognitively
 *   assent to its metaphysical claims. This reading models the creed as a
 *   pure coordination mechanism — the rhythmic, intergenerational recitation
 *   of fixed formulaic language stabilizes ecclesiastical identity and
 *   enables doctrinal variance to exist within a single institutional
 *   structure. The creed's authority is grounded not in enforcing belief but
 *   in maintaining a performative boundary: one recites the creed in the
 *   assembly, therefore one participates in the apostolic faith regardless of
 *   interior metaphysical interpretation. This reading contrasts with the
 *   strict_orthodox_reading (which treats the creed as enforcing substantive
 *   doctrinal assent) and the symbolic_confessional_reading (which treats the
 *   creed as expressing plural meanings that accommodate theological
 *   diversity). The liturgical_habituation_reading focuses on the structural
 *   function of repetition, belonging, and intergenerational transmission
 *   independent of truth-conditions. Extractiveness is very low (ε=0.08)
 *   because the constraint functions as pure coordination: the creed creates
 *   a shared linguistic space that permits both orthodox and heterodox
 *   interior belief while maintaining visible ecclesiastical unity.
 *   Suppression is minimal (0.12) because liturgical participation is
 *   voluntary (exit via leaving the tradition, though socially costly).
 *   Theater ratio (0.35) reflects that enforcement of the creed's recitation
 *   is primarily performative — modern Catholic and Orthodox Christianity
 *   enforce the creed through liturgical rubrics and participatory rhythm
 *   rather than doctrinal interrogation.
 *
 * KEY AGENTS:
 *   - Liturgical Participants (Moderate Power, Constrained Exit) — Primary coordinators of identity through repetition; experience the creed as belonging rather than belief requirement
 *   - Ecclesiastical Authority Structure (Institutional Power, Arbitrage Exit) — Maintains creed codification and liturgical enforcement; benefits from low-cost coordination across doctrinal variance
 *   - Heterodox Theologians Within Tradition (Powerful, Mobile Exit) — Can assert non-standard metaphysics while performing the creed; experience constraint as enabling interior variance
 *   - Historical Analyst (Analytical Position, Universal Scope) — Observes the creed's functional transformation from doctrinal enforcement (4th century) to performative identity (modern era)
 *   - Metaphysical Philosopher (Natural Law Perspective) — Risks naturalizing contingent institutional arrangements as metaphysical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.12).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed Authority (Liturgical Habituation Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "systematic_theology/ecclesiology/christian_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, '06166275-8641-4522-b557-f0b6d80577ac').
narrative_ontology:cs_kernel_codification('06166275-8641-4522-b557-f0b6d80577ac', fixed_text).
narrative_ontology:cs_authority_grounding('06166275-8641-4522-b557-f0b6d80577ac', lineage).
narrative_ontology:cs_interpretation_layer_present('06166275-8641-4522-b557-f0b6d80577ac').
narrative_ontology:cs_reading_relation('06166275-8641-4522-b557-f0b6d80577ac', nicene_creed_authority__strict_orthodox_reading, coexists_with).
narrative_ontology:cs_reading_relation('06166275-8641-4522-b557-f0b6d80577ac', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_axiom('06166275-8641-4522-b557-f0b6d80577ac', foundational, performative_participation_sufficient_for_identity).
narrative_ontology:cs_axiom_status(performative_participation_sufficient_for_identity, holdable).
narrative_ontology:cs_axiom_grounding('06166275-8641-4522-b557-f0b6d80577ac', performative_participation_sufficient_for_identity, instrumental).
narrative_ontology:cs_axiom('06166275-8641-4522-b557-f0b6d80577ac', foundational, interior_metaphysical_variance_compatible_with_visible_unity).
narrative_ontology:cs_axiom_status(interior_metaphysical_variance_compatible_with_visible_unity, holdable).
narrative_ontology:cs_axiom_grounding('06166275-8641-4522-b557-f0b6d80577ac', interior_metaphysical_variance_compatible_with_visible_unity, conventional).
narrative_ontology:cs_reference_frame('06166275-8641-4522-b557-f0b6d80577ac', apostolic_tradition_through_liturgical_transmission).
narrative_ontology:cs_drift_state('06166275-8641-4522-b557-f0b6d80577ac', contemporary_post_vatican_ii, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('06166275-8641-4522-b557-f0b6d80577ac', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, liturgical_community).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, ecclesiastical_authority_structure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITURGICAL PARTICIPANT (ROPE) — Constrained by social belonging and intergenerational tradition transmission. Experiences the creed recitation as coordination: performs identity alongside others in collective rhythm. The constraint functions as pure coordination mechanism — no extraction perceived, only rhythmic belonging. Exit costs are high (religious community loss) but primarily relational, not material coercion.
constraint_indexing:constraint_classification(nicene_creed_authority__liturgical_habituation_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: ECCLESIASTICAL AUTHORITY (ROPE) — Institutional actor maintaining creed codification and liturgical performance norms. Experiences the constraint as coordination infrastructure: the creed stabilizes doctrinal variance across dioceses without requiring coercive dogmatic testing. Benefits from the creed as an information standard that enables decentralized agreement. Arbitrage option (can reinterpret or modify creed language) but chooses maintenance for coordination value.
constraint_indexing:constraint_classification(nicene_creed_authority__liturgical_habituation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: HETERODOX THEOLOGIAN WITHIN TRADITION (ROPE) — Can assert non-standard metaphysical interpretations while performing creed liturgically. Mobile exit option (can leave tradition entirely or establish schism) but constrained within tradition if cognitive assent differs from liturgical performance. Experiences the constraint as enabling coordination between doctrinal diversity and visible unity — the creed functions precisely as an identity boundary that permits interior interpretation variance.
constraint_indexing:constraint_classification(nicene_creed_authority__liturgical_habituation_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: HISTORICAL ANALYST (PITON) — Views the creed from post-Christendom analytical distance. The liturgical recitation persists through institutional inertia: the creed's original function (defining orthodoxy against Arianism in 4th century) has been replaced by performative identity marking, yet the authority structure maintains the original codification language. Theater ratio high (0.35) because the creed's enforcement mechanism has atrophied — modern Catholicism and Orthodox Christianity enforce creedal identity primarily through liturgical participation, not doctrinal policing. The creed's authority is theatrical maintenance of legitimacy rather than active gate-keeping.
constraint_indexing:constraint_classification(nicene_creed_authority__liturgical_habituation_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: NATURAL LAW VIEW (MOUNTAIN) — From a metaphysical standpoint, the creed's content (God's nature, Christ's relation to Father, Holy Spirit's procession) claims to describe unchangeable realities. If these metaphysical claims are true, the creed's authority is immutable natural law. However, this perspective confuses the metaphysical truth-condition with the constraint's structural function. The constraint is about identity performance and boundary maintenance, not metaphysical assertion. The engine's false summit detector will flag this as naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(nicene_creed_authority__liturgical_habituation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, TR),
    TR >= 0.70.

:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The creed as a liturgical coordination mechanism does not extract from participants — it coordinates their identity. No beneficiary group gains material or institutional advantage from the creed's performance. The ecclesiastical authority benefits from coordination infrastructure, but this is coordination benefit (enabling doctrinal variance within institutional unity), not extraction benefit (taking value from others). Suppression (0.12): Minimal. Participants are not coerced to recite the creed beyond social belonging costs, which are real but relational rather than material. Exit options are genuinely available (leave the tradition) though socially expensive. The constraint does not suppress alternatives because the creed does not require exclusive belief — interior metaphysical variance coexists. Theater ratio (0.35): Moderate. The creed's modern function is performative — the enforcement mechanism targets participation in the liturgical rhythm, not cognitive assent. Medieval scholasticism had higher theater (0.38) because theologians performed doctrinal precision while the creed remained unchanged language. Modern post-Vatican II Catholicism lowered theater (0.35) by explicitly decentering cognitive assent in favor of lived ecclesial participation. The creed's authority persists as theatrical maintenance of legitimacy: the church says 'we recite this because we have always recited this,' which is performative authority, not doctrinal gate-keeping.
 *
 * PERSPECTIVAL GAP:
 *   The creed's classification ranges from rope (coordination view) to piton (degraded authority view) to mountain (naturalized metaphysics view). The liturgical participant sees pure rope — the constraint enables identity coordination without belief coercion. The heterodox theologian sees rope with high tolerance — the creed permits interior variance. The ecclesiastical authority sees rope with institutional benefit — coordination infrastructure that stabilizes visible unity. The historical analyst sees piton — the creed's original enforcement function (doctrinal gate-keeping) has been replaced by performative identity maintenance. The metaphysical philosopher risks mountain — treating the creed's metaphysical content as naturally law rather than contingent institutional performance. This perspectival gap reveals the core function: the creed works precisely because it separates performative identity from cognitive assent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The creed's beneficiaries are the liturgical community (benefits from coordinated identity) and the ecclesiastical authority structure (benefits from coordination infrastructure). Neither group is a victim of the creed — there is no extraction target. The liturgical participant (moderate power, constrained exit) experiences the constraint as coordination, not extraction. The ecclesiastical authority (institutional power, arbitrage exit) experiences the constraint as enabling their institutional function. The heterodox theologian (powerful, mobile exit) experiences the constraint as permitting interior variance. Because there are no victims and no high-power extraction, directionality values remain low (d ≈ 0.1–0.3), resulting in low effective extraction chi. The constraint classifies as rope across all perspectives because the structural data shows coordination without asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that the creed functions as pure coordination (rope) rather than coercive extraction (snare) or false natural law (mountain). The strict_orthodox_reading might generate mandatrophy if it claims the creed is simultaneously a coordination mechanism (binding diverse orthodoxies to visible unity) and an enforcement mechanism (requiring cognitive assent to specific metaphysical propositions). This reading avoids the tension by decoupling performance from belief: the creed coordinates identity through liturgical participation regardless of interior metaphysical variance. The natural law perspective (mountain) risks mandatrophy by treating metaphysical content as grounds for institutional authority, but the structural data reveals authority is grounded in performative function, not truth-conditions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_assent_vs_performative_identity,
    'Is the creed''s binding function grounded in requiring cognitive metaphysical assent, or does it function as identity boundary regardless of interior belief?',
    'Historical and ethnographic analysis: (a) Council of Nicaea enforcement records and doctrinal testing practices vs modern liturgical-only enforcement; (b) Contemporary theological diversity within creedal traditions (Catholic/Orthodox acceptance of non-Thomist metaphysics, Anglican spectrum); (c) Comparative analysis of schism causes — were breaks over doctrinal belief or over liturgical/institutional identity?',
    'If cognitive assent required: constraint is closer to snare (coercive belief discipline). If performative identity sufficient: constraint is pure rope (coordination without belief coercion). This omega distinguishes the strict_orthodox_reading (assent-based) from liturgical_habituation_reading (performance-based).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_assent_vs_performative_identity, empirical, 'Whether creed enforcement requires cognitive metaphysical assent or only performative liturgical participation').

omega_variable(
    ecclesiastical_authority_grounding_shift,
    'Did the authority grounding for the creed shift from doctrinal enforcement (4th–7th century) to performative identity maintenance (post-Reformation to present)?',
    'Historical periodization: (a) Canons and enforcement mechanisms from ecumenical councils; (b) Theological treatises on creedal authority across historical periods; (c) Institutional response to doctrinal heterodoxy — prosecution vs tolerance patterns; (d) Liturgical evolution — creed''s role in medieval mass vs modern liturgy.',
    'If shift occurred: liturgical_habituation_reading is accurate for contemporary constraint; strict_orthodox_reading describes a historical constraint that has transformed. If no shift: single constraint across centuries with changing enforcement capacity (piton rather than rope). Affects network.affects_constraints linkage and temporal scope of this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecclesiastical_authority_grounding_shift, empirical, 'Historical shift in ecclesiastical authority grounding from doctrinal enforcement to performative identity').

omega_variable(
    interior_variance_tolerance_limit,
    'What range of interior metaphysical variance can be accommodated within a single creedal tradition while maintaining the creed''s identity-boundary function?',
    'Theological and institutional analysis: (a) Boundaries of Catholic/Orthodox/Anglican acceptance (Thomist vs Scotist metaphysics within Catholicism, Filioque variance within Orthodoxy, Anglo-Catholic spectrum); (b) Schism cases where interior variance exceeded tradition''s tolerance; (c) Comparative analysis of traditions with different tolerance ranges (Reformation fragmentation vs Orthodox/Catholic unity despite variance).',
    'If tolerance is high: creed functions as pure coordination (very low extractiveness, rope classification stable). If tolerance is low: creed enforces substantive metaphysical positions (higher extractiveness, tangled_rope or snare possible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interior_variance_tolerance_limit, empirical, 'Institutional tolerance for interior metaphysical variance within creedal identity').

omega_variable(
    reading_under_determination_kernel_drift,
    'This reading instantiates one interpretation of the Nicene Creed authority kernel. The sibling readings (strict_orthodox_reading, symbolic_confessional_reading) represent alternative framings of the same authority claim. What structural observation would definitively distinguish which reading correctly captures the constraint''s function?',
    'Observation of institutional enforcement patterns under doctrinal challenge: (a) When a liturgically-participating member asserts metaphysical variance (e.g., denying Filioque, affirming Nestorianism), does the institution enforce creedal orthodoxy cognitively or merely require continued liturgical performance? (b) When schism threatens (e.g., Filioque controversy), does the institution prioritize doctrinal uniformity or liturgical/communion unity? (c) Post-Vatican II and modern Orthodox dialogue: acceptance of modified creedal language (filioque ecumenical revisions) — does this represent authority migration from content to performance?',
    'If enforcement targets cognitive assent: strict_orthodox_reading is correct (constraint is snare or tangled_rope). If enforcement targets performative participation: liturgical_habituation_reading is correct (constraint is rope). If enforcement permits interior variance: symbolic_confessional_reading is correct (constraint is scaffold or piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_under_determination_kernel_drift, empirical, 'Kernel reading under-determination: which sibling reading correctly captures the creed''s functional authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_ratio_4th_century, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(theater_ratio_medieval, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 500, 0.32).
narrative_ontology:measurement(theater_ratio_scholastic, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1000, 0.38).
narrative_ontology:measurement(theater_ratio_reformation, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1500, 0.42).
narrative_ontology:measurement(theater_ratio_modern, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1900, 0.35).

% Extraction over time
narrative_ontology:measurement(extractiveness_4th_century, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(extractiveness_medieval, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 500, 0.14).
narrative_ontology:measurement(extractiveness_scholastic, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1000, 0.11).
narrative_ontology:measurement(extractiveness_reformation, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1500, 0.09).
narrative_ontology:measurement(extractiveness_modern, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1900, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__liturgical_habituation_reading, 0.06).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% The Nicene Creed authority kernel generates three distinct constraint stories, each with different ε values reflecting different functional readings. The strict_orthodox_reading has higher ε (≥0.30) because enforcing cognitive assent involves coercion. The symbolic_confessional_reading has moderate ε (0.15–0.25) because accommodating plural interpretations reduces enforcement intensity. The liturgical_habituation_reading has very low ε (0.08) because performative participation requires no coercion beyond social belonging. All three are live readings in contemporary ecclesiastical practice and theology; none forecloses the others. They represent different authority groundings (doctrinal enforcement, interpretive accommodation, performative maintenance) that coexist across the Catholic, Orthodox, and Protestant traditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
