% ============================================================================
% CONSTRAINT STORY: interpretive_authority_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_interpretive_authority_concentration, []).

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
 *   constraint_id: interpretive_authority_concentration
 *   human_readable: Interpretive Authority Concentration in Post-Vatican II Catholicism
 *   domain: ecclesiastical_history/theological_doctrine
 *
 * SUMMARY:
 *   Vatican II (1962-1965) represents a critical institutional pivot in
 *   Catholic doctrine and practice. The council is commonly presented as a
 *   unified reinterpretation of Catholic tradition achieved through collegial
 *   discernment — a genuine theological and pastoral renewal. However, the
 *   constraint story reveals a structurally more complex reality: Vatican II
 *   operated simultaneously as a coordination mechanism (solving real
 *   problems of ecumenical engagement, pastoral modernization, and doctrinal
 *   clarity in secular societies) and as an extraction mechanism
 *   (concentrating interpretive authority in the magisterium, displacing
 *   competing theological schools, centralizing control over practice). The
 *   constraint's theater ratio (0.68) reflects that much of the conciliar
 *   process and subsequent implementation involved performative displays of
 *   continuity (the 'hermeneutics of continuity') despite substantial
 *   doctrinal and liturgical change. The rising extractiveness (0.32 → 0.58)
 *   and suppression (0.40 → 0.65) over the 1962-1975 interval track the shift
 *   from open conciliar dialogue to enforcement of unified interpretation.
 *   The kernel question is whether Vatican II is one contested reading of
 *   Catholic continuity (supporting the Tangled Rope classification) or
 *   multiple distinct doctrinal movements artificially bundled (supporting
 *   Snare).
 *
 * KEY AGENTS:
 *   - Traditional Practice Communities: Primary victim (powerless/identity_locked) — parishes and lay societies whose liturgical and doctrinal identity predates Vatican II; trapped by identity fusion with pre-conciliar forms
 *   - Vatican Administrative Core (Curia): Primary beneficiary (institutional/arbitrage) — consolidates interpretive authority, achieves centralization of doctrinal control, gains power to enforce unified reading
 *   - Episcopal Modernizer Faction (periti-advised bishops): Secondary beneficiary (institutional/arbitrage) — shapes the conciliar reinterpretation, achieves ecumenical and pastoral modernization, enhanced institutional authority
 *   - Diocesan Clergy: Secondary victim (moderate/constrained) — face career pressures to accept new framework; constrained by episcopal authority and implementation mandates; also benefit from clearer pastoral guidance for secular societies
 *   - Magisterial Clarity (abstract): Victim (powerless/trapped) — precision and unambiguity of binding doctrine potentially degraded by hermeneutics of continuity that permit multiple coexisting interpretations
 *   - Traditionalist Institutional Remnant: Tertiary actor (institutional/arbitrage) — maintains pre-conciliar institutional forms with degraded functional power; performs fidelity to pre-Vatican II discipline (Piton)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(interpretive_authority_concentration, 0.58).
domain_priors:suppression_score(interpretive_authority_concentration, 0.65).
domain_priors:theater_ratio(interpretive_authority_concentration, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(interpretive_authority_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(interpretive_authority_concentration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(interpretive_authority_concentration, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(interpretive_authority_concentration, tangled_rope).
narrative_ontology:human_readable(interpretive_authority_concentration, "Interpretive Authority Concentration in Post-Vatican II Catholicism").
narrative_ontology:topic_domain(interpretive_authority_concentration, "ecclesiastical_history/theological_doctrine").

domain_priors:requires_active_enforcement(interpretive_authority_concentration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(interpretive_authority_concentration, '55f852f1-8459-4118-84ee-a197721aa8f8').
narrative_ontology:cs_kernel_codification('55f852f1-8459-4118-84ee-a197721aa8f8', formalized).
narrative_ontology:cs_authority_grounding('55f852f1-8459-4118-84ee-a197721aa8f8', lineage).
narrative_ontology:cs_interpretation_layer_present('55f852f1-8459-4118-84ee-a197721aa8f8').
narrative_ontology:cs_reading_relation('55f852f1-8459-4118-84ee-a197721aa8f8', interpretive_authority_dispersal, coexists_with).
narrative_ontology:cs_reading_relation('55f852f1-8459-4118-84ee-a197721aa8f8', magisterial_infallibility_renewal, influences).
narrative_ontology:cs_axiom('55f852f1-8459-4118-84ee-a197721aa8f8', foundational, magisterial_interpretive_monopoly).
narrative_ontology:cs_axiom_status(magisterial_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('55f852f1-8459-4118-84ee-a197721aa8f8', magisterial_interpretive_monopoly, deontological).
narrative_ontology:cs_axiom('55f852f1-8459-4118-84ee-a197721aa8f8', foundational, hermeneutics_of_continuity_principle).
narrative_ontology:cs_axiom_status(hermeneutics_of_continuity_principle, holdable).
narrative_ontology:cs_axiom_grounding('55f852f1-8459-4118-84ee-a197721aa8f8', hermeneutics_of_continuity_principle, deontological).
narrative_ontology:cs_axiom('55f852f1-8459-4118-84ee-a197721aa8f8', secondary, traditional_community_subordination_justified).
narrative_ontology:cs_axiom_status(traditional_community_subordination_justified, holdable).
narrative_ontology:cs_axiom_grounding('55f852f1-8459-4118-84ee-a197721aa8f8', traditional_community_subordination_justified, instrumental).
narrative_ontology:cs_reference_frame('55f852f1-8459-4118-84ee-a197721aa8f8', apostolic_tradition_magisterial_stewardship).
narrative_ontology:cs_drift_state('55f852f1-8459-4118-84ee-a197721aa8f8', post_vatican_ii_implementation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('55f852f1-8459-4118-84ee-a197721aa8f8', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(interpretive_authority_concentration, vatican_administrative_core).
narrative_ontology:constraint_beneficiary(interpretive_authority_concentration, episcopal_modernizer_faction).
narrative_ontology:constraint_victim(interpretive_authority_concentration, traditional_practice_communities).
narrative_ontology:constraint_victim(interpretive_authority_concentration, magisterial_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL PRACTICE COMMUNITIES (SNARE) — Parishes and lay communities whose identity is constituted through pre-Vatican II liturgical and doctrinal forms. Cannot exit without abandoning their faith identity. Trapped by identity fusion, not material barriers. Suppression enforced through liturgical mandates, removal of traditional-form priests, and institutional pressure. Zero agency in the reinterpretation process.
constraint_indexing:constraint_classification(interpretive_authority_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: DIOCESAN CLERGY (TANGLED ROPE) — Priests at the diocesan level experience genuine coordination (serving transitional pastoral needs across old and new frameworks) alongside asymmetric extraction (career advancement tied to accepting new doctrinal framing, penalties for defending pre-Vatican II positions). Constrained by career dependency and decanal authority. Mixed extraction and coordination function.
constraint_indexing:constraint_classification(interpretive_authority_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EPISCOPAL MODERNIZER FACTION (ROPE) — Senior bishops and theological advisors (periti) who shaped Vatican II see the reinterpretation as solving genuine coordination problems: updating Church engagement with modernity, resolving ecumenical barriers, clarifying doctrine in contemporary language. They experience the constraint as coordination with significant arbitrage benefit (enhanced institutional authority, resolution of doctrinal tensions, greater capacity to govern). Net beneficiary position.
constraint_indexing:constraint_classification(interpretive_authority_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: VATICAN ADMINISTRATIVE CORE (TANGLED ROPE) — The Curia itself faces genuine coordination pressure (maintaining institutional coherence across rapidly changing Catholic societies) alongside extraction (centralizing interpretive authority, consolidating power over doctrinal definition, neutralizing rival theological schools). Active enforcement required to maintain interpretive monopoly. Both coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(interpretive_authority_concentration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the magisterium's interpretive authority over doctrine is presented as an immutable feature of Catholic continuity: the Pope and bishops have always held authority to interpret tradition. This perspective naturalizes the concentration of interpretive power. However, the structural data contradicts this — identifiable beneficiaries (Curia, modernizers) and victims (traditional communities) exist. False summit candidate.
constraint_indexing:constraint_classification(interpretive_authority_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: TRADITIONALIST INSTITUTIONAL REMNANT (PITON) — Traditional religious communities and societies (SSPX sympathizers, indult communities before Summorum Pontificum) maintain institutional forms and claims of doctrinal authority that persisted from pre-Vatican II structures. Theater ratio high (performing fidelity to pre-Vatican II discipline while lacking institutional power or enforcement capacity). Primary function has atrophied; maintained through inertia and identity commitment rather than effective governance.
constraint_indexing:constraint_classification(interpretive_authority_concentration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(interpretive_authority_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(interpretive_authority_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(interpretive_authority_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(interpretive_authority_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(interpretive_authority_concentration, TR),
    TR >= 0.70.

:- end_tests(interpretive_authority_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits significant extraction: the magisterium concentrates interpretive authority, traditional communities lose institutional voice and liturgical authority, competing theological schools (Thomist, Franciscan, nouvelle théologie) are subordinated to unified conciliar interpretation. However, extractiveness is not maximal because Vatican II genuinely solved coordination problems — ecumenical barriers were real, pastoral modernization was addressing real crises in secular societies, doctrinal clarity for contemporary contexts was a legitimate need. The extraction is entangled with genuine coordination. Initial extractiveness (0.32) reflects the pre-conciliar state of theological pluralism; it rises sharply post-conciliar as enforcement mechanisms activate. Suppression (0.65): Moderate-high. Significant barriers prevent resistance to the new framework: liturgical mandates remove traditional forms from parishes, seminary curricula shift, bishops enforce unified interpretation, career penalties for defending pre-conciliar positions, identity pressure on traditional communities. However, suppression is not total — traditionalist remnants persist, SSPX emerged as organized resistance, indult provision offered outlet. The rising trajectory (0.40 → 0.65) reflects enforcement intensification from conciliar rhetoric to post-conciliar implementation. Theater ratio (0.68): High. The conciliar process and subsequent implementation involved substantial performative content: the 'hermeneutics of continuity' performs doctrinal continuity while permitting radical reinterpretation; the council presents as collegial discernment while actual power flows from curial control and papal authority; post-conciliar enforcement claims fidelity to Vatican II documents while implementing changes that exceed or contradict the documents (especially in liturgy). Theater has increased over the interval as the gap between continuity claims and practical change has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. Traditional practice communities see Snare — pure extraction of their institutional voice and liturgical authority, with no exit without identity dissolution. Diocesan clergy see Tangled Rope — genuine pastoral coordination problems solved, but alongside career pressures and authority subordination. Modernizer bishops see Rope — solving real ecumenical and pastoral coordination problems with net institutional benefit. The Vatican Curia sees Tangled Rope or Rope — coordinating a global institution under changing social conditions while consolidating interpretive authority. The analytical observer risks seeing Mountain (magisterial authority is inherent to Catholic continuity) but structural evidence reveals this as a false summit. The traditionalist remnant sees Piton — their pre-conciliar institutional forms persist through inertia but lack functional power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by institutional position. Traditional communities are full victims (d → 1.0): identity-locked, trapped by cognitive fusion with pre-conciliar forms; the constraint flows extraction toward the magisterium. The Vatican Curia is partial beneficiary (d → 0.15-0.25): arbitrage exit (can interpret as they choose), institutional power, centralized authority. Modernizer bishops are beneficiaries with constrained movement (d → 0.20-0.30): they shape the interpretation but remain subordinate to papal authority; their gain is real but not unfettered. Diocesan clergy are mixed (d → 0.55): they experience both coordination (better tools for secular ministry) and extraction (career penalties, authority limits). The directionality derives from beneficiary/victim declarations and exit capacity: beneficiaries with arbitrage exit experience low or negative effective chi; victims with trapped/identity_locked exit experience high chi. The perspectival gap reflects how the same extractiveness (0.58) is experienced differently depending on d: for victims (d ≈ 0.9), effective extraction is severe; for beneficiaries (d ≈ 0.2), effective extraction is minimal or negative.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits unresolved mandatrophy because the classification depends on whether Vatican II is one contested reading of Catholic continuity (Tangled Rope: genuine coordination plus asymmetric extraction) or multiple independent doctrinal movements artificially unified (Snare: extraction dressed as coordination). The kernel omega variables address this directly. If Vatican II is a single coherent reinterpretation solving genuine coordination problems, Tangled Rope is correct — the constraint has both a real coordination function (ecumenism, pastoral modernization, doctrinal clarity) and asymmetric extraction (concentration of interpretive authority). If Vatican II is a composite of independent reforms unified by institutional convenience, Snare is more accurate — the coordination function is secondary; extraction is primary. The hermeneutics of continuity itself (performing doctrinal continuity while permitting radical reinterpretation) is the cover story either way. The rising theater ratio and suppression measurements support the Snare reading: as enforcement intensifies post-conciliar, the coordination function becomes less visible and the extraction mechanism more apparent. Resolving the kernel ambiguity requires historical/theological analysis beyond the constraint framework's scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vatican_ii_kernel_unity,
    'Is Vatican II a single doctrinal reinterpretation (one contested reading of Catholic tradition) or a composite of multiple independent doctrinal shifts that happened to be institutionalized simultaneously?',
    'Textual analysis of conciliar documents for coherence of interpretive principles; historiography of pre-conciliar preparation vs post-conciliar implementation; examination of whether specific reforms (liturgy, ecumenism, religious freedom, church-world relationship) require a shared philosophical framework or could be coherent independently.',
    'If single reinterpretation: the constraint represents genuine doctrinal coordination under interpretive authority concentration — Tangled Rope is accurate. If composite: Vatican II is a false unity imposed by institutional power, and the constraint is better classified as Snare (multiple independent shifts forced into artificial coherence). This resolves the mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vatican_ii_kernel_unity, conceptual, 'Whether Vatican II is one reading or multiple bundled reforms').

omega_variable(
    traditional_community_structural_exit,
    'Do traditional practice communities (FSSP, indult, SSPX-adjacent) have genuine structural mobility (constrained exit) or are they identity-locked and therefore immobilized despite formal exit options?',
    'Post-exit trajectory analysis: do communities that accept the new framework maintain their identity and community coherence, or does embrace of modernized doctrine dissolve the community''s self-conception? If identity persists after doctrinal shift, exit was materially constrained; if identity collapses, exit was identity-locked.',
    'If identity-locked: powerless/identity_locked perspective is correct; classification as Snare is structural. If constrained (high-cost exit): same perspective produces Snare, but mechanism is material suppression rather than cognitive capture. Affects interpretation of whether the constraint is extractive or merely harsh coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(traditional_community_structural_exit, empirical, 'Whether traditional community exit is identity-locked or materially constrained').

omega_variable(
    interpretive_authority_natural_vs_constructed,
    'Is the concentration of interpretive authority in the magisterium a natural feature of Catholic institutional structure (unchangeable law of hierarchical organization) or a contingent institutional arrangement that Vatican II crystallized and enforced?',
    'Historical analysis of doctrinal authority pre-Vatican II (diversity of theological schools, local interpretation practice, magisterial reserve); comparison with post-Vatican II enforcement of unified interpretation; examination of whether centralization was required by Vatican II documents or imposed through implementation.',
    'If natural law: mountain classification from analytical perspective is accurate. If constructed: mountain is a false summit; the constraint should classify as Tangled Rope or Snare from all perspectives, revealing the constraint as an institutional extraction mechanism dressed in theological continuity language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_natural_vs_constructed, conceptual, 'Whether interpretive authority concentration is natural law or constructed').

omega_variable(
    modernizer_faction_genuine_coordination,
    'Do the modernizer bishops'' (periti-advised) reinterpretations solve genuine coordination problems (ecumenism, church-world engagement, doctrinal clarity) or rationalize pre-existing institutional preferences to concentrate power?',
    'Comparison of pre-Vatican II coordination failures (ecumenical impasses, pastoral crises in secular societies, doctrinal ambiguities) with post-Vatican II solutions attributed to new interpretations; examination of whether non-centralizing alternative interpretations could have addressed the same problems.',
    'If genuine coordination problems: Rope/Tangled Rope from modernizer perspective is accurate; the constraint solves real issues alongside extracting benefit. If rationalization: the coordination function is cover story; extraction is primary; modernizer perspective should classify as Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernizer_faction_genuine_coordination, empirical, 'Whether modernizer reinterpretations address genuine coordination problems').

omega_variable(
    magisterial_clarity_victim_status,
    'Is ''magisterial clarity'' (precision and unambiguity of Catholic doctrinal teaching) a genuine victim of Vatican II''s hermeneutics of continuity, or is this a victim claim made by traditionalists without structural basis?',
    'Comparison of doctrinal precision pre- and post-Vatican II (document length, specificity of binding claims, reduction in theological ambiguity vs increase); examination of whether interpretive multiplicity post-Vatican II (progressive, conservative, radical readings coexisting within official Church) represents degradation of clarity or deliberate expansion of interpretive space.',
    'If clarity degraded: victims are real; constraint includes erosion of institutional coherence alongside extraction. If clarity preserved or intentionally expanded: ''magisterial clarity'' is not a genuine victim; constraint is pure beneficiary extraction (Curia/modernizers) with side effects, not genuine extraction from a victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_clarity_victim_status, empirical, 'Whether Vatican II degraded or intentionally expanded magisterial clarity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(interpretive_authority_concentration, 1962, 1975).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(interp_auth_tr_t0, interpretive_authority_concentration, theater_ratio, 0, 0.45).
narrative_ontology:measurement(interp_auth_tr_t2, interpretive_authority_concentration, theater_ratio, 2, 0.62).
narrative_ontology:measurement(interp_auth_tr_t5, interpretive_authority_concentration, theater_ratio, 5, 0.68).

% Extraction over time
narrative_ontology:measurement(interp_auth_be_t0, interpretive_authority_concentration, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(interp_auth_be_t2, interpretive_authority_concentration, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(interp_auth_be_t5, interpretive_authority_concentration, base_extractiveness, 5, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(interp_auth_su_t0, interpretive_authority_concentration, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(interp_auth_su_t2, interpretive_authority_concentration, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(interp_auth_su_t5, interpretive_authority_concentration, suppression_requirement, 5, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(interpretive_authority_concentration, identity_coordination).
narrative_ontology:affects_constraint(interpretive_authority_concentration, traditional_liturgy_suppression).
narrative_ontology:affects_constraint(interpretive_authority_concentration, theological_school_subordination).
narrative_ontology:affects_constraint(interpretive_authority_concentration, episcopal_authority_centralization).

% DUAL FORMULATION NOTE:
% Interpretive authority concentration is upstream of specific post-Vatican II enforcement mechanisms. Traditionalist liturgical suppression (lex orandi degradation), subordination of theological schools (Thomist, Franciscan displacement by conciliar neo-scholasticism), and episcopal authority centralization (bishops as executors of magisterial interpretation rather than independent teachers) are downstream constraints with their own extractiveness values. All three are structurally dependent on the interpretive authority concentration constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(interpretive_authority_concentration, institutional, 0.18).
constraint_indexing:directionality_override(interpretive_authority_concentration, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
