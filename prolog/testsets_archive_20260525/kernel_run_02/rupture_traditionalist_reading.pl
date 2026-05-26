% ============================================================================
% CONSTRAINT STORY: rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rupture_traditionalist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rupture_traditionalist_reading
 *   human_readable: Vatican II as Doctrinal Rupture (Traditionalist Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962-1965) represents a fundamental institutional rupture in
 *   Catholic ecclesiology according to this reading. The Council's documents
 *   contain systematic ambiguities that enable post-conciliar implementation
 *   far beyond what the Council fathers explicitly authorized. This
 *   constraint models Vatican II as an instrument of doctrinal and liturgical
 *   extraction from traditionalist Catholics: the Council creates the
 *   authoritative framework within which progressive reformers can implement
 *   heterodox changes while claiming fidelity to conciliar authority. The
 *   ambiguities in conciliar texts are not accidents of drafting but
 *   functional features of compromise — they permit supporters to claim the
 *   Council changed nothing essential (continuity narrative) while enabling
 *   implementers to effect radical transformation (rupture progressive
 *   narrative). From the traditionalist perspective, Vatican II is neither a
 *   natural law (immutable hermeneutical feature) nor pure coordination
 *   (legitimate development), but a Tangled Rope: genuine institutional
 *   coordination function overlaid with asymmetric extraction of
 *   traditionalist authority, liturgy, and doctrinal clarity. The
 *   constraint's extractiveness has risen from 0.22 (pre-conciliar baseline)
 *   to 0.58 (post-conciliar implementation phase) as the ambiguities have
 *   been operationalized into concrete institutional changes. Theater ratio
 *   indicates that post-conciliar discourse increasingly performs doctrinal
 *   legitimacy through appeals to 'the spirit of Vatican II' — claims that
 *   exceed what the texts explicitly authorize.
 *
 * KEY AGENTS:
 *   - Progressive Reform Coalition: Primary beneficiary (institutional/arbitrage) — uses Vatican II as authorization for doctrinal reinterpretation, liturgical reform, and institutional modernization; controls post-conciliar hermeneutics and papal implementation
 *   - Traditionalist Faithful: Primary victim (powerless/trapped) — experience doctrinal displacement and liturgical rupture with no authority structure available to validate their concerns; trapped in institutional obedience to reforms they reject as heterodox
 *   - Traditionalist Clergy and Religious: Secondary victim (organized/identity_locked) — experience binding mechanism fusing identity with pre-conciliar practice and doctrine; structurally mobile but cannot exercise exit option because identity would dissolve
 *   - Conservative Episcopacy and Theological Schools: Mixed (organized/constrained) — experience coordination function (conciliar documents enable debate) but also extraction (their interpretive authority is suppressed; they cannot teach pre-conciliar doctrine without qualification)
 *   - Post-Conciliar Magisterium: Institutional actor (institutional/arbitrage) — beneficiary of interpretive flexibility; can cite Council to authorize contradictory practices; maintains authority through performative appeals to 'conciliar intent' without textual precision
 *   - Conciliar Authority Structure: Institutional (institutional/arbitrage piton perspective) — maintains degraded authority through institutional inertia; conciliar documents persist as authority reference despite loss of hermeneutical clarity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rupture_traditionalist_reading, 0.58).
domain_priors:suppression_score(rupture_traditionalist_reading, 0.65).
domain_priors:theater_ratio(rupture_traditionalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rupture_traditionalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(rupture_traditionalist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rupture_traditionalist_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(rupture_traditionalist_reading, "Vatican II as Doctrinal Rupture (Traditionalist Reading)").
narrative_ontology:topic_domain(rupture_traditionalist_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(rupture_traditionalist_reading, formalized).
narrative_ontology:cs_authority_grounding(rupture_traditionalist_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(rupture_traditionalist_reading).
narrative_ontology:cs_kernel_id(rupture_traditionalist_reading, vatican_ii_doctrinal_authority).
narrative_ontology:cs_reading_relation(rupture_traditionalist_reading, continuity_reading, forecloses).
narrative_ontology:cs_reading_relation(rupture_traditionalist_reading, rupture_progressive_reading, coexists_with).
narrative_ontology:cs_axiom(rupture_traditionalist_reading, foundational, vatican_ii_contains_substantive_rupture).
narrative_ontology:cs_axiom_status(vatican_ii_contains_substantive_rupture, holdable).
narrative_ontology:cs_axiom_grounding(rupture_traditionalist_reading, vatican_ii_contains_substantive_rupture, empirically_contingent).
narrative_ontology:cs_axiom(rupture_traditionalist_reading, foundational, rupture_violates_conciliar_intent_and_doctrine).
narrative_ontology:cs_axiom_status(rupture_violates_conciliar_intent_and_doctrine, holdable).
narrative_ontology:cs_axiom_grounding(rupture_traditionalist_reading, rupture_violates_conciliar_intent_and_doctrine, deontological).
narrative_ontology:cs_reference_frame(rupture_traditionalist_reading, pre_conciliar_doctrinal_continuity).
narrative_ontology:cs_drift_state(rupture_traditionalist_reading, post_conciliar_implementation_phase, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rupture_traditionalist_reading, progressive_reform_faction).
narrative_ontology:constraint_victim(rupture_traditionalist_reading, traditional_liturgical_practice).
narrative_ontology:constraint_victim(rupture_traditionalist_reading, doctrinal_clarity).
narrative_ontology:constraint_victim(rupture_traditionalist_reading, missionary_orthodoxy).
narrative_ontology:constraint_victim(rupture_traditionalist_reading, episcopal_authority_constraint).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FAITHFUL SEEKING DOCTRINAL CLARITY (SNARE) — Cannot exit the Church or resist implementation of post-conciliar reforms. Bears full cost of doctrinal ambiguity and liturgical displacement with no mechanism for correction. Trapped in the institutional framework with no alternative authority structure available. Maximum experienced extraction — the faithful observe contradictions between pre-conciliar teaching and post-conciliar practice with no official resolution mechanism.
constraint_indexing:constraint_classification(rupture_traditionalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRADITIONALIST PRIESTS AND RELIGIOUS (IDENTITY_LOCKED SNARE) — Structurally mobile (could leave priesthood or leave the Church) but identity-fused with pre-conciliar practice and pre-conciliar doctrinal framework. Exit would require abandoning not just a role but a constituted identity (priestly vocation, religious habit, Tridentine liturgy as core to spiritual identity). Experience maximum extraction: their vows bind them to obey bishops implementing reforms they understand as doctrinal betrayal. The identity lock creates a binding mechanism stronger than material constraint.
constraint_indexing:constraint_classification(rupture_traditionalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: CONSERVATIVE EPISCOPACY AND DOCTRINAL SCHOOLS (TANGLED_ROPE) — Organized agents (traditional theology faculties, conservative bishops, Thomist networks) experience genuine coordination function: Vatican II documents enable communication of doctrinal continuity claims, create forums for theological debate, and maintain institutional cohesion despite disagreement. BUT also experience significant extraction: their interpretive frameworks are suppressed in favor of progressive hermeneutics; their authority to teach traditional doctrine is constrained by post-conciliar documents; their resistance to reform is framed as obstruction rather than legitimate doctrinal concern. High suppression (bishops cannot publicly reject conciliar authority without schism risk). Mixed experience: some coordination benefit but asymmetric extraction from the reform coalition.
constraint_indexing:constraint_classification(rupture_traditionalist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE REFORM COALITION (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: Vatican II documents enable liturgical reform, doctrinal reinterpretation, and institutional modernization. The ambiguities in conciliar texts are features, not bugs — they create interpretive space for heterodox implementation. Can arbitrage between conciliar text and heterodox application: cite Vatican II as authorization while implementing practices the Council fathers would not have approved. Zero or negative experienced extraction — the constraint subsidizes this actor's agenda.
constraint_indexing:constraint_classification(rupture_traditionalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONCILIAR AUTHORITY STRUCTURE — INSTITUTIONAL DEGRADATION (PITON) — From the viewpoint of institutional authority maintenance, Vatican II's documents are degraded instruments: they contain ambiguities that generate interpretive conflict rather than resolve it; they lack the precision of pre-conciliar doctrinal formulations; their authority relies on performative appeals to 'the spirit of the Council' rather than on clear doctrinal propositions. The authority structure persists through institutional inertia (Vatican II was solemnly promulgated; rejecting it risks schism) rather than through functional clarity. Theater ratio indicates that much post-conciliar discourse performs adherence to conciliar authority while implementing practices the conciliar texts do not explicitly authorize.
constraint_indexing:constraint_classification(rupture_traditionalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — HERMENEUTICAL IMMUTABILITY (MOUNTAIN) — From a civilizational/universal perspective, Vatican II's ambiguities are inherent to any large institutional council attempting to change practice while maintaining doctrinal continuity. Ambiguity is an immutable feature of language and institutional compromise — any text attempting to bridge pre-modern and modern frameworks will contain irreducible tensions. The post-conciliar implementation conflicts are therefore natural consequences of the hermeneutical situation, not evidence of rupture or conspiracy. However, this perspective risks naturalizing what is actually a contingent choice: the conciliar texts could have been written more precisely; the implementation conflicts are not inevitable features of institutional language but predictable consequences of specific drafting decisions and power asymmetries. Engine will flag this as false summit.
constraint_indexing:constraint_classification(rupture_traditionalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rupture_traditionalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rupture_traditionalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rupture_traditionalist_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rupture_traditionalist_reading, TR),
    TR >= 0.70.

:- end_tests(rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Council creates the institutional framework within which significant doctrinal and liturgical extraction occurs. From the traditionalist reading, the extraction is not maximal (0.72+) because Vatican II documents do contain explicit continuity language that constrains implementation — the progressive coalition must argue that reforms develop pre-conciliar doctrine rather than overturn it. But extraction is substantial because the ambiguities enable practices (vernacular Mass, female altar servers, liberalized marriage annulments) that pre-conciliar doctrine explicitly prohibited. The trajectory (0.22 → 0.58) reflects accumulation: Vatican II opens interpretive space; post-conciliar actors operationalize that space into concrete institutional changes; each new practice normalizes further reinterpretation. Suppression (0.65): Moderate-high. Traditionalist Catholics cannot formally reject conciliar authority without schism; bishops cannot teach pre-conciliar doctrine without papal correction; priests cannot celebrate traditional liturgy without episcopal permission (until Summorum Pontificum 2007); religious communities cannot maintain traditional practices without adaptation mandates. Suppression mechanisms include doctrinal correction (Mysterium Fidei pre-empting traditional Eucharistic teaching), disciplinary enforcement (suppression of Tridentine Mass), and institutional pressure (seminaries closed, traditional religious orders suppressed or reformed). Theater ratio (0.48): Moderate. Post-conciliar discourse engages in substantial performative work: appeals to 'the spirit of Vatican II' exceed textual warrant; claims of 'development' obscure rupture; emphasis on 'dialogue' and 'inculturation' mask asymmetric power (progressive coalition controls implementation). Pre-conciliar Church had lower theater (doctrine was explicit, liturgy was uniform); conciliar period shows rising theater as compromise language created ambiguities requiring interpretive management; post-conciliar implementation shows theater remaining elevated as post-conciliar actors manage the cognitive dissonance between conciliar text and actual practice.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The progressive coalition (Rope) experiences Vatican II as pure coordination — the Council solves the problem of modernizing Church practice while maintaining institutional continuity. The conservative episcopacy (Tangled Rope) experiences mixed coordination and extraction — they benefit from forums to articulate continuity but their interpretive authority is suppressed. Traditionalist religious communities (Snare with identity_locked exit) experience maximum extraction — they are bound by vows to obey bishops implementing reforms they understand as betrayal of their constituted identity (priesthood, religious vocation, sacramental orthodoxy fused with pre-conciliar practice). The post-conciliar magisterium (Rope) experiences arbitrage — they control hermeneutics and can authorize contradictory practices by appealing to conciliar ambiguity. The conciliar authority structure (Piton) experiences institutional degradation — the documents lose hermeneutical clarity and require performative legitimacy work. The analytical observer (Mountain) risks naturalizing hermeneutical ambiguity as an immutable feature of institutional language, missing the contingent choices (drafting precision, implementation restraint, hierarchical constraint) that would have prevented extraction. The perspectival gap is maximal because the same texts authorize all readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d reflects each agent's structural position relative to the extraction flow. Progressive coalition: beneficiary with arbitrage options → d ≈ 0.05 → negative f(d) → negative experienced extraction (the constraint subsidizes them). Traditionalist faithful: victim with no exit → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction. Conservative bishops: mixed beneficiary/victim with constrained exit → d ≈ 0.55 → f(d) ≈ 0.75 → moderate positive extraction. Traditionalist clergy: victim with identity-locked exit (structurally mobile but psychologically trapped) → d ≈ 0.89 → f(d) ≈ 1.28 → high experienced extraction (identity lock prevents exercising available structural exit). The d values are derived from beneficiary/victim declarations and exit options; the asymmetry in d explains why perspectives classify identically on the base metric properties (same ε, suppression, theater) but differently on experienced extractiveness (different χ values due to different d → f(d) scaling).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Vatican II functions simultaneously as all six types depending on structural position. The mandate trap here is: 'Is Vatican II coordination or extraction?' The traditionalist reading answers: 'Both simultaneously — genuine coordination function overlaid with asymmetric extraction.' The tangled rope classification captures this: beneficiaries (progressive coalition) coordinate institutional modernization while victims (traditionalist faithful) bear extraction of doctrinal clarity and liturgical tradition. The constraint requires active enforcement (suppression of traditional liturgy, doctrinal correction of continuity arguments, exile of schismatic bishops) which distinguishes it from pure rope. The asymmetric extraction (beneficiaries gain interpretive authority; victims lose doctrinal input) distinguishes it from symmetric coordination. The high suppression (0.65) reflects that traditionalists cannot exit (schism barrier) and cannot organize resistance within institutional structure (papal authority trump card). Mandatrophy is resolved not by choosing one type but by recognizing that the constraint's functionality depends entirely on perspectival position: from beneficiary view, it is rope; from victim view, it is snare; from organized victim view, it is tangled rope; from institutional maintenance view, it is degraded piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_continuity_threshold,
    'What degree of doctrinal reinterpretation constitutes rupture versus development? Where lies the boundary between organic development and discontinuity?',
    'Comparison of pre- and post-conciliar magisterial statements on specific doctrines (papal infallibility, Eucharistic theology, episcopal collegiality, religious liberty); analysis of whether post-conciliar development could be derived from pre-conciliar principles or requires new premises; historical analysis of conciliar bishops'' documented intentions versus actual post-conciliar implementation.',
    'If development standard applies: constraint reclassifies toward Rope from all perspectives; conciliar documents become coordination mechanism rather than extraction vehicle. If rupture standard applies: constraint confirms Tangled Rope / Snare classification; ambiguities become intentional concealment. This is the core epistemic pivot for the entire kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_continuity_threshold, conceptual, 'Boundary between doctrinal development and discontinuity').

omega_variable(
    authorial_intent_versus_text,
    'When conciliar texts are ambiguous, should interpretation follow the documented intent of Council fathers or the plain text of promulgated documents?',
    'Historical reconstruction of conciliar debates (acta concilii, father testimonies, council periti notes); comparison with promulgated text language; analysis of how progressive coalition explained conciliar documents to bishops during council vs. how they interpreted them post-council.',
    'If authorial intent governs: many post-conciliar practices violate conciliar intent, supporting rupture reading and snare classification. If text governs: ambiguities enable legitimate reinterpretation, supporting progressive reading and rope classification. This determines whether extraction was intentional (conciliar fathers did not authorize what happened) or consensual (text permits multiple readings and progressive one is legitimate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorial_intent_versus_text, empirical, 'Authority of authorial intent versus promulgated text').

omega_variable(
    implementation_agency_attribution,
    'Are post-conciliar ambiguities in liturgy, doctrine, and discipline the result of conciliar texts themselves or of post-conciliar papal and episcopal decisions that exceeded the texts?',
    'Textual analysis: Vatican II documents on specific practices (liturgy, mixed marriages, seminary formation, episcopal collegiality) versus Paul VI and successor implementations; distinction between what Council fathers approved and what post-conciliar commissions enacted; historical record of papal-episcopal negotiations 1965-1978.',
    'If conciliar texts caused ambiguities: rupture reading holds; Vatican II itself is the extraction mechanism. If post-conciliar actors exploited ambiguities: constraint may reclassify based on agency location; Vatican II becomes merely an instrument, not the primary constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_agency_attribution, empirical, 'Source of post-conciliar implementation divergence').

omega_variable(
    reading_kernel_ambiguity,
    'Is Vatican II best understood as a kernel (stabilized commitment with ambiguous content) or as a determinate decision that post-conciliar actors misinterpreted?',
    'If kernel: the constraint''s ε and suppression reflect genuine structural ambiguity in the Council''s own commitments; all three readings (continuity, rupture progressive, rupture traditionalist) are defensible interpretations of the same documents. If determinate decision: one reading is objectively correct and others are misreadings; the ε should reflect not ambiguity but intentional deception.',
    'This is the meta-uncertainty about the constraint itself. The question routes through omega precisely because the committer frame (Rules 1-5) does not itself resolve whether Vatican II is a kernel or a determinate claim read differently by factions. If kernel framing is correct, all three readings stand as permanent structural alternatives. If determinate framing is correct, one reading is the true structure and the others are ideological glosses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether Vatican II is kernel with irreducible ambiguity or determinate decision with disputed interpretation').

omega_variable(
    suppression_mechanism_internalized,
    'Is the experienced suppression (inability of traditionalist voices to resist reform) structural (institutional hierarchy, papal authority, threat of excommunication) or internalized (acceptance of conciliar authority as legitimate even by traditionalists)?',
    'Post-schism trajectory analysis: SSPX separatists experience lower suppression (they have exited the coercive structure) but also lose institutional resources; observation of whether traditionalist priests/religious who remain accept conciliar authority as legitimate or maintain internal resistance; comparison of suppression levels pre- and post-excommunication of schismatic bishops.',
    'If structural: suppression baseline remains 0.65+ regardless of traditionalist acceptance. If internalized: suppression may decline as generation accepting conciliar authority matures; alternatively, internalized suppression persists across generations through reframing of reform as development. Affects trajectory projection for constraint evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Suppression mechanism: structural coercion versus internalized legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rupture_traditionalist_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vat2_theater_preconcil, rupture_traditionalist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vat2_theater_concil, rupture_traditionalist_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(vat2_theater_postcon_early, rupture_traditionalist_reading, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(vat2_extract_preconcil, rupture_traditionalist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(vat2_extract_concil, rupture_traditionalist_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(vat2_extract_postcon_early, rupture_traditionalist_reading, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rupture_traditionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rupture_traditionalist_reading, continuity_reading).
narrative_ontology:affects_constraint(rupture_traditionalist_reading, rupture_progressive_reading).
narrative_ontology:affects_constraint(rupture_traditionalist_reading, post_conciliar_magisterial_authority).
narrative_ontology:affects_constraint(rupture_traditionalist_reading, traditionalist_liturgical_suppression).
narrative_ontology:affects_constraint(rupture_traditionalist_reading, ecclesial_authority_fragmentation).

% DUAL FORMULATION NOTE:
% Vatican II doctrinal authority is a contested kernel. Three structurally distinct constraints emerge from different readings: (1) continuity_reading (ε ≈ 0.15, Mountain or Rope — organic development, no rupture); (2) rupture_progressive_reading (ε ≈ 0.58, Tangled Rope — legitimate rupture justified by pastoral need, modernization as development); (3) rupture_traditionalist_reading (THIS CONSTRAINT, ε = 0.58, Tangled Rope — illegitimate rupture hidden in ambiguous texts, extraction from traditionalist position). The three readings share high structural similarity on base metrics but differ fundamentally in axioms, authority grounding, and victim sets. Each is a complete constraint story with its own perspectives, omegas, and measurements. They are linked via network.affects_constraints to show kernel decomposition: sibling readings activate different extraction mechanisms (continuity reading has no victims; progressive reading's victims are traditionalist; traditionalist reading's victims are progressively extracted traditionalists). The constraint family demonstrates ε-invariance principle: different observables (doctrinal fidelity vs. pastoral effectiveness) produce different ε values; rather than embed measurement parameter into one constraint, decompose into three stories and link them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rupture_traditionalist_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
