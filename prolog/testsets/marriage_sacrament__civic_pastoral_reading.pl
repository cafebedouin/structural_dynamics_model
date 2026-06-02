% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Relationship with Compassionate Discernment
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   Marriage as a sacrament in the Catholic tradition instantiates a
 *   contested kernel: the claim that Christian marriage is indissoluble. This
 *   story generates ONE reading of that kernel — the civic-pastoral reading —
 *   which emerged from Vatican II (1962–1965) and matured through Pope
 *   Francis's papacy. The civic-pastoral reading holds that while
 *   indissolubility remains the doctrinal ideal, pastoral compassion requires
 *   discernment in individual cases: a marriage that is truly dead in
 *   conscience may be acknowledged through annulment processes, pastoral
 *   accompaniment, or de facto reception of sacraments by divorced-remarried
 *   Catholics. This reading relativizes the doctrine by embedding it within a
 *   framework of pastoral judgment and human limitation. It is distinct from
 *   the hierarchical indissolubility reading, which holds that the doctrine
 *   is absolute and that pastoral response must work within doctrinal
 *   constraints (annulment remains the only legitimate path;
 *   divorced-remarried Catholics remain barred from sacraments absent
 *   annulment). The civic-pastoral reading creates a structural tension: it
 *   maintains doctrinal authority while undermining doctrinal clarity. It
 *   grants flexibility to pastoral actors while creating inconsistency across
 *   dioceses. It promises compassion to divorced-remarried Catholics while
 *   maintaining institutional gatekeeping. These tensions make it a tangled
 *   rope at the analytical level — genuine coordination (responding to
 *   pastoral need) combined with asymmetric extraction (managing
 *   institutional contradiction, destabilizing traditional identity,
 *   gatekeeping through discretion).
 *
 * KEY AGENTS:
 *   - Traditional Catholics: Identity-locked agents (globally distributed) whose self-concept is constituted through doctrinal stability; experience the reading as extraction through identity destabilization
 *   - Divorced-Remarried Catholics: Structurally constrained agents (globally distributed, moderate power) who face variable access to sacraments based on diocesan interpretation and annulment tribunal discretion
 *   - Diocesan Pastoral Authority: Institutional actors (constrained within hierarchy, bishops and tribunal officials) who experience genuine coordination benefits (interpretive flexibility) alongside extraction costs (responsibility for managing doctrinal contradiction)
 *   - Vatican Hierarchy: Institutional beneficiary (high power, arbitrage exit) that maintains doctrinal authority while gaining practical flexibility through pastoral application variance
 *   - Lay Conscience Coalition: Organized agents (national/global scale, moderate coordinated power) including DignityUSA, Future Church, and reform movements that use the reading as leverage toward formal doctrinal evolution
 *   - Liturgical Gatekeeping Apparatus: Institutional machinery (sacramental eligibility rules, pre-Cana preparation, marriage preparation, tribunal procedures) that performs doctrinal coherence verification while managing contradiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.48).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.62).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Relationship with Compassionate Discernment").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '23a5d6f3-e64f-43b1-9e25-a61d3802ff39').
narrative_ontology:cs_kernel_codification('23a5d6f3-e64f-43b1-9e25-a61d3802ff39', fixed_text).
narrative_ontology:cs_authority_grounding('23a5d6f3-e64f-43b1-9e25-a61d3802ff39', lineage).
narrative_ontology:cs_interpretation_layer_present('23a5d6f3-e64f-43b1-9e25-a61d3802ff39').
narrative_ontology:cs_reading_relation('23a5d6f3-e64f-43b1-9e25-a61d3802ff39', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('23a5d6f3-e64f-43b1-9e25-a61d3802ff39', foundational, indissolubility_subject_to_pastoral_discretion).
narrative_ontology:cs_axiom_status(indissolubility_subject_to_pastoral_discretion, holdable).
narrative_ontology:cs_axiom_grounding('23a5d6f3-e64f-43b1-9e25-a61d3802ff39', indissolubility_subject_to_pastoral_discretion, deontological).
narrative_ontology:cs_axiom('23a5d6f3-e64f-43b1-9e25-a61d3802ff39', foundational, pastoral_accompaniment_requires_access).
narrative_ontology:cs_axiom_status(pastoral_accompaniment_requires_access, holdable).
narrative_ontology:cs_axiom_grounding('23a5d6f3-e64f-43b1-9e25-a61d3802ff39', pastoral_accompaniment_requires_access, deontological).
narrative_ontology:cs_reference_frame('23a5d6f3-e64f-43b1-9e25-a61d3802ff39', pastoral_flexibility_framework).
narrative_ontology:cs_drift_state('23a5d6f3-e64f-43b1-9e25-a61d3802ff39', contemporary_post_francis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23a5d6f3-e64f-43b1-9e25-a61d3802ff39', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_authority_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, ecclesial_flexibility_advocates).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, doctrinal_stability_dependents).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, conscience_bound_adherents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOCTRINAL IDENTITY-LOCKED AGENT (SNARE) — A traditional Catholic whose identity is constituted through the indissolubility doctrine experiences the civic-pastoral reading as structural extraction. The reading relativizes the doctrine they internalized as unchangeable, making exit from the identity-frame unthinkable from within: to accept pastoral discernment is to abandon the absolute claim that constitutes their self-concept as 'truly faithful.' The suppression is partly structural (institutional gatekeeping of pastoral access) and partly internalized (identity fusion prevents recognizing the doctrine's contingency). Maximum experienced extraction.
constraint_indexing:constraint_classification(marriage_sacrament__civic_pastoral_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: DIVORCED-REMARRIED CATHOLIC (SNARE) — Structurally constrained by canon law and sacramental access bars, yet also bears the cost of doctrinal inconsistency. The civic-pastoral reading promises compassionate discernment but delivers inconsistent application — some dioceses grant annulments readily, others rarely. Suppression is high (material barriers to sacramental participation, social stigma, institutional gatekeeping). The constraint extracts from this agent: they remain excluded or face variable access depending on diocesan interpretation. No coordinating benefit flows to them; the arrangement exists to manage the institutional problem of doctrinal contradiction.
constraint_indexing:constraint_classification(marriage_sacrament__civic_pastoral_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DIOCESAN PASTORAL AUTHORITY (TANGLED ROPE) — Local bishops and marriage tribunal officials experience the constraint as both coordination and extraction. The civic-pastoral reading grants them interpretive flexibility (genuine coordination benefit — they can respond to conscience and pastoral need). But it also extracts from them: they become responsible for managing the contradiction between the doctrine's absolute claim and compassionate application. Their discretion is genuine but bounded; they bear the burden of case-by-case discernment rather than applying a clear rule. Suppression moderately high — they face pushback from traditionalists when they exercise flexibility. Beneficiary status: they retain authority and interpretive privilege, but constrained.
constraint_indexing:constraint_classification(marriage_sacrament__civic_pastoral_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ECCLESIASTICAL HIERARCHY (ROPE) — From the hierarchy's institutional perspective at generational timescale, the civic-pastoral reading is a coordination mechanism. It solves the institutional problem of doctrinal contradiction without formal reversal: indissolubility remains doctrine, but pastoral discernment allows flexibility in application. The hierarchy benefits through maintained doctrinal authority while gaining the arbitrage advantage of applied inconsistency (they can respond to political and demographic pressure without changing formal law). Low experienced extraction — they have full exit optionality (can change doctrine, maintain it, or apply it selectively) and all beneficiary characteristics.
constraint_indexing:constraint_classification(marriage_sacrament__civic_pastoral_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LAY CONSCIENCE COALITION (SCAFFOLD) — Progressive lay organizations (DignityUSA, Future Church, National Coalition for Church Reforms) see the civic-pastoral reading as a transitional scaffold toward doctrinal reformation. The constraint exists because hierarchy retains gatekeeping power, but the coalition applies pressure that creates exit paths: pastoral accompaniment, conscience-based reception of sacraments, and growing acceptance of remarriage de facto if not de jure. This is a sunset structure if the coalition can eventually move the hierarchy toward formal doctrinal evolution. Theater low — the coalition operates outside formal structures, so their influence is direct action rather than ritual performance.
constraint_indexing:constraint_classification(marriage_sacrament__civic_pastoral_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LITURGICAL CONSISTENCY APPARATUS (PITON) — The formal sacramental gatekeeping mechanisms (pre-Cana counseling, marriage preparation, annulment tribunal procedures, remarriage eligibility) persist largely through institutional inertia. The apparatus pretends to verify doctrinal coherence (theater = 0.68) but actually manages inconsistency. The primary function (doctrinal coherence) has atrophied; the apparatus persists because dismantling it would require explicit acknowledgment of doctrinal revision. Maintenance is performative — the machinery continues moving without functional output.
constraint_indexing:constraint_classification(marriage_sacrament__civic_pastoral_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW VIEW (MOUNTAIN) — A civilizational analytical perspective rooted in pre-Vatican II thomistic doctrine holds that marital indissolubility reflects an unchangeable natural law grounded in human ontology and divine order. From this view, the pastoral reading is a tragic compromise with cultural decline rather than a legitimate evolution. The mountain is false (the engine will detect this via beneficiary presence and commission structural decomposition), because it naturalizes what is actually an institutional choice backed by hierarchical authority.
constraint_indexing:constraint_classification(marriage_sacrament__civic_pastoral_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_sacrament__civic_pastoral_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_sacrament__civic_pastoral_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The reading centralizes discretion in pastoral authority (bishops, tribunal officials) rather than dispersing it through clear rules or deferring to conscience. This discretion is presented as coordinate (responding to pastorally need) but functions as extraction (managing the institution's contradiction without acknowledging it). The extractiveness is not as high as a pure snare (hierarchy retains formal authority and can revert to strict doctrine) but higher than pure rope (flexibility is bounded and inconsistently applied). The value reflects that genuine coordination exists — the pastoral reading does provide care to some divorced-remarried Catholics — alongside structural extraction (the same arrangement excludes others, depends on bishop ideology, and destabilizes doctrinal dependent identity). Suppression (0.62): Moderately high. The suppression combines structural barriers (canon law, tribunal gatekeeping, sacramental eligibility rules) with internalized mechanisms (doctrinal dependents' identity-lock prevents them from accepting flexibility; divorced-remarried Catholics internalize shame and exclusion even in transparent dioceses). The value reflects that suppression is robust against mere procedural change — raising transparency alone does not eliminate it. Theater ratio (0.68): Moderately high. The apparatus of marriage tribunal review, pre-Cana counseling, and sacramental eligibility verification is substantially performative: it maintains the appearance of rigorous discernment while the actual decision-making (approval/denial) follows discretion rather than clear criteria. The theater has increased over the measurement interval as flexibility has expanded while formal doctrine remains constant — the apparatus must work harder to maintain the appearance of doctrinal consistency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The traditional Catholic sees a snare: doctrinal relativization that extracts their identity-constituting certainty. The divorced-remarried Catholic sees another snare: variable and gatekept access that manages institutional contradiction rather than resolving their exclusion. The diocesan authority sees tangled rope: genuine coordination benefit (responding to conscience, maintaining pastoral relationship) combined with extraction cost (responsibility for managing inconsistency without clear guidance). The Vatican hierarchy sees rope: coordination mechanism that maintains authority while providing practical flexibility. The lay coalition sees scaffold: a temporary structure that can be leveraged toward doctrinal revision. The institutional apparatus sees itself as performing coherence (but functions as piton — the machinery persists through inertia). The natural law perspective sees mountain — unchangeable essence — but the engine's false summit detector will identify it as beneficiary naturalization. The core perspectival gap is between those whose identity depends on absolute doctrine (powerless agents) and those whose authority depends on applied flexibility (institutional beneficiaries).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status and exit capacity. Identity-locked traditionalists are victims with constrained exit (they can leave the church, but exit requires identity reconstruction, so exit_options = identity_locked rather than mobile). The reading extracts from them through identity destabilization. Divorced-remarried Catholics are victims with moderate exit (they can stop attending church or seek pastoral workarounds; exit is constrained but possible). Diocesan authorities are mixed — they benefit from interpretive flexibility (beneficiary characteristics) but are constrained by institutional responsibility (constrained exit relative to Vatican hierarchy). Vatican hierarchy are beneficiaries with high exit (they can change doctrine, maintain it, or apply it selectively without loss of authority). The lay coalition are organized victims with constrained exit (they remain within the institutional framework even as they apply pressure for change). These structural positions determine d values and feed the sigmoid f(d) to produce chi (effective extractiveness) for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves the mandatrophy by identifying the constraint as embodying both genuine coordination (pastoral response to human limitation, care for divorced-remarried Catholics) and asymmetric extraction (discretionary gatekeeping, destabilization of identity-dependent adherents, management of contradiction). The constraint is neither pure coordination nor pure extraction but a genuine hybrid. The mandatrophy is resolved by showing that both beneficiary and victim readings are structurally accurate — the hierarchy gains coordination benefits (flexibility) while extracting from doctrinal dependents (identity destabilization) and from divorced-remarried Catholics (inconsistent gatekeeping). The perspectival divergence (snare for victims, rope for beneficiary) is not a sign of misclassification but of structural asymmetry. The scaffold perspective (lay coalition) is also accurate — the reading is transitional if institutional power can be mobilized toward formal doctrinal revision. The piton perspective on the apparatus is accurate — the machinery persists through inertia while the primary function (doctrinal verification) has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pastoral_flexibility_vs_doctrinal_clarity,
    'Is the civic-pastoral reading''s ''compassionate discernment'' a genuine theological evolution or an institutional rationalization of doctrinal contradiction?',
    'Trace Vatican teaching evolution from Humanae Vitae (1968) through Amoris Laetitia (2016) and subsequent papal magisterium. Identify whether flexibility is framed as doctrinal development (new understanding of the same unchangeable principle) or as pastoral accommodation (suspending application for pastoral reasons while maintaining doctrine). Examine whether formal canon law has been revised or only interpretive practice.',
    'If genuine theological development: the constraint is a tangled rope managing legitimate innovation. If institutional rationalization: the constraint is a snare extracting from doctrinal dependents by destabilizing their identity-foundational commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pastoral_flexibility_vs_doctrinal_clarity, conceptual, 'Whether pastoral flexibility represents doctrinal development or institutional contradiction management').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'For divorced-remarried Catholics and identity-locked traditionalists, is suppression primarily external (institutional barriers to sacramental access, formal canon law) or internalized (they have internalized the doctrine''s claim and cannot psychologically accept bypass of it)?',
    'Historical analysis of post-Vatican II divorced-remarried Catholics: how many seek pastoral workarounds vs. how many remain suppressed by internalized doctrinal claim? Psychological research on religious identity after doctrinal relativization. Comparison of suppression experienced by those in high-transparency dioceses (where flexibility is known) vs. low-transparency dioceses (where hierarchy preserves doctrinal appearance).',
    'If primarily external: raising transparency and formalizing flexible application reduces suppression. If primarily internalized: expanding pastoral options fails to relieve suppression because the identity-lock persists. Suppression is then carried forward even after institutional barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of doctrinal dependents is structural or internalized').

omega_variable(
    annulment_tribunal_variance_mechanism,
    'Why does annulment approval rate vary dramatically by diocese (8% in some dioceses, 80%+ in others), and is this variation a feature of genuine pastoral discretion or a sign of extraction mechanism drift?',
    'Systematic analysis of annulment tribunal decision patterns: correlation between diocese demographics, bishop ideology, Vatican oversight intensity, and approval rates. Interview tribunal officials on decision criteria and pressure sources. Identify whether high-variance dioceses apply different substantive standards or merely apply the same standard with different rigor.',
    'If variance reflects genuine pastoral discernment: supports tangled rope classification (legitimate coordination with extraction cost). If variance reflects inconsistent application or ideological gatekeeping: supports snare classification (extraction mechanism using doctrinal inconsistency to maintain institutional control).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(annulment_tribunal_variance_mechanism, empirical, 'Mechanism underlying dramatic variance in annulment approval rates by diocese').

omega_variable(
    kernel_reading_contested,
    'Is the civic-pastoral reading a coherent alternative interpretation of the marriage sacrament kernel, or does it represent a partial rupture from the kernel that requires formal doctrinal revision to resolve?',
    'Theological analysis: can indissolubility and compassionate discernment coexist logically within a unified framework, or does one necessarily foreclose the other? Examine Vatican II documents (Gaudium et Spes) and post-conciliar magisterium (Paul VI, John Paul II, Francis) to identify whether flexibility is presented as development of doctrine (same core principle, new application) or as pastoral suspension (doctrine stands, but application is waived).',
    'If coherent alternative: both readings coexist_with each other; the constraint is a tangled rope managing legitimate pluralism. If partial rupture: the reading influences the hierarchical reading by creating pressure toward formal revision, but does not coexist in unified doctrine — the kernel itself is unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contested, conceptual, 'Whether civic-pastoral and hierarchical readings represent coherent alternatives or a doctrinal rupture').

omega_variable(
    identity_lock_breaking_trajectory,
    'For traditional Catholics whose doctrinal identity depends on indissolubility as absolute, what conditions would allow them to experience the civic-pastoral reading as non-extractive rather than as identity-annihilation?',
    'Ethnographic and interview research with traditional Catholics pre- and post-exposure to pastoral flexibility narratives. Identify framing shifts that allow internalization of flexibility without identity rupture (e.g., ''development of doctrine'' allows holding both indissolubility and flexibility; ''pastoral response to human weakness'' maintains doctrine while allowing exception). Track whether generational cohorts show different identity lock patterns.',
    'If identity-lock is brittle and narrative-sensitive: constraining language toward ''development'' vs ''exception'' significantly changes suppression experienced. If identity-lock is robust: no reframing dissolves it from within — exit requires identity reconstruction outside the religious framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_breaking_trajectory, empirical, 'Conditions for breaking identity lock among doctrinal traditionalists').

omega_variable(
    false_summit_natural_law_candidate,
    'Does the natural law perspective in PERSPECTIVE 7 represent a genuine ontological claim about marital essence, or does it naturalize an institutional arrangement that benefits from appearing unchangeable?',
    'Theological and historical analysis: trace the doctrine of marital indissolubility from pre-Reformation scholasticism through Vatican II to contemporary magisterium. Identify moments of doctrinal development, institutional contestation, and shifts in grounding (from sacramental theology to natural law to personalist theology). Compare with how the church handles other ''unchangeable'' doctrines when social pressure mounts (usury doctrine, slavery, etc.).',
    'If genuine natural law: mountain classification is appropriate, and civic-pastoral reading appears as a tragic compromise with modernity. If naturalized institutional arrangement: false summit detected — the constraint reclassifies to tangled rope or snare, and the natural law framing is revealed as a beneficiary rationalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, conceptual, 'Whether natural law perspective represents ontological claim or institutional naturalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_civic_theater_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(marr_civic_theater_t25, marriage_sacrament__civic_pastoral_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(marr_civic_theater_t50, marriage_sacrament__civic_pastoral_reading, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(marr_civic_extract_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(marr_civic_extract_t25, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(marr_civic_extract_t50, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(marr_civic_supp_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_civic_supp_t25, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(marr_civic_supp_t50, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% The marriage_sacrament kernel generates at least two distinct constraint stories with different extractiveness profiles and classification types. The hierarchical indissolubility reading (ε ≈ 0.25, claimed_type: mountain or rope) focuses on doctrinal coherence and maintains absolute indissolubility. The civic-pastoral reading (ε ≈ 0.48, claimed_type: tangled rope) focuses on pastoral flexibility and relativizes the doctrine. These are not the same constraint viewed differently — they embody different institutional commitments, beneficiary/victim structures, and measurement observables. They are linked through network dependency: the civic-pastoral reading downstream affects how the hierarchical reading is experienced and enforced.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__civic_pastoral_reading, institutional, 0.18).
constraint_indexing:directionality_override(marriage_sacrament__civic_pastoral_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
