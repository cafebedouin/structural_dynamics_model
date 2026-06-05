% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II Doctrinal Authority: Progressive Rupture Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962–1965) stands as the Catholic Church's most significant
 *   institutional reorientation in four centuries. This constraint models ONE
 *   reading of the Council's doctrinal authority: the progressive rupture
 *   reading. Under this interpretation, Vatican II represents a necessary
 *   break with pre-conciliar rigidity, and the 'spirit of the Council' —
 *   understood as the Council's underlying intent beyond its literal
 *   documents — authorizes ongoing reform beyond textual limits. This reading
 *   frames doctrinal changes (especially on religious freedom, which
 *   explicitly reverses the Syllabus of Errors of 1864) as the revelation of
 *   what the Council truly intended, even when that intent is not evident in
 *   the conciliar documents' letter. The reading treats textual ambiguities
 *   as intentional openings for further development, and post-conciliar
 *   implementation by reform-oriented bishops and theologians as the
 *   authentic realization of conciliar intent. This is a kernel reading — one
 *   of at least four structurally distinct interpretations of Vatican II that
 *   coexist in contemporary Catholicism (progressive rupture, traditionalist
 *   rupture, continuity, composite overdetermination). Each reading makes
 *   different structural claims about what authority Vatican II carries, who
 *   benefits from its interpretation, and what constraints it imposes on
 *   subsequent ecclesial development.
 *
 * KEY AGENTS:
 *   - Reform-Oriented Theological Elite (institutional/arbitrage): Primary beneficiaries — progressive theologians and modernist scholars whose interpretive authority is elevated by 'spirit of the Council' hermeneutics; career pathways and institutional influence expand significantly under this reading
 *   - Local Episcopal Implementation Authority (moderate/constrained): Secondary beneficiaries facing mixed extraction — bishops gain pastoral flexibility and ecumenical opening but lose doctrinal autonomy to progressive theological interpreters claiming expertise in authentic development
 *   - Pre-Conciliar Institutional Order (powerless/trapped): Primary victim — delegitimized as rigid and requiring rupture; no institutional advocate defending continuity; doctrinal forms, liturgical structures, and pre-conciliar teaching assigned wholesale to outdated framework
 *   - Doctrinal Continuity Principle (powerless/trapped): Structural victim — the institutional logic that doctrine develops within continuous tradition is displaced by rupture frame; no defended institutional mechanism preserves this principle under the reading
 *   - Traditionalist Episcopal Authority (moderate/trapped): Secondary victim — episcopal defenders of pre-conciliar teaching are marginalized or excluded from post-conciliar hierarchy; institutional resistance channels are suppressed
 *   - Vatican II Document Letter-Text (institutional/arbitrage): Ambiguous status — the conciliar documents are cited as authority while simultaneously being read as superseded by 'spirit'; text becomes performative prop rather than binding authority
 *   - Post-Conciliar Reform Movement (organized/constrained): Coalition beneficiary — organized progressive networks see the constraint as temporary scaffolding enabling transition; agents with organizational capacity experience lower extraction
 *   - Analytical Observer (analytical/analytical): Civilizational perspective — risks naturalizing what is contingent institutional choice (rupture framing) as structural inevitability of institutional modernization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.58).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II Doctrinal Authority: Progressive Rupture Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'f9e3081c-ddef-4c01-a914-65ffa91409db').
narrative_ontology:cs_kernel_codification('f9e3081c-ddef-4c01-a914-65ffa91409db', fixed_text).
narrative_ontology:cs_authority_grounding('f9e3081c-ddef-4c01-a914-65ffa91409db', extraction).
narrative_ontology:cs_interpretation_layer_present('f9e3081c-ddef-4c01-a914-65ffa91409db').
narrative_ontology:cs_reading_relation('f9e3081c-ddef-4c01-a914-65ffa91409db', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('f9e3081c-ddef-4c01-a914-65ffa91409db', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9e3081c-ddef-4c01-a914-65ffa91409db', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('f9e3081c-ddef-4c01-a914-65ffa91409db', foundational, doctrinal_rupture_necessity_under_modernity).
narrative_ontology:cs_axiom_status(doctrinal_rupture_necessity_under_modernity, holdable).
narrative_ontology:cs_axiom_grounding('f9e3081c-ddef-4c01-a914-65ffa91409db', doctrinal_rupture_necessity_under_modernity, empirically_contingent).
narrative_ontology:cs_axiom('f9e3081c-ddef-4c01-a914-65ffa91409db', foundational, spirit_textual_boundary_indeterminacy_as_virtue).
narrative_ontology:cs_axiom_status(spirit_textual_boundary_indeterminacy_as_virtue, holdable).
narrative_ontology:cs_axiom_grounding('f9e3081c-ddef-4c01-a914-65ffa91409db', spirit_textual_boundary_indeterminacy_as_virtue, deontological).
narrative_ontology:cs_reference_frame('f9e3081c-ddef-4c01-a914-65ffa91409db', rupture_from_syllabus_errors_framework).
narrative_ontology:cs_drift_state('f9e3081c-ddef-4c01-a914-65ffa91409db', contemporary_post_2013, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9e3081c-ddef-4c01-a914-65ffa91409db', '2026-02-26T14:37:22Z').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, reform_oriented_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, theological_modernists).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_implementation_hierarchy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, doctrinal_continuity_principle).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_institutional_framework).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_episcopal_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRE-CONCILIAR ORDER (SNARE) — The pre-Vatican II doctrinal structure, institutional hierarchy, and liturgical forms cannot exit the reinterpretation regime. This reading treats that entire framework as rigid, requiring rupture. The pre-conciliar order bears full extraction cost: delegitimized as a whole, its institutional defenders marginalized, its doctrinal authority permanently suspended as outdated. No self-correction mechanism for the victim — the reading authoritatively declares rupture as necessary, precluding defense of continuity as a legitimate option.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_progressive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOCAL EPISCOPAL IMPLEMENTATION AUTHORITY (TANGLED ROPE) — Bishops implementing Vatican II face genuine coordination problems (pastoral adaptation, ecumenical openness, liturgical renewal) alongside asymmetric extraction. The 'spirit of the Council' framing gives bishops freedom to reinterpret conciliar documents beyond their literal text, but also requires them to delegate theological authority upward to progressive theologians who claim expertise in 'authentic development.' Bishops benefit from pastoral flexibility; they are also constrained by external interpretation of what counts as legitimate reform. Mixed coordination and extraction.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORM-ORIENTED THEOLOGICAL ELITE (ROPE) — Theologians and progressive clergy experience the 'spirit of the Council' framing as pure coordination: opening institutional channels for doctrinal development, enabling their intellectual authority to shape ecclesial reform, creating career pathways for modernist scholarship. The reading directly benefits this group — they capture interpretive authority during implementation. Experiences minimal suppression because the reading aligns with their institutional interests.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_progressive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOCTRINAL CONTINUITY PRINCIPLE (SNARE) — This reading fundamentally redefines how doctrine is treated institutionally. The principle that doctrine develops organically within continuous tradition is displaced by a rupture frame that permits radical reinterpretation. The principle has no institutional advocate under this reading — it is not debated but presumed superseded. Trapped without exit, bearing full cost of delegitimization.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_progressive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-CONCILIAR REFORM MOVEMENT (SCAFFOLD) — Organized progressive agents (reform networks, theological associations, Vatican offices tasked with implementing change) see the constraint as temporary scaffolding toward a more open Church. The 'spirit of the Council' is framed as a temporary hermeneutical tool enabling transition from rigidity to authentic development. Sunset logic: once sufficient reform has occurred, the Church will stabilize in a new equilibrium where 'spirit of the Council' language is no longer necessary — the reforms become institutionalized. Organized agents perceive agency and exit pathway; low experienced extraction because they see the constraint as enabling rather than constraining.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_progressive_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: VATICAN II DOCUMENT LETTER-TEXT AUTHORITY (PITON) — The actual conciliar documents themselves (Sacrosanctum Concilium, Dignitatis Humanae, Lumen Gentium) are substantially performative props under this reading. The reading declares that authentic conciliar intent is located in the 'spirit,' not the text. Document language becomes a theater of legitimation — cited selectively when convenient, overridden by 'spirit' logic when literal meaning constrains reform. The text persists through institutional inertia (it is the Council's official output) but its functional authority is degraded. Theater ratio measures the gap between stated doctrinal care and actual hermeneutical practice: conciliar language is invoked as authority while simultaneously being read as superseded by deeper intent.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_progressive_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NECESSARY RUPTURE VIEW (MOUNTAIN) — From a civilizational scope, this reading treats doctrinal rupture as a structural inevitability. Pre-conciliar teaching on religious freedom, Church-state relations, and pluralism had become untenable in a post-colonial, human-rights framework. The Council's reorientation is presented as necessary, not contingent — a natural historical development that any viable institution must undergo. The reading risks naturalizing what is a contingent institutional choice (to reframe doctrine via rupture rather than via continuity) as an immutable structural requirement. This perspective instantiates a candidate false summit.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_progressive_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_progressive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__rupture_progressive_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, TR),
    TR >= 0.70.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading directly benefits reform-oriented clergy and theological elites by elevating their interpretive authority and creating institutional pathways for their scholarly influence. It extracts from traditionalist defenders of pre-conciliar teaching by delegitimizing their position wholesale and suppressing institutional channels for continuity-framed theology. The extraction is not as severe as pure snare (0.66+) because the reading can claim genuine institutional coordination functions (pastoral flexibility, ecumenical opening, doctrinal development). However, the coordination function is inseparable from the asymmetric benefit to progressive interpreters. Extractiveness has risen over time (0.38 → 0.58) as the implementation regime has solidified and institutional consequences for traditionalist resistance have increased. Suppression (0.62): Moderate-high. The reading requires active suppression of alternative interpretations — particularly the continuity reading and traditionalist positions. This suppression operates through institutional mechanisms (exclusion from theological hierarchies, marginalization of traditionalist scholarship) rather than formal prohibition. The 'spirit of the Council' framing itself acts as a suppression mechanism: it declares that textual defenses are insufficient and that only those claiming access to deeper conciliar intent are legitimate interpreters. Traditionalist exit options are substantially constrained (not quite trapped, but facing severe career and institutional costs for resistance). Theater ratio (0.68): Moderate-high and rising. The 'spirit of the Council' framing is substantially performative. Conciliar documents are cited as authority while being read as superseded by deeper intent. The reading invokes Vatican II's textual legitimacy while displacing the text's letter with hermeneutical extrapolation. Post-conciliar implementation language frequently cites Council texts while interpreting them in ways the documents do not explicitly authorize. The theater has increased over time (0.48 → 0.68) as the gap between stated fidelity to Vatican II and actual innovation has widened, requiring more rhetorical work to maintain the appearance of continuity while delivering rupture.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival divergence. The reform-oriented theological elite (Perspective 3) experience pure coordination (Rope) — the reading directly benefits them and creates no external constraints. Local bishops (Perspective 2) experience tangled rope — genuine pastoral coordination alongside asymmetric extraction from theological elites who claim to interpret authentic development. The pre-conciliar order and doctrinal continuity principle (Perspectives 1, 4) experience pure extraction (Snare) — delegitimized without institutional defense. The post-conciliar reform movement (Perspective 5) experiences the constraint as temporary scaffold — organized agents with exit pathways see low extraction because they perceive control. The analytical observer (Perspective 7) risks seeing immutable necessity (Mountain) — treating institutional rupture as structural inevitability of modernization. The document text itself (Perspective 6) becomes a degraded piton — invoked performatively while functionally superseded. No single type captures the constraint's structure; the presheaf of perspectives reveals how the same reading distributes extraction asymmetrically across differently-positioned agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural relationship to the constraint: d ≈ 0.0 indicates full beneficiary (extraction flows toward the agent), d ≈ 1.0 indicates full target (extraction flows from the agent). The progressive rupture reading achieves high extractiveness toward reform-oriented interpreters by making their hermeneutical authority the gatekeeper for legitimate development. The reading benefits from the fact that beneficiary and victim groups are structurally asymmetric: the beneficiaries (reform theologians, progressive clergy) are institutionally organized and mobile, while primary victims (pre-conciliar order, doctrinal continuity principle) are institutional abstractions without agency. Traditionalist victims (conservative bishops, continuity-defending scholars) face constrained exit — they can resist but at high career cost. The derived directionality feeds into the χ formula: reform theological elites have low d (beneficiaries) yielding negative f(d) contribution; traditional defenders have high d (victims) yielding high f(d). The global scope modifier σ(global) = 1.2 amplifies the effective extractiveness because the reading's authority claims apply universally across all Catholic dioceses. No directionality override is necessary; the structural data (beneficiaries, victims, exit options) maps correctly to the derived d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by explicit kernel-reading decomposition. The 'question of what Vatican II actually means' has no single correct answer — it is a genuine contested kernel with multiple structurally defensible readings. The progressive rupture reading is one reading, not the reading. It achieves its extractive structure not through incontrovertible epistemic authority but through institutional power: reform-oriented interpreters have captured key Vatican positions, theological institutions, and episcopal formation networks. The mandatrophy is resolved by acknowledging that all readings instantiate constraints with different ε values and different beneficiary/victim structures. A continuity reading would have lower extractiveness (continuity interpreters gain less institutional advantage from their reading) and higher coordination functions (the continuity reading can claim to preserve institutional identity). A traditionalist rupture reading would have high extractiveness (both toward modernists AND toward progressive bishops it views as betraying tradition). The presheaf of readings over the contested kernel prevents any single reading from claiming final authority — each reading's classification depends on who is positioned to benefit from that reading's institutional adoption. The manifold of readings IS the structure of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spirit_textual_boundary_definition,
    'What constitutes the boundary between authentic ''spirit of the Council'' development and heterodox innovation beyond conciliar intent?',
    'Historical documentation of Vatican statements on authentic development; analysis of cases where Vatican Rome rejected specific reforms as unauthorized; identification of institutional criteria applied to distinguish legitimate from illegitimate implementation',
    'If boundary is formally defined and consistently enforced: constraint functions as rule-based coordination (Rope). If boundary is indeterminate or selectively enforced: constraint functions as discretionary extraction (Snare). If boundary is explicitly left ambiguous: extraction mechanism is hidden in epistemic uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spirit_textual_boundary_definition, empirical, 'Definition and enforcement of boundaries for legitimate ''spirit of the Council'' development').

omega_variable(
    continuity_hermeneutics_foreclosure,
    'Does this reading''s core premise (rupture is necessary) logically foreclose the sibling continuity reading, or do they coexist as live institutional positions held by different theological factions?',
    'Examination of Vatican''s treatment of continuity-framed scholarship post-Vatican II; analysis of whether continuity advocates are silenced, sidelined, or engaged as legitimate interlocutors; documentation of institutional pathways available to each reading''s advocates',
    'If foreclosed: rupture reading directly eliminates continuity as coherent option — one framework cannot hold both. If coexist: both readings remain defensible within institutional discourse — marks a genuine contested kernel with no resolution. If influences: rupture reading creates structural pressure against continuity advocates (career risk, resource denial) without logically eliminating their position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_hermeneutics_foreclosure, conceptual, 'Whether rupture reading logically forecloses continuity reading or coexists with it').

omega_variable(
    post_conciliar_implementation_authenticity,
    'What proportion of post-Vatican II doctrinal and liturgical changes were explicitly authorized by conciliar documents vs authorized only by ''spirit of the Council'' hermeneutics?',
    'Systematic mapping of major changes (liturgical language, Mass orientation, marriage annulment rate increases, clerical celibacy proposals, catechetical content, episcopal power reallocation) against explicit conciliar text; documentation of which changes required hermeneutical extrapolation beyond stated conciliar provisions',
    'If most changes have explicit textual basis: reading''s narrative of rupture via spirit-text distinction is weakened; documents are more generative than reading claims. If majority require hermeneutical extrapolation: reading''s structure is confirmed — implementation genuinely goes beyond text, dependent on ''spirit'' logic for legitimacy. High ratio of extrapolation increases extractive mechanism''s scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_conciliar_implementation_authenticity, empirical, 'Proportion of post-conciliar changes authorized by explicit text vs ''spirit'' hermeneutics').

omega_variable(
    traditionalist_institutional_exit_capacity,
    'Did traditionalist resistance to this reading maintain independent institutional channels, or was resistance suppressed and marginalized?',
    'Documentation of traditionalist episcopal succession (SSPX, sede vacantism, conservative regional hierarchies); analysis of Vatican''s enforcement actions against traditionalist bishops; identification of whether traditionalist theological institutes maintained institutional standing or were excluded from postconciliar hierarchy',
    'If traditionalists maintained independent channels: reading coexists with rival reading; suppression ≤ 0.60. If traditionalists were systematically excluded: suppression ≥ 0.65, reading approaches snare classification. Institutional suppression amplifies extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_institutional_exit_capacity, empirical, 'Institutional channels and suppression capacity available to traditionalist resistance').

omega_variable(
    natural_law_status_of_rupture_necessity,
    'Is doctrinal rupture in response to modernity a contingent institutional choice, or a necessary structural feature of how living tradition must operate?',
    'Comparative analysis of other major institutional ruptures (Reformation, Enlightenment philosophy, Marxism) and institutional responses; philosophical analysis of whether tradition can accommodate paradigm-level doctrinal reversal without identity dissolution; examination of whether pre-conciliar theology was actually untenable or merely unpopular',
    'If contingent: reading is a contestable interpretation, not natural law. If necessary: reading''s mountain classification (Perspective 7) is justified. This distinction determines whether the analytical perspective should be mountain or tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_status_of_rupture_necessity, conceptual, 'Necessity vs contingency of doctrinal rupture as institutional response to modernity').

omega_variable(
    kernel_reading_identity_fusion_analysis,
    'Does the institutional embrace of this reading (rupture as necessary) constitute genuine theological development or identity-locked commitment to post-1960s institutional self-concept?',
    'Analysis of Vatican statements explicitly reconsidering Vatican II necessity (e.g., post-2013 Francis-era statements about need for reform of the reform); documentation of whether the Church demonstrates capacity to debate rupture necessity as contingent, or treats rupture as constitutive of post-conciliar identity; examination of whether institutional capacity for critical reflection on Vatican II exists',
    'If genuine theological development: reading is analytically defensible, not captured. If identity-locked: institutional commitment to rupture reading is itself an extraction mechanism — the Church''s post-conciliar identity is fused with this reading''s authority structure, enabling extraction through institutional identity preservation. High identity-lock would reclassify the reading as itself a snare mechanism at the institutional level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_fusion_analysis, conceptual, 'Whether institutional embrace of rupture reading constitutes theological development or identity fusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vat2_prog_theater_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(vat2_prog_theater_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(vat2_prog_theater_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(vat2_prog_extract_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(vat2_prog_extract_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(vat2_prog_extract_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vat2_prog_supp_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(vat2_prog_supp_t10, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(vat2_prog_supp_t20, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.18).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, postconciliar_episcopal_authority_implementation).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, liturgical_continuity_vernacular_reform).

% DUAL FORMULATION NOTE:
% This constraint story models the progressive rupture reading of Vatican II doctrinal authority. It is one member of a constraint family instantiating the contested kernel vatican_ii_doctrinal_authority. Sibling constraint stories (continuity, traditionalist rupture, composite overdetermination) have different ε values and different beneficiary/victim structures. The full system is the presheaf of constraints over the kernel; no single reading achieves final authority. Network edges link this reading to siblings and to downstream constraints (postconciliar implementation, liturgical change) that are affected by which reading gains institutional dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
