% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Doctrinal Authority (Composite Overdetermination Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962–1965) is presented in institutional history as a unified
 *   ecumenical council that reformed the Roman Catholic Church. The
 *   continuity reading sees it as organic development within unchanging
 *   tradition. The rupture-progressive reading sees necessary break from
 *   pre-conciliar rigidity. The rupture-traditionalist reading sees doctrinal
 *   rupture enabling heterodox implementation. The composite
 *   overdetermination reading rejects this binary entirely: Vatican II is not
 *   ONE shift but convergence of four structurally distinct changes
 *   (liturgical modernization, ecumenical opening, ecclesiological reframing,
 *   political reorientation) packaged as unified reform. Each component has
 *   independent extractiveness, independent suppression mechanisms,
 *   independent beneficiaries and victims. The packaging itself is the
 *   extraction mechanism: reformists gain unlimited mandate to reinterpret
 *   the Council's 'spirit'; traditionalists lose textual grounds for critique
 *   because their complaint about one component (liturgy) entangles them in
 *   rejection of others (ecumenism) where the conciliar text is less
 *   ambiguous. The constraint's extractiveness (0.38) and suppression (0.52)
 *   reflect this composite structure: moderate because genuine coordination
 *   gains exist (post-war modernization requires engagement with modernity,
 *   separated Christians, revised governance), but asymmetric extraction
 *   embedded in the packaging that prevents focused critique.
 *
 * KEY AGENTS:
 *   - Reformist Episcopal Bloc: Primary beneficiary (organized/constrained) — gains authority to reinterpret 'spirit of the Council' beyond textual limits; constrained by need to maintain Council's binding authority
 *   - Pre-Conciliar Institutional Framework: Primary victim (powerless/trapped) — definitional categories displaced without replacement; no appeal mechanism; suppressed by conciliar mandate itself
 *   - Traditionalist Episcopal Minority: Secondary victim (moderate/constrained) — barred from textual critique by composite packaging; career risk for dissent; cannot mount coherent defense
 *   - Papal Authority Structure: Net beneficiary (institutional/arbitrage) — gains flexibility to reinterpret documents while claiming faithful implementation; ultimate arbiter of tradition and reform
 *   - Post-Conciliar Theological Innovation: Beneficiary (institutional/arbitrage) — authorized by 'spirit of the Council'; freed from pre-conciliar doctrinal constraints; gains institutional legitimacy for experimental theology
 *   - Analytical Observer: Meta-perspective (analytical/analytical) — sees composite packaging as the constraint structure; recognizes component independence as key to understanding ambiguity as structural feature not bug
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.38).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.52).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Doctrinal Authority (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '1ee25acd-11c7-480a-ac66-faf60c8d27e0').
narrative_ontology:cs_kernel_codification('1ee25acd-11c7-480a-ac66-faf60c8d27e0', formalized).
narrative_ontology:cs_authority_grounding('1ee25acd-11c7-480a-ac66-faf60c8d27e0', extraction).
narrative_ontology:cs_interpretation_layer_present('1ee25acd-11c7-480a-ac66-faf60c8d27e0').
narrative_ontology:cs_reading_relation('1ee25acd-11c7-480a-ac66-faf60c8d27e0', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('1ee25acd-11c7-480a-ac66-faf60c8d27e0', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ee25acd-11c7-480a-ac66-faf60c8d27e0', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, influences).
narrative_ontology:cs_axiom('1ee25acd-11c7-480a-ac66-faf60c8d27e0', foundational, multi_component_structural_independence).
narrative_ontology:cs_axiom_status(multi_component_structural_independence, holdable).
narrative_ontology:cs_axiom_grounding('1ee25acd-11c7-480a-ac66-faf60c8d27e0', multi_component_structural_independence, empirically_contingent).
narrative_ontology:cs_axiom('1ee25acd-11c7-480a-ac66-faf60c8d27e0', foundational, packaging_as_extraction_mechanism).
narrative_ontology:cs_axiom_status(packaging_as_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('1ee25acd-11c7-480a-ac66-faf60c8d27e0', packaging_as_extraction_mechanism, instrumental).
narrative_ontology:cs_reference_frame('1ee25acd-11c7-480a-ac66-faf60c8d27e0', pre_conciliar_doctrinal_uniformity_and_institutional_continuity).
narrative_ontology:cs_drift_state('1ee25acd-11c7-480a-ac66-faf60c8d27e0', contemporary_post_vatican_iii_period, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1ee25acd-11c7-480a-ac66-faf60c8d27e0', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, reformist_episcopal_bloc).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, post_conciliar_theological_innovation).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, pre_conciliar_institutional_continuity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_episcopal_minority).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, doctrinal_stability_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRE-CONCILIAR INSTITUTIONAL FRAMEWORK (SNARE) — Cannot exit the Council's binding authority. Trapped within the conciliar legitimacy structure that supersedes it. Experiences maximum extraction: its definitional categories (liturgical uniformity, strict doctrinal formulae, ecclesiological hierarchy) are displaced without replacement framework. No alternative authority to which it can appeal; suppressed by conciliar mandate itself.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__composite_overdetermination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRADITIONALIST EPISCOPAL MINORITY (SNARE) — Constrained by conciliar authority even when disagreeing fundamentally with its direction. Career risk, institutional censure, loss of position for public dissent. The composite reading adds injury: cannot even mount coherent doctrinal defense because the Council packages liturgical, ecumenical, and ecclesiological changes together, making targeted critique impossible without appearing to reject conciliar authority wholesale. Extraction mechanism is enforced homogeneity masquerading as obedience.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__composite_overdetermination_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORMIST EPISCOPAL BLOC (TANGLED ROPE) — Experiences both coordination and extraction. The Council solves coordination problems: post-war ecclesiology requires engagement with modernity, ecumenical recognition, revised liturgy. But extraction is embedded: the composite packaging allows reformists to claim unlimited mandate ('spirit of the Council') while denying traditionalists textual grounds for critique. Constrained by need to maintain Council's binding authority even as they reinterpret it.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PAPAL AUTHORITY STRUCTURE (ROPE) — Pure coordination gain from Council. Vatican II crystallizes papal supremacy as the ultimate arbiter of tradition, doctrine, and reform. No pope can be bound by Council's letter (as Paul VI demonstrated with Humanae Vitae). Authority gains flexibility to reinterpret conciliar documents while claiming faithful implementation. Net beneficiary with full exit options: can invoke the Council or its spirit as needed, can revise interpretation later.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__composite_overdetermination_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / COMPOSITE READING (TANGLED ROPE) — Sees Vatican II not as unified reform but as packaging of distinct structural changes: (1) liturgical shift (Latin to vernacular; ε≈0.52, suppression≈0.45); (2) ecumenical opening (recognizing non-Catholic churches; ε≈0.28, suppression≈0.35); (3) ecclesiological reframing (collegiality doctrine; ε≈0.42, suppression≈0.58); (4) political reorientation (religious freedom, state separation; ε≈0.35, suppression≈0.48). Composite ε (0.38) is weighted average; suppression (0.52) reflects enforcement required to hold package together. Tangled Rope type reflects genuine coordination gains (post-war modernization) embedded in asymmetric extraction (reformists gain unlimited mandate; traditionalists lose textual grounds for critique).
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: CONCILIAR LEGITIMACY THEATER (PITON) — The Council's authority as 'ecumenical assembly' is substantially performative at civilizational scale. Pre-conciliar authority structures (Magisterium, papal teaching office) persist; the Council becomes a sanctioning ritual for changes already determined by power blocs. Post-conciliar popes reinterpret documents; liturgical changes exceed conciliar text; ecumenical commitments remain unfinalized. Theater ratio (0.68) captures the performative element: the conciliar assembly legitimates rather than determines. Primary function (binding authority) has partially atrophied; maintained through institutional inertia and legend of 'spirit of the Council' that cannot be precisely named.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__composite_overdetermination_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__composite_overdetermination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__composite_overdetermination_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, TR),
    TR >= 0.70.

:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) represents a weighted composite: liturgical change (ε≈0.52) most directly harms pre-conciliar institutional continuity and constrains traditionalist practice; ecumenical opening (ε≈0.28) has lower extractiveness because it is largely a gain (recognizing non-Catholic Christians, not suppressing Catholic identity); ecclesiological reframing (ε≈0.42) extracts from hierarchical authority structures and strict doctrinal gatekeeping; political reorientation (ε≈0.35) extracts from confessional state claims but gains institutional independence. The composite ε (0.38) reflects this range: substantial but not extreme because some components offer genuine coordination gains alongside extraction. Suppression (0.52) reflects enforced coherence of the package: traditionalist minority cannot critique one component without appearing to reject conciliar authority wholesale. Theater ratio (0.68) reflects increasing performativity: the Council's authority as 'ecumenical assembly' sanctions changes already determined; post-conciliar popes reinterpret documents; 'spirit of the Council' becomes a hermeneutic frame external to the text itself. Temporal measurements show rising extractiveness (0.22→0.38), rising theater ratio (0.42→0.68), and rising suppression (0.38→0.52) as the post-conciliar period unfolds: the composite packaging's extraction mechanism strengthens as reformist interpretation diverges from conciliar text and traditionalist critique is foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   The composite reading produces larger perspectival gaps than any single-shift model would predict. The pre-conciliar framework and traditionalist minority see Snare (pure extraction, no coordination benefit, high suppression). The reformist bloc and papal authority see Rope or Tangled Rope (genuine coordination alongside extraction). The analytical observer sees Tangled Rope with embedded asymmetry: real post-war modernization needs alongside real extraction from those who lose pre-conciliar definitional authority. The piton perspective (conciliar legitimacy theater) reveals that the Council's binding authority is substantially performative at civilizational scale—it legitimates rather than determines change. The crucial insight is that these gaps are NOT due to observer bias but to COMPOSITE STRUCTURE: if the Council were a unified shift, the gaps would resolve. Because it packages four independent components, each agent experiences a different combination of genuine coordination and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from beneficiary/victim status and exit options. The pre-conciliar framework is a pure victim with no exit (d≈0.98); traditionalist minority are victims with constrained exit (d≈0.72); reformist bloc are beneficiaries with constrained exit (d≈0.35); papal authority are beneficiaries with arbitrage exit (d≈0.08). The sigmoid f(d) transforms these to effective power modifiers. Composite packaging reduces exit options for critics: a traditionalist bishop cannot exercise arbitrage (leaving the Church) without appearing to reject conciliar authority; cannot exercise constrained exit (negotiated compromise) because the package forces binary choice. This structural elevation of d → higher f(d) → higher experienced χ is the extraction mechanism itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    component_independence_thesis,
    'Are the four structural components (liturgy, ecumenism, ecclesiology, politics) genuinely independent changes with separable ε values, or are they expressions of a single unified theological shift?',
    'Counterfactual analysis: for each component, identify whether Vatican III could plausibly reverse it while retaining others. If components are truly independent, each reversal is structurally coherent; if unified, reversing one collapses the framework.',
    'If independent: composite reading is correct; each component has different extraction dynamics. If unified: continuity or rupture reading is correct; single ε measurement is appropriate. Determines whether the ''ambiguity is structural'' claim stands or collapses to doctrinal confusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_independence_thesis, conceptual, 'Whether Vatican II components are independent or expressions of unified change').

omega_variable(
    packaging_intentionality,
    'Was the composite packaging (linking liturgy, ecumenism, ecclesiology, politics) deliberate strategy by the reformist bloc, or organic outcome of conciliar process?',
    'Historical analysis of pre-conciliar planning documents, commission reports, floor interventions; interviews with surviving Council fathers and Vatican officials; archival evidence of deliberate bundling vs. emergent clustering.',
    'If deliberate: packaging is an extraction mechanism; composite reading reveals institutional strategy. If emergent: packaging is an artifact of complex negotiation; extractiveness values are lower. Changes interpretation of whether suppression (0.52) reflects enforced homogeneity or negotiated compromise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(packaging_intentionality, empirical, 'Whether composite packaging was strategic or emergent').

omega_variable(
    textual_boundary_definiteness,
    'Can Vatican II documents (16 texts, 1965 promulgation) be read as a bounded authority set, or do they require the ''spirit of the Council'' as external hermeneutic frame to generate meaning?',
    'Systematic analysis of Vatican III and subsequent magisterial documents: how many invoke the ''spirit'' beyond the text? How many reverse specific conciliar passages? Measure hermeneutic stability: documents with stable interpretation vs. documents requiring external frame.',
    'If bounded: documents have definite meaning; ''spirit'' invocations are abuses; traditionalist critique has textual footing. If requiring ''spirit'' frame: composite reading is correct about package-level ambiguity; extractiveness derives from interpretive indeterminacy itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_boundary_definiteness, empirical, 'Whether conciliar documents have bounded or spirit-dependent meaning').

omega_variable(
    authority_continuity_under_composite_reading,
    'If Vatican II is composite packaging rather than unified reform, what grounds the authority of the entire Council? Can partial packages be binding while others are revisable?',
    'Systematic review of post-conciliar papal acts: selective enforcement (some Council documents treated as unchangeable, others as revisable). If enforcement is selective, identify which components are treated as binding and which are fluid.',
    'If enforcement is uniform: Council must be unified; composite reading fails. If selective: composite reading is correct; extractiveness reflects enforcement selectivity. Authority structure is not what it claims to be.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_continuity_under_composite_reading, conceptual, 'Authority grounds when Council is composite rather than unified').

omega_variable(
    sibling_reading_underdetermination,
    'Which of the three sibling readings (continuity, rupture-progressive, rupture-traditionalist) does Vatican II''s actual textual content most plausibly support?',
    'Philological and theological analysis of conciliar documents: identify passages supporting each reading. Measure textual support density: which reading can cite the most passages in direct support?',
    'If continuity has strongest textual support: Council documents claim unbroken tradition, but reformist practice contradicts them; extractiveness derives from reinterpretation against text. If rupture-progressive has strongest support: Council intentionally breaks with past; legitimacy of traditionalist dissent is stronger. If rupture-traditionalist has strongest support: ambiguities are doctrinal errors; Council is incoherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_underdetermination, empirical, 'Which sibling reading the conciliar documents most plausibly support').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vat2_comp_theater_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(vat2_comp_theater_t5, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(vat2_comp_theater_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(vat2_comp_extractiveness_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(vat2_comp_extractiveness_t5, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(vat2_comp_extractiveness_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(vat2_comp_suppression_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(vat2_comp_suppression_t5, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(vat2_comp_suppression_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, post_conciliar_hermeneutical_indeterminacy).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, liturgical_modernization_extraction_mechanism).

% DUAL FORMULATION NOTE:
% The composite reading decomposes Vatican II from a single constraint story into a constraint family: four independent component constraints (liturgy ε≈0.52, ecumenism ε≈0.28, ecclesiology ε≈0.42, politics ε≈0.35) linked by a composite packaging constraint (ε≈0.38). This is not a measurement ambiguity but a structural claim: each component is a distinct extraction mechanism with different victims and beneficiaries. The composite story models the packaging constraint itself—the mechanism that links the components and prevents targeted critique.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, moderate, 0.72).
constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__composite_overdetermination_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
