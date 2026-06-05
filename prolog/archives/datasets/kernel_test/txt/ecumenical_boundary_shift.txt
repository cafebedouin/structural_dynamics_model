% ============================================================================
% CONSTRAINT STORY: ecumenical_boundary_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecumenical_boundary_shift, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ecumenical_boundary_shift
 *   human_readable: Vatican II Ecumenical Boundary Shift and Doctrinal Authority
 *   domain: religious_institutional/doctrinal_authority/ecclesiology
 *
 * SUMMARY:
 *   Vatican II (1962-1965) presents a canonical case of doctrinal
 *   reinterpretation within a commitment system. The Council declared no new
 *   dogmas but reframed Catholic doctrine in terms of 'development' and
 *   'ressourcement' (ressourcing tradition through historical study),
 *   fundamentally altering how the Church relates to modernity, other
 *   Christian traditions, and its own institutional authority. The constraint
 *   operates on multiple levels: institutional (what counts as authoritative
 *   teaching), episcopal (who holds decision-making power), doctrinal (how
 *   doctrine relates to tradition), and lay (what role the baptized exercise
 *   in the Church). The core ambiguity: Vatican II is presented
 *   simultaneously as (1) no change in substance, only language
 *   ('hermeneutics of continuity'); (2) genuine development consistent with
 *   tradition ('legitimate magisterial development'); and (3) opening to the
 *   modern world ('ressourcement and aggiornamento'). These framings are
 *   structurally incompatible—one cannot simultaneously affirm that doctrine
 *   is unchanged, genuinely developed, and newly opened to modernity. The
 *   constraint's extractiveness comes from enforcing this incompatibility as
 *   legitimate: those who point out the contradiction are delegitimized as
 *   'rigid' or 'refusing the Spirit', while institutional actors on all sides
 *   claim Vatican II authority for contradictory positions. Theater ratio has
 *   increased over 60 years as the Council's documents have become
 *   increasingly invoked while increasingly reinterpreted, until Vatican II
 *   authority now provides rhetorical cover for nearly any position.
 *
 * KEY AGENTS:
 *   - Institutional Papacy: Primary beneficiary (institutional/arbitrage) — Vatican II elevated papal teaching supremacy while appearing to democratize Church authority through episcopal collegiality language; maintains control over official doctrine and its interpretation
 *   - Reformist Hierarchy: Primary beneficiary (institutional/arbitrage) — benefited from the Council's shift of power away from conservative curia toward 'progressive' bishops; consolidated control over post-Vatican II direction
 *   - Traditionalist Laity: Primary victim (powerless/identity_locked) — identity fused with pre-Vatican II practice and theology; institutional delegitimization ('schismatic', 'rigid') makes exit psychologically equivalent to apostasy; no organized voice
 *   - Conservative Episcopal Minority: Secondary victim (moderate/constrained) — lost institutional power during and after the Council; constrained by obedience while facing career penalties for resistance
 *   - Post-Vatican II Lay Catholics: Secondary victim (moderate/constrained) — inherit an underdetermined doctrinal framework; constrained in their capacity to understand or enforce what Catholic teaching requires
 *   - Doctrinal Continuity Claim: Victim (abstract/trapped) — the explicit Vatican II commitment to doctrinal continuity has been rendered hollow by institutional practice; cannot be enforced because enforcement would require acknowledging the doctrinal change the continuity claim denies
 *   - Ecumenical Movement: Mixed (organized/mobile) — experiences Vatican II as temporary coordination framework with sunset to full Christian communion; organized but sees constraints as provisional
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecumenical_boundary_shift, 0.52).
domain_priors:suppression_score(ecumenical_boundary_shift, 0.68).
domain_priors:theater_ratio(ecumenical_boundary_shift, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecumenical_boundary_shift, extractiveness, 0.52).
narrative_ontology:constraint_metric(ecumenical_boundary_shift, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ecumenical_boundary_shift, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecumenical_boundary_shift, tangled_rope).
narrative_ontology:human_readable(ecumenical_boundary_shift, "Vatican II Ecumenical Boundary Shift and Doctrinal Authority").
narrative_ontology:topic_domain(ecumenical_boundary_shift, "religious_institutional/doctrinal_authority/ecclesiology").

domain_priors:requires_active_enforcement(ecumenical_boundary_shift).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(ecumenical_boundary_shift, fixed_text).
narrative_ontology:cs_authority_grounding(ecumenical_boundary_shift, lineage).
narrative_ontology:cs_interpretation_layer_present(ecumenical_boundary_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecumenical_boundary_shift, institutional_papacy).
narrative_ontology:constraint_beneficiary(ecumenical_boundary_shift, progressive_reformers).
narrative_ontology:constraint_victim(ecumenical_boundary_shift, traditionalist_episcopal_authority).
narrative_ontology:constraint_victim(ecumenical_boundary_shift, doctrinal_continuity_claim).
narrative_ontology:constraint_victim(ecumenical_boundary_shift, lay_ecclesial_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONALIST LAITY (SNARE) — Identity-locked to pre-Vatican II Catholic practice and theology. Structurally mobile (could leave the Church), but identity fusion with traditional liturgy, devotion, and doctrinal certainty makes exit psychologically equivalent to apostasy. The constraint extracts doctrinal authority from this population — their own bishops declare their tradition invalid or incomplete — while suppressing any organized resistance through institutional delegitimization ('schismatic', 'rigid', 'refusing the Spirit'). Maximum experienced extraction with no viable escape route except identity death.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSERVATIVE EPISCOPAL MINORITY (TANGLED ROPE) — Bishops opposed to or uncertain about specific reforms (liturgical change, ecumenical openness, episcopal collegiality modifications) face constrained exit: they can resist specific decrees but face career penalty, loss of influence in the curia, and pressure from Vatican apparatus. The constraint provides genuine coordination benefit (the Council's deliberative structure enables doctrinal refinement) alongside asymmetric extraction (conservatives lose institutional power to reformers). They remain bound by obedience to papal authority even while that authority is shifting.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REFORMIST HIERARCHY (ROPE) — Progressive bishops and Vatican officials (John XXIII, the Dutch bishops, the Rhine bishops, periti theologians) experience Vatican II as pure coordination: the constraint enables them to reshape doctrine legitimately, establish ecumenical dialogue, and modernize ecclesiology. They benefit from the Council's authority (their readings become official teaching) while also coordinating genuine improvements in pastoral practice. Effective extraction runs toward this group, but framed as legitimate reform.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ECUMENICAL MOVEMENT (SCAFFOLD) — Organized Protestant, Orthodox, and progressive Catholic networks see Vatican II as a temporary coordination framework with a sunset clause: full communion with other Christian traditions is the stated goal, making the Catholic boundary itself provisional. The constraint provides genuine coordination (inter-church dialogue infrastructure) with low experienced extraction because the organized ecumenical actors see an exit path — eventual merger or full fellowship — that makes the current constraints temporary. Theater ratio moderates because ecumenical committees do genuine work, not performative theater.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL CHURCH (PITON) — From the Church's own perspective as an institution, Vatican II is increasingly performative ritual. The Council's stated doctrine is invoked to justify contradictory actions: 'Vatican II allows' both unprecedented liturgical experimentation AND strict doctrinal enforcement, both radical ecumenism AND renewed doctrinal assertion, both lay empowerment AND restored clerical hierarchy. The institution maintains theater around Vatican II authority without resolving the underlying contradictions. Theater ratio (0.64) reflects that interpretation of Vatican II has become a proxy game: how many opposing positions can be claimed to be 'Vatican II authentic'?
constraint_indexing:constraint_classification(ecumenical_boundary_shift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: POST-VATICAN II CATHOLICISM (TANGLED ROPE) — The actual community of 1.3 billion Catholics born after 1965 experiences Vatican II as an inherited constraint that both enables and extracts. They benefit from genuine improvements: vernacular liturgy, ecumenical respect, reduced institutional authoritarianism, permission for lay religious agency. But they also bear the cost of doctrinal instability — what counts as 'Catholic teaching' is now underdetermined, leaving lay populations in constrained interpretive space. They cannot fully exit (identity is shaped by post-Vatican II Catholicism) but also cannot enforce coherent doctrinal reading.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, Vatican II is an instance of the structural law of doctrinal reinterpretation: any living tradition eventually faces the choice between doctrinal rigidity (leading to schism) and reinterpretation (leading to ambiguity). The Council's shift from 'unchanging truth' to 'unchanging truth in new language' is presented as a natural necessity of institutional survival. However, the beneficiary structure contradicts the mountain classification — reformist hierarchy and papacy benefit substantially from the framing of change as natural rather than political — revealing this as a false summit.
constraint_indexing:constraint_classification(ecumenical_boundary_shift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecumenical_boundary_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ecumenical_boundary_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ecumenical_boundary_shift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecumenical_boundary_shift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ecumenical_boundary_shift, TR),
    TR >= 0.70.

:- end_tests(ecumenical_boundary_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high but not maximal. The constraint extracts doctrinal authority from traditionalist populations (through delegitimization) and from episcopal minorities (through centralized interpretation). However, the extraction is not pure coercion—Vatican II genuinely reforms some practices (vernacular liturgy, ecumenical respect) that were constraining. The extractiveness increases over time (0.18→0.52) as the Council's authority is invoked increasingly while its actual doctrinal commitments become increasingly reinterpreted, until the theater of Vatican II authority exceeds its functional content. Suppression (0.68): High. Multiple suppression mechanisms: institutional (removal of traditionalist bishops, delegitimization of dissent), doctrinal (reframing of traditionalist reading as 'not understanding Vatican II'), and internalized (traditionalists increasingly accept the frame that their position is 'rigid' or 'refusing the Spirit'). Suppression remains high across the interval because both structural penalties and internalized self-censorship persist. Theater ratio (0.64): Moderate-high and rising. Pre-Vatican II, Catholic institutional theater was low—doctrine was stated clearly, liturgy was fixed, authority structures were explicit. Vatican II introduced new theater: the Council claims doctrinal continuity while practicing doctrinal reinterpretation; claims to democratize authority while centralizing papal teaching supremacy; claims to open to modernity while reasserting doctrinal tradition. This theater has increased as Vatican II documents are invoked by contradictory positions, until Vatican II itself has become a performative symbol covering institutional conflicts rather than a guide to practice.
 *
 * PERSPECTIVAL GAP:
 *   The gap between traditionalist (snare) and reformist (rope) perspectives reveals that Vatican II is experienced as structurally opposite phenomena depending on one's position. The gap between the piton perspective (institutional self-view of degraded theater) and the rope perspective (reformist experience of legitimate development) reveals that the institution itself recognizes the increasing theater even while actors within it experience different realities. The gap between the scaffold perspective (ecumenical actors see a temporary boundary with sunset to full communion) and the piton perspective (institutional church increasingly treats the boundary as renewed) reveals that Vatican II's stated direction (toward ecumenical union) is contradicted by institutional practice (renewed doctrinal assertion and boundary maintenance). The gap between the tangled rope perspective (post-Vatican II lay population constrained by doctrinal instability) and the rope perspective (reformist hierarchy experiencing legitimate authorization) reveals that benefits accrue to those who participated in the Council while costs accrue to those who inherited its ambiguities.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim structure reveals who extracted doctrinal authority: the papacy and reformist hierarchy (beneficiaries) gained the power to define how Vatican II is interpreted, while traditionalists (victims) lost the power to define what counts as legitimate Catholic teaching. The ambiguity is whether this extraction was of authority or merely of a particular doctrinal position—the reformists claim the former (Vatican II is not a change, it is legitimate development of unchanging doctrine) while traditionalists claim the latter (Vatican II changed doctrine and that change is illegitimate). The constraint's suppression mechanism operates by making it impossible to ask this question: to claim that Vatican II involves doctrinal change is to be labeled 'not understanding Vatican II', while to claim that Vatican II involves no change is to accept the reformist framework that constrains traditionalist objections. The extraction is enforced through doctrinal delegitimization rather than direct coercion—traditionalists are told that their position is 'rigid', 'refusing the Spirit', or 'schismatic', rather than being forced to change through institutional penalties alone (though penalties do exist).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Vatican II is BOTH coordination AND extraction, and that these operate at different structural levels. The coordination function: Vatican II genuinely improved pastoral practice (vernacular liturgy reduces alienation, ecumenical respect reduces inter-church hostility, expanded lay roles distribute labor). The extraction function: Vatican II centralized control over doctrinal interpretation (papal teaching authority is elevated while appearing to democratize), delegitimized traditionalist authority (by reframing their position as 'not understanding development'), and imposed doctrinal instability on lay populations (who now inherit competing interpretations of what Vatican II 'really' means). The tangled rope classification holds: there is genuine coordination benefit AND asymmetric extraction. The mandatrophy is not resolved by choosing between coordination and extraction, but by recognizing that the Council achieved coordination benefits for those whose vision it authorized while imposing extraction costs on those whose vision it delegitimized. The false summit risk appears at the analytical perspective: the claim that Vatican II is a natural law of institutional evolution (all living traditions face the continuity-vs-change dilemma) naturalizes what is actually a specific institutional choice about how to manage that dilemma. Vatican II's choice was to claim continuity while practicing change, which solved the political problem (avoiding explicit schism) at the cost of doctrinal coherence. An alternative choice would have been to explicitly acknowledge doctrinal development and negotiate consensually, which would have been less extractive but more institutionally destabilizing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    single_reading_vs_overdetermined_composite,
    'Is Vatican II one coherent doctrinal reading applied across multiple domains, or multiple simultaneous independent readings that happened to coincide temporally?',
    'Structural analysis of domains where Vatican II doctrine has remained stable vs. heavily reinterpreted (e.g., Sacrosanctum Concilium vs. Unitatis Redintegratio); identification of whether reinterpretations cluster by domain or represent uniform drift; examination of whether conservative vs. progressive readings of the Council differ on a unified axis or on orthogonal axes per domain',
    'If single reading: the constraint has stable ε and beneficiary structure across all institutional contexts. If overdetermined composite: the constraint actually decomposes into multiple structurally distinct stories, each with its own ε, beneficiaries, and victims — the apparent unity is retrospective narration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(single_reading_vs_overdetermined_composite, conceptual, 'Whether Vatican II is one reading or multiple simultaneous readings').

omega_variable(
    doctrinal_continuity_vs_rupture,
    'Did Vatican II represent genuine doctrinal continuity (reinterpretation within tradition) or doctrinal rupture (substantive change in binding claims)?',
    'Historical-theological analysis of specific doctrinal claims (papal infallibility, salvation outside the Church, authority of Scripture vs. Tradition, nature of episcopal authority); assessment by both continuity-reading traditionalists and rupture-reading historians of whether the practical institutional change matches either narrative',
    'If continuity: the constraint''s suppression mechanism is framing—the reinterpretation itself is the extraction (traditionalist doctrinal authority delegitimized by being reinterpreted as ''not really different''). If rupture: the constraint''s suppression mechanism is institutional power—force was used to enforce the change despite its novelty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_continuity_vs_rupture, conceptual, 'Whether Vatican II represents continuity or rupture in Catholic doctrine').

omega_variable(
    papal_authority_gain_vs_loss,
    'Did Vatican II strengthen or weaken the papacy''s effective authority, despite the theoretical elevation of papal supremacy in Lumen Gentium?',
    'Comparative analysis of papal directive obedience rates pre/post-Vatican II (e.g., Humanae Vitae rejection rates, Summorum Pontificum implementation resistance); examination of whether episcopal conferences gained independence from Rome; assessment of whether the papacy''s ability to enforce doctrinal conformity increased or decreased',
    'If strengthened: Vatican II extraction mechanism is framing (doctrinal authority repackaged as ''opening'' while centralizing). If weakened: Vatican II is a tangled rope where institutional coordination genuinely improved but at cost of doctrinal instability. The beneficiary/victim structure inverts between these readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_gain_vs_loss, empirical, 'Whether Vatican II strengthened or weakened papal effective authority').

omega_variable(
    kernel_reading_ambiguity,
    'Vatican II is one reading of the Catholic doctrinal kernel (Sacred Tradition + Sacred Scripture + Magisterium). Which reading is instantiated: ''reading'' as legitimate development within tradition, or ''reading'' as strategic reinterpretation that naturalizes change?',
    'Examination of how Vatican II documents themselves describe their relationship to prior doctrine; assessment of whether the interpretive layer (bishops'' subsequent actions, papal clarifications, theological commentary) absorbs the doctrinal shift or propagates it as intentional change; comparison of Council fathers'' stated intent vs. post-Council institutional practice',
    'If ''legitimate development'': the constraint is structured coordination with embedded extraction that most bishops eventually accept. If ''strategic reinterpretation'': the constraint is a snare disguised as rope—suppression operates through the authority claim that the change is not a change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Vatican II as contested reading of Catholic doctrinal kernel').

omega_variable(
    lay_ecclesial_agency_extraction,
    'Did Vatican II''s apparent empowerment of the laity (through expanded roles, ecclesiology of ''people of God'', liturgical participation) constitute genuine agency expansion or a new form of managed inclusion that extracted traditional lay autonomy (parish autonomy, catechetical independence, devotional choice)?',
    'Comparative institutional analysis of pre/post-Vatican II lay decision-making power in parish governance, liturgical practice, catechetical content, and religious association formation; examination of whether new lay roles (lectors, eucharistic ministers, parish councils) enlarged practical authority or created positions without power while consolidating clerical control',
    'If genuine agency expansion: Vatican II extraction mechanism operates primarily against traditionalist episcopal minority and traditionalist laity (identity-locked), not against lay populations broadly. If managed inclusion: the constraint extracts doctrinal authority from the entire lay population (who now participate in a Church structure they do not control) under the cover of ''empowerment''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_ecclesial_agency_extraction, empirical, 'Whether Vatican II empowered or managed lay ecclesial agency').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of traditionalist opposition to Vatican II structural (institutional penalties, removal of dissent channels) or internalized (traditionalists have internalized the ''Spirit of Vatican II'' framing such that their own objections feel illegitimate)?',
    'Analysis of institutional penalties applied to traditionalists vs. progressives; examination of whether traditionalist resistance persists post-penalty or whether penalties trigger identity reconciliation; assessment of whether internalized suppression decreases after structural suppression is removed (e.g., Summorum Pontificum permitting traditional Mass without lifting other penalties)',
    'If structural: suppression remains constant at ~0.68. If internalized: suppression has increased over time as traditionalists have internalized ''rigid'', ''refusing the Spirit'' self-concepts, and even removal of structural barriers does not restore the sense that traditionalist readings are legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of Vatican II resistance is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecumenical_boundary_shift, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecum_tr_t0, ecumenical_boundary_shift, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ecum_tr_t8, ecumenical_boundary_shift, theater_ratio, 8, 0.48).
narrative_ontology:measurement(ecum_tr_t15, ecumenical_boundary_shift, theater_ratio, 15, 0.64).

% Extraction over time
narrative_ontology:measurement(ecum_be_t0, ecumenical_boundary_shift, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ecum_be_t8, ecumenical_boundary_shift, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(ecum_be_t15, ecumenical_boundary_shift, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecumenical_boundary_shift, identity_coordination).
narrative_ontology:affects_constraint(ecumenical_boundary_shift, papal_teaching_supremacy_structure).
narrative_ontology:affects_constraint(ecumenical_boundary_shift, episcopal_collegiality_authority_gap).
narrative_ontology:affects_constraint(ecumenical_boundary_shift, traditionalist_catholic_schism_formation).
narrative_ontology:affects_constraint(ecumenical_boundary_shift, lay_liturgical_agency_expansion).

% DUAL FORMULATION NOTE:
% Vatican II is a composite of multiple structurally distinct doctrinal shifts. The 'ecumenical_boundary_shift' story models the change in how the Church defines its relationship to other Christian traditions and to non-Christian modernity. Downstream constraints model the changes in specific doctrinal claims (papal supremacy vs. collegiality), institutional authority structures (who controls interpretation), and lay agency (liturgical vs. doctrinal participation). Each downstream constraint has its own ε and beneficiary/victim structure, though all are affected by the Vatican II kernel reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecumenical_boundary_shift, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
