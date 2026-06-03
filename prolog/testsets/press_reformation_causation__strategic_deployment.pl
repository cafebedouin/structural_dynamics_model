% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Press as Strategic Tool in the Reformation: Agency-First Reading
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint models ONE reading of the contested historical kernel:
 *   what causal role did the printing press play in enabling the Protestant
 *   Reformation? The strategic_deployment reading treats agency as upstream
 *   and the press as a neutral technological capacity that reformers and
 *   printers deliberately exploited to achieve predetermined theological and
 *   economic goals. Under this reading, the constraint is fundamentally about
 *   extraction and coordination: reformers and printers benefit from
 *   deliberate deployment of print technology to distribute scripture and
 *   theology at scale, while the Catholic Church's monopoly on scriptural
 *   interpretation is systematically undermined, and manuscript scribes lose
 *   economic viability. The press is experienced as a rope (coordination
 *   tool) by those deploying it strategically, but as a snare (systematic
 *   undermining of authority) by those losing control, and as tangled_rope
 *   (mixed benefit and extraction) by those caught in between. The constraint
 *   exhibits theater: censorship infrastructure (the Index Librorum
 *   Prohibitorum, book burning, surveillance networks) persists and maintains
 *   the appearance of control while becoming progressively less effective at
 *   suppressing texts. The measurement trajectory shows rising extractiveness
 *   and theater as strategic deployment becomes more coordinated and
 *   effective, while suppression requirements decline as the multiplication
 *   capacity of the press makes suppression increasingly futile.
 *
 * KEY AGENTS:
 *   - Protestant Reformers (powerful/mobile): Upstream strategic actors deliberately deploying print to distribute theology and attack Church authority — primary beneficiaries with agency and mobile options
 *   - Printer Guilds and Publishing Networks (institutional/arbitrage): Economic beneficiaries coordinating production and distribution; experience the press as coordination tool enabling profitable business
 *   - Catholic Church Authority (institutional/trapped): Primary victim losing monopoly on scriptural interpretation and textual authority; faces cascading erosion of control without structural recourse
 *   - Manuscript Scribes and Copy Networks (moderate/constrained): Secondary victims experiencing economic displacement and labor appropriation; also benefit from expanded demand for textual material
 *   - Heterodox and Radical Reformation Communities (organized/constrained): Tertiary beneficiaries and victims — benefit from mass print enabling their theological spread but experience elite reformer and printer gatekeeping
 *   - Censorship Infrastructure (institutional/arbitrage): Rendered progressively performative as print multiplication outpaces suppression capacity; persists through institutional inertia
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent strategic outcomes as technological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.52).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.48).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.52).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Press as Strategic Tool in the Reformation: Agency-First Reading").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causation__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, 'bdf4ec3b-4825-4724-bfea-bdf779839400').
narrative_ontology:cs_kernel_codification('bdf4ec3b-4825-4724-bfea-bdf779839400', fixed_text).
narrative_ontology:cs_authority_grounding('bdf4ec3b-4825-4724-bfea-bdf779839400', lineage).
narrative_ontology:cs_interpretation_layer_present('bdf4ec3b-4825-4724-bfea-bdf779839400').
narrative_ontology:cs_reading_relation('bdf4ec3b-4825-4724-bfea-bdf779839400', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('bdf4ec3b-4825-4724-bfea-bdf779839400', press_reformation_causation__mutual_shaping, influences).
narrative_ontology:cs_axiom('bdf4ec3b-4825-4724-bfea-bdf779839400', foundational, agency_upstream_of_technology).
narrative_ontology:cs_axiom_status(agency_upstream_of_technology, holdable).
narrative_ontology:cs_axiom_grounding('bdf4ec3b-4825-4724-bfea-bdf779839400', agency_upstream_of_technology, empirically_contingent).
narrative_ontology:cs_axiom('bdf4ec3b-4825-4724-bfea-bdf779839400', foundational, technology_is_instrumental).
narrative_ontology:cs_axiom_status(technology_is_instrumental, holdable).
narrative_ontology:cs_axiom_grounding('bdf4ec3b-4825-4724-bfea-bdf779839400', technology_is_instrumental, conventional).
narrative_ontology:cs_reference_frame('bdf4ec3b-4825-4724-bfea-bdf779839400', reformer_strategic_agency).
narrative_ontology:cs_drift_state('bdf4ec3b-4825-4724-bfea-bdf779839400', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bdf4ec3b-4825-4724-bfea-bdf779839400', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, printers_guilds).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_church_authority).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, manuscript_scribes).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, censorship_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CATHOLIC CHURCH AUTHORITY (SNARE) — Trapped by deliberate reformer deployment of print technology. The Church's monopoly on scriptural interpretation and textual authority is systematically undermined through coordinated action by reformers and printers who exploit the press's multiplication capacity. No exit from the information asymmetry created by mass reproduction. Maximum experienced extraction — the Church faces cascading loss of authority without structural recourse.
constraint_indexing:constraint_classification(press_reformation_causation__strategic_deployment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MANUSCRIPT SCRIBES & COPY NETWORKS (TANGLED ROPE) — Constrained by economic displacement and guild pressure, but also benefit from the expanded demand for textual material and eventual integration into printing infrastructure. The press-reformer alliance extracts their labor value and technical knowledge while destroying the scarcity economics that protected their craft. Mixed extraction: genuine coordination of text production happens alongside asymmetric appropriation of scribal expertise.
constraint_indexing:constraint_classification(press_reformation_causation__strategic_deployment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: PRINTER GUILDS & PUBLISHING NETWORKS (ROPE) — Net beneficiaries (institutional/arbitrage) who experience the constraint as coordination: connecting reformers to audiences, standardizing texts, enabling rapid dissemination. The press is their tool, and they profit from its deliberate deployment. Genuine coordination function — solving the logistics problem of moving ideas at scale — overlaps with profit extraction but the primary structure is enabling coordination.
constraint_indexing:constraint_classification(press_reformation_causation__strategic_deployment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTESTANT REFORMERS (ORGANIZED COALITION) (ROPE) — Deliberate agents deploying technology strategically. The constraint is experienced as a tool (rope) enabling their coordination of theology, scripture distribution, and ideological spread. They have agency and mobile options — they could pursue other paths (manuscript networks, oral preaching) but choose strategic press use. Low extraction experienced because this perspective controls the deployment logic.
constraint_indexing:constraint_classification(press_reformation_causation__strategic_deployment, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: HETERODOX & RADICAL REFORMATION COMMUNITIES (TANGLED ROPE) — Constrained by elite reformer gatekeeping and printer profit-seeking. These groups benefit from mass print technology enabling their theological spread, but experience extraction through elite reformer and printer control of printing capacity, textual authority, and distribution networks. Both benefited and targeted — genuine coordination (print enables their message) with asymmetric control.
constraint_indexing:constraint_classification(press_reformation_causation__strategic_deployment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: CENSORSHIP INFRASTRUCTURE (PITON) — The apparatus for manuscript control, book banning, and scriptural monopoly was rendered largely performative by the press's multiplication capacity. The censorship system persists (Index Librorum Prohibitorum, burning of books, surveillance networks) but operates with degraded function — it suppresses some texts but cannot suppress all, creating a theater of control rather than effective suppression. Maintained through institutional inertia despite reduced effectiveness.
constraint_indexing:constraint_classification(press_reformation_causation__strategic_deployment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — TECHNOLOGICAL DETERMINISM VIEW (MOUNTAIN) — From a civilizational perspective, the printing press is viewed as an invariant force that necessarily created the conditions for Reformation by making mass textual production inevitable and censorship impossible. This perspective naturalizes what is actually a contingent result of deliberate strategic deployment. The structural data will reveal this as a false summit — the press enabled Reformation but did not determine it; agency and strategic choice were upstream drivers.
constraint_indexing:constraint_classification(press_reformation_causation__strategic_deployment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(press_reformation_causation__strategic_deployment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(press_reformation_causation__strategic_deployment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, TR),
    TR >= 0.70.

:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting significant asymmetric benefit for reformers and printers alongside costs to Church authority and scribal networks. The measurement trajectory shows rising extractiveness (0.28 → 0.52 over 60 years) as strategic deployment becomes more coordinated and effective. The constraint is not maximal extraction (< 0.66) because genuine coordination of text production occurs alongside extraction — the press does solve real logistics problems (disseminating theology at scale) that benefit multiple actors. Suppression (0.48): Moderate, declining over time (0.65 → 0.48). The Catholic Church's suppression infrastructure is structurally constrained by the multiplication capacity of the press — censoring one printing location is impossible when printing occurs across 200+ European cities. Suppression remains non-zero because the Church does suppress some texts and succeeds in constraining some printing, but the mechanism becomes progressively less effective. Theater ratio (0.35): Moderate-low, reflecting that the censorship apparatus persists (theater) despite reduced effectiveness, but the primary constraint (strategic press deployment) operates with fairly high functional content — reformers are genuinely disseminating theology at scale through deliberate coordination, not merely performing dissemination. The slight rise in theater (0.22 → 0.35) reflects the censorship infrastructure becoming increasingly performative as its effectiveness declines. Claimed type (Tangled Rope): Justified by the presence of beneficiaries (reformers, printers), victims (Church, scribes), and genuine coordination function (print enables text dissemination that serves multiple parties alongside asymmetric extraction). The constraint requires active enforcement by reformers and printers (coordination of supply chains, author-printer networks, distribution logistics) and by the Church (maintenance of censorship apparatus despite declining effectiveness).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence between strategic deployers and those losing control. Reformers experience the constraint as pure coordination (rope) — solving the problem of distributing theology at scale — with high agency and mobile options. The Church experiences the constraint as pure extraction (snare) — systematic undermining of monopoly authority with no structural escape. Printers experience it as profitable coordination (rope) enabling market expansion. Scribes experience it as mixed extraction and disruption (tangled_rope) — economic displacement alongside some reintegration. The censorship infrastructure experiences it as progressive degradation (piton) — the performative apparatus persists despite reduced effectiveness. The civilizational analytical observer risks seeing it as technological inevitability (mountain) — the press 'naturally' enables mass information and therefore necessarily produced Reformation — but the structural data reveals this as a false summit: the press was a neutral capacity that required deliberate strategic exploitation to produce these outcomes. Without reformer coordination and printer profiteering, the press might have produced very different results (e.g., mass production of Catholic devotional texts, intensified surveillance through record-keeping, decentralized control of scripture without centralized theological contestation).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each actor's structural relationship to the constraint. Reformers and printers are beneficiaries with mobile/arbitrage exit options — they could pursue other dissemination methods (oral preaching, manuscript networks) but strategically choose the press because it is more profitable and effective. Their d values are low (0.15-0.25), producing negative or weak positive effective extractiveness chi — they experience the constraint as enabling (rope). The Catholic Church is a victim facing a trapped structural situation (monopoly on scriptural interpretation is systematically undermined with no structural recourse). Their d value is high (0.85-0.95), producing maximum experienced extraction chi — they experience the constraint as a snare. Manuscript scribes are victims with constrained exit (high displacement costs, some reintegration into printing) — their d value is moderate-high (0.65-0.75), producing high experienced extraction. Heterodox and radical reformer communities are both beneficiaries (their theology spreads) and victims (elite reformer and printer gatekeeping controls their access to press) — their d value is symmetric (0.50), producing moderate experienced extraction (tangled_rope). The censorship infrastructure (personified as an institutional actor) is losing control through technological multiplication — its d value is moderate-high (0.70-0.80), but the effective extractiveness declines over time as the multiplication mechanism outpaces suppression capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint illustrates how kernel readings map to different constraint types. The strategic_deployment reading (this story) produces tangled_rope: genuine coordination (text dissemination) with asymmetric extraction (reformer/printer benefits, Church/scribe costs) requiring active enforcement by both beneficiaries and victims. The technological_determinism reading would produce mountain: the press as an invariant force with inevitable effects (censorship becomes impossible, mass literacy follows, Reformation becomes necessary). The mutual_shaping reading would produce rope or piton: technology and agency coevolve without a clear upstream driver, and the constraint is experienced as a coordination mechanism that gradually transforms both technology and social practice. These are not different measurements of the same constraint — they are different constraints generated from different readings of the kernel. Mandatrophy is resolved not by choosing one type but by recognizing that the contest IS the analytical signal: the three readings generate three distinct constraints with different extractiveness values, different beneficiary/victim structures, and different temporal dynamics. The absence of a single 'true' type across all readings is precisely what makes this a kernel contest rather than a simple measurement ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_technological_inevitability,
    'Did the printing press cause the Reformation, or did Reformation reformers strategically exploit the press to achieve predetermined theological goals?',
    'Historical counterfactual analysis: would Reformation occur without the press (via oral preaching, manuscript networks)? Would the press create inevitable Reformation effects without reformer strategic deployment (e.g., did printing automatically undermine Church authority, or did reformers deliberately use it to attack Church claims)?',
    'If technology was deterministic: classification shifts toward mountain (technological inevitability) and technological_determinism reading becomes dominant. If agency was upstream: classification remains tangled_rope (strategic extraction) and strategic_deployment reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_vs_technological_inevitability, conceptual, 'Whether the press determined Reformation or reformers strategically exploited the press').

omega_variable(
    printer_autonomy_vs_reformer_direction,
    'How much were printers autonomous economic actors responding to market demand versus directed agents executing reformer strategy?',
    'Correspondence analysis: printer letters, guild records, and publishing decisions examined for evidence of reformer direction vs printer independent profit-seeking. Comparison of printing patterns in regions with strong reformer coordination vs regions without.',
    'If printers were autonomous: classification shifts toward tangled_rope from reformer perspective (extraction from printers) and rope from printer perspective (market coordination). If printers were directed: classification remains tangled_rope with printers as secondary beneficiaries executing on reformer strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(printer_autonomy_vs_reformer_direction, empirical, 'Whether printers were autonomous agents or directed by reformers').

omega_variable(
    text_standardization_as_extraction,
    'Was the press-driven standardization of biblical and theological texts an emancipatory coordination mechanism or an extraction mechanism that centralized interpretive authority?',
    'Analysis of textual variance pre/post-standardization: did standardization enable individual scriptural reading (democratization) or impose reformer-approved interpretations (centralization)? Comparison of textual control by scribal networks vs by print monopolies.',
    'If standardization was emancipatory: constraint is pure rope (coordination benefit outweighs control). If standardization was extractive: constraint is tangled_rope or snare (reformer control over interpretation substitutes for Church control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_standardization_as_extraction, empirical, 'Whether text standardization was emancipatory or extractive').

omega_variable(
    reading_vs_determinism_kernel_contest,
    'Is this a contest between three distinct readings of a single kernel (the role of the press in Reformation causation), or a fundamental disagreement about whether causation is even a meaningful analytical category for technological change?',
    'Meta-level examination: can all three readings (strategic_deployment, technological_determinism, mutual_shaping) be framed as alternative causal models operating on the same domain, or do they operate on incommensurable premises about what causation means? Do they share a common kernel or are they incompatible paradigms?',
    'If readings share a kernel: kernel_codification is fixed_text or formalized and reading_relations properly distinguish forecloses/coexists_with/influences. If readings are paradigmatically incommensurable: kernel_codification should be distributed and the contest is not resolvable within a single framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_determinism_kernel_contest, conceptual, 'Whether the three readings contest a single kernel or represent incommensurable paradigms').

omega_variable(
    suppression_counterfactual,
    'Could the Catholic Church have suppressed the Reformation through technological means if the press had not existed (e.g., through manuscript network surveillance and systematic destruction)?',
    'Historical analysis of pre-press suppression effectiveness: examination of heresy suppression rates in the 14th-15th centuries (before widespread printing) vs Reformation era. Did the press make suppression impossible or merely more expensive?',
    'If suppression was possible without press: technology was enabling, not determining. Strategic_deployment reading confirmed. If suppression was impossible without press: technology was necessary and partially deterministic. Technological_determinism gains ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_counterfactual, empirical, 'Whether suppression was possible before the press').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_reform_theater_t0, press_reformation_causation__strategic_deployment, theater_ratio, 0, 0.22).
narrative_ontology:measurement(press_reform_theater_t30, press_reformation_causation__strategic_deployment, theater_ratio, 30, 0.3).
narrative_ontology:measurement(press_reform_theater_t60, press_reformation_causation__strategic_deployment, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(press_reform_extraction_t0, press_reformation_causation__strategic_deployment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(press_reform_extraction_t30, press_reformation_causation__strategic_deployment, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(press_reform_extraction_t60, press_reformation_causation__strategic_deployment, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(press_reform_suppression_t0, press_reformation_causation__strategic_deployment, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(press_reform_suppression_t30, press_reformation_causation__strategic_deployment, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(press_reform_suppression_t60, press_reformation_causation__strategic_deployment, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, censorship_infrastructure_degradation).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, manuscript_scribe_displacement).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel contest family: three structurally distinct readings of the printing press's role in the Reformation. Each reading generates a different constraint (different ε, different beneficiary/victim structures, different types). Strategic_deployment (ε=0.52, tangled_rope) treats agency as upstream. Technological_determinism (ε≈0.08-0.15, mountain) treats the press as a natural law of information. Mutual_shaping (ε≈0.35-0.40, rope/piton boundary) treats technology and agency as coevolving. The family is linked via network.affects_constraints and differentiated via distinct base_properties and perspectives. The contest cannot be resolved by choosing one reading — the presence of three incommensurable readings IS the kernel's diagnostic signature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causation__strategic_deployment, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
