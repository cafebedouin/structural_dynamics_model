% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__market_access_reading, []).

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
 *   constraint_id: wto_treaty_framework__market_access_reading
 *   human_readable: WTO Market Access Reading: Symmetric Trade Liberalization Obligation
 *   domain: international_trade_law/development_economics
 *
 * SUMMARY:
 *   The market access reading of the WTO treaty framework instantiates trade
 *   liberalization as a symmetric universal obligation, with
 *   non-discrimination and market access as the primary treaty purpose.
 *   Special and Differential Treatment (S&D) provisions are classified as
 *   temporary transitional exceptions rather than permanent structural
 *   accommodations. Under this reading, tariff bindings, subsidy disciplines,
 *   and the elimination of local-content requirements are treated as
 *   immutable commitments that apply equally to all member states. The
 *   structural consequence is dramatic compression of policy space for
 *   infant-industry protection, forced integration into global supply chains
 *   at comparative-advantage specialization, and direct extraction flowing
 *   toward multinational corporations and developed-state exporters. This
 *   reading contests the developmental reading, which positions S&D and
 *   technology transfer obligations as core commitments recognizing
 *   structural asymmetries. The market access reading has achieved
 *   institutional dominance: the WTO dispute settlement mechanism has
 *   interpreted S&D provisions narrowly, tariff bindings have proven legally
 *   enforceable across multiple dispute cycles, and subsidy disciplines have
 *   been extended rather than relaxed. Yet the structural data reveals that
 *   this reading benefits identifiable institutional actors (MNCs, developed
 *   states) while imposing costs on identifiable victims (infant industries,
 *   subsistence agriculture, developing-state policy autonomy). The
 *   measurement trajectory shows rising extractiveness (0.42 → 0.65) and
 *   rising suppression (0.55 → 0.72) over the 30-year interval (1995–2025),
 *   indicating that the constraint's extraction mechanism has strengthened as
 *   tariff bindings matured and dispute rulings accumulated. The theater
 *   ratio (0.38 → 0.52) reflects the DSM's increasingly performative
 *   character: it awards remedies but cannot force compliance from powerful
 *   states. This is a diagnostic kernel-reading case: the same treaty text
 *   generates two irreconcilable constraint readings depending on whether S&D
 *   is treated as temporary exception or permanent structural feature.
 *
 * KEY AGENTS:
 *   - Multinational Corporations: Primary beneficiary (institutional/arbitrage) — captures extraction via market access elimination of local content requirements, global supply chain optimization, and access to developing-state markets at tariff-bound rates
 *   - Developed State Exporters: Primary beneficiary (institutional/arbitrage) — benefits from predictable tariff bindings and market access; externality absorption of infant industry failure costs
 *   - Infant Industries: Primary victim (powerless/trapped) — locked into competition before achieving scale competitiveness; tariff bindings and subsidy disciplines prevent policy tools that enabled developed-state industrialization
 *   - Subsistence Agricultural Sectors: Primary victim (powerless/trapped) — direct extraction via import surges from subsidized developed-state agriculture; price compression through tariff reduction schedules with no compensatory mechanisms
 *   - Developing State Government: Mixed position (moderate/constrained) — experiences coordination benefits (MFN access, rule participation) and extraction (policy space compression, infant industry sacrifice)
 *   - Least Developed Country Coalition: Organized victim (organized/constrained) — collective leverage through voting blocs but high coordination costs of exit; faces institutional lock-in despite organizing capacity
 *   - WTO Institutional Structure: Institutional performer (institutional/arbitrage) — maintains DSM theater while enforcement capacity degrades; persists through inertia despite limited rule-enforcement ability
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing comparative advantage as economic law; framework presented as reflecting market reality rather than contingent institutional choice benefiting specific actors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, 0.58).
domain_priors:suppression_score(wto_treaty_framework__market_access_reading, 0.68).
domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Market Access Reading: Symmetric Trade Liberalization Obligation").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international_trade_law/development_economics").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, 'dbe0f44a-2348-4c65-bbc3-34bc445c428e').
narrative_ontology:cs_kernel_codification('dbe0f44a-2348-4c65-bbc3-34bc445c428e', fixed_text).
narrative_ontology:cs_authority_grounding('dbe0f44a-2348-4c65-bbc3-34bc445c428e', extraction).
narrative_ontology:cs_interpretation_layer_present('dbe0f44a-2348-4c65-bbc3-34bc445c428e').
narrative_ontology:cs_reading_relation('dbe0f44a-2348-4c65-bbc3-34bc445c428e', wto_treaty_framework__developmental_reading, coexists_with).
narrative_ontology:cs_axiom('dbe0f44a-2348-4c65-bbc3-34bc445c428e', foundational, symmetric_universal_obligation).
narrative_ontology:cs_axiom_status(symmetric_universal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('dbe0f44a-2348-4c65-bbc3-34bc445c428e', symmetric_universal_obligation, deontological).
narrative_ontology:cs_axiom('dbe0f44a-2348-4c65-bbc3-34bc445c428e', foundational, temporary_transition_sdp).
narrative_ontology:cs_axiom_status(temporary_transition_sdp, overridden).
narrative_ontology:cs_axiom_grounding('dbe0f44a-2348-4c65-bbc3-34bc445c428e', temporary_transition_sdp, empirically_contingent).
narrative_ontology:cs_reference_frame('dbe0f44a-2348-4c65-bbc3-34bc445c428e', liberal_universalism_nondiscrimination).
narrative_ontology:cs_drift_state('dbe0f44a-2348-4c65-bbc3-34bc445c428e', contemporary_post_2015, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dbe0f44a-2348-4c65-bbc3-34bc445c428e', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, developed_state_exporters).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, capital_intensive_sectors).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, infant_industries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_state_industrial_capacity).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, subsistence_agricultural_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFANT INDUSTRY (SNARE) — Structurally locked into global competition before achieving scale competitiveness. Tariff bindings and subsidy disciplines prevent policy tools that enabled industrialization in current developed states. No exit: withdrawal from WTO creates trade retaliation; compliance prevents strategic protection. Maximum experienced extraction: gains from comparative advantage accrue to already-developed capital-intensive sectors; learning costs and externalities are borne locally.
constraint_indexing:constraint_classification(wto_treaty_framework__market_access_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBSISTENCE AGRICULTURE (SNARE) — Faces direct extraction via import surges from subsidized developed-state agriculture. Tariff reduction schedules mandate price compression. No structural exit: non-compliance triggers disputes and tariff escalation on other sectors; relocation costs are prohibitive for agricultural land. Suppression is structural: WTO rules prevent compensatory support mechanisms and technology transfer obligations.
constraint_indexing:constraint_classification(wto_treaty_framework__market_access_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEVELOPING STATE GOVERNMENT (TANGLED ROPE) — Experiences the constraint as mixed coordination and extraction. The market access framework provides genuine benefits: access to MFN tariff rates, reduction commitments on developed-state markets, and participation in rule-setting (however asymmetric). Simultaneously, tariff binding commitments and subsidy disciplines compress policy space for strategic industrial development. Constrained exit: withdrawal triggers retaliation but is structurally possible; compliance requires sacrificing industrial policy tools. The constraint's extraction is not maximal because some coordination benefits genuinely flow: preferential market access for limited sectors, dispute resolution mechanisms, technology transfer aspirations in text.
constraint_indexing:constraint_classification(wto_treaty_framework__market_access_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MULTINATIONAL CORPORATION (ROPE) — Experiences the framework as pure coordination: tariff reduction, non-discrimination, subsidy disciplines, and market access elimination of local content requirements enable global supply chain optimization. The constraint solves a genuine coordination problem: competing to exclude each other through tariff escalation and local-content requirements is mutually destructive; the framework enables mutual commitment to open markets. Net beneficiary with arbitrage options: can shift investment to highest-return locations globally, source inputs cross-border, and capture externalities of infant industry development without bearing learning costs.
constraint_indexing:constraint_classification(wto_treaty_framework__market_access_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPED STATE EXPORTER (ROPE) — Experiences the framework as coordination mechanism. Binding tariff rates, elimination of local content requirements, and disciplines on domestic support create predictable market access. The framework solves the coordination problem of preventing tit-for-tat protection spirals. Net beneficiary: can rely on tariff bindings to access developing-state markets, export without facing discriminatory local-content rules, and move production to lowest-cost locations globally (often developing states with labor-cost arbitrage rather than infant industry development).
constraint_indexing:constraint_classification(wto_treaty_framework__market_access_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LDC COALITION (TANGLED ROPE) — Organized actors (WTO coalitions, trade ministries, development advocacy) experience the framework as asymmetric constraint with limited exit. The market access reading provides coordination on MFN principles and some preferential access schemes (GSP, EBA). Simultaneously, the binding nature of tariff commitments and the subsidy framework (disciplines on local content, export subsidies) compress industrial policy space. The coalition has leverage through Doha negotiations and voting blocs but faces high coordination costs to exit — the framework's institutional lock-in is significant. Organized exit is possible but requires coalition-wide consensus and triggers cascading retaliation across sectors.
constraint_indexing:constraint_classification(wto_treaty_framework__market_access_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: WTO INSTITUTIONAL STRUCTURE (PITON) — The Dispute Settlement Mechanism and secretariat enforcement of the market access framework is substantially performative. The DSM can award remedies (tariff retaliation) but cannot force regulatory change; powerful states ignore adverse rulings (US/steel tariffs, China/subsidies counterclaims). The institutional structure persists through inertia — states remain members despite limited enforcement capacity — because the coordination benefits (access to rules, participation in negotiations) are still valuable. Theater ratio is high: the framework performs legality and symmetry while structural asymmetries persist. A piton because the primary function (rule enforcement) has degraded while the institutional form is maintained through procedural theater.
constraint_indexing:constraint_classification(wto_treaty_framework__market_access_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the market access framework instantiates comparative advantage as a law of economics: states that specialize according to factor endowments achieve Pareto-optimal outcomes. Trade liberalization is immutable because market-closing policies are self-defeating. This perspective sees the market access reading as reflecting economic reality, not contingent institutional choice. However, the structural data — identifiable beneficiaries (MNCs, developed exporters), identifiable victims (infant industries), active enforcement mechanisms, and suppression of alternative policy tools — contradicts the mountain classification. The engine will compute this as a false summit, revealing that 'comparative advantage' naturalizes what is actually a contingent institutional framework benefiting specific actors.
constraint_indexing:constraint_classification(wto_treaty_framework__market_access_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__market_access_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wto_treaty_framework__market_access_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wto_treaty_framework__market_access_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, TR),
    TR >= 0.70.

:- end_tests(wto_treaty_framework__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, rising to 0.65): The market access reading imposes measurable costs on infant industries and developing-state policy autonomy while providing asymmetric benefits to MNCs and developed-state exporters. Base extractiveness is high (0.58) but not maximum (0.70+) because some genuine coordination benefits exist — MFN access, binding commitments that prevent arbitrary discrimination, and access to developed-state markets provide real gains to developing states despite compression of policy space. The rising trajectory reflects empirical fact: early WTO years (1995–2005) saw high expectations that S&D would function as temporary exceptions; by 2010–2025, the DSM's narrow interpretation of S&D and the maturation of binding commitments revealed the framework's extraction mechanism more clearly. Developing states gradually recognized that tariff bindings made at very low baseline rates (often through coercive structural adjustment) locked in disadvantageous schedules. Suppression (0.68, rising to 0.72): High. Structural barriers to exit include treaty lock-in (30-year commitments), retaliation costs for non-compliance, and the absence of alternative coordination mechanisms for market access. The framework explicitly restricts policy tools that historically enabled development (infant-industry tariffs, local-content requirements, subsidy support for state capacity building). Unlike pure snare constraints, this suppression is not total because developing states retain formal participation rights, can negotiate Doha amendments (however slowly), and can technically withdraw (though at prohibitive cost). Rising suppression reflects that early years included broader S&D language (now narrowly interpreted) and that the constraint has ossified through dispute precedent. Theater ratio (0.52): Moderate-high. The WTO Dispute Settlement Mechanism performs legality and symmetry while structural asymmetries persist. The DSM cannot force compliance from powerful states (US steel tariffs, China subsidies counterclaims remain unenforced); it can only authorize tariff retaliation, which often harms the complaining state more than the violator. The procedural theater (symmetric dispute resolution, rule-of-law framing) masks asymmetric enforcement outcomes. Theater has risen over time as powerful-state non-compliance has become visible and normative (developed states increasingly violate DSM rulings without losing legitimacy), revealing the institution's degraded enforcement function. The constraint is tangled rope, not snare, precisely because some coordination genuinely occurs — the theater is not total, and the framework does solve some genuine problems (preventing tariff escalation spirals, enabling market access at predictable rates). The piton perspective reveals that the institutional structure persists through inertia despite degraded function.
 *
 * PERSPECTIVAL GAP:
 *   The market access and developmental readings coexist as genuinely incommensurable treaty interpretations. The market access reading emphasizes non-discrimination and equal obligation; the developmental reading emphasizes structural asymmetry and permanent accommodation. From the WTO's own institutional perspective, both readings have treaty textual support (GATT Articles XVI-XVIII contain both non-discrimination language and infant-industry exceptions). The perspectival gap is not a disagreement about facts but about which treaty obligations have primacy. The market access reading has achieved institutional dominance through DSM interpretation (narrow reading of S&D), but this dominance is contingent on the composition of the Appellate Body and the balance of power among major trading states. If the reading structure shifted (e.g., if India or African coalition states gained appellate influence), the same treaty text could sustain the developmental reading with different precedent. This is a genuine bifurcation in treaty interpretation, not a matter of empirical uncertainty.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural relationship to the market access constraint. Multinational corporations and developed exporters are beneficiaries with high arbitrage options (they can exit to bilateral/regional agreements or unilateral trade agreements without severe cost); their d-values are low (~0.05–0.20), producing negative or very low f(d) → low chi. Infant industries are trapped victims (tariff bindings lock them in; exit is blocked by retaliation; non-compliance triggers disputes they cannot win); their d-values are high (~0.90–0.95), producing high f(d) → high chi. Developing governments are constrained victims-with-limited-benefits; their d-values are moderate-high (~0.65–0.75), producing moderate f(d). The LDC coalition has organized power but constrained exit; d-value ~0.60–0.70. The analytical observer's canonical d is 0.73 (analytical power atom), producing moderate chi and the false-summit classification (mountain observed, but structural beneficiaries present). Each perspective's chi value (effective extraction experienced) is computed from these d-values via f(d) × ε × σ(S); developed exporters experience low chi (they benefit); infant industries experience high chi (they bear the cost); the developmental reading's competing interpretation of S&D availability would shift d-values downward for developing states (reducing experienced chi) and upward for developed states (in a framework where S&D is binding obligation rather than temporary exception).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing this as a kernel-reading case where two incommensurable interpretations of the same treaty text generate different constraints with different epsilon values and different benefit/cost distributions. The mandatrophy question — 'is trade liberalization coordination or extraction?' — has no single answer because the treaty itself is ambiguous on whether S&D is temporary or permanent, whether market access is the primary obligation or whether development accommodation is. The market access reading answers 'coordination with temporary exceptions' (epsilon 0.58, tangled rope from developing-state view). The developmental reading answers 'coordination with permanent asymmetry accommodation' (epsilon 0.38, tangled rope from both views, different perspectives). The constraint story resolves mandatrophy not by choosing a single answer but by documenting both readings as live interpretations of the same kernel, with different structural consequences. For decision-making purposes, the choice between readings is a political/normative choice about what the treaty should prioritize, not an empirical discovery of what it 'really' says. The omega variables document the ambiguity in the kernel text and the institutional path-dependence of which reading dominates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infant_industry_learning_window,
    'What is the empirically optimal learning window for infant industries to achieve competitiveness, and does the WTO binding schedule (typically 5–20 years) correspond to actual learning requirements or to political negotiation outcomes?',
    'Historical analysis of East Asian and Latin American industrialization trajectories; correlation between tariff protection duration and industry maturation; counterfactual analysis of firms that achieved competitiveness under protection vs. without',
    'If learning windows are empirically 20–50 years and WTO schedules are 5–20 years: constraint is pure extraction (epsilon upward to 0.70+, snare from all victim perspectives). If learning windows are shorter: constraint reflects legitimate market coordination and epsilon downward to 0.30–0.45 (tangled rope holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infant_industry_learning_window, empirical, 'Whether WTO binding schedules match empirical infant industry maturation timelines').

omega_variable(
    alternative_development_pathways_foreclosed,
    'Are the policy tools restricted by the market access reading (tariff protection, local content requirements, infant-industry exceptions, state-owned enterprise support) actually necessary for industrialization, or are there alternative development models that work without them?',
    'Comparative analysis of development trajectories with and without protected sectors; identification of successful late-industrializers and the policies they deployed during critical phases; analysis of current East Asian and Indian industrial policy within WTO constraints',
    'If alternative pathways are sufficient: constraint is primarily coordination (epsilon 0.30–0.45, rope or scaffold). If protected sectors are empirically necessary: constraint forecloses alternative development models (epsilon 0.60+, snare from developing-state perspective; supports the developmental reading''s core claim).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_development_pathways_foreclosed, empirical, 'Whether WTO restrictions foreclose empirically necessary development policies').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the market access reading logically foreclose the developmental reading within a single commitment framework, or do both readings coexist as live positions held by different WTO member states?',
    'Analysis of treaty text ambiguities (GATT Article XVIII infant-industry exceptions, S&D language, technology transfer aspirations); examination of state negotiation positions and dispute settlement history; assessment of whether a single state can coherently hold both readings simultaneously',
    'If foreclosure is real (core premises directly contradict): reading_relations should be ''forecloses''; omega documents logical incompatibility. If coexistence is possible (different states, different commitments): reading_relations should be ''coexists_with''; omega documents the ambiguity. Critical for mandatrophy resolution: if readings foreclose each other, the kernel has bifurcated; if they coexist, the constraint family exhibits genuine perspectival multiplicity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Whether market-access and developmental readings logically foreclose each other or coexist').

omega_variable(
    asymmetric_baseline_starting_conditions,
    'Does the market access reading''s claim of ''symmetric universal obligation'' accurately describe the treaty, given that developed states industrialized under high tariffs and subsidies before binding commitments were made, while developing states face bindings from the outset?',
    'Historical analysis of tariff levels and development trajectories in now-developed states during 19th-20th centuries vs. post-1995 WTO schedules for developing states; comparison of effective rates of protection when developed states were at similar income levels to current developing states',
    'If baseline asymmetries are severe: constraint should be reclassified as asymmetric extraction (forecloses genuine symmetry claim; supports developmental reading). If baselines were comparable: market access reading is defensible as symmetric framework despite distributional consequences. Affects cs_structure.axioms: is ''symmetric_universal_obligation'' holdable or overridden by historical fact?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetric_baseline_starting_conditions, empirical, 'Whether WTO framework is truly symmetric given historical disparities in baseline conditions').

omega_variable(
    reading_grounding_type_contingency,
    'Is the market access reading grounded primarily in empirical comparative advantage theory (falsifiable) or in deontological commitments to non-discrimination and equality before law (not falsifiable)?',
    'Textual analysis of treaty preambles and negotiation history; examination of dispute settlement rulings to determine whether DSB applies comparative advantage logic or non-discrimination principles as primary rationale; analysis of how developed states justify restrictions that appear to violate comparative advantage (e.g., agricultural subsidies, antidumping)',
    'If empirically grounded: comparative advantage evidence (learning curve effects, externalities, path dependence) can challenge the reading; epsilon sensitivity to empirical falsification is high. If deontologically grounded: the reading is not empirically falsifiable; grounding_type should be ''deontological'' in cs_structure.axioms, and the constraint persists regardless of comparative advantage evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_grounding_type_contingency, conceptual, 'Whether market access reading is grounded in empirical vs. deontological commitments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_ma_theater_1995, wto_treaty_framework__market_access_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(wto_ma_theater_2010, wto_treaty_framework__market_access_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(wto_ma_theater_2025, wto_treaty_framework__market_access_reading, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(wto_ma_extract_1995, wto_treaty_framework__market_access_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(wto_ma_extract_2010, wto_treaty_framework__market_access_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(wto_ma_extract_2025, wto_treaty_framework__market_access_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(wto_ma_suppress_1995, wto_treaty_framework__market_access_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(wto_ma_suppress_2010, wto_treaty_framework__market_access_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(wto_ma_suppress_2025, wto_treaty_framework__market_access_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, resource_allocation).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_treaty_framework__developmental_reading).

% DUAL FORMULATION NOTE:
% The market access reading and developmental reading are two interpretations of the same WTO kernel. They differ in epsilon (0.58 vs 0.38), in classification (snare vs tangled rope from developing-state perspectives), and in the distribution of beneficiaries and victims. Both are structurally valid readings of the treaty text; their coexistence is a feature of kernel ambiguity, not a measurement error. The network edge is bidirectional: each reading influences the other's institutional viability through dispute settlement precedent and negotiation dynamics. The market access reading currently dominates DSM interpretation, which influences the developmental reading's practical effectiveness (S&D provisions are narrowly construed, making the development accommodation weaker). If the reading balance shifted, the market access reading's practical scope would narrow.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, institutional, 0.15).
constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
