% ============================================================================
% CONSTRAINT STORY: denominational_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_denominational_fragmentation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: denominational_fragmentation
 *   human_readable: Denominational Fragmentation in the Protestant Reformation (1517-1648)
 *   domain: historical_sociology/religious_studies/political_economy
 *
 * SUMMARY:
 *   The Protestant Reformation (1517-1648) represents a historical composite
 *   exhibiting simultaneous coordinate equilibration, extraction
 *   accumulation, performative rise-and-fall, and victim diversification
 *   across multiple independent causal chains. A theological dispute
 *   (Luther's indexical critique of indulgence extraction) converges with
 *   political opportunity (territorial princes claiming church property
 *   sovereignty), technological shock (printing enabling vernacular scripture
 *   dissemination), and merchant class market segmentation (confessional
 *   divisions enabling niche trading networks). The constraint story models
 *   denominational fragmentation as a tangled coordination-extraction hybrid:
 *   peasants and women experience suppression via forced confessional
 *   conscription; princes benefit from jurisdictional authority reclamation;
 *   merchants profit from segmented devotional markets; theological
 *   institutions establish autonomous authority structures; yet the Roman
 *   papacy degrades into performative ritual while maintaining symbolic
 *   claims. The event is robustly overdetermined — no single causal chain is
 *   necessary, yet all converge to produce the historical outcome. The
 *   fragmentation cannot be reduced to theological indexicality, political
 *   competition, technological possibility, or economic incentive alone; the
 *   structure emerges from their simultaneous operation.
 *
 * KEY AGENTS:
 *   - Martin Luther and theological dissenters: Agents producing indexical critique (different listeners derive different meanings from scripture) — institutional power agents with analytical exit options, but constrained by church authority
 *   - Territorial Princes: Primary beneficiaries (organized/constrained) — gain church property, clerical appointment authority, and coalition-building opportunities from fragmented Christendom
 *   - Peasant Communities: Primary victims (powerless/trapped) — conscripted into whichever denomination their territorial lord adopted; experience suppression through violence (Peasants' War reprisals, Anabaptist persecution)
 *   - Female Devotional Communities: Secondary victims (powerless/trapped) — autonomous spirituality (Beguines, anchoresses, mystics) systematically absorbed or forbidden under denominational hierarchies; trapped in male-supervised structures
 *   - Merchant/Trading Classes: Secondary beneficiaries (powerful/mobile) — profit from confessional market segmentation, vernacular devotional print culture, territorial trading network advantages
 *   - Jewish Communities: Tertiary victims (powerless/trapped) — scapegoating intensification during denominational competition; increased pogroms and forced relocations
 *   - Reformed/Lutheran Institutional Structures: Institutional beneficiaries (institutional/arbitrage) — solve legitimacy coordination problem; institutional survival depends on perpetuating denominational boundaries
 *   - Roman Papacy: Institutional victim (institutional/arbitrage at piton level) — loses universal jurisdiction; maintains performative authority ritual despite functional impotence
 *   - Analytical Observer: Structural interpreter — models overdetermination and perspectival divergence across convergent causal chains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(denominational_fragmentation, 0.58).
domain_priors:suppression_score(denominational_fragmentation, 0.72).
domain_priors:theater_ratio(denominational_fragmentation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(denominational_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(denominational_fragmentation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(denominational_fragmentation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(denominational_fragmentation, tangled_rope).
narrative_ontology:human_readable(denominational_fragmentation, "Denominational Fragmentation in the Protestant Reformation (1517-1648)").
narrative_ontology:topic_domain(denominational_fragmentation, "historical_sociology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(denominational_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(denominational_fragmentation, territorial_princes).
narrative_ontology:constraint_beneficiary(denominational_fragmentation, merchant_trading_classes).
narrative_ontology:constraint_beneficiary(denominational_fragmentation, literacy_enabled_classes).
narrative_ontology:constraint_victim(denominational_fragmentation, universal_christendom_ideal).
narrative_ontology:constraint_victim(denominational_fragmentation, peasant_constituencies).
narrative_ontology:constraint_victim(denominational_fragmentation, female_devotional_autonomy).
narrative_ontology:constraint_victim(denominational_fragmentation, jewish_scapegoat_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(denominational_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(denominational_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(denominational_fragmentation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(denominational_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(denominational_fragmentation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

constraint_indexing:constraint_classification(denominational_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

constraint_indexing:constraint_classification(denominational_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(denominational_fragmentation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(denominational_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(denominational_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(denominational_fragmentation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(denominational_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(denominational_fragmentation, TR),
    TR >= 0.70.

:- end_tests(denominational_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint exhibits significant extraction from powerless populations (peasant conscription, female autonomy suppression, intensified Jewish persecution) while enabling coordination benefits for institutional actors (princes gain jurisdictional authority, denominations create autonomous institutional forms, merchants profit from market segmentation). The 0.35 → 0.58 trajectory reflects extraction accumulation: early fragmentation (1517) involved genuine theological dispute with uncertain outcomes; by 1532 (German Peasants' War aftermath), extraction mechanisms had solidified into mandatory confessional conscription with violent enforcement; by 1582 (post-Tridentine consolidation), extraction persisted at higher level through institutionalized control mechanisms. The slight decline by 1648 (Peace of Westphalia) reflects that territorial consolidation enabled some actors (princes, merchants) to stabilize expectations, reducing friction-based extraction. Suppression (0.72): High. Powerful structural barriers to exit: territorial conscription (princes require subjects to adopt territorial confession), legal penalties for dissent, violent enforcement (wars, pogroms, executions), economic dependency on confessionally-controlled institutions (land, trade networks, educational access). Suppression mechanisms differ by victim group: peasants face military violence and serfdom lock; women face institutional exclusion; Jews face pogroms; minority confession members face legal disability. Theater ratio (0.25 → 0.55 → 0.48): Rising then slightly declining. Early Reformation (1517-1532) emphasizes theological disputation and scriptural exegesis (low theater, high function). Mid-Reformation (1532-1582) exhibits increasing ritual formalization and catechism standardization as denominations institutionalize (theater rises as competition drives performative differentiation). Late Reformation (1582-1648) shows theater remaining high during Thirty Years War but declining post-Westphalia as institutions stabilize and devotional practice becomes routine rather than polemical. The peak at 1582 reflects Counter-Reformation theatrical intensification: Baroque Catholicism and Tridentine ritual elaboration respond to Protestant institutional challenge with maximum spectacle.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Territorial princes experience tangled rope (coordination function + asymmetric extraction in their favor). Reformed/Lutheran institutions experience rope (pure coordination of doctrinal legitimacy and institutional authority). Merchants experience tangled rope (coordination of market segmentation + profit extraction from devotional consumption). Peasants experience snare (pure extraction through forced conscription + violent suppression). Women experience snare (pure extraction through institutional exclusion + devotional autonomy suppression). Jewish communities experience snare (pure extraction through intensified persecution + scapegoating). The Roman papacy experiences piton (degraded institutional authority maintained through theater). The analytical observer experiences tangled rope at universal scope (genuine coordination functions embedded in extraction mechanisms; perspectival gaps are not measurement errors but structural features of how different agents relate to the same constraint). The gap is not reducible: there is no observer position from which all agents experience the same classification. The constraint's structure is fundamentally indexical — the classification you perceive depends on your structural position within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural relationship to extraction flow. Territorial princes occupy beneficiary position with mobile exit (can defect to competing denomination, can negotiate with other princes, can sponsor rival reformation) — derived d ≈ 0.35. Reformed/Lutheran institutions occupy pure beneficiary position with arbitrage exit (institutional survival depends on confessional perpetuation) — derived d ≈ 0.10. Merchants occupy beneficiary position with mobile/arbitrage hybrid (can operate across confessional boundaries, profit from segmentation, but also benefit from institutional stability) — derived d ≈ 0.25. Peasants occupy pure victim position with trapped exit (conscripted by territorial lord's confession, no legal exit, violent consequences for defection) — derived d ≈ 0.92. Women occupy pure victim position with identity_locked exit (autonomy requires breaking from confessionally-supervised structures, but identity is constituted through devotional practice itself — exit would require becoming a different person) — derived d ≈ 0.85. Jewish communities occupy pure victim position with trapped exit (persecution increases regardless of compliance; targeted regardless of doctrinal alignment) — derived d ≈ 0.96. Roman papacy occupies mixed position: institutional actor with arbitrage-level exit options, but functionally captured by jurisdictional loss (can theoretically exit papal claims, but cannot do so without institutional dissolution) — derived d ≈ 0.55 at piton level. These d values generate chi (effective extractiveness) via sigmoid function f(d) × ε × σ(S). Peasants and Jewish communities experience maximum chi due to high d, trapped exit, and regional/local scope intensity. Princes and merchants experience lower chi due to lower d and mobile exit. The analytical observer's d derives from analytical power atom and analytical exit — no structural position within the constraint, allowing calculation of constraint's chi at civilizational scope without observer bias.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: The constraint exhibits potential confusion between genuine coordination function and extractive overlay. Mandatrophy resolution shows they are not alternatives but simultaneous features. Territorial princes genuinely solve a coordination problem (how to allocate church jurisdiction without papal monopoly) while simultaneously extracting from peasants (conscription, property seizure, wealth concentration). Reformed institutions genuinely solve another coordination problem (how to ground doctrinal authority without papal arbitration) while simultaneously enabling extraction (institutional hierarchies that suppress female autonomy, minority persecution, intellectual conformity). Merchants genuinely coordinate market segmentation while extracting monopoly rents on devotional goods. The fragmentation is NOT 'really' coordination with extraction as side effect, NOR 'really' extraction disguised as coordination. Both functions are primary. The mandatrophy resolves by recognizing that tangled rope specifically models this hybrid: ε ≥ 0.30 (genuine coordination exists), requires_active_enforcement: true (extraction requires institutional maintenance), beneficiaries AND victims both declared (different agents benefit and suffer). The constraint is not coordinate-disguised-as-snare, nor snare-disguised-as-coordinate. It is structurally both, and perspectival classification depends on which function dominates the observer's experience of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contestation_structure,
    'Is denominational fragmentation a single kernel with multiple readings (schism, reform, revolution), or structurally distinct constraints masquerading as a unified event?',
    'Formal constraint decomposition: separate the theological-indexical constraint (Luther''s critique of indulgences extraction) from the political constraint (princes claiming church property) from the technological constraint (printing enabling dissemination) from the economic constraint (merchant class exploiting market fragmentation). Test whether each maintains structural coherence as independent constraint with its own ε, or whether they collapse under isolation.',
    'If single kernel with multiple readings: manifestations vary by position (princes see political opportunity, peasants see conscription, theologians see doctrinal vindication) but all instantiate same constraint. If distinct constraints: fragmentation is overdetermined composite event; no single narrative explains it. Classification changes from tangled_rope (hybrid constraint) to network of six independent constraints with different types.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contestation_structure, conceptual, 'Whether fragmentation is a single kernel with contested readings or multiple independent constraints').

omega_variable(
    suppression_mechanism_internalization,
    'Is religious suppression (forced confession allegiance) structural (territorial coercion, legal penalties) or internalized (confessional identity becomes self-enforcing)?',
    'Post-fragmentation trajectory analysis: When territorial conscription enforcers (princes, armies) were removed (Peace of Westphalia treaties, imperial collapse), did denominational loyalty persist at rates exceeding economic incentives? Cross-generational identity persistence in confessionally minority populations.',
    'If structural: suppression is external and removable; escape strategy is migration/defection. If internalized: suppression persists as identity lock even after external enforcement collapses. For powerless agents, this determines whether exit_options should be ''trapped'' (external barriers) or ''identity_locked'' (cognitive capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural coercion or internalized identity commitment').

omega_variable(
    overdetermination_integration,
    'When multiple independent causal chains (theological doctrine, political competition, technological innovation, merchant profit) converge to produce a single outcome, can a single constraint story capture the structure, or does integration fail at the formalization stage?',
    'Test whether a single (P,T,E,S) tuple can generate perspectives that capture all convergent causation, or whether the perspectival gaps require decomposition into separate constraint stories. Specifically: can the ''analytical observer'' perspective model the causal convergence without collapsing distinct mechanisms into undifferentiated ''overdetermination''?',
    'If integration succeeds: the constraint framework handles historically overdetermined events as single structures with multiple entry points. If it fails: historical composites require network decomposition (multiple stories linked via affects_constraints). Current formulation assumes integration succeeds; resolution determines whether future schemas need overdetermination gates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_integration, conceptual, 'Whether single constraint can integrate multiple independent causal chains').

omega_variable(
    female_autonomy_suppression_mechanism,
    'Was female devotional autonomy suppressed through new denominational control mechanisms, or did it persist in hidden/privatized forms despite official exclusion?',
    'Archival analysis of clandestine female-led prayer groups, underground mystic circles, and domestic spirituality practices in denominationally controlled regions. Comparison of suppression intensity across Catholic territorial enforcement vs Protestant pastoral surveillance (different enforcement mechanisms).',
    'If suppression was total: female communities experienced maximum extraction (snare classification for that population). If suppression was incomplete (underground persistence): extraction was lower and exit options more mobile than captured. Affects classification of gender-segmented victim experience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_autonomy_suppression_mechanism, empirical, 'Extent of female devotional autonomy suppression vs. hidden persistence').

omega_variable(
    jewish_scapegoating_causal_necessity,
    'Was intensified persecution of Jewish communities a causal consequence of fragmentation, or a contingent exploitation of fragmented authority that would have occurred regardless?',
    'Counterfactual analysis: construct models of religious competition dynamics with and without Jewish scapegoating. Examine whether denominational competition increased anti-Jewish violence above baseline medieval rates, or merely provided new rhetorical vehicles for existing prejudice.',
    'If necessary: fragmentation structurally produces scapegoating (victim set is endogenous to constraint). If contingent: Jewish persecution is an independent constraint riding the fragmentation wave (separate story). Determines whether Jewish communities should be classified as victims of denominational_fragmentation or victims of distinct antisemitism_institutional_scapegoating constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jewish_scapegoating_causal_necessity, conceptual, 'Whether Jewish persecution is structurally necessary to fragmentation or contingent consequence').

omega_variable(
    peasant_war_revolutionary_reading,
    'Did the German Peasants'' War (1524-1526) represent a reading of the Reformation constraint (using fragmented authority to organize rebellion) or emergence of a structurally distinct constraint (peasant uprising against seigneurial extraction)?',
    'Textual analysis of peasant demands (Twelve Articles) relative to Reformation doctrinal positions. Test whether peasants instrumentalized fragmentation as tactical opportunity, or whether peasant uprising was independent movement that coincided with fragmentation.',
    'If reading of Reformation: peasants had mobile exit options (could defect to radical sects, could invoke Lutheran authority to challenge lords) — classification would be tangled_rope (mixed extraction/coordination for organizing class). If independent constraint: peasants experienced pure snare in seigneurial extraction, and Reformation was background event. Affects whether constraint should include peasant warfare data in measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasant_war_revolutionary_reading, conceptual, 'Whether peasant uprising is reading of Reformation constraint or independent constraint').

omega_variable(
    merchant_class_arbitrage_quantification,
    'What percentage of merchant profit from post-Reformation period is attributable to confessional market segmentation vs. general expansion of trade networks?',
    'Economic history analysis: compare merchant house growth trajectories before and after confessional consolidation; examine whether merchant networks organizing along denominational lines showed higher profitability than non-confessional competitors; test correlation between religious diversity and trade volume in specific cities (Amsterdam vs Venice post-Reformation).',
    'High arbitrage attribution: merchants experienced fragmentation as significant extraction-reduction (mobile exit options, negotiable loyalty). Low attribution: merchant benefit was spillover, not structural cause. Affects directionality of merchant class relative to constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merchant_class_arbitrage_quantification, empirical, 'Proportion of merchant profit attributable to confessional market segmentation').

omega_variable(
    reformation_indexicality_authenticity,
    'Luther''s doctrine of scriptural authority and priesthood of all believers — does this represent genuine philosophical indexicality (the meaning of scripture is listener-relative), or is ''indexicality'' a retrospective interpretive frame imposed on theological dispute?',
    'Philological analysis: examine whether Luther and his immediate interlocutors deployed indexical reasoning (different readers have legitimate different interpretations based on their position), or whether they claimed univocal scripture meaning discoverable by proper method. Map dispute to contemporary philosophical logic of indexicality.',
    'If genuine indexicality: Reformation is structurally about listener-relative meaning-making; fragmentation follows necessarily from indexical logic. If retrospective frame: ''indexicality'' is observer category, not agent''s own category. Affects whether constraint is naturalized (indexical logic is unavoidable) or contingent (historical disagreement about scripture that could have been reconciled).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformation_indexicality_authenticity, conceptual, 'Whether Reformation theology instantiates indexicality or is reframed as indexical retrospectively').

omega_variable(
    theater_ratio_decline_over_interval,
    'Does the theater ratio decline across the interval (1517-1648) as denominational institutions mature and rituals become functionally embedded, or does it rise as competition intensifies performative differentiation?',
    'Archival analysis of devotional practice formalization: compare early Reformation spontaneous preaching/disputation (high function, low theater) to mid-17th-century standardized liturgies and catechism enforcement (institutionalized theater). Measure ratio of exegetical/doctrinal output to ritual/ceremonial output across decades.',
    'Declining theater: fragmentation is initially functional coordination problem (agents building new institutions) maturing into stable mechanism. Rising theater: fragmentation generates performative escalation as denominations compete for loyalty. Affects whether constraint is scaffold (with sunset as functions stabilize) or tangled_rope (extraction persists as theater rises).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_decline_over_interval, empirical, 'Trajectory of theater ratio (performative to functional activity) across 1517-1648').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(denominational_fragmentation, 0, 131).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(denom_frag_theater_1517, denominational_fragmentation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(denom_frag_theater_1532, denominational_fragmentation, theater_ratio, 15, 0.38).
narrative_ontology:measurement(denom_frag_theater_1582, denominational_fragmentation, theater_ratio, 65, 0.55).
narrative_ontology:measurement(denom_frag_theater_1648, denominational_fragmentation, theater_ratio, 131, 0.48).

% Extraction over time
narrative_ontology:measurement(denom_frag_extract_1517, denominational_fragmentation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(denom_frag_extract_1532, denominational_fragmentation, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(denom_frag_extract_1582, denominational_fragmentation, base_extractiveness, 65, 0.62).
narrative_ontology:measurement(denom_frag_extract_1648, denominational_fragmentation, base_extractiveness, 131, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(denominational_fragmentation, enforcement_mechanism).
narrative_ontology:affects_constraint(denominational_fragmentation, indulgence_extraction_system).
narrative_ontology:affects_constraint(denominational_fragmentation, peasant_serfdom_consciousness).
narrative_ontology:affects_constraint(denominational_fragmentation, jewish_institutional_scapegoating).
narrative_ontology:affects_constraint(denominational_fragmentation, female_mysticism_suppression).
narrative_ontology:affects_constraint(denominational_fragmentation, print_culture_authority_democratization).

% DUAL FORMULATION NOTE:
% Denominational fragmentation is downstream of multiple independent upstream constraints (theological indexicality critique, territorial political competition, merchant class economic incentive, technological printing innovation) and affects multiple independent downstream constraints (institutional formation of denominations, peasant consciousness development, intensification of persecution mechanisms). The network structure reflects overdetermination: no single upstream constraint is necessary for fragmentation to occur, but all converge to produce it. Downstream effects are similarly multiple and independent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(denominational_fragmentation, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
