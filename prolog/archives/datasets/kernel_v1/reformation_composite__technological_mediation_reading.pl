% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_technological_mediation, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Printing Press as Physical Constraint Enabling Reformation Scale (Technological Mediation Reading)
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   The technological mediation reading frames the Reformation as
 *   fundamentally enabled by printing press technology. This reading argues
 *   that local theological dissent (which existed for centuries before
 *   Luther) became a continental mass movement only because printing
 *   technology transformed the distribution of religious texts and ideas from
 *   manuscript culture (slow, expensive, geographically limited) to print
 *   culture (rapid, economical, networkable across European cities). The
 *   printing press creates an irreducible physical constraint: information
 *   density increases exponentially, institutional control mechanisms
 *   designed for manuscript scarcity become structurally obsolete, and the
 *   speed of textual dissemination outpaces pre-print institutional
 *   responses. This is a mountain constraint — not because the Reformation
 *   was inevitable, but because the transition from pre-print to print
 *   distribution operates at the level of information physics, not theology
 *   or politics. The contested kernel is the Reformation itself: what *is*
 *   the Reformation? Is it fundamentally a theological event (competing
 *   soteriological commitments), a political event (nation-states asserting
 *   sovereignty), or a technological event (printing enabling continental
 *   scale)? This constraint story instantiates the technological reading as a
 *   clean ε-invariant claim.
 *
 * KEY AGENTS:
 *   - Literate Urban Merchant Class: Beneficiary (institutional/arbitrage) — captures economic value from printing networks and text distribution; benefits from information access
 *   - Printing Industry (Gutenberg, early printers): Primary beneficiary (institutional/arbitrage) — owns capital, controls production, captures most direct economic rent from printing technology
 *   - Protestant Reformers (Luther, Calvin, et al.): Beneficiary but constrained (organized/constrained) — use printing to achieve continental scale but do not control the technology; subject to printer profitability, paper supply, capital requirements
 *   - Illiterate Rural Peasantry: Victim (powerless/trapped) — cannot exit the constraint of non-literacy; excluded from direct participation in printed text distribution; dependent on oral/sermon transmission
 *   - Catholic Church Authority: Victim (institutional/analytical) — faces structural constraint of pre-print institutional mechanisms becoming obsolete in face of print distribution; cannot suppress information at continental scale through traditional manuscript control
 *   - Analytical Observer: Neutral (analytical/analytical) — sees printing technology as an irreducible physical constraint that enables the historical transformation but does not determine its theological or political content
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.08).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.02).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Printing Press as Physical Constraint Enabling Reformation Scale (Technological Mediation Reading)").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, 'be1b2385-cf99-4177-9f02-ca8ede2496e8').
narrative_ontology:cs_kernel_codification('be1b2385-cf99-4177-9f02-ca8ede2496e8', distributed).
narrative_ontology:cs_authority_grounding('be1b2385-cf99-4177-9f02-ca8ede2496e8', diffuse_epistemic).
narrative_ontology:cs_reading_relation('be1b2385-cf99-4177-9f02-ca8ede2496e8', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('be1b2385-cf99-4177-9f02-ca8ede2496e8', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('be1b2385-cf99-4177-9f02-ca8ede2496e8', foundational, printing_as_necessary_infrastructure).
narrative_ontology:cs_axiom_status(printing_as_necessary_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('be1b2385-cf99-4177-9f02-ca8ede2496e8', printing_as_necessary_infrastructure, empirically_contingent).
narrative_ontology:cs_axiom('be1b2385-cf99-4177-9f02-ca8ede2496e8', secondary, technology_enables_not_determines).
narrative_ontology:cs_axiom_status(technology_enables_not_determines, holdable).
narrative_ontology:cs_axiom_grounding('be1b2385-cf99-4177-9f02-ca8ede2496e8', technology_enables_not_determines, conventional).
narrative_ontology:cs_reference_frame('be1b2385-cf99-4177-9f02-ca8ede2496e8', manuscript_culture_epistemic_dominance).
narrative_ontology:cs_drift_state('be1b2385-cf99-4177-9f02-ca8ede2496e8', early_sixteenth_century_reformation_onset, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('be1b2385-cf99-4177-9f02-ca8ede2496e8', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_urban_merchant_class).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, printing_industry).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, protestant_reformers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ILLITERATE RURAL PEASANTRY (MOUNTAIN) — Cannot exit the constraint of oral-only religious transmission. The printing press enables a continental movement that reaches cities and literate elites, but the structure of non-literacy is unaffected by printing technology. From this position, the constraint appears immutable: religious knowledge remains local, oral, and mediated by clergy regardless of printing press availability. The mountain is the accessibility collapse of literacy itself — a physical barrier to participation in the technological transformation.
constraint_indexing:constraint_classification(reformation_composite__technological_mediation_reading, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER / TECHNOLOGICAL DETERMINISM (MOUNTAIN) — From a universal/civilizational perspective, printing technology creates an irreducible physical constraint on information distribution. The transition from manuscript to print has intrinsic limits: printing presses require capital investment, distribution networks, paper supply chains, and literacy. These are not social arrangements or power structures that could be otherwise — they are technological facts. The printing press enables mass production of texts (a mountain for distribution capacity), but does not create literacy, does not determine what gets printed, and does not control how texts are received. The mountain classification here identifies the technological substrate as a necessary-but-not-sufficient condition for the continental movement.
constraint_indexing:constraint_classification(reformation_composite__technological_mediation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: PRINTING INDUSTRY / MERCHANT CLASS (ROPE) — From this institutional position, the constraint is pure coordination: establishing printing networks, distribution channels, paper supply, and urban literacy is a genuine coordination problem that benefits all participants. Printers, merchants, and urban elites experience the printing technology as solving a coordination problem (how to distribute ideas at scale) with minimal coercive overhead. The beneficiary experiences the technological constraint as enabling their work, not restricting it. This perspective sees the printing press as a Rope: it coordinates economic activity without asymmetric extraction.
constraint_indexing:constraint_classification(reformation_composite__technological_mediation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: CATHOLIC CHURCH AUTHORITY (MOUNTAIN) — From the institutional perspective of ecclesiastical authority grounded in manuscript culture, printing is an immutable technological fact that cannot be controlled or suppressed at scale. The Church cannot un-invent printing, cannot prevent distribution, cannot manage information density at continental scope using pre-print institutional mechanisms (manuscript copying, sermon delivery, oral transmission). The mountain here is the structural incompatibility between pre-print authority mechanisms and print-distributed challenges. The constraint is not the printing press itself but the irreversible transition from a system where religious texts could be filtered by clergy to one where texts reach readers directly.
constraint_indexing:constraint_classification(reformation_composite__technological_mediation_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: PROTESTANT REFORMERS (ROPE) — From the organized perspective of reform movements, the printing press is a coordination mechanism enabling a distributed challenge to institutional authority. Reformers use printing to coordinate across cities, share theological arguments, and mobilize literate publics. The constraint is experienced as enabling (coordination) with some costs (censorship, institutional opposition, capital requirements for printing). The classification is Rope because the coordination benefit dominates — printing makes a dispersed movement continental rather than local. Effective extraction is low relative to coordination value.
constraint_indexing:constraint_classification(reformation_composite__technological_mediation_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION THERMODYNAMICS (MOUNTAIN) — From the perspective of information physics, printing creates an irreversible thermodynamic transition: it increases the entropy of textual dissemination from a closed (manuscript) to an open (print) state. The printing press makes information density exponentially higher and control exponentially harder. This is not a policy choice or institutional arrangement — it is a physical fact about how information systems behave. No authority could prevent this transition through regulation alone; pre-print institutional authority mechanisms become structurally obsolete. The mountain is the one-directional flow of information complexity.
constraint_indexing:constraint_classification(reformation_composite__technological_mediation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reformation_composite__technological_mediation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reformation_composite__technological_mediation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(reformation_composite__technological_mediation_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reformation_composite__technological_mediation_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The printing press as a technological constraint operates at the level of information distribution physics, not social extraction. The technological constraint itself does not extract from most agents — it enables new possibilities while closing pre-print institutional mechanisms. Some actors benefit (printers, urban elites with literacy), but the extractiveness value reflects that the primary effect is capacity expansion, not redistribution. Suppression (0.02): Minimal. The printing technology creates possibilities rather than suppressing alternatives; manuscript culture continues in parallel but loses institutional dominance. No active coercive mechanism maintains the printing constraint — it is sustained by economic incentives (printers make money, texts reach wider audiences) and technological advantages (printing is faster and cheaper than manuscript copying). Accessibility Collapse (0.92): Very high. The printing press creates a sharp discontinuity in access to textual information: before printing, texts are expensive, geographically limited, clergy-mediated; after printing, texts proliferate and reach urban centers rapidly. The collapse in accessibility is precisely the mountain signature — a structural phase transition that cannot be reversed or undone through policy. Resistance (0.08): Very low. No significant resistance to the technology itself emerges; the resistance that does emerge (Church censorship, royal regulations) operates at the level of what gets printed, not whether printing happens. Theater Ratio (0.15): Very low. The printing press's functional role is transparent and mechanical — it distributes texts at scale. The performative content is minimal relative to the actual information capacity gain. From this reading's perspective, printing does what it claims to do.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals how the same technological constraint appears as liberatory, oppressive, or neutral depending on position. Urban literate elites and printers see the printing press as enabling coordinate (Rope) — it solves the problem of distributing ideas at scale. Illiterate peasants see it as immutable and exclusionary (Mountain) — it creates a new barrier (literacy) while distributing information in a form they cannot access. The Church sees it as a structural threat to pre-print authority mechanisms (Mountain) — not because printing itself is evil, but because pre-print institutional controls become obsolete. Reformers see it as a coordination mechanism (Rope) that enables their distributed movement. The analytical observer at civilizational scale sees it as a thermodynamic transition in information systems (Mountain) — irreversible, determined by physics rather than choice. All perspectives agree it is a mountain in the sense that it creates an immutable phase transition. But they disagree on which agent bears the immutability and which agent experiences it as enabling.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values for this technological constraint derive from structural position relative to printing technology adoption and literacy. Printers and urban merchants: d ≈ 0.05 (beneficiaries with high exit options via arbitrage; derive capital returns from printing). Literate reformers: d ≈ 0.20 (partial beneficiaries; constrained by capital requirements and printer decisions; can use printing but do not control it). Illiterate peasants: d ≈ 0.88 (excluded from direct participation; face literacy barrier; cannot arbitrage the technology). Church authority: d ≈ 0.75 (victim of obsolescence of pre-print control mechanisms; cannot prevent printing through traditional institutional means; face structural constraint rather than negotiable distribution). The sigmoid f(d) transforms these d values into experienced extractiveness chi, with high-d agents (trapped, excluded) experiencing the constraint's effects most acutely, and low-d agents (beneficiaries with options) experiencing minimal or negative extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this constraint is resolved by recognizing that technological, theological, and political readings address different structural questions about the same historical event. The technological reading does NOT claim that theology and politics are unimportant — it claims that the distribution mechanism (printing) is the constraint that made continental scale possible. Without printing, local theological dissent could exist and political conflicts could occur, but the movement could not achieve continental coordination. The reading is not 'technology caused the Reformation' but 'technology enabled the Reformation to achieve continental scale.' The mandatrophy dissolves when we recognize that the readings are orthogonal: theology determines content, politics determines resource allocation and state support, technology determines speed and scale of distribution. All three are true. The false summit danger is in naturalizing printing as 'inevitable progress' rather than a contingent technological adoption with winners and losers. The omega variables document this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    printing_necessity_vs_sufficiency,
    'Is printing a necessary condition for the continental Reformation movement, or merely a sufficient condition that accelerated pre-existing theological and political pressures?',
    'Counterfactual historical analysis: regions without printing presses (Ottoman Empire, Eastern Orthodox territories); pre-Reformation schisms and movements without printing (Lollards, Waldensians, Hussite challenge); correlation between printing adoption timeline and movement scale in different regions; analysis of whether regional variations in printing density predict movement intensity',
    'If necessary: the technological reading is correct — no printing means no continental movement. If merely sufficient: the theological and political readings are foundational, printing is instrumental. If neither: the constraint is misclassified and the technological mediation is secondary to social structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printing_necessity_vs_sufficiency, empirical, 'Whether printing is necessary or merely sufficient for continental Reformation').

omega_variable(
    alternative_distribution_mechanisms,
    'Could other technologies (faster travel, courier networks, architectural innovation enabling mass gatherings) have achieved similar continental scale and speed without printing press?',
    'Analysis of travel times and communication latency in pre-print networks vs print networks; modeling of manuscript distribution capacity vs print capacity; examination of non-print movements that did achieve continental scale (e.g., crusades, pilgrimages); evaluation of whether printing vs travel/courier technologies were the binding constraint',
    'If printing was the binding constraint: technological mediation reading is correct. If travel/courier/infrastructure was the binding constraint: printing is one component in a larger technological ecosystem, not the primary constraint. If multiple constraints were equally binding: the mountain classification is wrong — the constraint is fragmented across multiple technologies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_distribution_mechanisms, empirical, 'Whether printing or other transport/communication technologies were the binding constraint').

omega_variable(
    sibling_reading_committer_ambiguity,
    'This constraint is ONE reading of the reformation_composite kernel. How do the theological_fragmentation_reading and political_realignment_reading relate to this technological_mediation_reading within a single authoritative framework?',
    'Institutional archaeology: which authority (church, state, academic tradition) adjudicates the Reformation''s cause? Can that authority hold all three readings simultaneously, or do they logically foreclose each other? Analysis of whether Reformation historiography treats these as complementary dimensions or mutually exclusive claims.',
    'If framings coexist: all three readings are valid perspectives on a single kernel. If one forecloses others: this reading''s axiomatic foundation contradicts a sibling''s, and the kernel exhibits genuine logical conflict. If the readings are influenced but not foreclosed: the technological reading is upstream, enabling the theological and political dimensions to manifest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_committer_ambiguity, conceptual, 'Relationship between this technological reading and sibling theological/political readings within a single historical framework').

omega_variable(
    literacy_as_derived_vs_primary_constraint,
    'Is literacy (urban, merchant, clerical) a primary physical constraint on printing''s impact, or a derivative social arrangement that printing itself creates?',
    'Timeline analysis: literacy rates pre-printing vs post-printing; regions with high literacy but limited printing adoption vs low literacy with high printing adoption; examination of whether printing created demand for literacy or responded to existing literacy demand',
    'If literacy is primary: the constraint is actually (literacy + printing), not printing alone. The mountain classification remains valid but the mechanism is more complex. If printing created literacy demand: the printing press is even more powerful as a constraint — it bootstraps the system it depends on. If neither: the relationship is more reciprocal and less constraining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_as_derived_vs_primary_constraint, empirical, 'Whether literacy is a constraint on printing or a consequence of printing').

omega_variable(
    false_summit_natural_law_candidate,
    'Is printing a genuine natural law constraint (immutable technological limit), or a constructed constraint that benefits identifiable actors (printers, urban elites, reformers) and naturalizes contingent institutional arrangements?',
    'Analysis of suppression mechanisms: was printing suppressed by institutional authorities (creating an extractive constraint), or was it freely adopted and distributed? Examination of whether pre-print oral/manuscript culture was structurally superior to print or merely different. Evaluation of whether the ''naturalness'' of printing as a constraint is an artifact of its triumph rather than an intrinsic property.',
    'If genuine natural law: mountain classification holds. If false summit: the constraint should be reclassified as tangled_rope or snare — printing as a technology benefits specific actors who present it as inevitable. The theological and political readings become primary, technology becomes secondary justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, conceptual, 'Whether printing is a natural law or a false-summit naturalization of contingent institutional change').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1440, 1517).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reform_tech_theater_1440_gutenberg, reformation_composite__technological_mediation_reading, theater_ratio, 1440, 0.08).
narrative_ontology:measurement(reform_tech_theater_1480_early_adoption, reformation_composite__technological_mediation_reading, theater_ratio, 1480, 0.12).
narrative_ontology:measurement(reform_tech_theater_1517_reformation_onset, reformation_composite__technological_mediation_reading, theater_ratio, 1517, 0.15).

% Extraction over time
narrative_ontology:measurement(reform_tech_extractiveness_1440_gutenberg, reformation_composite__technological_mediation_reading, base_extractiveness, 1440, 0.03).
narrative_ontology:measurement(reform_tech_extractiveness_1480_early_adoption, reformation_composite__technological_mediation_reading, base_extractiveness, 1480, 0.06).
narrative_ontology:measurement(reform_tech_extractiveness_1517_reformation_onset, reformation_composite__technological_mediation_reading, base_extractiveness, 1517, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, global_infrastructure).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

% DUAL FORMULATION NOTE:
% The reformation_composite kernel decomposes into three distinct constraint stories with different observables and ε values. The technological_mediation_reading (ε=0.08, Mountain) treats printing as a necessary infrastructure phase transition. The theological_fragmentation_reading (ε≈0.35-0.45, likely Tangled Rope or Snare) treats competing soteriological commitments as the primary constraint. The political_realignment_reading (ε≈0.40-0.60, likely Tangled Rope) treats state consolidation and sovereignty assertion as the primary constraint. Each reading links to the others via network.affects_constraints, acknowledging that the Reformation cannot be fully understood as purely technological, purely theological, or purely political. The technological reading is upstream in the causal sense: printing enables the scale at which theological and political conflicts can manifest continuously across European territories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__technological_mediation_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
