% ============================================================================
% CONSTRAINT STORY: kjv_great_awakening
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_great_awakening, []).

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
 *   constraint_id: kjv_great_awakening
 *   human_readable: The Great Awakening's Reframing of Biblical Authority
 *   domain: religious/social
 *
 * SUMMARY:
 *   The First Great Awakening (c. 1730–1750) reframed biblical authority from
 *   institutional mediation (trained clergy, formal theology, ecclesiastical
 *   hierarchy) to direct personal experience and emotional encounter with
 *   scripture. Preachers like George Whitefield and Jonathan Edwards claimed
 *   that authentic faith required not doctrinal correctness but spiritual
 *   rebirth — a radical democratization of religious authority that
 *   simultaneously created new forms of extraction. The constraint exhibits
 *   hybrid character: it solves a genuine coordination problem (believers can
 *   access spiritual meaning without institutional gatekeepers) while
 *   simultaneously concentrating interpretive power in charismatic preachers,
 *   enabling new extraction from spiritually hungry audiences. The reframing
 *   is neither pure liberation nor pure capture — it is institutional
 *   displacement through authoritarian innovation. Established clergy
 *   (bishops, university-trained theologians, state-backed church
 *   hierarchies) lost interpretive monopoly. Evangelical preachers gained
 *   mobility and influence. The church itself fragmented into competing
 *   revival methodologies, denominational splits, and parallel structures
 *   that persist to this day. Theater ratio increased from 0.35 to 0.58 as
 *   the initial spontaneity of revival experience became formalized into
 *   revival meetings, conversion narratives, and emotional performance — the
 *   authenticity gate was replaced with emotional intensity gate, a form of
 *   Goodhart drift. Extractiveness increased from 0.22 to 0.38 as preachers
 *   consolidated followers, demanded loyalty and tithes, and built
 *   institutional power on the foundation of emotional dependence rather than
 *   doctrinal assent.
 *
 * KEY AGENTS:
 *   - Established Clergy: Primary victims (powerless/trapped) — lose interpretive monopoly and institutional authority; cannot exit without abandoning identity
 *   - Evangelical Preachers: Primary beneficiaries (organized/mobile) — gain influence, followers, mobility, and new forms of authority through emotional charisma
 *   - Parish Congregations: Secondary victims (moderate/constrained) — gain access to emotionally resonant spirituality but face conflict with traditional obligations and denominational authority
 *   - Reformed Denominations (Presbyterian, Congregational): Powerful institutional actors (powerful/mobile) — initially resist, then institutionalize Awakening methods to reclaim authority
 *   - Anglican Establishment: Institutional actor (institutional/arbitrage) — maintains formal state authority despite loss of functional spiritual influence; theater persists through establishment, not faith
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees reframing as institutional displacement that solves coordination problems while creating new extraction mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_great_awakening, 0.38).
domain_priors:suppression_score(kjv_great_awakening, 0.52).
domain_priors:theater_ratio(kjv_great_awakening, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_great_awakening, extractiveness, 0.38).
narrative_ontology:constraint_metric(kjv_great_awakening, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(kjv_great_awakening, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_great_awakening, tangled_rope).
narrative_ontology:human_readable(kjv_great_awakening, "The Great Awakening's Reframing of Biblical Authority").
narrative_ontology:topic_domain(kjv_great_awakening, "religious/social").

domain_priors:requires_active_enforcement(kjv_great_awakening).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_great_awakening, evangelical_preachers).
narrative_ontology:constraint_beneficiary(kjv_great_awakening, spiritual_enthusiasts).
narrative_ontology:constraint_victim(kjv_great_awakening, established_clergy).
narrative_ontology:constraint_victim(kjv_great_awakening, denominational_authority).
narrative_ontology:constraint_victim(kjv_great_awakening, formal_theological_training).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ESTABLISHED CLERGY (SNARE) — Trapped by loss of interpretive monopoly. Cannot exit the reframing without surrendering institutional legitimacy. Career, status, and congregational authority erode as emotional/personal piety displaces textual erudition as the standard for authentic faith. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.51.
constraint_indexing:constraint_classification(kjv_great_awakening, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PARISH CONGREGATION (TANGLED ROPE) — Constrained by social pressure and doctrinal obligation to their existing church, but also benefits from the new emotionally resonant preaching and inclusive spiritual experience. The Great Awakening offers liberation from dry formalism while simultaneously creating tension with traditional authority. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.40.
constraint_indexing:constraint_classification(kjv_great_awakening, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EVANGELICAL PREACHERS (ROPE) — Beneficiaries of the reframing. High mobility (travel between communities, establish new congregations), coordinating believers around experiential faith. The constraint solves a genuine coordination problem: enabling believers to access spiritual experience without traditional gatekeepers. d≈0.25, f(d)≈0.15, σ=0.9 → χ≈0.05. Low effective extraction because mobility + coordination function.
constraint_indexing:constraint_classification(kjv_great_awakening, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: REFORMED INSTITUTIONAL LEADERS (SCAFFOLD) — Powerful institutional actors (Presbyterian, Congregational leadership) who initially resisted the Awakening but eventually institutionalized its methods (revivals, emotional preaching, training new itinerants). They see the constraint as temporary — a phase of disruption being absorbed into denominational structure with built-in sunset: once the emotional energy is captured into formal revival methodology and trained pastoral leadership, the raw reframing loses force. d≈0.35, f(d)≈0.30, σ=0.9 → χ≈0.10.
constraint_indexing:constraint_classification(kjv_great_awakening, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANGLICAN ESTABLISHMENT (PITON) — Institutional actor maintaining formal authority structure despite functional obsolescence. The reframing has largely displaced them in spiritual influence, yet their ecclesiastical structure persists through state establishment and social convention. Theater ratio = 0.58 reflects the performative maintenance of Anglican prerogative (formal authority, ritual, institutional legitimacy) while actual religious authority has migrated to evangelical preachers. The constraint is maintained by inertia and establishment law, not by genuine spiritual function.
constraint_indexing:constraint_classification(kjv_great_awakening, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the Great Awakening's reframing contains both genuine coordination and structural extraction. The coordination function: enabling believers to access spiritual experience without institutional mediation (genuine problem solved). The extraction function: concentrating interpretive power in emotionally compelling preachers, enabling new forms of spiritual authority to consolidate while displacing competing centers (bishops, formal clergy, universities). The reframing is not a restoration of pure personal faith; it is institutional displacement through authoritarian innovation. ε=0.38, suppression=0.52 reflect this hybrid nature.
constraint_indexing:constraint_classification(kjv_great_awakening, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_great_awakening_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kjv_great_awakening, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kjv_great_awakening, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kjv_great_awakening, TR),
    TR >= 0.70.

:- end_tests(kjv_great_awakening_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Great Awakening's reframing extracted legitimacy from established institutions and transferred it to evangelical preachers. This was not as severe as later predatory revivals (e.g., late 19th-century faith-healing extractors at 0.65+) because the initial movement genuinely solved a coordination problem: believers gained access to spiritual meaning without institutional mediation. But extractiveness is not low because the new authority centers (preachers, revival societies, Methodist circuits) developed their own forms of coercion: emotional dependence, social pressure within revival communities, financial dependence on charismatic leaders. The value 0.38 reflects that the coordination gain partially offsets the new extraction. Suppression (0.52): Moderate-high. The reframing faced resistance from established church authorities (formal denunciations, pulpit exclusions, legal barriers to itinerant preaching). Believers faced social pressure from conservative family members and parish priests. But suppression was not total — the Awakening spread rapidly through informal networks, conversion narratives, and open-air preaching that bypassed institutional control. Suppression decreased over time as denominations absorbed revival methods. Theater ratio (0.58): Moderate-high. The initial Awakening (1730s) had relatively low theater (0.35) — genuine spiritual experience, unstructured revival gatherings, spontaneous preaching. By mid-century (1750), revival meetings had become formalized events with expected narratives (conviction, conversion, testimony), emotional performance, and organized itinerancy. The theater increased as the spontaneity gate was replaced with an emotional intensity gate (Goodhart drift). Still below 0.70 (piton threshold) because revival meetings maintained some genuine spiritual function, unlike purely performative ecclesiastical ritual.
 *
 * PERSPECTIVAL GAP:
 *   The established clergy see the reframing as pure extraction (Snare) — loss of interpretive authority, devaluation of formal training, erosion of institutional legitimacy. They cannot exit without abandoning their identity and livelihood. The evangelical preachers see coordination (Rope) — solving the problem of believers' spiritual hunger, enabling community formation around authentic experience. The parish congregations see a mixed constraint (Tangled Rope) — gaining emotional resonance and inclusive spirituality while facing conflict with traditional authority and denominational obligation. The Reformed institutions see a temporary disruption (Scaffold) — the reframing challenges their authority, but by absorbing revival methods into denomination training and circuit systems, they institutionalize the energy and reduce the threat. The Anglican Establishment sees a degraded ritual (Piton) — their formal authority persists through state establishment, but their functional influence has migrated to evangelical spaces; theater maintains their structure (0.58 reflects this maintenance). The analytical observer sees the full hybrid (Tangled Rope) — genuine coordination problem solved, but simultaneously enabling new forms of institutional capture and spiritual extraction. The perspectival gap is dramatic: established clergy experience catastrophic authority loss (Snare), while preachers experience liberation and power (Rope), while institutions experience manageable disruption (Scaffold), while abstract spiritual pluralism experiences displacement (Tangled Rope). No perspective is wrong — each is structurally accurate from that position.
 *
 * DIRECTIONALITY LOGIC:
 *   Established clergy: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — cannot exit institutional role without total identity loss. Evangelical preachers: Beneficiary + mobile → d≈0.25, f(d)≈0.15. Low extraction because mobility enables exit from any individual congregation + genuine coordination function (solving spiritual hunger). Parish congregations: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but not maximal — constrained by social bonds and denominational obligation, but can migrate between congregations or adopt private faith. Reformed institutions: Powerful + mobile → d≈0.35, f(d)≈0.30. Low effective extraction because institutions can reorganize, absorb methods, and adapt. They are victims of the initial reframing but have agency to institutionalize it. Anglican Establishment: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater gate (0.58), not from high chi. They maintain formal authority through establishment, not functional influence. Analytical observer: analytical → d≈0.72, f(d)≈1.15. The observer is external but sees the full extraction mechanism embedded in the new authority structure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint demonstrates the mandatrophy by showing that institutional displacement (Snare from established clergy's view) IS SIMULTANEOUSLY genuine coordination (Rope from evangelical preacher's view) and institutional adaptation (Scaffold from Reformed institutional view). The mandatrophy is NOT 'which classification is correct?' but 'whose structural position are we measuring?' The established clergy experience extraction because their monopoly is broken and their authority is devalued. The evangelical preachers experience coordination because they solve genuine believers' needs and have mobility. The Reformed institutions experience manageable disruption because they absorb methods. The analytical observer sees that institutional displacement always creates both — the new authority center (preachers) solves the old problem (institutional gatekeeping) while creating new problems (charismatic dependence, emotional extraction, sectarian competition). The Tangled Rope classification at the analytical level resolves mandatrophy: yes, there IS genuine coordination (believers' spiritual hunger + access to experience), AND yes, there IS asymmetric extraction (evangelical preachers consolidate power, traditional clergy lose legitimacy, spiritual dependence replaces doctrinal autonomy). The constraint is not a pure rope misclassified as snare; it is a genuine hybrid that different perspectives experience as different types. The 0.38 extractiveness reflects this: not high enough to be pure snare (χ ≥ 0.66), but high enough that a beneficiary group exists alongside victims. The mandatrophy is resolved by accepting that institutional displacement is structurally mixed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emotional_authority_vs_textual_authority,
    'Is the Great Awakening a liberation from institutional gatekeeping or merely a replacement of textual authority with charismatic authority?',
    'Historical analysis of interpretive diversity: did personal/emotional readings of scripture produce MORE theological pluralism than textual scholarship, or less? Measurement of doctrinal coherence and drift in revival-founded vs formally-trained congregations.',
    'If liberation: constraint is Rope (coordination around authentic experience). If replacement: constraint is Snare (one form of authority displaces another). If mixed: Tangled Rope confirmed — genuine coordination AND new extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emotional_authority_vs_textual_authority, empirical, 'Whether the reframing replaces one authority with another or genuinely decentralizes').

omega_variable(
    social_inclusion_vs_spiritual_extraction,
    'Did the Great Awakening genuinely include lower-class and marginalized believers in spiritual authority (Rope), or did it extract legitimacy from them while consolidating new forms of clerical power (Snare)?',
    'Demographic analysis of preacher origins and theological training; comparison of social mobility for believers in revival vs traditional congregations; measurement of decision-making power for women, enslaved persons, and non-landholders in Awakening vs established churches.',
    'If genuinely inclusive: Rope classification strengthened. If extractive: Snare classification of Awakening preachers relative to marginalized converts. If unequal inclusion: Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_inclusion_vs_spiritual_extraction, empirical, 'Whether inclusion was genuine or extractive').

omega_variable(
    institutional_absorption_timeline,
    'How long did it take for the Awakening''s emotional preaching to become formalized as institutional practice, and does this timeline indicate successful coordination (scaffold sunset) or permanent bifurcation into multiple authority centers (tangled rope equilibrium)?',
    'Timeline of Methodist circuit riders, Presbyterian revival societies, and evangelical seminary training. Measurement of conflict intensity and institutional splits during and after absorption period. Did institutions adopt Awakening methods to reclaim authority (scaffold sunset) or did they create parallel structures that competed indefinitely (tangled rope)?',
    'If sunset ~50 years: scaffold prediction confirmed. If conflict persists >100 years: tangled rope equilibrium confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_absorption_timeline, empirical, 'Whether institutional absorption created sunset or persistent competition').

omega_variable(
    scriptural_interpretation_gates,
    'Did the Great Awakening genuinely lower barriers to biblical interpretation (anyone can read scripture and experience God), or did it create new gates (emotional intensity, conversion narrative, prophetic speech) that replicated exclusivity in new form?',
    'Comparative analysis of who could preach: formal training required for established clergy vs experiential testimony required for evangelical preachers. Measurement of doctrinal disagreement and splits among Awakening-founded churches — did removing formal gates increase or decrease sectarian fragmentation?',
    'If truly open: Rope classification. If new gates: Snare classification. If mix: Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_interpretation_gates, conceptual, 'Whether interpretive gates were removed or relocated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_great_awakening, 1730, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gaba_tr_t0, kjv_great_awakening, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gaba_tr_t10, kjv_great_awakening, theater_ratio, 10, 0.5).
narrative_ontology:measurement(gaba_tr_t20, kjv_great_awakening, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(gaba_be_t0, kjv_great_awakening, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gaba_be_t10, kjv_great_awakening, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(gaba_be_t20, kjv_great_awakening, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_great_awakening, information_standard).
narrative_ontology:affects_constraint(kjv_great_awakening, methodist_discipline_enforcement).
narrative_ontology:affects_constraint(kjv_great_awakening, evangelical_seminary_professionalization).
narrative_ontology:affects_constraint(kjv_great_awakening, denominational_fragmentation_17th_19th_century).

% DUAL FORMULATION NOTE:
% The Great Awakening's reframing of biblical authority decomposes into two structurally distinct constraints: (1) the coordination mechanism itself (ε≈0.22, Rope) — enabling believers to access spiritual experience without institutional mediation, solving genuine epistemic and spiritual access problems; (2) the extraction mechanism embedded in charismatic authority (ε≈0.50+, Snare) — evangelical preachers consolidating spiritual dependence, creating new forms of coercion. The story as presented (ε=0.38, Tangled Rope) captures the hybrid at its moment of emergence. Downstream constraints track the divergence: Methodist circuit system absorbs and institutionalizes the coordination function (Scaffold/Rope), while evangelical splinters develop independent extraction (Snare). Initial story should link to both family members via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_great_awakening, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
