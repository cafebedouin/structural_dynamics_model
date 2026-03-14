% ============================================================================
% CONSTRAINT STORY: damnatio_memoriae_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_damnatio_memoriae_enforcement, []).

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
 *   constraint_id: damnatio_memoriae_enforcement
 *   human_readable: Damnatio Memoriae Enforcement: Erasure as Social Coordination and Political Extraction
 *   domain: political_sociology/institutional_memory
 *
 * SUMMARY:
 *   Damnatio memoriae — the Roman practice of legally erasing a person from
 *   history — represents a structural constraint spanning political
 *   authority, institutional memory, and narrative control. What began as an
 *   explicit legal sentence (erasure of name, destruction of monuments,
 *   prohibition on commemoration) has evolved into a distributed
 *   institutional practice that operates through archive reorganization,
 *   record suppression, and narrative monopolization. The constraint exhibits
 *   all characteristics of a tangled rope: the successor political authority
 *   genuinely benefits from narrative consolidation and regime legitimization
 *   (coordination function), but enforces this benefit through systematic
 *   suppression that damages the epistemological commons and consumes
 *   resources (extraction function). The theater ratio (0.65) reflects that
 *   maintaining damnatio memoriae in contexts with distributed information
 *   sources requires increasing performative effort — the institutional
 *   machinery of erasure becomes more visible and fragile as digital
 *   preservation and diaspora archives accumulate evidence. The constraint's
 *   classification shifts dramatically based on the observer's structural
 *   position: targets and historical epistemology experience it as pure
 *   snare; the successor authority experiences mixed coordination and
 *   extraction; counter-archival movements experience it as a temporary
 *   scaffold problem being solved through technological alternatives;
 *   official institutional memory experiences it as degraded inertial theater
 *   (piton); and the civilizational analytical observer risks naturalizing it
 *   as an inevitable feature of political authority.
 *
 * KEY AGENTS:
 *   - Erasure Targets: Primary victims (powerless/trapped) — face total institutional erasure with no recourse; identity and legacy confiscated by state authority
 *   - Historical Epistemology/Disciplinary Practice: Secondary victims (powerless/trapped) — institutional knowledge infrastructure becomes complicit in suppression; discipline loses epistemological autonomy
 *   - Successor Political Authority: Primary beneficiary (institutional/constrained) — consolidates power through narrative monopoly but becomes locked into enforcement burden; benefits from erasure but constrained by maintenance requirements
 *   - Hegemonic Historical Narrative: Institutional beneficiary (institutional/arbitrage) — single authoritative history stabilizes meaning and prevents alternative interpretation; experiences pure coordination benefit
 *   - Counter-Archival Movements: Organized resistance (organized/mobile) — diaspora communities, historical societies, digital archivists building alternative verification pathways that create exit options and undermine suppression regime
 *   - Official Institutional Memory: State apparatus (institutional/arbitrage) — maintains the erasure machinery; experiences the constraint as increasingly performative and theater-dependent as digital preservation makes erasure technically fragile
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choice (damnatio memoriae) as inevitable feature of governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(damnatio_memoriae_enforcement, 0.58).
domain_priors:suppression_score(damnatio_memoriae_enforcement, 0.72).
domain_priors:theater_ratio(damnatio_memoriae_enforcement, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(damnatio_memoriae_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(damnatio_memoriae_enforcement, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(damnatio_memoriae_enforcement, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(damnatio_memoriae_enforcement, tangled_rope).
narrative_ontology:human_readable(damnatio_memoriae_enforcement, "Damnatio Memoriae Enforcement: Erasure as Social Coordination and Political Extraction").
narrative_ontology:topic_domain(damnatio_memoriae_enforcement, "political_sociology/institutional_memory").

domain_priors:requires_active_enforcement(damnatio_memoriae_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(damnatio_memoriae_enforcement, successor_political_authority).
narrative_ontology:constraint_beneficiary(damnatio_memoriae_enforcement, hegemonic_historical_narrative).
narrative_ontology:constraint_victim(damnatio_memoriae_enforcement, erasure_targets).
narrative_ontology:constraint_victim(damnatio_memoriae_enforcement, historical_epistemology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ERASURE TARGET (SNARE) — Targeted for damnatio memoriae faces total suppression of record, name, and legacy. Trapped by institutional erasure machinery with no exit. Cannot defend record, reclaim narrative, or appeal to counterargument. Extraction is maximal: identity, legacy, and historical presence are confiscated. No coordination benefit flows to the target; suppression is the sole function from their perspective.
constraint_indexing:constraint_classification(damnatio_memoriae_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: HISTORICAL EPISTEMOLOGY / DISCIPLINARY MEMORY (SNARE) — The academic and institutional practice of history cannot exit the erasure regime once established. Sources are destroyed, archives are reorganized to conceal, and the institutional infrastructure of historical knowledge becomes complicit in the suppression. The discipline bears the cost of impoverished epistemology and compromised record. No benefit accrues to historical epistemology; extraction is pure.
constraint_indexing:constraint_classification(damnatio_memoriae_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 3: SUCCESSOR POLITICAL AUTHORITY (TANGLED ROPE) — Beneficiary from damnatio memoriae enforcement: erasing the previous regime's legitimacy, cleansing the record, and establishing narrative monopoly over institutional continuity all serve the successor's consolidation of power. But the successor is also constrained by the need to maintain the credibility of the erasure regime — must enforce consistency across all records, prevent counter-narratives, and manage the institutional burden of maintaining the false history. Experiences mixed coordination (unified narrative, stable succession) and extraction (resource cost of enforcement, vulnerability if erasure is ever discovered).
constraint_indexing:constraint_classification(damnatio_memoriae_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: HEGEMONIC HISTORICAL NARRATIVE (ROPE) — The institutionalized version of history benefits from damnatio memoriae as a coordination mechanism: creates single authoritative narrative, prevents alternative interpretations, and stabilizes institutional memory around approved meanings. The narrative has flexibility (can evolve with new erasures) and dominance (controls educational systems, public records, archival infrastructure). From this perspective, the constraint is pure coordination — solves the problem of narrative multiplicity. No extraction is visible because the narrative IS the beneficiary.
constraint_indexing:constraint_classification(damnatio_memoriae_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: COUNTER-ARCHIVAL MOVEMENTS (SCAFFOLD) — Organized agents (historical societies, diaspora communities, international human rights bodies, digital archivists) build alternative archival infrastructures (underground libraries, exile documentation, oral histories, digital preservation) that create parallel verification pathways outside the suppression regime. These movements have agency and see an exit path: as digital preservation technologies mature and international accountability mechanisms strengthen, the damnatio memoriae enforcement loses power. Classified as scaffold because the constraint is experienced as temporary and solvable through institutional innovation, with an implied sunset as alternative verification becomes unavoidable.
constraint_indexing:constraint_classification(damnatio_memoriae_enforcement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: OFFICIAL INSTITUTIONAL MEMORY (PITON) — The state's official archive and institutional memory apparatus treats damnatio memoriae as a required ritual: redactions must be processed, forbidden names must be struck from records, and the institutional procedures must be followed even when their practical effect has degraded. Classified as piton because the theater_ratio is high — maintaining erasure in the digital age requires substantial performative effort (removing digital records requires constant work, updates must be monitored, re-emergence of suppressed information must be pre-empted). The institutional memory sees its own enforcement as increasingly degraded and inertial: the original function (monopolizing narrative) persists but the mechanism (institutional erasure) has become fragile.
constraint_indexing:constraint_classification(damnatio_memoriae_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational analytical perspective, the constraint appears as a law of political biology: all regimes practice some form of historical revisionism, all institutional memories are partial, and the erasure of inconvenient pasts is inherent to how power operates. This perspective risks naturalizing what is actually a contingent institutional choice. However, structural data reveals this as a false summit: damnatio memoriae is not inherent to governance — it is a specific institutional technology that succeeds or fails based on media infrastructure, archival accessibility, and international enforcement mechanisms.
constraint_indexing:constraint_classification(damnatio_memoriae_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(damnatio_memoriae_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(damnatio_memoriae_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(damnatio_memoriae_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(damnatio_memoriae_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(damnatio_memoriae_enforcement, TR),
    TR >= 0.70.

:- end_tests(damnatio_memoriae_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. Initial extractiveness (0.35) reflects the Roman period when damnatio memoriae was enacted through explicit legal sentence with clear institutional support. Modern extractiveness (0.58) reflects the constraint's evolution into distributed institutional practice: suppression requires continuous archival work, monitoring of digital content, coordination across multiple institutional actors, and pre-emption of counter-narratives. The trajectory shows increasing extractiveness because digital preservation makes erasure technically harder, forcing the regime to invest more resources in suppression while effectiveness declines. Suppression (0.72): High and structural. Barriers to historical reconstruction include institutional control of official archives, legal prohibition on commemorating erased figures, educational curriculum monopoly, and (in some contexts) violent penalty for discussing suppressed names. However, suppression is not absolute — diaspora communities, underground documentation, and digital preservation create leakage. Theater ratio (0.65): Moderate-high and increasing. At T=0, the erasure was enforced through explicit institutional law with clear penalties — low theater because the mechanism was openly acknowledged. By T=10, maintenance requires substantial performative work: officials must continuously monitor for re-emergence of suppressed information, update digital records to remove references, teach revised history in schools, and manage the fragility of the erasure regime as digital copies proliferate. The theater increase reflects that the original institutional mechanism has degraded while the constraint persists.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The erasure target perceives pure extraction (snare) — their entire historical existence is confiscated with no benefit. Historical epistemology perceives the constraint as damage to the epistemic commons (snare) — the discipline loses autonomy and accuracy for institutional purposes. The successor authority perceives mixed benefit and burden (tangled rope) — narrative consolidation (rope) requires enforcement machinery (snare). The hegemonic narrative perceives pure coordination (rope) — unified history solves the problem of narrative multiplicity. Counter-archival movements perceive the constraint as a temporary problem with a sunset (scaffold) — digital and diaspora alternatives are making erasure progressively harder. Official memory perceives its own practice as degraded ritual (piton) — erasure was once a clear institutional function but has become increasingly theatrical as digital preservation makes complete suppression impossible. The civilizational observer risks seeing an immutable law (mountain) — 'all regimes rewrite history' — but the structural data reveals this as false summit: damnatio memoriae is contingent on specific archival infrastructure and enforcement capacity; it fails predictably when those infrastructure assumptions break.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the constraint's extraction flow. Erasure targets have d=0.95 (full targets, trapped, powerless) — they experience maximum extraction through narrative confiscation. Historical epistemology has d=0.88 (victim of integrity damage, trapped, powerless) — loses epistemological autonomy. Successor authority has d=0.48 (hybrid: benefits from narrative consolidation but constrained by enforcement burden; organized power can sometimes exit by reducing enforcement, but political costs are high) — derives d from institutional power with moderate exit optionality. Hegemonic narrative has d=0.10 (beneficiary with arbitrage: can shift with new targets, control interpretation) — derives d from institutional power with arbitrage exit. Counter-archival movements have d=0.35 (victims of suppression but with exit options through organizational capacity and technological alternatives) — derives d from organized power with mobile exit. Official memory has d=0.42 (institutional actor increasingly constrained by technical degradation of erasure mechanism, but still invested in enforcement) — derives d from institutional power with constrained exit. The analytical observer has d=0.72 (analytical risk of naturalizing contingency) — derives d from analytical context observing from outside the constraint's enforcement structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that 'is damnatio memoriae enforcement coordination or extraction?' depends entirely on which agent's perspective you measure from. For the successor authority seeking narrative monopoly, it is coordination (rope). For the epistemology of history, it is pure extraction (snare). For counter-archival movements with technological alternatives, it is a temporary coordination failure being solved (scaffold). For the official state memory apparatus, the original coordination function (unified narrative) has atrophied while enforcement persists as inertial theater (piton). The tangled rope classification captures the fact that the constraint CONTAINS BOTH coordination function (narrative consolidation) and extraction mechanism (suppression of inconvenient pasts) simultaneously, and both operate through the same institutional machinery. The regime cannot have the coordination benefit without paying the extraction cost of suppression. Mandatrophy resolution: the constraint is not mislabeled as pure coordination (rope) — it is genuinely tangled because narrative consolidation and suppression are structurally inseparable. The piton perspective signals that the coordination function may be degrading: digital preservation is making erasure technically infeasible, causing the regime to maintain enforcement increasingly through theater (ritual redaction, educational enforcement) rather than through actual suppression. The scaffold perspective signals that exit is becoming structural: counter-archival movements and digital redundancy create pathways around institutional erasure, making the constraint solvable rather than eternal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_preservation_threshold,
    'At what level of digital preservation redundancy does damnatio memoriae enforcement become structurally impossible?',
    'Empirical analysis of suppression attempts in high-digital-redundancy contexts (distributed cloud storage, blockchain-backed archives, diaspora digital networks). Comparison of erasure success rates pre-digital vs post-digital eras.',
    'If threshold < 3 independent copies: enforcement remains viable via technical suppression. If threshold > 10 copies: enforcement becomes pure theater, reclassifying to piton from all institutional perspectives. Impact on mandatrophy: if enforcement becomes purely theatrical, successor authority loses the tangled rope benefit (coordination) and experiences pure extraction (theater maintenance cost).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_preservation_threshold, empirical, 'Digital preservation threshold for erasure feasibility').

omega_variable(
    institutional_commitment_durability,
    'Can institutional erasure enforcement survive regime transition if successor regimes have different narratives to protect?',
    'Historical analysis of multi-generational erasure regimes: do successors to the successor continue the same damnatio memoriae? Do they reactivate erased figures? Do they create new targets? Longitudinal analysis of archival policy across regime transitions.',
    'If enforcement degrades with successor-to-successor transition: classification shifts to scaffold (temporary) from institutional perspective. If enforcement perpetuates across regimes: snare classification solidifies (structural extraction), suggesting the regime commits to long-term suppression cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_commitment_durability, empirical, 'Durability of erasure commitment across regime transitions').

omega_variable(
    narrative_reconstruction_pace,
    'How quickly can erased historical figures be reconstructed from fragmentary sources, diaspora archives, and counter-documentation?',
    'Empirical study of reconstruction timelines for historically suppressed figures: correlation between suppression duration and reconstruction completeness. Time-to-availability of suppressed information via non-institutional channels (exile documentation, oral histories, underground archives).',
    'If reconstruction time < 1 generation: scaffold perspective confirmed — erasure has built-in sunset. If reconstruction time > 3 generations: snare becomes self-sustaining because the target''s generation dies before reconstruction occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_reconstruction_pace, empirical, 'Temporal dynamics of historical narrative reconstruction').

omega_variable(
    suppression_mechanism_type,
    'Is suppression accomplished through institutional control (active erasure) or through epistemic closure (agents internalize the erasure narrative)?',
    'Comparison of suppression persistence in contexts with high institutional capacity (state archives, official media monopolies) vs low institutional capacity (diaspora communities, post-regime successor regimes). Analysis of counter-narrative emergence: does suppression fail suddenly (institutional capacity lost) or gradually (internalized belief shifts)?',
    'If suppression is primarily institutional: piton and scaffold classifications become viable — the constraint is inertial and solvable. If suppression is primarily internalized (identity lock at cultural level): snare persists across institutional transitions and becomes harder to break.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_type, empirical, 'Suppression mechanism: institutional control vs cognitive internalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(damnatio_memoriae_enforcement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(damn_tr_t0, damnatio_memoriae_enforcement, theater_ratio, 0, 0.4).
narrative_ontology:measurement(damn_tr_t5, damnatio_memoriae_enforcement, theater_ratio, 5, 0.55).
narrative_ontology:measurement(damn_tr_t10, damnatio_memoriae_enforcement, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(damn_be_t0, damnatio_memoriae_enforcement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(damn_be_t5, damnatio_memoriae_enforcement, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(damn_be_t10, damnatio_memoriae_enforcement, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(damnatio_memoriae_enforcement, identity_coordination).
narrative_ontology:affects_constraint(damnatio_memoriae_enforcement, narrative_monopolization).
narrative_ontology:affects_constraint(damnatio_memoriae_enforcement, institutional_epistemic_closure).
narrative_ontology:affects_constraint(damnatio_memoriae_enforcement, regime_succession_legitimacy).

% DUAL FORMULATION NOTE:
% Damnatio memoriae enforcement contains multiple structurally distinct constraints that should be decomposed by ε-invariance analysis: (1) narrative_consolidation (ε=0.25, Rope) — the genuine coordination problem of unified historical narrative; (2) suppression_enforcement (ε=0.68, Snare) — the extraction mechanism of erasure; (3) regime_legitimization (ε=0.52, Tangled Rope) — the hybrid function where successor authority consolidates power through history control. The single 'damnatio memoriae enforcement' story represents the tangled combination, but institutions attempting to break the constraint typically succeed by addressing suppression_enforcement without touching narrative_consolidation — allowing alternative narratives while maintaining regime continuity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(damnatio_memoriae_enforcement, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
