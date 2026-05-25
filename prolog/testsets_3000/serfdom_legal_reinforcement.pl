% ============================================================================
% CONSTRAINT STORY: serfdom_legal_reinforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_serfdom_legal_reinforcement, []).

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
 *   constraint_id: serfdom_legal_reinforcement
 *   human_readable: Serfdom Legal Reinforcement
 *   domain: political_economy/feudal_systems
 *
 * SUMMARY:
 *   Serfdom legal reinforcement represents a canonical extraction constraint
 *   enforced through property law, bonding obligations, and deliberate
 *   suppression of exit alternatives. A serf is legally bound to a lord's
 *   land, cannot leave without permission, cannot marry without consent,
 *   cannot choose occupation or inheritance path. The legal framework
 *   combines explicit binding (formal ownership of peasant labor) with
 *   suppression mechanisms (flight penalties including mutilation, death, or
 *   re-enslavement) and identity-fusing cultural norms that make exit
 *   unthinkable across generations. The constraint exhibits all six DR types
 *   from different structural positions: pure snare for the bound peasant;
 *   rope (coordination) for nobility; tangled rope for intermediate gentry;
 *   identity-locked snare at generational timescale; temporary problem with
 *   sunset clause for abolitionists; degraded remnant for post-abolition
 *   legal systems; and snare at civilizational analytical view. The
 *   constraint's extractiveness increases slightly over its early period as
 *   feudalism systematizes, peaks mid-period, then declines as legal
 *   abolition spreads across Europe. Theater ratio increases over time as the
 *   legal fiction of 'natural order' grows more elaborate even as the actual
 *   extraction persists.
 *
 * KEY AGENTS:
 *   - Bound Peasantry: Primary victim (powerless/trapped at biographical scale, identity_locked at generational scale) — bears extraction through labor obligations, restricted autonomy, legal penalties for exit
 *   - Landed Nobility: Primary beneficiary (institutional/arbitrage) — captures labor surplus and feudal tribute; has full exit optionality through emigration or estate transfer
 *   - Minor Gentry: Secondary institutional actor (moderate/constrained) — benefits from serf labor but constrained by reciprocal feudal obligations to higher nobility; experiences mixed coordination and extraction
 *   - Abolitionist Movement: Organized reform actor (organized/constrained) — centralizing monarchies, merchant classes, church reformers perceive serfdom as dysfunctional and work to abolish it; see sunset clause and organized exit pathway
 *   - Legal and Church Apparatus: Institutional maintainers (institutional/arbitrage) — codify and legitimize binding through law and theology; see serfdom as natural order; persist through inertia after abolition through substitute mechanisms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the full causal structure of extraction and recognizes that abolition did not fully eliminate the mechanism but merely transformed its legal form
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(serfdom_legal_reinforcement, 0.68).
domain_priors:suppression_score(serfdom_legal_reinforcement, 0.75).
domain_priors:theater_ratio(serfdom_legal_reinforcement, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(serfdom_legal_reinforcement, extractiveness, 0.68).
narrative_ontology:constraint_metric(serfdom_legal_reinforcement, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(serfdom_legal_reinforcement, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(serfdom_legal_reinforcement, snare).
narrative_ontology:human_readable(serfdom_legal_reinforcement, "Serfdom Legal Reinforcement").
narrative_ontology:topic_domain(serfdom_legal_reinforcement, "political_economy/feudal_systems").

domain_priors:requires_active_enforcement(serfdom_legal_reinforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(serfdom_legal_reinforcement, landed_nobility).
narrative_ontology:constraint_victim(serfdom_legal_reinforcement, bound_peasantry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOUND PEASANT (SNARE) — Trapped by law, custom, and economic dependency. Peasant is legally bound to land, cannot leave without lord's permission, cannot marry without consent, cannot choose occupation. No structural exit pathway; escape requires violation of law with severe penalty (mutilation, execution, enslavement). Maximum experienced extraction.
constraint_indexing:constraint_classification(serfdom_legal_reinforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LANDED NOBILITY (ROPE) — Experiences serfdom as coordination mechanism for agricultural labor and feudal obligation fulfillment. The legal framework solves the problem of securing reliable labor supply and maintaining social hierarchy. Nobility has full exit option (can emigrate, change domains, sell estates). Net beneficiary with arbitrage optionality — extraction flows toward them.
constraint_indexing:constraint_classification(serfdom_legal_reinforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: MINOR GENTRY (TANGLED ROPE) — Intermediate position. Benefits from access to bound labor and feudal privilege but also faces constraints: bound by reciprocal feudal obligations to higher nobility, limited military options, dependent on tenant peasant productivity. Experiences genuine coordination (social stability, predictable labor) alongside asymmetric extraction (can extract from peasants but must yield to nobility).
constraint_indexing:constraint_classification(serfdom_legal_reinforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: SERF WITH INTERNALIZED CONSTRAINT (IDENTITY_LOCKED) — At generational time horizon, the bound peasant's identity becomes fused with serf status. The peasant's self-concept, social role, and worldview are constituted through the binding constraint: 'I am a serf,' 'this is my place,' 'the lord owns me.' Legal constraints remain in place, but the binding mechanism is now partially cognitive. The serf could theoretically flee (constrained exit) but cannot imagine doing so because their identity is constituted through servitude. Escaping would require becoming a different person — abandoning inherited social identity, severing kinship and community bonds, adopting a persona of lawlessness or vagabondage with no legitimate social place.
constraint_indexing:constraint_classification(serfdom_legal_reinforcement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 5: ABOLITIONIST MOVEMENT (SCAFFOLD) — Organized agents (monarchical centralization, emerging merchant class, church reform movements) begin dismantling serfdom through legal reforms. The constraint is perceived as temporary and dysfunctional for emerging economic systems. The sunset is real: over 300-500 years, serfdom is legally abolished across Europe. Suppression remains high during transition (former serfs face economic coercion and discrimination) but declines as alternatives emerge. Low effective extraction because organized agents have agency and see a clear exit pathway.
constraint_indexing:constraint_classification(serfdom_legal_reinforcement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: REMNANT LEGAL APPARATUS (PITON) — After formal abolition, traces of serfdom persist through sharecropping, debt-bondage, and quasi-feudal land tenure. The legal mechanisms are degraded — serfdom is nominally illegal but functionally reinstated through debt traps and agricultural contracts. Theater ratio is high (formal legal equality coexists with structural slavery). The apparatus persists through institutional inertia: land-ownership structures, debt relationships, and agricultural contracts preserve the binding mechanism even after the legal label is removed.
constraint_indexing:constraint_classification(serfdom_legal_reinforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, serfdom is a pure extraction mechanism with minimal coordination function. The legal framework's claimed purpose (social stability, orderly land management) is secondary to its actual function (capturing peasant labor and preventing exit). The constraint persists not because coordination fails without it but because the extractive surplus justifies enforcing it. The analytical observer sees the full causal chain: property law + legal bonding + suppression of alternatives = snare.
constraint_indexing:constraint_classification(serfdom_legal_reinforcement, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(serfdom_legal_reinforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(serfdom_legal_reinforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(serfdom_legal_reinforcement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(serfdom_legal_reinforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(serfdom_legal_reinforcement, TR),
    TR >= 0.70.

:- end_tests(serfdom_legal_reinforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The bound peasant transfers a substantial portion of agricultural surplus to the lord through labor obligations (typically 3-4 days per week corvée labor), tribute payments, and inherited restrictions on economic mobility. The extraction is direct and unambiguous — the legal system explicitly allocates peasant labor to the lord. The value reflects core extraction without secondary rent-seeking layering (which would push it toward 0.75+). Suppression (0.75): Very high. Structural barriers to exit include: (1) legal prohibition with severe penalties, (2) economic dependency (landlessness), (3) cultural/identity lock (serf identity internalized across generations), (4) kinship/community bonds to land, (5) absence of alternative social roles or legitimate refuge. Flight is possible (some serfs did flee to towns or forests) but carries execution risk, mutilation, enslavement, and permanent status degradation. Theater ratio (0.55): Moderate. The legal system produces significant performative content — elaborate justifications via natural law, divine order, social hierarchy necessity — but the core extraction mechanism is relatively transparent. Serfs understand what they owe and why; the theater is not about hiding the extraction but about legitimizing it as necessary. Theater increases over time as the ideology elaborates while abolition pressure grows.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates complete perspectival inversion: the beneficiary (nobility) sees rope/coordination, the victim (peasant) sees snare/extraction, and the analytical observer sees snare/pure extraction. The identity-locked perspective reveals a critical mechanism: at biographical timescale the peasant might escape (constrained exit, not trapped), but internalization of serf identity prevents any escape imagining. The generational identity-locked classification exposes that serfdom's binding is not purely structural (legal + economic) but partially cognitive (identity-fused). The abolitionist scaffold perspective shows the constraint is not immutable — organized action can dissolve it. The piton perspective (post-abolition remnants) shows that legal abolition does not automatically eliminate the extraction mechanism — it persists through substitution (sharecropping, debt-bondage, discriminatory law). The perspectival gaps reveal the full anatomy of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's power level, exit options, and structural relationship to extraction flow. Bound peasants with trapped exit and victim status: d ≈ 0.95 (maximum target), f(d) ≈ 1.42 (maximum experienced extraction). Landed nobility with institutional power, arbitrage exit, and beneficiary status: d ≈ 0.05 (maximum beneficiary), f(d) ≈ -0.12 (negative experienced extraction — they are subsidized). Minor gentry with moderate power, constrained exit, and mixed status: d ≈ 0.50-0.60 (slightly toward target), f(d) ≈ 0.65-0.85 (moderate experienced extraction). Abolitionists with organized power and constrained exit: d ≈ 0.55 (slight target but with agency), f(d) ≈ 0.75 (moderate experienced extraction). The directionality chain shows why powerless trapped serfs experience maximum extraction while beneficiary nobility experience minimum or negative extraction from the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   CANONICAL SNARE EXEMPLAR: This constraint resolves the mandatrophy by demonstrating that snare classification is not arbitrary — it has structural justification (trapped exit + high suppression + asymmetric extraction) that is stable across multiple independent perspectives. The peasant sees snare. The analytical observer sees snare. The abolitionists see a snare that can be dismantled (scaffold is the organized reform pathway). The only perspective that sees rope (coordination) is the beneficiary (nobility), which is precisely the position that would rationalize extraction as coordination. The mandatrophy is resolved by noting that snare is the correct classification from all positions EXCEPT the beneficiary's, and the beneficiary's perspective is the least reliable guide to the constraint's true function. Post-abolition piton classification confirms that the extraction persists even after legal form changes — serfdom was never about coordination; it was pure extraction using legal form as theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_internalization_threshold,
    'At what generational point does legal constraint become internalized identity constraint? What fraction of serfdom''s binding is legal vs cognitive?',
    'Historical analysis of serf flight patterns; records of escape attempts and reasons for return; ethnographic data on serf self-reporting; comparison of serf behavior before vs after legal abolition',
    'If primarily legal (70%+): serfdom is pure snare with trapped exit. If substantially internalized (50%+): serfdom involves identity_locked agents whose classification shifts at biographical timescale. Affects projections for post-abolition social mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_internalization_threshold, empirical, 'Proportion of serfdom binding that is legal vs internalized').

omega_variable(
    alternative_labor_adequacy,
    'Could feudal agricultural systems function without serfdom using free-wage labor or cooperative tenant arrangements?',
    'Economic analysis of alternative arrangements historically available; comparison with regions that abolished serfdom earlier; counterfactual modeling of wage-based feudal agriculture',
    'If viable: serfdom is pure extraction (snare). If necessary: serfdom provides real coordination function and should reclassify toward tangled_rope. Changes evaluation of abolitionist movement''s feasibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_labor_adequacy, conceptual, 'Viability of feudal systems without legal serfdom').

omega_variable(
    post_abolition_substitution,
    'After legal abolition of serfdom, what fraction of extraction is preserved through sharecropping, debt-bondage, and discriminatory laws? Does the total suppression and extractiveness persist?',
    'Quantitative comparison of serf labor obligations pre-abolition vs sharecropper obligations post-abolition; measurement of income/autonomy changes; documentation of legal substitution mechanisms',
    'If high substitution (80%+): serfdom merely changed legal form, becomes piton classification. If low substitution (20%-): abolition genuinely freed peasants, scaffold perspective confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_abolition_substitution, empirical, 'Substitution of serfdom extraction through post-abolition mechanisms').

omega_variable(
    peasant_organization_constraint,
    'Why did serfdom''s extraction capacity remain stable despite peasant numerical majority? Could serfs have coordinated to overturn the system absent external reform?',
    'Analysis of peasant revolts and their outcomes; comparison of regions with vs without serf-led insurrections; study of organizational barriers; examination of counter-evidence cases',
    'If coordination impossible: suppression is high enough that trapped exit correctly describes serfs. If coordination was prevented: highlights that extraction specifically targets organizing capacity (higher suppression than base extraction alone would suggest).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasant_organization_constraint, empirical, 'Structural capacity for serf-coordinated resistance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(serfdom_legal_reinforcement, 0, 750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(serf_tr_t0, serfdom_legal_reinforcement, theater_ratio, 0, 0.4).
narrative_ontology:measurement(serf_tr_t250, serfdom_legal_reinforcement, theater_ratio, 250, 0.55).
narrative_ontology:measurement(serf_tr_t500, serfdom_legal_reinforcement, theater_ratio, 500, 0.65).
narrative_ontology:measurement(serf_tr_t750, serfdom_legal_reinforcement, theater_ratio, 750, 0.75).

% Extraction over time
narrative_ontology:measurement(serf_be_t0, serfdom_legal_reinforcement, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(serf_be_t250, serfdom_legal_reinforcement, base_extractiveness, 250, 0.68).
narrative_ontology:measurement(serf_be_t500, serfdom_legal_reinforcement, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(serf_be_t750, serfdom_legal_reinforcement, base_extractiveness, 750, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(serfdom_legal_reinforcement, enforcement_mechanism).
narrative_ontology:affects_constraint(serfdom_legal_reinforcement, feudal_obligation_reciprocity).
narrative_ontology:affects_constraint(serfdom_legal_reinforcement, peasant_revolt_suppression).
narrative_ontology:affects_constraint(serfdom_legal_reinforcement, post_abolition_sharecropping).

% DUAL FORMULATION NOTE:
% Serfdom legal reinforcement is the primary extraction mechanism. It structurally enables and depends on feudal_obligation_reciprocity (the reciprocal duties between nobility and monarchy) and peasant_revolt_suppression (the legal/military apparatus preventing organized resistance). Post-abolition sharecropping represents the constraint's transformation into substitute form after legal abolition — not a separate constraint but a downstream manifestation of the same extraction mechanism operating through different legal cover.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(serfdom_legal_reinforcement, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
