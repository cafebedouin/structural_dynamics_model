% ============================================================================
% CONSTRAINT STORY: constitutional_consecration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_consecration, []).

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
 *   constraint_id: constitutional_consecration
 *   human_readable: The Proposition of Equality as a Binding Sacrifice
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The American constitutional project is founded on a proposition: 'all men
 *   are created equal' and endowed with unalienable rights. Lincoln's
 *   Gettysburg Address re-frames this as a binding sacrifice — the nation is
 *   consecrated to this proposition, and the Civil War is fought to preserve
 *   it. Yet the constraint operates as a structural hybrid: for the
 *   institutional authority that authored it, the proposition functions as
 *   pure coordination (a shared creed unifying diverse interests). For the
 *   enslaved, it functions as pure extraction (a promise invoked to
 *   legitimize their bondage while denying them legal standing to claim it).
 *   For those who take it seriously — abolitionists, civil rights advocates,
 *   constitutional bearers — it functions as tangled rope (enabling moral
 *   claims while constraining what institutional channels allow). Over time,
 *   as formal legal slavery ended but material extraction persisted through
 *   new mechanisms (mass incarceration, wage suppression, segregation), the
 *   constraint has degraded into theater: ceremonial invocation of equality
 *   in contexts (presidential inaugurations, civil rights commemorations)
 *   that perform legitimacy without enforcing material change. The constraint
 *   exhibits all six classification types from different structural
 *   positions, making it a diagnostic exemplar for how institutions bundle
 *   coordination and extraction under a shared creed.
 *
 * KEY AGENTS:
 *   - Founding Institutional Authority (slaveholding elite): Primary beneficiary (institutional/arbitrage) — captures coordination benefit while maintaining institutional flexibility to exclude
 *   - Enslaved and Subjugated Populations: Primary victim (powerless/trapped) — bound by the proposition while denied standing to claim it
 *   - Constitutional Bearer (abolitionists, civil rights advocates, nonconformist citizens): Secondary victim (moderate/constrained) — demands enforcement while bearing suppression costs
 *   - Abolitionist Movement: Organized agent (organized/constrained) — mobilizes moral claims; experiences constraint as enabling and extracting simultaneously
 *   - Liberal Democratic Myth-Maintenance System: Institutional actor (institutional/arbitrage) — performs legitimacy through ceremonial equality invocation while material extraction continues
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional project as inherent truth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_consecration, 0.38).
domain_priors:suppression_score(constitutional_consecration, 0.68).
domain_priors:theater_ratio(constitutional_consecration, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_consecration, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_consecration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(constitutional_consecration, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_consecration, tangled_rope).
narrative_ontology:human_readable(constitutional_consecration, "The Proposition of Equality as a Binding Sacrifice").
narrative_ontology:topic_domain(constitutional_consecration, "political/legal/constitutional").

domain_priors:requires_active_enforcement(constitutional_consecration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_consecration, founding_institutional_authority).
narrative_ontology:constraint_beneficiary(constitutional_consecration, national_legitimacy_apparatus).
narrative_ontology:constraint_victim(constitutional_consecration, enslaved_and_subjugated_populations).
narrative_ontology:constraint_victim(constitutional_consecration, constitutional_promise_bearer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED AND SUBJUGATED POPULATIONS (SNARE) — Bound by the proposition of equality yet denied legal personhood. The constraint operates as pure extraction: they are told they are included in 'all men are created equal' while systematically excluded from its protection. No exit option; no legal standing to claim the promise. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.54.
constraint_indexing:constraint_classification(constitutional_consecration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CONSTITUTIONAL BEARER / NONCONFORMIST CITIZEN (TANGLED ROPE) — Those who take the proposition seriously and demand its enforcement. Constrained by legal structures that deny standing and by social suppression of abolitionist and civil rights movements. The constraint both coordinates (it establishes a shared ideal they can invoke) and extracts (it requires them to bear the psychological and political cost of the gap between promise and practice). d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(constitutional_consecration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOUNDING INSTITUTIONAL AUTHORITY / SLAVEHOLDING ELITE (ROPE) — Benefits from the proposition as a coordination mechanism: it unifies diverse colonies under a shared creed while allowing institutional flexibility on enforcement. The slaveholders experience the constraint as pure coordination—they can invoke 'all men are created equal' in rhetoric while maintaining property rights in persons through arbitrage (constitutional silence on slavery = institutional flexibility). d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(constitutional_consecration, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ABOLITIONIST MOVEMENT (TANGLED ROPE) — Organized agents seeing the proposition as both enabling (it provides the vocabulary for moral claims) and constraining (it requires them to work through constitutional channels against institutions designed to resist change). The constraint extracts moral authority from their labor while offering only slow institutional change. d≈0.62, f(d)≈0.92, σ=1.0 → χ≈0.35.
constraint_indexing:constraint_classification(constitutional_consecration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LIBERAL DEMOCRATIC MYTH-MAINTENANCE (PITON) — The proposition has become largely performative in modern institutional context. Invoked in ceremonial contexts (inaugurations, civil rights commemorations) to maintain liberal legitimacy while material extraction persists through mass incarceration, wage suppression, and residential segregation. The constraint persists through institutional inertia—it no longer coordinates effectively, but abandoning it would expose the extraction. theater_ratio=0.65 reflects increasing theatricality. d≈0.05, f(d)≈-0.10, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(constitutional_consecration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a universal analytical perspective, the proposition could be seen as reflecting an immutable truth: the inherent equality of human beings is a fact of nature, not contingent on institutional recognition. However, base properties (ε=0.38, suppression=0.68, theater=0.65) contradict the mountain gates—the constraint is institutional, not natural. This perspective reveals a false summit: the naturalization of equality as 'self-evident truth' obscures the contingent institutional work required to instantiate it.
constraint_indexing:constraint_classification(constitutional_consecration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_consecration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_consecration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_consecration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_consecration, TR),
    TR >= 0.70.

:- end_tests(constitutional_consecration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint initially had high extraction potential (0.52) because it created a gap between promise and practice that could be indefinitely deferred. Over the interval, formal legal victories (abolition, civil rights legislation) reduced the raw extraction value—the gap narrowed in law if not in practice. However, modern extraction operates through new mechanisms (mass incarceration, segregation enforcement) that replace direct slavery, keeping base extractiveness moderate. Suppression (0.68): High. The constraint requires active suppression of claims for enforcement: legal suppression (denying standing, enforcing segregation), social suppression (violence against civil rights movements), and institutional suppression (police action, incarceration). This is not residual friction—it is structural to maintaining the extraction. Theater ratio (0.65): Moderate-high and rising. The constraint began with low theater (0.15)—direct slavery required no performance of legitimacy, it operated openly. As formal legal equality was achieved, theater increased: the constraint now operates through ceremonial invocation while material mechanisms persist. Modern equality rhetoric is substantially performative—it maintains liberal legitimacy while material extraction continues through proxies (criminal justice, wealth accumulation, educational access).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single institutional design can coordinate for some agents while extracting from others. The founding elite experience the proposition as rope: it unifies diverse colonies under a shared creed while leaving them free to define 'men' narrowly (enslaved persons excluded). Those subject to enslavement experience snare: they are told they are equal while held as property. Abolitionists experience tangled rope: the proposition enables their moral claims (they can invoke 'all men are created equal' against the institution) while constraining what pathways are available (courts will not recognize enslaved persons as parties, politics requires gradualism). The organized abolitionist movement experiences mixed coordination-extraction: their labor is extracted (moral authority is attributed to individual conscience rather than systemic change) while coordination occurs (movement creates solidarity and shared purpose). The modern institutional system experiences piton: the proposition persists ceremonially (inaugurations, commemorations) but no longer functions—it has been replaced by new extraction mechanisms (incarceration, segregation) that require less rhetorical cover. The analytical observer might see a mountain (equality as self-evident truth), but the structural data reveals this as a false summit—the constraint is entirely institutional, not natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding elite: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. The proposition costs them nothing (they define its scope), and provides coordination benefit. Enslaved persons: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. They cannot exit and bear full cost of the constraint's enforcement gap. Constitutional bearer: Victim + constrained → d≈0.70, f(d)≈1.08. Significant extraction but not maximal. They can exit (migrate, stop agitating) but at high cost. Abolitionist movement: Victim + constrained + organized → d≈0.62, f(d)≈0.92. Moderate extraction with some agency through collective action. Modern liberal system: Institutional + arbitrage → d≈0.05, f(d)≈-0.10. Piton classification comes from high theater gate, not from directionality—the system benefits from the constraint's existence (legitimacy) while its extraction mechanisms operate elsewhere (incarceration, segregation). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; the false summit detector should flag this.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sincerity_of_proposition,
    'Did the founders intend the proposition as a binding moral commitment or as rhetorical cover for institutional slavery?',
    'Comparative analysis of founding-era documents; examination of whether constitutional silence on slavery was accidental or strategic; study of founders'' private correspondence vs public rhetoric',
    'If sincere: constraint is mislabeled tangled_rope (contains genuine coordination aspiration). If cover: constraint is pure snare (rhetorical entrapment). Classification shifts from coordinated hypocrisy to malicious deception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_of_proposition, conceptual, 'Whether the proposition was sincere moral commitment or institutional cover').

omega_variable(
    enforcement_mechanism_sufficiency,
    'Does constitutional language alone enforce equality, or is the constraint''s power entirely dependent on organized social movements and external force (civil war, civil rights mobilization)?',
    'Historical analysis of enforcement pathways; counterfactual comparison of legal change rates with vs without organized pressure; examination of how constitutional language constrains vs enables suppression',
    'If language sufficient: constraint is primarily rope/mountain (self-enforcing coordination). If dependent on external force: constraint is primarily snare/tangled_rope (requires suppression of resistance). Determines whether institutional design enables or merely channels extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_sufficiency, empirical, 'Whether constitutional language alone enforces equality').

omega_variable(
    temporal_resolution_possibility,
    'Is the gap between proposition and practice inherent to large-scale institutional change, or does persistent non-enforcement indicate structural commitment to extraction?',
    'Longitudinal analysis of actual enforcement rates vs legal accessibility; measurement of suppression intensity over time; correlation between legal victories and material outcomes',
    'If inherent: constraint is scaffold with very long sunset (temporary coordination problem). If structural: constraint is piton (theater substitutes for function indefinitely). Determines whether the system is fixing or naturalizing inequality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_resolution_possibility, empirical, 'Whether enforcement gaps are temporary or structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_consecration, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(const_cons_tr_t0, constitutional_consecration, theater_ratio, 0, 0.15).
narrative_ontology:measurement(const_cons_tr_t40, constitutional_consecration, theater_ratio, 40, 0.42).
narrative_ontology:measurement(const_cons_tr_t80, constitutional_consecration, theater_ratio, 80, 0.65).

% Extraction over time
narrative_ontology:measurement(const_cons_be_t0, constitutional_consecration, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(const_cons_be_t40, constitutional_consecration, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(const_cons_be_t80, constitutional_consecration, base_extractiveness, 80, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_consecration, information_standard).
narrative_ontology:affects_constraint(constitutional_consecration, democratic_franchise_exclusion).
narrative_ontology:affects_constraint(constitutional_consecration, racial_property_accumulation_gap).
narrative_ontology:affects_constraint(constitutional_consecration, mass_incarceration_system).

% DUAL FORMULATION NOTE:
% The constitutional proposition of equality is distinct from its enforcement mechanisms. This constraint captures the structural gap between proposition and practice. Downstream constraints (franchise exclusion, property gaps, incarceration) represent specific institutional mechanisms through which extraction continues after formal legal equality. Each has its own ε value and structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_consecration, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
