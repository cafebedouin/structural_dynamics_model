% ============================================================================
% CONSTRAINT STORY: nato_alliance_credibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nato_alliance_credibility, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: nato_alliance_credibility
 *   human_readable: NATO Alliance Credibility Constraint
 *   domain: geopolitical/institutional_coordination
 *
 * SUMMARY:
 *   The NATO alliance credibility constraint operates as a mixed
 *   coordination-extraction mechanism that varies dramatically across member
 *   perspectives. From the institutional view of the US security
 *   establishment, NATO is a coordination solution enabling distributed
 *   burden-sharing for European security and global power projection. From
 *   the view of Eastern European members, it is an extractive mechanism that
 *   locks vulnerable states into permanent military subordination justified
 *   by credibility requirements. From the Western European perspective, it
 *   presents a hybrid: genuine coordination benefit (collective deterrence)
 *   coupled with asymmetric extraction (forced defense spending levels,
 *   strategic deference to US preferences). The constraint's theater ratio
 *   (0.58) reflects significant performative content: NATO summit rituals,
 *   unified declaratory statements, and symbolic military exercises mask
 *   underlying burden-sharing disputes and strategic divergences. The
 *   extractiveness value (0.58) reflects the constraint's strong net
 *   extraction from dependent members, moderate extraction from Western
 *   European states, and net benefit flow to the US security establishment.
 *   The constraint exhibits characteristics of both Tangled Rope (mixed
 *   coordination and extraction) and Snare (for powerless dependent members),
 *   with a Scaffold dimension emerging as organized European autonomy
 *   initiatives build alternative security pathways that may eventually
 *   render NATO's monopoly on credible deterrence obsolete.
 *
 * KEY AGENTS:
 *   - US Security Establishment: Primary beneficiary (institutional/arbitrage) — captures power projection capabilities, intelligence integration benefits, and force positioning at distributed cost
 *   - Eastern European NATO Members: Primary victims (powerless/trapped) — structurally immobile due to Russian proximity and limited independent military capability; extract maximum value from dependence
 *   - Western European States: Secondary victims/mixed (powerful/constrained) — face significant but surmountable exit costs; experience genuine coordination benefit alongside extraction
 *   - European Strategic Autonomy Movement: Organized alternative (organized/constrained) — building parallel security coordination pathways (PESCO, European Defense Fund) with implicit sunset as capabilities mature
 *   - NATO Cold War Institution: Institutional inertia actor (institutional/arbitrage) — maintains alliance structure through bureaucratic persistence despite functional fit degradation; high theater ratio
 *   - Post-Soviet Non-Aligned States: Identity-locked agents (moderate/identity_locked) — structurally mobile but identity-fused with Western alignment, preventing recognition of alternatives
 *   - Analytical Observer: Civilizational frame (analytical/analytical) — risks naturalizing contingent Cold War institutional arrangements as immutable security law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nato_alliance_credibility, 0.58).
domain_priors:suppression_score(nato_alliance_credibility, 0.65).
domain_priors:theater_ratio(nato_alliance_credibility, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nato_alliance_credibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(nato_alliance_credibility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nato_alliance_credibility, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nato_alliance_credibility, tangled_rope).
narrative_ontology:human_readable(nato_alliance_credibility, "NATO Alliance Credibility Constraint").
narrative_ontology:topic_domain(nato_alliance_credibility, "geopolitical/institutional_coordination").

domain_priors:requires_active_enforcement(nato_alliance_credibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nato_alliance_credibility, us_security_establishment).
narrative_ontology:constraint_beneficiary(nato_alliance_credibility, eastern_european_nato_members).
narrative_ontology:constraint_victim(nato_alliance_credibility, western_european_strategic_autonomy).
narrative_ontology:constraint_victim(nato_alliance_credibility, non_aligned_states).
narrative_ontology:constraint_victim(nato_alliance_credibility, fiscal_burden_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EASTERN EUROPEAN NATO MEMBER (SNARE) — Structurally trapped by geographic proximity to Russia and lack of independent military capability. Exit costs are catastrophic (invasion risk, economic isolation). NATO credibility constraint becomes extractive: dependent members must maintain high defense spending, accept US force positioning, and subordinate strategic autonomy to maintain the alliance's credibility for their own survival. Maximum extraction from structurally immobile agent.
constraint_indexing:constraint_classification(nato_alliance_credibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WESTERN EUROPEAN STATE (TANGLED ROPE) — Faces real but surmountable costs to NATO exit (reputational damage, loss of collective defense, economic friction with US). Genuine coordination function: NATO provides collective deterrence against Russian expansion that no single European state could achieve alone. But also extraction: must maintain high defense spending levels, defer strategic decisions to US preferences, and tolerate US nuclear presence. Mixed coordination and asymmetric extraction.
constraint_indexing:constraint_classification(nato_alliance_credibility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US SECURITY ESTABLISHMENT (ROPE) — Benefits from NATO as coordination mechanism for power projection, intelligence sharing, and burden-sharing in European security. Experiences constraint as coordination: alliance enables US to maintain global strategic posture with distributed costs. Primary beneficiary with institutional power and arbitrage exit options (can unilaterally withdraw or restructure). Positive directionality flow.
constraint_indexing:constraint_classification(nato_alliance_credibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EUROPEAN STRATEGIC AUTONOMY MOVEMENT (SCAFFOLD) — Organized agents (EU strategic autonomy initiatives, France's strategic independence tradition, PESCO frameworks) see NATO credibility constraint as temporary. Building alternative European defense coordination pathways (PESCO, European Defense Fund, franco-german axis) with implicit sunset: as European military capability and integration deepen, NATO's monopoly on credible deterrence weakens. High suppression but declining over generational timescale as alternatives mature.
constraint_indexing:constraint_classification(nato_alliance_credibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: COLD WAR INSTITUTIONAL LEGACY (PITON) — NATO's institutional form (Article 5 collective defense, NATO command structure, integrated military planning) persists through institutional inertia rather than perfect functional fit to post-Cold War threat landscape. Theater component is significant: NATO summit rhetoric about 'united response' masks underlying burden-sharing disputes, capability gaps, and strategic divergence between nuclear-armed and non-nuclear members. The alliance performs unity (NATO expansion, annual exercises, unified statements) while managing chronic dysfunction. Theater ratio reflects gap between performative cohesion and actual strategic coordination.
constraint_indexing:constraint_classification(nato_alliance_credibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: POST-SOVIET NON-ALIGNED STATE (TANGLED ROPE) — Structurally mobile (could pursue alternative security arrangements, regional partnerships, pragmatic neutrality) but identity-locked into an internalized framework where NATO credibility and Western alignment are constitutive of national identity and civilizational belonging. The constraint is extractive (must maintain expensive military interoperability, cannot pursue alternative relationships without identity crisis) but perceived as coordination by the state leadership (believes NATO credibility is prerequisite for its own credibility and sovereignty). Identity fusion prevents recognition of structural mobility.
constraint_indexing:constraint_classification(nato_alliance_credibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — A naturalizing perspective treats NATO credibility as an immutable requirement of international security, framing the alliance as a natural law of geopolitics: 'collective defense is structurally necessary,' 'credibility is irreplaceable,' 'alliances are permanent.' However, the structural data (beneficiary/victim declarations, the existence of organized European autonomy movements, identity-locked rather than truly trapped membership) reveals this mountain as a false summit — NATO credibility is a contingent institutional arrangement, not an unchangeable fact of strategic geometry.
constraint_indexing:constraint_classification(nato_alliance_credibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nato_alliance_credibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nato_alliance_credibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nato_alliance_credibility, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nato_alliance_credibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nato_alliance_credibility, TR),
    TR >= 0.70.

:- end_tests(nato_alliance_credibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The constraint's extractive power has grown over the 20-year interval (from 0.35 to 0.58) as burden-sharing burdens have accumulated and NATO's functional monopoly on credible European deterrence has persisted despite the original threat (Soviet military) disappearing. The initial low extractiveness (0.35 post-Cold War) reflected genuine uncertainty about constraint necessity. As Russian threat perception regenerated (particularly post-2014 Ukraine, 2022 invasion), the extractiveness increased because the constraint shifted from optional cooperation to mandatory alliance maintenance. Current extractiveness reflects that dependent members extract maximum value (they pay high costs but avoid Russian coercion), while Western European states extract moderate value (coordination benefits partially offset extraction). Suppression (0.65): Moderate-high and stable. Structural barriers to exit include military interdependence, intelligence sharing dependencies, NATO command integration, and for dependent members, existential threat from exit. These barriers are real but not absolute — European autonomy movements show that alternative arrangements are structurally possible, just costly. Theater ratio (0.58): Moderate and increasing. NATO's performative content has increased over the interval as functional coordination has declined relative to symbolic maintenance of alliance unity. Annual summits produce declaratory statements about 'resolute unity' while members dispute burden-sharing levels. Military exercises serve coordination and theater functions simultaneously. The theater ratio reflects this hybrid: roughly equal parts genuine coordination requirement and performative display of cohesion.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival gap is driven by three structural factors: (1) Asymmetric threat exposure — dependent members face existential risk from alliance exit while beneficiaries face only reputational/strategic cost; (2) Identity fusion — post-Soviet members have internalized Western alignment as national identity, preventing recognition of alternatives even when alternatives exist; (3) Theater mask — NATO's performative cohesion obscures underlying strategic divergence and burden-sharing disputes. The Eastern European perspective (Snare: maximum extraction, zero alternatives) and the US perspective (Rope: pure coordination, positive benefit) emerge from the same constraint because one perspective sees structural immobility while the other sees beneficial power projection. The Western European perspective (Tangled Rope: mixed) reveals that both are partially correct — the constraint does coordinate collective defense AND extract disproportionate burden from some members. The Scaffold perspective (European autonomy) reveals that the Snare classification depends on specific current-period institution arrangements; as alternatives mature, the 'trap' becomes optional. The Piton perspective reveals that performative maintenance of alliance unity masks functional degradation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's position in the extraction flow. US security establishment: institutional power + arbitrage exit = low d (0.15), negative f(d), net benefit flow. Eastern European members: powerless + trapped = high d (0.95), high f(d) (1.42), maximum extraction experienced. Western European states: powerful + constrained exit (costly but possible) = moderate d (0.55), moderate f(d), mixed experience. European autonomy coalition: organized + constrained (building alternatives) = moderate d (0.40), low f(d), declining extraction as exit pathways develop. Post-Soviet identity-locked state: moderate power + identity_locked = elevated d (0.89) despite structurally mobile alternatives, because cognitive capture prevents exercising exit option. The directionality pipeline shows that identity-locked agents experience extraction equivalent to trapped agents despite having mobile exit options — the constraint's power comes from internalized identity, not external barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is: 'Is NATO credibility a pure coordination mechanism (Rope), a mixed hybrid (Tangled Rope), or pure extraction (Snare)?' The resolution emerges from perspectival decomposition. For US security establishment: NATO is coordination (Rope) — genuine benefit from distributed burden-sharing. For dependent members: NATO is extraction (Snare) — forced permanent military subordination justified by credibility maintenance. For Western European states: NATO is hybrid (Tangled Rope) — coordination function (deterrence) is real, but extraction (forced spending levels) is also real. For organized European autonomy movements: NATO is temporary (Scaffold) — the constraint exists because alternatives haven't matured, but building alternatives reveals the institution as contingent, not necessary. The mandatrophy resolves by recognizing that the constraint serves BOTH coordination AND extraction simultaneously, but in asymmetric doses to different members. The beneficiary (US) experiences coordination; the dependent victims experience extraction; the moderate-power members experience both. This is precisely the definition of Tangled Rope: genuine coordination function + asymmetric extraction. The false mountain perspective (viewing NATO as immutable strategic law) is revealed as such by the organized alternatives and identity-lock dynamics showing that the constraint is contingent, not necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_mechanism_ambiguity,
    'Is NATO credibility based on genuine integrated military capability or on symbolic/reputational commitment that could be satisfied through alternative mechanisms?',
    'Analysis of actual vs required integrated capabilities; assessment of whether European-only deterrent could achieve equivalent credibility; historical comparison of credibility maintained under different alliance structures (Warsaw Pact, bilateral treaties)',
    'If primarily reputational: European autonomy alternative is viable and Scaffold sunset is structural. If primarily capability-based: current NATO structure is less substitutable and extraction becomes more necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_mechanism_ambiguity, empirical, 'Whether NATO credibility depends on integrated capability or reputational commitment').

omega_variable(
    burden_sharing_fairness_threshold,
    'At what NATO defense spending distribution threshold does the constraint flip from coordination to pure extraction for Western European states?',
    'Quantitative analysis of defense spending elasticity; survey data on Western European willingness-to-pay at different NATO burden levels; modeling of symmetric alternative arrangements',
    'If threshold is already exceeded: Western European perspective should reclassify to Snare. If threshold is distant: current extraction is moderate and Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_sharing_fairness_threshold, empirical, 'Burden-sharing fairness threshold for Western European members').

omega_variable(
    russian_threat_endogeneity,
    'To what extent does NATO credibility constraint create the Russian threat perception it claims to respond to, versus responding to independently generated Russian threat?',
    'Counterfactual analysis of Russian military posture under hypothetical scenarios (NATO dissolution, European autonomy, US withdrawal); longitudinal assessment of Russian capability buildup correlation with NATO action sequences; Russian strategic documents analysis',
    'If highly endogenous (NATO actions drive Russian threat): the constraint manufactures its own justification and extraction is severable from coordination function. If exogenous: Russian threat is real prerequisite and extraction is tied to genuine security need.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(russian_threat_endogeneity, conceptual, 'Whether NATO credibility constraint endogenously creates Russian threat').

omega_variable(
    identity_lock_reversibility,
    'For post-Soviet non-aligned states with identity-locked NATO alignment, what identity-frame shifts would enable recognition of structural alternatives?',
    'Historical cases of national identity reframing (Finland''s identity shift, Yugoslavia''s multipolar alignment history); longitudinal tracking of generational attitude changes; analysis of elite identity construction narratives',
    'If reversible: identity lock is a temporary cognitive state and the constraint''s extractive power diminishes as identities evolve. If irreversible: constraint has achieved permanent internalization and future generations inherit it as natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Reversibility of post-Soviet identity-locked NATO alignment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nato_alliance_credibility, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(natocred_tr_t0, nato_alliance_credibility, theater_ratio, 0, 0.42).
narrative_ontology:measurement(natocred_tr_t10, nato_alliance_credibility, theater_ratio, 10, 0.52).
narrative_ontology:measurement(natocred_tr_t20, nato_alliance_credibility, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(natocred_be_t0, nato_alliance_credibility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(natocred_be_t10, nato_alliance_credibility, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(natocred_be_t20, nato_alliance_credibility, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nato_alliance_credibility, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nato_alliance_credibility, 0.18).
narrative_ontology:affects_constraint(nato_alliance_credibility, european_defense_autonomy).
narrative_ontology:affects_constraint(nato_alliance_credibility, russian_security_dilemma).
narrative_ontology:affects_constraint(nato_alliance_credibility, us_commitment_credibility).

% DUAL FORMULATION NOTE:
% NATO credibility constraint is upstream of specific alliance binding mechanisms (extended deterrence commitments, force positioning agreements, joint command structures) and downstream of broader geopolitical threat perception structures. The constraint family includes: (1) nato_alliance_credibility (this story, ε=0.58, Tangled Rope) — the core alliance maintenance mechanism; (2) european_defense_autonomy (ε=0.35, Scaffold) — emerging alternative coordination pathways; (3) russian_security_dilemma (ε=0.52, Tangled Rope) — endogenous threat generation that justifies credibility requirements; (4) us_commitment_credibility (ε=0.45, Tangled Rope) — the specific mechanism through which US guarantees are maintained. NATO credibility constraint affects all three downstream constraints: its maintenance or degradation changes the cost-benefit calculus for European autonomy, intensifies or relaxes the Russian security dilemma, and determines whether US commitment credibility can be maintained at acceptable cost.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nato_alliance_credibility, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
