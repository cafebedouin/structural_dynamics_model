% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty (Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   Federation membership presents a structural dilemma: the federal compact
 *   requires coordination of policy across member states (genuine collective
 *   action problem — solving free-rider dynamics, harmonizing standards,
 *   enabling trade), yet the sovereignty reading subordinates transnational
 *   mobility to national labor market protection. This constraint
 *   instantiates ONE reading of the contested federation_membership kernel.
 *   The sovereignty reading treats national authority over borders as
 *   non-negotiable, derives legitimacy from state retention of labor market
 *   control, and perceives free movement as a negotiable policy concession
 *   rather than a foundational right. The competing integration reading (a
 *   separate constraint story) subordinates border authority to supranational
 *   mobility rights and identifies extraction as the primary structural
 *   feature. This story covers ONLY the sovereignty reading: the mechanism by
 *   which border control extracts from mobile citizens while enabling
 *   national labor market coordination.
 *
 * KEY AGENTS:
 *   - National Labor Market Protectors: Beneficiary (institutional/arbitrage) — domestic constituencies and state actors benefit from border control that prevents wage/employment competition; perceive the mechanism as legitimate sovereignty expression
 *   - Mobile Citizens: Victim (powerless/trapped) — workers whose livelihoods depend on cross-border mobility face visa delays, work permit restrictions, residency penalties; cannot exit the federal framework without renouncing citizenship entirely
 *   - Border Control Authorities: Beneficiary (institutional/arbitrage) — state agencies retain administrative authority, resources, and legitimacy through border enforcement; experience the constraint as defining their mandate
 *   - Free Movement Advocacy Coalition: Organized (organized/constrained) — transnational labor unions, migration advocates, diaspora networks perceive the restriction as temporary scaffolding with a sunset mechanism
 *   - Regional Trade Bodies: Powerful but constrained (powerful/constrained) — interstate institutions coordinate across the constraint; experience both genuine coordination function and asymmetric extraction simultaneously
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes the structural dilemma: real coordination function + real extraction asymmetry = genuine tangled_rope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.58).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.62).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '23a1d1ca-8f72-4771-b2bd-b0b6a18951db').
narrative_ontology:cs_kernel_codification('23a1d1ca-8f72-4771-b2bd-b0b6a18951db', formalized).
narrative_ontology:cs_authority_grounding('23a1d1ca-8f72-4771-b2bd-b0b6a18951db', extraction).
narrative_ontology:cs_interpretation_layer_present('23a1d1ca-8f72-4771-b2bd-b0b6a18951db').
narrative_ontology:cs_reading_relation('23a1d1ca-8f72-4771-b2bd-b0b6a18951db', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('23a1d1ca-8f72-4771-b2bd-b0b6a18951db', foundational, national_labor_market_protection_foundational).
narrative_ontology:cs_axiom_status(national_labor_market_protection_foundational, holdable).
narrative_ontology:cs_axiom_grounding('23a1d1ca-8f72-4771-b2bd-b0b6a18951db', national_labor_market_protection_foundational, instrumental).
narrative_ontology:cs_axiom('23a1d1ca-8f72-4771-b2bd-b0b6a18951db', foundational, border_authority_non_negotiable).
narrative_ontology:cs_axiom_status(border_authority_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('23a1d1ca-8f72-4771-b2bd-b0b6a18951db', border_authority_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('23a1d1ca-8f72-4771-b2bd-b0b6a18951db', retained_national_authority_over_labor_markets).
narrative_ontology:cs_drift_state('23a1d1ca-8f72-4771-b2bd-b0b6a18951db', contemporary_migration_pressure_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('23a1d1ca-8f72-4771-b2bd-b0b6a18951db', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_labor_market_protectors).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, border_control_authorities).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, transnational_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOBILE CITIZEN (SNARE) — Citizens whose livelihoods depend on cross-border mobility experience the federation's border control regime as pure extraction. The suppression is structural: visa delays, work permit restrictions, residency penalties. Exit is unavailable — the citizen cannot choose not to be federated; withdrawal requires renouncing citizenship entirely. The coordination function (free movement as policy) is negotiable, not foundational, making extraction the primary structural feature from this perspective.
constraint_indexing:constraint_classification(federation_membership__sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: NATIONAL LABOR MARKET PROTECTORS (ROPE) — State actors and domestic labor constituencies perceive the federation as a coordination mechanism solving a genuine collective action problem: preventing wage/employment competition across borders while maintaining federal institutional benefits. Border control is experienced as the legitimate expression of national sovereignty — retained authority to manage labor markets. Low suppression from this perspective because the mechanism is perceived as justified, not coercive.
constraint_indexing:constraint_classification(federation_membership__sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL TRADE BODIES (TANGLED ROPE) — Interstate bodies experience the tension directly: they coordinate trade, investment, and infrastructure (genuine coordination function) while simultaneously enforcing border barriers that extract from mobile citizens and restrict labor arbitrage (asymmetric extraction). High power but constrained exit — members are locked into bilateral dependencies. The constraint both enables and restricts their operation.
constraint_indexing:constraint_classification(federation_membership__sovereignty_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: FREE MOVEMENT ADVOCACY COALITION (SCAFFOLD) — Organized actors (migration advocates, diaspora networks, transnational labor unions) see the mobility restriction as temporary institutional scaffolding: a transitional phase where states retain border authority pending convergence of wage levels and labor standards that would eliminate demand for protection. Scaffold derives from the coalition's perception of a sunset mechanism — as inequality decreases, the sovereignty-based restriction loses legitimacy. Constrained by the incumbent regulatory regime; they have agency and see a clear transition pathway.
constraint_indexing:constraint_classification(federation_membership__sovereignty_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: LEGACY BORDER ADMINISTRATION (PITON) — The border control apparatus itself persists largely through institutional inertia. Modern travel technology, digital identity systems, and EU-style open borders demonstrate that the enforcement infrastructure is theater — maintained because dismantling existing state capacity is politically costly, not because the border control function is optimized. The institutional routine persists despite degraded functionality. Theater ratio reflects the gap between stated border security outcomes and actual enforcement capacity.
constraint_indexing:constraint_classification(federation_membership__sovereignty_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The civilizational view recognizes the structure as a genuine dilemma: free movement coordination (real function) vs. national labor market protection (real extraction asymmetry). Both functions are operant. The constraint is neither pure coordination nor pure extraction but genuinely hybrid. From this reading, the sovereignty principle (retained border authority as non-negotiable) creates the asymmetry — subordinating mobile citizens' interests to national constituencies' protection.
constraint_indexing:constraint_classification(federation_membership__sovereignty_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federation_membership__sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federation_membership__sovereignty_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federation_membership__sovereignty_reading, TR),
    TR >= 0.70.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The sovereignty reading creates asymmetric extraction from mobile citizens (visa delays, work permit delays, employment discrimination) that exceeds the coordination function's legitimate cost. However, the constraint is not pure extraction because it does solve a real collective action problem: preventing downward wage spirals and labor market race-to-bottom dynamics. The measurement trajectory (0.35 → 0.48 → 0.58) reflects accumulating extraction as border enforcement capacity has intensified and mobility pressure has increased, while the coordination benefit has remained stable. Suppression (0.62): High. Mobile citizens face structural barriers: visa/work permit administrative delay (2-12 months), residency restrictions, employment discrimination, deportation risk, exclusion from social entitlements during transition periods. These are not primarily economic barriers (which would be constrained) but structural legal barriers (which approach trapped). The measurement trajectory (0.42 → 0.52 → 0.62) reflects hardening border enforcement, particularly as migration pressure increased. Theater ratio (0.48): Moderate. The sovereignty reading includes performative elements — border security rhetoric exceeding actual enforcement capacity, ceremonial sovereignty affirmations — but the border control function is not primarily theatrical. Unlike legacy border administration (piton perspective), the sovereignty reading's enforcement apparatus is actively legitimated and regularly deployed. The moderate theater ratio reflects that the suppression mechanism is partly performative narrative (sovereignty rhetoric) and partly operational (actual mobility restrictions), without either dominating.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty reading naturalizes border authority in ways the integration reading explicitly rejects. A beneficiary perceives rope (coordination); a victim perceives snare (extraction); the analytical observer recognizes both structures coexist in the same institutional mechanism. The gap is not merely about disagreement on values but about whether the border mechanism's legitimacy is foundational (sovereignty reading) or instrumental (integration reading). This is the kernel-level disagreement that generates separate constraint stories with different ε values, different beneficiary/victim structures, and different primary types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for the sovereignty reading: national labor market protectors benefit from border control and have low cost to participate (institutional actors with arbitrage options can choose enforcement intensity) → d ≈ 0.10 → f(d) ≈ -0.05 → low/negative χ. Mobile citizens bear full cost and cannot exit (trapped boundary crossing) → d ≈ 0.92 → f(d) ≈ 1.32 → high χ. The regional trade bodies occupy middle ground: they benefit from federal coordination (d ≈ 0.35) but are constrained by member dependencies → χ ≈ 0.50, producing tangled_rope rather than rope. The analytical observer (d ≈ 0.72, canonical analytical directionality) experiences χ ≈ 0.80 (scope modifier 1.1 applied), correctly classifying the constraint as tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   SOVEREIGNTY READING MANDATROPHY: The constraint avoids mandatrophy by recognizing the genuine coordination function — preventing labor market race-to-bottom, enabling wage/benefit stability, reducing long-term volatility. The extraction is real (asymmetric burden on mobile citizens) but not the only structural feature. The tangled_rope classification captures the simultaneity: the mechanism both coordinates labor supply across borders AND extracts from those whose livelihoods depend on mobility. The beneficiaries' perception of legitimate sovereignty (rope from their perspective) does not eliminate the victims' experience of extraction (snare from their perspective). The mandatrophy is resolved by acknowledging that the constraint is properly classified as tangled_rope FROM the analytical frame — it has both real coordination and real extraction, and neither feature eliminates the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_integration_empirical,
    'Is national border authority empirically necessary for labor market stability, or do alternative coordination mechanisms (wage/benefit harmonization, portable entitlements, skills recognition treaties) achieve equivalent outcomes with lower extraction?',
    'Comparative analysis of labor market volatility, wage inequality, and employment stability in open-border regimes (EU post-Schengen) vs. restricted-border regimes; measurement of welfare outcomes for mobile citizens under each regime',
    'If alternative mechanisms suffice: sovereignty reading''s suppression (0.62) is unjustified; constraint reclassifies toward rope or snare depending on whether beneficiaries genuinely need the borders. If borders are empirically necessary: extraction is justified coordination cost; constraint remains tangled_rope but with lower perceived unfairness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_empirical, empirical, 'Whether national border authority is empirically necessary for labor market stability').

omega_variable(
    citizenship_exit_feasibility,
    'Can a mobile citizen genuinely exit the constraint by renouncing citizenship, or does the citizenship/non-citizenship boundary itself carry extraction (visa requirements, deportation risk, exclusion from services)?',
    'Legal and historical analysis of statelessness costs, alternative-citizenship acquisition timelines, service exclusions for non-citizens; comparison with trapped vs. constrained exit costs',
    'If citizenship exit is feasible at reasonable cost: exit_options should shift from trapped to constrained, changing mobile_citizens classification from snare to tangled_rope. If citizenship boundary itself extracts: confirms trapped classification and snare structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizenship_exit_feasibility, empirical, 'Whether citizenship exit is feasible for mobile citizens').

omega_variable(
    reading_decomposition_kernel_choice,
    'This constraint instantiates the SOVEREIGNTY READING of the federation_membership kernel. The sibling INTEGRATION READING would subordinate border authority to supranational mobility rights and declare extraction the primary structural feature. Are these readings genuinely coexistent (live positions held by different parties) or does one reading logically foreclose the other within a single consistent framework?',
    'Historical and institutional analysis: (a) Do states holding the sovereignty reading recognize the integration reading as a legitimate alternative that could be adopted by different member states, or is it treated as logically incompatible with federation membership itself? (b) Can a single federal framework hold both readings (e.g., EU opt-outs where some members retain border authority)? (c) Do the foundational axioms of each reading directly contradict or merely prioritize differently?',
    'If readings coexist: both remain live policy options; the constraint family includes two separate stories with different ε values and classifications. If one forecloses the other: the reading relationship should shift from coexists_with to forecloses; the kernel is under resolution pressure, not under stable contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_decomposition_kernel_choice, conceptual, 'Whether sovereignty and integration readings are coexistent or mutually foreclosing').

omega_variable(
    false_summit_natural_borders,
    'Does the sovereignty reading naturalize borders as inevitable features of the federal compact, or are they treated as chosen policy? If naturalized, what hidden beneficiaries might be adopting ''natural border necessity'' as a cover story for labor market protection extraction?',
    'Textual analysis of federal founding documents and sovereignty readings: are borders presented as natural consequences of federalism or as explicit treaty provisions? Historical comparison: how did similar federations treat borders at their founding vs. current periods?',
    'If borders are naturalized: false summit candidate — the constraint may classify as mountain (natural law) from sovereignty perspective but snare from analytical perspective. Engine FSM detection would flag. If borders are presented as chosen: no false summit; the tangled_rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_borders, conceptual, 'Whether borders are naturalized or treated as chosen policy in sovereignty reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_sov_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fed_sov_tr_t5, federation_membership__sovereignty_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(fed_sov_tr_t10, federation_membership__sovereignty_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(fed_sov_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fed_sov_be_t5, federation_membership__sovereignty_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fed_sov_be_t10, federation_membership__sovereignty_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fed_sov_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(fed_sov_su_t5, federation_membership__sovereignty_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(fed_sov_su_t10, federation_membership__sovereignty_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, labor_mobility_supply_shock).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, wage_convergence_pressure).

% DUAL FORMULATION NOTE:
% Federation membership decomposes into two structurally distinct constraints: (1) sovereignty_reading (ε=0.58, tangled_rope) treating border authority as foundational and mobility as negotiable policy; (2) integration_reading (ε=0.42, tangled_rope estimated, but check separately) treating mobility rights as foundational and border authority as contingent. The epsilon values differ because the observables differ: sovereignty reading measures extraction FROM mobile citizens TO labor market protectors; integration reading measures extraction FROM border authorities TO mobile citizens. These are inverse directionality framings of the same institutional mechanism. Both readings are live policy positions held by different member states (EU member states split on Schengen participation, migration policy, freedom of movement). Link the two stories via network.affects_constraints to enable the engine to compute cross-reading contamination and recognize the kernel-level contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
