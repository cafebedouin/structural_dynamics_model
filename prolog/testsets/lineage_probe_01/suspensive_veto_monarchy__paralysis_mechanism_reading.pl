% ============================================================================
% CONSTRAINT STORY: suspensive_veto_monarchy__paralysis_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_suspensive_veto_monarchy__paralysis_mechanism_reading, []).

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
 *   constraint_id: suspensive_veto_monarchy__paralysis_mechanism_reading
 *   human_readable: The Suspensive Veto as Paralysis Mechanism (1791-1792)
 *   domain: legal/constitutional/revolutionary
 *
 * SUMMARY:
 *   The suspensive veto of the 1791 Constitutional Monarchy was designed as a
 *   executive governor on legislative will — a delay mechanism preventing
 *   hasty action while preserving the legislature's ultimate authority after
 *   reconsideration. In theory, the veto was Montesquieu operationalized: the
 *   executive could say 'no, wait, reconsider,' but could not say 'no,
 *   never.' In practice, from 1791 to August 1792, the veto became a
 *   paralysis mechanism. The court faction, aligned with émigré resistance
 *   and refractory clergy, used the veto to block the Legislative Assembly's
 *   emergency legislation precisely when that legislation was most necessary:
 *   décrets on émigré suppression (1791), loyalty oaths for clergy (1791),
 *   war taxes (April 1792), conscription of the National Guard (1792). The
 *   veto's extractive force lay not in what it prevented the Assembly from
 *   doing in normal times, but in what it prevented the Assembly from doing
 *   in emergency. As war approached and émigrés massed on the borders with
 *   Prussian and Austrian armies, the Assembly became structurally trapped:
 *   it could legislate but could not enact; it could debate security measures
 *   but could not enforce them; it could recognize the emergency but could
 *   not respond to it. The paralysis at the constitutional level created the
 *   conditions for insurrection at the street level. The sans-culottes,
 *   seeing the constitutional mechanism fail to protect them from
 *   counter-revolutionary organizing, organized the storming of the Tuileries
 *   (August 10, 1792) and the suspension of the constitution. In this
 *   reading, the veto was the insurrection's recruiting sergeant — it
 *   recruited the people to revolution by proving that constitutional
 *   mechanisms alone could not defend the revolution.
 *
 * KEY AGENTS:
 *   - The Court Faction (Louis XVI, Émigrés, Refractory Clergy): Primary beneficiary (institutional/arbitrage) — uses the veto to block Assembly legislation and coordinate with foreign powers; gains time and strategic advantage during the emergency they help create
 *   - The Constitutional Settlement: Primary victim (powerless/trapped) — the mechanism designed to govern now governs nothing; unable to adapt or escape its own structure
 *   - The Revolutionary Legislative Assembly: Secondary victim (moderate/trapped) — legislatively empowered but executively blocked; power without effect; trapped in constitutional compliance as the emergency overflows the constitutional container
 *   - The Urban Popular Movement and Sans-Culottes: Tertiary victim and later agent (organized/constrained) — initially constrained by the constitutional mechanism's failure to secure their interests; later forced into insurrectionary action to solve what constitutional means could not
 *   - The Analytical Observer: Civilizational view (analytical/analytical) — risks seeing the paralysis as an immutable feature of mixed government rather than as a contingent weapon deployment by the court faction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(suspensive_veto_monarchy__paralysis_mechanism_reading, 0.78).
domain_priors:suppression_score(suspensive_veto_monarchy__paralysis_mechanism_reading, 0.82).
domain_priors:theater_ratio(suspensive_veto_monarchy__paralysis_mechanism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(suspensive_veto_monarchy__paralysis_mechanism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(suspensive_veto_monarchy__paralysis_mechanism_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(suspensive_veto_monarchy__paralysis_mechanism_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(suspensive_veto_monarchy__paralysis_mechanism_reading, snare).
narrative_ontology:human_readable(suspensive_veto_monarchy__paralysis_mechanism_reading, "The Suspensive Veto as Paralysis Mechanism (1791-1792)").
narrative_ontology:topic_domain(suspensive_veto_monarchy__paralysis_mechanism_reading, "legal/constitutional/revolutionary").

domain_priors:requires_active_enforcement(suspensive_veto_monarchy__paralysis_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(suspensive_veto_monarchy__paralysis_mechanism_reading, '02b95da3-81ce-4b7a-be53-588f83cea00c').
narrative_ontology:cs_kernel_codification('02b95da3-81ce-4b7a-be53-588f83cea00c', formalized).
narrative_ontology:cs_authority_grounding('02b95da3-81ce-4b7a-be53-588f83cea00c', lineage).
narrative_ontology:cs_interpretation_layer_present('02b95da3-81ce-4b7a-be53-588f83cea00c').
narrative_ontology:cs_reading_relation('02b95da3-81ce-4b7a-be53-588f83cea00c', suspensive_veto_monarchy__constitutional_monarchy_design_reading, coexists_with).
narrative_ontology:cs_reading_relation('02b95da3-81ce-4b7a-be53-588f83cea00c', suspensive_veto_monarchy__varennes_broken_trust_reading, influences).
narrative_ontology:cs_axiom('02b95da3-81ce-4b7a-be53-588f83cea00c', foundational, veto_deployment_is_deliberate_strategy).
narrative_ontology:cs_axiom_status(veto_deployment_is_deliberate_strategy, holdable).
narrative_ontology:cs_axiom_grounding('02b95da3-81ce-4b7a-be53-588f83cea00c', veto_deployment_is_deliberate_strategy, empirically_contingent).
narrative_ontology:cs_axiom('02b95da3-81ce-4b7a-be53-588f83cea00c', secondary, constitutional_mechanism_can_be_weaponized).
narrative_ontology:cs_axiom_status(constitutional_mechanism_can_be_weaponized, holdable).
narrative_ontology:cs_axiom_grounding('02b95da3-81ce-4b7a-be53-588f83cea00c', constitutional_mechanism_can_be_weaponized, instrumental).
narrative_ontology:cs_reference_frame('02b95da3-81ce-4b7a-be53-588f83cea00c', legitimate_executive_review_via_suspensive_veto).
narrative_ontology:cs_drift_state('02b95da3-81ce-4b7a-be53-588f83cea00c', april_1792_war_and_emigre_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('02b95da3-81ce-4b7a-be53-588f83cea00c', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(suspensive_veto_monarchy__paralysis_mechanism_reading, suspensive_veto_monarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(suspensive_veto_monarchy__paralysis_mechanism_reading, court_faction).
narrative_ontology:constraint_beneficiary(suspensive_veto_monarchy__paralysis_mechanism_reading, aristocratic_resistance).
narrative_ontology:constraint_victim(suspensive_veto_monarchy__paralysis_mechanism_reading, constitutional_settlement).
narrative_ontology:constraint_victim(suspensive_veto_monarchy__paralysis_mechanism_reading, revolutionary_legislative_assembly).
narrative_ontology:constraint_victim(suspensive_veto_monarchy__paralysis_mechanism_reading, urban_popular_movement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL SETTLEMENT (SNARE) — The settlement cannot exit the mechanism it created. The suspensive veto, designed as a balanced governor, becomes a deadlock lock. Emergency legislation on émigré suppression and refractory priests cannot pass. The Legislative Assembly has no structural escape from the veto's paralysis at the moment of maximum need (war in 1792). The victim is the constitutional framework itself — it dies from its own mechanism.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__paralysis_mechanism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REVOLUTIONARY LEGISLATIVE ASSEMBLY (SNARE) — Trapped in the constitutional mechanism they swore to uphold. Requires executive concurrence (via the vetoed deputy-king's signature) to pass legislation on émigré nobles and refractory clergy — precisely the groups whose destabilization creates the emergency they are trying to legislate against. As war approaches (April 1792), the veto becomes a weapon: vetoes on war taxes, émigré conscription, loyalty requirements. The Assembly experiences maximum extraction — power without effect.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__paralysis_mechanism_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: COURT FACTION (ROPE) — Experiences the veto as a coordination mechanism: the formal constitutional right to review and delay legislation. From the court's internal logic, the veto is not extraction but legitimate executive governance. The court uses the veto to coordinate with foreign powers (Austria, Prussia) by preventing the Assembly from legislating internal security measures. The veto enables the court's arbitrage: they can delay, signal to emigrés and foreign allies, and benefit from the resulting instability.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__paralysis_mechanism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: URBAN POPULAR MOVEMENT (TANGLED ROPE) — Organized but constrained. The sans-culottes benefit from the Assembly's legislative language on citizenship and labor (coordination), but are extracted from by the veto mechanism that prevents security legislation against émigré organizing and counter-revolutionary clergy (asymmetric harm). The veto creates the emergency the sans-culottes must solve by insurrection. The paralysis at the constitutional level forces the paralyzed agents to organize extra-constitutional action — which becomes the recruiting sergeant for August 10, 1792.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__paralysis_mechanism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL DESIGN (PITON) — From a generational perspective, the constitutional mechanism persists through performative compliance: legislators go through the formal motions of submission to the veto even as they recognize its paralytic function. The theater ratio (0.55) reflects that the formal constitutional procedure still commands rhetorical respect while being structurally inert. The mechanism that was supposed to prevent tyranny by slowing arbitrary will instead enables tyranny of stasis by preventing any will from acting.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__paralysis_mechanism_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, constitutional deadlock is an irreducible property of mixed government with genuinely separated powers and no supreme adjudicator. Any system where two coordinate powers must agree will produce deadlock when they diverge fundamentally. This perspective sees the paralysis as a necessary consequence of the constitutional structure itself — not a contingent failure but an immutable feature of the design. However, the structural data reveals this as a false summit: the actual constraint is not the deadlock mechanism but the emergency that forces the deadlock into lethality.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__paralysis_mechanism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suspensive_veto_monarchy__paralysis_mechanism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(suspensive_veto_monarchy__paralysis_mechanism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suspensive_veto_monarchy__paralysis_mechanism_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(suspensive_veto_monarchy__paralysis_mechanism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(suspensive_veto_monarchy__paralysis_mechanism_reading, TR),
    TR >= 0.70.

:- end_tests(suspensive_veto_monarchy__paralysis_mechanism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The veto's extraction lies in its power to prevent emergency legislation at precisely the moment when such legislation is most necessary. The Assembly is legislatively supreme — it can pass any law it votes — but the veto strips that supremacy of effect whenever the executive chooses to refuse. The extraction escalates from 0.42 (September 1791, veto used but constitution still seems viable) to 0.78 (July-August 1792, veto has prevented all security legislation during an active war and border crisis). Suppression (0.82): Very high and rising. The Assembly cannot override the veto within the 1791 Constitution (the two-year suspensive period means the same veto can block the same law through the entire legislative session). The alternatives available to the Assembly are all suppressed: constitutional amendment (requires Assembly plus royal consent, which the crown will veto), executive action by decree (not permitted under the 1791 Constitution except in narrow emergency clauses), popular action (the Assembly is supposed to represent the people's will, so circumventing it delegitimizes both the people and the assembly). Theater ratio (0.55): Moderate. The constitutional procedures are still formally observed — the Assembly still petitions the crown for signature, the crown still formally considers and formally refuses. But by 1792, both sides know the procedure is largely performative; the real decision-making happens outside the constitutional frame (court-émigré-foreign-power coordination, sans-culottes mobilization, war pressures). Mandatrophy (resolved): This constraint resolves mandatrophy by showing that the 'mixed government' designed to balance powers produces not balance but mutual strangulation when the powers refuse to cooperate. The constitutional mechanism is doing exactly what it was designed to do — prevent single-actor tyranny — but in an emergency where coordination is more valuable than prevention, that mechanism becomes tyranny of the center.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. The court faction sees the veto as a legitimate coordination mechanism (Rope) — from their perspective, it is working exactly as intended, giving them time to maneuver. The Assembly sees it as paralyzing extraction (Snare) — power without effect. The sans-culottes see it as a tangled constraint (Tangled Rope) — it provides them with the rhetoric of constitutional rights but denies them the protection those rights promise. The constitutional mechanism itself is trapped in its own structure (Snare). Only the piton perspective (the mechanism persists through ritual) and the mountain perspective (deadlock is inevitable in mixed government) attempt to transcend the perspectival conflict. But the mountain perspective naturalizes what is actually a contingent deployment: the veto became paralytic only because the court faction chose to use it as a weapon rather than as a governor.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's relationship to the extraction flow. The court faction are beneficiaries with arbitrage options: they can use the veto to delay, signal to external allies, and wait for military intervention (low d ≈ 0.15). The Assembly are victims without exit: they are constitutionally bound to respect the veto and have no override mechanism (high d ≈ 0.92). The sans-culottes are victims with constrained exit: they could theoretically walk away from the revolution, but they perceive it as their only protection, and the veto's failure to protect them forces them toward the extra-constitutional exit of insurrection (d ≈ 0.85). The constitutional settlement itself has no agency and no exit (trapped, d ≈ 1.0). These directionality values produce the observed chi values when scaled by the beneficiary's lower power-to-victim ratio and the national scope.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES VIA READING CONTEST: This constraint demonstrates mandatrophy at the highest level — the dispute is not about classification (all perspectives agree the veto is highly extractive by 1792) but about the reading of the kernel that grounds legitimacy. The constitutional_monarchy_design_reading argues the veto was conceptually sound but failed under stress. The paralysis_mechanism_reading (this one) argues the veto was weaponized from the start. The varennes_broken_trust_reading argues Varennes destroyed the veto's legitimacy retroactively. The three readings produce different terminal states: design failure, weapons deployment, or breach of trust. No single classification resolves this because the dispute is over which reading of the original commitment (the 1791 Constitution) is correct. The engine's false-summit detector will flag the mountain perspective — it treats deadlock as natural law when it is actually a strategic choice by the court to weaponize the constitutional mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_as_coordination_vs_veto_as_weapon,
    'Is the suspensive veto functioning as a constitutional coordination mechanism (legitimate executive review) or as a covert weapon (deliberate paralysis coordinated with foreign enemies)?',
    'Documentary evidence of court intentions: correspondence between Louis XVI and émigré networks, Austrian intelligence reports, financial records of secret subsidies to refractory clergy. Timing analysis: were veto deployments clustered around moments of maximum threat to the crown?',
    'If coordination: the mechanism failed despite good faith, and the constitutional_monarchy_design_reading is correct — a design that worked in theory but not practice. If weapon: this reading (paralysis_mechanism_reading) is correct — the veto was consciously weaponized to create the emergency that justified the court''s appeal to foreign military intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_as_coordination_vs_veto_as_weapon, empirical, 'Whether veto was constitutional review or deliberate paralysis mechanism').

omega_variable(
    emergency_duration_threshold,
    'At what point did the veto''s paralysis cross from a design tension (acceptable constitutional friction) to an existential threat (emergency that overwrites the constitution)?',
    'Timeline of: (a) veto deployment, (b) destabilization events (émigré border crossings, refractory clergy organizing, war declaration), (c) popular mobilization, (d) constitutional collapse. Identify the causal sequence — did the veto-created paralysis precede and enable destabilization, or did independent events create the emergency context?',
    'If veto preceded destabilization: paralysis_mechanism_reading (this reading) stands — the veto''s extraction caused the emergency. If independent destabilization forced the emergency: constitutional_monarchy_design_reading gains support — the veto was a minor factor in larger systemic collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_duration_threshold, empirical, 'Causal sequence of veto paralysis and destabilization events').

omega_variable(
    alternative_constitutional_protocols,
    'Could the constitutional deadlock have been broken by available mechanisms (joint session of Assembly and royal ministers, constitutional amendment, executive use of emergency decrees under Article 10 of the 1791 Constitution)?',
    'Constitutional text analysis: examine unused protocols and their historical invocation patterns. Did the court deliberately avoid emergency protocols that would have required justification? Could the Assembly have used its own procedural powers to override or reframe the veto?',
    'If alternatives existed and were deliberately avoided: extraction is maximal — the court chose paralysis over available exit mechanisms. If alternatives did not exist: the constitutional mechanism itself was the binding constraint, supporting the mountain perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_constitutional_protocols, empirical, 'Whether constitutional override mechanisms were available and deliberately avoided').

omega_variable(
    reading_divergence_point,
    'Which reading of the suspensive veto kernel is true: the constitutional_monarchy_design_reading (the veto was a serious design that failed under stress), the paralysis_mechanism_reading (the veto was a weapon that created paralysis), or the varennes_broken_trust_reading (Varennes destroyed the veto''s legitimacy)?',
    'This is the core omega for kernel dispute resolution. All three readings are internally coherent but predict different causal chains. Evidence: (1) court intentions (documentary), (2) veto timing and targets (behavioral), (3) available alternatives and their non-use (structural), (4) popular perception of the veto pre- and post-Varennes (social), (5) comparative analysis of other constitutional monarchies and their veto usage (structural).',
    'Each reading produces a different terminal classification. Design reading → the constitution failed. Paralysis mechanism reading (this one) → the veto was weaponized. Varennes broken trust reading → the crown lost the right to veto. The three readings coexist in contemporary historical debate and foreclose different policy futures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_divergence_point, conceptual, 'Which reading of the suspensive veto kernel is true').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(suspensive_veto_monarchy__paralysis_mechanism_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(veto_para_theater_t0, suspensive_veto_monarchy__paralysis_mechanism_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(veto_para_theater_t6, suspensive_veto_monarchy__paralysis_mechanism_reading, theater_ratio, 6, 0.52).
narrative_ontology:measurement(veto_para_theater_t12, suspensive_veto_monarchy__paralysis_mechanism_reading, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(veto_para_extract_t0, suspensive_veto_monarchy__paralysis_mechanism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(veto_para_extract_t6, suspensive_veto_monarchy__paralysis_mechanism_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(veto_para_extract_t12, suspensive_veto_monarchy__paralysis_mechanism_reading, base_extractiveness, 12, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(veto_para_supp_t0, suspensive_veto_monarchy__paralysis_mechanism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(veto_para_supp_t6, suspensive_veto_monarchy__paralysis_mechanism_reading, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(veto_para_supp_t12, suspensive_veto_monarchy__paralysis_mechanism_reading, suppression_requirement, 12, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(suspensive_veto_monarchy__paralysis_mechanism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(suspensive_veto_monarchy__paralysis_mechanism_reading, suspensive_veto_monarchy__constitutional_monarchy_design_reading).
narrative_ontology:affects_constraint(suspensive_veto_monarchy__paralysis_mechanism_reading, suspensive_veto_monarchy__varennes_broken_trust_reading).
narrative_ontology:affects_constraint(suspensive_veto_monarchy__paralysis_mechanism_reading, royal_flight_varennes_delegitimization).
narrative_ontology:affects_constraint(suspensive_veto_monarchy__paralysis_mechanism_reading, august_10_1792_insurrection).

% DUAL FORMULATION NOTE:
% The suspensive veto kernel has three distinct readings, each with its own constraint story and its own ε value. The design reading treats the veto as a coordination mechanism that failed (lower ε). The paralysis mechanism reading (this one) treats the veto as a weapon deployed to create deadlock (higher ε, 0.78). The varennes reading treats the veto as already delegitimized by the king's flight (intermediate ε). All three stories are linked via the kernel network: they contest the interpretation of the same constitutional commitment. The extractiveness escalates across the family: constitutional_monarchy_design_reading (ε ≈ 0.35) → suspensive_veto_monarchy__paralysis_mechanism_reading (ε = 0.78) → the insurrection that resulted (ε ≈ 0.90 once the mechanism has failed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(suspensive_veto_monarchy__paralysis_mechanism_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
