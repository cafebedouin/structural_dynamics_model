% ============================================================================
% CONSTRAINT STORY: existential_coordination_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_existential_coordination_failure, []).

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
 *   constraint_id: existential_coordination_failure
 *   human_readable: Existential Coordination Failure
 *   domain: global_governance/existential_risk
 *
 * SUMMARY:
 *   Existential coordination failure emerges from a fundamental asymmetry:
 *   actors making present-day decisions bear trivial costs from catastrophic
 *   future risks, while bearing substantial costs from the coordination
 *   mechanisms required to prevent those risks. This creates a structural
 *   snare where individual rationality (defection on coordination) produces
 *   collective irrationality (civilizational extinction risk). The constraint
 *   exhibits all six DR types from different perspectives, revealing how
 *   institutional framing naturalizes what is actually an engineered
 *   incentive structure. From the perspective of future generations and the
 *   biosphere, the constraint is an inescapable extraction mechanism — they
 *   bear the full cost of coordination failure and have zero negotiating
 *   power. From the perspective of individual nations and firms, it appears
 *   as a simple coordination problem with obvious defection payoffs. From the
 *   perspective of global institutions, it is a genuine coordination
 *   challenge mixed with extraction (resource asymmetry, enforcement gaps,
 *   state veto). The theater ratio has risen over time as performative
 *   international agreements (Paris, BWC, NPT) substitute for actual
 *   coordination mechanisms, indicating Goodhart drift — the measure (treaty
 *   signature, emissions commitments) replaces the goal (actual emissions
 *   reduction, biosecurity prevention). The analytical observer risks seeing
 *   this as an inevitable tragedy of the commons — a mountain of collective
 *   action theory — but the structural data reveals it as a contingent
 *   institutional arrangement: the tragedy is produced by institutions that
 *   make defection individually optimal. Alternative institutional designs
 *   could restructure incentives, but their absence is naturalized rather
 *   than engineered.
 *
 * KEY AGENTS:
 *   - Future Generations & Biosphere: Primary victim (powerless/trapped) — no negotiating power, no exit option, bearing full cost of coordination failure across multiple existential risks
 *   - Individual Nations and Firms: Defector coalition (institutional/arbitrage) — benefit from defection on coordination (carbon emissions, pandemic research shortcuts, AI speed, nuclear deterrence); experience constraint as obstacle to optimization
 *   - Global Coordination Institutions: Hybrid actor (organized/constrained) — genuinely aim to coordinate; structurally constrained by nation-state sovereignty, enforcement gaps, and resource asymmetry
 *   - International Treaty Frameworks: Performative institutions (institutional/arbitrage) — maintain symbolic coordination role while lacking enforcement authority; degraded from functional coordination
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable laws of collective action
 *   - Emerging Existential Risk Coalition: Nascent alternative pathway (powerful/mobile) — building coordination mechanisms outside traditional state sovereignty; attempting scaffold function with sunset pathway
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(existential_coordination_failure, 0.78).
domain_priors:suppression_score(existential_coordination_failure, 0.82).
domain_priors:theater_ratio(existential_coordination_failure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(existential_coordination_failure, extractiveness, 0.78).
narrative_ontology:constraint_metric(existential_coordination_failure, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(existential_coordination_failure, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(existential_coordination_failure, snare).
narrative_ontology:human_readable(existential_coordination_failure, "Existential Coordination Failure").
narrative_ontology:topic_domain(existential_coordination_failure, "global_governance/existential_risk").

domain_priors:requires_active_enforcement(existential_coordination_failure).
% --- Structural relationships ---
narrative_ontology:constraint_victim(existential_coordination_failure, future_generations).
narrative_ontology:constraint_victim(existential_coordination_failure, planetary_biosphere).
narrative_ontology:constraint_victim(existential_coordination_failure, human_species_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS / BIOSPHERE (SNARE) — No exit option. Bearing full cost of coordination failure across existential risks (climate, pandemics, AI, nuclear war, ecosystem collapse). Suppression is total: cannot negotiate, cannot organize, cannot influence present-day coordination mechanisms. Maximum experienced extraction.
constraint_indexing:constraint_classification(existential_coordination_failure, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL NATIONS AND FIRMS (ROPE) — Experience the constraint as a coordination problem, not extraction. Each actor benefits from defecting (carbon emissions, pandemic research shortcuts, AI competitive advantage, nuclear deterrence). Coordination mechanisms that require unified sacrifice appear as mere obstacles to individual optimization. Exit is available through defection or carve-outs. Net beneficiary perspective — immediate gains for defection outweigh civilizational risk.
constraint_indexing:constraint_classification(existential_coordination_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: GLOBAL COORDINATION INSTITUTIONS (TANGLED ROPE) — Possess both genuine coordination function (UN, IPCC, biosecurity forums) and extraction mechanism (sovereign states controlling institutions, capacity concentrated in wealthy nations, enforcement power asymmetric). Constrained by nation-state sovereignty and resource limitations. See both the necessity of coordination and the structural barriers to it — mixed experience of genuine benefit and systematic constraint.
constraint_indexing:constraint_classification(existential_coordination_failure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL TREATY FRAMEWORKS (PITON) — Performative rather than functional. Paris Agreement, Biological Weapons Convention, Nuclear Non-Proliferation Treaty operate with high theater ratio: compliance monitoring is weak, verification is limited, enforcement is absent. The treaties persist through institutional inertia and symbolic commitment rather than functional coordination. Degraded from their ostensible coordination purpose.
constraint_indexing:constraint_classification(existential_coordination_failure, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / TRAGEDY OF THE COMMONS (FALSE SUMMIT) — Risks naturalizing the coordination failure as an immutable law of collective action — 'rational actors cannot cooperate on diffuse costs.' But the structural data reveals this is not a mountain of physics; it is a snare of incentive asymmetry. The 'tragedy' is contingent on institutions that make defection rewarding. Alternative institutional designs (global taxation, AI governance, pandemic prevention) could restructure incentives, but their absence is naturalized as inevitable rather than engineered.
constraint_indexing:constraint_classification(existential_coordination_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: EMERGING EXISTENTIAL RISK COALITION (SCAFFOLD) — Organized coalitions (longtermism movement, biosecurity communities, AI safety researchers, climate commitment coalitions) are building alternative coordination pathways outside traditional state sovereignty. These coalitions have agency and see a sunset: as norms around existential risk shift, coordination mechanisms that bypass nation-state veto become more viable. Current suppression (constrained by state competition) may decrease as coordination norms mature. Scaffold classification reflects genuine coordination function with visible sunset pathway.
constraint_indexing:constraint_classification(existential_coordination_failure, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(existential_coordination_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(existential_coordination_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(existential_coordination_failure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(existential_coordination_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(existential_coordination_failure, TR),
    TR >= 0.70.

:- end_tests(existential_coordination_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high, rising over time. The constraint is the structural condition that allows present actors to ignore existential risks because bearing the costs of prevention would reduce their immediate payoffs. The extraction is from future agents to present agents. The rising trajectory reflects both increasing certainty that coordination is failing (climate commitments not met, biosecurity infrastructure degraded, AI governance delayed) and increasing sunk costs in defection (vested interests in carbon intensity, pandemic research shortcuts, AI competitive race). Suppression (0.82): Extremely high. Multiple suppression mechanisms: (1) Temporal suppression — future costs are discounted by standard economic models to near-zero present value. (2) Uncertainty suppression — low-probability, high-impact risks are cognitively discounted. (3) Institutional suppression — competing priorities at national level override global coordination. (4) Informational suppression — evidence of existential risk is actively disputed by those benefiting from defection. (5) Incentive suppression — coordination mechanisms that would reduce defection payoffs are absent by design. Theater ratio (0.68): High and rising. International treaty frameworks operate primarily through symbolic commitment and performative monitoring. Paris Agreement signatories set 'targets' with no enforcement mechanisms; biosecurity conventions lack real-time monitoring; nuclear agreements depend on voluntary verification. The increase from 0.48 to 0.68 reflects rising gap between rhetorical commitment and actual coordination — agreements accumulate without producing behavior change.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival distance between institutional defectors (Rope) and trapped future agents (Snare). The defector coalition genuinely perceives coordination as suboptimal; they are not lying or deceiving — they are rationally maximizing present payoffs under institutional structures that make defection rewarding. This is why no amount of moral suasion or information campaigns can resolve the constraint — the institutional incentive structure is not a belief problem but a payoff structure problem. The only resolution is institutional redesign (global taxation, AI governance enforcement, pandemic prevention investment). The scaffold perspective represents the attempt to build alternative institutions; its viability depends on whether norms shift fast enough to enable new coordination pathways before catastrophic risks materialize. The temporal dimension is crucial: as the constraint tightens (extractiveness rising from 0.55 to 0.78), the scaffold's sunset timeline compresses. Coalition formation accelerates under crisis pressure, but so does defector coalition entrenchment.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) reflects the agent's structural position in the defection-coordination payoff matrix. Defector coalition members experience low d because they benefit from the constraint (defection is rewarded). Global institutions experience moderate d because they genuinely want to coordinate but face structural barriers. Future generations experience maximum d because the entire extraction flows toward them — they bear costs of both coordination failure (risk materialization) and failed coordination attempts (sunk institutional costs). The piton perspective's d is low (institutional arbitrage) because the institutions benefit from maintaining symbolic coordination without actual enforcement. The scaffold perspective's d is moderate-to-low (powerful but constrained) because emerging coalitions have agency and exit paths but face resistance from entrenched defector institutions. The mountain perspective's d is analytically undefined (analytical observer) but risks computing as universal-scope d, which would be high — risking misclassification. The false summit detector should flag this as naturalizing contingent institutional design as universal law.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint is Snare under multiple perspectives (powerless/trapped, moderate/constrained view of institutional failures). The snare classification is not challenged by coordination narratives — those are rope perspectives from agents who benefit from defection. The mandatrophy resolves by recognizing that institutional power actors benefit from the defection equilibrium and will frame it as an inevitable coordination problem (tragedy of commons, mountain framing) rather than an engineered snare. The false summit (mountain perspective) is diagnostic: it reveals that 'inevitable' collective action failure is the beneficiary coalition's preferred framing. The constraint resolves to Snare because: (1) Future generations are trapped with no exit (Snare gate requires at least one victim with no exit — satisfied). (2) Present institutions extract present value while externalizing future costs (extraction mechanism for victims). (3) Suppression is extremely high (0.82 exceeds Snare threshold 0.60). (4) Effective extraction χ scales the base ε = 0.78 by f(d) for trapped victims, producing χ ≥ 0.66. The snare classification stands. The mandatrophy is resolved by distinguishing genuine coordination problems (Rope) from engineered defection traps (Snare). The defector coalition's Rope perspective is their genuine experience; the future victim's Snare perspective is the structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_mechanism_adequacy,
    'Are current international institutions (UN, IPCC, WHO, UNODA) genuinely incapable of existential risk coordination, or is their failure a function of their design and enforcement authority?',
    'Counterfactual institutional analysis: what enforcement mechanisms, funding structures, or decision-making rules would enable these institutions to function as coordination mechanisms rather than performative bodies?',
    'If incapable by nature: mountain perspective (tragedy of commons) is justified. If incapable by design: snare classification confirmed — the constraint is engineered, not inevitable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_mechanism_adequacy, conceptual, 'Whether coordination failure is inherent or engineered').

omega_variable(
    defection_incentive_structure,
    'What incentive magnitude would make universal existential risk coordination individually rational (not requiring altruism)?',
    'Economic modeling of risk-adjusted payoff matrices across defection vs coordination scenarios; sensitivity analysis on discount rates, probability estimates, and value assignments to existential outcomes',
    'If small incentive shift sufficient: constraint is Tangled Rope at lower suppression. If large shift required: constraint is Snare — defection is locked in by payoff structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defection_incentive_structure, empirical, 'Magnitude of incentive shift needed for rational coordination').

omega_variable(
    catastrophic_failure_trigger_threshold,
    'At what point of civilizational stress (temperature, pandemic severity, AI capability, weapons proliferation) does coordination failure itself become individually rational and coordination becomes extractive (powerful agents suppress evidence of risk)?',
    'Historical analysis of civilizational crisis response; modeling of information suppression and denial during high-stress scenarios (pandemic, financial collapse, war)',
    'If threshold low: constraint is already transitioning to active suppression (Snare with rising suppression measure). If threshold high: current framing as ''coordination problem'' is accurate until crisis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophic_failure_trigger_threshold, empirical, 'Crisis threshold triggering transition to active suppression').

omega_variable(
    longtermism_identity_lock_mechanism,
    'Does the emerging longtermism coalition avoid becoming identity-locked around a particular risk prioritization (e.g., AI risk > climate > biosecurity), thereby replicating the coordination failure it aims to solve?',
    'Organizational dynamics analysis: coalition flexibility, cross-coalition dialogue, willingness to shift priorities; comparison to historical cause movements that became ideologically rigid',
    'If identity-locked: the scaffold is unsustainable — the coalition will fracture when risk profiles shift. If adaptive: scaffold classification confirmed, sunset is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(longtermism_identity_lock_mechanism, conceptual, 'Risk of coalition becoming identity-locked around particular existential risks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(existential_coordination_failure, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exis_tr_t0, existential_coordination_failure, theater_ratio, 0, 0.48).
narrative_ontology:measurement(exis_tr_t20, existential_coordination_failure, theater_ratio, 20, 0.6).
narrative_ontology:measurement(exis_tr_t40, existential_coordination_failure, theater_ratio, 40, 0.68).
narrative_ontology:measurement(exis_tr_t60, existential_coordination_failure, theater_ratio, 60, 0.74).

% Extraction over time
narrative_ontology:measurement(exis_be_t0, existential_coordination_failure, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(exis_be_t20, existential_coordination_failure, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(exis_be_t40, existential_coordination_failure, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(exis_be_t60, existential_coordination_failure, base_extractiveness, 60, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(existential_coordination_failure, global_infrastructure).
narrative_ontology:affects_constraint(existential_coordination_failure, climate_mitigation_freeriding).
narrative_ontology:affects_constraint(existential_coordination_failure, pandemic_prevention_underinvestment).
narrative_ontology:affects_constraint(existential_coordination_failure, ai_governance_race_dynamics).
narrative_ontology:affects_constraint(existential_coordination_failure, nuclear_deterrence_instability).
narrative_ontology:affects_constraint(existential_coordination_failure, biodiversity_collapse_coordination).

% DUAL FORMULATION NOTE:
% Existential coordination failure is the upstream constraint that structures incentives across all specific existential risks. Climate freeriding, pandemic prevention gaps, and AI governance races are downstream manifestations of the same coordination failure mechanism. Each downstream constraint has its own ε value and perspectives; all are linked by this structural constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(existential_coordination_failure, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
