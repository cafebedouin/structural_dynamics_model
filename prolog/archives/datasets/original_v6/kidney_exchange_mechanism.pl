% ============================================================================
% CONSTRAINT STORY: kidney_exchange_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kidney_exchange_mechanism, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kidney_exchange_mechanism
 *   human_readable: Kidney Exchange Mechanism
 *   domain: healthcare/organ_allocation
 *
 * SUMMARY:
 *   The kidney exchange mechanism solves a fundamental coordination problem
 *   in organ transplantation: patients with willing living donors who are
 *   immunologically incompatible cannot transplant directly, yet perfect
 *   matches may exist elsewhere in the donor population. By enabling chains
 *   and cycles of exchange — donor A's kidney goes to patient B whose donor
 *   C's kidney goes to patient A's original patient — the mechanism
 *   dramatically increases transplant access for incompatible pairs. Unlike
 *   most constraints analyzed in this system, kidney exchange is
 *   Pareto-improving: all participants (waitlisted patients, their donors,
 *   transplant programs, the public health system) are made better off by the
 *   constraint's existence. This pure coordination nature is reflected in the
 *   rope classification and low extractiveness (0.28). The mechanism's growth
 *   trajectory shows increasing extractiveness as scale increases
 *   computational complexity and information asymmetries, but the underlying
 *   function remains coordinative. No class of agent experiences the
 *   constraint as maximally extractive or trapping — the modality is
 *   coordination with constraints, not exploitation.
 *
 * KEY AGENTS:
 *   - Waitlisted Patients with Incompatible Donors: Primary beneficiaries (powerless/constrained) — gain access to transplants who otherwise would wait years on dialysis; face constraint of algorithm-mediated matching
 *   - Altruistic Donors (Non-Directed): Primary beneficiaries (moderate/mobile) — gain confidence that their donation is well-allocated; can exit by choosing not to donate
 *   - Transplant Programs and Organ Procurement Organizations: Secondary beneficiaries (institutional/arbitrage) — increase transplant volume, improve outcomes, enhance institutional mission fulfillment
 *   - Incompatible Donor-Patient Pairs: Agents (moderate/constrained) — participate voluntarily in exchange; constrained by medical compatibility requirements but benefit from coordination
 *   - Medical Ethics and Policy Community: Organized observers (organized/constrained) — oversee fairness of mechanism; constrained by regulatory requirements but benefit from solution to coordination problem
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees pure coordination mechanism solving legitimate collective action problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kidney_exchange_mechanism, 0.28).
domain_priors:suppression_score(kidney_exchange_mechanism, 0.35).
domain_priors:theater_ratio(kidney_exchange_mechanism, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kidney_exchange_mechanism, extractiveness, 0.28).
narrative_ontology:constraint_metric(kidney_exchange_mechanism, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(kidney_exchange_mechanism, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kidney_exchange_mechanism, rope).
narrative_ontology:human_readable(kidney_exchange_mechanism, "Kidney Exchange Mechanism").
narrative_ontology:topic_domain(kidney_exchange_mechanism, "healthcare/organ_allocation").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kidney_exchange_mechanism, waitlisted_patients).
narrative_ontology:constraint_beneficiary(kidney_exchange_mechanism, transplant_surgeons).
narrative_ontology:constraint_beneficiary(kidney_exchange_mechanism, organ_procurement_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAITLISTED PATIENT (ROPE) — Patient with willing but immunologically incompatible living donor faces high costs to exit (remain on waitlist with years of dialysis, health deterioration) but the exchange mechanism offers genuine coordination benefit: access to a compatible kidney through chain exchange. Experiences the constraint as primarily coordinative with manageable constraints.
constraint_indexing:constraint_classification(kidney_exchange_mechanism, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: ALTRUISTIC DONOR (ROPE) — Willing to donate kidney but mechanism ensures their donation is not wasted on incompatible recipient. The constraint coordinates their altruistic intent with medical reality. Can exit by not donating (mobile option) but the exchange mechanism makes their choice genuinely beneficial. Low experienced extraction.
constraint_indexing:constraint_classification(kidney_exchange_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSPLANT PROGRAM (ROPE) — Institutional beneficiary that orchestrates exchanges. Experiences mechanism as coordination infrastructure that increases transplant volume and improves outcomes. No extraction experienced — gains operational efficiency and fulfills mission.
constraint_indexing:constraint_classification(kidney_exchange_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICAL ETHICS COMMUNITY (ROPE) — Organized agents (bioethics committees, transplant societies) view kidney exchange as resolving a genuine collective action problem (incompatibility) while respecting autonomy and fairness norms. Sees constraint as legitimate coordination mechanism with sunset logic as xenotransplantation or artificial organs mature.
constraint_indexing:constraint_classification(kidney_exchange_mechanism, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — At civilizational scale, kidney exchange is a pure coordination mechanism that solves the incompatibility problem with minimal coercive overhead. No agent is maximally trapped; no agent captures rents. The mechanism is transparent, regulated, and solves a genuine collective action problem. Classification as rope is stable across all observables.
constraint_indexing:constraint_classification(kidney_exchange_mechanism, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kidney_exchange_mechanism_tests).
:- end_tests(kidney_exchange_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate, increasing over time. Initial extractiveness near zero reflects the mechanism's Pareto-improving nature — all participants benefit from coordination that was previously impossible. The trajectory rising to 0.28 reflects secondary effects: as pools scale, algorithm complexity increases, and information asymmetries emerge between sophisticated programs and individual patients. Better-resourced transplant centers gain advantage in chain management; patients with educated advocates navigate matching better. However, the core extractiveness remains fundamentally coordinative rather than zero-sum. Suppression (0.35): Moderate. Barriers to participation include medical eligibility (not all patients/donors are compatible), geographic constraints (exchanges work best in regional or national pools), and timing coordination (all surgeries must occur synchronously). These are structural constraints on coordination, not coercive suppression. Patients can opt out and remain on waitlist; donors can decline participation. Theater ratio (0.25): Low. The mechanism is substantially transparent — algorithms for optimal matching are published, outcomes are tracked, and fairness criteria are explicit in policy. Some theater exists (waitlist prioritization involves opaque medical judgment) but the core coordination is functional rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between the waitlisted patient's constrained position (high cost to exit by remaining on dialysis waitlist) and the altruistic donor's mobile position (can exit freely by not participating). Both experience the constraint as rope (coordinative), but for different reasons. The patient is constrained by medical necessity and biological incompatibility; the donor is mobile but chooses participation because the coordination aligns with altruistic intent. The transplant program sees arbitrage: operational efficiency and mission fulfillment. The ethics community sees generational coordination: solving a permanent collective action problem while preserving autonomy norms. All perspectives converge on rope classification because the constraint genuinely solves a legitimate coordination problem without requiring coercion — the closest the system comes to extraction is the computational complexity that may emerge at scale, but this remains below the tangled_rope threshold (which requires both coordination AND asymmetric extraction with high suppression).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary-victim declarations and exit options. Waitlisted patients are beneficiaries with constrained exit (high cost to exit dialysis/waitlist) — this produces moderate d (around 0.40-0.50). Altruistic donors are beneficiaries with mobile exit (can choose not to participate) — this produces low d (around 0.15-0.25). Transplant programs are beneficiaries with arbitrage options (can modify programs, exit markets) — this produces very low d (around 0.05-0.15). No agents are declared as victims because no agent is structurally required to lose for others to gain in this mechanism. The ethical design of kidney exchange explicitly aims for Pareto improvement. The engine derives consistently low d values across all perspectives, which explains the uniform rope classification and low effective extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy because the coordination function is genuine and the classification is stable across all observables. At the basic level, kidney exchange solves incompatibility through coordinated chains and cycles — this is pure coordination. At the institutional level, transplant programs experience it as coordination infrastructure that increases their productive capacity — not extraction. At the societal level, it increases total transplants performed, improving public health outcomes. The theater_ratio trajectory (rising from 0.10 to 0.25) reflects increased algorithmic opacity as pools scale, but this is noise-level theater — the core mechanism remains functional. The constraint would only transition to tangled_rope if evidence emerged that the waitlist prioritization or chain termination practices systematically advantage certain demographic groups, turning algorithmic coordination into extraction of fairness. The omegas flag this as a future risk (especially if socioeconomic disparities in transplant outcomes emerge), but current data supports the rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    list_length_redistribution_fairness,
    'Does the waitlist prioritization system (time-on-list, biological compatibility, etc.) ensure fairness or does it create implicit extraction of advantage toward better-informed or better-connected patients?',
    'Longitudinal analysis of waitlist outcomes by socioeconomic status, geographic location, and healthcare access; comparison of exchange participation rates across demographic groups',
    'If unfair: constraints should be reclassified as tangled_rope (coordination + asymmetric extraction by information advantage). If fair: rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(list_length_redistribution_fairness, empirical, 'Whether waitlist prioritization creates implicit extraction by information advantage').

omega_variable(
    altruistic_donor_motivation_stability,
    'Is altruistic donor motivation robust or vulnerable to erosion as exchanges become more transactional and chain-breaking becomes economically rationalized?',
    'Survey data on donor motivations pre/post exchanges; analysis of how media framing and commercialization risk affect recruitment; international comparison of countries with/without paired exchange programs',
    'If motivation erodes: the mechanism may transition from rope (genuine altruism) to tangled_rope (coordinative function + extraction of altruism as subsidy). If stable: rope classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(altruistic_donor_motivation_stability, empirical, 'Stability of altruistic donor motivation over mechanism scaling').

omega_variable(
    chain_termination_equity,
    'Does the practice of chain termination (non-directed donation chain ending at highest-priority waitlist patient) distribute terminal transplants fairly or concentrate them toward better-positioned patients?',
    'Retrospective analysis of chain termination outcomes; comparison of characteristics of final recipients vs non-recipients in chains; modeling of optimal vs actual termination strategies',
    'If inequitable: suppression scores should rise; mechanism may transition toward tangled_rope or snare. If equitable: suppression remains moderate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chain_termination_equity, empirical, 'Fairness of chain termination point selection').

omega_variable(
    computational_complexity_emergence,
    'As exchange pools expand, does the algorithmic optimization problem (finding maximum welfare gains) create a hidden coordination cost that candidates cannot verify or exit from?',
    'Complexity analysis of matching algorithms; audit of algorithm behavior for edge cases; participant understanding of matching logic; comparative outcomes under simplified vs optimized matching rules',
    'If hidden cost emerges: theater_ratio rises; extractiveness may increase as algorithm becomes opaque optimization that coordinates on technical criteria invisible to participants. If transparent: theater_ratio remains low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_complexity_emergence, empirical, 'Whether algorithmic optimization introduces hidden coordination costs').

omega_variable(
    xenotransplantation_sunset_timing,
    'How near is xenotransplantation to clinical viability, and does the timeline make kidney exchange a temporary mechanism or a permanent institutional feature?',
    'Technical review of xenotransplantation development; expert forecasts of clinical deployment; policy analysis of regulatory timelines for animal-to-human organ approval',
    'If xeno-timeline < 10 years: kidney exchange should be reclassified as scaffold (temporary with real sunset). If > 30 years: rope classification appropriate (long-term institutional solution).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(xenotransplantation_sunset_timing, empirical, 'Timeline to xenotransplantation clinical viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kidney_exchange_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kidney_tr_t0, kidney_exchange_mechanism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kidney_tr_t5, kidney_exchange_mechanism, theater_ratio, 5, 0.18).
narrative_ontology:measurement(kidney_tr_t10, kidney_exchange_mechanism, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(kidney_be_t0, kidney_exchange_mechanism, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(kidney_be_t5, kidney_exchange_mechanism, base_extractiveness, 5, 0.18).
narrative_ontology:measurement(kidney_be_t10, kidney_exchange_mechanism, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kidney_exchange_mechanism, resource_allocation).
narrative_ontology:affects_constraint(kidney_exchange_mechanism, organ_allocation_waitlist_priority).
narrative_ontology:affects_constraint(kidney_exchange_mechanism, xenotransplantation_development).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
