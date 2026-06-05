% ============================================================================
% CONSTRAINT STORY: sotu_1986_reagan_gramm_rudman_hollings
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1986_reagan_gramm_rudman_hollings, []).

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
 *   constraint_id: sotu_1986_reagan_gramm_rudman_hollings
 *   human_readable: Gramm-Rudman-Hollings Deficit Reduction Mechanism
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   The Gramm-Rudman-Hollings Balanced Budget and Emergency Deficit Control
 *   Act of 1985 created a statutory mechanism intended to force the federal
 *   government to reduce deficits by a specified percentage each fiscal year,
 *   with automatic sequestration (across-the-board spending cuts) triggered
 *   if targets were missed by April 15th. Reagan administration rhetoric
 *   presented this as imposing discipline comparable to household budgeting,
 *   a natural law of sound fiscal management. However, the mechanism exhibits
 *   classic Tangled Rope structure: genuine coordination function (multiple
 *   administrations and Congress must align on fiscal targets) combined with
 *   significant asymmetric extraction (agencies and program beneficiaries
 *   bear the burden of cuts while future creditors benefit from deficit
 *   reduction). The constraint's theater ratio increased sharply as political
 *   actors learned to circumvent, suspend, or modify sequestration through
 *   legislative maneuvers, rendering the 'automatic' enforcement
 *   substantially performative.
 *
 * KEY AGENTS:
 *   - Federal agencies: Institutional victim (trapped exit) — face mandatory budget cuts; cannot negotiate or exit; no control over deficit levels
 *   - Program beneficiaries (Medicare, SNAP, education): Powerless victim (trapped exit) — benefits reduced by sequestration; no alternative source of federal benefits
 *   - Congress: Moderate beneficiary (constrained exit) — gains coordination benefit (deficit limits provide rationale for budget tradeoffs) but also bears extraction (reduced discretion, political pressure from affected constituencies)
 *   - Future creditors/bondholders: Institutional beneficiary (arbitrage exit) — benefit from deficit reduction without bearing costs; can trade securities freely; perfect exit option
 *   - Defense contractors: Powerful beneficiary (constrained exit) — benefit from preferential protection of defense spending under GRH structure; constrained by political requirements but net beneficiary
 *   - Reagan administration: Institutional designer (arbitrage) — frames mechanism as natural law; controls implementation discretion; benefits from political cover for cuts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1986_reagan_gramm_rudman_hollings, 0.52).
domain_priors:suppression_score(sotu_1986_reagan_gramm_rudman_hollings, 0.65).
domain_priors:theater_ratio(sotu_1986_reagan_gramm_rudman_hollings, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1986_reagan_gramm_rudman_hollings, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1986_reagan_gramm_rudman_hollings, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1986_reagan_gramm_rudman_hollings, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1986_reagan_gramm_rudman_hollings, tangled_rope).
narrative_ontology:human_readable(sotu_1986_reagan_gramm_rudman_hollings, "Gramm-Rudman-Hollings Deficit Reduction Mechanism").
narrative_ontology:topic_domain(sotu_1986_reagan_gramm_rudman_hollings, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1986_reagan_gramm_rudman_hollings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1986_reagan_gramm_rudman_hollings, future_creditors).
narrative_ontology:constraint_beneficiary(sotu_1986_reagan_gramm_rudman_hollings, deficit_conscious_taxpayers).
narrative_ontology:constraint_victim(sotu_1986_reagan_gramm_rudman_hollings, federal_agencies).
narrative_ontology:constraint_victim(sotu_1986_reagan_gramm_rudman_hollings, discretionary_program_beneficiaries).
narrative_ontology:constraint_victim(sotu_1986_reagan_gramm_rudman_hollings, social_safety_net_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT BENEFICIARY (SNARE) — Federal program recipients (Medicare, education, housing assistance) face automatic cuts with no exit. Suppression is complete: benefits are administered by the federal government and cannot be obtained elsewhere. The mechanism extracts by forcing tradeoffs between budgets rather than generating new revenue.
constraint_indexing:constraint_classification(sotu_1986_reagan_gramm_rudman_hollings, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERAL AGENCY (SNARE) — Agencies cannot exit the sequestration requirement and cannot control deficit levels — the target is set exogenously. They experience the mechanism as pure extraction: forced cuts regardless of operational necessity or public mandate. No coordination benefit, only constraint.
constraint_indexing:constraint_classification(sotu_1986_reagan_gramm_rudman_hollings, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESSIONAL NEGOTIATOR (TANGLED ROPE) — Congress benefits from the constraint as a coordination mechanism (imposes fiscal discipline, removes blame for cuts, forces priorities) but also bears extraction costs (limited discretion, political pressure, reduced flexibility). Can organize coalitions to modify or delay sequestration, but at cost. Mixed extraction and coordination benefit.
constraint_indexing:constraint_classification(sotu_1986_reagan_gramm_rudman_hollings, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FUTURE CREDITOR (ROPE) — Bondholders and foreign creditors benefit from deficit reduction without bearing direct costs. The mechanism coordinates fiscal discipline across multiple administrations. Exit cost is zero — they can trade securities freely. Net beneficiary with minimal extraction experienced.
constraint_indexing:constraint_classification(sotu_1986_reagan_gramm_rudman_hollings, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEFENSE CONTRACTING INTEREST (TANGLED ROPE) — Defense spending receives preferential protection under GRH (military personnel costs exempted, sequestration applies to remainder). Contractors benefit from coordination (predictable budget constraints prevent arbitrary cuts) and extraction (government obligated to maintain defense capacity). High power, constrained by political requirements, net beneficiary.
constraint_indexing:constraint_classification(sotu_1986_reagan_gramm_rudman_hollings, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: BUDGETARY THEATER (PITON) — The mechanism's enforcement is substantially performative: courts and Congress have repeatedly suspended or modified sequestration rules. The statutory language (April 15 trigger, automatic cuts) is maintained but not consistently applied. Theater ratio reflects the gap between the stated mechanism and actual fiscal discipline achieved — many sequestration events were avoided or delayed through political negotiation.
constraint_indexing:constraint_classification(sotu_1986_reagan_gramm_rudman_hollings, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the mechanism naturalizes an economic principle: government budgets must eventually balance, deficit spending has hard limits, and coordination mechanisms are required to enforce fiscal discipline. The Reagan framing presents this as a law of sound economics comparable to household budgeting constraints. However, structural data contradicts this — identifiable beneficiaries (future creditors, defense interests) and victims (agencies, program recipients) reveal this as a constructed institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(sotu_1986_reagan_gramm_rudman_hollings, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1986_reagan_gramm_rudman_hollings_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1986_reagan_gramm_rudman_hollings, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1986_reagan_gramm_rudman_hollings, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1986_reagan_gramm_rudman_hollings, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1986_reagan_gramm_rudman_hollings, TR),
    TR >= 0.70.

:- end_tests(sotu_1986_reagan_gramm_rudman_hollings_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The mechanism forces real spending cuts with genuine burden on agencies and beneficiaries. However, extractiveness is not as severe as a pure snare because: (1) the deficit reduction has real economic rationale (crowding out, inter-temporal transfer costs), and (2) Congress retains some ability to negotiate or modify targets. The baseline value (0.35 at t=0) reflects the initial optimistic framing and legal uncertainty; it rose to 0.52 as political actors learned to work around the mechanism, revealing the true extraction cost. Suppression (0.65): High. Agencies cannot exit the sequestration requirement, program beneficiaries cannot obtain federal benefits elsewhere, and taxpayers cannot easily exit the system. However, suppression is not total (0.90) because political negotiation and legal challenges have created some escape hatches. Theater ratio (0.58): Moderate-high. While the mechanism's statutory language is clear (April 15 trigger, automatic cuts), actual enforcement was repeatedly suspended or modified through legislative action (Continuing Resolutions, modifications to GRH rules). The performative element increased over time as political actors discovered that sequestration could be avoided through negotiation, reducing the constraint's functional role relative to its theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Beneficiaries (creditors, administration) classify as Rope or low-extraction Tangled Rope. Trapped victims (agencies, program beneficiaries) classify as Snare. Moderate actors with negotiating capacity (Congress) classify as Tangled Rope. The piton perspective reveals that the mechanism's theatrical maintenance (continued statutory language despite political override) masks the underlying extraction by making the constraint appear automatic and inevitable. The mountain perspective risks naturalizing this as a law of economics (deficits must be reduced, budgets must balance) when the structural data reveals contingent institutional arrangements (choice to use sequestration mechanism, choice to exempt defense, choice of beneficiaries).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives based on exit options and role in the extraction flow. Powerless program beneficiaries (trapped exit) experience d ≈ 0.95 (full target), yielding high χ. Federal agencies (trapped exit, no beneficiary role) experience d ≈ 0.92. Congress (constrained exit, mixed beneficiary/victim) experiences d ≈ 0.55 (mixed). Future creditors (arbitrage exit, pure beneficiary) experience d ≈ 0.08 (full beneficiary, minimal extraction). Reagan administration (institutional, arbitrage exit) experiences d ≈ 0.10 (benefits from mechanism design, minimal extraction cost). Defense contractors (powerful, constrained exit, preferential protection) experience d ≈ 0.35 (mixed beneficiary role within extraction framework).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that the GRH mechanism exhibits genuine coordination function (multiple actors must align on fiscal targets, sequestration threat coordinates behavior) combined with irreducible asymmetric extraction (burden falls on agencies and beneficiaries, benefit accrues to creditors). This is not confusion between coordination and extraction — both are real. The mechanism solves the coordination problem of multi-actor deficit reduction while simultaneously extracting from those who bear sequestration costs. The theater ratio increase over time reveals degradation: as political actors learned to circumvent sequestration, the mechanism retained its coordination rationale (threat of cuts maintains fiscal discipline) but reduced its functional enforcement, making it increasingly theatrical. The false summit risk is high: naturalizing the mechanism as 'fiscal discipline law' disguises the political choice to use sequestration (rather than progressive taxation, discretionary spending caps, or other mechanisms) to achieve deficit reduction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automatic_enforcement_effectiveness,
    'Does the threat of automatic sequestration actually enforce deficit targets, or does political negotiation systematically override the mechanism?',
    'Historical analysis of GRH deficit targets vs actual deficits; count of sequestration events vs suspension/modification events; correlation between presence of GRH and actual deficit reduction vs pre-GRH era',
    'If enforcement effective: Tangled Rope classification stable. If consistently overridden: reclassify as Piton (theater-maintained) or Scaffold (political negotiation creates exit pathway).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(automatic_enforcement_effectiveness, empirical, 'Whether GRH sequestration threat actually enforces compliance or is politically negotiable').

omega_variable(
    distributional_incidence_targeting,
    'Is the pattern of GRH cuts (defense vs social programs vs agency operations) driven by fiscal necessity or by political preferences masquerading as automatic mechanism?',
    'Comparative analysis of sequestration patterns across administrations; identification of consistent protection vs cutting patterns; examination of legal challenges to specific cut allocations',
    'If cuts follow fiscal logic: extraction is proportional to program magnitude. If cuts follow politics: mechanism is extractive tool for specific constituencies (evidence that defense interests benefit asymmetrically).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_incidence_targeting, empirical, 'Whether cut patterns reflect fiscal logic or political preferences').

omega_variable(
    alternative_fiscal_discipline_mechanisms,
    'Could fiscal discipline be achieved through less extractive mechanisms (progressive revenue increases, discretionary spending caps with exemptions, multi-year budgeting)?',
    'Comparative institutional analysis of fiscal discipline across OECD countries; identification of mechanisms with lower suppression and higher coordination benefit; simulation of alternative mechanisms under GRH scenario constraints',
    'If alternatives exist with lower extraction: GRH mechanism choice is extractive (reveals preference for extraction over coordination). If GRH is least-bad option: classification as necessary Tangled Rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_fiscal_discipline_mechanisms, conceptual, 'Whether less extractive fiscal discipline mechanisms are available').

omega_variable(
    household_budget_framing_validity,
    'Is the household budget analogy appropriate for sovereign fiscal policy, or does it naturalize inappropriate constraints on a government with taxing authority and currency control?',
    'Economic theory comparison: fiscal constraints on households vs monetarily sovereign governments; empirical analysis of countries with currency control showing ability to sustain higher deficits; examination of GRH framing rhetoric vs economic reality',
    'If analogy invalid: GRH is naturalized misconception (false summit). Classification shifts toward extractive snare for agencies depending on whether deficits are economically necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(household_budget_framing_validity, conceptual, 'Whether household budget analogy applies to sovereign fiscal policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1986_reagan_gramm_rudman_hollings, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grh_tr_t0, sotu_1986_reagan_gramm_rudman_hollings, theater_ratio, 0, 0.42).
narrative_ontology:measurement(grh_tr_t3, sotu_1986_reagan_gramm_rudman_hollings, theater_ratio, 3, 0.52).
narrative_ontology:measurement(grh_tr_t6, sotu_1986_reagan_gramm_rudman_hollings, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(grh_be_t0, sotu_1986_reagan_gramm_rudman_hollings, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(grh_be_t3, sotu_1986_reagan_gramm_rudman_hollings, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(grh_be_t6, sotu_1986_reagan_gramm_rudman_hollings, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1986_reagan_gramm_rudman_hollings, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1986_reagan_gramm_rudman_hollings, federal_budgetary_discretion).
narrative_ontology:affects_constraint(sotu_1986_reagan_gramm_rudman_hollings, social_safety_net_vulnerability).

% DUAL FORMULATION NOTE:
% GRH is downstream of broader debates over federal fiscal policy and upstream of specific agency-level impacts (defense spending patterns, Medicare reductions). The mechanism itself is structurally distinct from the underlying deficit problem it addresses — it is an enforcement architecture, not a revenue/spending decision.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1986_reagan_gramm_rudman_hollings, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
