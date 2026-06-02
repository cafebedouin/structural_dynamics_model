% ============================================================================
% CONSTRAINT STORY: us_sanctions_icc_israel_case
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sanctions_icc_israel_case, []).

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
 *   constraint_id: us_sanctions_icc_israel_case
 *   human_readable: US Sanctions Threat Against ICC Officials Investigating Israel
 *   domain: political/international_law
 *
 * SUMMARY:
 *   The bipartisan US bill proposing sanctions against ICC officials
 *   investigating alleged Israeli war crimes creates a direct structural
 *   conflict between national sovereignty and international criminal
 *   jurisdiction. The constraint operates through credible threat of coercive
 *   penalties (visa bans, asset freezes) against individuals attempting to
 *   fulfill their institutional mandate. The constraint exhibits classical
 *   snare structure: suppression is high (threat of personal sanctions,
 *   potential loss of career), extractiveness is significant (forces ICC to
 *   abandon or slow investigation), and the extracted value flows to the
 *   Israeli state and US alliance interests. However, the constraint also
 *   exhibits tangled rope characteristics from the US perspective (it solves
 *   genuine coordination problems around alliance relationships and immunity
 *   management) and piton characteristics from the historical view (the
 *   US-ICC distance is partly performative posturing). The theater ratio has
 *   increased over the interval as the threat has become more explicit and
 *   visible, while base extractiveness has increased as the bill moved from
 *   proposal to serious legislative consideration.
 *
 * KEY AGENTS:
 *   - ICC Prosecutors and Investigators: Primary victims (powerless/trapped) — face personal legal and financial jeopardy; cannot exit without abandoning institutional duty
 *   - Israeli State: Primary beneficiary (institutional/arbitrage) — shields nationals from ICC prosecution; captures immunity through US power
 *   - US Congress and Executive: Enforcer (institutional/constrained) — uses sanctions authority to manage ICC reach; benefits from immunity management but incurs reputational costs
 *   - International Criminal Justice System: Secondary victim (moderate/constrained) — loses institutional independence; constrained by US enforcement threats
 *   - ICC Member States: Tertiary actors (organized/mobile) — face incentives to abandon ICC support if prosecution becomes politically costly through alliance pressure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional design vulnerabilities as inherent law of international relations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sanctions_icc_israel_case, 0.58).
domain_priors:suppression_score(us_sanctions_icc_israel_case, 0.72).
domain_priors:theater_ratio(us_sanctions_icc_israel_case, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sanctions_icc_israel_case, snare).
narrative_ontology:human_readable(us_sanctions_icc_israel_case, "US Sanctions Threat Against ICC Officials Investigating Israel").
narrative_ontology:topic_domain(us_sanctions_icc_israel_case, "political/international_law").

domain_priors:requires_active_enforcement(us_sanctions_icc_israel_case).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_sanctions_icc_israel_case, israeli_state).
narrative_ontology:constraint_victim(us_sanctions_icc_israel_case, icc_prosecutorial_independence).
narrative_ontology:constraint_victim(us_sanctions_icc_israel_case, international_criminal_justice_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ICC PROSECUTORS/INVESTIGATORS (SNARE) — Face explicit threat of personal sanctions (visa bans, asset freezes) if they continue investigation. No viable exit: resigning abandons institutional mandate; continuing faces legal and financial jeopardy. Maximum suppression through threat of coercive sanctions. Cannot negotiate or coordinate around the threat — it is unilateral and credible. Structural position: full victim of extraction.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ICC AND INTERNATIONAL JUSTICE SYSTEM (SNARE) — Constrained by dependence on US cooperation for enforcement (asset seizure, extradition support). Cannot investigate cases threatening US ally interests without incurring institutional penalties. Exit options exist but are costly: pursuing investigation despite sanctions harms the institution's legitimacy and funding. The constraint extracts institutional compliance through threat of withdrawal and reputational damage.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ISRAELI STATE (ROPE) — Primary beneficiary. Experiences the constraint as effective coordination against threats to state security (exemption from ICC prosecution). High arbitrage capacity — can leverage alliance relationships and US power asymmetry. Net extraction runs toward this actor. The constraint solves the collective action problem of managing ICC prosecution risk.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US STATE AND CONGRESS (TANGLED ROPE) — Benefits from constraint (ability to shield allied state from ICC accountability) AND bears coordination costs (reputational damage, alliance strain, legal inconsistency). High suppression via legislative threat. Extraction is hybrid: uses coercive power (sanctions authority) to extract compliance but also genuinely solves coordination problem (preventing ICC from undermining alliance relationships). Active enforcement required; extraction is not pure.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: US-ICC STRUCTURAL RELATIONSHIP (PITON) — The US has never joined the ICC, maintaining structural independence from the institution. The threat to sanction ICC officials is partly performative — it reinforces the US posture of unaccountability to international institutions. The historical pattern (US non-participation, periodic threats to constrain ICC reach) is maintained through institutional theater rather than functional necessity. Theater ratio high because the threat operates primarily through reputational posturing and alliance signaling.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN-FRAMING) — From a civilizational view, the conflict between national sovereignty and international criminal jurisdiction is a structural feature of the current international system. No state can be forced to join the ICC; the ICC has no independent enforcement capacity; prosecution of nationals of non-member states creates alignment problems between the court and its dependency on state cooperation. This perspective risks naturalizing what is actually a contingent institutional design problem as an immutable law of international relations.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sanctions_icc_israel_case_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sanctions_icc_israel_case, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_sanctions_icc_israel_case, TR),
    TR >= 0.70.

:- end_tests(us_sanctions_icc_israel_case_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts investigative compliance from the ICC system through credible threat of sanctions against individual officials. The extraction is not total (ICC retains formal independence) but is substantial enough to deter investigation of cases affecting allied states. The measure reflects the trajectory from legislative proposal (0.35, theoretical threat) through serious consideration (0.58, credible implementation risk). Suppression (0.72): High. Multiple suppression mechanisms operate: personal financial/career risk to investigators, institutional reputational damage to ICC if it defies US sanctions, alliance pressure on ICC member states to discourage prosecution, and political costs to supporting states. Investigators have no safe exit: complying abandons mandate, continuing risks sanctions. Theater ratio (0.65): Moderate-high. The constraint operates through both performative and functional mechanisms. Performative: alliance posturing, public statements of US exceptionalism, legislative theater around ICC limitations. Functional: actual sanctions would impose real costs. The theater has increased as the threat became more visible and explicit.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspective (investigators trapped, ICC system victimized) conflicts sharply with the rope perspective (US solves genuine coordination problem of managing ICC reach). From the Israeli state view, the constraint is beneficial coordination preventing prosecutions. From the ICC view, the constraint is pure coercion blocking legitimate investigation. From the US state view, the constraint is both coordination (managing alliance relationships) and coercion (enforcing immunity through threat). The piton perspective reveals that the US-ICC distance is maintained partly through performative posturing (the US never joined the ICC, maintains structural independence). The analytical observer risks seeing immutable law (structural vulnerability of ICC to state pressure) when the constraint actually reflects contingent choices about institutional design and funding dependence.
 *
 * DIRECTIONALITY LOGIC:
 *   ICC investigators and prosecutors derive high d (d ≈ 0.92) from victim status (explicit sanction threats) and trapped exit options (cannot abandon mandate without institutional failure, cannot continue without legal jeopardy). This produces high f(d) ≈ 1.38, amplifying effective extraction. The Israeli state derives low d (d ≈ 0.10) from beneficiary status and arbitrage capacity (can leverage alliance relationships and US power). This produces negative f(d) ≈ -0.05, making effective extraction negative (constraint subsidizes this actor). The ICC system derives moderate-high d (d ≈ 0.68) from victim status but constrained (not trapped) exit options — institutional options exist but all are costly. This produces f(d) ≈ 1.05, moderate experienced extraction. The US state derives d ≈ 0.55 from mixed position: beneficiary of immunity management (low d contribution) but also enforcement costs and alliance strain (high d contribution). This produces f(d) ≈ 0.75, blended extraction reflecting the tangled_rope hybrid nature.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the snare and tangled_rope classifications capture different structural realities from different positions. From the ICC and investigator perspective, it is purely extractive (snare) — the threat offers no coordination benefit, only suppression. From the US state perspective, it is hybrid (tangled_rope) — it solves genuine coordination problems (managing alliance relationships, preventing politically costly prosecutions) while also imposing reputational costs and legal inconsistency. The mandatrophy is resolved by recognizing that both readings are correct at their respective observation sites. The constraint is a snare TO the ICC system and a tangled rope FROM the US state perspective. The analytical observer's mountain framing (ICC dependence on state cooperation is a law of international relations) is partially valid but risks naturalizing contingent institutional design choices (ICC funding structures, enforcement mechanisms, member commitment) as immutable features. The proper analysis distinguishes structural vulnerability (how ICC is designed) from strategic response (US choice to weaponize sanctions threat).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_enforcement_credibility,
    'Will the US actually implement sanctions if ICC investigators proceed with the case, or is the threat primarily performative?',
    'Observation of US behavior if ICC issues arrest warrants; tracking of actual sanctions implementation and their severity; Congressional follow-through on legislative threats',
    'If credible: constraint is highly suppressive (0.72+), snare classification robust. If performative: suppression drops to 0.40-0.50, constraint may degrade to rope or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_enforcement_credibility, empirical, 'Whether US sanctions threat is credible or performative').

omega_variable(
    icc_prosecutorial_independence_resilience,
    'Can ICC investigators maintain independence and pursue investigations despite sanctions threats, or does the threat effectively block the investigation?',
    'Tracking of ICC investigative activity before and after sanctions threat; analysis of investigator departures, budget impacts, and case decisions; comparison to other cases where ICC faced pressure',
    'If ICC maintains independence: snare perspective is one view; constraint has lower functional suppression. If investigators are deterred: snare perspective confirmed — threat achieves full extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(icc_prosecutorial_independence_resilience, empirical, 'Whether ICC can maintain independence under sanctions threat').

omega_variable(
    alliance_compliance_threshold,
    'What is the minimum level of enforcement (sanctions implementation, asset seizures) required to keep ICC member states from challenging US exceptions?',
    'Analysis of ICC member state responses to US sanctions threat; comparison of responses to cases where ICC faced pressure; modeling of alliance incentives and free-riding behavior',
    'If low threshold: US needs minimal enforcement to maintain compliance. If high threshold: credible enforcement is necessary or alternative pressure mechanisms must emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_compliance_threshold, conceptual, 'Enforcement level needed to maintain alliance compliance').

omega_variable(
    structural_icc_vulnerability,
    'Is ICC prosecutorial vulnerability to state pressure (especially US pressure) an inherent feature of the current institutional design, or a contingent policy problem?',
    'Comparative analysis of ICC operational independence vs other international courts; historical analysis of ICC''s institutional evolution and capacity-building',
    'If inherent: mountain perspective has merit — ICC dependence on state cooperation is a structural law of international relations. If contingent: constraint reflects policy choices about ICC funding, enforcement mechanisms, and member commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_icc_vulnerability, conceptual, 'Whether ICC vulnerability to pressure is structural or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sanctions_icc_israel_case, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ussicc_tr_t0, us_sanctions_icc_israel_case, theater_ratio, 0, 0.5).
narrative_ontology:measurement(ussicc_tr_t2, us_sanctions_icc_israel_case, theater_ratio, 2, 0.58).
narrative_ontology:measurement(ussicc_tr_t4, us_sanctions_icc_israel_case, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(ussicc_be_t0, us_sanctions_icc_israel_case, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ussicc_be_t2, us_sanctions_icc_israel_case, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(ussicc_be_t4, us_sanctions_icc_israel_case, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_sanctions_icc_israel_case, enforcement_mechanism).
narrative_ontology:affects_constraint(us_sanctions_icc_israel_case, icc_structural_legitimacy).
narrative_ontology:affects_constraint(us_sanctions_icc_israel_case, us_international_accountability_exemption).

% DUAL FORMULATION NOTE:
% This constraint is downstream of structural ICC vulnerabilities (dependence on state cooperation for enforcement) but represents a distinct strategic choice to weaponize sanctions threat. The upstream structural constraints define the vulnerability; this constraint operationalizes that vulnerability through explicit threat. Link via affects_constraints captures the functional dependency while preserving the analytical distinction between vulnerability and exploitation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
