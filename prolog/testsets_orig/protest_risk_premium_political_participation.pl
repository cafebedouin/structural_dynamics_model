% ============================================================================
% CONSTRAINT STORY: protest_risk_premium_political_participation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_protest_risk_premium_political_participation, []).

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
 *   constraint_id: protest_risk_premium_political_participation
 *   human_readable: Protest Risk Premium in Political Participation
 *   domain: political_economy/civil_resistance
 *
 * SUMMARY:
 *   The protest risk premium creates a structural barrier to political
 *   participation by imposing differential costs on those who dissent
 *   relative to those who accept the status quo. Marginalized constituencies
 *   and political dissidents face imprisonment, violence, employment
 *   discrimination, and social stigma for public dissent, while incumbent
 *   elites and passive citizens incur minimal costs. This constraint exhibits
 *   the defining characteristic of tangled rope: a genuine coordination
 *   function (collective action can change policy) is embedded within an
 *   asymmetric extraction mechanism (costs borne by powerless participants,
 *   benefits distributed broadly including to free-riders and elites). The
 *   constraint operates through state security apparatus enforcement, elite
 *   political strategy, and internalized fear. Extractiveness has risen from
 *   0.38 to 0.58 over the measurement interval, reflecting both increased
 *   police capacity and expanding surveillance. Theater ratio remains
 *   moderate (0.48) because suppression operates through actual enforcement
 *   (police action, imprisonment, violence) rather than purely performative
 *   deterrence, distinguishing it from constraints maintained primarily
 *   through institutional theater.
 *
 * KEY AGENTS:
 *   - Marginalized Dissidents: Primary victims (powerless/trapped) — face maximal suppression with minimal exit options; identity-locked to political activism
 *   - Moderate Citizen Activists: Secondary victims (moderate/constrained) — face significant but surmountable costs; have genuine agency but face suppression
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — extracts compliance and demonstrates state capacity; has maximal exit options and enforcement control
 *   - Elite Opposition Parties: Tertiary beneficiary (powerful/mobile) — benefit from grassroots mobilization energy while constraining radical association; coordinate with activists while extracting labor
 *   - Incumbent Political Elites: Quaternary beneficiary (institutional/arbitrage) — benefit from suppression of dissent and preservation of status quo
 *   - International Human Rights Framework: Institutional observer (institutional/arbitrage) — nominally protects assembly rights but enforcement is theatrical; monitors and reports without capacity to constrain state action
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional suppression as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(protest_risk_premium_political_participation, 0.58).
domain_priors:suppression_score(protest_risk_premium_political_participation, 0.65).
domain_priors:theater_ratio(protest_risk_premium_political_participation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(protest_risk_premium_political_participation, extractiveness, 0.58).
narrative_ontology:constraint_metric(protest_risk_premium_political_participation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(protest_risk_premium_political_participation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(protest_risk_premium_political_participation, tangled_rope).
narrative_ontology:human_readable(protest_risk_premium_political_participation, "Protest Risk Premium in Political Participation").
narrative_ontology:topic_domain(protest_risk_premium_political_participation, "political_economy/civil_resistance").

domain_priors:requires_active_enforcement(protest_risk_premium_political_participation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(protest_risk_premium_political_participation, incumbent_political_elites).
narrative_ontology:constraint_beneficiary(protest_risk_premium_political_participation, status_quo_preservers).
narrative_ontology:constraint_victim(protest_risk_premium_political_participation, marginalized_constituencies).
narrative_ontology:constraint_victim(protest_risk_premium_political_participation, political_dissidents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED DISSIDENT (SNARE) — Faces imprisonment, police violence, employment blacklisting, family harassment. No meaningful alternatives to suppress dissent. The risk premium is absolute and unavoidable. Structural exit is materially impossible; cognitive exit is identity-locked for committed activists. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(protest_risk_premium_political_participation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERATE CITIZEN ACTIVIST (TANGLED ROPE) — Faces real but surmountable costs: social stigma, workplace friction, legal fees for minor charges, relationship strain. Genuine coordination function exists — collective action changes policy — but extraction is asymmetric: costs borne by participants, benefits distributed widely (including to free-riders). Significant suppression but with agency.
constraint_indexing:constraint_classification(protest_risk_premium_political_participation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE SECURITY APPARATUS (ROPE) — Experiences protest suppression as a coordination mechanism: maintains order, demonstrates state capacity, signals boundaries of acceptable dissent. Net beneficiary — extracts compliance from marginalized agents. Low effective extraction from its own position because it has maximal exit options and controls the enforcement apparatus.
constraint_indexing:constraint_classification(protest_risk_premium_political_participation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ELITE OPPOSITION PARTY (TANGLED ROPE) — Benefits from grassroots protest energy mobilizing voters and generating media coverage. But also constrained by protest risk premium — street-level violence risks tainting electoral brand; association with radical activists alienates centrist donors. Coordinates with grassroots while extracting their labor for elite electoral gain. Has exit options (negotiation, electoral focus only) but strategically maintains protest ecosystem.
constraint_indexing:constraint_classification(protest_risk_premium_political_participation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL HUMAN RIGHTS FRAMEWORK (PITON) — Nominally protects freedom of assembly and protest. But enforcement is theatrical — condemnations issued, reports filed, monitors deployed. Actual constraints on state suppression remain minimal. The framework persists through institutional legitimacy (NGOs, UN bodies, treaty language) despite low functional capacity to prevent protest risk premiums. Theater ratio reflects the gap between rights-on-paper and rights-in-practice.
constraint_indexing:constraint_classification(protest_risk_premium_political_participation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some cost to collective action is inherent: organizing requires time and resources; visibility to authorities is inevitable; punishment for norm violation is built into how societies maintain order. This perspective naturalizes the protest risk premium as an immutable feature of political systems. The engine's false summit detector will identify this as misclassification — the premium is contingent on state capacity and institutional design, not lawful.
constraint_indexing:constraint_classification(protest_risk_premium_political_participation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(protest_risk_premium_political_participation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(protest_risk_premium_political_participation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(protest_risk_premium_political_participation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(protest_risk_premium_political_participation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(protest_risk_premium_political_participation, TR),
    TR >= 0.70.

:- end_tests(protest_risk_premium_political_participation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The protest risk premium extracts significant costs from participants — imprisonment duration ranges from days (minor protest) to years (sedition charges); employment discrimination is documented; family harassment is systematic in some regimes. But extraction is not maximal (snare threshold: 0.66) because: (1) coordination genuinely happens despite suppression (protests do change policy at baseline rates); (2) some participation is voluntary (identity-locked agents choose to participate accepting costs); (3) benefits are distributed widely including to supporters who bear no cost (free-rider problem indicates coordination, not pure extraction). Suppression (0.65): High. State security apparatus has multiple suppression tools: criminal law, police violence, intelligence surveillance, employment blacklisting, social stigma. Barriers to exit are severe — marginalized agents cannot 'opt out' of targeted categories (ethnicity, political affiliation). Moderate agents face real costs but can reduce participation or shift to legal advocacy. Theater ratio (0.48): Moderate. The constraint operates substantially through actual enforcement (people are actually imprisoned, beaten, blacklisted) rather than performative deterrence. But institutional theater exists: governments claim suppression is necessary for order; rights frameworks claim suppression will be addressed; opposition elites claim they oppose suppression while tacitly benefiting from it. The theater is lower than piton-level constraints because material consequences dominate the suppression mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Marginalized dissidents see pure snare: the risk premium is absolute, extraction is maximal, no genuine coordination benefit reaches them (benefits accrue to opposition elites and satisfied citizens). Moderate activists see tangled_rope: genuine coordination (protests do produce policy change) is embedded in extraction (costs borne by participants, benefits to free-riders). Opposition elites see rope: the protest ecosystem coordinates mobilization and generates electoral advantage. The security apparatus sees rope: the suppression mechanism coordinates order and state capacity signaling. The human rights framework sees its own performative role (piton) — issued condemnations and reports without enforcement. The civilizational analyst risks seeing mountain (immutable law of political systems) but structural data reveals this as naturalizing contingent institutional suppression. The perspectival gap reveals that observed constraint type is entirely position-dependent: powerless → snare, moderate → tangled_rope, powerful beneficiary → rope, institutional executor → rope, international observer → piton, analytical transcendent → mountain. All readings are valid from their positions; none is 'correct' in isolation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across agent types. Marginalized dissidents (powerless + trapped) derive d ~0.95 — they are full extraction targets with no exit. Moderate activists (moderate + constrained) derive d ~0.65 — they bear significant extraction but retain some agency and partial exit. Elite security apparatus (institutional + arbitrage) derives d ~0.10 — beneficiary with maximal exit options. Opposition parties (powerful + mobile) derive d ~0.35 — partial beneficiaries with exit options but strategically maintaining protest ecosystem. This variation produces perspectival gap: the same constraint is experienced as snare by powerless agents, tangled_rope by moderate agents, and rope by institutional beneficiaries. The chi formula scales these differently: powerless agents experience χ ≈ 0.58 × f(0.95) × σ(national) ≈ 0.58 × 1.42 × 1.0 ≈ 0.82 (high effective extraction). Institutional beneficiaries experience χ ≈ 0.58 × f(0.10) × σ(national) ≈ 0.58 × -0.01 × 1.0 ≈ -0.006 (negative effective extraction / net benefit). The perspectival gap is extreme because directionality varies widely: high-power agents and low-power agents occupy opposite structural positions relative to this extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The protest risk premium is genuine tangled_rope (not snare, not rope) because it simultaneously coordinates collective action AND extracts asymmetric costs. The mandatrophy — mislabeling coordination as extraction or vice versa — is resolved by declaring both structural functions explicitly: beneficiaries (incumbent elites, status quo preservers) benefit from suppressed participation AND from the perception that participation is 'free' (opposite of true). Victims (marginalized constituencies, dissidents) bear extraction costs AND contribute to collective action that produces public goods. The extraction is not incidental to coordination; it is embedded in the coordination structure. The coordination is not incidental to extraction; it is the mechanism through which extraction gains legitimacy ('people are free to participate if they accept the costs'). This is the defining signature of tangled_rope: both functions are essential to the constraint's operation. Removing suppression changes the coordination function fundamentally (protest becomes low-cost, participation surges, policy responsiveness increases or decreases depending on regime design). Removing the coordination benefit (preventing protests from producing policy change) collapses the constraint toward snare (pure suppression with no function except fear). The tangled_rope classification affirms that the constraint is neither natural law nor pure extraction — it is a contingent institutional arrangement that could be reformed by adjusting suppression or coordination terms independently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_type,
    'Is suppression primarily structural (legal barriers, police capacity, economic dependency) or internalized (anticipated punishment, identity-based self-censorship, normalized fear)?',
    'Longitudinal surveys measuring stated vs actual participation constraints; analysis of participation changes following decriminalization or police withdrawal; cognitive interviews with non-participants about barrier types',
    'If primarily structural: escape requires institutional reform. If primarily internalized: escape persists after institutional barriers removed — constraint becomes identity-locked despite mobile exit options. Mixed composition means post-exit suppression persists partially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_type, empirical, 'Structural vs internalized suppression mechanism in protest participation').

omega_variable(
    elite_extraction_intentionality,
    'Do incumbent elites deliberately engineer the protest risk premium to suppress participation, or does it emerge as an incidental byproduct of security apparatus institutional incentives?',
    'Historical analysis of protest policy formation; internal security doctrine review; cross-regime comparison of suppression intensity vs opposition party coordination patterns; media analysis of elite framing of protest risks',
    'If deliberate: state apparatus classifies as intentional snare. If incidental: suppression is institutional byproduct, classification shifts toward scaffold (temporary coordination failure) or piton (degraded institutional logic). Affects remediation strategy — institutional reform vs structural accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_extraction_intentionality, conceptual, 'Whether elite suppress protest intentionally or through institutional incentive drift').

omega_variable(
    coordination_function_authenticity,
    'Does the protest risk premium genuinely coordinate collective action (protest reaches critical mass despite suppression) or does it primarily extract consent through fear (suppression prevents coordination)?',
    'Time-series analysis of protest size vs suppression intensity; comparison of regimes with different suppression strategies; measurement of successful vs failed collective action campaigns under varying risk conditions',
    'If authentic coordination: snare classification is incorrect — should be tangled_rope or even rope (high-cost coordination). If primarily extraction: snare is confirmed — suppression dominates, coordination function is cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_authenticity, empirical, 'Whether protest risk premium coordinates action or primarily extracts consent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(protest_risk_premium_political_participation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prpp_tr_t0, protest_risk_premium_political_participation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prpp_tr_t5, protest_risk_premium_political_participation, theater_ratio, 5, 0.42).
narrative_ontology:measurement(prpp_tr_t10, protest_risk_premium_political_participation, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(prpp_be_t0, protest_risk_premium_political_participation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prpp_be_t5, protest_risk_premium_political_participation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(prpp_be_t10, protest_risk_premium_political_participation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(protest_risk_premium_political_participation, enforcement_mechanism).
narrative_ontology:affects_constraint(protest_risk_premium_political_participation, electoral_participation_suppression).
narrative_ontology:affects_constraint(protest_risk_premium_political_participation, civil_society_capacity_constraints).
narrative_ontology:affects_constraint(protest_risk_premium_political_participation, authoritarian_institutional_inertia).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(protest_risk_premium_political_participation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
