% ============================================================================
% CONSTRAINT STORY: spanish_political_opposition_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spanish_political_opposition_suppression, []).

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
 *   constraint_id: spanish_political_opposition_suppression
 *   human_readable: Spanish Political Opposition Suppression (Post-2017 Catalonia)
 *   domain: political/governance
 *
 * SUMMARY:
 *   Following the October 2017 Catalan independence referendum and
 *   declaration of autonomy, Spain's central government constructed a
 *   multi-layered suppression apparatus targeting pro-independence movements
 *   and regional autonomy advocates. This constraint operates through
 *   criminal prosecution (sedition and rebellion charges against elected
 *   officials and activists), institutional control of the judiciary through
 *   appointment processes and political pressure, surveillance infrastructure
 *   tracking independence activists, control of Spanish-language media
 *   narratives, and coercive institutional pressure on regional government
 *   structures. The suppression mechanism exhibits all characteristics of a
 *   snare: high extractiveness (coercive apparatus extracts political
 *   compliance through threat of prosecution), high suppression (multiple
 *   barrier types: legal jeopardy, surveillance, institutional closure, media
 *   marginalization), and minimal coordination function. The theater_ratio
 *   (0.65) reflects that Spanish institutions maintain the performative forms
 *   of judicial independence and democratic procedure while the substantive
 *   content has been subordinated to suppression logic. The constraint does
 *   not solve a coordination problem — independence advocates and the Spanish
 *   state are not trying to coordinate action; instead, one party is
 *   preventing the other from exercising political choice. Base
 *   extractiveness has risen over the interval (0.42 → 0.68) as prosecution
 *   has intensified, trials have accumulated, and institutional pressure on
 *   the judiciary has solidified. Suppression_requirement has similarly
 *   increased (0.55 → 0.78) as the central government has escalated
 *   enforcement mechanisms to maintain closure. This rising trajectory
 *   indicates an enforcement ratchet — the apparatus has become more
 *   comprehensive and more coercive as initial measures proved insufficient
 *   to eliminate independence advocacy.
 *
 * KEY AGENTS:
 *   - Pro-Independence Activists and Regional Autonomy Advocates: Primary victims (powerless/trapped) — face criminal prosecution, surveillance, and institutional closure of political pathways; bear maximum extraction through coercive apparatus
 *   - Catalan Political Parties and Regional Institutions: Secondary victims (moderate/constrained) — nominal electoral participation under threat of prosecution for leadership; institutional marginalization and judicial pressure
 *   - Spanish Central Government (Executive and Security Apparatus): Primary beneficiary (institutional/arbitrage) — consolidates power, controls territorial boundaries, expands security infrastructure; experiences constraint as coordination of state legitimacy
 *   - Spanish Judiciary: Captured institutional actor (institutional/arbitrage) — maintains theatrical independence while performing executive will; subject to institutional pressure and politicized appointment processes; piton classification reflects degraded independence
 *   - EU Institutions and International Democratic Observers: Moderate institutional actor (powerful/mobile) — perceive mixed coordination (rule of law protection) and extraction (suppression of democratic opposition); mobile exit options create tangled_rope perspective
 *   - Spanish-Language Media: Subordinate actor (powerful/constrained) — faces institutional pressure to frame suppression as legitimate security measure; controls public narrative about independence threat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spanish_political_opposition_suppression, 0.68).
domain_priors:suppression_score(spanish_political_opposition_suppression, 0.78).
domain_priors:theater_ratio(spanish_political_opposition_suppression, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spanish_political_opposition_suppression, extractiveness, 0.68).
narrative_ontology:constraint_metric(spanish_political_opposition_suppression, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(spanish_political_opposition_suppression, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spanish_political_opposition_suppression, snare).
narrative_ontology:human_readable(spanish_political_opposition_suppression, "Spanish Political Opposition Suppression (Post-2017 Catalonia)").
narrative_ontology:topic_domain(spanish_political_opposition_suppression, "political/governance").

domain_priors:requires_active_enforcement(spanish_political_opposition_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_victim(spanish_political_opposition_suppression, pro_independence_activists).
narrative_ontology:constraint_victim(spanish_political_opposition_suppression, regional_autonomy_advocates).
narrative_ontology:constraint_victim(spanish_political_opposition_suppression, catalan_political_parties).
narrative_ontology:constraint_victim(spanish_political_opposition_suppression, judicial_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRO-INDEPENDENCE ACTIVISTS (SNARE) — Trapped by sedition and rebellion charges, surveillance infrastructure, and institutional closure of political pathways. Exit options are functionally eliminated: continued advocacy triggers prosecution; abandoning political activity requires renouncing identity and regional commitment. Maximum extraction through coercive legal apparatus with minimal coordination benefit.
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CATALAN POLITICAL PARTIES (SNARE) — Face judicial pressure, media control, and institutional marginalization. Can nominally participate in elections but operate under constant threat of criminal prosecution for party leadership and institutional staff. High suppression (prosecution of elected officials, institutional capture of judiciary) with no genuine coordination benefit — extraction mechanism is purely coercive.
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SPANISH CENTRAL GOVERNMENT (ROPE) — Frames suppression as coordination of state legitimacy and legal order. Experiences constraint as solving the problem of maintaining territorial integrity and preventing institutional dissolution. Net beneficiary through centralized authority consolidation, media narrative control, and security apparatus expansion. Chi is negative or near-zero — extraction runs toward this actor.
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EU AND INTERNATIONAL OBSERVERS (TANGLED ROPE) — See coordination function (maintaining rule of law, preventing democratic backsliding across EU member states) but also genuine extraction and asymmetric coercion. Pressure on Spain for due-process compliance creates mild friction against the suppression apparatus, but EU institutional interests in maintaining Spain's stability and NATO alignment create countervailing pressures. Mobile exit options (diplomatic alternatives, treaty leverage) but real costs to exercising them.
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: SPANISH JUDICIAL SYSTEM (PITON) — The judiciary nominally functions as independent arbiter but has been subjected to institutional pressure and politicized appointment processes. Judicial performance of impartiality (theater) persists despite compromised independence. Judges issue criminal convictions that serve executive interests (sedition charges) while maintaining the form of independent adjudication. The constraint has captured judicial function; the system maintains theatrical independence to preserve legitimacy.
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STATE SOVEREIGNTY VIEW (MOUNTAIN) — Frames suppression as inherent to state maintenance of territorial integrity and monopoly on legitimate coercion. From this perspective, the suppression mechanism is a natural law of statecraft: states necessarily prevent armed secession and institutional dissolution. However, the structural data contradicts this framing — identifiable beneficiaries (central government), specific mechanisms (selective prosecution, surveillance, media control), and alternatives (negotiated devolution, federal restructuring) indicate this is not immutable natural law but a contingent political choice.
constraint_indexing:constraint_classification(spanish_political_opposition_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spanish_political_opposition_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(spanish_political_opposition_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spanish_political_opposition_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(spanish_political_opposition_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(spanish_political_opposition_suppression, TR),
    TR >= 0.70.

:- end_tests(spanish_political_opposition_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The suppression apparatus directly prevents pro-independence actors from achieving political objectives through democratic institutional means. The extraction flows in one direction: from powerless activists toward central government consolidation of control. The extraction is not total (0.72+) because some nominally legal political participation remains possible and because international pressure provides some countervailing force, but the extraction is severe and escalating. The constraint uses multiple mechanisms (prosecution, surveillance, institutional pressure, media control) to maintain closure, indicating systematic rather than ad-hoc suppression. Suppression (0.78): High. Multiple barriers prevent exit: legal jeopardy (sedition and rebellion charges carry 10+ year sentences), institutional closure (regional government functions subordinated to central control), surveillance infrastructure (tracking independence activity), media marginalization (Spanish-language media frames independence as illegitimate threat), and identity trap (pro-independence activists cannot abandon regional political commitment without dissolving identity). Suppression is structural rather than incidental — it is the mechanism that sustains the constraint. Theater ratio (0.65): Moderate-high. Spanish institutions maintain performative adherence to democratic and judicial procedure (trials are conducted, convictions go through courts, regional elections are held) while the substantive content has been subordinated to suppression logic. Judges issue convictions that serve executive interests; trials function as theater of legitimate adjudication while predetermined outcomes serve political suppression. The theater has increased over the interval as the apparatus has matured and learned to perform legitimacy while executing suppression.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The pro-independence activist sees a pure snare (trapped, coercive, extractive, no coordination benefit). The Catalan political party sees a snare with some constrained participation (can run for office but operates under prosecution threat). The Spanish central government sees a rope (solving the coordination problem of maintaining state cohesion and territorial integrity — they genuinely believe they are coordinating legitimate state function). The EU observer sees tangled_rope (coordination function of rule-of-law preservation but also genuine extraction and asymmetric coercion). The Spanish judiciary sees itself as piton (maintaining the form of independence while function has been captured). The civilizational analytical observer risks seeing a mountain (state monopoly on coercion as natural law) but this is a false summit — the specific mechanisms (selective prosecution, surveillance, media control, judicial capture) are contingent political choices, not immutable limits. The false summit is the most diagnostically important gap: Spain's central government and its supporters frame suppression as inherent to state maintenance, but the structural data reveals a specific institutional choice about how to manage territorial dispute. Alternatives exist (negotiated devolution, federal restructuring, supranational mediation) but have been foreclosed by political choice, not by natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position relative to the extraction mechanism. Pro-independence activists have d ≈ 0.95 (trapped victims with no exit options, bearing full extraction cost — f(d) ≈ 1.42). Catalan political parties have d ≈ 0.75 (constrained victims, can nominally participate but operate under suppression — f(d) ≈ 1.15). Spanish central government has d ≈ 0.08 (institutional beneficiary with arbitrage options, extraction flows toward them — f(d) ≈ -0.05). EU observers have d ≈ 0.55 (symmetric position, powerful exit options allow mobile rather than trapped — f(d) ≈ 0.65). Spanish judiciary has d ≈ 0.20 (institutional beneficiary but with captured rather than genuine autonomy — f(d) ≈ 0.02, reflects piton status). Analytical observer has d ≈ 0.72 (observes the structure but risks naturalizing it — canonical analytical d, f(d) ≈ 1.15). These derivations are structural facts, not observer-dependent. The engine computes chi = ε × f(d) × σ(S) from these parameters; the resulting effective extractiveness values differ across perspectives because the agents' structural relationships to the constraint differ, not because the constraint changes.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE EXTRACTION ANALYSIS: This constraint resolves the mandatrophy by demonstrating that the snare classification is robust across the full range of observations. Every perspective from a victim or subordinate position (pro-independence activists, Catalan parties, EU rule-of-law advocates, international observers) sees snare or tangled_rope. The only perspective that could see rope or mountain is the beneficiary (Spanish central government) or the captured institution (Spanish judiciary), and both of these are clearly beneficiary/captor relationships rather than genuine coordination. The analytical observer's temptation to see this as a mountain (state maintenance as natural law) is precisely the false-summit pathology that the framework exists to detect. The increasing extractiveness and suppression_requirement over time (0.42 → 0.68, 0.55 → 0.78) indicate an enforcement ratchet: the apparatus escalates mechanisms in response to continued opposition activity, suggesting that the suppression is contingent (dependent on escalating enforcement) rather than inevitable (as a mountain would be). If suppression were natural law, it would not require escalation — it would simply obtain. The escalation proves contingency. The constraint is a genuine snare: pure extraction mechanism maintained through coercive apparatus with minimal coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sedition_charge_legitimacy,
    'Are sedition and rebellion charges applied to pro-independence political activity based on genuine threat of armed insurrection, or do they criminalize protected political speech and assembly?',
    'Comparative analysis of sedition charges across European democracies; examination of whether charged defendants possessed weapons, training, or operational planning vs. purely symbolic or rhetorical activity; international legal expert review of whether charges meet international standards for legitimate criminal prosecution',
    'If charges are legitimate: suppression is law enforcement (snare justified by security needs). If charges criminalize protected political activity: suppression is authoritarianism (snare via judicial weaponization). Classification remains snare either way, but the falsifiability of the state''s legitimacy claim determines mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sedition_charge_legitimacy, empirical, 'Legitimacy of sedition charges applied to political activity').

omega_variable(
    judicial_independence_capture_mechanism,
    'Is Spanish judicial suppression of independence activity driven by independent judicial assessment of sedition law, or by institutional pressure from executive branches and government-affiliated appointment processes?',
    'Analysis of judicial appointment patterns pre- and post-2017; comparison of conviction rates for similar charges across regions with different judicial political composition; examination of reversals or acquittals by higher courts; interviews with judicial actors regarding institutional pressure',
    'If independent assessment: judiciary constrains suppression through rule of law (snare with judicial brake). If institutionally captured: suppression is unilateral executive power disguised as judicial process (snare with enhanced extraction via delegitimization of justice system). Affects whether tangled_rope is viable from any perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_capture_mechanism, empirical, 'Whether judicial system is independent or captured in suppression apparatus').

omega_variable(
    exit_options_for_catalan_political_actors,
    'Can Catalan political parties pursue independence advocacy through legal political processes (electoral participation, institutional negotiation, petition) without triggering prosecution, or are all pathways to institutional change criminalized?',
    'Empirical documentation of whether elected Catalan politicians can perform legislative functions, propose constitutional amendments, or advocate for secession through parliamentary means without criminal consequences; comparison with treatment of secession advocates in other democracies',
    'If legal pathways exist: exit_options for political parties could be ''constrained'' rather than ''trapped'' (classification shifts from snare toward tangled_rope). If all institutional pathways are criminalized: trap is total, snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_options_for_catalan_political_actors, empirical, 'Whether legal political pathways to independence exist or are criminalized').

omega_variable(
    state_monopoly_legitimacy,
    'Is the mountain perspective (state sovereignty as natural law) defensible, or does it naturalize a specific institutional choice about how to manage territorial disputes?',
    'Comparative historical analysis of how other multi-ethnic democracies managed comparable secession movements — Canada/Quebec, Belgium/Flanders, UK/Scotland. Examination of whether territories successfully negotiated independence or devolution through negotiation rather than suppression.',
    'If alternatives exist: mountain framing is false summit (contingent choice presented as immutable law). Triggers FSM engine evaluation and potential reclassification to tangled_rope or snare depending on whether coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_legitimacy, conceptual, 'Whether state monopoly on coercion is natural law or contingent institutional choice').

omega_variable(
    surveillance_infrastructure_scope,
    'Does Spanish surveillance infrastructure targeting pro-independence activists extend to ordinary political speech, or is it narrowly tailored to genuine security threats?',
    'Documentation of surveillance targets and scope (Freedom of Information requests, privacy reports, technical surveillance audits); comparison with surveillance infrastructure deployed against other political movements (Basque nationalism, regional socialism); assessment of whether surveillance meets European data-protection and proportionality standards',
    'If narrowly tailored: suppression has some rule-of-law structure (snare with procedural constraints). If broadly applied to all independence rhetoric: suppression is mass surveillance (snare with enhanced extraction via epistemic closure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_infrastructure_scope, empirical, 'Scope of surveillance targeting political opposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spanish_political_opposition_suppression, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spos_tr_t0, spanish_political_opposition_suppression, theater_ratio, 0, 0.48).
narrative_ontology:measurement(spos_tr_t3, spanish_political_opposition_suppression, theater_ratio, 3, 0.6).
narrative_ontology:measurement(spos_tr_t6, spanish_political_opposition_suppression, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(spos_be_t0, spanish_political_opposition_suppression, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spos_be_t3, spanish_political_opposition_suppression, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(spos_be_t6, spanish_political_opposition_suppression, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(spos_su_t0, spanish_political_opposition_suppression, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(spos_su_t3, spanish_political_opposition_suppression, suppression_requirement, 3, 0.72).
narrative_ontology:measurement(spos_su_t6, spanish_political_opposition_suppression, suppression_requirement, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spanish_political_opposition_suppression, enforcement_mechanism).
narrative_ontology:affects_constraint(spanish_political_opposition_suppression, catalan_identity_lock_institutional).
narrative_ontology:affects_constraint(spanish_political_opposition_suppression, spanish_judicial_independence_degradation).
narrative_ontology:affects_constraint(spanish_political_opposition_suppression, european_rule_of_law_contamination).

% DUAL FORMULATION NOTE:
% Spanish political opposition suppression operates as a single primary constraint (snare) affecting multiple downstream domains: identity-lock dynamics for affected activists (separate story), judicial institutional capture (separate story), and contamination of EU rule-of-law standards (separate story). Each downstream constraint has its own ε and perspectives; this story focuses on the suppression mechanism itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(spanish_political_opposition_suppression, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
