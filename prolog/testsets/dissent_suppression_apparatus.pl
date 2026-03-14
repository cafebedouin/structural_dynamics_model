% ============================================================================
% CONSTRAINT STORY: dissent_suppression_apparatus
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dissent_suppression_apparatus, []).

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
 *   constraint_id: dissent_suppression_apparatus
 *   human_readable: Dissent Suppression Apparatus
 *   domain: political/social/institutional
 *
 * SUMMARY:
 *   Dissent suppression apparatus encompasses the institutional mechanisms
 *   through which state and incumbent power structures prevent organized
 *   opposition, neutralize vocal critics, and maintain information monopoly.
 *   This constraint operates through legal persecution, economic retaliation,
 *   social ostracism, credibility destruction, and violence — sometimes
 *   individually, usually in combination. The apparatus exhibits strong
 *   perspectival divergence: for the state it is coordination (maintaining
 *   stability), for the dissident it is a snare (inescapable extraction), for
 *   the organized resistance it is temporary (scaffold with regime collapse
 *   endpoint), for international observers it is pure extraction. The
 *   extractiveness trend shows accumulation: initial suppression targets
 *   explicit opposition, but over time it expands to pre-emptive silencing of
 *   potential critics, self-censorship internalization, and creation of fear
 *   as preventive mechanism. Theater ratio increases as the apparatus
 *   matures: early-stage suppression is crude violence, late-stage
 *   suppression is performative legality (show trials, official channels that
 *   report dissent, fake opposition that legitimizes regime as democratic).
 *   The mandatrophy is resolved through multi-perspectival analysis: the
 *   state sees Rope, the dissident sees Snare, the analyst sees Snare with
 *   temporal degradation toward pure theater (Piton).
 *
 * KEY AGENTS:
 *   - Silenced Dissidents: Primary victims (powerless/trapped) — face legal persecution, economic loss, social destruction; no viable exit
 *   - Cautious Critics: Secondary victims (moderate/constrained) — retain some agency and platform but self-censor extensively; exit possible at high cost
 *   - State Apparatus: Primary beneficiary (institutional/arbitrage) — maintains power monopoly, prevents opposition coalition, extracts compliance
 *   - Incumbent Power Structures: Secondary beneficiary (institutional/arbitrage) — protected from accountability, retain resource and information advantages
 *   - Independent Media: Victim (institutional/constrained) — face censorship, licensing revocation, advertising suppression; some institutional protection through international pressure
 *   - International Resistance Networks: Organized actors (organized/constrained) — exile groups, diaspora, international NGOs; see apparatus as temporary, work toward regime change
 *   - Compliance Theater Infrastructure: Institutional actor (institutional/arbitrage) — state media, official narrative, propaganda systems; increasingly performative as suppression deepens
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees apparatus as pure extraction with false coordination narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dissent_suppression_apparatus, 0.68).
domain_priors:suppression_score(dissent_suppression_apparatus, 0.82).
domain_priors:theater_ratio(dissent_suppression_apparatus, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dissent_suppression_apparatus, extractiveness, 0.68).
narrative_ontology:constraint_metric(dissent_suppression_apparatus, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(dissent_suppression_apparatus, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dissent_suppression_apparatus, snare).
narrative_ontology:human_readable(dissent_suppression_apparatus, "Dissent Suppression Apparatus").
narrative_ontology:topic_domain(dissent_suppression_apparatus, "political/social/institutional").

domain_priors:requires_active_enforcement(dissent_suppression_apparatus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dissent_suppression_apparatus, state_apparatus).
narrative_ontology:constraint_beneficiary(dissent_suppression_apparatus, incumbent_power_structures).
narrative_ontology:constraint_victim(dissent_suppression_apparatus, dissident_voices).
narrative_ontology:constraint_victim(dissent_suppression_apparatus, political_opposition).
narrative_ontology:constraint_victim(dissent_suppression_apparatus, marginalized_communities).
narrative_ontology:constraint_victim(dissent_suppression_apparatus, independent_media).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SILENCED DISSIDENT (SNARE) — Faces legal persecution, economic retaliation, social ostracism, and credibility destruction for expressing dissent. Material barriers to exit include criminal liability, asset seizure, and loss of employment. No viable alternative to silence or exile. Trapped agent with maximum experienced extraction.
constraint_indexing:constraint_classification(dissent_suppression_apparatus, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAUTIOUS CRITIC (TANGLED ROPE) — Moderate power through professional position or limited platform. Can speak cautiously on approved topics but faces career damage, platform removal, or social penalty for crossing regime boundaries. Benefits from coordination on safe topics (public discourse exists) while bearing asymmetric extraction through self-censorship. High suppression but not total trap — exit possible at significant cost.
constraint_indexing:constraint_classification(dissent_suppression_apparatus, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE APPARATUS (ROPE) — Primary beneficiary from suppression apparatus. Experiences constraint as coordination mechanism: maintains regime stability, prevents coalition formation among opposition, ensures information monopoly. For the state, the apparatus solves the collective action problem of holding power. Net extraction flows toward this actor.
constraint_indexing:constraint_classification(dissent_suppression_apparatus, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL RESISTANCE (SCAFFOLD) — Organized diaspora, exile networks, and international NGOs see suppression apparatus as temporary — constrained by information flow leaks, international pressure, generational turnover, and the inherent instability of coercive control. Exit mechanism is regime collapse or normalization. Temporal arc shows suppression apparatus as historically contingent, not permanent.
constraint_indexing:constraint_classification(dissent_suppression_apparatus, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPLIANCE THEATER (PITON) — Institutional mechanisms (official media, state-controlled narrative, propaganda infrastructure) persist through inertia long after effectiveness has degraded. High theater ratio (0.65): formal structures of public participation, state media presenting as independent, official dissent channels that serve to collect intelligence rather than enable genuine voice. The machinery operates because it must, not because it works — it has become degraded performance.
constraint_indexing:constraint_classification(dissent_suppression_apparatus, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global view, systematic suppression of dissent is extractive across all observables and time horizons. Coordination benefit is minimal; extraction is the primary function. The apparatus prevents coalition formation, captures surplus from suppressed populations, and creates generational trauma. No natural law framing applies — this is a contingent institutional choice, not an immutable constraint.
constraint_indexing:constraint_classification(dissent_suppression_apparatus, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dissent_suppression_apparatus_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dissent_suppression_apparatus, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dissent_suppression_apparatus, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dissent_suppression_apparatus, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dissent_suppression_apparatus, TR),
    TR >= 0.70.

:- end_tests(dissent_suppression_apparatus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The apparatus systematically transfers resources (through fines, asset seizure, lost income), status (through reputation destruction), and agency (through coerced silence) from dissidents to the state. The measurement trend shows extraction accumulating over time as suppression mechanisms layer — legal persecution + economic retaliation + social ostracism + credential revocation = compounding extraction. Initial suppression targets explicit opposition, escalation extends to pre-emptive silence of potential critics. Suppression (0.82): Very high. Multiple overlapping barriers prevent exit: criminal liability for dissent, visa/travel restrictions, economic asset freezes, social isolation, credibility destruction, physical safety risk. Barriers are structural (legal frameworks) and social (reputation effects persist post-exit). Suppression does not require constant violence — the apparatus maintains itself through threat, internalized fear, and institutional inertia. Theater ratio (0.65): Moderate-high. Early-stage suppression is crude and transparently coercive (visible violence, arbitrary arrest). Late-stage suppression develops performative structures: official media presenting as independent news, state-controlled opposition that legitimizes regime, show trials that appear legal, official dissent channels that function as intelligence collection. Theater increases as apparatus matures because crude suppression becomes politically costly; performative suppression achieves same extraction with lower international pressure.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The state apparatus sees Rope — suppression solves the coordination problem of regime stability and prevents destabilizing opposition coalitions. The dissident sees Snare — trapped by legal, economic, and social barriers with no exit option. The cautious critic sees Tangled Rope — benefits from public discourse and professional standing while bearing extraction through self-censorship and constant threat. The organized resistance sees Scaffold — apparatus is historically contingent, regime collapse or generational change will remove it, international pressure can weaken it. The compliance theater sees Piton — the performative structures (state media, official opposition) persist through institutional inertia despite declining effectiveness; bureaucracy maintains itself. The analytical observer sees Snare — no genuine coordination benefit, only extraction with false justification narrative. The perspectival gap reveals the core structural question: does suppression apparatus solve a real coordination problem (regime stability) or is stability claimed as justification for extraction that could be avoided through non-coercive governance?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status combined with exit options. Dissidents classified as powerless/trapped exhibit d ≈ 0.95 (maximum target): no arbitrage options, no mobile escape, material barriers comprehensive. Cautious critics as moderate/constrained exhibit d ≈ 0.65-0.75: some agency (professional position, limited platform) but exit costs are high enough to sustain self-censorship. State apparatus as institutional/arbitrage exhibits d ≈ 0.05-0.15 (maximum beneficiary): arbitrage access means they can exit constraint while maintaining power (migrate, reframe, shift repression mechanisms); they set the rules. The sigmoid f(d) converts these d values to experienced extractiveness multipliers: powerless/trapped agents experience the apparatus as maximal extraction; institutional/arbitrage agents experience it as coordination benefit. Scope modifier σ(national) = 1.0, so effective extraction χ = ε × f(d) × 1.0. For a dissident, χ ≈ 0.68 × 1.42 ≈ 0.96 (severe snare). For the state, χ ≈ 0.68 × (-0.12) ≈ -0.08 (negative extraction = coordination benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that suppression apparatus is a genuine Snare (high extraction, high suppression, minimal coordination benefit) from the structural perspective of the dissident, but the state perceives it as Rope (coordination that solves the regime stability problem). This is not a contradiction — it is a directionality gap. The apparatus IS extractive from the victim's perspective; it IS coordination-solving from the beneficiary's perspective. Both are true. The mandatrophy dissolves when we recognize that 'is it extraction or coordination?' is the wrong question. The right question is 'who experiences it as extraction vs. coordination?' The answer is determined by beneficiary/victim status and exit options. The classification Snare is mandatorily claimed because the base_properties extractiveness (0.68) exceeds the snare threshold (0.46) and suppression (0.82) exceeds the threshold (0.60). The analytical observer at the civilizational scale confirms Snare: over decades, suppression apparatus generates accumulated grievance, hidden opposition growth, generational trauma, and eventual destabilization — it does not actually stabilize regimes long-term. The coordination narrative ('suppression maintains stability') is empirically false — regimes with suppression apparatus experience higher revolution/collapse rates than those with open dissent channels. Therefore the apparatus is extractive with false justification, not coordination with extraction side effect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the suppression apparatus provide any genuine coordination benefit (e.g., preventing violence, enabling unified response) or is it pure extraction with coordination claimed as justification?',
    'Comparative historical analysis: societies with suppression vs. open dissent channels during crises (pandemic, war, economic shock). Measure outcome variance and conflict escalation.',
    'If coordination benefit exists: reclassify closer to Tangled Rope. If coordination benefit is rhetorical only: confirms Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether suppression apparatus provides genuine coordination or pure extraction').

omega_variable(
    identity_lock_vs_trapped_distinction,
    'Are dissidents trapped by material barriers or by internalized belief that dissent is futile/dangerous (identity-locked into silence)?',
    'Post-escape trajectories: if dissidents freed from apparatus continue self-censoring, suppression is partly internalized. Generational analysis: do children of dissidents show reduced suppression effects?',
    'If identity-locked dominant: suppression is more resilient to structural change; requires cognitive reframing not just barrier removal. If trapped dominant: barrier removal enables rapid exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_trapped_distinction, empirical, 'Whether dissidents are structurally trapped or identity-locked into silence').

omega_variable(
    regime_stability_function,
    'Does suppression apparatus actually stabilize regime or does it destabilize through accumulated grievance and hidden opposition growth?',
    'Longitudinal regime stability data: correlation between suppression intensity and regime duration; hidden opposition growth rates; revolution/collapse timing relative to suppression escalation.',
    'If stabilizing: apparatus provides coordination benefit (lower classification). If destabilizing: apparatus is purely extractive with false justification (confirms Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regime_stability_function, empirical, 'Whether suppression apparatus stabilizes or destabilizes regime').

omega_variable(
    exit_route_viability,
    'For constrained critics, is emigration/platform migration actually viable or is it foreclosed by asset freezes, visa restrictions, and exile dangers?',
    'Availability and cost of exit routes over time; proportion of constrained agents actually achieving exit; barriers encountered by those attempting exit.',
    'If viable: classification should be constrained. If foreclosed: reclassify constrained agents as trapped, raising overall extraction experienced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_route_viability, empirical, 'Whether exit routes for constrained critics are actually viable').

omega_variable(
    measurement_basis_stability,
    'Does the apparatus persist in same form regardless of observable (legal framework, violence level, information control mechanism) or does classification change by measurement?',
    'Measure extractiveness via legal persecution only vs. via economic retaliation only vs. via social ostracism only. Do ε values cluster (single constraint) or diverge (multiple constraints)?',
    'If stable: single constraint story. If diverges: decompose into separate stories (legal suppression, economic suppression, social suppression) with ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_basis_stability, conceptual, 'Whether suppression apparatus is single constraint or decomposable mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dissent_suppression_apparatus, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(diss_tr_t0, dissent_suppression_apparatus, theater_ratio, 0, 0.42).
narrative_ontology:measurement(diss_tr_t10, dissent_suppression_apparatus, theater_ratio, 10, 0.54).
narrative_ontology:measurement(diss_tr_t20, dissent_suppression_apparatus, theater_ratio, 20, 0.65).
narrative_ontology:measurement(diss_tr_t30, dissent_suppression_apparatus, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(diss_be_t0, dissent_suppression_apparatus, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(diss_be_t10, dissent_suppression_apparatus, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(diss_be_t20, dissent_suppression_apparatus, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(diss_be_t30, dissent_suppression_apparatus, base_extractiveness, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dissent_suppression_apparatus, enforcement_mechanism).
narrative_ontology:affects_constraint(dissent_suppression_apparatus, state_capacity_concentration).
narrative_ontology:affects_constraint(dissent_suppression_apparatus, information_monopoly).
narrative_ontology:affects_constraint(dissent_suppression_apparatus, opposition_coalition_formation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dissent_suppression_apparatus, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
