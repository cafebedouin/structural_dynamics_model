% ============================================================================
% CONSTRAINT STORY: blame_attribution_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blame_attribution_equilibrium, []).

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
 *   constraint_id: blame_attribution_equilibrium
 *   human_readable: Blame Attribution Equilibrium
 *   domain: social/institutional/psychological
 *
 * SUMMARY:
 *   Blame attribution equilibrium is a structural mechanism by which
 *   institutions (organizations, states, communities) manage accountability
 *   for failures by distributing blame asymmetrically according to power
 *   rather than causal responsibility. The constraint coordinates
 *   institutional survival and internal cohesion while systematically
 *   extracting credibility, rights, and resources from lower-power groups.
 *   The mechanism is genuinely functional — it prevents institutional
 *   collapse under the weight of systemic failure accountability — but relies
 *   on suppressing alternative attribution systems and asymmetric information
 *   about true causality. The constraint exhibits hallmarks of tangled rope:
 *   legitimate coordination function (preventing institutional fragmentation)
 *   combined with systematic extraction (protecting powerful actors by
 *   exposing vulnerable ones). Theater ratio of 0.68 reflects increasing
 *   performative content: modern investigations, hearings, and accountability
 *   rituals show decreasing correlation with independent causal analysis,
 *   indicating the primary function has shifted from determining
 *   responsibility to producing institutional legitimacy.
 *
 * KEY AGENTS:
 *   - Scapegoated Groups: Primary victims (powerless/trapped) — blamed for systemic failures; bear legal, economic, and reputational costs; cannot exit without abandoning identity or livelihood
 *   - Institutional Beneficiaries: Primary beneficiaries (institutional/arbitrage) — protected from accountability; can shift blame narratives or selectively exonerate as needed
 *   - Middle Institutional Layer: Secondary actors (moderate/constrained) — implement blame attribution on behalf of authority; experience coordination benefit (alignment) and extraction risk (career exposure)
 *   - Reform Coalition: Organized advocates (organized/mobile) — work to dismantle blame attribution through investigation and norm-setting; expect institutional sunset as transparency increases
 *   - Blame Attribution Ritual: Institutional mechanism (institutional/arbitrage) — formal procedures (courts, inquiries, investigations) maintain high theater with decreasing functional accuracy
 *   - Analytical Observer: External analysis (analytical/analytical) — identifies constraint as tangled rope with measurable asymmetry in blame distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blame_attribution_equilibrium, 0.58).
domain_priors:suppression_score(blame_attribution_equilibrium, 0.62).
domain_priors:theater_ratio(blame_attribution_equilibrium, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blame_attribution_equilibrium, extractiveness, 0.58).
narrative_ontology:constraint_metric(blame_attribution_equilibrium, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(blame_attribution_equilibrium, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blame_attribution_equilibrium, tangled_rope).
narrative_ontology:human_readable(blame_attribution_equilibrium, "Blame Attribution Equilibrium").
narrative_ontology:topic_domain(blame_attribution_equilibrium, "social/institutional/psychological").

domain_priors:requires_active_enforcement(blame_attribution_equilibrium).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blame_attribution_equilibrium, dominant_institutional_actors).
narrative_ontology:constraint_beneficiary(blame_attribution_equilibrium, blame_redistributors).
narrative_ontology:constraint_victim(blame_attribution_equilibrium, scapegoated_groups).
narrative_ontology:constraint_victim(blame_attribution_equilibrium, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCAPEGOATED GROUP (SNARE) — Blamed for systemic failures they did not cause or could not prevent. Trapped by legal liability, employment termination, loss of professional standing, or community expulsion. Cannot exit the blame attribution without abandoning identity or livelihood. Bears extraction in the form of reputational damage, economic penalty, and psychological cost. No meaningful coordination benefit — the system coordinates others' exoneration by the group's sacrifice.
constraint_indexing:constraint_classification(blame_attribution_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE INSTITUTIONAL LAYER (TANGLED ROPE) — Implements blame attribution on behalf of higher authorities but also constrained by reputational and legal exposure. Experiences genuine coordination benefit (aligns with authority, distributes internal conflict) alongside significant extraction (career risk if attribution fails, moral friction). Exit is costly — requires whistleblowing or institutional exit — but possible. The coordination function is real: blame attribution reduces internal organizational conflict.
constraint_indexing:constraint_classification(blame_attribution_equilibrium, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL BENEFICIARY (ROPE) — Primary benefit from blame attribution equilibrium. Protects organization, leadership, or dominant coalition from accountability for failures. Experiences the constraint as pure coordination: redirecting blame consolidates internal alignment and prevents institutional fragmentation. Net beneficiary with optionality — can shift blame narratives, change scapegoats, or selectively exonerate when politically necessary (arbitrage option).
constraint_indexing:constraint_classification(blame_attribution_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized advocates (lawyers, journalists, civil rights organizations) working to dismantles blame attribution mechanisms through investigation, litigation, and norm-setting. See the constraint as temporary institutional architecture vulnerable to factual exposure and legal pressure. Expect sunset: as organizational opacity declines and documentation requirements increase, blame attribution becomes harder to sustain. Mobile exit — reformers can withdraw effort and declare victory as norms shift.
constraint_indexing:constraint_classification(blame_attribution_equilibrium, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: BLAME ATTRIBUTION RITUAL AS DEGRADED INSTITUTION (PITON) — The formal procedures of investigation, adjudication, and accountability (courts, HR hearings, public inquiries) maintain high theater while decreasing functional attribution. Modern attribution rituals are protracted, expensive, and often produce predetermined conclusions. The functional mechanism (identifying true causal responsibility) has atrophied; the performative mechanism (appearing to seek accountability) persists through institutional inertia. Theater ratio 0.68 reflects this degradation.
constraint_indexing:constraint_classification(blame_attribution_equilibrium, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilization-level analysis reveals blame attribution as a genuine coordination mechanism (dispersing collective frustration and preventing internal institutional collapse) that is systematically exploited for asymmetric extraction (protecting powerful actors while exposing vulnerable ones). The constraint has BOTH real coordination function AND systematic bias in distribution. Classification as tangled rope is robust across empirical observables: causal analysis shows scapegoating, network analysis shows asymmetric blame flows, historical analysis shows institutional pattern repeats.
constraint_indexing:constraint_classification(blame_attribution_equilibrium, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blame_attribution_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blame_attribution_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blame_attribution_equilibrium, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blame_attribution_equilibrium, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(blame_attribution_equilibrium, TR),
    TR >= 0.70.

:- end_tests(blame_attribution_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The blame attribution system produces real extraction from scapegoated groups through legal liability, employment termination, reputational damage, and psychological cost. But extraction is not total — it requires active maintenance and can be resisted (hence tangled rope, not snare). The measurement shows growth from 0.35 to 0.58 over the interval, indicating that as organizational complexity increases and causal responsibility becomes harder to determine, blame attribution becomes increasingly extractive. Suppression (0.62): Moderate-high. Significant barriers to exit include legal liability (once scapegoated, economic and legal penalties accumulate), social isolation (reputation effects spread beyond original failure), and epistemological barriers (alternative causal accounts are suppressed in favor of institutional narrative). However, suppression is not total — whistleblowers, investigators, and journalists can sometimes pierce the narrative, and litigation can reverse attributions. Theater ratio (0.68): High and increasing. The formal accountability processes (investigations, hearings, trials) produce high performative content: they appear to seek truth and accountability while actual outcomes correlate weakly with independent causal analysis. As organizational documentation has increased, the gap between what records show and what attributions conclude has widened, indicating increased theater.
 *
 * PERSPECTIVAL GAP:
 *   The scapegoated group experiences the constraint as pure extraction (Snare) — they bear costs for failures they did not cause and cannot prevent. The institutional beneficiary experiences it as pure coordination (Rope) — blame attribution solves the problem of preserving institutional alignment when systemic failures occur. The middle institutional layer experiences it as mixed (Tangled Rope) — they benefit from hierarchy alignment but risk career exposure if attribution fails. The analytical observer identifies the gap as structural: the coordination function is real but depends on asymmetric extraction. If blame were distributed symmetrically according to actual causal responsibility, the coordination function would collapse because it would require powerful actors to accept accountability. The constraint persists because the asymmetry IS the coordination mechanism — it works by protecting some at the expense of others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position. Scapegoated groups have high d (~0.92) because they are systemic victims with no exit options — blame becomes part of their social identity, compounding over time. Institutional beneficiaries have low d (~0.08) because they benefit from the extraction without bearing costs — the constraint flows toward them. Middle institutional layer has moderate d (~0.58) because they implement the system but also face reputational and legal exposure. Reform coalitions have moderate d (~0.55) because they are organized and mobile but fighting against institutional inertia. The analytical observer has high d (~0.75) because they see the full asymmetry but cannot exit the civilization in which blame attribution operates. All directionality values feed the sigmoid f(d) to produce experienced extractiveness (chi), explaining why the same constraint produces snare, rope, and tangled rope classifications from different perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves the mandatrophy by identifying both the genuine coordination function (blame attribution prevents institutional collapse under systemic failure) and the systematic extraction (powerful actors are protected while vulnerable actors are exposed). The constraint is NOT pure coordination masking as extraction (which would be a snare falsely labeled rope); it is hybrid coordination-extraction where the asymmetry is constitutive of the coordination function. This is distinct from the false summit case: blame attribution genuinely serves coordination; the problem is that it does so through asymmetric extraction. Resolving the mandatrophy requires acknowledging both facts: the system works (prevents institutional fragmentation) AND it is extractive (distributes accountability according to power, not causality). Reform that preserves institutional stability while enabling symmetric accountability (radical transparency, statistical causal analysis) would transform this into a rope constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_accuracy_vs_equilibrium,
    'Is the blame attribution system failing to identify true causality (epistemic failure) or systematically distributing blame according to power asymmetries (structural mechanism)?',
    'Post-hoc causal analysis: compare institutional blame assignments with independent forensic investigation or counterfactual analysis; measure correlation between blamed agent''s power level and assignment frequency',
    'If epistemic failure: constraint is primarily snare (extraction via incapacity). If structural mechanism: constraint is tangled rope (genuine coordination + systematic asymmetry). If both: tangled rope is confirmed — coordination depends on the bias.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attribution_accuracy_vs_equilibrium, empirical, 'Whether blame attribution failure is epistemic or structural').

omega_variable(
    scapegoat_replaceability,
    'Are scapegoat targets interchangeable (blame is displaced according to opportunity) or do they have intrinsic properties that explain selection?',
    'Comparison of blamed groups across similar institutional failures; analysis of whether same group is blamed repeatedly or different groups blamed for different failures in same institution; interview data on blame selection rationale',
    'If interchangeable: blame attribution is pure extraction (Snare dominant). If intrinsic: attribution has some causal grounding (Tangled Rope confirmed). If systematically biased by power: proves structural mechanism (Tangled Rope confirmed with measured asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scapegoat_replaceability, empirical, 'Whether scapegoat selection reflects opportunity or causal connection').

omega_variable(
    institutional_alternative_collapse,
    'Would institutions collapse (fragmenting under mutual recrimination) without blame attribution, or can accountability be distributed symmetrically without catastrophic internal conflict?',
    'Historical cases where blame attribution mechanisms failed or were rejected; observation of organizations with transparent accountability; simulation of zero-blame environments',
    'If collapse without blame: coordination function is genuine and necessary (Rope possible from institutional perspective). If no collapse: coordination function is performative cover (Snare from victim perspective validated; Piton for ritual).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_alternative_collapse, conceptual, 'Whether blame attribution is functionally necessary for institutional stability').

omega_variable(
    reform_coalition_effectiveness,
    'Do transparency mechanisms, investigation standards, and litigation actually reduce blame attribution distortion or do they create new equilibria of performative accountability?',
    'Measurement of blame attribution bias before and after reform interventions; comparison of pre-reform and post-reform blame distributions; analysis of whether investigation procedures discover exculpatory evidence at higher rates after reform',
    'If effective: Scaffold classification confirmed with real sunset. If performative: reforms create Piton dynamics — high theater, low functional change. If partial: Tangled Rope persists with reduced suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_coalition_effectiveness, empirical, 'Whether institutional reforms reduce blame attribution bias or recreate it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blame_attribution_equilibrium, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blame_tr_t0, blame_attribution_equilibrium, theater_ratio, 0, 0.48).
narrative_ontology:measurement(blame_tr_t2, blame_attribution_equilibrium, theater_ratio, 2, 0.56).
narrative_ontology:measurement(blame_tr_t4, blame_attribution_equilibrium, theater_ratio, 4, 0.62).
narrative_ontology:measurement(blame_tr_t6, blame_attribution_equilibrium, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(blame_be_t0, blame_attribution_equilibrium, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(blame_be_t2, blame_attribution_equilibrium, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(blame_be_t4, blame_attribution_equilibrium, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(blame_be_t6, blame_attribution_equilibrium, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blame_attribution_equilibrium, enforcement_mechanism).
narrative_ontology:affects_constraint(blame_attribution_equilibrium, institutional_opacity).
narrative_ontology:affects_constraint(blame_attribution_equilibrium, accountability_asymmetry).
narrative_ontology:affects_constraint(blame_attribution_equilibrium, scapegoating_cycles).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(blame_attribution_equilibrium, institutional, 0.12).
constraint_indexing:directionality_override(blame_attribution_equilibrium, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
