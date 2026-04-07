% ============================================================================
% CONSTRAINT STORY: stereotype_formation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stereotype_formation, []).

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
 *   constraint_id: stereotype_formation
 *   human_readable: Stereotype Formation as Cognitive Coordination and Identity Extraction
 *   domain: social/cognitive/identity
 *
 * SUMMARY:
 *   Stereotype formation is a cognitive and social constraint that
 *   coordinates rapid identity categorization while extracting from the
 *   stereotyped group through identity lock, epistemic injustice, and
 *   stereotype threat. The constraint exhibits dual function: it genuinely
 *   reduces cognitive load for categorizers and enables institutional sorting
 *   (coordination), yet simultaneously extracts from targets through
 *   psychological costs, reduced individual autonomy, and systematic
 *   credibility deficits (extraction). The extractiveness value (0.58)
 *   reflects that while genuine coordination benefits exist, they are
 *   asymmetrically distributed — beneficiaries experience coordination
 *   without extraction; targets experience extraction despite the
 *   coordination function. The suppression value (0.65) reflects multiple
 *   reinforcement mechanisms: internalized identity fusion (target cannot
 *   imagine themselves outside the stereotype), social enforcement
 *   (in-group/out-group sanctions for non-conformity), and institutional
 *   enforcement (hiring, housing, credit sorting based on stereotype). The
 *   theater_ratio (0.68) captures the performative component of stereotype
 *   maintenance: explicit stereotype articulation is often socially
 *   sanctioned (performative), yet behavioral conformity and institutional
 *   sorting continue (functional). The constraint shows measurable drift over
 *   the 10-year interval: extractiveness increased as awareness of stereotype
 *   harm grew without corresponding institutional change, causing stereotype
 *   maintenance to require more active enforcement; theater ratio increased
 *   as explicit stereotyping became more socially taboo, shifting the
 *   mechanism from overt articulation to implicit behavioral and
 *   institutional enforcement.
 *
 * KEY AGENTS:
 *   - Stereotyped Individual: Primary victim (powerless/identity-locked) — bears cognitive load, identity bifurcation, epistemic injustice; cannot exit despite structural mobility because identity is fused with the stereotyped category
 *   - In-Group Member: Secondary beneficiary and victim (moderate/constrained) — benefits from in-group coordination but also imprisoned by stereotype rigidity; constrained exit because leaving the stereotype group entails social cost
 *   - Dominant Group Member: Primary beneficiary (powerful/arbitrage) — benefits from stereotype-based shortcuts in cognition, hiring, social sorting; experiences zero personal extraction
 *   - Stereotype-Reinforcing Institution: Institutional beneficiary (institutional/arbitrage) — uses stereotypes for administrative efficiency and institutional sorting; can exit to alternative classification systems if incentives change
 *   - Anti-Stereotype Coalition: Organized agent (organized/mobile) — sees stereotype formation as temporary coordination failure with sunset via cognitive retraining, diversity exposure, and institutional norm change
 *   - Evolutionary Cognition Perspective: Institutional observer (institutional/arbitrage) — sees stereotypes as atrophied but residually functional heuristics maintained through institutional inertia despite known harms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes stereotype formation as persistent institutional feature combining genuine coordination with asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stereotype_formation, 0.58).
domain_priors:suppression_score(stereotype_formation, 0.65).
domain_priors:theater_ratio(stereotype_formation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stereotype_formation, extractiveness, 0.58).
narrative_ontology:constraint_metric(stereotype_formation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(stereotype_formation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stereotype_formation, tangled_rope).
narrative_ontology:human_readable(stereotype_formation, "Stereotype Formation as Cognitive Coordination and Identity Extraction").
narrative_ontology:topic_domain(stereotype_formation, "social/cognitive/identity").

domain_priors:requires_active_enforcement(stereotype_formation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stereotype_formation, dominant_group_members).
narrative_ontology:constraint_beneficiary(stereotype_formation, stereotype_reinforcing_institutions).
narrative_ontology:constraint_victim(stereotype_formation, stereotyped_group_members).
narrative_ontology:constraint_victim(stereotype_formation, individual_epistemic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STEREOTYPED INDIVIDUAL (SNARE) — Identity-locked to the stereotype. Structurally mobile (could relocate, change careers, find different social contexts) but identity-fused with the stereotyped category. Cannot exercise exit options because exiting the stereotype would require abandoning the identity frame that makes sense of their social position. Experiences maximum extraction: constant cognitive load of stereotype threat, identity bifurcation, epistemic injustice.
constraint_indexing:constraint_classification(stereotype_formation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: STEREOTYPE-REINFORCING INSTITUTION (ROPE) — Benefits from stereotype via reduced cognitive load in categorization, institutional sorting, and resource allocation. Experiences the stereotype as pure coordination: rapid identity classification enables administrative efficiency, hiring heuristics, and organizational sorting. Zero experienced extraction — the institution extracts from targets but not from itself. Net beneficiary with exit options (can adopt alternative classification systems).
constraint_indexing:constraint_classification(stereotype_formation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: IN-GROUP MEMBER (TANGLED ROPE) — Moderate power, constrained exit. Genuinely benefits from stereotype-based in-group coordination (shared identity, collective action, resource pooling), but also bears costs: stereotype rigidifies their own identity, limits their individual expression, and commits them to defending a cognitive category they may not fully endorse. Both beneficiary and victim — mixed experience with genuine coordination function and asymmetric extraction.
constraint_indexing:constraint_classification(stereotype_formation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANTI-STEREOTYPE COALITION (SCAFFOLD) — Organized agents (diversity initiatives, critical consciousness pedagogy, intersectional frameworks) see stereotype formation as a temporary coordination failure with a sunset. Constraint has low effective extraction from the coalition's perspective because they have agency and visibility of exit paths: cognitive retraining, exposure diversity, and institutional norm shifts toward individuation. Theater ratio is moderate — some performative DEI work, but genuine cognitive retraining also occurs.
constraint_indexing:constraint_classification(stereotype_formation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EVOLUTIONARY COGNITION (PITON) — Sees stereotype formation as an atrophied but residually functional cognitive heuristic. Humans evolved categorical thinking for rapid environmental threat assessment in small groups; stereotypes are the vestigial machinery running in modern mass-society contexts where it causes extraction. The mechanism persists through institutional inertia (educational socialization, media reinforcement, institutional sorting rules) despite widespread recognition that it causes harm. High theater: we perform 'natural' categorical thinking while knowing it's cognitively constructed.
constraint_indexing:constraint_classification(stereotype_formation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, stereotype formation is a genuine coordination mechanism (enables rapid social navigation, collective identity, institutional sorting) with embedded asymmetric extraction (targets bear cognitive and epistemic costs). The constraint is not a natural law or temporary problem but a persistent institutional feature: coordination benefits accrue to institutional actors and in-group members; extraction costs accrue to stereotyped individuals. Requires active enforcement via media, education, institutional sorting, and intermittent stereotype activation.
constraint_indexing:constraint_classification(stereotype_formation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stereotype_formation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stereotype_formation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stereotype_formation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(stereotype_formation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(stereotype_formation, TR),
    TR >= 0.70.

:- end_tests(stereotype_formation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Initial value (0.35) reflected stereotype as primarily functional with manageable costs. Over the interval, extractiveness increased (0.48 → 0.58) as awareness of stereotype harms (stereotype threat literature, social identity theory empirics) grew without corresponding institutional change, forcing maintainers to enforce stereotypes more actively and targets to bear higher awareness tax. Suppression (0.65): High, reflecting multiple reinforcement layers. Internalized (target self-conforms to avoid social/institutional penalties), social (in-group enforcement of stereotype norms), and institutional (sorting rules, hiring heuristics, media representation) mechanisms all contribute. Barriers to exit include not just material costs (job loss, social rejection) but identity costs (becoming a different person). Theater ratio (0.68): High and increasing. Explicit stereotype articulation has become socially taboo in many contexts, shifting the mechanism from overt statement to implicit behavior and institutional sorting. This performative shift increases theater ratio: more of stereotype maintenance is now 'hidden' in institutional practices, making the functional component less visible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full range of DR classification from single base properties. The stereotyped individual sees pure extraction (Snare) because they experience identity lock and maximum suppression. The institutional beneficiary sees pure coordination (Rope) because they experience zero personal extraction. The in-group member sees mixed coordination and extraction (Tangled Rope) because stereotype provides both in-group coordination benefits and identity rigidity costs. The anti-stereotype coalition sees a temporary problem with visible exit paths (Scaffold) — cognitive retraining, diversity exposure, and institutional norm shifts are building alternative pathways. The evolutionary perspective sees a degraded heuristic maintained by inertia (Piton) — stereotyping persists despite known harms because the machinery is institutionally entrenched. The analytical observer sees a persistent institutional feature (Tangled Rope) combining coordination and extraction at civilizational scale. No single type is correct; all are legitimate perspectival readings of the constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the agent's structural position relative to the stereotype extraction flow. Beneficiaries with arbitrage options (institutional actors, dominant-group members) experience low d values: they can exit stereotype-based categorization without cost (alternative heuristics exist) and benefit from extraction. Targets with identity-lock (stereotyped individuals) experience high d values approaching 1.0: they cannot exercise exit options because doing so requires identity renegotiation, and they bear the full extraction cost. In-group members with constrained exit experience intermediate d values (0.45-0.65): they have some exit capacity (could adopt individual assessment frameworks) but face social costs (in-group rejection). Organized coalitions with mobile exit experience low-moderate d values (0.25-0.45): they have clear exit paths (institutional norm change, cognitive retraining) and face no extraction themselves. The analytical observer (analytical exit) experiences d around 0.72: they see the constraint clearly but are themselves locked in institutional positions that perpetuate stereotyping, creating the oracle gap (they can analyze the constraint but cannot unilaterally exit the stereotyping institutions they inhabit).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY by showing that stereotype formation is genuinely both coordination and extraction, not one disguised as the other. The coordination component is real: stereotypes do reduce cognitive load and enable institutional sorting. The extraction component is real: targets bear identity and epistemic costs asymmetrically. The mandatrophy dissolves when we recognize that the constraint has different classification from different observation points — it is not mislabeled but rather genuinely manifests different aspects depending on position. The mandatrophy resolution framework addresses the risk that stereotyping advocates claim 'it's just efficient coordination' (hiding extraction) while critics claim 'it's pure extraction' (denying coordination benefit). The correct frame is Tangled Rope: genuine coordination with real costs borne asymmetrically. This enables targeted reform: if the goal is coordination with lower extraction, alternative mechanisms (individual-based assessment, transparent criteria, intersectionality frameworks) can preserve coordination benefits while reducing asymmetric harm. If the goal is equal distribution of coordination benefits, in-group and out-group must both benefit from the categorization scheme, which requires dismantling the power asymmetry embedded in the stereotype.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_versus_constrained,
    'Is the stereotyped individual''s inability to exit the stereotype primarily due to identity fusion (cognitive capture) or structural barriers (social/economic consequences of exit)?',
    'Longitudinal tracking of agents who exit stereotyped contexts: do suppression/identity costs persist post-exit? Does identity renegotiation occur gradually or remain locked despite environmental change?',
    'If primarily identity-locked: suppression is internalized and self-perpetuating; intervention requires cognitive reframing. If primarily constrained: suppression is structural; intervention targets barriers. If mixed: both mechanisms required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_versus_constrained, empirical, 'Whether stereotype lock is cognitive or structural').

omega_variable(
    coordination_functionality_actual_versus_rhetorical,
    'Does stereotype-based categorization actually reduce cognitive load and enable coordination, or is this benefit rhetorical cover for extraction?',
    'Experimental comparison: cognitive load and coordination efficiency with stereotype-based sorting vs. individual-information-based sorting. Field observation of institutions that have replaced stereotype-based heuristics with alternative metrics.',
    'If genuine: stereotype formation is Tangled Rope (coordination + extraction). If rhetorical: it is Snare (extraction dressed as coordination). Classification determines whether reform targets can succeed via ''better coordination'' frames vs. requiring adversarial identity disruption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_functionality_actual_versus_rhetorical, empirical, 'Whether stereotype coordination benefit is real or rhetorical').

omega_variable(
    individual_autonomy_measurement,
    'How much of the stereotyped individual''s epistemic autonomy loss is due to the constraint itself vs. due to pre-existing power asymmetries that the stereotype merely reifies?',
    'Comparison of epistemic autonomy levels: (a) stereotyped individuals in high-power positions (e.g., female CEOs, elite athletes from stereotyped groups) vs (b) stereotyped individuals in low-power positions; (c) non-stereotyped individuals in analogous power positions',
    'If constraint is primary cause: removing the stereotype shifts epistemic autonomy. If power asymmetry is primary: stereotype is a symptom, not the root extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_autonomy_measurement, empirical, 'Attribution of epistemic autonomy loss to stereotype vs. pre-existing power').

omega_variable(
    suppression_mechanism_cognitive_social_institutional,
    'What proportion of suppression is internalized (the stereotyped individual self-polices to match the stereotype), social (in-group/out-group rejection of non-conformity), vs. institutional (formal rules and incentives)?',
    'Survey and interview data on sources of stereotype conformity pressure. Observation of suppression levels in anonymous vs. identifiable contexts, and in institutions with explicit anti-stereotype norms vs. institutions with implicit permissiveness.',
    'If primarily internalized: cognitive intervention can shift suppression. If primarily social: in-group norm change required. If primarily institutional: formal rule change required. Most likely mixed; proportion determines intervention target.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_cognitive_social_institutional, empirical, 'Sources of stereotype suppression mechanisms').

omega_variable(
    theater_ratio_measurement_validity,
    'Does the measured theater_ratio (0.68) accurately capture the performative versus functional split in stereotype formation, or does it conflate distinct phenomena?',
    'Disaggregation of theater ratio: measure separately (a) performative stereotype articulation (explicit statements), (b) behavioral stereotype conformity without explicit acknowledgment, (c) institutional sorting based on stereotype. Compare ratio across contexts with explicit norms against stereotyping vs. contexts with permissive norms.',
    'If theater_ratio is valid: constraint classification as Tangled Rope is appropriate. If theater is measured but function is minimal: reclassify as Snare or Piton with higher theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_measurement_validity, empirical, 'Validity of theater ratio measurement for stereotype formation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stereotype_formation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stereo_tr_t0, stereotype_formation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(stereo_tr_t5, stereotype_formation, theater_ratio, 5, 0.55).
narrative_ontology:measurement(stereo_tr_t10, stereotype_formation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(stereo_be_t0, stereotype_formation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stereo_be_t5, stereotype_formation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(stereo_be_t10, stereotype_formation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stereotype_formation, identity_coordination).
narrative_ontology:affects_constraint(stereotype_formation, stereotype_threat).
narrative_ontology:affects_constraint(stereotype_formation, epistemic_injustice).
narrative_ontology:affects_constraint(stereotype_formation, categorical_sorting_institutional).

% DUAL FORMULATION NOTE:
% Stereotype formation decomposes into three structurally distinct constraints: stereotype_threat (immediate psychological cost, higher extractiveness), epistemic_injustice (credibility deficit, medium extractiveness), and categorical_sorting_institutional (administrative efficiency, lower extractiveness but larger scope). This story models the unified mechanism; downstream stories isolate specific instantiations with their own ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stereotype_formation, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
