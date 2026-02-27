% ============================================================================
% CONSTRAINT STORY: toxic_social_infection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_toxic_social_infection, []).

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
 *   constraint_id: toxic_social_infection
 *   human_readable: The Infecting Character (Psychic Vampire)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The infecting character—or 'psychic vampire'—is a social phenomenon in
 *   which a specific individual's chronic instability, emotional
 *   dysregulation, or personality pathology creates an extractive constraint
 *   on those in proximity. Unlike deliberate manipulation or coercion, the
 *   extraction operates through the character's radiation of disaster,
 *   crisis, and emotional emergency. Those nearby are drawn into caretaking,
 *   crisis response, emotional absorption, and management of the person's
 *   recurring crises. The constraint extracts attention, emotional labor,
 *   psychological stability, and relational bandwidth from the proximity
 *   network while offering no reciprocal coordination benefit. The infecting
 *   character often lacks intent to harm—the extraction is a structural
 *   property of their dysregulation—yet the effect is severe and persistent.
 *   Social containers (families, workplaces, communities) develop rituals and
 *   role specializations to manage the person, leading to a piton-like
 *   degradation in which the container maintains the person through theater
 *   rather than functional change. The constraint violates the assumption
 *   that proximity relationships are fundamentally reciprocal, and creates a
 *   peculiar moral and practical problem: the person is often suffering
 *   themselves, yet their suffering generates suffering in others through
 *   psychological contagion.
 *
 * KEY AGENTS:
 *   - Infecting Character: Unstable source (moderate power, biographical horizon) — chronically dysregulated; generates instability that radiates outward; often lacks conscious intent but creates structural extraction
 *   - Proximity-Exposed Individual: Primary victim (powerless/trapped) — family member, intimate partner, or close coworker; cannot exit without major life cost; bears full psychological and emotional extraction
 *   - Extended Social Network: Secondary victims (moderate/constrained) — friends, colleagues, community members; experience both coordination (shared complaint, mutual support) and extraction (emotional labor of managing the person)
 *   - Organizational Container: Institutional responder (institutional/constrained) — workplace, family system, community; maintains formal procedures and rituals to contain the person despite degraded function; piton-like persistence
 *   - Professional Helper: Bounded responder (powerful/arbitrage) — therapist, counselor, doctor; has role-based boundaries and compensation; experiences coordination rather than extraction
 *   - Analytical Observer: Civilizational witness (analytical/analytical) — sees the infecting character as both a recurring social problem and a coordination failure; recognizes that civilization develops containers (therapy, clergy, peer support) precisely because this dynamic is structural
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(toxic_social_infection, 0.58).
domain_priors:suppression_score(toxic_social_infection, 0.68).
domain_priors:theater_ratio(toxic_social_infection, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(toxic_social_infection, extractiveness, 0.58).
narrative_ontology:constraint_metric(toxic_social_infection, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(toxic_social_infection, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(toxic_social_infection, snare).
narrative_ontology:human_readable(toxic_social_infection, "The Infecting Character (Psychic Vampire)").
narrative_ontology:topic_domain(toxic_social_infection, "social/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_victim(toxic_social_infection, proximity_exposed_individuals).
narrative_ontology:constraint_victim(toxic_social_infection, organizational_morale).
narrative_ontology:constraint_victim(toxic_social_infection, social_network_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROXIMITY-EXPOSED INDIVIDUAL (SNARE) — Family member, coworker, or intimate partner cannot exit the relationship without social cost (family obligations, workplace hierarchy, emotional entanglement). Bears full psychological and emotional extraction. d≈0.92, f(d)≈1.39, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(toxic_social_infection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EXTENDED SOCIAL NETWORK (TANGLED ROPE) — Friends, colleagues beyond immediate proximity experience both coordination benefit (shared complaint, mutual support against the infecting character) and extraction (emotional labor of managing the person, obligation to listen, risk of being drawn into drama). d≈0.68, f(d)≈1.04, σ=0.9 → χ≈0.54.
constraint_indexing:constraint_classification(toxic_social_infection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ORGANIZATIONAL CONTAINER (PITON) — Workplace, family system, or community organization maintains the person through habit and theater despite degraded function. Formal performance reviews, family gatherings, institutional rituals persist as performative containers. theater_ratio=0.64 reflects that much institutional response is procedural ritual rather than functional change. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.40.
constraint_indexing:constraint_classification(toxic_social_infection, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: AWARE MODERATE WITH MOBILE EXIT (SNARE) — Individual in the network who recognizes the dynamic and can theoretically leave but faces real costs: social reputation (labeled as 'cold' or 'unsupportive'), guilt programming, loss of social capital. Mobile exit exists but extraction penalty is high. d≈0.58, f(d)≈0.76, σ=0.8 → χ≈0.35.
constraint_indexing:constraint_classification(toxic_social_infection, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: STRUCTURED PROFESSIONAL (ROPE) — Therapist, doctor, or professional helper who has role-based containment boundaries and paid compensation. Experience is coordination (providing structure, being paid for labor) rather than extraction. d≈0.08, f(d)≈-0.07, σ=0.8 → χ≈-0.04.
constraint_indexing:constraint_classification(toxic_social_infection, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the infecting character as both a coordination problem (social systems need mechanisms for containing disruption) and an extraction mechanism (the instability redistributes burden to those around it). Civilization develops containers (therapy, clergy, peer support) precisely because this dynamic is recurring and structured. d≈0.70, f(d)≈1.13, σ=1.2 → χ≈0.74.
constraint_indexing:constraint_classification(toxic_social_infection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(toxic_social_infection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(toxic_social_infection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(toxic_social_infection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(toxic_social_infection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(toxic_social_infection, TR),
    TR >= 0.70.

:- end_tests(toxic_social_infection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. The infecting character extracts significant emotional labor, attention, and psychological stability from those in proximity, but the extraction is not deliberate or economically quantifiable like debt extraction. The trajectory from 0.35 to 0.58 reflects that exposure time amplifies the extraction mechanism — initial contact involves curiosity and willingness to help (low extraction), but sustained proximity reveals the depth of the person's instability and the reciprocity deficit (extraction increases). The value remains below 0.66 (snare threshold) because some relationships do benefit from association with the person (secondary gains like social coalition formation), and the person's own suffering creates counterbalancing sympathy. Suppression (0.68): High. Multiple barriers prevent exit or refusal to engage: social norms around care and loyalty, guilt programming ('I'm the only one who understands them'), family obligation, workplace hierarchy, and fear of social judgment. The person often creates crises that demand immediate attention, leaving no space for reflection or boundary-setting. Theater ratio (0.64): Elevated. Social response to the infecting character is substantially performative: family gatherings occur despite anticipating chaos, workplace procedures are followed despite knowing they won't change the person's behavior, friends maintain contact despite the relational drain. The theater increases over time as the container learns to manage through ritual rather than expecting behavioral change.
 *
 * PERSPECTIVAL GAP:
 *   The infecting character creates a perspectival divergence that exceeds most constraints. The trapped proximity individual (powerless/trapped) experiences pure snare with minimal escape — the constraint appears totalizing. The extended network (moderate/constrained) experiences tangled rope — they have some organizational agency through group support but remain partially extracted. The organizational container (institutional/constrained) experiences piton — the formal procedures persist despite low functional output. The aware individual with mobile exit (moderate/mobile) experiences snare with an escape option, but the escape carries high psychological cost. The professional helper (powerful/arbitrage) experiences rope — their role provides containment boundaries and compensation. The analytical observer experiences the full complexity: tangled rope at the societal level, because civilization needs both the ability to contain such people (coordination) and accepts the extraction cost (asymmetry). None of these perspectives align, creating a profound misalignment about whether the constraint is primarily a care problem (therapeutic framing) or an extraction problem (victim framing) or a societal regulation problem (institutional framing).
 *
 * DIRECTIONALITY LOGIC:
 *   Proximity-exposed individual: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction relative to power. Cannot exit; bears full instability transfer. Extended network: Victim + constrained → d≈0.68, f(d)≈1.04. Significant extraction but some organizational agency through group support. Aware moderate with mobile exit: Victim + mobile → d≈0.58, f(d)≈0.76. Extraction reduced by exit option, but exit penalty (guilt, reputation) remains high. Professional helper: Beneficiary (compensated labor) + arbitrage → d≈0.08, f(d)≈-0.07. Role-based boundaries and payment flip the relationship; they benefit from the interaction. Organizational container: Not primarily beneficiary or victim, but constrained institutional actor; piton classification derives from theater gate, not directionality. Analytical observer: Sees full structure; d≈0.70, f(d)≈1.13, reflecting recognition that the extraction problem is systemic and unavoidable at scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The infecting character constraint resolves the mandatrophy by distinguishing between intentional snare (predatory extraction, moral culpability) and structural snare (dysregulation radiates extraction despite lack of intent). This prevents mislabeling as rope or scaffold. The rope misclassification would claim that the infecting character's presence 'solves' a coordination problem — it doesn't; it creates one. The scaffold misclassification would claim the extraction is temporary and reducible through structural intervention — it persists unless the person undergoes fundamental change (treatment threshold omega). The snare classification is correct across all proximate perspectives because the extraction is non-reciprocal and the exit barriers are structural. The piton perspective (organizational container maintaining through theater) is a secondary observation: the organization is not being snared, but rather maintaining a snared person through ritual. The professional helper perspective (rope) is correct within bounded role constraints — the helper's role provides protection that the intimate proximity individual lacks. The mandatrophy is resolved by recognizing that moral culpability is decoupled from structural classification: the person can be both genuinely suffering AND causing genuine extraction, with neither canceling the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_vs_structural,
    'Does the infecting character deliberately extract emotional labor and attention, or is the extraction a structural byproduct of their genuine instability?',
    'Analysis of pattern consistency across relationships (deliberate extraction would vary by context to optimize gain; structural extraction is invariant). Interview-based study comparing subject''s reported intentionality against observer-coded behavioral patterns.',
    'If intentional: classification drifts toward deliberate snare; suppression increases, moral culpability enters. If structural: classification remains snare but frames extraction as non-volitional property of the person''s emotional dysregulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_vs_structural, conceptual, 'Whether extraction is intentional or a structural byproduct').

omega_variable(
    contagion_mechanism_threshold,
    'What is the empirical threshold of exposure time or relationship intimacy at which the infecting character''s instability causally transfers to proximity individuals, versus proximity individuals'' distress being reaction rather than contagion?',
    'Longitudinal psychometric tracking of individuals before and after prolonged proximity. Analysis of stress markers, mood disorder incidence, and sleep disruption in those with high vs low exposure. Randomized exposure control to test causal direction.',
    'If contagion threshold is low (<10 hours exposure): almost everyone in proximity becomes infected; extractiveness is higher. If high (>100 hours): only intimate contacts show markers; extraction is more selective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contagion_mechanism_threshold, empirical, 'Exposure threshold for contagion of instability').

omega_variable(
    container_saturation_point,
    'At what point does a social container (family, organization, community) become destabilized by the infecting character''s presence rather than merely stressed?',
    'Organizational collapse case studies; measurement of turnover, productivity, cohesion before/after a high-infection person joins or leaves. Threshold analysis of maximum infectious load per container size.',
    'If saturation is high (person can be absorbed by large systems): suppression is lower, extraction more distributed. If low (person destabilizes any group above critical mass): suppression is higher, network-level effect is more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(container_saturation_point, empirical, 'Organizational saturation point for infectious character disruption').

omega_variable(
    treatment_versus_lifetime_trait,
    'Is the infecting character''s instability treatable through therapy/medication/intervention, or is it a personality-level constant that persists regardless of external intervention?',
    'Longitudinal follow-up of individuals identified as infecting characters who enter therapy vs control groups. Measurement of behavioral change, relationship stability, and infectious output after intervention.',
    'If treatable: extraction is theoretically reducible; scaffolding interventions could have sunset logic. If permanent: extraction is structural and unavoidable; only containment strategies work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treatment_versus_lifetime_trait, empirical, 'Treatability of the infecting character''s core instability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(toxic_social_infection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsi_tr_t0, toxic_social_infection, theater_ratio, 0, 0.48).
narrative_ontology:measurement(tsi_tr_t5, toxic_social_infection, theater_ratio, 5, 0.56).
narrative_ontology:measurement(tsi_tr_t10, toxic_social_infection, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(tsi_be_t0, toxic_social_infection, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tsi_be_t5, toxic_social_infection, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(tsi_be_t10, toxic_social_infection, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(toxic_social_infection, enforcement_mechanism).
narrative_ontology:affects_constraint(toxic_social_infection, emotional_labor_extraction).
narrative_ontology:affects_constraint(toxic_social_infection, loyalty_obligation_enforcement).
narrative_ontology:affects_constraint(toxic_social_infection, care_responsibility_asymmetry).

% DUAL FORMULATION NOTE:
% The infecting character is downstream of personality pathology and individual dysregulation but represents a distinct structural constraint on social systems. Upstream constraints concern the mechanisms by which personality disorders emerge and persist; this constraint concerns the extraction mechanism that radiates from dysregulated individuals to their proximity networks. The network links reflect that the infecting character phenomenon produces extractive load on care systems, creates asymmetric loyalty obligations, and externalizes the person's emotional labor burden onto those nearby.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(toxic_social_infection, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
