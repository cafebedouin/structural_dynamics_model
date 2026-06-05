% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__retributive_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: State Killing Legitimacy (Retributive Desert Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the retributive reading of the
 *   state_killing_legitimacy kernel — the claim that a murderer forfeits
 *   their right to life through proportional desert (lex talionis). This is
 *   one of three structurally distinct readings of how state killing can be
 *   legitimated. The retributive reading differs fundamentally from the
 *   deterrence reading (which justifies execution as a rational signal to
 *   prevent future murders) and the abolition reading (which claims state
 *   killing categorically violates inviolable human dignity regardless of
 *   desert or utility). The retributive reading's core normative claim is
 *   that proportional response to grave wrongdoing is a requirement of
 *   justice itself — that the state's authority to execute murderers derives
 *   not from utility but from the perpetrator's moral forfeiture of their
 *   life-right through the commission of murder. This constraint exhibits
 *   high extractiveness (0.68) because the reading concentrates killing
 *   authority in the state by providing a legitimacy frame that removes
 *   alternatives (mercy, exile, life imprisonment) from the moral menu. High
 *   suppression (0.72) reflects that the desert framework suppresses the
 *   competing principle of inviolable human dignity by carving out an
 *   exception: certain humans can be treated as rightless beings. The theater
 *   ratio (0.55) indicates moderate performativity — retributive
 *   justifications are still articulated in judicial opinions and clemency
 *   debates, but they increasingly share authority with deterrence,
 *   incapacitation, and human-rights considerations rather than standing as
 *   the sole legitimacy ground.
 *
 * KEY AGENTS:
 *   - Condemned Offender: Primary victim (powerless/trapped) — no exit from the desert-based legitimacy claim; forfeiture of life-right is presented as morally deserved
 *   - Victim's Family: Secondary agent (moderate/constrained) — benefits from symbolic affirmation of victim's worth through state enforcement of desert, but constrained by state's monopoly on execution and timing
 *   - State Retributive Authority: Primary beneficiary (institutional/arbitrage) — gains legitimacy to kill by framing execution as justice rather than power; controls interpretation and enforcement
 *   - Retributive Legal Tradition: Institutional custodian (institutional/arbitrage) — maintains apparatus of desert-based sentencing; experiences own doctrine as degraded but persists through inertia
 *   - Human Dignity Principle: Structural victim (powerless/trapped) — the reading provides a mechanism to suspend inviolable dignity; principle has no exit from the framework that denies its universality
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent commitment to desert as an immutable moral law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.68).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "State Killing Legitimacy (Retributive Desert Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, 'd882c8d8-7bad-4e5e-b2de-404c83342aa7').
narrative_ontology:cs_kernel_codification('d882c8d8-7bad-4e5e-b2de-404c83342aa7', formalized).
narrative_ontology:cs_authority_grounding('d882c8d8-7bad-4e5e-b2de-404c83342aa7', lineage).
narrative_ontology:cs_interpretation_layer_present('d882c8d8-7bad-4e5e-b2de-404c83342aa7').
narrative_ontology:cs_reading_relation('d882c8d8-7bad-4e5e-b2de-404c83342aa7', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d882c8d8-7bad-4e5e-b2de-404c83342aa7', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('d882c8d8-7bad-4e5e-b2de-404c83342aa7', foundational, murder_creates_moral_forfeiture).
narrative_ontology:cs_axiom_status(murder_creates_moral_forfeiture, holdable).
narrative_ontology:cs_axiom_grounding('d882c8d8-7bad-4e5e-b2de-404c83342aa7', murder_creates_moral_forfeiture, deontological).
narrative_ontology:cs_axiom('d882c8d8-7bad-4e5e-b2de-404c83342aa7', foundational, proportional_response_constitutes_justice).
narrative_ontology:cs_axiom_status(proportional_response_constitutes_justice, holdable).
narrative_ontology:cs_axiom_grounding('d882c8d8-7bad-4e5e-b2de-404c83342aa7', proportional_response_constitutes_justice, deontological).
narrative_ontology:cs_reference_frame('d882c8d8-7bad-4e5e-b2de-404c83342aa7', classical_retributive_authority).
narrative_ontology:cs_drift_state('d882c8d8-7bad-4e5e-b2de-404c83342aa7', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d882c8d8-7bad-4e5e-b2de-404c83342aa7', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, state_retributive_authority).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, condemned_offender).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, human_dignity_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED OFFENDER (SNARE) — No exit. The desert-based legitimacy claim denies the offender any appeal to innocence or humanity; the claim is that they have forfeited the right to life itself through their action. Maximum suppression: the reading removes alternatives (mercy, exile, life imprisonment) by declaring them morally inadequate. High extraction: the state takes the offender's life as the price of restored moral order. The offender experiences this as pure extraction — the framework that justifies it offers no negotiation or escape.
constraint_indexing:constraint_classification(state_killing_legitimacy__retributive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VICTIM'S FAMILY (TANGLED ROPE) — Both benefit and bear cost. They benefit from the state's enforcement of proportional desert: the state's willingness to kill the murderer affirms the value of their slain loved one and provides symbolic restoration. But they are also constrained by the state's monopoly on execution — they cannot choose mercy, cannot control the timing or manner of death, and may find that state killing re-traumatizes them or does not provide the promised closure. Mixed experience: genuine coordination function (affirming victim's worth) coupled with asymmetric extraction (state controls the mechanism and narrative).
constraint_indexing:constraint_classification(state_killing_legitimacy__retributive_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE RETRIBUTIVE AUTHORITY (ROPE) — Benefits from the desert framework. The state's capacity to execute murderers is legitimated as the enforcement of a moral principle (proportional desert), not as arbitrary killing. The state experiences the constraint as coordination: preserving the moral order by making punishment proportional to crime. Net beneficiary — the desert-based legitimacy claim transfers the state's killing from a question of utility or policy into a question of justice. Low experienced extraction because the state has designed the framework and controls interpretation.
constraint_indexing:constraint_classification(state_killing_legitimacy__retributive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RETRIBUTIVE LEGAL TRADITION (PITON) — The apparatus of desert-based sentencing persists institutionally but with degraded theoretical standing. Modern criminal law systems still invoke proportionality in sentencing guidelines, but the core legitimacy claim — that murderers categorically forfeit life-rights — has atrophied in most Western jurisdictions. The machinery remains (death penalty statutes, appellate review, execution protocols) but the performative content has risen: the ritual of execution is maintained not because the desert argument persuades contemporary jurists but because the system inherited it and changing course requires explicit political action. Theater ratio (0.55) reflects that retributive justifications are still articulated in judicial opinions and clemency debates, but they do not determine outcomes — deterrence, incapacitation, and human-rights considerations compete for authority. The tradition sees itself as degraded: it persists through legal inertia rather than conviction.
constraint_indexing:constraint_classification(state_killing_legitimacy__retributive_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMAN DIGNITY PRINCIPLE (SNARE) — The reading constitutes a structural attack on human dignity as an inviolable right. Desert-based legitimacy claims that certain humans can forfeit fundamental rights through their actions; the reading provides a mechanism (proportional desert) for permanent suspension of dignity. This perspective views the constraint as pure extraction from the principle itself: the state extracts the ability to treat any human as a rightless being by appealing to desert. No coordination benefit; no exit; maximum suppression of the competing principle (inviolable dignity). The dignity principle is trapped in a framework that denies its applicability to the worst offenders.
constraint_indexing:constraint_classification(state_killing_legitimacy__retributive_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the retributive reading may appear as an immutable principle: that proportional response to grave wrongdoing is inherent to justice itself, not a contingent institutional choice. This perspective sees desert as a foundational moral fact about human agency and accountability — wrongdoing creates a moral debt that can only be discharged through proportional suffering. However, the structural data contradicts the mountain classification. The reading benefits identifiable parties (state authority, moral-order narrative), suppresses alternatives through deliberate institutional design, and requires active enforcement. The 'immutable principle of desert' framing naturalizes what is actually a contingent normative commitment. The engine's false-summit detection will flag this.
constraint_indexing:constraint_classification(state_killing_legitimacy__retributive_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__retributive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_killing_legitimacy__retributive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_killing_legitimacy__retributive_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, TR),
    TR >= 0.70.

:- end_tests(state_killing_legitimacy__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The retributive reading legitimates state killing by providing a moral frame — desert — that makes execution appear as justice rather than killing. The state extracts the authority to take life by appealing to a principle (proportionality) that is presented as objective and necessary rather than as a political choice. The measurement trajectory (0.62 → 0.68 over the interval) reflects that extractiveness has risen as retributive doctrines have become more elaborate and institutionalized, creating more sophisticated frameworks for justifying execution. Suppression (0.72): High. The reading suppresses alternatives by claiming they are morally inadequate: mercy to a murderer is unjust (disrespects desert), exile is inadequate (fails proportionality), life imprisonment is incomplete (offender retains life-right they have forfeited). These alternatives are removed from the moral menu, not through open debate but through the logical structure of the desert claim. Theater ratio (0.55): Moderate. Retributive arguments are sincerely articulated in judicial opinions, clemency petitions, and philosophy, but they increasingly share the stage with deterrence arguments and human-rights concerns. The increasing theater ratio (0.40 → 0.55 over the interval) reflects that retributive language persists in judicial rhetoric while playing a diminishing role in actual decision-making — outcomes are increasingly driven by deterrence calculations, political sentiment, and human-rights commitments, making the retributive justification increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   The retributive reading produces a sharp perspectival gap between the state/tradition (rope/piton) and the condemned offender (snare). The state and legal tradition see the constraint as coordinating justice — enforcing a principle of proportionality that is experienced as legitimating rather than extractive. The condemned offender sees pure extraction — the forfeiture of their life-right with no negotiation or escape. The victim's family occupies the tangled-rope middle: they benefit from the state's affirmation of their loved one's worth but are constrained by the state's control over the mechanism. The human dignity principle sees the constraint as pure extraction (snare) — the reading provides a method to suspend inviolable dignity. The analytical observer risks seeing desert as a natural law (mountain) rather than recognizing it as a contingent normative commitment that benefits particular parties (state authority, retributive tradition) while imposing costs on others (condemned offenders, human dignity principle).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural relationship to the constraint: whether they benefit or bear cost, and whether they have exit options. The condemned offender is a victim with no exit (trapped) — high d, high experienced extraction. The state is an institutional beneficiary with arbitrage options — low d, low/negative experienced extraction (the state designed the framework). The victim's family is a moderate-power agent with constrained options — mid-range d, mixed experience. The dignity principle is treated as a victim of the framework (the reading provides a method to suspend it) with no exit — high d. The tradition sees itself as custodian of the principle, with institutional arbitrage — low d, but with piton-level degradation (low function, high theater). The directionality overrides are not needed — the structural derivation captures the actual relationships.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    desert_foundation_empirical_vs_normative,
    'Is the claim that murderers ''forfeit life-rights through desert'' grounded in empirical facts about causation and responsibility, or is it a normative commitment about how society should respond to grave wrongs?',
    'Philosophical analysis of the grounding type: Does the retributive reading make claims about what IS (empirically) the case about human agency, or only claims about what OUGHT to be done? Can the desert claim be falsified by evidence about moral psychology, brain states, or the nature of agency?',
    'If grounded in empirical claims about agency and causation: the reading is vulnerable to empirical challenge (neuroscience, psychology, philosophy of action) and could be foreclosed by sufficient evidence of determinism or agency impossibility. If grounded in pure normative commitment: empirical findings do not touch the reading; it coexists with deterrence and abolition readings as a live normative position. This determines the reading''s epistemic status and vulnerability to drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_foundation_empirical_vs_normative, conceptual, 'Whether desert claim is empirically contingent or normatively foundational').

omega_variable(
    proportionality_measure_specification,
    'What makes a punishment ''proportional'' to murder under the retributive reading? Is there a stable measure of proportionality that justifies execution, or does the proportionality standard shift with political and social context?',
    'Historical analysis of retributive sentencing standards: Has the ''proportional'' response to murder been stable across jurisdictions and time periods, or has it varied widely (life imprisonment, long sentences, execution, exile, compensation)? Does variation correlate with principled philosophical refinement or with political/cultural drift?',
    'If stable measure exists: the retributive reading has determinate content and can be applied consistently. If proportionality is context-dependent: the reading provides a legitimacy frame that can justify varied outcomes, suggesting it is functioning as a cover story for outcomes determined by other factors (political will, resource availability, cultural sentiment). This affects whether the reading is a principle or a theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_measure_specification, empirical, 'Whether proportionality standard is stable or context-dependent').

omega_variable(
    sibling_reading_logical_status,
    'Do the deterrence and abolition readings logically foreclose the retributive reading, or can all three readings remain live normative positions in different jurisdictions and traditions?',
    'Logical analysis: Does accepting the deterrence reading (execution justified as rational signal) require rejecting the claim that desert creates a moral right to life-taking? Does accepting abolition (state killing violates dignity) require rejecting the claim that murderers forfeit rights? Or can parties coherently hold different readings based on different foundational normative commitments (different views of human dignity, moral agency, state authority)?',
    'If readings foreclose each other: one reading is correct and the others are errors; the constraint is a unidimensional dispute about a single principle. If readings coexist: each represents a distinct normative framework; the dispute is about which framework to adopt, not which is true. This determines whether the kernel represents a resolvable disagreement or an irreducible normative pluralism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_logical_status, conceptual, 'Logical relationship between retributive, deterrence, and abolition readings').

omega_variable(
    desert_moral_status_universality,
    'Does the retributive reading apply universally (all murderers forfeit life-rights, regardless of circumstance), or only to paradigmatic cases? What boundary conditions — insanity, extreme duress, self-defense, mistake of law — suspend the desert claim?',
    'Doctrinal analysis of retributive jurisprudence: Which factors (if any) prevent a murderer from forfeiting life-rights? Do these exceptions represent genuine limitations of the desert principle, or do they reveal that desert is not the operative principle (that other factors like deterrence, incapacitation, or human-rights constraints actually determine outcomes)?',
    'If desert applies universally with few exceptions: the reading has strong determinate content. If many exceptions exist: the reading may be operating as a post-hoc legitimacy frame for outcomes determined by other principles. Proliferation of exceptions suggests the desert principle is degrading in real institutional practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_moral_status_universality, empirical, 'Scope and boundary conditions of the desert principle').

omega_variable(
    kernel_reading_contest_resolution_path,
    'This constraint is one reading of a contested kernel (state_killing_legitimacy). The retributive reading coexists with deterrence and abolition readings. Over what timespan, and through what mechanism, might one reading displace the others?',
    'Historical trajectory analysis: In jurisdictions that have moved from execution to abolition, what role did retributive arguments play? Did retributive scholars actively defend execution, or did the desert principle migrate into other contexts (life sentences, restitution)? Does the principle retain force in contemporary jurisprudence, or is it now invoked primarily for symbolic/traditional reasons (piton dynamic)?',
    'If retributive arguments are actively defending execution in major jurisdictions: the reading is holding its ground. If retributive scholars have largely ceded the field to deterrence and human-rights arguments: the reading is in drift toward atrophy. This affects whether the reading should be classified as a live normative principle or a degraded (piton-like) institutional artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution_path, empirical, 'How the contest between readings evolves over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__retributive_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement(stat_tr_t100, state_killing_legitimacy__retributive_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__retributive_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(stat_be_t100, state_killing_legitimacy__retributive_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__retributive_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(stat_su_t100, state_killing_legitimacy__retributive_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, proportional_punishment_principle).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel state_killing_legitimacy. The other readings (deterrence and abolition) are separate constraint stories with their own ε values, perspectives, and structural data. All three readings share the same kernel but instantiate different legitimacy frames and produce different victim/beneficiary structures. The constraint family is linked bidirectionally: each reading's network.affects_constraints includes the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
