% ============================================================================
% CONSTRAINT STORY: categorical_impermissibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_categorical_impermissibility, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: categorical_impermissibility
 *   human_readable: State Killing as Categorical Impermissibility
 *   domain: criminal_justice/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   State killing under the categorical impermissibility reading denies that
 *   any legitimate authority — neither retributive desert nor deterrence
 *   function nor collective security need — can justify execution. The
 *   constraint operates on two levels: (1) immediate: the condemned person is
 *   subjected to lethal state violence with zero exit option and no
 *   compensation, making execution an irreducible extraction; (2)
 *   generational: normalizing execution as a state prerogative degrades the
 *   human dignity norm and creates spillover effects (police violence
 *   legitimacy, torture permissibility, military doctrine). This is the
 *   reading that treats the right to life as inalienable even for murderers —
 *   a categorical claim that forecloses or coexists with competing readings
 *   of state killing authority. The measurement trajectory shows rising
 *   extractiveness (0.55 to 0.68) as execution technology becomes more
 *   routinized and less shocking, and theater ratio rising modestly (0.48 to
 *   0.55) as ritual increasingly replaces genuine deliberation about state
 *   legitimacy. The abolitionist coalition's scaffold perspective reflects
 *   the structural reality that execution is being systematically displaced
 *   by international law and domestic legislative abolition — the constraint
 *   has an identifiable sunset path even though it remains active in
 *   retentionist jurisdictions.
 *
 * KEY AGENTS:
 *   - Condemned Persons: Primary victim (powerless/trapped) — bear the irreducible extraction of state lethal authority; have no exit option, no ability to appeal the use of state killing power.
 *   - Human Dignity Norm: Secondary victim (powerless/trapped) — abstract collective good that cannot organize or defend itself; degraded by normalization of state killing; experiences spillover extraction to police, interrogation, military contexts.
 *   - Victim Families and Sentencing Community: Mixed agent (moderate/constrained) — experience both coordination (collective justice for murdered victim) and extraction (psychological cost of knowing state killing was done in their name); can exit but at social cost.
 *   - Abolitionist Coalition: Organized agent (organized/constrained) — international human rights bodies, EU law, NGOs, some legislatures building alternative pathways (life-without-parole, abolition treaties). See capital punishment as temporary institutional form being phased out. Have agency and exit path (sunset logic).
 *   - State Criminal Justice Authority: Institutional beneficiary (institutional/arbitrage) — experiences execution as coordination mechanism and legitimate exercise of state power under competing readings (retributive, deterrence). Under categorical_impermissibility reading, no legitimate beneficiary exists.
 *   - Analytical Observer: Universalist perspective (analytical/analytical) — risks naturalizing a contested normative commitment (inalienability of right to life) as a transcultural law, potentially obscuring that this is one reading of a contested kernel.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(categorical_impermissibility, 0.68).
domain_priors:suppression_score(categorical_impermissibility, 0.75).
domain_priors:theater_ratio(categorical_impermissibility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(categorical_impermissibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(categorical_impermissibility, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(categorical_impermissibility, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(categorical_impermissibility, snare).
narrative_ontology:human_readable(categorical_impermissibility, "State Killing as Categorical Impermissibility").
narrative_ontology:topic_domain(categorical_impermissibility, "criminal_justice/constitutional_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(categorical_impermissibility, formalized).
narrative_ontology:cs_authority_grounding(categorical_impermissibility, lineage).
narrative_ontology:cs_interpretation_layer_present(categorical_impermissibility).
narrative_ontology:cs_kernel_id(categorical_impermissibility, state_killing_authority).
narrative_ontology:cs_reading_relation(categorical_impermissibility, retributive_desert, forecloses).
narrative_ontology:cs_reading_relation(categorical_impermissibility, deterrence_instrument, coexists_with).
narrative_ontology:cs_axiom(categorical_impermissibility, foundational, inalienability_categorical).
narrative_ontology:cs_axiom_status(inalienability_categorical, holdable).
narrative_ontology:cs_axiom(categorical_impermissibility, foundational, state_killing_illegitimate).
narrative_ontology:cs_axiom_status(state_killing_illegitimate, holdable).
narrative_ontology:cs_reference_frame(categorical_impermissibility, human_dignity_inviolability).
narrative_ontology:cs_drift_state(categorical_impermissibility, contemporary_abolitionist_era, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_victim(categorical_impermissibility, condemned_persons).
narrative_ontology:constraint_victim(categorical_impermissibility, human_dignity_norm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED PERSON (SNARE) — Absolute structural extraction with zero exit options. State exercises lethal monopoly over a human unable to resist, appeal, or escape. The condemned bears maximum cost of the state's assertion of killing authority. Under this reading, execution is categorical violence regardless of crime committed — the right to life is inalienable even when forfeited by the condemned's own actions.
constraint_indexing:constraint_classification(categorical_impermissibility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HUMAN DIGNITY NORM (SNARE) — Abstract collective good that cannot organize or exit. Execution normalizes state killing and degrades the norm that human life has categorical worth. Jurisdictions that execute embed killing as a legitimate state function, creating spillover effects (police violence legitimacy, torture in interrogation, military doctrine). The norm has no advocate and no structural protection — trapped by the execution system's demonstration that killing can be state action.
constraint_indexing:constraint_classification(categorical_impermissibility, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: VICTIM FAMILY AND SENTENCING COMMUNITY (TANGLED ROPE) — Mixed experience: execution provides symbolic justice (coordination function — society collectively affirms the murdered victim's wrongfulness) alongside extraction of psychological cost (the knowledge that state killing was done in their name). Exit is constrained: refusing execution is possible but socially costly. Both benefits and costs accrue to this group.
constraint_indexing:constraint_classification(categorical_impermissibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ABOLITIONIST COALITION (SCAFFOLD) — Organized agents (human rights bodies, international law, NGOs, some legislatures) see capital punishment as a temporary institutional form being systematically replaced by life-without-parole sentences. The scaffold perspective recognizes execution as a degrading practice that violates human dignity and is being phased out through international pressure (EU conditionality, UN protocols, treaty obligations). Sunset logic applies: as abolition spreads, state killing loses legitimacy and becomes unthinkable. Exit path is structural — the coalition has agency and sees the end-state.
constraint_indexing:constraint_classification(categorical_impermissibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: STATE CRIMINAL JUSTICE AUTHORITY (ROPE) — Experiences capital punishment as a coordination mechanism: the state and society solve the legitimate problem of responding to heinous crime through the ritual of execution. This perspective sees no extraction — only the coordination of collective condemnation. The state's exit option is arbitrage: it can perform justice through execution or through life imprisonment, and it chooses execution when it serves legitimacy needs. Under the retributive reading (not this one), this is experienced as low-extraction coordination.
constraint_indexing:constraint_classification(categorical_impermissibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalist/civilizational perspective, human dignity and the right to life might be seen as immutable constraints on state authority — categorical principles that no government can legitimately violate regardless of circumstances. The mountain reading holds that inalienability of the right to life is a transcultural, transhistorical truth about human personhood. However, this perspective risks naturalizing what is actually a contested normative commitment. The engine's false summit detection will flag this as a reading of a kernel (state_killing_authority), not a discovered law.
constraint_indexing:constraint_classification(categorical_impermissibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(categorical_impermissibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(categorical_impermissibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(categorical_impermissibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(categorical_impermissibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(categorical_impermissibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The state extracts from the condemned person the irreducible cost of their life under a power structure with zero exit options. The condemned has no compensation, no remedy, no structural protection. This is not the highest possible extractiveness (0.72+) because execution is theoretically constrained by legal procedure and is not continuous extraction over time (it is singular), but the extraction is severe and irreversible. The measurement trajectory rising from 0.55 to 0.68 reflects accumulation: as execution becomes more routine and less morally questioned, the extractiveness increases because the scaffolding of justification erodes. Suppression (0.75): High. Multiple barriers prevent exit or resistance: the condemned has already been through the justice system, appeals are exhausted, the state monopolizes lethal force, public discourse about abolition is suppressed in retentionist jurisdictions through appeals to retributive desert and victim justice. International pressure for abolition faces nationalist resistance. Theater ratio (0.55): Moderate. Execution ritual (trial, appeals, formal execution) has both genuinely deliberative elements (the justice system's attempt to distinguish murderers from innocent people, which has real epistemic function) and performative elements (the certainty of execution once sentenced masks the deep incoherence in the state's claimed authority to kill, ritual solemnity replaces argument about legitimacy). The ratio is lower than pure theater (0.70+) because the legal determination of guilt has genuine epistemic stakes; it is higher than low theater because once guilt is determined, execution follows mechanically — the execution itself adds no epistemic content, only dramatization. Claimed type (snare) is justified by the high extractiveness and suppression combined with the absence of coordination benefit to the condemned or the human dignity norm.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates deep perspectival divergence that reveals the kernel structure. The condemned person experiences pure extraction (snare) — they are subjected to state killing with zero exit option. The human dignity norm experiences snare — it is degraded by normalization of state authority to kill. Victim families experience mixed coordination and extraction (tangled rope) — execution provides symbolic justice but at the cost of society normalizing killing. The abolitionist coalition experiences a sunset constraint (scaffold) — capital punishment is a temporary institutional form being phased out by international law and domestic abolition movements. The state criminal justice authority experiences coordination (rope) — execution solves the legitimate problem of collective response to heinous crime. The analytical observer at civilizational scale risks seeing natural law (mountain) — the inalienability of the right to life as a transcultural principle — but this is a false summit: the claim naturalizes what is actually a contested reading of state authority limits. The perspectival gap reveals that the constraint is not a discovered law but a reading of the kernel state_killing_authority that coexists with competing readings (retributive_desert, deterrence_instrument).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is determined by structural position: the condemned person (powerless/trapped) experiences maximum extraction (d ≈ 0.95); the human dignity norm (powerless/trapped) also experiences maximum extraction (d ≈ 0.95); victim families (moderate/constrained) experience mixed (d ≈ 0.50-0.55); the abolitionist coalition (organized/constrained) experiences moderate extraction despite their opposition because they are constrained by entrenched state practice (d ≈ 0.45-0.50); the state authority (institutional/arbitrage) experiences this reading as a constraint on its claimed power, but under the retributive or deterrence readings, it experiences execution as legitimate coordination (d ≈ 0.15-0.20 under categorical_impermissibility, d ≈ 0.05 under retributive). The analytical observer (analytical/analytical) has canonical d ≈ 0.72-0.73, producing a moderate-high χ because the analytical position is observing the structure without the power to enforce either reading. Under categorical_impermissibility, the state has no legitimate beneficiary and the primary victims (condemned person, human dignity norm) have no exit — this is the structural signature of a snare from the target and norm perspectives, tangled rope from the victim family perspective, scaffold from the abolitionist organized perspective, and rope from the state authority perspective under competing readings.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inalienability_grounding,
    'On what ground is the right to life claimed to be inalienable — transcultural moral law, Kantian dignity, international legal convention, or contingent policy commitment?',
    'Comparative jurisprudence: track whether inalienability claims survive cross-cultural and cross-historical scrutiny, or whether they are parochial to rights-based liberal democracies. Identify the authority structure that validates the inalienability claim.',
    'If transcultural/ahistorical: categorical_impermissibility is a mountain (natural law). If contingent to liberal legal tradition: it is a snare (institutional extraction via normalization) or a contested kernel reading (coexists with retributive and deterrence readings). Affects classification of the analytical perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inalienability_grounding, conceptual, 'Grounding of inalienability claim (transcultural vs. contingent)').

omega_variable(
    forfeiture_paradox,
    'Can a right be simultaneously inalienable and unviolated by execution if the condemned violates others'' rights through murder? Does the murderer''s action change the moral ontology of their own right to life?',
    'Logical analysis of property persistence under violation: if the right to life is inalienable, can it be suspended/overridden without being alienated? Examine whether this distinction is coherent or conflates separate concepts (inalienability vs. immunity vs. override conditions).',
    'If distinction is coherent: categorical_impermissibility stands as consistent principle. If conflated: the reading faces internal incoherence and collapses into the retributive reading (desert-based forfeiture). Critical for determining whether the reading is holdable or self-defeating.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forfeiture_paradox, conceptual, 'Logical coherence of inalienability claim under conditions of severe crime').

omega_variable(
    state_authority_legitimacy,
    'What source of legitimate authority does the state possess to adjudicate and enforce the right to life? If authority derives from social contract or consent, does consent to the state include consent to be killed by it?',
    'Social contract theory analysis: examine whether foundational texts (Hobbes, Locke, Rousseau, modern constitutions) explicitly or implicitly reserve capital authority to the state. Track historical progression of consent-based limitations on state power.',
    'If no legitimate authority exists for state killing: categorical_impermissibility is justified by constraint on state sovereignty. If authority exists but is contingent: the reading coexists with retributive and deterrence readings (different parties consent to different theories of state power).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_legitimacy, conceptual, 'Source and scope of legitimate state authority to execute').

omega_variable(
    measurement_temporal_asymmetry,
    'Does the measured extractiveness (0.68) reflect the constraint''s structure at the moment of execution, or its structural effect on the human dignity norm and abolitionist movement over generational time? Do these measurement contexts yield different ε values?',
    'Decompose the constraint into two observables: (A) immediate execution as state violence (extraction from condemned person), (B) execution''s role in normalizing state killing authority (extraction from human dignity norm, measured over generational time). Compute ε for each separately.',
    'If ε values differ significantly: categorical_impermissibility decomposes into a constraint family (immediate snare + generational dignity erosion snare). If stable: single unified constraint. Affects network structure and decomposition guidance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_temporal_asymmetry, empirical, 'Whether extractiveness is time-invariant or decomposes across temporal scales').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(categorical_impermissibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catimperm_tr_t0, categorical_impermissibility, theater_ratio, 0, 0.48).
narrative_ontology:measurement(catimperm_tr_t5, categorical_impermissibility, theater_ratio, 5, 0.52).
narrative_ontology:measurement(catimperm_tr_t10, categorical_impermissibility, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(catimperm_be_t0, categorical_impermissibility, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(catimperm_be_t5, categorical_impermissibility, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(catimperm_be_t10, categorical_impermissibility, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(categorical_impermissibility, enforcement_mechanism).
narrative_ontology:affects_constraint(categorical_impermissibility, retributive_desert).
narrative_ontology:affects_constraint(categorical_impermissibility, deterrence_instrument).
narrative_ontology:affects_constraint(categorical_impermissibility, police_lethal_authority).
narrative_ontology:affects_constraint(categorical_impermissibility, torture_interrogation_legitimacy).

% DUAL FORMULATION NOTE:
% categorical_impermissibility is one reading of state_killing_authority kernel. Sibling readings retributive_desert and deterrence_instrument will be authored as separate constraints with their own ε values, perspectives, and beneficiary/victim structures. The three constraints form a kernel family linked through network.affects_constraints showing how different readings of the same authority structure produce different classification profiles. Upstream effects: if categorical_impermissibility is adopted, it constrains the legitimacy of police lethal authority and torture in interrogation (both of which depend on state killing being permissible). Downstream effects: execution's normalization of state killing spills over to other lethal authority contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
