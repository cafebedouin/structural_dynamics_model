% ============================================================================
% CONSTRAINT STORY: deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deterrence_reading, []).

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
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deterrence_reading
 *   human_readable: Capital Punishment as Deterrence: The Utilitarian Framing
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   Capital punishment in the United States can be justified through multiple
 *   readings of the state's authority to execute. This story instantiates the
 *   DETERRENCE READING: the state executes capital offenders to raise the
 *   cost of capital crimes and thereby prevent future murders. Future
 *   potential victims are the primary beneficiaries; executed offenders and
 *   wrongfully convicted persons are the victims. The constraint exhibits
 *   Tangled Rope structure because it combines a genuine coordination
 *   function (establishing credible consequences that may reduce capital
 *   crime) with asymmetric extraction (death is irreversible and concentrated
 *   on the executed person). The empirical efficacy of deterrence is
 *   contested, introducing omega variables that determine whether the
 *   constraint's extractiveness is justified (ε → 0.30, Rope) or excessive (ε
 *   → 0.70+, Snare). The theater ratio has increased over 60 years as
 *   appellate scrutiny and exoneration evidence have revealed systemic errors
 *   while execution ritual maintains public legitimacy. The constraint is one
 *   reading of a contested kernel (state_execution_authority) alongside
 *   retributive and abolition readings, which would decompose into separate
 *   constraint stories with different beneficiary/victim sets and ε values.
 *
 * KEY AGENTS:
 *   - Future Potential Murder Victims: Primary beneficiaries (powerful/constrained) — benefit from reduced homicide risk if deterrence is efficacious; constrained by information gaps about actual deterrence
 *   - Executed Offenders: Primary victims (powerless/trapped) — bears irreversible cost of execution; structurally unable to exit or negotiate consequences
 *   - Wrongfully Convicted Persons: Secondary victims (moderate/identity_locked) — trapped by conviction error and identity fusion with criminal label; may be exonerated but face permanent social death
 *   - State as Executor: Institutional beneficiary (institutional/arbitrage) — experiences constraint as coordination mechanism; has arbitrage to substitute alternative punishments if equally deterring
 *   - Capital Crime Prosecutors: Institutional participants (powerful/constrained) — benefit from career advancement and institutional budget expansion; constrained by appellate reversals and exoneration evidence
 *   - International Legal Community: Institutional observer (institutional/arbitrage) — views capital punishment as degraded institution maintained through domestic political inertia rather than functional necessity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent deterrence hypothesis as immutable law of rational criminal justice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deterrence_reading, 0.52).
domain_priors:suppression_score(deterrence_reading, 0.65).
domain_priors:theater_ratio(deterrence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deterrence_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(deterrence_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(deterrence_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deterrence_reading, tangled_rope).
narrative_ontology:human_readable(deterrence_reading, "Capital Punishment as Deterrence: The Utilitarian Framing").
narrative_ontology:topic_domain(deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(deterrence_reading, fixed_text).
narrative_ontology:cs_authority_grounding(deterrence_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(deterrence_reading).
narrative_ontology:cs_kernel_id(deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deterrence_reading, future_potential_victims).
narrative_ontology:constraint_beneficiary(deterrence_reading, deterrence_benefiting_communities).
narrative_ontology:constraint_victim(deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(deterrence_reading, imprisoned_innocent_persons_wrongful_conviction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXECUTED OFFENDER (SNARE) — Maximum extraction with no exit. The offender's life is instrumentalized as the cost mechanism. Their death is non-negotiable and cannot be commuted absent political will to abolish capital punishment. Trapped by legal sentence, unable to exit system.
constraint_indexing:constraint_classification(deterrence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WRONGFULLY CONVICTED PERSON (SNARE) — Trapped by both institutional error and identity fusion with the criminal label. Even if exonerated pre-execution, the person has internalized the identity imposed by the system and faces permanent social death. The constraint extracts maximum cost through both incarceration and identity damage. Identity lock makes exit perception impossible even when structural escape routes exist.
constraint_indexing:constraint_classification(deterrence_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE AS EXECUTOR (ROPE) — The state experiences the deterrence mechanism as a coordination solution: raising the cost of capital crimes maintains social order and establishes credible commitment to law enforcement. From this institutional perspective, the constraint solves a collective action problem (preventing murders requires credible consequences). The state has arbitrage: it can substitute execution with life imprisonment if equally deterring, shift punishment modality, or adjust the constraint's enforcement intensity.
constraint_indexing:constraint_classification(deterrence_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FUTURE POTENTIAL MURDER VICTIMS (TANGLED ROPE) — Primary beneficiaries of the deterrence mechanism. They benefit from reduced homicide risk IF deterrence efficacy is real. However, they also bear costs: the constraint requires maintaining a credible execution apparatus, which creates moral hazard (wrongful convictions), fiscal extraction (criminal justice infrastructure), and social cost (retaliation, violence escalation). Powerful in aggregate (voting, political mobilization) but constrained by information gaps about actual deterrence efficacy and wrongful conviction rates.
constraint_indexing:constraint_classification(deterrence_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CAPITAL CRIME PROSECUTORS (TANGLED ROPE) — Experience the constraint as both coordination (establishing credible consequences for capital crimes) and extraction (career advancement, political leverage tied to capital convictions; budget expansion justified by capital cases). Constrained by appellate reversals and exoneration evidence; moderate power through discretion in prosecution decisions. The constraint benefits their institutions while imposing the cost of maintaining trial infrastructure and appellate scrutiny.
constraint_indexing:constraint_classification(deterrence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL SYSTEM (PITON) — From a global perspective, capital punishment is largely performative theater maintained through institutional inertia. Most developed democracies have abolished it; the constraint persists in the US through constitutional interpretation (8th Amendment reading) and state-level variation. The international consensus treats execution as a degraded institution — maintained because domestic political coalitions defend it, not because it solves coordination problems that cannot be solved by alternatives (life imprisonment, incapacitation).
constraint_indexing:constraint_classification(deterrence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a purely utilitarian logical frame, if capital punishment deters homicide at rates significantly better than alternatives, then it is an immutable requirement of rational criminal justice: you cannot avoid executing people if it prevents more murders. This perspective sees the constraint as a derivative from the axiom 'minimize harm.' However, this naturalization obscures the empirical contestation: deterrence efficacy is uncertain, wrongful executions impose irreversible costs, and alternative mechanisms (life imprisonment, incapacitation, risk assessment) may achieve equivalent deterrence without capital punishment. The analytical observer risks false-summit reasoning.
constraint_indexing:constraint_classification(deterrence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deterrence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deterrence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deterrence_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deterrence_reading, TR),
    TR >= 0.70.

:- end_tests(deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint's extractiveness depends entirely on whether deterrence efficacy is real and substantial. If capital punishment deters capital crimes better than life imprisonment, the extractiveness is justified by the beneficiary interest (future victims). However, the empirical evidence is contested — meta-analyses produce conflicting results, and substitution equivalence (LWOP vs capital) is uncertain. The measurement trajectory shows extractiveness increasing from 0.35 to 0.52 over 60 years, reflecting growing awareness of wrongful convictions and systemic error rates, which increases the extraction cost without clear evidence of increased deterrence benefit. Suppression (0.65): Moderate-high. Multiple barriers prevent exit or challenge: legal finality doctrines prevent appeal after conviction becomes final; institutional momentum maintains execution protocols despite exoneration evidence; political cost of opposing capital punishment constrains policy change. Wrongfully convicted persons face both structural suppression (unable to prove innocence post-execution, irreversibility of death) and internalized suppression (identity fusion with criminal label). Theater ratio (0.58): Moderate. The deterrence mechanism requires public visibility (visible executions, legal process, state commitment) to function, but much of the apparatus is performative: lengthy appeals serve finality rather than justice; victim impact statements in capital trials have unclear sentencing effect; execution ritual dramatizes state power more than it demonstrates rational punishment allocation.
 *
 * PERSPECTIVAL GAP:
 *   The deterrence reading produces maximum perspectival divergence across power atoms and exit options. The state executor sees Rope (coordination) because it has arbitrage and institutional capacity. Future victims see Tangled Rope (mixed benefit and cost uncertainty) because they benefit IF deterrence works but also bear costs of wrongful conviction and moral hazard. The executed offender sees Snare (pure extraction) because death is non-negotiable and irreversible. The wrongfully convicted person sees Snare with identity lock because they are structurally mobile (exoneration is possible) but identity-fused with the criminal label, making exit impossible even after structural liberation. The international observer sees Piton because execution ritual persists through domestic political inertia despite global consensus that life imprisonment achieves equivalent outcomes. The analytical observer risks Mountain classification by treating the deterrence hypothesis as a logical necessity, ignoring the empirical contestation of deterrence efficacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by whether the agent benefits from or bears cost from the deterrence mechanism. Future potential murder victims (beneficiaries with constrained exit) derive d from their interest in reduced homicide risk — if deterrence works, d is low (~0.40, experiencing negative extraction, i.e., protection). Executed offenders (victims with trapped exit) have high d (~0.95) because death is the maximum extraction possible. The state as executor (institutional/arbitrage) has low d (~0.10) because it benefits from the coordination mechanism and can substitute alternatives. Wrongfully convicted persons (victims with identity_locked exit) have moderately high d (~0.85) because they bear irreversible cost and cannot escape identity fusion with the criminal label even if exonerated. The canonical directionality derivation applies f(d) sigmoid: beneficiaries with arbitrage (d≈0.10) experience f(d)≈-0.12; full victims with trapped exit (d≈0.95) experience f(d)≈1.42. The interaction of these directionalities with ε=0.52 and scope modifiers produces the perspectival classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading resolves mandatrophy by making the classification empirically contingent. If deterrence efficacy is confirmed (omegas #1 and #3), the constraint reclassifies from Tangled Rope toward Rope or Scaffold — genuine coordination mechanism with lower extractiveness and justified cost asymmetry. If deterrence is shown to be negligible or substitutable (omegas #1 and #3), the constraint reclassifies from Tangled Rope toward Snare — pure extraction without corresponding benefit to the ostensible future beneficiaries. The sibling retributive and abolition readings would produce different mandatrophy resolutions: retributive would fix the constraint regardless of deterrence efficacy (proportional desert), abolition would deny it entirely (dignity violation). The deterrence reading is utility-maximizing, so it stands or falls with empirical evidence about what prevents murders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical_dispute,
    'Does capital punishment actually deter capital crimes at rates significantly higher than life imprisonment or other severe sentences?',
    'Meta-analysis of longitudinal comparative criminology studies; quasi-experimental designs comparing deterrence across states with/without capital punishment controlling for confounds; synthetic cohort analysis of offender decision-making under certainty vs severity of punishment',
    'If deterrence efficacy is real and substantial (ε → 0.30): constraint reclassifies toward Rope or Scaffold (coordination without unjustified extraction). If efficacy is negligible or comparable to life imprisonment (ε → 0.70+): constraint reclassifies toward Snare (pure extraction masquerading as utilitarian necessity). If efficacy is moderate and uncertain: remains Tangled Rope but with high epistemic uncertainty omega.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical_dispute, empirical, 'Whether capital punishment deters murders better than alternatives').

omega_variable(
    wrongful_conviction_rate_uncertainty,
    'What is the true rate of wrongful capital convictions, and is it predictable/preventable through system improvements?',
    'DNA exoneration data extrapolation; audit studies of capital case review procedures; longitudinal tracking of post-conviction review effectiveness',
    'If wrongful conviction rate approaches zero: deterrence benefit may justify execution risk. If rate is 2-5% or higher: the utilitarian calculation inverts — executions cause more total harm than prevented through deterrence. Directly modifies victims set composition and sacrificial cost assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_rate_uncertainty, empirical, 'Wrongful capital conviction rate and preventability').

omega_variable(
    substitution_equivalence_question,
    'Is life-without-parole equally deterring as capital punishment for potential capital offenders, or does the severity differential produce distinct behavioral effects?',
    'Comparative analysis of deterrence across jurisdictions varying only sentence severity (capital vs LWOP); offender interviews on decision-making; rational-choice modeling under different penalty regimes',
    'If LWOP is equally deterring: execution is substitutable, reducing extractiveness to ~0.20-0.35 (Rope/Scaffold). If capital punishment produces distinct deterrence from irreversibility: constraint maintains higher extractiveness. If LWOP is MORE deterring (incarceration cost vs death preference): constraint is pure extraction (Snare, ε → 0.75+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_equivalence_question, empirical, 'Whether LWOP deterrence is equivalent to capital punishment').

omega_variable(
    kernel_reading_contestation,
    'Is this constraint best understood through the deterrence reading (utilitarian cost-raising), the retributive reading (proportional desert), or the abolition reading (inherent dignity violation)?',
    'This omega documents the committer structure: the same state execution authority is read three ways. The deterrence reading (THIS constraint) justifies execution by appeal to future victims'' interests. The retributive reading justifies it by appeal to proportional punishment for past harm. The abolition reading denies the authority entirely on grounds of inherent human dignity. These are not empirical disputes — they are value and framing choices that modify beneficiary/victim composition and constraint logic fundamentally.',
    'The deterrence reading requires empirical deterrence efficacy (omega #1). The retributive reading requires only that desert is a legitimate sentencing principle (conceptual). The abolition reading requires that dignity is inviolable (preference). Which reading is adopted changes which future murder victims appear in the beneficiary set, whether the executed person is an instrumental cost or a rights-bearing end, and whether alternative interpretations of the 8th Amendment apply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'The contested kernel reading: deterrence vs retribution vs abolition').

omega_variable(
    irreversibility_moral_hazard,
    'Does the irreversibility of execution create structural incentives for error tolerance that other punishments do not, independent of deterrence efficacy?',
    'Comparative analysis of appeal success rates, post-conviction review standards, and evidentiary sufficiency thresholds across capital vs non-capital cases; institutional study of how finality doctrines affect appellate scrutiny',
    'If irreversibility creates distinct moral hazard (higher error tolerance): suppression metric should increase to 0.75+, pushing constraint toward pure Snare. If appellate review systems compensate adequately: suppression remains at current 0.65 level. This addresses whether the constraint''s enforcement mechanism is institutionally sound or rests on structural error production.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_moral_hazard, empirical, 'Whether capital punishment''s irreversibility creates unique error-tolerance incentives').

omega_variable(
    temporal_horizon_substitution,
    'Does execution prevent future murders specifically, or does it merely shift the temporal composition of harm (preventing some future murders while creating current executional harm)?',
    'Cohort analysis: aggregate prevented murders (via deterrence) vs executions carried out; comparison across time horizons (5-year, 20-year, lifetime). Net harm calculation with uncertainty bounds.',
    'If prevented murders exceed executions by large margin: utilitarian calculation favors constraint (ε reduces). If margin is small or uncertain: extractiveness increases (ε → 0.60+) because the constraint cannot justify the irreversible harm it causes. This directly addresses the core beneficiary claim (future victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_horizon_substitution, empirical, 'Net prevention of murders vs cost of executions over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deterrence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deterr_theater_t0, deterrence_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(deterr_theater_t30, deterrence_reading, theater_ratio, 30, 0.54).
narrative_ontology:measurement(deterr_theater_t60, deterrence_reading, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(deterr_extract_t0, deterrence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(deterr_extract_t30, deterrence_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(deterr_extract_t60, deterrence_reading, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(deterrence_reading, retributive_reading).
narrative_ontology:affects_constraint(deterrence_reading, abolition_reading).
narrative_ontology:affects_constraint(deterrence_reading, wrongful_conviction_cascade).
narrative_ontology:affects_constraint(deterrence_reading, appellate_review_bottleneck).
narrative_ontology:cs_reading_relation(deterrence_reading, retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation(deterrence_reading, abolition_reading, coexists_with).
% Temporal layer: classical deterrence theory as reference frame; systematic meta-analyses
% have substantially challenged the empirical premise (that CP deters more than LWOP).
% Criminal justice authority structure has not acknowledged this as dispositive.
% Engine computes: axiom_overriding + substantial + false → axiom_foreclosure.
narrative_ontology:cs_reference_frame(deterrence_reading, classical_deterrence_theory).
narrative_ontology:cs_drift_state(deterrence_reading, post_meta_analysis_era,
    gap(axiom_overriding, substantial, false)).

% DUAL FORMULATION NOTE:
% The state_execution_authority kernel decomposes into three constraint stories: deterrence_reading (this story, ε=0.52, Tangled Rope), retributive_reading (ε varies by retributive principle strength, likely 0.30-0.45, Rope/Tangled Rope), and abolition_reading (ε≈0.80+, Snare from all perspectives). These are not observational variants of one constraint but fundamentally different readings of the same kernel that produce different structural classifications. Each reading makes different empirical and philosophical claims about what justifies execution. The deterrence reading is downstream of wrongful_conviction_cascade (errors accumulate in the execution apparatus) and appellate_review_bottleneck (appeals cannot prevent executions once finality is reached).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deterrence_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
