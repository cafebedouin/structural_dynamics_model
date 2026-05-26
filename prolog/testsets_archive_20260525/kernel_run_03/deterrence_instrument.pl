% ============================================================================
% CONSTRAINT STORY: deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deterrence_instrument, []).

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
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deterrence_instrument
 *   human_readable: Capital Punishment as Deterrence Instrument
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint models capital punishment justified specifically and
 *   exclusively through deterrence logic: the state executes condemned
 *   persons to prevent future murders committed by unrelated actors at a rate
 *   and cost ratio that society finds acceptable. This is ONE reading of the
 *   contested kernel 'state killing authority' — distinct from retributive
 *   desert (punishment proportional to moral culpability) and categorical
 *   abolition (no state killing authority under any circumstance). The
 *   deterrence reading instrumentalizes the condemned person's death as a
 *   means to protect potential future victims. This reading creates a
 *   specific beneficiary structure (future victims protected via deterrence,
 *   state authority that exercises killing power) and a specific victim
 *   structure (condemned persons whose death serves as instrument, families
 *   of condemned, and the epistemic reliability of criminology itself if
 *   deterrence claims are empirically false). The extractiveness has
 *   increased from 0.55 to 0.68 over the interval as empirical challenges to
 *   deterrence efficacy have accumulated, forcing the doctrine to rely
 *   increasingly on theoretical claims rather than demonstrated prevention —
 *   the theater ratio has correspondingly risen from 0.35 to 0.58 as the
 *   justification becomes more performative.
 *
 * KEY AGENTS:
 *   - Condemned Persons: Primary victims (powerless/trapped) — instrumentalized as deterrence mechanism; bear full cost with zero exit capacity
 *   - Families of Condemned: Secondary victims (powerless/trapped) — intergenerational trauma, status loss, no compensation or agency
 *   - Potential Future Victims: Nominal beneficiaries (abstract, statistical) — their protection is the stated justification but they have no voice in the decision and no compensation mechanisms
 *   - State Authority Apparatus: Primary beneficiary (institutional/arbitrage) — captures legitimacy claims (protecting citizens, rational exercise of sovereignty), centralizes killing authority, maintains control over life-death decisions
 *   - Crime Victims and Advocates: Enlist as legitimacy source (moderate/constrained) — deployed rhetorically to justify executions but derive no material benefit; often disappointed by the constraint's failure to prevent future crime or restore their losses
 *   - Criminal Justice Professionals: Institutional intermediaries (moderate/constrained) — implement the constraint; experience coordination (clear procedures) alongside extraction (moral responsibility, cognitive dissonance)
 *   - Deterrence Doctrine (Piton): The institutional form itself — persists through inertia despite empirical contestation; seen from civilizational perspective as degraded ritual
 *   - Analytical Observer: Perspective holder (analytical/analytical) — risks naturalizing contingent empirical claim and normative axiom as logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deterrence_instrument, 0.68).
domain_priors:suppression_score(deterrence_instrument, 0.75).
domain_priors:theater_ratio(deterrence_instrument, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deterrence_instrument, extractiveness, 0.68).
narrative_ontology:constraint_metric(deterrence_instrument, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(deterrence_instrument, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deterrence_instrument, snare).
narrative_ontology:human_readable(deterrence_instrument, "Capital Punishment as Deterrence Instrument").
narrative_ontology:topic_domain(deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(deterrence_instrument, formalized).
narrative_ontology:cs_authority_grounding(deterrence_instrument, extraction).
narrative_ontology:cs_interpretation_layer_present(deterrence_instrument).
narrative_ontology:cs_kernel_id(deterrence_instrument, state_killing_authority).
narrative_ontology:cs_reading_relation(deterrence_instrument, retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation(deterrence_instrument, categorical_abolition, coexists_with).
narrative_ontology:cs_axiom(deterrence_instrument, foundational, deterrence_empirical_efficacy).
narrative_ontology:cs_axiom_status(deterrence_empirical_efficacy, holdable).
narrative_ontology:cs_axiom_grounding(deterrence_instrument, deterrence_empirical_efficacy, empirically_contingent).
narrative_ontology:cs_axiom(deterrence_instrument, foundational, instrumental_life_cost_justified).
narrative_ontology:cs_axiom_status(instrumental_life_cost_justified, holdable).
narrative_ontology:cs_axiom_grounding(deterrence_instrument, instrumental_life_cost_justified, deontological).
narrative_ontology:cs_reference_frame(deterrence_instrument, rational_harm_prevention_authority).
narrative_ontology:cs_drift_state(deterrence_instrument, contemporary_empirical_contestation_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deterrence_instrument, potential_future_victims).
narrative_ontology:constraint_beneficiary(deterrence_instrument, state_authority_apparatus).
narrative_ontology:constraint_victim(deterrence_instrument, condemned_persons).
narrative_ontology:constraint_victim(deterrence_instrument, families_of_condemned).
narrative_ontology:constraint_victim(deterrence_instrument, epistemic_reliability_of_deterrence_claim).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED PERSON (SNARE) — No exit option available. Faces maximal extraction under this reading: their death is instrumentalized as a means to deter others' potential future crimes. The beneficiary class (potential future victims, abstract and unverifiable) provides the legitimacy claim. The condemned person bears the full cost of the deterrence wager with no possibility of exit or negotiation. Their death is meaningful only if deterrence works — but that claim is epistemically unverifiable from their perspective.
constraint_indexing:constraint_classification(deterrence_instrument, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FAMILIES OF THE CONDEMNED (SNARE) — Intergenerational trauma inflicted with no exit. Bear the cost of state execution, status loss, and grief without agency or compensation. Trapped within the jurisdiction's legal system and social stigma. Experience extraction through loss of kinship and relational identity.
constraint_indexing:constraint_classification(deterrence_instrument, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CRIME VICTIMS AND ADVOCATES (SNARE) — Constrained by the traumatic reality of crime victimization and the false promise that execution of a future perpetrator would prevent their harm (which is already irreversible). This reading instrumentalizes potential future victims but cannot actually restore or compensate actual victims. Victims and advocates are often enlisted to legitimize the constraint but derive no material benefit — the benefit flows to the state authority apparatus through enhanced legitimacy.
constraint_indexing:constraint_classification(deterrence_instrument, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE AUTHORITY APPARATUS (ROPE) — From the state's internal perspective, capital punishment functions as a coordination mechanism: it centralizes the right to kill, displaces vigilante justice, and creates a procedural framework for exercising sovereignty over life. The state benefits from enhanced legitimacy claims (protecting future citizens, exercising rational authority), bureaucratic integration, and the appearance of order. Sees the constraint as coordination of a legitimate state function, not extraction. Arbitrage exit option because the state apparatus can choose to abandon capital punishment if the deterrence claim fails without losing fundamental authority.
constraint_indexing:constraint_classification(deterrence_instrument, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CRIMINAL JUSTICE PROFESSIONALS (TANGLED ROPE) — Judges, prosecutors, defense attorneys, and correctional staff experience genuine coordination (the deterrence framework provides clear procedures and legitimacy claims for capital punishment) alongside asymmetric extraction (they must execute or defend executions even when doubting efficacy, bear moral responsibility, face cognitive dissonance between professed deterrence belief and empirical skepticism). Constrained by professional requirements and career path dependencies — exit (becoming skeptical of deterrence in practice) carries high cost.
constraint_indexing:constraint_classification(deterrence_instrument, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DETERRENCE DOCTRINE AS INSTITUTIONAL FORM (PITON) — From the long-term institutional perspective, the deterrence justification for capital punishment has become substantially performative. The empirical claim has been contested for decades; meta-analyses show no reliable deterrent effect; the doctrine persists through institutional inertia and historical authority rather than epistemic function. Theater ratio is moderate because some genuinely rationalist discourse occurs (cost-benefit calculations, empirical studies), but the operative mechanism is now tradition and identity (the state 'is' the kind of authority that executes), not demonstrated deterrence. Piton classification derives from theater gate and degraded functional claim.
constraint_indexing:constraint_classification(deterrence_instrument, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — From a universalist analytical perspective, some argue that if capital punishment actually prevented murders at acceptable cost, it would be justified — this appears as an immutable logical principle: rational choice theory dictates that any punishment mechanism preventing greater harm is justified. However, this reading declares beneficiaries (potential future victims, state authority), which triggers false summit detection. The 'logical necessity' naturalizes what is actually a contingent empirical claim (deterrence efficacy) coupled with a contested normative axiom (acceptability threshold). The engine will reclassify this as false summit, revealing that the mountain is constructed.
constraint_indexing:constraint_classification(deterrence_instrument, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deterrence_instrument_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deterrence_instrument, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deterrence_instrument, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deterrence_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deterrence_instrument, TR),
    TR >= 0.70.

:- end_tests(deterrence_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts significantly from condemned persons (their death), families (relational loss and trauma), and the epistemic commons (if deterrence is not demonstrated, the extraction is masked by a false empirical cover story). The beneficiary set is asymmetrically distributed: state authority and abstract potential future victims gain; condemned persons and families lose absolutely. The extractiveness has risen from 0.55 to 0.68 as deterrence efficacy claims have faced sustained empirical challenge over the interval — as the empirical foundation weakens, the constraint relies more on rhetorical and procedural performance (theater) to maintain legitimacy. Suppression (0.75): Very high. The condemned person has no exit option (trapped); families have only exit through migration or emotional suppression; crime victims are suppressed by the false promise that executing future perpetrators will restore their losses; criminal justice professionals face career suppression (exit costs for expressing deterrence doubt); the general public is suppressed by institutional authority claims about what is necessary for safety. The constraint persists despite contestation because alternatives (abolition, life imprisonment without parole) face their own institutional and legitimacy barriers. Theater ratio (0.58): Moderate-high and rising. The constraint involves some genuine rationalist discourse (cost-benefit analyses, criminological studies attempting to measure deterrence). But the operative institutional mechanism is increasingly performative: the belief that capital punishment deters has been disputed in major meta-analyses for 50+ years; the persistence of the doctrine reflects institutional inertia, political identity, and historical practice rather than demonstrated efficacy. The theater ratio rises as empirical contestation increases but the doctrine remains institutionally entrenched.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits striking perspectival disagreement rooted in structural position. The state apparatus sees coordination (Rope): centralizing the right to kill, creating procedure, protecting future citizens. Criminal justice professionals see mixed coordination and extraction (Tangled Rope): genuine procedures alongside moral responsibility for implementing a claim they increasingly doubt. Condemned persons and families see pure extraction (Snare): their death serves others' interests with no exit or benefit to themselves. Crime victims see a false promise (Snare): execution of future perpetrators does nothing to restore their loss or prevent their own victimization. The institutional form itself (Piton) is seen as degraded ritual by civilizational observers who note the empirical contestation and persistent performance. The analytical observer risks seeing a logical necessity (Mountain) — 'if deterrence worked, it would be justified' — but this naturalizes two contingent elements: the empirical claim (deterrence actually prevents murders) and the normative axiom (the life of a condemned person is an acceptable cost for statistical prevention of future murders). The false summit detection reveals that the 'logical necessity' is constructed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the extraction flow. Condemned persons (trapped, powerless) have d ≈ 0.95 (full target): they experience the constraint entirely as extraction. Families of condemned (trapped, powerless) have d ≈ 0.92 (near-full target): collateral damage through relational loss. Potential future victims (statistical beneficiaries with no voice) have d ≈ 0.15 (partial beneficiary): they are rhetorically positioned as benefiting but have no agency in or compensation from the constraint. State authority (institutional, arbitrage) has d ≈ 0.08 (beneficiary): captures legitimacy and control with ready exit if the deterrence claim fails (can abandon capital punishment and maintain state authority). Crime victims have d ≈ 0.70 (near-full target): deployed as legitimacy sources but their actual losses are not addressed by future executions; they are instrumentalized and then disappointed. Criminal justice professionals (moderate, constrained) have d ≈ 0.55 (symmetric harm and benefit): genuine coordination benefits (clear procedures, legitimate framework) mixed with extraction (moral responsibility, career costs of doubt). The piton perspective (institutional, civilizational) has d ≈ 0.50 (degraded, ambiguous): the institutional form persists but its function is contested. The analytical false summit perspective has d ≈ 0.72 (observer position): sees the structure without the naturalizing frame.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by instantiating the deterrence reading as a coherent but contested position. The mandatrophy question 'is capital punishment a justified harm-prevention instrument or pure extraction?' is resolved BY the deterrence reading's structure, not answered externally. The reading declares: 'it is justified AS an instrument IF deterrence works at acceptable cost.' The embedded empirical uncertainty (omega_deterrence_empirical_contestation) is irreducible — we do not know whether deterrence actually works — so the constraint's classification cannot be finalized without resolving that omega. If deterrence is proven, extractiveness drops toward 0.45 (rational harm-prevention instrument, closer to Tangled Rope). If deterrence is disproven, extractiveness stays at 0.68 (pure extraction masked by false empirical claim, confirmed Snare). If deterrence remains contested/inconclusive, the theater ratio rises further and the constraint approaches Piton classification (performative maintenance of an empirically undermined doctrine). The false summit detection on Perspective 7 shows that the analytical reader risks naturalizing this empirical contingency as logical necessity, which is the mandatrophy's deepest trap: the reader mistakes a contestable claim about deterrence efficacy and acceptable cost for an immutable principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_contestation,
    'Does capital punishment actually prevent future murders at rates measurable above background crime trends and confounding variables?',
    'Meta-analysis of criminological studies controlling for: regional crime trends, demographic changes, policing intensity, economic factors, incapacitation effects (removal of the specific perpetrator vs. general deterrence). Cross-national and temporal comparisons between abolitionist and retentionist jurisdictions.',
    'If deterrence is proven: the extractiveness floor shifts from 0.68 toward 0.45 (rational harm-prevention instrument rather than pure extraction). If deterrence is disproven: extractiveness remains 0.68 (extraction mechanism masked by empirical cover story). If uncertain/context-dependent: omegas remain unresolved, constraint remains snare, doctrine becomes piton (performative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_empirical_contestation, empirical, 'Whether capital punishment prevents future murders at measurable rates').

omega_variable(
    acceptable_cost_definition,
    'What constitutes ''acceptable cost'' for preventing a statistical future murder? How is the life of the condemned person weighed against the statistical value of preventing unknown future murders?',
    'Normative analysis of how different ethical frameworks value statistical lives (utilitarian cost-benefit), individual rights (deontological inviolability), and procedural justice (due process). Comparison to other contexts where statistical lives are traded off (automotive safety, medical rationing, environmental regulation).',
    'If cost is defined as utilitarian calculus: acceptability depends on empirical deterrence rate. If cost is defined as inviolable rights: no cost is acceptable regardless of deterrence efficacy (collapses to categorical abolition reading). If procedural: acceptability depends on trial quality, not deterrence efficacy (collapses to retributive desert reading). Different resolutions route to different constraint types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_cost_definition, preference, 'Definition and valuation of acceptable cost for deterrence benefit').

omega_variable(
    false_positive_catastrophe,
    'When the deterrence claim fails (no measurable prevention of future murders), what is the epistemic status of the executions already carried out? Are they retrospectively recast as retributive or purely extractive?',
    'Historical and legal analysis of how jurisdictions respond when deterrence justifications collapse: do they reframe as desert-based retribution, accept that executions were unjustified harm, or simply stop engaging with the question? Comparison to other discredited justifications (preventive detention, involuntary sterilization) and how legal systems handled the reckoning.',
    'If reframe to retribution: the reading loses its distinctive structure (becomes retributive_desert reading). If accept unjustified harm: snare classification is confirmed, constraint becomes condemned as pure extraction. If epistemically disengage: doctrine becomes piton (performative maintenance without justificatory coherence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_positive_catastrophe, conceptual, 'Epistemic status of executions if deterrence claim fails').

omega_variable(
    future_victim_beneficiary_circularity,
    'Are potential future victims (declared as beneficiaries) ever actual stakeholders in the capital punishment decision, or does the benefit accrue only to state authority that claims to act on their behalf?',
    'Institutional and empirical analysis: do future potential victims have voice, veto, or compensation mechanisms in capital punishment decisions? Or is their status purely rhetorical — invoked to legitimize state action but not genuinely consulted? Comparison to other future-focused policies (environmental regulation, pension systems) where future stakeholders have explicit mechanisms.',
    'If truly stakeholders: beneficiary declaration is accurate, extraction to victims is partial (some coordination of future protection occurs). If rhetorical only: beneficiaries are actually only the state apparatus, snare classification is confirmed with higher purity (no genuine coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_victim_beneficiary_circularity, empirical, 'Whether potential future victims are actual beneficiary stakeholders or rhetorical cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deterrence_instrument, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dete_tr_t0, deterrence_instrument, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dete_tr_t2, deterrence_instrument, theater_ratio, 2, 0.48).
narrative_ontology:measurement(dete_tr_t4, deterrence_instrument, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(dete_be_t0, deterrence_instrument, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(dete_be_t2, deterrence_instrument, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(dete_be_t4, deterrence_instrument, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(deterrence_instrument, retributive_desert).
narrative_ontology:affects_constraint(deterrence_instrument, categorical_abolition).

% DUAL FORMULATION NOTE:
% The deterrence_instrument reading is one constraint within the state_killing_authority kernel family. The sibling constraints (retributive_desert and categorical_abolition) share the same contested authority structure but instantiate different beneficiary sets and different justificatory mechanisms. Deterrence grounds state killing in future-focused harm prevention (benefits flow to potential future victims and state authority). Retribution grounds it in past-focused proportional punishment (benefits flow to justice mechanism and deterrent effect is secondary). Abolition denies any benefit suffices to ground the authority. Each reading has its own constraint story with its own ε value, perspectives, and omegas. The network edges show how the readings influence each other's structural plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
