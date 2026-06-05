% ============================================================================
% CONSTRAINT STORY: bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bodily_autonomy_primary, []).

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
 *   constraint_id: bodily_autonomy_primary
 *   human_readable: Bodily Autonomy vs. State-Mandated Medical Intervention
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the bodily_autonomy_primary reading of the
 *   contested kernel 'legitimate health intervention.' Under this reading,
 *   informed consent for medical interventions is foundational — state
 *   coercion violates bodily integrity regardless of public benefit. The
 *   constraint emerges at the intersection of medical ethics doctrine
 *   (Nuremberg Code, Helsinki Declaration, professional medical standards
 *   requiring informed consent), constitutional law (Fourteenth Amendment
 *   liberty interests in bodily integrity), and public health emergency
 *   response (state authority to mandate vaccination, quarantine, treatment
 *   during epidemiological crisis). The structural tension is acute: the
 *   constraint declares bodily autonomy inviolable, yet state apparatus
 *   implements mandates that override individual refusal under threat of
 *   employment loss, healthcare access denial, and social services exclusion.
 *   This reading treats mandate-targeted workers as primary victims and the
 *   public health apparatus as beneficiary/extractor via employment and
 *   access leverage. The constraint exhibits Tangled Rope dynamics: genuine
 *   coordination function exists (disease prevention, healthcare system
 *   protection), but the mechanism relies on suppression (livelihood threat,
 *   access restriction) and asymmetric extraction (state and compliant
 *   populations benefit; mandate-targeted individuals bear bodily
 *   intervention cost). Theater ratio remains moderate (0.35) because the
 *   constraint is mechanically straightforward — coercion operates directly
 *   through employment/access threat, not through institutional performance.
 *   Theater rises slightly over the interval as mandate enforcement continues
 *   despite declining disease threat, suggesting extraction motivation
 *   becomes more visible. Extractiveness rises sharply at t=6 (mandate
 *   introduction with employment enforcement) and stabilizes at t=12 (endemic
 *   enforcement baseline).
 *
 * KEY AGENTS:
 *   - Mandate-Targeted Workers: Primary victims (powerless/trapped) — face binary choice (intervention or employment loss); experience maximum suppression and extraction. Concentrated in essential services (healthcare, emergency services, transportation, military) where mandates bite hardest.
 *   - Public Health Apparatus: Primary beneficiary (institutional/arbitrage) — captures public health authority and emergency powers; experiences constraint as pure coordination (disease control, system protection). Maximum exit options (can modify mandate, lift requirement, shift strategy). Includes CDC, state health departments, hospital systems, occupational health agencies.
 *   - Hesitant But Mobile Population: Secondary victim-beneficiary (moderate/constrained) — experiences mixed constraint (intervention cost) and benefit (indirect disease protection, reduced healthcare burden). Moderate exit options (relocation, delay, exemption navigation). Includes reluctant but economically mobile workers.
 *   - Bioethics & Rights Coalition: Organized actors (organized/constrained) — medical ethicists, civil liberties unions, professional medical associations. Perceive constraint as temporary emergency measure with sunsettable components. Build institutional resistance (informed refusal pathways, exemption procedures, legal challenges).
 *   - Affluent & Jurisdictionally Mobile: Secondary beneficiaries (powerful/mobile) — experience extraction (formal mandate) but with high exit capacity (interstate mobility, private healthcare access, legal navigation). Effective suppression drops to 0.25-0.30 due to mobility; extract themselves from constraint.
 *   - Medical Licensing Authority: Institutional actor (institutional/arbitrage) — maintains informed consent doctrine while enforcing mandates circumventing it. Experiences institutional contradiction (certify ethical practitioners while coercing interventions). Theater rises as gap widens.
 *   - Vulnerable Populations (disease-exposed, immunocompromised, children): Secondary beneficiaries under public_health_primary reading; ambiguous under bodily_autonomy_primary reading — receive indirect protection from mandate but not directly targeted.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bodily_autonomy_primary, 0.58).
domain_priors:suppression_score(bodily_autonomy_primary, 0.68).
domain_priors:theater_ratio(bodily_autonomy_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bodily_autonomy_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(bodily_autonomy_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(bodily_autonomy_primary, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(bodily_autonomy_primary, "Bodily Autonomy vs. State-Mandated Medical Intervention").
narrative_ontology:topic_domain(bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(bodily_autonomy_primary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bodily_autonomy_primary, public_health_apparatus).
narrative_ontology:constraint_beneficiary(bodily_autonomy_primary, nonvaccinated_population_indirect).
narrative_ontology:constraint_victim(bodily_autonomy_primary, mandate_targeted_workers).
narrative_ontology:constraint_victim(bodily_autonomy_primary, medical_autonomy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(bodily_autonomy_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(bodily_autonomy_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(bodily_autonomy_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(bodily_autonomy_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(bodily_autonomy_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

constraint_indexing:constraint_classification(bodily_autonomy_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(bodily_autonomy_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bodily_autonomy_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bodily_autonomy_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bodily_autonomy_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bodily_autonomy_primary, TR),
    TR >= 0.70.

:- end_tests(bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time from 0.35 to 0.62. The constraint shows classic extraction accumulation: extractiveness rises when mandate enforcement mechanisms activate (t=6, +0.23 jump) and remains elevated as enforcement baseline stabilizes. The magnitude reflects asymmetric burden distribution: mandate-targeted workers bear bodily intervention cost; public health apparatus and compliant populations bear coordination benefit. Extraction is not maximal (0.58, not 0.75+) because: (1) genuine disease control coordination exists, (2) some exit options remain (relocation, exemption pathways, legal challenge), (3) public health justification is empirically grounded (reducing hospitalization/mortality), (4) suppression is high but not total — refusal is possible at cost. Suppression (0.68): High structural suppression through employment threat and access restriction. Livelihood threat (job termination, denied healthcare access) creates material barriers to exit for economically dependent workers. Suppression does not reach 0.80+ because: (1) some jurisdictions lack mandates (interstate exit option exists, though costly), (2) exemption pathways exist (though accessibility gradients apply), (3) informal evasion possible (though with social cost), (4) legal challenges persist (constraint faces institutional resistance). Theater Ratio (0.35): Moderate, rising from 0.20 to 0.38. The constraint is mechanically transparent — coercion operates directly via livelihood threat, not through elaborate institutional theater. Theater rises slightly over interval as mandate enforcement continues despite declining disease threat (t=6-12 interval shows dropping case loads nationally), suggesting institutional inertia rather than proportional response to epidemiological conditions. If disease threat had persisted at peak, theater would remain lower (constraint perceived as justified); declining threat with stable enforcement increases perceived theater (why maintain mandate when emergency has passed?).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The mandate-targeted worker (Snare, d=0.95) and the public health authority (Rope, d=0.05) experience the identical structural mechanism from opposite positions along the extraction gradient. The worker perceives coercive bodily intervention under employment duress; the authority perceives coordinated disease control serving collective benefit. The worker's powerlessness (no exit, maximum suppression) and the authority's institutional power (broad exit options, emergency discretion) are not disagreements about the constraint's type — they are observations about how the same constraint operates from different structural positions. The bioethics coalition (Scaffold) adds temporal perspective: the same constraint is seen as emergency-temporary from the organized standpoint (sunset as disease threat recedes) but as permanent extraction from the powerless standpoint (no automatic sunset, no cost-benefit rebalancing). The affluent mobile agent (Tangled Rope, d=0.30) perceives lower suppression than the economically trapped worker (d=0.95) because their exit options are genuinely different — they can arbitrage to non-mandate jurisdictions, access private alternatives, or navigate exemptions with legal resources. The medical licensing authority (Piton) perceives the constraint as degraded ritual — the institution maintains informed consent doctrine while enforcing interventions that bypass consent, creating institutional theater (signatures obtained, process performed) masking absence of genuine voluntary choice. The analytical observer's mountain classification reveals a false summit: the 'natural law of bodily integrity' is naturalization of a contested institutional arrangement where different stakeholders read the kernel (legitimate health intervention) differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from structural position within this specific constraint. Mandate-targeted workers (powerless/trapped): d ≈ 0.95 (nearly full extraction target) — face maximum suppression, zero exit capacity, bear all bodily intervention cost. Public health apparatus (institutional/arbitrage): d ≈ 0.05 (full beneficiary) — benefits from authority, emergency powers, population-level disease control, has maximum exit options. Hesitant but mobile population (moderate/constrained): d ≈ 0.55 (symmetric) — bears intervention cost and retains some mobility; benefits from indirect disease protection; some exit capacity. Affluent mobile agents (powerful/mobile): d ≈ 0.30 (net beneficiary) — nominal mandate applies but effective exit options are high; actual suppression experienced is much lower due to arbitrage capacity. The sigmoid f(d) function maps these d values to experienced extractiveness modifiers: trapped powerless agents see the highest chi (effective extraction); arbitrage-capable beneficiaries see the lowest chi (minimal felt constraint). Perspective-by-perspective chi values (using f(d) sigmoid and scope modifiers): powerless trapped (national scope, σ=1.0, d=0.95) → chi ≈ 0.58 × 1.42 × 1.0 ≈ 0.82 (experienced as Snare); institutional arbitrage (global scope, σ=1.2, d=0.05) → chi ≈ 0.58 × (-0.12) × 1.2 ≈ -0.08 (experienced as negative extraction, pure coordination); powerful mobile (regional scope, σ=0.9, d=0.30) → chi ≈ 0.58 × 0.22 × 0.9 ≈ 0.11 (experienced as low Tangled Rope or Rope). The perspectival gap reflects true structural differences in constraint experience, not disagreement about the facts — different agents occupy genuinely different positions relative to extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint manifests classic mandatrophy structure: the mechanism declares inviolable principle (bodily autonomy) while executing violation (coerced intervention) through institutional theater (informed consent form obtained under duress). The false summit is the analytical mountain — bodily autonomy is presented as natural law ('fundamental right,' 'Nuremberg principle,' 'medical ethics foundation') but the structural data reveals construction: beneficiaries exist (public health apparatus, disease-protected populations), extraction is active (bodily intervention forced via employment threat), suppression is deliberately deployed (livelihood threat). The mandatrophy resolution shows that 'bodily autonomy as inviolable principle' and 'state mandate overriding bodily autonomy' cannot coexist in the same institutional framework — the mandate mechanism requires either (1) abandoning the autonomy principle (honest Snare classification, no false summit), or (2) redefining consent under duress as 'informed consent' (maintaining Tangled Rope performance while operationally snare). The bodily_autonomy_primary reading commits to option 1 (treats mandate-coerced consent as non-consent, classifies as Snare from worker perspective, Tangled Rope from analytical perspective). The sibling public_health_primary reading would commit to option 2 (treats disease protection as overriding autonomy, classifies as Rope from public health perspective, accepts higher ε). The proportionality_reading bridges: permits mandate only when disease threat meets evidence threshold, sunsetting as threat recedes. The three readings are structurally incommensurable — the mandatrophy cannot be resolved by empirical data because the contest is value-laden (is bodily autonomy inviolable, or tradeable for public health?). The engine's role is to make the contest visible by showing what each reading commits you to.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of ''legitimate health intervention'' is constitutive: bodily autonomy as inviolable, or public health as primary with autonomy traded for protection?',
    'This omega documents the committer-frame contest itself. THIS constraint instantiates the bodily_autonomy_primary reading (autonomy as foundational). Sibling constraints (public_health_primary, proportionality_reading) instantiate alternative readings from the same kernel. No empirical data resolves which reading is ''correct'' — the contest is structural and value-laden.',
    'Choosing bodily_autonomy_primary reading: mandate-coerced workers enter victim set, state appears as extractor, ε rises to 0.58 (tangled rope). Choosing public_health_primary reading: vulnerable disease-exposed populations enter victim set, reluctant acceptors enter beneficiary set, extraction is reframed as coordination, ε drops. Choosing proportionality_reading: extraction magnitude scales to disease severity and efficacy evidence, ε varies across temporal phases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Reading contest within legitimate_health_intervention kernel').

omega_variable(
    duress_vs_informed_consent,
    'Does a consent form obtained under threat of employment loss constitute informed consent or duress-coerced medical intervention?',
    'Legal doctrine analysis: US case law (Jacobson, Zucht, Cruzan, Glucksberg) treats informed consent as requiring absence of duress. Empirical marker: does the agent demonstrate comprehension and voluntary choice, or do they demonstrate resigned compliance? Post-intervention regret and litigation patterns; comparison of refusal rates before/after employment threat introduction.',
    'If duress: suppression rises to 0.80+, ε rises to 0.65+, constraint reclassifies as pure Snare from more perspectives. If true informed refusal: suppression drops to 0.40, ε drops to 0.35, constraint reclassifies as Rope (coordination with option to opt out). Current classification (0.68 suppression, 0.58 ε, Tangled Rope) assumes duress creates the snare component while residual coordination remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duress_vs_informed_consent, empirical, 'Whether employment threat invalidates informed consent').

omega_variable(
    proportionality_threshold_absence,
    'Does the mandate scale enforcement intensity to disease severity (hospitalizations, mortality rates), or is enforcement maintained at constant level independent of epidemiological conditions?',
    'Temporal analysis: track mandate enforcement (employment terminations, access restrictions, penalties) against disease severity metrics (case fatality rate, hospitalization capacity, excess mortality). Does enforcement decrease as severity decreases? Are sunset thresholds applied (e.g., ''mandate lifts when hospitalizations drop below X per 100k'')?',
    'If proportional: constraint appears as temporary emergency measure (Scaffold from public health perspective), ε declines over time as threat recedes, theater ratio declines as mandate becomes advisory. If disproportional: constraint appears as permanent extraction mechanism (Snare from vulnerable perspective), ε remains constant or rises, theater ratio rises as enforcement continues despite waning threat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_absence, empirical, 'Whether enforcement intensity scales to epidemiological justification').

omega_variable(
    exemption_accessibility_gradient,
    'Are exemption pathways (medical, religious, philosophical) equally accessible across socioeconomic strata, or do they create a second-order extraction gradient where wealthy/educated agents exit while poor/marginalized agents remain trapped?',
    'Empirical analysis of exemption rates by income, education, profession, race, geography. Do high-income professionals (physicians, attorneys, executives) obtain exemptions at higher rates than low-wage workers? Does exemption documentation burden (legal fees, medical consultation costs, time) create barriers for economically constrained groups?',
    'If equal accessibility: suppression measured at 0.68 is accurate for the entire target population. If gradient exists: suppression for low-income trap victims rises to 0.85+, suppression for high-income mobile agents drops to 0.25, constraint decomposes into two distinct constraints with different ε values and different victim sets. False-summit risk: the public health apparatus perceives constraint as Rope (enabling coordination) while vulnerable populations experience Snare, and the apparatus systematically misreads the suppression gradient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exemption_accessibility_gradient, empirical, 'Whether exemption accessibility creates second-order extraction gradient').

omega_variable(
    permanence_vs_sunset_architecture,
    'Is the mandate legislatively permanent or administratively temporary with scheduled review? Does the constraint contain its own sunset mechanism, or does it persist absent active repeal?',
    'Policy document review: what are the explicit termination conditions? Is mandate tied to disease-severity threshold (drops when cases below X), sunset date (expires on Y if not renewed), or legislative reauthorization requirement? If no explicit sunset: what is the de facto removal likelihood given political economy constraints?',
    'If genuine sunset clause with low-cost trigger: Scaffold classification is supported, theater is maintained low (constraint operates as intended temporary measure), ε is justified as bounded. If permanent or high-cost sunset: Scaffold reclassifies to Tangled Rope or Snare, theater rises (constraint claims temporary status but persists indefinitely), ε rises, suggests extraction mechanism has become permanent rather than emergency-responsive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permanence_vs_sunset_architecture, empirical, 'Whether mandate contains effective sunset mechanism or persists indefinitely').

omega_variable(
    bodily_autonomy_constitutional_status,
    'Is bodily autonomy protected as a fundamental constitutional right with heightened judicial scrutiny, or does it receive rational-basis review allowing substantial state restriction?',
    'Constitutional law doctrine: Supreme Court jurisprudence on Fourteenth Amendment liberty (Cruzan, Glucksberg) and Griswold privacy line establish bodily integrity as fundamental right. But Jacobson v Massachusetts (1905) permits overrides for compelling state interest. Current constitutional reading determines judicial deference to mandate — heightened scrutiny requires narrow tailoring and least-restrictive-means; rational basis allows broad discretion.',
    'If fundamental right with heightened scrutiny: mandates face legal challenge on narrowness and alternatives, constraint is legally constrained (external legal suppression of the suppression mechanism itself), ε drops to 0.40-0.45, constraint appears as Rope (coordination with legal boundaries). If rational-basis deference: state discretion is broad, mandate can persist with minimal evidence of necessity, ε rises to 0.65+, constraint appears as Snare to powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bodily_autonomy_constitutional_status, conceptual, 'Constitutional rank of bodily autonomy affects judicial scrutiny of mandate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bodily_autonomy_primary, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bodaut_theater_t0, bodily_autonomy_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bodaut_theater_t6, bodily_autonomy_primary, theater_ratio, 6, 0.35).
narrative_ontology:measurement(bodaut_theater_t12, bodily_autonomy_primary, theater_ratio, 12, 0.38).

% Extraction over time
narrative_ontology:measurement(bodaut_extract_t0, bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bodaut_extract_t6, bodily_autonomy_primary, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(bodaut_extract_t12, bodily_autonomy_primary, base_extractiveness, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(bodily_autonomy_primary, public_health_primary).
narrative_ontology:affects_constraint(bodily_autonomy_primary, proportionality_reading).
narrative_ontology:affects_constraint(bodily_autonomy_primary, informed_consent_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-story constraint family decomposing the contested kernel 'legitimate_health_intervention.' Each reading instantiates the same structural mechanism (state-mandated medical intervention) but with different beneficiary/victim declarations and ε values. The bodily_autonomy_primary reading (this constraint, ε=0.58) treats autonomy as foundational and mandate-targeted workers as primary victims. The public_health_primary reading (sibling, ε varies by protective efficacy) treats disease control as foundational and vulnerable populations as primary victims. The proportionality_reading (sibling, ε varies temporally) treats justification as proportional to threat. All three are structurally valid; the choice between them is value-laden (constitutional, not empirical). Link all three via network.affects_constraints to preserve the kernel-reading structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bodily_autonomy_primary, institutional, 0.05).
constraint_indexing:directionality_override(bodily_autonomy_primary, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
