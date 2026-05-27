% ============================================================================
% CONSTRAINT STORY: bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
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
 *   constraint_id: bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primary: State Coercion Justification Framework
 *   domain: constitutional_law/public_health_ethics/political_philosophy
 *
 * SUMMARY:
 *   This constraint embodies one foundational reading of the contested kernel
 *   concerning state authority to impose medical interventions on individuals
 *   who refuse them. The bodily_autonomy_primary reading holds that
 *   individual consent to medical intervention is a principle that cannot be
 *   violated, regardless of claimed collective benefits. This reading
 *   instantiates the axiom that bodily autonomy is inviolable and that the
 *   state lacks legitimate authority to compel medical treatment. Under this
 *   reading, vaccine mandates, forced quarantine, and medical intervention
 *   without consent constitute state extraction via coercive suppression of
 *   refusal, not legitimate public health coordination. The classification
 *   emerges as a Snare from multiple perspectives because the coercion
 *   mechanism (employment loss, institutional access restriction, mobility
 *   limitation) suppresses exit options while extracting compliance from
 *   those who fundamentally oppose the medical intervention. The constraint's
 *   theater ratio reflects that mandate justifications (framed as voluntary
 *   incentives, necessary for collective benefit, temporary emergency
 *   measures) obscure the underlying coercive structure. The temporal
 *   measurements show extractiveness rising sharply as mandates shifted from
 *   guidance to enforcement, peaking as suppression mechanisms (occupational
 *   barriers, institutional access control) matured.
 *
 * KEY AGENTS:
 *   - Coerced Unvaccinated Individuals: Primary victim (powerless/trapped) — subject to medical intervention they refuse; employment, travel, institutional access all contingent on compliance
 *   - Medical Conscience Objectors: Secondary victim (moderate/constrained) — healthcare practitioners and workers whose professional duty conflicts with mandate participation; licensing and employment dependency creates suppression
 *   - Public Health Authority: Nominal beneficiary (powerful/mobile) — exercises coercive power; under this reading, extracts compliance while claiming coordination function
 *   - State Apparatus: Institutional beneficiary (institutional/constrained) — monopolizes medical coercion authority; enforces suppression through administrative and legal mechanisms
 *   - Analytical Observer: Independent position (analytical/analytical) — affirms bodily autonomy axiom; classifies coercive medical intervention as pure extraction regardless of public health narratives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bodily_autonomy_primary, 0.68).
domain_priors:suppression_score(bodily_autonomy_primary, 0.75).
domain_priors:theater_ratio(bodily_autonomy_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bodily_autonomy_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(bodily_autonomy_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bodily_autonomy_primary, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bodily_autonomy_primary, snare).
narrative_ontology:human_readable(bodily_autonomy_primary, "Bodily Autonomy Primary: State Coercion Justification Framework").
narrative_ontology:topic_domain(bodily_autonomy_primary, "constitutional_law/public_health_ethics/political_philosophy").

domain_priors:requires_active_enforcement(bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bodily_autonomy_primary, 'ce673e9d-20a9-47cc-8960-e2e4d6a944f0').
narrative_ontology:cs_created_at('ce673e9d-20a9-47cc-8960-e2e4d6a944f0', '').
narrative_ontology:cs_kernel_codification('ce673e9d-20a9-47cc-8960-e2e4d6a944f0', formalized).
narrative_ontology:cs_authority_grounding('ce673e9d-20a9-47cc-8960-e2e4d6a944f0', lineage).
narrative_ontology:cs_interpretation_layer_present('ce673e9d-20a9-47cc-8960-e2e4d6a944f0').
narrative_ontology:cs_kernel_id(bodily_autonomy_primary, vaccine_mandate_balance).
narrative_ontology:cs_reading_relation('ce673e9d-20a9-47cc-8960-e2e4d6a944f0', public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('ce673e9d-20a9-47cc-8960-e2e4d6a944f0', proportionality_reading, influences).
narrative_ontology:cs_axiom('ce673e9d-20a9-47cc-8960-e2e4d6a944f0', foundational, bodily_autonomy_inviolable).
narrative_ontology:cs_axiom_status(bodily_autonomy_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('ce673e9d-20a9-47cc-8960-e2e4d6a944f0', bodily_autonomy_inviolable, deontological).
narrative_ontology:cs_axiom('ce673e9d-20a9-47cc-8960-e2e4d6a944f0', foundational, state_lacks_medical_coercion_authority).
narrative_ontology:cs_axiom_status(state_lacks_medical_coercion_authority, holdable).
narrative_ontology:cs_axiom_grounding('ce673e9d-20a9-47cc-8960-e2e4d6a944f0', state_lacks_medical_coercion_authority, deontological).
narrative_ontology:cs_reference_frame('ce673e9d-20a9-47cc-8960-e2e4d6a944f0', liberal_individual_rights_framework).
narrative_ontology:cs_drift_state('ce673e9d-20a9-47cc-8960-e2e4d6a944f0', contemporary_pandemic_policy, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_victim(bodily_autonomy_primary, coerced_individuals).
narrative_ontology:constraint_victim(bodily_autonomy_primary, medical_conscience_objectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED UNVACCINATED (SNARE) — Subject to medical intervention they refuse; exit options collapse (employment, travel, institutional access all blocked); the constraint exists precisely to suppress alternatives. Maximum experienced extraction — no escape except capitulation or geographic relocation.
constraint_indexing:constraint_classification(bodily_autonomy_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MEDICAL CONSCIENCE OBJECTORS (SNARE) — Healthcare workers and practitioners whose deepest commitments are violated by mandate participation; constrained by professional license dependency and employment barriers; suppression is high through occupational capture. Significant extraction with some costlier exit paths (career change, relocation, underground practice).
constraint_indexing:constraint_classification(bodily_autonomy_primary, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — Experiences the constraint as coordination: justifying and implementing population-level disease control. Under this reading, the authority has legitimate power to impose medical interventions for collective benefit. This perspective sees the constraint as genuine coordination (solving collective action problem). Experiences low suppression because authority exercises power voluntarily within its conception of legitimacy.
constraint_indexing:constraint_classification(bodily_autonomy_primary, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE APPARATUS (TANGLED ROPE) — The state both coordinates (establishes rule of law, enables collective action) and extracts (compels medical compliance under coercive authority). Active enforcement required. Constrained exit — the state cannot exit its own jurisdiction but can revise the governing framework. This reading locks in coercion as justified by collective benefit.
constraint_indexing:constraint_classification(bodily_autonomy_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (SNARE) — From the standpoint of this reading's own axioms, state medical coercion is extractive suppression of bodily autonomy. The analytical position affirms the foundational axiom (individual consent is inviolable) and therefore classifies mandatory medical intervention as pure extraction regardless of justifying narratives about collective benefit. The constraint exists to suppress the alternative framework (public_health_primary reading).
constraint_indexing:constraint_classification(bodily_autonomy_primary, snare,
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
    constraint_indexing:constraint_classification(bodily_autonomy_primary, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint operates by suppressing refusal (individuals lose employment, institutional access, travel rights if they decline medical intervention). The primary beneficiary is the state apparatus and public health authority; the cost is borne by those whose bodily autonomy is violated. The measurement trajectory shows extractiveness rising from 0.35 (pre-pandemic, minimal medical coercion) to 0.72 (peak enforcement) as suppression mechanisms matured. Under this reading, the extractiveness is inherent to the coercive justification, not contingent on epidemiological severity. Suppression (0.75): Severe. The constraint suppresses alternatives by blocking exit: employment termination for noncompliance, institutional access denial, mobility restrictions, professional license jeopardy. The suppression is structural, not merely circumstantial. Theater ratio (0.35): Moderate-Low. Unlike constraints that use elaborate performative justification to obscure extraction, this reading's coercion is relatively transparent — mandates are openly justified by collective benefit, not disguised as voluntary mechanisms. However, the framing as 'incentive-based' rather than 'coercive' introduces theater (suggesting choice where suppression removes it). The measurement trajectory shows theater increasing as enforcement shifted from explicit mandate to incentive language, with the 'choice' narrative becoming the primary performative cover for the underlying coercion.
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap is between the coerced individual's experience (snare: suppressed choice, coerced compliance, extraction via bodily autonomy violation) and the public health authority's experience (rope: solving collective action problem, exercising legitimate public health power). This reading privileges the victim perspective — it interprets the collective benefit justification as extractive rationalization, not as a genuine coordination function. The analytical observer affirms this gap by holding that bodily autonomy violation cannot be reframed as coordination regardless of epidemiological outcomes. The state apparatus perspective (tangled rope) reflects that the state both coordinates (establishes rule, enables collective action) and extracts (compels medical compliance); this reading subordinates the coordination function to the coercion mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading assigns directionality based on the coercive power structure: individuals refusing medical intervention are the victims (high d, high f(d), high χ); the public health authority is the beneficiary extracting compliance (low d, negative χ); medical conscience objectors are secondary victims facing occupational suppression (moderate-high d). The analytical observer at civilizational scope sees the constraint as coercive suppression of bodily autonomy — affirming the reading's foundational axiom, the analytical position derives d from the principle violation rather than from empirical outcomes. The state apparatus maintains medium-constrained exit because it cannot exit its own jurisdiction but can revise the framework if the reading's axiom is accepted. The reading treats the public health authority's coordination narrative as a cover story: collective benefit claims do not change the extractive structure, they only provide justification for suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by holding that no level of collective benefit justifies bodily autonomy violation. The snare classification is stable across all perspectives that affirm the foundational axiom. The mandatrophy surfaces as the tension between different readings of the contested kernel: the public_health_primary reading would classify the same constraint as tangled_rope (genuine coordination with asymmetric extraction) or even rope (pure coordination), while this reading insists on snare (pure extraction). The mandatrophy is not resolved by data but by which axiom governs the analysis — bodily autonomy inviolability (this reading) vs. state authority to protect collective welfare (public_health_primary reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_benefit_threshold,
    'What level of collective benefit justifies state compulsion of individual medical intervention? Is any positive externality sufficient, or is a threshold required?',
    'Comparative analysis of thresholds across jurisdictions and historical cases; examination of which diseases/interventions crossed the mandate threshold and which did not; correlation between declared collective benefit magnitude and actual epidemiological outcomes',
    'This reading assumes NO threshold — the axiom is inviolability of bodily autonomy. If a threshold exists and is real, the constraint reclassifies toward proportionality_reading (balanced framework). If no threshold is enforced (all mandates pass), the snare classification is affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_benefit_threshold, conceptual, 'Whether collective benefit threshold exists for justifying medical coercion').

omega_variable(
    medical_conscription_vs_voluntary_incentive,
    'Is this reading''s distinction between coercive mandate and incentive-based compliance (lost employment, restricted access) a real categorical boundary or a rhetorical one? Does the constraint operate identically whether framed as mandate or incentive?',
    'Empirical analysis of compliance rates, subjective experience, and reported coercion under mandate vs. incentive framings; examination of which subjects perceive choice and which perceive coercion across framing conditions; historical comparison of the same intervention under different framing regimes',
    'If the boundary is real (incentive-based truly allows exit): constraint reclassifies toward tangled_rope (mixed coordination/extraction). If purely rhetorical (subjects experience identical suppression): snare classification is confirmed and the ''choice'' framing is identified as theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_conscription_vs_voluntary_incentive, empirical, 'Whether incentive-based compliance is genuinely distinct from coercive mandate').

omega_variable(
    reading_kernel_instability,
    'Does the bodily_autonomy_primary reading rest on a foundational axiom that has been formally overridden or substantially eroded in the legal tradition that grounds it?',
    'Jurisprudential analysis of constitutional tradition and precedent; examination of cases where bodily autonomy was asserted vs. cases where it was subordinated to collective benefit; assessment of whether the axiom status has shifted from holdable to overridden in the reading''s own doctrinal lineage',
    'If the axiom remains holdable: this reading is a live constitutional position. If it has been formally overridden in precedent: the reading persists as a dissenting position but not as the governing framework. If it has been substantially eroded (too many exceptions): the reading faces axiom_overriding drift that may force reclassification of its authority grounding from lineage to something more fragile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_instability, empirical, 'Whether bodily autonomy axiom is still holdable in the doctrinal tradition').

omega_variable(
    sibling_reading_counterfactual,
    'If this reading (bodily_autonomy_primary) and the public_health_primary reading coexist as held by different parties, what structural conditions allow both to persist? Why does one not definitively displace the other?',
    'Institutional analysis of how different jurisdictions, legal traditions, and policy regimes adopt different readings; examination of whether parties operating under different readings can coordinate or whether they inevitably enter conflict; assessment of whether the readings are genuinely coexistent or whether one is subordinated to the other in any actual governance structure',
    'If truly coexistent: the kernel is genuinely contested and both readings remain live. If one is subordinated: the kernel classification should shift toward forecloses or influences relationship. If conflict is inevitable: the coexistence is unstable and will resolve toward one reading gaining institutional dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_counterfactual, conceptual, 'Why bodily_autonomy_primary and public_health_primary readings coexist rather than one foreclosing the other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bodily_autonomy_primary, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bodily_autonomy_pre_pandemic_theater, bodily_autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bodily_autonomy_mandate_framing_theater, bodily_autonomy_primary, theater_ratio, 2, 0.28).
narrative_ontology:measurement(bodily_autonomy_incentive_language_theater, bodily_autonomy_primary, theater_ratio, 4, 0.35).
narrative_ontology:measurement(bodily_autonomy_choice_narrative_theater, bodily_autonomy_primary, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(bodily_autonomy_pre_pandemic_extraction, bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bodily_autonomy_mandate_onset_extraction, bodily_autonomy_primary, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(bodily_autonomy_enforcement_escalation, bodily_autonomy_primary, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(bodily_autonomy_compliance_pressure_peak, bodily_autonomy_primary, base_extractiveness, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(bodily_autonomy_primary, public_health_primary).
narrative_ontology:affects_constraint(bodily_autonomy_primary, proportionality_reading).
narrative_ontology:affects_constraint(bodily_autonomy_primary, medical_conscience_protection).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_balance kernel decomposes into three structurally distinct constraints, each with its own extractiveness value and axioms. bodily_autonomy_primary (ε=0.68) prioritizes consent inviolability. public_health_primary (ε=0.40–0.55 estimated) prioritizes collective benefit. proportionality_reading (ε=0.35–0.50 estimated) seeks balance between the two. Each reading is a separate constraint story because they employ different justifications, different victim sets, and different suppression mechanisms. The measurement trajectories and fundamental axioms differ sharply. All three remain live positions in contemporary governance, held by different parties and jurisdictions. They are linked through network.affects_constraints to show that asserting one reading creates structural pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
