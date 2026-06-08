% ============================================================================
% CONSTRAINT STORY: intergenerational_justice_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intergenerational_justice_asymmetry, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: intergenerational_justice_asymmetry
 *   human_readable: Intergenerational Justice Asymmetry in Germline Genetic Modification
 *   domain: bioethics/reproductive_medicine/genetic_engineering
 *
 * SUMMARY:
 *   Germline genetic modification creates a structural temporal asymmetry:
 *   the first generation of modified individuals bears disproportionate
 *   safety risks (unknown long-term effects, potential off-target mutations,
 *   psychosocial burden of being 'first') while future generations receive
 *   the benefits (disease elimination, reduced modification risk as
 *   techniques mature, normalized social status). This asymmetry is distinct
 *   from standard research risk because it is irreversible and
 *   identity-constituting — the first modified generation cannot exit the
 *   modification, and their identity is partly constituted through being the
 *   experimental cohort. The constraint exhibits different types depending on
 *   the observer's structural position and time horizon. From the first
 *   generation's biographical perspective, it is a snare — they are trapped
 *   in a modification they did not consent to, bearing risk for others'
 *   benefit. From the institutional perspective coordinating disease
 *   prevention, it is rope — a necessary coordination cost for solving a
 *   genuine collective action problem. From the bioethics review perspective,
 *   it is scaffold — a temporary asymmetry with a sunset as safety data
 *   accumulates. The constraint's extractiveness has increased modestly over
 *   the interval (0.15 → 0.28) as the gap between first-generation risk and
 *   institutional benefit has become more visible. Theater ratio has also
 *   increased (0.25 → 0.42) as consent frameworks and review processes have
 *   become more elaborate without necessarily improving first-generation
 *   protection.
 *
 * KEY AGENTS:
 *   - First Modified Generation: Primary victim (powerless/trapped) — bears maximum safety risk with minimal personal benefit; cannot exit modification; identity-locked
 *   - Future Generations: Primary beneficiary (powerless/trapped initially, but benefit rather than cost) — receive disease elimination and reduced risk without bearing first-generation experimental burden
 *   - Genetic Disease Prevention Programs: Institutional beneficiary (institutional/arbitrage) — coordinate disease elimination; capture mission fulfillment and prestige
 *   - Research Institutions: Mixed actor (institutional/constrained) — both coordinate (advance medicine) and extract (capture research priority, patents during high-risk phase)
 *   - Prospective Parents: Moderate power agents (moderate/constrained) — make high-stakes coordination decision; constrained by disease severity
 *   - Bioethics Review Coalitions: Organized actors (organized/mobile) — see asymmetry as temporary with sunset logic as safety data accumulates
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees coordination mechanism for genuine collective action problem, but classification depends on whether alternatives are suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intergenerational_justice_asymmetry, 0.28).
domain_priors:suppression_score(intergenerational_justice_asymmetry, 0.35).
domain_priors:theater_ratio(intergenerational_justice_asymmetry, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intergenerational_justice_asymmetry, extractiveness, 0.28).
narrative_ontology:constraint_metric(intergenerational_justice_asymmetry, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(intergenerational_justice_asymmetry, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intergenerational_justice_asymmetry, rope).
narrative_ontology:human_readable(intergenerational_justice_asymmetry, "Intergenerational Justice Asymmetry in Germline Genetic Modification").
narrative_ontology:topic_domain(intergenerational_justice_asymmetry, "bioethics/reproductive_medicine/genetic_engineering").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intergenerational_justice_asymmetry, future_generations).
narrative_ontology:constraint_beneficiary(intergenerational_justice_asymmetry, genetic_disease_prevention_programs).
narrative_ontology:constraint_beneficiary(intergenerational_justice_asymmetry, research_institutions).
narrative_ontology:constraint_victim(intergenerational_justice_asymmetry, first_modified_generation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIRST MODIFIED GENERATION (SNARE) — Cannot exit the modification they did not consent to; bears maximum safety risk with minimal personal benefit. Trapped by biological irreversibility and identity fusion with the modification. The coordination story (preventing genetic disease) is real but the extraction is asymmetric — this generation is the experimental cohort for benefits that accrue primarily to their descendants.
constraint_indexing:constraint_classification(intergenerational_justice_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GENETIC DISEASE PREVENTION PROGRAMS (ROPE) — Institutional actors coordinating a genuine collective action problem: eliminating heritable disease. From this perspective the first-generation risk is a necessary coordination cost, analogous to vaccine trials. Arbitrage exit because programs can shift to somatic therapy if germline modification proves too costly. Net beneficiary through institutional prestige and mission fulfillment.
constraint_indexing:constraint_classification(intergenerational_justice_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: BIOETHICS REVIEW COALITIONS (SCAFFOLD) — Organized actors (IRBs, national bioethics councils, international consortia) see the asymmetry as a temporary coordination problem with a sunset: as long-term safety data accumulates and consent frameworks mature, the risk-benefit ratio will equalize across generations. The constraint is transitional — justified by the transition to a post-genetic-disease state, not by the steady state.
constraint_indexing:constraint_classification(intergenerational_justice_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: PROSPECTIVE PARENTS (ROPE) — Moderate power agents making a high-stakes coordination decision: accept first-generation risk to prevent transmission of severe genetic disease. Constrained by the severity of the disease (exit is possible but costly — choosing not to modify means accepting disease transmission). Experience the constraint as coordination because the alternative (disease transmission) is worse from their perspective.
constraint_indexing:constraint_classification(intergenerational_justice_asymmetry, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RESEARCH INSTITUTIONS (TANGLED ROPE) — Both coordinate (advance genetic medicine, prevent disease) and extract (capture research priority, institutional prestige, patent rights during the high-uncertainty phase). Constrained by regulatory frameworks and reputational risk but benefit asymmetrically from first-mover advantage. The coordination function is genuine but the extraction is real — institutions capture value during the risk phase that they do not fully bear.
constraint_indexing:constraint_classification(intergenerational_justice_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective with full information, the intergenerational asymmetry is a coordination mechanism for solving a genuine collective action problem: eliminating heritable disease requires someone to bear first-generation risk, and the alternative (perpetual disease transmission) is worse for the collective. The asymmetry is a necessary cost of the transition, not extraction. However, this classification depends critically on whether the coordination function (disease prevention) is genuine and whether alternatives (somatic therapy, preimplantation screening) are being suppressed.
constraint_indexing:constraint_classification(intergenerational_justice_asymmetry, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intergenerational_justice_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intergenerational_justice_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intergenerational_justice_asymmetry, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(intergenerational_justice_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The first modified generation bears disproportionate risk, but the extraction is not severe because: (1) the coordination function (disease prevention) is genuine, (2) prospective parents consent on behalf of their children (proxy consent has limits but is not absent), (3) the risk-benefit ratio may be acceptable for severe genetic diseases. The value reflects real asymmetry without claiming pure extraction. The increase over time (0.15 → 0.28) reflects growing visibility of the first-generation burden as more data accumulates. Suppression (0.35): Low-moderate. The first generation is identity-locked (cannot exit the modification) but suppression is not total because: (1) prospective parents can choose not to modify (exit at the family level), (2) regulatory frameworks provide some protection, (3) somatic alternatives exist for some conditions. The increase over time (0.20 → 0.35) reflects regulatory frameworks hardening around germline modification, making alternatives harder to access. Theater ratio (0.42): Moderate. Consent processes and bioethics review have become more elaborate (rising from 0.25 to 0.42) but the core protection gap remains: no framework can obtain consent from the first modified generation themselves, and long-term safety data is inherently unavailable at the time of modification. The theater is the performative consent ritual that cannot bridge the irreducible temporal gap.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how temporal position determines classification. The first modified generation experiences maximum extraction (snare) because they are trapped in an irreversible modification they did not consent to, bearing risk for others' benefit. Prospective parents experience coordination (rope) because they are solving a genuine problem (preventing disease transmission) and the alternative is worse. Institutional actors experience coordination (rope or tangled rope) because they are advancing a legitimate medical goal, though research institutions also extract during the high-risk phase. Bioethics review coalitions see a temporary problem with a sunset (scaffold) because safety data will eventually equalize the risk-benefit ratio. The analytical observer sees coordination at the civilizational scale (rope) because the alternative (perpetual disease transmission) is collectively worse. The gap reveals that 'intergenerational justice' is not a single structural fact but a perspectival phenomenon — what looks like necessary coordination cost from one temporal position looks like extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   The first modified generation is the primary victim — they bear the safety risk. Directionality for powerless/trapped agents is high (near 1.0), producing high effective extraction. Future generations are beneficiaries — they receive disease elimination without bearing first-generation risk. Genetic disease prevention programs are institutional beneficiaries — they coordinate a genuine function and capture prestige. Research institutions are mixed — they both coordinate (advance medicine) and extract (capture priority during the risk phase), producing moderate directionality. Prospective parents are moderate power agents making a constrained choice — they experience the constraint as coordination because the alternative (disease transmission) is worse from their perspective, producing low-moderate directionality. Bioethics review coalitions are organized actors with mobile exit — they can shift frameworks if the asymmetry proves unacceptable, producing low directionality. The analytical observer sees coordination at the civilizational scale, producing low or negative directionality. The perspectival gap is substantial: the first generation sees snare (trapped, high extraction), while institutional actors see rope (coordination, low extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the classification depends on the observer's temporal position and relationship to the risk-benefit distribution. The first generation's snare classification is their structural reality — they are trapped and bear disproportionate risk. The institutional rope classification is also structurally real — the coordination function (disease prevention) is genuine. The scaffold classification from bioethics review is real if the sunset logic holds (safety data will accumulate and equalize risk). The analytical rope classification is real if alternatives are not being suppressed. No single type is 'correct' — the constraint IS the temporal asymmetry, and different observers occupy different positions in that asymmetry. The critical omega is whether somatic alternatives are being suppressed — if yes, the coordination story is cover and the constraint shifts toward snare from more perspectives. If no, the rope classification holds and the asymmetry is a necessary coordination cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    somatic_alternative_suppression,
    'Are somatic gene therapy alternatives being systematically underinvested relative to their potential, creating artificial pressure toward germline modification?',
    'Comparative funding analysis: germline vs somatic research investment; technical feasibility assessments for somatic alternatives; regulatory pathway comparison',
    'If somatic alternatives are viable but suppressed: the coordination story is cover and the constraint reclassifies toward snare from more perspectives. If somatic alternatives are genuinely insufficient: the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(somatic_alternative_suppression, empirical, 'Whether somatic therapy alternatives are being suppressed').

omega_variable(
    risk_magnitude_threshold,
    'What level of first-generation risk is acceptable for preventing transmission of genetic disease? Is the current risk-benefit ratio within that threshold?',
    'Comparative risk analysis: germline modification safety data vs disease burden; ethical frameworks for acceptable research risk; long-term follow-up studies',
    'If current risk exceeds threshold: first-generation perspective shifts from constrained victim to trapped victim, and extractiveness increases. If risk is within threshold: rope classification from more perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(risk_magnitude_threshold, preference, 'Acceptable risk threshold for first-generation subjects').

omega_variable(
    consent_framework_adequacy,
    'Can prospective-parent consent adequately represent the interests of the first modified generation, or does the identity-lock (the child cannot exit the modification) create an irreducible consent gap?',
    'Philosophical analysis of proxy consent limits; empirical data on first-generation subjects'' retrospective attitudes; comparison with other irreversible parental decisions',
    'If consent gap is irreducible: the powerless/trapped perspective is structural rather than contingent, and the constraint cannot be fully coordinated. If proxy consent is adequate: rope classification holds from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_framework_adequacy, conceptual, 'Whether proxy consent can bridge the generational gap').

omega_variable(
    benefit_distribution_timeline,
    'How many generations must pass before the cumulative benefit (disease elimination) outweighs the first-generation risk? Is the timeline short enough to justify the asymmetry?',
    'Quantitative modeling: disease prevalence reduction over time; risk amortization across generations; discount rates for future health benefits',
    'If timeline is short (2-3 generations): rope classification justified. If timeline is long (10+ generations): the asymmetry looks more like extraction from the first generation to subsidize distant descendants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_distribution_timeline, empirical, 'Timeline for benefit distribution to justify first-generation risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intergenerational_justice_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intgen_theater_t0, intergenerational_justice_asymmetry, theater_ratio, 0, 0.25).
narrative_ontology:measurement(intgen_theater_t3, intergenerational_justice_asymmetry, theater_ratio, 3, 0.35).
narrative_ontology:measurement(intgen_theater_t6, intergenerational_justice_asymmetry, theater_ratio, 6, 0.42).
narrative_ontology:measurement(intgen_theater_t10, intergenerational_justice_asymmetry, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(intgen_extract_t0, intergenerational_justice_asymmetry, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(intgen_extract_t3, intergenerational_justice_asymmetry, base_extractiveness, 3, 0.22).
narrative_ontology:measurement(intgen_extract_t6, intergenerational_justice_asymmetry, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(intgen_extract_t10, intergenerational_justice_asymmetry, base_extractiveness, 10, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(intgen_suppress_t0, intergenerational_justice_asymmetry, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(intgen_suppress_t3, intergenerational_justice_asymmetry, suppression_requirement, 3, 0.28).
narrative_ontology:measurement(intgen_suppress_t6, intergenerational_justice_asymmetry, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(intgen_suppress_t10, intergenerational_justice_asymmetry, suppression_requirement, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intergenerational_justice_asymmetry, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of safety_risk_structure (the physical/biological risk profile of germline modification) but represents a distinct structural phenomenon: the temporal distribution of that risk across generations. The upstream constraint (safety_risk_structure) is a mountain — the biological risk is what it is. This constraint (intergenerational_justice_asymmetry) is the coordination/extraction mechanism that distributes that risk asymmetrically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
