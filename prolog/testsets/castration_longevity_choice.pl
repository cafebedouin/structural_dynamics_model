% ============================================================================
% CONSTRAINT STORY: castration_longevity_choice
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_castration_longevity_choice, []).

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
 *   constraint_id: castration_longevity_choice
 *   human_readable: The Castration-Longevity Trade-off
 *   domain: biological/technological/social
 *
 * SUMMARY:
 *   The castration-longevity trade-off presents a constraint that extracts
 *   reproductive autonomy and bodily integrity as the price of access to
 *   potential lifespan extension. The constraint structure is historically
 *   rooted in reproductive medicine's gatekeeping authority and is maintained
 *   by asymmetric information about alternatives, unproven longevity gains,
 *   and suppression of non-castrative pathways. From the perspective of a
 *   male subject facing the choice, this is a snare: both reproductive
 *   sacrifice and the alternative (accepting mortality) result in permanent
 *   loss. From the perspective of longevity researchers, it is a coordination
 *   mechanism: mobilizing research teams and subject populations around a
 *   tractable biological intervention. From historical and institutional
 *   perspectives, it is a piton: a degraded apparatus of reproductive
 *   medicine authority persisting through inertia. The constraint exhibits
 *   the core feature of tangled rope: it offers genuine coordination function
 *   (access to longevity research, potential lifespan gain) while
 *   simultaneously extracting reproductive autonomy and bodily integrity from
 *   vulnerable populations. The theater ratio is rising (0.42 to 0.58 over
 *   the interval) as performative research ethics and normative medical
 *   authority increasingly prop up a mechanism whose functional justification
 *   is weak. The constraint is vulnerable to scaffold dynamics: emerging
 *   technologies (gamete preservation, selective hormone interventions,
 *   reproductive alternatives) create exit pathways that reduce extraction
 *   while maintaining longevity benefit. Reproductive autonomy advocacy
 *   movements are actively building alternative frames and research designs
 *   that decouple longevity science from reproductive sacrifice.
 *
 * KEY AGENTS:
 *   - Male Subject Facing Choice: Primary victim (powerless/trapped) — bears irreversible loss of reproductive function and bodily integrity; no genuine exit option within the constraint
 *   - Longevity Researchers: Primary beneficiary (institutional/arbitrage) — gain publications, funding, career advancement from castration-longevity research paradigm
 *   - Patient-Participants: Secondary victim (moderate/constrained) — face medical gatekeeping and informed consent asymmetry; gain potential longevity but lose reproductive autonomy
 *   - Historical/Gender Medicine Apparatus: Institutional beneficiary (institutional/arbitrage) — maintains reproductive control authority through castration-centric framing; benefits from theater of medical expertise
 *   - Reproductive Autonomy Advocates: Organized agents (organized/constrained) — working to build alternative research frames and sunset the castration paradigm through norm change
 *   - Eunuch Communities: Complex agent (organized/constrained) — historically and presently experience castration as both extraction and community coordination; bring lived expertise to constraint analysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(castration_longevity_choice, 0.38).
domain_priors:suppression_score(castration_longevity_choice, 0.62).
domain_priors:theater_ratio(castration_longevity_choice, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(castration_longevity_choice, extractiveness, 0.38).
narrative_ontology:constraint_metric(castration_longevity_choice, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(castration_longevity_choice, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(castration_longevity_choice, tangled_rope).
narrative_ontology:human_readable(castration_longevity_choice, "The Castration-Longevity Trade-off").
narrative_ontology:topic_domain(castration_longevity_choice, "biological/technological/social").

domain_priors:requires_active_enforcement(castration_longevity_choice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(castration_longevity_choice, longevity_medicine_researchers).
narrative_ontology:constraint_beneficiary(castration_longevity_choice, gerontology_funding_institutions).
narrative_ontology:constraint_victim(castration_longevity_choice, male_reproductive_autonomy).
narrative_ontology:constraint_victim(castration_longevity_choice, gender_identity_integrity).
narrative_ontology:constraint_victim(castration_longevity_choice, bodily_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MALE SUBJECT FACING LONGEVITY CHOICE (SNARE) — An individual man cannot exit the constraint: choosing castration for longevity gain sacrifices reproductive and sexual function irreversibly. Choosing against castration sacrifices potential longevity. Both options result in permanent loss. The constraint extracts reproductive autonomy and bodily integrity as the price of the longevity question itself. Maximum experienced extraction because the choice is framed as binary with no neutral option.
constraint_indexing:constraint_classification(castration_longevity_choice, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PATIENT-PARTICIPANT IN LONGEVITY TRIALS (TANGLED ROPE) — Constrained by limited trial availability, medical gatekeeping, and informed consent asymmetry. Benefits from access to experimental longevity protocols and potential lifespan extension. Mixed extraction: participation offers genuine longevity benefit but requires acceptance of reproductive loss and subordination to researcher authority. Moderate extraction with some coordination function.
constraint_indexing:constraint_classification(castration_longevity_choice, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LONGEVITY MEDICINE RESEARCHERS (ROPE) — Institutional actors benefit from the constraint as a research paradigm: castration as hormonal intervention offers measurable longevity outcomes, publishable data, patent opportunities, and funding flows. Experiences the constraint as coordination: mobilizing research teams, recruiting subjects, establishing biomarkers. Net beneficiary with low experienced extraction because the constraint aligns researcher incentives with institutional recognition.
constraint_indexing:constraint_classification(castration_longevity_choice, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HISTORICAL GENDER/REPRODUCTIVE MEDICINE APPARATUS (PITON) — The medical establishment's historical control over reproductive choices (forced sterilization, eugenics, reproductive medicine gatekeeping) persists as theatrical institutional authority even as its functional justification has eroded. The castration-longevity frame reactivates this degraded institutional power. Theater ratio high because much of the medical authority deployed around this choice is performative (normative appeals, expert gatekeeping) rather than functional. Institutional inertia maintains the apparatus despite reduced actual legitimacy.
constraint_indexing:constraint_classification(castration_longevity_choice, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REPRODUCTIVE AUTONOMY ADVOCACY MOVEMENT (SCAFFOLD) — Organized agents (reproductive rights organizations, disability justice communities, gender advocates) recognize the constraint as a temporary extractive frame that can be replaced by better alternatives: decoupling longevity research from reproductive sacrifice, developing non-castrative hormonal interventions, centering bodily autonomy in research ethics. See the sunset as achievable through norm shifts in research design and informed consent practice. Low effective extraction because advocacy creates exit pathways and the constraint's legitimacy is increasingly questioned.
constraint_indexing:constraint_classification(castration_longevity_choice, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EUNUCH COMMUNITIES AND HISTORICAL PRACTITIONERS (TANGLED ROPE) — Communities with lived experience of castration (historical eunuchs, voluntary castration practitioners, intersex/trans communities) experience this constraint as both coordination and extraction. The constraint extracts social/sexual recognition and reproductive function but offers alternative longevity potential and community belonging. Complex trade-off with asymmetric costs: those without prior reproductive stakes experience lower extraction than those losing it.
constraint_indexing:constraint_classification(castration_longevity_choice, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / BIOLOGICAL NATURALISM VIEW (MOUNTAIN) — From a civilizational/universal perspective, testosterone-driven reproductive trade-offs are framed as immutable biological law: sexual dimorphism, reproductive senescence, and the fundamental conflict between somatic maintenance and reproductive effort are constants of mammalian physiology. This perspective naturalizes the castration-longevity trade-off as inherent to biology itself. However, the structural data reveals false summit risk: this is not a law of physics but a contingent biological arrangement subject to technological intervention (hormone blocking, gonadal preservation, synthetic hormone substitution). The mountain classification naturalizes what may be technologically malleable.
constraint_indexing:constraint_classification(castration_longevity_choice, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(castration_longevity_choice_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(castration_longevity_choice, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(castration_longevity_choice, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(castration_longevity_choice, TR),
    TR >= 0.70.

:- end_tests(castration_longevity_choice_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts reproductive autonomy and bodily integrity from male subjects, but the extraction is partially offset by potential longevity gain and research access. The value reflects that this is not pure extraction (some coordination and benefit exist) but significant extraction nonetheless. Suppression (0.62): High. Barriers to reproductive autonomy include: irreversibility of castration, incomplete information on longevity gains, medical gatekeeping of reproductive decision-making, social stigma around reproductive modification, suppression of alternative pathways (selective hormone interventions), and institutional momentum of reproductive medicine authority. Theater ratio (0.58): Moderate-high. Much of the constraint's legitimacy rests on performative elements: medical expertise appeal, normative authority of longevity science, theatrical informed consent protocols, and appeal to natural biological law. The rising trajectory (0.42 to 0.58) reflects degradation of functional justification — as alternatives emerge and longevity gains prove modest, the apparatus increasingly relies on institutional theater rather than evidence.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perspectival disagreement on all dimensions. The male subject sees a snare (forced binary choice with permanent loss). The researcher sees a rope (solving a research coordination problem). The medical establishment sees a piton (performative exercise of authority). The patient-participant sees tangled rope (mixed coordination and extraction). The reproductive autonomy movement sees a scaffold (temporary problem solvable through alternatives). The eunuch community sees a tangled rope (extraction offset by community coordination). The analytical naturalism view sees a mountain (immutable biological law) — but structural analysis reveals this as a false summit. The perspectival gap is maximal: no two perspectives agree on type, and the mountain classification naturalizes what is actually a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is determined by each agent's structural position relative to the longevity-reproductive extraction flow. Male subjects forced to choose experience maximum extraction (d ≈ 0.95, f(d) ≈ 1.42) because they are victims with no exit: castration removes reproductive function irreversibly, and choosing against castration accepts mortality. Longevity researchers experience negative extraction (d ≈ 0.05, f(d) ≈ -0.12) because they are beneficiaries with arbitrage options: they can pivot to alternative research, obtain publication regardless of outcomes, and maintain career trajectories. Patient-participants in trials experience moderate extraction (d ≈ 0.55, f(d) ≈ 0.75) because they are constrained victims with some agency: they face medical gatekeeping and consent asymmetry but can refuse participation (unlike subjects psychologically pressured by longevity promise). The historical medicine apparatus experiences low or negative extraction (d ≈ 0.10, f(d) ≈ -0.05) because it is a beneficiary institution maintaining authority and legitimacy through the constraint. Reproductive advocates experience moderate extraction (d ≈ 0.45, f(d) ≈ 0.52) because they are organized agents fighting an entrenched constraint: they have coalition power and exit pathways (alternative research, norm change) but face institutional resistance. The piton classification of the historical medicine apparatus derives from theater ratio gate, not from high experienced extraction — the apparatus extracts less in absolute terms than it did historically, but maintains presence through institutional inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT STRUCTURE RESOLVES MANDATROPHY: The castration-longevity constraint avoids both false-natural-law and false-coordination errors through explicit mandatrophy analysis. (1) FALSE NATURAL LAW RISK: The analytical observer's mountain classification ('testosterone-driven senescence is immutable biology') is revealed as a false summit by the structural data: the constraint requires active enforcement (medical gatekeeping, informed consent suppression), benefits identifiable agents (researchers), and suppresses alternatives (selective hormone interventions, reproductive substitution). These are markers of social structure, not physical law. The mountain framing naturalizes what is technologically malleable. (2) FALSE COORDINATION RISK: The researcher's rope classification ('mobilizing research teams around a tractable intervention') obscures the extraction of reproductive autonomy from subjects. The tangled rope classification corrects this by acknowledging that the constraint offers genuine coordination function (research access, potential longevity benefit) AND asymmetric extraction (reproductive loss, bodily integrity). The mandatrophy is resolved by: (a) declaring beneficiaries (longevity researchers) and victims (male reproductive autonomy) explicitly, (b) requiring active enforcement to establish asymmetry, (c) measuring theater ratio to detect performative maintenance, and (d) including perspectives from both beneficiaries and victims to expose the perspectival gap. The classification avoids both naturalizing the constraint as inevitable biology and falsely celebrating it as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    testosterone_longevity_causation,
    'Does testosterone directly cause lifespan reduction, or is the correlation mediated by behavior, risk-taking, or confounding biological factors?',
    'Randomized hormone supplementation trials in castrated populations; longitudinal tracking of endogenous testosterone variation vs mortality in non-intervention populations; pathway analysis of immune function, cardiovascular stress, and cellular senescence',
    'If direct causal: castration-longevity trade-off is real and consequential. If mediated: alternative interventions (behavioral modification, selective hormone blocking, immune support) may achieve longevity without reproductive sacrifice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(testosterone_longevity_causation, empirical, 'Whether testosterone directly causes lifespan reduction or acts through confounders').

omega_variable(
    longevity_gain_magnitude,
    'What is the actual longevity gain from castration in humans? Historical records show 5-10 year gains in eunuch populations; is this replicable in modern cohorts and under controlled conditions?',
    'Comparative longevity analysis of voluntary castration practitioners vs matched controls; analysis of historical eunuch mortality records; prospective studies of castrated individuals in modern medical contexts',
    'If gain > 10 years and reproducible: trade-off is attractive to some agents and extraction is morally ambiguous. If gain < 2 years: extraction mechanism is barely offset by benefit, constraint classifies as pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(longevity_gain_magnitude, empirical, 'Magnitude of actual lifespan extension from human castration').

omega_variable(
    informed_consent_feasibility,
    'Can genuine informed consent be achieved for castration given the irreversibility, psychological impact, and social consequences? Or does the constraint structure inherently suppress true consent?',
    'Longitudinal psychological assessment of castration decision-makers; analysis of regret rates and cognitive frames used in consent discussions; audit of consent protocols in longevity research',
    'If genuine consent achievable: tangled rope classification holds; agent agency is real. If inherent suppression: classification becomes snare for all; constraint structure itself violates autonomy conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_feasibility, conceptual, 'Whether informed consent is achievable for irreversible castration decision').

omega_variable(
    alternative_hormonal_pathways,
    'Can selective hormone blocking (DHT inhibitors, selective androgen receptor modulators, GnRH agonists) achieve longevity gains without full castration? Are these alternatives being actively researched or suppressed by the castration-centric framing?',
    'Literature review of hormone-selective intervention trials; comparison of research funding patterns for castration vs alternatives; mechanistic analysis of which testosterone pathways drive senescence',
    'If effective alternatives exist but are de-emphasized: constraint structure is extractive (suppresses alternatives). If castration is genuinely optimal: constraint structure is coordinative (offers best option). If alternatives are superior: castration framing is theater (performative choice masking better pathways).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_hormonal_pathways, empirical, 'Whether selective hormone interventions offer comparable longevity without castration').

omega_variable(
    reproductive_substitution_viability,
    'Do emerging technologies (gamete preservation, assisted reproduction, artificial gamete production) make castration-for-longevity less extractive by decoupling longevity from reproductive sacrifice?',
    'Assessment of gamete cryopreservation success rates in pre-castration preservation; viability of assisted reproduction from preserved gametes; timeline and cost of emerging artificial gamete technology',
    'If reproductive substitution is viable and accessible: extraction of bodily integrity remains but reproductive autonomy loss is reversible, reducing snare classification. Constraint transitions toward scaffold with sunset (reproductive technology catches up).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reproductive_substitution_viability, empirical, 'Whether reproductive technology can substitute for castration-lost biological reproduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(castration_longevity_choice, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cast_long_tr_t0, castration_longevity_choice, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cast_long_tr_t30, castration_longevity_choice, theater_ratio, 30, 0.5).
narrative_ontology:measurement(cast_long_tr_t60, castration_longevity_choice, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(cast_long_be_t0, castration_longevity_choice, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cast_long_be_t30, castration_longevity_choice, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(cast_long_be_t60, castration_longevity_choice, base_extractiveness, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(castration_longevity_choice, resource_allocation).
narrative_ontology:affects_constraint(castration_longevity_choice, reproductive_medicine_gatekeeping).
narrative_ontology:affects_constraint(castration_longevity_choice, longevity_research_prioritization).
narrative_ontology:affects_constraint(castration_longevity_choice, informed_consent_asymmetry).

% DUAL FORMULATION NOTE:
% The castration-longevity trade-off decomposes into three structurally distinct constraints: (1) reproductive_medicine_gatekeeping (ε ≈ 0.35, institutional control over reproductive choices) — upstream constraint that enables castration as a medical option; (2) longevity_research_prioritization (ε ≈ 0.45, institutional bias toward castration pathways over alternatives) — constraint that suppresses research into selective hormonal interventions; (3) informed_consent_asymmetry (ε ≈ 0.40, psychological/informational barriers to genuine consent for irreversible procedures) — constraint that reduces subject agency in choice. The castration_longevity_choice constraint depends on all three: it cannot function without medical authority to authorize castration, institutional bias against alternatives, and consent suppression. This is a network family where the focal constraint (castration_longevity_choice) is downstream of three institutional constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(castration_longevity_choice, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
