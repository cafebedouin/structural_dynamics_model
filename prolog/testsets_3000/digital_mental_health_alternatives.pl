% ============================================================================
% CONSTRAINT STORY: digital_mental_health_alternatives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_mental_health_alternatives, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_mental_health_alternatives
 *   human_readable: Digital Mental Health Alternatives as Substitutes for Professional Care
 *   domain: healthcare/mental_health/digital_innovation
 *
 * SUMMARY:
 *   Digital mental health platforms position themselves as solutions to
 *   therapist shortage and geographic access barriers. This constraint
 *   operates at the intersection of genuine healthcare need, technological
 *   scaling, venture capital incentives, and regulatory asymmetry. The
 *   platforms deliver both real coordination functions (24/7 availability,
 *   cost reduction, extended reach) and asymmetric extraction (data
 *   harvesting, algorithmic behavioral control, information asymmetry about
 *   clinical equivalence). Users at different economic and epistemic
 *   positions experience the constraint differently: wealthy insured users
 *   benefit from supplementary digital tools; underserved populations risk
 *   substituting algorithmic chat for human clinical care; mental health
 *   professionals face erosion of autonomy as platforms become
 *   standard-of-care pathways; regulators lack oversight capacity for
 *   algorithmic clinical decision-making. The constraint's extractiveness has
 *   increased over the measurement interval as platforms have shifted from
 *   supplementary tools to claimed replacements for professional care, and
 *   theater has remained moderate (evidence marketing is present but
 *   regulated somewhat by healthcare claims standards).
 *
 * KEY AGENTS:
 *   - Digital Health Platforms: Primary beneficiary (institutional/arbitrage) — captures venture funding, user engagement data, network effects; positions as solving access gap while monetizing user behavior
 *   - Severely Mentally Ill Without Professional Access: Primary victim (powerless/trapped) — geographic and economic barriers to human care; lacks epistemic resources to evaluate algorithmic substitute; highest extraction experience
 *   - Insured Middle-Class Users: Secondary beneficiary/victim (moderate/constrained) — genuine access to human therapy but uses app for convenience; experiences both coordination benefit and extraction through data harvesting
 *   - Clinical Psychology Profession: Mixed actor (moderate/constrained) — benefits from demand reduction and triage support; harmed by erosion of autonomy and protocol-driven practice
 *   - Low-Income Populations: Primary victim (powerless/constrained) — positioned as 'solution' to access gap but lack resources to evaluate clinical validity; vulnerable to identity lock around platform
 *   - Health Regulators and Standard-Setting Bodies: Organized agents (organized/constrained) — building evidence standards and licensing frameworks that could reduce extraction if enforcement succeeds; currently theatric (regulations written for human therapy, applied to algorithmic systems)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating therapeutic scarcity as natural law rather than contingent institutional architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_mental_health_alternatives, 0.58).
domain_priors:suppression_score(digital_mental_health_alternatives, 0.65).
domain_priors:theater_ratio(digital_mental_health_alternatives, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_mental_health_alternatives, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_mental_health_alternatives, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(digital_mental_health_alternatives, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_mental_health_alternatives, tangled_rope).
narrative_ontology:human_readable(digital_mental_health_alternatives, "Digital Mental Health Alternatives as Substitutes for Professional Care").
narrative_ontology:topic_domain(digital_mental_health_alternatives, "healthcare/mental_health/digital_innovation").

domain_priors:requires_active_enforcement(digital_mental_health_alternatives).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_mental_health_alternatives, digital_health_platforms).
narrative_ontology:constraint_beneficiary(digital_mental_health_alternatives, technology_investors).
narrative_ontology:constraint_beneficiary(digital_mental_health_alternatives, algorithmic_content_providers).
narrative_ontology:constraint_victim(digital_mental_health_alternatives, users_with_severe_mental_illness).
narrative_ontology:constraint_victim(digital_mental_health_alternatives, low_income_populations).
narrative_ontology:constraint_victim(digital_mental_health_alternatives, clinical_psychology_profession).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEVERELY MENTALLY ILL WITHOUT ACCESS (SNARE) — Users trapped in regions/economic strata with no licensed therapist access. Digital platforms position themselves as 'solutions' but deliver algorithmic conversation and behavior tracking rather than therapeutic relationship. No exit: barriers are geographic, economic, and epistemic (belief that digital is equivalent to professional). Maximum suppression through information asymmetry — users cannot evaluate whether an app is therapeutic or extractive.
constraint_indexing:constraint_classification(digital_mental_health_alternatives, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSURED MIDDLE-CLASS USER (TANGLED ROPE) — Has insurance access to human therapists but uses app to supplement (genuine coordination benefit: extended availability, reduced session cost). Also experiences extraction: data harvesting, behavioral prediction, algorithmic nudging toward platform engagement rather than clinical outcomes. Mixed: real coordination function (availability) plus asymmetric information and behavioral targeting.
constraint_indexing:constraint_classification(digital_mental_health_alternatives, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DIGITAL HEALTH PLATFORM (ROPE) — Genuine coordination function: extends mental health reach to underserved regions, provides 24/7 availability, scales human clinical labor through triage and protocol-guided interventions. Net beneficiary through data monetization, user engagement metrics, and venture funding. Experiences the constraint as enabling coordination, not extraction — the platform solves a real problem (access gap) while capturing value.
constraint_indexing:constraint_classification(digital_mental_health_alternatives, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLINICAL PSYCHOLOGY PROFESSION (TANGLED ROPE) — Faces genuine supply shortage (therapist shortage is real, particularly in rural areas). Digital alternatives reduce demand pressure and can provide triage/diagnostic support, enabling human therapists to serve more patients. But also experiences extraction: therapist labor is increasingly structured around platform protocols and algorithmic recommendations; professional autonomy declines as apps become standard-of-care referral pathway.
constraint_indexing:constraint_classification(digital_mental_health_alternatives, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATION AND STANDARD-SETTING COALITION (SCAFFOLD) — Organized actors (medical boards, health authorities, digital-health advocacy) are establishing evidence standards, licensing requirements, and data protection regulations that could reduce extractiveness of digital platforms. If regulatory maturation succeeds (sunset clause: 10-15 years), platforms will be forced toward genuine clinical validation rather than engagement optimization. Current theater is high (marketing claims about efficacy); regulation aims to replace theater with measurement.
constraint_indexing:constraint_classification(digital_mental_health_alternatives, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TELEHEALTH LICENSING FRAMEWORK (PITON) — State-by-state licensing reciprocity, asynchronous care regulations, and reimbursement parity rules were designed to enable remote human therapy delivery. But as digital apps proliferate, these frameworks increasingly protect performative compliance rather than clinical outcomes. Platforms claim 'licensed oversight' while operating algorithmic systems licensed professionals didn't design and can't audit. Theater ratio high: regulatory theater that masks algorithmic opacity.
constraint_indexing:constraint_classification(digital_mental_health_alternatives, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, some form of scalability constraint is inherent to therapeutic relationship: one-to-one clinical care cannot scale infinitely to meet global mental health need. The substitution of digital tools for human connection is thus inevitable and unchangeable. However, structural data reveals this as false summit: the constraint is not natural law but engineered scarcity (therapist training bottleneck, insurance reimbursement structures, geographic distribution) and strategic substitution (platforms positioning algorithmic tools as equivalent to human care). The naturalization conceals contingent institutional choices.
constraint_indexing:constraint_classification(digital_mental_health_alternatives, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_mental_health_alternatives_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_mental_health_alternatives, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_mental_health_alternatives, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_mental_health_alternatives, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_mental_health_alternatives, TR),
    TR >= 0.70.

:- end_tests(digital_mental_health_alternatives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Initial value (0.32) reflected genuine coordination function — platforms filled access gap for underserved populations. Current value (0.58) reflects shift toward claimed therapeutic equivalence without clinical validation, data monetization growth, and behavioral targeting. Platforms have moved from supplements to substitutes. Suppression (0.65): High. Multiple mechanisms: (1) geographic/economic barriers to alternative care; (2) information asymmetry about clinical efficacy; (3) algorithmic opacity — users cannot audit whether recommendations are evidence-based or engagement-optimized; (4) marketing claims positioning algorithmic tools as therapeutic equivalent without adequate disclosure of limitations. Theater ratio (0.48): Moderate. Marketing theater is present (efficacy claims) but constrained by healthcare advertising standards. Theater is lower than pure pitons because platforms deliver genuine functionality (they do provide mental health information, behavioral tracking, crisis support). Theater will increase if regulatory oversight remains weak (platforms will move toward pure marketing); will decrease if evidence standards are enforced (platforms forced to validate claims).
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is approximately 2.5 classification types: platform sees Rope (pure coordination), trapped user sees Snare (pure extraction). This gap reveals the constraint's dual nature — genuinely addresses coordination problem (therapist shortage) while structurally enabling asymmetric extraction (data monetization, algorithmic control, information asymmetry). The Tangled Rope classification is the analytical resolution: the constraint is both coordination AND extraction, the coordination is genuine, and the extraction is real.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform beneficiaries experience low d (beneficiary + arbitrage exit = ~0.10-0.20). Trapped users experience high d (victim + trapped exit = ~0.95). Insured users experience moderate d (mixed benefit + constrained exit = ~0.50). Psychology profession experiences moderate-high d (partial victim + constrained exit = ~0.65) — constrained because exiting clinical practice entirely is costly, but some agency exists (advocacy, regulatory engagement, professional societies). Directionality overrides: none needed; structural derivation captures asymmetries adequately. The high chi values for powerless agents derive from high d (trapped exit) combined with moderate base extractiveness, producing chi ≈ 0.58 × 1.42 × 1.0 = 0.82 at powerless/trapped context.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY CANDIDATE: Digital mental health alternatives decompose into multiple structurally distinct constraints: (1) Therapist shortage coordination (ε ≈ 0.15, Rope) — genuine supply-demand gap; (2) Platform data monetization (ε ≈ 0.65, Snare) — algorithmic behavioral extraction; (3) Clinical validation standards (ε ≈ 0.42, Scaffold with sunset clause) — regulatory pathway to align platforms with outcomes. The parent constraint (digital_mental_health_alternatives) is the integration of all three. At ε=0.58, mandatrophy requires demonstrating why this is Tangled Rope not Snare: the genuine coordination function (extending access, reducing wait times, enabling triage) prevents classification as pure extraction. But the extraction component is significant (data harvesting, behavioral optimization, information asymmetry), preventing pure Rope classification. The mandatrophy resolves by showing that the constraint cannot be collapsed into either pure type — the beneficiary's Rope and the victim's Snare are both empirically grounded in the same institutional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    therapeutic_efficacy_measurement,
    'Are outcome measures for digital mental health platforms measuring clinical improvement or platform engagement metrics?',
    'Randomized controlled trials comparing digital-only vs human therapy vs combined for specific diagnoses (depression, anxiety, PTSD). Long-term follow-up (12+ months post-intervention) to measure relapse and symptom persistence.',
    'If digital-only equals human therapy: platform classification moves toward Rope (pure coordination) across all perspectives. If digital-only underperforms: classification stays Snare/Tangled Rope; extraction mechanism confirmed. If hybrid is optimal: Scaffold classification confirmed (digital as complement, not substitute).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(therapeutic_efficacy_measurement, empirical, 'Whether digital mental health efficacy equals or exceeds human therapy').

omega_variable(
    data_monetization_asymmetry,
    'What proportion of user behavioral data collected by mental health apps is monetized or sold to third parties, and do users comprehend this?',
    'Audit of platform privacy policies; analysis of advertising networks and data broker relationships; user comprehension studies (do users believe their therapeutic data is proprietary or shared?).',
    'If data monetization is extensive and unknown to users: suppression mechanism confirmed, extraction component elevated. If transparent and minimal: extraction mechanism reduces, classification shifts toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_monetization_asymmetry, empirical, 'Extent of data monetization in digital mental health platforms').

omega_variable(
    algorithmic_clinical_validity,
    'Are diagnostic and treatment recommendations from digital mental health algorithms based on clinical evidence or on engagement optimization?',
    'Reverse-engineering of recommendation algorithms; comparison of recommendation patterns to clinical guidelines; user feedback analysis tracking whether recommendations align with professional therapist guidance.',
    'If evidence-based: tangled rope classification confirmed (genuine coordination + some extraction). If engagement-optimized: snare classification elevated (recommendations designed to increase app usage, not clinical outcomes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_clinical_validity, empirical, 'Whether algorithms prioritize clinical validity or engagement').

omega_variable(
    access_gap_substitution_ratio,
    'For underserved populations, does digital mental health reduce unmet mental health need or substitute for professional care that would otherwise be sought?',
    'Cohort analysis comparing digital-app users in underserved areas to (a) wait-listed users without app access and (b) users with human therapist access. Measure: symptom improvement, care-seeking trajectory, outcomes by initial severity.',
    'If digital fills access gap: Rope/Scaffold perspectives confirmed (coordination without displacement). If substitutes for professional care users would seek: Snare perspective confirmed (users settle for lower-efficacy alternative due to framing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_gap_substitution_ratio, empirical, 'Whether digital platforms fill access gap or substitute for professional care').

omega_variable(
    identity_lock_mechanism,
    'Do users develop identity lock around digital mental health apps, perceiving them as equivalent to human therapy and resisting professional referral?',
    'Qualitative interviews with long-term digital app users; analysis of user forums and support communities; referral acceptance rates when clinical indicators suggest human care needed.',
    'If identity lock present: exit_options shift from constrained/mobile to identity_locked; suppression mechanism operates cognitively as well as materially. Classification intensifies toward Snare for powerless users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether users develop identity lock around digital platforms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_mental_health_alternatives, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmha_tr_t0, digital_mental_health_alternatives, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dmha_tr_t3, digital_mental_health_alternatives, theater_ratio, 3, 0.42).
narrative_ontology:measurement(dmha_tr_t6, digital_mental_health_alternatives, theater_ratio, 6, 0.48).
narrative_ontology:measurement(dmha_tr_t9, digital_mental_health_alternatives, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(dmha_be_t0, digital_mental_health_alternatives, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dmha_be_t3, digital_mental_health_alternatives, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(dmha_be_t6, digital_mental_health_alternatives, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(dmha_be_t9, digital_mental_health_alternatives, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_mental_health_alternatives, resource_allocation).
narrative_ontology:affects_constraint(digital_mental_health_alternatives, therapist_labor_shortage).
narrative_ontology:affects_constraint(digital_mental_health_alternatives, mental_health_data_monetization).
narrative_ontology:affects_constraint(digital_mental_health_alternatives, clinical_validation_standards).

% DUAL FORMULATION NOTE:
% Digital mental health alternatives decompose into a constraint family. Parent constraint: digital_mental_health_alternatives (ε=0.58, Tangled Rope). Upstream: therapist_labor_shortage (ε ≈ 0.15, Rope) — the coordination problem digital platforms address. Parallel: mental_health_data_monetization (ε ≈ 0.65, Snare) — the extraction mechanism embedded in platform business models. Downstream: clinical_validation_standards (ε ≈ 0.42, Scaffold) — regulatory pathway with sunset clause that could reduce parent constraint extractiveness. Each story has its own metrics and perspectives; the parent story integrates their structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
