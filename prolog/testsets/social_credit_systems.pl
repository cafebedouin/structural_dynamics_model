% ============================================================================
% CONSTRAINT STORY: social_credit_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_credit_systems, []).

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
 *   constraint_id: social_credit_systems
 *   human_readable: Social Credit Systems as Extractive Surveillance and Behavioral Control
 *   domain: governance/surveillance/social_control
 *
 * SUMMARY:
 *   Social credit systems represent a structural constraint in which
 *   centralized rating of citizen behavior drives cascading access
 *   restrictions across employment, finance, housing, education, and transit.
 *   The constraint exhibits multiple classification types from different
 *   structural positions: pure extraction (snare) from the perspective of
 *   rated individuals who cannot exit; coordination mechanism (rope) from the
 *   state apparatus that uses the system for behavioral prediction; hybrid
 *   extraction-coordination (tangled rope) from corporate operators who
 *   profit from data monopolies while providing genuine information
 *   aggregation; and institutional inertia (piton) when viewed as the
 *   digitization of legacy surveillance structures (credit scoring,
 *   watchlists, background checks). The extractiveness has increased over the
 *   measurement interval (0.35 → 0.68) as the system has expanded from
 *   financial risk assessment to universal behavioral scoring, and as the
 *   theater ratio has increased (0.32 → 0.62) reflecting growth in procedural
 *   legitimacy claims (appeals boards, transparency reports) that obscure
 *   unchanged underlying extraction mechanisms. The constraint is
 *   mandatrophy-resolved by recognizing that different institutional actors
 *   genuinely experience different constraint types: the state experiences
 *   coordination (rope), the operator experiences hybrid (tangled rope), and
 *   the citizen experiences pure extraction (snare). The mandatrophy is not a
 *   misclassification but a perspectival fact — the constraint IS different
 *   things to different agents.
 *
 * KEY AGENTS:
 *   - Rated Individuals: Primary victims (powerless/trapped) — unable to exit system; targeted for behavioral compliance through score-based access restriction
 *   - State Administration: Primary beneficiary (institutional/arbitrage) — uses system for centralized behavioral prediction and compliance optimization; experiences constraint as coordination
 *   - Corporate Rating Operators: Secondary beneficiary (powerful/constrained) — monopolize data infrastructure and behavioral prediction IP; extract switching costs and government-enforced compliance
 *   - Social Autonomy Commons: Collective victim (powerless/trapped) — abstract good of autonomous behavior, privacy, and trust networks is depleted; cannot organize or exit
 *   - Privacy Rights Movement: Organized opposition (organized/mobile) — frames constraint as temporary with sunset via generational pressure and legal challenge; has agency and exit options
 *   - Institutional Legacy of Surveillance: Piton persistence mechanism (institutional/arbitrage) — system maintained through procedural theater and regulatory compliance rituals despite low functional verification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_credit_systems, 0.68).
domain_priors:suppression_score(social_credit_systems, 0.72).
domain_priors:theater_ratio(social_credit_systems, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_credit_systems, extractiveness, 0.68).
narrative_ontology:constraint_metric(social_credit_systems, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(social_credit_systems, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_credit_systems, snare).
narrative_ontology:human_readable(social_credit_systems, "Social Credit Systems as Extractive Surveillance and Behavioral Control").
narrative_ontology:topic_domain(social_credit_systems, "governance/surveillance/social_control").

domain_priors:requires_active_enforcement(social_credit_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_credit_systems, state_apparatus).
narrative_ontology:constraint_beneficiary(social_credit_systems, corporate_rating_operators).
narrative_ontology:constraint_victim(social_credit_systems, rated_individuals).
narrative_ontology:constraint_victim(social_credit_systems, social_autonomy).
narrative_ontology:constraint_victim(social_credit_systems, privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE RATED INDIVIDUAL (SNARE) — Citizens subject to social credit scoring cannot exit the system. Score affects employment, education, housing, transit, credit access, and social visibility. Trapped: exit requires leaving the nation-state. Suppression is maximal: behavioral compliance through constant panopticon effect, public shaming lists, blacklisting cascades. No meaningful alternatives exist for accessing services. Pure extraction with minimal coordination function.
constraint_indexing:constraint_classification(social_credit_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SOCIAL AUTONOMY COMMONS (SNARE) — The collective capacity for autonomous behavior, privacy, and social trust is the victim. Generational damage: children grow up with internalized surveillance, treating constant observation as normal. Trust networks erode as reputation data becomes centralized and weaponized. The commons cannot organize or exit. Suppression through normalization and fear is complete. No beneficiary exists within this perspective — only extraction of social freedom.
constraint_indexing:constraint_classification(social_credit_systems, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ADMINISTRATION (ROPE) — The state apparatus experiences the constraint as pure coordination: aggregating behavioral data enables targeted policy, reduces enforcement costs, and coordinates citizen compliance without explicit coercion. The state is not trapped; it can modify or abandon the system. Arbitrage option: can pivot to other surveillance or social management modalities. Low experienced extraction because the state IS the beneficiary. The coordination function (behavioral prediction and compliance optimization) is genuine for the administrator.
constraint_indexing:constraint_classification(social_credit_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CORPORATE RATING OPERATORS (TANGLED ROPE) — Private firms operating the rating infrastructure experience both coordination and extraction. Coordination function: they solve the state's information aggregation problem and provide a service (credit scoring, risk assessment). Extraction: they capture data monopolies, create switching costs, extract behavioral prediction intellectual property, and leverage government enforcement to suppress competitive alternatives. Constrained exit: they depend on government contracts but can theoretically migrate between jurisdictions. Moderate-high extraction reflecting the hybrid nature.
constraint_indexing:constraint_classification(social_credit_systems, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PRIVACY RIGHTS MOVEMENT (SCAFFOLD) — Organized civil society actors (privacy advocates, data protection campaigns, international human rights bodies) see the social credit system as a temporary institutional arrangement with a sunset clause embedded in generational pressure. Movement has agency and exit options: voice through courts, exit through policy change, migration to privacy-protected jurisdictions. Sees the constraint as a staging problem — current surveillance infrastructure will face technical, legal, and social pressure over the next 15-30 years as privacy consciousness matures. Low experienced extraction because organized agents have meaningful alternatives and are fighting the constraint structure itself.
constraint_indexing:constraint_classification(social_credit_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL LEGACY (PITON) — At civilizational scale, social credit systems represent the inertial persistence of older surveillance and reputation management structures (credit scoring, background checks, watchlists, criminal records) now digitized and automated. The system persists through theater: regulatory compliance rituals, appeals processes, and oversight boards that create the appearance of due process while the underlying extraction mechanism remains unchanged. Theater ratio high because most oversight performs legitimacy rather than preventing score manipulation. Institutional power + arbitrage (could be reformed or abandoned) + high theater = piton classification.
constraint_indexing:constraint_classification(social_credit_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/global perspective, social credit systems represent a structural snare on human autonomy and epistemic freedom. The constraint is that centralized behavioral prediction and scoring creates information asymmetry so severe that the rated population cannot know, contest, or escape the criteria determining their life outcomes. The suppression is civilizational: normalization of constant monitoring, internalization of panopticon logic, and entrenchment of the power gradient through technological lock-in. Classification as snare is stable across analytical contexts because the structural asymmetry is intrinsic to the design.
constraint_indexing:constraint_classification(social_credit_systems, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_credit_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_credit_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_credit_systems, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_credit_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_credit_systems, TR),
    TR >= 0.70.

:- end_tests(social_credit_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The system captures behavioral data as commodity, restricts access to services based on scores citizens cannot contest, extracts compliance through normalized surveillance panopticon, and creates switching costs that prevent exit. The trajectory from 0.35 → 0.68 reflects expansion from domain-specific (credit/finance) to universal behavioral scoring. Suppression (0.72): Very high. Suppression operates through multiple mechanisms: formal blacklisting and access restriction (structural); normalized panopticon psychology and shame (internalized); centralized data control preventing alternative reputation systems (informational); and path dependence (generation-long acquisition of dependence on scored systems). Theater ratio (0.58): Moderate-high. Significant performative activity includes appeals processes, algorithmic audit boards, transparency reports, and due process rituals — most of which do not substantively change scoring or improve contestability. Theater has increased as systems mature and face legitimacy pressure. The theater masks that underlying scoring remains opaque, permanently immutable for most citizens, and functions as behavioral control rather than risk assessment. Theater is not so high (>0.70) as to classify as piton because the core extraction mechanism (denial of access, behavioral coercion) remains functionally direct — the theater is added legitimacy, not substituted for function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival divergence between beneficiary and victim. The state administration genuinely experiences a coordination problem solved by social credit (aggregating behavioral data, predicting risk, optimizing compliance) — for them, the constraint IS rope-like: it reduces enforcement costs and enables targeted policy. The corporate operator experiences both coordination (solving the state's data needs) and extraction (capturing data monopolies and behavioral IP) — genuinely tangled_rope. But the rated citizen experiences pure snare: they cannot exit, their behavior is monitored for compliance, and the system restricts access to essential services based on non-contestable scores. The social autonomy commons — the collective capacity for privacy and autonomous behavior — is the victim of pure extraction with zero coordination function; it receives no benefit from the system. The analytical observer sees snare at civilizational scale: the structural asymmetry between central rated observer and decentralized rated population is inherent to the design and cannot be reformed within the system's logic. The piton classification reveals the performative layer: procedural theater (appeals, audits, oversight) that legitimates the system while the underlying extraction mechanism persists unchanged from pre-digitized surveillance structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position relative to the extraction flow. The rated individual (victim + trapped) derives d ≈ 0.95, producing maximum f(d) ≈ 1.42 — experienced extractiveness is amplified by the powerless trapped position. The state (beneficiary + arbitrage) derives d ≈ 0.05, producing negative f(d) ≈ -0.12 — extraction runs toward the state, not away. The corporate operator (beneficiary + constrained) derives d ≈ 0.20, producing f(d) ≈ 0.02 — low experienced extraction because constrained exit is possible and data monopoly is the benefit. The social autonomy commons (victim + trapped) derives d ≈ 0.98 — collective good experiences maximum extraction with zero exit capacity. The privacy movement (organized + mobile) derives d ≈ 0.55, producing f(d) ≈ 0.75 — moderate extraction because they have agency and are actively challenging the constraint. The scope modifier σ(S) at national scope is 1.0, so the chi formula produces: rated individual χ = 0.68 × 1.42 × 1.0 ≈ 0.96; state χ = 0.68 × (-0.12) × 1.0 ≈ -0.08; corporate χ = 0.68 × 0.02 × 1.0 ≈ 0.01. The perspectival gap reflects that the same ε (0.68) yields wildly different experienced extractiveness (χ) for different agents based on their directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by correctly identifying that the state and operator genuinely experience coordination (rope/tangled_rope) while the citizen genuinely experiences pure extraction (snare). This is NOT a measurement error or a false classification disagreement — it is a structural fact: the constraint exists to extract behavioral compliance from the rated population and to provide coordination infrastructure for the rater. Both functions are real. The state IS solving a real coordination problem (aggregating behavior data for policy). The citizen IS being extracted from (their autonomy and privacy are captured as commodities; their behavior is constrained through score-based access denial). The system is not a snare mistaken for a rope or vice versa; it is a snare FOR the citizen and a rope FOR the state. The mandatrophy is resolved by recognizing that the classification type is agent-dependent, not constraint-dependent. The same constraint is simultaneously snare and rope from different perspectives because the extraction from one agent IS the coordination for the other. The false summit detection on the analytical mountain classification confirms this: the framing of social credit as 'inevitable' or 'a law of technology' is naturalization of what is actually a policy choice with distributional consequences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_external_coercion,
    'To what degree is compliance driven by internalized shame and self-surveillance versus external punishment and blacklisting mechanisms?',
    'Behavioral analysis during periods of system opacity (e.g., when citizens believe they are unobserved) versus system transparency; tracking changes in compliance rates following algorithmic audits or temporary scoring freezes',
    'If primarily internalized: suppression is higher than structural barriers suggest — victims carry panopticon logic even after system removal, expanding effective suppression beyond exit. If primarily external: exit becomes more feasible once structural barriers are removed, reclassifying the exit_options upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_external_coercion, empirical, 'Whether suppression is internalized panopticon or external enforcement').

omega_variable(
    score_contestability_and_due_process,
    'Do formal appeals processes and algorithmic audit rights constitute meaningful contestation, or are they pure theater masking immutable scoring?',
    'Success rate of citizen appeals; transparency of scoring criteria; ability to improve scores through behavior change versus algorithmic permanence; comparison of stated criteria versus actual scoring function',
    'If contestable: classify as tangled_rope (some coordination function via procedural fairness) rather than pure snare. If theater: confirms snare and raises theater_ratio. If permanently immutable: escalates trap severity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(score_contestability_and_due_process, empirical, 'Whether appeals processes are substantive or theatrical').

omega_variable(
    cross_system_information_leakage,
    'Does social credit score function as a master identifier enabling function creep into unrelated domains (e.g., medical care, legal proceedings, family custody)?',
    'Audit of institutional uses of social credit data; tracking expansion of criteria and domains subject to scoring over time; analysis of coupling between the rating system and downstream systems',
    'If significant creep: suppression and extractiveness increase — the system becomes a universal control mechanism rather than domain-specific. If bounded: constraints on scope may reduce effective extraction. If universal: reclassify as civilizational rather than national scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_system_information_leakage, empirical, 'Extent of function creep across institutional domains').

omega_variable(
    behavioral_change_versus_selection_effect,
    'Does the constraint operate by changing behavior (compliance extraction) or by filtering population (removing high-risk individuals from domains)?',
    'Comparison of outcome distributions before/after scoring implementation; analysis of whether constrained individuals improve scores or are permanently excluded; tracking of demographic shifts in accessed services',
    'If behavioral change dominates: suppression mechanism is internalized compliance — extraction persists over time as agents remain in the system. If selection/filtering dominates: extraction is acute (high initial suppression) but potential for system population to stabilize. Different policy implications for constraint amelioration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_change_versus_selection_effect, empirical, 'Whether the system enforces behavioral compliance or population filtering').

omega_variable(
    technological_inevitability_framing,
    'Is the social credit system justified as technologically inevitable (big data makes scoring necessary and objective) or as policy choice (explicit surveillance policy)?',
    'Analysis of policy justifications and regulatory framing; examination of alternative technology implementations that would preserve data utility while reducing surveillance (e.g., federated scoring, encrypted aggregation); international comparison of similar economies with and without systems',
    'If framed as inevitable: false naturalization (mountain misclassification) — increases acceptance and reduces policy pressure to modify. If framed as policy choice: maintains snare classification and supports organizing for constraint removal. The framing itself becomes a suppression mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_inevitability_framing, conceptual, 'Whether system is framed as inevitable or policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_credit_systems, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scs_tr_t0, social_credit_systems, theater_ratio, 0, 0.32).
narrative_ontology:measurement(scs_tr_t3, social_credit_systems, theater_ratio, 3, 0.45).
narrative_ontology:measurement(scs_tr_t6, social_credit_systems, theater_ratio, 6, 0.58).
narrative_ontology:measurement(scs_tr_t9, social_credit_systems, theater_ratio, 9, 0.62).

% Extraction over time
narrative_ontology:measurement(scs_be_t0, social_credit_systems, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scs_be_t3, social_credit_systems, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(scs_be_t6, social_credit_systems, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(scs_be_t9, social_credit_systems, base_extractiveness, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_credit_systems, enforcement_mechanism).
narrative_ontology:affects_constraint(social_credit_systems, algorithmic_opacity_in_governance).
narrative_ontology:affects_constraint(social_credit_systems, digital_identity_monopoly).
narrative_ontology:affects_constraint(social_credit_systems, behavioral_prediction_commodification).

% DUAL FORMULATION NOTE:
% Social credit systems decompose into three structurally distinct constraints: (1) enforcement_mechanism_coordination — the state's genuine problem of behavioral prediction and policy targeting; (2) data_monopoly_extraction — corporate capture of behavioral data and access restriction through proprietary algorithms; (3) autonomy_commons_depletion — collective loss of privacy and autonomous behavior as externality of centralized scoring. The three stories share base properties but have different beneficiaries, victims, and perspectives. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_credit_systems, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
