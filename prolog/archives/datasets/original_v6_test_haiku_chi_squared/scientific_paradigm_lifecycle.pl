% ============================================================================
% CONSTRAINT STORY: scientific_paradigm_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scientific_paradigm_lifecycle, []).

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
 *   constraint_id: scientific_paradigm_lifecycle
 *   human_readable: The Crisis of a Scientific Paradigm
 *   domain: scientific/sociological
 *
 * SUMMARY:
 *   The crisis phase of a dominant scientific paradigm creates a structural
 *   constraint that exhibits multiple DR types simultaneously. Following
 *   Kuhn's model, a paradigm enters crisis when anomalies accumulate beyond
 *   the paradigm's capacity to explain or suppress them. This constraint
 *   reveals how institutional mechanisms designed for quality control (peer
 *   review, journal gatekeeping, grant evaluation) become extraction
 *   mechanisms when they systematically suppress competing frameworks. The
 *   paradigm-defending establishment benefits from the crisis delay — their
 *   career capital, institutional position, and theoretical investments
 *   accumulate value during the suppression period. Early-career researchers
 *   discovering anomalies bear the costs: publication rejection, career
 *   damage, and forced conformity to frameworks they recognize as incomplete.
 *   The constraint's theater ratio (0.68) reflects that paradigm defense
 *   increasingly becomes performative: reviewers assess 'paradigm fit' rather
 *   than empirical validity, journals accept anomaly-conforming
 *   reinterpretations rather than paradigm-challenging findings, and funding
 *   bodies demand that anomalies be addressed within the paradigm rather than
 *   through alternative frameworks. From some perspectives (analytical
 *   observer at civilizational scale), the crisis appears as an immutable
 *   feature of science — inevitable and necessary. From the perspective of
 *   those discovering anomalies (powerless/trapped), it appears as pure
 *   extraction via suppression. From the perspective of organized researchers
 *   (anomaly constituency), it appears as a mixed coordination-extraction
 *   hybrid. From the perspective of paradigm defenders
 *   (institutional/arbitrage), it appears as legitimate quality control.
 *
 * KEY AGENTS:
 *   - Anomaly Discovering Researchers: Early-career victims (powerless/trapped) — face systematic publication rejection and career risk; cannot exit field without abandoning research trajectory
 *   - Alternative Framework Proposers: Secondary victims (moderate/constrained) — develop non-paradigm theoretical models; constrained by gatekeeping and citation metrics; can exit but at significant cost
 *   - Accumulated Anomaly Constituency: Organized victims (organized/constrained) — researchers whose work accumulates anomalies over decades; benefit from paradigm methodology but victimized by suppression
 *   - Paradigm-Defending Establishment: Primary beneficiary (institutional/arbitrage) — senior researchers, journal editors, funding agencies invested in paradigm legitimacy; accumulate career capital during suppression period
 *   - Incumbent Research Institutions: Secondary beneficiary (institutional/arbitrage) — universities, labs, funding bodies whose prestige and resources are tied to paradigm legitimacy
 *   - Pluralistic Reform Movement: Powerful reform agents (powerful/mobile) — funding bodies and young institution-builders advocating paradigm pluralism with sunset clause; see crisis as temporary coordination failure
 *   - Peer Review System: Institutional mechanism (institutional/arbitrage) — journal gatekeeping and grant review; increasingly performative in crisis phase (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional suppression as inherent to science
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scientific_paradigm_lifecycle, 0.58).
domain_priors:suppression_score(scientific_paradigm_lifecycle, 0.62).
domain_priors:theater_ratio(scientific_paradigm_lifecycle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scientific_paradigm_lifecycle, tangled_rope).
narrative_ontology:human_readable(scientific_paradigm_lifecycle, "The Crisis of a Scientific Paradigm").
narrative_ontology:topic_domain(scientific_paradigm_lifecycle, "scientific/sociological").

domain_priors:requires_active_enforcement(scientific_paradigm_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scientific_paradigm_lifecycle, paradigm_defending_establishment).
narrative_ontology:constraint_beneficiary(scientific_paradigm_lifecycle, incumbent_research_institutions).
narrative_ontology:constraint_victim(scientific_paradigm_lifecycle, anomaly_discovering_researchers).
narrative_ontology:constraint_victim(scientific_paradigm_lifecycle, emerging_alternative_frameworks).
narrative_ontology:constraint_victim(scientific_paradigm_lifecycle, field_predictive_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANOMALY DISCOVERER (SNARE) — Early career researcher discovers empirical violations of paradigm orthodoxy. Cannot exit the field without abandoning research trajectory; career advancement requires paradigm acceptance; anomaly publication faces systematic rejection. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.96. High extraction via suppression of inconvenient findings.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE FRAMEWORK PROPOSER (SNARE) — Develops non-paradigm theoretical model to explain anomalies. Constrained by journal gatekeeping, citation metrics, and grant funding tied to paradigm legitimacy. Can eventually exit by changing fields, but at significant cost. d≈0.85, f(d)≈1.20, σ=1.0 → χ≈0.70. Significant extraction through institutional resistance.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANOMALY CONSTITUENCY (TANGLED ROPE) — Organized group of researchers who accumulate anomalies over decades. Benefits from paradigm's existing methodology, training infrastructure, and accumulated knowledge. Simultaneously victimized by suppression of anomaly publication and blocked alternative research directions. d≈0.58, f(d)≈0.68, σ=1.2 → χ≈0.47. Mixed coordination (knowledge infrastructure) and extraction (suppression).
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PARADIGM DEFENDING ESTABLISHMENT (ROPE) — Senior researchers, journal editors, funding agencies invested in paradigm legitimacy. Sees constraint as coordination function: enforcing standards protects field from pseudoscience and premature abandonment of successful framework. Benefits from institutional power and career capital accumulated under paradigm. d≈0.02, f(d)≈-0.18, σ=1.2 → χ≈-0.12. Net beneficiary; negative extraction indicates subsidy.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PLURALISTIC REFORM MOVEMENT (SCAFFOLD) — Powerful actors (funding bodies, young institution-builders) advocating paradigm pluralism, dedicated anomaly-study programs, and alternative framework development with sunset clause. Sees crisis as temporary coordination failure with path to resolution: increased funding for anomaly research, multi-paradigm journals, and postdoctoral positions in emerging frameworks. d≈0.35, f(d)≈0.33, σ=1.2 → χ≈0.23. Low extraction because reformers have exit options and see institutional solutions.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PEER REVIEW RITUAL (PITON) — Paradigm-orthodoxy enforcement mechanism (journal gate-keeping, grant review, conference selection) persists through institutional inertia long after its functional verification capacity has degraded. Theater_ratio=0.68: reviewers increasingly perform 'paradigm fit' assessment rather than genuine anomaly evaluation. The ritual maintains itself because alternatives haven't fully replaced it, but its core function (preventing false positives via expert validation) has atrophied under the weight of anomaly accumulation.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From civilizational perspective, paradigm crisis might appear as an immutable feature of scientific progress: all dominant paradigms eventually encounter anomalies, forcing crisis. This perspective risks naturalizing what are actually contingent institutional arrangements (career incentives, journal gatekeeping, funding concentration) as inherent to the logic of science itself. The structural data (ε=0.58, suppression=0.62, theater=0.68) reveals this as a false summit: the crisis mechanism is socially constructed and could be redesigned.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scientific_paradigm_lifecycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scientific_paradigm_lifecycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(scientific_paradigm_lifecycle, TR),
    TR >= 0.70.

:- end_tests(scientific_paradigm_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The constraint extracts from anomaly discoverers through publication suppression, career penalties, and forced conformity. But the extraction is not total — some anomalies do get published in specialized venues, and alternative frameworks do eventually develop. The value reflects that suppression mechanisms are effective enough to delay paradigm transition by years or decades, but not powerful enough to prevent it indefinitely. Suppression (0.62): High. Multiple coordinated mechanisms suppress anomalies and alternatives: peer review gatekeeping, citation disadvantage, grant funding tied to paradigm legitimacy, conference exclusion, and implicit career penalties. The suppression is structural and sustained but not absolute — some researchers publish anomalies in preprints, specialty journals, and books. Theater ratio (0.68): High. Paradigm defense in crisis phase increasingly becomes performative. Peer reviewers assess paradigm fit rather than empirical validity. Journals accept paradigm-reinterpreting responses to anomalies rather than paradigm-challenging findings. Grant reviewers demand that anomalies be addressed 'within the framework' rather than through alternative approaches. The performative content has increased over the interval (0.35→0.68) as anomaly accumulation has outpaced the paradigm's explanatory capacity, making genuine scientific justification for suppression harder to maintain.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates profound perspectival divergence from a single structural reality. The anomaly discoverer sees a snare (trapped, powerless, high extraction via suppression). The alternative framework proposer sees a snare (constrained exit, blocked development). The accumulated anomaly constituency sees tangled rope (mixed benefits from paradigm methodology and costs from suppression). The paradigm-defending establishment sees rope (legitimate coordination and quality control). The pluralistic reform movement sees a temporary scaffold (coordination failure with institutional solutions). The peer review system sees itself as degraded piton (ritual persisting through inertia). The analytical observer at civilizational scale risks seeing mountain (inevitable crisis as inherent to science). The perspectival gap reveals that the same structural phenomenon — institutional suppression of anomalies and alternatives — is experienced and classified differently depending on the observer's structural position and exit options. An analytical task is to determine which perspectives are accurate empirical assessments vs which perspectives are legitimizing narratives.
 *
 * DIRECTIONALITY LOGIC:
 *   Anomaly discoverer: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; researcher cannot exit without abandoning career. Alternative framework proposer: Victim + constrained → d≈0.85, f(d)≈1.20. High extraction; exit possible but costly. Anomaly constituency: Victim + beneficiary + constrained → d≈0.58, f(d)≈0.68. Mixed; researchers benefit from paradigm methodology but victimized by suppression. Paradigm-defending establishment: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.18. Net beneficiary; institutional position and career capital accumulate during suppression. Pluralistic reformers: Powerful + mobile → d≈0.35, f(d)≈0.33. Low extraction; reformers have agency and mobility. Peer review system: Institutional + arbitrage → d≈0.02, f(d)≈-0.18. Piton classification comes from theater gate (0.68≥0.70), not from chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival risk (naturalizes contingent suppression).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint's extractiveness (0.58) is below the mandatrophy threshold (0.70), so mandatrophy resolution is not required by the schema. However, the analysis reveals a latent mandatrophy: is this constraint a tangled rope (genuine coordination function of paradigm orthodoxy combined with asymmetric extraction) or a snare (pure extraction masked as quality control)? The resolution depends on whether paradigm defense genuinely prevents false positives (quality control function = rope component) or merely preserves incumbent career benefits (extraction function = snare component). The omega variable 'extraction_vs_quality_control' captures this ambiguity. Resolution mechanism: empirical error-rate analysis comparing false positive rates in paradigm-defended vs pluralistic periods. If paradigm defense prevents significantly more false positives: tangled rope is correct (mixed). If false positive rates are equal or higher in paradigm-defended periods: snare is correct (pure extraction). Current evidence suggests the constraint exhibits BOTH: genuine quality control function in early paradigm phase (rope-like) degrading into extraction during crisis phase (snare-like). The theater ratio progression (0.35→0.68) indicates transition from functional gatekeeping to performative suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anomaly_accumulation_threshold,
    'What quantitative threshold of anomalies triggers crisis recognition: is it a statistical threshold (e.g., >5% of claims produce non-fitting data) or a social recognition process (e.g., when prestigious researchers admit problems)?',
    'Historical metrology of paradigm-critical literature; longitudinal tracking of anomaly publication rates preceding major paradigm shifts (Ptolemaic→Copernican, Newtonian→Relativistic, classical→quantum)',
    'If threshold is empirical: crisis is automatic (mountain-like). If threshold is social: crisis is contingent on institutional recognition (tangled rope or snare-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anomaly_accumulation_threshold, empirical, 'Threshold for anomaly accumulation that triggers crisis recognition').

omega_variable(
    alternative_framework_viability,
    'Can a genuine alternative framework develop while the dominant paradigm''s institutional apparatus suppresses it, or does suppression necessarily prevent viable competitors from emerging?',
    'Case study analysis: rate of alternative framework maturation under suppression vs rate under parallel institutional support (comparing historical anomaly programs vs modern pluralism experiments)',
    'If suppression prevents viability: snare classification is correct; only paradigm collapse enables transition. If alternatives can mature despite suppression: scaffold perspective is correct; reform movements can create space for transition without collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_framework_viability, empirical, 'Whether alternative frameworks can mature under paradigm suppression').

omega_variable(
    extraction_vs_quality_control,
    'Does institutional paradigm defense serve a genuine quality-control function (filtering false positives) or primarily serve extraction of career benefits from incumbents?',
    'Error rate analysis: compare false positive rates in paradigm-defended periods vs pluralistic periods; measure citation accumulation asymmetries between paradigm-conform and anomaly-focused research',
    'If genuine quality control: establishment perspective (rope) is correct. If primarily extraction: tangled rope or snare perspectives are correct; institutional resistance is rent-seeking rather than epistemic virtue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_quality_control, empirical, 'Whether paradigm defense serves quality control or extraction').

omega_variable(
    crisis_duration_predictability,
    'Is the duration of paradigm crisis determined by structural/sociological factors (organizational inertia, generational turnover) or by intrinsic anomaly severity?',
    'Comparative historical analysis of paradigm crisis durations (Copernican shift ~100 years, quantum revolution ~25 years, string theory stasis ~40 years); correlation with institutional factors (number of tenured defenders, funding concentration, journal monopoly)',
    'If determined by organizational inertia: extraction mechanism is institutional/contingent (tangled rope). If determined by anomaly severity: crisis is more objective/automatic (mountain-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_duration_predictability, conceptual, 'Whether crisis duration is determined by organizational or anomaly factors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scientific_paradigm_lifecycle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paradigm_tr_t0, scientific_paradigm_lifecycle, theater_ratio, 0, 0.35).
narrative_ontology:measurement(paradigm_tr_t5, scientific_paradigm_lifecycle, theater_ratio, 5, 0.52).
narrative_ontology:measurement(paradigm_tr_t10, scientific_paradigm_lifecycle, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(paradigm_be_t0, scientific_paradigm_lifecycle, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(paradigm_be_t5, scientific_paradigm_lifecycle, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(paradigm_be_t10, scientific_paradigm_lifecycle, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scientific_paradigm_lifecycle, enforcement_mechanism).
narrative_ontology:affects_constraint(scientific_paradigm_lifecycle, peer_review_gatekeeping).
narrative_ontology:affects_constraint(scientific_paradigm_lifecycle, publication_bias_accumulation).
narrative_ontology:affects_constraint(scientific_paradigm_lifecycle, academic_career_lock_in).

% DUAL FORMULATION NOTE:
% The scientific paradigm lifecycle is modeled as a single constraint story capturing the crisis phase. However, this could decompose into multiple distinct constraints: (1) the paradigm's explanatory adequacy (mountain-like if the paradigm is truly exhausted, rope-like if anomalies are resolvable), (2) the institutional suppression mechanism (tangled rope or snare), and (3) the social/career dynamics that sustain paradigm loyalty (snare or piton). The single story integrates these because they operate synchronously during crisis; decomposition becomes necessary only if one wishes to model pre-crisis or post-resolution phases separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(scientific_paradigm_lifecycle, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
