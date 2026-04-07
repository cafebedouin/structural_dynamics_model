% ============================================================================
% CONSTRAINT STORY: organizational_assessment_validity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_assessment_validity, []).

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
 *   constraint_id: organizational_assessment_validity
 *   human_readable: Organizational Assessment Validity Constraint
 *   domain: organizational_management/human_resources
 *
 * SUMMARY:
 *   Organizational assessment validity describes the structural constraint
 *   created when performance evaluation systems must simultaneously serve
 *   coordination (genuine feedback enabling development) and extraction
 *   (identifying termination and wage-suppression targets) without
 *   transparent acknowledgment of the dual function. This constraint
 *   generates a perspectival cascade: employees experience snare (career
 *   consequences without control over evaluation criteria), peers experience
 *   tangled rope (genuine feedback coordination mixed with implicit threats),
 *   administrators experience rope (legitimate information function with exit
 *   options), leadership experiences tangled rope (coordination benefit plus
 *   weaponizable data advantage), the legacy ritual persists as piton
 *   (institutional inertia despite documented inefficacy), and organized
 *   alternatives (continuous feedback, skill-based evaluation) approach
 *   scaffold status (alternative pathways with sunset logic). The constraint
 *   exhibits high theater ratio (68%) reflecting that assessment cycles are
 *   substantially performative: preparation, calibration meetings,
 *   documentation, and rating appeal processes consume organizational energy
 *   without proportionate improvement in decision quality. The growing
 *   divergence between actual business decisions and assessment
 *   recommendations suggests the ritual has become decoupled from genuine
 *   function.
 *
 * KEY AGENTS:
 *   - Assessed Employees: Primary victims (powerless/trapped) — career outcomes depend on assessment scores; cannot refuse participation without employment risk
 *   - Peer Raters: Secondary victims (moderate/constrained) — provide genuine feedback value but face career consequences for honest assessment
 *   - Assessment Administration: Primary beneficiary (institutional/arbitrage) — maintains documentation justifying HR decisions; coordinates succession planning
 *   - Senior Leadership: Extractive beneficiary (powerful/mobile) — uses assessment data to identify and remove dissidents; benefits from information asymmetry about assessment use
 *   - Assessment Legacy Ritual: Institutional actor (institutional/arbitrage) — persists through inertia; maintained because alternative systems require acknowledging assessment's past inefficacy
 *   - Continuous Feedback Movement: Organized coalition (organized/constrained) — building alternative assessment pathways with lower theater and genuine validity focus
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks treating assessment necessity as natural law rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_assessment_validity, 0.58).
domain_priors:suppression_score(organizational_assessment_validity, 0.62).
domain_priors:theater_ratio(organizational_assessment_validity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_assessment_validity, extractiveness, 0.58).
narrative_ontology:constraint_metric(organizational_assessment_validity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(organizational_assessment_validity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_assessment_validity, tangled_rope).
narrative_ontology:human_readable(organizational_assessment_validity, "Organizational Assessment Validity Constraint").
narrative_ontology:topic_domain(organizational_assessment_validity, "organizational_management/human_resources").

domain_priors:requires_active_enforcement(organizational_assessment_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_assessment_validity, assessment_administrators).
narrative_ontology:constraint_beneficiary(organizational_assessment_validity, senior_leadership).
narrative_ontology:constraint_victim(organizational_assessment_validity, assessed_employees).
narrative_ontology:constraint_victim(organizational_assessment_validity, organizational_learning_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASSESSED EMPLOYEE (SNARE) — Career outcomes directly depend on assessment scores; cannot refuse participation without jeopardizing employment. Trapped between providing authentic self-assessment (which reveals vulnerabilities used against them) and strategic self-presentation (which corrupts validity). Maximum extraction: assessment mechanism exists ostensibly to develop the employee but actually determines compensation, promotion, and termination decisions.
constraint_indexing:constraint_classification(organizational_assessment_validity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PEER RATER (TANGLED ROPE) — Provides coordination function (peer feedback genuinely improves self-awareness and performance) but faces social and career consequences for honest assessment. Constrained by fear of retaliation, workplace cohesion concerns, and the knowledge that ratings may be used against peers. Experiences both genuine learning coordination and extracted labor (unpaid evaluation work whose output is weaponized).
constraint_indexing:constraint_classification(organizational_assessment_validity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ASSESSMENT ADMINISTRATION (ROPE) — Experiences the constraint as coordination mechanism: documenting performance justifies HR decisions, enables succession planning, and creates defensible records. Net beneficiary with exit options (can adjust assessment design, frequency, stakes). Sees genuine coordination benefit: rigorous evaluation improves organizational capability.
constraint_indexing:constraint_classification(organizational_assessment_validity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SENIOR LEADERSHIP (TANGLED ROPE) — Benefits from assessment data for talent decisions and performance management (coordination function); also extracts by using assessment mechanism to identify and remove dissidents or low-performers without firing costs. Mobile exit option (can switch to different assessment design) but benefits from current system's information asymmetry. Mixed coordination-extraction with asymmetric benefit.
constraint_indexing:constraint_classification(organizational_assessment_validity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY ASSESSMENT RITUAL (PITON) — Annual performance review cycles persist through institutional inertia despite well-documented inefficacy: studies show minimal correlation between annual ratings and subsequent performance, high rater bias, and theater-heavy preparation. Ritual continues because 'this is how we do talent management' and because exit would require admitting the prior decades of ritualized assessment were performative. Theater ratio dominates: preparation time, calibration meetings, and documentation consume resources without improving decision quality.
constraint_indexing:constraint_classification(organizational_assessment_validity, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONTINUOUS FEEDBACK MOVEMENT (SCAFFOLD) — Organized advocates (consulting firms, tech companies, worker advocates) are building alternative assessment pathways: real-time feedback systems, skill-based rather than person-based evaluation, and continuous data collection replacing annual reviews. These alternatives have sunset logic: as continuous feedback systems mature and prove more predictive, the annual assessment mechanism's extraction function declines. Constrained by incumbent institutional resistance but sees clear exit path through technological and cultural norm change.
constraint_indexing:constraint_classification(organizational_assessment_validity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, some form of performance assessment is arguably necessary for any organization above minimal size: without mechanisms to differentiate capability, resource allocation defaults to political capture or nepotism. This perspective treats organizational assessment as a natural law: any system of work requires evaluation. However, the structural data (high theater, documented bias, extraction-driven design) contradicts the mountain classification — the engine flags this as a false summit naturalizing contingent institutional arrangements.
constraint_indexing:constraint_classification(organizational_assessment_validity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_assessment_validity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_assessment_validity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_assessment_validity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_assessment_validity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_assessment_validity, TR),
    TR >= 0.70.

:- end_tests(organizational_assessment_validity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Assessment mechanisms genuinely extract by creating information asymmetry (what assessors know about assessment use), by creating career consequences without transparent rationale, and by converting employee vulnerability (willingness to accept feedback) into weaponized data. But extractiveness is not maximal because some organizations do use assessments for legitimate development purposes, and the constraint's effects are modulated by whether assessment data actually drives promotion/termination decisions. Suppression (0.62): High. Multiple suppression mechanisms operate simultaneously: (1) career consequences prevent honest self-assessment, (2) social punishment suppresses honest peer feedback, (3) institutional opacity about assessment use suppresses challenge to validity, (4) lack of independent verification of assessment accuracy suppresses correction mechanisms. Theater ratio (0.68): High and increasing. Annual review cycles allocate substantial calendar time, preparation energy, and organizational attention to documentation and calibration meetings with minimal correlation to actual performance outcomes. The ratio has grown as organizations have added 360-degree feedback, mid-year check-ins, and continuous feedback systems without removing the performative annual review, creating layered theater.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap exists between the assessed employee (who sees pure extraction justified as necessary assessment) and the administrator (who sees pure coordination justified as development). The contradiction is resolved by recognizing that assessment IS both simultaneously: a genuine coordination mechanism (feedback does improve self-awareness and organizational capability when applied constructively) AND an extraction mechanism (career consequences without transparent criteria create asymmetric power that enables termination and wage suppression without explicit cost). The gap reveals that extractiveness increases when assessment stakes are high (assessment drives compensation and termination decisions) and decreases when stakes are low (assessment informs development without career consequences). Current trajectory shows stakes rising (extraction increasing) while the ritual's actual predictive validity declines (suggesting theater is increasing relative to function).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from the structural position of each agent relative to the extraction flow. Assessed employees face high d (close to 1.0, pure targets) because they bear career consequences without control over evaluation. Peer raters face moderate d (~0.55) because they both provide genuine coordination value and face suppression if they rate honestly. Administrators face low d (~0.15) because the assessment mechanism coordinates their legitimate function. Leaders face low-moderate d (~0.35) because they benefit from assessment data but are not fully captured by the constraint (they have exit options through assessment redesign). The assessment ritual itself faces moderate-high d (~0.70) as its function has atrophied despite institutional persistence. The continuous feedback coalition faces moderate d (~0.50) because they experience the constraint as solvable rather than inherent. These directionality values produce the observed chi distribution: powerless victims experience maximum χ, institutional beneficiaries experience minimum or negative χ, and moderate agents experience intermediate χ that permits both their genuine coordination benefits and their experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves through the observation that 'organizational assessment' is not a single constraint but a family of constraints with different extractiveness values depending on whether assessment outcomes drive: (1) Developmental feedback only (ε ≈ 0.15, Rope), (2) Promotion decisions (ε ≈ 0.40, Tangled Rope), (3) Compensation (ε ≈ 0.55, Tangled Rope), or (4) Termination (ε ≈ 0.72, Snare). The current story measures organizational assessment as typically implemented in large firms with high-stakes consequences (ε = 0.58, Tangled Rope boundary). The false natural law (mountain perspective) assumes assessment is inherently necessary — but the necessity is only for some form of feedback, not for this specific high-theater, high-extraction implementation. The scaffold perspective (continuous feedback alternatives) is structurally sound: organizations can achieve the coordination benefits (genuine feedback, succession planning, capability improvement) through low-theater mechanisms without the high-suppression, high-theater extraction apparatus of annual reviews. The theater ratio trajectory (increasing from 0.42 to 0.68) indicates progressive decoupling: ritual has expanded while actual decision-relevance has declined, suggesting the constraint is degrading toward piton status even as extractiveness remains high.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    validity_criterion_ambiguity,
    'What constitutes valid organizational assessment — predictive validity (does rating predict future performance), construct validity (does it measure what it claims), or convergent validity (do multiple assessors agree)?',
    'Longitudinal empirical analysis comparing assessment scores to subsequent performance metrics, independent of the assessment mechanism; meta-analysis of validity coefficients across assessment methods',
    'If predictive validity criterion is used: most current assessments fail (correlations ~0.3), validating snare classification. If convergent validity criterion is used: assessments can meet threshold despite poor prediction, validating piton classification. If construct validity is criterion: requires examination of whether organization is measuring actual job requirements or performative criteria.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(validity_criterion_ambiguity, empirical, 'Which validity criterion defines assessment success').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the assessment mechanism''s primary function coordination (legitimate performance feedback enabling development) or extraction (identifying targets for termination or wage suppression)?',
    'Structural analysis of assessment outcomes: correlation between low assessments and subsequent terminations; comparison of wage growth trajectories for high vs low assessed employees; analysis of assessment stakes (does assessment drive compensation, promotion, or termination decisions?)',
    'If primarily coordination: classification shifts toward Rope/Tangled Rope with lower extractiveness. If primarily extraction: classification shifts toward Snare with higher extractiveness and stronger suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether assessment serves coordination or extraction function').

omega_variable(
    strategic_gaming_prevalence,
    'To what extent does knowledge of assessment criteria cause strategic behavior that corrupts the assessment''s own validity?',
    'Comparison of assessment results pre- and post-transparency of rating criteria; analysis of behavior change after employees learn how they are being evaluated; experimental manipulation of assessment knowledge and measurement of gaming intensity',
    'If gaming is pervasive: assessment provides no valid signal (reliability approaches zero), validating snare perspective (extraction with no coordination benefit). If gaming is minimal: assessment retains some validity, supporting tangled rope classification with genuine coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_gaming_prevalence, empirical, 'Extent of strategic gaming in assessment responses').

omega_variable(
    rater_capture_mechanism,
    'Are peer raters captured by the assessment system (induced to provide strategic rather than honest ratings) or do they maintain independence?',
    'Analysis of peer rating patterns before and after implementation of rating consequences for raters; comparison of peer ratings to independent behavioral data; structural analysis of whether raters face consequences for their ratings',
    'If raters are captured: peer ratings provide no valid signal and become pure extraction mechanism (assessment scores become arbitrary assignment of status rather than valid feedback). If raters maintain independence: genuine peer feedback coordination occurs despite systemic extraction at other levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rater_capture_mechanism, empirical, 'Whether rater independence is maintained or captured').

omega_variable(
    unconscious_bias_vs_structural_extraction,
    'Do assessment biases (demographic disparities, implicit associations, halo effects) represent inherent human limitations (approaching mountain), or deliberate (if implicit) extraction mechanisms targeting specific groups?',
    'Comparison of bias patterns across assessment types; analysis of whether training reduces bias or merely redistributes it; examination of correlation between bias patterns and demographic groups targeted by organizational changes (layoffs, reassignments)',
    'If bias is inherent human limitation: extractiveness decreases but approaches the mountain boundary. If bias is systematic targeting: extractiveness increases and demonstrates asymmetric impact validating snare classification for affected groups.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unconscious_bias_vs_structural_extraction, conceptual, 'Whether bias represents inherent limitation or structural targeting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_assessment_validity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oav_tr_t0, organizational_assessment_validity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(oav_tr_t5, organizational_assessment_validity, theater_ratio, 5, 0.55).
narrative_ontology:measurement(oav_tr_t10, organizational_assessment_validity, theater_ratio, 10, 0.68).
narrative_ontology:measurement(oav_tr_t15, organizational_assessment_validity, theater_ratio, 15, 0.75).

% Extraction over time
narrative_ontology:measurement(oav_be_t0, organizational_assessment_validity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oav_be_t5, organizational_assessment_validity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(oav_be_t10, organizational_assessment_validity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(oav_be_t15, organizational_assessment_validity, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_assessment_validity, resource_allocation).
narrative_ontology:affects_constraint(organizational_assessment_validity, organizational_learning_systems).
narrative_ontology:affects_constraint(organizational_assessment_validity, psychological_safety_emergence).
narrative_ontology:affects_constraint(organizational_assessment_validity, wage_compression_mechanisms).

% DUAL FORMULATION NOTE:
% Organizational assessment validity decomposes into two structurally distinct constraints: (1) assessment_as_feedback_mechanism (ε ≈ 0.15, Rope when stakes are low) and (2) assessment_as_career_gatekeeping (ε ≈ 0.68, Snare when high stakes). They are networked because gatekeeping assessments suppress honest feedback (affecting feedback validity upstream). Assessment reform that shifts from high-stakes career gatekeeping to low-stakes development feedback resolves the tangled rope by decomposing it into pure rope + separate gatekeeping constraint with transparent criteria.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_assessment_validity, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
