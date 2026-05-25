% ============================================================================
% CONSTRAINT STORY: senior_tech_dependence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_senior_tech_dependence, []).

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
 *   constraint_id: senior_tech_dependence
 *   human_readable: Senior Technology Dependence and Digital Exclusion
 *   domain: social/economic/technology
 *
 * SUMMARY:
 *   The digitalization of essential services creates a structural extraction
 *   mechanism targeting older adults with lower digital literacy. The
 *   constraint exhibits a perspectival range from mountain (naturalized
 *   technological change) to snare (pure extraction of trapped seniors) to
 *   rope (coordination function for platforms). The tension arises because
 *   genuine coordination benefits (platforms enable service delivery, reduce
 *   transaction costs, increase accessibility for digitally fluent users)
 *   coexist with asymmetric extraction (forced obsolescence,
 *   complexity-driven exclusion, unpaid digital support labor displaced onto
 *   family caregivers). Theater ratio growth (0.35 to 0.55) reflects
 *   increasing performativity of legacy support channels — phone banking
 *   lines, in-person services, and paper alternatives are maintained as
 *   theatrical gestures while policy and infrastructure actively push
 *   digitalization. Extractiveness growth (0.35 to 0.58) reflects
 *   accumulating extraction mechanisms: initial mandatory adoption, then
 *   platform redesigns, then algorithmic opacity, then privacy extraction
 *   through behavioral data harvesting.
 *
 * KEY AGENTS:
 *   - Digitally Isolated Seniors: Primary victims (powerless/trapped) — face biological cognitive decline, economic dependency on digital services, and zero alternatives for essential services like banking and healthcare benefits
 *   - Tech-Competent Seniors: Secondary victims (moderate/constrained) — navigate forced updates, platform redesigns, and algorithmic changes; have exit options through alternative platforms but not from digital ecosystem itself
 *   - Tech Platforms: Primary beneficiaries (institutional/arbitrage) — monetize senior userbase through behavioral data harvesting, attention capture, service lock-in; have complete exit optionality and can alter terms unilaterally
 *   - Family Caregivers: Secondary beneficiaries and victims (organized/constrained) — benefit from remote monitoring and communication but bear unpaid support labor and emotional burden of managing seniors' digital safety
 *   - Legacy Support Infrastructure: Performative actors (institutional/arbitrage) — telephone hotlines and in-person services maintained for regulatory compliance while resources shift to digital channels
 *   - Analytical Observer: Risks naturalizing contingent choices as immutable technological progress
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(senior_tech_dependence, 0.58).
domain_priors:suppression_score(senior_tech_dependence, 0.68).
domain_priors:theater_ratio(senior_tech_dependence, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(senior_tech_dependence, extractiveness, 0.58).
narrative_ontology:constraint_metric(senior_tech_dependence, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(senior_tech_dependence, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(senior_tech_dependence, tangled_rope).
narrative_ontology:human_readable(senior_tech_dependence, "Senior Technology Dependence and Digital Exclusion").
narrative_ontology:topic_domain(senior_tech_dependence, "social/economic/technology").

domain_priors:requires_active_enforcement(senior_tech_dependence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(senior_tech_dependence, tech_platforms).
narrative_ontology:constraint_beneficiary(senior_tech_dependence, digital_service_providers).
narrative_ontology:constraint_beneficiary(senior_tech_dependence, younger_caregivers).
narrative_ontology:constraint_victim(senior_tech_dependence, older_adults).
narrative_ontology:constraint_victim(senior_tech_dependence, low_tech_literacy_seniors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIGITALLY ISOLATED SENIOR (SNARE) — Trapped by cognitive decline, economic dependency on digital-only services (banking, healthcare, social benefits), and physical mobility constraints. Cannot exit the digital ecosystem without sacrificing essential services. High suppression due to lack of alternatives and cognitive barriers to learning new systems. No genuine coordination benefit — extraction is pure.
constraint_indexing:constraint_classification(senior_tech_dependence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TECH-COMPETENT SENIOR (TANGLED ROPE) — Has developed digital literacy but faces constant platform redesigns, mandatory updates, and feature deprecation. Experiences both coordination (access to services, social connection) and extraction (forced obsolescence, privacy extraction, attention capture). Exit options are constrained by necessity of digital access but moderately available through alternative platforms.
constraint_indexing:constraint_classification(senior_tech_dependence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DIGITAL PLATFORM (ROPE) — Benefits from senior userbase growth and monetization through behavioral data, attention capture, and service lock-in. Experiences the constraint as coordination: platforms standardize digital access infrastructure, solve collective action problems of service delivery. Net beneficiary with high exit optionality — can pivot business models, leave markets, alter service terms.
constraint_indexing:constraint_classification(senior_tech_dependence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FAMILY CAREGIVERS (TANGLED ROPE) — Organize around providing digital support to aging parents/relatives. Benefit from services (remote monitoring, communication) but bear extraction costs (unpaid support labor, emotional burden, responsibility for managing seniors' digital security and privacy). Active enforcement of 'helping with tech' creates asymmetric care work.
constraint_indexing:constraint_classification(senior_tech_dependence, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY SUPPORT INFRASTRUCTURE (PITON) — Telephone hotlines, in-person banking, paper mail, customer service representatives are increasingly performative. Appear to exist for senior support but operate with reduced staffing, longer wait times, and constant pressure to push users toward digital alternatives. Theater ratio indicates ritual compliance rather than functional support. Maintained through regulatory requirement and institutional inertia, not genuine effectiveness.
constraint_indexing:constraint_classification(senior_tech_dependence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some digital divide is inherent to technological change: cognitive capacities differ, neuroplasticity declines with age, and technological fluency has always required generational adaptation. This perspective risks naturalizing what is actually a contingent policy choice — the decision to digitize services without maintaining non-digital alternatives. The constraint is presented as immutable but is architecturally contingent.
constraint_indexing:constraint_classification(senior_tech_dependence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(senior_tech_dependence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(senior_tech_dependence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(senior_tech_dependence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(senior_tech_dependence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(senior_tech_dependence, TR),
    TR >= 0.70.

:- end_tests(senior_tech_dependence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Measured at high-moderate level. The constraint exhibits genuine coordination benefits (platforms enable service delivery that would otherwise be infeasible) alongside asymmetric extraction (complexity barriers, forced adoption, behavioral data harvesting, unpaid family support labor). The value reflects the mixed nature. Measurement trajectory (0.35 → 0.58 over interval) shows accumulation of extraction mechanisms as platforms mature and legacy alternatives fade. Suppression (0.68): High suppression reflects multiple barriers to exit: cognitive decline reduces learning capacity, economic dependency makes digital access mandatory, physical mobility constraints limit in-person alternatives, lack of alternative service providers creates lock-in. Suppression is not absolute (some seniors adapt, some platforms offer accessibility features) but substantial. Theater ratio (0.55): Moderate theater reflects that legacy support infrastructure is increasingly performative while policy/infrastructure actively digitalize. Not high theater (which would indicate completely abandoned ritual) but substantial (phone support lines exist but are underfunded and encourage digital alternatives). Growth trajectory (0.35 → 0.55) shows increasing gap between apparent support and actual capacity.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (institutional platform) experiences rope — solving the coordination problem of service delivery, reducing infrastructure costs, enabling global scale. The trapped victim experiences snare — mandatory adoption with no alternatives and no escape. The moderate victim experiences tangled rope — has adapted to digital ecosystem but faces constant friction from redesigns and complexity. Family caregivers experience tangled rope — both enabled by services and burdened by unpaid support labor. Legacy support infrastructure appears as piton — performative gestures of support while functional capacity transfers to digital channels. The analytical observer risks seeing mountain — naturalizing digitalization as inevitable technological progress rather than examining contingent policy choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from structural position in the extraction flow. Trapped seniors (victims with zero exit options) have d ≈ 0.95, experiencing nearly full extraction. Tech-competent seniors (victims with moderate exit options) have d ≈ 0.70, experiencing substantial but not maximum extraction. Family caregivers (organized agents bearing unpaid labor) have d ≈ 0.65, experiencing moderate-high extraction mixed with coordination benefits. Platforms (beneficiaries with complete arbitrage options) have d ≈ 0.10, experiencing negative extraction (extraction flows toward them). Legacy support infrastructure (institutional beneficiaries with performance requirements) have d ≈ 0.05. The analytical observer (analytical position) has canonical d ≈ 0.73. These values feed into the sigmoid f(d) to produce effective extractiveness chi for each perspective, generating the full range of classifications from snare to rope.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is NOT fully resolved (mandatrophy_resolved: false) because the constraint exhibits genuine coordination functions that could be emphasized to argue for rope classification, while also showing clear asymmetric extraction that supports snare classification. The resolution depends on which structural question is prioritized: (1) Are platforms genuinely solving coordination problems that have no alternative? (2) Are platforms actively maintaining suppression through design complexity to maximize extraction? If (1) is primary, the constraint is rope with extraction overlay = tangled rope. If (2) is primary, the constraint is snare disguised as coordination. The omega variables identify the empirical resolutions: simplified interface testing, service reversibility analysis, and platform feature utility measurement. Until these are resolved, both interpretations remain plausible. The mandatrophy indicates a real structural ambiguity, not a classification error — the constraint legitimately contains both coordination and extraction, and different perspectives weight them differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_decline_mechanism,
    'Is measured suppression due to biological cognitive decline in aging, or due to designed platform complexity that could be reduced?',
    'Comparative usability testing between seniors and younger users on simplified vs standard interfaces; cross-national analysis of countries with mandatory accessible design standards',
    'If biological: constraint is closer to mountain (immutable limit). If designed complexity: constraint is snare (extractive by choice). This determines whether addressing senior tech dependence requires accommodation (treating decline as given) or design justice (treating complexity as choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_decline_mechanism, empirical, 'Whether suppression is biological or designed').

omega_variable(
    service_digitalization_reversibility,
    'What proportion of senior-essential services (banking, healthcare, benefits, social contact) could realistically operate non-digital alternatives if policy required it?',
    'Cost-benefit analysis of maintaining parallel digital/non-digital service pathways; historical case studies (countries that maintain phone banking, paper billing, in-person application processes)',
    'If reversible: the constraint is policy-contingent and classification depends on willingness to maintain alternatives. If irreversible: digitalization is structural and exit from digital ecosystem entails loss of essential services — pure snare from the trapped senior perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(service_digitalization_reversibility, empirical, 'Whether digital-only service provision is economically reversible').

omega_variable(
    family_caregiver_identity_lock,
    'Do family caregivers experience tech support labor as identity-locked (constituted through family role, filial obligation) or as constrained (high-cost unpaid work they could delegate if alternatives existed)?',
    'Qualitative interviews with family caregivers; analysis of language use regarding obligation vs choice; comparison with paid tech support professionals regarding burden and identity fusion',
    'If identity-locked: family caregiver extraction is cultural/relational, not economic. Classification remains Tangled Rope but with different mechanisms. If constrained: family caregiver burden could be relieved through paid support infrastructure — constraint is economic, not relational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(family_caregiver_identity_lock, conceptual, 'Whether family caregiver tech support is identity-locked or constrained').

omega_variable(
    platform_simplicity_tradeoff,
    'Do platforms design for complexity to maximize engagement and data extraction, or does feature richness genuinely serve user needs better than simplified alternatives?',
    'Comparative analysis of platform feature usage by age cohort; A/B testing simplified vs standard interfaces; measurement of user satisfaction and task completion time',
    'If designed for extraction: platforms actively maintain suppression through complexity — snare from all non-institutional perspectives. If genuine feature value: complexity is coordination overhead — tangled rope is correctly classified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_simplicity_tradeoff, empirical, 'Whether platform complexity serves user needs or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(senior_tech_dependence, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(std_tr_t0, senior_tech_dependence, theater_ratio, 0, 0.35).
narrative_ontology:measurement(std_tr_t5, senior_tech_dependence, theater_ratio, 5, 0.45).
narrative_ontology:measurement(std_tr_t10, senior_tech_dependence, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(std_be_t0, senior_tech_dependence, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(std_be_t5, senior_tech_dependence, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(std_be_t10, senior_tech_dependence, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(senior_tech_dependence, resource_allocation).
narrative_ontology:affects_constraint(senior_tech_dependence, digital_divide_access).
narrative_ontology:affects_constraint(senior_tech_dependence, caregiving_labor_extraction).
narrative_ontology:affects_constraint(senior_tech_dependence, behavioral_data_harvesting).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(senior_tech_dependence, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
