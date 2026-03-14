% ============================================================================
% CONSTRAINT STORY: physician_geographic_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_physician_geographic_distribution, []).

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
 *   constraint_id: physician_geographic_distribution
 *   human_readable: Physician Geographic Distribution Constraint
 *   domain: healthcare_policy/health_equity
 *
 * SUMMARY:
 *   Physician geographic distribution — the systematic concentration of
 *   medical professionals in urban, high-income regions and the corresponding
 *   deficit in rural and low-income areas — operates as a tangled
 *   coordination-extraction hybrid. On the coordination side, the constraint
 *   reflects genuine economic efficiency: urban centers have larger patient
 *   populations, greater disease complexity, and better infrastructure for
 *   specialized and research medicine. On the extraction side, the same
 *   distribution mechanism deprives rural populations of basic healthcare
 *   access, forcing travel for routine care and accepting elevated health
 *   outcome disparities as the cost of medical professionalization. The
 *   constraint exhibits different structural characters from different
 *   positions: to rural communities it is a snare with no exit; to urban
 *   medicine it is rope with substantial benefits; to reform advocates it is
 *   a solvable problem with policy sunset mechanisms; to the medical
 *   licensing system it is an inertial piton; to the civilizational observer
 *   it risks appearing as a natural law of economics. The extractiveness has
 *   risen from 0.42 to 0.58 over the measurement interval (20 years) as
 *   specialization has deepened and urban centers have consolidated
 *   advantages, while policy interventions (loan forgiveness, telehealth)
 *   have remained piecemeal. The theater ratio (0.58) reflects that much of
 *   the justification invokes professional quality standards and
 *   infrastructure requirements, but significant portions of these arguments
 *   are performative — sufficient care could be delivered in rural areas with
 *   lower credentialism and task-shifting, yet credentialing rules and
 *   specialty board monopolies maintain artificial barriers.
 *
 * KEY AGENTS:
 *   - Rural Populations: Primary victim (powerless/trapped) — bear health outcome disparities with no exit option; lack infrastructure, population density, economic opportunity to attract physicians
 *   - Urban Medical Institutions: Primary beneficiary (institutional/arbitrage) — concentrate specialist resources, research funding, teaching infrastructure; possess arbitrage options unavailable to rural counterparts
 *   - Rural Physicians: Secondary victim (moderate/constrained) — face income penalty, isolation, specialty backup limitations, family education barriers; also benefit from autonomy and continuity
 *   - Policy Reform Coalition: Organized agents (organized/constrained) — telehealth advocates, loan forgiveness programs, health equity networks perceive solvable problem with sunset mechanisms
 *   - Medical Licensing and Credentialing System: Institutional actor (institutional/arbitrage) — maintains task-shifting barriers and specialty monopolies through credentialism; perpetuates constraint through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing maldistribution as immutable consequence of market economics or human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(physician_geographic_distribution, 0.58).
domain_priors:suppression_score(physician_geographic_distribution, 0.62).
domain_priors:theater_ratio(physician_geographic_distribution, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(physician_geographic_distribution, extractiveness, 0.58).
narrative_ontology:constraint_metric(physician_geographic_distribution, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(physician_geographic_distribution, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(physician_geographic_distribution, tangled_rope).
narrative_ontology:human_readable(physician_geographic_distribution, "Physician Geographic Distribution Constraint").
narrative_ontology:topic_domain(physician_geographic_distribution, "healthcare_policy/health_equity").

domain_priors:requires_active_enforcement(physician_geographic_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(physician_geographic_distribution, urban_medical_institutions).
narrative_ontology:constraint_beneficiary(physician_geographic_distribution, high_income_regions).
narrative_ontology:constraint_victim(physician_geographic_distribution, rural_populations).
narrative_ontology:constraint_victim(physician_geographic_distribution, low_income_regions).
narrative_ontology:constraint_victim(physician_geographic_distribution, patient_health_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL COMMUNITIES (SNARE) — Cannot exit the geographic constraint. Structural barriers to physician access are immutable from this position: lack of infrastructure, low population density, limited economic opportunity, geographic isolation. Rural residents bear the extraction (delayed care, preventive medicine gaps, health outcome disparities) with no genuine exit option and minimal coordination benefit.
constraint_indexing:constraint_classification(physician_geographic_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL PHYSICIANS (TANGLED ROPE) — Constrained by income differential, geographic isolation, limited specialty backup, educational disadvantages for children, and career advancement barriers. Simultaneously benefit from rural practice through autonomy, patient continuity, and community embeddedness. Extraction is asymmetric but real; coordination is genuine but unequal.
constraint_indexing:constraint_classification(physician_geographic_distribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN MEDICAL INSTITUTIONS (ROPE) — Experience the constraint as coordination: geographic concentration enables specialization, research infrastructure, teaching hospitals, and economic efficiency. Net beneficiaries of the distribution pattern. Can exit through relocation decisions with minimal cost — possess arbitrage options unavailable to rural populations.
constraint_indexing:constraint_classification(physician_geographic_distribution, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICY REFORM ADVOCATES (SCAFFOLD) — Organized agents (medical associations, health equity networks, telehealth advocates, loan forgiveness programs) perceive the constraint as a solvable coordination failure with built-in sunset mechanisms. Telehealth, physician assistants, nurse practitioners, and loan forgiveness programs create alternative access pathways. Suppression is declining as policy interventions mature — this is low effective extraction because the coalition perceives and is building exit mechanisms.
constraint_indexing:constraint_classification(physician_geographic_distribution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEDICAL LICENSING SYSTEM (PITON) — The MD/licensing requirement perpetuates the geographic constraint through institutionalized credentialism. State licensure, specialty board certification, and hospital credentialing rules maintain high barriers to task-shifting and alternative provider models, even when those alternatives could deliver equivalent care in underserved regions. The system performs legitimacy (quality assurance) but much of its effect is theatrical — the actual function (ensuring competence) could be achieved with lower geographic friction. Institutional inertia maintains barriers that would not survive scrutiny.
constraint_indexing:constraint_classification(physician_geographic_distribution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, some geographic maldistribution is inherent to economic incentive structures: physicians allocate to high-income regions through rational economic choice. This perspective naturalizes the distribution as an immutable consequence of capitalism and human nature. However, empirical evidence contradicts the mountain classification — countries with universal healthcare, salary equalization, and state placement systems achieve far more equitable geographic distribution, revealing the maldistribution as contingent on institutional design, not natural law.
constraint_indexing:constraint_classification(physician_geographic_distribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(physician_geographic_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(physician_geographic_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(physician_geographic_distribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(physician_geographic_distribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(physician_geographic_distribution, TR),
    TR >= 0.70.

:- end_tests(physician_geographic_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint concentrates physician resources and the attendant professional status, research opportunities, and income into urban centers, depriving rural populations. The extraction is asymmetric and persistent. However, extractiveness is not as severe as pure snare (0.70+) because some coordination function is genuine — urban concentration does enable specialization and efficiency. The rise from 0.42 to 0.58 over the interval reflects increasing specialization and consolidation of urban advantages, while rural alternatives (telehealth, mid-level providers) remain nascent. Suppression (0.62): Significant. Barriers to rural physician distribution include: income differential ($50-150k less in rural practice), geographic isolation, limited specialty backup, school quality differences, spouse employment limitations, perception of limited professional growth. These are high-friction but not absolutely immutable — physicians can choose rural practice, but at substantial cost. Suppression would be higher (0.75+) if barriers were total; it is moderate because some physicians do choose rural practice despite constraints. Theater ratio (0.58): Moderate-high. Professional quality standards and infrastructure requirements are partially legitimate but partially performative. Much of the credentialing infrastructure (specialty board certification, hospital credentialing, state licensure variance) maintains artificial barriers that don't track actual clinical competence. Task-shifting to NPs and PAs demonstrates that many services require lower credentialism than current monopolistic restrictions demand. Theater has risen slightly over the interval as specialization increases and credentialing becomes more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Rural communities experience a snare (pure extraction with no exit). Urban institutions experience rope (coordination with benefits). Policy advocates experience scaffold (temporary problem being solved). The medical licensing system experiences piton (degraded ritual maintained through inertia). Rural physicians experience tangled rope (mixed coordination and extraction). The analytical observer risks mountain (naturalizing as economic necessity). This gap reveals that the constraint is not a uniform phenomenon — it is multiple overlapping mechanisms: genuine economic efficiency coordination, extractive credentialing monopoly, institutional inertia in licensing systems, rational individual choice aggregating to collective harm, and policy-solvable maldistribution. No single classification captures all mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by agent position relative to the extraction flow. Rural populations (powerless/trapped) experience full extraction — high d, high f(d), high chi. Urban institutions (institutional/arbitrage) are beneficiaries — low d, negative or low f(d), low chi. Rural physicians (moderate/constrained) are partly trapped and partly benefiting — moderate d, moderate f(d), moderate chi. Policy reformers (organized/constrained) have agency and see exit pathways — lower d despite constraint, moderate f(d). The licensing system (institutional/arbitrage) is net beneficiary of the constraint's perpetuation — low d. The analytical observer (analytical/analytical) derives d from the structural relationship — if seeing the constraint as natural law, d is high (trapped by nature); if seeing as policy-solvable, d is moderate (constrained by contingent arrangements). The perspectival gap shows different d values for the same structural phenomenon, revealing that directionality is genuinely relative to observer position, not an absolute property of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that all six types are legitimate perspectival readings of overlapping but distinct mechanisms. The snare (pure extraction from rural populations) is real from their position. The rope (coordination with urban benefits) is real from that position. The scaffold (policy-solvable problem) is real for reform advocates with agency and exit pathways. The piton (inertial credentialing system) is real as an institutional phenomenon. The tangled rope (mixed extraction/coordination for moderate rural physicians) is real for that actor. The mountain (natural law of economics) is a false summit — the analytical observer risks naturalizing contingent institutional design. The constraint is not 'actually' one type; it is the presheaf of all six types over different observer positions. Understanding this prevents false solutions: you cannot solve a snare with scaffold policy tools if the victims remain trapped; you cannot justify doing nothing if the analytical observer's mountain classification is a false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    income_substitution_adequacy,
    'Would income equalization (salary parity across regions) be sufficient to achieve geographic distribution without coercive placement?',
    'Natural experiments comparing jurisdictions with and without salary equalization; longitudinal tracking of physician location choices pre/post compensation reform',
    'If sufficient: constraint is primarily economic extraction (snare); policy solution is loan forgiveness + salary parity. If insufficient: constraint reflects deeper factors (lifestyle preferences, family considerations, perception of professional opportunity) requiring different intervention levers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(income_substitution_adequacy, empirical, 'Whether income equalization alone drives geographic redistribution').

omega_variable(
    telehealth_access_sufficiency,
    'Can telehealth plus distributed mid-level providers (NPs, PAs) adequately substitute for onsite physicians in rural areas, or do certain patient populations and conditions require geographic colocation?',
    'Clinical outcome comparison: remote-enabled care vs in-person-only care by condition type and patient demographic; identification of care domains where colocation is irreplaceable',
    'If substitution is sufficient: scaffold sunset is viable — policy can phase MD concentration toward alternative delivery models. If insufficient: constraint remains structurally necessary (mountain/rope) and reform must focus on recruitment/retention rather than task-shifting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(telehealth_access_sufficiency, empirical, 'Whether telehealth and mid-level providers can substitute for onsite physicians').

omega_variable(
    coercive_placement_sustainability,
    'Can government-mandated physician placement in underserved areas maintain compliance and quality of care over time, or does coercion generate silent non-compliance and clinical shortcuts?',
    'Longitudinal outcome tracking in jurisdictions with mandatory placement programs; measurement of provider satisfaction, retention, disciplinary incidents, and patient outcomes; comparison to volunteer incentive models',
    'If sustainable: coercive enforcement is viable policy (Snare classification is mitigated by stronger policy enforcement). If unsustainable: suppression persists even under enforcement, revealing the constraint as deeper than policy levers can address (stronger snare classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercive_placement_sustainability, empirical, 'Whether mandatory physician placement can sustain compliance and care quality').

omega_variable(
    extraction_beneficiary_clarity,
    'Who actually benefits from the current geographic maldistribution? Is it primarily urban medical institutions, or is the extraction more diffuse (urban patients, pharmaceutical companies, specialist networks, academic researchers)?',
    'Network analysis of institutional coupling and financial flows; identification of which actors accumulate resources under the current distribution; measurement of economic rents captured by urban medicine sector',
    'If benefits are concentrated: snare targeting specific institutional actors is tractable (regulatory intervention on urban concentration). If benefits are diffuse: constraint is more complex than pure snare — it may be multiple overlapping extraction mechanisms requiring different policy levers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_clarity, empirical, 'Clarity on which institutions actually benefit from geographic maldistribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(physician_geographic_distribution, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phys_geo_tr_t0, physician_geographic_distribution, theater_ratio, 0, 0.48).
narrative_ontology:measurement(phys_geo_tr_t10, physician_geographic_distribution, theater_ratio, 10, 0.54).
narrative_ontology:measurement(phys_geo_tr_t20, physician_geographic_distribution, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(phys_geo_be_t0, physician_geographic_distribution, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(phys_geo_be_t10, physician_geographic_distribution, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(phys_geo_be_t20, physician_geographic_distribution, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(physician_geographic_distribution, resource_allocation).
narrative_ontology:affects_constraint(physician_geographic_distribution, healthcare_access_disparity).
narrative_ontology:affects_constraint(physician_geographic_distribution, medical_licensing_monopoly).
narrative_ontology:affects_constraint(physician_geographic_distribution, rural_hospital_closure).

% DUAL FORMULATION NOTE:
% Physician geographic distribution is downstream of medical credentialing monopolies and upstream of healthcare access disparities. The constraint's extractiveness depends partly on whether alternative provider models (NPs, PAs, community health workers) are permitted — if task-shifting is allowed, extractiveness drops and scaffold perspective strengthens. Current decomposition treats geographic distribution as primary; alternative decomposition would separate credentialing barriers (piton) from distribution outcomes (tangled rope) as distinct constraints in a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(physician_geographic_distribution, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
