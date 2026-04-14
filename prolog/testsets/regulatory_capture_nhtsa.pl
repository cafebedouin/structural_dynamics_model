% ============================================================================
% CONSTRAINT STORY: regulatory_capture_nhtsa
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_nhtsa, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: regulatory_capture_nhtsa
 *   human_readable: Regulatory Capture at NHTSA (National Highway Traffic Safety Administration)
 *   domain: regulatory_governance/transportation_safety
 *
 * SUMMARY:
 *   Regulatory capture at NHTSA exemplifies how an institutional constraint
 *   operates across multiple structural positions simultaneously. The
 *   National Highway Traffic Safety Administration was established to set and
 *   enforce vehicle safety standards, creating a genuine coordination
 *   function — uniform standards reduce consumer confusion and facilitate
 *   interstate commerce. However, over decades, the regulatory process has
 *   been systematically captured by automotive manufacturers through multiple
 *   mechanisms: revolving-door employment, technical expertise capture,
 *   cost-benefit analysis inflation, and comment flooding in
 *   notice-and-comment proceedings. The constraint exhibits tangled-rope
 *   characteristics: genuine coordination of baseline safety standards
 *   persists alongside systematic extraction favoring manufacturers over
 *   consumer safety. Extractiveness has risen from 0.35 (1970s, early
 *   regulation) to 0.58 (2020s, mature capture), while theater ratio has
 *   increased from 0.45 to 0.68, indicating that the formal process has
 *   become increasingly performative relative to its real function. The
 *   constraint operates across six distinct observational contexts, each
 *   perceiving a different classification, making it a diagnostic exemplar
 *   for understanding how institutional capture manifests as a tangled rope
 *   rather than pure extraction (snare) or pure coordination (rope).
 *
 * KEY AGENTS:
 *   - Automotive Manufacturers: Primary beneficiary (institutional/arbitrage) — capture NHTSA standards-setting to minimize compliance costs while preserving liability protection
 *   - Vehicle Safety Standards: Primary victim (powerless/trapped) — abstract public good that cannot exit; bears cost of weakened standards with no compensation mechanism
 *   - Consumer Protection Advocates: Secondary victim (moderate/constrained) — face high resource barriers to challenge manufacturer-favorable rulings; dependent on NHTSA data access and regulatory process
 *   - Independent Safety Researchers: Secondary beneficiary (powerful/constrained) — benefit from regulatory infrastructure and testing data but constrained by industry influence over research funding and publication channels
 *   - NHTSA Leadership: Captured institutional actor (powerful/constrained) — identity fused with industry stakeholder relations; manages coordination function while serving manufacturer interests
 *   - Congress and Executive Leadership: Oversight institution (organized/arbitrage) — nominally sets NHTSA mandate but lacks technical expertise and faces manufacturer lobbying; arbitrage between consumer safety and industry competitiveness
 *   - Analytical Observer: External analyst (analytical/analytical) — risks naturalizing capture as inevitable structural feature of regulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_nhtsa, 0.58).
domain_priors:suppression_score(regulatory_capture_nhtsa, 0.65).
domain_priors:theater_ratio(regulatory_capture_nhtsa, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_nhtsa, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_nhtsa, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regulatory_capture_nhtsa, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_nhtsa, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_nhtsa, "Regulatory Capture at NHTSA (National Highway Traffic Safety Administration)").
narrative_ontology:topic_domain(regulatory_capture_nhtsa, "regulatory_governance/transportation_safety").

domain_priors:requires_active_enforcement(regulatory_capture_nhtsa).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_nhtsa, automotive_manufacturers).
narrative_ontology:constraint_victim(regulatory_capture_nhtsa, vehicle_safety_standards).
narrative_ontology:constraint_victim(regulatory_capture_nhtsa, consumer_protection).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER SAFETY STANDARD (SNARE) — The abstract public good of vehicle safety cannot exit the regulatory capture mechanism; bears full cost of weakened standards. Consumers face maximum coercion through degraded safety requirements with minimal coordination benefit. No exit option available.
constraint_indexing:constraint_classification(regulatory_capture_nhtsa, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT SAFETY ADVOCATES (TANGLED ROPE) — Consumer groups and safety researchers benefit from access to NHTSA data and regulatory process but face high costs to challenge industry-favorable rulings (litigation, resource barriers). Genuine coordination function (setting baseline safety standards) exists alongside asymmetric extraction favoring manufacturers.
constraint_indexing:constraint_classification(regulatory_capture_nhtsa, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AUTOMOTIVE MANUFACTURERS (ROPE) — Primary beneficiary experiencing the constraint as coordination of compliance standards and liability frameworks. Manufacturers can arbitrage between NHTSA, state regulators, and international standards. Net positive extraction flow toward this institution. Coordination function is genuine but subordinated to extraction.
constraint_indexing:constraint_classification(regulatory_capture_nhtsa, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NHTSA LEADERSHIP (TANGLED ROPE) — Institutional actor whose identity has fused with industry stakeholder relations. Leadership must coordinate safety rulemaking (genuine function) while managing manufacturer compliance costs and political pressure. Constrained by revolving-door employment patterns and industry capture of technical expertise. Extraction flows both toward and from manufacturers depending on regulatory cycle phase.
constraint_indexing:constraint_classification(regulatory_capture_nhtsa, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NHTSA FORMAL PROCESS (PITON) — Notice-and-comment rulemaking, cost-benefit analysis, technical testing protocols persist as performative theater. The process generates legitimate-appearing regulatory outputs while manufacturers systematically undermine stringency through comment flooding, expert testimony manipulation, and cost projection inflation. Theater maintains institutional legitimacy; function has degraded to ritual.
constraint_indexing:constraint_classification(regulatory_capture_nhtsa, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, regulatory capture appears as an immutable structural consequence of concentrated industry power meeting information asymmetries: whenever regulators depend on regulated industry for technical expertise and political support, capture is inevitable. However, this perspective risks naturalizing a contingent institutional arrangement as physical law.
constraint_indexing:constraint_classification(regulatory_capture_nhtsa, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_nhtsa_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_nhtsa, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_nhtsa, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_nhtsa, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_nhtsa, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_nhtsa_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. Manufacturers capture baseline safety standards, reducing their compliance costs relative to uncontaminated regulation. The extraction is not total (complete capture would yield ε ≈ 0.75+) because NHTSA retains some functional autonomy, consumer groups can still challenge rulings, and international standards provide downward pressure. The trajectory shows acceleration — capture deepened from 1980s to 2020s as manufacturers invested in regulatory expertise and lobbying infrastructure. Suppression (0.65): Moderate-high. Barriers to independent safety advocacy include: litigation costs, technical expertise concentration in industry, publication bias against negative vehicle safety findings, and career risk for NHTSA staff who challenge industry consensus. However, suppression is not total — some groups successfully litigate, independent researchers publish, and congressional oversight exists. Theater ratio (0.68): Moderate-high and increasing. NHTSA's formal notice-and-comment process, cost-benefit analysis, and technical testing generate legitimate-appearing regulatory outputs, but manufacturers systematically manipulate the process through comment flooding, expert testimony, and inflated cost projections. The theater has increased as manufacturers professionalized capture efforts, while the actual function of independent safety standard-setting has degraded to manufacturer accommodation with cosmetic regulation.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap exists between the manufacturer perspective (rope: coordination of compliance standards, genuine function) and the consumer safety perspective (snare: degraded standards with no exit). NHTSA leadership occupies an intermediate position (tangled rope) — they coordinate genuine safety rulemaking while serving manufacturer interests through capture. The independent advocates see tangled rope (mixed function and extraction), while the formal regulatory process itself appears as piton (theater masking degraded function). The analytical observer risks the false summit of seeing capture as inevitable (mountain) — naturalization of what is actually a contingent institutional arrangement sustained by specific career incentives and information asymmetries. These gaps reveal that the classification of the same constraint depends entirely on the observer's structural position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are determined by each agent's structural position relative to the extraction flow. Manufacturers are beneficiaries with arbitrage exit options (d ≈ 0.15, low f(d)) — they experience the constraint as coordination and can move to other regulators if needed. Consumer safety is a victim with no exit (d ≈ 0.95, high f(d)) — the abstract public good cannot leave and bears maximum extraction. Moderate safety advocates face constrained exit (d ≈ 0.70, moderate-high f(d)) — they can litigate or protest but at high cost. NHTSA leadership faces capture-specific directionality: they are nominally regulators but are identity-locked to industry stakeholder relations, making their d intermediate (≈ 0.55) — they maintain some regulatory function (beneficiary-adjacent) while serving manufacturer interests (victim-adjacent). The perspectival gap reflects these directionality differences: manufacturers see rope (coordination), NHTSA leadership sees tangled rope (mixed coordination/extraction), safety advocates see snare (pure extraction), and the analytical observer risks seeing mountain (inevitable capture).
 *
 * MANDATROPHY ANALYSIS:
 *   REGULATORY CAPTURE MANDATROPHY: The constraint's existence relies on suppressing the alternative frame — that safety standards SHOULD prioritize consumer protection over manufacturer cost minimization. The tangled-rope classification resolves the mandatrophy by acknowledging that genuine coordination (baseline standards) and systematic extraction (manufacturer cost minimization) coexist in the same institutional structure. The constraint is NOT a snare (pure extraction) because safety standards do serve a real coordination function — they reduce consumer confusion and facilitate interstate commerce. The constraint is NOT a rope (pure coordination) because manufacturers systematically capture the standards-setting process to minimize their own costs relative to what independent regulation would require. The tangled rope captures both dynamics: the institutional mechanism simultaneously solves a coordination problem (uniform safety standards) and enables extraction (manufacturer capture of the standards-setting process). Breaking the capture would NOT eliminate the coordination function — reforming NHTSA with independent expertise, insulated funding, and outcome accountability would preserve or improve coordination while reducing extraction. The mandatrophy is resolved by recognizing that the institutional form (regulatory agency with manufacturer input) can serve either coordination or extraction depending on the capture level and structural safeguards.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_vs_coordination_boundary,
    'At what point does legitimate manufacturer input in rulemaking become regulatory capture? How does one distinguish coordination from extraction?',
    'Comparative analysis of rule stringency across jurisdictions with different capture levels; longitudinal tracking of proposed vs final rule stringency; cost projections vs actual compliance costs; safety outcome correlations with rule stringency',
    'If boundary is ambiguous: capture appears inevitable and debate becomes rhetorical. If boundary is detectable: capture mechanisms become addressable through structural reform (independent expertise, funding insulation, outcome accountability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_vs_coordination_boundary, empirical, 'Threshold distinguishing manufacturer influence from regulatory capture').

omega_variable(
    revolving_door_causality,
    'Does industry employment of former NHTSA officials cause regulatory capture, or do captured regulators self-select into industry employment?',
    'Matching analysis comparing regulatory voting patterns of officials before and after departure to industry; career trajectory analysis of regulators with different tenure lengths; difference-in-differences estimation around regulatory transitions',
    'If causal (employment → capture): revolving door itself is the extraction mechanism. If selection (capture → employment): employment is symptom of pre-existing capture. If bidirectional: capture is mutually reinforced through career incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolving_door_causality, empirical, 'Causality of revolving-door career effects on capture').

omega_variable(
    international_standard_coordination,
    'Do global harmonization efforts (Geneva agreements, international standards bodies) represent genuine efficiency coordination or manufacturer arbitrage?',
    'Analysis of voting patterns in international standards bodies; correlation between industry concentration and international standard stringency; comparative stringency analysis across jurisdictions with different capture levels',
    'If coordination: international standards provide downward pressure on capture. If arbitrage: manufacturers coordinate globally to establish weak baseline, then capture individual regulators into compliance. If mixed: both mechanisms operate simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_standard_coordination, empirical, 'Whether international standards coordinate or enable manufacturer arbitrage').

omega_variable(
    identity_lock_regulatory_class,
    'To what extent is NHTSA capture maintained through identity-locking of leadership to industry stakeholder relations rather than through structural material dependency?',
    'Post-capture institutional analysis: if NHTSA leadership transitions to individuals with no industry ties, does capture mechanism persist? Do leadership identity narratives emphasize ''working with industry'' as constitutive of regulatory role? Do captured leaders perceive captured outcomes as correct rather than forced?',
    'If identity-locked: breaking capture requires identity frame shift (difficult, generational). If structurally forced: capacity exists for immediate remediation through funding/staffing reforms. If both: identity lock may persist even after structural barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_regulatory_class, conceptual, 'Role of identity fusion in maintaining regulatory capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_nhtsa, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nhtsa_tr_t0, regulatory_capture_nhtsa, theater_ratio, 0, 0.45).
narrative_ontology:measurement(nhtsa_tr_t5, regulatory_capture_nhtsa, theater_ratio, 5, 0.58).
narrative_ontology:measurement(nhtsa_tr_t10, regulatory_capture_nhtsa, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(nhtsa_be_t0, regulatory_capture_nhtsa, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nhtsa_be_t5, regulatory_capture_nhtsa, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(nhtsa_be_t10, regulatory_capture_nhtsa, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_nhtsa, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(regulatory_capture_nhtsa, 0.12).
narrative_ontology:affects_constraint(regulatory_capture_nhtsa, vehicle_safety_information_asymmetry).
narrative_ontology:affects_constraint(regulatory_capture_nhtsa, automotive_lobbying_structural_power).

% DUAL FORMULATION NOTE:
% NHTSA capture is downstream of manufacturer structural power in automobile markets but represents a distinct constraint focused on the regulatory process itself. The upstream constraints concern market structure and information flows; this constraint concerns how regulatory institutions translate market power into policy capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_nhtsa, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
