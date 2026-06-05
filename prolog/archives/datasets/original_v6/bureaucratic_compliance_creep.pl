% ============================================================================
% CONSTRAINT STORY: bureaucratic_compliance_creep
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_compliance_creep, []).

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
 *   constraint_id: bureaucratic_compliance_creep
 *   human_readable: Bureaucratic Compliance Creep
 *   domain: administrative/regulatory
 *
 * SUMMARY:
 *   Bureaucratic compliance creep is a constraint where regulatory
 *   requirements accumulate faster than they are removed, creating an
 *   expanding overhead burden on regulated organizations. The constraint
 *   exhibits a classic hybrid structure: genuine coordination functions
 *   (preventing catastrophic failures, protecting public interests, ensuring
 *   fair competition) coexist with extraction mechanisms (regulatory capture
 *   by incumbents, compliance service industries, complexity barriers to
 *   entry). The theater ratio has increased from 0.42 to 0.68 over the
 *   measurement interval, indicating that compliance activity increasingly
 *   consists of documentation and reporting that serve archival rather than
 *   enforcement purposes. The base extractiveness has increased from 0.32 to
 *   0.58, suggesting that both functional and performative requirements have
 *   accumulated. Different agents experience the same constraint radically
 *   differently: frontline operators see an inescapable trap; managers see a
 *   mixed coordination-extraction trade-off; regulatory agencies see
 *   coordination; large multinationals see a barrier protecting their market
 *   position; the compliance bureaucracy sees degraded ritual maintained
 *   through inertia; and the analytical observer sees a constraint that
 *   serves legitimate functions while simultaneously enabling rent
 *   extraction.
 *
 * KEY AGENTS:
 *   - Frontline Operators: Primary victims (powerless/trapped) — face non-negotiable compliance requirements with no exit option; accumulating documentation and reporting burden
 *   - Mid-Level Managers: Secondary victims (moderate/constrained) — constrained by compliance overhead; also benefit from safety/quality coordination systems; can exit through career mobility but at cost
 *   - Regulatory Agencies: Primary beneficiaries (institutional/arbitrage) — experience compliance systems as coordination mechanisms enabling their mandates; can arbitrage between requirements
 *   - Large Multinational Enterprises: Secondary beneficiaries (powerful/mobile) — bear absolute compliance costs but experience lower marginal cost per unit; benefit from complexity barriers that disadvantage smaller competitors
 *   - Compliance Service Industry: Secondary beneficiaries (institutional/arbitrage) — consulting firms, software vendors, audit services profit from requirement expansion and complexity
 *   - Compliance Bureaucracy: Institutional actor (institutional/arbitrage) — maintains procedures through inertia; performs archival function rather than enforcement; sees own system as degraded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_compliance_creep, 0.58).
domain_priors:suppression_score(bureaucratic_compliance_creep, 0.65).
domain_priors:theater_ratio(bureaucratic_compliance_creep, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_compliance_creep, extractiveness, 0.58).
narrative_ontology:constraint_metric(bureaucratic_compliance_creep, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bureaucratic_compliance_creep, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_compliance_creep, tangled_rope).
narrative_ontology:human_readable(bureaucratic_compliance_creep, "Bureaucratic Compliance Creep").
narrative_ontology:topic_domain(bureaucratic_compliance_creep, "administrative/regulatory").

domain_priors:requires_active_enforcement(bureaucratic_compliance_creep).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_compliance_creep, regulatory_agencies).
narrative_ontology:constraint_beneficiary(bureaucratic_compliance_creep, compliance_service_providers).
narrative_ontology:constraint_victim(bureaucratic_compliance_creep, regulated_organizations).
narrative_ontology:constraint_victim(bureaucratic_compliance_creep, operational_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE OPERATOR (SNARE) — Compliance requirements accumulate without relief. Exit is impossible: non-compliance results in organizational penalties, personal liability, and license revocation. The operator bears full cost of compliance overhead without capacity to reduce it. Maximum extraction from an agent with no alternatives.
constraint_indexing:constraint_classification(bureaucratic_compliance_creep, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL MANAGER (TANGLED ROPE) — Constrained by the requirement to maintain regulatory compliance while managing operations. Benefits from compliance systems that demonstrate organizational safety/quality but bears significant overhead in documentation, reporting, and personnel allocation. Can exit through career relocation but at substantial cost. Mixed coordination and extraction.
constraint_indexing:constraint_classification(bureaucratic_compliance_creep, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AGENCY (ROPE) — Experiences the constraint as coordination: compliance requirements create a communication and accountability mechanism that enables public health, safety, and fairness objectives. Can arbitrage between regulatory standards (choosing which requirements to enforce). Net beneficiary experiencing low or negative extraction.
constraint_indexing:constraint_classification(bureaucratic_compliance_creep, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE MULTINATIONAL (TANGLED ROPE) — Powerful agent with mobile exit options (relocate operations, lobbying influence, regulatory arbitrage across jurisdictions). Both benefits (complexity barrier to entry protects market position) and bears costs (absolute compliance burden is substantial in dollar terms, though marginal cost per unit is lower than for competitors). Genuine coordination function mixed with asymmetric extraction that privileges incumbents over entrants.
constraint_indexing:constraint_classification(bureaucratic_compliance_creep, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPLIANCE BUREAUCRACY (PITON) — Compliance procedures persist through institutional inertia. Many specific requirements have lost their original justification (outdated regulations remain on the books; reporting mechanisms serve archival rather than enforcement purposes). Theater ratio is high because compliance activity is often performative rather than functionally necessary. Agencies maintain requirements because alternatives haven't been built, not because they work optimally. The system performs its own memory rather than its stated function.
constraint_indexing:constraint_classification(bureaucratic_compliance_creep, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global civilizational view, compliance systems serve genuine coordination functions (preventing tragedy of the commons, information asymmetries, externalities) while simultaneously extracting rents through regulatory capture, compliance service markets, and complexity barriers. The constraint is neither pure coordination (rope) nor pure extraction (snare) — it is a hybrid where the extraction mechanism parasitizes the legitimate coordination function.
constraint_indexing:constraint_classification(bureaucratic_compliance_creep, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_compliance_creep_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_compliance_creep, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_compliance_creep, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_compliance_creep, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_compliance_creep, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_compliance_creep_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Compliance creep extracts through several mechanisms: time burden (documentation, reporting, training), capital burden (systems, monitoring equipment), opportunity cost (resources diverted from productive activity), and market barriers (complexity disadvantages entrants). However, the extraction is not as severe as pure snare (0.72+) because genuine coordination functions exist — preventing catastrophic failures, protecting public health, ensuring fair competition. The increase from 0.32 to 0.58 over 20 years reflects both real requirement accumulation and theater expansion. Suppression (0.65): High. Organizations cannot easily exit: non-compliance results in penalties, license revocation, legal liability. Some exit options exist (geographic relocation, lobbying influence for large firms) but are constrained by practical factors. Smaller organizations face near-total suppression; larger organizations have more mobility. Theater ratio (0.68): High-moderate. A significant portion of compliance activity is performative: documentation maintained for regulatory appearance rather than operational necessity, reporting that is filed and rarely reviewed, procedures that persist after their original justification has become obsolete. The increase from 0.42 to 0.68 reflects the expansion of documentation and reporting requirements outpacing substantive regulatory changes. Claimed type (Tangled Rope): The constraint simultaneously coordinates and extracts. Genuine coordination function prevents externalities and information asymmetries. Asymmetric extraction benefits incumbents (complexity barriers reduce competition) and compliance service industries. Active enforcement is required: regulatory agencies must continuously add and update requirements; the system does not persist passively.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between agents who benefit from coordination (regulatory agencies, the public interest in catastrophic failure prevention) and agents who bear accumulating overhead (operators, smaller organizations). The secondary gap is between small/powerless organizations experiencing snare-like accumulation and large/powerful organizations able to arbitrage or lobby against requirements. The third gap is between lived experience of operators (snare: inescapable, accumulating) and the legitimate coordination function the system serves (rope: prevents bad outcomes). The analytical position must hold both: the system genuinely prevents catastrophic failures AND accumulates theatre and extraction beyond functional necessity. This is not a false summit (mountain that would be revealed as snare) — it is a genuine hybrid (tangled rope) where no single perspective captures the full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent's structural position. Frontline operators are victims with trapped exit options: d ≈ 0.95 (maximum target), experiencing maximum extraction. Mid-level managers are victims with constrained exit options: d ≈ 0.70 (high target), experiencing significant extraction with some agency. Regulatory agencies are beneficiaries with arbitrage options: d ≈ 0.10 (beneficiary), experiencing low or negative extraction — the constraint subsidizes their capacity to execute their mandate. Large multinationals are mixed: officially victims (bear compliance costs) but beneficiaries of complexity barriers: d ≈ 0.45 (symmetric), experiencing moderate extraction masked by market position protection. Compliance service providers are clear beneficiaries: d ≈ 0.05, experiencing strong subsidy from requirement growth. The analytical observer at institutional power with analytical exit options derives d ≈ 0.72 (moderate target), reflecting the honest position that the constraint serves some functions while extracting asymmetrically. The engine will derive these values from the beneficiary/victim declarations and exit options; the directionality overrides are not needed because the structural data clearly reflects the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that the constraint is genuinely a tangled rope — not a rope falsely labeled as tangled, and not a snare hidden behind a coordination narrative. The coordination function is real and substantial: compliance systems do prevent many failures, protect public interests, and enable fair information flows. The extraction is equally real: requirement accumulation creates rents for compliance service industries, market barriers that protect incumbents, and performative overhead that serves no protective function. The hybrid classification is not a hedge — it is a precise description of a constraint that solves coordination problems while enabling extraction mechanisms. The false summit risk (natural law framing) would be 'compliance requirements are an immutable feature of modern governance.' The structural data contradicts this: requirements are human choices subject to design revision, removal, and replacement. Theater creep (0.42 → 0.68) indicates that performative elements are growing faster than functional ones, suggesting opportunity for reform through requirement pruning and process simplification. The mandatrophy is fully resolved by the tangled rope classification with measured beneficiary/victim declarations and increasing theater ratio that enables future monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_theater_threshold,
    'How much of measured compliance activity is genuine risk mitigation vs. performative compliance theater?',
    'Regression analysis of compliance expenditure against downstream outcomes (actual harm prevented); comparison of complaint investigation rates before vs. after regulatory changes; analysis of organizations using minimal vs. maximal compliance practices and their actual safety/quality records',
    'If theater ratio is actually ≥0.80: constraint should reclassify toward Piton. If theater ratio is actually ≤0.40: constraint should reclassify toward Rope. Current estimate of 0.68 reflects genuine mixed function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_theater_threshold, empirical, 'Proportion of compliance activity that is performative vs. functionally necessary').

omega_variable(
    regulatory_capture_mechanism,
    'Do large incumbents shape compliance requirements specifically to increase barriers to entry?',
    'Analysis of regulatory proposal origins and beneficiary patterns; comparison of compliance cost as percentage of revenue for market leaders vs. entrants; historical tracking of requirement complexity changes and industry consolidation rates',
    'If systematic capture is confirmed: extractiveness should increase toward 0.68+; asymmetric extraction component dominates. If capture is minimal: extractiveness should decrease toward 0.45; constraint functions as mixed coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Whether large incumbents systematically shape requirements to disadvantage competitors').

omega_variable(
    exit_option_feasibility,
    'For regulated organizations, how real is the exit option of relocating operations to lower-compliance jurisdictions?',
    'Analysis of actual organization relocation patterns; survey of regulatory arbitrage capabilities by sector and firm size; identification of practical constraints to geographic exit (supply chains, customer bases, talent availability)',
    'If exit is genuinely available: many agents should reclassify from trapped to constrained or mobile. If exit is illusory: suppression value should increase because exit options are formally available but structurally blocked. Current assessment assumes constrained/mobile options for larger actors; trapped for smaller ones.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_feasibility, empirical, 'Whether geographic exit to lower-compliance jurisdictions is practically available').

omega_variable(
    compliance_creep_measurement,
    'What is the actual rate of new requirement addition vs. removal? Is ''creep'' a documented phenomenon or a perception bias?',
    'Historical audit of regulatory requirement counts by sector and year; analysis of requirement lifecycle (creation, modification, repeal); tracking of time-to-compliance burden over multi-year periods for fixed-scope organizations',
    'If net requirement growth is ≥2% annually: creep is structural and extractiveness should be stable at 0.58+. If net growth is <1% annually: creep may be illusory, and extractiveness should decrease. Measurements show theater_ratio increasing more than base_extractiveness, suggesting creep is primarily in theater (documentation, reporting) rather than in new substantive requirements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_creep_measurement, empirical, 'Net annual rate of new compliance requirements vs. removals').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_compliance_creep, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcc_tr_t0, bureaucratic_compliance_creep, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bcc_tr_t10, bureaucratic_compliance_creep, theater_ratio, 10, 0.55).
narrative_ontology:measurement(bcc_tr_t20, bureaucratic_compliance_creep, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(bcc_be_t0, bureaucratic_compliance_creep, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(bcc_be_t10, bureaucratic_compliance_creep, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(bcc_be_t20, bureaucratic_compliance_creep, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_compliance_creep, enforcement_mechanism).
narrative_ontology:affects_constraint(bureaucratic_compliance_creep, regulatory_capture).
narrative_ontology:affects_constraint(bureaucratic_compliance_creep, market_concentration_through_compliance_barriers).
narrative_ontology:affects_constraint(bureaucratic_compliance_creep, organizational_innovation_suppression).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
