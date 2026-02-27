% ============================================================================
% CONSTRAINT STORY: ny_private_school_discount
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ny_private_school_discount, []).

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
 *   constraint_id: ny_private_school_discount
 *   human_readable: Discount-for-Data Scheme in Private Schools
 *   domain: economic/education
 *
 * SUMMARY:
 *   A new private school startup in New York offers a 42% tuition discount in
 *   exchange for extensive parental data provision, including children's
 *   behavioral profiles, academic performance, health information, family
 *   demographics, and device usage patterns. The scheme targets low-income
 *   families priced out of private education, framing data exchange as a
 *   'market solution' to education access. The constraint exhibits high
 *   extractiveness (0.58) because the discount is structurally coercive — it
 *   exploits education insecurity to capture data that is then monetized
 *   through broker networks for behavioral profiling and secondary targeting.
 *   Suppression is high (0.68) because families face genuine barriers to
 *   exit: public school alternatives may be inadequate, relocation is costly,
 *   and the legal architecture governing private schools creates a regulatory
 *   gap. Theater ratio is low (0.35) — the extraction is functionally real,
 *   not ceremonial — because the school makes no pretense of data
 *   minimization or protection; data monetization is the actual business
 *   model.
 *
 * KEY AGENTS:
 *   - Low-Income Families: Primary victims (powerless/trapped) — face genuine tuition barriers; the discount is economically coercive given education access constraints
 *   - Child Privacy Commons: Diffuse victim (powerless/trapped) — unorganized collective interest in child data protection; cannot negotiate or withdraw retroactively
 *   - School Operator: Primary beneficiary (institutional/arbitrage) — solves operational costs through data monetization; experiences regulatory arbitrage
 *   - Data Broker Ecosystem: Secondary beneficiary (institutional/arbitrage) — purchases and re-monetizes child behavioral data for downstream targeting
 *   - Parent Advocacy Coalition: Organized responder (organized/constrained) — sees mixed coordination failure and extraction; has partial exit options
 *   - State Regulators: Institutional theater (institutional/arbitrage) — maintain fiction of oversight while private schools operate under minimal surveillance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ny_private_school_discount, 0.58).
domain_priors:suppression_score(ny_private_school_discount, 0.68).
domain_priors:theater_ratio(ny_private_school_discount, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ny_private_school_discount, extractiveness, 0.58).
narrative_ontology:constraint_metric(ny_private_school_discount, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ny_private_school_discount, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ny_private_school_discount, snare).
narrative_ontology:human_readable(ny_private_school_discount, "Discount-for-Data Scheme in Private Schools").
narrative_ontology:topic_domain(ny_private_school_discount, "economic/education").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ny_private_school_discount, school_operator).
narrative_ontology:constraint_beneficiary(ny_private_school_discount, data_broker_ecosystem).
narrative_ontology:constraint_victim(ny_private_school_discount, low_income_families).
narrative_ontology:constraint_victim(ny_private_school_discount, child_privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME FAMILY (SNARE) — Faces genuine tuition barrier; the 42% discount is economically coercive given education access constraints. Cannot easily exit: alternative schools are unaffordable, public options may be inadequate, relocation is costly. Data extraction is the price of market participation. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(ny_private_school_discount, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CHILD PRIVACY COMMONS (SNARE) — Collective interest in child data protection is diffuse, unorganized, and has no direct exit mechanism. Bears full cost of data monetization and secondary use (behavioral profiling, discrimination risk). Cannot negotiate, cannot withdraw consent retroactively. d≈0.88, f(d)≈1.28, σ=0.9 → χ≈0.68.
constraint_indexing:constraint_classification(ny_private_school_discount, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SCHOOL OPERATOR (ROPE) — Frames the constraint as efficient market coordination: parents voluntarily exchange data for tuition relief; school solves operational costs through data monetization. From this perspective, the mechanism is consensual coordination. Experiences high regulatory arbitrage (FERPA compliance theater, minimal enforcement in private school context). d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(ny_private_school_discount, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PARENT ADVOCACY COALITION (TANGLED ROPE) — Organized agents (education advocacy groups, privacy nonprofits, parent unions) see both coordination failure (market for education access) and extraction (data asymmetry). They have partial exit options (regulatory campaigns, public attention) and some enforcement capacity (media, litigation). d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(ny_private_school_discount, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DATA BROKER ECOSYSTEM (SNARE) — Views this as a structural feature of behavioral capitalism: consumer data is the commodity, education is the carrot. The ecosystem has no incentive to create exit options because constraint perpetuation is profitable. Theater ratio is low (0.35) because the extraction mechanism is functionally real, not ceremonial. d≈0.15, f(d)≈0.02, σ=1.2 → χ≈0.01. Data brokers are beneficiaries with near-zero perceived extraction cost.
constraint_indexing:constraint_classification(ny_private_school_discount, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY PRETENSE (PITON) — Education authorities and state regulators maintain the fiction of oversight while private schools operate under minimal surveillance. Parental consent forms are performative (buried in enrollment documents, written in impenetrable legalese). Theater ratio = 0.35 fails piton gate (requires ≥0.70), so this classification is borderline; the constraint is functionally extractive, not ceremonial. However, the regulatory theater (opt-out language, consent language, data minimization claims) persists through institutional inertia despite being widely flouted.
constraint_indexing:constraint_classification(ny_private_school_discount, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ny_private_school_discount_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ny_private_school_discount, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ny_private_school_discount, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ny_private_school_discount, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ny_private_school_discount, TR),
    TR >= 0.70.

:- end_tests(ny_private_school_discount_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The 42% discount is economically coercive. For a family with annual income ~$45K, this discount (~$4K-$8K depending on baseline tuition) represents meaningful economic pressure. The constraint extracts because (a) families are forced to choose between data privacy and education access, and (b) the data extracted is monetized without ongoing family benefit. Over the 4-year interval, extractiveness increases from 0.35 to 0.58 as the school scales and data broker relationships deepen, confirming that extraction accumulates as the mechanism matures. Suppression (0.68): Families have limited exit options. Public school alternatives may be inadequate (overcrowded, low-performing). Relocation is costly. Behavioral shift to homeschooling is not feasible for working parents. Alternative private schools are unaffordable. The legal architecture (FERPA exempts private schools from mandatory transparency; state data protection laws often exclude educational records) creates a regulatory gap that suppresses exit via law. Theater ratio (0.35): The extraction is functionally real, not ceremonial. The school genuinely collects data and sells it to brokers; the process is not performative or decorative. Theater declines over time (0.55 → 0.35) as the school's operations mature and the legitimizing language (consent forms, privacy notices) is simplified or removed.
 *
 * PERSPECTIVAL GAP:
 *   The low-income family sees a snare: a coercive choice disguised as a voluntary transaction. The child privacy commons sees a snare: extraction from a powerless, unorganized collective. The school operator sees a rope: efficient market coordination between education supply and family demand. The data broker ecosystem is nearly invisible to families but sees itself as a beneficiary in a rope (information standard coordination). The parent advocacy coalition sees a tangled rope: real coordination failure (education access) layered with real extraction (data asymmetry). The regulatory theater perspective sees a piton (oversight maintained through forms and language despite actual non-enforcement). The perspectival gap is driven by exit options: families cannot exit; the school and brokers can exit if regulation threatens. The organized coalition has partial exit options (public pressure, legislative campaigns) that mediate their perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income families: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction perception because they cannot walk away. Child privacy commons: Victim + trapped → d≈0.88, f(d)≈1.28. High extraction because the collective is diffuse and has no exit option. School operator: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; can exit if regulation threatens. Data brokers: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Near-zero perceived extraction; structural position is pure benefit. Parent advocacy coalition: Mixed + constrained → d≈0.52, f(d)≈0.68. Moderate extraction because the coalition has some agency (media, litigation, regulatory pressure) but not full exit options. The directionality gradient from 0.92 (trapped family) to 0.08 (institutional beneficiary) creates the perspectival gap: the same constraint is experienced as severe extraction from below and as efficient coordination from above.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids mislabeling coordination as extraction by declaring both beneficiaries AND victims. If beneficiaries were omitted, the entire mechanism would appear as pure predation (snare across all perspectives). If victims were omitted, the entire mechanism would appear as consensual (rope from all perspectives). The actual structure is: the school creates a real coordination function (solving education access) and uses it as cover for real extraction (data monetization). The snare classification holds because (a) the primary mechanism is extraction (data sale), not coordination (education access is just the carrot), and (b) the beneficiary (school/brokers) has full control over exit options while the victim (family) does not. From the organized coalition perspective, it appears as tangled rope because the coalition has some agency and can negotiate policy responses. From the regulatory perspective, it appears as piton because the oversight theater persists despite non-enforcement. The mandatrophy is resolved by recognizing that this is a snare with coordination theater — not a rope with extractive side effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_voluntariness_threshold,
    'At what discount level does ''voluntary'' consent become coercive given education access constraints?',
    'Empirical analysis of family income elasticity; comparison of discount levels across socioeconomic strata; survey data on perceived voluntariness',
    'If threshold < 42%: this scheme crosses into coercion. If threshold > 60%: discount framing succeeds in obscuring extraction. Policy intervention depends entirely on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_voluntariness_threshold, empirical, 'Discount threshold at which consent becomes coercive').

omega_variable(
    data_monetization_secondary_use,
    'How extensively is child behavioral data re-sold and used for downstream targeting, discrimination, or manipulation?',
    'Data broker subpoena; tracking of data flows through broker networks; analysis of downstream use cases (targeted advertising, credit scoring proxies, employment filtering)',
    'If secondary use is limited: extraction cost is lower than modeled. If secondary use includes discrimination or manipulation: extraction cost is catastrophic, moving toward pure predation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_monetization_secondary_use, empirical, 'Extent of secondary use and downstream targeting').

omega_variable(
    alternative_school_availability,
    'Are there genuinely affordable alternative schools (public, charter, community-based) in the same geographic region?',
    'Regional school census; cost analysis of alternatives; mobility barriers (transportation, catchment areas); quality proxy comparison',
    'If alternatives exist and are accessible: families have exit options, moving toward tangled_rope. If alternatives are absent or inaccessible: families are trapped, confirming snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_school_availability, empirical, 'Availability of affordable alternatives').

omega_variable(
    data_governance_regulatory_gap,
    'Do private schools fall under meaningful data protection regulation (COPPA, FERPA, state privacy law)?',
    'Legal analysis of jurisdiction; empirical survey of enforcement by state attorney general; comparison to public school obligations',
    'If regulation applies and is enforced: constraint is manageable via law. If gap exists: regulation theater persists while extraction is unmonitored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_governance_regulatory_gap, empirical, 'Regulatory coverage and enforcement gap').

omega_variable(
    parent_awareness_asymmetry,
    'Do parents understand the full scope of data collection, broker relationships, and secondary use?',
    'Survey of enrolled families; analysis of consent documents; comparison of parent stated understanding vs actual data practices',
    'If awareness gap is large: extraction operates under asymmetric information (snare is worsened). If parents are informed: at least some coercion is mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parent_awareness_asymmetry, empirical, 'Parent understanding of data practices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ny_private_school_discount, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nyps_tr_t0, ny_private_school_discount, theater_ratio, 0, 0.55).
narrative_ontology:measurement(nyps_tr_t2, ny_private_school_discount, theater_ratio, 2, 0.42).
narrative_ontology:measurement(nyps_tr_t4, ny_private_school_discount, theater_ratio, 4, 0.35).

% Extraction over time
narrative_ontology:measurement(nyps_be_t0, ny_private_school_discount, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nyps_be_t2, ny_private_school_discount, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(nyps_be_t4, ny_private_school_discount, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ny_private_school_discount, information_standard).
narrative_ontology:affects_constraint(ny_private_school_discount, education_access_inequality).
narrative_ontology:affects_constraint(ny_private_school_discount, behavioral_data_brokers).
narrative_ontology:affects_constraint(ny_private_school_discount, coppa_regulatory_gap).

% DUAL FORMULATION NOTE:
% This constraint is upstream of broader behavioral capitalism mechanisms (data broker ecosystem, algorithmic discrimination) and downstream of education access inequality. The discount-for-data scheme operationalizes the intersection of these two problems: education market failure + data extraction infrastructure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ny_private_school_discount, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
