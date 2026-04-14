% ============================================================================
% CONSTRAINT STORY: institutional_rent_seeking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_rent_seeking, []).

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
 *   constraint_id: institutional_rent_seeking
 *   human_readable: Institutional Rent Seeking and Regulatory Capture
 *   domain: political_economy/institutional_dynamics
 *
 * SUMMARY:
 *   Institutional rent seeking occurs when organizations use their regulatory
 *   or market power to extract value without creating genuine economic
 *   surplus — using rules designed to coordinate legitimate collective
 *   problems to instead protect themselves from competition and redistribute
 *   wealth toward incumbents. This constraint exhibits the classic tangled
 *   rope structure: legitimate coordination functions (consumer protection,
 *   market stability, quality assurance) coexist with asymmetric extraction
 *   (barrier-to-entry protection, incumbent insulation, regulatory capture).
 *   The same regulatory apparatus that genuinely prevents predatory practices
 *   also prevents legitimate new entrants from competing. The constraint has
 *   intensified over the measurement interval as regulatory complexity has
 *   increased, theater has risen, and the capture mechanism has deepened. New
 *   entrants face an explicit regulatory moat designed and maintained by the
 *   very agencies ostensibly protecting consumer interests — creating a
 *   structural lock-in where the coordination problem (we need market
 *   stability) becomes the extraction mechanism (only we can enter because
 *   only we can afford compliance).
 *
 * KEY AGENTS:
 *   - New Market Entrants: Primary victims (powerless/trapped) — face insurmountable regulatory barriers designed to exclude them
 *   - Incumbent Oligopoly: Primary beneficiaries (institutional/arbitrage) — capture protected market share and extraction rents
 *   - Regulatory Agency: Captured actor (institutional/constrained) — maintains genuine statutory duties while serving incumbent interests through de facto capture
 *   - Small Incumbent Firms: Secondary victims (moderate/constrained) — protected from new entry but extracted by larger competitors and compliant to the same rules
 *   - Consumer Surplus: Structural victim (powerless/trapped) — restricted choice and higher prices from reduced competition; cannot participate in regulatory process
 *   - Economic Efficiency: Abstract victim (analytical/trapped) — allocative losses from protected inefficient incumbents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_rent_seeking, 0.58).
domain_priors:suppression_score(institutional_rent_seeking, 0.65).
domain_priors:theater_ratio(institutional_rent_seeking, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_rent_seeking, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_rent_seeking, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_rent_seeking, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_rent_seeking, tangled_rope).
narrative_ontology:human_readable(institutional_rent_seeking, "Institutional Rent Seeking and Regulatory Capture").
narrative_ontology:topic_domain(institutional_rent_seeking, "political_economy/institutional_dynamics").

domain_priors:requires_active_enforcement(institutional_rent_seeking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_rent_seeking, incumbent_firms).
narrative_ontology:constraint_beneficiary(institutional_rent_seeking, regulatory_agencies).
narrative_ontology:constraint_victim(institutional_rent_seeking, new_market_entrants).
narrative_ontology:constraint_victim(institutional_rent_seeking, consumer_surplus).
narrative_ontology:constraint_victim(institutional_rent_seeking, economic_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED ENTREPRENEUR (SNARE) — New market entrants face regulatory barriers designed to protect incumbents. Licensing requirements, compliance costs, and discretionary approval gates create insurmountable barriers to entry. The constraint extracts opportunity rents from aspiring competitors with no coordination benefit and no realistic exit.
constraint_indexing:constraint_classification(institutional_rent_seeking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL INCUMBENT FIRM (TANGLED ROPE) — Benefits from regulatory protection against new entrants (coordination of market stability) but bears ongoing compliance costs and faces extraction by larger competitors who can better navigate regulatory complexity. Mixed experience: protected but also constrained by the same rules they lobbied for.
constraint_indexing:constraint_classification(institutional_rent_seeking, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT OLIGOPOLY (ROPE) — Large firms experience the constraint as pure coordination: regulatory barriers prevent competition and stabilize market share. Minimal suppression from their perspective because compliance is routine. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(institutional_rent_seeking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — Genuine coordination function: manages market stability and protects consumer welfare (institutional mandate). Simultaneous extraction: captured by incumbent firms through revolving-door personnel exchange, lobbying pressure, and career incentives aligned with industry preferences. Caught between statutory duty and de facto capture.
constraint_indexing:constraint_classification(institutional_rent_seeking, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROTECTIVE REGULATION FRAMEWORK (PITON) — The formal regulatory apparatus maintains a theater of impartial review, but the functional purpose (protecting consumer welfare, enabling competition) has atrophied. Regulations persist through institutional inertia, justified by safety/quality narratives that once were genuine but now primarily serve incumbent protection.
constraint_indexing:constraint_classification(institutional_rent_seeking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, some regulatory structure is necessary (genuine coordination for market stability and consumer protection). But the analytical observer sees clear evidence of asymmetric extraction: regulatory barriers are designed and maintained by incumbent interests, not by impartial welfare maximization. The coordination function and extraction mechanism are structurally inseparable.
constraint_indexing:constraint_classification(institutional_rent_seeking, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_rent_seeking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_rent_seeking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_rent_seeking, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_rent_seeking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_rent_seeking, TR),
    TR >= 0.70.

:- end_tests(institutional_rent_seeking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant rents from excluded competitors and consumers, but the extraction is not maximal (which would be >0.70) because genuine coordination benefits exist — regulations do prevent some fraud and destabilizing behavior. The extractiveness has grown from 0.35 to 0.58 over the interval as incumbents have learned to exploit regulatory complexity and regulatory capacity has not kept pace with market sophistication. Suppression (0.65): High. Multiple barriers prevent exit and alternatives: legal prohibition on unregulated provision in many sectors, enforcement against informal providers, high compliance costs that scale with firm size, and information asymmetries about requirements. These are not trivial barriers — they constitute real suppression of alternatives. Theater ratio (0.68): Moderate-high. Regulatory reviews maintain an appearance of impartial safety assessment but increasingly serve as venue for incumbent lobbying and barrier reinforcement. The performative content has increased as the technical justification for many regulations has weakened (outdated standards maintained for incumbents) while the compliance theater has expanded.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer's classification (tangled rope) aligns with the regulatory agency's lived experience (tangled rope) and the consumer/excluded agent's lived experience (snare, from their perspective). The incumbent's rope classification is their genuine subjective experience but misses the extraction vector entirely — they see only the coordination benefit that accrues to them. The small incumbent's tangled rope classification reflects their split position: protected but also constrained. The piton classification (the regulatory framework itself) reflects that the apparatus has become largely performative — maintained through institutional inertia and political economy, not genuine functional necessity. The deepest perspectival gap is between the powerless excluded entrepreneur (snare) and the institutional incumbent (rope) — they experience maximum directionality difference (d: 0.95 vs 0.15) due to their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options. The excluded entrepreneur has d ≈ 0.95 (full target with no exit) → high f(d) → high chi experienced. The incumbent oligopoly has d ≈ 0.15 (beneficiary with arbitrage exit) → negative f(d) → low/negative chi experienced. The regulatory agency has d ≈ 0.50 (symmetric: genuine mission vs capture pressure) but this is modulated by their power level (institutional) and exit options (constrained — they cannot simply abandon their statutory role) → canonical d ≈ 0.50 × institutional context → moderate d. The consumer surplus agent would have d ≈ 1.0 (full victim) but this is an abstract collective, not an organized political actor, so their power is listed as powerless but their coordination capacity approaches zero. The constraint's effective directionality concentrates extraction on those with least power and least exit capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   Institutional rent seeking resolves the mandatrophy by showing that tangled rope is the correct type when genuine coordination and extraction coexist structurally. The constraint is NOT pure rope (pure coordination with no asymmetric extraction) because the regulatory barriers serve no coordination function that couldn't be served by less restrictive means — the barriers exist precisely to extract rents. The constraint is NOT pure snare (pure extraction with no coordination) because genuine harm does happen from unregulated markets (fraud, quality failures, destabilizing competition) and the regulatory apparatus does prevent some of these. The coordination function and extraction function are inseparable: they are achieved through the same regulatory mechanism. Dismantling the extraction would require also dismantling the coordination benefit, which is why the constraint persists despite clear inefficiency — the beneficiaries have successfully framed rent extraction as necessary coordination cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_vs_extractive_regulation,
    'What portion of regulatory requirements serve legitimate consumer protection vs incumbent rent extraction?',
    'Comparative analysis of regulatory stringency across jurisdictions; correlation between regulatory intensity and consumer outcomes; empirical testing of stated safety/quality rationales',
    'If legitimate portion > 60%: reclassify toward pure rope. If legitimate portion < 30%: reclassify toward snare. Current classification assumes 40-50% legitimate coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_vs_extractive_regulation, empirical, 'Portion of regulation serving legitimate vs extractive purposes').

omega_variable(
    regulatory_capture_mechanism_intensity,
    'Is the capture mechanism primarily through personnel exchange (revolving door), lobbying pressure, or ideological alignment?',
    'Network analysis of personnel flows between regulator and industry; campaign finance and lobbying expenditure tracking; regulatory decision correlation with industry preferences',
    'If personnel exchange dominates: capture is structural and near-permanent. If lobbying dominates: capture is contingent on political coalitions. Different mechanisms imply different sunset trajectories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism_intensity, empirical, 'Primary mechanism of regulatory capture').

omega_variable(
    alternative_market_entry_pathways,
    'Do non-regulated alternative service providers (e.g., online platforms, unregistered practitioners, informal markets) effectively bypass the regulatory barrier?',
    'Market share analysis of regulated vs alternative providers; consumer preference data; regulatory enforcement effectiveness against alternatives',
    'If alternatives are viable: exit is not trapped (reclassify to constrained). If alternatives are suppressed/illegal: exit remains trapped (snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_market_entry_pathways, empirical, 'Whether alternative pathways bypass regulatory barriers').

omega_variable(
    rent_extraction_magnitude_vs_coordination_value,
    'What is the ratio of extractive rents captured by incumbents to the social value of the coordination function provided by regulation?',
    'Deadweight loss calculation; consumer surplus reduction from restricted competition; comparison to estimated coordination costs of non-regulatory alternatives',
    'If rent >> coordination value: snare classification dominates. If rent < coordination value: rope or tangled rope remains accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rent_extraction_magnitude_vs_coordination_value, empirical, 'Ratio of extractive rents to coordination value').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_rent_seeking, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rentseek_tr_t0, institutional_rent_seeking, theater_ratio, 0, 0.52).
narrative_ontology:measurement(rentseek_tr_t10, institutional_rent_seeking, theater_ratio, 10, 0.62).
narrative_ontology:measurement(rentseek_tr_t20, institutional_rent_seeking, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(rentseek_be_t0, institutional_rent_seeking, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rentseek_be_t10, institutional_rent_seeking, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rentseek_be_t20, institutional_rent_seeking, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_rent_seeking, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_rent_seeking, regulatory_capture).
narrative_ontology:affects_constraint(institutional_rent_seeking, market_entry_barriers).
narrative_ontology:affects_constraint(institutional_rent_seeking, consumer_welfare_extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_rent_seeking, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
