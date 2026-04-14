% ============================================================================
% CONSTRAINT STORY: regulatory_capture_compliance_standards
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_compliance_standards, []).

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
 *   constraint_id: regulatory_capture_compliance_standards
 *   human_readable: Regulatory Capture via Compliance Standards
 *   domain: political_economy/regulatory_capture
 *
 * SUMMARY:
 *   Regulatory capture via compliance standards represents a structurally
 *   sophisticated extraction mechanism where incumbent firms shape technical
 *   standards that appear neutral but systematically exclude competitors. The
 *   constraint exhibits a genuine coordination function—safety and
 *   interoperability standards are real and necessary—alongside systematic
 *   extraction channeled through complexity, opacity, and incumbent
 *   expertise. This makes it a canonical tangled rope: the beneficiary
 *   (incumbent firms) genuinely benefits from coordination mechanisms (stable
 *   technical environment, predictable requirements) while simultaneously
 *   using those mechanisms to extract from the victim (new entrants). The
 *   regulatory agency occupies a constrained institutional position where
 *   genuine regulatory mandate (consumer protection, market fairness) is
 *   systematically captured. The high theater ratio (0.65) reflects that much
 *   of the compliance apparatus consists of ritual documentation and
 *   consultant-mediated transliteration rather than genuine technical
 *   verification. Over a 20-year interval, extractiveness has drifted upward
 *   (0.35 → 0.58) as standards have accumulated without simplification, and
 *   theater has risen (0.42 → 0.65) as documentation requirements have
 *   outpaced functional improvement. The trajectory indicates institutional
 *   entropy: rules accumulate, complexity increases, and the original
 *   coordination function becomes harder to distinguish from the
 *   incumbency-protection function.
 *
 * KEY AGENTS:
 *   - Incumbent Regulated Firms: Primary beneficiary (institutional/arbitrage) — shapes standards through lobbying, benefits from complexity barriers, can influence regulatory process through technical expertise and revolving-door relationships
 *   - New Market Entrants: Primary victim (powerless/trapped) — faces immobilizing compliance burden, must match incumbent-designed specifications, cannot afford compliance consulting, cannot exit without abandoning market entry
 *   - Consumer Protection System: Secondary victim (powerless/trapped) — regulatory mandate is co-opted; markets have reduced competition and innovation, consumers bear the cost of reduced choice and slower technological progress
 *   - Regulatory Agency: Constrained institutional actor (institutional/constrained) — genuinely coordinates safety standards but constrained by resource limitations, expertise dependence on regulated firms, and revolving-door employment patterns
 *   - Compliance Consulting Industry: Institutional actor with degraded function (institutional/constrained) — originally served to translate standards into operation; now perpetuates complexity by repackaging incumbent-designed specifications as necessary procedure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees mixed coordination and extraction; identifies that the system requires enforcement to maintain coordination while extraction systematically flows to beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_compliance_standards, 0.58).
domain_priors:suppression_score(regulatory_capture_compliance_standards, 0.68).
domain_priors:theater_ratio(regulatory_capture_compliance_standards, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_compliance_standards, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_compliance_standards, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(regulatory_capture_compliance_standards, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_compliance_standards, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_compliance_standards, "Regulatory Capture via Compliance Standards").
narrative_ontology:topic_domain(regulatory_capture_compliance_standards, "political_economy/regulatory_capture").

domain_priors:requires_active_enforcement(regulatory_capture_compliance_standards).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_compliance_standards, incumbent_regulated_firms).
narrative_ontology:constraint_victim(regulatory_capture_compliance_standards, new_market_entrants).
narrative_ontology:constraint_victim(regulatory_capture_compliance_standards, consumer_protection_system).
narrative_ontology:constraint_victim(regulatory_capture_compliance_standards, regulatory_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW MARKET ENTRANT (SNARE) — Faces immobilizing compliance burden designed by and for incumbents. No exit without abandoning entry. Compliance standards are structurally opaque (written in industry jargon, requiring expensive consulting). Cannot compete on innovation; must match incumbent-approved technical specifications. Maximum extraction, zero coordination benefit.
constraint_indexing:constraint_classification(regulatory_capture_compliance_standards, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSUMER PROTECTION (SNARE) — The regulatory mandate is co-opted to serve incumbent interests rather than consumer safety. Compliance theater masks extraction: rules exist in name of protection but are written to exclude competitors. The system has no exit—consumers are trapped in a market with reduced competition and innovation. The abstracted regulatory mission bears the cost.
constraint_indexing:constraint_classification(regulatory_capture_compliance_standards, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT REGULATED FIRM (ROPE) — Experiences the constraint as coordination of technical standards, market stability, and competitive positioning. Benefits from complexity barriers that exclude new entrants. Has arbitrage options: can influence standards through lobbying, can diversify into new markets, can outsource compliance. Net beneficiary. Views compliance standards as enabling legitimate inter-firm coordination on safety and interoperability—which is partly true.
constraint_indexing:constraint_classification(regulatory_capture_compliance_standards, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — Constrained by resource limitations, dependence on industry expertise, and revolving-door employment. Genuinely coordinates safety standards (coordination function: authentic risk management). Simultaneously captured—writes and enforces standards that exclude competitors rather than achieve safety objectives. High enforcement cost; moderate technical coordination benefit. The agency cannot fully exit its capture, but has marginal agency in specific standards revisions.
constraint_indexing:constraint_classification(regulatory_capture_compliance_standards, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPLIANCE CONSULTING INDUSTRY (PITON) — Originally emerged to translate genuine regulatory requirements into operational procedure. Now largely performative: consultants help clients navigate opaque standards designed by incumbents. Theater ratio high (0.65) because much consulting effort goes to repackaging existing practice rather than substantive compliance improvement. Inertia: the consulting sector's business model depends on standards remaining complex and opaque. Degraded function.
constraint_indexing:constraint_classification(regulatory_capture_compliance_standards, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (safety, interoperability, consistent measurement) alongside systematic extraction (incumbents use standards to exclude competitors). The constraint is not pure snare (coordination is real) nor pure rope (extraction is structural). The system requires active enforcement to maintain the coordination function while the extraction flows to beneficiaries. Definitional tangled rope: mixed motives with asymmetric power.
constraint_indexing:constraint_classification(regulatory_capture_compliance_standards, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_compliance_standards_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_compliance_standards, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_compliance_standards, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_compliance_standards, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_compliance_standards, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_compliance_standards_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits substantial extraction—new entrants face compliance costs that are orders of magnitude higher than incumbents' marginal compliance costs, and this asymmetry is structurally designed rather than incidental. However, extractiveness is not maximal (0.70+) because genuine coordination function exists: safety standards are not entirely theater, interoperability requirements serve real functions, and some technical complexity is irreducible. The 0.58 value reflects the hybrid: approximately 35% of measured compliance cost is genuine safety/interoperability coordination; approximately 65% is extractive overhead (barriers to entry, complexity without functional benefit, consultant intermediation). Suppression (0.68): Moderate-high. Barriers to exit from the extracted position include: high sunk costs of compliance (learning, certification, consulting), capital requirements, information asymmetry (standards are opaque to outsiders), and regulatory uncertainty (standards can change unpredictably). Exit barriers are high but not absolute—some entrants do successfully navigate them, and offshore alternatives exist in some sectors. The value reflects that suppression is strong but not total, leaving marginal space for exit. Theater ratio (0.65): Moderate-high. Traditional compliance certification, documentation requirements, and consulting-mediated standard translation constitute substantial performative content. Much compliance effort (audits, certifications, compliance reports) serves signaling and ritualistic functions rather than direct safety verification. However, theater is not dominant (not 0.85+) because genuine technical verification occurs in some standards. The value reflects that the certification system is substantially but not entirely theater.
 *
 * PERSPECTIVAL GAP:
 *   The magnitude of perspectival divergence reveals the constraint's extractive structure. The incumbent (institutional/arbitrage) experiences Rope—they genuinely coordinate a technical environment while benefiting from protection against disruptive competition. Their experience is authentic: they do solve real coordination problems. The entrant (powerless/trapped) experiences Snare—immobilizing barriers, no genuine benefit, zero alternative pathways. The regulatory agency (institutional/constrained) experiences Tangled Rope—genuinely tries to serve consumer protection while constrained by captured institutional position and resource limits. The consumer protection mission (powerless/trapped) experiences Snare—the regulatory mandate is hollowed out to serve incumbent interests, no exit pathway. This perspectival scatter (Rope → Snare → Tangled Rope → Snare) is diagnostic of institutional capture: beneficiaries see coordination, victims see extraction, and captured regulators see mixed motives they cannot untangle. The wide gap between incumbent and entrant perspectives on the same structural facts indicates that the inequality is not incidental but designed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position: who benefits, who bears costs, and what options exist for exit. Incumbent firms get low d (~0.10-0.15) because they are net beneficiaries with arbitrage options—they can influence standards, can choose which markets to serve, can adjust operations. The engine computes their effective extraction chi as low or slightly negative (they benefit). New entrants get high d (~0.85-0.95) because they bear full costs with no exit options—trapped by capital requirements and information asymmetry. Their chi is maximal. The regulatory agency gets moderate d (~0.55-0.65) because it is structurally constrained but not fully trapped—constrained exit options (resource limits, expertise dependence, career incentives) but some marginal agency in standard revisions. The consumer protection system gets very high d (~0.95) because it is an abstracted collective good that bears the cost of reduced competition and innovation with no capacity to organize or exit. The analytical observer gets moderate-high d (~0.72) reflecting their external position measuring extraction flowing through the system. These directionality values feed the sigmoid f(d) to produce the experienced extractiveness chi for each perspective, explaining why the incumbent genuinely experiences Rope while the entrant genuinely experiences Snare—they occupy opposite positions in the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that it is neither pure coordination nor pure extraction but a hybrid where the coordination function is genuine and the extraction mechanism is systematic. The trap: if you eliminate the extractive overhead, you risk eliminating genuine safety coordination. The mandatrophy resolution shows that the solution is not to abandon standards but to decompose the coordination from the extraction. Open-access standard-setting processes (ISO, IETF model) can preserve genuine coordination while reducing incumbent influence. Outcome-based standards (performance targets rather than prescriptive specifications) can preserve safety while eliminating incumbent-designed complexity. Third-party testing (academic labs, nonprofits, decentralized verification) can replace consultant-mediated ritual with technical verification. The key insight: the coordination function and the extraction mechanism are operationally separable. They are hybrid not because they must be, but because the institutional design conflates them. Regulatory reform that separates these functions converts the constraint from Tangled Rope toward Rope (if coordination is preserved) or Scaffold (if standards sunset and alternative mechanisms mature). The analytical observer's Tangled Rope classification is the structural truth—it contains both rope and snare components. The reform goal is to isolate the rope and eliminate the snare without collapsing the whole system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    standards_capture_vs_legitimate_coordination,
    'Do compliance standards primarily serve genuine safety/interoperability coordination, or have they been primarily optimized for incumbent protection?',
    'Comparative analysis: compare standards that emerged through open-access processes (e.g., ISO open standards bodies) vs closed industry-led processes; measure correlation between standard complexity and incumbent market share; analyze revision history for whether standards tighten after new entrants begin conforming or tighten preemptively.',
    'If primarily coordination: constraint should be classified as Rope from regulatory agency perspective and Rope/Scaffold from entrant perspective (high exit barriers but genuine benefits). If primarily extraction: Snare/Tangled Rope classifications confirmed; extraction mechanism is institutional rather than incidental.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(standards_capture_vs_legitimate_coordination, empirical, 'Whether standards serve coordination or incumbent protection').

omega_variable(
    regulatory_agency_identity_lock,
    'Is the regulatory agency genuinely captured (constrained by external barriers—funding, expertise dependence, revolving door) or identity-locked (has internalized the incumbent frame as ''how good regulation works'')?',
    'Interview analysis of agency personnel: do regulators see incumbent preferences as necessary technical requirements or as policy choices? Exposure experiment: how do agency positions shift when given alternative expertise sources (academic labs, nonprofits) with no incumbent connections? Longitudinal tracking of agency positions after leadership turnover.',
    'If constrained: agency could shift with resource/structural reform. If identity_locked: agency''s self-conception must change—regulators must reframe ''good regulation'' away from incumbent stability toward consumer dynamism. Identity lock requires different intervention logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_agency_identity_lock, empirical, 'Whether regulatory capture is structural or identity-based').

omega_variable(
    entrant_barrier_irreducibility,
    'Are compliance standards genuinely necessary for safety/function, or could substantially equivalent safety be achieved through alternative verification mechanisms (third-party testing, outcome-based standards, performance certification)?',
    'Cross-domain comparison: industries with outcome-based standards vs prescriptive standards; comparison of safety performance, innovation rates, and competition metrics; controlled experiments with alternative verification approaches in low-risk domains.',
    'If standards are irreducible: entrant barriers are partly legitimate costs of safety coordination (reduces Snare classification, increases Rope). If alternatives exist: barriers are extraction-optimized rather than safety-optimized (confirms Snare/extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrant_barrier_irreducibility, empirical, 'Whether compliance standards are irreducibly necessary').

omega_variable(
    extractive_overhead_magnitude,
    'What proportion of compliance cost is genuinely attributable to safety/interoperability coordination vs incumbent-optimized complexity?',
    'Cost decomposition analysis: compare compliance costs for startups vs incumbents; measure cost reduction when entrants receive direct regulatory guidance vs consulting intermediary; longitudinal tracking of compliance costs before/after standards simplification initiatives.',
    'If >70% is extractive overhead: extractiveness score increases to 0.65+, Snare classification strengthens. If <30% is extractive overhead: Tangled Rope classification strengthens with higher coordination weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_overhead_magnitude, empirical, 'Proportion of compliance cost attributable to extraction vs coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_compliance_standards, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture_compliance_standards, theater_ratio, 0, 0.42).
narrative_ontology:measurement(regcap_tr_t10, regulatory_capture_compliance_standards, theater_ratio, 10, 0.55).
narrative_ontology:measurement(regcap_tr_t20, regulatory_capture_compliance_standards, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture_compliance_standards, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_be_t10, regulatory_capture_compliance_standards, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(regcap_be_t20, regulatory_capture_compliance_standards, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_compliance_standards, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture_compliance_standards, market_concentration_barriers).
narrative_ontology:affects_constraint(regulatory_capture_compliance_standards, regulatory_agency_resource_dependency).
narrative_ontology:affects_constraint(regulatory_capture_compliance_standards, information_asymmetry_expertise).

% DUAL FORMULATION NOTE:
% Regulatory capture via compliance standards is downstream of specific industry domains (finance, healthcare, telecom, environmental) and upstream of market concentration. The constraint story applies across domains but manifests differently depending on how standards are embedded in regulatory infrastructure and whether alternative verification mechanisms are available. Decompose by sector if sector-specific metrics are needed; this story models the generic institutional mechanism of capture through standards design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_compliance_standards, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
