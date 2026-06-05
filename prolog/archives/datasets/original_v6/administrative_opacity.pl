% ============================================================================
% CONSTRAINT STORY: administrative_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_administrative_opacity, []).

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
 *   constraint_id: administrative_opacity
 *   human_readable: Administrative Opacity as Coordination-Extraction Hybrid
 *   domain: institutional/governance
 *
 * SUMMARY:
 *   Administrative opacity — the systematic withholding or obscuring of
 *   decision rationale, process documentation, and outcome criteria by
 *   government agencies and large organizations — functions as both a
 *   coordination mechanism and an extraction device. The same opaque system
 *   that enables hierarchical coordination (unified implementation of policy
 *   without constant horizontal negotiation) also enables discretionary power
 *   abuse, decision arbitrariness, and capture of benefit by administrative
 *   insiders at the expense of subordinates and citizens. The constraint
 *   exhibits all six classification types from different structural
 *   positions, illustrating how institutional opacity naturalizes contingent
 *   power arrangements as inevitable technical necessities. Theater ratio
 *   (0.65) reflects the performative dimensions of administrative secrecy:
 *   redaction rituals, classification schemes that persist beyond their
 *   protective function, procedural opacity that claims to serve efficiency
 *   while primarily serving power concentration.
 *
 * KEY AGENTS:
 *   - Citizens and Subordinates: Primary victims (powerless/trapped) — face opaque decision criteria with no appeal mechanism; must comply without understanding rationale
 *   - Subordinate Administrators: Secondary victims (moderate/constrained) — execute opaque policies, bear accountability pressure downward, lack discretionary authority upward
 *   - Administrative Hierarchy: Primary beneficiary (institutional/arbitrage) — captures coordination benefits and discretionary power; can arbitrage between public transparency claims and internal opacity
 *   - Discretionary Power Holders: Secondary beneficiary (powerful/arbitrage) — extract value from information asymmetry; can make decisions shielded from review
 *   - Open Government Coalition: Organized agent (organized/mobile) — transparency advocates, FOIA reformers, building alternative pathways with sunset logic
 *   - Secrecy Apparatus: Institutional actor (institutional/arbitrage) — maintains classification and redaction systems that persist through inertia despite eroded functional justification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing opacity as inherent to administration rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(administrative_opacity, 0.58).
domain_priors:suppression_score(administrative_opacity, 0.68).
domain_priors:theater_ratio(administrative_opacity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(administrative_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(administrative_opacity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(administrative_opacity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(administrative_opacity, tangled_rope).
narrative_ontology:human_readable(administrative_opacity, "Administrative Opacity as Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(administrative_opacity, "institutional/governance").

domain_priors:requires_active_enforcement(administrative_opacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(administrative_opacity, administrative_hierarchy).
narrative_ontology:constraint_beneficiary(administrative_opacity, discretionary_power_holders).
narrative_ontology:constraint_victim(administrative_opacity, subordinate_agents).
narrative_ontology:constraint_victim(administrative_opacity, affected_public).
narrative_ontology:constraint_victim(administrative_opacity, organizational_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT OF BUREAUCRACY (SNARE) — Citizens or subordinates navigating opaque administrative systems face insurmountable barriers to understanding decision rationale, appealing outcomes, or exiting the system. No transparency into criteria, process, or appeal mechanisms. Maximum suppression and experienced extraction. Zero degrees of freedom.
constraint_indexing:constraint_classification(administrative_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUBORDINATE ADMINISTRATOR (TANGLED ROPE) — Mid-level bureaucrats experience genuine coordination (standardized procedures, organizational coherence) alongside asymmetric extraction (accountability pressure flows down, discretion flows up; responsibility without authority). Can exit through resignation but faces career costs. Real but surmountable barriers.
constraint_indexing:constraint_classification(administrative_opacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADMINISTRATIVE HIERARCHY (ROPE) — The institutional structure experiences opacity as coordination mechanism: enables unified policy implementation, prevents micro-level deviation, concentrates discretionary power at top levels. Benefits from informational asymmetry as management tool. Can arbitrage between public accountability claims and internal discretion.
constraint_indexing:constraint_classification(administrative_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OPEN GOVERNMENT COALITION (SCAFFOLD) — Freedom of information advocates, transparency mandates, and data-sharing infrastructure see opacity as temporary institutional default with a sunset: FOIA reforms, algorithmic transparency requirements, and digital recordkeeping create alternative pathways. Organized agents have exit options and can enforce sunsets. Theater decreasing as norms shift toward radical transparency.
constraint_indexing:constraint_classification(administrative_opacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SECRECY APPARATUS (PITON) — Traditional classification schemes (state secrets, privacy protection, national security) maintain opacity through institutional inertia even when the original functional justification has atrophied. Theater ratio high: performative security reviews, ritualized redaction, opacity claims that no longer serve protective function but persist through bureaucratic reflex. The apparatus sees its own process as increasingly ceremonial.
constraint_indexing:constraint_classification(administrative_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, some opacity appears inherent: perfect transparency creates coordination failure (endless decision-making paralyzes action), principal-agent problems are unsolvable without information asymmetry, complex systems require discretion at implementation level. This perspective naturalizes what the structural data reveals as a contingent institutional choice. Engine's false summit detector identifies naturalization.
constraint_indexing:constraint_classification(administrative_opacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(administrative_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(administrative_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(administrative_opacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(administrative_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(administrative_opacity, TR),
    TR >= 0.70.

:- end_tests(administrative_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Administrative opacity enables measurable extraction: subordinates cannot contest decisions, citizens cannot audit outcomes, discretionary beneficiaries operate without transparency. But extraction is not maximal (0.70+) because some administrative coordination is genuine — hierarchical systems do require some discretionary space to function. The value reflects that opacity serves both functions simultaneously. Suppression (0.68): High. Structural barriers to exit and information access are severe: citizens cannot easily leave jurisdictions, subordinates face career consequences for transparency violation, public has no formal mechanisms to compel disclosure. Suppression does not reach maximum (1.0) because some jurisdictions have implemented FOIA laws and transparency requirements — suppression is high but not absolute. Theater ratio (0.65): Moderate-high, increasing over the measurement interval. Traditional secrecy apparatus (classification schemes, redaction procedures) increasingly performs ceremony rather than function. Modern threats are not addressed by Cold War-era classification systems, but ritual redaction persists. Digital transparency infrastructure (algorithmic audits, open data portals) is exposing how much 'opacity' is theater rather than security. The theater ratio rises over time as the gap between claimed function and actual function widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Citizens trapped within opaque systems see pure extraction (Snare). Subordinate administrators see mixed coordination and extraction (Tangled Rope) — they benefit from standardized procedures but bear asymmetric accountability. The institutional hierarchy sees coordination (Rope) — opacity enables unified policy execution. Transparency advocates see a temporary institutional default with a sunset (Scaffold) — FOIA reforms and open data norms are building alternatives. The secrecy apparatus sees its own degraded ritual (Piton) — classification schemes persist through inertia despite eroded function. The civilizational analytical observer risks naturalizing opacity as inherent (Mountain) — 'some information asymmetry is necessary for effective administration' — but the structural data reveals this as a false summit: the contingent institutional choice to concentrate discretion can be replaced with structured transparency without coordination failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position relative to the opacity extraction flow. Citizens and subordinates are maximum targets (d ≈ 0.90) — they bear costs without benefits or exit options. Subordinate administrators are moderate targets (d ≈ 0.55) — they face mixed costs (accountability pressure) and benefits (procedural clarity). The administrative hierarchy is maximum beneficiary (d ≈ 0.10) — they capture discretionary power and informational advantage without costs. Discretionary power holders are maximum beneficiaries (d ≈ 0.05) — they operate in opaque space with no external review. The organizational agent (administrative_hierarchy) with arbitrage options experiences negative effective extraction (f(d) ≈ -0.12) — opacity is a coordination subsidy. The powerless agent (subordinate) with trapped options experiences maximum extraction (f(d) ≈ 1.42). The perspectival gap between these extremes reveals that 'opacity' is not a neutral property — it is a redistributive mechanism concentrating benefits upward and costs downward.
 *
 * MANDATROPHY ANALYSIS:
 *   Opacity does not resolve the mandatrophy of 'Is this coordination or extraction?' — it exemplifies the failure mode. The constraint genuinely coordinates at the institutional level (hierarchy without constant negotiation) AND genuinely extracts at the citizen level (discretionary power without accountability). Both perspectives are structurally accurate. The false summit is the civilizational view that naturalizes this as inevitable. Transparency reforms (FOIA, algorithmic audits, open data) demonstrate that coordination can function with much lower opacity — the coordination benefits do not require the level of opacity currently practiced. This reveals that much opacity is extraction disguised as coordination necessity. The mandatrophy is resolved not by choosing one type but by recognizing that the constraint's power comes from conflating coordination (legitimate) with extraction (unjustified). Separating these — maintaining coordination mechanisms while removing discretionary opacity — is the structural reform. The scaffold perspective's sunset logic is the mandatrophy resolution: build transparency infrastructure that preserves coordination benefits while removing extraction mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_functional_necessity,
    'Is administrative opacity functionally necessary for coordination, or is it an extraction mechanism hiding behind necessity claims?',
    'Comparative study of transparent vs opaque administrative systems; measurement of coordination failure rates and extraction magnitude across transparency levels',
    'If necessary: classification shifts toward Rope (pure coordination); suppression ≤ 0.40 becomes justified. If unnecessary: classification remains Tangled Rope or Snare; suppression ≥ 0.60 reveals pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_functional_necessity, empirical, 'Whether opacity is functionally necessary or purely extractive').

omega_variable(
    discretion_abuse_threshold,
    'At what level of discretionary power does opacity shift from enabling effective administration to enabling systematic abuse?',
    'Audit of discretionary decisions against stated policy; measurement of outcome variance and beneficiary asymmetry; comparison of outcomes across transparency interventions (FOI requests, algorithmic audits)',
    'If threshold is crossed: victim classification is justified; suppression level increases. If threshold is not crossed: beneficiary claims about coordination are sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_abuse_threshold, empirical, 'Threshold at which discretion becomes systematic abuse').

omega_variable(
    transparency_coordination_cost,
    'What is the actual coordination cost of moving from opacity to radical transparency? Does it scale linearly with system complexity?',
    'Implementation cost studies: FOIA compliance, algorithmic transparency infrastructure, audit requirements; measurement of decision-making speed and quality with vs without transparency; organizational adoption curves for transparency systems',
    'If costs are moderate: scaffold sunset is realistic (2–10 year transition). If costs are prohibitive: scaffold perspective is aspirational; opacity remains locked in by structural necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transparency_coordination_cost, empirical, 'Actual cost and feasibility of transparency infrastructure').

omega_variable(
    identity_lock_in_bureaucrats,
    'Do bureaucrats internalize opacity norms as part of their professional identity, making them unable to perceive transparency as legitimate?',
    'Qualitative analysis of bureaucratic culture; comparison of institutional identity in high-transparency vs high-opacity jurisdictions; measurement of psychological resistance to transparency reforms',
    'If significant identity lock-in: even transparent systems will face sabotage from institutional actors (piton perspective becomes more severe). Reformers need identity-frame breaking, not just structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_bureaucrats, conceptual, 'Professional identity fusion with opacity norms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(administrative_opacity, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(admin_opacity_tr_t0, administrative_opacity, theater_ratio, 0, 0.55).
narrative_ontology:measurement(admin_opacity_tr_t15, administrative_opacity, theater_ratio, 15, 0.62).
narrative_ontology:measurement(admin_opacity_tr_t30, administrative_opacity, theater_ratio, 30, 0.65).
narrative_ontology:measurement(admin_opacity_tr_t45, administrative_opacity, theater_ratio, 45, 0.68).

% Extraction over time
narrative_ontology:measurement(admin_opacity_be_t0, administrative_opacity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(admin_opacity_be_t15, administrative_opacity, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(admin_opacity_be_t30, administrative_opacity, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(admin_opacity_be_t45, administrative_opacity, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(administrative_opacity, enforcement_mechanism).
narrative_ontology:affects_constraint(administrative_opacity, regulatory_capture).
narrative_ontology:affects_constraint(administrative_opacity, principal_agent_divergence).
narrative_ontology:affects_constraint(administrative_opacity, bureaucratic_accountability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
