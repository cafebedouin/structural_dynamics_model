% ============================================================================
% CONSTRAINT STORY: structural_extraction_without_actor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_extraction_without_actor, []).

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
 *   constraint_id: structural_extraction_without_actor
 *   human_readable: The Inertial Rent Siphon
 *   domain: economic/social/institutional
 *
 * SUMMARY:
 *   The Inertial Rent Siphon describes a structural extraction mechanism
 *   where a fee, regulatory requirement, or bureaucratic overhead persists
 *   long after the original service provider has vanished, been automated, or
 *   ceased providing value. The fee collection apparatus remains embedded in
 *   institutional infrastructure — payment processors, compliance frameworks,
 *   regulatory mandates — and continues extracting from end-users, service
 *   providers, or both. The original beneficiary may have been a specific
 *   monopolist (eliminated by competition), a public service (eliminated by
 *   automation), or a mythical justification (never existed). The constraint
 *   exhibits piton characteristics: the performative maintenance of a rule
 *   whose primary function has atrophied, sustained by institutional inertia
 *   and career systems that benefit from compliance bureaucracy. The
 *   theater_ratio rises over time (0.35 → 0.78) as the original service
 *   justification fades from institutional memory but enforcement becomes
 *   more ritualized. Extractiveness also rises (0.32 → 0.58) because the same
 *   fee structure now captures rents it was never designed to capture, as
 *   legitimate justifications disappear.
 *
 * KEY AGENTS:
 *   - End Users / Consumers: Primary victims (powerless/trapped) — bear the fee cost with no identifiable service in return
 *   - Downstream Service Providers: Secondary victims (moderate/constrained) — face fee structures that reduce competitiveness relative to incumbents who can absorb costs
 *   - Productive Economy: Tertiary victim (organized/mobile) — entire economic sectors bear the cumulative drag of unjustified fees
 *   - Legacy Institutional Steward: Ambiguous actor (institutional/constrained) — administers the fee collection but increasingly recognizes the lack of justification
 *   - Compliance Bureaucracy: Defensive beneficiary (organized/constrained) — perpetuates the requirement to justify administrative roles and budgets
 *   - Incumbent Monopolist / Ghost Beneficiary: Historical beneficiary (powerful/mobile) — may or may not still exist; if present, uses fee structure to suppress competition
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — sees the constraint as diagnostic piton confirming regulatory degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_extraction_without_actor, 0.58).
domain_priors:suppression_score(structural_extraction_without_actor, 0.65).
domain_priors:theater_ratio(structural_extraction_without_actor, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_extraction_without_actor, extractiveness, 0.58).
narrative_ontology:constraint_metric(structural_extraction_without_actor, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(structural_extraction_without_actor, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_extraction_without_actor, piton).
narrative_ontology:human_readable(structural_extraction_without_actor, "The Inertial Rent Siphon").
narrative_ontology:topic_domain(structural_extraction_without_actor, "economic/social/institutional").

% --- Structural relationships ---
narrative_ontology:constraint_victim(structural_extraction_without_actor, end_users_consumers).
narrative_ontology:constraint_victim(structural_extraction_without_actor, downstream_service_providers).
narrative_ontology:constraint_victim(structural_extraction_without_actor, productive_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER / CONSUMER (SNARE) — Faces a mandatory fee or bureaucratic requirement with no exit option and no identifiable beneficiary to justify it. The cost is embedded in transaction flow. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81. The constraint appears as pure extraction without compensation or service.
constraint_indexing:constraint_classification(structural_extraction_without_actor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL STEWARD / LEGACY ADMINISTRATOR (PITON) — The organization that collects the fee or enforces the requirement persists, but the original function or service has atrophied or been automated. The ritual continues through regulatory compliance and institutional inertia. theater_ratio=0.78 confirms piton gate. The steward experiences the constraint as a maintenance burden with declining justification. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.17. Low effective extraction because the steward itself sees the process as degraded.
constraint_indexing:constraint_classification(structural_extraction_without_actor, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPLIANCE BUREAUCRACY (PITON) — The structure that enforces or collects the fee has become a career system unto itself, disconnected from any service rationale. Compliance officers, middle managers, and auditing systems perpetuate the requirement. The bureaucracy has agency but experiences the constraint as self-preserving rather than functionally necessary. d≈0.45, f(d)≈0.50, σ=0.9 → χ≈0.22. Higher d than the steward because the bureaucracy actively defends the requirement despite lack of service; lower χ than snare because the bureaucracy can reorganize or eliminate the requirement without existential risk.
constraint_indexing:constraint_classification(structural_extraction_without_actor, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INCUMBENT MONOPOLIST / GHOST BENEFICIARY (TANGLED ROPE) — A powerful actor (incumbent firm, regulatory body, or institutional monopoly) nominally justifies the fee as payment for a service or compliance with a rule, but the original service has been automated, outsourced, or eliminated. The incumbent retains the fee collection mechanism and uses it to suppress competition. The fee functions both as a coordination requirement (nominally ensuring quality/safety) and as an asymmetric extraction mechanism (extracting rents from competitors and end-users). d≈0.15, f(d)≈0.05, σ=1.2 → χ≈0.03. The incumbent is a net beneficiary with low effective extraction because they can exit at will; the constraint exists to extract FROM others, not to constrain the incumbent.
constraint_indexing:constraint_classification(structural_extraction_without_actor, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THEORETICAL REGULATORY JUSTIFICATION (ROPE) — From a civilizational perspective focused on legitimate coordination, the original rationale for the fee (consumer protection, safety assurance, resource allocation) may have been sound when designed. The constraint classifies as Rope from the perspective that coordination mechanisms can degrade into theater without changing their structural classification. d≈0.25, f(d)≈0.12, σ=1.0 → χ≈0.07. This perspective sees a coordination problem (ensuring trust/safety) and minimizes the extraction component, treating the fee as a legitimate coordination cost.
constraint_indexing:constraint_classification(structural_extraction_without_actor, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — The constraint is a diagnostic piton: a former Snare (when the original incumbent provided an actual service) or former Tangled Rope (when coordination was genuine) that has degraded into pure theater. The beneficiary vanished (automation, regulation change, competitive displacement), but the fee persists. theater_ratio=0.78 captures the performative enforcement of a rule that no longer produces the claimed output. d≈0.68, f(d)≈1.08, σ=1.2 → χ≈0.48. The high d reflects the observer's position as external analyst; the classification as piton confirms institutional inertia.
constraint_indexing:constraint_classification(structural_extraction_without_actor, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_extraction_without_actor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_extraction_without_actor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_extraction_without_actor, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_extraction_without_actor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(structural_extraction_without_actor, TR),
    TR >= 0.70.

:- end_tests(structural_extraction_without_actor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The fee structure extracts from end-users and competitors without producing the claimed service. The extraction is not as severe as a pure snare (0.66+) because some upstream institutional actors are also trapped by the requirement and cannot easily eliminate it. The value reflects that the constraint is self-perpetuating through bureaucratic inertia rather than actively designed extraction by a powerful monopolist. Suppression (0.65): Moderate-high. The fee is usually mandatory (legal/regulatory requirement, embedded in payment infrastructure, or enforced through licensing). Alternatives are suppressed either by regulation (explicit prohibition) or by the cost structure of the fee itself (makes competition unviable). Theater ratio (0.78): High and rising. The original justification (consumer protection, service quality, resource allocation) is increasingly performative. Compliance documentation, audit trails, and regulatory filings create the appearance of justified governance, but the actual service has vanished. The rise from 0.35 to 0.78 over the measurement interval indicates Goodhart drift: as the original metric (quality of service) became harder to achieve, the system substituted metrics of compliance (documentation, audit completion) that created theater without function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a stark perspectival divide. From the end-user and downstream provider perspectives, the constraint is a snare: pure extraction with no offsetting benefit. From the institutional steward and compliance bureaucracy perspectives, it is a piton: a ritual that persists through inertia and career structures, increasingly recognized as degraded. From the perspective of an incumbent monopolist (if one still exists), it may be a tangled rope: a coordination mechanism on its surface that actually functions to suppress competition and extract rents. From the analytical observer, it is clearly a piton: a former coordination mechanism or rent-sharing arrangement that has degraded into pure performance. The perspectival gap between the end-user's snare and the steward's piton is diagnostic — it reveals that the constraint is extracting from powerless actors to maintain bureaucratic structures, not to provide service.
 *
 * DIRECTIONALITY LOGIC:
 *   End-user / Consumer: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. No exit, no offsetting benefit. Downstream Service Providers: Victim + constrained → d≈0.80, f(d)≈1.18. High extraction; some providers can exit by relocating or changing business model, but the fee creates a structural disadvantage. Productive Economy: Victim + mobile → d≈0.70, f(d)≈1.05. Moderate-high extraction; the economy has general mobility but bears cumulative drag. Legacy Institutional Steward: Victim + constrained → d≈0.35, f(d)≈0.30. The steward is both perpetuating the constraint and trapped by it; constrained exit because the organization's legitimacy rests on the fee collection. Compliance Bureaucracy: Beneficiary + constrained → d≈0.45, f(d)≈0.50. The bureaucracy nominally benefits from the fee's existence (job security, budget justification), but is also constrained by the lack of legitimate justification; if the fee is eliminated, the bureaucracy loses function. Incumbent Monopolist (if present): Beneficiary + arbitrage → d≈0.15, f(d)≈0.05. Net beneficiary with high exit options; can eliminate the fee at any time but benefits from its extraction or suppression effects. Analytical Observer: analytical → d≈0.68, f(d)≈1.08. Observes the constraint structure from outside; the high d reflects the observer's structural position as external analyst of extractive mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via Piton classification. The constraint has a dual nature: it contains a genuine coordination function (the original justification: consumer protection, safety assurance, resource allocation) but that function has been replaced entirely by theatrical performance. The fee persists because eliminating it would require acknowledging that the function it claimed to provide no longer exists — a blow to institutional legitimacy. The mandatrophy is resolved by recognizing the constraint as a degraded institution: it WAS potentially a tangled rope (coordination with extraction) or a snare (pure extraction), but the coordination function has atrophied and only the extraction theater remains. The high theater_ratio (0.78) and rising extractiveness (0.32 → 0.58) confirm this degradation. The classification as piton reflects that institutional inertia — not any legitimate current function — sustains the constraint. Incumbent actors who benefit from the fee's suppression effects (by keeping competitors disadvantaged) have an interest in its perpetuation, but those beneficiaries are historically contingent, not structural to the constraint itself. The constraint is a piton because it is maintained by an institutional system whose primary function is now self-perpetuation rather than service delivery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_beneficiary_existence,
    'Did the original beneficiary actually exist, or was the fee extraction couched in false justification from inception?',
    'Historical documentation of original service provision; audit of what service was actually delivered in early years vs. claimed service; comparison with similar jurisdictions that did not implement the fee',
    'If original beneficiary existed: constraint is Piton (degradation). If never existed: constraint may be Snare (pure extraction from inception) or Tangled Rope (concealed extraction couched in coordination language).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_beneficiary_existence, empirical, 'Whether the original service beneficiary actually existed or fee was extraction from start').

omega_variable(
    incumbent_capture_mechanism,
    'Is the ghost beneficiary a specific incumbent firm or a generalized regulatory capture where the fee mechanism was captured by the regulated industry?',
    'Analysis of fee revenue flows; identification of which entities actually receive fee payments; comparison of fee structure changes when incumbents change; cross-jurisdictional comparison of fee levels correlated with market concentration',
    'If specific incumbent: constraint is primarily extraction mechanism by that firm (Tangled Rope from their perspective). If generalized capture: constraint is structural feature of regulatory-industrial complex (Snare for powerless, Piton for bureaucracy).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_capture_mechanism, empirical, 'Whether extraction benefits specific incumbent or dispersed bureaucracy').

omega_variable(
    automation_timeline,
    'At what point did automation or process change eliminate the original service function, and how long did extraction continue after that point?',
    'Temporal analysis of fee justification rhetoric vs. actual service delivery; identification of process changes; measurement of extraction duration post-automation',
    'If lag is short (< 2 years): may indicate rational transition period (Scaffold). If lag is long (> 10 years): confirms institutional inertia and piton classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(automation_timeline, empirical, 'Time lag between service elimination and continued fee extraction').

omega_variable(
    suppression_mechanism_source,
    'Is suppression of alternatives (regulatory prohibition, technical barriers, information asymmetry) active enforcement by the fee collector or passive structural lock-in?',
    'Analysis of regulatory exceptions, licensing requirements, and enforcement actions against alternatives; identification of whether alternatives are legally prohibited, technically infeasible, or merely disadvantaged by fee structure',
    'If active enforcement: Tangled Rope (requires_active_enforcement=true). If passive: Piton or Snare depending on whether enforcement is visible or hidden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Whether suppression of alternatives is active or passive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_extraction_without_actor, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(siphon_tr_t0, structural_extraction_without_actor, theater_ratio, 0, 0.35).
narrative_ontology:measurement(siphon_tr_t5, structural_extraction_without_actor, theater_ratio, 5, 0.58).
narrative_ontology:measurement(siphon_tr_t15, structural_extraction_without_actor, theater_ratio, 15, 0.78).

% Extraction over time
narrative_ontology:measurement(siphon_be_t0, structural_extraction_without_actor, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(siphon_be_t5, structural_extraction_without_actor, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(siphon_be_t15, structural_extraction_without_actor, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_extraction_without_actor, resource_allocation).
narrative_ontology:affects_constraint(structural_extraction_without_actor, regulatory_capture).
narrative_ontology:affects_constraint(structural_extraction_without_actor, bureaucratic_rent_seeking).
narrative_ontology:affects_constraint(structural_extraction_without_actor, competitive_moat_via_compliance).

% DUAL FORMULATION NOTE:
% This constraint is a diagnostic piton that emerges when coordination mechanisms degrade into pure extraction. It is upstream of and distinct from specific regulatory capture scenarios (which may use this constraint's fee structure to suppress competition). The inertial rent siphon is the structural feature that makes regulatory capture persistent: once a fee or requirement is embedded in institutional infrastructure, it becomes very difficult to eliminate even when the original beneficiary or service provider vanishes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_extraction_without_actor, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
