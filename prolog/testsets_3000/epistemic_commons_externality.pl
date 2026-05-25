% ============================================================================
% CONSTRAINT STORY: epistemic_commons_externality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_commons_externality, []).

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
 *   constraint_id: epistemic_commons_externality
 *   human_readable: Epistemic Commons Externality: Knowledge Production with Asymmetric Costs and Benefits
 *   domain: epistemology/knowledge_production/economics
 *
 * SUMMARY:
 *   The epistemic commons externality describes the structural extraction
 *   that occurs when knowledge production systems generate asymmetric
 *   benefits and costs across distributed actors. Those who produce or
 *   amplify information claims (extractors) capture attention and economic
 *   value while bearing minimal verification responsibility. Those who depend
 *   on commons reliability — fact-checkers, researchers, citizens making
 *   decisions — bear costs of verifying upstream claims, correcting false
 *   information, and managing the accumulated noise. The constraint exhibits
 *   tangled rope structure: it contains a genuine coordination function
 *   (shared knowledge base enabling collaborative truth-seeking) alongside
 *   structural extraction (asymmetric burden distribution, appropriation of
 *   verification labor, externalized costs of misinformation). The theater
 *   ratio (0.55) reflects that traditional gatekeeping institutions (peer
 *   review, editorial boards) persist in performative form while unable to
 *   scale verification to information velocity. The extractiveness trajectory
 *   (0.28 → 0.52) shows acceleration as information production capacity
 *   outpaces verification capacity, and extractors optimize for attention
 *   capture rather than reliability. This is not a technological problem — it
 *   is a structural problem of incentive misalignment encoded in attention
 *   economies.
 *
 * KEY AGENTS:
 *   - Knowledge Extractors: Institutional beneficiaries (institutional/arbitrage) — platforms, publishers, attention-capturing agents who appropriate value from commons without bearing verification costs
 *   - Commons Reliability: Primary victim (powerless/trapped) — abstract collective good that cannot organize or exit; bears full cost of falsehood accumulation
 *   - Distributed Fact-Checkers: Secondary victims (powerless/trapped) — volunteers and small organizations attempting commons maintenance, competing against resource advantages of extractors, bearing reputational cost of false negatives
 *   - Research Community: Mixed position (moderate/constrained) — benefits from knowledge base coordination but constrained to maintain redundant verification, cannot exit without losing collaborative advantage
 *   - Traditional Gatekeeping: Institutional actor (institutional/arbitrage) — peer review and editorial systems maintain status through performative rituals despite degraded verification function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as structural misalignment between attention-capture incentives and reliability-maintenance costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_commons_externality, 0.52).
domain_priors:suppression_score(epistemic_commons_externality, 0.48).
domain_priors:theater_ratio(epistemic_commons_externality, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_commons_externality, extractiveness, 0.52).
narrative_ontology:constraint_metric(epistemic_commons_externality, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(epistemic_commons_externality, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_commons_externality, tangled_rope).
narrative_ontology:human_readable(epistemic_commons_externality, "Epistemic Commons Externality: Knowledge Production with Asymmetric Costs and Benefits").
narrative_ontology:topic_domain(epistemic_commons_externality, "epistemology/knowledge_production/economics").

domain_priors:requires_active_enforcement(epistemic_commons_externality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_commons_externality, knowledge_extractors).
narrative_ontology:constraint_beneficiary(epistemic_commons_externality, attention_capturing_agents).
narrative_ontology:constraint_victim(epistemic_commons_externality, commons_reliability).
narrative_ontology:constraint_victim(epistemic_commons_externality, distributed_fact_checkers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMONS RELIABILITY (SNARE) — Cannot exit the degradation spiral. As falsehoods accumulate and correction costs rise, the epistemic commons loses reliability without alternative institutional structure. Powerless collective good bears extraction with no agency, no voice, no exit. Maximum experienced extraction.
constraint_indexing:constraint_classification(epistemic_commons_externality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISTRIBUTED FACT-CHECKERS (SNARE) — Individual volunteers and small organizations attempting to maintain epistemic commons reliability. Structurally trapped: unpaid, competing against institutional resources of misinformation producers, bearing reputational cost of false negatives. Exit would mean abandoning the commons to accelerating degradation. No sunsetting mechanism.
constraint_indexing:constraint_classification(epistemic_commons_externality, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: RESEARCH COMMUNITY (TANGLED ROPE) — Benefits from ability to build on shared knowledge base (coordination function) while bearing costs of having to verify upstream claims (asymmetric extraction). Constrained exit: leaving the commons means losing collaborative advantages; staying means investing in redundant verification. Significant but not maximal extraction.
constraint_indexing:constraint_classification(epistemic_commons_externality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: KNOWLEDGE EXTRACTORS (ROPE) — Institutions, platforms, and individuals who capture attention and economic value from knowledge claims without bearing verification costs. Experience the constraint as pure coordination: the commons provides free verification infrastructure (via others' fact-checking) that they appropriate. Net beneficiary with full exit optionality.
constraint_indexing:constraint_classification(epistemic_commons_externality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL GATEKEEPING (PITON) — Peer review, editorial boards, academic credentials once served verification function. Now substantially performative: unable to scale to information velocity, yet persisted through institutional inertia. Theater ratio high because the ritual persists despite reduced functional verification. Institutional actors still benefit (maintaining status) but from degraded mechanism.
constraint_indexing:constraint_classification(epistemic_commons_externality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, sees both genuine coordination function (shared knowledge base enables collaborative progress) and structural extraction (asymmetric verification burden, attention capture without responsibility, false information externalities). The constraint's extraction is not incidental — it is built into the incentive structure of attention economies.
constraint_indexing:constraint_classification(epistemic_commons_externality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_commons_externality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_commons_externality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_commons_externality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_commons_externality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_commons_externality, TR),
    TR >= 0.70.

:- end_tests(epistemic_commons_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The original research group's extraction from the commons is systematic but not total — some extractors do contribute verification work, and some secondary benefits flow back through collaborative structures. The acceleration trajectory reflects growing incentive misalignment as information velocity increases. Suppression (0.48): Moderate. Not all agents face equal suppression barriers — extractors face minimal barriers (information asymmetry favors them), while fact-checkers face high barriers (resource limitation, scaling problem, strategic obscuration by some extractors). The aggregate suppression score reflects the system-level median. Theater ratio (0.55): Moderate. Traditional verification institutions (peer review, editorial boards) contribute some functional verification but operate substantially through performative ritual — the appearance of vetting without scaling capacity. New institutional forms (platform moderation, distributed fact-checking) have lower theater ratio but face suppression barriers that limit their reach.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal here. Extractors perceive a functional commons (Rope) that enables their work. Fact-checkers perceive a degrading system (Snare) that traps them in unsustainable labor. Researchers perceive mixed benefits and costs (Tangled Rope). Traditional institutions see their own degraded rituals (Piton). The analytical observer sees structural extraction encoded in attention-economy incentives (Tangled Rope deepening toward Snare). The gap reveals that the commons is not a neutral infrastructure — it is a distributional mechanism that accumulates extraction in the form of false information, reputational cost, and uncompensated verification labor.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) maps from structural position: extractors with arbitrage exit options derive low d (they benefit from the constraint); fact-checkers with trapped exit options derive high d (they bear maximum costs); researchers with constrained options derive mid-range d (mixed benefits and costs); analytical observer derives d from the systemic view of extraction flow. The institutional beneficiaries (extractors, gatekeeping systems) have d ≈ 0.10-0.20, producing negative or near-zero effective extraction in their perspective — they see the constraint as coordination. The powerless fact-checkers have d ≈ 0.95, producing high effective extraction — they see pure snare. The analytical observer has d ≈ 0.72, showing clear extraction flow despite coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here arises from the tension between the coordination function (knowledge sharing) and the extraction mechanism (asymmetric verification burden). A naive analysis might classify this as Rope (coordination) because genuine collaborative knowledge-building occurs. However, the structural data forces Tangled Rope: beneficiaries exist (extractors benefit from unverified amplification), victims exist (fact-checkers bear asymmetric costs), and active enforcement is required (institutional pressure to maintain verification infrastructure despite incentive misalignment). The resolution is that the constraint is genuinely both — it coordinates knowledge production while extracting verification labor from powerless agents. The mandatrophy dissolves when the analysis recognizes that coordination and extraction are not opposites in attention economies — they are often the same mechanism viewed from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_cost_attribution,
    'How much of the measured suppression (0.48) is attributable to inherent complexity of verification versus strategic information obscuration by extractors?',
    'Comparative analysis of verification costs for claims with high vs low financial incentive for obfuscation; measurement of fact-checker resource allocation across claim types',
    'If suppression is inherent: commons degradation is coordination problem (Rope from more perspectives). If suppression is strategic: constraint becomes pure extraction (Snare from analytical perspective). Current measurement assumes 60/40 split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_attribution, empirical, 'Attribution of suppression to complexity vs strategic obscuration').

omega_variable(
    commons_recovery_threshold,
    'Is there a degradation threshold beyond which the epistemic commons becomes irreversibly unreliable, or can it recover given institutional reform?',
    'Historical case studies of information ecosystems that recovered from high falsehood density; measurement of baseline reliability thresholds for different claim categories',
    'If recovery possible: suppression is temporal and potentially sunsettable (Scaffold classification becomes valid). If irreversible: suppression is structural (Snare deepens, Mountain risk from naturalization).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_recovery_threshold, empirical, 'Whether epistemic commons degradation is reversible').

omega_variable(
    extractor_incentive_alignment,
    'Do knowledge extractors have sufficient incentive misalignment with commons reliability to maintain asymmetric extraction, or would they voluntarily contribute to verification if externalities were internalized?',
    'Natural experiments with algorithmic ranking changes that reward accuracy; measurement of voluntary fact-checking contributions by high-extraction platforms when reputation incentives shift',
    'If incentive misalignment is fundamental: requires institutional enforcement (Tangled Rope with high suppression). If misalignment is contingent: Rope becomes possible with reformed incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractor_incentive_alignment, conceptual, 'Degree of inherent vs contingent incentive misalignment').

omega_variable(
    distributed_verification_sustainability,
    'Can volunteer-powered distributed fact-checking scale to cover the full information ecosystem, or is it structurally inadequate?',
    'Measurement of fact-checker coverage ratio (claims checked / total claims circulated) over time; burnout and resource depletion analysis in fact-checking organizations',
    'If scalable: Scaffold perspective valid with institutional support. If inadequate: victims remain structurally trapped (Snare perspective hardens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_verification_sustainability, empirical, 'Scalability of distributed verification mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_commons_externality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epcomm_tr_t0, epistemic_commons_externality, theater_ratio, 0, 0.32).
narrative_ontology:measurement(epcomm_tr_t3, epistemic_commons_externality, theater_ratio, 3, 0.42).
narrative_ontology:measurement(epcomm_tr_t6, epistemic_commons_externality, theater_ratio, 6, 0.5).
narrative_ontology:measurement(epcomm_tr_t10, epistemic_commons_externality, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(epcomm_be_t0, epistemic_commons_externality, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(epcomm_be_t3, epistemic_commons_externality, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(epcomm_be_t6, epistemic_commons_externality, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(epcomm_be_t10, epistemic_commons_externality, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_commons_externality, information_standard).
narrative_ontology:affects_constraint(epistemic_commons_externality, misinformation_production_incentive).
narrative_ontology:affects_constraint(epistemic_commons_externality, verification_bottleneck).
narrative_ontology:affects_constraint(epistemic_commons_externality, attention_capture_asymmetry).

% DUAL FORMULATION NOTE:
% The epistemic commons externality is upstream of specific misinformation claims but represents a distinct structural constraint on the verification infrastructure itself. Related constraints (misinformation incentive, verification bottleneck, attention asymmetry) are downstream — they operate within the commons structure rather than on the commons structure itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_commons_externality, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
