% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__zero_rating_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_interpretation__zero_rating_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tcp_ip_interpretation__zero_rating_reading
 *   human_readable: Zero-Rating Exemption Interpretation of TCP/IP
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This constraint story captures the 'zero_rating_reading' of the contested
 *   kernel 'tcp_ip_interpretation'. The reading holds that TCP/IP's
 *   end-to-end principle permits ISPs to selectively exempt sponsored content
 *   from data caps. The standing arrangement is the regulatory and commercial
 *   regime that allows zero-rating partnerships. The constraint exhibits a
 *   genuine coordination function (managing congestion and caps) but has
 *   evolved to substantially extract value for incumbents and ISPs, raising
 *   barriers for competitors. The engine will compute per-seat
 *   classifications from the structural data below; the claimed type
 *   (tangled_rope) reflects the author's structural judgment, independent of
 *   the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.65).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.55).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "Zero-Rating Exemption Interpretation of TCP/IP").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '57744c72-25c9-4050-864d-86928b82d143').
narrative_ontology:cs_kernel_codification('57744c72-25c9-4050-864d-86928b82d143', distributed).
narrative_ontology:cs_authority_grounding('57744c72-25c9-4050-864d-86928b82d143', practice).
narrative_ontology:cs_interpretation_layer_present('57744c72-25c9-4050-864d-86928b82d143').
narrative_ontology:cs_reading_relation('57744c72-25c9-4050-864d-86928b82d143', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('57744c72-25c9-4050-864d-86928b82d143', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('57744c72-25c9-4050-864d-86928b82d143', foundational, sponsored_data_exemptions_allowed).
narrative_ontology:cs_axiom_status(sponsored_data_exemptions_allowed, holdable).
narrative_ontology:cs_axiom_grounding('57744c72-25c9-4050-864d-86928b82d143', sponsored_data_exemptions_allowed, conventional).
narrative_ontology:cs_reference_frame('57744c72-25c9-4050-864d-86928b82d143', early_mobile_internet_data_cap_regime).
narrative_ontology:cs_drift_state('57744c72-25c9-4050-864d-86928b82d143', contemporary_streaming_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('57744c72-25c9-4050-864d-86928b82d143', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, competing_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, internet_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, net_neutrality_advocates).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__zero_rating_reading, zero_rating_permitted).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__zero_rating_reading, network_management_flexibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement zero-rating partnerships with content providers, exempting sponsored data from user caps. They control the technical infrastructure and negotiate commercial terms. Justify the practice as network management and consumer benefit. Could face regulatory changes but have significant lobbying power.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, isps, agenda_setter,
    powerful, biographical, constrained, global).

% Large platforms (video, social media, music) pay ISPs for zero-rating or benefit from ISP-initiated exemptions. Gain competitive advantage over rivals who cannot afford such deals. Their market position is reinforced by the arrangement.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_content_providers, beneficiary,
    powerful, biographical, mobile, global).

% Startups and smaller content providers face higher effective costs to reach users because their data counts against caps while zero-rated rivals do not. They may attempt to negotiate zero-rating deals but lack leverage. Exit options limited to building their own networks (prohibitively expensive) or accepting disadvantage.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, competing_content_providers, payer,
    moderate, biographical, constrained, global).

% Benefit from free access to zero-rated services but face reduced choice and innovation as non-zero-rated services become relatively more expensive. Their data plans are effectively shaped by ISP-content provider deals. Switching ISPs is often difficult due to market concentration.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, internet_users, payer,
    organized, biographical, constrained, global).

% Civil society groups, academics, and some regulators who argue zero-rating violates non-discrimination principles. They participate in regulatory proceedings and public debate but are structurally excluded from the commercial negotiations that define zero-rating partnerships.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, net_neutrality_advocates, excluded,
    organized, generational, analytical, global).

% National telecommunications authorities and competition agencies that evaluate zero-rating under net neutrality rules. They can permit, restrict, or ban the practice. Their decisions shape the enforcement landscape but they do not operate the networks or create the content.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__zero_rating_reading, isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__zero_rating_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows ISPs to manage network congestion and data cap constraints by partnering with content providers to exempt specific traffic from metering, theoretically reducing load on metered pathways and making popular services more accessible.
% TRANSFER_FUNCTION: Transfers competitive advantage and user attention from non-zero-rated content providers to zero-rated incumbents; transfers revenue from ISPs (foregone metered data revenue) or content providers (zero-rating fees) to the partnership ecosystem; transfers policy leverage from open-internet advocates to commercial negotiation.
% ABSENT_VOICES: Small content providers and startups lacking resources for zero-rating deals; users in developing regions who rely on zero-rated 'walled garden' services but have no representation in policy forums; future innovators whose market entry is blocked by the incumbent advantage.
% DISAPPEARANCE_RATIONALE: If zero-rating disappeared overnight, ISPs would lose a key product differentiation and revenue tool; incumbent content providers would lose a competitive moat; competing providers would face a more level playing field; users would see uniform data metering; regulators would shift focus to other network management practices. The mobile internet economy would reorganize around uniform data treatment.
% FOUNDING_PROBLEM: Early mobile broadband (2000s-2010s) featured very low data caps (hundreds of MBs). Zero-rating emerged to make essential or popular services (email, messaging, later video) usable without exhausting caps, framed as expanding access.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and incumbent platforms attest the problem remains live, citing growing video traffic and network congestion. Independent studies (e.g., OECD broadband reports, academic measurements) show data caps have risen dramatically and zero-rating now primarily advantages established video/social platforms. Regulatory records from EU, India, California show explicit findings that the original access-expansion justification has been superseded by competitive distortion.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__zero_rating_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tcp_ip_interpretation__zero_rating_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.65) reflects the significant competitive advantage transferred to zero-rated incumbents and the revenue extracted from the ecosystem. Suppression (0.55) is moderate: alternatives (non-zero-rated services) exist but are disadvantaged by metering, not banned. Theater ratio (0.3) captures the persistent 'network management' justification that increasingly covers competitive favoritism. Accessibility collapse (0.4) indicates alternatives are reachable but costly. Resistance (0.6) reflects sustained advocacy, regulatory action, and some market pushback. Measurements show a clear drift: extractiveness and suppression have risen as data caps grew and zero-rating shifted from access-expansion to competitive weapon.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP seat, the constraint is coordination with incidental beneficiary effects (d near 0.2). From competing content providers, it is extraction with constrained exit (d near 0.8). Internet users sit near symmetric (d ~0.5) — genuine benefit from free data, diffuse harm from reduced choice. Net neutrality advocates are excluded (d not computed). Regulators observe analytically (d=0.5). The engine will compute these divergences from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs and incumbent content providers are declared beneficiaries (collect gains, shape rules). Competing content providers and internet users are declared victims (bear costs, constrained exit). Net neutrality advocates are excluded (would object, not in negotiation). Regulators are observers. This mapping drives the engine's directionality derivation: beneficiaries get low d, victims get high d, excluded/observer get analytical d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (low data caps limiting access) is contested: caps have risen, but zero-rating persists and expands. The arrangement now primarily serves incumbent advantage rather than access expansion — a classic mandatrophy signal. However, ISPs still claim congestion management, and some users in developing regions rely on zero-rated basics. The engine's mismatch detection (founding_problem_status=contested + disappearance_verdict=world_rearranges) will flag this as a potential mandatrophy case without pre-judging.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_rating_reading_of_tcp_ip_kernel,
    'Does the zero_rating_reading represent a legitimate interpretation of TCP/IP''s end-to-end principle, or a commercial repurposing of protocol ambiguity?',
    'Historical analysis of RFCs and IETF debates on service differentiation; legal analysis of whether zero-rating constitutes ''discrimination'' under net neutrality statutes.',
    'If the reading is a legitimate interpretation, the constraint is a genuine coordination mechanism with extractive drift. If it is a repurposing, the coordination claim is cover and the constraint is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_rating_reading_of_tcp_ip_kernel, conceptual, 'Commitment-system framing ambiguity: is zero-rating a reading of the kernel or a deviation from it?').

omega_variable(
    coordination_vs_extraction_boundary,
    'At what point does zero-rating''s congestion-management function become subordinate to its competitive-favoritism function?',
    'Empirical measurement of traffic patterns: if zero-rated traffic exceeds a threshold of total traffic or if zero-rating deals correlate with market concentration rather than congestion metrics.',
    'Shifts classification along the rope-tangled_rope-snare spectrum; informs whether the coordination function is vestigial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Boundary ambiguity between genuine coordination and extractive cover.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of non-zero-rated alternatives structural (metering economics) or internalized (user perception that zero-rated services are ''free'' and therefore superior)?',
    'Behavioral experiments: if users switch to non-zero-rated alternatives when price difference is removed, suppression is structural. If preference persists, internalized component exists.',
    'If internalized, effective suppression is higher than structural measure; constraint persists even if metering is equalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in user choice architecture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_ip_zero_rating_tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tcp_ip_zero_rating_tr_t5, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(tcp_ip_zero_rating_tr_t10, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(tcp_ip_zero_rating_tr_t15, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(tcp_ip_zero_rating_tr_t20, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(tcp_ip_zero_rating_tr_t30, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(tcp_ip_zero_rating_be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(tcp_ip_zero_rating_be_t5, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(tcp_ip_zero_rating_be_t10, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(tcp_ip_zero_rating_be_t15, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(tcp_ip_zero_rating_be_t20, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(tcp_ip_zero_rating_be_t30, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tcp_ip_zero_rating_su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tcp_ip_zero_rating_su_t5, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(tcp_ip_zero_rating_su_t10, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(tcp_ip_zero_rating_su_t15, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(tcp_ip_zero_rating_su_t20, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(tcp_ip_zero_rating_su_t30, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__zero_rating_reading, 0.15).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the tcp_ip_interpretation kernel. The neutrality_reading asserts non-discrimination; the prioritization_reading permits QoS differentiation. Zero-rating is a price-discrimination variant that structurally advantages capital-intensive incumbents. All three readings share the kernel (TCP/IP protocol suite) but instantiate different constraints with different ε and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
