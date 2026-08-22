% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__zero_rating_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Zero-Rating Reading of TCP/IP: Sponsored-Content Data-Cap Exemption Authority
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This story instantiates the zero-rating reading of the contested TCP/IP
 *   interpretation kernel: the claim that the protocol suite's silence on
 *   application-layer differentiation authorizes ISPs to exempt specific
 *   sponsored content from subscriber data caps. This is a distinct
 *   constraint from the neutrality reading (which reads TCP/IP as requiring
 *   non-discrimination) and the prioritization reading (which reads it as
 *   permitting differentiated service quality as network management) — those
 *   are separate stories with their own ε and stakeholder structure, linked
 *   here only through the shared kernel and network edges. Under this
 *   reading, ISPs negotiate bilateral exemption programs with content
 *   providers; incumbent platforms with negotiating leverage and marketing
 *   budgets capture the exemptions, while independent entrants and smaller
 *   ISPs cannot access comparable terms.
 *
 * KEY AGENTS:
 *   - sponsoring_isps: agenda_setter, administers exemption programs and collects sponsorship value
 *   - incumbent_platform_operators: primary beneficiary, buys structural cost advantage over rivals
 *   - independent_content_startups: primary target, pays a data-cost tax competitors do not bear
 *   - capped_data_subscribers: mixed beneficiary/payer, gets short-term relief but consumption steered toward sponsors
 *   - non_sponsoring_small_isps: secondary payer, loses subscribers to bundled incumbent deals
 *   - telecom_regulators: analytical observer, adjudicates the exemption's legal status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.58).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "Zero-Rating Reading of TCP/IP: Sponsored-Content Data-Cap Exemption Authority").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, 'ec7cee1b-3f31-4225-93bd-bb2f244b0cc1').
narrative_ontology:cs_kernel_codification('ec7cee1b-3f31-4225-93bd-bb2f244b0cc1', distributed).
narrative_ontology:cs_authority_grounding('ec7cee1b-3f31-4225-93bd-bb2f244b0cc1', distributed).
narrative_ontology:cs_reading_relation('ec7cee1b-3f31-4225-93bd-bb2f244b0cc1', tcp_ip_interpretation__neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('ec7cee1b-3f31-4225-93bd-bb2f244b0cc1', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('ec7cee1b-3f31-4225-93bd-bb2f244b0cc1', foundational, application_layer_agnosticism_permits_commercial_differentiation).
narrative_ontology:cs_axiom_status(application_layer_agnosticism_permits_commercial_differentiation, holdable).
narrative_ontology:cs_axiom_grounding('ec7cee1b-3f31-4225-93bd-bb2f244b0cc1', application_layer_agnosticism_permits_commercial_differentiation, conventional).
narrative_ontology:cs_axiom('ec7cee1b-3f31-4225-93bd-bb2f244b0cc1', secondary, sponsored_exemption_serves_access_not_discrimination).
narrative_ontology:cs_axiom_status(sponsored_exemption_serves_access_not_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('ec7cee1b-3f31-4225-93bd-bb2f244b0cc1', sponsored_exemption_serves_access_not_discrimination, instrumental).
narrative_ontology:cs_reference_frame('ec7cee1b-3f31-4225-93bd-bb2f244b0cc1', carrier_commercial_discretion_baseline).
narrative_ontology:cs_drift_state('ec7cee1b-3f31-4225-93bd-bb2f244b0cc1', post_open_internet_order_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ec7cee1b-3f31-4225-93bd-bb2f244b0cc1', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_platform_operators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, sponsoring_isps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, independent_content_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, capped_data_subscribers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, non_sponsoring_small_isps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, capped_data_subscribers).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__zero_rating_reading, network_management_discretion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer zero-rating programs, deciding which content providers qualify for data-cap exemption and at what commercial terms. Collects sponsorship fees or exclusive-carriage advantages from partnered platforms, and can revise or expand the program at will since it is presented as ordinary commercial network management rather than a regulated common-carriage function.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, sponsoring_isps, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, sponsoring_isps, beneficiary).

% Pay to have their traffic exempted from subscriber data caps, cementing their position as the default usable service for cap-constrained users. Their scale lets them absorb sponsorship costs that smaller rivals cannot, so the exemption functions as a moat rather than a marginal marketing expense.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_platform_operators, beneficiary,
    institutional, civilizational, arbitrage, global).

% Cannot afford sponsorship arrangements and so their traffic counts fully against a user's data cap while incumbent competitors' traffic does not. Users effectively pay a premium in data allowance to try their service, which depresses adoption regardless of product quality. They have no seat at the table where exemption terms are set.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, independent_content_startups, payer,
    powerless, biographical, trapped, national).

% Get real short-term relief: using a zero-rated app does not consume their capped data, which matters for people on tight or expensive plans. At the same time their consumption is steered toward whichever platforms bought exemptions, narrowing what they can practically afford to try. Switching to a plan or ISP without these exemptions usually costs more or is not locally available.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, capped_data_subscribers, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, capped_data_subscribers, beneficiary).

% Lack the negotiating leverage or subscriber base to attract sponsorship deals from major platforms, so cannot offer competitive zero-rating packages. Regional and rural carriers lose subscribers to larger incumbents whose bundled exemptions look like a better deal, without ever competing on price or service quality.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, non_sponsoring_small_isps, payer,
    moderate, biographical, constrained, regional).

% Adjudicate whether zero-rating constitutes a network-management exception permitted under the governing framework or a discriminatory practice requiring intervention. Their rulings shift depending on political administration, producing a patchwork of enforcement across jurisdictions.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__zero_rating_reading, sponsoring_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__zero_rating_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Zero-rating lets ISPs offer subscribers relief from data caps on specific services, which can genuinely expand access to information and communication tools for cost-constrained users where general data pricing would otherwise exclude them.
% TRANSFER_FUNCTION: Moves competitive visibility and user attention from unsponsored content providers to sponsoring incumbents, and moves sponsorship payments (or reciprocal carriage concessions) from platforms to ISPs, while nominally 'free' data is recovered through the base plan's pricing structure.
% ABSENT_VOICES: Independent content startups and civil-society open-internet advocates are rarely present when exemption terms are negotiated bilaterally between ISPs and large platforms; their objection — that this recreates the appearance of a rival curated internet — is voiced mainly in regulatory comment periods after programs already exist.
% DISAPPEARANCE_RATIONALE: If the zero-rating exemption authority vanished, sponsoring ISPs would lose a differentiation and revenue lever, incumbent platforms would lose a structural cost advantage over new entrants, and capped subscribers would face symmetric data costs across all services — usage patterns would likely diversify as the effective price advantage for sponsored platforms disappeared.
% FOUNDING_PROBLEM: Mobile and fixed data plans in many markets were capped and expensive; carriers argued sponsored data let cost-constrained users access specific valuable services (messaging, education, health information) without exhausting a scarce data allowance.
% FOUNDING_PROBLEM_CORROBORATION: Sponsoring ISPs and their partnered platforms attest the founding problem (data-cost exclusion) remains live and that the arrangement solves it. Independent researchers, competition economists, and several national regulators attest that the exemption mechanism has become primarily a competitive-moat and ISP-monetization tool, citing markets where general data prices fell substantially while zero-rating programs expanded rather than contracted.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored substantial-but-not-extreme (0.68 at interval end) because a genuine coordination benefit is real (capped subscribers do get functional relief) even as the sponsorship mechanism systematically advantages incumbents over new entrants. Suppression (0.58) reflects that alternatives are not fully foreclosed — unsponsored services remain technically reachable, just costlier to reach — so this sits below the suppression levels typical of a pure snare. Theater ratio rises over the interval (0.20 to 0.42) as 'consumer benefit' framing increasingly substitutes for competitive analysis of the actual entry-barrier effect; this is read as Goodhart-style metric substitution, not a static state.
 *
 * PERSPECTIVAL GAP:
 *   From the sponsoring ISP and incumbent platform seats, this reading experiences as a genuine rope: a voluntary commercial arrangement providing real value to cost-constrained users. From the independent content startup and non-sponsoring small ISP seats, the identical structure operates as enforced extraction — a toll gate erected on top of a protocol that was silent about paying for priority. The engine should compute divergent per-seat types precisely because both descriptions are structurally accurate from their respective positions; that divergence is the object of study, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Sponsoring ISPs and incumbent platforms sit near the full-beneficiary end: they set terms, collect value, and hold arbitrage-grade exit (they can operate under any TCP/IP reading without existential risk). Independent content startups and non-sponsoring small ISPs sit near the full-target end: trapped or constrained exit, no leverage to negotiate comparable terms, and their competitive position is structurally degraded by a mechanism they cannot access. Capped data subscribers are genuinely mixed — real benefit plus steered consumption — which is why they carry both beneficiary and payer roles rather than being forced into one bucket.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is deliberate: the coordination function (real data-cost relief for constrained users) is not fictional cover — it is a real, measurable benefit for capped_data_subscribers. What makes it tangled rather than a clean rope is that the same mechanism that delivers that benefit is also the mechanism that raises entry barriers for competitors who cannot buy in. Classifying this as pure snare would erase the genuine subscriber benefit; classifying it as pure rope would erase the documented entry-barrier and competitive-foreclosure effect. The founding problem (cap-driven access exclusion) is contested-status, not dead — some populations still face binding data caps — which is why founding_problem_status is 'contested' rather than 'dead': a clean zombie-mandate flag would overstate how thoroughly the original problem has been solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protocol_silence_vs_authorization,
    'Does TCP/IP''s silence on application-layer differentiation constitute affirmative authorization for sponsored-content exemptions, or merely an absence of prohibition that later commercial practice filled in without protocol-level warrant?',
    'Historical analysis of protocol design intent (RFC discussions, end-to-end principle literature) versus documented commercial practice timeline; comparison with jurisdictions that have explicitly ruled on the question through legislation or court decision.',
    'If protocol silence is read as authorization, this reading''s claim to legitimacy strengthens and shifts the burden of proof onto regulators seeking to prohibit zero-rating. If read as mere absence-of-prohibition, the reading''s normative weight weakens substantially and the neutrality_reading''s foreclosure claim strengthens correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_silence_vs_authorization, conceptual, 'Whether protocol silence should be read as permission or as an open question later resolved by policy.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly do the three sibling readings (zero_rating, neutrality, prioritization) disagree — is it about what TCP/IP itself specifies, or about what regulatory frameworks built atop TCP/IP should require?',
    'Doctrinal mapping of each reading''s core premise against the actual RFC-level protocol specification versus against downstream telecom regulation (e.g., Open Internet Order provisions, EU net neutrality regulation); identify whether the disagreement is protocol-level or policy-level.',
    'If the disagreement is purely policy-level (all three readings agree on what TCP/IP specifies and disagree only about permissible business practices atop it), the ''kernel'' is better understood as the regulatory framework rather than the protocol itself, which would imply a different constraint family boundary and possibly different network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Committer-frame ambiguity: is the contested kernel the protocol or the regulatory doctrine built on top of it?').

omega_variable(
    subscriber_benefit_durability,
    'Is the short-term subscriber data relief a durable structural benefit, or does it erode as general data pricing falls and cap constraints loosen over time, leaving only the entry-barrier effect?',
    'Longitudinal comparison of data pricing trends and cap prevalence in markets with versus without active zero-rating programs.',
    'If general data pricing has fallen enough that caps rarely bind, the coordination-function justification weakens over time even as the entry-barrier effect persists — this would push the classification trajectory from tangled_rope toward snare as the interval extends.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subscriber_benefit_durability, empirical, 'Whether the founding coordination benefit is decaying while the extractive effect persists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(tcp__tr_t16, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(tcp__tr_t24, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(tcp__be_t16, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(tcp__be_t24, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(tcp__su_t16, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(tcp__su_t24, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__zero_rating_reading, 0.12).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the colloquial 'net neutrality debate' / 'what does TCP/IP require' question, per the ε-invariance principle: the neutrality_reading (non-discrimination mandate), the prioritization_reading (permitted network-management differentiation), and this zero_rating_reading (permitted sponsored-content exemption) each instantiate structurally distinct claims with different beneficiary/victim sets and different ε values. They share a kernel (tcp_ip_interpretation) but are not the same constraint measured three ways — each has its own stable ε assessed from its own reading's standing arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
