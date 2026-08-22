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
 *   human_readable: Zero-Rating Reading of TCP/IP: Sponsored Data Exemption Authority
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This story instantiates the zero_rating_reading of the contested TCP/IP
 *   interpretation kernel: the claim that end-to-end packet transport permits
 *   ISPs to selectively exempt sponsored content from subscriber data caps,
 *   treating this as a legitimate commercial packaging decision rather than a
 *   violation of protocol-level neutrality. Under this reading, ISPs
 *   negotiate exemption deals with content providers (typically
 *   well-resourced incumbents), and the exempted traffic is metered
 *   differently from non-exempted traffic traversing the identical protocol
 *   stack. The reading is distinct from the neutrality_reading (which treats
 *   any differential treatment as a violation of the end-to-end principle)
 *   and from the prioritization_reading (which addresses differentiated
 *   service quality/latency as network management, not data-cap accounting).
 *   This story authors only the zero-rating claim's own structure: its own
 *   beneficiaries, victims, and extraction profile, without averaging across
 *   or referencing the siblings' verdicts.
 *
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
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "Zero-Rating Reading of TCP/IP: Sponsored Data Exemption Authority").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, 'd684debe-7c21-44c7-bc36-34073c94ad12').
narrative_ontology:cs_kernel_codification('d684debe-7c21-44c7-bc36-34073c94ad12', distributed).
narrative_ontology:cs_authority_grounding('d684debe-7c21-44c7-bc36-34073c94ad12', distributed).
narrative_ontology:cs_reading_relation('d684debe-7c21-44c7-bc36-34073c94ad12', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('d684debe-7c21-44c7-bc36-34073c94ad12', tcp_ip_interpretation__prioritization_reading, influences).
narrative_ontology:cs_axiom('d684debe-7c21-44c7-bc36-34073c94ad12', foundational, billing_layer_exempt_from_neutrality_scope).
narrative_ontology:cs_axiom_status(billing_layer_exempt_from_neutrality_scope, holdable).
narrative_ontology:cs_axiom_grounding('d684debe-7c21-44c7-bc36-34073c94ad12', billing_layer_exempt_from_neutrality_scope, conventional).
narrative_ontology:cs_axiom('d684debe-7c21-44c7-bc36-34073c94ad12', secondary, commercial_sponsorship_is_legitimate_packaging).
narrative_ontology:cs_axiom_status(commercial_sponsorship_is_legitimate_packaging, holdable).
narrative_ontology:cs_axiom_grounding('d684debe-7c21-44c7-bc36-34073c94ad12', commercial_sponsorship_is_legitimate_packaging, instrumental).
narrative_ontology:cs_reference_frame('d684debe-7c21-44c7-bc36-34073c94ad12', protocol_silence_as_permission).
narrative_ontology:cs_drift_state('d684debe-7c21-44c7-bc36-34073c94ad12', post_2015_regulatory_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d684debe-7c21-44c7-bc36-34073c94ad12', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_platform_operators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, isp_zero_rating_partners).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, independent_app_developers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, unsponsored_content_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, rural_low_income_subscribers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, rural_low_income_subscribers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the last-mile network and decides which content partners get data-cap exemptions under commercial sponsorship deals. Sets the technical and contractual terms for zero-rating, administers the metering exceptions, and collects sponsorship fees or bundled-service revenue from participating platforms. Can exit any regulatory challenge by reframing the practice as a consumer benefit.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, isp_zero_rating_partners, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, isp_zero_rating_partners, beneficiary).

% Large, already-dominant content and social platforms pay ISPs (directly or through bundled partnerships) to have their traffic exempted from subscriber data caps. This cements their position as the default 'free' option for cap-constrained users, widening their usage lead over rivals without needing to compete on service quality.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, incumbent_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Small developers and startups cannot afford sponsorship deals with every major ISP. Their traffic counts against user data caps while incumbent competitors' traffic does not, making their product a comparatively 'costly' choice for cap-sensitive users regardless of underlying quality. Exit means either seeking their own sponsorship deal (usually out of reach) or accepting a durable usage disadvantage.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, independent_app_developers, payer,
    moderate, biographical, constrained, national).

% Early-stage services with no negotiating leverage over ISPs face the same data-cap penalty as independent developers but lack even the resources to attempt a sponsorship negotiation. They are effectively priced out of the cap-constrained user segment before they can establish a user base.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, unsponsored_content_startups, payer,
    powerless, biographical, trapped, national).

% Subscribers on tight data plans get real short-term relief from data charges on zero-rated services, which is a genuine benefit given household budgets. But their consumption is steered toward whichever platforms bought exemptions, narrowing their practical choice set and making them dependent on the sponsored menu rather than open competition; they have no realistic alternative ISP in their area.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, rural_low_income_subscribers, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, rural_low_income_subscribers, beneficiary).

% Evaluate whether zero-rating arrangements violate non-discrimination principles or count as legitimate commercial packaging. Their rulings determine whether the sponsored-exemption practice continues, is restricted, or is banned outright, and they hear competing framings from ISPs, platforms, and competitive-entry advocates.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% Would-be competitors evaluating whether to build a data-intensive service factor in the near-certainty that they cannot match incumbents' sponsored-data reach. Their objection to the practice as an entry barrier is rarely heard in the commercial negotiations that create these exemptions; they are not party to the ISP-platform contracts that shape the market they would enter.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, prospective_market_entrants, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__zero_rating_reading, isp_zero_rating_partners).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__zero_rating_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Zero-rating genuinely solves a real problem for cap-constrained users: it lets ISPs offer meaningful data relief on high-value services without redesigning the underlying metering and billing infrastructure, and it lets platforms and ISPs strike commercial bundling arrangements that mobile operators use as a competitive differentiator.
% TRANSFER_FUNCTION: Moves effective network access and user attention from unsponsored competitors to sponsoring incumbents; moves sponsorship revenue from platforms to ISPs; moves genuine but narrowly-channeled cost relief to cap-constrained subscribers in exchange for narrowing their effective choice set to the sponsored menu.
% ABSENT_VOICES: Prospective market entrants and unsponsored startups who would compete on merit are not present at the ISP-platform negotiating table where exemption terms are set; their objection — that the practice functions as a pay-to-play toll on competitive entry — surfaces only in regulatory comment periods, if at all.
% DISAPPEARANCE_RATIONALE: If sponsored-content exemptions were banned overnight, incumbent platforms would lose their differential data-cost advantage, cap-constrained subscribers would face equal metering across all services (a real short-term loss of relief but a restoration of comparative choice), and ISPs would lose a bundling revenue stream, prompting them to seek alternative differentiation strategies. Independent developers and startups would compete on a metering-neutral basis for the first time.
% FOUNDING_PROBLEM: Mobile and fixed broadband networks have finite capacity and subscribers on metered or capped plans face real cost barriers to using data-intensive services; zero-rating was framed as a way to extend affordable access to specific services without requiring a full data-plan upgrade.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and sponsoring platforms attest the founding problem (data-cost barriers for low-income users) remains live and that zero-rating is a direct answer to it. Telecom regulators in several jurisdictions and independent competition economists attest that the practice has shifted from access-extension to competitive gatekeeping — the affordability problem is real but the sponsored-exemption structure now primarily entrenches incumbents rather than solving affordability broadly, since a genuinely neutral solution (e.g. general cap relief or subsidized base data) would address the same problem without the exemption's entry-barrier effect.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction (0.68 at interval end) reflects that the exemption mechanism, once established, systematically channels subscriber attention and developer investment toward whichever platforms can afford sponsorship, independent of underlying merit or network cost. Suppression (0.58) is moderate: no subscriber is technically barred from using unsponsored services, but the accumulated cap-cost disadvantage functions as a soft barrier that grows as sponsored platforms entrench their usage share. Theater ratio (0.42) captures that a real affordability function persists (some low-income users get genuine relief) alongside a growing performative element — 'consumer benefit' framing used to defend a practice that increasingly serves incumbent entrenchment.
 *
 * PERSPECTIVAL GAP:
 *   From the ISP/incumbent-platform seat, this is coordination: solving a real budget problem for subscribers through commercial partnership, no different from any bundled service offer. From the independent-developer and unsponsored-startup seat, the identical mechanism is a toll gate: their traffic is charged, the incumbent's is not, for reasons unrelated to network cost or service quality. The engine computes these as structurally different seat experiences from the same beneficiary/victim declarations — the claim (tangled_rope) is authored to reflect that both readings are simultaneously true of different seats, not to resolve which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs and incumbent platforms sit near the beneficiary end: ISPs administer and collect from the exemption structure, platforms receive a durable usage-share subsidy relative to competitors. Independent developers and unsponsored startups sit near the target end: they pay the parallel structural cost of NOT being exempted, a cost that scales with their inability to negotiate sponsorship. Rural low-income subscribers are the hardest case for directionality: they receive a genuine, if narrow, benefit (real dollars saved on sponsored services) while simultaneously bearing the cost of a narrowed effective choice set — this is modeled with a dual role (beneficiary + payer) rather than forcing a single direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (affordability for cap-constrained users) is genuinely live in many markets, which prevents this reading from being mislabeled as pure extraction with no coordination function — some sponsored-data relief is a real transfer to real budget-constrained households. But the founding-problem status is marked contested rather than resolved: regulators and competition economists observe that a neutral fix (general cap relief, subsidized base data) would address the same affordability problem without entrenching incumbents, meaning the current structure persists substantially because it profits ISPs and incumbent platforms, not because it is the only available solution to the founding problem. This is the tangled_rope signature: a real coordination function (affordability relief) coexisting with asymmetric extraction (competitive entrenchment) requiring active enforcement (contractual exemption administration) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Is the TCP/IP protocol''s silence on billing/metering treatment a genuine license for sponsored-data arrangements (this reading), an implicit prohibition under the end-to-end principle (neutrality_reading), or a separate question entirely from service-quality prioritization (prioritization_reading)?',
    'The disagreement is located in whether ''non-discrimination'' in TCP/IP''s original design intent refers to packet handling/delivery order (prioritization_reading''s domain) or extends to the billing/accounting layer that determines whether a packet counts against a cap (this reading''s domain). A sibling reading would change the structural classification: under neutrality_reading, the exemption itself constitutes the violation regardless of the commercial framing; under this reading, exemption is permissible commercial packaging layered atop a neutral transport layer.',
    'If future protocol governance rulings (e.g. from IETF or national regulators) formally locate billing treatment within the neutrality principle''s scope, this reading''s core premise (that TCP/IP permits sponsored-data exemption) would be substantially weakened, likely triggering reclassification toward snare as the coordination defense collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Where the zero-rating reading''s core license claim sits relative to the neutrality reading''s prohibition claim.').

omega_variable(
    affordability_function_durability,
    'Does the zero-rating mechanism''s affordability benefit for low-income subscribers persist as a genuine coordination function, or is it a transitional justification that will be argued away once incumbent entrenchment is politically secure?',
    'Track whether ISPs and sponsoring platforms continue expanding zero-rating to smaller/independent developers over time (supporting genuine affordability function) versus concentrating exemptions exclusively among the largest incumbents (supporting entrenchment-only function).',
    'If exemption access remains concentrated among incumbents with declining marginal expansion to independent developers, the coordination function claim weakens further and the constraint drifts from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(affordability_function_durability, empirical, 'Whether the affordability coordination function is durable or a fading cover story.').

omega_variable(
    regulatory_framing_capture,
    'Is the ''consumer benefit'' framing used by ISPs and incumbent platforms a good-faith account of a real benefit, or a captured framing designed to forestall regulatory reclassification of the practice as discriminatory?',
    'Compare independent (non-ISP-funded) economic analyses of subscriber welfare outcomes in jurisdictions with zero-rating against jurisdictions that ban it, controlling for baseline affordability policy.',
    'If independent analysis finds subscriber welfare outcomes are equivalent or better under a ban-plus-general-subsidy regime, the consumer-benefit framing is substantially undercut and the theater_ratio for this reading should be revised upward in future stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_framing_capture, empirical, 'Whether the consumer-benefit defense of zero-rating is genuine or a captured regulatory narrative.').


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
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(tcp__tr_t16, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(tcp__tr_t24, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(tcp__be_t16, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 16, 0.63).
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
% This constraint is one of three sibling readings decomposed from the natural-language label 'net neutrality debates over TCP/IP interpretation,' per the ε-invariance principle: neutrality_reading (strict non-discrimination, likely mountain-adjacent or rope depending on enforcement), prioritization_reading (differentiated service quality as network management, likely rope or tangled_rope depending on abuse), and this zero_rating_reading (sponsored-content cap exemptions, tangled_rope). Each reading has its own ε, beneficiary/victim structure, and classification because each addresses a structurally distinct claim about what TCP/IP's design permits. They are linked here via affects_constraints because a regulatory or engineering ruling on one reading's legitimacy structurally pressures the others' legitimacy conditions (e.g., a strong neutrality_reading ruling would tend to foreclose or restrict this zero_rating_reading's continued operation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
