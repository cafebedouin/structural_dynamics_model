% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_interpretation__neutrality_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: tcp_ip_interpretation__neutrality_reading
 *   human_readable: TCP/IP End-to-End Principle — Neutrality Reading
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This story instantiates the neutrality reading of the TCP/IP end-to-end
 *   principle kernel: the claim that the protocol's design commits network
 *   operators to non-discriminatory packet handling as a matter of
 *   architectural necessity extended into policy, foreclosing content- or
 *   application-based differentiation. Two sibling readings of the same
 *   underlying kernel — a prioritization reading (differentiated service
 *   quality as legitimate network management) and a zero-rating reading
 *   (selective sponsored-content exemptions) — are separate constraints with
 *   their own ε values and are not described further here; see
 *   kernel_context. This reading treats the standing arrangement under
 *   contest as the enforced non-discrimination regime as currently
 *   implemented via net neutrality rules, assessed by this reading's own
 *   lights.
 *
 * KEY AGENTS:
 *   - telecom_regulators: agenda_setter (institutional/analytical) — administers and enforces the non-discrimination reading
 *   - last_mile_isps: primary payer (powerful/constrained) — bears the constraint's foreclosed revenue models
 *   - edge_application_developers: primary beneficiary (moderate/mobile) — gains guaranteed non-discriminatory reach
 *   - small_content_publishers: beneficiary (powerless/constrained) — depends on non-discrimination absent negotiating leverage
 *   - internet_users: beneficiary (organized/constrained) — receives protection substituting for weak access-market competition
 *   - network_infrastructure_investors: payer (organized/mobile) — bears foreclosed differentiated-pricing return models
 *   - isp_affiliated_content_services: excluded (powerful/trapped) — self-preferencing interest structurally ruled out by design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.58).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.42).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Principle — Neutrality Reading").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, '8f52e306-d472-4b65-a83f-dcb8fc3e9f7e').
narrative_ontology:cs_kernel_codification('8f52e306-d472-4b65-a83f-dcb8fc3e9f7e', distributed).
narrative_ontology:cs_authority_grounding('8f52e306-d472-4b65-a83f-dcb8fc3e9f7e', distributed).
narrative_ontology:cs_reading_relation('8f52e306-d472-4b65-a83f-dcb8fc3e9f7e', tcp_ip_interpretation__prioritization_reading, forecloses).
narrative_ontology:cs_reading_relation('8f52e306-d472-4b65-a83f-dcb8fc3e9f7e', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('8f52e306-d472-4b65-a83f-dcb8fc3e9f7e', foundational, packet_agnosticism_is_architecturally_mandated).
narrative_ontology:cs_axiom_status(packet_agnosticism_is_architecturally_mandated, holdable).
narrative_ontology:cs_axiom_grounding('8f52e306-d472-4b65-a83f-dcb8fc3e9f7e', packet_agnosticism_is_architecturally_mandated, conventional).
narrative_ontology:cs_axiom('8f52e306-d472-4b65-a83f-dcb8fc3e9f7e', secondary, commercial_differentiation_by_content_is_illegitimate_regardless_of_technical_feasibility).
narrative_ontology:cs_axiom_status(commercial_differentiation_by_content_is_illegitimate_regardless_of_technical_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('8f52e306-d472-4b65-a83f-dcb8fc3e9f7e', commercial_differentiation_by_content_is_illegitimate_regardless_of_technical_feasibility, instrumental).
narrative_ontology:cs_reference_frame('8f52e306-d472-4b65-a83f-dcb8fc3e9f7e', end_to_end_architectural_neutrality).
narrative_ontology:cs_drift_state('8f52e306-d472-4b65-a83f-dcb8fc3e9f7e', post_2015_open_internet_order_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('8f52e306-d472-4b65-a83f-dcb8fc3e9f7e', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_application_developers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, small_content_publishers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, internet_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, last_mile_isps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, network_infrastructure_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the non-discrimination reading of the end-to-end principle: bans paid prioritization, throttling by content type, and discriminatory traffic shaping absent narrowly defined network-management justification. Sets the interpretive frame that ISPs must operate within and can revise it through rulemaking.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, telecom_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Owns the physical last-mile infrastructure and bears the constraint's costs directly: cannot charge content or application providers for prioritized delivery, cannot use traffic differentiation as a revenue lever, and must treat congestion management as narrowly technical rather than commercial. Argues this reading forecloses legitimate service-tier products that would fund network buildout.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, last_mile_isps, payer,
    powerful, biographical, constrained, national).

% Builds and deploys applications at the network edge without needing commercial arrangements with every ISP whose pipes their traffic crosses. Under this reading, a small streaming or messaging startup reaches users on the same technical terms as an incumbent — no toll can be levied for delivery, no throttling risk for competing with an ISP's own affiliated service.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_application_developers, beneficiary,
    moderate, biographical, mobile, global).

% Depends on being reachable by users without paying interconnection or prioritization fees to ISPs. Has no leverage to negotiate individually with network operators and would be squeezed out first under a discrimination-permissive regime; the neutrality reading is what keeps their content deliverable at the same technical priority as anyone else's.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, small_content_publishers, beneficiary,
    powerless, biographical, constrained, national).

% Accesses whatever content and applications it chooses without the ISP steering traffic toward favored partners or degrading disfavored services. Exit from a given ISP is often limited to one or two providers regionally, so the protection this reading provides substitutes for competitive discipline that the access market itself does not supply.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_users, beneficiary,
    organized, generational, constrained, national).

% Finances last-mile buildout and rural expansion on projected returns; this reading forecloses differentiated-pricing revenue models the investment case sometimes assumed, and argues the constraint depresses expected yield on marginal infrastructure investment, particularly in low-density areas.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, network_infrastructure_investors, payer,
    organized, generational, mobile, national).

% A cable operator's own streaming or telephony service would benefit from favorable routing over rivals, but the neutrality reading prohibits exactly this kind of self-preferencing. This actor's commercial interest is structurally excluded from the arrangement by design, not merely absent from the conversation.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, isp_affiliated_content_services, excluded,
    powerful, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees that any packet, from any sender, is delivered on the same technical terms regardless of its content or application — letting edge innovators build on the network without needing individual commercial relationships with every carrier between them and their users.
% TRANSFER_FUNCTION: Prevents ISPs from converting last-mile control into a toll collected from content and application providers; the value that would have flowed to ISPs as prioritization or exemption fees instead accrues to edge developers, publishers, and users as unconstrained reach and access.
% ABSENT_VOICES: ISP-affiliated content services and prospective differentiated-service customers (e.g., telemedicine or industrial IoT operators wanting guaranteed low-latency lanes) would argue for permitted differentiation; they are foreclosed by this reading's own terms, not merely unheard in an otherwise open forum.
% DISAPPEARANCE_RATIONALE: If the neutrality reading were abandoned, ISPs would move quickly to monetize prioritization and exemption arrangements, edge providers without negotiating leverage would face new tolls or degraded delivery, and the economics of launching a new internet service would shift toward requiring carriage deals — a materially different internet economy would emerge within a few product cycles.
% FOUNDING_PROBLEM: Early internet architecture needed a rule preventing the operators of physical transmission infrastructure from using that chokepoint to pick winners among the applications and content running over it, so that innovation could happen at the edge without permission from the carriers in the middle.
% FOUNDING_PROBLEM_CORROBORATION: Independent competition economists and consumer advocacy groups outside the beneficiary set (edge developers, publishers) corroborate that access-market concentration persists and that the underlying chokepoint problem the principle addresses has not resolved. ISPs and their investors dispute the framing, arguing the problem has evolved into a legitimate need for differentiated quality-of-service products that the current reading forecloses without technical justification.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tcp_ip_interpretation__neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-rising 0.58: from the ISP/investor seat, the rule constrains legitimate revenue optimization and forecloses pricing models that could fund infrastructure, which those payers experience as an extractive constraint on their commercial latitude. From the edge-developer/publisher/user seats the same rule is closer to protective coordination. Suppression (0.42) reflects real but partial enforcement machinery — the rule is actively policed through complaint mechanisms and regulatory rulemaking, but is not maximally coercive since technical network-management exceptions remain available and jurisdictional variation exists. Accessibility collapse is moderate (0.4): ISPs retain some room to design non-discriminatory tiered general service, so alternatives are not fully foreclosed for them, only the discriminatory subset. Resistance is high (0.68): ISPs and infrastructure investors actively litigate, lobby, and seek rule rollback, which is the clearest evidence this is a live, contested constraint rather than settled background law.
 *
 * DIRECTIONALITY LOGIC:
 *   Edge developers, small publishers, and internet users are declared beneficiaries: the rule subsidizes their access and reach by removing a toll ISPs could otherwise levy, so their derived directionality sits toward the beneficiary end. Last-mile ISPs and infrastructure investors are declared victims of this reading's cost structure: their revenue latitude is directly constrained by the enforced non-discrimination rule, pushing their derived directionality toward the target end. ISP-affiliated content services are excluded rather than victimized in the payer sense — their self-preferencing option is foreclosed by design, which is different from bearing a cost they would otherwise not incur.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents this reading from being mislabeled as pure extraction against ISPs or as costless pure coordination for edge actors. The founding problem (chokepoint control blocking edge innovation) remains live by outside corroboration (competition economists, advocacy groups), which argues against a mandatrophy verdict — the arrangement is not merely inertial theater sustained by initial justification. But the ISP/investor side's contestation is genuine and structurally grounded, not merely sour grapes, which is why the constraint requires active enforcement rather than persisting as pure voluntary coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Is the TCP/IP end-to-end principle a technical architectural fact that logically mandates non-discrimination, or is ''non-discrimination'' a policy gloss layered onto a protocol that is itself neutral on commercial differentiation?',
    'This is the central disagreement location among the three kernel readings (neutrality, prioritization, zero-rating): the prioritization reading holds that the protocol supports differentiated service quality as legitimate network management without violating end-to-end design; the zero-rating reading holds that selective exemption for sponsored content is compatible with non-discrimination properly understood. No purely technical fact resolves this because the protocol specification itself is silent on commercial policy — resolution would require either judicial/regulatory settlement of the interpretive question or broad multi-stakeholder consensus that does not currently exist.',
    'If the prioritization or zero-rating readings prevail structurally (in law or dominant practice), this neutrality reading''s classification shifts from a live, enforced tangled_rope toward a contested or superseded reading with much lower effective enforcement and correspondingly different extraction distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Location of the kernel contest: technical necessity vs. policy interpretation of end-to-end principle.').

omega_variable(
    natural_law_vs_constructed_rule,
    'Is the non-discrimination requirement an inherent property of how packet-switched networks must operate, or is it a constructed regulatory choice that happens to track the technical architecture without being required by it?',
    'Comparative analysis of packet-switched network implementations that DO implement differentiated service (e.g., MPLS quality-of-service tiers, private enterprise networks with prioritized traffic classes) — if these operate successfully as packet-switched TCP/IP networks, non-discrimination is not architecturally necessary, only policy-preferred.',
    'If non-discrimination is shown to be a constructed policy choice rather than technical necessity, the neutrality reading''s claim to inevitability weakens substantially and its classification moves further from any mountain-adjacent framing toward pure constructed coordination-with-extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_rule, empirical, 'Whether non-discrimination is technically necessitated or a superimposed policy choice.').

omega_variable(
    infrastructure_investment_causal_link,
    'Does the neutrality reading''s foreclosure of differentiated pricing actually depress last-mile infrastructure investment, or is the investment effect negligible relative to other market factors (competition, subsidy programs, demand growth)?',
    'Longitudinal comparison of infrastructure buildout rates in jurisdictions with strict neutrality enforcement versus jurisdictions permitting differentiated service arrangements, controlling for market structure and subsidy regimes.',
    'If the causal link is weak, the ISP/investor victim framing is substantially overstated and the constraint''s extraction claim against them should be revised downward; if strong, the tangled_rope classification''s asymmetric-extraction gate is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(infrastructure_investment_causal_link, empirical, 'Whether the constraint measurably depresses infrastructure investment as claimed by payer stakeholders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tcp__tr_t5, tcp_ip_interpretation__neutrality_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(tcp__tr_t10, tcp_ip_interpretation__neutrality_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(tcp__tr_t15, tcp_ip_interpretation__neutrality_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__neutrality_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(tcp__tr_t25, tcp_ip_interpretation__neutrality_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tcp__be_t5, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(tcp__be_t10, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(tcp__be_t25, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(tcp__su_t5, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement(tcp__su_t10, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(tcp__su_t15, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(tcp__su_t25, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__neutrality_reading, 0.12).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'TCP/IP end-to-end principle' kernel. The neutrality_reading (this file) treats enforced non-discrimination as substantially extractive against ISPs and infrastructure investors while coordinating edge innovation. The prioritization_reading treats differentiated quality-of-service as legitimate network management with a different beneficiary/victim structure. The zero_rating_reading treats selective sponsored-content exemption as an access-expansion mechanism with yet another structure. Per the ε-invariance principle, these are three distinct constraints sharing a kernel, not one constraint measured three ways — each carries its own ε, beneficiaries, victims, and classification, linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
