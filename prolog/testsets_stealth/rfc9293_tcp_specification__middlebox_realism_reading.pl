% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__middlebox_realism_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: Middlebox-Mediated TCP Operation (Middlebox Realism Reading of RFC 9293)
 *   domain: technological/infrastructural/governance
 *
 * SUMMARY:
 *   RFC 9293 (2022) is the canonical specification of TCP. This story
 *   instantiates the middlebox_realism_reading of the
 *   rfc9293_tcp_specification kernel: the text describes ideal endpoint
 *   behavior, but the operative constraint on real TCP is the deployed
 *   middlebox population — NATs, stateful firewalls, DPI devices — and
 *   specification authority is therefore subordinate to what the network
 *   actually does. The standing arrangement under contest is
 *   middlebox-mediated TCP operation, and epsilon is authored for THAT
 *   arrangement as this reading sees it: a structure with a genuine
 *   coordination face (address mediation during IPv4 scarcity, local policy
 *   enforcement without global consensus, legacy translation) and an
 *   asymmetric transfer face (control over connection behavior and traffic
 *   visibility moves from endpoints to on-path actors; protocol evolution is
 *   taxed; surveillance capability accrues without endpoint consent). Claim
 *   and metrics are independent authored facts: claimed_type tangled_rope is
 *   asserted from the two-faced structure; the metric values are authored
 *   from the descriptive record of the arrangement's actual operation. The
 *   strict_invariance_reading and optimization_latitude_reading are separate
 *   constraints over the same kernel text, linked via
 *   network.affects_constraints; their epsilon values differ because they are
 *   different constraints, not different views of one.
 *
 * KEY AGENTS:
 *   - isp_network_operators: de facto agenda-setter and primary beneficiary (institutional/arbitrage) — operates the NAT/DPI fleets that determine wire behavior and collects control, address-economics savings, and policy capability
 *   - state_surveillance_agencies: secondary beneficiary (institutional/arbitrage) — derives scalable intelligence from on-path visibility that endpoints never granted
 *   - enterprise_security_teams: beneficiary within their own edges (powerful/constrained) — purchases and configures the enforcement apparatus, invested in the perimeter model
 *   - endpoint_application_developers: primary target (organized/constrained) — bears lost connection autonomy and the engineering tax of surviving arbitrary interference
 *   - protocol_innovators: target (moderate/constrained) — new transports are broken or forced to disguise themselves by the ossified deployed population
 *   - end_users: diffuse target with incidental benefit (powerless/trapped) — bears surveillance exposure and breakage, receives edge filtering and working NAT connectivity
 *   - ietf_specification_community: formal agenda-setter, effectively excluded (institutional/identity_locked) — authors the kernel text whose governing force this reading denies; responds by designing for a hostile path
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.7).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.68).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "Middlebox-Mediated TCP Operation (Middlebox Realism Reading of RFC 9293)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "technological/infrastructural/governance").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '24ddc691-2c67-47de-9f51-e67adc394b2f').
narrative_ontology:cs_kernel_codification('24ddc691-2c67-47de-9f51-e67adc394b2f', fixed_text).
narrative_ontology:cs_authority_grounding('24ddc691-2c67-47de-9f51-e67adc394b2f', expertise).
narrative_ontology:cs_interpretation_layer_present('24ddc691-2c67-47de-9f51-e67adc394b2f').
narrative_ontology:cs_reading_relation('24ddc691-2c67-47de-9f51-e67adc394b2f', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('24ddc691-2c67-47de-9f51-e67adc394b2f', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_axiom('24ddc691-2c67-47de-9f51-e67adc394b2f', foundational, wire_behavior_constitutes_the_standard).
narrative_ontology:cs_axiom_status(wire_behavior_constitutes_the_standard, holdable).
narrative_ontology:cs_axiom_grounding('24ddc691-2c67-47de-9f51-e67adc394b2f', wire_behavior_constitutes_the_standard, empirically_contingent).
narrative_ontology:cs_axiom('24ddc691-2c67-47de-9f51-e67adc394b2f', foundational, on_path_control_prerogative).
narrative_ontology:cs_axiom_status(on_path_control_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('24ddc691-2c67-47de-9f51-e67adc394b2f', on_path_control_prerogative, conventional).
narrative_ontology:cs_reference_frame('24ddc691-2c67-47de-9f51-e67adc394b2f', aspirational_endpoint_behavior_model).
narrative_ontology:cs_drift_state('24ddc691-2c67-47de-9f51-e67adc394b2f', contemporary_middlebox_deployments, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('24ddc691-2c67-47de-9f51-e67adc394b2f', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, isp_network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_security_teams).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_application_developers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, protocol_innovators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, end_users).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, operator_path_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the on-path equipment — NAT gateways, stateful firewalls, DPI boxes — through which subscriber traffic passes. They decide what packet shapes their networks tolerate, rewrite addresses, terminate idle connections, and refresh inspection rulesets on vendor timelines. Address translation lets them serve many customers with scarce IPv4 space instead of completing IPv6 rollout. Their exit is wide: they choose which equipment to deploy and can replace entire fleets; nothing binds them to any particular protocol posture.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, isp_network_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, isp_network_operators, beneficiary).

% Run perimeter policy for their organizations: egress filtering, application awareness, data-loss prevention at the edge. They receive enforcement capability they could not practically obtain endpoint-by-endpoint, and they purchase and configure the boxes that provide it. Their commitment runs deep — security architecture, compliance certifications, and staff skills are built around the perimeter model — so moving to endpoint-centric trust models would mean re-architecting decades of accumulated practice.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_security_teams, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_security_teams, agenda_setter).

% Obtain visibility into traffic that endpoints never knowingly granted: metadata, handshake fingerprints, and — where encryption permits or is bypassed — content. On-path collection scales cheaply compared to per-device compromise. Their position is indifferent to any single network's choices; they adapt collection techniques to whatever the deployed population allows.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies, beneficiary,
    institutional, generational, arbitrage, national).

% Build software that must traverse networks they do not control. Connections fail or degrade depending on which boxes sit on the path; they engineer retries, fallbacks, and disguise strategies such as wrapping traffic in TLS on port 443 to survive arbitrary interference. They cannot select their users' paths, and shipping a new transport means years of accommodating hostile intermediaries.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_application_developers, payer,
    organized, biographical, constrained, global).

% Design successor transports and extensions. Deployed middleboxes strip unknown options, block unfamiliar port and protocol combinations, and freeze header layouts, so new protocols must either masquerade as tolerated traffic or encrypt everything to be opaque. The path to deployment runs through the very population whose behavior they are trying to change; leaving public-standards work for closed platforms is the main alternative exit.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, protocol_innovators, payer,
    moderate, generational, constrained, global).

% Send traffic across infrastructure they cannot see or choose. They receive incidental protection from edge filtering and working connectivity from address translation, and they bear surveillance exposure, latency added by inspection, and breakage when boxes mishandle novel traffic. Switching providers rarely changes the class of equipment sitting on the path.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, end_users, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, end_users, beneficiary).

% Authors and maintains RFC 9293 and the surrounding standards process; publishes normative requirements for endpoint behavior and documents known middlebox interactions. Its determinations bind no one on the wire: operators deploy what vendors ship, and vendors ship what buyers request. The community responds by designing for a hostile path — encrypting transports, padding, disguising — rather than by commanding it. Members' professional identities are fused with the open-standards project, so abandoning the effort is unthinkable even as its writ fails to run.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_specification_community, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, ietf_specification_community, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__middlebox_realism_reading, isp_network_operators).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__middlebox_realism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mediates scarce IPv4 addresses across many hosts per address (NAT), lets each network owner enforce local policy at its edge without global agreement, filters unsolicited inbound traffic, and translates between protocol generations so heterogeneous networks interoperate. These functions are performed by the same boxes that inspect and modify traffic.
% TRANSFER_FUNCTION: Moves control over connection behavior and visibility into traffic from endpoints and their users to whichever actor operates the on-path equipment; moves protocol-evolution costs onto innovators, who must accommodate or evade the deployed population; moves intelligence value to operators and states.
% ABSENT_VOICES: End users are absent from every procurement and ruleset decision that shapes their traffic; application developers typically learn of new interference from failure reports after deployment; the IETF's recorded objection that pervasive monitoring is an attack (RFC 7258 / BCP 188) carries no vote in operator purchasing. Each sits outside the vendor-operator conversation where wire behavior is actually settled.
% DISAPPEARANCE_RATIONALE: Overnight removal would break IPv4-only addressing for hundreds of millions of hosts behind NAT, strip enterprise perimeters bare, eliminate bulk on-path collection, and immediately un-tax protocol evolution — new transports would deploy directly to endpoints. The internet's current shape depends on the arrangement.
% FOUNDING_PROBLEM: IPv4 address exhaustion in the early 1990s, plus network owners' demand to enforce local policy on traffic crossing their links without waiting for global consensus. NAT and the firewall industry were built to solve these two problems together.
% FOUNDING_PROBLEM_CORROBORATION: Address-exhaustion records from the regional registries (IANA/RIPE/ARIN depletion reports) corroborate the original scarcity problem from outside the operator set; two decades of independent measurement research (IMC/SIGCOMM middlebox studies) corroborates the population's prevalence and interference with the specified protocol; the IETF's BCP 188 and privacy advocates attest the control-transfer side. Operator trade literature attests only the policy-enforcement half — no beneficiary-party source seriously disputes that the address-scarcity justification has weakened as IPv6 deployment has grown, which is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.70: control over connection behavior, traffic visibility, and protocol-evolution latitude transfers to on-path actors without endpoint delegation, discounted by residual endpoint sovereignty (stacks still run, TLS limits payload access). Suppression 0.68: persistence now depends on actively maintained enforcement — refreshed DPI rulesets, throttling and blocking of encrypted evasions (early QUIC throttling, ECH filtering) — not on participant preference; the suppression_requirement series is authored because this story specifically tracks enforcement-capacity buildup from passive translation (0.20) to counter-evasion hardening (0.68). Theater 0.32: NAT and basic filtering do real work, but a growing share of DPI activity is marketed 'value-add' whose security contribution is difficult to distinguish from checkbox compliance. Accessibility_collapse 0.50: partial exits exist (encrypted transports, overlays, VPNs) but every practical path crosses someone's middleboxes, so alternatives narrow without vanishing. Resistance 0.60: QUIC, TLS 1.3, Encrypted Client Hello, BCP 188's 'pervasive monitoring is an attack' ruling, and coordinated browser-vendor deployment (an example of coalition power among otherwise constrained developers) constitute sustained, partly successful pushback. Boltzmann coordination_type resource_allocation: NAT's mediation of scarce IPv4 addresses is the load-bearing coordination function — if it failed, the IPv4 internet breaks — so the type default floor applies without override. All three metric series share one time grid (t=0,5,10,16,21,26,31 on a 1994–2025 mapping) so no metric row is backfilled.
 *
 * PERSPECTIVAL GAP:
 *   The operator seat experiences the arrangement as legitimate network management and property rights over owned infrastructure: they bought the boxes, they answer for their network's behavior, local policy is their prerogative. The endpoint-developer and innovator seats experience the same structure as expropriated autonomy and a tax on evolution levied by unaccountable intermediaries. The IETF seat experiences its own authority as circumvented — it publishes requirements that bind no one on the wire. The user seat mostly experiences nothing at all: the structure is invisible until it breaks something. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place isp_network_operators, enterprise_security_teams, and state_surveillance_agencies near the subsidized end (low d), amplified for the agencies by arbitrage-grade exit and for operators by their agenda-setting position. Victim declarations place endpoint_application_developers, protocol_innovators, and end_users near the full-target end (high d), with end_users slightly moderated by their secondary beneficiary position (incidental filtering and connectivity gains) and pushed toward full-target by trapped exit. The ietf_specification_community sits outside the beneficiary/victim declaration set — its position (formal authority, no wire jurisdiction) is carried by its role and situation rather than by a directionality input; no override is authored because the per-power-atom override mechanism would misstate the named groups sharing its power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (IPv4 address scarcity) is substantially resolved by IPv6, yet the arrangement has persisted and intensified — classic mandate-outlived-function pressure, visible in the rising suppression_requirement series tracking enforcement buildup rather than coordination need. Classification as tangled_rope rather than snare or piton keeps both faces honest: genuine residual coordination functions (local policy enforcement demand, legacy translation, incomplete IPv6 deployment) prevent a pure-extraction reading, while concentrated beneficiaries (operators collecting control and savings, agencies collecting intelligence) prevent an inertial-piton reading. The contested founding_problem_status paired with world_rearranges disappearance verdict feeds the mismatch check without gaming it: if IPv6 completion ever renders address mediation vestigial while enforcement persists, expect drift toward harder forms — the measurements' divergence between flatlining extractiveness growth and steeply rising suppression requirement traces exactly that handoff from coordination justification to enforcement self-justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the rfc9293_tcp_specification kernel is structurally correct: an invariant state machine implementations must replicate exactly (strict_invariance_reading), specified outcomes with implementation latitude (optimization_latitude_reading), or an aspirational endpoint model subordinate to deployed middlebox behavior (this reading)?',
    'Cross-reading corpus comparison plus wire-measurement evidence: if observed interoperability failures trace to implementation deviation from the text, the invariance reading gains; if they trace to middlebox interference with compliant implementations, this reading gains.',
    'Sibling readings yield different victim sets (deviant implementers vs. none vs. endpoint autonomy) and different extraction profiles; the classification of the whole constraint family shifts with the resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This story is one reading of the rfc9293_tcp_specification kernel; sibling readings instantiate structurally different constraints over the same text.').

omega_variable(
    middlebox_necessity_vs_rent,
    'Is the deployed middlebox population a live coordination necessity (address scarcity where IPv6 lags, genuine edge security) or rent-seeking infrastructure whose founding justifications have decayed?',
    'Compare security outcomes and addressing economics between networks operating heavy middlebox fleets and comparable networks operating transparently; evaluate IPv6-completion counterfactuals for the address-mediation function.',
    'If rent dominates, the arrangement trends toward pure extraction with a cover-story coordination function; if necessity dominates, part of the measured extraction is irreducible coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_necessity_vs_rent, empirical, 'Whether the coordination face of the arrangement is live or vestigial.').

omega_variable(
    surveillance_anchor_question,
    'Does state-surveillance demand anchor the arrangement''s persistence more strongly than operator economics, such that removing the commercial justifications would leave the control-transfer core intact?',
    'Natural experiments where commercial middleboxes were retired (regulatory mandates, corporate decommissioning) contrasted with jurisdictions where state demand sustains fleets regardless of commercial value.',
    'If surveillance anchors persistence, the beneficiary structure concentrates and the arrangement hardens; if operator economics anchor it, remediation through market and regulatory incentives is feasible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(surveillance_anchor_question, empirical, 'Which beneficiary class anchors the arrangement''s persistence.').

omega_variable(
    encryption_arms_race_trajectory,
    'Will ubiquitous encryption (QUIC, TLS 1.3, Encrypted Client Hello) shrink the middlebox population''s control over traffic, or will counter-evasion enforcement keep pace and entrench it?',
    'Extend the measurement series beyond the interval: a falling suppression_requirement with stable extractiveness indicates successful endpoint exit; a continued rise indicates entrenchment.',
    'Endpoint success would push the arrangement toward vestigial maintenance of a shrinking control surface; entrenchment pushes it toward harder forms with the specification''s authority reduced further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encryption_arms_race_trajectory, empirical, 'Whether the endpoint-exit strategy succeeds or the enforcement response entrenches the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 31).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(rfc9_tr_t0, observed).
narrative_ontology:measurement(rfc9_tr_t5, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(rfc9_tr_t5, observed).
narrative_ontology:measurement(rfc9_tr_t10, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(rfc9_tr_t10, observed).
narrative_ontology:measurement(rfc9_tr_t16, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement_basis(rfc9_tr_t16, observed).
narrative_ontology:measurement(rfc9_tr_t21, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 21, 0.28).
narrative_ontology:measurement_basis(rfc9_tr_t21, observed).
narrative_ontology:measurement(rfc9_tr_t26, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 26, 0.3).
narrative_ontology:measurement_basis(rfc9_tr_t26, observed).
narrative_ontology:measurement(rfc9_tr_t31, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 31, 0.32).
narrative_ontology:measurement_basis(rfc9_tr_t31, observed).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(rfc9_be_t0, observed).
narrative_ontology:measurement(rfc9_be_t5, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(rfc9_be_t5, observed).
narrative_ontology:measurement(rfc9_be_t10, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(rfc9_be_t10, observed).
narrative_ontology:measurement(rfc9_be_t16, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement_basis(rfc9_be_t16, observed).
narrative_ontology:measurement(rfc9_be_t21, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 21, 0.66).
narrative_ontology:measurement_basis(rfc9_be_t21, observed).
narrative_ontology:measurement(rfc9_be_t26, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 26, 0.68).
narrative_ontology:measurement_basis(rfc9_be_t26, observed).
narrative_ontology:measurement(rfc9_be_t31, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 31, 0.7).
narrative_ontology:measurement_basis(rfc9_be_t31, observed).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(rfc9_su_t0, observed).
narrative_ontology:measurement(rfc9_su_t5, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement_basis(rfc9_su_t5, observed).
narrative_ontology:measurement(rfc9_su_t10, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(rfc9_su_t10, observed).
narrative_ontology:measurement(rfc9_su_t16, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement_basis(rfc9_su_t16, observed).
narrative_ontology:measurement(rfc9_su_t21, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 21, 0.58).
narrative_ontology:measurement_basis(rfc9_su_t21, observed).
narrative_ontology:measurement(rfc9_su_t26, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 26, 0.64).
narrative_ontology:measurement_basis(rfc9_su_t26, observed).
narrative_ontology:measurement(rfc9_su_t31, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 31, 0.68).
narrative_ontology:measurement_basis(rfc9_su_t31, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, resource_allocation).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__optimization_latitude_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'what RFC 9293 requires of TCP' covers three structurally distinct claims with different epsilon values and different victim sets, so the kernel is authored as three linked stories. The strict_invariance_reading is the traditional upstream account (implementations replicate an invariant machine) and is the version cited in vendor interoperability claims; this middlebox_realism_reading cites two decades of measurement research on deployed middlebox behavior as evidence against the upstream account's descriptive adequacy; the optimization_latitude_reading mediates (outcomes specified, latitude permitted). Each story links to the others via affects_constraints; epsilon here (0.70, referent: the standing middlebox-mediated arrangement as this reading assesses it) is stable within this story and deliberately not averaged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
