% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: tcp_ip_interpretation__neutrality_reading
 *   human_readable: TCP/IP End-to-End Non-Discrimination (Neutrality Reading)
 *   domain: technology governance / internet policy
 *
 * SUMMARY:
 *   This constraint is the neutrality reading of the contested
 *   tcp_ip_interpretation kernel: the claim that TCP/IP's end-to-end
 *   architecture normatively requires broadband ISPs to refrain from content
 *   or application-based discrimination. It is one of three structurally
 *   distinct readings (alongside prioritization and zero-rating), decomposed
 *   per the Îµ-invariance principle because each reading produces a different
 *   Îµ, different beneficiary-victim structures, and different enforcement
 *   requirements. The constraint coordinates a shared transport layer for
 *   global innovation while asymmetrically extracting revenue opportunity
 *   from ISPs.
 *
 * KEY AGENTS:
 *   - broadband_isps (payer/institutional): bear the opportunity cost of prohibited content discrimination and argue the constraint stifles infrastructure investment.
 *   - edge_innovators (beneficiary/moderate): rely on equal treatment to reach users without negotiating carriage with every access provider.
 *   - end_users (beneficiary/organized): gain unrestricted access to lawful content but face constrained ISP choice and potential subscription cost shifts.
 *   - net_neutrality_regulators (agenda_setter/institutional): enforce non-discrimination rules with jurisdictionally varying intensity.
 *   - open_internet_advocates (observer/organized): monitor compliance gaps and document violations from an analytical seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.62).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.55).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Non-Discrimination (Neutrality Reading)").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology governance / internet policy").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, 'a9bd8774-ee81-4f98-b89b-f50455ba9a20').
narrative_ontology:cs_kernel_codification('a9bd8774-ee81-4f98-b89b-f50455ba9a20', fixed_text).
narrative_ontology:cs_authority_grounding('a9bd8774-ee81-4f98-b89b-f50455ba9a20', lineage).
narrative_ontology:cs_interpretation_layer_present('a9bd8774-ee81-4f98-b89b-f50455ba9a20').
narrative_ontology:cs_reading_relation('a9bd8774-ee81-4f98-b89b-f50455ba9a20', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9bd8774-ee81-4f98-b89b-f50455ba9a20', tcp_ip_interpretation__zero_rating_reading, forecloses).
narrative_ontology:cs_axiom('a9bd8774-ee81-4f98-b89b-f50455ba9a20', foundational, end_to_end_non_discrimination).
narrative_ontology:cs_axiom_status(end_to_end_non_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('a9bd8774-ee81-4f98-b89b-f50455ba9a20', end_to_end_non_discrimination, instrumental).
narrative_ontology:cs_axiom('a9bd8774-ee81-4f98-b89b-f50455ba9a20', foundational, prohibition_of_content_discrimination).
narrative_ontology:cs_axiom_status(prohibition_of_content_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('a9bd8774-ee81-4f98-b89b-f50455ba9a20', prohibition_of_content_discrimination, conventional).
narrative_ontology:cs_reference_frame('a9bd8774-ee81-4f98-b89b-f50455ba9a20', end_to_end_open_internet).
narrative_ontology:cs_drift_state('a9bd8774-ee81-4f98-b89b-f50455ba9a20', contemporary_broadband_markets, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a9bd8774-ee81-4f98-b89b-f50455ba9a20', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_innovators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, end_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, broadband_isps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate last-mile and transit broadband infrastructure. Under the neutrality reading, they are prohibited from blocking, throttling, or granting paid priority to content and applications. They bear the opportunity cost of foregone revenue from content-based service tiers and argue the constraint limits network-management flexibility and infrastructure reinvestment. Their exit is constrained by franchise obligations, sunk physical plant, and regulatory licensing.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, broadband_isps, payer,
    institutional, generational, constrained, national).

% Develop applications, services, and content that reach end users over broadband networks without negotiating carriage or prioritization agreements with individual ISPs. They depend on the non-discrimination constraint to ensure their packets receive treatment equal to incumbents', preserving permissionless innovation.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_innovators, beneficiary,
    moderate, biographical, mobile, global).

% Subscribe to broadband access to reach content and services. They benefit from the ability to access any lawful application without ISP interference, though in many markets they have limited provider choice. They indirectly bear cost shifts if ISPs raise subscription rates to offset lost prioritization revenue.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, end_users, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__neutrality_reading, end_users, payer).

% Set, interpret, and enforce rules that prohibit broadband providers from engaging in content or application discrimination. They justify the constraint as protecting the open architecture of the internet and vary in enforcement intensity across jurisdictions and political cycles.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, net_neutrality_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Civil society and academic organizations that monitor ISP compliance, document throttling or zero-rating violations, and advocate for non-discrimination rules. They sit as analytical observers tracking the gap between the neutrality reading and actual market practice.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, open_internet_advocates, observer,
    organized, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents broadband access markets from fragmenting into balkanized, application-specific toll roads by preserving a general-purpose transport layer where any lawful edge innovation can reach users without prior negotiation with network gatekeepers.
% TRANSFER_FUNCTION: Moves control over traffic treatment and quality-of-service monetization from broadband ISPs to end users and edge innovators; constrains ISPs from extracting rents via content-based discrimination and transfers that value to permissionless edge innovation.
% ABSENT_VOICES: Rural and low-income users with monopoly or duopoly broadband access are structurally excluded from regulatory proceedings; they cannot practically exit to competitive alternatives, yet their dependence is rarely centered in policy design. Non-Western ISPs and users operating under different network-management paradigms are also absent from the IETF-centric framing of the kernel.
% DISAPPEARANCE_RATIONALE: If the non-discrimination constraint vanished overnight, ISPs would rapidly implement paid prioritization, zero-rating bundles, and content-based blocking. The edge innovation ecosystem would shift from permissionless entry to carriage negotiations, reproducing gatekeeper models and fragmenting the global internet into access-fiefdoms.
% FOUNDING_PROBLEM: As the internet commercialized in the 1990s, vertically integrated gatekeepers threatened to replicate the closed-network model inside broadband access, breaking the end-to-end architecture that had enabled rapid, unpermissioned innovation.
% FOUNDING_PROBLEM_CORROBORATION: Edge innovators and open internet advocates attest the problem remains live, citing ongoing zero-rating and throttling. Broadband ISPs and some regulatory economists attest platform competition and wireless alternatives have resolved the founding problem, and that the constraint now functions as regulatory overreach. Independent peer-reviewed economic analyses and multi-jurisdictional regulatory impact assessments are mixed, corroborating neither side exclusively.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is moderately high because the constraint forecloses substantial ISP revenue models (paid prioritization, zero-rating, application-specific tiers) that have proven profitable in non-neutral regimes. Suppression (0.55) reflects the active regulatory enforcement required to prevent discrimination against ISP financial incentives. Theater ratio (0.25) is low: the coordination functionâpreserving a general-purpose innovation platformâis largely genuine, though some enforcement activity has become performative around high-profile disputes. Accessibility collapse (0.40) is moderate: walled gardens, mobile zero-rating plans, and non-neutral jurisdictions demonstrate that alternatives remain accessible. Resistance (0.58) is elevated because ISPs persistently lobby, litigate, and innovate around the constraint. The measurement series run on a single shared grid (T=0 to T=25) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The ISP seat experiences the constraint as regulatory extraction that destroys legitimate network-management and revenue tools; from this seat the arrangement is a snare or tangled rope depending on whether the coordination story is accepted. The edge-innovator and end-user seats experience it as essential protection without which gatekeeper tolls would tax every new service. The engine computes this divergence from the structural data: identical protocol packets are read as either public infrastructure or regulatory takings depending on the seat's position in the value chain.
 *
 * DIRECTIONALITY LOGIC:
 *   Broadband ISPs are declared victims (high d, near full target): they bear the foregone revenue and operational constraints. Edge innovators and end users are declared beneficiaries (low d, near full beneficiary/subidy): the constraint subsidizes their access to the network by preventing ISP extraction. Net neutrality regulators and open internet advocates sit at low d because they do not capture rents; their interest is coordinative or analytical. No directionality overrides are needed because the structural derivation chain produces accurate d values from beneficiary/victim declarations combined with exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure extraction (snare) by requiring a genuine coordination function: the prevention of network balkanization and the preservation of permissionless innovation. It prevents mislabeling as pure coordination (rope) by requiring asymmetric extraction: ISPs demonstrably lose revenue opportunities. If the founding problem were dead and the constraint persisted purely to externalize costs onto infrastructure providers, it would drift toward snare; the temporal measurements show extraction increasing as the digital economy matured, which is consistent with a live coordination function whose distributional consequences have grown, not with mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tcp_ip_kernel_contest,
    'This constraint instantiates the neutrality reading of the tcp_ip_interpretation kernel. What would change structurally if the prioritization reading were adopted instead?',
    'Comparative policy analysis of jurisdictions adopting prioritization versus neutrality frameworks, measuring edge-provider entry rates, ISP revenue diversification, and latency-sensitive application quality outcomes.',
    'Adopting the prioritization reading would invert the beneficiary-victim structure: ISPs would become beneficiaries with new revenue streams, while permissionless edge innovation would face higher barriers to entry. The constraint would likely reclassify from tangled_rope toward rope or scaffold depending on enforcement asymmetries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tcp_ip_kernel_contest, conceptual, 'Structural delta between neutrality and prioritization readings of the TCP/IP kernel').

omega_variable(
    coordination_vs_rent_extraction,
    'Does the non-discrimination constraint solve a live collective-action problem, or has it become a vehicle for large edge incumbents to externalize infrastructure costs onto ISPs while locking out new entrants through regulatory complexity?',
    'Panel analysis of edge-provider entry rates, capital expenditure trends, and traffic-concentration metrics across jurisdictions with varying neutrality enforcement, controlling for market concentration and income levels.',
    'If large incumbents capture the primary benefit while compliance costs deter new ISPs and small edge players, the constraint drifts toward snare. If diffuse innovation benefits persist, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_rent_extraction, empirical, 'Whether the constraint''s coordination function is live or captured by incumbents').

omega_variable(
    zero_rating_foreclosure,
    'Does the neutrality reading logically foreclose the zero-rating reading within a single commitment framework, or can both be sustained through interpretive flexibility around ''reasonable network management'' and ''user consent''?',
    'Logical analysis of the non-discrimination axiom against selective-exemption claims; regulatory case studies of jurisdictions that have attempted to permit zero-rating within nominally neutral frameworks.',
    'If foreclosed, the kernel emits mutually exclusive constraints and regulatory regimes must choose one reading. If flexible, the kernel supports more pluralism than the neutrality reading strictly admits, weakening the foreclosure claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_rating_foreclosure, conceptual, 'Logical relationship between neutrality and zero-rating readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tcp__tr_t5, tcp_ip_interpretation__neutrality_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(tcp__tr_t10, tcp_ip_interpretation__neutrality_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(tcp__tr_t15, tcp_ip_interpretation__neutrality_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__neutrality_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(tcp__tr_t25, tcp_ip_interpretation__neutrality_reading, theater_ratio, 25, 0.25).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(tcp__be_t5, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(tcp__be_t10, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(tcp__be_t25, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(tcp__su_t5, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(tcp__su_t10, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(tcp__su_t15, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(tcp__su_t25, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 25, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the tcp_ip_interpretation constraint family, decomposed per the Îµ-invariance principle. The neutrality reading, prioritization reading, and zero-rating reading share the same RFC kernel but emit structurally distinct constraints with different Îµ values, beneficiary sets, and enforcement requirements. Each reading must be authored as a separate, Îµ-invariant constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
