% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems — Balanced Coexistence Reading (Negotiated Dual-Legitimacy Compact)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   one_country_two_systems_framework: the balanced_coexistence_reading,
 *   under which the Joint Declaration/Basic Law arrangement is a negotiated
 *   dual-legitimacy compact — neither sovereignty nor autonomy absolute,
 *   contested boundaries resolved through political accommodation rather than
 *   legal supremacy. Per the epsilon-invariance principle, the sibling
 *   readings (autonomy_primacy_reading, sovereignty_primacy_reading) are
 *   separate constraint files, not views folded into this one. All three
 *   readings share the same epsilon referent — the standing central-SAR
 *   governing arrangement — but epsilon is reading-indexed: this reading
 *   assesses that arrangement as a functioning-but-ratcheting compact with
 *   extraction concentrated at crisis points; the autonomy-primacy sibling
 *   assesses the same arrangement as breached treaty obligation; the
 *   sovereignty-primacy sibling assesses it as faithful implementation. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   (a real coordination function joined to asymmetric extraction held in
 *   place by active enforcement) while the metrics independently describe the
 *   arrangement's actual operation, including drift well past the
 *   medium-epsilon regime this reading's own framework anticipated. KEY
 *   AGENTS (by structural relationship): - prc_central_authorities: Primary
 *   agenda-setter (institutional/arbitrage) — administers the compact,
 *   collects sovereignty assurance and gateway rents, can rewrite terms
 *   directly - hongkong_business_establishment: Principal beneficiary with
 *   payer costs (powerful/mobile) — collects stability and market access,
 *   pays political-conformity costs - multinational_interface_firms:
 *   Secondary beneficiary (powerful/arbitrage) — business model is the
 *   two-systems interface itself - hongkong_pan_democratic_movement: Primary
 *   target (organized/constrained) — bears disqualification, prosecution,
 *   organizational dissolution - hongkong_independent_press_sector: Target
 *   (moderate/trapped) — bears closure, arrest, and self-censorship pressure
 *   - hongkong_judiciary: Dual-positioned target-beneficiary
 *   (institutional/identity_locked) — stewards the common-law system while
 *   absorbing interpretive subordination - hongkong_voting_public: Diffuse
 *   target (powerless/constrained) — bears representation loss and emigration
 *   pressure - uk_joint_declaration_cosignatory: Excluded co-signatory
 *   (institutional/constrained) — attests breach, holds no seat -
 *   comparative_constitutional_scholars: Analytical observer
 *   (analytical/analytical)
 *
 * KEY AGENTS:
 *   - prc_central_authorities: Primary agenda-setter (institutional/arbitrage) — administers the compact through Basic Law machinery, NPCSC interpretations, and appointment powers; collects sovereignty assurance, gateway economics, and precedent control; demonstrated capacity to impose terms directly (2020 security legislation without local passage)
 *   - hongkong_business_establishment: Principal beneficiary with payer costs (powerful/mobile) — collects political stability, common-law commercial courts, and mainland market access; pays patriotic-conformity expectations and security-law compliance costs; capital partially mobile but anchored by property and licenses
 *   - multinational_interface_firms: Secondary beneficiary (powerful/arbitrage) — regional business models depend on one jurisdiction combining common-law enforcement, free capital flow, and mainland proximity; hedge by diversifying hubs
 *   - hongkong_pan_democratic_movement: Primary target (organized/constrained) — bears candidate disqualification, security-law prosecution, imprisoned leadership, dissolved allied organizations; individual emigration available but collective exit dissolves the movement
 *   - hongkong_independent_press_sector: Target (moderate/trapped) — flagship closures, arrested editors, prosecuted journalists; surviving outlets self-censor; exit means abandoning the Cantonese-language audience
 *   - hongkong_judiciary: Dual-positioned target-beneficiary (institutional/identity_locked) — stewards a common-law system of international standing while absorbing binding NPCSC interpretations, designated-judge arrangements, and oath requirements; senior foreign judges have resigned rather than continue
 *   - hongkong_voting_public: Diffuse target (powerless/constrained) — lost proportional representation in the 2021 electoral restructuring; bears family-splitting emigration waves and policy made without consent
 *   - uk_joint_declaration_cosignatory: Excluded co-signatory (institutional/constrained) — registered the 1984 treaty at the UN and formally attests breach; no enforcement mechanism attaches; remedies reduce to statements, visa schemes, and coordinated sanctions
 *   - comparative_constitutional_scholars: Analytical observer (analytical/analytical) — supply the outside-the-parties record of how boundary disputes are actually resolved
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.72).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.74).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems — Balanced Coexistence Reading (Negotiated Dual-Legitimacy Compact)").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).
narrative_ontology:has_sunset_clause(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, 'cc40a1bc-9143-4c71-a677-0e61afa2b54d').
narrative_ontology:cs_kernel_codification('cc40a1bc-9143-4c71-a677-0e61afa2b54d', fixed_text).
narrative_ontology:cs_authority_grounding('cc40a1bc-9143-4c71-a677-0e61afa2b54d', lineage).
narrative_ontology:cs_interpretation_layer_present('cc40a1bc-9143-4c71-a677-0e61afa2b54d').
narrative_ontology:cs_reading_relation('cc40a1bc-9143-4c71-a677-0e61afa2b54d', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('cc40a1bc-9143-4c71-a677-0e61afa2b54d', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('cc40a1bc-9143-4c71-a677-0e61afa2b54d', foundational, mutual_limitation_constitutes_the_compact).
narrative_ontology:cs_axiom_status(mutual_limitation_constitutes_the_compact, holdable).
narrative_ontology:cs_axiom_grounding('cc40a1bc-9143-4c71-a677-0e61afa2b54d', mutual_limitation_constitutes_the_compact, conventional).
narrative_ontology:cs_axiom('cc40a1bc-9143-4c71-a677-0e61afa2b54d', foundational, accommodation_not_supremacy_resolves_boundaries).
narrative_ontology:cs_axiom_status(accommodation_not_supremacy_resolves_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('cc40a1bc-9143-4c71-a677-0e61afa2b54d', accommodation_not_supremacy_resolves_boundaries, instrumental).
narrative_ontology:cs_axiom('cc40a1bc-9143-4c71-a677-0e61afa2b54d', secondary, fifty_year_guarantee_period_binding).
narrative_ontology:cs_axiom_status(fifty_year_guarantee_period_binding, holdable).
narrative_ontology:cs_axiom_grounding('cc40a1bc-9143-4c71-a677-0e61afa2b54d', fifty_year_guarantee_period_binding, conventional).
narrative_ontology:cs_reference_frame('cc40a1bc-9143-4c71-a677-0e61afa2b54d', joint_declaration_negotiated_equilibrium).
narrative_ontology:cs_drift_state('cc40a1bc-9143-4c71-a677-0e61afa2b54d', post_national_security_law_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cc40a1bc-9143-4c71-a677-0e61afa2b54d', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_authorities).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_business_establishment).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, multinational_interface_firms).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_pan_democratic_movement).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_independent_press_sector).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_voting_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_business_establishment).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__balanced_coexistence_reading, functional_division_of_powers_doctrine).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__balanced_coexistence_reading, incremental_accommodation_feasibility).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__balanced_coexistence_reading, dual_system_interoperability_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds sovereign authority over Hong Kong and administers the compact through the Basic Law, NPCSC interpretations, and appointment powers. Collects assured sovereignty, a controlled gateway between its economy and global finance, and a reunification showcase. Has demonstrated the capacity to amend operating terms directly when dissatisfied, as with the 2020 security legislation imposed without local passage. Its alternatives include deeper direct administration, which it exercises selectively.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_authorities, agenda_setter,
    institutional, generational, arbitrage, continental).

% Collects political stability, common-law commercial courts, and preferential access to mainland markets under the two-systems interface. Pays in political-conformity expectations: patriotic-education participation, security-law compliance costs, and quiet withdrawal from civic advocacy. Capital is partially mobile through dual listings and regional offices, but rooted property portfolios and license holdings anchor it.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_business_establishment, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_business_establishment, payer).

% Regional business models depend on the interface itself: one jurisdiction offering common-law contract enforcement, free capital flow, and mainland market proximity. They lobby quietly for continuity and hedge by diversifying hubs. Relocation is feasible over a few years, making their commitment conditional rather than captive.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, multinational_interface_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Organized parties, unions, and civil groups that contested elections and mass-mobilized for universal suffrage and autonomy guarantees. Bears disqualification from office, prosecution under security legislation, imprisonment of leadership, and forced dissolution of allied organizations. Emigration is available to individuals but dissolves the movement itself, so collective exit is effectively unavailable.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_pan_democratic_movement, payer,
    organized, biographical, constrained, local).

% Operates newsrooms under licensing dependence, advertising-market pressure, and security-legislation exposure. Flagship outlets have been closed, editors arrested, and journalists prosecuted; remaining outlets self-censor to survive. Exit means abandoning the Cantonese-language audience that constitutes the sector's reason to exist.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_independent_press_sector, payer,
    moderate, biographical, trapped, local).

% Stewards a common-law system of international standing, which depends on the compact for its continued existence and prestige. Absorbs binding NPCSC interpretations that override local adjudication, security-law designated-judge arrangements, and oath requirements for judges. Judges cannot exit without abandoning the bench and the legal tradition their professional identity is built on; several senior foreign judges have resigned rather than continue.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_judiciary, payer,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_judiciary, beneficiary).

% Lost proportionally elected representation through the 2021 electoral restructuring, which screens candidates for patriotism. Bears the diffuse costs of political closure: emigration waves splitting families, eroded civic space, and policy made without their consent. Individual exit exists through foreign visa schemes; collective voice does not.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_voting_public, payer,
    powerless, biographical, constrained, local).

% Co-signed the 1984 Joint Declaration, registered it at the United Nations, and formally attests its breach. Holds no seat in the current arrangement: no enforcement mechanism attaches to its attestation, and its remedies reduce to statements, visa schemes, and coordinated sanctions with partners.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, uk_joint_declaration_cosignatory, excluded,
    institutional, generational, constrained, global).

% Analyze the compact's operation against comparative federal and devolution arrangements, publish assessments of interpretive practice, and provide the outside-the-parties record of how boundary disputes are actually resolved.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_authorities).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__balanced_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains interoperability between two legal, economic, and administrative systems inside one sovereign state: border and customs regimes, currency arrangements, contract enforcement, and market access are standardized once through the compact instead of renegotiated transaction-by-transaction, preserving Hong Kong's distinct system as working infrastructure for both sides.
% TRANSFER_FUNCTION: Moves discretionary authority over Hong Kong affairs from local institutions to central organs at each crisis point — security legislation, electoral design, judicial interpretation — and moves stability assurances, market access, and sovereignty recognition from the center to Hong Kong's business establishment and international capital.
% ABSENT_VOICES: The UK co-signatory holds formal breach attestations with no seat in the arrangement; Hong Kong voters screened out by the 2021 electoral restructuring have no proportional voice; Taiwan, for whom this framework is the standing offer, observes without standing; disqualified candidates and dissolved civil organizations are absent from every consultative body that remains.
% DISAPPEARANCE_RATIONALE: Overnight removal would force immediate reorganization of the border, currency, customs, and legal-certainty arrangements that make Hong Kong function as an interface: capital would reprice jurisdiction risk within days, the common-law courts' mandate would lapse into direct administration, and both the mainland's gateway economics and the territory's financial-center status would restructure around whatever replaced the compact.
% FOUNDING_PROBLEM: Reintegrate a capitalist, common-law colonial enclave into a socialist one-party state without destroying the enclave's economic value or triggering capital and population flight — recovering sovereignty while preserving the system that made the territory valuable.
% FOUNDING_PROBLEM_CORROBORATION: British negotiating archives, UN treaty-registration records, and contemporaneous diplomatic correspondence corroborate the founding problem itself from outside the benefiting parties. On current status, attestation splits along the kernel's fault line: the administering party declares the problem solved and integration maturing; the former co-signatory and much international legal scholarship attest the management problem as live and worsening. No attestation of current status exists that is both outside all benefiting parties and favorable to the arrangement's present operation — that absence is itself signal.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.72 at interval end) reflects this reading's own assessment of the standing arrangement: the compact delivers real interface coordination, but each crisis cycle since 2003 has moved discretionary authority from local to central organs, and the 2020 security legislation bypassed the local legislature entirely — well past the medium-epsilon regime this reading's framework anticipated, which is itself the finding. Suppression (0.74) tracks the enforcement machinery accumulated at each crisis point — interpretation practice, dedicated prosecutorial units, candidate screening — and is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream by directionality and scope. Theater (0.42) captures the widening gap between 'high degree of autonomy' pronouncements and administrative practice. Accessibility collapse sits mid-range (0.45): full-democracy, independence, and full-integration alternatives remain conceivable but politically blocked, while individual exit through emigration stays partially open. Resistance (0.60) records repeated mass contestation (2003, 2014, 2019) continuing post-2020 through exile media, international advocacy, and departure. The dominant coordination function is resource allocation — the compact allocates jurisdictional authority and market access between two systems once, centrally, rather than transaction-by-transaction — hence coordination_type resource_allocation. The measurement series run on one shared nine-point grid (1984-2024) so every tracked metric is authored at every examined time point. Trajectories show punctuated equilibrium rather than smooth drift: plateau, crisis-step, partial relief (2010), larger steps (2014, 2020). The step pattern is part of the mechanism, not noise — concessions are extracted during calm phases and locked in at crisis resolution, an intermittent-reinforcement dynamic.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the center's position the arrangement is a successfully administered compact honored in letter while reserved powers are exercised sparingly and legally; from the pan-democratic, press, and voting-public seats the same structure operates as a ratchet converting each crisis into permanent central discretion. The business and interface-firm seats sit between: they experience the arrangement as stability worth a conformity price, and their partial mobility damps their experienced extraction. The judiciary's identity lock makes its seat distinctive — it cannot evaluate the arrangement from outside because exiting would dissolve the institution doing the evaluating. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The center is the structural beneficiary: it collects sovereignty assurance, gateway rents, and precedent control, and its demonstrated ability to rewrite terms places it near the beneficiary pole despite formal submission to the compact. Business establishment and interface firms receive stability and access — low-to-moderate directionality, damped further by arbitrage-grade exit. The pan-democratic movement, independent press, and voting public bear the transfers under constrained or trapped exit — near the target pole. The judiciary is dual-positioned: it receives the continued existence of its court system (benefit) while absorbing interpretive subordination (cost), and its identity lock holds it nearer the target end than its formal privilege would suggest. The UK co-signatory is excluded rather than positioned: it attests breach but collects nothing and bears nothing under the current operation, so it feeds no directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reintegration of the enclave without destroying its economic value — was substantively solved at the 1997 handover, but the ongoing management problem the compact addresses (operating two systems inside one sovereignty) is contested rather than dead, so no mandatrophy declaration is authored. The tangled_rope claim prevents two symmetrical misreadings: a pure-coordination reading that would ignore the documented ratchet of central discretion, and a pure-extraction reading that would erase the real interface function — border, currency, contract enforcement, market access — that both sides still rely on daily. The receipt surface sharpens the picture: gains accrue to a named seat (the center), keeping the arrangement capture-flavored, while the persistence of genuine coordination function keeps it from collapsing into pure extraction. If the accommodation channel closes permanently (see omega accommodation_channel_survival), the coordination half atrophies and the structure migrates toward the sovereignty-primacy sibling's profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the balanced_coexistence_reading of kernel one_country_two_systems_framework; what would change structurally if the same standing arrangement were instantiated under the sibling readings?',
    'Compile the sibling files (autonomy_primacy_reading, sovereignty_primacy_reading) and compare victim sets, enforcement requirements, and epsilon over the identical referent.',
    'Under sovereignty_primacy the arrangement''s centralization reads as faithful implementation of a revocable delegation; under autonomy_primacy the same facts read as breached treaty guarantee. This file''s medium-high epsilon is reading-indexed, not topic-indexed — the disagreement is located in the boundary-resolution mechanism (political accommodation vs enforceable guarantee vs sovereign revocation), not in the facts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one kernel, three readings, three structurally distinct constraints.').

omega_variable(
    accommodation_channel_survival,
    'Does a functioning political-accommodation channel survive the post-2020 enforcement architecture, or has the cumulative ratchet closed renegotiation as a live mechanism?',
    'Track whether post-2020 boundary disputes (electoral design, press licensing, professional-body autonomy) produce any negotiated adjustment versus unilateral central determination.',
    'If the channel is closed, this reading describes a lapsed regime and the standing arrangement''s dynamics converge on the sovereignty-primacy sibling''s profile; if open, the periodic-renegotiation dynamic this reading anticipates persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_channel_survival, empirical, 'Whether the balanced reading''s own resolution mechanism still operates.').

omega_variable(
    civil_society_leverage_depletion,
    'Is civil society''s economic and international bargaining leverage durable, or is it depleting under emigration and capital-relocation pressure?',
    'Longitudinal data on professional emigration rates, corporate hub relocation, and outcomes of international engagement campaigns.',
    'Depleting leverage converts the anticipated periodic-renegotiation cycle into a one-way ratchet, raising effective extraction for residents who remain and cannot exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_leverage_depletion, empirical, 'Durability of the bargaining-power channel this reading''s regime assumes.').

omega_variable(
    sunset_clause_function_reading,
    'Does the Basic Law Article 5 fifty-year provision operate as a sunset (transitional support toward eventual integration) or as a guarantee period within a steady-state compact?',
    'Drafting-history analysis and observation of whether renewal-negotiation infrastructure emerges before 2047.',
    'A sunset reading pressures classification toward transitional-support categories; the guarantee reading supports the steady-state compact classification asserted here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_function_reading, conceptual, 'Functional interpretation of the fifty-year provision.').

omega_variable(
    joint_declaration_bindingness,
    'Does the Sino-British Joint Declaration retain binding force as the compact''s foundation, given the co-signatory''s formal breach attestations and the administering party''s characterization of it as a historical document?',
    'Authoritative international-law settlement, or observable practice: whether either party continues to invoke the treaty in future boundary disputes.',
    'If the foundation instrument is treated as void, the balanced reading loses its conventional grounding and the arrangement rests on unilateral promulgation alone, shifting the authority structure toward extraction-grounded legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(joint_declaration_bindingness, conceptual, 'Bindingness of the compact''s foundational instrument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 1984, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1984, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 1984, 0.15).
narrative_ontology:measurement(one__tr_t1990, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 1997, 0.18).
narrative_ontology:measurement(one__tr_t2003, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2003, 0.22).
narrative_ontology:measurement(one__tr_t2010, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(one__tr_t2014, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2014, 0.3).
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2019, 0.33).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(one__tr_t2024, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(one__be_t1984, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 1984, 0.3).
narrative_ontology:measurement(one__be_t1990, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 1997, 0.35).
narrative_ontology:measurement(one__be_t2003, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2003, 0.42).
narrative_ontology:measurement(one__be_t2010, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(one__be_t2014, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2019, 0.6).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(one__be_t2024, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1984, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 1984, 0.25).
narrative_ontology:measurement(one__su_t1990, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 1997, 0.28).
narrative_ontology:measurement(one__su_t2003, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2003, 0.38).
narrative_ontology:measurement(one__su_t2010, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement(one__su_t2014, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2014, 0.48).
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(one__su_t2024, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2024, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, resource_allocation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'One Country, Two Systems' decomposes into three structurally distinct constitutional commitments sharing one kernel and one epsilon referent (the standing central-SAR arrangement) but differing in reading-indexed epsilon, victim sets, and enforcement profiles. This file is the balanced_coexistence_reading member. Coupling: the sovereignty-primacy member cites the compact's formal persistence as evidence for its reading; the autonomy-primacy member cites the same facts as breach; this member cites the same facts as a ratcheting compact. Family edges enable contamination propagation — if this member's coordination function is judged collapsed, both siblings' evidentiary bases shift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
