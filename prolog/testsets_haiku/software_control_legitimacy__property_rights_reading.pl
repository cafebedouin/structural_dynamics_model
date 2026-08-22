% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right (Intellectual Property Reading)
 *   domain: political_economy/intellectual_property/software_engineering
 *
 * SUMMARY:
 *   The property-rights reading of software control holds that creators have
 *   legitimate authority to restrict use, modification, and distribution to
 *   protect their investment and enable commercial sustainability. This
 *   reading generates extractiveness because it creates monopoly positions
 *   and locks users and developers into vendor-controlled ecosystems. The
 *   constraint exhibits both genuine coordination (investment incentives,
 *   support infrastructure, licensing simplification for enterprise users)
 *   and substantial asymmetric extraction (licensing fees, modification
 *   restrictions, lock-in, and denial of labor value to FOSS contributors).
 *   The reading is one of four contested readings of the broader kernel
 *   'software control legitimacy,' competing with freedom-imperative,
 *   pragmatic-openness, and commons-based readings. This story instantiates
 *   ONLY the property-rights reading as a clean, ε-invariant constraint; the
 *   sibling readings are separate constraint stories.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: institutional agenda-setters collecting licensing revenue and controlling enforcement infrastructure (high power, arbitrage exit)
 *   - institutional_investors_in_software: institutional beneficiaries capturing IP value appreciation and monopoly rents (high power, arbitrage exit)
 *   - FOSS advocates and contributors: organized victims denied modification and monetization rights (moderate power, constrained exit — trapped within legal IP regime, identity-locked to open-source mission)
 *   - users denied modification rights: powerless victims locked into vendor-provided functionality (powerless, constrained exit)
 *   - commercial users bound to single vendor: moderate-power victims trapped by switching costs and sunk integration investment (moderate power, identity-locked)
 *   - developing-world software users: powerless victims priced out or legally restricted from access (powerless, trapped)
 *   - open-source alternative developers: excluded, organized competition structurally subordinated by IP law (organized power, constrained exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.62).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.58).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right (Intellectual Property Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "political_economy/intellectual_property/software_engineering").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, 'f101e60d-6cad-4699-93a0-6878c0ebb027').
narrative_ontology:cs_kernel_codification('f101e60d-6cad-4699-93a0-6878c0ebb027', fixed_text).
narrative_ontology:cs_authority_grounding('f101e60d-6cad-4699-93a0-6878c0ebb027', extraction).
narrative_ontology:cs_interpretation_layer_present('f101e60d-6cad-4699-93a0-6878c0ebb027').
narrative_ontology:cs_reading_relation('f101e60d-6cad-4699-93a0-6878c0ebb027', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('f101e60d-6cad-4699-93a0-6878c0ebb027', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('f101e60d-6cad-4699-93a0-6878c0ebb027', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('f101e60d-6cad-4699-93a0-6878c0ebb027', foundational, software_as_property_right).
narrative_ontology:cs_axiom_status(software_as_property_right, holdable).
narrative_ontology:cs_axiom_grounding('f101e60d-6cad-4699-93a0-6878c0ebb027', software_as_property_right, conventional).
narrative_ontology:cs_axiom('f101e60d-6cad-4699-93a0-6878c0ebb027', foundational, creator_authority_legitimate_to_restrict_modification_and_distribution).
narrative_ontology:cs_axiom_status(creator_authority_legitimate_to_restrict_modification_and_distribution, holdable).
narrative_ontology:cs_axiom_grounding('f101e60d-6cad-4699-93a0-6878c0ebb027', creator_authority_legitimate_to_restrict_modification_and_distribution, deontological).
narrative_ontology:cs_reference_frame('f101e60d-6cad-4699-93a0-6878c0ebb027', creator_intellectual_property_authority).
narrative_ontology:cs_drift_state('f101e60d-6cad-4699-93a0-6878c0ebb027', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f101e60d-6cad-4699-93a0-6878c0ebb027', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, institutional_investors_in_software).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates_and_contributors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, users_denied_modification_rights).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, commercial_users_bound_to_single_vendor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, academic_researchers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, developing_world_software_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce licensing terms that restrict use, modification, and redistribution. Collect revenue through licensing fees, support contracts, and vendor lock-in mechanisms. Justify restrictions as necessary to protect R&D investment and enable sustained commercial operations. Control the technical and legal infrastructure that enforces the property-right claim.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive returns on capital invested in software development through licensing revenue and IP value appreciation. The property-right framing secures their investment thesis: restricted software generates monopoly rents and defensible market positions. Benefit from strong intellectual property law and enforcement.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, institutional_investors_in_software, beneficiary,
    institutional, generational, arbitrage, global).

% Denied the ability to modify, study, or redistribute proprietary software they use or contribute to. Their labor in competing open-source ecosystems is devalued by the property-right reading, which treats their work as non-commercial and subordinate. Face legal liability if they reverse-engineer or circumvent license restrictions. Operate within a regulatory and licensing regime they do not control.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates_and_contributors, payer,
    organized, generational, constrained, global).

% Cannot modify software to suit their needs, study how it works, or verify security and privacy properties. Locked into whatever the vendor provides or charges. Dependent on the vendor for security patches and feature updates. Cannot fork or adapt software to different hardware or use cases.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, users_denied_modification_rights, payer,
    powerless, biographical, constrained, global).

% Pay licensing fees, support costs, and upgrade obligations. Build their own systems and workflows on the vendor's controlled platform, creating switching costs and vendor dependence. Cannot negotiate license terms or redirect development roadmap without leaving their existing investment. Trapped by the sunk cost of integration and employee training.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, commercial_users_bound_to_single_vendor, payer,
    moderate, biographical, identity_locked, global).

% Often need to use proprietary software for research but face licensing costs and modification restrictions that impede replication and methodological transparency. Cannot legally modify algorithms or verify implementations. Excluded from developing derivative works. Some have access through academic site licenses, but these are restricted and temporary.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, academic_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, academic_researchers, excluded).

% Cannot afford proprietary licensing costs and are legally restricted from using open alternatives that violate copyright in their jurisdiction. Lack technical capacity to audit or modify software for local conditions. Dependent on vendors willing to serve their market at affordable prices; often left behind.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, developing_world_software_users, payer,
    powerless, biographical, trapped, global).

% Develop and maintain free and open-source alternatives but operate in a framing that treats their work as lower-status and non-commercial. Compete with vendors who have legal monopolies on their code and can bundle or acquire competing projects. Structurally excluded from the licensing revenue and IP value appreciation that flow to proprietary vendors.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, open_source_alternative_developers, excluded,
    organized, generational, constrained, global).

% The apparatus of copyright law, patent law, trade secret protection, and DMCA-style anti-circumvention provisions that enforces software as property. Not an agent itself, but the enabling structure the property-right reading depends on. Governed by legislatures and courts.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, intellectual_property_legal_system, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(software_control_legitimacy__property_rights_reading, intellectual_property_legal_system).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__property_rights_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Software property rights coordinate investment incentives with market access: vendors invest in development with assurance they can capture returns through licensing; users and enterprises gain access to maintained, supported software; IP law provides the enforcement structure that makes vendor investment signals credible.
% TRANSFER_FUNCTION: Moves value from software users and derivative developers to proprietary vendors and institutional investors: licensing fees, support contracts, switching costs, and monopoly rents flow upward. Restricts labor value creation for FOSS contributors and academic researchers who cannot monetize modifications or derivative work.
% ABSENT_VOICES: FOSS developers, users who would prefer to modify and audit software, and developing-world users priced out by licensing costs are structurally excluded from the licensing negotiation. They can voice dissent through alternative development (open source) but cannot renegotiate the terms under which proprietary software operates.
% DISAPPEARANCE_RATIONALE: If software property rights enforcement vanished overnight, proprietary vendors would lose licensing revenue and market-dominant positions; users would fork and modify existing code; FOSS ecosystems would expand into currently proprietary markets; software development would shift toward collective models and community contribution. The commercial software industry as currently structured would reorganize entirely.
% FOUNDING_PROBLEM: Early software development was vulnerable to free-riding on expensive R&D investment: a vendor could invest heavily in development, and competitors could copy the work without bearing the cost. Without property-right protection, market incentives for continued investment were weak; venture capital and institutional funding for software development were unreliable.
% FOUNDING_PROBLEM_CORROBORATION: Software vendors and venture capitalists attest the founding problem remains live and that property protection is necessary for commercial incentives. Open-source historians (Stallman, Raymond, Perens) and FOSS advocates attest the problem was solved by peer review and community reputation long before it required property protection; they cite successful large-scale FOSS projects (Linux, Apache, MySQL, PostgreSQL) funded through alternative mechanisms. Empirical evidence from the literature is mixed: high-growth FOSS ecosystems contradict the necessity claim; however, some major proprietary vendors credit property protection with enabling their scale. No consensus outside the benefiting parties.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because the constraint transfers substantial value from users and derivative developers to vendors and investors through licensing fees, modification restrictions, and monopoly positioning. It is not maximal because genuine coordination value exists: property protection does fund software development, and some users benefit from vendor support and bundled services. Suppression (0.58) is moderately high because the constraint requires active legal and technical enforcement (DMCA anti-circumvention, copyright prosecution, DRM) to prevent users from modifying or reverse-engineering software. Accessibility collapse (0.71) is high because alternatives to proprietary software are legally foreclosed in many jurisdictions and technically foreclosed through IP protection and DRM. Resistance (0.64) is high because FOSS communities mount sustained organizational opposition. Theater ratio (0.28) is low because the security and quality maintenance justifications, while real, do not account for the majority of enforcement activity — most enforcement defends modification restrictions and pricing power, not user safety. The temporal series show extractiveness rising from 1980 to ~2010 (when the property-right regime matured through DMCA, software patent expansion, and cloud/SaaS lock-in) and then plateauing despite FOSS expansion, indicating a stabilized but contested equilibrium. Suppression requirement rose sharply with legal infrastructure development (copyright takedowns, DMCA enforcement, patent litigation) and has remained stable. Theater ratio has risen gradually as vendors invest in open-source-adjacent projects (cloud platforms shipping with open components) while maintaining property-right boundaries — a marker of defensive reframing.
 *
 * PERSPECTIVAL GAP:
 *   The property-rights reading's beneficiaries (vendors, investors) experience this constraint as coordination (investment protection enabling innovation and market stability). The payer seats (users, FOSS developers) experience it as extraction with legitimacy cover: modification restrictions are justified as security, but they function to maintain market power; pricing is justified as reflecting development cost, but it functions to extract surplus from inelastic demand. The FOSS advocates experience identity-locking: many are professionally committed to open-source models and face career/mission costs from the property-right framing even when they could individually exit by taking proprietary jobs. The engine should compute high divergence between the beneficiary seat (d near 0.0, low effective extraction) and the payer seats (d near 1.0, high effective extraction). Commercial users sit in an intermediate position: they benefit from vendor support and ecosystem stability (d moderate), but face vendor lock-in and rising licensing costs (pushing d higher).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: proprietary software vendors and institutional investors operate with high power and arbitrage exit — they can shift between markets, technologies, and business models. The constraint directly subsidizes them through licensing revenue and IP monopoly rents. They structure and enforce the constraint, so d approaches 0.0 (full beneficiary). Victim directionality: users denied modification rights and FOSS advocates bear costs (modification restrictions, lock-in, legal liability for reverse-engineering) with limited exit. FOSS advocates carry identity-locking (professional commitment to open-source; exiting would require abandoning their mission or retraining into proprietary sectors). Commercial users carry identity-locking (sunk integration costs; switching requires operational disruption). Users generally are powerless; d approaches 1.0 (full target). The constraint's distributive effect: beneficiaries are institutional, high-power, and concentrated; victims are diffuse and include powerless individuals, organized advocates, and moderate-power enterprises. No override needed because the derivation chain (beneficiary/victim + exit + power) naturally produces the correct directionality profile.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate was to secure investment incentives for software development. The founding problem (high R&D costs, free-riding risk) was real in 1980–1990. The problem status is NOW contested: FOSS ecosystems demonstrably fund large-scale software development (Linux, Apache, Kubernetes, PyTorch) with alternative incentive structures (peer review, reputation, corporate subsidies, government funding). The constraint persists despite the founding problem being substantially addressed by non-property mechanisms. The ticket that keeps property rights in place is: (1) institutional investors benefit from monopoly rents and will not release them, (2) the legal/technical regime (DMCA, patent law, copyright) has become self-reinforcing, (3) path-dependent switching costs lock users and enterprises into proprietary ecosystems. The theater_ratio rise (0.08 to 0.28) reflects growing performative investment: vendors adopt open-source aesthetics (releasing 'open-source components'), establish bug-bounty programs, and claim transparency while maintaining core property restrictions. This is classic mandatrophy: the original function (funding development) is alive but partially obsolete; the extraction function persists through institutional inertia and deliberate maintenance by beneficiaries. A constraint that was Rope (genuine coordination, low extraction) is drifting toward Snare territory (extraction, legitimacy cover) but sits currently in Tangled Rope (mixed coordination + active extraction requiring enforcement). The rising suppression_requirement and steady extractiveness indicate hardening enforcement infrastructure and stabilized redistribution, not a solution seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_of_property_protection_for_investment,
    'Is strong property protection (copyright, patents, DMCA) actually necessary to fund software development, or would venture capital and institutional funding flow to software development under alternative incentive structures (reputation, peer review, government funding, corporate subsidy)?',
    'Comparative analysis of FOSS vs. proprietary development funding pathways; historical analysis of software funding pre- and post-DMCA; natural experiments from jurisdictions with weaker IP enforcement (e.g., China''s software development funded via state coordination + vendor lock-in rather than IP rents).',
    'If property protection is not necessary, ε should be reclassified upward (from 0.62 toward 0.80+) as pure extraction rather than coordination. The founding problem would be overdetermined — solved by non-property mechanisms — and the constraint would drift from Tangled Rope toward Snare. Conversely, if property protection IS necessary, current ε is correctly calibrated as mixed coordination + extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_property_protection_for_investment, empirical, 'Whether intellectual property protection is structurally required for software investment incentives or whether alternative mechanisms suffice.').

omega_variable(
    separability_of_property_and_support,
    'Are property rights over software code structurally inseparable from vendor support, security updates, and professional services, or could users obtain bundled support for open-source software through alternative service models?',
    'Empirical analysis of support-model diversity in FOSS ecosystems (Red Hat, Canonical, SUSE, etc.); market testing of open-source-software-as-a-service models; survey of enterprise adoption drivers for proprietary vs. open-source with comparable support.',
    'If property rights are separable from support, the coordination function ε represents is smaller than authored (vendorized support is coordination, but it does not require property rights), and the pure extraction component is larger. This would shift ε upward (toward 0.70+) and the classification toward Snare. If inseparable, current calibration stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(separability_of_property_and_support, empirical, 'Whether the coordination value of vendor support could be achieved under open-source property models.').

omega_variable(
    property_right_vs_reading_legitimacy,
    'Is the property-rights reading a descriptively accurate account of how software control actually functions, or does it serve a committer-frame legitimation function that obscures the constraint''s extractive character?',
    'Discourse analysis of vendor framing and institutional IP law; cross-reading comparison with FOSS and freedom readings; stakeholder interview on perceived legitimacy and actual constraints (do users believe property restrictions serve them, or do they experience restrictions as limiting without corresponding benefit).',
    'If the reading serves primarily legitimation, the constraint is closer to Snare than Tangled Rope, ε should rise, and theater_ratio should rise further as defensive reframing continues. The committer-frame signal (held by vendors and investors) differs materially from the empirical-reality signal (held by users and FOSS advocates). This is an omegas-level uncertainty because it resolves at the observer/interpretive level, not purely through empirical data.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(property_right_vs_reading_legitimacy, conceptual, 'Whether the property-rights framing captures real coordination value or primarily serves extractive interests.').

omega_variable(
    identity_locking_depth_in_foss_advocates,
    'For FOSS advocates classified as identity_locked, how deep is the identity fusion with open-source principles? If the legal or market regime shifted, how many would remain committed to FOSS vs. exit into proprietary development?',
    'Career-transition analysis of FOSS developers who shift into proprietary roles; survey of identity-commitment depth among FOSS contributors; economic analysis of wage premium for proprietary vs. FOSS development roles.',
    'If identity-locking is deep (exit is psychologically costly despite wage opportunity), suppression is effectively higher than measured (internalized suppression persists after institutional barriers are removed). If shallow (identity is contingent on market opportunity), exit_options should reclassify from identity_locked toward constrained, and effective extraction is lower. This affects how the engine models the FOSS seat''s experience of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locking_depth_in_foss_advocates, empirical, 'Whether FOSS advocates'' exit constraint is structural (legal/economic) or internalized (identity/mission commitment).').

omega_variable(
    kernel_contention_location,
    'The core disagreement among the four readings of the software_control_legitimacy kernel is whether software code is properly classified as property (with corresponding creator authority) or as a commons, user freedom, or development methodology choice. This is a classification dispute about the fundamental nature of software, not a disagreement about consequences. If one reading''s core premise is adopted (e.g., ''software IS property''), does it logically foreclose the others, or can multiple readings coexist as live positions in different institutional contexts?',
    'Formal logical analysis of each reading''s foundational axioms and their contradictions; institutional analysis of whether readings coexist in practice (do they? courts, legislatures, and corporations do operate under different readings simultaneously) or whether one has achieved hegemonic dominance.',
    'If readings logically foreclose each other, the reading_relations should use ''forecloses'' rather than ''coexists_with''. If they coexist (which appears empirically true — courts issue conflicting rulings, legislatures hedge, corporations use both proprietary and open models), the ''coexists_with'' relation stands. This affects how the engine models the kernel''s structural stability: a kernel with mutual foreclosures is fragile and subject to sudden flips; a kernel with coexistence is stable but contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contention_location, conceptual, 'Whether the four readings of software control are logically incompatible or structurally coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1980, software_control_legitimacy__property_rights_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(soft_tr_t1990, software_control_legitimacy__property_rights_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(soft_tr_t2000, software_control_legitimacy__property_rights_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(soft_tr_t2010, software_control_legitimacy__property_rights_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement(soft_tr_t2018, software_control_legitimacy__property_rights_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(soft_tr_t2026, software_control_legitimacy__property_rights_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t1980, software_control_legitimacy__property_rights_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(soft_be_t1990, software_control_legitimacy__property_rights_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(soft_be_t2000, software_control_legitimacy__property_rights_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(soft_be_t2010, software_control_legitimacy__property_rights_reading, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement(soft_be_t2018, software_control_legitimacy__property_rights_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(soft_be_t2026, software_control_legitimacy__property_rights_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1980, software_control_legitimacy__property_rights_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(soft_su_t1990, software_control_legitimacy__property_rights_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(soft_su_t2000, software_control_legitimacy__property_rights_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(soft_su_t2010, software_control_legitimacy__property_rights_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(soft_su_t2018, software_control_legitimacy__property_rights_reading, suppression_requirement, 2018, 0.58).
narrative_ontology:measurement(soft_su_t2026, software_control_legitimacy__property_rights_reading, suppression_requirement, 2026, 0.58).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1980, tn=2026
narrative_ontology:measurement(soft_grid_01, software_control_legitimacy__property_rights_reading, accessibility_collapse(class), 1980, 0.42).
narrative_ontology:measurement(soft_grid_02, software_control_legitimacy__property_rights_reading, accessibility_collapse(class), 2026, 0.75).
narrative_ontology:measurement(soft_grid_03, software_control_legitimacy__property_rights_reading, accessibility_collapse(individual), 1980, 0.45).
narrative_ontology:measurement(soft_grid_04, software_control_legitimacy__property_rights_reading, accessibility_collapse(individual), 2026, 0.72).
narrative_ontology:measurement(soft_grid_05, software_control_legitimacy__property_rights_reading, accessibility_collapse(organizational), 1980, 0.38).
narrative_ontology:measurement(soft_grid_06, software_control_legitimacy__property_rights_reading, accessibility_collapse(organizational), 2026, 0.68).
narrative_ontology:measurement(soft_grid_07, software_control_legitimacy__property_rights_reading, accessibility_collapse(structural), 1980, 0.35).
narrative_ontology:measurement(soft_grid_08, software_control_legitimacy__property_rights_reading, accessibility_collapse(structural), 2026, 0.77).
narrative_ontology:measurement(soft_grid_09, software_control_legitimacy__property_rights_reading, resistance(class), 1980, 0.48).
narrative_ontology:measurement(soft_grid_10, software_control_legitimacy__property_rights_reading, resistance(class), 2026, 0.72).
narrative_ontology:measurement(soft_grid_11, software_control_legitimacy__property_rights_reading, resistance(individual), 1980, 0.42).
narrative_ontology:measurement(soft_grid_12, software_control_legitimacy__property_rights_reading, resistance(individual), 2026, 0.58).
narrative_ontology:measurement(soft_grid_13, software_control_legitimacy__property_rights_reading, resistance(organizational), 1980, 0.52).
narrative_ontology:measurement(soft_grid_14, software_control_legitimacy__property_rights_reading, resistance(organizational), 2026, 0.68).
narrative_ontology:measurement(soft_grid_15, software_control_legitimacy__property_rights_reading, resistance(structural), 1980, 0.35).
narrative_ontology:measurement(soft_grid_16, software_control_legitimacy__property_rights_reading, resistance(structural), 2026, 0.64).
narrative_ontology:measurement(soft_grid_17, software_control_legitimacy__property_rights_reading, stakes_inflation(class), 1980, 0.25).
narrative_ontology:measurement(soft_grid_18, software_control_legitimacy__property_rights_reading, stakes_inflation(class), 2026, 0.65).
narrative_ontology:measurement(soft_grid_19, software_control_legitimacy__property_rights_reading, stakes_inflation(individual), 1980, 0.28).
narrative_ontology:measurement(soft_grid_20, software_control_legitimacy__property_rights_reading, stakes_inflation(individual), 2026, 0.68).
narrative_ontology:measurement(soft_grid_21, software_control_legitimacy__property_rights_reading, stakes_inflation(organizational), 1980, 0.32).
narrative_ontology:measurement(soft_grid_22, software_control_legitimacy__property_rights_reading, stakes_inflation(organizational), 2026, 0.72).
narrative_ontology:measurement(soft_grid_23, software_control_legitimacy__property_rights_reading, stakes_inflation(structural), 1980, 0.22).
narrative_ontology:measurement(soft_grid_24, software_control_legitimacy__property_rights_reading, stakes_inflation(structural), 2026, 0.62).
narrative_ontology:measurement(soft_grid_25, software_control_legitimacy__property_rights_reading, suppression(class), 1980, 0.28).
narrative_ontology:measurement(soft_grid_26, software_control_legitimacy__property_rights_reading, suppression(class), 2026, 0.61).
narrative_ontology:measurement(soft_grid_27, software_control_legitimacy__property_rights_reading, suppression(individual), 1980, 0.18).
narrative_ontology:measurement(soft_grid_28, software_control_legitimacy__property_rights_reading, suppression(individual), 2026, 0.52).
narrative_ontology:measurement(soft_grid_29, software_control_legitimacy__property_rights_reading, suppression(organizational), 1980, 0.22).
narrative_ontology:measurement(soft_grid_30, software_control_legitimacy__property_rights_reading, suppression(organizational), 2026, 0.58).
narrative_ontology:measurement(soft_grid_31, software_control_legitimacy__property_rights_reading, suppression(structural), 1980, 0.25).
narrative_ontology:measurement(soft_grid_32, software_control_legitimacy__property_rights_reading, suppression(structural), 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__property_rights_reading, 0.18).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% The software_control_legitimacy kernel decomposes into four structurally distinct constraints, one per reading. The property_rights_reading (this story) treats software control as a property right held by vendors and investors, generating extraction through modification restrictions and licensing monopolies. The freedom_imperative_reading (separate story) treats software control as fundamental user freedom, generating extraction from the denial of control. The pragmatic_openness_reading (separate story) treats software as a development methodology choice, generating only modest extraction. The commons_reading (separate story) treats software as collective resource governance, generating extraction from exclusion of voices in governance. Each reading has its own ε (measured from the standing arrangement under that reading's assessment), beneficiary/victim set, and classification. All four are linked by network.affects_constraints; upstream readings (property_rights and freedom_imperative) establish the terms within which the pragmatic and commons readings operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
