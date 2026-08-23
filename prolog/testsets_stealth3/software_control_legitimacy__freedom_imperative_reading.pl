% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__freedom_imperative_reading, []).

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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Software Control Legitimacy — Freedom Imperative Reading (Proprietary Software as Denial of User Control)
 *   domain: technological/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the software_control_legitimacy
 *   kernel: the freedom-imperative reading, which holds that control over
 *   one's computing is a fundamental user right and that proprietary software
 *   is ethically illegitimate because it denies users the ability to run,
 *   study, modify, and share their software. Per the epsilon-referent rule,
 *   epsilon's referent is the STANDING ARRANGEMENT UNDER CONTEST — the
 *   proprietary-software regime of restrictive licenses, EULAs, DRM, and
 *   enforced exclusivity — assessed by this reading's own lights. It is NOT
 *   the free-software order this reading endorses (that would drive epsilon
 *   toward zero for every advocacy reading and destroy the measurement). The
 *   reading therefore authors high epsilon: every proprietary interaction is,
 *   in its frame, a rights denial. The claim/metric independence rule
 *   applies: claimed_type states my structural belief (tangled_rope — the
 *   arrangement has a genuine funding-and-support coordination face AND
 *   asymmetric freedom-denial extraction, held together by active
 *   enforcement), while the metrics describe the arrangement's operation as
 *   this reading descriptively assesses it. Sibling readings are OTHER
 *   constraint stories, not part of this one; their structural deltas are
 *   recorded in the kernel_reading_contestation omega. KEY AGENTS (by
 *   structural relationship): - proprietary_software_vendors: Agenda-setter
 *   (institutional/arbitrage) — sets license terms, operates enforcement
 *   machinery, collects license and subscription revenue -
 *   platform_ecosystem_operators: Beneficiary (institutional/arbitrage) —
 *   collects distribution commissions atop the closed model -
 *   cloud_service_providers: Beneficiary (institutional/arbitrage) — collects
 *   subscription revenue while placing executed code beyond user reach -
 *   end_users: Primary target (powerless/constrained) — bears the freedom
 *   denial at every interaction - independent_developers: Target with
 *   secondary benefit (moderate/constrained) — pays fees and forgoes source
 *   access inside an ecosystem that pays them - enterprise_licensees:
 *   Institutional target with secondary benefit (powerful/constrained) — pays
 *   heavily, receives accountability - repair_and_modification_community:
 *   Target nearest full capture (organized/identity_locked) — craft and
 *   identity barred by technical-legal machinery - free_software_movement:
 *   Analytical-adversarial observer (organized/analytical) — publishes the
 *   doctrine, enforces copyleft, campaigns; holds no seat in license drafting
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: Agenda-setter (institutional/arbitrage) — sets and enforces the licensing terms; primary recipient of the arrangement's revenue
 *   - end_users: Primary target (powerless/constrained) — bears the four-freedoms denial; individual leverage near zero
 *   - enterprise_licensees: Institutional target with secondary beneficiary position (powerful/constrained) — absorbs lock-in costs in exchange for support and accountability
 *   - independent_developers: Dual-positioned target (moderate/constrained) — pays platform fees and accepts review rules while earning inside the ecosystem
 *   - repair_and_modification_community: Target with identity-locked exit (organized/identity_locked) — craft barred by signed bootloaders, parts pairing, and anti-circumvention law
 *   - cloud_service_providers: Beneficiary (institutional/arbitrage) — subscription revenue with code never distributed
 *   - platform_ecosystem_operators: Beneficiary (institutional/arbitrage) — commissions on closed distribution channels
 *   - free_software_movement: Adversarial analytical observer (organized/analytical) — doctrinal source, copyleft enforcer, legislative campaigner; outside the arrangement's own decision structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.78).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.62).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Software Control Legitimacy — Freedom Imperative Reading (Proprietary Software as Denial of User Control)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "technological/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, 'ba3365fa-84c0-4ea2-a161-44e88eae23d5').
narrative_ontology:cs_kernel_codification('ba3365fa-84c0-4ea2-a161-44e88eae23d5', fixed_text).
narrative_ontology:cs_authority_grounding('ba3365fa-84c0-4ea2-a161-44e88eae23d5', lineage).
narrative_ontology:cs_interpretation_layer_present('ba3365fa-84c0-4ea2-a161-44e88eae23d5').
narrative_ontology:cs_reading_relation('ba3365fa-84c0-4ea2-a161-44e88eae23d5', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('ba3365fa-84c0-4ea2-a161-44e88eae23d5', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('ba3365fa-84c0-4ea2-a161-44e88eae23d5', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('ba3365fa-84c0-4ea2-a161-44e88eae23d5', foundational, user_computing_control_is_fundamental_right).
narrative_ontology:cs_axiom_status(user_computing_control_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('ba3365fa-84c0-4ea2-a161-44e88eae23d5', user_computing_control_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('ba3365fa-84c0-4ea2-a161-44e88eae23d5', secondary, freedom_precedes_development_efficiency).
narrative_ontology:cs_axiom_status(freedom_precedes_development_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('ba3365fa-84c0-4ea2-a161-44e88eae23d5', freedom_precedes_development_efficiency, deontological).
narrative_ontology:cs_reference_frame('ba3365fa-84c0-4ea2-a161-44e88eae23d5', universal_four_freedoms_baseline).
narrative_ontology:cs_drift_state('ba3365fa-84c0-4ea2-a161-44e88eae23d5', contemporary_computing_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ba3365fa-84c0-4ea2-a161-44e88eae23d5', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, platform_ecosystem_operators).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, cloud_service_providers).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, end_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, independent_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, enterprise_licensees).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, repair_and_modification_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, independent_developers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, enterprise_licensees).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, exclusive_licensing_funds_development_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish software under restrictive licenses, draft the EULA terms users must accept, operate activation and DRM systems, and enforce compliance through license audits and anti-circumvention law. License and subscription revenue funds their development pipelines; their source code and roadmaps stay private. Exit looks like pivoting business models — several vendors have open-sourced selected components while keeping flagship products closed, demonstrating the pivot is available to them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Run app stores, console platforms, and walled-garden distribution channels that require signed binaries and approved payment rails. They take a percentage of third-party sales and decide what software devices are permitted to execute. Their revenue depends on the closed-distribution model; opening their channels to unsigned code would dismantle the commission stream.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, platform_ecosystem_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Deliver software as network services running on their own servers, so customers interact only through clients and interfaces they do not control. Subscription revenue replaces license sales, and the executed code is never distributed, placing it beyond customer inspection regardless of what any license would permit. Releasing service internals would hand competitors the ability to host equivalent services.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, cloud_service_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Use proprietary operating systems, applications, and devices daily. They receive compiled binaries without source, accept agreements granting revocable usage permissions, and have no lawful path to study, modify, or redistribute what runs on their machines. Switching to free alternatives is feasible in some categories but blocked in others by file formats, professional tooling requirements, peripheral support, and accumulated skills.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, end_users, payer,
    powerless, biographical, constrained, global).

% Build products on top of proprietary platforms and toolchains: they pay SDK and license fees, comply with platform review rules, and cannot incorporate or study the underlying platform code. At the same time they earn their living inside the ecosystem those platforms create, selling to its user base — their income and their constraints come from the same closed structure.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, independent_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, independent_developers, beneficiary).

% Deploy proprietary software at organizational scale under volume agreements carrying audit clauses, scheduled upgrade cycles, and support contracts. They receive vendor accountability, certified integrations, and a contractual counterparty to hold liable when systems fail; they also absorb lock-in costs, forced migration timelines, and pricing they cannot meaningfully negotiate individually.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, enterprise_licensees, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, enterprise_licensees, beneficiary).

% Repair devices, replace components, install alternative firmware, and modify hardware-software behavior. Signed bootloaders, serialized parts pairing, DRM, and anti-circumvention law progressively bar these activities, with takedown notices and warranty-voiding warnings accompanying the technical barriers. The activity is their craft and, for many, their livelihood and identity; abandoning modification is not a realistic option for them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, repair_and_modification_community, payer,
    organized, biographical, identity_locked, global).

% Publish and maintain the four-freedoms doctrine, steward free software licenses, audit license compliance, and campaign against DRM and for right-to-repair legislation. They hold no seat in proprietary license drafting; their influence operates through working counter-examples (complete free software stacks), copyleft enforcement, and public argument. Their institutional identity is constituted by the freedom frame itself.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_movement, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the funding-and-trust problem of commercial software: exclusive licenses make copies excludable so development can be financed by sales and subscriptions; vendor maintenance, support contracts, and certified integrations give deploying organizations a single accountable counterparty; standardized vendor-managed products reduce integration risk across enterprises.
% TRANSFER_FUNCTION: Moves license fees, subscription payments, and distribution commissions from end users, deploying organizations, and third-party developers to vendors and platform operators; additionally moves control rights — the ability to study, modify, and share — from everyone who touches the software to the vendor alone.
% ABSENT_VOICES: End users never negotiated the terms they operate under: EULAs are presented as consent, but no user sat across the table when they were drafted. The free software movement speaks publicly yet holds no seat in license drafting or standards bodies dominated by vendors. Future users are absent entirely — today's DRM and bootloader-locking decisions bind people who had no opportunity to object.
% DISAPPEARANCE_RATIONALE: If restrictive software licensing and its enforcement machinery vanished overnight, the software economy would reorganize: forks of every major proprietary product would appear within weeks, funding would migrate to support contracts, dual licensing, and sponsored development, platform operators would lose their commission gates, and the repair and modification communities' barred activities would resume immediately. Arrangements across the economy visibly depend on the constraint continuing to hold.
% FOUNDING_PROBLEM: Early software circulated freely alongside hardware; as software became a product in its own right, vendors faced unauthorized copying that undermined revenue. Licensing restrictions were built to make software excludable so that development could be funded by selling copies and, later, subscriptions.
% FOUNDING_PROBLEM_CORROBORATION: No fully neutral arbiter exists; both sides' evidence is on the record. Vendor trade associations and business economists attest the funding problem is live, citing development cost studies and the fragility of volunteer-supported critical infrastructure. Corroboration from OUTSIDE the benefiting parties: the free software movement attests the problem is solvable without freedom-stripping terms, pointing to Debian, Apache, and Linux Foundation-funded development; academic software-economics research (Lerner and Tirole and successors) documents both the reality of the funding problem and the viability of non-proprietary funding mechanisms. The dispute over status is therefore corroborated from both directions rather than asserted by beneficiaries alone.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 at interval end) because the reading's frame counts every closed-source interaction as denial of a fundamental right, and the standing arrangement has expanded across the interval from desktop binaries to phones, consoles, and server-side services. Suppression (0.62) is authored as a RAW STRUCTURAL property — the legal and technical machinery (DMCA anti-circumvention, license audits, activation, signed bootloaders) that bars study, modification, and sharing — and is deliberately NOT scaled here; the engine owns any directionality and scope scaling of extractiveness only. Theater is low (0.18): the arrangement delivers working, supported software, and its performative share ('user agreement' framed as negotiated consent, DRM framed as 'protection') is real but minor. Accessibility_collapse is moderate (0.48): free alternatives persist in general-purpose computing but collapse in specific domains (professional creative tooling, kernel-level anticheat, certain firmware and peripheral support), which is exactly the partial-collapse profile of a construct rather than a natural law. Resistance (0.55) reflects forty years of organized movement, copyleft enforcement, and right-to-repair legislative campaigns.
 *   
 *   The temporal series run on ONE SHARED GRID (1984, 1991, 1999, 2007, 2015, 2024) with every tracked metric authored at every point. The suppression_requirement series is authored because enforcement-capacity change IS this story's traced dynamic: shrink-wrap EULAs with weak technical teeth (1984-1991), the DMCA ratchet and product activation (late 1990s), walled-garden distribution and always-on verification (2007-2015), then a partial plateau with slight easing by 2024 (right-to-repair wins, some genuine open-sourcing) — hence the small decline from the 2015 peak rather than monotone rise. Theater rises through the 'trusted computing'/'protection' rhetorical era and dips after 2015 as subscription vendors increasingly drop the pretense that purchasers own anything, marketing rental openly instead. Fixing_cost is authored 'prohibitive': dismantling the proprietary funding model wholesale would strand development financing across every industry that depends on commercially supported software, a cost far exceeding what any single fixer captures from fixing it.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the vendor seat the arrangement is commerce it built and maintains: it funds development, prices risk, and delivers supported products — a coordination structure it administers with arbitrage-grade exit (it can pivot models, as several have). From the end-user seat the same structure is experienced as a rights denial at every interaction: binaries without source, revocable permissions, no lawful path to study or modify. The enterprise seat sits between — it pays the heaviest sums but receives vendor accountability and a contractual counterparty, so its computed extraction is damped by its secondary-beneficiary position. Identity-lock dynamics bind two seats: the free software movement's ideological identity is constituted through the freedom frame (exit would mean the seat dissolving, not defecting), and the repair community's craft identity makes abandonment of modification unthinkable — which is why the repair seat is authored identity_locked rather than merely constrained. If the freedom frame broke institutionally, the movement seat would not convert to the property-rights position; it would lose its organizing identity entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (proprietary_software_vendors, platform_ecosystem_operators, cloud_service_providers) drive derived directionality toward the beneficiary pole, amplified by their arbitrage-grade exit — the arrangement subsidizes them. Victim declarations (end_users, independent_developers, enterprise_licensees, repair_and_modification_community) drive directionality toward the target pole; constrained exit keeps the first three short of full-target, while the repair community's identity_locked exit places it nearest the full-target end. The dual-positioned seats (independent_developers, enterprise_licensees, each payer with secondary_role beneficiary) derive intermediate directionality, which is structurally honest: they simultaneously fund and profit from the ecosystem. No directionality_overrides were needed: the derivation chain produces the correct d for every seat from the beneficiary/victim declarations plus power and exit atoms, and adding overrides would duplicate data the structural derivation already captures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making software excludable so its development could be funded — is CONTESTED, not dead: vendors and business economists attest it live (development costs money; support and liability need funding), while the movement and the software-economics literature attest that non-proprietary funding (support contracts, dual licensing, crowdfunding, foundation sponsorship) solves it for large classes of software. Because status is contested and the disappearance verdict is world_rearranges, no zombie/mandatrophy flag fires. The tangled_rope classification prevents two symmetric mislabels: reading the arrangement as pure snare erases the genuine coordination face that even this reading's own tradition concedes (the FSF itself sells support contracts for free software — funding without freedom-stripping is possible, which proves the funding problem is real while refuting the claim that it requires proprietary terms); reading it as pure rope erases the categorical freedom denial this reading places at the center of the analysis. Mandatrophy is NOT resolved: the arrangement's mandate is disputed between its beneficiaries and its targets, and the corpus keeps the sibling readings as separate stories precisely so that dispute is carried structurally rather than averaged away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the freedom_imperative_reading of the software_control_legitimacy kernel; would the sibling readings (pragmatic_openness_reading, property_rights_reading, commons_reading) produce structurally different constraints from the same subject matter?',
    'Generate each sibling as its own constraint story and compare victim sets, epsilon, and computed type across the family.',
    'property_rights_reading relocates vendors from beneficiaries to legitimate rights-holders and shrinks the victim set to license violators; commons_reading replaces the categorical victim set with governance-failure cases; pragmatic_openness_reading lowers epsilon substantially and may compute rope. The classification of THIS story is valid only for THIS reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one kernel, four readings, each a separate epsilon-invariant constraint.').

omega_variable(
    categorical_frame_epsilon,
    'Is the authored epsilon of 0.78 an artifact of the categorical deontological frame — counting every closed binary as full denial of the four freedoms — rather than a measure of incremental harm?',
    'Re-measure the same standing arrangement under the pragmatic_openness lens, where closed source is one legitimate methodology among several; compare the two readings'' epsilon over the identical referent.',
    'Under the pragmatic lens epsilon drops sharply and the arrangement may compute as rope; the corpus deliberately keeps both readings as separate stories so this divergence is measurable rather than averaged away.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_frame_epsilon, conceptual, 'Reading-indexed epsilon: the categorical freedom frame inflates measured extraction relative to outcome-based frames.').

omega_variable(
    saas_boundary,
    'Does the freedom imperative''s victim set include users of software delivered as a network service, where the executed code is never distributed and the four freedoms are structurally unreachable regardless of license?',
    'Track the reading''s own tradition: AGPL-style advocacy extends the freedoms to server-side execution, while other strands treat services as outside copyright''s reach entirely; observe which boundary the movement institutionalizes.',
    'Inclusion expands the victim set massively (most new software is becoming service-delivered) and pushes epsilon higher; exclusion leaves the current values standing and caps the reading''s reach at distributed binaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_boundary, conceptual, 'Boundary dispute inside the reading over whether SaaS falls within the freedom imperative''s jurisdiction.').

omega_variable(
    openness_sincerity,
    'Is the post-2015 wave of vendor open-sourcing genuine decay of the proprietary arrangement, or strategic repositioning that preserves control under open branding (open-core, controlled foundations, CLA-gated contributions)?',
    'Track whether freed components acquire independent community governance or remain vendor-controlled: trademark custody, contribution license agreements, and roadmap control are the observable markers.',
    'Genuine decay implies scaffold-like drift toward an eventual sunset of the arrangement; repositioning implies extraction continues under open branding and the suppression_requirement series resumes rising after its 2015-2024 plateau.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(openness_sincerity, empirical, 'Whether the recent softening of the arrangement is attrition or adaptation.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression structural (DRM, anti-circumvention law, signed bootloaders, license audits) or internalized (users'' learned acceptance that software is an opaque appliance they are not meant to inspect)?',
    'Post-barrier trajectory: cohorts and jurisdictions with strong free-software education reveal whether control-seeking behavior returns when legal barriers lift, or whether the black-box expectation persists independently.',
    'If a substantial share is internalized, effective suppression exceeds the structural measure — removing the legal machinery alone would not restore user control-seeking, and the constraint''s persistence would outlive its enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized components of the suppression holding the arrangement in place.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 1984, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1984, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1984, 0.1).
narrative_ontology:measurement_basis(soft_tr_t1984, observed).
narrative_ontology:measurement(soft_tr_t1991, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1991, 0.12).
narrative_ontology:measurement_basis(soft_tr_t1991, observed).
narrative_ontology:measurement(soft_tr_t1999, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1999, 0.15).
narrative_ontology:measurement_basis(soft_tr_t1999, observed).
narrative_ontology:measurement(soft_tr_t2007, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2007, 0.2).
narrative_ontology:measurement_basis(soft_tr_t2007, observed).
narrative_ontology:measurement(soft_tr_t2015, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement_basis(soft_tr_t2015, observed).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2024, 0.18).
narrative_ontology:measurement_basis(soft_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t1984, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1984, 0.6).
narrative_ontology:measurement_basis(soft_be_t1984, observed).
narrative_ontology:measurement(soft_be_t1991, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1991, 0.63).
narrative_ontology:measurement_basis(soft_be_t1991, observed).
narrative_ontology:measurement(soft_be_t1999, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1999, 0.68).
narrative_ontology:measurement_basis(soft_be_t1999, observed).
narrative_ontology:measurement(soft_be_t2007, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2007, 0.72).
narrative_ontology:measurement_basis(soft_be_t2007, observed).
narrative_ontology:measurement(soft_be_t2015, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement_basis(soft_be_t2015, observed).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2024, 0.78).
narrative_ontology:measurement_basis(soft_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1984, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1984, 0.3).
narrative_ontology:measurement_basis(soft_su_t1984, observed).
narrative_ontology:measurement(soft_su_t1991, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1991, 0.34).
narrative_ontology:measurement_basis(soft_su_t1991, observed).
narrative_ontology:measurement(soft_su_t1999, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1999, 0.45).
narrative_ontology:measurement_basis(soft_su_t1999, observed).
narrative_ontology:measurement(soft_su_t2007, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2007, 0.58).
narrative_ontology:measurement_basis(soft_su_t2007, observed).
narrative_ontology:measurement(soft_su_t2015, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement_basis(soft_su_t2015, observed).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(soft_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'software openness/control legitimacy' decomposes into four readings of one kernel, per the epsilon-invariance principle. This story is the freedom_imperative_reading; its epsilon (0.78) is reading-indexed over the FIXED referent of the standing proprietary arrangement — the pragmatic reading would author markedly lower epsilon over the identical referent, and the property-rights reading would relocate the beneficiary/victim structure entirely. The freedom reading is upstream of the pragmatic reading historically (the pragmatic framing emerged from and borrows moral energy from the freedom movement), which is modeled as an influences edge in cs_structure.reading_relations; family members are linked via network.affects_constraints in both directions of documentation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
