% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__commons_reading, []).

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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Commons Governance of Software Control
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the commons_reading of the
 *   software_control_legitimacy kernel. It models software control as a
 *   commons governance question — neither absolute freedom
 *   (freedom_imperative_reading) nor absolute property
 *   (property_rights_reading) nor mere methodology choice
 *   (pragmatic_openness_reading) but negotiated collective management of
 *   shared digital infrastructure. The constraint is the governance
 *   arrangement itself: the polycentric rules, norms, and institutions that
 *   allocate authority over software among stakeholder communities. Base
 *   extractiveness (0.32) reflects governance overhead and compliance costs;
 *   suppression (0.42) reflects the structural exclusion of absolutist
 *   positions from governance participation; theater_ratio (0.38) captures
 *   the growing gap between commons ideals and platform-mediated practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.32).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.42).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Commons Governance of Software Control").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, 'df2e2e99-deda-43ac-9f51-bb845b1ac482').
narrative_ontology:cs_kernel_codification('df2e2e99-deda-43ac-9f51-bb845b1ac482', distributed).
narrative_ontology:cs_authority_grounding('df2e2e99-deda-43ac-9f51-bb845b1ac482', practice).
narrative_ontology:cs_interpretation_layer_present('df2e2e99-deda-43ac-9f51-bb845b1ac482').
narrative_ontology:cs_reading_relation('df2e2e99-deda-43ac-9f51-bb845b1ac482', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('df2e2e99-deda-43ac-9f51-bb845b1ac482', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('df2e2e99-deda-43ac-9f51-bb845b1ac482', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('df2e2e99-deda-43ac-9f51-bb845b1ac482', foundational, software_is_shared_infrastructure).
narrative_ontology:cs_axiom_status(software_is_shared_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('df2e2e99-deda-43ac-9f51-bb845b1ac482', software_is_shared_infrastructure, conventional).
narrative_ontology:cs_axiom('df2e2e99-deda-43ac-9f51-bb845b1ac482', foundational, legitimacy_requires_negotiated_governance).
narrative_ontology:cs_axiom_status(legitimacy_requires_negotiated_governance, holdable).
narrative_ontology:cs_axiom_grounding('df2e2e99-deda-43ac-9f51-bb845b1ac482', legitimacy_requires_negotiated_governance, deontological).
narrative_ontology:cs_reference_frame('df2e2e99-deda-43ac-9f51-bb845b1ac482', polycentric_commons_governance).
narrative_ontology:cs_drift_state('df2e2e99-deda-43ac-9f51-bb845b1ac482', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('df2e2e99-deda-43ac-9f51-bb845b1ac482', '2026-06-11T14:30:00Z').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, commons_participants).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, collective_stewards).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, freedom_imperative_adherents).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, property_rights_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, software_users).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, collective_stewards).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, commons_based_peer_production).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, ostrom_design_principles).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, collective_stewardship_legitimacy).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, polycentric_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Multi-stakeholder governance bodies (foundations, working groups, maintainer collectives) that set and administer commons rules for software projects. They derive authority from the practice of commoning and community participation. Their decisions bind participants but they face pressure from platform vendors and commercial forks.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, commons_governance_bodies, agenda_setter,
    organized, generational, constrained, global).

% Communities of developers, users, and organizations that participate in commons-governed software. They gain influence over roadmap, access to shared infrastructure, and protection from unilateral enclosure. They contribute labor and governance effort in exchange for collective stewardship rights.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_communities, beneficiary,
    organized, biographical, mobile, global).

% Long-term maintainers and institutions that invest heavily in commons infrastructure. They benefit from stability and shared maintenance but bear disproportionate governance labor and compliance costs. Exit is costly due to sunk investment in the commons.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, collective_stewards, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, collective_stewards, payer).

% Advocates and projects (e.g., FSF-aligned) that view any compromise on user freedom as ethically illegitimate. They are denied governance participation in commons models that permit proprietary derivatives or commercial licensing. Their identity is fused to the absolutist freedom frame, making exit from the debate nearly impossible.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, freedom_imperative_adherents, payer,
    moderate, generational, identity_locked, global).

% Commercial vendors and IP maximalists who view software control as a property right enabling commercial sustainability. They are excluded from commons governance that subordinates exclusion rights to collective negotiation. They can exit to proprietary models but lose commons network effects and talent pools.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, property_rights_adherents, payer,
    powerful, biographical, constrained, global).

% End users of commons-governed software who gain reliable, transparent, and forkable tools without vendor lock-in. They participate indirectly through community channels but lack direct governance power. Exit is easy (switch software) but costly in switching costs.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, software_users, beneficiary,
    powerless, immediate, mobile, global).

% Companies building proprietary software on or adjacent to commons infrastructure. They are structurally excluded from commons governance but extract value from commons outputs. They would argue for stronger IP protections and against copyleft obligations if admitted.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, proprietary_software_vendors, excluded,
    powerful, biographical, arbitrage, global).

% Cloud and platform providers (AWS, GitHub, app stores) that host and distribute commons software. They shape commons dynamics through infrastructure control but are not accountable to commons governance. They benefit from commons labor while capturing platform rents.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, platform_operators, excluded,
    institutional, generational, arbitrage, global).

% Researchers of commons governance (Ostrom lineage, digital commons studies) who analyze the structural dynamics without direct stake. They provide the empirical and theoretical framework for evaluating whether commons rules meet design principles.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, commons_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages shared digital infrastructure as a commons through negotiated collective governance, solving the problem of how diverse stakeholders can jointly steward interdependent software without relying on absolute property rights or absolute freedom claims.
% TRANSFER_FUNCTION: Moves governance authority from unilateral control (by creators or users) to collective negotiation among stakeholder communities; governance overhead, compliance costs, and maintenance labor are distributed across participants while value (stability, interoperability, talent pool) accrues to the commons.
% ABSENT_VOICES: Absolutist proponents (both freedom-imperative and property-rights) who would reject any negotiated compromise on principle; future generations who inherit the governance structures but lack representation; non-participating users in the Global South who lack access to commons governance channels.
% DISAPPEARANCE_RATIONALE: If commons governance vanished overnight, control would default to either proprietary enclosure (property_rights_reading) or copyleft absolutism (freedom_imperative_reading), restructuring how software projects are governed, how stakeholders participate, and how value flows in the digital ecosystem.
% FOUNDING_PROBLEM: The failure of both proprietary enclosure and copyleft absolutism to provide stable, inclusive governance for increasingly interdependent digital infrastructure — proprietary models create rent-seeking and fragility; absolutist freedom models create fragmentation and exclusion of commercial sustainers.
% FOUNDING_PROBLEM_CORROBORATION: Documented in Ostrom's commons design principles (verified across resource domains), the historical FSF vs. open source split (attested by both factions' own archives), and repeated platform governance failures (e.g., Redis, Terraform, Elastic license changes) analyzed by independent scholars outside the direct beneficiary communities.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).
:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The commons governance model is a genuine coordination mechanism (solving interdependence of digital infrastructure) but carries extraction in the form of governance transaction costs and the structural exclusion of absolutist positions. The claimed_type tangled_rope reflects this hybrid: coordination function (beneficiaries: stakeholder_communities, collective_stewards) plus asymmetric extraction (victims: freedom_imperative_adherents, property_rights_adherents denied governance voice). Active enforcement is required (governance bodies police commons boundaries). The metrics are authored descriptively: extractiveness is moderate because well-designed commons minimize overhead; suppression is moderate because exclusion is structural (governance rules define who participates) not coercive; theater is rising as platform operators capture commons outputs without accountability.
 *
 * PERSPECTIVAL GAP:
 *   From the commons governance seat, the arrangement is a rope — genuine coordination solving a collective action problem. From the freedom-imperative seat, it is a snare — compromising fundamental freedom for pragmatic compromise. From the property-rights seat, it is a snare — undermining legitimate exclusion rights. From the platform operator seat, it is a resource to be harvested. The engine computes these per-seat classifications from the structural data; the authored claim (tangled_rope) represents the authoring seat's structural judgment that the constraint is hybrid at the system level.
 *
 * DIRECTIONALITY LOGIC:
 *   Commons governance bodies (agenda_setter, organized power, constrained exit) sit near symmetric d — they administer the constraint but bear governance labor. Stakeholder communities and collective stewards (beneficiaries) have mobile/constrained exit and organized/moderate power — they gain net benefits but pay participation costs. Freedom-imperative adherents (payer, identity_locked exit) and property-rights adherents (payer, constrained exit) bear the cost of exclusion from governance; their identity_locked or constrained exit amplifies effective extraction. Software users (beneficiary, powerless, mobile) gain diffuse benefits with easy exit. Platform operators and proprietary vendors (excluded, powerful/institutional, arbitrage exit) are structurally outside the commons but shape its environment — their exclusion is not victimhood but structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governing interdependent digital infrastructure) remains live — if anything, interdependence has increased with cloud-native ecosystems. The commons model has not atrophied; it has expanded (CNCF, Linux Foundation, language foundations). However, the rising theater_ratio signals mandatrophy risk: governance rituals increasingly serve platform interests rather than commons participants. The constraint is not a piton because the coordination function is still actively needed and the agenda_setters (commons governance bodies) could reform it — but the cost of reform is high relative to their captured benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the commons_reading a distinct constraint from its sibling readings of the software_control_legitimacy kernel, or a perspective on a single constraint?',
    'Apply ε-invariance test: if measuring extraction under commons governance yields a different ε than measuring under freedom-imperative or property-rights governance, they are distinct constraints. The commons reading''s ε is defined by its own governance overhead and exclusion of absolutists.',
    'If distinct, each reading gets its own constraint story with independent ε, beneficiaries, victims, and classification. If not distinct, the framework would need a measurement-basis parameter (which it rejects). This omega documents the ε-invariance commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to ε-invariance across kernel readings: each reading instantiates a separate constraint.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the exclusion of freedom-imperative and property-rights adherents from commons governance structural (rules define participation criteria) or internalized (absolutists self-exclude by refusing compromise)?',
    'Post-exclusion trajectory analysis: if excluded parties form parallel governance (e.g., FSF maintaining separate copyleft infrastructure) and maintain distinct identity, suppression is partially internalized. If they seek entry but are barred by rules, suppression is structural.',
    'If internalized, effective suppression is higher than structural measure suggests — the target carries the exclusion with them. If structural, commons rule reform could reduce suppression without changing the constraint''s nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for absolutist position exclusion.').

omega_variable(
    commons_rule_variability,
    'How much does base extractiveness (ε) vary across different commons governance rule sets (copyleft vs. permissive vs. hybrid foundations)?',
    'Comparative analysis of governance overhead, compliance costs, and exclusion intensity across major commons institutions (Linux Foundation, Apache, FSF, CNCF, language-specific foundations).',
    'High variability would mean ''commons governance'' is not a single constraint but a family. Low variability would support treating it as one constraint with a stable ε. This affects whether we need decomposed stories per governance model.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_rule_variability, empirical, 'Variance of ε across commons governance implementations.').

omega_variable(
    beneficiary_boundary,
    'Who exactly constitutes ''stakeholder communities'' and ''collective stewards'' — where is the boundary between participant and non-participant in commons governance?',
    'Governance charter analysis: formal membership criteria, contribution thresholds, and voting rights across commons institutions. Ethnographic study of de facto participation vs. formal rules.',
    'Boundary ambiguity inflates both beneficiary and victim counts. If boundaries are porous, extraction may be lower (easy entry) but suppression may be higher (soft exclusion). Precise boundaries affect directionality derivation for marginal actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_boundary, conceptual, 'Definitional boundary of commons governance participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sclcr_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sclcr_tr_t5, software_control_legitimacy__commons_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(sclcr_tr_t10, software_control_legitimacy__commons_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(sclcr_tr_t15, software_control_legitimacy__commons_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(sclcr_tr_t20, software_control_legitimacy__commons_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(sclcr_tr_t25, software_control_legitimacy__commons_reading, theater_ratio, 25, 0.37).
narrative_ontology:measurement(sclcr_tr_t30, software_control_legitimacy__commons_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(sclcr_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(sclcr_be_t5, software_control_legitimacy__commons_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(sclcr_be_t10, software_control_legitimacy__commons_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(sclcr_be_t15, software_control_legitimacy__commons_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement(sclcr_be_t20, software_control_legitimacy__commons_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(sclcr_be_t25, software_control_legitimacy__commons_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(sclcr_be_t30, software_control_legitimacy__commons_reading, base_extractiveness, 30, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(sclcr_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sclcr_su_t5, software_control_legitimacy__commons_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(sclcr_su_t10, software_control_legitimacy__commons_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(sclcr_su_t15, software_control_legitimacy__commons_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(sclcr_su_t20, software_control_legitimacy__commons_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(sclcr_su_t25, software_control_legitimacy__commons_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(sclcr_su_t30, software_control_legitimacy__commons_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__commons_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, open_source_sustainability).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, platform_governance).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_supply_chain_security).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, digital_public_goods_governance).

% DUAL FORMULATION NOTE:
% This constraint is one member of the software_control_legitimacy constraint family (kernel). It decomposes the colloquial 'software freedom vs. IP' debate into structurally distinct claims: commons_reading (this story), freedom_imperative_reading, pragmatic_openness_reading, property_rights_reading. The commons reading's ε (0.32) differs substantially from the freedom-imperative reading's ε (near 0 for copyleft, high for proprietary) and property-rights reading's ε (high for proprietary control). They are linked via cs_structure.reading_relations and share the kernel_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
