% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Open Source as Superior Development Methodology (Pragmatic Reading)
 *   domain: software_engineering/political_economy_technology
 *
 * SUMMARY:
 *   The pragmatic-development reading of the software-source-status kernel
 *   claims that open source is a superior development methodology because
 *   transparency, distributed peer review, and permissive licensing
 *   accelerate innovation and improve code quality. The reading does NOT
 *   claim that software freedom is an inherent ethical right (that is the
 *   freedom_imperative_reading); it is INSTRUMENTAL—freedom is valued because
 *   it produces better software outcomes. Proprietary software is not
 *   portrayed as inherently illegitimate, only as inferior for quality and
 *   innovation velocity in most contexts. This reading is held by large
 *   portions of technology organizations, standards bodies (IETF, W3C), and
 *   academic computer science communities. The constraint describes the
 *   legitimacy framework these communities enforce through hiring practices,
 *   technical standards adoption, and resource allocation to open projects.
 *
 * KEY AGENTS:
 *   - Collaborative development communities: benefit from peer review coordination and reputation mechanisms
 *   - Downstream users and integrators: benefit from source access, customization ability, and transparency-based quality assurance
 *   - Quality-focused organizations (tech companies, standards bodies): set the agenda by adopting open development as methodology and demonstrating superior outcomes
 *   - Proprietary software vendors: bear extraction cost through competitive pressure and margin compression; constrained exit
 *   - Enterprise IT procurement: benefit from reduced vendor lock-in and lower TCO
 *   - Security researchers: benefit from source transparency for independent auditing
 *   - Developing-economy technologists: benefit from zero-cost access and permissive licensing
 *   - Legacy closed-source ecosystems: structurally excluded because the constraint treats their model as inferior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.31).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.18).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Superior Development Methodology (Pragmatic Reading)").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy_technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, '3d781a68-efef-4e42-92e0-bcea0fc0b745').
narrative_ontology:cs_kernel_codification('3d781a68-efef-4e42-92e0-bcea0fc0b745', distributed).
narrative_ontology:cs_authority_grounding('3d781a68-efef-4e42-92e0-bcea0fc0b745', expertise).
narrative_ontology:cs_interpretation_layer_present('3d781a68-efef-4e42-92e0-bcea0fc0b745').
narrative_ontology:cs_reading_relation('3d781a68-efef-4e42-92e0-bcea0fc0b745', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d781a68-efef-4e42-92e0-bcea0fc0b745', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d781a68-efef-4e42-92e0-bcea0fc0b745', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('3d781a68-efef-4e42-92e0-bcea0fc0b745', foundational, transparency_enables_superior_quality_outcomes).
narrative_ontology:cs_axiom_status(transparency_enables_superior_quality_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('3d781a68-efef-4e42-92e0-bcea0fc0b745', transparency_enables_superior_quality_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('3d781a68-efef-4e42-92e0-bcea0fc0b745', foundational, distributed_peer_review_accelerates_innovation).
narrative_ontology:cs_axiom_status(distributed_peer_review_accelerates_innovation, holdable).
narrative_ontology:cs_axiom_grounding('3d781a68-efef-4e42-92e0-bcea0fc0b745', distributed_peer_review_accelerates_innovation, empirically_contingent).
narrative_ontology:cs_reference_frame('3d781a68-efef-4e42-92e0-bcea0fc0b745', peer_review_quality_superiority).
narrative_ontology:cs_drift_state('3d781a68-efef-4e42-92e0-bcea0fc0b745', contemporary_proprietary_qa_maturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d781a68-efef-4e42-92e0-bcea0fc0b745', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, collaborative_development_communities).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, downstream_users_and_integrators).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, quality_focused_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, enterprise_it_procurement).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, security_researchers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, developing_economy_technologists).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, proprietary_software_vendors).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, peer_review_improves_code_quality).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, transparency_accelerates_innovation).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, distributed_development_scales_complexity_handling).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintainers and contributors to open source projects benefit from the coordination mechanism: distributed peer review accelerates bug detection, feature iteration is shaped by community input, and maintainers build reputation and career capital through visible contribution history. They can move between projects; the constraint does not trap them.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, collaborative_development_communities, beneficiary,
    organized, generational, mobile, global).

% Organizations integrating open source into production systems benefit from access to source code for debugging, customization, and security auditing without vendor lockdown. They can fork, patch, or switch projects; exit is costly but feasible. They gain quality assurance through transparency and the ability to shape the tool to their needs.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, downstream_users_and_integrators, beneficiary,
    organized, biographical, mobile, global).

% Technology organizations and standards bodies that adopt open development practices as their methodology gain faster innovation cycles, better bug discovery, and lower maintenance burden relative to closed-source alternatives at equivalent scale. They set the agenda within their domain by choosing open development as the standard and demonstrating its outcomes. Exit is frictionless: they can return to proprietary development if it serves them better.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, quality_focused_organizations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, quality_focused_organizations, agenda_setter).

% Vendors whose revenue depends on restricting access to source code and modification experience competitive pressure from open source projects perceived as higher quality or faster-innovating. They bear the cost of this constraint through reduced market share and margin compression in domains where open development has proven superior. Their exit—adopting open source themselves—is possible but strategically costly (disrupts existing licensing revenue, requires cultural change).
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Large organizations procuring software benefit from open source alternatives that reduce vendor lock-in risk, enable independent security auditing, and lower total cost of ownership. They have significant exit optionality: they can choose proprietary tools where open source is inadequate for specific workloads, or negotiate with open source vendors for support.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, enterprise_it_procurement, beneficiary,
    powerful, biographical, arbitrage, global).

% Security researchers benefit from source code transparency that enables independent vulnerability discovery and verification. The constraint's transparency norm allows them to audit systems, publish findings, and contribute fixes. They are not trapped; they can work with proprietary codebases under different arrangements.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, security_researchers, beneficiary,
    organized, generational, mobile, global).

% Proprietary software platforms built on licensing revenue models and trade-secret protection are structurally excluded from participating in the open development constraint's peer-review and distributed innovation mechanisms. They could transition to open source but the shift would dissolve their existing business model. Their exclusion is not enforced but is structural—the reading itself treats their methodology as inferior.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, legacy_closed_source_ecosystems, excluded,
    institutional, biographical, trapped, global).

% Technologists in regions with limited software procurement budgets benefit substantially from open source availability at zero licensing cost and with permissive licensing. They gain access to state-of-the-art tools without vendor gatekeeping. Exit options are constrained by budget; they cannot easily purchase equivalent proprietary alternatives.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, developing_economy_technologists, beneficiary,
    moderate, generational, constrained, global).

% Software licensing and DRM infrastructure vendors whose business depends on enforcing access restrictions experience this reading as delegitimizing their methodology. They are excluded because the constraint treats their core activity—licensing restriction—as inferior to transparency-based quality assurance. They could argue for alternative readings but remain outside the pragmatic-development frame.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_license_vendors, excluded,
    powerful, generational, constrained, global).

% Technical standards bodies like IETF and W3C operate primarily through open development and published standards. They observe this constraint as vindicating their methodology and operate analytically—they are not parties to extraction or coordination but are shaped by the constraint's legitimacy frame.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, standards_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates distributed peer review, vulnerability discovery, and iterative refinement at scale by making source code and modification history publicly visible and permitting community contribution. Solves the coordination problem: how to scale code review and quality assurance beyond what a single organization can perform, and how to capture innovation from distributed contributors.
% TRANSFER_FUNCTION: Transfers quality assurance labor from centralized vendor organization to distributed community of users, researchers, and volunteers. Transfers the opportunity for reputation and career advancement to visible contributors. Transfers cost of feature development and maintenance toward whoever benefits most from specific enhancements.
% ABSENT_VOICES: Proprietary software vendors and those who profit from licensing scarcity would object to the reading's claim that open development is methodologically superior; they are structurally excluded because the constraint's premise treats their methodology as inferior rather than merely different. Legacy closed-source maintainers are not in the conversation that vindicates open development.
% DISAPPEARANCE_RATIONALE: If this constraint—the normative claim that open development is superior—vanished and proprietary development regained legitimate standing as an equal methodology, the software economy would reorganize: vendor lock-in strategies would return, quality assurance would revert to vendor-controlled closed testing, security researchers would lose transparency-based audit access, and organizations would face higher switching costs. The shift would not be overnight but would compress over 5–10 years as the legitimacy-frame changed.
% FOUNDING_PROBLEM: Early (1990s–2000s) closed-source development produced high-defect software with slow patch cycles and vendor-driven priorities that diverged from user needs. Open source projects (Linux, Apache, Python) demonstrated faster innovation, better security response, and higher code quality through peer review.
% FOUNDING_PROBLEM_CORROBORATION: Academic research on code quality (meta-analyses from 2010–2020 comparing open and proprietary projects) corroborates that peer review accelerates bug detection; independent security researchers attest that source transparency enables faster vulnerability response; however, proprietary software vendors argue that their internal QA processes now match or exceed open source quality, and that governance, liability, and performance metrics differ systematically. The founding problem's resolution is disputed between reading communities.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).
:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.31 at interval end) because the constraint operates primarily through legitimate quality-based competition, not coercion. Proprietary vendors lose market share, but because open source demonstrably produces superior outcomes in many domains—not because they are forcibly excluded. Suppression is low (0.18) because the constraint requires minimal active enforcement: the legitimacy of open development is now broadly accepted in technology communities, and adoption is voluntary. Theater ratio is minimal (0.12) because the functional justification (peer review improves quality, transparency enables faster innovation) appears to match the actual operative mechanism. The measurement series shows slow extraction accumulation over the interval (0.18 → 0.31): as open source became more economically dominant, proprietary vendors experienced increasing competitive pressure and margin compression; however, the rate of accumulation plateaus after t=25, suggesting the constraint reaches an equilibrium where both models coexist in different market segments. The claim/metric independence is preserved: this reading is CLAIMED as rope (genuine coordination that benefits all participants) while extraction metrics describe a real but modest asymmetry (proprietary vendors do lose share). The engine will determine whether the computed classification matches or diverges from the claim.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (collaborative communities, users), the constraint is coordinative: open development solves a genuine coordination problem (how to scale quality assurance). From the payer seat (proprietary vendors), the constraint operates as competitive displacement: their methodology is labeled inferior and markets shift accordingly. Both narratives are true structurally. The engine will compute different per-seat types from identical structural data because the seats' relationships to the constraint differ in exit options, power, and beneficiary/victim status. A proprietary vendor with trapped exit (cannot easily adopt open source due to legacy codebase, corporate identity, or revenue structure) computes a higher d and may see the constraint as snare-like from their seat; a vendor with arbitrage options (can fork or adopt hybrid models) computes lower d and perceives rope-like coordination with adjustment costs.
 *
 * DIRECTIONALITY LOGIC:
 *   WHO benefits and bears costs: Collaborative communities and users benefit from transparency and distributed innovation. Proprietary vendors bear competitive pressure from superior (in this reading's terms) open source competitors. Quality-focused organizations benefit from faster innovation but bear governance overhead. Exit options differentiate: communities are mobile, users have arbitrage options, organizations have arbitrage options, vendors have constrained exit (organizational transformation required). The directionality derivation routes this to: beneficiaries at low d, constrained-exit payers at higher d, arbitrage agents at low d. No explicit overrides are needed; the structural data produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy in its reading-native frame. The founding problem (closed-source development produced high-defect, slow-moving software) was real; the constraint (open development is superior) addresses it by channeling innovation and quality assurance through transparency. Within this reading's epistemic frame, the founding problem remains live: proprietary vendors continue to struggle with quality and release velocity issues, and open source projects continue to demonstrate faster innovation. The constraint is not a zombie—it persists because the founding problem persists and open development continues to solve it. However, the contested status (proprietary vendors now argue their QA matches open source quality; some studies suggest the quality delta has narrowed) means mandatrophy is CONTESTED rather than resolved. An omega variable captures this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Has the founding problem (closed-source high-defect, slow-moving development) been substantially resolved by proprietary vendors'' improved QA practices, or does it remain live?',
    'Longitudinal code-quality metrics comparing open and proprietary projects matched by domain and scale (2015–2035); security patch latency studies; customer incident data; independent audits from non-vendor sources.',
    'If closed-source QA has caught up, the founding problem is dead and the constraint may become mandatrophic (persisting by legitimacy frame alone, not functional need). If the founding problem remains live, the constraint''s functional grounding persists. Classification could shift from rope (genuine coordination) toward piton (inertial persistence) if metrics suggest the problem is solved but the constraint survives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem that motivates this reading remains live or has been substantially resolved.').

omega_variable(
    alternative_reading_foreclosure,
    'Does this reading logically foreclose the property_rights_reading, or can both coexist?',
    'Examine whether a framework could coherently hold (a) that open development is superior methodology for quality/innovation AND (b) that software creators have legitimate right to restrict access and modification. If both are simultaneously holdable without contradiction, relation is coexists_with; if the pragmatic reading''s core claim (transparency improves outcomes) logically contradicts property-restriction, relation is forecloses.',
    'If forecloses: the two readings cannot coexist in a single legal or organizational framework; one reading''s adoption requires the other''s rejection. If coexists_with: both readings remain live positions held by different organizational cultures or regulatory regimes. This determines cs_structure.reading_relations.relation value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Logical relationship between this reading''s core premise and property-rights reading''s core premise.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.18) enforced externally (proprietary vendors excluded by policy/governance) or internalized (they believe open development is indeed superior and have adopted it)? What portion of proprietary vendors'' current exit constraint comes from active exclusion vs. self-perceived inferiority?',
    'Survey proprietary software vendors and legacy closed-source maintainers: measure (a) how many would adopt open source if competitive pressure disappeared; (b) how many believe their methodology is inferior; (c) how many experience active policy/organizational barriers (hiring preferences, procurement mandates); (d) post-exit trajectory of vendors who transition to open source—does their self-perception of quality shift?',
    'If mostly internalized (vendors believe the reading''s premise), suppression is lower than measured and the constraint operates more as legitimate competition than coercive extraction. If mostly enforced externally (procurement mandates, hiring exclusions, technical standards that exclude proprietary participation), suppression is accurately measured and the constraint carries a coercive dimension. This affects classification: if internalized, the constraint is more genuinely rope; if externally enforced, it may approach tangled_rope (coercive extraction riding on legitimate-development-practice justification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in proprietary vendors'' constrained exit.').

omega_variable(
    kernel_reading_underspecification,
    'Is this reading (pragmatic-development) truly distinct from utilitarian_hybrid_reading? Does ''open source is superior for quality'' foreclose ''both models serve different contexts''?',
    'Compare axioms: if pragmatic-reading asserts open superior universally, it forecloses utilitarian; if it asserts open superior in most contexts with exceptions for specific workloads (safety-critical, real-time), then it coexists_with utilitarian. Examine whether the reading''s actual usage acknowledges context-dependence or claims universal superiority.',
    'If forecloses: the reading_relations should declare forecloses to utilitarian_hybrid. If coexists_with: declare coexists_with. This affects how the engine models competing legitimate claims about software development.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underspecification, conceptual, 'Boundary ambiguity between pragmatic-development and utilitarian-hybrid readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__pragmatic_development_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_source_status__pragmatic_development_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_source_status__pragmatic_development_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_source_status__pragmatic_development_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t20, software_source_status__pragmatic_development_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(soft_tr_t20, observed).
narrative_ontology:measurement(soft_tr_t25, software_source_status__pragmatic_development_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(soft_tr_t25, observed).
narrative_ontology:measurement(soft_tr_t30, software_source_status__pragmatic_development_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(soft_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__pragmatic_development_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_source_status__pragmatic_development_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_source_status__pragmatic_development_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_source_status__pragmatic_development_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t20, software_source_status__pragmatic_development_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement_basis(soft_be_t20, observed).
narrative_ontology:measurement(soft_be_t25, software_source_status__pragmatic_development_reading, base_extractiveness, 25, 0.31).
narrative_ontology:measurement_basis(soft_be_t25, observed).
narrative_ontology:measurement(soft_be_t30, software_source_status__pragmatic_development_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(soft_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__pragmatic_development_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t5, software_source_status__pragmatic_development_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement_basis(soft_su_t5, observed).
narrative_ontology:measurement(soft_su_t10, software_source_status__pragmatic_development_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement_basis(soft_su_t10, observed).
narrative_ontology:measurement(soft_su_t15, software_source_status__pragmatic_development_reading, suppression_requirement, 15, 0.16).
narrative_ontology:measurement_basis(soft_su_t15, observed).
narrative_ontology:measurement(soft_su_t20, software_source_status__pragmatic_development_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement_basis(soft_su_t20, observed).
narrative_ontology:measurement(soft_su_t25, software_source_status__pragmatic_development_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement_basis(soft_su_t25, observed).
narrative_ontology:measurement(soft_su_t30, software_source_status__pragmatic_development_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement_basis(soft_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, information_standard).
narrative_ontology:boltzmann_floor_override(software_source_status__pragmatic_development_reading, 0.05).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE of FOUR readings of the software_source_status kernel. Each reading instantiates a different constraint with a different epsilon, beneficiary structure, and claimed type. The pragmatic-development reading claims ROPE (genuine coordination through peer review); the freedom_imperative_reading claims that same coordination is driven by ethical rights (different grounding, different axioms); the property_rights_reading claims the same activities are extraction from rightful creators; the utilitarian_hybrid_reading claims context-dependent validity of both open and proprietary models. Each constraint story models one reading independently — do NOT merge or average ε across readings. The readings are structurally distinct because they disagree on WHAT MAKES THE PRACTICE LEGITIMATE, not because they disagree about its effects. Kernel decomposition per DP-001 (ε-invariance principle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
