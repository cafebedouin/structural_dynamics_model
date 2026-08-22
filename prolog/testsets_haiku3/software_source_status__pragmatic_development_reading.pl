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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Open Source as Superior Development Methodology (Pragmatic Reading)
 *   domain: software_engineering/political_economy_of_technology
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   software_source_status. The reading claims that open-source development
 *   is a superior METHODOLOGY for producing high-quality, secure software —
 *   the mechanism is peer review, distributed bug detection, and rapid
 *   community iteration. This reading does NOT claim software freedom is a
 *   fundamental right (that is the freedom_imperative_reading); it claims
 *   freedom is INSTRUMENTALLY VALUABLE because it enables the methodology. It
 *   accepts that proprietary software is not inherently illegitimate but
 *   argues that on software-quality grounds, open models outperform. The
 *   reading brackets patent law (treats it as orthogonal) and does not
 *   address whether programmers have a right to restrict others' use (treats
 *   that as a separate normative question). Importantly: the constraint
 *   extracted by this reading has LOWER extractiveness (0.38) than snare-type
 *   constraints would have because no party is structurally trapped into a
 *   worse outcome — the beneficiaries genuinely receive software quality
 *   benefits, the payers (proprietary vendors) face competitive disadvantage
 *   but retain the choice to adopt open development themselves. This is
 *   genuinely a rope-type coordination arrangement, not a snare masquerading
 *   as one.
 *
 * KEY AGENTS:
 *   - open_source_developers: Organize collaborative development; set/defend the methodology norm; benefit from peer participation and distributed debugging.
 *   - downstream_developers: Integrate open-source components; gain access to peer-reviewed code; reduced risk and time-to-market.
 *   - security_researchers: Audit open-source code; enable systematic vulnerability discovery; build careers on findings; benefit from visibility.
 *   - proprietary_software_vendors: Face institutional pressure to justify closed models; carry competitive disadvantage under this reading's framing; not structurally locked into losses but face legitimacy erosion.
 *   - enterprise_adopters: Choose between open and proprietary; gain quality and audit benefits from open but also sustainability risk; genuinely benefited by the constraint but not locked in.
 *   - users: Diffuse benefit through reduced defects and faster security response; powerless and unorganized; lack visibility into open/closed distinction.
 *   - academic_researchers: Arbitrate empirical claims; produce evidence on software quality differences; carry no extractive stake; analytical seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.38).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.22).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Superior Development Methodology (Pragmatic Reading)").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy_of_technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, '142bc5f8-e585-49fe-9539-ad1ea38c91cb').
narrative_ontology:cs_kernel_codification('142bc5f8-e585-49fe-9539-ad1ea38c91cb', distributed).
narrative_ontology:cs_authority_grounding('142bc5f8-e585-49fe-9539-ad1ea38c91cb', diffuse_epistemic).
narrative_ontology:cs_reading_relation('142bc5f8-e585-49fe-9539-ad1ea38c91cb', software_source_status__freedom_imperative_reading, influences).
narrative_ontology:cs_reading_relation('142bc5f8-e585-49fe-9539-ad1ea38c91cb', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('142bc5f8-e585-49fe-9539-ad1ea38c91cb', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('142bc5f8-e585-49fe-9539-ad1ea38c91cb', foundational, open_development_superior_for_quality).
narrative_ontology:cs_axiom_status(open_development_superior_for_quality, holdable).
narrative_ontology:cs_axiom_grounding('142bc5f8-e585-49fe-9539-ad1ea38c91cb', open_development_superior_for_quality, empirically_contingent).
narrative_ontology:cs_axiom('142bc5f8-e585-49fe-9539-ad1ea38c91cb', secondary, distributed_peer_review_mechanism_valid).
narrative_ontology:cs_axiom_status(distributed_peer_review_mechanism_valid, holdable).
narrative_ontology:cs_axiom_grounding('142bc5f8-e585-49fe-9539-ad1ea38c91cb', distributed_peer_review_mechanism_valid, empirically_contingent).
narrative_ontology:cs_reference_frame('142bc5f8-e585-49fe-9539-ad1ea38c91cb', open_source_development_as_optimal_methodology).
narrative_ontology:cs_drift_state('142bc5f8-e585-49fe-9539-ad1ea38c91cb', contemporary_hybrid_licensing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('142bc5f8-e585-49fe-9539-ad1ea38c91cb', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, peer_review_participants).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, downstream_developers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, quality_verification_communities).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, innovation_ecosystem).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, security_researchers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, enterprise_adopters).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, users_of_software).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, enterprise_adopters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize collaborative development under permissive or copyleft licenses. They set technical standards, manage code review processes, and establish norms that visibility-through-code is superior to closed iteration. They benefit from peer participation, bug reports from diverse users, and collaborative problem-solving. They could exit to proprietary development or proprietary-backed open models but choose coordination.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_developers, agenda_setter,
    organized, generational, mobile, global).

% Integrate open source components into their own projects (proprietary or open). They gain access to peer-reviewed, battle-tested code without replicating development effort. They can inspect, understand, and modify the code when bugs arise or requirements shift. Their benefit is real software quality, reduced time-to-market, and reduced risk. They choose this because the alternative (building from scratch or licensing proprietary closed libraries) is more costly.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, downstream_developers, beneficiary,
    moderate, biographical, mobile, global).

% Audit open source code for vulnerabilities and can report findings to the community. The visibility of code enables systematic security analysis; closed source requires reverse-engineering or black-box testing. They build careers on vulnerability disclosure and mitigation. They benefit from the constraint's transparency; their work is enabled by it.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, security_researchers, beneficiary,
    organized, generational, mobile, global).

% Face institutional and market pressure to justify closed-source licensing decisions. The pragmatic-development reading delegitimizes their closed models on efficiency grounds (not on freedom grounds, which they can contest). Their code is harder to audit, slower to evolve through external contributions, and increasingly seen as a market vulnerability. They carry both competitive disadvantage and reputational cost under this reading.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_software_vendors, payer,
    powerful, generational, arbitrage, global).

% Can choose between open source and proprietary libraries for infrastructure. Under this reading they benefit from open source's audit surface, flexibility, and community support; they also carry switching costs if they move to proprietary alternatives or if open source library abandonment occurs. They gain software quality and risk reduction but also exposure to open source sustainability questions.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, enterprise_adopters, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, enterprise_adopters, payer).

% Hold portfolio claims on software techniques that open source development might infringe. They are excluded from the discussion because the pragmatic reading brackets patent legitimacy entirely (treating it as orthogonal to the development methodology comparison). Under this reading, the code's openness is what matters for quality; patent claims are a separate legal/political question not addressed by the methodology argument.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, patent_holders_on_software, excluded,
    institutional, generational, trapped, global).

% Use software built from open source components or pure open source tools. They benefit from the peer review that underpins code quality and from the community's ability to fix security issues. They typically lack visibility into whether the software they use is open or closed source; the benefit accrues diffusely through reduced defects, faster security patching, and ecosystem innovation.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, users_of_software, beneficiary,
    powerless, biographical, constrained, global).

% Study software quality, development velocity, and security outcomes empirically. They measure whether open source development produces better software faster, test the mechanism (peer review, distributed debugging), and arbitrate the factual claims the reading makes. They carry no extractive stake but produce the evidence that would settle the reading's core empirical claims.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, academic_research_communities, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__pragmatic_development_reading, open_source_developers).
narrative_ontology:fixing_cost_class(software_source_status__pragmatic_development_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the distributed-debugging problem: software quality depends on finding and fixing subtle bugs; closed-source iteration by one vendor finds fewer bugs than open-source iteration with distributed testing and reporting. The constraint coordinates this by making the code visible, establishing review norms, and enabling contribution.
% TRANSFER_FUNCTION: Transfers the intellectual property claim from 'sole creator controls all modification and use' to 'creator retains attribution but community can modify.' Under this reading the transfer is not zero-sum extraction but a reallocation that improves software outcomes for all parties. The developer trades exclusive control for quality improvements and reduced long-term maintenance burden.
% ABSENT_VOICES: Proprietary software advocates who dispute the empirical claim (that open development produces better software faster). They argue for closed-source iteration advantages (coherent vision, rapid iteration without consensus overhead, protection of trade secrets) and are implicitly excluded from the pragmatic reading's framing. Patent holders are structurally excluded because the reading does not address patent legitimacy.
% DISAPPEARANCE_RATIONALE: If the open-source-as-superior-methodology reading vanished, institutional incentives would shift: vendors could justify closed source on methodology grounds; the peer-review infrastructure would degrade without the legitimacy narrative supporting it; software quality outcomes would diverge as closed iterations replaced open ones. The software industry would reorganize around proprietary or hybrid models.
% FOUNDING_PROBLEM: Software bugs cause cascading failures in mission-critical systems; a single vendor's test suite cannot find all bugs; distributed review catches more. Early proprietary development practices isolated code from external scrutiny, leaving vulnerabilities undiscovered until production failure.
% FOUNDING_PROBLEM_CORROBORATION: Security researchers, open source communities, and enterprise adopters attest that the founding problem persists — zero-day vulnerabilities still emerge from closed-source code, and distributed peer review does catch bugs proprietary review misses. Academic studies (e.g., studies comparing CVE discovery rates in open vs. closed projects) corroborate the mechanism from seats outside the beneficiary set. Proprietary vendors dispute the magnitude of the advantage.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38 at interval end) and rising slowly. The rise reflects increasing institutional capture of the open-source norm by large vendors (they adopt open licensing for signal/capture while retaining significant proprietary upper layers), which shifts some of the coordination benefit to vendors' bottom lines. Early in the interval, extractiveness is lower (0.28) because the norm is more organic, less institutionalized. Theater is low (0.18) because the constraint's primary function — coordinating distributed peer review for quality — remains genuine; the performative share is mainly vendors' citation of 'open source' in marketing without depth of community engagement. Suppression is consistently low (0.22 at interval end) because no party needs coercion to participate — downstream developers WANT the peer-reviewed code, security researchers WANT the transparency, and even proprietary vendors now often choose to open-source components. The suppression that does exist is subtle: institutional pressure against vendors to justify closed models (soft norm enforcement), patent litigation pressure on open communities (legal suppression, not authored directly here but referenced in the excluded_voices), and sustainability pressure on open projects (funders prefer projects with commercial backing). Accessibility collapse is moderate (0.45): once a developer understands the peer-review mechanism, the alternative (closed proprietary development) is theoretically accessible — there is no lock-in on the level of economic traps or identity fusion. However, in practice, abandoning the open-development norm carries reputational cost in many communities, and forking into proprietary requires capital. Resistance is moderate (0.58): proprietary vendors resist the reading's legitimacy claim; they dispute the quality metrics and argue for closed development's coherence and market advantages. Patent holders implicitly resist but are excluded from the frame. The measurement series show extractiveness and theater rising slowly over the interval as institutional capture increases, but at a gentle trajectory — this is a mature reading that has stabilized, not one in rapid transformation.
 *
 * PERSPECTIVAL GAP:
 *   The pragmatic reading should compute DIFFERENTLY from the freedom-imperative reading in per-seat classification. Proprietary vendors under the pragmatic reading experience the constraint as competitive disadvantage on methodology grounds (they can respond by adopting open development), whereas under the freedom-imperative reading they experience moral censure (ethical illegitimacy regardless of outcomes). The pragmatic reading is structurally more like rope for them (coordination disadvantage, mobile exit, choice-available response); the freedom-imperative is more like snare (moral trap, less exit available if the reading is hegemonic). The engine should compute this divergence from the structural difference between 'efficiency claim' (pragmatic) and 'ethical claim' (freedom-imperative) — the two readings instantiate different constraints with different extractiveness profiles. Similarly, downstream developers under this reading benefit from peer review; under the property-rights reading they would experience the open-license requirement as a restriction on what they can do with the code (less dramatic, but a different d-position). The key to perspectival gaps is that the KERNEL (software source status) is the same, but the reading-instantiated constraints have different stakeholder roles and directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Open-source developers sit near the beneficiary end (d ≈ 0.25): they benefit from peer participation and distributed debugging, face low suppression (participation is voluntary), and have high exit options (can fork, privatize, or shift to hybrid models). Downstream developers also sit beneficiary-side (d ≈ 0.30): they gain software quality and risk reduction; exit options are mobile (can switch to proprietary libraries); they are genuinely benefited without extraction. Security researchers sit beneficiary-side (d ≈ 0.20): the transparency is the core benefit; they face no cost under this reading. Enterprise adopters sit near the symmetric point (d ≈ 0.50): they genuinely benefit from open source's auditability and community support but also carry sustainability risk and switching costs if they depend on abandoned projects. Proprietary vendors sit toward the target end (d ≈ 0.65): they face competitive and reputational disadvantage under this reading's framing (the reading delegitimizes closed models on efficiency grounds). Importantly, they are NOT trapped — they can adopt open development, and many increasingly do. The directionality is not d=1.0 (full target) because no party is structurally locked into losses; the vendors carry disadvantage but retain arbitrage exit. Users sit near neutral (d ≈ 0.45): they benefit from the constraint's effects (better software) but are powerless and unorganized; the benefit accrues diffusely. No directionality override is needed; the derivation from beneficiary/victim declarations and exit options produces the correct picture.
 *
 * MANDATROPHY ANALYSIS:
 *   The pragmatic reading does not present a mandatrophy-resolved situation. The founding problem (software bugs cause cascading failures; distributed peer review finds more bugs) is LIVE — zero-day vulnerabilities and software failures remain real problems in production systems. The disappearance verdict is world_rearranges because the constraint coordinates a genuine solution to an ongoing coordination problem. No mandate has outlived its function. However, there is a LOW-LEVEL mandatrophy threat: as proprietary vendors increasingly embed open components while keeping upper layers closed, the coordination surface becomes fragmented. If the open layers' peer review cannot effectively audit closed wrapper behavior, the constraint's core function (distributed debugging enables quality) degrades. This is not yet dead-mandate territory but a drift toward it. An omega variable (sibling_reading_boundary and proprietary_embedded_in_open_ecosystem) captures this uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_quality_claim_contestability,
    'Does open-source development actually produce better software faster than proprietary development, holding all else equal?',
    'Controlled empirical comparison: measure defect density, security vulnerability discovery rate, time-to-patch, and innovation velocity across matched pairs of open and closed projects. Academic studies have begun this; meta-analysis of published results would resolve.',
    'If open development demonstrably produces superior quality metrics, the pragmatic reading''s core claim is vindicated and the constraint is genuinely a coordination solution. If quality outcomes are equivalent or proprietary is superior, the constraint becomes ideological cover for a redistributive claim about code access that is not methodologically justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_quality_claim_contestability, empirical, 'Whether the open-development methodology claim rests on empirically true software quality comparisons.').

omega_variable(
    licensing_model_instrumental_vs_terminal,
    'Is open licensing valued BECAUSE it produces better software (instrumental), or is software freedom a foundational value and the quality claim is post-hoc rationalization?',
    'Discourse analysis of community statements and motivations; historical tracing of the claim''s evolution (did quality metrics drive adoption or follow it?); behavioral observation of willingness to trade licensing openness for better software if proprietary offered superior outcomes.',
    'If instrumental (quality is the goal, licensing is means), the pragmatic reading is analytically distinct from the freedom_imperative_reading and the constraint is a genuine coordination tool. If terminal (freedom is the value), the reading is actually identical to freedom_imperative and the quality claim is narrative cover — the constraint''s type would recompute as ideological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_model_instrumental_vs_terminal, conceptual, 'Whether open licensing is valued for its consequences (better software) or as an end in itself.').

omega_variable(
    proprietary_embedded_in_open_ecosystem,
    'As proprietary software increasingly embeds open components under permissive licenses while keeping upper layers closed, does the pragmatic reading''s coordination function degrade?',
    'Track the shift in software composition (open vs. closed, by layer/project over time). Measure whether peer review coverage decays when code is embedded in closed wrappers. Measure whether the quality gains from the open layers persist or are negated by closed upper-layer choices.',
    'If the empirical quality gains remain robust even in hybrid models, the constraint''s claim is resilient. If the gains degrade substantially, the reading may be describing a transient coordination problem that is being bypassed by hybrid licensing — the constraint could be approaching obsolescence or reclassifying toward theater-heavy piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_embedded_in_open_ecosystem, empirical, 'Whether the constraint''s coordination function remains effective as software architecture hybridizes.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does the claim ''open development is superior for software quality'' logically foreclose the property_rights_reading (''software is intellectual property; creators have legitimate right to restrict''), or can both be held simultaneously?',
    'Analyze the logical structure: the pragmatic reading asserts a causal claim (openness → quality); the property-rights reading asserts a normative claim (creators own restrictions). Pragmatic efficiency does not rule out a creator''s right to choose inefficiency. The readings coexist unless one claims the other''s premises are incoherent.',
    'If the readings coexist, the sibling relationship is coexists_with (both live, held by different parties). If the pragmatic reading claims creators have no RIGHT to restrict access even if they technically could, then foreclosure applies. The boundary determines whether the kernel admits two live readings or one dominant one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Logical independence of the pragmatic-development and property-rights readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__pragmatic_development_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_source_status__pragmatic_development_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_source_status__pragmatic_development_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_source_status__pragmatic_development_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t20, software_source_status__pragmatic_development_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(soft_tr_t20, observed).
narrative_ontology:measurement(soft_tr_t25, software_source_status__pragmatic_development_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(soft_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__pragmatic_development_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_source_status__pragmatic_development_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_source_status__pragmatic_development_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_source_status__pragmatic_development_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t20, software_source_status__pragmatic_development_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(soft_be_t20, observed).
narrative_ontology:measurement(soft_be_t25, software_source_status__pragmatic_development_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(soft_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__pragmatic_development_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t5, software_source_status__pragmatic_development_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement_basis(soft_su_t5, observed).
narrative_ontology:measurement(soft_su_t10, software_source_status__pragmatic_development_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(soft_su_t10, observed).
narrative_ontology:measurement(soft_su_t15, software_source_status__pragmatic_development_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement_basis(soft_su_t15, observed).
narrative_ontology:measurement(soft_su_t20, software_source_status__pragmatic_development_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(soft_su_t20, observed).
narrative_ontology:measurement(soft_su_t25, software_source_status__pragmatic_development_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(soft_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__pragmatic_development_reading, 0.18).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel software_source_status. The kernel admits four analytically distinct readings: freedom_imperative_reading (freedom as foundational right), pragmatic_development_reading (this reading — open development superior for quality), property_rights_reading (creators own restrictions), and utilitarian_hybrid_reading (outcome optimization). These are separate constraint stories with different ε values, different beneficiary/victim structures, and different types. They are linked via network.affects_constraints to enable corpus analysis of kernel-reading dynamics. Each reading should be generated as a self-contained constraint story, independent of the others in structural data, with committer-frame omega variables documenting the sibling relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
