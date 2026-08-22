% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software Control as Denial of User Freedom (Freedom Imperative Reading)
 *   domain: political_economy_of_technology/software_engineering/intellectual_property
 *
 * SUMMARY:
 *   Under the freedom imperative reading of the software control legitimacy
 *   kernel, proprietary software is understood as ethically illegitimate
 *   because it denies users control over computations occurring on their own
 *   devices. The constraint is the set of legal, technical, and institutional
 *   mechanisms that enforce proprietary ownership restrictions: copyright
 *   law, patents, DMCA anti-circumvention rules, DRM, end-user license
 *   agreements, and architectural lock-in. From this reading's seat, all
 *   proprietary software operates as a snare: it extracts user autonomy and
 *   computational transparency while presenting the arrangement as necessary
 *   for quality, security, and sustainable development. The beneficiary is
 *   the set of software rights holders who collect rents through control. The
 *   victims are end users (denied autonomy), dependent developers
 *   (platform-locked), and security researchers (barred from transparency).
 *   The founding problem (cost recovery for expensive software) is dead;
 *   alternative models (open source, commons governance) demonstrate the
 *   coordination functions persist without the control extraction. This
 *   reading claims that the remaining extraction is pure snare, justified by
 *   an inaccurate naturalness claim.
 *
 * KEY AGENTS:
 *   - End users: structurally trapped by network effects and vendor lock-in; bear the cost of surrendered autonomy
 *   - Software rights holders: agenda-setters using copyright, patents, and technical architecture to enforce control; collect rents through licensing and lock-in
 *   - Dependent developers: moderately powerful but identity-locked to proprietary platforms; pay through loss of autonomy over their own development environment
 *   - Security researchers: organized but legally barred from inspecting code; constrained by DMCA and proprietary licensing
 *   - Free software movement: organized advocates for user freedom but excluded from institutional software policy decisions
 *   - Technology regulators: observers investigating whether software restrictions constitute anti-competitive behavior or violate emerging rights to repair and computational autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.92).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.88).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software Control as Denial of User Freedom (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "political_economy_of_technology/software_engineering/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, 'b897eb01-68f6-41cf-8dbe-0d615fb29e88').
narrative_ontology:cs_kernel_codification('b897eb01-68f6-41cf-8dbe-0d615fb29e88', fixed_text).
narrative_ontology:cs_authority_grounding('b897eb01-68f6-41cf-8dbe-0d615fb29e88', distributed).
narrative_ontology:cs_reading_relation('b897eb01-68f6-41cf-8dbe-0d615fb29e88', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('b897eb01-68f6-41cf-8dbe-0d615fb29e88', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('b897eb01-68f6-41cf-8dbe-0d615fb29e88', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('b897eb01-68f6-41cf-8dbe-0d615fb29e88', foundational, computational_self_determination_as_fundamental_right).
narrative_ontology:cs_axiom_status(computational_self_determination_as_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('b897eb01-68f6-41cf-8dbe-0d615fb29e88', computational_self_determination_as_fundamental_right, deontological).
narrative_ontology:cs_axiom('b897eb01-68f6-41cf-8dbe-0d615fb29e88', foundational, user_autonomy_supersedes_creator_property).
narrative_ontology:cs_axiom_status(user_autonomy_supersedes_creator_property, holdable).
narrative_ontology:cs_axiom_grounding('b897eb01-68f6-41cf-8dbe-0d615fb29e88', user_autonomy_supersedes_creator_property, deontological).
narrative_ontology:cs_reference_frame('b897eb01-68f6-41cf-8dbe-0d615fb29e88', user_computational_freedom_framework).
narrative_ontology:cs_drift_state('b897eb01-68f6-41cf-8dbe-0d615fb29e88', contemporary_proprietary_dominance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b897eb01-68f6-41cf-8dbe-0d615fb29e88', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, software_rights_holders).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, end_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, dependent_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, security_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, dependent_developers).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, user_autonomy_as_fundamental_right).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, computational_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase or access proprietary software that executes on their devices but controls remain with the vendor. They cannot inspect source code, modify the software to suit their needs, redistribute it, or understand what computations occur on their machines. Exit requires abandoning essential digital infrastructure (operating systems, productivity tools, communication platforms) for which proprietary incumbents have created network effects and switching costs. They bear the full extraction cost: surrendered autonomy, potential surveillance, inability to repair or adapt the software, and forced dependency on vendor decisions about security updates, feature changes, and discontinuation.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, end_users, payer,
    powerless, biographical, trapped, global).

% Set and enforce licensing terms that restrict copying, modification, and distribution of software. Use legal frameworks (copyright, patents, DMCA, terms of service) and technical measures (code obfuscation, DRM, license enforcement) to maintain control over the artifact and its use. Collect rents through licensing fees, subscription models, and lock-in strategies. Determine feature sets, security practices, and end-of-life timelines unilaterally. Their position is structural: they own the code and use state authority (intellectual property law) and technical architecture to enforce the constraint.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, software_rights_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Build applications on top of proprietary platforms (iOS, Windows, Android ecosystems) and must accept the rights holder's terms to reach markets. They gain access to large user bases but lose the ability to fork, modify the underlying system, or port their work to alternative platforms. Their professional identity and career advancement are constituted through mastery of proprietary systems; exiting means accepting downward mobility. They pay through loss of control over their own development environment and dependence on vendor decisions about API stability, feature availability, and platform economics.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, dependent_developers, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, dependent_developers, beneficiary).

% Cannot inspect proprietary software to find security vulnerabilities before they are exploited. They are barred by law from reverse-engineering closed code (DMCA section 1201) and face legal liability for disclosure even when acting in good faith. They must wait for vendors to discover and patch vulnerabilities or work through bug-bounty programs that require secrecy agreements. Their capacity to protect systems is structurally limited by the access restriction itself. They pay through inability to do their job (security research) transparently and the necessity of operating in legal gray zones.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, security_researchers, payer,
    organized, biographical, constrained, global).

% Advocates for user freedom and computational self-determination through copyleft licensing, open-source development, and software freedoms (use, study, modify, redistribute). They would be natural representatives in discussions of software control legitimacy but are systematically excluded from standard commercial and institutional software development decisions. Their voice is marginalized as ideological rather than technical; they have built alternative infrastructure (Linux, GNU tools) but lack institutional power to enforce their reading of legitimacy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_movement, excluded,
    organized, generational, constrained, global).

% Investigate whether software restrictions constitute anti-competitive behavior, whether users have a right to repair, and what security obligations rights holders bear. They observe the constraint from outside and can impose remedies (right-to-repair mandates, interoperability requirements, source code disclosure orders) that would restructure the enforcement mechanism. They operate at the boundaries of the constraint, testing whether the legal framework supporting it is legitimate.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, technology_regulators, observer,
    institutional, generational, analytical, national).

% The normative claim that computational self-determination is a fundamental human right—the principle that humans should retain control over the computations occurring on their devices. This is listed as a beneficiary not because it collects rents but because the freedom imperative reading vindicates this principle against competing framings (property rights, pragmatic development methodology). It is a proposition, not an actor; it is included to clarify what the reading's structure serves.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, computational_autonomy_principle, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(software_control_legitimacy__freedom_imperative_reading, computational_autonomy_principle).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, software_rights_holders).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function. The reading argues that software distribution, security review, and quality assurance can be and are accomplished through open-source and commons-based models without proprietary control. The appearance of coordination (curation, platform stability) is separable from and does not require proprietary ownership restrictions.
% TRANSFER_FUNCTION: Transfers computational autonomy, code transparency, and the right to modify software from users to software rights holders. The arrangement moves control over device functionality to the creator/vendor, who unilaterally determines features, security practices, discontinuation timelines, and data flows. It also transfers lock-in dependencies, making users unable to migrate without abandoning entire ecosystems.
% ABSENT_VOICES: Free software advocates and open-source communities are excluded from institutional software policy decisions, despite having developed working alternatives and possessing expert knowledge of sustainable software models. User collectives and end-user unions are absent from licensing negotiations and IP law formation. Security researchers are excluded by legal barriers (DMCA) from full participation in vulnerability disclosure and defense.
% DISAPPEARANCE_RATIONALE: If proprietary software control and its enforcement vanished, computational infrastructure would reorganize rapidly around open-source, libre software, and commons-based models. Users would inspect and modify code. Developers would not face platform lock-in. Vendor exit would not destroy user access (code would remain available for forking and maintenance). Security research would accelerate. Network effects would revert to open platforms (Linux, FreeBSD) instead of proprietary incumbents. The software economy would shift from licensing and subscription models to service, support, and development work. This reorganization is not merely theoretical—Linux, Apache, and open-source ecosystems demonstrate it is feasible at scale.
% FOUNDING_PROBLEM: Early computing made software expensive to reproduce and distribute. Vendors invested substantial capital in development and needed mechanisms to recover costs and ensure profitability. Proprietary ownership and licensing allowed vendors to charge for software as a commodity, funding continued development.
% FOUNDING_PROBLEM_CORROBORATION: Open-source communities, including enterprises (Red Hat, Canonical, Mozilla), have demonstrated sustainable software development and funding without proprietary control restrictions. Linux has become the dominant server operating system; Apache powers the majority of web servers; open-source tools are foundational to modern development. Multiple independent analyses (academic, industrial, regulatory) document that software cost recovery is no longer a barrier to development. Regulatory testimony from competition authorities and user advocates outside the rights-holder community confirms: the founding problem is solved by alternative models. Vendors themselves reinvest in open-source (contributing to Linux, supporting Apache) while maintaining proprietary restrictions on consumer-facing products, which proves the founding problem is not live—the proprietary restrictions persist for rent collection and lock-in, not for cost recovery.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured at 0.92 because this reading assesses proprietary control restrictions as nearly total denial of user computational autonomy. The extraction is not a percentage fee or price—it is the removal of control itself. Users cannot inspect code, modify it, redistribute it, or migrate their computing environment without abandoning essential infrastructure. The measurement series shows steady accumulation (0.85 to 0.92 over 40 time points): as software has become more central to daily life and more concentrated in proprietary platforms, the scope of extraction has widened. Suppression (0.88) reflects the combined force of legal restriction (copyright, DMCA), technical measures (code obfuscation, DRM, license enforcement), and market dominance (network effects making exit prohibitively costly). Theater ratio (0.22) is relatively low because the constraint's enforcement is straightforward and openly defended; there is less theatrical cover required compared to constraints that need to obscure their true function. The opening framing as 'security review' and 'quality assurance' is theater, but only a modest proportion of the machinery—the core enforcement is direct (you cannot modify the code because we own it and will prosecute you). Accessibility collapse (0.91) is very high: once a user understands that they cannot modify or inspect proprietary software, and that open-source alternatives exist, the constraint becomes transparent. The collapse is almost complete because the mechanism is simple and the deprivation is clear. Resistance (0.42) is moderate because substantial organized movements (free software, open-source communities, user advocates) actively resist the constraint, but they lack institutional power to overcome IP law and market dominance. This gap between high extraction/suppression and moderate resistance is the signature of a snare: the victims know it is happening but cannot escape it.
 *
 * PERSPECTIVAL GAP:
 *   From the software rights holder seat (the agenda-setter), the constraint is presented as necessary coordination: proprietary ownership funds development, ensures quality control, and provides security review. From this seat, the arrangement appears to be mutual benefit—users get reliable software, developers get sustainable income, rights holders recover their investment. From the end-user seat (powerless, trapped), the constraint is pure extraction: control over their own computing is denied, and the cost cannot be avoided without abandoning essential infrastructure. The perspective divergence is structural: the rights holder benefits from the constraint and experiences it as legitimate; the end user bears the cost and experiences it as illegitimate restriction. The engine computes this divergence from the base directionality data (power atoms, exit options, beneficiary/victim declarations) without requiring reconciliation of the claim. The freedom imperative reading authoring this story occupies the end-user perspective and declares the arrangement unethical precisely because of this asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   End users are mapped as victims (trapped exit, powerless position, clear cost bearing) with directionality near 1.0 (full targets of extraction). Software rights holders are mapped as beneficiaries and agenda-setters (institutional power, arbitrage exit options, direct collection of control rents) with directionality near 0.0 (full beneficiaries, subsidy from the arrangement). Dependent developers are dual-positioned: they benefit from platform access (market reach) but pay through platform lock-in (constrained exit, identity-locked to proprietary tools). Security researchers are mapped as payers: they bear suppression costs (legal barriers to research) but do not clearly benefit from the arrangement. The divergence between the reading's claim (snare: pure extraction) and the computed directionality is minimal here—the base_properties metric (extractiveness 0.92, suppression 0.88) and the beneficiary/victim structure align. The snare classification follows directly from the structural data without requiring overrides. The direction of extraction is consistent: from powerless, trapped end-users to institutional rights holders.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cost recovery for software development in an era of expensive hardware and difficult replication) is declared dead in the six_questions.founding_problem_status field. Under this reading, the problem was solved by network effects, open-source sustainability models, and reduced replication costs. Yet the constraint persists—proprietary software licensing remains dominant and enforced. This is mandatrophy: the mandate to fund software development through proprietary control has outlived its function. The constraint is maintained through institutional inertia (IP law, market incumbency, path dependence) and active rent collection (licensing fees, lock-in strategies) rather than by necessity. The theater_ratio (0.22) is diagnostic here: if the constraint were purely functional (still needed to fund development), theater would be lower (the function would do the work). The modest but present theater reflects the gap between the stated purpose (fund development) and the actual mechanism (extract control). The disappearance_verdict (world_rearranges) combined with founding_problem_status (dead) is the classic mandatrophy signal: if the constraint vanished, the world would reorganize (proof it is not natural), yet the problem it was built for no longer exists (proof the mandate is dead). Regulatory testimony and independent open-source communities corroborate this: alternative models sustain software development without proprietary control restriction. The classification as snare is the computational consequence: a constraint with high extraction, high suppression, moderate theater, dead founding problem, and organized resistance is exactly what a snare looks like.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_freedom_vs_property,
    'This constraint is ONE reading of a contested kernel: software_control_legitimacy. The sibling readings are property_rights_reading (creators have legitimate authority to restrict), pragmatic_openness_reading (open source is better methodology but proprietary is legitimate), and commons_reading (neither absolute freedom nor absolute property but negotiated collective management). Does the freedom imperative reading''s core axiom (computational_self_determination_as_right) logically foreclose the property_rights_reading''s axiom (creator_property_authority), or do they coexist as irreconcilable commitments held by different parties?',
    'Meta-ethical and jurisprudential analysis: can a framework simultaneously hold that (a) users have fundamental rights to computational self-determination and (b) creators have property rights to restrict use and modification? If yes, the readings coexist; if the rights genuinely conflict (one must yield), determine the priority rule.',
    'If foreclosed: the property_rights_reading is logically incoherent within a framework that privileges user autonomy as fundamental. If coexistent: both readings remain live and the constraint''s classification depends on which framework the observer occupies. This omega routes the kernel contest into the classification system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_freedom_vs_property, conceptual, 'Whether the freedom imperative and property-rights axioms logically foreclose each other or coexist as incommensurable commitments.').

omega_variable(
    separability_of_distribution_from_control,
    'Can software distribution, curation, and quality assurance (the coordination story) be accomplished through open-source and commons-based models without proprietary control restriction? Or is control restriction structurally necessary to fund and sustain software development?',
    'Empirical comparison: Linux, Apache, Wikipedia, and open-source ecosystems demonstrate large-scale, high-quality software development without proprietary control. Measure development cost and time, security outcomes, and feature velocity in open-source vs. proprietary models. If open-source models sustain comparable or superior quality, the coordination functions are separable from control restriction.',
    'If functions are separable: the measured extractiveness (0.92) is justified as pure extraction divorced from coordination necessity. The snare classification is confirmed. If inseparable: part of the extraction represents the legitimate cost of coordination and the reading''s claim of categorical illegitimacy is weakened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(separability_of_distribution_from_control, empirical, 'Whether software distribution and quality coordination require proprietary control restriction or are separable from it.').

omega_variable(
    computational_autonomy_as_fundamental_right,
    'Is computational self-determination (the ability to inspect, modify, and control code running on one''s own device) a fundamental human right comparable to bodily autonomy, freedom of thought, or privacy? Or is it a preference or luxury good that reasonably competes with other values (vendor sustainability, network effects, specialized expertise)?',
    'Normative and democratic deliberation: does society treat the right to computational transparency and control as fundamental (e.g., through regulation, universal access, mandates) or as a discretionary preference for enthusiasts? Examine rights frameworks (UN declarations, constitutional law, regulatory directives) and which societies elevate computational autonomy.',
    'If fundamental: the reading''s framing of proprietary software as ethically illegitimate is justified; all proprietary code becomes a violation of human dignity. If discretionary: the constraint''s classification shifts toward tangled_rope (legitimate coordination with extractive overlay) rather than pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(computational_autonomy_as_fundamental_right, preference, 'Whether computational self-determination is a fundamental right or a contestable preference.').

omega_variable(
    internet_archive_and_vendor_abandonment,
    'When proprietary software vendors discontinue products or services, users lose access to functionality they relied on, cannot modify the code to extend its life, and cannot port their data. Does this abandonment scenario constitute a harm specific to proprietary control that would not occur under open-source models?',
    'Historical case study: compare discontinuation harms in proprietary software (Adobe Flash, Windows XP security updates, abandoned smartphone apps) against open-source discontinuation (unmaintained FOSS libraries). Measure user impact and recovery options.',
    'If proprietary discontinuation causes unique, severe harm: this represents a failure mode of the constraint that strengthens the freedom imperative reading''s case. If open-source discontinuation causes comparable harm: the reading''s claim of categorical superiority is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internet_archive_and_vendor_abandonment, empirical, 'Whether proprietary software abandonment creates unique harms compared to open-source discontinuation.').

omega_variable(
    suppression_mechanism_identity_locked_or_structural,
    'The measured suppression (0.88) reflects both structural barriers (legal IP restrictions, technical code obfuscation, DRM) and internalized barriers (users accept proprietary software as normal, lack awareness of alternatives, have identity-fused with proprietary platforms). What proportion is structural vs. internalized?',
    'Post-exit trajectory: if users who migrate to open-source systems report increased autonomy and decreased suppression, the suppression was substantially structural. If suppression persists (users remain dependent, cannot evaluate alternatives, re-adopt proprietary tools), it is partially internalized.',
    'If structural: the suppression is external and can be removed by legal/technical remedies (right-to-repair, open-source mandates). If internalized: the constraint is deeper, dependent on changing user consciousness and habits, which may require longer timescales or different interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_locked_or_structural, empirical, 'Proportion of suppression that is structural (legal/technical barriers) vs. internalized (learned helplessness, normalized acceptance).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(soft_tr_t20, observed).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(soft_tr_t25, observed).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(soft_tr_t30, observed).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(soft_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 5, 0.87).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 10, 0.89).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 15, 0.9).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 20, 0.91).
narrative_ontology:measurement_basis(soft_be_t20, observed).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 25, 0.92).
narrative_ontology:measurement_basis(soft_be_t25, observed).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 30, 0.92).
narrative_ontology:measurement_basis(soft_be_t30, observed).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 40, 0.92).
narrative_ontology:measurement_basis(soft_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0, 0.79).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 5, 0.81).
narrative_ontology:measurement_basis(soft_su_t5, observed).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement_basis(soft_su_t10, observed).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 15, 0.84).
narrative_ontology:measurement_basis(soft_su_t15, observed).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement_basis(soft_su_t20, observed).
narrative_ontology:measurement(soft_su_t25, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement_basis(soft_su_t25, observed).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement_basis(soft_su_t30, observed).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement_basis(soft_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, global_infrastructure).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This story is part of the software_control_legitimacy kernel family, which decomposes a single natural-language concept into four structurally distinct constraint stories, one per reading of the contested kernel. Each reading instantiates a different epsilon (how much extraction), different beneficiary/victim structure, and different classification. The freedom_imperative_reading (this file) treats proprietary software as categorical denial of user rights (high epsilon, snare). The property_rights_reading treats it as legitimate creator authority (lower epsilon, rope or legitimate coordination). The pragmatic_openness_reading treats it as methodology choice (even lower epsilon, rope). The commons_reading treats it as commons governance question (moderate epsilon, tangled_rope or negotiated coordination). They share the same kernel (software control legitimacy) but diverge in what legitimacy framework they apply and who counts as the rights-holder. This decomposition follows the epsilon-invariance principle: different readings have different epsilon values because they are looking at different arrangements (one looking at the deprivation of control, another at the property right, another at development methodology). Each story links to the others via network.affects_constraints to enable kernel-contest analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
