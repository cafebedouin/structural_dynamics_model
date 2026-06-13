% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__freedom_imperative_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Software Source Code Restriction (Freedom Imperative Reading)
 *   domain: intellectual_property/political_economy_of_technology
 *
 * SUMMARY:
 *   This constraint embodies the freedom-imperative reading of contested
 *   software source-code status. The reading holds that access to software
 *   source code is an ethical imperative, that users have inalienable rights
 *   to inspect, modify, and redistribute software, and that proprietary
 *   licensing is categorically illegitimate. Under this reading, all
 *   proprietary software operates as a constraint that extracts user agency
 *   and freedom; users become victims rather than beneficiaries. The
 *   measuring station (this reading) sits in the Free Software Foundation /
 *   software-freedom movement tradition. This is NOT a neutral assessment: it
 *   is the position that proprietary software is an injustice, authored from
 *   that frame. The constraint story captures how that reading models the
 *   situation structurally — what it claims, who it identifies as suffering,
 *   what it identifies as unjust, and what tensions exist within the reading
 *   itself.
 *
 * KEY AGENTS:
 *   - Proprietary software publishers: institutional power, control enforcement, collect licensing monopoly rents
 *   - Software users (collective): organized power, identity-locked exit, structurally prevented from understanding or modifying their tools
 *   - Derivative developers: moderate power, constrained exit, creative agency restricted
 *   - Security researchers: moderate power, constrained exit, prevented from protecting users
 *   - Maintenance communities (Global South): powerless, trapped exit, inherit unmaintainable infrastructure
 *   - Free/open-source community: organized power, mobile exit, demonstrates alternatives
 *   - Excluded developers (Global South, localization contexts): moderate power, excluded from participation entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.88).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.76).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Software Source Code Restriction (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "intellectual_property/political_economy_of_technology").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, 'a568910b-5b9f-4c89-95c7-98992683a948').
narrative_ontology:cs_kernel_codification('a568910b-5b9f-4c89-95c7-98992683a948', distributed).
narrative_ontology:cs_authority_grounding('a568910b-5b9f-4c89-95c7-98992683a948', distributed).
narrative_ontology:cs_reading_relation('a568910b-5b9f-4c89-95c7-98992683a948', software_source_status__pragmatic_development_reading, forecloses).
narrative_ontology:cs_reading_relation('a568910b-5b9f-4c89-95c7-98992683a948', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('a568910b-5b9f-4c89-95c7-98992683a948', software_source_status__utilitarian_hybrid_reading, forecloses).
narrative_ontology:cs_axiom('a568910b-5b9f-4c89-95c7-98992683a948', foundational, source_code_access_inalienable_right).
narrative_ontology:cs_axiom_status(source_code_access_inalienable_right, holdable).
narrative_ontology:cs_axiom_grounding('a568910b-5b9f-4c89-95c7-98992683a948', source_code_access_inalienable_right, deontological).
narrative_ontology:cs_axiom('a568910b-5b9f-4c89-95c7-98992683a948', foundational, proprietary_closure_categorically_unjust).
narrative_ontology:cs_axiom_status(proprietary_closure_categorically_unjust, holdable).
narrative_ontology:cs_axiom_grounding('a568910b-5b9f-4c89-95c7-98992683a948', proprietary_closure_categorically_unjust, deontological).
narrative_ontology:cs_reference_frame('a568910b-5b9f-4c89-95c7-98992683a948', software_freedom_governance_framework).
narrative_ontology:cs_drift_state('a568910b-5b9f-4c89-95c7-98992683a948', contemporary_cloud_ai_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a568910b-5b9f-4c89-95c7-98992683a948', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, proprietary_software_publishers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, software_users_collective).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, derivative_developers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, security_researchers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, maintenance_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, free_and_open_source_community).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, hardware_manufacturers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, hardware_manufacturers).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, source_code_as_civic_infrastructure).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, user_autonomy_and_dignity).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, collective_software_stewardship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control and enforce licensing terms that restrict access to source code, modify rights, and redistribution freedoms. Justify restrictions as protecting business models, incentivizing investment, and maintaining quality control. Collect economic value from licensing fees and control over the software's evolution. Deploy legal and technical enforcement (DMCA, code signing, legal threats) to maintain source closure.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Cannot inspect, modify, repair, or understand the software they depend on for work, communication, and daily life. Are locked into versions and upgrade cycles controlled by publishers. Bear costs of security vulnerabilities they cannot patch themselves, compatibility locks, and inability to adapt software to their needs. Exit is constrained by network effects and digital dependency; many cannot realistically switch because their entire workflow is built on closed software stacks.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_users_collective, payer,
    organized, biographical, identity_locked, global).

% Cannot build upon, extend, or integrate closed software without license violations. Are barred from creating derived works or improvements. Must either pay for commercial licenses to access APIs (which may still restrict derivative use), reverse-engineer (legally risky), or avoid the software entirely. Their creative agency and ability to improve existing tools is constrained by licensing restrictions.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, derivative_developers, payer,
    moderate, biographical, constrained, global).

% Cannot audit closed software for vulnerabilities without the publisher's permission (which may be withheld or conditioned on non-disclosure). Discover critical flaws but are prohibited from disclosing them publicly or building fixes without violating the DMCA or license terms. Their ability to protect users from security threats is structurally constrained.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, security_researchers, payer,
    moderate, biographical, constrained, global).

% In the Global South and resource-constrained regions, cannot maintain or adapt software after publisher abandonment. A closed codebase for software used in schools, hospitals, or government becomes unsustainable waste when the publisher shifts business models. The community bears the cost of losing critical infrastructure it cannot repair or fork.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, maintenance_communities, payer,
    powerless, biographical, trapped, regional).

% Operates outside and against the proprietary model. Has built viable alternatives (Linux, Apache, Firefox, etc.) that demonstrate software can be produced and maintained through freedom-respecting models. Their existence proves the closure is not technically necessary. They are excluded from mainstream technology stacks through market dominance, not technical superiority.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_and_open_source_community, beneficiary,
    organized, generational, mobile, global).

% Increasingly investigate whether software source access is a public good or critical infrastructure question. Some jurisdictions (EU, France) have proposed or enacted requirements for source access in critical systems. Others defer to property-rights framing. They measure whether restriction regimes serve or harm public interests.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, regulatory_authorities_jurisdictions, observer,
    institutional, generational, analytical, national).

% Locked into closed firmware and boot ecosystems. Cannot modify or inspect code running on their own hardware without licensing from software publishers. Yet also depend on software closure to prevent user customization and preserve their own control of hardware lifespans and ecosystems. A dual position: payer where they lack autonomy over their own devices, beneficiary where they use closure to constrain user freedom.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, hardware_manufacturers, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, hardware_manufacturers, beneficiary).

% Those who would fork, redistribute, or build alternatives are legally barred from doing so. Software developers in the Global South who want to localize or adapt software for their own communities cannot without violating IP law. These are not participants in the market at all — they are excluded by design.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, excluded_developers, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, proprietary_software_publishers).
narrative_ontology:fixing_cost_class(software_source_status__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At the reading's own level (not at the kernel level): the freedom imperative reading does not frame this as a coordination problem to be solved. It frames source closure as an unjust constraint that prevents genuine coordination — the constraint's stated 'function' is coordination of development and quality, but the reading contests that the claimed function requires closure. The reading's analysis is: a genuine coordination function exists (how software gets made, maintained, improved), but closure is not the only way to achieve it, and closure carries the cost of unjust user subjugation.
% TRANSFER_FUNCTION: Transfers control, knowledge, and agency from users to publishers. Publishers gain: monopoly power over software evolution, ability to embed surveillance and lock-in, revenue from licensing, control over who can benefit from the software's labor. Users lose: ability to audit, repair, modify, or redistribute software; understanding of tools they depend on; capacity to adapt software to their own needs; freedom to fork and exit if dissatisfied.
% ABSENT_VOICES: Derivative developers globally (especially in resource-constrained regions) who would build on closed software but are legally barred. Security researchers who discover vulnerabilities but cannot disclose or patch without legal risk. Maintenance communities in the Global South that inherit unmaintainable closed systems. Software developers in countries without strong IP enforcement who would localize and adapt software but cannot under international licensing regimes. These voices are structurally excluded by the licensing mechanism itself.
% DISAPPEARANCE_RATIONALE: If source closure and its enforcement vanished overnight, the software ecosystem would reorganize around freedom-respecting models (as Linux, Apache, and thousands of open-source projects demonstrate it can). Publishers would lose the ability to enforce monopoly upgrades and licensing lock-in. Users would gain the ability to audit, fork, and maintain software. Derivative developers would create new applications from open codebases. Maintenance communities would repair and adapt software for local contexts. The arrangement persists only because legal and technical enforcement hold it in place.
% FOUNDING_PROBLEM: Early software was rare, expensive to develop, and needed capital investment to produce. The proprietary model was justified as necessary to recover development costs and incentivize innovation. The market scarcity and high development costs of the 1980s–1990s created a genuine coordination problem: how to fund software development and maintain quality.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation, major open-source foundations, academic research on open-source software development (from Linus Torvalds' git to collaborative Linux kernel development to modern AI models trained on open data) document that software can be produced at massive scale without source closure. Cloud computing and global internet infrastructure enable distributed development at zero marginal cost. Business models exist (services, support, customization) that fund development without source restriction. Economists and software engineers outside the proprietary sector attest the founding problem — capital scarcity — is solved. Publishers attest closure is still necessary for business models, but that is a claim about preferred profit structures, not about technical necessity.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88 at interval end) because the reading treats source closure as fundamentally extractive of user autonomy and freedom — it is not a coordination cost, it is the systematic prevention of coordination that respects human agency. The measurement series tracks a slow rise from 0.71 to 0.88 over the interval, reflecting the reading's historical observation that proprietary dominance (especially in cloud, mobile, and AI) has intensified since the 1980s, with more critical infrastructure locked behind closed licenses and stronger technical enforcement (DMCA, secure boot, DRM). Suppression rises from 0.62 to 0.76 because the enforcement machinery — legal threats, DMCA takedowns, code signing, anti-tampering measures — has grown more sophisticated and widespread. Theater ratio stays relatively low (0.28 to 0.42) because while publishers' security and quality arguments are real, the reading contends they are instrumentally secondary; the primary function of closure is to maintain monopoly power, not to achieve the stated coordination benefits. All measurements are on one shared time grid (every metric at every time point).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (users, derivative developers, researchers, maintenance communities) and the agenda-setter seat (publishers) should compute to radically different type classifications from the same structural data. From the publisher's seat, the arrangement is legitimate intellectual property protection and sustainable business model. From the user seat (the freedom reading's measuring station), the same structure is fundamental injustice. The divergence is NOT a measurement error; it is the core truth the reading asserts: that power asymmetry produces incommensurable evaluations of the same constraint. The engine computes this per-seat divergence from the structural data (beneficiary/victim, power, exit, scope). The authored claim does not adjudicate the divergence; the claim ASSERTS it.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary publishers are the structural beneficiaries (d near 0.0: they control the constraint, collect monopoly rents, have high power and mobile/arbitrage exit options). Software users collectively are near full targets (d near 1.0: they are identity-locked, have no alternative ecosystem at their desired scale, suffer direct costs in agency and freedom, organized but constrained by network effects and digital dependency). Derivative developers and security researchers are in the 0.7–0.85 range: they face severe constraints on their creative and protective agency, moderate power, but not completely trapped as users are. Maintenance communities in the Global South are near full targets (d near 1.0: powerless, trapped, inherit incomputable infrastructure). The free-open-source community sits near 0.0–0.15 (they are beneficiaries of the closure regime in the sense that it creates demand for alternatives; they have arbitrage-grade exit and organized power). The reading does not hide this: it names who benefits (publishers and their customers in high-income countries who value low purchase price over autonomy) and who pays (users globally who lose freedom, developers everywhere who cannot innovate).
 *
 * MANDATROPHY ANALYSIS:
 *   The freedom reading identifies a fundamental mandatrophy: the founding problem (capital scarcity for software development) is solved, but the constraint (source closure) persists and has intensified. This is not a Piton — the arrangement is not atrophying; it is actively enforced and extracting at full capacity. But it is a Snare whose mandate is dead: the justification (needed to fund development) no longer holds, yet the extraction machinery remains and strengthens. The reading does not invoke Piton because the arrangement is not theater — it is fully functional extraction riding on a dead mandate. The key is the gap between founding problem and current reality: if the founding problem were still live (capital scarcity were preventing software development), the arrangement might be legitimate coordination cost. But cloud computing, collaborative development, and successful open-source at scale prove the founding problem is solved. The constraint persists as pure rent collection justified by obsolete narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_death_or_contestation,
    'Is the founding problem (capital scarcity for software development) genuinely solved, or does the constraint''s justification remain contested by legitimate economic arguments?',
    'Economic analysis: What is the capital required to develop mission-critical software (OS, databases, AI systems) under open-source models vs. proprietary models? Do open-source projects systematically underfund maintenance and security? Is the apparent viability of open-source predicated on underpaid labor or volunteer time that would not be sustainable at scale?',
    'If the founding problem is solved, the constraint is a Snare with dead mandate and should be dismantled (freedom reading''s verdict). If capital constraints are real and open-source cannot sustain critical infrastructure, closure may remain a coordination cost (pragmatic or property readings'' position).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_death_or_contestation, empirical, 'Whether the founding mandate for source closure is obsolete or still live.').

omega_variable(
    identity_lock_mechanism_internalized_or_structural,
    'The measured suppression (0.76) treats user identity-lock as partially structural. Is the lock structural (users are genuinely unable to migrate due to network effects, market dominance, ecosystem lock-in) or partially internalized (users could migrate but have internalized the belief that proprietary software is inevitable/superior)?',
    'Post-exit suppression trajectory: if users who migrate to open-source systems report persistent felt constraints about proprietary tools, suppression is partially internalized. If constraints lift immediately after switching, suppression is primarily structural. Also: measure switching costs in jurisdictions with strong open-source uptake (e.g., some EU governments, Brazil, Russia post-sanctions) to isolate structural vs. internalized components.',
    'If primarily structural, the constraint''s suppression is correctly measured and reflects real exit barriers (markets are concentrated, switching costs are high). If partially internalized, the measured suppression underestimates how much of the constraint''s force is maintained by belief in inevitability rather than hard technical barriers — increasing the urgency of consciousness-raising and alternative-visibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalized_or_structural, empirical, 'Whether user suppression is structural (market dominance, lock-in) or internalized (belief in propriety inevitability).').

omega_variable(
    global_south_harm_or_heterogeneous_impact,
    'Does source closure harm the Global South uniformly, or does the impact differ across regions and economic contexts?',
    'Ethnographic and economic study of software dependency and repair capacity in different regions. How do maintenance communities in countries with strong pirate software ecosystems experience closure? Do they have de facto access through informal channels (reducing harm) or is closure enforced such that they inherit broken infrastructure (increasing harm)?',
    'If harm is uniform, the Global South case is clear evidence of victimhood (powerless, trapped seats bearing the full cost). If harm is heterogeneous (some regions have workarounds, others face total exclusion), the analysis must differentiate by region rather than treating the Global South as monolithic — some maintenance communities may have constrained but workable exit options (arbitrage through informal distribution), while others are genuinely trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_south_harm_or_heterogeneous_impact, empirical, 'Whether source closure harms all regions equally or creates differentiated victim sets.').

omega_variable(
    foreclosure_vs_coexistence_reading_contest,
    'Does the freedom reading logically foreclose the property-rights reading, or can both readings coexist as positions held by different parties?',
    'Examine whether a single framework (e.g., a jurisdiction''s legal system) could recognize BOTH user inalienable rights to source access AND creator property rights over software patents/business methods. If one framework can hold both (user gets source, creator gets patent protection on innovations), the readings coexist. If user source access necessarily extinguishes creator property claims, the readings foreclose each other.',
    'If forecloses: the readings cannot coexist in a single legal framework; one must win, the other must be suppressed. If coexists: different parties can maintain different readings simultaneously without logical contradiction, even though they conflict materially (e.g., GPL and proprietary software coexisting in the same ecosystem). This omega determines the cs_structure.reading_relations value between freedom_reading and property_rights_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_reading_contest, conceptual, 'Whether the freedom and property-rights readings logically foreclose each other or can coexist as different parties'' positions.').

omega_variable(
    surveillance_and_freedom_coupling,
    'Is source closure inherently coupled to surveillance risk, or is surveillance a separable threat that can be present in both open and closed software?',
    'Audit both open-source and proprietary software for surveillance vectors (telemetry, tracking, data exfiltration). The freedom reading often implies source closure enables hidden surveillance. If open-source code can contain surveillance vectors despite transparency, the threat is not unique to closure. If proprietary software exhibits systematic surveillance that open-source lacks, the coupling is real.',
    'If coupled: source access is necessary to detect and prevent surveillance; closure is doubly harmful (lack of agency + hidden tracking). If separable: both open and closed software require auditing; source access helps but is not sufficient to ensure privacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_and_freedom_coupling, empirical, 'Whether surveillance risk is inherently tied to source closure or a separable concern.').

omega_variable(
    reading_contingency_on_normative_framing,
    'Does the freedom reading''s claim that source access is an inalienable right rest on a particular normative framework (e.g., software-as-civic-infrastructure, autonomy-centered ethics, labor-dignity framing) that other readings reject?',
    'Examine how the freedom reading grounds its claim to inalienability. Is it grounded in: (a) a deontological principle (freedom is inherently valuable, users have intrinsic rights), (b) a consequentialist argument (source access produces better outcomes), (c) a labor/dignity argument (users have a right to understand and maintain what they depend on)? The property-rights reading grounds itself differently (creator property rights as foundational). If the two readings rest on incommensurable ethical foundations, they may be non-resolvable by evidence alone.',
    'If the freedom reading rests on contestable normative premises (even if internally sound), it cannot claim universal validity; it remains one reading among others. If the premises can be grounded empirically (e.g., autonomy is necessary for human flourishing, source access correlates with better outcomes), the reading gains force beyond normative preference.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contingency_on_normative_framing, conceptual, 'Whether the freedom reading''s foundational claims rest on contestable normative frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t7, software_source_status__freedom_imperative_reading, theater_ratio, 7, 0.31).
narrative_ontology:measurement_basis(soft_tr_t7, observed).
narrative_ontology:measurement(soft_tr_t14, software_source_status__freedom_imperative_reading, theater_ratio, 14, 0.34).
narrative_ontology:measurement_basis(soft_tr_t14, observed).
narrative_ontology:measurement(soft_tr_t21, software_source_status__freedom_imperative_reading, theater_ratio, 21, 0.37).
narrative_ontology:measurement_basis(soft_tr_t21, observed).
narrative_ontology:measurement(soft_tr_t28, software_source_status__freedom_imperative_reading, theater_ratio, 28, 0.4).
narrative_ontology:measurement_basis(soft_tr_t28, observed).
narrative_ontology:measurement(soft_tr_t35, software_source_status__freedom_imperative_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(soft_tr_t35, observed).
narrative_ontology:measurement(soft_tr_t42, software_source_status__freedom_imperative_reading, theater_ratio, 42, 0.42).
narrative_ontology:measurement_basis(soft_tr_t42, observed).
narrative_ontology:measurement(soft_tr_t50, software_source_status__freedom_imperative_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(soft_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t7, software_source_status__freedom_imperative_reading, base_extractiveness, 7, 0.74).
narrative_ontology:measurement_basis(soft_be_t7, observed).
narrative_ontology:measurement(soft_be_t14, software_source_status__freedom_imperative_reading, base_extractiveness, 14, 0.78).
narrative_ontology:measurement_basis(soft_be_t14, observed).
narrative_ontology:measurement(soft_be_t21, software_source_status__freedom_imperative_reading, base_extractiveness, 21, 0.82).
narrative_ontology:measurement_basis(soft_be_t21, observed).
narrative_ontology:measurement(soft_be_t28, software_source_status__freedom_imperative_reading, base_extractiveness, 28, 0.85).
narrative_ontology:measurement_basis(soft_be_t28, observed).
narrative_ontology:measurement(soft_be_t35, software_source_status__freedom_imperative_reading, base_extractiveness, 35, 0.87).
narrative_ontology:measurement_basis(soft_be_t35, observed).
narrative_ontology:measurement(soft_be_t42, software_source_status__freedom_imperative_reading, base_extractiveness, 42, 0.88).
narrative_ontology:measurement_basis(soft_be_t42, observed).
narrative_ontology:measurement(soft_be_t50, software_source_status__freedom_imperative_reading, base_extractiveness, 50, 0.88).
narrative_ontology:measurement_basis(soft_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t7, software_source_status__freedom_imperative_reading, suppression_requirement, 7, 0.65).
narrative_ontology:measurement_basis(soft_su_t7, observed).
narrative_ontology:measurement(soft_su_t14, software_source_status__freedom_imperative_reading, suppression_requirement, 14, 0.68).
narrative_ontology:measurement_basis(soft_su_t14, observed).
narrative_ontology:measurement(soft_su_t21, software_source_status__freedom_imperative_reading, suppression_requirement, 21, 0.71).
narrative_ontology:measurement_basis(soft_su_t21, observed).
narrative_ontology:measurement(soft_su_t28, software_source_status__freedom_imperative_reading, suppression_requirement, 28, 0.73).
narrative_ontology:measurement_basis(soft_su_t28, observed).
narrative_ontology:measurement(soft_su_t35, software_source_status__freedom_imperative_reading, suppression_requirement, 35, 0.75).
narrative_ontology:measurement_basis(soft_su_t35, observed).
narrative_ontology:measurement(soft_su_t42, software_source_status__freedom_imperative_reading, suppression_requirement, 42, 0.76).
narrative_ontology:measurement_basis(soft_su_t42, observed).
narrative_ontology:measurement(soft_su_t50, software_source_status__freedom_imperative_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement_basis(soft_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(software_source_status__freedom_imperative_reading, 0.18).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, digital_rights_management_enforcement).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, open_source_community_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel: software_source_status. Four structurally distinct readings exist, each with different ε values and stakeholder structures. The freedom_imperative_reading claims all proprietary software is extractive (high ε), while the property_rights_reading frames it as legitimate property protection (lower ε). The pragmatic_development_reading frames open source as superior methodology (instrumental framing of freedom). The utilitarian_hybrid_reading claims both serve different contexts (context-dependent ε). Each reading instantiates a different constraint with different beneficiary/victim sets. They are linked via the kernel, not fused into one constraint. The ε-invariance principle requires separate stories per reading because the observable that measures 'is proprietary software harmful?' yields radically different answers under each reading. The freedom reading measures 'user autonomy lost' (high ε), the property reading measures 'creator property rights respected' (low ε from creator seat), and the utilitarian reading measures 'welfare served by this license' (context-dependent ε). These are not the same constraint viewed from different angles; they are different constraints with different ε values instantiated from the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
