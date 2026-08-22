% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software Control as Negotiated Commons Governance
 *   domain: political_economy/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   Software control under the commons reading is neither absolute freedom
 *   (which would eliminate all governance ability) nor absolute property
 *   (which would foreclose distributed participation). It is a negotiated
 *   arrangement through which developer communities, users, and institutions
 *   collectively govern shared digital infrastructure. Licensing requirements
 *   (GPL, MIT, Apache, BSD, etc.) encode governance rules: rights granted,
 *   conditions for use, attribution obligations, patent covenants. The
 *   constraint extracts from those who want unilateral control (proprietary
 *   firms, governments seeking digital sovereignty through enclosed systems)
 *   and from freedom absolutists who reject any negotiated restriction. It
 *   coordinates stakeholder communities around shared stewardship. The
 *   reading instantiates one specific constraint with one stable epsilon
 *   (0.58): the measured extractiveness of commons governance rules AS
 *   GOVERNANCE MECHANISM (not as libertarian ideal, not as property claim,
 *   but as practiced collective management). Sibling readings of the same
 *   kernel instantiate different ε values with different beneficiaries and
 *   victims.
 *
 * KEY AGENTS:
 *   - Developer communities (Agenda setter / Beneficiary) — write and maintain code, establish norms and licensing; power: organized; exit: mobile (can fork, start new projects); spatially global; they set the operational agenda
 *   - User collectives (Beneficiary) — depend on software, participate in governance through contributions and voice; power: organized; exit: constrained (vendor lock-in on many systems); spatial scope: global; they benefit from shared stewardship and retain inspection/modification rights
 *   - Public commons stakeholders (Beneficiary) — governments, institutions, non-profits that depend on shared infrastructure; power: moderate; exit: constrained (capacity limitations); spatial scope: global; they benefit from reduced cost and participation rights
 *   - Commercial open-source firms (Payer / Beneficiary) — build business models on commons software; power: powerful; exit: constrained (compete on licensing terms they did not unilaterally set); spatial scope: global; they pay compliance costs and governance participation they cannot fully control
 *   - Proprietary software advocates (Excluded) — claim exclusive property rights; power: powerful; exit: arbitrage (abandon commons, compete separately); spatial scope: global; excluded because property rights cannot coexist with commons governance
 *   - Freedom absolutists (Excluded) — insist all software must be completely free; power: moderate; exit: mobile (operate parallel communities); spatial scope: global; excluded because commons permits negotiated restrictions
 *   - Infrastructure maintainers (Agenda setter) — operate repositories, version control, licensing verification, CI/CD platforms; power: organized; exit: constrained (critical infrastructure); spatial scope: global; they enforce governance rules technically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.58).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.42).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Negotiated Commons Governance").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "political_economy/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, '0f18c660-cda5-43e5-9094-f04138330dff').
narrative_ontology:cs_kernel_codification('0f18c660-cda5-43e5-9094-f04138330dff', distributed).
narrative_ontology:cs_authority_grounding('0f18c660-cda5-43e5-9094-f04138330dff', practice).
narrative_ontology:cs_interpretation_layer_present('0f18c660-cda5-43e5-9094-f04138330dff').
narrative_ontology:cs_reading_relation('0f18c660-cda5-43e5-9094-f04138330dff', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f18c660-cda5-43e5-9094-f04138330dff', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('0f18c660-cda5-43e5-9094-f04138330dff', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('0f18c660-cda5-43e5-9094-f04138330dff', foundational, legitimate_control_is_negotiated_not_absolute).
narrative_ontology:cs_axiom_status(legitimate_control_is_negotiated_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('0f18c660-cda5-43e5-9094-f04138330dff', legitimate_control_is_negotiated_not_absolute, conventional).
narrative_ontology:cs_axiom('0f18c660-cda5-43e5-9094-f04138330dff', secondary, distributed_governance_enables_stewardship).
narrative_ontology:cs_axiom_status(distributed_governance_enables_stewardship, holdable).
narrative_ontology:cs_axiom_grounding('0f18c660-cda5-43e5-9094-f04138330dff', distributed_governance_enables_stewardship, instrumental).
narrative_ontology:cs_reference_frame('0f18c660-cda5-43e5-9094-f04138330dff', collaborative_governance_framework).
narrative_ontology:cs_drift_state('0f18c660-cda5-43e5-9094-f04138330dff', contemporary_platform_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f18c660-cda5-43e5-9094-f04138330dff', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, developer_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, user_collectives).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, public_commons_stakeholders).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, excluded_absolutist_voices).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, marginalized_development_models).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, commercial_open_source_firms).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, commercial_open_source_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and maintain code, establish norms and licensing regimes, enforce community standards through code review and social sanction. They set the operational agenda for what software can do and what rights users have. They participate in deliberative processes that decide licensing, patent covenants, forking rules, and governance structure. They benefit from distributed contribution, peer review, and shared stewardship responsibility. They move within the commons (can start new projects, fork, adopt different licenses) but are bound by the governance framework they collectively maintain.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, developer_communities, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, developer_communities, beneficiary).

% Depend on commons software for critical infrastructure, personal computing, and development environments. They participate in governance through bug reports, feature requests, community discussions, and voting in projects with democratic governance. They retain rights to inspect source code, modify it for their needs, and distribute modified versions (subject to licensing terms). They cannot unilaterally change governance rules they find objectionable but can exit to forked projects or proprietary alternatives at significant cost.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, user_collectives, beneficiary,
    organized, biographical, constrained, global).

% Public institutions (government agencies, public universities, public utilities), non-profits, and civil society organizations that depend on commons software for digital infrastructure they could not afford to build proprietary. They benefit from reduced licensing costs, reduced lock-in risk, ability to inspect and modify software for security and accessibility, and reduced dependence on commercial vendors. They are constrained by the governance choices made in commons communities and by capacity limitations that prevent meaningful participation in governance deliberation.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, public_commons_stakeholders, beneficiary,
    moderate, generational, constrained, global).

% Build business models on commons software (support services, proprietary extensions, hosting, managed services, consulting). They benefit from access to shared infrastructure, developer talent, and user communities they did not have to build. They pay through licensing restrictions that prevent pure proprietary relicensing, through community veto over attempts to re-enclose commons code, through governance participation costs (must justify business practices to communities), and through inability to unilaterally set terms. They cannot exit costlessly because their entire business model depends on commons legitimacy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, commercial_open_source_firms, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, commercial_open_source_firms, beneficiary).

% Argue that exclusive property rights in software are the legitimate ground for control and sustainable business models. They are excluded from commons governance because acceptance of collective decision-making would dissolve the property claim they depend on. They operate in separate markets (proprietary software, cloud platforms, software-as-a-service) and have built substantial value in those markets. They compete with commons software but do not participate in commons governance.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, proprietary_software_advocates, excluded,
    powerful, biographical, arbitrage, global).

% Insist that all software must be free in the sense of user freedom and control, rejecting any proprietary or restricted-use arrangement as categorically illegitimate. They are excluded from commons governance because the commons permits negotiated licensing restrictions (copyleft requirements, attribution obligations, non-commercial-use clauses in some projects) that do not guarantee complete freedom for all downstream users. They operate parallel communities with stricter freedom requirements (Free Software Foundation, GPL-only projects, Copyleft Alliance) and view the broader commons as insufficiently committed to absolute freedom.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, freedom_absolutist_advocates, excluded,
    moderate, biographical, mobile, global).

% Maintain critical shared digital infrastructure that enables commons governance: package repositories (npm, PyPI, Cargo), version control platforms (GitHub, GitLab), CI/CD systems, security scanners, and licensing verification tools. They set rules about what code can be hosted, how licensing is verified, what repositories can fork or mirror, and how dependencies are resolved. They operate the technical substrate through which governance rules are enforced. They are constrained by the need to maintain legitimacy with the communities that depend on their infrastructure and by potential government regulation of critical digital infrastructure.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, infrastructure_maintainers, agenda_setter,
    organized, generational, constrained, global).

% Evaluate commons governance in software for alignment with public interests: digital sovereignty, supply-chain security, strategic autonomy, data protection, and equitable access to digital infrastructure. They have capacity to mandate commons participation (open source requirements for public procurement, open data initiatives), restrict or regulate commons development (export controls on cryptography, sanctions on developer communities), or enforce alternative models (national software champions, state-controlled infrastructure, bans on certain licenses). They can observe and influence but do not directly participate in commons governance.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, national_governments, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__commons_reading, developer_communities).
narrative_ontology:fixing_cost_class(software_control_legitimacy__commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes shared rules for software stewardship that prevent total enclosure, permit distributed contribution, enable inspection and modification, and distribute governance authority among developer communities, users, and institutions. Solves the collective-action problem of maintaining and evolving digital infrastructure that multiple parties depend on without centralizing control in a single entity.
% TRANSFER_FUNCTION: Moves development labor, infrastructure maintenance work, and governance authority from proprietary holders to distributed communities. Moves rights to inspect, modify, and redistribute code from exclusive licensors to licensed communities. Moves decision-making power from markets and individual property claims to collective deliberation and commons rules.
% ABSENT_VOICES: Proprietary software advocates (excluded because property rights cannot coexist with commons governance) and freedom absolutists (excluded because commons permits negotiated restrictions). Users in developing nations who lack technical capacity to participate meaningfully in governance. Downstream software consumers who depend on commons choices made without their voice (transitive dependencies, supply-chain depth).
% DISAPPEARANCE_RATIONALE: If commons governance of software vanished, digital infrastructure would reorganize rapidly: critical systems currently maintained by volunteer communities would either be commercialized, nationalized, abandoned, or forked into proprietary variants. Interoperability standards negotiated through commons processes would fragment into competing proprietary platforms. The distributed stewardship model would collapse into centralized control by well-capitalized firms or governments.
% FOUNDING_PROBLEM: Early computing locked users and downstream developers out of control over software that affected them; monopolistic pricing, lock-in, security failures hidden from users, and inability to adapt software to local needs. The problem was formulated as: how can digital infrastructure be governed so that no single entity can unilaterally exclude others from participation, inspection, modification, and continued stewardship?
% FOUNDING_PROBLEM_CORROBORATION: Software supply-chain vulnerabilities (Log4Shell, xz-utils backdoor attempt, SolarWinds) demonstrate that centralized proprietary control fails to guarantee security. Cloud provider lock-in continues to constrain user autonomy. Digital colonialism (dominant firms extracting value from developing-nation data and labor with no local governance participation) is documented by development economists and technology justice advocates outside the commons movement. Government digital sovereignty policies cite these problems in mandating open-source requirements.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because commons governance DOES extract from certain positions: it denies property-rights absolutists unilateral control, it requires freedom absolutists to accept negotiated terms, it obligates commercial firms to accept community veto over certain business practices. But it is NOT as extractive as a pure snare because the beneficiaries (developer communities, user collectives, public institutions) genuinely participate in setting and enforcing the rules; they are not merely coerced. Suppression is moderate (0.42) because the constraint's persistence depends on active enforcement (licensing compliance verification, fork-capability maintenance, exclusion of proprietary lock-in mechanisms) but NOT on coercing participation — developers and users voluntarily choose commons projects in many contexts. The suppression measures enforcement of boundaries AGAINST enclosure, not enforcement of compliance WITHIN the system. Theater is low-moderate (0.31): most commons governance is genuinely functional (licensing serves real coordination and rights purposes), but some performative activity exists (sustainability theater when volunteer communities are exhausted, equity theater when governance remains actually dominated by large firms, meritocracy theater in communities with hidden power hierarchies). The measurement series tracks the interval from early open-source (lower enforcement burden, less theater) through maturity (stable extraction and suppression as governance institutions harden) — extractiveness and suppression rise slightly early (governance infrastructure develops), then plateau as the system reaches stability. Theater begins low and rises as the constraint matures: initially, software freedom was a functional political project; later, governance theater emerges as institutional maintenance becomes routinized and equity rhetoric increases relative to real participation.
 *
 * PERSPECTIVAL GAP:
 *   Developer communities (organized, mobile, global) will perceive this as rope: negotiated governance that enables their work and distributes stewardship. User collectives (organized but spatially and vendor-constrained) will perceive this as near-symmetric: real benefits from participation but real constraints from governance they cannot unilaterally alter. Commercial firms (powerful but exit-constrained by license compliance obligations) will perceive this as tangled_rope: genuine infrastructure benefits but significant extraction through licensing restrictions and community veto. Proprietary advocates (excluded) will perceive this as snare: their preferred model is forbidden and they are excluded from the decision-making. Freedom absolutists (excluded) will perceive this as snare: negotiated restrictions are imposed on them and they cannot achieve absolute freedom within the commons framework. These divergences are structural, not perspectival error — the same constraint generates different effective extractions for different seats because the constraint is ABOUT the allocation of control authority, which inherently affects seats differently based on what authority they wanted and whether they got it.
 *
 * DIRECTIONALITY LOGIC:
 *   Developer communities are near the beneficiary end of directionality (d ≈ 0.2–0.3): they set the rules, control the infrastructure, benefit from distributed contribution and shared stewardship. They experience relatively low effective extraction. User collectives sit near-symmetric (d ≈ 0.45–0.55): they benefit from the coordination (shared infrastructure, modification rights) but are constrained by the governance rules they participate in setting. Commercial open-source firms sit nearer the target end (d ≈ 0.6–0.7): they benefit from commons infrastructure but must accept licensing terms and community veto they cannot unilaterally override — the constraint's enforcement directly limits their business model freedom. Proprietary advocates and freedom absolutists sit at the far target end (d ≈ 0.85–0.95): the constraint directly denies them unilateral control and excludes them from participation. These directionality differences MUST produce per-seat type divergence: from a developer-community seat, commons governance is genuine rope (coordination benefiting all). From a proprietary-advocate or commercial-firm seat, the same structure reads as tangled_rope or snare (extraction enforced through licensing and community veto). The engine computes these divergences from the structural data; the authored claim (tangled_rope) reflects the overall structure from a relatively power-balanced analytical perspective that sees BOTH the genuine coordination AND the real extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (users locked out of software control, monopolistic pricing, lock-in, security failure) is live: supply-chain vulnerabilities continue, cloud lock-in persists, digital colonialism remains active. The commons governance reading addresses this problem by distributing control authority and enabling inspection/modification rights. The constraint's disappearance verdict (world_rearranges) aligns with founding_problem_status=live: if commons governance vanished, digital infrastructure would reorganize toward re-enclosure (proprietary platforms, national champions, fragmented interoperability), and the founding problem would re-materialize immediately. No mandatrophy signal: the constraint's function has not outlived its problem. However, theater_ratio elevation (0.18 → 0.31) over the interval suggests governance performativity is increasing: equity rhetoric without real power-sharing, sustainability theater masking volunteer exhaustion, meritocracy claims in communities with hidden hierarchies. This is a signal of potential future drift: if the genuine governance function decays while the theatrical maintenance increases, the constraint could transition from tangled_rope toward piton (governance machinery persists out of institutional inertia while actual collective stewardship atrophies). The constraint is currently live; the trajectory suggests vigilance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_function_decay,
    'Can commons governance of software remain genuinely participatory as infrastructure scales, or does governance inevitably concentrate in the hands of well-capitalized maintainers and infrastructure operators?',
    'Long-term observation of governance participation patterns in scaling commons projects (Linux, Apache, Node.js ecosystem); comparison of voting rights distribution, decision-making frequency, and participation rates at project inception versus maturity; case studies of governance collapse or power reconcentration.',
    'If governance genuinely decays into concentration, the constraint transitions from tangled_rope (asymmetric but participatory) toward piton (inertial theater) — the beneficiary/victim alignment shifts as developers become de facto agenda-setters rather than coordinators, and public commons stakeholders become passive dependents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_function_decay, empirical, 'Whether participatory governance can scale without reconcentrating power.').

omega_variable(
    commons_vs_individual_freedom_boundary,
    'Is there a logically coherent middle ground between ''all software must be free'' and ''all software can be proprietary,'' or do the two absolutist readings foreclose the commons reading within any single coherent framework?',
    'Formal analysis of the logical structure of freedom claims: can a commons governance framework acknowledge negotiated licensing restrictions AND user freedom simultaneously, or are these contradictory demands? Empirical: do practitioners experience the commons as coherent or constantly embattled between internal freedom-absolutist and property-rights factions?',
    'If the commons reading is logically foreclosed (forced to choose between absolutisms), then the constraint is not sustainable as a middle position — it collapses into one of the sibling readings. If coherent, the commons reading stands as a stable alternative. This is the core kernel question: does the commons reading instantiate genuine governance, or is it a temporary compromise destined to collapse?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_vs_individual_freedom_boundary, conceptual, 'Whether commons governance is logically coherent or foreclosed by absolutist boundary conditions.').

omega_variable(
    extraction_from_excluded_positions,
    'Who exactly is extracted from by commons governance, and is this extraction structural (necessary to the commons function) or contingent (a side effect of particular governance choices)?',
    'Detailed stakeholder analysis: proprietary firms can opt out entirely and build separate proprietary markets — are they extracted from or simply excluded? Freedom absolutists operate parallel communities (Free Software Foundation, Copyleft projects) — are they extracted from or excluded? If all extracted parties CAN exit, is it suppression-backed extraction or simply unfavorable terms they reject?',
    'If extraction is structural and unavoidable, commons governance is genuinely tangled_rope. If extraction is contingent and extractees can exit costlessly, it may be closer to rope with coordination benefits that some reject. The distinction determines whether suppression (enforcement against alternatives) is necessary or merely present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_from_excluded_positions, empirical, 'Whether commons-governance extraction is structural or an artifact of current institutional choices.').

omega_variable(
    sibling_reading_foreclosure_status,
    'Does the commons reading logically foreclose the property_rights and freedom_imperative readings (making them impossible to hold within the commons framework), or do they merely coexist as competing positions held by different parties?',
    'Formal logical analysis: if one accepts commons governance as THE legitimate framework for software control, must one reject property rights as a POSSIBLE legitimate framework? Or can one accept commons governance for public infrastructure while accepting property rights for commercial software? Empirically: do practitioners frame this as ''one framework is right'' or ''multiple frameworks are appropriate for different contexts''?',
    'If foreclosure is genuine (commons logic rules out property-rights logic in any single framework), the reading relations should include forecloses edges. If the readings merely coexist as contextual choices, they should be coexists_with. This determines the stability of the commons reading: does it compete or exclude?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'Whether commons governance logically forecloses or merely competes with absolutist readings.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.42) structural and external (licensing enforcement, fork barriers, patent covenants that require active policing) or internalized (developers internalize the norms of commons governance, no longer needing external enforcement)?',
    'Historical tracking of enforcement effort: if enforcement costs (legal action, licensing disputes, fork disputes, governance-rule violations) decline over time while suppression remains stable, suppression is increasingly internalized. Counterfactual: if enforcement infrastructure suddenly disappeared, how quickly would commons governance norms re-establish themselves?',
    'If suppression is structural, the constraint requires active enforcement to persist — it is contingent on the infrastructure maintainers and legal systems that police licensing. If internalized, the constraint persists through culture and norm-adoption even if external enforcement decays — it is more stable but also potentially more coercive (norms can be harder to resist than explicit rules). This affects piton risk: internalized suppression can mask theater and inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether commons-governance suppression is enforced externally or internalized as developer/user norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__commons_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__commons_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__commons_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__commons_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(soft_tr_t20, observed).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__commons_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(soft_tr_t25, observed).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__commons_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(soft_tr_t30, observed).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__commons_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(soft_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__commons_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__commons_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__commons_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__commons_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(soft_be_t20, observed).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__commons_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(soft_be_t25, observed).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__commons_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement_basis(soft_be_t30, observed).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__commons_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(soft_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__commons_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement_basis(soft_su_t5, observed).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__commons_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement_basis(soft_su_t10, observed).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__commons_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(soft_su_t15, observed).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__commons_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(soft_su_t20, observed).
narrative_ontology:measurement(soft_su_t25, software_control_legitimacy__commons_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(soft_su_t25, observed).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__commons_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(soft_su_t30, observed).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__commons_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(soft_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__commons_reading, 0.2).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).

% DUAL FORMULATION NOTE:
% The software_control_legitimacy kernel decomposes into four distinct constraints, one per reading. Each reading instantiates a different ε, different beneficiary/victim sets, and different type classifications because each reading defines 'legitimate control' differently. The commons reading (this file) argues control should be negotiated through collective governance (ε=0.58, tangled_rope). The freedom_imperative reading argues all software must be free in the liberty sense (makes proprietary arrangements the referent, ε high for property restrictions). The property_rights reading argues exclusive creator control is legitimate (makes commons restrictions the referent, ε high for governance mandates). The pragmatic_openness reading argues open source is methodologically superior but does not make legitimacy claims (ε lower, focuses on development outcomes). These are NOT the same constraint viewed from different angles — the ε referent and beneficiary structure differ fundamentally. Each reading forms a family member linked through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__commons_reading, organized, 0.25).
constraint_indexing:directionality_override(software_control_legitimacy__commons_reading, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
