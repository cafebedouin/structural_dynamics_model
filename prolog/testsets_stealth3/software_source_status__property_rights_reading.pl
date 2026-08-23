% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__property_rights_reading, []).

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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Software as Intellectual Property - Property Rights Reading
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   This story instantiates the property_rights_reading of the
 *   software_source_status kernel. The standing arrangement under contest is
 *   the proprietary software licensing regime: source code held as a
 *   proprietary asset, license terms restricting access, modification, and
 *   redistribution, enforced through click-through contracts, activation and
 *   DRM machinery, audit programs, and copyright plus anti-circumvention law.
 *   By this reading's own lights, license fees are prices for products,
 *   restrictions are legitimate exercises of the creator's ownership, and
 *   enforcement is property protection, so the reading authors low extraction
 *   over this referent, conceding friction only where terms have drifted
 *   beyond classical property: subscription models over software users
 *   believed they had bought, DRM that breaks owned devices, and EULA clauses
 *   that strip statutory rights. The claim (rope: a coordination regime that
 *   funds software production by letting creators appropriate returns) and
 *   the metrics (a heavy enforcement apparatus meeting sustained resistance)
 *   are authored independently; the engine computes per-seat classifications
 *   from the structural data. The three sibling readings of the same kernel
 *   are separate constraint stories linked through
 *   cs_structure.reading_relations. KEY AGENTS (by structural relationship):
 *   - proprietary_software_vendors: agenda-setting rights-holder
 *   (institutional/arbitrage) - drafts and enforces the terms and collects
 *   the revenue - end_users: primary target (powerless/constrained) - pays
 *   and is bound by non-negotiable terms - enterprise_licensees:
 *   institutional target (powerful/constrained) - pays at scale, endures
 *   audits, cannot own what it runs - independent_software_creators:
 *   secondary beneficiary (moderate/mobile) - the regime makes selling indie
 *   software possible - independent_developers: dual-positioned
 *   (moderate/mobile) - barred from proprietary code they depend on,
 *   protected in code they write - free_software_movement: excluded
 *   counter-movement (organized/mobile) - rejects the framing itself; outside
 *   license drafting and legislation - copyright_legislatures_courts:
 *   co-agenda-setter (institutional/trapped) - enacts and adjudicates the
 *   framework - ip_policy_analysts: analytical observer
 *   (analytical/analytical) - studies the regime without stake
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.2).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.6).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Software as Intellectual Property - Property Rights Reading").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "economic/legal/technological").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, '4b1f11b4-9cb9-492f-a830-08526eec0db9').
narrative_ontology:cs_kernel_codification('4b1f11b4-9cb9-492f-a830-08526eec0db9', formalized).
narrative_ontology:cs_authority_grounding('4b1f11b4-9cb9-492f-a830-08526eec0db9', lineage).
narrative_ontology:cs_interpretation_layer_present('4b1f11b4-9cb9-492f-a830-08526eec0db9').
narrative_ontology:cs_reading_relation('4b1f11b4-9cb9-492f-a830-08526eec0db9', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('4b1f11b4-9cb9-492f-a830-08526eec0db9', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b1f11b4-9cb9-492f-a830-08526eec0db9', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('4b1f11b4-9cb9-492f-a830-08526eec0db9', foundational, creator_ownership_of_code_is_a_right).
narrative_ontology:cs_axiom_status(creator_ownership_of_code_is_a_right, holdable).
narrative_ontology:cs_axiom_grounding('4b1f11b4-9cb9-492f-a830-08526eec0db9', creator_ownership_of_code_is_a_right, deontological).
narrative_ontology:cs_axiom('4b1f11b4-9cb9-492f-a830-08526eec0db9', secondary, license_restrictions_bind_accepted_counterparties).
narrative_ontology:cs_axiom_status(license_restrictions_bind_accepted_counterparties, holdable).
narrative_ontology:cs_axiom_grounding('4b1f11b4-9cb9-492f-a830-08526eec0db9', license_restrictions_bind_accepted_counterparties, conventional).
narrative_ontology:cs_reference_frame('4b1f11b4-9cb9-492f-a830-08526eec0db9', creator_property_entitlement).
narrative_ontology:cs_drift_state('4b1f11b4-9cb9-492f-a830-08526eec0db9', post_open_source_infrastructure_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b1f11b4-9cb9-492f-a830-08526eec0db9', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, independent_software_creators).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, end_users).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, enterprise_licensees).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, independent_developers).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, copyright_incentive_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, lockean_labour_entitlement).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, freedom_of_contract_in_licensing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish and license software as a product: they draft the license and subscription terms, operate the activation, update, and DRM infrastructure that enforces them, run audit and legal programs against unlicensed use, and fund industry coalitions and legislative lobbying for stronger protection. License and subscription revenue is their primary income stream. If rules tighten or loosen in one jurisdiction they can shift corporate domicile, restructure offerings, or adopt dual-licensing.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Small studios and individual developers who sell their software under license. Copyright and enforceable licenses are what make charging for their work possible at all; they collect license revenue, pick terms project by project, and can move any given project to open terms whenever they choose.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_software_creators, beneficiary,
    moderate, biographical, mobile, global).

% Run software under terms they never negotiated: click-through licenses accepted at install, activation servers, mandatory updates. They pay license and subscription fees, may not modify, repair, or share what they run, and face real switching costs if they leave an ecosystem: retraining, data migration, and file compatibility with the people they work with.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, end_users, payer,
    powerless, biographical, constrained, global).

% Deploy software at scale under negotiated enterprise agreements. They pay substantial license and maintenance fees, absorb periodic audits and true-up bills, and staff vendor-management functions to stay compliant. Negotiation wins discounts and some terms but never ownership of the code; their operations are entangled with specific platforms, so leaving a vendor is a multi-year migration program.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, enterprise_licensees, payer,
    powerful, biographical, constrained, global).

% Build products, integrations, and tools around proprietary platforms. They cannot read, fix, or extend the proprietary code they depend on and interoperate only through vendor-controlled interfaces that can change or close. The same legal machinery protects the code they write themselves, which they can license commercially; they can take their own work open at any time.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_developers, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, independent_developers, beneficiary).

% Foundations, projects, copyleft license stewards, and digital-rights litigators who hold that software should be free to run, study, modify, and share, and who reject treating code as ordinary property. They are not in the rooms where licenses and copyright statutes are drafted; they act through their own licensing, test-case litigation, and public campaigns, and they run a complete parallel ecosystem that needs no permission from rights-holders.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, free_software_movement, excluded,
    organized, generational, mobile, global).

% Legislatures write the copyright and anti-circumvention statutes the licensing system stands on; courts decide whether license terms bind, whether reverse engineering is lawful, and what fair use allows. They respond to rights-holder lobbying and to periodic counter-pressure from technology firms and consumer advocates. They cannot decline the function: the framework applies to every dispute that reaches them.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, copyright_legislatures_courts, agenda_setter,
    institutional, generational, trapped, global).

% Economists, legal scholars, and policy researchers who study how software licensing regimes affect innovation, prices, security, and access. They publish empirical work on licensing economics and enforcement, testify in hearings, and hold no position in any licensing outcome.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, ip_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the funding problem of software production: copies of code are costless, so without a way to appropriate returns, production would be underfunded. Property rights and licensing let creators charge for use, standardize the terms under which code reaches users, and finance development, support, and security response. It also coordinates trust: users get a legally accountable vendor behind the code they run.
% TRANSFER_FUNCTION: Moves license and subscription fees, audit exposure, and compliance obligations from users and licensees to rights-holders, and moves control over modification, repair, and redistribution from users to rights-holders. Independent creators receive protection for their own work from the same machinery.
% ABSENT_VOICES: End users have no seat in license drafting or copyright legislation, which rights-holder coalitions dominate; the free software movement is absent from those rooms and acts through its own licenses and litigation instead; repair shops, security researchers, and archivists who would modify or interoperate with proprietary code are excluded and, under anti-circumvention rules, exposed to liability for the very acts they would object with.
% DISAPPEARANCE_RATIONALE: If software property and enforceable licensing vanished overnight, the commercial software economy would rearrange around services, support, hardware bundling, and patronage; license revenue would collapse and vendors would reprice or exit; the free software ecosystem would become the default rather than the alternative; and the audit, DRM, and anti-circumvention apparatus would lose its object. The rearrangement would be large and fast, which is what stakeholders at every seat looks like.
% FOUNDING_PROBLEM: In the 1970s and 1980s software detached from hardware as a standalone product while copying remained costless: creators faced uncompensated duplication of work that was expensive to produce. The founding problem was how to fund software production when a finished program can be copied by anyone at no cost.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the free software movement itself accepts that software production requires real funding and has built its own answers (support and service revenue, foundations, public grants), disputing only that property rights are the necessary solution; industry cost studies and developer labor statistics attest that development remains expensive; no serious party claims software production is costless. The contest is over the solution, not the problem.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).
:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.2 as a reading-indexed value over the standing arrangement: by the property reading's lights the regime is voluntary exchange, with friction only at the drift points where terms extend past classical ownership (perpetual licenses replaced by subscriptions, DRM on owned hardware, terms waiving statutory remedies). Suppression is authored as a raw structural property and is deliberately NOT reconciled to the reading's evaluation: the enforcement apparatus (anti-circumvention statutes, license audits, activation servers, litigation against reverse engineering) is real coercive machinery, and suppression is unscaled by power or scope - the engine owns any scaling of extractiveness, never suppression. Theater is moderate (0.3): enforcement tracks genuine copying, but a growing share of activity is rhetorical and performative (anti-piracy campaigns framing infringement as theft, DRM that inconveniences payers while being bypassed by infringers). Accessibility collapse is low (0.35): the free software and open source ecosystems are visible, lawful, and viable alternatives, so understanding the regime does not close off exits from it. Resistance is high (0.6): a forty-five year counter-movement (copyleft licensing, digital-rights litigation, right-to-repair campaigns) actively contests the regime. The measurement series run on one shared six-point grid (t = 0, 9, 18, 27, 36, 45 over a 1980-2025 interval) with all three tracked metrics authored at every point; the suppression series is the story's central dynamic, tracing the enforcement ratchet from early software copyright through the anti-circumvention era to subscription-terms enforcement, and all series end at their base_properties values.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seat and the payer seats should compute differently. From proprietary_software_vendors the arrangement is a voluntary exchange system they designed: terms are offered, counterparties accept, revenue follows, and enforcement defends the offer's integrity. From end_users and enterprise_licensees the same structure is non-negotiable adhesion: terms arrive attached to software the person or organization already depends on, audits arrive as unilateral exercises, and 'accepting' is the price of continued operation. enterprise_licensees sit between: their bargaining power wins discounts and negotiated terms but never ownership, so they experience the regime as costly but manageable. independent_software_creators and the vendor seat should compute similarly (both collect under the regime), while independent_developers straddle: protected for code they write, barred from code they depend on. The engine computes these per-seat classifications from power, exit options, and role; the divergence is the measurement, not something the authored claim resolves.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the low-d end: proprietary_software_vendors collect license and subscription revenue and write the terms, so the regime subsidizes their business model; independent_software_creators collect protection that makes commercial indie software viable. Victims map to the high-d end: end_users bear fees, restrictions, and lock-in with no negotiating seat; enterprise_licensees bear audit exposure and migration costs; independent_developers bear interoperability denial on the proprietary side while holding protection on their own side, which is why they carry a secondary beneficiary role and should derive an intermediate directionality. free_software_movement is authored as excluded rather than payer: the regime's terms do not bind their own code (they license under copyleft), but their normative position has no seat in license drafting or copyright legislation. copyright_legislatures_courts co-administer the framework and derive an agenda-setter position rather than a beneficiary position: they enforce and adjudicate but do not collect the revenue. Even under this reading's low-extraction assessment, the gains demonstrably accrue at the vendor seat, which is why gain_flow names it: receipt of gain is a structural fact about where revenue lands, independent of how legitimate the reading holds the transfer to be.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Treating the reading's rope claim as settled would erase what the structural data discloses to the payer seats: an enforcement ratchet (suppression series 0.25 to 0.60 over the interval) that has outgrown the copying it was built for. Treating the arrangement as pure extraction would erase the genuine funding-coordination function that independent_software_creators visibly rely on and that the founding problem still demands. The live mandatrophy question is whether the coordination function is atrophying while the legal machinery persists: the founding problem (funding software production when copies are free) remains live, but answers that bypass this arrangement are scaling (free software infrastructure, service and support revenue, patronage and public funding). If the incentive_necessity_question omega resolves toward 'not necessary,' the arrangement persists as maintained entitlement rather than needed coordination, and rising suppression against a shrinking proprietary-only production base is the drift signature. The kernel decomposition prevents the opposite error too: the freedom-imperative sibling authors high extraction over the same referent, and the difference between the two files is the reading index, not a disagreement about facts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the property_rights_reading of the software_source_status kernel; which structural features would re-author if a sibling reading (freedom_imperative, pragmatic_development, utilitarian_hybrid) were adopted over the same standing arrangement?',
    'Generate the sibling stories over the identical referent (the proprietary-licensing regime) and compare authored epsilon, victim sets, and per-seat classifications; the disagreement is located in whether restricting access and modification is a legitimate owner''s right or an injustice or instrumental tradeoff.',
    'Under the freedom-imperative reading the same referent re-authors with high extraction and users re-sit as rights-holders rather than contractual counterparties; under the pragmatic and utilitarian readings extraction moves toward the middle and the victim set narrows to contexts where licensing underperforms. This file''s low value is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which reading of the kernel this constraint instantiates and what sibling readings would change.').

omega_variable(
    voluntary_exchange_vs_lockin,
    'Are licensing transactions genuinely voluntary exchanges between owner and counterparty, as this reading holds, or structurally pressured by network effects and ecosystem lock-in that make refusal prohibitively costly?',
    'Measure switching costs, interoperability availability, and market concentration in categories where proprietary software dominates (desktop operating systems, office suites, enterprise resource planning); use natural experiments where mandated interoperability or public procurement shifts to open standards.',
    'If exchange is structurally pressured, the reading''s low extraction understates what users bear: the same license terms take more than a voluntary-exchange frame can register, pushing payer-seat classifications toward hybrid or extractive profiles the reading''s own seat cannot see.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_exchange_vs_lockin, empirical, 'Whether the licensing relationship is genuinely voluntary or coerced by lock-in structure.').

omega_variable(
    enforcement_scope_drift,
    'Does the enforcement machinery (anti-circumvention rules, license audits, litigation against reverse engineering) track actual copying of the creator''s work, or does it extend beyond property protection into controlling devices, blocking interoperability, and deterring competition?',
    'Compare anti-circumvention takedown and litigation records against classical copyright infringement: what share targets circumvention for repair, security research, or interoperability rather than duplication for redistribution?',
    'Enforcement beyond property protection is coercive structure serving the arrangement rather than the right it claims to protect; this is the mechanism by which the reading''s legitimate-enforcement evaluation and the payer seats'' lived experience diverge over the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_scope_drift, empirical, 'Whether enforcement scope tracks the property right or expands past it.').

omega_variable(
    incentive_necessity_question,
    'Is the property regime necessary to fund software production (the incentive justification the reading inherits from copyright doctrine), or have alternative production and funding models (free software, service revenue, patronage, public funding) demonstrated working answers that bypass it?',
    'Compare production output, quality, and funding across proprietary and free-software ecosystems over the interval; track the share of critical infrastructure running on non-proprietary code and the growth of service-based vendor revenue.',
    'If the incentive justification weakens, the arrangement''s funding-coordination function atrophies while the legal machinery persists; the founding problem acquires answers that do not need this arrangement, and it drifts toward maintenance of entitlement rather than funding of production.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_necessity_question, empirical, 'Whether the incentive justification for the regime still holds against demonstrated alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__property_rights_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t9, software_source_status__property_rights_reading, theater_ratio, 9, 0.14).
narrative_ontology:measurement_basis(soft_tr_t9, observed).
narrative_ontology:measurement(soft_tr_t18, software_source_status__property_rights_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement_basis(soft_tr_t18, observed).
narrative_ontology:measurement(soft_tr_t27, software_source_status__property_rights_reading, theater_ratio, 27, 0.26).
narrative_ontology:measurement_basis(soft_tr_t27, observed).
narrative_ontology:measurement(soft_tr_t36, software_source_status__property_rights_reading, theater_ratio, 36, 0.28).
narrative_ontology:measurement_basis(soft_tr_t36, observed).
narrative_ontology:measurement(soft_tr_t45, software_source_status__property_rights_reading, theater_ratio, 45, 0.3).
narrative_ontology:measurement_basis(soft_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__property_rights_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t9, software_source_status__property_rights_reading, base_extractiveness, 9, 0.1).
narrative_ontology:measurement_basis(soft_be_t9, observed).
narrative_ontology:measurement(soft_be_t18, software_source_status__property_rights_reading, base_extractiveness, 18, 0.14).
narrative_ontology:measurement_basis(soft_be_t18, observed).
narrative_ontology:measurement(soft_be_t27, software_source_status__property_rights_reading, base_extractiveness, 27, 0.16).
narrative_ontology:measurement_basis(soft_be_t27, observed).
narrative_ontology:measurement(soft_be_t36, software_source_status__property_rights_reading, base_extractiveness, 36, 0.18).
narrative_ontology:measurement_basis(soft_be_t36, observed).
narrative_ontology:measurement(soft_be_t45, software_source_status__property_rights_reading, base_extractiveness, 45, 0.2).
narrative_ontology:measurement_basis(soft_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__property_rights_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t9, software_source_status__property_rights_reading, suppression_requirement, 9, 0.35).
narrative_ontology:measurement_basis(soft_su_t9, observed).
narrative_ontology:measurement(soft_su_t18, software_source_status__property_rights_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(soft_su_t18, observed).
narrative_ontology:measurement(soft_su_t27, software_source_status__property_rights_reading, suppression_requirement, 27, 0.55).
narrative_ontology:measurement_basis(soft_su_t27, observed).
narrative_ontology:measurement(soft_su_t36, software_source_status__property_rights_reading, suppression_requirement, 36, 0.58).
narrative_ontology:measurement_basis(soft_su_t36, observed).
narrative_ontology:measurement(soft_su_t45, software_source_status__property_rights_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement_basis(soft_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_patent_regime).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, dmca_anticircumvention_regime).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, copyright_term_extension_regime).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, right_to_repair_reform).

% DUAL FORMULATION NOTE:
% The colloquial label 'the software IP debate' conflates structurally distinct commitments, so the kernel software_source_status decomposes into four reading-stories (this file plus the freedom-imperative, pragmatic-development, and utilitarian-hybrid readings), each with its own epsilon, victim set, and enforcement profile, linked through cs_structure.reading_relations rather than averaged into one story. This file additionally stands upstream of the neighboring legal-machinery constraints: the property reading is the legitimating frame for the anti-circumvention and term-extension regimes, which in turn harden this reading's enforcement base - contamination propagates in both directions across those edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
