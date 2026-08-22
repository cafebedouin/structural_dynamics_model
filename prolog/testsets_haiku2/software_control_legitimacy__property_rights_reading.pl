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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software as Property Right: Restricted Use and Distribution Authority
 *   domain: economic/intellectual_property/technology
 *
 * SUMMARY:
 *   This constraint embodies the reading that software control is
 *   fundamentally a matter of property rights: creators have legitimate
 *   authority to restrict use, modification, and distribution of their
 *   software to protect investment and enable sustainable commercial
 *   development. Under this reading, licensing restrictions, source-code
 *   secrecy, reverse-engineering prohibitions, and patent enforcement are
 *   justified tools for aligning creator incentive (profitability) with
 *   funder interest (ROI) and user interest (vendor stability and support).
 *   The constraint operates through copyright law, patent systems, end-user
 *   licensing agreements (EULAs), and legal enforcement against circumvention
 *   (DMCA). This is ONE reading of a contested kernel (kernel_id:
 *   software_control_legitimacy). Sibling readings frame the same domain as
 *   user-freedom imperatives (freedom_imperative_reading), pragmatic
 *   development-methodology choices (pragmatic_openness_reading), or
 *   commons-governance problems (commons_reading). Each reading instantiates
 *   a different constraint with different victim sets, beneficiary
 *   structures, and ε values. This story generates the constraint as the
 *   property-rights reading sees it: moderate extraction from those denied
 *   access/modification rights, substantial enforcement machinery targeting
 *   interoperability and reverse-engineering, moderate theater ratio as the
 *   'software innovation requires investment' justification increasingly
 *   competes with evidence of FOSS stability.
 *
 * KEY AGENTS:
 *   - commercial_software_vendors: beneficiary (institutional power, global scope, arbitrage exits) — set licensing terms and enforce restrictions; receive revenue
 *   - venture_capital_investors: beneficiary (powerful, generational horizon) — fund software under assumption IP protection enables exits
 *   - FOSS developers: payer (moderate power, constrained exit) — denied licensing-revenue opportunity because their model permits redistribution
 *   - security_researchers: payer (powerful, constrained by legal restrictions) — blocked from analyzing proprietary code; research slowed
 *   - interoperability_advocates: payer (organized, constrained) — cannot build compatible tools without licensing permission
 *   - end_users: dual-positioned (beneficiary from stable support, payer through license fees and lock-in)
 *   - patent_law_institutions: agenda_setter (institutional power, analytical exit) — administer the enforcement framework that makes restrictions legal
 *   - open standards bodies: observer (excluded from design leverage because proprietary vendors dominate relevant markets)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.58).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.62).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software as Property Right: Restricted Use and Distribution Authority").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "economic/intellectual_property/technology").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, '769d5294-4cfc-4037-a4bf-6620d1a1e5d1').
narrative_ontology:cs_kernel_codification('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', formalized).
narrative_ontology:cs_authority_grounding('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', extraction).
narrative_ontology:cs_interpretation_layer_present('769d5294-4cfc-4037-a4bf-6620d1a1e5d1').
narrative_ontology:cs_reading_relation('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', foundational, creator_investment_protection_justifies_restriction).
narrative_ontology:cs_axiom_status(creator_investment_protection_justifies_restriction, holdable).
narrative_ontology:cs_axiom_grounding('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', creator_investment_protection_justifies_restriction, instrumental).
narrative_ontology:cs_axiom('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', foundational, licensing_revenue_necessary_for_venture_sustainability).
narrative_ontology:cs_axiom_status(licensing_revenue_necessary_for_venture_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', licensing_revenue_necessary_for_venture_sustainability, empirically_contingent).
narrative_ontology:cs_reference_frame('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', creator_investment_recovery_imperative).
narrative_ontology:cs_drift_state('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', contemporary_open_source_maturity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('769d5294-4cfc-4037-a4bf-6620d1a1e5d1', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, commercial_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, venture_capital_investors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, patent_holders).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, security_researchers).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, interoperability_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, end_users).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, end_users).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, enterprise_customers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell proprietary software under restrictive licenses (EULAs, patent assertions). Receive licensing revenue, subscription fees, support contracts. Enforce licensing restrictions through legal mechanisms (DMCA, patent litigation). Justify restrictions as necessary to recoup R&D investment and sustain vendor support services over time.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, commercial_software_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Fund software startups under the assumption that intellectual property protection (patents, copyrights, trade secrets) creates defensible market positions enabling venture returns. Benefit from IP-created moats and exit events (acquisitions, IPOs) where IP portfolios command premium valuations. Lobby for expanded patent scope and copyright duration.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, venture_capital_investors, beneficiary,
    powerful, generational, arbitrage, global).

% Develop software under permissive (MIT, Apache) or copyleft (GPL) licenses that permit use, modification, and redistribution. Denied direct licensing revenue because their model explicitly rejects licensing restrictions. Must monetize through support contracts, consulting, hosted services, or embedded systems — alternative models that are inherently less scalable than vendor licensing. Face legal pressure when proprietary vendors claim patents cover their implementations.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_developers, payer,
    moderate, biographical, constrained, global).

% Study software security by analyzing code, testing implementations, and discovering vulnerabilities. Restricted by reverse-engineering prohibitions (DMCA § 1201), license terms forbidding disassembly, and trade-secret law. Cannot examine proprietary software without vendor permission, slowing vulnerability discovery. Face legal exposure if they circumvent access controls even for legitimate research purposes.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, security_researchers, payer,
    powerful, biographical, constrained, global).

% Seek open data formats, published APIs, and interface specifications enabling software from different vendors to interoperate. Blocked by proprietary APIs, closed file formats, and licensing restrictions on reverse-engineering that prevent learning the specs needed for compatibility. Cannot build compatible implementations without violating licensing terms or trade-secret law.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, interoperability_advocates, payer,
    organized, generational, constrained, global).

% Benefit from stable, professionally maintained software with vendor support services. Pay through perpetual/subscription licensing fees. Experience restricted use (cannot inspect code, modify for local needs, share with unlicensed colleagues, or switch to competing implementations easily). Locked in by proprietary file formats and API dependencies.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, end_users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, end_users, payer).

% Require source-code access, audit rights, and right-to-modify for compliance and security governance. Forced into expensive licensing modifications (source-code escrow, modification agreements) by vendors. Despite their volume and power, licensing restrictions prevent forking or employing alternative implementations if vendor support degrades. Constrained exit because switching costs (data migration, staff retraining) are prohibitive.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, enterprise_customers, payer,
    powerful, biographical, constrained, regional).

% Administer patent and copyright systems through which software control becomes legally enforceable. Define what 'software invention' qualifies for patenting, how long copyrights persist, what reverse-engineering and circumvention restrictions are permissible. Make enforcement decisions through litigation support and regulatory interpretation.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, patent_law_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Develop interoperability standards (TCP/IP, HTML, XML, etc.) by consensus among vendors and technical community. Often excluded from designing standards around proprietary software because requiring implementations to be open-source violates vendor licensing preferences. Role is constrained to standardizing on terms proprietary vendors find acceptable.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, open_standards_bodies, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__property_rights_reading, commercial_software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns creator incentives (profitable software development) with funder interests (venture capital ROI through exits) and user interests (professional vendor support, long-term maintenance, security updates) by enabling exclusive revenue through licensing restrictions. The restriction prevents competitors from free-riding on development investment, creating defensible market positions that justify sustained funding.
% TRANSFER_FUNCTION: Moves wealth from end users, enterprises, alternative developers (FOSS), and interoperability advocates to commercial software vendors and their investors through licensing fees, subscription renewals, support contracts, and through the structural denial of alternative business models (FOSS cannot monetize licensing). Also transfers opportunity from security researchers and interoperability advocates to vendors through legal restrictions on reverse-engineering and API access.
% ABSENT_VOICES: Users in resource-constrained regions (cannot afford licenses), downstream open-source projects and tools dependent on interoperability, researchers in jurisdictions with different IP preferences or enforcement capacity, future developers who might innovate on source code if accessible, and competing vendors who might enter markets if interoperability standards were open. These parties would argue for alternative property arrangements and are structurally excluded from the beneficiary set.
% DISAPPEARANCE_RATIONALE: If software property rights disappeared overnight, venture funding of closed-source software would sharply decline. Vendors would migrate to alternative revenue models (support/hosting fees, embedded systems, bundling with hardware). FOSS development and adoption would accelerate because the legal barriers to derivative works would lift. Security research would accelerate because reverse-engineering restrictions would vanish. Interoperability would increase because reimplementation would no longer risk legal liability. Markets would reorganize around network effects and reputation (as Linux, Apache, and Firefox ecosystems demonstrate) rather than IP-created scarcity.
% FOUNDING_PROBLEM: Early software markets (1970s-1980s) faced under-investment because software could be copied and redistributed costlessly once created, making it hard for developers to recoup development costs or for investors to see venture returns. This created a capital problem: complex software required upfront investment that only made sense if the creator could prevent others from copying the work.
% FOUNDING_PROBLEM_CORROBORATION: Commercial vendors and venture capital attest the problem is still live, citing ongoing R&D costs of $1-10B+ for enterprise software and the need for exclusive revenue to justify that investment. FOSS advocates and independent economic researchers (O'Reilly 2021, Linux Foundation surveys) attest the founding problem is substantially solved by alternative funding models: support contracts (Red Hat, Canonical), cloud hosting revenue (Amazon, Google), embedded systems (Android, Chrome), and community-driven development (Linux kernel, Apache Foundation). Evidence from competing jurisdictions with weaker IP enforcement (China, India) shows some innovation flourishes under different property regimes. Peer-reviewed economic research shows open-source development produces stable, secure software comparable to proprietary at lower cost of capital.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.58) rather than high because the constraint genuinely coordinates creator-funder-user interests (investment enables stability), not pure extraction. But it is measurably above zero because the reading excludes alternative coordination possibilities (FOSS models with support revenue, collective IP licensing) and creates lock-in beyond what investment recovery alone would justify. The measurement series shows modest growth from 0.38 to 0.58 over the interval: initial extractiveness was lower because the constraint competed with stronger FOSS alternatives and network-effects arguments. As commercial software vendors consolidated market power and successfully lobbied for extended IP protection (longer copyright terms, broader patent scope, DMCA enforcement), extraction increased. The trajectory plateaus at 0.58 because regulatory and public-opinion pressure around interoperability and security research began constraining further expansion. Suppression (0.62) is higher than extractiveness because the constraint's persistence requires active legal enforcement: reverse-engineering prohibitions, DMCA takedown actions, patent litigation. This enforcement target is not just source-code protection but the suppression of interoperability and derivative-work ecosystems. Theater ratio (0.28) is moderate-low because the 'software innovation requires investment' justification is real (R&D is capital-intensive) but increasingly ornamental: evidence of stable, well-funded FOSS ecosystems means the justification competes with alternative coordination stories. The engine computes per-seat type divergence from these metrics and structural data: vendors compute as ropish-to-tangled (coordination + asymmetric benefit); FOSS developers compute as snare-adjacent (restrictions target their model, not a necessary coordination function); security researchers compute as snare (restrictions actively harm their function). This divergence is exactly what the measurement apparatus exists to surface.
 *
 * PERSPECTIVAL GAP:
 *   The perspective divergence between beneficiaries and payers is substantial and intentional: this is exactly the seat divergence tangled_rope exhibits. From the vendor seat, the constraint is genuine coordination: it enables sustainable development funding by preventing competitors from copying their work, allowing them to invest in quality, security, and long-term maintenance. From the FOSS developer seat, the same constraint is opportunistic extraction: it denies them legitimate business models and creates legal barriers to their work. From the security-researcher seat, it is active obstruction: it slows the discovery and patching of vulnerabilities. From the end-user seat, it is a trade-off (professional support in exchange for restricted use and lock-in). The engine computes these different type-readings from the authored structural data (power, exit, beneficiary/victim, spatial scope). The claim/metric independence is deliberate: this story CLAIMS the constraint is tangled_rope (coordination + enforcement) while authoring metrics for moderate extraction + significant suppression. The engine determines whether the computed types align with or diverge from the claim; divergence is diagnostic, not error.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional treatment is asymmetric because the constraint's coordination and extraction functions fall on different agents. Commercial vendors and VC investors sit near d=0.0 (beneficiaries): the constraint subsidizes them by creating exclusive licensing revenue and defensible market positions that venture capital rewards with funding and exits. FOSS developers sit near d=1.0 (targets): the constraint specifically denies them a revenue model (licensing restrictions) and creates legal exposure (circumvention liability). Security researchers sit near d=0.8 (high targets): the constraint actively restricts their legitimate work (vulnerability research requires code access). Interoperability advocates sit near d=0.8: the constraint suppresses the standard-setting work they do. End users sit near d=0.5 (symmetric): they receive coordination benefit (stable vendor support, professional maintenance) but bear cost through licensing fees and lock-in, which roughly balances. Enterprise customers sit near d=0.65 (shifted toward target): despite their power, the constraint's restriction of modification rights and forced escrow agreements shift them toward extraction. Directionality was derived from the beneficiary/victim declarations and exit-options analysis; no override was needed because the structural positions map cleanly to the roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows early mandatrophy signals: the founding problem (insufficient investment in software development) was substantially addressed by the 1980s-1990s for established vendors, yet the constraint persists and expands. The measurement series shows extractiveness plateauing at 0.58 despite decades of enforcement investment; this suggests the constraint is increasingly maintained for monopoly rent rather than incentive provision. The theater ratio (0.28) is low enough to suggest the 'innovation requires investment' narrative is becoming detached from actual vendor behavior (which increasingly emphasizes lock-in and interoperability suppression over R&D investment). However, mandatrophy is not declared as resolved because: (1) the founding problem is contested (some argue venture-funded software development still requires IP protection; open-source proponents argue it doesn't); (2) the constraint is still actively enforced, not yet degraded to theatrical performance; (3) alternative revenue models (support, cloud services, embedded systems) are proliferating but have not yet made IP-based licensing structurally optional for most vendors. The constraint sits in the mandatrophy-warning zone: the founding problem's status is increasingly contested, enforcement costs are rising, and theater is increasing — these are early signals that the original justification is weakening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    investment_recovery_necessity,
    'Is restricting source code access and modification rights necessary to recover software development investment, or can comparable investment be sustained through alternative revenue models (services, support, cloud hosting, embedded systems)?',
    'Empirical comparison of investment levels and development stability between proprietary and open-source ecosystems across comparable software categories (databases, web servers, operating systems). Analysis of VC funding flows in open-source companies (Red Hat, Canonical, etc.).',
    'If investment can be sustained through alternative models, the property-right claim''s foundational justification weakens and the extracted benefit becomes harder to defend as necessary coordination cost. If investment genuinely requires licensing restrictions, the extraction is more defensible as incentive provision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investment_recovery_necessity, empirical, 'Whether licensing restrictions are structurally necessary for software development funding.').

omega_variable(
    property_right_vs_monopoly_extraction,
    'Does this reading anchor property protection in genuine scarcity (ideas that require labor to create) or in artificial scarcity created by law (copyright term length, patent scope)?',
    'Doctrinal analysis: compare the scope of protected rights (duration, derivative-work control, reverse-engineering prohibition) against labor required for original creation. Compare against historical justifications in patent/copyright law.',
    'If the reading relies on artificial-scarcity expansion beyond original-labor justification, the property framing becomes a cover story for monopoly extraction. The constraint reclassifies from tangled_rope (coordination + asymmetric extraction) toward snare (pure extraction masked as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_right_vs_monopoly_extraction, conceptual, 'Whether software control is property protection or monopoly rent-seeking masquerading as such.').

omega_variable(
    interoperability_suppression,
    'Is the suppression of interoperability (through proprietary APIs, closed file formats, license restrictions on reverse-engineering) a side effect of property protection or a primary target of control?',
    'Historical analysis of vendor behavior: do vendors actively prosecute interoperability attempts beyond what is necessary to protect source code? Are vendors'' pricing strategies aligned with the cost of development (suggesting genuine recovery) or with lock-in effects (suggesting extraction)?',
    'If interoperability suppression is a primary target, the constraint is not merely protecting investment but also creating market lock-in beyond what property rights alone would justify. Suppression metric is accurate; extraction includes monopoly rent beyond recovery incentives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interoperability_suppression, empirical, 'Whether property-right enforcement is the means or suppression of competition is the end.').

omega_variable(
    kernel_reading_contest,
    'Is software control fundamentally a property-rights question or fundamentally a commons-governance question or a user-freedom question?',
    'No empirical resolution — this is the kernel itself. Different frameworks privilege different foundational claims. The property-rights reading makes assumptions about what constitutes legitimate authority that the freedom_imperative and commons readings reject at the foundation.',
    'Classification depends on which kernel interpretation is accepted. The property-rights reading (this constraint) generates tangled_rope + moderate extraction. The freedom_imperative reading generates snare + high extraction (property restriction = oppression). The commons reading generates scaffold or rope + negotiated extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The foundational contest that cannot be resolved within a single reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__property_rights_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__property_rights_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__property_rights_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(soft_tr_t32, software_control_legitimacy__property_rights_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__property_rights_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__property_rights_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__property_rights_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__property_rights_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(soft_be_t32, software_control_legitimacy__property_rights_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__property_rights_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(soft_su_t8, software_control_legitimacy__property_rights_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(soft_su_t16, software_control_legitimacy__property_rights_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__property_rights_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(soft_su_t32, software_control_legitimacy__property_rights_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__property_rights_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__property_rights_reading, 0.18).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__commons_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_patent_scope_expansion).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, dmca_reverse_engineering_prohibition).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, api_interoperability_suppression).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel software_control_legitimacy. All four readings (property_rights, freedom_imperative, pragmatic_openness, commons) share the same domain but instantiate different constraints with different ε values, beneficiary/victim sets, and computed types. Link via network.affects_constraints to enable contention analysis across readings. The property_rights_reading (this story) generates tangled_rope + moderate extraction from vendor seats; the freedom_imperative_reading generates snare + high extraction from user seats. The pragmatic_openness_reading generates rope + negotiated benefits. The commons_reading generates scaffold or piton depending on governance maturity. Scholars and practitioners disagreeing about software control are not disagreeing about a single constraint viewed from different angles — they are instantiating genuinely different constraints through their reading choices. Each reading is ε-invariant within itself but produces different χ across seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__property_rights_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
