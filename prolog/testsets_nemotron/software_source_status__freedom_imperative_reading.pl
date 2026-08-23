% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Proprietary Software as Injustice (Freedom Imperative Reading)
 *   domain: technological/political/economic
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_imperative_reading of the
 *   software_source_status kernel. It treats the ethical claim 'proprietary
 *   software is an injustice' as a structural description of an extractive
 *   constraint: the proprietary licensing regime denies users the four
 *   freedoms (run, study, modify, share) and enforces this denial through
 *   copyright, contract, and technical measures. The reading identifies all
 *   proprietary software vendors, IP intermediaries, and platform gatekeepers
 *   as beneficiaries who extract rents from a captive user base comprising
 *   virtually all software users, independent developers, educational
 *   institutions, research organizations, and Global South tech ecosystems.
 *   The constraint is claimed as a snare — pure extraction with a
 *   coordination cover story (IP incentivizes innovation) that the reading
 *   rejects as empirically false and morally illegitimate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.82).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.78).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Proprietary Software as Injustice (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "technological/political/economic").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, 'c5deeda0-55f0-4435-9ea3-844aaaefeb7f').
narrative_ontology:cs_kernel_codification('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', fixed_text).
narrative_ontology:cs_authority_grounding('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', lineage).
narrative_ontology:cs_interpretation_layer_present('c5deeda0-55f0-4435-9ea3-844aaaefeb7f').
narrative_ontology:cs_reading_relation('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', foundational, software_freedom_as_inherent_right).
narrative_ontology:cs_axiom_status(software_freedom_as_inherent_right, holdable).
narrative_ontology:cs_axiom_grounding('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', software_freedom_as_inherent_right, deontological).
narrative_ontology:cs_axiom('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', foundational, proprietary_restriction_as_categorical_injustice).
narrative_ontology:cs_axiom_status(proprietary_restriction_as_categorical_injustice, holdable).
narrative_ontology:cs_axiom_grounding('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', proprietary_restriction_as_categorical_injustice, deontological).
narrative_ontology:cs_axiom('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', secondary, copyleft_as_ethical_obligation).
narrative_ontology:cs_axiom_status(copyleft_as_ethical_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', copyleft_as_ethical_obligation, deontological).
narrative_ontology:cs_reference_frame('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', pre_commercial_source_sharing_norm).
narrative_ontology:cs_drift_state('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', contemporary_cloud_ai_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c5deeda0-55f0-4435-9ea3-844aaaefeb7f', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, ip_licensing_intermediaries).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, platform_gatekeepers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, independent_developers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, educational_institutions).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, research_organizations).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, global_south_tech_ecosystems).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, software_freedom_as_inherent_right).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, source_access_as_moral_entitlement).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, copyleft_as_ethical_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and distribute software under restrictive licenses that deny users the freedoms to run, study, modify, and share. Enforce restrictions through copyright law, EULAs, and technical measures (DRM, product activation, cloud tethering). Extract monopoly rents from captive user bases and control the software supply chain. Their business model depends on maintaining artificial scarcity of copyable goods.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, proprietary_software_vendors, beneficiary).

% Patent assertion entities, licensing collectives, and compliance tool vendors who monetize the proprietary software regime. They do not produce software but extract rents from the enforcement layer — audits, settlements, compliance software, and legal threats. Their existence is parasitic on the restriction regime; they would have no role in a freedom-respecting ecosystem.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, ip_licensing_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% OS vendors, app store operators, and cloud platforms who leverage control over distribution channels to enforce proprietary terms. They take commissions (15-30%), mandate proprietary toolchains, and use technical measures to prevent sideloading or alternative distribution. Their gatekeeping power amplifies the extraction of upstream proprietary vendors.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, platform_gatekeepers, beneficiary,
    institutional, generational, arbitrage, global).

% Individuals and organizations who depend on proprietary software for work, communication, education, and civic participation. They cannot study how the software operates, cannot fix bugs or adapt it to their needs, cannot share it with others, and face vendor lock-in, forced upgrades, surveillance, and arbitrary termination of service. Exit requires abandoning accumulated data, workflows, skills, and network effects — often prohibitively costly.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_users, payer,
    powerless, biographical, constrained, global).

% Developers who want to build on, modify, or learn from existing proprietary software but are legally and technically barred. They face cease-and-desist threats for reverse engineering, cannot distribute derivative works, and must recreate functionality from scratch. The proprietary regime raises the cost of entry and diverts creative effort into duplicating restricted functionality.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Schools and universities forced to teach proprietary tools because industry demands them, paying license fees that divert public funds from education to vendors. Students learn vendor-specific workflows rather than transferable computational principles. Proprietary software in education normalizes restriction and prevents students from studying the systems they use.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, educational_institutions, payer,
    organized, generational, constrained, global).

% Scientific researchers blocked from inspecting, verifying, or modifying the computational tools their results depend on. Proprietary black boxes undermine reproducibility, prevent algorithmic auditing, and create single points of failure in the knowledge infrastructure. Grant money flows to license fees rather than open infrastructure.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, research_organizations, payer,
    organized, generational, constrained, global).

% Developing-world technology sectors denied the ability to adapt, localize, and build upon proprietary software due to cost, language barriers, and vendor neglect. They cannot fork software to add local language support, accessibility features, or hardware compatibility. The proprietary regime entrenches technological dependency and extracts wealth from the periphery to the center.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, global_south_tech_ecosystems, payer,
    powerless, generational, trapped, global).

% Activists, lawyers, and developers who organize around the ethical claim that software freedom is a prerequisite for a free society. They maintain the GNU project, defend copyleft licenses in court, build free replacements for proprietary systems, and articulate the moral framework that identifies proprietary software as injustice. They do not extract from the constraint; they oppose it.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_advocates, observer,
    organized, civilizational, analytical, global).

% Developers who contribute to open source for practical reasons (quality, collaboration, career advancement) but do not share the ethical commitment to user freedom. They often work for proprietary vendors, contribute to permissively licensed projects that enable proprietary forks, and oppose copyleft as 'viral.' Their voice is excluded from the freedom-imperative framing because they reject its foundational premise.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, pragmatic_open_source_developers, excluded,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The freedom-imperative reading identifies NO genuine coordination function served by proprietary restrictions. The arrangement solves no collective-action problem for users or developers; it creates artificial scarcity to enable extraction. Any coordination (interoperability, standards, security) is achieved DESPITE proprietary restrictions, not because of them.
% TRANSFER_FUNCTION: Moves control over computational artifacts from users to vendors; moves money from users (license fees, subscriptions, data, attention) to vendors and intermediaries; moves developmental capacity from the commons into private silos; moves political agency from the governed to the gatekeepers.
% ABSENT_VOICES: Future generations who will inherit the locked-down computational infrastructure; users in repressive regimes who cannot audit the software that surveils them; developers in the Global South who cannot afford proprietary toolchains; the pragmatic open source developers (excluded stakeholder) who would argue that freedom-talk hurts adoption.
% DISAPPEARANCE_RATIONALE: If the ethical imperative against proprietary software were universally recognized and enforced overnight, the entire software economy would reorganize around freedom-respecting models: copyleft would become the default, vendor lock-in would dissolve, users would gain full control over their computing, and the extractive intermediaries (patent trolls, compliance vendors, gatekeepers) would lose their reason to exist. The rearrangement would be total.
% FOUNDING_PROBLEM: The proprietary software regime emerged in the 1970s-80s when companies began restricting source code that had previously been shared, using copyright law to enforce artificial scarcity on copyable goods. The founding problem was not a coordination failure but a business model innovation: how to monetize software by denying users the freedoms that the medium naturally affords.
% FOUNDING_PROBLEM_CORROBORATION: The proprietary regime's own historians (e.g., Levy's 'Hackers,' Ceruzzi's 'A History of Modern Computing') document that source sharing was the norm before commercialization; the FSF's founding documents (GNU Manifesto, Stallman's 'Why Software Should Be Free') articulate the continuity between pre-commercial sharing and the freedom imperative. No independent scholar argues that proprietary restrictions solved a genuine coordination problem — the literature treats enclosure as a business model choice, not a technical necessity.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.82) is very high because the proprietary regime extracts multiple forms of value (money, data, control, developmental capacity) from a nearly universal user base while providing no genuine coordination benefit — the software would exist and function without restrictions. Suppression (0.78) is high because the regime actively deploys legal (copyright, patents, DMCA 1201, CFAA), technical (DRM, trusted computing, cloud tethering), and economic (network effects, switching costs) measures to prevent exit and suppress free alternatives. Theater ratio (0.12) is low because the enforcement is real and effective, not performative — the constraint delivers what it promises to its beneficiaries. Accessibility collapse (0.65) is moderate-high: free alternatives exist but are systematically disadvantaged by the proprietary regime's control of distribution, standards, and hardware. Resistance (0.72) is high: the free software movement, open source, right-to-repair, and regulatory pushback (DMA, CRA) represent sustained opposition.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute dramatically different effective extraction (χ) for each seat. For proprietary vendors (beneficiary, institutional power, arbitrage exit), χ will be negative (subsidy) — the constraint enriches them. For Global South ecosystems (victim, powerless, trapped), χ will approach the theoretical maximum — they bear the full weight of extraction with no exit. For software users in wealthy countries (victim, powerless, constrained), χ will be high but moderated slightly by the existence of some free alternatives. This seat divergence IS the structural reality: the same constraint is a mountain of subsidy for vendors and a snare of total extraction for trapped users.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation follows directly from the beneficiary/victim declarations and exit options. Proprietary vendors, IP intermediaries, and platform gatekeepers are declared beneficiaries with institutional/organized power and arbitrage/mobile exit — they sit at the beneficiary end (d ≈ 0.0-0.15). Software users, independent developers, educational institutions, research organizations, and Global South ecosystems are declared victims with powerless/moderate power and constrained/trapped exit — they sit at the target end (d ≈ 0.85-1.0). Free software advocates are observers (d = 0.5 by default). Pragmatic open source developers are excluded — they would object to the ethical framing but are not structurally positioned as payers or beneficiaries of the proprietary regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The proprietary regime's founding problem (monetizing software through artificial scarcity) is dead — the original business model has mutated into cloud rent extraction, data harvesting, and attention markets, but the restriction architecture persists. The constraint persists not because it solves a coordination problem but because the beneficiaries have captured the legal, technical, and economic infrastructure of enforcement. This is a classic snare: the coordination story (IP incentivizes innovation) is cover; the real function is extraction. The mandate (copyright as incentive) has atrophied into a pure extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (freedom_imperative_reading) of the contested kernel software_source_status. What structural elements distinguish this reading from its siblings, and where is the disagreement located?',
    'Map each sibling reading to its own constraint story with distinct beneficiary/victim sets, ε values, and claimed types. The disagreement is located in: (1) whether users have inalienable rights to source access (this reading: yes; property_rights: no; pragmatic: instrumental; utilitarian: welfare-dependent), (2) whether licensing restrictions are categorically illegitimate (this reading: yes; others: no), (3) whether proprietary vendors are beneficiaries or coordinators (this reading: extractive beneficiaries; property_rights: legitimate rights-holders; pragmatic: quality producers; utilitarian: welfare-optimizers).',
    'If the kernel framing is accepted, the four readings are four distinct constraints with different ε, different victim sets, and different classifications. If the kernel is rejected, each reading stands alone. The engine must treat them as separate constraints linked by network.affects_constraints, not as one constraint with variable ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: this constraint as one reading of a contested kernel').

omega_variable(
    proprietary_innovation_claim,
    'Does the proprietary regime actually produce innovation that would not occur under a freedom-respecting regime, or is the innovation claim a post-hoc justification for extraction?',
    'Compare innovation rates, security outcomes, and user welfare in domains dominated by proprietary vs. free software (e.g., operating systems, compilers, web infrastructure, AI/ML frameworks). Control for funding levels. Historical counterfactual: the internet, the web, and modern cloud infrastructure were built on free software.',
    'If proprietary restrictions are not necessary for innovation, the coordination cover story collapses and the constraint is a pure snare. If they are necessary in some domains, those domains might be tangled_rope (coordination + extraction) rather than snare — but this reading categorically rejects that possibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_innovation_claim, empirical, 'Whether the proprietary regime''s coordination justification has empirical support').

omega_variable(
    copyleft_enforcement_as_suppression,
    'From the property_rights_reading''s perspective, copyleft enforcement (GPL compliance) looks like suppression. Does this reading''s own enforcement mechanism constitute a reverse-snare?',
    'Analyze whether copyleft restrictions on proprietary relicensing operate as coordination (protecting the commons) or extraction (denying creators the right to choose their license). The freedom-imperative reading treats copyleft as defensive coordination; the property_rights_reading treats it as aggressive suppression. This is a genuine perspectival divergence.',
    'If copyleft is suppression, the freedom-imperative reading''s constraint story contains an internal contradiction: it denounces suppression while deploying it. This would complicate the snare classification and might produce a tangled_rope seat for proprietary developers who want to incorporate free code.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(copyleft_enforcement_as_suppression, conceptual, 'Whether the reading''s own remedial mechanism (copyleft) mirrors the suppression it condemns').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 1983, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ssf_fir_tr_t1983, software_source_status__freedom_imperative_reading, theater_ratio, 1983, 0.05).
narrative_ontology:measurement(ssf_fir_tr_t1990, software_source_status__freedom_imperative_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(ssf_fir_tr_t1998, software_source_status__freedom_imperative_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(ssf_fir_tr_t2005, software_source_status__freedom_imperative_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(ssf_fir_tr_t2012, software_source_status__freedom_imperative_reading, theater_ratio, 2012, 0.11).
narrative_ontology:measurement(ssf_fir_tr_t2018, software_source_status__freedom_imperative_reading, theater_ratio, 2018, 0.12).
narrative_ontology:measurement(ssf_fir_tr_t2025, software_source_status__freedom_imperative_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(ssf_fir_be_t1983, software_source_status__freedom_imperative_reading, base_extractiveness, 1983, 0.35).
narrative_ontology:measurement(ssf_fir_be_t1990, software_source_status__freedom_imperative_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(ssf_fir_be_t1998, software_source_status__freedom_imperative_reading, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(ssf_fir_be_t2005, software_source_status__freedom_imperative_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(ssf_fir_be_t2012, software_source_status__freedom_imperative_reading, base_extractiveness, 2012, 0.72).
narrative_ontology:measurement(ssf_fir_be_t2018, software_source_status__freedom_imperative_reading, base_extractiveness, 2018, 0.78).
narrative_ontology:measurement(ssf_fir_be_t2025, software_source_status__freedom_imperative_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(ssf_fir_su_t1983, software_source_status__freedom_imperative_reading, suppression_requirement, 1983, 0.4).
narrative_ontology:measurement(ssf_fir_su_t1990, software_source_status__freedom_imperative_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(ssf_fir_su_t1998, software_source_status__freedom_imperative_reading, suppression_requirement, 1998, 0.6).
narrative_ontology:measurement(ssf_fir_su_t2005, software_source_status__freedom_imperative_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(ssf_fir_su_t2012, software_source_status__freedom_imperative_reading, suppression_requirement, 2012, 0.72).
narrative_ontology:measurement(ssf_fir_su_t2018, software_source_status__freedom_imperative_reading, suppression_requirement, 2018, 0.75).
narrative_ontology:measurement(ssf_fir_su_t2025, software_source_status__freedom_imperative_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This is the freedom_imperative_reading of the software_source_status kernel. It differs from the pragmatic_development_reading in claiming freedom as inherent rather than instrumental; from the property_rights_reading in denying the legitimacy of software IP restrictions; from the utilitarian_hybrid_reading in rejecting welfare calculus as the basis for software governance. All four readings share the kernel 'software source code status' but instantiate different constraints with different ε, different victim sets, and different structural types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__freedom_imperative_reading, institutional, 0.05).
constraint_indexing:directionality_override(software_source_status__freedom_imperative_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
