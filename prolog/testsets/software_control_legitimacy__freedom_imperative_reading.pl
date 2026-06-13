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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software as Denial of User Control (Freedom Imperative Reading)
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the FREEDOM IMPERATIVE READING of the
 *   software control legitimacy kernel. Under this reading, proprietary
 *   software is categorically ethically illegitimate because it denies users
 *   fundamental control over their own computing. The reading treats
 *   computational autonomy as an irreducible right—the ability to inspect,
 *   understand, modify, and redistribute the code running on one's own
 *   machine—and defines proprietary licensing as a violation of that right.
 *   All proprietary software users enter the victim set. All users qua
 *   freedom-holders are the beneficiaries. This reading forecloses the
 *   property-rights reading (you cannot simultaneously hold that source code
 *   is the vendor's property and that users have a fundamental right to
 *   access and modify it within a single framework) and coexists with the
 *   pragmatic-openness and commons readings (which hold different legitimacy
 *   premises but do not directly contradict the core claim that users deserve
 *   computational autonomy). The extraction is high because the reading
 *   treats the denial of freedom itself as the primary harm, independent of
 *   secondary effects.
 *
 * KEY AGENTS:
 *   - software_users_as_freedom_holders: trapped powerless agents whose computing is mandatory for social participation yet whose freedom to control it is systematically denied
 *   - proprietary_software_vendors: institutional actors who administer the constraint by enforcing closed-source licensing, legal restrictions (DMCA, terms of service), and technical barriers to modification
 *   - dependent_developers: constrained-mobility actors whose livelihoods are bound to proprietary platforms they do not control
 *   - excluded_auditors_and_researchers: organized agents (security researchers, accessibility advocates, academics) blocked by law and contract from auditing critical systems
 *   - open_source_advocates: mobile-exit alternative that demonstrates the feasibility of user-controlled software but occupies a diminished market share due to network effects and institutional lock-in
 *   - freedom_imperative_philosophers: analytical observers who define and articulate what computational autonomy requires
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.82).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.71).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software as Denial of User Control (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, '318b377f-4f67-4eeb-9387-ef6475bce97c').
narrative_ontology:cs_kernel_codification('318b377f-4f67-4eeb-9387-ef6475bce97c', distributed).
narrative_ontology:cs_authority_grounding('318b377f-4f67-4eeb-9387-ef6475bce97c', extraction).
narrative_ontology:cs_reading_relation('318b377f-4f67-4eeb-9387-ef6475bce97c', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('318b377f-4f67-4eeb-9387-ef6475bce97c', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('318b377f-4f67-4eeb-9387-ef6475bce97c', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('318b377f-4f67-4eeb-9387-ef6475bce97c', foundational, computational_autonomy_fundamental_right).
narrative_ontology:cs_axiom_status(computational_autonomy_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('318b377f-4f67-4eeb-9387-ef6475bce97c', computational_autonomy_fundamental_right, deontological).
narrative_ontology:cs_axiom('318b377f-4f67-4eeb-9387-ef6475bce97c', foundational, user_control_incompatible_with_closed_source).
narrative_ontology:cs_axiom_status(user_control_incompatible_with_closed_source, holdable).
narrative_ontology:cs_axiom_grounding('318b377f-4f67-4eeb-9387-ef6475bce97c', user_control_incompatible_with_closed_source, deontological).
narrative_ontology:cs_reference_frame('318b377f-4f67-4eeb-9387-ef6475bce97c', user_computational_self_determination).
narrative_ontology:cs_drift_state('318b377f-4f67-4eeb-9387-ef6475bce97c', contemporary_surveillance_capitalism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('318b377f-4f67-4eeb-9387-ef6475bce97c', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, software_users_as_freedom_holders).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, dependent_developers).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, excluded_auditors_and_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, open_source_advocates_and_community).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, user_autonomy_is_fundamental_right).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, computational_self_determination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Humans whose computing is conducted through proprietary software. Under this reading, they are the rightful beneficiaries of a freedom imperative: they should possess the right to inspect, modify, and control the code running on their machines. They are trapped because computing infrastructure has become socially mandatory (finance, employment, healthcare, education) yet proprietary barriers prevent them from exercising control. The constraint names their deprivation and asserts their rights as the beneficiaries of freedom they are owed.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, software_users_as_freedom_holders, beneficiary,
    powerless, biographical, trapped, global).

% Corporations and software publishers who restrict access to source code and prevent modification/redistribution (Microsoft, Apple, Google, Adobe, Autodesk, etc.). Under this reading, they are the extractors: they collect economic rents from artificial scarcity, control user behavior through proprietary systems, and maintain dependency by enforcing legal and technical barriers to access. They set enforcement policy by controlling licensing terms and legal frameworks that criminalize reverse engineering and circumvention. They benefit from suppressing user autonomy and withholding information.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, agenda_setter).

% End users of proprietary software (Windows, macOS, iOS, Android systems; commercial cloud services; enterprise software like SAP, Salesforce). They bear the direct cost of the constraint: they cannot audit their own systems for security, cannot modify them for accessibility needs, cannot port their work to other systems, cannot understand what the software is doing with their data. Their identity lock is professional/institutional: careers, workflows, credentials, and professional standing are bound to specific proprietary platforms (Windows for office work, iOS for mobile). They are trapped by network effects and switching costs.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users, payer,
    powerless, biographical, identity_locked, global).

% Software developers whose livelihoods depend on building for proprietary platforms (iOS App Store, Windows ecosystem, Android, commercial game engines like Unity and Unreal). They are constrained: exit to open platforms is technically possible but economically costly in terms of market reach and revenue. They pay through mandatory licensing fees (developer agreements), restrictive terms of service, content moderation policies they do not control, and loss of autonomy over distribution and monetization of their own work.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, dependent_developers, payer,
    moderate, biographical, constrained, global).

% Security researchers, accessibility advocates, academic computer scientists, and civil-liberties technologists who are blocked from auditing critical infrastructure and consumer systems. They are excluded by law (DMCA, anti-circumvention provisions) and by terms-of-service restrictions. They would contribute to collective knowledge and security but are prevented. Their constraint is both economic (licensing restrictions) and legal (legal penalties for security research, jailbreaking, reverse engineering). They bear the cost in lost research opportunities and in harms from unaudited vulnerabilities.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, excluded_auditors_and_researchers, payer,
    organized, biographical, constrained, global).

% Communities building and maintaining free and open-source software (Linux, Mozilla, Apache Foundation, GNU project, countless independent projects and libraries). They represent the alternative that the freedom imperative reading asserts is ethically necessary. They operate on principles of user control, transparency, and collective governance. Their exit is mobile—they can fork projects, rebuild ecosystems, migrate to alternative governance models—but they occupy a smaller share of the computational landscape because proprietary software captures institutional lock-in and network effects.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, open_source_advocates_and_community, beneficiary,
    organized, generational, mobile, global).

% Thinkers in computer ethics, technology philosophy, critical code studies, and digital rights activism who analyze software control as a freedom question (Richard Stallman, Cory Doctorow, Yochai Benkler, and the Free Software Foundation). Their role is analytical and advocacy: they define the constraint, name its structure, articulate what user control requires, and mobilize discourse around computational autonomy as fundamental rights. They do not directly benefit or pay but frame the meaning and moral stakes of the arrangement.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, freedom_imperative_philosophers_and_advocates, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading rejects the framing that proprietary software solves a genuine coordination problem. Under this reading, the claimed coordination benefits (security review, quality assurance, unified UX) are post-hoc justifications for a control mechanism whose actual function is rent extraction and behavioral control. Any genuine coordination function (e.g., consistent standards for interoperability) could be served by open systems with public accountability.
% TRANSFER_FUNCTION: Transfers computational autonomy, behavioral control, and informational asymmetry from users to vendors. Users surrender the ability to inspect, modify, and control software; vendors gain the power to modify user behavior unilaterally, collect data, lock users into platform ecosystems, and extract economic rents through licensing. The transfer is masked as a consumer good but operates as a power seizure.
% ABSENT_VOICES: Users themselves are largely absent from the deliberation that creates and sustains this constraint—they are not asked whether they consent to closed systems. Open-source developers and alternative-platform builders would voice structural critique but occupy a marginal position in institutional decision-making. Downstream users (those in developing nations, those without resources to switch) are particularly silenced. Software workers in the Global South, whose labor underpins proprietary software supply chains but who lack corresponding access or autonomy, are excluded from the conversation.
% DISAPPEARANCE_RATIONALE: If proprietary software enforcement disappeared overnight—if source code became publicly available, modification became legal, and interoperability became mandatory—the software landscape would reorganize entirely. Vendors would lose control mechanisms but would retain their engineering capability; users would gain the capacity to audit, modify, and migrate their software; the software supply chain would shift from vendor-controlled distribution to peer-reviewed, community-governed models. The computational economy would rearrange toward distributed authority and user control. Within months, open-source alternatives would accelerate from marginal to primary role; professional ecosystems would diversify away from single-vendor lock-in.
% FOUNDING_PROBLEM: The constraint was not built to solve a problem; it was imposed to capture a market and consolidate power. Historically, proprietary software emerged as vendors realized they could charge for software as a commodity and legally prevent copying through copyright and trade-secret law. In the 1970s-80s, when hardware was expensive and software was abundant, vendors enclosed software as a scarce, controllable good. The founding narrative (that proprietary models enable investment in quality software) is a post-hoc justification created after the enclosure had already occurred. The actual founding event is the legal and technical move to enclose computational commons that were once open (Unix before fragmentation, early internet before platform concentration).
% FOUNDING_PROBLEM_CORROBORATION: The original problem claimed—that software quality requires proprietary incentives and that proprietary licensing is necessary to fund software development—has been thoroughly contradicted by empirical evidence from open-source development. Linux, Apache, Firefox, Kubernetes, Rust, LLVM, and millions of critical systems demonstrate that peer-reviewed, collectively-governed software achieves superior quality, security, and reliability without proprietary enclosure. Computer scientists, systems administrators, and security researchers who work with both open and proprietary code attest to the technical superiority of auditable systems. Major companies (Google, Amazon, Meta, Microsoft itself) now rely on open-source for core infrastructure and contribute substantially to it. Academic literature on software quality metrics shows no systematic advantage for proprietary development. The constraint persists not because it solves the founding problem but because vendors benefit from the dependency and behavioral control it creates.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).

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
 *   Extractiveness is high (0.82) and rising slowly over the interval because the constraint intensifies as computing becomes more central to all human activity—and as surveillance and behavioral control features deepen within proprietary ecosystems. Suppression is elevated (0.71) because the constraint depends on active legal enforcement (DMCA, copyright law, anti-circumvention provisions) and technical barriers (code signing, DRM, platform lock-in) to prevent users from circumventing restrictions. Theater ratio is moderate (0.28) because while vendors justify proprietary models as quality-ensuring or security-enabling, the actual function is increasingly visible as control and data extraction—the narrative cover is declining relative to the pure suppression machinery. Accessibility collapse is high (0.79) because once a user is embedded in a proprietary ecosystem (professionally, socially, through network effects), the barrier to exit is nearly absolute—they cannot port their work, their credentials, their social graphs to an alternative without substantial loss. Resistance is substantial (0.68) because open-source movements, security researchers, and civil-liberties technologists mount continuous challenge to the constraint, even though they lack the institutional power of vendors. The measurement series shows extractiveness plateauing around 0.82 by interval midpoint—the constraint has reached a near-stable state of maximal control enabled by ubiquitous computing, though it cannot rise further without forcing visible crisis (users organizing for alternatives, regulatory intervention).
 *
 * PERSPECTIVAL GAP:
 *   From the vendor seat (agenda_setter): the constraint is a legitimate property right and business model that funds quality software and enables innovation investment. From the user seat (payer): the constraint is experienced as illegitimate deprivation of control and entrapment in a system they do not understand and cannot modify. From the open-source community seat (beneficiary with mobile exit): the constraint is a political choice that could be revoked if social will shifted toward different models. The engine will compute these as different directionality values per seat because the structural relationships differ—vendors have power and mobile exit, users have neither; open-source has mobility but lacks institutional dominance. This divergence is the measurement point: the same structural arrangement generates radically different effective extraction depending on the seat you occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors are institutional actors with arbitrage-quality exit (they could pivot to open-source or other business models; the exit is constrained only by economic incentive, not by capability). They are agenda-setters, not targets of the constraint—they administer it. Their directionality is low (near beneficiary end, 0.1–0.2 range). Proprietary software users are powerless, identity-locked (professional credentials, network effects, workflow dependency bind them to proprietary platforms), and trapped in a mandatory computing infrastructure. Their directionality is high (near target end, 0.85–0.95 range). Open-source advocates and builders have organized power and mobile exit (they can fork, rebuild, create alternatives) but face strong suppression from network effects and institutional inertia. Their directionality is moderate (0.4–0.6 range, reflecting partial exit capability). The framework's directionality derivation should produce these asymmetries from beneficiary/victim declarations + exit options + power atom without override. If it does not, that divergence flags a measurement problem.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy—the founding problem it was built to solve (or rather, the founding condition it emerged to exploit) remains the driver of its persistence. Users still cannot audit their own systems; vendors still collect rents from that incapacity. The constraint serves its original purpose (extracting value from information asymmetry and locked-in dependency) so well that the question is not why it persists despite losing function but whether external forces (regulation, user mobilization, open-source alternatives reaching critical mass) can disrupt it. Mandatrophy would look like: vendors continuing to enforce proprietary licensing and DMCA provisions even after their business models no longer depended on it, purely from institutional inertia. We are not yet at that stage—the extraction is too profitable, the suppression too necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_right_vs_contingent_claim,
    'Is computational autonomy a fundamental human right grounded in inherent dignity, or is it a contingent claim specific to a historical moment (post-industrial computing culture) that may not generalize across contexts or futures?',
    'This is a conceptual/philosophical omega that cannot be resolved by empirical data alone. Resolution would require clarification of what grounds a right as ''fundamental'' in a given ethical framework—deontological (duties to respect autonomy), capabilities-based (access to self-determination as precondition for flourishing), or historical-materialist (autonomy claims emerge from specific configurations of technology and power). Different ethical traditions will reach different verdicts.',
    'If computational autonomy is fundamental, then proprietary software is categorically illegitimate and the constraint is pure extraction regardless of beneficial side effects. If it is contingent, then the claim loses its categorical force and the constraint could be justified by benefits (security, accessibility) that outweigh autonomy losses in specific contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fundamental_right_vs_contingent_claim, conceptual, 'Whether the freedom imperative grounds a universal right or a culturally-specific claim.').

omega_variable(
    identity_lock_persistence_mechanism,
    'Is the measured identity lock on proprietary software users structural (the technical ecosystem genuinely requires proprietary tooling for professional viability) or internalized (users have been socialized to believe proprietary platforms are the only legitimate option, even when technical alternatives exist)?',
    'Natural experiment via jurisdiction-level policy shifts: if mandatory open-source adoption (e.g., a government requiring open-source for all public systems) produces rapid user reskilling and ecosystem reorientation, identity lock was primarily internalized; if it produces technical crises and skill gaps, lock was structural. Alternatively, controlled migration studies where users attempt switching to open-source systems and report switching costs.',
    'If identity lock is structural, the constraint''s suppression is grounded in genuine technical dependency and cannot be eliminated without infrastructure change. If internalized, then breaking identity frames (retraining, cultural shift, regulatory mandates) could substantially reduce suppression without technical intervention. This affects the cost estimate for fixing the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_mechanism, empirical, 'Whether user lock-in to proprietary platforms is structural or internalized.').

omega_variable(
    vendor_coordination_function_separability,
    'Could genuine coordination functions (security review, quality assurance, interoperability standards) be provided through open-source governance and public accountability mechanisms, or are these functions structurally dependent on vendor proprietary control?',
    'Comparative analysis of open-source security and quality practices vs. proprietary ones; examination of Linux kernel security (community-audited, collective governance) vs. Windows security (vendor-controlled); study of how open-source projects achieve consensus on standards vs. how proprietary vendors impose them unilaterally.',
    'If functions are separable, the constraint''s claimed coordination justification is a cover story and the extractiveness is pure (no legitimate coordination cost to subtract). If inseparable, some portion of the measured extraction is necessary cost, not pure rent-seeking.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vendor_coordination_function_separability, empirical, 'Whether proprietary vendor control is structurally necessary for software quality and security coordination or merely one model among alternatives.').

omega_variable(
    network_effect_inevitability,
    'Are the network effects locking users into proprietary platforms (everyone uses Windows because everyone uses Windows; every organization uses Microsoft Office because it is the standard; developers target iOS because the App Store dominates) technological inevitabilities or contingent outcomes of market concentration that different policy choices could alter?',
    'Historical comparison across jurisdictions and time periods: did European regulatory pressure on Microsoft produce platform diversification? Did open-source alternatives (Linux) gain share when businesses faced cost pressures? Do jurisdictions with mandated open-source procurement shift their network effects? Do emerging economies without locked-in investment in Windows show different adoption patterns?',
    'If network effects are contingent outcomes of policy choices, then the constraint could be substantially disrupted through regulation, procurement policy, or interoperability mandates. If they are technological inevitabilities, the constraint is more stable and policy interventions may be insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_inevitability, empirical, 'Whether proprietary platform lock-in is a technological inevitability or a contingent outcome of market and policy choices.').

omega_variable(
    committer_reading_contest,
    'This reading treats software control as a matter of fundamental user rights. How does this contrast with the property_rights_reading, which treats software as the creator''s property, and how would each reading handle a scenario where user autonomy and creator incentives directly conflict?',
    'The resolution lies in deeper commitments about the source of property rights. The freedom imperative reading derives from commitments to human autonomy as foundational; the property rights reading derives from commitments to creator incentives and ownership as foundational. In a conflict, each reading prioritizes its foundational axiom. A framework that holds both axioms cannot coherently exist—this is a foreclosure relation. The engine computes this through the axioms and reading_relations fields.',
    'The foreclosure relationship means that as evidence accumulates about open-source viability (contradicting the property-rights axiom that proprietary incentives are necessary for software quality), the property-rights reading loses coherence within its own tradition and the freedom-imperative reading gains structural pressure. Conversely, if empirical evidence showed open-source models producing systematically worse security outcomes, the freedom imperative reading''s claim that user control is compatible with safety would come under pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_contest, conceptual, 'The fundamental axiom conflict between freedom imperative and property rights readings that prevents their coexistence in a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t6, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement_basis(soft_tr_t6, observed).
narrative_ontology:measurement(soft_tr_t12, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(soft_tr_t12, observed).
narrative_ontology:measurement(soft_tr_t18, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement_basis(soft_tr_t18, observed).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(soft_tr_t24, observed).
narrative_ontology:measurement(soft_tr_t32, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement_basis(soft_tr_t32, observed).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(soft_tr_t40, observed).
narrative_ontology:measurement(soft_tr_t50, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(soft_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t6, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 6, 0.71).
narrative_ontology:measurement_basis(soft_be_t6, observed).
narrative_ontology:measurement(soft_be_t12, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement_basis(soft_be_t12, observed).
narrative_ontology:measurement(soft_be_t18, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 18, 0.77).
narrative_ontology:measurement_basis(soft_be_t18, observed).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement_basis(soft_be_t24, observed).
narrative_ontology:measurement(soft_be_t32, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 32, 0.81).
narrative_ontology:measurement_basis(soft_be_t32, observed).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(soft_be_t40, observed).
narrative_ontology:measurement(soft_be_t50, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement_basis(soft_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t6, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement_basis(soft_su_t6, observed).
narrative_ontology:measurement(soft_su_t12, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement_basis(soft_su_t12, observed).
narrative_ontology:measurement(soft_su_t18, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement_basis(soft_su_t18, observed).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(soft_su_t24, observed).
narrative_ontology:measurement(soft_su_t32, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(soft_su_t32, observed).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(soft_su_t40, observed).
narrative_ontology:measurement(soft_su_t50, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(soft_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__freedom_imperative_reading, 0.18).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a four-story constraint family instantiating different readings of the software_control_legitimacy kernel. The family decomposes because the ε values differ dramatically across readings: the freedom imperative reading treats all proprietary software as categorical denial of rights (high ε, 0.82); the pragmatic openness reading treats proprietary as a legitimate business model with trade-offs (moderate ε, ~0.45); the property rights reading treats creator ownership as foundational (low ε, ~0.25); the commons reading treats software as requiring collective governance outside both individual freedom and individual property (moderate ε, ~0.50). These are not the same constraint viewed from different angles—they have different beneficiary/victim structures, different suppression mechanisms, and different founding narratives. The freedom imperative reading forecloses the property rights reading (the axioms are mutually exclusive). All readings affect each other through legitimacy erosion, policy spillovers, and institutional countermeasures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__freedom_imperative_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
