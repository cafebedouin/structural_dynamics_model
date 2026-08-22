% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Proprietary Software as Categorical Injustice (Freedom Imperative Reading)
 *   domain: political_economy_of_technology/intellectual_property/software_engineering
 *
 * SUMMARY:
 *   Under the freedom imperative reading, proprietary software is
 *   structurally an injustice: users have an inalienable right to source
 *   code, modification, and audit of the software running on their systems.
 *   This reading places all proprietary software in the victim/target frame
 *   and treats licensing restrictions as illegitimate constraints on human
 *   autonomy and computational justice. The constraint is CLAIMED as snare
 *   (extractive, coercive, victimizing) and the authored metrics describe a
 *   highly extractive, heavily suppressed arrangement with increasing
 *   enforcement costs relative to function. This reading is one of four
 *   competing frameworks of the contested kernel 'software source status';
 *   the others—pragmatic development, property rights, utilitarian hybrid—are
 *   distinct constraints with different ε values, victim sets, and authority
 *   groundings.
 *
 * KEY AGENTS:
 *   - Software corporations: institutional beneficiaries controlling source and licensing; extractive power over global software use
 *   - Software users (global majority): powerless victims trapped in proprietary dependency; no autonomy over computation
 *   - Derivative developers: moderate-power payers restricted from legal modification and interoperability
 *   - Global south populations: powerless victims locked out of software economy by cost and colonial infrastructure
 *   - Security researchers: moderate-power payers unable to audit or disclose vulnerabilities responsibly
 *   - Open source movements: organized beneficiaries operating outside/against the proprietary regime
 *   - IP regimes: non-agent beneficiary doctrine legitimizing the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.89).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.91).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Proprietary Software as Categorical Injustice (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "political_economy_of_technology/intellectual_property/software_engineering").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, 'c403f19b-6c01-4d87-989d-cb83eba1bfc3').
narrative_ontology:cs_kernel_codification('c403f19b-6c01-4d87-989d-cb83eba1bfc3', distributed).
narrative_ontology:cs_authority_grounding('c403f19b-6c01-4d87-989d-cb83eba1bfc3', distributed).
narrative_ontology:cs_reading_relation('c403f19b-6c01-4d87-989d-cb83eba1bfc3', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('c403f19b-6c01-4d87-989d-cb83eba1bfc3', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('c403f19b-6c01-4d87-989d-cb83eba1bfc3', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('c403f19b-6c01-4d87-989d-cb83eba1bfc3', foundational, software_freedom_inalienable_right).
narrative_ontology:cs_axiom_status(software_freedom_inalienable_right, holdable).
narrative_ontology:cs_axiom_grounding('c403f19b-6c01-4d87-989d-cb83eba1bfc3', software_freedom_inalienable_right, deontological).
narrative_ontology:cs_axiom('c403f19b-6c01-4d87-989d-cb83eba1bfc3', foundational, proprietary_licensing_categorical_injustice).
narrative_ontology:cs_axiom_status(proprietary_licensing_categorical_injustice, holdable).
narrative_ontology:cs_axiom_grounding('c403f19b-6c01-4d87-989d-cb83eba1bfc3', proprietary_licensing_categorical_injustice, deontological).
narrative_ontology:cs_reference_frame('c403f19b-6c01-4d87-989d-cb83eba1bfc3', universal_software_freedom_entitlement).
narrative_ontology:cs_drift_state('c403f19b-6c01-4d87-989d-cb83eba1bfc3', contemporary_proprietary_dominance, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('c403f19b-6c01-4d87-989d-cb83eba1bfc3', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, software_corporations).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, intellectual_property_regimes).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, derivative_developers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, marginalized_communities).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, global_south_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, open_source_movements).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, security_researchers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, open_source_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold proprietary control over source code and restrict user access through licensing agreements, Digital Rights Management, and legal frameworks. Justify restrictions as protecting intellectual property, funding development, and controlling quality. Benefit from monopoly control over software modification, pricing, and distribution. Can exit to open-source models at cost to brand/licensing revenue but retain institutional capacity to do so.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_corporations, agenda_setter,
    institutional, generational, arbitrage, global).

% Depend entirely on proprietary software for work, education, communication, and survival in digitized societies. Cannot inspect, modify, repair, or audit the code running on their machines. Cannot migrate to alternatives without severe disruption to workflow, compatibility, or social integration. Trapped by network effects, digital infrastructure dependency, and lack of viable free alternatives at scale. Bear the cost of hidden functionality, surveillance infrastructure embedded in code, and forced obsolescence.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_users, payer,
    powerless, biographical, trapped, global).

% Cannot legally inspect, modify, or extend proprietary software even for interoperability or security purposes. Face legal liability for reverse-engineering, circumventing DRM, or creating derivative works. Can only work with open-source alternatives when available, but face institutional and market pressure to build on proprietary platforms. Cannot contribute improvements back to the software ecosystem; innovations are locked in silos.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, derivative_developers, payer,
    moderate, biographical, constrained, global).

% Face compounded digital exclusion: proprietary software with no accessibility features, no localization in minority languages, and no pathway to modify software to suit local needs. Professional and educational gatekeeping through proprietary software monopolies (Microsoft Office in schools, Adobe Creative Suite in design fields) creates structural barriers to participation. Digital rights violation becomes inseparable from cultural erasure and economic exclusion.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, marginalized_communities, payer,
    powerless, biographical, identity_locked, global).

% Cannot afford licensing costs for proprietary software; infrastructure gaps make open-source alternatives difficult to deploy at scale; institutional frameworks (national health systems, schools, governments) locked into proprietary ecosystems through colonial-era technology transfer agreements. Pirated copies carry legal liability and zero warranty. Cannot modify software for local infrastructure constraints or participate in global software economy on equal terms.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, global_south_populations, payer,
    powerless, biographical, trapped, global).

% Cannot audit proprietary software for security vulnerabilities or backdoors. Discover vulnerabilities but must navigate legal ambiguity in responsible disclosure, facing threats of legal action under Computer Fraud and Abuse Act or DMCA anti-circumvention clauses. Knowledge of security flaws cannot be weaponized for improvement because source is inaccessible; must negotiate with uncooperative vendors or watch vulnerabilities persist.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, security_researchers, payer,
    moderate, biographical, constrained, global).

% Operate outside and against the proprietary regime; benefit from freedom to audit, modify, and redistribute software. Face institutional and market pressure to adopt proprietary tools, license compliance complexity, and corporate co-optation of open-source work (foundations, dual licensing). Benefit from the freedom imperative's moral framing but also bear costs of building alternatives to entrenched proprietary monopolies.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, open_source_movements, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, open_source_movements, payer).

% Non-agent: the legal and economic doctrine that software is property subject to copyright, patents, and licensing restrictions. Vindicated by proprietary software's existence and enforcement; constrains and legitimizes the constraint itself.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, intellectual_property_regimes, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(software_source_status__freedom_imperative_reading, intellectual_property_regimes).

% Analyze and document the injustice of proprietary software; build alternative infrastructure; articulate the freedom imperative. Take testimony from users, developers, security researchers. Can produce evidence that proprietary software violates user autonomy, but lack institutional power to mandate remedies.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, open_source_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, software_corporations).
narrative_ontology:fixing_cost_class(software_source_status__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading rejects any benign coordination framing. Under the freedom imperative, the arrangement coordinates nothing legitimate: it enforces a structure that denies users fundamental rights to control computation running on their machines. Any purported coordination benefits (quality assurance, security review, development funding) are presented as rationalization for rights violation, not as legitimate coordination problems.
% TRANSFER_FUNCTION: Transfers computational control, knowledge of system behavior, modification rights, and audit capacity from users and derivative developers to software corporations. Moves wealth from users and developing nations (through unaffordable licensing, piracy prosecution, and technology lock-in) to corporate shareholders and IP-owning elites. Transfers vulnerability knowledge from security researchers to a privileged set of vendor decision-makers who can choose to ignore it.
% ABSENT_VOICES: Software users in the global south are structurally excluded from decision-making about the tools that govern their work and survival. Subsistence-economy populations with no purchasing power for licenses have no voice in software governance despite bearing the heaviest costs. Marginalized users whose needs don't align with corporate profit motives (accessibility, minority languages, local infrastructure) are absent from product development. Future generations who depend on software preservation have no seat in current licensing decisions.
% DISAPPEARANCE_RATIONALE: If proprietary software licensing vanished overnight and all software became source-accessible, the software economy reorganizes: users gain the right and capacity to audit, modify, and repair their systems; derivative developers can legally build interoperable tools; security vulnerabilities can be crowdsourced for repair; marginalized communities can localize and adapt software to their needs; the global south can participate in software economy without colonial extraction. Software development continues but with redistributed control and knowledge.
% FOUNDING_PROBLEM: Software is computational knowledge embedded in machines; users have an ethical right to understand and control the computation running on their systems, to modify it for their needs, to audit it for their security, and to share improvements with their community. Proprietary licensing denies this right.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by the Free Software Foundation, Open Source advocates, security researchers who document the harms of source unavailability, and communities in the global south documenting exclusion from software governance. Corroborated by academic analysis of software freedom as a precondition for digital autonomy and justice. NOT corroborated by software corporations or IP-focused policy makers, who contest the framing itself.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is very high (0.89 endpoint) under this reading because proprietary software extracts computational control, knowledge, modification rights, and economic participation from billions of users and developers. The extraction is not compensated by the services delivered—quality assurance and security review are presented as reasons for rights denial, not legitimate trade-offs. Suppression is extremely high (0.91) because the constraint persists through legal threats (DMCA, Computer Fraud and Abuse Act), contract enforcement, digital locks, and institutional lock-in, all designed to prevent users from accessing and modifying software. Theater ratio is moderate (0.42): corporate marketing frames proprietary software as serving user interests, but the real functional performance is rights denial and extraction. The measurement series shows rising extractiveness and suppression over the 40-year interval, indicating deepening enforcement machinery, expanding proprietary control, and decreasing viable open-source alternatives at critical infrastructure scales. All metrics authored on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (users, developers, global south, security researchers) experience the constraint as categorical violation of their autonomy and access rights. They perceive no legitimate coordination function—only rights denial dressed in quality and security rhetoric. The beneficiary seats (corporations) experience proprietary control as legitimate property right and justified business model; they perceive the constraint as enabling innovation funding and quality control. The observer seats (open-source advocates, security researchers operating as analysts) document the structural injustice: that the stated benefits can be achieved through open governance, that alternatives exist, and that the persistence of proprietary models depends on legal suppression rather than functional superiority. The engine computes this divergence per-seat from the power, exit, and structural positions; it is not author-adjudicated.
 *
 * DIRECTIONALITY LOGIC:
 *   Software corporations sit at d ≈ 0.0 (full beneficiary): they set the rules, collect unrestricted control and pricing power, have arbitrage-level exit (can switch models), and operate at institutional power. Users sit at d ≈ 1.0 (full target): powerless position, trapped exit, no control over the rules, bear extraction. Global south populations sit at d ≈ 0.95+ (trapped targets): powerless + identity_locked (digital participation inseparable from access to licensed tools). Derivative developers sit at d ≈ 0.75 (substantial target): moderate power but constrained exit, bear legal and technical barriers. Security researchers sit at d ≈ 0.65 (target with some leverage): moderate power but constrained by legal threat, can sometimes negotiate with vendors. Open-source movements sit at d ≈ 0.2 (beneficiary of the freedom frame but payer through opportunity costs of building alternatives).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem under this reading—users' need for computational autonomy and rights—remains live and contested. The constraint persists not because the problem is solved but because proprietary enforcement machinery has grown more sophisticated. This is mandatrophy: the founding problem of proprietary software (secure, funded software development) may have been real in the 1980s, but it is now solved through open-source alternatives (Linux, Firefox, TensorFlow, etc.), yet proprietary licensing persists through legal coercion rather than functional necessity. The rising extraction and suppression metrics support the mandatrophy reading: the constraint requires escalating enforcement (DRM, licensing audits, legal threats) to persist because it no longer solves a genuine coordination problem for most users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_as_intrinsic_vs_instrumental,
    'Is software freedom an intrinsic human right (intrinsic dignity claim) or an instrumental precondition for autonomy and justice (consequentialist claim)?',
    'Philosophical analysis of normative grounding; empirical assessment of whether open-source software produces superior outcomes for users and societies; natural experiments from jurisdictions adopting open-source mandates.',
    'If intrinsic right: the constraint violates fundamental dignity regardless of functional outcomes; no amount of proprietary software quality or security justifies denial. If instrumental: the classification depends on empirical assessment of whether open-source actually delivers the promised autonomy and justice benefits; a purely symbolic freedom with no practical impact could be demoted as a value claim rather than structural injustice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_as_intrinsic_vs_instrumental, conceptual, 'Whether freedom is a non-negotiable right or an empirical precondition for justice.').

omega_variable(
    structural_necessity_vs_legal_enforcement,
    'How much of proprietary software''s persistence is due to genuine functional superiority or coordination benefits versus legal enforcement (DMCA, copyright, patent) and institutional lock-in?',
    'Counterfactual analysis: in jurisdictions with weakened IP enforcement (some development nations), what is the market share and quality profile of open-source alternatives? Does software quality correlate with proprietary vs. open-source ownership, or with community/institution size and resources?',
    'If persistence is structural (functional superiority): some victims may be misclassified; the extraction may be lower than authored. If persistence is enforcement-dependent (legal coercion is load-bearing): the suppression metric is correctly high and the constraint meets the snare definition. This distinction is critical to assessing mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_necessity_vs_legal_enforcement, empirical, 'Whether proprietary software''s dominance reflects genuine market preference or legal/institutional coercion.').

omega_variable(
    global_south_capability_and_exit,
    'Is the global south truly trapped in proprietary dependency, or do viable open-source alternatives exist at scale that could be adopted by institutional actors with resources?',
    'Deployment case studies of open-source at national scale (India''s National Informatics Centre, Brazil''s migration to LibreOffice, etc.); analysis of why institutional adoption of free software faces barriers (training, interoperability, legacy systems) versus why it should be structurally possible.',
    'If truly trapped: global south exit_options should be reclassified as more severely constrained; victim status is correct. If viable alternatives exist but face institutional barriers: exit is ''constrained'' rather than ''trapped''; the analysis of structural versus political suppression is refined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_south_capability_and_exit, empirical, 'Whether open-source alternatives are materially viable for institutional actors in the global south.').

omega_variable(
    reading_foreclosure_via_property_rights,
    'Does the property_rights_reading logically foreclose the freedom_imperative_reading, or do they coexist as incommensurable normative frameworks held by different parties?',
    'Philosophical analysis: can a party simultaneously hold that software is legitimate intellectual property (property_rights frame) AND that users have inalienable rights to source/modification (freedom_imperative frame)? Or are they logically contradictory such that a single commitment system must choose one?',
    'If foreclosed: the freedom and property readings cannot coexist within one legal/institutional framework; the constraint''s type classification depends on which reading dominates. If coexisting: they are held by different parties and represent a genuine institutional contest rather than logical incompatibility. This affects whether the engine computes reading_relations as ''forecloses'' or ''coexists_with''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_via_property_rights, conceptual, 'Whether property-rights and freedom-imperative readings are logically incompatible or just normatively opposed.').

omega_variable(
    supply_side_effect_on_development,
    'Would mandatory source-code publication eliminate the financial incentive for proprietary software development, or would alternative funding mechanisms (sponsorship, institutional subsidies, open-source models) sustain software quality and innovation?',
    'Historical comparison with successful open-source development (Linux kernel, Apache, TensorFlow); analysis of software development funding mechanisms across proprietary and open models; natural experiments from GPL-licensed software that achieved complex, performant outcomes.',
    'If development collapses without proprietary incentives: the freedom reading may violate a genuine coordination problem (funding complex software development); extraction may be justifiable cost of innovation. If viable development occurs at open-source scale: the proprietary model is not functionally necessary and the extraction is pure rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_side_effect_on_development, empirical, 'Whether proprietary licensing is necessary to fund software development or whether open-source mechanisms are sufficient.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(soft_tr_t8, software_source_status__freedom_imperative_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(soft_tr_t16, software_source_status__freedom_imperative_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(soft_tr_t24, software_source_status__freedom_imperative_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(soft_tr_t32, software_source_status__freedom_imperative_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(soft_tr_t40, software_source_status__freedom_imperative_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(soft_be_t8, software_source_status__freedom_imperative_reading, base_extractiveness, 8, 0.76).
narrative_ontology:measurement(soft_be_t16, software_source_status__freedom_imperative_reading, base_extractiveness, 16, 0.81).
narrative_ontology:measurement(soft_be_t24, software_source_status__freedom_imperative_reading, base_extractiveness, 24, 0.85).
narrative_ontology:measurement(soft_be_t32, software_source_status__freedom_imperative_reading, base_extractiveness, 32, 0.87).
narrative_ontology:measurement(soft_be_t40, software_source_status__freedom_imperative_reading, base_extractiveness, 40, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.84).
narrative_ontology:measurement(soft_su_t8, software_source_status__freedom_imperative_reading, suppression_requirement, 8, 0.86).
narrative_ontology:measurement(soft_su_t16, software_source_status__freedom_imperative_reading, suppression_requirement, 16, 0.88).
narrative_ontology:measurement(soft_su_t24, software_source_status__freedom_imperative_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(soft_su_t32, software_source_status__freedom_imperative_reading, suppression_requirement, 32, 0.9).
narrative_ontology:measurement(soft_su_t40, software_source_status__freedom_imperative_reading, suppression_requirement, 40, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(software_source_status__freedom_imperative_reading, 0.12).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% The software_source_status kernel decomposes into four distinct constraint stories, each with different ε values, beneficiary/victim sets, and authority groundings. This file (freedom_imperative_reading) treats all proprietary software as injustice and enters users/developers in the victim set. The pragmatic_development_reading focuses on methodology superiority without categorical freedom claims. The property_rights_reading treats software as legitimate intellectual property. The utilitarian_hybrid_reading maximizes aggregate welfare across models. Each reading is a coherent constraint with its own classification; they are linked here to represent the kernel contest, not to be merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__freedom_imperative_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
