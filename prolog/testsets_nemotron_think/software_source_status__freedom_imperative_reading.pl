% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Proprietary Software Licensing Regime (Freedom Imperative Reading)
 *   domain: economic/technological/political
 *
 * SUMMARY:
 *   The freedom imperative reading (FSF/Stallman position) evaluates the
 *   proprietary software licensing regime as a fundamental ethical violation.
 *   The regime uses copyright, patent, trade secret, and contract law to deny
 *   users the four essential freedoms: to run, study, modify, and share
 *   software. This reading asserts these freedoms are inalienable rights, not
 *   negotiable permissions. The constraint story models the proprietary
 *   licensing system as seen from this reading: a snare that extracts control
 *   and wealth from users while suppressing free alternatives through legal
 *   and technical enforcement. The claimed type (snare) diverges from the
 *   regime's self-presentation as a coordination mechanism (rope) — this
 *   divergence is the measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.88).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.82).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Proprietary Software Licensing Regime (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "economic/technological/political").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, '2650dda4-f950-453e-80f9-4802f844ed36').
narrative_ontology:cs_kernel_codification('2650dda4-f950-453e-80f9-4802f844ed36', formalized).
narrative_ontology:cs_authority_grounding('2650dda4-f950-453e-80f9-4802f844ed36', lineage).
narrative_ontology:cs_interpretation_layer_present('2650dda4-f950-453e-80f9-4802f844ed36').
narrative_ontology:cs_reading_relation('2650dda4-f950-453e-80f9-4802f844ed36', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('2650dda4-f950-453e-80f9-4802f844ed36', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('2650dda4-f950-453e-80f9-4802f844ed36', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('2650dda4-f950-453e-80f9-4802f844ed36', foundational, software_freedom_is_inalienable_right).
narrative_ontology:cs_axiom_status(software_freedom_is_inalienable_right, holdable).
narrative_ontology:cs_axiom_grounding('2650dda4-f950-453e-80f9-4802f844ed36', software_freedom_is_inalienable_right, deontological).
narrative_ontology:cs_axiom('2650dda4-f950-453e-80f9-4802f844ed36', foundational, proprietary_software_is_injustice).
narrative_ontology:cs_axiom_status(proprietary_software_is_injustice, holdable).
narrative_ontology:cs_axiom_grounding('2650dda4-f950-453e-80f9-4802f844ed36', proprietary_software_is_injustice, deontological).
narrative_ontology:cs_reference_frame('2650dda4-f950-453e-80f9-4802f844ed36', user_freedom_as_primary_axiom).
narrative_ontology:cs_drift_state('2650dda4-f950-453e-80f9-4802f844ed36', contemporary_proprietary_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2650dda4-f950-453e-80f9-4802f844ed36', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, ip_lawyers).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, license_compliance_industry).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, platform_gatekeepers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, independent_developers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, security_researchers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, educational_institutions).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, global_south_tech_ecosystems).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, software_freedom_as_human_right).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, user_autonomy_over_computation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control source code, distribution channels, and licensing terms for dominant software products. Extract monopoly rents through artificial scarcity of copyable goods. Enforce restrictions via copyright law, DRM, EULAs, and trade secrecy. Lobby for stronger IP enforcement internationally. Can pivot to services/support models but resist because licensing extracts more.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Build practices around licensing compliance, infringement litigation, patent prosecution, and trade secret protection. The proprietary regime creates continuous demand for legal services. Would lose a major practice area if software freedom became the norm, but skills transfer to other IP domains.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, ip_lawyers, beneficiary,
    organized, biographical, mobile, global).

% Sell audit tools, compliance consulting, and license management SaaS to enterprises terrified of BSA/FAST raids. Entire business model depends on proprietary licensing complexity and enforcement threat. Would need to pivot to open source compliance (smaller market) if regime collapsed.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, license_compliance_industry, beneficiary,
    organized, biographical, mobile, global).

% Control app stores, OS distribution, and hardware signing keys. Extract 15-30% commissions and enforce proprietary-only distribution policies. Use security/curation framing to justify gatekeeping. The proprietary software model is their revenue foundation; they actively suppress sideloading and alternative distribution.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, platform_gatekeepers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, platform_gatekeepers, agenda_setter).

% Depend on proprietary software for work, communication, finance, healthcare, and civic participation. Cannot inspect, modify, or share the software they rely on. Forced into upgrade cycles, subscription traps, and data extraction. Switching costs are high (retraining, data lock-in, network effects). Some migrate to free alternatives but face compatibility barriers.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_users, payer,
    organized, biographical, constrained, global).

% Cannot legally study, modify, or build upon proprietary libraries and platforms they depend on. Forced to work around artificial restrictions or pay licensing fees. Patent thickets and API copyright claims (e.g., Oracle v. Google) create litigation risk. Many contribute to free software but cannot sustain full-time work without proprietary income.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Cannot legally audit proprietary code for vulnerabilities (DMCA 1201, CFAA, EULA prohibitions). Must choose between responsible disclosure (often ignored or met with legal threats) and silence. Zero-day markets exploit this asymmetry. The constraint directly prevents them from doing their job of protecting users.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, security_researchers, payer,
    moderate, biographical, trapped, global).

% Forced to teach proprietary tools (Microsoft, Adobe, MATLAB, etc.) because industry demands those skills. Pay massive license fees that drain budgets from actual education. Cannot give students source code to study. Graduates enter workforce with tool dependency, not computational understanding.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, educational_institutions, payer,
    organized, generational, constrained, global).

% Proprietary licensing extracts wealth from developing economies to US/EU vendors. Local tech sectors cannot build on proprietary foundations. Software piracy criminalizes necessary access. Free software is the only viable path to technological sovereignty but faces institutional pressure to adopt proprietary 'standards'.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, global_south_tech_ecosystems, payer,
    moderate, generational, trapped, global).

% Articulate and campaign for the ethical claim that software freedom is an inalienable right. Build free alternatives (GNU, Linux, FSF, SFC, FSFE). Provide legal defense (GPL enforcement). Their constraint is the counter-norm they seek to impose: all software must respect user freedom.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_advocates, agenda_setter,
    organized, generational, analytical, global).

% Study the economic, social, and political effects of software licensing regimes. Compare innovation rates, security outcomes, and power distributions under proprietary vs. free models. Their analysis informs but does not determine the constraint's operation.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, policy_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The proprietary licensing regime claims to coordinate software production by granting temporary monopolies that incentivize investment in development. This is the 'IP incentivizes innovation' coordination story.
% TRANSFER_FUNCTION: Moves control over software (source access, modification rights, distribution rights) from users to vendors. Moves monetary payment (licenses, subscriptions, compliance costs) from users, developers, institutions to vendors, lawyers, and compliance industry. Moves legal risk onto researchers, educators, and independent developers.
% ABSENT_VOICES: Users in restrictive jurisdictions (where free software is discouraged or banned), workers whose labor is mediated by proprietary platforms they cannot audit, future generations who inherit locked-down computational infrastructure. They are absent because the regime defines them as 'licensees' not rights-holders.
% DISAPPEARANCE_RATIONALE: If the proprietary licensing regime vanished overnight, vendors would lose legal enforcement of artificial scarcity. Users would gain immediate rights to study, modify, and share all software. Vendors would pivot to services/support/freemium models. The software economy would reorganize around freedom-respecting models within months. Power would shift from gatekeepers to users and developers.
% FOUNDING_PROBLEM: Early commercial software (1970s-80s) faced rampant copying with no legal framework. Vendors sought copyright protection to prevent unauthorized redistribution and fund development. The proprietary model emerged as a pragmatic solution to fund software production in a market economy.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (funding software development) is solved by multiple demonstrated models: free software (Linux, GCC, Python, PostgreSQL), open core, SaaS, crowdfunding, public funding, corporate sponsorship. The proprietary licensing regime persists despite the founding problem being solved — confirmed by economic studies (e.g., Lerner & Tirole 2002, Nagle 2019) and the thriving free software ecosystem. The FSF and academic researchers outside the vendor beneficiary set corroborate this.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is very high (0.88) because the regime transfers nearly all control over software from users to vendors, converting copyable goods into artificial scarcity rent streams. Suppression is high (0.82) and rising: DMCA 1201, CFAA, EULAs, DRM, hardware signing, and trade agreements (TRIPS, ACTA, TPP) create overlapping enforcement layers. Theater ratio is low (0.12) because the extraction is real and acknowledged — vendors openly defend their right to restrict. Accessibility collapse (0.78) reflects that free alternatives exist but face massive structural barriers (network effects, hardware lock-in, patent threats). Resistance (0.71) is substantial: free software movement, open source, right-to-repair, security research exemptions, and global south adoption of free software.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor/gatekeeper seat, the regime is a rope (coordination via IP incentives). From the user/developer/researcher seats, it is a snare (extraction via artificial scarcity). From the free software advocate seat, it is a snare that must be abolished. The engine computes these per-seat classifications from the structural data — the freedom imperative reading's claim (snare) matches the computed type for the victim seats, not the beneficiary seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors, platform gatekeepers, IP lawyers, and compliance industry are structural beneficiaries (d near 0.0) — they collect rents and control the rules. Users, independent developers, security researchers, educators, and global south ecosystems are structural targets (d near 1.0) — they bear the costs with constrained or trapped exit. Free software advocates are agenda setters for the counter-constraint (d analytical). Policy analysts are pure observers. The derivation chain from beneficiary/victim declarations + power + exit produces this directionality gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (funding software development) is dead — multiple working funding models exist for free software. The regime persists because beneficiaries (vendors, gatekeepers, lawyers) extract sufficient rents to maintain enforcement, while victims face high coordination costs to dismantle it. This is classic mandatrophy: the mandate (IP incentivizes innovation) has outlived its function but the constraint persists through institutional inertia and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the freedom_imperative_reading a distinct constraint from its siblings, or a different evaluation of the same constraint?',
    'Apply ε-invariance test: if changing the reading changes the beneficiary/victim structure, extractiveness, or suppression of the proprietary licensing regime, they are distinct constraints. The freedom imperative reading puts ALL proprietary software in the victim set; pragmatic reading does not. This ε difference confirms distinct constraints.',
    'Confirms this JSON models one reading of a kernel, not a parameterized evaluation. Requires separate JSON files for each sibling reading linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether readings of the software_source_status kernel are distinct constraints per ε-invariance').

omega_variable(
    property_rights_foreclosure,
    'Does the freedom imperative reading''s core premise (software freedom as inalienable right) logically foreclose the property rights reading (creator''s right to restrict) within any single framework?',
    'Analyze whether a single legal/ethical framework can simultaneously hold ''users have inalienable right to source'' and ''creators have legitimate right to deny source''. If mutually exclusive, relation = forecloses. If different parties can hold each without contradiction, relation = coexists_with.',
    'Determines cs_structure.reading_relations entry for property_rights_reading. A forecloses relation means the kernel has a genuine logical fracture; coexists_with means political contestation without logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_rights_foreclosure, conceptual, 'Logical relationship between freedom imperative and property rights readings').

omega_variable(
    extraction_measurement_stability,
    'Does the extractiveness metric (0.88) remain stable across different operationalizations of ''proprietary software regime'' (e.g., desktop vs. SaaS vs. embedded)?',
    'Decompose the regime into sub-constraints (desktop licensing, SaaS terms, firmware locks, API copyright) and measure ε for each. If ε varies widely, the regime is a constraint family needing decomposition per ε-invariance.',
    'If ε varies, this story should be split into multiple constraint stories linked via network.affects_constraints. Current ε is a regime-level aggregate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_measurement_stability, empirical, 'Whether the proprietary licensing regime has a single stable ε or requires decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 1983, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1983, software_source_status__freedom_imperative_reading, theater_ratio, 1983, 0.05).
narrative_ontology:measurement(soft_tr_t1990, software_source_status__freedom_imperative_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(soft_tr_t1998, software_source_status__freedom_imperative_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(soft_tr_t2005, software_source_status__freedom_imperative_reading, theater_ratio, 2005, 0.11).
narrative_ontology:measurement(soft_tr_t2012, software_source_status__freedom_imperative_reading, theater_ratio, 2012, 0.11).
narrative_ontology:measurement(soft_tr_t2018, software_source_status__freedom_imperative_reading, theater_ratio, 2018, 0.12).
narrative_ontology:measurement(soft_tr_t2025, software_source_status__freedom_imperative_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(soft_be_t1983, software_source_status__freedom_imperative_reading, base_extractiveness, 1983, 0.45).
narrative_ontology:measurement(soft_be_t1990, software_source_status__freedom_imperative_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(soft_be_t1998, software_source_status__freedom_imperative_reading, base_extractiveness, 1998, 0.65).
narrative_ontology:measurement(soft_be_t2005, software_source_status__freedom_imperative_reading, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement(soft_be_t2012, software_source_status__freedom_imperative_reading, base_extractiveness, 2012, 0.78).
narrative_ontology:measurement(soft_be_t2018, software_source_status__freedom_imperative_reading, base_extractiveness, 2018, 0.83).
narrative_ontology:measurement(soft_be_t2025, software_source_status__freedom_imperative_reading, base_extractiveness, 2025, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1983, software_source_status__freedom_imperative_reading, suppression_requirement, 1983, 0.35).
narrative_ontology:measurement(soft_su_t1990, software_source_status__freedom_imperative_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(soft_su_t1998, software_source_status__freedom_imperative_reading, suppression_requirement, 1998, 0.65).
narrative_ontology:measurement(soft_su_t2005, software_source_status__freedom_imperative_reading, suppression_requirement, 2005, 0.72).
narrative_ontology:measurement(soft_su_t2012, software_source_status__freedom_imperative_reading, suppression_requirement, 2012, 0.77).
narrative_ontology:measurement(soft_su_t2018, software_source_status__freedom_imperative_reading, suppression_requirement, 2018, 0.8).
narrative_ontology:measurement(soft_su_t2025, software_source_status__freedom_imperative_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(software_source_status__freedom_imperative_reading, 0.1).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, dmca_1201_anti_circumvention).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_patent_regime).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, right_to_repair_legislation).

% DUAL FORMULATION NOTE:
% This is the freedom_imperative_reading of the software_source_status kernel. The kernel decomposes into four constraint stories (this + three siblings) with different ε, beneficiary/victim structures, and claimed types. Linked via affects_constraints. The dual formulation is: the same kernel label ('software freedom') covers structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__freedom_imperative_reading, institutional, 0.1).
constraint_indexing:directionality_override(software_source_status__freedom_imperative_reading, organized, 0.85).
constraint_indexing:directionality_override(software_source_status__freedom_imperative_reading, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
