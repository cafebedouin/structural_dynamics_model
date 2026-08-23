% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Software Freedom Imperative (FSF/GNU reading)
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   The freedom imperative reading (originating in Stallman's GNU Manifesto
 *   and the Four Freedoms) asserts that proprietary software — software that
 *   denies users the freedom to run, study, modify, and share — is ethically
 *   illegitimate. This reading instantiates a constraint: all software must
 *   respect user freedom. From this reading's perspective, the standing
 *   arrangement (proprietary dominance) extracts from users by denying them
 *   control over their computing. The constraint's claimed_type is mountain
 *   (fundamental right, natural law), but it declares beneficiaries (users)
 *   and victims (vendors), triggering FSM evaluation. The engine will compute
 *   per-seat types: users experience coordination (rope), vendors experience
 *   extraction (snare/tangled_rope).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.78).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.72).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, mountain).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Software Freedom Imperative (FSF/GNU reading)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).
domain_priors:emerges_naturally(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, '5125f21a-10d0-4663-9289-d75653811828').
narrative_ontology:cs_kernel_codification('5125f21a-10d0-4663-9289-d75653811828', distributed).
narrative_ontology:cs_authority_grounding('5125f21a-10d0-4663-9289-d75653811828', lineage).
narrative_ontology:cs_interpretation_layer_present('5125f21a-10d0-4663-9289-d75653811828').
narrative_ontology:cs_reading_relation('5125f21a-10d0-4663-9289-d75653811828', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('5125f21a-10d0-4663-9289-d75653811828', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('5125f21a-10d0-4663-9289-d75653811828', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('5125f21a-10d0-4663-9289-d75653811828', foundational, user_control_fundamental_right).
narrative_ontology:cs_axiom_status(user_control_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('5125f21a-10d0-4663-9289-d75653811828', user_control_fundamental_right, deontological).
narrative_ontology:cs_axiom('5125f21a-10d0-4663-9289-d75653811828', foundational, proprietary_software_illegitimate).
narrative_ontology:cs_axiom_status(proprietary_software_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('5125f21a-10d0-4663-9289-d75653811828', proprietary_software_illegitimate, deontological).
narrative_ontology:cs_reference_frame('5125f21a-10d0-4663-9289-d75653811828', four_freedoms_doctrine).
narrative_ontology:cs_drift_state('5125f21a-10d0-4663-9289-d75653811828', contemporary_surveillance_capitalism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5125f21a-10d0-4663-9289-d75653811828', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, free_software_developers).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, four_freedoms_doctrine).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, software_freedom_as_human_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Depend on software for daily life, work, communication, and civic participation. The freedom imperative asserts they have a fundamental right to control their computing: to run, study, modify, and share software. Their identity as autonomous agents is fused with this claim — accepting proprietary software is experienced as surrendering agency. Exit from the proprietary ecosystem requires adopting free software exclusively, which entails significant switching costs (hardware compatibility, application gaps, social coordination).
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders, beneficiary,
    moderate, biographical, identity_locked, universal).

% Build business models on restricting user control: licensing rather than selling, prohibiting reverse engineering, controlling distribution channels, and collecting behavioral data. The freedom imperative demands they abandon these restrictions — effectively expropriating their core IP and revenue model. They can comply (open-source their code, shift to services/support), resist (lobby for stronger IP law, use technical measures like DRM), or exit the market. Exit is constrained by sunk costs in proprietary codebases and shareholder obligations.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, payer,
    institutional, biographical, constrained, global).

% Write and maintain software under copyleft licenses (GPL family) that legally enforce the four freedoms. They set the normative agenda through the Free Software Foundation, GNU Project, and affiliated organizations. They benefit from the constraint because it validates their labor model and expands the commons they draw from. Their exit options are strong — they can move between projects, fork codebases, and operate outside proprietary ecosystems entirely.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_developers, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, free_software_developers, beneficiary).

% Confront policy questions: right-to-repair mandates, public procurement preferences for open source, interoperability requirements (DMA in EU), and whether software freedom constitutes a consumer right or a developer privilege. They observe the conflict between vendor IP claims and user autonomy claims. Their analytical seat lets them see the full structure, but they are not directly constrained by the freedom imperative unless they codify it into law.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, regulators_and_legislators, observer,
    institutional, generational, analytical, national).

% Want software that works reliably for their immediate tasks. They lack technical capacity to exercise freedoms 1 and 3 (study and modify source code) and often lack viable free-software alternatives for specialized needs (CAD, proprietary formats, industry-specific tools). They are structurally excluded from the freedom discourse because their practical constraints make the four freedoms theoretical. If the freedom imperative were enforced categorically, they would lose access to tools they depend on, with no realistic path to replacing them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, pragmatic_end_users, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global software commons around the principle that users must control their computing. Solves the problem of vendor lock-in, surveillance, and planned obsolescence by establishing a normative and legal baseline (copyleft) that prevents enclosure of the software commons.
% TRANSFER_FUNCTION: Transfers decision-making authority over software behavior from vendors to users. Vendors lose the right to restrict use, modification, and distribution; users gain the legal and practical ability to inspect, alter, and share software. The transfer is not monetary but capacitative: control over computation moves from centralized vendors to distributed users.
% ABSENT_VOICES: Pragmatic end-users who lack technical capacity to exercise source-code freedoms and depend on proprietary tools for livelihood (see pragmatic_end_users stakeholder). Small proprietary vendors in developing economies who cannot afford the compliance burden of copyleft or the business-model pivot to services. Hardware manufacturers who rely on proprietary firmware for device differentiation and security certification.
% DISAPPEARANCE_RATIONALE: If the freedom imperative vanished overnight, copyleft licenses would lose their normative force, vendors would accelerate enclosure (SaaS-only models, hardware-locked bootloaders, AI-model-as-service), and the free software ecosystem would fragment into permissive-licensed fragments vulnerable to proprietary capture. The software commons would collapse into a series of vendor-controlled silos within 5-10 years.
% FOUNDING_PROBLEM: The 1980s shift from shared academic/industrial software culture to proprietary enclosure: vendors began distributing binaries only, using copyright and trade secret law to prevent users from fixing bugs, adapting software, or understanding what it does. Stallman's printer-driver incident (denied source code for a Xerox 9700) crystallized the problem: users had become helpless dependents on vendor benevolence.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by independent observers outside the FSF orbit: Shoshana Zuboff's surveillance capitalism thesis (vendor extraction of behavioral surplus), the right-to-repair movement (farmers, hospitals, independent repair shops blocked by DRM and copyright), and EU Digital Markets Act findings (gatekeeper control over software distribution). No major institutional actor disputes that vendor control has intensified since the 1980s; the dispute is whether the freedom imperative is the correct remedy.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, ExtMetricName, E),
    domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(software_control_legitimacy__freedom_imperative_reading),
    narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) reflects the categorical demand that vendors abandon their core business model — a total transfer of control rights. Suppression (0.72) reflects the enforcement needed: copyleft licenses, legal action against GPL violators, political advocacy for right-to-repair and interoperability mandates. Theater is low (0.12) because the movement's coordination function (building a free software commons) is genuine and its primary activity; enforcement serves the freedom claim, not a hidden extraction agenda. Accessibility collapse (0.82) is high for vendors: once the freedom claim is accepted, the proprietary model has no legitimate fallback. Resistance (0.68) is substantial: vendors lobby, litigate, and deploy technical countermeasures (DRM, Tivoization, SaaS).
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (vendors) and beneficiary seat (users) compute different types: vendors experience a snare (coercive extraction with no coordination benefit for them); users experience a rope (genuine coordination solving vendor lock-in). The agenda-setter seat (free software developers) experiences a scaffold — the copyleft mechanism is transitional, meant to become unnecessary when freedom is the norm. The engine computes this divergence from the structural data; the claimed mountain type is the reading's self-presentation, not the engine's verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Users are structural beneficiaries (d ≈ 0.15): the constraint subsidizes them with control rights they otherwise lack. Their identity_locked exit (ideological fusion with the freedom claim) keeps d from reaching 0.0. Proprietary vendors are structural targets (d ≈ 0.85): the constraint extracts their IP control and revenue model. Their constrained exit (sunk costs, fiduciary duties) keeps d from reaching 1.0. Free software developers sit near beneficiary (d ≈ 0.2) — they gain commons access but bear maintenance burden. Regulators are analytical (d = 0.5 by definition). Pragmatic users are excluded — they bear costs (lost tool access) without the ideological framework that makes the tradeoff meaningful.
 *
 * MANDATROPHY ANALYSIS:
 *   The freedom imperative prevents mislabeling by naming the extraction explicitly: vendor control IS the extraction, not a coordination service. The mandate (user freedom) has not atrophied — surveillance capitalism and right-to-repair struggles confirm the founding problem is live. The constraint is not a piton: theater is low, enforcement is functional, and the commons continues expanding (Linux, Android kernel, GNU toolchain, RISC-V). Mandatrophy is unresolved because the proprietary model persists and adapts (SaaS, AI-as-service), creating new enclosure frontiers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the freedom_imperative_reading instantiate a distinct constraint from the software_control_legitimacy kernel, or is it the kernel itself?',
    'Compare structural predictions: if the kernel is the contested question and readings are answers, each reading should produce a different constraint with different beneficiary/victim sets. Verify that pragmatic_openness, property_rights, and commons readings would author different stakeholders and metrics.',
    'If this reading IS the kernel (not a reading of it), the constraint family decomposition fails and network.affects_constraints links become incoherent. The committer frame requires treating it as one reading among siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the freedom imperative is a reading of the kernel or the kernel itself.').

omega_variable(
    natural_law_vs_constructed_norm,
    'Is the four-freedoms claim a genuine natural law (mountain) or a constructed normative demand that benefits identifiable agents (false summit)?',
    'Test FSM signature: mountain claim + declared beneficiaries + non-zero extractiveness from vendor seat. If the engine reclassifies vendor seat as snare/tangled_rope while user seat computes as rope, the mountain claim is a false summit for vendors but genuine coordination for users.',
    'If false summit, the mountain claim functions as ideological cover for a constraint that extracts from vendors. If genuine mountain, the extraction metric is mis-specified — a natural law cannot extract. The omega documents the ambiguity; the engine resolves via FSM.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_norm, conceptual, 'Natural-law status of the freedom claim vs. its constructed benefit to users-as-rights-holders.').

omega_variable(
    universal_vs_cultural_freedom,
    'Is the user freedom claim genuinely universal (universal scope) or culturally specific to hacker/Western libertarian traditions?',
    'Empirical survey of non-Western free software communities (e.g., Chinese GNU/Linux users, Brazilian telecentros, Indian Kerala model): do they adopt the four freedoms as intrinsic rights or as pragmatic tools? If the latter, spatial_scope should be regional/cultural, not universal.',
    'If culturally specific, the universal scope declaration inflates effective extraction (scope modifier) and misrepresents the constraint''s actual reach. The pragmatic_end_users stakeholder would be larger and more central.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_vs_cultural_freedom, empirical, 'Universality of the freedom claim across cultural contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 1983, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1983, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1983, 0.05).
narrative_ontology:measurement(soft_tr_t1991, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1991, 0.07).
narrative_ontology:measurement(soft_tr_t1998, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement(soft_tr_t2007, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2007, 0.1).
narrative_ontology:measurement(soft_tr_t2013, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2013, 0.11).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(soft_be_t1983, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1983, 0.35).
narrative_ontology:measurement(soft_be_t1991, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1991, 0.42).
narrative_ontology:measurement(soft_be_t1998, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(soft_be_t2007, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2007, 0.68).
narrative_ontology:measurement(soft_be_t2013, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2013, 0.74).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1983, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1983, 0.25).
narrative_ontology:measurement(soft_su_t1991, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1991, 0.35).
narrative_ontology:measurement(soft_su_t1998, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(soft_su_t2007, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2007, 0.62).
narrative_ontology:measurement(soft_su_t2013, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2013, 0.68).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__freedom_imperative_reading, 0.08).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This reading and property_rights_reading are logical contraries within the software_control_legitimacy kernel: one asserts user control as fundamental right, the other asserts creator control as property right. They cannot both be true in a single legal framework (forecloses relation). The pragmatic_openness_reading and commons_reading occupy intermediate positions that coexist with both but are pulled toward this reading's stricter copyleft boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__freedom_imperative_reading, moderate, 0.15).
constraint_indexing:directionality_override(software_control_legitimacy__freedom_imperative_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
