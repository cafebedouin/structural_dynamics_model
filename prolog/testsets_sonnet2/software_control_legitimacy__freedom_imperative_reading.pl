% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software as Denial of Computing Freedom (Freedom Imperative Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the freedom_imperative_reading of the
 *   software_control_legitimacy kernel: proprietary software is categorically
 *   ethically illegitimate because it denies users the four freedoms (run,
 *   study, share, modify) over software running on their own machines. Under
 *   this reading, every proprietary software arrangement — not merely abusive
 *   instances of it — belongs in the victim set, because the denial of source
 *   and modification rights is the wrong itself, independent of how well or
 *   badly a given vendor behaves. This is a single, specific claim distinct
 *   from the pragmatic_openness_reading (which treats openness as a
 *   development-quality choice among legitimate alternatives), the
 *   property_rights_reading (which treats vendor control as a legitimate
 *   extension of creator property rights), and the commons_reading (which
 *   treats the whole question as one of negotiated collective governance
 *   rather than a rights violation). Each of those is a structurally
 *   different constraint with its own ε and its own beneficiary/victim
 *   structure, generated as separate stories.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: agenda_setter/beneficiary (institutional/arbitrage) — writes and enforces license terms restricting the four freedoms
 *   - platform_dominant_incumbents: beneficiary (institutional/arbitrage) — profits from lock-in the closed-source norm sustains
 *   - end_users_of_proprietary_software: payer (powerless/constrained) — denied control over software they run
 *   - downstream_developers_denied_source: payer (moderate/constrained) — excluded from building on or auditing dependencies
 *   - repair_and_modification_communities: payer (powerless/trapped) — blocked from repairing or extending owned devices
 *   - free_software_movement: excluded (organized/constrained) — raises this exact objection but is marginalized in policy venues
 *   - legislators_and_courts: observer (institutional/analytical) — adjudicates copyright and anti-circumvention law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.78).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.62).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software as Denial of Computing Freedom (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, '3286b1c0-47d6-4454-85fc-9e08926de46c').
narrative_ontology:cs_kernel_codification('3286b1c0-47d6-4454-85fc-9e08926de46c', distributed).
narrative_ontology:cs_authority_grounding('3286b1c0-47d6-4454-85fc-9e08926de46c', distributed).
narrative_ontology:cs_reading_relation('3286b1c0-47d6-4454-85fc-9e08926de46c', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('3286b1c0-47d6-4454-85fc-9e08926de46c', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('3286b1c0-47d6-4454-85fc-9e08926de46c', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('3286b1c0-47d6-4454-85fc-9e08926de46c', foundational, user_control_is_baseline_entitlement).
narrative_ontology:cs_axiom_status(user_control_is_baseline_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('3286b1c0-47d6-4454-85fc-9e08926de46c', user_control_is_baseline_entitlement, deontological).
narrative_ontology:cs_axiom('3286b1c0-47d6-4454-85fc-9e08926de46c', secondary, restriction_of_source_access_is_intrinsic_harm).
narrative_ontology:cs_axiom_status(restriction_of_source_access_is_intrinsic_harm, holdable).
narrative_ontology:cs_axiom_grounding('3286b1c0-47d6-4454-85fc-9e08926de46c', restriction_of_source_access_is_intrinsic_harm, deontological).
narrative_ontology:cs_reference_frame('3286b1c0-47d6-4454-85fc-9e08926de46c', four_freedoms_baseline_entitlement).
narrative_ontology:cs_drift_state('3286b1c0-47d6-4454-85fc-9e08926de46c', contemporary_platform_lockin_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3286b1c0-47d6-4454-85fc-9e08926de46c', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, platform_dominant_incumbents).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, end_users_of_proprietary_software).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, downstream_developers_denied_source).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, repair_and_modification_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write license terms, enforce them through copyright, DRM, and EULA litigation, and control the source code and update path for software users depend on. Collects licensing and subscription revenue directly from the arrangement they administer and defend as legitimate protection of investment.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, beneficiary).

% Benefit from the closed-source norm because it locks users and third-party developers into their ecosystems, insulating market position from forkability or independent audit. Do not administer the arrangement directly but structurally profit from its persistence across the industry.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, platform_dominant_incumbents, beneficiary,
    institutional, civilizational, arbitrage, global).

% Run software they cannot inspect, modify, or fully control on hardware they own; cannot verify what the program does with their data or fix defects themselves. Exit means abandoning functionality, data, or workflows built around the proprietary tool — rarely a free choice in practice.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, end_users_of_proprietary_software, payer,
    powerless, biographical, constrained, global).

% Cannot build on, audit, or extend proprietary components they depend on for interoperability; must reverse-engineer or negotiate access, and can be blocked entirely by license terms or technical countermeasures. Their labor is structurally excluded from participating in the software's evolution.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, downstream_developers_denied_source, payer,
    moderate, biographical, constrained, global).

% Attempt to repair, adapt, or extend the lifespan of devices and software they own but are blocked by locked bootloaders, signed firmware, and anti-circumvention law. Bear the cost of premature obsolescence and lost autonomy over property they purchased.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, repair_and_modification_communities, payer,
    powerless, biographical, trapped, national).

% Argues from this reading's own premise that the four freedoms (run, study, share, modify) are ethical baselines, not features; treated in policy and market venues as an ideological fringe rather than a legitimate rights claim, so its objection to proprietary licensing rarely reaches the forums that set licensing law.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_movement, excluded,
    organized, civilizational, constrained, global).

% Adjudicate copyright, DMCA-style anti-circumvention statutes, and right-to-repair legislation; hear testimony from vendors and freedom advocates and can shift the enforcement machinery that keeps proprietary control intact or open it to challenge.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, legislators_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, diffuse).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized under this reading as a legitimate coordination function of the closed-source arrangement itself — vendors coordinate internal engineering and support, but this reading holds that legitimate coordination does not require denying users the four freedoms; any coordination benefit could be achieved without control-denial.
% TRANSFER_FUNCTION: Moves control over a good's ongoing function — the capacity to inspect, repair, modify, and redistribute — from the people who possess and use the software to the entity that licenses it, in exchange for continued permission to use a copy.
% ABSENT_VOICES: The free software movement and organized user-freedom advocates routinely raise this objection but are institutionally treated as an ideological minority position in legislative and standards venues dominated by vendor and property-rights framings; their exclusion is why the categorical illegitimacy claim rarely reaches the bodies that set licensing and anti-circumvention law.
% DISAPPEARANCE_RATIONALE: If proprietary control were dissolved overnight — source universally available, modification and redistribution unrestricted — vendor revenue models built on license scarcity would collapse or transform, platform lock-in would weaken sharply, and a large repair, forking, and modification economy would emerge among users and developers currently blocked from it.
% FOUNDING_PROBLEM: Early software industry needed a way to fund large engineering efforts and recoup development investment when copying software was costless; closed licensing was adopted as the commercial vehicle for that funding.
% FOUNDING_PROBLEM_CORROBORATION: Vendors and platform incumbents attest the funding problem remains live and justifies continued control. The free software movement, digital rights organizations, and right-to-repair advocates — outside the beneficiary set — attest that alternative funding models (service, support, dual licensing, public funding) have demonstrated the coordination function is separable from control-denial, and that the persisting restriction now serves market position rather than the original funding problem.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.78 by interval end) because, from this reading's own premises, the categorical denial of source access, modification rights, and redistribution rights constitutes a rights violation across the entire installed base of proprietary software, not a contained harm limited to bad actors. Suppression (0.62) reflects the active legal machinery — copyright enforcement, anti-circumvention statutes, DRM, EULA litigation — required to keep the restriction in force against users' latent capacity to inspect and modify code they possess. Theater ratio is kept low (0.2) because the enforcement functions are not performative; the restriction is materially effective at preventing modification. Both measured metrics rise over the interval, tracking the reading's own view that anti-circumvention law and DRM have hardened over time (DMCA-style statutes, signed firmware, secure boot) rather than loosened.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor/agenda-setter seat, licensing is a legitimate mechanism for funding development and protecting investment — a rope, in their own account. From the payer seats (end users, downstream developers, repair communities) under this reading's premises, the same mechanism computes as an enforced denial of a baseline freedom, closer to tangled_rope or snare depending on exit mobility. The engine computes these divergent seat classifications from the declared structural data; this story does not adjudicate between the vendor's self-account and the reading's verdict — it only authors the reading's structural claim honestly.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors and platform incumbents are declared beneficiaries with arbitrage-grade exit — they set the terms and can restructure licensing at will, so directionality sits near the full-beneficiary end. End users, downstream developers, and repair/modification communities are declared victims with constrained-to-trapped exit — the software or device they need is only available under the restrictive terms, so directionality sits near the full-target end. The free software movement is excluded rather than a direct payer: it bears no direct extraction but is denied the standing to change the arrangement it objects to on principle.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (funding software development when copying is costless) is contested as live: this reading holds that alternative funding models (service contracts, dual licensing, public and foundation funding, support subscriptions) have demonstrated the coordination function is separable from denying users control, so the persistence of closed licensing now serves market position and lock-in rather than the original funding rationale. Naming this as tangled_rope rather than pure snare préserves the vendor's genuine (if separable) coordination function — organizing engineering effort, providing support, maintaining a coherent product — while still registering the asymmetric extraction this reading holds is built into the arrangement's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_conduct_based_illegitimacy,
    'Is proprietary control illegitimate categorically (as this reading holds, by the mere fact of restricting the four freedoms) or only when specific conduct (data harvesting, forced obsolescence, anti-repair enforcement) crosses an independent harm threshold?',
    'Compare user and developer outcomes across proprietary software with strong support/transparency commitments versus proprietary software with none; if categorical harm claims do not track measurable outcome differences, the categorical framing is weaker than the conduct-based framing this reading rejects.',
    'If harm tracks conduct rather than mere restriction, this reading''s beneficiary/victim structure over-generalizes and a conduct-indexed reading (closer to property_rights or pragmatic_openness with carve-outs) would be more descriptively accurate; if harm tracks restriction itself regardless of conduct, the categorical framing is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_conduct_based_illegitimacy, conceptual, 'Whether illegitimacy attaches to the fact of proprietary control or to specific harmful conduct enabled by it.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which reading of the software_control_legitimacy kernel is the ''correct'' structural account — freedom_imperative, property_rights, pragmatic_openness, or commons — or is the kernel itself irreducibly contested with no fact-of-the-matter resolution?',
    'This is not resolvable by data internal to any one reading; it is a normative/conceptual dispute about the moral status of source-code access. Track whether legal, economic, and cultural practice converges toward one reading over time (e.g., expanding right-to-repair legislation would evidence movement toward this reading; expanding IP enforcement would evidence movement toward property_rights).',
    'If practice converges toward this reading, its extraction and victim framing gains descriptive as well as normative force; if practice remains split or moves toward property_rights, this reading remains a minority ethical claim rather than an emerging structural consensus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, preference, 'Whether one reading of the contested kernel represents the settling truth or the kernel remains genuinely and permanently multi-valent.').

omega_variable(
    coordination_separability,
    'Is the vendor''s coordination function (funding development, providing coherent support, maintaining product quality) genuinely separable from control-denial, or does effective coordination require some degree of restriction on modification and redistribution?',
    'Comparative study of large-scale open-source projects with commercial support models (funded via service contracts, dual licensing, foundation grants) against proprietary projects of comparable scale and complexity, measuring whether coordination quality (release cadence, security response, feature coherence) differs systematically.',
    'If coordination quality is comparable without restriction, the tangled_rope classification (genuine coordination plus separable extraction) is confirmed; if coordination reliably degrades without some restriction, the extraction is closer to a necessary cost of coordination and the classification would move toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_separability, empirical, 'Whether the vendor coordination function requires control-denial as a structural precondition or merely uses it as a funding convenience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(soft_tr_t32, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 32, 0.19).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(soft_be_t32, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 32, 0.77).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(soft_su_t8, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(soft_su_t16, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(soft_su_t32, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__freedom_imperative_reading, 0.08).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This story is one of four members of the software_control_legitimacy kernel family. Each reading — freedom_imperative, pragmatic_openness, property_rights, commons — is authored as a separate constraint with its own ε, beneficiary/victim structure, and claimed type, per the ε-invariance principle: the natural-language label 'software control legitimacy' conflates four structurally distinct normative claims about the same underlying arrangement. This reading (freedom_imperative) authors the highest ε and the widest victim set (all proprietary software users) because it treats control-denial itself, not any specific harmful conduct, as the wrong. property_rights_reading is expected to author the lowest ε (restriction is a legitimate right, not an extraction). pragmatic_openness_reading is expected to author near-neutral ε (a methodology preference, not a rights violation). commons_reading is expected to author moderate, contested ε (negotiated governance rather than categorical verdict in either direction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
