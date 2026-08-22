% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Proprietary Software Licensing as Ethical Injustice (Freedom Imperative Reading)
 *   domain: software engineering / political economy of technology / intellectual property
 *
 * SUMMARY:
 *   Proprietary software licensing restricts users from viewing, modifying,
 *   or redistributing the source code of software they run, enforced through
 *   copyright law, EULAs, and technical measures like DRM. This reading holds
 *   that such restriction is not a neutral market choice or a legitimate
 *   exercise of property rights but a fundamental ethical wrong: the four
 *   freedoms (to run, study, share, modify) are treated as inalienable,
 *   comparable to civil liberties, such that any license abridging them
 *   constitutes an injustice regardless of the economic arrangement's
 *   efficiency or the vendor's investment. On this reading virtually all
 *   proprietary software enters the victim-generating structure, since the
 *   restriction itself — not any downstream harm — is the wrong being
 *   measured.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: agenda_setter/beneficiary (institutional/arbitrage) — sets license terms, enforces via copyright and DRM, collects licensing revenue
 *   - copyright_licensing_intermediaries: beneficiary (organized/mobile) — profits from administering and enforcing restriction
 *   - end_users_without_source_access: payer (powerless/constrained) — bears loss of inspection, audit, and adaptation rights
 *   - downstream_developers_barred_from_modification: payer (moderate/trapped) — foreclosed from building on existing work
 *   - device_owners_subject_to_drm: payer (powerless/trapped) — nominal ownership without control
 *   - digital_commons: payer, non-agent (powerless/trapped) — diminished shared knowledge base
 *   - free_software_movement: excluded (organized/analytical) — holds the rights framing but lacks legislative access
 *   - legislatures_and_courts: observer (institutional/analytical) — administers the arrangement as ordinary IP law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.72).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.68).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Proprietary Software Licensing as Ethical Injustice (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software engineering / political economy of technology / intellectual property").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, 'a22261ab-9e45-4240-a029-32d27e11ea02').
narrative_ontology:cs_kernel_codification('a22261ab-9e45-4240-a029-32d27e11ea02', distributed).
narrative_ontology:cs_authority_grounding('a22261ab-9e45-4240-a029-32d27e11ea02', distributed).
narrative_ontology:cs_reading_relation('a22261ab-9e45-4240-a029-32d27e11ea02', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('a22261ab-9e45-4240-a029-32d27e11ea02', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('a22261ab-9e45-4240-a029-32d27e11ea02', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('a22261ab-9e45-4240-a029-32d27e11ea02', foundational, source_access_as_inalienable_right).
narrative_ontology:cs_axiom_status(source_access_as_inalienable_right, holdable).
narrative_ontology:cs_axiom_grounding('a22261ab-9e45-4240-a029-32d27e11ea02', source_access_as_inalienable_right, deontological).
narrative_ontology:cs_axiom('a22261ab-9e45-4240-a029-32d27e11ea02', secondary, restriction_wrong_independent_of_consequences).
narrative_ontology:cs_axiom_status(restriction_wrong_independent_of_consequences, holdable).
narrative_ontology:cs_axiom_grounding('a22261ab-9e45-4240-a029-32d27e11ea02', restriction_wrong_independent_of_consequences, deontological).
narrative_ontology:cs_reference_frame('a22261ab-9e45-4240-a029-32d27e11ea02', four_freedoms_founding_definition).
narrative_ontology:cs_drift_state('a22261ab-9e45-4240-a029-32d27e11ea02', contemporary_cloud_and_drm_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a22261ab-9e45-4240-a029-32d27e11ea02', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, copyright_licensing_intermediaries).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, end_users_without_source_access).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, downstream_developers_barred_from_modification).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, device_owners_subject_to_drm).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, digital_commons).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, software_freedom_as_natural_right).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, the_four_freedoms_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write license terms that forbid users from viewing, modifying, or redistributing source code, and enforce those terms through copyright law, DRM, and end-user license agreements. Collect licensing revenue and maintain competitive advantage by keeping implementation details opaque. Can exit any given jurisdiction's regulatory pressure by relocating incorporation or restructuring licensing terms.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, proprietary_software_vendors, beneficiary).

% Law firms, licensing bodies, and enforcement contractors that profit from administering, litigating, and policing proprietary license terms. Their revenue depends on the restriction regime remaining in force and contested.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, copyright_licensing_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Run software whose behavior they cannot inspect, audit for security flaws, or adapt to their needs. Bear the cost of vendor lock-in, forced upgrades, and inability to fix or extend tools essential to their work or lives. Switching means abandoning data, workflows, or compatible ecosystems built around the proprietary product — on this reading, this is not a market preference but a rights violation, since access to the source of software one depends on is treated as inalienable.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, end_users_without_source_access, payer,
    powerless, biographical, constrained, global).

% Would extend, fix, or build upon existing software but are legally forbidden from accessing or altering its source. Reimplement functionality from scratch at enormous duplicated effort, or abandon the improvement entirely. Their labor is foreclosed by license terms rather than by any technical limitation.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, downstream_developers_barred_from_modification, payer,
    moderate, biographical, trapped, global).

% Own hardware whose embedded software they are legally and technically barred from inspecting or replacing, even for devices they purchased outright. DRM and tivoization mean nominal ownership without control — on this reading, this is the injustice in its most acute form, since the physical property right is real but the software governing it is not theirs to command.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, device_owners_subject_to_drm, payer,
    powerless, biographical, trapped, global).

% The shared body of software knowledge and reusable code that would exist if source were universally available. Diminished every time a useful program is released only as a binary, since its techniques cannot be studied, its bugs cannot be collectively fixed, and its lineage terminates in a black box.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, digital_commons, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(software_source_status__freedom_imperative_reading, digital_commons).

% Advocates (FSF and aligned developers) who hold that the four freedoms are moral entitlements, not licensing preferences, and that proprietary licensing is comparable to other forms of unjust restriction on liberty. Largely excluded from legislative and standards-body processes that shape copyright and licensing law, which are dominated by vendor and rights-holder interests.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_movement, excluded,
    organized, civilizational, analytical, global).

% Adjudicate copyright, DMCA anti-circumvention provisions, and licensing disputes. Generally treat software as ordinary intellectual property subject to owner control, which this reading holds to be a category error — but the observer seat records the arrangement as currently enforced, not as this reading would have it be.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, legislatures_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: On this reading, there is no genuine coordination problem that proprietary licensing solves that could not be solved, and solved better, by freely licensed software with voluntary support and service markets; what proprietary licensing actually coordinates is vendor revenue capture, not any function users need restricted.
% TRANSFER_FUNCTION: Moves control over how software may be run, studied, modified, and shared from the people who depend on that software to the entities that hold copyright over it, and moves the resulting economic rents from users and downstream developers to vendors and licensing intermediaries.
% ABSENT_VOICES: The free software movement and unrepresented end users are structurally absent from the legislative and standards processes (copyright terms, DMCA anti-circumvention rules, licensing frameworks) that entrench proprietary control; their objection — that source access is a right, not a courtesy — is treated as a fringe position in those venues.
% DISAPPEARANCE_RATIONALE: If proprietary licensing restrictions vanished overnight, all software would default to freely inspectable, modifiable, redistributable form; vendor revenue models built on restricting access would collapse or convert to service/support models; downstream developers and the digital commons would gain immediate access to previously locked source; device owners would gain control over hardware they already own.
% FOUNDING_PROBLEM: Proprietary licensing was framed as solving the problem of funding software development — ensuring creators could capture returns on investment by controlling copying and modification.
% FOUNDING_PROBLEM_CORROBORATION: Vendors and licensing intermediaries attest the funding problem is live and requires continued restriction. The free software movement, along with independent economists studying open-source-funded ecosystems (Linux, Apache, the broader FOSS commercial ecosystem), attest from outside the vendor beneficiary set that functioning funding models exist without source restriction, and that restriction serves rent extraction rather than being necessary to solve the funding problem this reading contests as ever having justified the restriction.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.72) and rising because this reading treats the restriction of source access itself as the extractive act, compounding as software mediates more of daily and economic life (DRM'd hardware, cloud-tethered devices, algorithmic infrastructure). Suppression is authored substantial (0.68) because enforcement runs through copyright litigation, DMCA anti-circumvention provisions, and technical DRM measures that criminalize circumvention even for legitimate interoperability or repair purposes — this is active, structural coercion, not mere market friction. Accessibility collapse is authored at 0.6: once a user understands what source access would allow, the collapse of that alternative under a restrictive license is total for that specific product, though free-software alternatives exist in parallel for many categories, which caps collapse below mountain-level. Resistance is authored high (0.7): the free software movement, right-to-repair advocates, and open-source communities mount organized, sustained resistance, which a genuine mountain would not encounter. Theater ratio is low-moderate (0.3) and rising: some vendor 'transparency' and 'open governance' initiatives function partly as legitimacy theater around a substantially unchanged restriction core.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors and licensing intermediaries sit near the full-beneficiary end: they set the terms, enforce them, and collect the resulting rents, with mobile or arbitrage-grade exit from any single jurisdiction's regulatory pressure. End users, downstream developers, and device owners sit near the full-target end: they bear the restriction's costs, have constrained or trapped exit (switching costs, DRM lock-in, ecosystem dependency), and under this reading's framing have a right being violated rather than a preference being priced. The digital commons is authored as a non-agent payer — it cannot exit at all, which is why its exit_options is trapped and its agent flag is false; it is included for narrative completeness, not fed into directionality as a collecting party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (funding software development via restriction) is authored as contested rather than dead outright, because vendors sincerely maintain the problem is live, while this reading — corroborated by economists studying FOSS-funded ecosystems from outside the vendor beneficiary set — holds that functioning funding models without source restriction already exist and have for decades (Linux, Apache, Postgres, RedHat/IBM support models), meaning restriction persists for rent capture rather than necessity. This is the mismatch the founding_problem fields are designed to surface: status=contested plus disappearance_verdict=world_rearranges signals a live capture dispute rather than a settled mandate, and the corpus should not resolve it here — that resolution belongs to empirical work on FOSS funding sustainability, which is exactly what the omega below routes to.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    software_freedom_natural_right_or_policy_preference,
    'Is software freedom (the four freedoms) genuinely a fundamental ethical entitlement comparable to other inalienable rights, or is it a strongly-held policy preference dressed in rights language?',
    'This is fundamentally a conceptual/normative question not resolvable by empirical data alone; philosophical argument about the grounds of intellectual property rights versus user autonomy rights, and comparative analysis of how other restriction-of-use domains (right to repair, medical device firmware, agricultural equipment) are adjudicated, would inform but not settle it.',
    'If software freedom is a genuine natural right, the tangled_rope/snare classification of nearly all proprietary licensing is structurally correct under this reading and the victim set is properly universal. If it is better understood as an instrumental or preference claim, this reading collapses toward the pragmatic_development_reading or utilitarian_hybrid_reading, and much of the authored extraction and suppression would need reattribution to those readings instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(software_freedom_natural_right_or_policy_preference, preference, 'Whether software freedom is a rights claim or a strongly held instrumental/policy preference — the foundational uncertainty this reading is built on.').

omega_variable(
    sibling_reading_delta_property_rights,
    'Where exactly does this reading''s premise conflict with the property_rights_reading''s premise, and can both be simultaneously true within a single legal framework?',
    'Doctrinal analysis of whether a legal system can simultaneously recognize copyright as legitimate property AND recognize source-access as an inalienable user right that copyright may not override — most existing legal systems currently side with property_rights_reading, which is itself evidence about where the disagreement is located (in the moral foundation, not merely in policy preference).',
    'If the two premises are logically incompatible in any single framework (a right that copyright cannot override vs. copyright as the ultimate arbiter of access), the relationship should be forecloses rather than coexists_with; current legal practice suggests they currently coexist across different advocacy communities without one being logically forced out, which is why this story codes the relation as coexists_with rather than forecloses — but this is a contestable judgment call, not a settled fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta_property_rights, conceptual, 'Where the freedom_imperative and property_rights readings actually disagree, and whether that disagreement is logical incompatibility or competing values held by different parties.').

omega_variable(
    fsm_style_beneficiary_naturalization_check,
    'Does vendor-side rhetoric ever present the restriction regime as a natural or inevitable feature of software economics (rather than a constructed legal choice), and if so does that framing function analogously to a false-summit mountain claim?',
    'Content analysis of vendor and industry-association public communications for language treating IP restriction as a natural consequence of creative labor (e.g., ''you wouldn''t steal a car'') versus acknowledgment that it is a specific, revisable legal construction (copyright term length, DMCA scope) that has changed substantially over time.',
    'If vendor rhetoric substantially naturalizes the restriction regime, that supports this reading''s injustice framing (a constructed extraction dressed as natural necessity); if vendor rhetoric is candidly economic rather than naturalizing, the injustice framing rests more purely on the rights claim itself rather than on a naturalization-of-extraction move.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_style_beneficiary_naturalization_check, empirical, 'Whether proprietary licensing rhetoric naturalizes a constructed restriction, which would parallel false-summit dynamics even though this constraint is authored as tangled_rope, not mountain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soft_tr_t8, software_source_status__freedom_imperative_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(soft_tr_t16, software_source_status__freedom_imperative_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(soft_tr_t24, software_source_status__freedom_imperative_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(soft_tr_t32, software_source_status__freedom_imperative_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(soft_tr_t40, software_source_status__freedom_imperative_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(soft_be_t8, software_source_status__freedom_imperative_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(soft_be_t16, software_source_status__freedom_imperative_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(soft_be_t24, software_source_status__freedom_imperative_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(soft_be_t32, software_source_status__freedom_imperative_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(soft_be_t40, software_source_status__freedom_imperative_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(soft_su_t8, software_source_status__freedom_imperative_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(soft_su_t16, software_source_status__freedom_imperative_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(soft_su_t24, software_source_status__freedom_imperative_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(soft_su_t32, software_source_status__freedom_imperative_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(soft_su_t40, software_source_status__freedom_imperative_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(software_source_status__freedom_imperative_reading, 0.08).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraint stories decomposing the natural-language concept 'the software freedom debate' (kernel_id: software_source_status) per the ε-invariance principle: freedom_imperative_reading (this story, tangled_rope, ε=0.72 — restriction as categorical rights violation), pragmatic_development_reading (open source as superior methodology, freedom instrumental to quality), property_rights_reading (software as legitimate IP, restriction as owner's right), and utilitarian_hybrid_reading (licensing choice as context-dependent welfare maximization). Each reading authors its own stable ε over the same standing arrangement (current proprietary licensing practice) assessed by that reading's own lights — the four ε values are expected to differ substantially and are not meant to be averaged or reconciled into one 'true' ε for 'the' constraint. All four are linked bidirectionally via affects_constraints as members of one kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
