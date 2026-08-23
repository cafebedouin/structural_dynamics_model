% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Copyleft as Commons-Preserving Reciprocity Obligation
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'copyleft as commons' reading of the
 *   GPL reciprocity obligation: the viral copyleft clause (Section 2(b) of
 *   GPLv2, Section 5 of GPLv3) functions as institutional technology that
 *   prevents enclosure of the software commons by making reciprocity a
 *   condition of use. The beneficiary is the commons itself as a persistent
 *   institution; the victims are actors who would extract private value by
 *   privatizing derivatives. The constraint is a tangled rope — it performs
 *   genuine coordination (enabling massively collaborative development by
 *   guaranteeing contributions remain free) while asymmetrically extracting
 *   the exit option from proprietary-derivative vendors. The claimed type
 *   (tangled_rope) and metrics are authored independently: the metrics
 *   describe a moderately extractive, actively enforced constraint with low
 *   theater; the claim asserts this structure is genuinely hybrid
 *   coordination/extraction. The engine will compute per-seat classifications
 *   from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.48).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Copyleft as Commons-Preserving Reciprocity Obligation").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '40218e1d-042a-45d4-9c53-90d73c4e0be1').
narrative_ontology:cs_kernel_codification('40218e1d-042a-45d4-9c53-90d73c4e0be1', fixed_text).
narrative_ontology:cs_authority_grounding('40218e1d-042a-45d4-9c53-90d73c4e0be1', lineage).
narrative_ontology:cs_interpretation_layer_present('40218e1d-042a-45d4-9c53-90d73c4e0be1').
narrative_ontology:cs_reading_relation('40218e1d-042a-45d4-9c53-90d73c4e0be1', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('40218e1d-042a-45d4-9c53-90d73c4e0be1', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('40218e1d-042a-45d4-9c53-90d73c4e0be1', foundational, commons_enclosure_is_primary_threat).
narrative_ontology:cs_axiom_status(commons_enclosure_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('40218e1d-042a-45d4-9c53-90d73c4e0be1', commons_enclosure_is_primary_threat, empirically_contingent).
narrative_ontology:cs_axiom('40218e1d-042a-45d4-9c53-90d73c4e0be1', foundational, mandatory_reciprocity_preserves_commons).
narrative_ontology:cs_axiom_status(mandatory_reciprocity_preserves_commons, holdable).
narrative_ontology:cs_axiom_grounding('40218e1d-042a-45d4-9c53-90d73c4e0be1', mandatory_reciprocity_preserves_commons, empirically_contingent).
narrative_ontology:cs_reference_frame('40218e1d-042a-45d4-9c53-90d73c4e0be1', gnu_manifesto_commons_vision).
narrative_ontology:cs_drift_state('40218e1d-042a-45d4-9c53-90d73c4e0be1', contemporary_cloud_saas_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('40218e1d-042a-45d4-9c53-90d73c4e0be1', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons_institution).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, free_software_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_compliance_community).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_derivative_vendors).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_based_peer_production_viability).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, reciprocity_as_enclosure_prevention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective pool of GPL-licensed code that remains perpetually available for use, modification, and redistribution. The copyleft obligation ensures improvements flow back into this pool rather than being enclosed. The FSF and GNU Project act as its institutional stewards, but the commons itself is the primary beneficiary.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons_institution, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons_institution, agenda_setter).

% End users and developers who rely on the commons for software freedom. They benefit from the guarantee that GPL code they depend on will not be captured and closed by third parties. Their exit option is real — they can use permissively licensed alternatives — but the commons provides unique value they would lose.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, free_software_users, beneficiary,
    organized, biographical, mobile, global).

% Developers, lawyers, and organizations (e.g., Software Freedom Conservancy, FSF) who monitor compliance, enforce copyleft through legal action, and maintain the social norms around reciprocity. They invest labor in enforcement and benefit from a healthy commons. Exit means abandoning the enforcement infrastructure they built.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_compliance_community, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_compliance_community, beneficiary).

% Companies and developers who would prefer to incorporate GPL code into proprietary products without reciprocating. They bear the cost of the constraint: they must either release their derivatives under GPL (forgoing proprietary capture) or avoid GPL code entirely (forgoing its utility). Their exit is constrained by the ubiquity of GPL in critical infrastructure (Linux, GCC, etc.).
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers, payer,
    powerful, biographical, constrained, global).

% Vendors who have built or would build business models around proprietary derivatives of GPL code. The constraint structurally excludes this business model. They are trapped because the code they want to enclose is often infrastructure they cannot easily replace (kernels, compilers, core libraries).
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_derivative_vendors, payer,
    institutional, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_derivative_vendors, excluded).

% Developers and organizations who argue that copyleft's reciprocity requirement is itself a form of enclosure — it prevents code from flowing into more permissive ecosystems. They would object to the framing of copyleft as purely protective. They have high exit (can use/write BSD/MIT code) but are excluded from the GPL commons governance conversation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, permissive_license_advocates, excluded,
    organized, biographical, arbitrage, global).

% Researchers and theorists studying commons governance, institutional design, and license ecology. They see the full structure: the commons as beneficiary, exit-maximizers as payers, enforcement community as agenda-setters, and the tension between reciprocity as protection vs. restriction.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the commons enclosure problem in software: without mandatory reciprocity, actors can extract value from the commons (incorporate GPL code into proprietary products) without contributing improvements back, leading to progressive enclosure of the shared resource. The GPL makes reciprocity a condition of use, aligning individual incentives with commons preservation.
% TRANSFER_FUNCTION: Transfers the option to privatize derivative works from individual exit-maximizers (who would capture the value of commons-derived improvements for private gain) to the software commons institution (which retains those improvements in the shared pool). The transfer is not monetary — it is the foreclosure of a privatization pathway.
% ABSENT_VOICES: Proprietary software vendors who would enclose GPL code are structurally excluded by the license terms themselves — they cannot participate in the GPL commons on their preferred terms. Permissive-license advocates who view copyleft as restrictive are excluded from the governance of the GPL commons (the FSF controls the license text). Future developers who might prefer different reciprocity terms have no voice in the license's evolution.
% DISAPPEARANCE_RATIONALE: If the copyleft obligation vanished overnight, the GPL would become functionally permissive. Companies would immediately begin incorporating GPL code into proprietary products without releasing changes. The Linux kernel, GCC, and core infrastructure would face rapid enclosure. The commons would shrink dramatically as improvements flow into private repos instead of upstream. The entire free software ecosystem would reorganize around new protective mechanisms or fragment.
% FOUNDING_PROBLEM: The enclosure of software commons: in the 1980s, previously free Unix code was being proprietary-ized by vendors (AT&T, Sun, etc.), cutting off the community that built it. The GPL was designed to make enclosure legally impossible — to ensure that once code entered the commons, it could never be removed.
% FOUNDING_PROBLEM_CORROBORATION: The historical record of Unix enclosure (documented in FSF/GNU histories and Levy's 'Hackers') corroborates the founding problem. However, the status is contested: the FSF and compliance community attest the threat remains live (cloud/SaaS creates new enclosure vectors). Permissive-license advocates and some corporate OSPOs attest the problem is substantially solved — the commons is now large enough to be resilient, and copyleft's costs outweigh its protective benefits. No neutral third-party corroboration exists; the dispute is structural.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects that the constraint forecloses a valuable exit option (proprietary derivation) for a subset of actors, but most participants in the commons experience it as low-extraction coordination — they contribute voluntarily and benefit from others' reciprocated contributions. Suppression (0.55) is moderate: the constraint is enforced through copyright law and community pressure, but alternatives exist (permissive licenses, clean-room reimplementation). Theater ratio (0.22) is low and rising slowly — the enforcement machinery (compliance programs, legal actions) is functional, but a growing share of activity addresses edge cases (SaaS, linking boundaries) rather than core enclosure. The temporal series shows extractiveness and suppression rising as the commons grows more valuable and enclosure incentives increase; theater rises as enforcement addresses marginal cases.
 *
 * PERSPECTIVAL GAP:
 *   From the commons_institution and compliance_community seats, the constraint computes as rope or low-extraction tangled_rope: the coordination function (commons preservation) dominates, extraction is the price of that coordination. From the exit_maximizer and proprietary_vendor seats, it computes as snare: the coordination story is cover for extracting their labor/innovation into the commons without compensation. The engine will reveal this divergence. The observer seat should see the hybrid structure clearly — genuine coordination function with asymmetric extraction requiring active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   The software_commons_institution is the primary beneficiary (d near 0.0) — the constraint subsidizes its persistence by foreclosing enclosure. free_software_users are secondary beneficiaries (d ~0.2) — they gain commons access at the cost of constrained license choice. gpl_compliance_community are agenda_setters with mild beneficiary position (d ~0.3) — they administer enforcement but also depend on the commons. individual_exit_maximizers and proprietary_derivative_vendors are payers (d near 0.8-0.9) — they bear the full cost of foreclosed privatization. permissive_license_advocates are excluded (d undefined) — they are not governed by the constraint but are structurally affected by its dominance. The analytical_observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (software enclosure) remains partially live but has mutated. The original threat (vendors proprietary-izing Unix-like systems) is largely dead — the commons won that battle. But new enclosure vectors emerged: SaaS/cloud (running GPL code without distribution triggers), Tivoization (hardware locks preventing user modification), and 'open core' models (commons as loss leader for proprietary tiers). GPLv3 addressed Tivoization and patent threats; AGPL addressed SaaS. The constraint has adapted rather than atrophied — it is not a piton. However, the rising theater_ratio and the contested founding_problem_status signal that the coordination/extraction balance is under active renegotiation. The constraint avoids mandatrophy by evolving its enforcement scope (v2→v3→AGPL), but each evolution increases complexity and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_reading,
    'This constraint is one reading (copyleft_as_commons_reading) of the contested kernel gpl_reciprocity_obligation. How do the sibling readings (copyleft_as_freedom_reading, copyleft_as_restriction_reading) structurally differ in their beneficiary/victim assignments and extractiveness assessments?',
    'Author separate constraint stories for each sibling reading with their own ε, stakeholders, and claimed_type. Compare the three stories'' structural outputs. The kernel context documents the decomposition; the engine does not merge them.',
    'If the three readings produce substantially different classifications (e.g., one computes as rope, one as tangled_rope, one as snare), the kernel is structurally ambiguous — the label ''GPL copyleft'' conceals multiple constraints. This validates the ε-invariance decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Kernel/reading decomposition structure — this story is one of three constraints from one kernel.').

omega_variable(
    enclosure_threat_in_cloud_era,
    'Is the commons enclosure threat still live in the cloud/SaaS era, or has the commons grown resilient enough that copyleft''s extraction cost exceeds its protective benefit?',
    'Empirical study of enclosure events post-2010: measure rate of proprietary forks of GPL projects vs. permissive projects; track adoption of AGPL vs. GPL for cloud services; analyze whether companies avoid GPL code due to copyleft or adopt it despite copyleft.',
    'If enclosure threat is dead, founding_problem_status becomes ''dead'' and the constraint risks mandatrophy (persisting as piton). If threat is live but mutated (SaaS enclosure not addressed by GPLv2), the constraint''s suppression and extractiveness are miscalibrated — it suppresses the wrong exits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enclosure_threat_in_cloud_era, empirical, 'Whether the founding problem persists in mutated form, affecting mandatrophy assessment.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.55) primarily structural (copyright enforcement, legal risk) or internalized (developers self-censoring, companies avoiding GPL due to cultural pressure rather than legal necessity)?',
    'Survey companies that avoid GPL: distinguish ''legal risk'' from ''policy preference'' from ''engineering cost''. Measure GPL avoidance in jurisdictions with weak copyright enforcement vs. strong. Track whether suppression persists after legal threat is removed (e.g., after clean-room reimplementation).',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than the structural measure — the target carries the suppression with them. This would increase effective extraction for payer seats and strengthen snare/tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in license compliance.').

omega_variable(
    commons_as_beneficiary_ontology,
    'Is ''software_commons_institution'' a genuine beneficiary (an entity that collects value) or a vindicated proposition (a state of affairs the constraint preserves)? The schema requires beneficiaries to be actors; the commons may be better modeled as a vindicated_proposition.',
    'Analyze whether the commons ''collects'' value in any sense comparable to an actor. If not, move ''software_commons_institution'' from beneficiaries to vindicated_propositions and identify the actual institutional actors (FSF, GNU Project, Linux Foundation) as beneficiaries/agenda_setters.',
    'If the commons is not an actor, the beneficiary structure changes: the real beneficiaries are the steward institutions and users. This alters directionality derivation and may change the computed classification for agenda_setter seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commons_as_beneficiary_ontology, conceptual, 'Ontological status of the commons as beneficiary vs. vindicated proposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_reciprocity_commons_tr_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl_reciprocity_commons_tr_t1995, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(gpl_reciprocity_commons_tr_t2002, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2002, 0.12).
narrative_ontology:measurement(gpl_reciprocity_commons_tr_t2007, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2007, 0.15).
narrative_ontology:measurement(gpl_reciprocity_commons_tr_t2013, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2013, 0.18).
narrative_ontology:measurement(gpl_reciprocity_commons_tr_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement(gpl_reciprocity_commons_tr_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(gpl_reciprocity_commons_be_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1989, 0.25).
narrative_ontology:measurement(gpl_reciprocity_commons_be_t1995, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1995, 0.32).
narrative_ontology:measurement(gpl_reciprocity_commons_be_t2002, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2002, 0.38).
narrative_ontology:measurement(gpl_reciprocity_commons_be_t2007, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2007, 0.42).
narrative_ontology:measurement(gpl_reciprocity_commons_be_t2013, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2013, 0.45).
narrative_ontology:measurement(gpl_reciprocity_commons_be_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2019, 0.47).
narrative_ontology:measurement(gpl_reciprocity_commons_be_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(gpl_reciprocity_commons_su_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1989, 0.3).
narrative_ontology:measurement(gpl_reciprocity_commons_su_t1995, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(gpl_reciprocity_commons_su_t2002, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2002, 0.45).
narrative_ontology:measurement(gpl_reciprocity_commons_su_t2007, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2007, 0.5).
narrative_ontology:measurement(gpl_reciprocity_commons_su_t2013, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2013, 0.52).
narrative_ontology:measurement(gpl_reciprocity_commons_su_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2019, 0.54).
narrative_ontology:measurement(gpl_reciprocity_commons_su_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.15).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, agpl_network_copyleft_extension).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, lgpl_library_exception).

% DUAL FORMULATION NOTE:
% This story is one of three in the gpl_reciprocity_obligation constraint family. The copyleft_as_freedom_reading centers user freedom as beneficiary and proprietary capture as victim; the copyleft_as_restriction_reading centers developer freedom as victim and has no clear beneficiary (or treats 'license simplicity' as vindicated proposition). The three stories differ in ε (this: 0.48, freedom: ~0.35, restriction: ~0.65) and in stakeholder role assignments. They are linked via affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, institutional, 0.15).
constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
