% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__interface_boundary_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: Interface-Boundary Reading of the GPL Derivative-Work Trigger
 *   domain: legal/software_licensing/open_source_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   gpl_derivative_work_trigger: the interface_boundary_reading, under which
 *   a clean API boundary makes the combination a non-derivative aggregation
 *   even when coupling is tight (dynamic linking, shared address space). The
 *   sibling readings — broad_copyleft_reading (any linking, even dynamic,
 *   creates a derivative work triggering source obligations) and
 *   narrow_linking_permissive_reading (linking is mere aggregation; only
 *   modifications trigger obligations) — are separate constraint files with
 *   their own epsilon values, beneficiaries, and victims; per the
 *   epsilon-invariance principle they are not folded into this story. The
 *   epsilon referent here is the standing arrangement under contest — the de
 *   facto regime in which linking across documented interfaces does not
 *   trigger GPL source obligations for the linking side — assessed by this
 *   reading's own lights, which endorse the arrangement. The claimed type
 *   follows the manifest's expected structural delta: a scaffold that carried
 *   the software industry through its migration from monolithic to modular,
 *   mixed-license architectures, with a sunset character (the reading holds
 *   until definitive adjudication absorbs it into settled law, or until
 *   modular composition becomes so uncontested that the interpretive
 *   protection is moot; the LGPL precedent embeds exactly such an
 *   upgrade-or-relicense condition). The metrics are authored independently
 *   of that claim and describe moderately extractive operation with rising
 *   enforcement intensity.
 *
 * KEY AGENTS:
 *   - proprietary_ecosystem_integrators: Primary beneficiary (powerful/mobile) — collect architectural freedom and market access; pay compliance costs as a secondary burden
 *   - enterprise_software_vendors: Secondary beneficiary (powerful/constrained) — entire product lines rest on the reading; exit is slow
 *   - gpl_copyright_holders: Agenda setter (institutional/identity_locked) — administers the license whose purpose the reading bypasses; harmed despite administering
 *   - copyleft_enforcement_organizations: Challenger-agenda setter (organized/constrained) — litigate the boundary question with limited resources
 *   - gpl_contributors: Primary payer (organized/mobile) — bear reciprocity loss when their work is consumed without source return
 *   - gpl_downstream_users: Secondary payer (moderate/constrained) — lose full-stack source visibility; locked in post-purchase
 *   - dual_license_vendors: Beneficiary via arbitrage (powerful/arbitrage) — ambiguity drives customers to paid licenses
 *   - firmware_security_auditors: Excluded voice (moderate/trapped) — need full-stack source, hold no seat in boundary determinations
 *   - courts_and_judges: Analytical observer (institutional/analytical) — adjudicate piecemeal; no definitive merits ruling exists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.42).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.47).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, scaffold).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "Interface-Boundary Reading of the GPL Derivative-Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "legal/software_licensing/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:has_sunset_clause(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, 'bf3ac85c-f453-4761-81a0-f17b11c212a9').
narrative_ontology:cs_kernel_codification('bf3ac85c-f453-4761-81a0-f17b11c212a9', fixed_text).
narrative_ontology:cs_authority_grounding('bf3ac85c-f453-4761-81a0-f17b11c212a9', practice).
narrative_ontology:cs_interpretation_layer_present('bf3ac85c-f453-4761-81a0-f17b11c212a9').
narrative_ontology:cs_reading_relation('bf3ac85c-f453-4761-81a0-f17b11c212a9', gpl_derivative_work_trigger__broad_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('bf3ac85c-f453-4761-81a0-f17b11c212a9', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_axiom('bf3ac85c-f453-4761-81a0-f17b11c212a9', foundational, clean_api_boundary_precludes_derivation).
narrative_ontology:cs_axiom_status(clean_api_boundary_precludes_derivation, holdable).
narrative_ontology:cs_axiom_grounding('bf3ac85c-f453-4761-81a0-f17b11c212a9', clean_api_boundary_precludes_derivation, conventional).
narrative_ontology:cs_axiom('bf3ac85c-f453-4761-81a0-f17b11c212a9', foundational, runtime_integration_not_dispositive_of_work_identity).
narrative_ontology:cs_axiom_status(runtime_integration_not_dispositive_of_work_identity, holdable).
narrative_ontology:cs_axiom_grounding('bf3ac85c-f453-4761-81a0-f17b11c212a9', runtime_integration_not_dispositive_of_work_identity, conventional).
narrative_ontology:cs_reference_frame('bf3ac85c-f453-4761-81a0-f17b11c212a9', modular_mixed_license_coexistence).
narrative_ontology:cs_drift_state('bf3ac85c-f453-4761-81a0-f17b11c212a9', contemporary_post_vmware_docket, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('bf3ac85c-f453-4761-81a0-f17b11c212a9', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, enterprise_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, gpl_contributors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, gpl_downstream_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, dual_license_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_ecosystem_integrators).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, clean_interface_nonderivation_doctrine).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, modular_architecture_independence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build commercial applications and tools that link against GPL-licensed libraries through their published APIs, shipping closed application layers on top of open cores. They maintain internal boundary documentation and legal review to defend the position that their code aggregates with, rather than derives from, the GPL components. They also bear ongoing compliance and counsel costs. If the reading collapsed, their options are rewriting against permissively licensed substitutes, purchasing commercial licenses, or opening their source.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_ecosystem_integrators, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_ecosystem_integrators, payer).

% Ship appliances, cloud platforms, and embedded products built on GPL kernels and toolchains. Their product legality rests on the interface boundary position, and their stacks are too deep to re-base quickly, so leaving is slower and costlier than it is for smaller integrators. They fund license-compliance teams and settle privately when challenged.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, enterprise_software_vendors, beneficiary,
    powerful, biographical, constrained, global).

% Hold copyright in flagship GPL works, publish the license and official interpretive guidance, and run compliance processes for violations. Their stated purpose is guaranteeing that everyone receiving the software can study and modify the whole working system. The interface boundary position lets linked proprietary layers escape that guarantee, which they read as defeating the license's purpose. Their institutional identity is fused with the copyleft mission; abandoning enforcement would dissolve the institution's reason to exist.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_copyright_holders, agenda_setter,
    institutional, generational, identity_locked, global).

% Bring compliance negotiations and, rarely, litigation to test whether linked proprietary code must be released. They operate on donations and volunteer counsel, so their capacity is small next to the vendors they challenge. Their suits so far have ended in settlements or procedural dismissals rather than rulings on the merits.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, copyleft_enforcement_organizations, agenda_setter,
    organized, biographical, constrained, global).

% Write code accepted into GPL projects on the understanding that improvements remain free for everyone, including competitors. When proprietary products consume their work through stable interfaces without releasing modifications or additions, the expected reciprocity does not arrive. Individually they can redirect effort to permissively licensed projects or paid employment, which erodes their collective leverage over license terms.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_contributors, payer,
    organized, biographical, mobile, global).

% Deploy phones, routers, vehicles, and servers built on GPL cores with closed linked components. Under a broader reading of the license they would receive complete corresponding source for the whole shipped system; under this reading they receive the GPL parts only. Once hardware is purchased, practical switching costs hold them in place regardless of how the license is interpreted.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_downstream_users, payer,
    moderate, biographical, constrained, global).

% Publish the same codebase under the GPL and under paid commercial licenses. Uncertainty about whether linking triggers source obligations pushes risk-averse customers toward the paid option, so unclear boundaries raise commercial-license revenue. They can reprice or relicense quickly as interpretations shift.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, dual_license_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Need complete source for everything running on a device in order to find vulnerabilities and verify patches. Closed linked components leave unauditable regions inside otherwise open stacks. They have no seat in license negotiations, compliance determinations, or the drafting of interface documentation; their objections surface only after incidents.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, firmware_security_auditors, excluded,
    moderate, biographical, trapped, global).

% Decide derivative-work disputes case by case under general copyright doctrine. No major jurisdiction has issued a definitive ruling on whether linking across a clean interface creates a derivative work; decisions remain fragmented across borders and frequently procedural. They see the ecosystem's reliance interests but resolve only the disputes actually brought before them.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, courts_and_judges, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Lets independently licensed software components compose: a common core develops under copyleft while application layers develop under any license, meeting at documented interfaces. It solves the all-or-nothing problem in which strict source-sharing terms would drive vendors off GPL components entirely and split ecosystems into incompatible islands.
% TRANSFER_FUNCTION: Moves architectural freedom and market access from GPL copyright holders to proprietary integrators; moves away from downstream users the guarantee of full-stack source; moves value created in the GPL commons into proprietary products without reciprocal source return.
% ABSENT_VOICES: Firmware security auditors and end users who cannot demand source for linked proprietary components are outside the conversation entirely. Broad-copyleft advocates participate in public discourse but hold no seat in the private compliance negotiations where boundary determinations actually get made.
% DISAPPEARANCE_RATIONALE: If the interface boundary position vanished overnight and the broad reading took hold, thousands of shipping products would instantly carry unmet source-disclosure obligations; vendors would face mass relitigation, forced disclosure, or emergency re-basing onto permissive components; Android-class ecosystems, embedded Linux fleets, and dual-license businesses would reorganize within quarters. The arrangement, not a natural fact, holds the current mixed-licensing world in place.
% FOUNDING_PROBLEM: Strict readings of the GPL's derivative-work trigger made every form of combination legally hazardous: vendors faced a choice between opening entire products or avoiding GPL components altogether, which threatened both the license's adoption and the growth of shared infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Parties outside the benefiting set attest the founding problem was real: contemporaneous 1990s industry commentary documents vendor avoidance of GPL libraries, and the FSF itself created the LGPL with an explicit library-specific compromise, acknowledging the all-or-nothing problem from inside the copyleft movement. Whether the problem remains live is disputed: integrators attest it persists as long as architectures evolve; copyleft organizations attest this reading solved it by conceding the store. Academic copyright scholarship on abstraction-filtration-comparison corroborates the problem statement without endorsing this reading's resolution of it.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).
:- end_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the reading's own lights endorse the arrangement: it delivers large coordination value at a cost its holders consider legitimate, while conceding real losses — contributor reciprocity and downstream source visibility — that even sympathetic accounts acknowledge. Suppression (0.47) is a raw, unscaled structural property: the reading needs comparatively little coercion to persist because industry interest aligns with it, but it does require keeping the rival broad interpretation from gaining practical force, backed by an enforcement apparatus that matured over the interval. Theater ratio (0.26) reflects a functional criterion with growing gaming: formally clean boundaries sometimes wrap deep functional dependence, and compliance documentation shades into ritual. Accessibility collapse is low (0.30) — permissive substitutes, dual licensing, commercial terms, and clean-room rewrites remain genuinely available. Resistance is substantial (0.60): organized copyleft enforcement has contested the reading continuously for two decades. The measurement series run on one shared eight-point grid (t=0..35) with all three metrics authored at every point; trajectories are monotonic, not cyclical — enforcement capacity built up stepwise (violations reporting circa t=11, the BusyBox litigation wave circa t=16-18, the VMware/Hellwig docket circa t=24-28) without oscillation, so suppression_requirement is tracked because enforcement-capacity change is genuinely part of this story. End-state measurement values equal the base_properties scalars by construction.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the proprietary integrator seat the arrangement looks like near-pure coordination: it solves composition, costs them only compliance overhead, and their mobile exit keeps effective burden low. From the contributor and downstream-user seats the same structure operates as one-way transfer: work and visibility flow out, reciprocity does not flow back. From the copyright-holder seat — nominally the agenda setter — the reading registers as defeat of institutional purpose, a reminder that administering an arrangement and benefiting from it are different facts. The engine computes these per-seat classifications from power, exit, and role data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (integrators, enterprise vendors, dual licensors) sit near the beneficiary end of directionality: the arrangement subsidizes their product legality, and arbitrage-grade or mobile exit damps their effective burden further. Victims (contributors, downstream users) sit near the target end: contributors bear diffuse reciprocity loss with only individually-mobile exit, and users are constrained post-purchase. The paradox seat is gpl_copyright_holders: as agenda setter the derivation might read them as advantaged, but their identity_locked exit and the arrangement's defeat of their license purpose place their experienced position near the target end — administration without benefit. Firmware security auditors, excluded and trapped, bear costs with no seat at all. Larger spatial scope (global reliance) amplifies verification difficulty for any would-be challenger, which is part of why enforcement stays negotiated and private rather than judicial.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification keeps the reading's transitional justification visible and prevents two mislabels. Reading it as pure rope would erase the asymmetric transfer — the reciprocity loss and the foreclosed full-source guarantee are real costs riding on real coordination. Reading it as pure snare would erase the genuine composition function that made mixed-license ecosystems possible at all. The sunset declaration is load-bearing: the reading's defenders themselves frame it as interim doctrine pending definitive adjudication, and the LGPL precedent shows the copyleft tradition embedding explicit upgrade-or-sunset conditions for exactly this boundary problem. The sunset_clause_realism omega guards the flank: if no jurisdiction ever resolves the merits and reliance never declines, the transitional story is rhetoric and the structure is a steady-state hybrid demanding reclassification. Founding_problem_status is contested rather than dead, so no zombie mismatch fires; the arrangement's persistence tracks a dispute about whether its problem is solved, not inertial outliving of an obviously dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_gpl_trigger,
    'This constraint is one reading of the kernel gpl_derivative_work_trigger; would instantiating broad_copyleft_reading or narrow_linking_permissive_reading instead change the victim set, beneficiary set, and computed type?',
    'Author the sibling files and compare computed classifications across the kernel family; divergence in victim sets (full-stack source expectants present vs. absent) marks where the readings structurally part.',
    'Under the broad reading, victims expand to every user of dynamically linked proprietary stacks and epsilon rises sharply; under the narrow reading, victims shrink toward modifiers-only and epsilon falls toward rope territory. Every classification in this file is conditional on the interface-boundary premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_gpl_trigger, conceptual, 'Committer structure: which reading of the GPL trigger kernel this story instantiates and what sibling readings would structurally change.').

omega_variable(
    boundary_cleanliness_operability,
    'Can a clean API boundary be operationalized objectively, or is the criterion gameable — superficially clean interfaces wrapped around deep functional dependence?',
    'Comparative audit of shipped boundary documentation against actual build-time and runtime dependencies; measure how often formally clean boundaries conceal header-level, macro-level, or shared-data-structure coupling.',
    'If systematically gameable, theater_ratio is understated and effective extraction runs above the authored epsilon; in practice the reading degenerates toward the narrow permissive outcome regardless of its stated conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_cleanliness_operability, empirical, 'Operational stability of the reading''s central criterion.').

omega_variable(
    sunset_clause_realism,
    'Does this reading genuinely carry a transition endpoint — absorption into definitive case law or obsolescence once modular composition is uncontested — or is the scaffold characterization a steady-state rule wearing transitional clothing?',
    'Track whether any jurisdiction issues a merits ruling adopting or rejecting the reading, and whether reliance on the reading declines as modular patterns become the default; absence of both after another decade indicates steady state.',
    'If steady state, reclassification from scaffold toward tangled_rope (real coordination plus asymmetric reciprocity loss) is warranted, and the sunset declaration is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_realism, conceptual, 'Whether the reading''s transitional justification is real or rhetorical.').

omega_variable(
    contributor_reciprocity_loss,
    'Do GPL contributors experience net harm from uncompensated API consumption of their work, or does ecosystem growth compensate them through reputation, employment, and improved shared infrastructure?',
    'Longitudinal survey and career-outcome data comparing contributors to heavily API-consumed copyleft projects against matched permissively licensed projects.',
    'If compensation dominates, the victim declarations overstate harm and epsilon drops; if loss dominates, the reading''s extraction is understated and the victim set broadens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contributor_reciprocity_loss, empirical, 'Net welfare effect on the contributor seat.').

omega_variable(
    merits_adjudication_unresolved,
    'The leading challenge to this reading ended on procedural grounds without reaching whether the defendant''s closed modules were derivative of the GPL kernel; do the reading''s prospects survive an actual merits ruling in a major jurisdiction?',
    'Monitor and analyze the next merits-stage linking case in German, US, or Dutch dockets; pre-register predictions about reliance behavior under adverse and favorable outcomes.',
    'An adverse merits ruling collapses the reliance base abruptly — suppression spikes, beneficiaries re-base, and the scaffold''s sunset arrives early; a favorable ruling hardens the reading into settled law, ending its transitional phase from the other direction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merits_adjudication_unresolved, empirical, 'Judicial untestedness of the reading''s core premise.').

omega_variable(
    authority_grounding_underdetermination,
    'Is the reading''s authority grounded in practitioner custom (compliance practice treating clean boundaries as sufficient) or in lineage (published interpretive tradition of the license''s stewards)? The two framings yield different commitment-system classifications.',
    'Trace whose interpretive statements vendors actually cite in compliance documentation: internal counsel custom and industry practice versus the stewards'' published positions.',
    'Practice-grounding supports the declared cs_structure; lineage-grounding would bind the reading to an authority that officially rejects it for dynamic linking, destabilizing the interpretation layer and shifting the drift assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_underdetermination, conceptual, 'Framing under-determination in the reading''s authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(gpl__tr_t35, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 35, 0.26).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 25, 0.39).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(gpl__be_t35, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 35, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 15, 0.33).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(gpl__su_t25, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 25, 0.44).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(gpl__su_t35, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 35, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the GPL linking question' decomposes into three structurally distinct constraints forming the gpl_derivative_work_trigger family: broad_copyleft_reading (any linking creates derivation; highest epsilon, victims include all dynamic-linking users), this interface_boundary_reading (boundary quality decides work identity; moderate epsilon, scaffold-shaped), and narrow_linking_permissive_reading (linking is always aggregation; lowest epsilon). The broad reading is upstream: it is the original steward position that the other two readings react against, and its texts are cited as evidence in contests over both siblings. Each file carries its own epsilon, beneficiaries, and victims; mutual affects_constraints edges keep the family connected for contamination propagation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
