% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GNU GPL Reciprocity Obligation (Freedom-Preservation Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested GPL reciprocity
 *   kernel: the freedom-preservation reading, under which the GPL's viral
 *   clause exists to guarantee that no user's access to source and
 *   modification rights can be closed off by a downstream proprietary
 *   integrator. Under this reading, the beneficiary class is downstream users
 *   (whose freedoms are structurally protected across all future forks) and
 *   the victim class is proprietary integrators and resource-constrained
 *   commercial derivative developers (who are barred from closing derivative
 *   source). This is a distinct constraint from the restriction reading (same
 *   clause, victim framed as the primary lens) and the commons reading (same
 *   clause, framed around collective infrastructure preservation rather than
 *   individual user freedom) — each is authored as its own file with its own
 *   ε, per the ε-invariance principle. Do not average across these readings;
 *   they are siblings, not observation angles on one thing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.62).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GNU GPL Reciprocity Obligation (Freedom-Preservation Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'b7c4601b-6379-436c-bb68-64ac30ef9d49').
narrative_ontology:cs_kernel_codification('b7c4601b-6379-436c-bb68-64ac30ef9d49', fixed_text).
narrative_ontology:cs_authority_grounding('b7c4601b-6379-436c-bb68-64ac30ef9d49', lineage).
narrative_ontology:cs_interpretation_layer_present('b7c4601b-6379-436c-bb68-64ac30ef9d49').
narrative_ontology:cs_reading_relation('b7c4601b-6379-436c-bb68-64ac30ef9d49', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7c4601b-6379-436c-bb68-64ac30ef9d49', gpl_reciprocity_obligation__copyleft_as_commons_reading, influences).
narrative_ontology:cs_axiom('b7c4601b-6379-436c-bb68-64ac30ef9d49', foundational, user_derivative_access_is_inalienable_freedom).
narrative_ontology:cs_axiom_status(user_derivative_access_is_inalienable_freedom, holdable).
narrative_ontology:cs_axiom_grounding('b7c4601b-6379-436c-bb68-64ac30ef9d49', user_derivative_access_is_inalienable_freedom, deontological).
narrative_ontology:cs_axiom('b7c4601b-6379-436c-bb68-64ac30ef9d49', secondary, reciprocity_obligation_protects_rather_than_restricts).
narrative_ontology:cs_axiom_status(reciprocity_obligation_protects_rather_than_restricts, holdable).
narrative_ontology:cs_axiom_grounding('b7c4601b-6379-436c-bb68-64ac30ef9d49', reciprocity_obligation_protects_rather_than_restricts, conventional).
narrative_ontology:cs_reference_frame('b7c4601b-6379-436c-bb68-64ac30ef9d49', free_software_four_freedoms_doctrine).
narrative_ontology:cs_drift_state('b7c4601b-6379-436c-bb68-64ac30ef9d49', post_saas_cloud_delivery_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b7c4601b-6379-436c-bb68-64ac30ef9d49', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_licensed_project_maintainers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_derivative_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive software along with the guaranteed right to inspect, modify, and redistribute its source. Because any derivative work must carry the same guarantee forward, a user who receives a GPL-licensed program today can trust that neither the original author nor any downstream commercial integrator can strip that right from a later version reaching them. Their exit option if a vendor tries to lock down a fork is to use the still-available free version or fork it themselves — an option the license structurally preserves.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    powerless, generational, constrained, global).

% Choose the GPL for their projects specifically to bind all future contributors and redistributors to reciprocal disclosure. They administer the license terms, defend them through enforcement bodies (e.g. Software Freedom Conservancy-style actions), and treat the reciprocity clause as the mechanism that keeps their contributed labor from being captured into a closed fork they cannot benefit from.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_licensed_project_maintainers, agenda_setter,
    organized, civilizational, mobile, global).

% Want to incorporate GPL-licensed code into a commercial product without releasing their own proprietary additions as source. The license forecloses this: any distribution of a combined/derivative work must be released under the GPL in full source form. Their options are to avoid GPL code entirely, negotiate a separate commercial license from the copyright holder (if offered), or isolate GPL components behind a boundary the license's derivative-work test respects. For architectures where isolation is impractical, this is a hard bar to their preferred business model.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Smaller firms and independent developers who build on GPL code and would like to monetize closed enhancements without the resources to negotiate dual-licensing deals available to larger players. They bear the reciprocity obligation without the bargaining leverage that lets bigger firms find workarounds, so the constraint lands on them with less flexibility than on well-resourced proprietary integrators.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_derivative_developers, payer,
    moderate, biographical, constrained, national).

% Prefer MIT/BSD-style licensing that imposes no reciprocity obligation and argue that user freedom is better served by maximizing adoption, including proprietary adoption, rather than by restricting who may build on the code. They are not opposed within the GPL ecosystem itself — they simply build elsewhere — so their objection to the freedom-reading's premises rarely surfaces inside GPL project governance.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, permissive_license_advocates, excluded,
    organized, generational, mobile, global).

% Drafts and stewards the GPL text, articulates the freedom-preservation rationale publicly, and coordinates enforcement guidance. Sits both as the analytical voice explaining why the mechanism exists and as an agenda-setting body whose institutional mission depends on the reciprocity clause continuing to be read as freedom-protective rather than merely restrictive.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_foundation, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_foundation, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of contributed labor being captured: without a reciprocity requirement, improvements made by a distributed community of volunteer and paid contributors could be forked into a closed proprietary product that gives nothing back, discouraging future contribution. The GPL's viral clause coordinates many independent contributors' willingness to keep contributing by guaranteeing none of their labor can be enclosed downstream.
% TRANSFER_FUNCTION: Moves the option to keep modifications private away from anyone who distributes a derivative of GPL code, and moves the guarantee of continued source access toward every recipient of the software, present and future. No money changes hands directly; what transfers is a right (to close the source) taken from distributors and a right (to inspect and modify) preserved for users.
% ABSENT_VOICES: Permissive-license advocates who believe reciprocity itself is an unnecessary restriction on freedom are not present inside GPL project governance — they simply do not use GPL code, so their disagreement with the freedom-reading's core premise is structurally exported rather than debated within the constraint's own forums.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished overnight (e.g., all GPL projects relicensed permissively), proprietary vendors could immediately fork, close, and sell derivatives without publishing source. Some maintainers predict a wave of proprietary capture of formerly-copyleft codebases within a few release cycles; the user guarantee of continued access to modifications would no longer be enforceable, only voluntary.
% FOUNDING_PROBLEM: In the 1980s, software increasingly shipped without source code, and modifications made by users or small firms to shared tools could be locked away in proprietary forks with no obligation to share improvements back — undermining the collaborative development model the free software movement wanted to sustain.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and GPL maintainers attest the problem remains live, citing ongoing enforcement actions against embedded-device vendors who ship modified GPL code without source. Independent industry analysts and permissive-license advocates outside the GPL ecosystem note that modern platform economics (SaaS, cloud APIs) have partly routed around the mechanism the GPL targets, since network-delivered software often escapes the distribution trigger the license depends on — suggesting the founding problem has partially shifted rather than remaining fully live in its original form.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28) because the reciprocity requirement does not extract wealth or labor from proprietary integrators in the traditional rent-seeking sense — it withholds a business-model option (closed redistribution) rather than transferring value away from them. Suppression is comparatively high (0.62) because the mechanism is genuinely coercive with respect to licensing alternatives: once a project is GPL-licensed and has accreted many contributors under that license, relicensing permissively requires unanimous consent that is often practically unobtainable, which suppresses the alternative-licensing option even for maintainers who might prefer it later. Theater ratio is low (0.12) — enforcement actions (source-disclosure suits) are functionally real, not performative. Resistance (0.55) reflects the sustained pushback from proprietary integrators and dual-licensing negotiations, which is real active resistance, not passive acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users are the structural beneficiaries: the clause exists to protect a right they hold merely by receiving the software, and it costs them nothing to exercise. Proprietary integrators are the structural targets: the clause is precisely engineered to foreclose their preferred exit (closing derivative source), and their exit options are constrained to isolation architectures or license negotiation. Commercial derivative developers occupy the target role too, but with less bargaining power than large integrators, making the same constraint bite harder on them despite an identical formal obligation — this is why they are listed separately with `moderate` power rather than folded into `proprietary_integrators`.
 *
 * MANDATROPHY ANALYSIS:
 *   The freedom-preservation reading resists mandatrophy because its founding problem (proprietary enclosure of collaboratively-developed code) remains partially live: embedded-device vendors are still sued for GPL violations decades after the license's founding, which is direct evidence the mechanism still does real work rather than persisting as pure inertia. Where the founding problem has shifted (SaaS delivery routing around the distribution trigger), the reading itself does not silently expand to cover it — that gap is exactly what motivates parallel licenses like the AGPL, a separate constraint, not a stretching of this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_vs_restriction_framing_primacy,
    'Is the reciprocity clause''s essential nature best captured by framing it as protecting user freedom (this reading), constraining integrator business models (sibling reading), or preserving a collective commons (sibling reading) — or is this a genuine three-way indeterminacy with no fact of the matter about which framing is primary?',
    'There may be no empirical resolution: the three framings describe the same legal mechanism from three different value-standpoints. Partial evidence could come from tracing which framing FSF founders and early GPL drafters (Stallman''s stated intent) treated as primary versus which framing dominates in litigation outcomes and legal scholarship today.',
    'If the freedom framing is shown to be a post-hoc justification layered onto a mechanism originally designed primarily as an anti-enclosure device for the commons (the commons reading), this reading''s beneficiary-centered story would be revealed as narrower than the clause''s actual design intent, without changing the clause''s ε — but reallocating which constraint file is ''upstream'' in the network.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(freedom_vs_restriction_framing_primacy, conceptual, 'Whether this reading''s individual-freedom frame is the mechanism''s essential nature or one of several coequal, contested framings of the same textual kernel.').

omega_variable(
    user_freedom_beneficiary_diffuseness,
    'Downstream users are named as the beneficiary class, but most end users never exercise the inspection/modification rights the GPL preserves for them — does a right that is structurally guaranteed but rarely exercised still constitute a genuine benefit for directionality purposes, or is the beneficiary class better described as a narrow band of technically capable users and forks-in-waiting?',
    'Survey data on actual exercise of GPL-granted rights (fork rates, source-inspection rates) across representative GPL-licensed projects, compared against the size of the nominal user population.',
    'If exercised rights are concentrated in a small technical minority, the ''beneficiary'' class for directionality purposes may be narrower and more organized than ''downstream_users'' suggests, which would shift the derived directionality for that stakeholder group toward a less purely-beneficiary position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_freedom_beneficiary_diffuseness, empirical, 'Whether the broad downstream-user beneficiary class overstates who actually benefits from the preserved freedoms.').

omega_variable(
    relicensing_lockin_suppression_mechanism,
    'Is the high suppression score (0.62) attributable mainly to the reciprocity clause''s own design, or to the practical unanimous-consent barrier to relicensing that emerges once a project has many contributors — a separate, somewhat contingent institutional fact layered on top of the license text?',
    'Compare suppression levels across GPL projects with centralized copyright assignment (where relicensing is unilaterally possible, e.g. FSF-assigned projects) versus projects with distributed contributor ownership (where relicensing requires tracking down every contributor).',
    'If suppression is mostly attributable to distributed-ownership lock-in rather than the reciprocity clause itself, this constraint''s suppression score may be partly measuring a downstream institutional artifact rather than the kernel obligation, suggesting a further decomposition (a fourth sibling: the copyright-assignment/relicensing-lockin constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relicensing_lockin_suppression_mechanism, empirical, 'Whether measured suppression reflects the licensing clause itself or a separable contributor-consent lock-in mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 1989, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl__tr_t1996, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1996, 0.06).
narrative_ontology:measurement(gpl__tr_t2003, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2003, 0.08).
narrative_ontology:measurement(gpl__tr_t2010, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(gpl__tr_t2018, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2018, 0.11).
narrative_ontology:measurement(gpl__tr_t2025, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1989, 0.15).
narrative_ontology:measurement(gpl__be_t1996, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1996, 0.18).
narrative_ontology:measurement(gpl__be_t2003, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2003, 0.22).
narrative_ontology:measurement(gpl__be_t2010, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(gpl__be_t2018, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2018, 0.27).
narrative_ontology:measurement(gpl__be_t2025, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1989, 0.45).
narrative_ontology:measurement(gpl__su_t1996, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1996, 0.5).
narrative_ontology:measurement(gpl__su_t2003, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement(gpl__su_t2010, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(gpl__su_t2018, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2018, 0.61).
narrative_ontology:measurement(gpl__su_t2025, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.1).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gpl_reciprocity_obligation kernel, sharing the identical GPL derivative-work text. copyleft_as_restriction_reading frames the same clause primarily around the business-model constraint imposed on proprietary integrators (victim-centered framing). copyleft_as_commons_reading frames it around ecosystem-level commons preservation (institutional-technology framing). This file frames it around individual user-freedom preservation (beneficiary-centered framing). All three are linked bidirectionally; none is authored as more 'true' than the others — each has its own stable ε, beneficiary/victim structure, and claimed type per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
