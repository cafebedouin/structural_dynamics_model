% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__commons_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software Control as Negotiated Commons Governance
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the commons reading of the
 *   software-control-legitimacy kernel: software governance is treated as a
 *   negotiated collective-management problem over shared digital
 *   infrastructure, rejecting both the free-software-imperative claim (that
 *   any control beyond the end user is illegitimate) and the property-rights
 *   claim (that creators hold unrestricted authority). Governance bodies —
 *   foundations, standards consortia, maintainer councils — administer
 *   negotiated rules (licensing frameworks, CLAs, contribution requirements,
 *   trademark policy) that bind participants regardless of their prior
 *   ideological commitments. The structural delta from the sibling readings:
 *   both absolutist positions enter this story's victim set, because commons
 *   governance structurally denies either position a totalizing win; the
 *   beneficiary is the organized stakeholder community that can sustain
 *   participation in the negotiation, not any single ideological camp.
 *
 * KEY AGENTS:
 *   - governance_body_participants: agenda_setter (organized/constrained) — administers negotiated rules
 *   - stakeholder_communities: beneficiary (organized/constrained) — gets predictable negotiated access
 *   - downstream_integrators: beneficiary/payer (moderate/constrained) — builds on the commons, pays compliance costs
 *   - free_software_absolutists: excluded/payer (moderate/trapped) — denied ethical veto
 *   - proprietary_rights_absolutists: excluded/payer (powerful/constrained) — denied unrestricted property claim
 *   - unrepresented_end_users: excluded (powerless/trapped) — no governance seat at all
 *   - core_maintainers: payer/agenda_setter (moderate/constrained) — does the labor, partially sets direction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.38).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.42).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Negotiated Commons Governance").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, '26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d').
narrative_ontology:cs_kernel_codification('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', distributed).
narrative_ontology:cs_authority_grounding('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', distributed).
narrative_ontology:cs_reading_relation('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', foundational, control_authority_is_negotiated_not_unilateral).
narrative_ontology:cs_axiom_status(control_authority_is_negotiated_not_unilateral, holdable).
narrative_ontology:cs_axiom_grounding('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', control_authority_is_negotiated_not_unilateral, conventional).
narrative_ontology:cs_axiom('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', foundational, stakeholder_participation_legitimates_governance_outcomes).
narrative_ontology:cs_axiom_status(stakeholder_participation_legitimates_governance_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', stakeholder_participation_legitimates_governance_outcomes, instrumental).
narrative_ontology:cs_axiom('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', secondary, no_single_party_holds_prior_veto_over_shared_infrastructure).
narrative_ontology:cs_axiom_status(no_single_party_holds_prior_veto_over_shared_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', no_single_party_holds_prior_veto_over_shared_infrastructure, conventional).
narrative_ontology:cs_reference_frame('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', polycentric_negotiated_governance).
narrative_ontology:cs_drift_state('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', post_foundation_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('26b70d9a-faf8-4a27-a0c9-1b79b4c3f23d', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, governance_body_participants).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, downstream_integrators).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, free_software_absolutists).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, proprietary_rights_absolutists).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, unrepresented_end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, downstream_integrators).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, core_maintainers).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, polycentric_governance_viability).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, commons_management_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit on foundations, steering committees, or standards bodies that negotiate license terms, contribution rules, and governance charters for shared codebases and infrastructure (e.g. Linux Foundation-style bodies, W3C-style consortia). They set the rules of participation and adjudicate disputes between contributors, corporate sponsors, and users. Their authority rests on continued buy-in from the parties they govern, not on unilateral property claim or ideological mandate.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, governance_body_participants, agenda_setter,
    organized, generational, constrained, global).

% Corporate sponsors, maintainer collectives, and downstream user organizations that get predictable, negotiated access to shared infrastructure without needing to win either a purity contest or a licensing war. They contribute resources (code, funding, governance labor) in exchange for a voice in how the commons is managed. Exit means forking or abandoning the shared project, which is costly but not impossible.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_communities, beneficiary,
    organized, generational, constrained, global).

% Companies and developers who build products on top of commons-governed infrastructure. They benefit from stable, negotiated rules rather than either unrestricted freedom (which can mean unstable forks and no support guarantees) or rigid proprietary lock-in. They also pay through mandatory compliance with governance-imposed contribution requirements, licensing fees to sustain the commons, or CLA (contributor license agreement) obligations.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, downstream_integrators, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, downstream_integrators, payer).

% Developers and advocates who hold that all software control by anyone other than the end user is ethically illegitimate. Under commons governance, their position is treated as one negotiating stance among several rather than a binding ethical floor — governance bodies routinely approve licenses and restrictions (trademark controls, CLA assignment, dual licensing) that the absolutist position would reject outright. They are structurally denied a veto; their participation is invited but their conclusions are not privileged.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, free_software_absolutists, excluded,
    moderate, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, free_software_absolutists, payer).

% Firms and rights-holders who hold that creators have unrestricted authority to control use, modification, and distribution of their software as pure property. Commons governance structurally overrides this: participation in shared infrastructure requires accepting negotiated terms (copyleft obligations, patent non-assertion pledges, governance oversight of roadmap) that a pure property claim would reject. Their exit option — building fully proprietary, non-participating alternatives — is real but increasingly costly as commons-governed infrastructure becomes the default substrate (e.g. Linux kernel, core web standards).
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, proprietary_rights_absolutists, excluded,
    powerful, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, proprietary_rights_absolutists, payer).

% Individual users of software built atop commons-governed infrastructure who have no seat at any governance table — not the foundation board, not the corporate sponsor negotiations, not the maintainer councils. They experience the outcomes of governance decisions (license changes, deprecations, feature removals for compliance reasons) without having consented to the negotiated framework that produced them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, unrepresented_end_users, excluded,
    powerless, biographical, trapped, global).

% Individual engineers who do the ongoing technical labor of maintaining shared infrastructure under governance rules set largely by better-resourced corporate stakeholders. They bear the burnout and unpaid-labor costs of the commons arrangement even as they participate in setting technical direction; their exit (walking away from a critical project) is possible but often blocked by reputational and community-obligation pressure.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, core_maintainers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, core_maintainers, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shared digital infrastructure (kernels, protocols, core libraries, standards) requires ongoing maintenance, security response, and directional decisions that no single party can unilaterally provide at the scale the infrastructure now operates. Commons governance solves the genuine collective-action problem of coordinating investment, contribution, and rule-setting across parties with divergent interests who nonetheless depend on the same shared substrate.
% TRANSFER_FUNCTION: Moves governance authority and negotiating leverage toward organized, resourced participants (corporate sponsors, foundation boards, veteran maintainer collectives) and away from both unaffiliated individual users and parties holding absolutist ideological or property positions who decline or are unable to participate in the negotiated framework. Contribution labor and compliance costs flow from individual maintainers and smaller downstream integrators toward the collective infrastructure; governance voice flows toward whoever can sustain a seat at the table.
% ABSENT_VOICES: Unrepresented end users have no governance seat and are not consulted on license changes or roadmap decisions that materially affect them. Free software absolutists and proprietary rights absolutists are formally invited to participate but structurally cannot win outright — the governance process is designed to produce negotiated compromise, which by construction denies either absolutist position the total victory their framework demands.
% DISAPPEARANCE_RATIONALE: Stakeholder communities and governance body participants would say the world rearranges catastrophically — critical infrastructure (kernels, protocols, security-critical libraries) would fragment into incompatible forks or revert to purely proprietary or purely libertarian-freedom models, each with their own severe costs. Absolutists on both sides would say the world improves for their position specifically, since the negotiated middle ground is exactly what currently blocks their preferred resolution. The verdict depends on which seat is asked, which is itself evidence the governance layer is doing real adjudicative work rather than merely ratifying a foregone consensus.
% FOUNDING_PROBLEM: As shared software infrastructure became foundational to commercial and civic life (kernels, web standards, cryptographic libraries), neither pure freedom-based governance (unpredictable forking, no enforceable quality or security floor, no mechanism to fund maintenance) nor pure property-based governance (fragmentation into incompatible proprietary silos, no interoperability, underinvestment in shared foundations) could produce stable, secure, adequately maintained infrastructure that multiple competing commercial and civic actors could all rely on.
% FOUNDING_PROBLEM_CORROBORATION: Independent security researchers and academic commons-governance scholars (e.g. Ostrom-tradition institutional economists studying digital commons) attest that unmanaged shared infrastructure suffers real maintenance and security underinvestment absent negotiated governance — this is not solely asserted by the foundations and corporate sponsors who benefit from administering it. However, no fully independent audit exists of whether the SPECIFIC governance bodies now in place solve the problem proportionately to the authority and compliance burden they extract; that gap is exactly the tangled-rope signature.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, contested).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).
:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects a genuine but moderate transfer: governance bodies extract compliance labor and negotiating deference from participants who would otherwise hold absolutist positions, but the extraction funds real coordination (security maintenance, interoperability) rather than pure rent capture — this is why the ε sits well below a snare-level reading and is variable depending on the specific commons' rules, exactly as the kernel context predicts. Suppression (0.42) is moderate: neither absolutist camp is criminalized or forcibly barred, but their totalizing claims are structurally unable to prevail within the negotiated framework, which is a real (if soft) form of suppression of alternatives. Theater ratio (0.28) is present but not dominant — governance bodies do real adjudicative work, though some compliance ritual (CLA signing, code-of-conduct enforcement theater) has grown over the measured interval. Resistance (0.55) is elevated because both absolutist camps actively contest the legitimacy of the negotiated middle ground on principled grounds, not merely self-interest.
 *
 * DIRECTIONALITY LOGIC:
 *   Governance body participants and stakeholder communities sit near the beneficiary end: they set or ratify the rules and receive predictable, negotiated access in return for participation costs they can absorb. Free software absolutists and proprietary rights absolutists sit toward the target end DESPITE having real resources (the proprietary side is often powerful) because the constraint specifically extracts from their ability to prevail on principle — the commons reading structurally requires that neither win outright, which is itself the cost imposed on them. Unrepresented end users sit at the extreme target end: powerless, trapped, and structurally absent from the negotiation entirely, bearing downstream consequences of decisions made without them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unmanaged infrastructure underinvestment and fragmentation) remains live per independent corroboration, which argues against pure mandatrophy — this is not simply a dead mandate propped up by institutional inertia. But the founding_problem_status is qualified: the corroboration explicitly notes no independent audit confirms the CURRENT governance bodies solve the problem proportionately to the authority and compliance burden extracted. This is the tangled-rope signature precisely: real coordination function (verified) plus asymmetric extraction (unaudited, likely present) riding the same structure. Classifying this as tangled_rope rather than rope prevents the common error of treating 'there is a genuine coordination problem here' as sufficient to certify the current governance arrangement as non-extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_reading_versus_property_rights_reading,
    'Is the negotiated-governance framing itself a legitimate resolution of the control question, or is it a soft capture mechanism by which organized/well-resourced actors (corporate foundation sponsors) launder property-like control through the language of collective management, without ever conceding the property_rights_reading''s core premise that creators hold prior authority?',
    'Track whether governance-body voting power and rule-setting authority correlate with prior ownership/investment stakes (property-rights logic persisting under commons branding) versus correlating with contribution volume or user-affected-population (genuine commons logic). Audit specific foundation charters for veto rights held by founding corporate sponsors.',
    'If governance authority tracks prior ownership stakes, this reading collapses structurally into a disguised property_rights_reading and the commons framing is theater; if it tracks contribution/impact, the commons_reading is structurally distinct as claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_reading_versus_property_rights_reading, conceptual, 'Whether commons governance is genuinely distinct from property-rights control or a relabeling of it.').

omega_variable(
    commons_reading_versus_freedom_imperative_reading,
    'Does treating the freedom-imperative position as one negotiating stance among several (rather than a binding ethical floor) itself constitute a substantive rejection of the freedom_imperative_reading''s core premise, or merely a procedural bracketing that leaves the ethical question unresolved?',
    'Examine whether governance bodies that adopt commons framing ever approve outcomes (e.g. permissive relicensing, DRM integration, telemetry requirements) that the freedom_imperative_reading would deem categorically impermissible — if so, the commons reading is substantively, not just procedurally, incompatible with the freedom imperative on those points.',
    'Determines whether commons_reading and freedom_imperative_reading are in a forecloses relationship on specific governance outcomes even though the two readings coexist as general orientations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_reading_versus_freedom_imperative_reading, conceptual, 'Whether commons governance procedurally brackets or substantively overrides the freedom-imperative ethical claim.').

omega_variable(
    variable_epsilon_across_commons_instances,
    'Given that the expected structural delta explicitly notes ''variable ε depending on commons rules,'' what specific governance features (voting weight formulas, CLA terms, veto structures, fork-friendliness) predict whether a given commons-governed project sits nearer the rope end or the snare end of the tangled-rope spectrum?',
    'Comparative case study across major commons-governed infrastructure projects (Linux kernel, Apache Foundation projects, W3C standards, Rust language governance) coding for governance-rule variables and correlating with independently observed extraction indicators (maintainer burnout rates, corporate-vs-community decision outcomes, fork frequency and success rate).',
    'Would allow this story''s ε (0.38) to be understood as a corpus-average estimate rather than a fixed universal value — individual commons instances could be authored as separate, more specific constraint stories with their own ε once this variable is resolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(variable_epsilon_across_commons_instances, empirical, 'The story''s ε is an aggregate estimate across a genuinely heterogeneous class of commons governance arrangements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(soft_tr_t4, software_control_legitimacy__commons_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__commons_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(soft_tr_t12, software_control_legitimacy__commons_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__commons_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__commons_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__commons_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(soft_be_t4, software_control_legitimacy__commons_reading, base_extractiveness, 4, 0.27).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__commons_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(soft_be_t12, software_control_legitimacy__commons_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__commons_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__commons_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__commons_reading, base_extractiveness, 24, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(soft_su_t4, software_control_legitimacy__commons_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(soft_su_t8, software_control_legitimacy__commons_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(soft_su_t12, software_control_legitimacy__commons_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(soft_su_t16, software_control_legitimacy__commons_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__commons_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__commons_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__commons_reading, 0.15).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the software_control_legitimacy kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: freedom_imperative_reading (control as absolute user-rights violation), pragmatic_openness_reading (control as a neutral methodology choice), property_rights_reading (control as unrestricted creator property), and this commons_reading (control as negotiated collective governance). Each reading has its own beneficiary/victim structure and its own ε; they are linked here rather than merged because attempting to average or parameterize across them would violate DP-001 (ε-invariance) — the commons_reading's ε (0.38, moderate, tangled_rope) is not commensurable with what either absolutist sibling would report for the same underlying software, because each reading disputes what the 'underlying software' governance fact even IS.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
