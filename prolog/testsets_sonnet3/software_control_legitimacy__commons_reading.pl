% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software Control as Negotiated Commons Governance
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the commons reading of the
 *   software-control-legitimacy kernel: software control is neither a pure
 *   freedom claim nor a pure property claim but a governance question
 *   requiring negotiated collective management of shared infrastructure.
 *   Under this reading, both the freedom-imperative position and the
 *   property-rights position are structurally denied unilateral authority —
 *   they enter the victim set not because the commons reading extracts from
 *   them economically, but because it denies them the veto/control each
 *   absolutism claims as a matter of principle. The beneficiaries are the
 *   stakeholder communities and governance bodies empowered to negotiate
 *   terms on everyone's behalf. Extractiveness is moderate (0.44) and rising
 *   modestly over the interval as governance bodies accumulate process
 *   overhead and incumbent influence; this is a tangled_rope precisely
 *   because the coordination function (avoiding both fragmentation and
 *   proprietary lock-in) is real AND the negotiated bodies extract unpaid
 *   process labor from unaffiliated contributors and dilute the control
 *   claims of both absolutist camps through enforced participation
 *   requirements.
 *
 * KEY AGENTS:
 *   - stakeholder_governance_communities: agenda_setter (organized/constrained) — runs the negotiation process
 *   - maintainer_collectives: beneficiary/payer (organized/constrained) — does the technical work under governance overhead
 *   - downstream_institutional_users: beneficiary (institutional/mobile) — consumes stable shared infrastructure
 *   - absolutist_freedom_advocates: payer (moderate/identity_locked) — denied unilateral freedom veto
 *   - absolutist_property_claimants: payer (moderate/constrained) — denied unilateral property veto
 *   - unaffiliated_contributors: payer (powerless/trapped) — bears process cost with no governance seat
 *   - commons_governance_theorists: observer (analytical) — evaluates capture vs. genuine coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.44).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.38).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.38).
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
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, 'a5519636-cf0f-4b92-9e23-eed3bbc977a1').
narrative_ontology:cs_kernel_codification('a5519636-cf0f-4b92-9e23-eed3bbc977a1', distributed).
narrative_ontology:cs_authority_grounding('a5519636-cf0f-4b92-9e23-eed3bbc977a1', distributed).
narrative_ontology:cs_reading_relation('a5519636-cf0f-4b92-9e23-eed3bbc977a1', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5519636-cf0f-4b92-9e23-eed3bbc977a1', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('a5519636-cf0f-4b92-9e23-eed3bbc977a1', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('a5519636-cf0f-4b92-9e23-eed3bbc977a1', foundational, no_single_party_holds_unilateral_control_legitimacy).
narrative_ontology:cs_axiom_status(no_single_party_holds_unilateral_control_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a5519636-cf0f-4b92-9e23-eed3bbc977a1', no_single_party_holds_unilateral_control_legitimacy, conventional).
narrative_ontology:cs_axiom('a5519636-cf0f-4b92-9e23-eed3bbc977a1', secondary, collective_process_confers_legitimacy_absent_unilateral_right).
narrative_ontology:cs_axiom_status(collective_process_confers_legitimacy_absent_unilateral_right, holdable).
narrative_ontology:cs_axiom_grounding('a5519636-cf0f-4b92-9e23-eed3bbc977a1', collective_process_confers_legitimacy_absent_unilateral_right, instrumental).
narrative_ontology:cs_reference_frame('a5519636-cf0f-4b92-9e23-eed3bbc977a1', negotiated_multistakeholder_governance).
narrative_ontology:cs_drift_state('a5519636-cf0f-4b92-9e23-eed3bbc977a1', contemporary_foundation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5519636-cf0f-4b92-9e23-eed3bbc977a1', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_governance_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, maintainer_collectives).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, downstream_institutional_users).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolutist_freedom_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolutist_property_claimants).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, unaffiliated_contributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, maintainer_collectives).
narrative_ontology:constraint_vindicates(software_control_legitimacy__commons_reading, collective_stewardship_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates foundations, working groups, and governance boards (e.g. project steering committees, standards bodies) that set contribution rules, licensing terms, and access policies for shared codebases through negotiated process — votes, RFCs, technical committees. Collects legitimacy and durable influence from running the process; bears the cost of maintaining consensus machinery.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_governance_communities, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, stakeholder_governance_communities, beneficiary).

% Small groups of core maintainers who do the ongoing technical work; the commons framing gives them a legitimate claim to set contribution standards and reject bad-faith forks, but the negotiated-governance overhead (meetings, RFC review, consensus building) is unpaid labor layered on top of coding.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, maintainer_collectives, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, maintainer_collectives, payer).

% Corporations and public agencies that consume shared infrastructure (compilers, libraries, protocols) governed by these negotiated processes. Benefit from predictable, collectively-maintained infrastructure without bearing full development cost; can exit to alternative stacks if governance becomes hostile, but rarely do because switching costs are high.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, downstream_institutional_users, beneficiary,
    institutional, generational, mobile, global).

% Hold that any restriction on user modification/redistribution rights is illegitimate regardless of process. The commons governance model overrules their position whenever the negotiated body adopts terms (contributor agreements, trademark policy, code-of-conduct gates) that constrain redistribution or forking in ways they consider a freedom violation. Their objection is heard in governance forums but structurally outvoted; exit means forking, which fragments the very commons they wanted access to.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolutist_freedom_advocates, payer,
    moderate, biographical, identity_locked, global).

% Hold that creators/investors should have unilateral control to restrict use and monetize without community override. The commons process denies them that unilateral authority — licensing terms, contribution rules, and access decisions are made collectively, diluting any single rights-holder's control. They can withhold contributions or build proprietary forks, but lose access to the shared commons's network effects and legitimacy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolutist_property_claimants, payer,
    moderate, biographical, constrained, global).

% Individual developers who contribute code, bug reports, or documentation but hold no seat in the governance bodies that set the rules. They bear the transaction costs of governance process (CLA signing, style enforcement, contribution review delay) without a vote in how those costs are set; their only exit is to stop contributing, forfeiting recognition for work already done.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, unaffiliated_contributors, payer,
    powerless, immediate, trapped, global).

% Scholars and practitioners (drawing on Ostrom-style commons theory) who evaluate whether a given negotiated governance structure actually avoids both tragedy-of-the-commons underinvestment and elite capture, or merely dresses up one of those failure modes in participatory language.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, commons_governance_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__commons_reading, stakeholder_governance_communities).
narrative_ontology:fixing_cost_class(software_control_legitimacy__commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shared digital infrastructure (languages, libraries, protocols, standards) requires ongoing collective decisions about contribution rules, compatibility, and access that no single actor can make unilaterally without either fragmenting the ecosystem or freezing it; negotiated governance bodies solve the genuine problem of coordinating many independent contributors and users around one evolving artifact.
% TRANSFER_FUNCTION: Moves decision authority away from both individual rights-holders (who would otherwise unilaterally restrict) and individual users (who would otherwise unilaterally fork/appropriate) toward organized governance bodies; moves unpaid process labor onto maintainers and unaffiliated contributors; moves durable influence and legitimacy toward whoever controls the governance seats.
% ABSENT_VOICES: Both absolutist camps are present in governance forums but structurally outvoted by design — the commons reading exists precisely to deny either absolutism a veto. Unaffiliated contributors with the least power are least likely to hold governance seats at all; their objections rarely reach the forums where rules are actually negotiated.
% DISAPPEARANCE_RATIONALE: If negotiated governance bodies vanished, shared infrastructure would fracture into unilaterally-controlled proprietary forks (property claimants) and unrestricted but uncoordinated free forks (freedom advocates) within a short period; the compatibility and stability that institutional users currently rely on would degrade, and maintainer collectives would lose their basis for excluding bad-faith actors.
% FOUNDING_PROBLEM: Neither pure permissionless forking nor pure proprietary control produced stable, high-quality shared infrastructure at scale — permissionless commons fragmented under free-riding and incompatible forks, while proprietary control produced underinvestment in interoperability and lock-in; negotiated governance bodies (foundations, standards committees, steering councils) emerged to hold a middle position that could sustain both investment and openness.
% FOUNDING_PROBLEM_CORROBORATION: Commons governance theorists (Ostrom-tradition researchers studying digital infrastructure) independently attest that negotiated governance solves real coordination failures neither absolutism solves alone; however, critics from both absolutist camps and some unaffiliated-contributor advocacy groups argue the governance bodies have drifted into representing incumbent maintainer and corporate-sponsor interests rather than the broader user/contributor base, making the founding problem's current resolution contested rather than settled.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.44, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.44) is moderate because the negotiated-governance model genuinely solves a coordination problem (interoperability, sustained investment, protection against bad-faith forks) that neither absolutism solves alone — this is real coordination value, not pure cover. But it is not zero because governance seats are unevenly distributed: unaffiliated contributors and both absolutist camps bear real costs (unpaid process labor, denied control) without proportional voice. Suppression (0.38) reflects that neither absolutism is permitted to exit the negotiated framework without losing access to the shared commons — a soft but real coercive structure. Resistance (0.55) is elevated because both absolutist camps actively organize against the commons framing from opposite directions, which is a distinguishing feature of this reading versus a reading where absolutism has simply faded.
 *
 * DIRECTIONALITY LOGIC:
 *   Stakeholder governance communities and maintainer collectives derive low d (near beneficiary) because they set and administer the terms. Downstream institutional users derive low-moderate d — genuine beneficiaries of stability, with mobile exit tempering any extraction they might otherwise experience. Both absolutist camps derive high d: the commons reading structurally overrides their core claim to unilateral control, and their exit options (identity_locked for freedom advocates whose position is near-ideological; constrained for property claimants who can fork but lose commons network effects) push them toward the target end. Unaffiliated contributors derive the highest d among payers: powerless, trapped by sunk contribution history, bearing costs with no governance voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating shared infrastructure without either fragmentation or proprietary lock-in) remains genuinely live for large multi-stakeholder projects, which is why founding_problem_status is 'contested' rather than 'dead' — this classification prevents mislabeling the commons reading as pure extraction dressed as coordination. But the rising extractiveness trend (0.30→0.44) combined with governance bodies increasingly representing incumbent maintainer/corporate-sponsor interests (per theorist corroboration) signals the beginning of the pattern the tangled_rope classification is built to catch: real coordination function persisting alongside growing asymmetric extraction from those without governance seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_capture_vs_genuine_coordination,
    'Do negotiated governance bodies in practice avoid elite capture (by maintainer incumbents or corporate sponsors), or do they merely relabel one of the two absolutist failure modes in participatory language?',
    'Longitudinal study of governance-seat composition and voting outcomes across major foundations/steering committees, tracking whether decisions systematically favor sponsor organizations or incumbent maintainers over the broader contributor/user base.',
    'If capture is confirmed, the tangled_rope classification should shift toward snare for the governance layer; if genuine rotation and responsiveness are found, extraction should be revised downward and the rope reading strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_capture_vs_genuine_coordination, empirical, 'Whether commons governance bodies are captured or genuinely representative.').

omega_variable(
    committer_reading_boundary_ambiguity,
    'Is the boundary between commons_reading and pragmatic_openness_reading sharp, or do many real governance structures blend negotiated-legitimacy claims with mere methodology pragmatism such that classifying a given project under one reading rather than the other is itself contested?',
    'Case-by-case examination of governance charters: does the charter assert normative legitimacy for collective control (commons) or merely operational preference for openness (pragmatic)? Charters that explicitly invoke stakeholder rights or democratic process land in commons_reading; charters that invoke code quality or velocity land in pragmatic_openness_reading.',
    'Misclassifying a pragmatic_openness project as commons_reading would overstate the legitimacy claims being made and could inflate perceived victimization of absolutist positions where none was intended.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_boundary_ambiguity, conceptual, 'Ambiguity in distinguishing commons legitimacy claims from mere methodology pragmatism.').

omega_variable(
    unaffiliated_contributor_representation_gap,
    'Is the absence of unaffiliated contributors from governance seats a structural necessity of scale (too many contributors to seat all of them) or a chosen design that concentrates influence among sponsor-affiliated maintainers?',
    'Compare governance structures that use rotating/elected contributor seats versus appointment-based or sponsor-allocated seats, controlling for project size.',
    'If scale necessity, the extraction from unaffiliated contributors is closer to unavoidable coordination cost; if design choice, it is closer to avoidable asymmetric extraction and should weight the classification further toward snare-adjacent tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unaffiliated_contributor_representation_gap, empirical, 'Whether contributor exclusion from governance is scale-necessary or a capturable design choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soft_tr_t4, software_control_legitimacy__commons_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__commons_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(soft_tr_t12, software_control_legitimacy__commons_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__commons_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__commons_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__commons_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(soft_be_t4, software_control_legitimacy__commons_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__commons_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(soft_be_t12, software_control_legitimacy__commons_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__commons_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__commons_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__commons_reading, base_extractiveness, 24, 0.44).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_control_legitimacy__commons_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__commons_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).

% DUAL FORMULATION NOTE:
% This file is one of four sibling readings of the software_control_legitimacy kernel. Each reading is authored as a separate, ε-invariant constraint with its own beneficiary/victim structure: freedom_imperative_reading treats proprietary control itself as the extraction (property claimants as sole victims, near-snare); property_rights_reading treats collective override of creator authority as the extraction (freedom advocates and commons bodies as victims, near-tangled_rope from the opposite direction); pragmatic_openness_reading treats the whole legitimacy contest as overstated, authoring low suppression and near-rope status since it denies either absolutism a normative claim to answer. This commons_reading file authors both absolutisms as victims of a legitimacy override while treating negotiated governance bodies and downstream users as beneficiaries — ε=0.44 here should NOT be reconciled with or averaged against the sibling files' ε values; each is a separate structural claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
