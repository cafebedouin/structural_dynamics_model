% ============================================================================
% CONSTRAINT STORY: information_control_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_control_regime, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: information_control_regime
 *   human_readable: Organizational Information Control Regime Justified by Dangerous Knowledge Concern
 *   domain: organizational_psychology/group_dynamics/epistemology
 *
 * SUMMARY:
 *   An organizational psychology research group or therapeutic community
 *   implements comprehensive information control policies justified by dual
 *   concerns: protecting members from dangerous knowledge that could cause
 *   psychological harm, and protecting proprietary research from competitive
 *   appropriation. The regime classifies internal communications, restricts
 *   external consultation, enforces confidentiality through legal agreements,
 *   and controls access to records. Leadership frames the restrictions as
 *   ethical necessity; members seeking external validation and departing
 *   members experience them as barriers to accountability and
 *   reality-testing. The constraint is claimed as tangled_rope (genuine
 *   coordination function with asymmetric extraction) while metrics describe
 *   substantial and rising extraction, active enforcement, and moderate
 *   theatricality as the dangerous knowledge justification is deployed beyond
 *   its original scope.
 *
 * KEY AGENTS:
 *   - leadership_controlling_narrative: Agenda-setter (institutional/arbitrage) — sets classification policies, enforces restrictions, collects narrative control
 *   - members_seeking_external_validation: Primary payer (moderate/identity_locked) — bear epistemic constraint, cannot reality-test experiences externally
 *   - departing_members: Secondary payer (powerless/trapped) — bound by agreements after exit, face legal and social costs for disclosure
 *   - external_researchers: Excluded (organized/mobile) — systematically denied access, their exclusion is the enforcement target
 *   - rank_and_file_members: Beneficiary and payer (moderate/constrained) — protected from information overload, constrained in epistemic access
 *   - regulatory_oversight_bodies: Observer (institutional/analytical) — investigate when complaints surface, face systematic information barriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_control_regime, 0.68).
domain_priors:suppression_score(information_control_regime, 0.76).
domain_priors:theater_ratio(information_control_regime, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_control_regime, extractiveness, 0.68).
narrative_ontology:constraint_metric(information_control_regime, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(information_control_regime, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(information_control_regime, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(information_control_regime, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_control_regime, tangled_rope).
narrative_ontology:human_readable(information_control_regime, "Organizational Information Control Regime Justified by Dangerous Knowledge Concern").
narrative_ontology:topic_domain(information_control_regime, "organizational_psychology/group_dynamics/epistemology").

domain_priors:requires_active_enforcement(information_control_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_control_regime, leadership_controlling_narrative).
narrative_ontology:constraint_victim(information_control_regime, members_seeking_external_validation).
narrative_ontology:constraint_victim(information_control_regime, departing_members).
narrative_ontology:constraint_victim(information_control_regime, external_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(information_control_regime, rank_and_file_members).
narrative_ontology:constraint_victim(information_control_regime, rank_and_file_members).
narrative_ontology:constraint_vindicates(information_control_regime, dangerous_knowledge_doctrine).
narrative_ontology:constraint_vindicates(information_control_regime, organizational_harm_prevention_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets classification policies, determines what constitutes dangerous or proprietary knowledge, enforces disclosure restrictions through employment contracts and confidentiality agreements. Justifies the regime as protecting members from psychological harm and the organization from competitive disadvantage. Controls access to internal records and decides what information can be shared externally. Benefits from preventing unfavorable external scrutiny and maintaining control over organizational narrative.
narrative_ontology:constraint_stakeholder(information_control_regime, leadership_controlling_narrative, agenda_setter,
    institutional, generational, arbitrage, regional).

% Experience psychological distress or confusion within the organization and seek external professional consultation or peer support. Face contractual and social barriers to sharing their experiences: confidentiality agreements prohibit discussing internal practices, leadership frames external consultation as disloyalty or dangerous exposure, and the dangerous knowledge narrative makes them fear harming others by speaking. Their identity is fused with organizational membership, making exit psychologically costly even when they recognize harm.
narrative_ontology:constraint_stakeholder(information_control_regime, members_seeking_external_validation, payer,
    moderate, biographical, identity_locked, local).

% Have left or are leaving the organization but remain bound by confidentiality agreements and internalized prohibitions against disclosure. Face legal threats if they share experiences publicly, social ostracism from remaining members if they speak, and psychological burden from the dangerous knowledge frame that positions their testimony as potentially harmful to others. Cannot access their own records or internal documentation that would corroborate their experiences.
narrative_ontology:constraint_stakeholder(information_control_regime, departing_members, payer,
    powerless, biographical, trapped, local).

% Academic researchers, journalists, or regulatory investigators seeking to study organizational practices or evaluate claims of harm. Systematically denied access to internal records, prevented from interviewing current members without leadership mediation, and face legal barriers when departing members attempt to share information. Their exclusion is the enforcement target: the regime exists precisely to prevent independent verification of internal practices.
narrative_ontology:constraint_stakeholder(information_control_regime, external_researchers, excluded,
    organized, biographical, mobile, national).

% Receive protection from information overload and potentially distressing knowledge about organizational conflicts or leadership decisions. Also bear costs: cannot reality-test their experiences against external frameworks, cannot access information that might inform their continued participation, and absorb the dangerous knowledge narrative that makes them police their own and others' disclosure. Benefit from reduced cognitive burden but pay through constrained epistemic access.
narrative_ontology:constraint_stakeholder(information_control_regime, rank_and_file_members, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(information_control_regime, rank_and_file_members, payer).

% Other psychological research groups, therapeutic communities, or organizational development practitioners who would benefit from access to this organization's methods and outcomes data. The proprietary research justification explicitly targets them: they are framed as the competitive threat the secrecy protects against, though the same barriers prevent accountability.
narrative_ontology:constraint_stakeholder(information_control_regime, competing_organizations, excluded,
    institutional, generational, mobile, regional).

% Professional ethics boards, institutional review boards, or legal authorities with jurisdiction over psychological research and organizational conduct. Investigate complaints when they surface but face systematic information barriers: confidentiality agreements prevent complainants from providing documentation, leadership controls access to records, and the dangerous knowledge frame is deployed to argue that disclosure itself would harm subjects and violate research ethics.
narrative_ontology:constraint_stakeholder(information_control_regime, regulatory_oversight_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(information_control_regime, leadership_controlling_narrative).
narrative_ontology:fixing_cost_class(information_control_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects research subjects from premature exposure to distressing findings, maintains confidentiality of sensitive personal disclosures made in therapeutic or research contexts, and prevents competitive appropriation of proprietary methodologies before publication.
% TRANSFER_FUNCTION: Moves epistemic authority and narrative control from distributed membership to centralized leadership, extracting members' capacity for independent reality-testing and external validation in exchange for continued organizational belonging and protection from dangerous knowledge.
% ABSENT_VOICES: Departing members who signed away their disclosure rights are structurally silenced; external researchers who could provide independent assessment are excluded by design; competing organizations that might offer alternative frameworks never enter the conversation because the regime prevents the information flow that would enable comparison.
% DISAPPEARANCE_RATIONALE: If the information control regime vanished overnight, current members would seek external consultation within days, departing members would share their experiences publicly, external researchers would gain access to internal records, and the organization's practices would face independent scrutiny that could validate or refute leadership's framing. The epistemic landscape would reorganize around distributed verification rather than centralized narrative control.
% FOUNDING_PROBLEM: Early organizational research involved ethically problematic disclosure of sensitive personal information and premature publication of findings that harmed research subjects; competitive organizations appropriated methodologies without proper attribution, undermining the founding group's sustainability.
% FOUNDING_PROBLEM_CORROBORATION: Leadership attests the founding problems remain live, citing ongoing risks of subject harm and competitive theft. Departing members and external researchers attest the regime has expanded far beyond protecting subjects to preventing any external accountability; regulatory oversight bodies' investigative reports document that confidentiality agreements now cover organizational practices unrelated to research subject protection, supporting the function-shift reading.
narrative_ontology:disappearance_verdict(information_control_regime, world_rearranges).
narrative_ontology:founding_problem_status(information_control_regime, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(information_control_regime, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(information_control_regime, 'none', 1).
narrative_ontology:epsilon_provenance(information_control_regime, 0.68, 'claude-sonnet-4-5-20250929', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_control_regime_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_control_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(information_control_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because the regime transfers epistemic authority and narrative control from distributed membership to centralized leadership, extracting members' capacity for independent verification. The extraction has accumulated over time as classification expanded from protecting research subjects to preventing organizational accountability. Suppression is higher (0.76) because persistence depends on active enforcement: legal agreements, social sanctions for disclosure, and deployment of the dangerous knowledge frame to make members police their own and others' speech. Theater ratio is moderate (0.42): the subject protection function is real for some classified information, but a growing share of enforcement activity defends organizational narrative control rather than member welfare. Accessibility collapse is moderate (0.61): alternatives exist (external consultation, independent research, regulatory complaint) but are made costly by legal barriers and internalized prohibitions. Resistance is substantial (0.58): departing members resist through testimony despite legal threats, external researchers push for access, and some current members seek validation despite organizational prohibitions. The measurement series shows all three metrics rising over the interval on one shared time grid, modeling enforcement intensification and function drift.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently: from leadership's position the arrangement is legitimate protection of subjects and intellectual property that they built and maintain; from the identity-locked and trapped payer seats the same structure operates as enforced epistemic extraction preventing accountability. The dangerous knowledge justification is the hinge: leadership experiences it as genuine ethical concern, members seeking validation experience it as internalized suppression that makes them complicit in their own silencing. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership is the structural beneficiary: sets the rules, collects narrative control, maintains arbitrage-grade exit (can leave without the epistemic constraints they impose on others). Members seeking external validation are primary targets: identity-locked exit (organizational belonging is fused with self-concept), bear the epistemic extraction directly, constrained from reality-testing. Departing members are secondary targets with even less power: trapped exit (legal agreements bind them after departure), face ongoing costs for any disclosure. External researchers are excluded rather than coordinated — their exclusion is what the enforcement machinery exists to maintain. Rank-and-file members sit near symmetric: genuine benefit from reduced cognitive burden, diffuse cost through constrained epistemic access. The engine will compute different types from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this as pure coordination (rope) or pure extraction (snare). The coordination function is real: some information genuinely should be protected to prevent subject harm and premature disclosure. The extraction is also real: the same regime prevents external accountability, transfers narrative control to leadership, and constrains members' epistemic access beyond what subject protection requires. The mandate (protect subjects and intellectual property) has partly outlived its function as the regime expanded to cover organizational practices unrelated to research ethics, but the coordination core persists. A rope classification would miss the substantial extraction and identity-lock dynamics; a snare classification would miss the genuine subject protection function and the fact that rank-and-file members do receive some coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dangerous_knowledge_scope,
    'What proportion of classified information genuinely poses psychological harm risk to members versus serving narrative control functions unrelated to member welfare?',
    'Independent ethics review of classification decisions comparing stated rationale against actual content; natural experiment from organizations with more permissive disclosure policies measuring member harm outcomes.',
    'A narrow genuine-harm scope would establish most extraction as unjustified by the stated rationale and support mandatory disclosure reforms; a wide scope would support leadership''s framing that extensive control is necessary for member protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dangerous_knowledge_scope, empirical, 'Whether dangerous knowledge justification tracks actual harm risk or serves as cover for narrative control.').

omega_variable(
    proprietary_vs_accountability_boundary,
    'Is the proprietary research justification structurally separable from the accountability prevention function, or does protecting intellectual property necessarily prevent external verification of organizational practices?',
    'Comparative analysis of research organizations that maintain IP protection while allowing external audit of non-proprietary practices; regulatory frameworks that mandate disclosure of organizational conduct while protecting trade secrets.',
    'If separable, the accountability barrier is pure extraction riding on legitimate IP protection; if inseparable, part of the measured extraction is the necessary cost of maintaining competitive advantage in a research market.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proprietary_vs_accountability_boundary, conceptual, 'Whether the constraint''s IP protection and accountability prevention components are structurally separable.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression primarily structural (legal agreements, leadership sanctions) or internalized (members have absorbed the dangerous knowledge frame and self-censor even when external barriers are removed)?',
    'Post-exit suppression trajectory: if departing members remain silent after confidentiality agreements expire or are unenforceable, reclassify as substantially internalized; if disclosure increases when legal barriers fall, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — members carry the suppression with them after exit, and the regime has successfully colonized their epistemic autonomy. If structural, removal of legal barriers would substantially reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression persists as internalized cognitive pattern after structural barriers are removed.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds members seeking external validation — is it professional identity (career path dependence on organizational credentials), relational identity (self-concept constituted through organizational belonging), ideological identity (worldview that makes the organization''s framework necessary for psychological coherence), or institutional identity (the organization has become the member''s primary reference group)?',
    'Longitudinal study of exit patterns: which identity frame predicts exit difficulty, post-exit adjustment, and likelihood of seeking external validation after departure. Interview data from departing members about what made exit psychologically costly.',
    'Different identity-lock mechanisms suggest different intervention points: professional locks respond to credential portability, relational locks to alternative community access, ideological locks to framework pluralism, institutional locks to reference group diversification. Misidentifying the mechanism leads to ineffective exit support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Which identity-fusion mechanism creates the identity_locked exit classification for members seeking external validation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_control_regime, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(info_tr_t0, information_control_regime, theater_ratio, 0, 0.18).
narrative_ontology:measurement(info_tr_t5, information_control_regime, theater_ratio, 5, 0.24).
narrative_ontology:measurement(info_tr_t10, information_control_regime, theater_ratio, 10, 0.29).
narrative_ontology:measurement(info_tr_t15, information_control_regime, theater_ratio, 15, 0.34).
narrative_ontology:measurement(info_tr_t20, information_control_regime, theater_ratio, 20, 0.38).
narrative_ontology:measurement(info_tr_t25, information_control_regime, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(info_be_t0, information_control_regime, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(info_be_t5, information_control_regime, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(info_be_t10, information_control_regime, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(info_be_t15, information_control_regime, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(info_be_t20, information_control_regime, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(info_be_t25, information_control_regime, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(info_su_t0, information_control_regime, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(info_su_t5, information_control_regime, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(info_su_t10, information_control_regime, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(info_su_t15, information_control_regime, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(info_su_t20, information_control_regime, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(info_su_t25, information_control_regime, suppression_requirement, 25, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_control_regime, identity_coordination).
narrative_ontology:boltzmann_floor_override(information_control_regime, 0.08).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
