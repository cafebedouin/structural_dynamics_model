% ============================================================================
% CONSTRAINT STORY: institutional_verification_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_verification_collapse, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_verification_collapse
 *   human_readable: Institutional Verification Capacity Collapse Under Generative Media Flood
 *   domain: technology_governance/information_epistemology/digital_forensics
 *
 * SUMMARY:
 *   Hany Farid, a pioneering digital forensics expert, reports his caseload
 *   scaling from one case every few weeks to dozens per day, while his
 *   confidence in detection methods erodes. This is the operational signature
 *   of institutional verification collapse: the expert bottleneck that was
 *   once a quality gate has become a systemic vulnerability. Institutions
 *   continue routing verification requests to experts even as those experts
 *   publicly state the system is failing. The constraint is CLAIMED as
 *   tangled_rope (genuine coordination function with asymmetric extraction)
 *   while metrics describe substantially extractive operation with rising
 *   theater ratio—the engine measures that divergence. KEY AGENTS (by
 *   structural relationship): - forensic_experts: Agenda-setters and payers
 *   (institutional/identity_locked) — built the verification system, now
 *   trapped in its collapse, bearing unsustainable caseloads -
 *   truth_dependent_institutions: Payers (institutional/constrained) —
 *   courts, newsrooms, electoral bodies absorbing verification costs and
 *   delays - deepfake_creators: Beneficiaries (organized/arbitrage) —
 *   generate at scale, benefit from bottleneck regardless of detection
 *   improvements - disinformation_campaigns: Beneficiaries (organized/mobile)
 *   — weaponize the bottleneck strategically during critical windows -
 *   adversarial_state_actors: Beneficiaries (institutional/arbitrage) —
 *   deploy as information warfare, benefit from systemic erosion -
 *   journalism_organizations: Payers (organized/constrained) — verification
 *   timelines exceed news cycles - judicial_systems: Payers
 *   (institutional/trapped) — evidentiary standards require verification that
 *   is increasingly unavailable - platform_companies: Excluded
 *   (institutional/mobile) — host content but excluded from verification
 *   architecture - cryptographic_authentication_advocates: Excluded
 *   (organized/mobile) — propose structural alternatives but cannot force
 *   adoption - epistemology_researchers: Observers (analytical/analytical) —
 *   document the collapse and contested framings
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_verification_collapse, 0.68).
domain_priors:suppression_score(institutional_verification_collapse, 0.72).
domain_priors:theater_ratio(institutional_verification_collapse, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_verification_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(institutional_verification_collapse, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(institutional_verification_collapse, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(institutional_verification_collapse, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(institutional_verification_collapse, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_verification_collapse, tangled_rope).
narrative_ontology:human_readable(institutional_verification_collapse, "Institutional Verification Capacity Collapse Under Generative Media Flood").
narrative_ontology:topic_domain(institutional_verification_collapse, "technology_governance/information_epistemology/digital_forensics").

domain_priors:requires_active_enforcement(institutional_verification_collapse).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(institutional_verification_collapse, '96a61f52-3f77-4470-9f87-af348c18c658').
narrative_ontology:cs_kernel_codification('96a61f52-3f77-4470-9f87-af348c18c658', distributed).
narrative_ontology:cs_authority_grounding('96a61f52-3f77-4470-9f87-af348c18c658', expertise).
narrative_ontology:cs_interpretation_layer_present('96a61f52-3f77-4470-9f87-af348c18c658').
narrative_ontology:cs_reading_relation('96a61f52-3f77-4470-9f87-af348c18c658', institutional_verification_collapse__indexical_realism, coexists_with).
narrative_ontology:cs_reading_relation('96a61f52-3f77-4470-9f87-af348c18c658', institutional_verification_collapse__distributed_verification, influences).
narrative_ontology:cs_reading_relation('96a61f52-3f77-4470-9f87-af348c18c658', institutional_verification_collapse__post_evidentiary, coexists_with).
narrative_ontology:cs_axiom('96a61f52-3f77-4470-9f87-af348c18c658', foundational, verification_capacity_irreversibly_lost).
narrative_ontology:cs_axiom_status(verification_capacity_irreversibly_lost, holdable).
narrative_ontology:cs_axiom_grounding('96a61f52-3f77-4470-9f87-af348c18c658', verification_capacity_irreversibly_lost, empirically_contingent).
narrative_ontology:cs_axiom('96a61f52-3f77-4470-9f87-af348c18c658', foundational, speed_asymmetry_structural).
narrative_ontology:cs_axiom_status(speed_asymmetry_structural, holdable).
narrative_ontology:cs_axiom_grounding('96a61f52-3f77-4470-9f87-af348c18c658', speed_asymmetry_structural, empirically_contingent).
narrative_ontology:cs_axiom('96a61f52-3f77-4470-9f87-af348c18c658', secondary, expert_testimony_remains_authoritative).
narrative_ontology:cs_axiom_status(expert_testimony_remains_authoritative, overridden).
narrative_ontology:cs_axiom_grounding('96a61f52-3f77-4470-9f87-af348c18c658', expert_testimony_remains_authoritative, conventional).
narrative_ontology:cs_reference_frame('96a61f52-3f77-4470-9f87-af348c18c658', indexical_verification_paradigm).
narrative_ontology:cs_drift_state('96a61f52-3f77-4470-9f87-af348c18c658', post_generative_ai_acceleration, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('96a61f52-3f77-4470-9f87-af348c18c658', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_verification_collapse, deepfake_creators).
narrative_ontology:constraint_beneficiary(institutional_verification_collapse, disinformation_campaigns).
narrative_ontology:constraint_beneficiary(institutional_verification_collapse, adversarial_state_actors).
narrative_ontology:constraint_victim(institutional_verification_collapse, truth_dependent_institutions).
narrative_ontology:constraint_victim(institutional_verification_collapse, forensic_experts).
narrative_ontology:constraint_victim(institutional_verification_collapse, journalism_organizations).
narrative_ontology:constraint_victim(institutional_verification_collapse, judicial_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hany Farid and peers built careers on the premise that expert analysis can distinguish authentic from manipulated media. Their caseload scaled from one case per few weeks to dozens per day while confidence in their own methods erodes. They cannot exit without abandoning professional identity; they continue verification work while publicly stating the system is becoming 'utterly useless.' Their authority derives from decades of forensic methodology development, but the speed asymmetry makes that methodology increasingly ceremonial.
narrative_ontology:constraint_stakeholder(institutional_verification_collapse, forensic_experts, agenda_setter,
    powerful, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(institutional_verification_collapse, forensic_experts, payer).

% Courts, newsrooms, electoral certification bodies, human rights documentation organizations depend on the ability to establish what actually happened through visual evidence. They route verification requests to forensic experts at accelerating rates, absorbing both the direct cost of analysis and the institutional cost of delayed or uncertain verdicts. Their exit option is abandoning visual evidence as a truth mechanism entirely, which would require redesigning core institutional functions around acknowledged epistemic uncertainty.
narrative_ontology:constraint_stakeholder(institutional_verification_collapse, truth_dependent_institutions, payer,
    institutional, generational, constrained, global).

% Generate synthetic media at scale using openly available tools. They benefit from the verification bottleneck: every authentic-looking fake that enters circulation before detection creates doubt about all visual claims, and the expert capacity constraint means most fakes are never analyzed at all. Their costs are negligible and falling; institutional verification costs are high and rising. They can shift tools, platforms, and techniques faster than detection methods can adapt.
narrative_ontology:constraint_stakeholder(institutional_verification_collapse, deepfake_creators, beneficiary,
    organized, immediate, arbitrage, global).

% Weaponize the verification collapse strategically: flood information channels with synthetic media during critical windows (elections, crises, trials) when institutional verification cannot keep pace. They do not need their fakes to be undetectable indefinitely—only long enough to shape initial narratives and exhaust verification capacity. The bottleneck is the weapon; they benefit from its existence regardless of detection method improvements.
narrative_ontology:constraint_stakeholder(institutional_verification_collapse, disinformation_campaigns, beneficiary,
    organized, immediate, mobile, global).

% Deploy generative media as an information warfare tool, targeting rival states' evidentiary institutions. They benefit doubly: from the direct effect of specific fakes and from the systemic erosion of visual evidentiary authority in adversary societies. They can invest in both generation capacity and in undermining detection infrastructure, while their own domestic information environment may operate under different epistemic rules.
narrative_ontology:constraint_stakeholder(institutional_verification_collapse, adversarial_state_actors, beneficiary,
    institutional, generational, arbitrage, global).

% Must verify visual claims before publication to maintain credibility, but verification timelines now exceed news cycle speeds. They route material to forensic experts, wait days or weeks for analysis, and often publish without verification or decline to publish verifiable stories because verification is unavailable. The cost is both direct (expert fees, staff time) and reputational (publishing fakes, or being scooped by outlets that publish without verification).
narrative_ontology:constraint_stakeholder(institutional_verification_collapse, journalism_organizations, payer,
    organized, biographical, constrained, global).

% Evidentiary standards require establishing authenticity of visual evidence, but expert testimony is increasingly hedged and delayed. Courts face a choice between relaxing evidentiary standards (undermining the trial process) or excluding visual evidence entirely (losing access to a historically central evidence class). They cannot exit the visual evidentiary framework without legislative change, but continuing within it means accepting either unreliable verdicts or massive case backlogs.
narrative_ontology:constraint_stakeholder(institutional_verification_collapse, judicial_systems, payer,
    institutional, generational, trapped, national).

% Host and distribute the synthetic media but are structurally excluded from the verification process—they lack forensic expertise, face liability risks from content moderation decisions, and operate at scales where per-item expert review is economically impossible. They would argue for automated detection and distributed verification systems if included in institutional design conversations, but current verification architecture treats them as distribution channels, not verification partners.
narrative_ontology:constraint_stakeholder(institutional_verification_collapse, platform_companies, excluded,
    institutional, biographical, mobile, global).

% Propose point-of-capture authentication and blockchain provenance as alternatives to post-hoc forensic analysis. They are excluded from institutional evidentiary standards, which remain anchored to expert testimony about artifact properties rather than cryptographic proof of origin. They would benefit from institutional adoption of their systems but cannot force the transition; institutions continue routing to forensic experts even as those experts report diminishing confidence.
narrative_ontology:constraint_stakeholder(institutional_verification_collapse, cryptographic_authentication_advocates, excluded,
    organized, generational, mobile, global).

% Document the collapse as it unfolds, analyze competing framings of what is happening and what should be done. They see the full structure: the speed asymmetry, the expert bottleneck, the institutional path dependency, the beneficiary incentives, and the contested readings of whether this is a solvable technical problem or an irreversible epistemic shift. Their analysis feeds policy debates but does not directly constrain any actor.
narrative_ontology:constraint_stakeholder(institutional_verification_collapse, epistemology_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(institutional_verification_collapse, deepfake_creators).
narrative_ontology:fixing_cost_class(institutional_verification_collapse, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Institutional verification through expert forensic analysis coordinates shared truth claims: courts, newsrooms, and electoral bodies route contested visual evidence to credentialed experts whose testimony establishes a common evidentiary basis for institutional action.
% TRANSFER_FUNCTION: Transfers epistemic authority and institutional resources from truth-dependent institutions to forensic experts (who bear unsustainable caseloads and reputational costs of declining confidence) and implicitly to deepfake creators and disinformation campaigns (who benefit from the verification bottleneck without paying its costs).
% ABSENT_VOICES: Platform companies and cryptographic authentication advocates are excluded from institutional verification design. Platforms host the content but lack standing in evidentiary processes; authentication advocates propose structural alternatives but cannot force adoption. Both would reshape the constraint if included, but institutional path dependency keeps verification anchored to expert testimony.
% DISAPPEARANCE_RATIONALE: If the expert-mediated verification bottleneck vanished overnight—either through automated detection achieving parity with generation, or through institutional adoption of cryptographic authentication, or through abandonment of visual evidence as an evidentiary class—courts would redesign trial procedures, newsrooms would restructure verification workflows, disinformation campaigns would lose their primary weapon, and forensic experts would either retrain for infrastructure design or exit the field entirely. The information ecosystem would reorganize around whatever replaced post-hoc expert analysis.
% FOUNDING_PROBLEM: Pre-generative-AI era: manipulated visual media (Photoshop, video editing) could mislead institutional decision-making. Expert forensic analysis could detect manipulation through artifact analysis, compression signatures, lighting inconsistencies, providing courts and newsrooms with reliable verification.
% FOUNDING_PROBLEM_CORROBORATION: Farid himself (the founding expert) attests the founding problem is dead: 'The visual system is going to be utterly useless' because generation has outpaced detection. Independent technical analysis from AI researchers confirms the speed asymmetry is structural, not a temporary gap. Journalism organizations and courts (victims, not beneficiaries) corroborate through their own operational testimony: verification timelines now exceed decision-relevant windows. The only parties claiming the founding problem is still live are those with institutional inertia preventing them from redesigning around its absence.
narrative_ontology:disappearance_verdict(institutional_verification_collapse, world_rearranges).
narrative_ontology:founding_problem_status(institutional_verification_collapse, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(institutional_verification_collapse, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-16',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(institutional_verification_collapse, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_verification_collapse_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_verification_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(institutional_verification_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end, rising from 0.38) because the constraint extracts epistemic authority and institutional resources from truth-dependent institutions while benefiting actors who generate synthetic media at negligible cost. The speed asymmetry (upstream constraint propagation_speed_asymmetry) is the structural driver: generation outpaces detection by orders of magnitude, making expert verification a bottleneck rather than a solution. Suppression is high (0.72, rising from 0.45) because institutions are path-dependent on expert testimony—evidentiary standards, professional norms, and legal frameworks all anchor to the expert-mediated model, making exit costly even as the model fails. Theater ratio is moderate and rising (0.41, from 0.12) because expert analysis increasingly serves a legitimation function rather than a truth-finding one: institutions continue routing to experts to satisfy procedural requirements even when experts report declining confidence. The measurements span the generative AI acceleration period (roughly 2019-2029 in real-world terms), showing extraction and theater accumulating as the founding problem dies but the institutional arrangement persists.
 *
 * PERSPECTIVAL GAP:
 *   The forensic expert seat and the truth-dependent institution seat should compute differently despite both being payers. Experts are identity-locked: their professional identity is constituted through the claim that expert analysis can establish truth, so the collapse is an existential threat they cannot exit without abandoning self-concept. Institutions are constrained but not identity-locked: they depend on verification capacity but are not constituted by it, so they could in principle redesign around its absence (though path dependency makes this costly). The beneficiary seats (creators, campaigns, state actors) experience the same structure as a resource: the bottleneck is the weapon, and they benefit from its persistence regardless of whether detection methods improve. The excluded seats (platforms, authentication advocates) see structural alternatives the current architecture cannot accommodate. The engine computes these divergences from the power/exit/role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Forensic experts are dual-positioned: agenda-setters (they built and maintain the verification system) but also payers (identity-locked, bearing unsustainable caseloads and reputational costs of public failure statements). Truth-dependent institutions are pure payers: constrained exit, absorbing both direct verification costs and institutional costs of delayed/uncertain decisions. Deepfake creators and disinformation campaigns are pure beneficiaries: they collect the epistemic uncertainty the bottleneck creates without paying verification costs; their d values sit near the beneficiary end. Adversarial state actors are institutional beneficiaries with arbitrage exit—they can invest in both generation and in undermining detection while operating their own domestic information environments under different rules. Journalism and judicial systems are payers with differentiated exit constraints: journalism is constrained (can shift practices within professional norms), judicial systems are trapped (cannot abandon visual evidence without legislative change). Platform companies and authentication advocates are excluded—they would reshape the constraint if included but lack standing in current institutional design.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a textbook mandatrophy case: the founding problem (pre-generative manipulation detectable through expert analysis) is dead, attested by the founding expert himself and corroborated by independent technical analysis and victim testimony. The institutional arrangement persists because path dependency (evidentiary standards, professional norms, legal frameworks) makes exit costly, not because the coordination function remains live. The theater ratio rising from 0.12 to 0.41 tracks the shift from functional verification to procedural legitimation. The R5 genealogy interview makes this explicit: founding_problem_status is 'dead', corroborated by Farid (the expert who built the system), AI researchers (independent technical analysis), and victim institutions (operational testimony that verification no longer works within decision-relevant timeframes). The only parties claiming the problem is still live are those with institutional inertia preventing redesign—which is exactly the mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    detection_parity_achievability,
    'Can detection methods ever achieve parity with generation methods, or is the speed asymmetry structurally irreversible?',
    'Longitudinal technical analysis of generation vs detection capability growth rates; theoretical computer science analysis of whether detection is inherently harder than generation; empirical observation of whether detection lag narrows or widens over multi-year timescales.',
    'If detection can achieve parity, the constraint is a temporary coordination problem (tangled_rope transitioning to rope as detection improves). If the asymmetry is irreversible, the constraint is a permanent epistemic shift (tangled_rope transitioning to snare as extraction accumulates without coordination function recovery). The indexical_realism vs epistemic_collapse reading contest turns on this question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(detection_parity_achievability, empirical, 'Whether the generation-detection speed asymmetry is temporary or structural.').

omega_variable(
    institutional_path_dependency_strength,
    'How costly is it for truth-dependent institutions to abandon expert-mediated verification and redesign around cryptographic authentication or acknowledged epistemic uncertainty?',
    'Policy experiments in jurisdictions that mandate alternative verification architectures; cost-benefit analysis of institutional redesign vs continuing with degraded expert verification; observation of whether institutions actually transition when given the option or remain anchored to expert testimony despite its declining reliability.',
    'If path dependency is weak, institutions will transition to alternative verification architectures as expert confidence erodes, and the constraint will dissolve. If path dependency is strong, institutions will continue routing to experts even as theater ratio approaches 1.0, and the constraint will persist as pure extraction (mandatrophy). The distributed_verification vs epistemic_collapse reading contest turns partly on this question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_path_dependency_strength, empirical, 'Whether institutional inertia will prevent transition to alternative verification architectures.').

omega_variable(
    indexicality_as_grounding,
    'Did visual media ever derive evidentiary authority from indexicality (physical light capture), or was authority always grounded in social consensus that merely used indexicality as a legitimation story?',
    'Historical analysis of pre-photographic and early-photographic evidentiary practices; media theory analysis of how photographic ''objectivity'' was constructed and contested; comparison of how different cultures and legal traditions treated visual evidence.',
    'If authority was always grounded in indexicality, the current crisis is a genuine epistemic collapse requiring new truth mechanisms. If authority was always grounded in social consensus, the crisis reveals what was always true, and institutions should redesign around acknowledged uncertainty rather than trying to restore a verification capacity that was never as robust as claimed. The indexical_realism vs post_evidentiary reading contest turns on this question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indexicality_as_grounding, conceptual, 'Whether visual evidentiary authority was ever grounded in indexicality or always in social consensus.').

omega_variable(
    expert_identity_lock_breaking_point,
    'At what point does the gap between expert confidence and institutional reliance become so wide that experts collectively exit the field rather than continue providing increasingly ceremonial testimony?',
    'Observation of expert career trajectories; analysis of whether forensic experts retrain for cryptographic authentication infrastructure design, shift to other domains, or continue verification work despite public statements of declining confidence; measurement of expert testimony hedging language over time.',
    'If experts exit en masse, the institutional bottleneck becomes a hard constraint forcing institutional redesign (accelerating transition to alternative architectures). If experts remain identity-locked and continue providing ceremonial verification, the theater ratio continues rising and the constraint persists as mandatrophy. The speed of institutional transition depends partly on whether the expert community breaks its own identity lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expert_identity_lock_breaking_point, empirical, 'Whether forensic experts will collectively exit or remain identity-locked in a failing system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_verification_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, institutional_verification_collapse, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(inst_tr_t0, observed).
narrative_ontology:measurement(inst_tr_t2, institutional_verification_collapse, theater_ratio, 2, 0.18).
narrative_ontology:measurement_basis(inst_tr_t2, observed).
narrative_ontology:measurement(inst_tr_t4, institutional_verification_collapse, theater_ratio, 4, 0.25).
narrative_ontology:measurement_basis(inst_tr_t4, observed).
narrative_ontology:measurement(inst_tr_t6, institutional_verification_collapse, theater_ratio, 6, 0.32).
narrative_ontology:measurement_basis(inst_tr_t6, observed).
narrative_ontology:measurement(inst_tr_t8, institutional_verification_collapse, theater_ratio, 8, 0.37).
narrative_ontology:measurement_basis(inst_tr_t8, observed).
narrative_ontology:measurement(inst_tr_t10, institutional_verification_collapse, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(inst_tr_t10, projected).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, institutional_verification_collapse, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(inst_be_t0, observed).
narrative_ontology:measurement(inst_be_t2, institutional_verification_collapse, base_extractiveness, 2, 0.47).
narrative_ontology:measurement_basis(inst_be_t2, observed).
narrative_ontology:measurement(inst_be_t4, institutional_verification_collapse, base_extractiveness, 4, 0.54).
narrative_ontology:measurement_basis(inst_be_t4, observed).
narrative_ontology:measurement(inst_be_t6, institutional_verification_collapse, base_extractiveness, 6, 0.61).
narrative_ontology:measurement_basis(inst_be_t6, observed).
narrative_ontology:measurement(inst_be_t8, institutional_verification_collapse, base_extractiveness, 8, 0.65).
narrative_ontology:measurement_basis(inst_be_t8, observed).
narrative_ontology:measurement(inst_be_t10, institutional_verification_collapse, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(inst_be_t10, projected).

% Suppression requirement over time
narrative_ontology:measurement(inst_su_t0, institutional_verification_collapse, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(inst_su_t0, observed).
narrative_ontology:measurement(inst_su_t2, institutional_verification_collapse, suppression_requirement, 2, 0.52).
narrative_ontology:measurement_basis(inst_su_t2, observed).
narrative_ontology:measurement(inst_su_t4, institutional_verification_collapse, suppression_requirement, 4, 0.58).
narrative_ontology:measurement_basis(inst_su_t4, observed).
narrative_ontology:measurement(inst_su_t6, institutional_verification_collapse, suppression_requirement, 6, 0.64).
narrative_ontology:measurement_basis(inst_su_t6, observed).
narrative_ontology:measurement(inst_su_t8, institutional_verification_collapse, suppression_requirement, 8, 0.68).
narrative_ontology:measurement_basis(inst_su_t8, observed).
narrative_ontology:measurement(inst_su_t10, institutional_verification_collapse, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(inst_su_t10, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_verification_collapse, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(institutional_verification_collapse, 0.12).

% DUAL FORMULATION NOTE:
% This constraint is one reading (epistemic_collapse) of the visual_evidentiary_authority kernel. Sibling readings (indexical_realism, distributed_verification, post_evidentiary) would instantiate as separate constraint stories with different beneficiary/victim structures, different ε values, and different coordination vs extraction balances. The readings are linked via cs_structure.reading_relations and should be cross-referenced when the full kernel family is generated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_verification_collapse, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
