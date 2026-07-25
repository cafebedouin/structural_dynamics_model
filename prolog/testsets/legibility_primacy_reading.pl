% ============================================================================
% CONSTRAINT STORY: legibility_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legibility_primacy_reading, []).

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
 *   constraint_id: legibility_primacy_reading
 *   human_readable: Legibility-Primacy Reading of Cooperative Artifact Legitimacy
 *   domain: epistemics_of_cooperation/signaling_theory/authorship
 *
 * SUMMARY:
 *   This story instantiates the legibility-primacy reading of the contested
 *   kernel cooperative_artifact_legitimacy: the claim that the purpose of
 *   joint output is efficient consumption by an evaluator/audience, and that
 *   smoothing away individual authorial texture — including 'ghost-writing'
 *   as normal editorial integration — is a legitimate cost of producing
 *   usable collective work, because the artifact belongs to the enterprise
 *   rather than to any single hand. Under this reading, the coordination
 *   function (audience legibility) is real, but it is bundled with an
 *   extraction function: durable credit consolidates onto the frontperson and
 *   the institution while the labor of contributors whose texture was
 *   smoothed away is rendered structurally invisible. Sibling readings of the
 *   same kernel — authorial_primacy_reading (which holds individual
 *   authorship as the ground truth the artifact must preserve) and
 *   process_transparency_reading (which holds that the production process
 *   itself, not just the artifact, must remain legible) — are NOT represented
 *   here; they are separate constraint files with their own ε values,
 *   beneficiary/victim structures, and classifications, linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legibility_primacy_reading, 0.68).
domain_priors:suppression_score(legibility_primacy_reading, 0.71).
domain_priors:theater_ratio(legibility_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legibility_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legibility_primacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legibility_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legibility_primacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(legibility_primacy_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legibility_primacy_reading, tangled_rope).
narrative_ontology:human_readable(legibility_primacy_reading, "Legibility-Primacy Reading of Cooperative Artifact Legitimacy").
narrative_ontology:topic_domain(legibility_primacy_reading, "epistemics_of_cooperation/signaling_theory/authorship").

domain_priors:requires_active_enforcement(legibility_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legibility_primacy_reading, 'bb72436a-9fc7-43aa-963d-2675efeca699').
narrative_ontology:cs_kernel_codification('bb72436a-9fc7-43aa-963d-2675efeca699', distributed).
narrative_ontology:cs_authority_grounding('bb72436a-9fc7-43aa-963d-2675efeca699', practice).
narrative_ontology:cs_interpretation_layer_present('bb72436a-9fc7-43aa-963d-2675efeca699').
narrative_ontology:cs_reading_relation('bb72436a-9fc7-43aa-963d-2675efeca699', cooperative_artifact_legitimacy__authorial_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('bb72436a-9fc7-43aa-963d-2675efeca699', cooperative_artifact_legitimacy__process_transparency_reading, influences).
narrative_ontology:cs_axiom('bb72436a-9fc7-43aa-963d-2675efeca699', foundational, artifact_belongs_to_enterprise_not_hand).
narrative_ontology:cs_axiom_status(artifact_belongs_to_enterprise_not_hand, holdable).
narrative_ontology:cs_axiom_grounding('bb72436a-9fc7-43aa-963d-2675efeca699', artifact_belongs_to_enterprise_not_hand, conventional).
narrative_ontology:cs_axiom('bb72436a-9fc7-43aa-963d-2675efeca699', foundational, audience_consumability_is_primary_legitimacy_condition).
narrative_ontology:cs_axiom_status(audience_consumability_is_primary_legitimacy_condition, holdable).
narrative_ontology:cs_axiom_grounding('bb72436a-9fc7-43aa-963d-2675efeca699', audience_consumability_is_primary_legitimacy_condition, instrumental).
narrative_ontology:cs_reference_frame('bb72436a-9fc7-43aa-963d-2675efeca699', editorial_house_voice_convention).
narrative_ontology:cs_drift_state('bb72436a-9fc7-43aa-963d-2675efeca699', post_contributorship_taxonomy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('bb72436a-9fc7-43aa-963d-2675efeca699', '').
narrative_ontology:cs_kernel_id(legibility_primacy_reading, cooperative_artifact_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legibility_primacy_reading, evaluating_audience).
narrative_ontology:constraint_beneficiary(legibility_primacy_reading, publishing_institution).
narrative_ontology:constraint_beneficiary(legibility_primacy_reading, credited_frontperson).
narrative_ontology:constraint_victim(legibility_primacy_reading, ghostwritten_contributors).
narrative_ontology:constraint_victim(legibility_primacy_reading, junior_coauthors).
narrative_ontology:constraint_victim(legibility_primacy_reading, specialist_technical_staff).
narrative_ontology:constraint_vindicates(legibility_primacy_reading, collective_ownership_of_joint_work_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives smoothed, internally consistent output that can be read, verified, and acted on quickly without needing to track which sentence came from which hand. Benefits directly from the erasure of authorial texture — the smoothing is what makes the artifact cheap to consume. Has no stake in who gets credited internally, only in the throughput of usable work.
narrative_ontology:constraint_stakeholder(legibility_primacy_reading, evaluating_audience, beneficiary,
    organized, immediate, arbitrage, national).

% Sets and enforces the house style, editorial integration process, and single-byline or single-voice convention. Owns the final artifact as institutional output and collects the reputational and commercial credit that flows to 'the report,' 'the brief,' or 'the paper' as a unified thing. Administers the norm that smoothing is normal editorial work, not appropriation, and can revise that norm at will — it bears none of the cost of enforcing it.
narrative_ontology:constraint_stakeholder(legibility_primacy_reading, publishing_institution, agenda_setter,
    institutional, generational, arbitrage, national).

% The named author, principal investigator, or public voice whose name attaches to the smoothed artifact. Benefits from the legibility convention because it consolidates credit onto a single legible identity that the audience can track and reward. Retains enough standing to renegotiate credit arrangements if they choose to, unlike the contributors beneath them.
narrative_ontology:constraint_stakeholder(legibility_primacy_reading, credited_frontperson, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legibility_primacy_reading, credited_frontperson, agenda_setter).

% Wrote substantial portions of the text, ran the analysis, or drafted the argument, but their sentences are absorbed into a house voice and their name does not travel with the artifact. Have no institutional standing to insist on attribution without risking the relationship that gives them work at all. Exit means declining future assignments, not recovering credit for past ones — the labor is real and the erasure is permanent once the artifact circulates.
narrative_ontology:constraint_stakeholder(legibility_primacy_reading, ghostwritten_contributors, payer,
    powerless, biographical, trapped, national).

% Contribute substantive intellectual content but are positioned lower in a credit hierarchy that the smoothing convention reinforces — their distinct contributions are folded into 'the team's' output, which in practice reads as the senior author's output. Can push back at real career cost; most calculate that visible resistance costs more than the erased credit is worth.
narrative_ontology:constraint_stakeholder(legibility_primacy_reading, junior_coauthors, payer,
    moderate, biographical, constrained, national).

% Editors, translators, research assistants, and technical writers whose structural function is precisely to erase their own texture from the final product. The convention that names this erasure 'normal editorial integration' is the same convention that makes their invisibility a job requirement rather than a grievance.
narrative_ontology:constraint_stakeholder(legibility_primacy_reading, specialist_technical_staff, payer,
    powerless, immediate, trapped, national).

% Editors' guilds, contributorship-standard advocates, and some funding bodies argue for granular contribution statements (CRediT-style taxonomies) instead of single-voice smoothing. Their position is rarely represented inside the institutions that set editorial convention — they publish position papers that circulate mainly among themselves, not inside the newsroom or lab that would have to adopt them.
narrative_ontology:constraint_stakeholder(legibility_primacy_reading, attribution_reform_advocates, excluded,
    moderate, generational, constrained, national).

% Historians of science, media scholars, and authorship-ethics researchers who study how credit conventions form and who they serve, without being party to any specific artifact's production.
narrative_ontology:constraint_stakeholder(legibility_primacy_reading, process_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legibility_primacy_reading, publishing_institution).
narrative_ontology:fixing_cost_class(legibility_primacy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Cooperative output is genuinely more usable when it presents a single coherent voice: an evaluator or audience can verify, cite, and act on a smoothed artifact far faster than on a patchwork of visibly disjoint authorial fragments. Editorial integration solves a real legibility problem for anyone downstream of the collective enterprise.
% TRANSFER_FUNCTION: Moves durable, portable, career-fungible credit from the people who did distributed labor (drafting, analysis, editing, translation) to the single legible identity or institutional brand the audience can track — while the underlying labor itself is not compensated any differently, only its attribution is consolidated upward.
% ABSENT_VOICES: Attribution reform advocates and granular-contributorship standards bodies would argue smoothing is separable from credit erasure — that legibility for the audience does not require invisibility for the contributor. They are structurally outside the editorial rooms and lab hierarchies that set the convention, so their alternative rarely reaches the point of adoption.
% DISAPPEARANCE_RATIONALE: If the legibility-primacy convention vanished overnight, artifacts would need to carry visible multi-voice attribution or contribution statements; institutions would lose the ability to consolidate credit onto a single frontperson or brand; ghostwriters, junior coauthors, and technical staff would gain a mechanism to claim recognition for identifiable labor, changing hiring, promotion, and citation practices across the affected fields.
% FOUNDING_PROBLEM: Collective enterprises produce output faster and more usably when it reads as one coherent voice rather than a visibly seamed assembly of contributions — early journalism, scientific publishing, and institutional writing all needed a convention that let audiences trust and process joint work without adjudicating internal authorship disputes on every artifact.
% FOUNDING_PROBLEM_CORROBORATION: Publishing institutions and credited frontpersons attest the problem remains live: audiences still need consumable, single-voice artifacts, and coordination costs of granular attribution are real. Independent corroboration from outside the beneficiary set — editors' guilds, CRediT-taxonomy advocates, and authorship-ethics researchers — attests that the legibility problem is now separable from the credit-erasure problem: contribution statements and layered bylines demonstrably preserve audience legibility while restoring attribution, which these outside observers read as evidence the current arrangement over-solves for legibility and under-solves for credit on purpose, not by necessity.
narrative_ontology:disappearance_verdict(legibility_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(legibility_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legibility_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(legibility_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legibility_primacy_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legibility_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legibility_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legibility_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 at interval end) reflects that the credit transfer from contributor to frontperson/institution is real and increases as smoothing conventions harden into institutional habit rather than case-by-case editorial judgment. Suppression (0.71) is high because the convention actively forecloses alternative attribution claims — a ghostwriter or junior coauthor who asserts credit is treated as violating professional norms, not exercising a legitimate right. Theater ratio (0.42) captures that a meaningful share of 'editorial integration' language now functions as institutional cover for what is, in a growing share of cases, credit consolidation rather than genuine legibility service. accessibility_collapse (0.58) is moderate rather than near-total because contribution-statement alternatives exist and are adopted in some fields (contrast with a mountain's near-complete collapse). resistance (0.52) reflects real but structurally weak pushback — junior coauthors and technical staff resist individually but rarely coordinate.
 *
 * PERSPECTIVAL GAP:
 *   From the publishing institution's seat, this looks like a rope: pure coordination solving a genuine audience-legibility problem, with alternatives (visible multi-voice text) freely available but simply less efficient. From the ghostwritten contributor's seat, the same structure computes as extractive: the coordination story is real but is being used as cover for a credit transfer they have no practical way to resist. The engine's per-seat computation is expected to diverge along exactly this line — that divergence is the data, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The evaluating audience and publishing institution sit near the full-beneficiary end: they receive the coordination benefit (legible, verifiable output) essentially free of the attribution cost. The credited frontperson is a beneficiary with some agenda-setting power — they did not necessarily design the convention but they profit from it and could push back with less career risk than their juniors. Ghostwritten contributors, junior coauthors, and specialist technical staff sit toward the full-target end: their labor is real, identifiable, and extracted from without correspondingly identifiable compensation in credit currency. Trapped/constrained exit options for these payers (declining future work, career risk) push their effective extraction upward relative to a hypothetical mobile contributor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (audiences need consumable joint output) remains partially live, which prevents this from collapsing into a pure snare reading — there is a genuine coordination function still being served. But the founding_problem_status is authored as contested, not simply live, because outside corroboration (editors' guilds, contributorship-standard advocates) demonstrates the legibility problem is now separable from the credit-erasure problem via contribution statements and layered bylines. The persistence of full smoothing after that separability was demonstrated is exactly the mandatrophy signature: the mandate (audience legibility) has been partially satisfied by better means, but the institutional practice (credit consolidation) persists because it now serves a different, unstated function — protecting the frontperson's and institution's accumulated credit position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legibility_credit_separability,
    'Is audience-side legibility structurally separable from contributor-side credit erasure, or does genuine coordination benefit require the smoothing that also erases attribution?',
    'Comparative study of fields that adopted granular contribution statements (e.g. CRediT taxonomy in scientific publishing) versus fields that retained single-voice smoothing: measure whether audience comprehension, citation efficiency, and verification speed degrade when attribution is made granular and visible.',
    'If separable, this reading''s coordination justification collapses to pure cover for extraction, moving the constraint toward snare. If genuinely inseparable in some domains, part of the measured extraction is the real price of the coordination function, supporting a tangled_rope rather than snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legibility_credit_separability, empirical, 'Whether legibility for the audience genuinely requires invisibility for the contributor.').

omega_variable(
    kernel_reading_selection,
    'Which reading of cooperative_artifact_legitimacy governs a given collective artifact in practice — legibility-primacy, authorial-primacy, or process-transparency — and who decides?',
    'This is inherently a conceptual/preference question resolved by institutional convention rather than empirical fact: different fields (journalism, science, corporate reports, ghostwritten memoirs) have settled on different dominant readings, and the choice reflects whose interests the governing institution is structured to serve, not a discoverable fact about the artifact.',
    'The reading selected determines which parties are named as beneficiaries and victims in the compiled constraint. This file commits to the legibility-primacy reading; sibling files commit to the other readings; a full account of any given real-world artifact requires reading across the family, not choosing one as ''correct.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which reading of the contested kernel a given institution''s practice actually instantiates, and on what authority.').

omega_variable(
    invisibility_as_job_requirement,
    'For specialist technical staff whose structural function includes self-erasure (editors, translators, ghostwriters proper), is the extraction measured here distinguishable from a negotiated feature of the employment relationship they entered knowingly?',
    'Compare compensation and career-advancement structures for contributors who accept invisibility as an explicit condition of paid work versus contributors (junior coauthors, uncredited research assistants) for whom invisibility was not an explicit bargain but an emergent convention applied to them.',
    'If knowingly negotiated, the extraction for that subgroup is better modeled as a priced feature of a rope-like employment coordination; if emergent and unbargained, it strengthens the tangled_rope/victim reading for that subgroup specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invisibility_as_job_requirement, empirical, 'Whether contributor invisibility was a bargained employment term or an unbargained convention applied after the fact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legibility_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legibility_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t8, legibility_primacy_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(legi_tr_t16, legibility_primacy_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(legi_tr_t24, legibility_primacy_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(legi_tr_t32, legibility_primacy_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(legi_tr_t40, legibility_primacy_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legibility_primacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(legi_be_t8, legibility_primacy_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(legi_be_t16, legibility_primacy_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(legi_be_t24, legibility_primacy_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(legi_be_t32, legibility_primacy_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(legi_be_t40, legibility_primacy_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legibility_primacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(legi_su_t8, legibility_primacy_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(legi_su_t16, legibility_primacy_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(legi_su_t24, legibility_primacy_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(legi_su_t32, legibility_primacy_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(legi_su_t40, legibility_primacy_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legibility_primacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legibility_primacy_reading, 0.08).
narrative_ontology:affects_constraint(legibility_primacy_reading, authorial_primacy_reading).
narrative_ontology:affects_constraint(legibility_primacy_reading, process_transparency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the cooperative_artifact_legitimacy kernel. legibility_primacy_reading (this file) grounds legitimacy in audience consumability and treats smoothing as normal editorial integration; authorial_primacy_reading grounds legitimacy in preservation of the individual contributor's distinct hand and treats smoothing as appropriation; process_transparency_reading grounds legitimacy in the visibility of the production process itself, independent of both. The three files share the same underlying labor relationships but assign different beneficiary/victim structures and different ε values because they measure legitimacy against different reference conditions. Each file's classification should be read as a reading, not as competing evidence about a single fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
