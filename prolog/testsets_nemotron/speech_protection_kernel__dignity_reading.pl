% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditional on Dignity Maintenance
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint instantiates the dignity_reading of the
 *   speech_protection_kernel: speech protection is conditional on expression
 *   not functioning as structural subordination of historically target
 *   groups. It recognizes group harm as distinct from individual harm, treats
 *   hate speech and group libel as categorically unprotected, and makes equal
 *   dignity a prerequisite for speech protection rather than a competing
 *   value. The kernel is contested — five readings coexist in constitutional
 *   discourse — and this story authors only the dignity_reading's structural
 *   profile.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.22).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.35).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Dignity Maintenance").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '9469db33-a0d2-473f-9161-d53dcb56c73b').
narrative_ontology:cs_kernel_codification('9469db33-a0d2-473f-9161-d53dcb56c73b', fixed_text).
narrative_ontology:cs_authority_grounding('9469db33-a0d2-473f-9161-d53dcb56c73b', lineage).
narrative_ontology:cs_interpretation_layer_present('9469db33-a0d2-473f-9161-d53dcb56c73b').
narrative_ontology:cs_reading_relation('9469db33-a0d2-473f-9161-d53dcb56c73b', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('9469db33-a0d2-473f-9161-d53dcb56c73b', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('9469db33-a0d2-473f-9161-d53dcb56c73b', speech_protection_kernel__marketplace_reading, forecloses).
narrative_ontology:cs_reading_relation('9469db33-a0d2-473f-9161-d53dcb56c73b', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('9469db33-a0d2-473f-9161-d53dcb56c73b', foundational, group_harm_distinct_from_individual_harm).
narrative_ontology:cs_axiom_status(group_harm_distinct_from_individual_harm, holdable).
narrative_ontology:cs_axiom_grounding('9469db33-a0d2-473f-9161-d53dcb56c73b', group_harm_distinct_from_individual_harm, deontological).
narrative_ontology:cs_axiom('9469db33-a0d2-473f-9161-d53dcb56c73b', foundational, equal_dignity_prerequisite_for_speech_protection).
narrative_ontology:cs_axiom_status(equal_dignity_prerequisite_for_speech_protection, holdable).
narrative_ontology:cs_axiom_grounding('9469db33-a0d2-473f-9161-d53dcb56c73b', equal_dignity_prerequisite_for_speech_protection, deontological).
narrative_ontology:cs_axiom('9469db33-a0d2-473f-9161-d53dcb56c73b', secondary, hate_speech_as_structural_subordination_unprotected).
narrative_ontology:cs_axiom_status(hate_speech_as_structural_subordination_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('9469db33-a0d2-473f-9161-d53dcb56c73b', hate_speech_as_structural_subordination_unprotected, conventional).
narrative_ontology:cs_reference_frame('9469db33-a0d2-473f-9161-d53dcb56c73b', post_war_constitutional_dignity_settlement).
narrative_ontology:cs_drift_state('9469db33-a0d2-473f-9161-d53dcb56c73b', contemporary_digital_speech_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9469db33-a0d2-473f-9161-d53dcb56c73b', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, historically_marginalized_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, civil_rights_organizations).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, anti_discrimination_enforcement_bodies).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, hate_speech_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, extremist_organizations).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, platform_operators_moderating_at_scale).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, equal_dignity_as_constitutional_prerequisite).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, group_harm_distinct_from_individual_harm).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, structural_subordination_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose equal participation in public life has been historically undermined by hate speech and structural subordination. They gain protection from speech that functions to maintain their subordinate status. Exit from the constraint's protection is identity-locked — their dignity claim is constitutive of their political standing.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, historically_marginalized_groups, beneficiary,
    moderate, generational, identity_locked, national).

% Organizations that litigate, advocate, and shape doctrine to enforce dignity-based speech restrictions. They benefit from the constraint's recognition of group harm and structural subordination as actionable categories. They can pivot strategies but their organizational identity is bound to this framework.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, civil_rights_organizations, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, civil_rights_organizations, agenda_setter).

% State agencies and tribunals (human rights commissions, EEOC equivalents, constitutional courts) that administer the constraint. They set enforcement priorities, adjudicate complaints, and define the boundary of unprotected speech. Their institutional mandate ties them to this framework; exit would require legislative change.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, anti_discrimination_enforcement_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Individuals whose expression is restricted because it targets protected groups in ways deemed to function as structural subordination. They bear the cost of lost speech opportunities and potential penalties. Exit means ceasing the restricted speech or moving to jurisdictions with different regimes.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, hate_speech_speakers, payer,
    powerless, immediate, constrained, local).

% Organized groups whose core ideology depends on speech that subordinates target groups. They face bans, platform removal, and legal liability. Their exit is trapped — the constraint directly targets their organizational raison d'être; compliance dissolves them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, extremist_organizations, payer,
    organized, biographical, trapped, national).

% Large platforms that must build and operate content moderation systems implementing dignity-based standards. They bear compliance costs, reputational risk from both over- and under-enforcement, and the operational burden of adjudicating structural subordination at scale. They shape enforcement through policy choices but cannot exit the regulatory obligation without leaving markets.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, platform_operators_moderating_at_scale, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, platform_operators_moderating_at_scale, agenda_setter).

% Organizations and scholars who argue for near-categorical speech protection. They would object to group-harm and structural-subordination categories as doctrinal innovations that undermine the speech principle. They are excluded from the constraint's authoritative interpretation but contest it in public discourse and litigation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, free_speech_absolutist_advocates, excluded,
    organized, civilizational, mobile, national).

% Final arbiters that authoritatively interpret the constraint's scope. They read the kernel through this reading (or competing ones) and their decisions lock in the structural relationships for all other seats. Their exit is analytical — they can change reading but only through the slow logic of precedent.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, constitutional_courts_supreme_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a pluralistic public sphere where equal dignity is a precondition for genuine participation, preventing speech from functioning as a mechanism of caste maintenance.
% TRANSFER_FUNCTION: Transfers speech opportunities from speakers whose expression subordinates target groups to the protected groups' capacity to participate as equals; transfers enforcement costs to state agencies and platform operators.
% ABSENT_VOICES: Future generations who would inherit the doctrinal settlement; speakers in jurisdictions without dignity-based frameworks who cannot test the boundary; target group members who internalize subordination and do not file complaints.
% DISAPPEARANCE_RATIONALE: If dignity-conditional protection vanished, hate speech and group libel would become presumptively protected; historically marginalized groups would lose a structural shield against speech that maintains their subordinate status; enforcement bodies would lose their mandate; platforms would face pressure to adopt permissive standards; the public sphere would reorganize around a different speech/dignity equilibrium.
% FOUNDING_PROBLEM: Post-war constitutional orders recognized that formal equality of speech rights coexisted with substantive subordination — groups could not effectively participate when speech functioned to maintain their caste position. The constraint was built to close the gap between formal speech equality and actual participatory equality.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law (ICCPR Art. 20, CERD Art. 4), European Court of Human Rights hate speech jurisprudence, Canadian constitutional law (R. v. Keegstra), and post-apartheid South African constitutionalism corroborate from outside the immediate beneficiary set that group-targeted speech can function as structural subordination requiring restriction. The beneficiary organizations themselves (civil rights groups, enforcement bodies) attest the problem remains live; free speech absolutist advocates attest it is dead or never existed.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).
:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22) is modest but nonzero: the constraint extracts speech opportunities from hate speakers and extremist organizations, and compliance costs from platforms. Suppression (0.35) reflects active enforcement machinery (human rights tribunals, platform moderation regimes, criminal hate speech laws) that must be maintained. Theater ratio (0.18) captures performative enforcement — symbolic prosecutions, platform policy announcements exceeding operational capacity, and the gap between doctrinal recognition of structural subordination and the practical difficulty of adjudicating it. Accessibility collapse (0.42) is moderate: alternative frameworks (absolutist, marketplace) remain live and contested. Resistance (0.58) is high: the constraint faces sustained challenge from free speech advocates, platform operators, and jurisprudential traditions that reject group-harm categories.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (marginalized groups, civil rights orgs) experience this as coordination — a genuine solution to the problem of speech-as-subordination. The payer seats (hate speakers, extremist orgs, platforms) experience it as extraction — a restriction on their speech or operations backed by state power. The agenda-setter seats (enforcement bodies, platforms) experience it as administrative burden with institutional legitimacy at stake. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the author's judgment that both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically marginalized groups and civil rights organizations are structural beneficiaries — the constraint subsidizes their equal participation (d near 0.0). Anti-discrimination enforcement bodies are agenda_setters who administer the constraint and benefit institutionally from its mandate (d ~0.15). Hate speech speakers are powerless payers with constrained exit (d ~0.9). Extremist organizations are organized but trapped payers — the constraint targets their core function (d ~0.95). Platform operators are institutional payers who also shape enforcement (dual role, d ~0.55). Free speech absolutist advocates are excluded observers (d ~0.7). Constitutional courts are analytical observers (d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (speech maintaining caste subordination) remains contested — not dead. The constraint has not atrophied into piton because the coordination function is still claimed as live by beneficiaries and enforcement bodies. However, the rising extractiveness and suppression metrics suggest the coordination function may be narrowing relative to the enforcement apparatus. If the founding problem were widely accepted as solved (status=dead) while the constraint persisted, mandatrophy would be unresolved — a candidate for piton reclassification. Currently contested status keeps it in tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    group_harm_operationalization,
    'Can ''structural subordination'' and ''group harm'' be adjudicated with sufficient specificity to avoid becoming a heuristic for viewpoint suppression?',
    'Longitudinal study of enforcement outcomes: track whether restrictions disproportionately target disfavored viewpoints vs. functionally subordinating speech across jurisdictions and time.',
    'If operationalization fails, the constraint''s coordination function collapses into viewpoint-based suppression, reclassifying toward snare. If it succeeds, the tangled_rope classification holds with a genuine coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(group_harm_operationalization, empirical, 'Whether the constraint''s core coordination concept is administrable without collapsing into viewpoint discrimination.').

omega_variable(
    platform_governance_capture,
    'Do platform operators'' dual role (payer + agenda_setter) create a structural tendency toward over-enforcement that serves their commercial risk-avoidance rather than dignity protection?',
    'Comparative analysis of platform moderation decisions vs. court/tribunal rulings on the same speech categories; measurement of false positive rates for dignity-based takedowns.',
    'If platforms systematically over-enforce beyond legal requirements, the constraint''s effective extraction on speakers rises and its coordination function is diluted by private governance incentives — potential drift toward snare from the speaker''s seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_governance_capture, empirical, 'Whether private governance captures the constraint''s enforcement in ways that amplify extraction.').

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the dignity_reading''s structural subordination test diverge from the harm_threshold_reading''s demonstrable harm test in contested cases?',
    'Case law analysis identifying decisions where the two readings would produce different outcomes; doctrinal scholarship mapping the conceptual boundary.',
    'If the boundary is porous, the two readings may be empirically indistinguishable in operation, suggesting they are one constraint with two labels (requiring merger). If sharp, they remain distinct kernel readings with different ε profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether dignity_reading and harm_threshold_reading are structurally distinct constraints or observationally equivalent.').

omega_variable(
    committer_frame_location,
    'This constraint is one reading (dignity_reading) of the speech_protection_kernel. The sibling readings are absolutist_reading, democratic_participation_reading, harm_threshold_reading, marketplace_reading. The disagreement is located in: (1) whether group harm is a distinct category from individual harm, (2) whether hate speech is categorically unprotected, (3) whether equal dignity is a prerequisite for speech protection.',
    'The kernel''s committer structure is resolved through the six sibling constraint stories'' cross-comparison — each reading authors its own ε, beneficiaries, victims, and type. The engine detects convergence/divergence.',
    'If sibling readings author substantially different ε and structural profiles, the kernel is genuinely contested. If they converge, the kernel label may mask a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_location, conceptual, 'Commitment structure: this reading''s location within the kernel''s contested framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1945, speech_protection_kernel__dignity_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(spee_tr_t1965, speech_protection_kernel__dignity_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(spee_tr_t1985, speech_protection_kernel__dignity_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(spee_tr_t1995, speech_protection_kernel__dignity_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(spee_tr_t2005, speech_protection_kernel__dignity_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(spee_tr_t2015, speech_protection_kernel__dignity_reading, theater_ratio, 2015, 0.17).
narrative_ontology:measurement(spee_tr_t2025, speech_protection_kernel__dignity_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(spee_be_t1945, speech_protection_kernel__dignity_reading, base_extractiveness, 1945, 0.08).
narrative_ontology:measurement(spee_be_t1965, speech_protection_kernel__dignity_reading, base_extractiveness, 1965, 0.12).
narrative_ontology:measurement(spee_be_t1985, speech_protection_kernel__dignity_reading, base_extractiveness, 1985, 0.15).
narrative_ontology:measurement(spee_be_t1995, speech_protection_kernel__dignity_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement(spee_be_t2005, speech_protection_kernel__dignity_reading, base_extractiveness, 2005, 0.2).
narrative_ontology:measurement(spee_be_t2015, speech_protection_kernel__dignity_reading, base_extractiveness, 2015, 0.22).
narrative_ontology:measurement(spee_be_t2025, speech_protection_kernel__dignity_reading, base_extractiveness, 2025, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1945, speech_protection_kernel__dignity_reading, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(spee_su_t1965, speech_protection_kernel__dignity_reading, suppression_requirement, 1965, 0.18).
narrative_ontology:measurement(spee_su_t1985, speech_protection_kernel__dignity_reading, suppression_requirement, 1985, 0.25).
narrative_ontology:measurement(spee_su_t1995, speech_protection_kernel__dignity_reading, suppression_requirement, 1995, 0.28).
narrative_ontology:measurement(spee_su_t2005, speech_protection_kernel__dignity_reading, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement(spee_su_t2015, speech_protection_kernel__dignity_reading, suppression_requirement, 2015, 0.33).
narrative_ontology:measurement(spee_su_t2025, speech_protection_kernel__dignity_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__dignity_reading, 0.1).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel decomposes into five constraint stories (one per reading). This dignity_reading recognizes group harm and structural subordination as grounds for restricting speech. The absolutist_reading treats protection as near-categorical. The harm_threshold_reading requires demonstrable individual harm. The marketplace_reading prioritizes truth-discovery via counterspeech. The democratic_participation_reading prioritizes political expression for self-governance. Each has distinct ε, beneficiaries, victims, and claimed_type. They are linked here and in each sibling's network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__dignity_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
