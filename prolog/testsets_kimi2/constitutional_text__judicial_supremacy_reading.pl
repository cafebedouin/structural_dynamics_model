% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Text
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint instantiates the judicial_supremacy_reading of the
 *   constitutional_text kernel. It reads constitutional text as granting
 *   courts final interpretive authority, such that judicial invalidation of
 *   legislation is the conclusive determination of constitutional meaning.
 *   Sibling readings include legislative_sovereignty_reading (parliament as
 *   supreme) and popular_sovereignty_reading (the people retain ultimate
 *   authority). The constraint is a commitment system with a formalized
 *   kernel and lineage-based authority grounded in the constitutional text
 *   and its judicial exegesis.
 *
 * KEY AGENTS:
 *   - Judicial Branch (agenda_setter/institutional/analytical exit): exercises conclusive interpretive authority and controls constitutional meaning.
 *   - Rights Claimants (beneficiary/moderate/constrained exit): receive enforceable protections against majoritarian legislation.
 *   - Legislative Majorities (payer/organized/constrained exit): bear the cost of nullified legislation and lost democratic autonomy.
 *   - Constitutional Scholars (observer/analytical/analytical exit): analyze the legitimacy and effects of interpretive supremacy.
 *   - Popular Sovereignty Advocates (excluded/moderate/constrained exit): assert democratic constituent authority but are structurally excluded from final interpretive power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.58).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.62).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Text").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, 'da2bd902-6c9e-4dad-91d2-6cd390966ec5').
narrative_ontology:cs_kernel_codification('da2bd902-6c9e-4dad-91d2-6cd390966ec5', formalized).
narrative_ontology:cs_authority_grounding('da2bd902-6c9e-4dad-91d2-6cd390966ec5', lineage).
narrative_ontology:cs_interpretation_layer_present('da2bd902-6c9e-4dad-91d2-6cd390966ec5').
narrative_ontology:cs_reading_relation('da2bd902-6c9e-4dad-91d2-6cd390966ec5', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('da2bd902-6c9e-4dad-91d2-6cd390966ec5', constitutional_text__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('da2bd902-6c9e-4dad-91d2-6cd390966ec5', foundational, courts_conclusive_interpreters).
narrative_ontology:cs_axiom_status(courts_conclusive_interpreters, holdable).
narrative_ontology:cs_axiom_grounding('da2bd902-6c9e-4dad-91d2-6cd390966ec5', courts_conclusive_interpreters, conventional).
narrative_ontology:cs_axiom('da2bd902-6c9e-4dad-91d2-6cd390966ec5', foundational, legislative_override_constitutionally_void).
narrative_ontology:cs_axiom_status(legislative_override_constitutionally_void, holdable).
narrative_ontology:cs_axiom_grounding('da2bd902-6c9e-4dad-91d2-6cd390966ec5', legislative_override_constitutionally_void, conventional).
narrative_ontology:cs_reference_frame('da2bd902-6c9e-4dad-91d2-6cd390966ec5', constitutional_text_as_judicially_adjudicated).
narrative_ontology:cs_drift_state('da2bd902-6c9e-4dad-91d2-6cd390966ec5', contemporary_political_order, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('da2bd902-6c9e-4dad-91d2-6cd390966ec5', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, judicial_branch).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises final and conclusive interpretive authority over constitutional text; invalidates legislation and binds coordinate branches to its interpretations. Controls the agenda of constitutional meaning through case selection and doctrinal elaboration.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, judicial_branch, agenda_setter,
    institutional, generational, analytical, national).

% Individuals and minority groups seeking protection against legislative encroachment. They benefit from a centralized enforcement mechanism that can nullify majoritarian statutes on constitutional grounds without needing to win electoral majorities.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Elected legislative coalitions whose policy preferences are blocked when courts invalidate statutes as unconstitutional. They lack an institutional override mechanism within the ordinary political process and must resort to costly constitutional amendment or compliance.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_majorities, payer,
    organized, biographical, constrained, national).

% Academic analysts who map the distribution of interpretive authority. They debate whether judicial supremacy is constitutionally necessary, democratically legitimate, or a contingent political development.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% Activists and theorists who assert that ultimate constitutional authority resides with the people rather than courts. They are structurally excluded from final interpretive authority under a reading that treats judicial invalidation as conclusive.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, popular_sovereignty_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, judicial_branch).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves disagreements about constitutional meaning through a single, hierarchical arbiter, providing legal certainty and protecting entrenched rights against transient legislative majorities.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to nullify legislation from legislative majorities to courts, and transfers enforceable rights-protections from the political process to judicially recognized claimants.
% ABSENT_VOICES: Popular sovereignty movements and legislative supremacy theorists who would argue for democratic override or constituent authority are excluded from the final interpretive agenda.
% DISAPPEARANCE_RATIONALE: Without judicial supremacy, constitutional interpretation would become contested between legislative and popular forums; rights-claimants would lose centralized enforcement, and legislative majorities would regain autonomy to determine the constitutionality of their own enactments.
% FOUNDING_PROBLEM: The risk of tyranny by legislative majorities and the need for a stable, uniform mechanism to enforce constitutional boundaries and protect minority rights against transient political pressures.
% FOUNDING_PROBLEM_CORROBORATION: Rights-claimants and constitutional scholars outside the judiciary corroborate the ongoing threat of majoritarian overreach. Legislative actors and democratic theorists contest whether this threat necessitates judicial supremacy, arguing that electoral accountability and political competition provide sufficient safeguards.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a substantial transfer of policy authority from electoral majorities to courts. Suppression (0.62) is high because the constraint actively closes off legislative override and popular constitutionalism as alternatives. Theater_ratio (0.30) captures the ritualized nature of judicial procedure and opinion-writing, which partially substitutes for raw enforcement. Accessibility_collapse (0.78) is high because once entrenched, judicial supremacy appears as the only viable constitutional arrangement, with alternatives (legislative override, departmentalism) treated as legally void. Resistance (0.55) registers persistent political pushback (court-curbing proposals, non-compliance movements) that is institutionally contained but ongoing. The metric series run on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the rights-claimant seat, the constraint is protective coordination that secures minorities against legislative overreach. From the legislative-majority seat, it is an extractive arrangement that removes democratic autonomy over constitutional meaning. The judicial seat experiences authority and institutional role-fulfillment. The engine computes these divergent seat-level classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-claimants are declared beneficiaries with constrained exit, placing them low on the directionality axis; the engine will compute low effective extraction for them. Legislative majorities are declared victims with constrained exit, placing them high on directionality; the engine will compute high effective extraction for them. The judicial branch sits low-to-mid directionality: as agenda-setter with analytical exit it is structurally protected, though its institutional power is amplified by the constraint's scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmajoritarian tyrannyâremains contested. If the problem were universally acknowledged as solved, the constraint would risk becoming a piton (theater without function) or snare (pure power aggrandizement). Because the need for rights-protection against majorities remains disputed and institutionally contested, the constraint retains its hybrid character: genuine coordination function coupled with asymmetric democratic cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supremacy_sovereignty_compatibility,
    'Does the judicial supremacy reading logically foreclose the popular sovereignty reading, or can popular sovereignty operate as a metaprinciple that authorizes judicial supremacy as its ordinary interpretive mechanism?',
    'Comparative constitutional analysis of regimes that combine judicial review with robust popular amendment or constituent assembly mechanisms; logical examination of whether ultimate popular authority is compatible with conclusive judicial authority.',
    'If compatible, the forecloses relation to popular sovereignty weakens to influences or coexists_with, altering the kernel''s constraint-family topology and potentially reclassifying the democratic cost as coordination overhead rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supremacy_sovereignty_compatibility, conceptual, 'Whether judicial supremacy and popular sovereignty are mutually exclusive or nestable.').

omega_variable(
    democratic_responsiveness_quantification,
    'What is the measurable magnitude of democratic responsiveness lost to judicial supremacy, and does this loss constitute extraction or necessary coordination cost?',
    'Cross-national empirical studies correlating judicial review strength with policy responsiveness to public opinion, and counterfactual analysis of legislative outputs under alternative constitutional arrangements.',
    'A large, measurable responsiveness gap would support the victim classification of legislative majorities and justify the current extractiveness score; a negligible gap would suggest the constraint functions more as a rope with low asymmetric cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_responsiveness_quantification, empirical, 'Empirical magnitude of democratic cost under judicial supremacy.').

omega_variable(
    judicial_authority_motivation,
    'Is judicial supremacy exercised primarily to protect rights, or does the institutional authority become self-sustaining and self-expanding independent of rights-protection needs?',
    'Quantitative analysis of judicial dockets and invalidation patterns correlated with rights-salience versus institutional-power cases; historical tracing of judicial doctrine expansion.',
    'If authority expansion is independent of rights-protection, the coordination story is cover for institutional aggrandizement and the constraint trends toward snare; if tightly coupled to rights, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_authority_motivation, empirical, 'Whether judicial power tracks rights-protection or institutional self-interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cons_tr_t12, constitutional_text__judicial_supremacy_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__judicial_supremacy_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(cons_tr_t36, constitutional_text__judicial_supremacy_reading, theater_ratio, 36, 0.32).
narrative_ontology:measurement(cons_tr_t48, constitutional_text__judicial_supremacy_reading, theater_ratio, 48, 0.3).
narrative_ontology:measurement(cons_tr_t60, constitutional_text__judicial_supremacy_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cons_be_t12, constitutional_text__judicial_supremacy_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(cons_be_t24, constitutional_text__judicial_supremacy_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(cons_be_t36, constitutional_text__judicial_supremacy_reading, base_extractiveness, 36, 0.55).
narrative_ontology:measurement(cons_be_t48, constitutional_text__judicial_supremacy_reading, base_extractiveness, 48, 0.58).
narrative_ontology:measurement(cons_be_t60, constitutional_text__judicial_supremacy_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t12, constitutional_text__judicial_supremacy_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(cons_su_t24, constitutional_text__judicial_supremacy_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(cons_su_t36, constitutional_text__judicial_supremacy_reading, suppression_requirement, 36, 0.6).
narrative_ontology:measurement(cons_su_t48, constitutional_text__judicial_supremacy_reading, suppression_requirement, 48, 0.62).
narrative_ontology:measurement(cons_su_t60, constitutional_text__judicial_supremacy_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
