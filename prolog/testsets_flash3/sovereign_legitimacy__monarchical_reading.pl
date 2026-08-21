% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Legitimacy by Inherited Right
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint describes the monarchical reading of sovereign
 *   legitimacy, where authority is inherited and divinely sanctioned. It is a
 *   Snare because it extracts heavily from subjects and actively suppresses
 *   alternative legitimacy claims. The metrics reflect a system that relies
 *   on high suppression to maintain its extractive function, with a
 *   significant but not dominant theatrical component (rituals, ceremonies)
 *   that reinforces its claims. The claimed type 'snare' reflects the
 *   structural reality of this reading, despite its proponents' claims of
 *   'mountain' or 'rope' (natural order, coordination for stability).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.85).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.9).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, snare).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Legitimacy by Inherited Right").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, 'cbbb88fb-4c16-4e94-bece-42d5d2200d19').
narrative_ontology:cs_kernel_codification('cbbb88fb-4c16-4e94-bece-42d5d2200d19', formalized).
narrative_ontology:cs_authority_grounding('cbbb88fb-4c16-4e94-bece-42d5d2200d19', lineage).
narrative_ontology:cs_interpretation_layer_present('cbbb88fb-4c16-4e94-bece-42d5d2200d19').
narrative_ontology:cs_reading_relation('cbbb88fb-4c16-4e94-bece-42d5d2200d19', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('cbbb88fb-4c16-4e94-bece-42d5d2200d19', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('cbbb88fb-4c16-4e94-bece-42d5d2200d19', foundational, authority_descends_from_divine_right).
narrative_ontology:cs_axiom_status(authority_descends_from_divine_right, holdable).
narrative_ontology:cs_axiom_grounding('cbbb88fb-4c16-4e94-bece-42d5d2200d19', authority_descends_from_divine_right, theological).
narrative_ontology:cs_axiom('cbbb88fb-4c16-4e94-bece-42d5d2200d19', foundational, bloodline_continuity_confers_legitimacy).
narrative_ontology:cs_axiom_status(bloodline_continuity_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('cbbb88fb-4c16-4e94-bece-42d5d2200d19', bloodline_continuity_confers_legitimacy, conventional).
narrative_ontology:cs_reference_frame('cbbb88fb-4c16-4e94-bece-42d5d2200d19', absolute_monarchical_order).
narrative_ontology:cs_drift_state('cbbb88fb-4c16-4e94-bece-42d5d2200d19', contemporary_global_political_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cbbb88fb-4c16-4e94-bece-42d5d2200d19', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, subjects_excluded_from_authority).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, alternative_legitimacy_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, divine_sanction_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate authority by birthright, claiming divine sanction and historical continuity. Benefits directly from the system's structure, which grants them power and privilege without popular consent. Exit means renouncing their identity and power.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class, agenda_setter,
    institutional, generational, identity_locked, national).

% Derives power and status from proximity to the sovereign and participation in the inherited system. Benefits from the stability and privileges afforded by the monarchical structure. Exit is possible but means losing status and influence.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, constrained, national).

% Bear the costs of governance without representation or recourse. Their consent is not required for the exercise of authority, and their participation in political life is severely restricted. Exit options are limited to rebellion or emigration.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, subjects_excluded_from_authority, payer,
    powerless, biographical, trapped, national).

% Advocate for different sources of political authority (e.g., popular sovereignty, meritocracy). They are actively suppressed by the monarchical system, which delegitimizes their claims and often punishes their advocacy. Their 'exit' is to cease advocating or to flee.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, alternative_legitimacy_claimants, excluded,
    moderate, biographical, constrained, national).

% Religious institutions or figures who interpret and validate the divine right of the sovereign. They benefit from their privileged position and influence within the system, often receiving material support and social deference. Their identity is fused with the monarchical order.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, divine_sanction_interpreters, beneficiary,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, stable, and unquestionable line of succession, preventing internal power struggles and civil war by establishing a single, divinely ordained source of authority.
% TRANSFER_FUNCTION: Transfers absolute political power, economic resources, and social status from the general populace to the hereditary ruling class and aristocratic hierarchy, in exchange for perceived stability and order.
% ABSENT_VOICES: Republican theorists, proponents of popular sovereignty, and any group advocating for a merit-based or consent-based system of governance are systematically excluded from the discourse on legitimate authority. Their arguments are deemed illegitimate or seditious.
% DISAPPEARANCE_RATIONALE: If the principle of monarchical legitimacy vanished overnight, the entire political and social order would collapse. The hereditary ruling class would lose its claim to power, the aristocratic hierarchy would be delegitimized, and a power vacuum would emerge, leading to widespread contestation over new forms of governance.
% FOUNDING_PROBLEM: To establish an indisputable and stable form of governance that prevents internal strife and ensures continuity of power, often in post-feudal or early state-formation contexts.
% FOUNDING_PROBLEM_CORROBORATION: The hereditary ruling class and their supporters claim the problem of instability is ever-present and only monarchical rule can prevent it. Historians and political scientists outside the benefiting parties attest that while the founding problem was real, its contemporary status is largely 'dead' in many contexts, with the system persisting due to entrenched interests and suppression of alternatives.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because power and resources are concentrated in a small, unelected elite. Suppression is very high (0.90) due to the active delegitimization and punishment of any challenge to the monarchical principle. Theater ratio (0.40) reflects the significant role of ritual, ceremony, and symbolic continuity in maintaining the illusion of naturalness and inevitability, even as the underlying power dynamics are coercive. Accessibility collapse is high (0.75) as alternatives are systematically removed or made unthinkable. Resistance is moderate (0.60) as challenges, though suppressed, are persistent.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the hereditary ruling class, this is a natural, divinely ordained order (a Mountain or Rope). From the perspective of the subjects and alternative claimants, it is a coercive Snare. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary ruling class and aristocratic hierarchy are clear beneficiaries (d near 0.0), as the system is designed to serve their interests. Subjects excluded from authority are the primary victims (d near 1.0), bearing the costs without benefit. Alternative legitimacy claimants are also targets, facing active suppression. Divine sanction interpreters are beneficiaries, as their institutional power is tied to validating the monarchical claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing chaos, ensuring stability) is often invoked, but its persistence is increasingly due to the benefits it confers on the ruling class rather than its functional necessity. The high extractiveness and suppression, coupled with the contested status of the founding problem, indicate a system that has drifted from coordination to pure extraction, preventing mislabeling as a Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_empirical_status,
    'Is the claim of divine sanction for monarchical rule empirically verifiable or purely a matter of faith/tradition?',
    'Theological and historical analysis of the origins and persistence of the claim, and its impact on political behavior, without recourse to supernatural verification.',
    'If purely faith-based, the ''naturalness'' claim of the constraint is weakened, increasing its computed extractiveness and reducing its Mountain-like characteristics. If it has demonstrable, non-coercive social coordination effects, it might slightly reduce extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_sanction_empirical_status, conceptual, 'The epistemic grounding of divine sanction claims.').

omega_variable(
    succession_stability_vs_extraction,
    'Does the inherited succession mechanism genuinely prevent civil war and instability more effectively than alternative systems, or is this a cover story for extraction?',
    'Comparative historical analysis of states with monarchical vs. republican succession, controlling for other factors like economic development and external threats.',
    'If monarchical succession is demonstrably less stable or more prone to internal conflict than claimed, the coordination function is undermined, increasing the effective extractiveness. If it proves genuinely more stable, the coordination aspect is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_stability_vs_extraction, empirical, 'The actual stability benefits of inherited succession.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, physical coercion) or internalized (subjects believe in divine right, fear divine retribution)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., after a regime change, former subjects still self-censor or defer to old authority), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient to external challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sove_tr_t25, sovereign_legitimacy__monarchical_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(sove_tr_t50, sovereign_legitimacy__monarchical_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(sove_tr_t75, sovereign_legitimacy__monarchical_reading, theater_ratio, 75, 0.45).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__monarchical_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(sove_be_t25, sovereign_legitimacy__monarchical_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(sove_be_t50, sovereign_legitimacy__monarchical_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement(sove_be_t75, sovereign_legitimacy__monarchical_reading, base_extractiveness, 75, 0.88).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__monarchical_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(sove_su_t25, sovereign_legitimacy__monarchical_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(sove_su_t50, sovereign_legitimacy__monarchical_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement(sove_su_t75, sovereign_legitimacy__monarchical_reading, suppression_requirement, 75, 0.9).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__monarchical_reading, suppression_requirement, 100, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'monarchical_reading' of the 'sovereign_legitimacy' kernel, distinct from 'republican_reading' and 'constitutional_hybrid_reading'. Each reading represents a different structural claim about the source and flow of legitimate authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
