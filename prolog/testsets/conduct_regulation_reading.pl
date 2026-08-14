% ============================================================================
% CONSTRAINT STORY: conduct_regulation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conduct_regulation_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: conduct_regulation_reading
 *   human_readable: Conduct-Regulation Reading of Algorithmic Attribution (Process-Level Loop Constraint)
 *   domain: law_and_technology/products_liability/first_amendment
 *
 * SUMMARY:
 *   Litigants and regulators facing algorithmic-harm cases have historically
 *   been stalled by a doctrinal fork: is the algorithmic output speech
 *   (triggering First Amendment defenses) or a product (triggering
 *   defect-liability regimes)? Courts adopting the conduct_regulation_reading
 *   avoid this fork entirely by asking process-level questions instead — how
 *   stale is the data the loop ingests, how narrowly is the output
 *   geographically targeted, does the loop auto-implement its own
 *   suggestions, and how much friction exists before a human can reject the
 *   suggested action. This lets liability attach to specific, discoverable
 *   engineering facts without ever deciding what kind of thing the output is,
 *   and without requiring proof that the firm intended any particular harmful
 *   result.
 *
 * KEY AGENTS:
 *   - algorithmic_deployment_firms: Primary target (institutional/constrained) — bears redesign and liability costs keyed to loop architecture
 *   - harmed_downstream_users: Primary beneficiary (powerless/trapped) — gains a tractable liability theory not contingent on winning a categorization fight
 *   - courts_seeking_tractable_doctrine: Secondary beneficiary and agenda-setter (institutional/analytical) — gains an administrable axis for adjudication
 *   - regulators_of_automated_decision_systems: agenda-setter (institutional/analytical) — administers thresholds on recency, locality, friction
 *   - platform_speech_advocates and manufacturing_analogy_theorists: Excluded voices — their frameworks are structurally unnecessary within this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conduct_regulation_reading, 0.42).
domain_priors:suppression_score(conduct_regulation_reading, 0.38).
domain_priors:theater_ratio(conduct_regulation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conduct_regulation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(conduct_regulation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(conduct_regulation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(conduct_regulation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(conduct_regulation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conduct_regulation_reading, tangled_rope).
narrative_ontology:human_readable(conduct_regulation_reading, "Conduct-Regulation Reading of Algorithmic Attribution (Process-Level Loop Constraint)").
narrative_ontology:topic_domain(conduct_regulation_reading, "law_and_technology/products_liability/first_amendment").

domain_priors:requires_active_enforcement(conduct_regulation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(conduct_regulation_reading, '50f50552-22ca-4afd-9963-3464380551a0').
narrative_ontology:cs_kernel_codification('50f50552-22ca-4afd-9963-3464380551a0', distributed).
narrative_ontology:cs_authority_grounding('50f50552-22ca-4afd-9963-3464380551a0', distributed).
narrative_ontology:cs_reading_relation('50f50552-22ca-4afd-9963-3464380551a0', conduct_regulation_reading__products_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('50f50552-22ca-4afd-9963-3464380551a0', conduct_regulation_reading__expressive_attribution_reading, coexists_with).
narrative_ontology:cs_reading_relation('50f50552-22ca-4afd-9963-3464380551a0', conduct_regulation_reading__technician_intent_reading, influences).
narrative_ontology:cs_axiom('50f50552-22ca-4afd-9963-3464380551a0', foundational, liability_attaches_to_loop_structure_not_output_category).
narrative_ontology:cs_axiom_status(liability_attaches_to_loop_structure_not_output_category, holdable).
narrative_ontology:cs_axiom_grounding('50f50552-22ca-4afd-9963-3464380551a0', liability_attaches_to_loop_structure_not_output_category, instrumental).
narrative_ontology:cs_axiom('50f50552-22ca-4afd-9963-3464380551a0', foundational, intent_proof_unnecessary_for_conduct_regulation).
narrative_ontology:cs_axiom_status(intent_proof_unnecessary_for_conduct_regulation, holdable).
narrative_ontology:cs_axiom_grounding('50f50552-22ca-4afd-9963-3464380551a0', intent_proof_unnecessary_for_conduct_regulation, conventional).
narrative_ontology:cs_created_at('50f50552-22ca-4afd-9963-3464380551a0', '').
narrative_ontology:cs_kernel_id(conduct_regulation_reading, algorithmic_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conduct_regulation_reading, harmed_downstream_users).
narrative_ontology:constraint_beneficiary(conduct_regulation_reading, courts_seeking_tractable_doctrine).
narrative_ontology:constraint_beneficiary(conduct_regulation_reading, regulators_of_automated_decision_systems).
narrative_ontology:constraint_victim(conduct_regulation_reading, algorithmic_deployment_firms).
narrative_ontology:constraint_victim(conduct_regulation_reading, recommendation_engineering_teams).
narrative_ontology:constraint_vindicates(conduct_regulation_reading, process_level_regulability_without_categorization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy recommendation and ranking loops at scale. Under this reading, liability attaches not to whether the output is 'speech' or a 'product' but to structural facts about the loop itself: how stale or fresh the ingested data is, how geographically targeted it is, whether outputs auto-implement without human review, and how much friction exists before a human can reject a suggested action. Firms cannot escape scrutiny by recharacterizing outputs as expressive content, but they also cannot be held liable merely for having built a recommender at all — the constraint targets specific engineering choices, which they can redesign.
narrative_ontology:constraint_stakeholder(conduct_regulation_reading, algorithmic_deployment_firms, payer,
    institutional, biographical, constrained, national).

% Build the pipelines whose input recency and auto-implementation behavior are now directly regulable. They must instrument and justify specific design choices (data freshness windows, rejection-friction thresholds) that previously were treated as pure engineering discretion, adding compliance overhead to architecture decisions.
narrative_ontology:constraint_stakeholder(conduct_regulation_reading, recommendation_engineering_teams, payer,
    moderate, immediate, constrained, national).

% Experience concrete harms from algorithmically generated recommendations that were acted upon (self-harm content pushed to a vulnerable user, tightly-coupled financial or medical suggestions auto-executed). Under prior doctrine, they had to win an unresolved categorization fight (is this speech, is this a product) before liability could even be assessed. This reading lets them point directly at structural facts — staleness of the data, absence of a rejection step — without needing to prove the firm intended the outcome or characterize the output's ontological status.
narrative_ontology:constraint_stakeholder(conduct_regulation_reading, harmed_downstream_users, beneficiary,
    powerless, biographical, trapped, national).

% Adjudicate claims against algorithmic systems without needing to resolve the speech/product classification fight or find subjective intent, both of which have proven doctrinally intractable. They gain a stable, administrable axis (input characteristics and action-coupling) that can be litigated on discoverable engineering facts rather than contested metaphysics.
narrative_ontology:constraint_stakeholder(conduct_regulation_reading, courts_seeking_tractable_doctrine, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(conduct_regulation_reading, courts_seeking_tractable_doctrine, agenda_setter).

% Draft and enforce rules keyed to loop architecture — data recency requirements, mandatory friction before auto-implementation, locality-scoped targeting limits. They administer the constraint and can tighten or loosen its thresholds without ever having to legislate what an algorithmic output 'is.'
narrative_ontology:constraint_stakeholder(conduct_regulation_reading, regulators_of_automated_decision_systems, agenda_setter,
    institutional, generational, analytical, national).

% Would argue that any output-shaping regulation implicates expressive interests and deserves First Amendment scrutiny regardless of the process-level framing. Because this reading declines to characterize outputs as speech at all, their objection has no doctrinal foothold within this reading's own terms — they can raise it in a different forum (the expressive_attribution_reading) but not inside this one.
narrative_ontology:constraint_stakeholder(conduct_regulation_reading, platform_speech_advocates, excluded,
    organized, biographical, constrained, national).

% Would argue liability should track product-defect doctrine (design defect, failure to warn). This reading does not need their framework either — it regulates the loop's inputs and couplings directly, bypassing the question of whether the output is a 'product' with a 'defect.'
narrative_ontology:constraint_stakeholder(conduct_regulation_reading, manufacturing_analogy_theorists, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives courts and regulators a way to impose liability and design requirements on algorithmic recommendation loops using discoverable, engineering-level facts (data recency, locality of targeting, auto-implementation, rejection friction) rather than requiring resolution of an unresolved and possibly unresolvable categorization question (is the output speech or a product) or proof of subjective intent.
% TRANSFER_FUNCTION: Moves compliance and redesign costs from harmed users (who previously bore the cost of an intractable threshold fight) to deploying firms and their engineering teams, who must now instrument and justify specific loop-level design choices.
% ABSENT_VOICES: Speech-framing advocates and product-liability-analogy theorists are structurally excluded from this reading's own terms — not because their arguments are wrong, but because this reading is deliberately built to not need them. They remain live in the sibling readings.
% DISAPPEARANCE_RATIONALE: If this process-level reading vanished, litigants and regulators would fall back onto the unresolved speech/product classification fight or an intent requirement, both of which have historically stalled claims against algorithmic harms — cases that currently proceed on loop-architecture facts would lose their doctrinal footing and either fail or migrate to whichever sibling reading a given court adopts.
% FOUNDING_PROBLEM: Courts and regulators faced algorithmic-harm claims that stalled indefinitely because litigants could not get traction on whether an output was speech (First Amendment defenses apply) or a product (defect doctrine applies), and could rarely prove a firm's specific intent regarding any particular output.
% FOUNDING_PROBLEM_CORROBORATION: Attested by judicial opinions and law-review commentary (outside any deploying firm) noting the doctrinal deadlock in cases like recommendation-algorithm wrongful-death suits, where courts explicitly avoided or postponed the speech/product question; also attested by regulators drafting algorithmic-accountability rules who cite the classification fight as the reason prior enforcement stalled.
narrative_ontology:disappearance_verdict(conduct_regulation_reading, world_rearranges).
narrative_ontology:founding_problem_status(conduct_regulation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(conduct_regulation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(conduct_regulation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(conduct_regulation_reading, 0.42, 'claude-sonnet-5', 'algorithmic_authorless_harm_2026_20260813_215102', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conduct_regulation_reading_tests).
:- end_tests(conduct_regulation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) — the reading imposes real redesign and compliance costs on deploying firms but does not attempt to shut down algorithmic deployment altogether, only to condition it on specific structural facts. Suppression is moderate (0.38) because the constraint is enforced through litigation and regulatory rulemaking rather than prior restraint, and firms retain the option of redesigning their loops (adding friction, using less stale data) to exit the liability zone rather than being trapped. Theater ratio is low-to-moderate (0.22) and rising slowly, reflecting some risk that firms will perform compliance (adding cosmetic 'confirm' buttons) rather than substantively reduce coupling tightness — a Goodhart risk worth watching as the doctrine matures.
 *
 * DIRECTIONALITY LOGIC:
 *   Harmed downstream users are the clearest beneficiaries: they gain a viable liability theory without needing to resolve an intractable classification question or prove intent, so their directionality sits near the full-beneficiary end. Deploying firms and their engineering teams bear the compliance and redesign costs and sit near the target end, though their exit is only 'constrained' rather than 'trapped' — they can redesign the loop's structural properties (increase friction, use fresher/more localized data appropriately) to move out of the liability zone, which is exactly the point of a conduct-focused rather than status-focused rule. Courts and regulators are administrators who benefit from doctrinal tractability without bearing direct extraction themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — doctrinal deadlock from the speech/product/intent trilemma — remains fully live (status: live), which forecloses a mandatrophy reading where this constraint persists as inertial theater after its function disappeared. Corroboration comes from judicial opinions and regulatory rulemaking outside the beneficiary set (harmed users), which is exactly the kind of external attestation the R5 genealogy interview asks for. Because the constraint targets loop-level design choices rather than firm identity or output category, it also avoids becoming a permanent extraction mechanism disconnected from function: as firms redesign loops to reduce coupling tightness, the constraint's bite on any given firm should recede — a self-limiting feature rare in extraction-heavy tangled ropes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorization_avoidance_durability,
    'Can courts sustain a purely process-level liability rule indefinitely, or will pressure to resolve the underlying speech/product/intent classification eventually reassert itself as cases scale?',
    'Track appellate treatment over a multi-year window: if higher courts affirm process-level rulings without reaching classification questions, the avoidance is durable; if appellate courts repeatedly reach back to classify the output, the sidestep is unstable.',
    'If unstable, this reading functions as a temporary doctrinal expedient rather than a genuinely independent framework, and its distinct existence as a separate constraint (rather than a sub-rule of one of the sibling readings) would be undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorization_avoidance_durability, conceptual, 'Whether process-level liability can remain independent of the classification fight it was built to avoid.').

omega_variable(
    friction_gaming_risk,
    'Will firms satisfy the rejection-friction and auto-implementation thresholds with cosmetic compliance (a pro forma confirm click) rather than substantively reducing tight action-coupling?',
    'Empirical audit of post-adoption UI/UX changes in deployed systems: measure whether rejection friction correlates with actual behavioral divergence in downstream action rates, not just the presence of a click-through step.',
    'If gaming is widespread, the rising theater_ratio trajectory understates the true drift and the constraint degrades toward piton-like performative compliance despite nominally regulating the right variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(friction_gaming_risk, empirical, 'Risk that firms satisfy the letter of the coupling requirement without reducing the underlying extraction.').

omega_variable(
    cross_reading_forum_shopping,
    'Will litigants and firms strategically select which sibling reading (conduct, product, speech, intent) to argue under depending on which is more favorable to their position in a given case?',
    'Track whether the same firm argues for the expressive_attribution_reading in one venue and against process-level liability in another, or whether plaintiffs plead in the alternative across readings within a single kernel.',
    'Widespread forum shopping across readings would suggest the kernel itself is unsettled enough that no single reading, including this one, is stable law yet — all four readings may be simultaneously live and none dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_reading_forum_shopping, conceptual, 'Whether the coexistence of readings invites strategic reading-selection by parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conduct_regulation_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cond_tr_t0, conduct_regulation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cond_tr_t4, conduct_regulation_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(cond_tr_t8, conduct_regulation_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(cond_tr_t12, conduct_regulation_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(cond_tr_t16, conduct_regulation_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(cond_tr_t20, conduct_regulation_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cond_tr_t24, conduct_regulation_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(cond_be_t0, conduct_regulation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cond_be_t4, conduct_regulation_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(cond_be_t8, conduct_regulation_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(cond_be_t12, conduct_regulation_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(cond_be_t16, conduct_regulation_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(cond_be_t20, conduct_regulation_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(cond_be_t24, conduct_regulation_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cond_su_t0, conduct_regulation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cond_su_t4, conduct_regulation_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(cond_su_t8, conduct_regulation_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(cond_su_t12, conduct_regulation_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(cond_su_t16, conduct_regulation_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(cond_su_t20, conduct_regulation_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(cond_su_t24, conduct_regulation_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conduct_regulation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(conduct_regulation_reading, products_liability_reading).
narrative_ontology:affects_constraint(conduct_regulation_reading, expressive_attribution_reading).
narrative_ontology:affects_constraint(conduct_regulation_reading, technician_intent_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the algorithmic_attribution kernel, each instantiating a structurally distinct constraint with its own epsilon and stakeholder structure: products_liability_reading (firm-as-manufacturer, defect doctrine), expressive_attribution_reading (firm-as-speaker, First Amendment scrutiny), technician_intent_reading (liability keyed to engineer intent), and this conduct_regulation_reading (liability keyed to loop-level input and coupling structure, avoiding all three classification questions). Per the epsilon-invariance principle, these are not one constraint measured four ways — they are four constraints linked by network edges, sharing a kernel but not a classification, a beneficiary set, or an epsilon value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
