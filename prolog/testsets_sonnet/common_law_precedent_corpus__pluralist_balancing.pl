% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__pluralist_balancing, []).

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
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Contextual/Domain-Sensitive Precedent Weighting Doctrine
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'pluralist balancing' reading of the
 *   common law precedent kernel: courts do not treat precedent as uniformly
 *   binding (strict stare decisis) or as a generally revisable adaptive
 *   framework (evolutionary framework), but instead calibrate precedent
 *   weight case-by-case according to domain-specific factors — institutional
 *   competence, reliance interests, statutory versus common-law source,
 *   constitutional versus commercial subject matter. The genuine coordination
 *   function is real: different legal domains plausibly warrant different
 *   stability/adaptation tradeoffs. But the multi-tier weighting scheme also
 *   creates a research-intensive forecasting problem that only well-resourced
 *   repeat litigants can solve efficiently, producing asymmetric extraction
 *   layered onto the coordination function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.48).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.42).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.48).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Contextual/Domain-Sensitive Precedent Weighting Doctrine").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal_theory/jurisprudence/constitutional_law").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, 'feb7189c-dea1-4af5-9feb-83a68004ba36').
narrative_ontology:cs_kernel_codification('feb7189c-dea1-4af5-9feb-83a68004ba36', distributed).
narrative_ontology:cs_authority_grounding('feb7189c-dea1-4af5-9feb-83a68004ba36', practice).
narrative_ontology:cs_interpretation_layer_present('feb7189c-dea1-4af5-9feb-83a68004ba36').
narrative_ontology:cs_reading_relation('feb7189c-dea1-4af5-9feb-83a68004ba36', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('feb7189c-dea1-4af5-9feb-83a68004ba36', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('feb7189c-dea1-4af5-9feb-83a68004ba36', foundational, precedent_weight_is_domain_relative).
narrative_ontology:cs_axiom_status(precedent_weight_is_domain_relative, holdable).
narrative_ontology:cs_axiom_grounding('feb7189c-dea1-4af5-9feb-83a68004ba36', precedent_weight_is_domain_relative, conventional).
narrative_ontology:cs_axiom('feb7189c-dea1-4af5-9feb-83a68004ba36', secondary, institutional_competence_determines_stability_need).
narrative_ontology:cs_axiom_status(institutional_competence_determines_stability_need, holdable).
narrative_ontology:cs_axiom_grounding('feb7189c-dea1-4af5-9feb-83a68004ba36', institutional_competence_determines_stability_need, instrumental).
narrative_ontology:cs_reference_frame('feb7189c-dea1-4af5-9feb-83a68004ba36', domain_sensitive_common_law_tradition).
narrative_ontology:cs_drift_state('feb7189c-dea1-4af5-9feb-83a68004ba36', contemporary_doctrinal_fragmentation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('feb7189c-dea1-4af5-9feb-83a68004ba36', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, repeat_institutional_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, legal_academics).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, solo_practitioners).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, pro_se_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, first_time_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, trial_court_judges).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, law_as_reasoned_elaboration).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, context_sensitivity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides, case by case, how much weight a prior holding carries in this domain and this posture. Retains discretion to distinguish, narrow, extend, or overrule depending on context factors it itself selects and articulates. Bears no direct cost from unpredictability it creates; gains interpretive latitude and reputational credit for 'nuanced' reasoning.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Large firms, insurers, and government agencies litigate the same issue categories repeatedly and can afford to map which domains treat precedent as near-binding versus highly contextual. They invest in doctrinal forecasting capacity that amortizes across many cases, converting the variable-weight regime into a navigable asset rather than a hazard.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, repeat_institutional_litigants, beneficiary,
    organized, generational, mobile, national).

% Produce theory explaining and justifying why precedent weight should vary by domain (institutional competence, reliance interests, error costs). Their scholarship is cited by courts articulating context-sensitive tests, and the perpetual contestability of the balancing framework generates an ongoing supply of publishable doctrinal puzzles.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_academics, beneficiary,
    moderate, civilizational, analytical, national).

% Must advise clients on precedent strength without the research infrastructure to track domain-by-domain variance in how strictly courts treat prior rulings. A holding that looks controlling in one line of cases may be freely distinguished in another; the cost of misjudging which regime applies falls on the client and, reputationally, on the practitioner.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, solo_practitioners, payer,
    moderate, biographical, constrained, regional).

% Represent themselves without access to the domain-specific case law and secondary literature needed to know whether a favorable-looking precedent will actually bind their case's context. They cannot forecast which balancing factors a court will invoke to distinguish away precedent that appears to help them, and cannot afford counsel who could.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, pro_se_litigants, payer,
    powerless, immediate, trapped, local).

% Enter the system once, for a single dispute, with no accumulated map of which domains treat precedent as near-absolute and which permit courts wide contextual discretion. They bear the full unpredictability cost that repeat players have priced out through accumulated institutional knowledge.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, first_time_litigants, payer,
    powerless, immediate, trapped, regional).

% Apply appellate-articulated balancing tests to concrete facts, but themselves face reversal risk if they misjudge how much contextual latitude a higher court intended in a given domain. They administer the doctrine day to day yet cannot control the higher court's evolving sense of which domains warrant which weight.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, trial_court_judges, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, trial_court_judges, payer).

% Would argue that the pluralist balancing framework itself should be replaced with either firmer stare decisis (for predictability) or explicit doctrinal evolution (for candor), but have no forum to contest the meta-level choice of balancing-as-method — that choice is made and remade by courts applying it, not litigated as a discrete question.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, future_litigants_seeking_change, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows courts to preserve the stabilizing function of precedent in domains where reliance interests and institutional competence favor rigidity (property, commercial transactions), while permitting responsiveness where facts, technology, or social understanding are still settling (tort standards, constitutional doctrine, family law) — solving the genuine tension between predictability and adaptability without committing to one extreme system-wide.
% TRANSFER_FUNCTION: Moves the cost of legal unpredictability from institutions with the resources to track domain-specific precedent behavior onto litigants who lack that research capacity — repeat players convert variable weighting into a navigable asset; one-shot and under-resourced litigants absorb the forecasting risk as case losses or settlement discounts.
% ABSENT_VOICES: One-shot and pro se litigants who bear the unpredictability cost have no forum to contest the meta-choice of pluralist balancing over a bright-line rule; the choice of HOW MUCH to weigh precedent in a domain is made by the same courts applying it, never independently litigated or subject to legislative override in most common-law systems.
% DISAPPEARANCE_RATIONALE: Courts and academics would say the world rearranges badly — either ossifying into unworkable strict stare decisis or dissolving into unconstrained judicial preference, undermining law's claimed coherence. Repeat litigants and their counsel would adapt quickly since they already track domain variance informally. Solo and pro se litigants might see little practical change, since the unpredictability they face today would simply be replaced by a different, possibly more transparent, unpredictability under either sibling reading.
% FOUNDING_PROBLEM: Common law needed a way to be both stable enough to support reliance and planning, and flexible enough to correct erroneous or outdated rules without waiting for legislative action — a single fixed rule for precedent weight (always binding, or always revisable) could not serve both goals across all legal domains simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative law scholars outside the judiciary corroborate that the felt need for a middle path between rigid stare decisis and free reinterpretation is long-standing and cross-jurisdictional (documented in comparative studies of common law systems). However, corroboration that pluralist balancing specifically — rather than clearer domain-specific rules legislatively assigned — is the necessary solution comes mainly from judges and academics who administer and theorize the doctrine; practicing litigators and access-to-justice researchers are more skeptical that unpredictability serves any party but repeat institutional actors.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, contested).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) sits at a medium level reflecting genuine mixed function: courts are not simply extracting rents from litigants, but the domain-by-domain variability does create differential costs that fall disproportionately on those without doctrinal-forecasting infrastructure. Suppression (0.42) is moderate — there is no formal bar to arguing precedent weight, but the multi-factor balancing tests are opaque enough that unrepresented and under-resourced litigants cannot effectively contest how a court characterizes the domain. Theater ratio (0.38) reflects that a meaningful share of judicial 'balancing' analysis functions to post-hoc justify outcomes reached on other grounds, dressed in the language of context-sensitivity. Accessibility collapse is moderate (0.4) — alternative framings (bright-line rules, legislative codification) remain available and are periodically proposed but rarely adopted. Resistance (0.55) is real: access-to-justice scholars, legislative reform advocates, and dissenting judges consistently push back against the unpredictability the pluralist approach generates.
 *
 * PERSPECTIVAL GAP:
 *   From the appellate/academic seat, pluralist balancing looks like a rope: sophisticated, context-sensitive coordination that improves on both rigid extremes. From the pro se or first-time litigant seat, the same doctrine looks like a tangled rope shading toward a snare: an unpredictable, resource-gated system where the 'context factors' that determine precedent weight are legible only to institutional repeat players. The engine should register this divergence as structural, not as measurement error — the coordination function is real for the domain-competence question, and the extraction is real for the forecasting-cost question, simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate judiciary and legal academics sit near the beneficiary end: they generate and administer the balancing tests and gain interpretive latitude and scholarly material respectively, at no direct personal cost. Repeat institutional litigants are structural beneficiaries once their forecasting investment amortizes — the variability that burdens one-shot players becomes a competitive moat for them. Solo practitioners, pro se litigants, and first-time litigants sit near the target end: trapped or constrained exit, no capacity to build the domain-specific precedent-tracking capability that neutralizes the unpredictability cost. Trial court judges are a genuine dual seat — they administer the doctrine (agenda-setter) but also absorb reversal risk from misjudging appellate intent (payer), making them structurally different from the appellate seat that sets the balancing tests without bearing application risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (single fixed precedent-weight rule cannot serve all legal domains) remains genuinely live — property law and rapidly evolving tort/constitutional doctrine really do have different stability needs. This blocks a pure snare or piton classification: there is an ongoing coordination function, not merely inertial or purely extractive machinery. But the framework does not resolve which domains get which weight through any transparent, contestable process — that meta-choice is made by the same courts who benefit from the resulting interpretive discretion, which is exactly the enforcement-without-victim-voice structure that keeps this a tangled rope rather than a clean rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is pluralist balancing a distinct, principled reading of the precedent kernel, or is it simply strict stare decisis and evolutionary framework each applied inconsistently across domains without a coherent unifying theory?',
    'Doctrinal analysis of whether courts articulate stable, predictable domain-classification criteria (supporting a genuine pluralist theory) versus ad hoc invocation of ''context'' to reach preferred outcomes (supporting the critique that pluralism is a label for unprincipled inconsistency).',
    'If domain classification is principled and stable, pluralist balancing is a genuine third reading with its own coordination logic. If domain classification is unstable or outcome-driven, this reading collapses into a disguised form of the evolutionary_framework reading, and its distinct ε and classification would need re-evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether pluralist balancing is a coherent third kernel reading or a disguised version of the evolutionary framework.').

omega_variable(
    domain_classification_capture,
    'Who effectively decides which domains get near-binding precedent treatment and which get high contextual flexibility — and is that meta-level classification itself subject to the same extraction dynamics as the object-level precedent question?',
    'Track over multiple decades whether domain classifications (e.g., ''commercial law deserves strict precedent,'' ''family law deserves flexibility'') shift in ways correlated with which litigant classes benefit from the shift.',
    'If domain classification systematically shifts to favor repeat institutional litigants over time, the pluralist framework has a second-order capture problem beyond the first-order variance already measured, which would push extractiveness and suppression higher at the meta-level than at the object level measured here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domain_classification_capture, empirical, 'Whether the domain-classification layer of pluralist balancing is itself subject to capture.').

omega_variable(
    unpredictability_cost_distribution,
    'Does the unpredictability cost of domain-variable precedent weighting fall primarily on under-resourced litigants (extraction reading), or is it evenly distributed as a genuine cost of a system trying to serve heterogeneous domain needs (coordination reading)?',
    'Comparative empirical study of settlement rates, appeal rates, and case outcomes for represented versus pro se litigants across domains with high versus low precedent-weight variance.',
    'Confirms or disconfirms the asymmetric-extraction component of the tangled_rope classification; if costs are evenly distributed, the constraint moves closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unpredictability_cost_distribution, empirical, 'Whether unpredictability costs are asymmetrically or evenly distributed across litigant classes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.22).
narrative_ontology:measurement(comm_tr_t8, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 8, 0.26).
narrative_ontology:measurement(comm_tr_t16, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 16, 0.3).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 24, 0.33).
narrative_ontology:measurement(comm_tr_t32, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 32, 0.36).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(comm_be_t8, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(comm_be_t16, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(comm_be_t32, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 32, 0.46).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comm_su_t8, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(comm_su_t16, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(comm_su_t32, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__pluralist_balancing, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, evolutionary_framework).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the common_law_precedent_corpus kernel. strict_stare_decisis claims high rigidity and near-uniform binding force (predicted lower ε from litigant unpredictability but higher rigidity cost for adaptation-seeking parties). evolutionary_framework claims precedent as a generally revisable adaptive resource responsive to normative change (predicted different beneficiary structure favoring reform-seeking litigants over reliance-seeking ones). pluralist_balancing (this story) occupies the middle with domain-variable weighting, producing multi-tier extractiveness and a distinct victim set (under-resourced litigants who cannot forecast domain classification) not present in the same form in either sibling. Each reading has its own stable ε; they are not measurements of one constraint from different angles but three structurally distinct constraints sharing a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
