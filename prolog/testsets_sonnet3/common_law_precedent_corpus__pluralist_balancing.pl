% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Pluralist Balancing Reading of Precedent Weight
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This story instantiates the pluralist-balancing reading of the common-law
 *   precedent kernel: precedent weight is treated as an explicit function of
 *   domain and context, calibrated case-by-case rather than fixed by a
 *   uniform rule of binding force (strict stare decisis) or by a general
 *   license for reinterpretation (evolutionary framework). Under this
 *   reading, courts openly weigh reliance interests, subsequent doctrinal
 *   developments, and the character of the domain (commercial vs.
 *   constitutional vs. tort) when deciding how much a given precedent
 *   controls. The coordination function is real — it avoids both ossification
 *   and doctrinal chaos — but the same discretion that enables sensible
 *   calibration also creates a multi-tier system where repeat players and
 *   domain specialists can track and exploit the current calibration while
 *   one-off and cross-domain litigants cannot predict it. ε is authored for
 *   the pluralist-balancing arrangement as it stands, not for either sibling
 *   reading's alternative.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: administers the domain-by-domain calibration
 *   - specialist_bar_advocates and institutional_litigants: benefit from tracking the current calibration
 *   - pro_se_litigants, novel_claim_plaintiffs, small_firm_counsel: bear the unpredictability costs
 *   - strict_stare_decisis_advocates: excluded critics who see the calibration as covert result-orientation
 *   - legal_academics: analytical observers of doctrinal drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.52).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.44).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.52).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Pluralist Balancing Reading of Precedent Weight").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, '96ad0295-2803-409c-a570-b3c1bb1bd945').
narrative_ontology:cs_kernel_codification('96ad0295-2803-409c-a570-b3c1bb1bd945', distributed).
narrative_ontology:cs_authority_grounding('96ad0295-2803-409c-a570-b3c1bb1bd945', practice).
narrative_ontology:cs_interpretation_layer_present('96ad0295-2803-409c-a570-b3c1bb1bd945').
narrative_ontology:cs_reading_relation('96ad0295-2803-409c-a570-b3c1bb1bd945', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('96ad0295-2803-409c-a570-b3c1bb1bd945', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('96ad0295-2803-409c-a570-b3c1bb1bd945', foundational, precedent_weight_is_domain_relative).
narrative_ontology:cs_axiom_status(precedent_weight_is_domain_relative, holdable).
narrative_ontology:cs_axiom_grounding('96ad0295-2803-409c-a570-b3c1bb1bd945', precedent_weight_is_domain_relative, conventional).
narrative_ontology:cs_axiom('96ad0295-2803-409c-a570-b3c1bb1bd945', foundational, reliance_and_correction_interests_require_case_by_case_balancing).
narrative_ontology:cs_axiom_status(reliance_and_correction_interests_require_case_by_case_balancing, holdable).
narrative_ontology:cs_axiom_grounding('96ad0295-2803-409c-a570-b3c1bb1bd945', reliance_and_correction_interests_require_case_by_case_balancing, instrumental).
narrative_ontology:cs_reference_frame('96ad0295-2803-409c-a570-b3c1bb1bd945', context_sensitive_common_law_practice).
narrative_ontology:cs_drift_state('96ad0295-2803-409c-a570-b3c1bb1bd945', contemporary_appellate_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('96ad0295-2803-409c-a570-b3c1bb1bd945', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, specialist_bar_advocates).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, institutional_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, pro_se_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, novel_claim_plaintiffs).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, small_firm_counsel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, institutional_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides, case by case, how much weight a given precedent carries in a given domain — treating tax precedent as nearly binding while treating tort or constitutional precedent as more revisable. Administers the calibration itself: which factors (domain, reliance interests, subsequent doctrinal drift, panel composition) justify departure. Retains discretion to reclassify a case's domain when convenient.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Practice repeatedly in a narrow domain (e.g., patent, admiralty, tax) and build deep familiarity with how much weight precedent actually carries there. This domain-specific knowledge is a durable competitive advantage over generalist counsel and functions as an informal barrier to entry.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, specialist_bar_advocates, beneficiary,
    organized, biographical, arbitrage, national).

% Repeat players (insurers, large corporations, government agencies) who litigate across many domains and can afford counsel who track the varying precedent weight in each. They can forum-shop or venue-shop toward doctrinal areas where precedent is treated more favorably to their position, and absorb the occasional adverse departure as a cost of doing business at scale.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, institutional_litigants, beneficiary,
    powerful, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, institutional_litigants, payer).

% Cannot know in advance how much weight the controlling precedent will actually be given in their case's domain, since that weight is itself a contextual judgment made after the fact. Bear the cost of unpredictability directly: an outcome that looks settled on paper can be reopened as 'context-appropriate adaptation,' and they lack the resources to argue the domain classification itself.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, pro_se_litigants, payer,
    powerless, immediate, trapped, local).

% Bring claims that straddle doctrinal domains (e.g., a claim mixing tort and regulatory theory) and cannot predict which domain's precedent-weight convention will be applied, or by whom. Litigation strategy must hedge against multiple possible treatments of the same precedent, multiplying cost and risk without a corresponding increase in claim value.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, novel_claim_plaintiffs, payer,
    moderate, biographical, constrained, regional).

% Lack the cross-domain research capacity of large firms to track how precedent weight is currently calibrated in each area of practice. Must either specialize narrowly (forgoing broader practice) or risk mis-predicting how much a precedent will bind in an unfamiliar domain, both of which are competitive disadvantages relative to institutional and specialist counterparts.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, small_firm_counsel, payer,
    moderate, biographical, constrained, regional).

% Argue that any domain-contextual weighting is itself a disguised license for result-oriented departure from precedent, undermining the rule-of-law value of predictability. They participate in scholarly and judicial-selection debates but do not control the doctrine actually applied in any given panel's opinion.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, strict_stare_decisis_advocates, excluded,
    organized, generational, analytical, national).

% Study and critique how courts actually calibrate precedent weight across domains, publish empirical analyses of doctrinal drift, and testify before legislative and judicial-reform bodies without directly bearing litigation costs or collecting litigation revenue.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_academics, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows courts to preserve the stabilizing, reliance-protecting function of precedent in domains where predictability is paramount (property, commercial transactions) while permitting doctrinal correction in domains where social or scientific understanding evolves (tort, constitutional rights, family law) — avoiding both ossification and unchecked judicial revision.
% TRANSFER_FUNCTION: Moves predictability and litigation-cost-efficiency toward repeat players and domain specialists who can track the current calibration, and moves unpredictability and hedging costs onto one-off litigants, cross-domain claimants, and counsel without the resources to monitor doctrinal drift across multiple fields.
% ABSENT_VOICES: Strict stare decisis advocates object that contextual weighting is a covert vehicle for result-oriented reasoning, but their critique operates at the level of jurisprudential theory and rarely displaces a sitting panel's discretion in an individual case. Pro se litigants who are directly harmed by unpredictable domain reclassification are almost never in a position to raise the objection effectively — they lack the doctrinal vocabulary and appellate access to contest the classification itself.
% DISAPPEARANCE_RATIONALE: Institutional litigants, specialist bar, and the judiciary itself would say the system would become either rigid (if replaced by strict stare decisis) or unstable (if replaced by unconstrained evolutionary reinterpretation) — arrangements built around the current calibration (specialist practices, doctrinal treatises organized by domain) would need to reorganize. Strict stare decisis advocates and some novel-claim plaintiffs would say the underlying unpredictability the arrangement produces would simply be replaced by a different, possibly clearer, rule — so the verdict is genuinely disputed rather than settled.
% FOUNDING_PROBLEM: Common law needed a way to be reliable enough for people to plan around while remaining capable of correcting bad or outdated rules without waiting for legislative action — pure rigidity ossifies error, pure flexibility destroys the reliance interest precedent is supposed to protect.
% FOUNDING_PROBLEM_CORROBORATION: Appellate judges and specialist bar organizations attest the domain-sensitive balancing still serves this founding function, citing areas like commercial law where predictability remains high alongside tort law where doctrine has visibly evolved. Legal academics studying doctrinal drift and strict-stare-decisis scholars attest, from outside the beneficiary set, that in practice the calibration is frequently invoked post hoc to justify results reached on other grounds, and that pro se and novel-claim litigants bear the resulting unpredictability without any corresponding voice in how domains are classified.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, contested).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52) reflects that this reading's discretion is genuinely exploitable by repeat players without being nakedly extractive — it is lower than a pure snare but non-trivial because domain classification decisions are made by the same body that benefits from appearing principled while retaining flexibility. Suppression (0.44) is moderate: there is no formal bar to raising a precedent-weight argument, but the practical capacity to contest a domain classification is unevenly distributed. Theater ratio (0.31) captures that some domain-sensitivity rulings perform careful doctrinal reasoning that functions primarily to justify a result reached on other grounds. Accessibility collapse (0.40) and resistance (0.55) are mid-range: alternatives (clearer per-domain rules, codified weighting factors) exist and are actively argued for by strict-stare-decisis critics, so this is not a naturalized, unquestioned arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate judges sit as agenda-setters (analytical exit, institutional power) who administer rather than bear the calibration. Specialist bar advocates and institutional litigants derive low-to-symmetric directionality: they benefit from tracking the calibration and have mobile/arbitrage exit (venue selection, counsel selection). Pro se litigants, novel-claim plaintiffs, and small-firm counsel derive high directionality: trapped or constrained exit, no capacity to contest domain classification, and the unpredictability cost lands squarely on them. This asymmetry is exactly what the tangled-rope classification requires: a genuine coordination function (avoiding ossification/chaos) coexisting with asymmetric extraction (unpredictability costs concentrated on the least-resourced litigants) enforced through the same doctrinal machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — balancing reliance interests against the need for doctrinal correction — remains partly live (some domains genuinely need updating; commercial law genuinely benefits from stability). This prevents a blanket 'pure extraction' reading. But the founding-problem corroboration surfaces contested status: outside observers (academics, strict-stare-decisis scholars) report the calibration is frequently invoked post hoc, which is the signature of mandatrophy not yet fully resolved — the arrangement retains its coordination justification in name while its discretionary machinery increasingly serves classification-shopping by those able to track it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_classification_discretion_ambiguity,
    'Is the discretion to classify a case''s controlling domain (and thus its precedent weight) a principled application of jurisprudential theory, or is it a post hoc rationalization mechanism that tracks desired outcomes?',
    'Empirical study comparing stated domain classifications against outcome predictions from case facts alone (blind coding), tracking whether classification correlates more with doctrinal factors or with litigant identity/resources.',
    'If classification tracks litigant resources or identity more than doctrinal factors, the tangled_rope reading strengthens toward snare; if it tracks doctrinal factors consistently, the coordination function is more robust than critics claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_classification_discretion_ambiguity, empirical, 'Whether domain classification discretion is principled or outcome-driven.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the pluralist-balancing reading itself a stable jurisprudential commitment, or is it the label courts apply retroactively to justify oscillating between strict-stare-decisis and evolutionary-framework behavior depending on the case?',
    'Track whether courts announce ex ante which domains receive which weighting treatment (supporting a stable pluralist commitment) versus only articulating the balancing rationale in the opinion after reaching a result (supporting the oscillation-relabeled-as-pluralism hypothesis).',
    'If pluralist balancing is genuinely a distinct stable commitment, it is a coherent third kernel reading; if it is a post hoc label for oscillation between the other two readings, the kernel effectively has two live readings, not three, and this story''s distinctiveness is a documentary artifact rather than a jurisprudential fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether pluralist balancing is a distinct stable reading or a retroactive label for oscillation between siblings.').

omega_variable(
    specialist_advantage_natural_or_constructed,
    'Is the advantage specialist bar advocates and institutional litigants hold over pro se and novel-claim litigants an inevitable feature of any sufficiently complex legal system, or is it specifically amplified by the domain-contextual weighting convention (as opposed to a uniform-rule regime)?',
    'Comparative analysis of jurisdictions using strict stare decisis versus pluralist balancing, measuring outcome variance and cost differentials between represented and unrepresented litigants in each regime.',
    'If the specialist advantage is comparable across regimes, this reading is not distinctively extractive relative to its siblings; if pluralist balancing measurably widens the gap, this reading''s extraction is a specific cost of its context-sensitivity design choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(specialist_advantage_natural_or_constructed, empirical, 'Whether specialist/institutional advantage is regime-specific to pluralist balancing or a general feature of legal complexity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comm_tr_t8, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 8, 0.23).
narrative_ontology:measurement(comm_tr_t16, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 16, 0.26).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 24, 0.28).
narrative_ontology:measurement(comm_tr_t32, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 32, 0.3).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comm_be_t8, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(comm_be_t16, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(comm_be_t32, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(comm_su_t8, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(comm_su_t16, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 24, 0.41).
narrative_ontology:measurement(comm_su_t32, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 32, 0.43).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 40, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__pluralist_balancing, 0.1).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, evolutionary_framework).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the common_law_precedent_corpus kernel. strict_stare_decisis treats precedent as near-uniformly binding (lower context-dependent variance, higher rigidity, more predictable but more ossification risk). evolutionary_framework treats precedent as generally open to contemporary reinterpretation (lower rigidity, higher adaptation capacity, less reliance protection). pluralist_balancing (this story) sits structurally between them: medium rigidity, explicit domain-by-domain calibration, and — distinctively — a multi-tier extractiveness profile where the calibration discretion itself becomes a resource that repeat players and specialists can exploit in ways the more uniform sibling readings do not as directly permit. Each reading has its own ε, beneficiary/victim structure, and stakeholder set; they are linked here rather than merged into one constraint per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
