% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Precedent Weight Balancing Doctrine (Domain- and Context-Sensitive Stare Decisis)
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This story instantiates the 'pluralist balancing' reading of the
 *   common-law-precedent kernel: precedent weight is neither uniformly
 *   binding (strict_stare_decisis) nor generally open to reinterpretation
 *   under evolving norms (evolutionary_framework), but is calibrated
 *   case-by-case according to domain, pedigree, and reliance interests. The
 *   reading's own claim is that this produces genuine coordination —
 *   stability where needed, adaptation where needed. The metrics authored
 *   here describe a structurally different reality: the very flexibility that
 *   makes balancing attractive in principle creates a multi-tier system where
 *   sophisticated repeat players learn to navigate the weighting factors
 *   while unsophisticated litigants experience an unpredictable, effectively
 *   unreviewable judicial discretion. The ε value (0.52) is authored for the
 *   pluralist-balancing arrangement as this reading's own lights assess it —
 *   moderate but rising extraction, not the near-zero or near-total
 *   extraction that would attach to the sibling readings' own arrangements.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: sets and administers the weighting framework, retains discretion, institutional/analytical exit
 *   - repeat_litigants_with_domain_expertise: arbitrages domain-specific weighting practices, powerful/arbitrage exit
 *   - pro_se_litigants: bears unpredictability cost with no capacity to model weighting factors, powerless/trapped
 *   - criminal_appellants: liberty interests turn on unreviewable weighting posture, powerless/trapped
 *   - legal_realist_observers: analytical seat documenting outcome variance by litigant sophistication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.52).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.44).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.52).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Precedent Weight Balancing Doctrine (Domain- and Context-Sensitive Stare Decisis)").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, 'ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48').
narrative_ontology:cs_kernel_codification('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48', distributed).
narrative_ontology:cs_authority_grounding('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48', practice).
narrative_ontology:cs_interpretation_layer_present('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48').
narrative_ontology:cs_reading_relation('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48', foundational, precedent_weight_is_domain_relative).
narrative_ontology:cs_axiom_status(precedent_weight_is_domain_relative, holdable).
narrative_ontology:cs_axiom_grounding('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48', precedent_weight_is_domain_relative, instrumental).
narrative_ontology:cs_axiom('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48', foundational, judicial_discretion_over_weighting_criteria_is_legitimate).
narrative_ontology:cs_axiom_status(judicial_discretion_over_weighting_criteria_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48', judicial_discretion_over_weighting_criteria_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48', case_by_case_domain_sensitive_weighting).
narrative_ontology:cs_drift_state('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48', post_administrative_state_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab26306f-6ab0-42a2-aa3f-0fdb2c2b3e48', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, repeat_litigants_with_domain_expertise).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, constitutional_law_scholars).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, pro_se_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, small_business_defendants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, criminal_appellants).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, law_as_living_practical_reason).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, judicial_discretion_as_legitimate_craft).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides, case by case, how much weight a given precedent deserves based on the domain (constitutional, commercial, criminal, tort), the age and pedigree of the precedent, and perceived social change. This grants judges the tool to distinguish inconvenient precedent without formally overruling it, and to treat other precedent as near-absolute when it serves institutional stability. The judiciary controls the very framework used to evaluate its own consistency.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, beneficiary).

% Corporations, government litigators, and specialized firms that appear repeatedly before the same courts learn which domains get high-rigidity treatment and which get flexible treatment. They forum-shop, sequence litigation, and frame arguments around the domain-specific weighting rules, effectively arbitraging the variable stability standard that costs unsophisticated parties dearly.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, repeat_litigants_with_domain_expertise, beneficiary,
    powerful, biographical, arbitrage, national).

% Academic careers are built on theorizing which factors should weigh precedent more or less heavily in which domains. The doctrine's inherent indeterminacy generates a perpetual supply of contestable questions, law review output, and judicial clerkship pipelines organized around mastering the balancing framework.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, constitutional_law_scholars, beneficiary,
    organized, civilizational, analytical, national).

% Self-represented parties cannot predict whether the precedent governing their case will be treated as binding or distinguishable because that determination depends on domain-sensitive factors known mainly to specialists. They lose cases not on the merits but on failure to anticipate how much weight a court will assign to controlling authority, with no meaningful path to appeal the weighting decision itself.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, pro_se_litigants, payer,
    powerless, immediate, trapped, local).

% Face unpredictable outcomes when precedent in commercial law is treated as flexible in one circuit and rigid in another, or shifts weight depending on framing as contract, tort, or regulatory. They lack the litigation budget to test multiple framings and often settle rather than risk an adverse balancing determination they cannot forecast.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, small_business_defendants, payer,
    moderate, biographical, constrained, regional).

% Liberty interests turn on whether a court treats sentencing or procedural precedent as tightly binding or as ripe for domain-specific reweighing given 'evolving standards.' Appellants have no control over which posture a panel adopts and bear the cost of the resulting unpredictability with prison time, not money.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, criminal_appellants, payer,
    powerless, immediate, trapped, national).

% Would prefer clear rules they can codify around, but the balancing framework keeps significant swaths of law in judge-administered flux, making legislative override costly and uncertain. They are formally supreme but structurally sidelined from the day-to-day operation of precedent weighting.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legislatures, excluded,
    institutional, generational, constrained, national).

% Empirical scholars who track outcome variance across domains and courts, documenting where the balancing framework produces genuine adaptive coordination versus where it functions as unreviewable judicial discretion dressed in doctrinal language.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_realist_observers, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__pluralist_balancing, repeat_litigants_with_domain_expertise).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__pluralist_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows the legal system to preserve stability where reliance interests are strong (property, contract formation) while permitting adaptation where social or factual conditions have changed (family law, constitutional rights, technology-adjacent regulation) — solving the genuine problem that a single fixed stringency for precedent would either freeze the law against manifest injustice or destabilize every settled expectation.
% TRANSFER_FUNCTION: Moves predictability away from parties without the resources to model domain-specific weighting practices (pro se litigants, small businesses, criminal appellants) toward parties who can invest in mastering and gaming those practices (repeat institutional litigants, specialized appellate counsel, the judiciary itself, which retains discretion over the very framework).
% ABSENT_VOICES: Legislatures, whose codified rules would remove the discretion the balancing framework preserves, are structurally sidelined — override requires supermajorities or constitutional amendment, while judges adjust weighting incrementally through opinion-writing. Ordinary litigants who bear the unpredictability cost are not consulted on how weighting factors are set.
% DISAPPEARANCE_RATIONALE: If domain-sensitive balancing vanished overnight in favor of either strict rule-bound stare decisis or an unconstrained evolutionary posture, litigation strategy, forum selection, appellate briefing practice, and the entire academic apparatus theorizing precedent weight would need to reorganize around a single, more predictable (or more explicitly political) standard. Institutional litigants who currently profit from domain arbitrage would lose that edge; unsophisticated litigants would gain predictability but lose case-specific equitable outcomes.
% FOUNDING_PROBLEM: Neither absolute rule-boundedness (which produces injustice when circumstances the rule never anticipated arise) nor unconstrained case-by-case reinvention (which destroys the reliance interests law exists to protect) adequately serves a legal system that must handle both stable commercial expectations and evolving social and constitutional questions within the same body of doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Judges and legal academics (largely inside the beneficiary set) attest the balancing approach remains necessary and functioning as designed. Empirical legal-realist scholarship and access-to-justice researchers, outside the beneficiary set, document that domain-sensitive weighting correlates strongly with litigant sophistication and resources rather than with principled doctrinal factors, suggesting the founding coordination problem persists in name while the operative function has partly shifted toward discretion-preservation for the judiciary and arbitrage for repeat players.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction (0.52) sits at a medium level reflecting the reading's own structural delta: not the near-zero extraction a strict rule-bound regime would claim for its own predictability, nor the high extraction a critic of unconstrained reinterpretation might assign to the evolutionary reading's own arrangement. Suppression (0.44) is moderate because domain-sensitive balancing does not formally foreclose alternative arguments, but it does require litigants to correctly anticipate an unpublished weighting calculus, which functions as a soft barrier. Theater ratio (0.38) reflects that some balancing-factor recitation in opinions is genuine analytical work and some is post-hoc justification for outcomes reached on other grounds. Accessibility collapse (0.40) is moderate — alternative legal theories remain formally available, but practical navigation collapses toward those who can afford domain expertise. All temporal metrics run on one shared 0/12/24/36/48/60 grid.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, domain-sensitive balancing is genuine institutional coordination — a craft-based tool for reconciling stability and justice across radically different legal domains. From the pro se litigant's or criminal appellant's seat, the identical mechanism functions as unreviewable, unpredictable discretion masquerading as principled doctrine. The engine should compute divergent per-seat types from this same structural data: the agenda_setter and sophisticated-beneficiary seats likely compute toward rope/tangled_rope, while the powerless payer seats likely compute toward tangled_rope or snare given trapped exit options and immediate time horizons.
 *
 * DIRECTIONALITY LOGIC:
 *   The appellate judiciary sits nearest the beneficiary end: it authors and administers the weighting framework and bears none of the unpredictability cost. Repeat litigants with domain expertise and constitutional scholars are also beneficiaries — they either arbitrage the variable standard or build careers theorizing it. Pro se litigants, small business defendants, and criminal appellants are victims: trapped or constrained exit, immediate or biographical time horizons, and no capacity to model or influence the domain-specific weighting that determines their outcomes. Legislatures are excluded rather than coordinated — formally supreme but functionally sidelined from the operative weighting practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling stable reliance interests with adaptive justice) remains genuinely live in the abstract — legal systems facing both settled commercial expectations and evolving social questions do need some mechanism to differentiate. But the specific balancing framework as currently administered has drifted from that founding function toward serving as a discretion-preservation and arbitrage-enablement mechanism: the judiciary retains maximal interpretive latitude, and sophisticated litigants monetize the resulting unpredictability. Classifying this as tangled_rope rather than snare or rope is the mandatrophy-relevant move: it prevents mislabeling a doctrine that still performs real coordination work (differential treatment across genuinely different domains is not obviously illegitimate) as pure extraction, while also refusing to certify it as clean coordination given the documented, resource-correlated outcome variance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pluralist_balancing_kernel_reading,
    'Is domain-sensitive precedent weighting a distinct, coherent doctrinal reading of stare decisis, or is it simply strict_stare_decisis and evolutionary_framework applied inconsistently by different panels without a stable underlying principle?',
    'Track whether courts articulate a consistent, predictable set of domain-classification rules that determine weighting ex ante (supporting a genuine third reading) versus whether weighting outcomes correlate primarily with panel composition or litigant sophistication after the fact (supporting the inconsistency hypothesis).',
    'If the pluralist reading collapses into inconsistent application of the other two readings, this story''s independent ε and classification would not hold — it would need to be re-absorbed into whichever sibling reading actually governs a given domain, rather than standing as its own kernel reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pluralist_balancing_kernel_reading, conceptual, 'Whether pluralist balancing is a genuine third reading or an artifact of inconsistent application of the other two.').

omega_variable(
    domain_classification_disagreement_locus,
    'Where exactly do the three sibling readings disagree — is it about how much weight precedent should carry across ALL domains uniformly, or specifically about who gets to decide, case-by-case, which domain-classification rule applies?',
    'Compare judicial opinions that explicitly invoke each reading''s rationale (strict textual fidelity to precedent vs. contemporary values vs. domain-context balancing) to determine whether the disagreement is about the weighting rule itself or about the meta-level authority to select among weighting rules.',
    'If the disagreement is meta-level (about selection authority), pluralist_balancing effectively subsumes the discretion both siblings claim to constrain, which would justify a stronger ''influences'' or even ''forecloses'' relation rather than mere coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_classification_disagreement_locus, conceptual, 'Locating whether sibling readings disagree on weighting substance or on selection authority.').

omega_variable(
    beneficiary_capture_of_discretion,
    'Is the judiciary''s retention of domain-sensitive discretion best understood as an institutional beneficiary capturing the framework it administers, or as a necessary structural feature of any workable balancing doctrine?',
    'Compare jurisdictions or eras with more constrained, codified domain-classification rules (reducing judicial discretion) against those with open-ended balancing, measuring whether outcome predictability and litigant-resource correlation differ.',
    'If constrained-discretion jurisdictions show materially better predictability without sacrificing adaptive capacity, this supports reading the current open-ended balancing as tangled_rope drifting toward snare; if predictability does not improve, it supports treating judicial discretion as intrinsic to the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_discretion, empirical, 'Whether judicial discretion over weighting is capture or structural necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.22).
narrative_ontology:measurement(comm_tr_t12, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 12, 0.26).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 24, 0.3).
narrative_ontology:measurement(comm_tr_t36, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 36, 0.33).
narrative_ontology:measurement(comm_tr_t48, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 48, 0.36).
narrative_ontology:measurement(comm_tr_t60, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 60, 0.38).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(comm_be_t12, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(comm_be_t36, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 36, 0.47).
narrative_ontology:measurement(comm_be_t48, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 48, 0.5).
narrative_ontology:measurement(comm_be_t60, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comm_su_t12, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(comm_su_t36, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 36, 0.39).
narrative_ontology:measurement(comm_su_t48, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 48, 0.42).
narrative_ontology:measurement(comm_su_t60, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 60, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__pluralist_balancing, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the common_law_precedent_corpus kernel. strict_stare_decisis authors precedent as a near-binding backward constraint (lower context-variance, higher formal rigidity, ε assessed against that arrangement's own coordination claim). evolutionary_framework authors precedent as an adaptive vehicle for contemporary normative reinterpretation (lower formal rigidity, ε assessed against that arrangement's own claim to legitimate adaptation). pluralist_balancing (this story) authors an intermediate, domain-sensitive weighting practice whose own distinctive risk is multi-tier extractiveness driven by unpredictable domain-switching costs rather than uniform rigidity or uniform flexibility. Each story's ε is stable and assessed against its own reading's standing arrangement; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
