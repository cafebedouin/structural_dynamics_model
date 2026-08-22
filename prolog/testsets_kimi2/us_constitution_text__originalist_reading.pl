% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Original Public Meaning Constraint on Constitutional Interpretation
 *   domain: constitutional law / legal philosophy / interpretive theory
 *
 * SUMMARY:
 *   This constraint story models the originalist reading of the U.S.
 *   Constitution as an institutionalized interpretive methodology:
 *   constitutional meaning is fixed at ratification, and judges must recover
 *   the original public understanding through historical evidence. The
 *   constraint operates not merely as an academic theory but as an active
 *   enforcement structure within the federal judiciary, controlling which
 *   sources are admissible, which arguments win, and which rights-claims are
 *   cognizable. It is claimed as a coordination mechanism (judicial
 *   constraint, democratic legitimacy) but functions as a tangled rope: it
 *   genuinely stabilizes interpretation while asymmetrically extracting
 *   interpretive possibility from rights-claimants and transferring
 *   institutional dominance to a specific political movement.
 *
 * KEY AGENTS:
 *   - conservative_legal_movement: Primary beneficiary (institutional/generational) â captures judicial appointments and doctrinal outcomes
 *   - adaptive_rights_claimants: Primary target (powerless/constrained) â bear the cost of foreclosed interpretive avenues
 *   - originalist_judiciary: Agenda-setter/enforcer (institutional/identity_locked) â controls docket and opinion-writing, enforces methodological boundaries
 *   - living_constitutionalist_jurists: Excluded alternative (institutional/constrained) â present in dissent, excluded from controlling methodology
 *   - legal_historians: Analytical observer (moderate/mobile) â provide empirical findings that are selectively appropriated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.7).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.82).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Original Public Meaning Constraint on Constitutional Interpretation").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "constitutional law / legal philosophy / interpretive theory").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, 'e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22').
narrative_ontology:cs_kernel_codification('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22', fixed_text).
narrative_ontology:cs_authority_grounding('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22', lineage).
narrative_ontology:cs_interpretation_layer_present('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22').
narrative_ontology:cs_reading_relation('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22', us_constitution_text__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22', secondary, judicial_discretion_constrained_by_history).
narrative_ontology:cs_axiom_status(judicial_discretion_constrained_by_history, holdable).
narrative_ontology:cs_axiom_grounding('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22', judicial_discretion_constrained_by_history, conventional).
narrative_ontology:cs_reference_frame('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22', contemporary_jurisprudence, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('e0f79d99-c3bf-41a8-b5af-d6ccd87c4f22', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, adaptive_rights_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls judicial appointment pipelines, law school faculty hiring, and clerkship networks. Benefits from an interpretive method that narrows constitutional outcomes to those compatible with 18th- and 19th-century social practice, securing durable legal victories on regulatory, social, and cultural issues without winning legislative majorities.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary,
    institutional, generational, mobile, national).

% Individuals and groups seeking constitutional protection for rights not demonstrably grounded in the original public meaning of the ratified text (e.g., privacy, reproductive autonomy, LGBTQ+ equality). Their claims are systematically disadvantaged by a framework that treats post-ratification social change as irrelevant to constitutional meaning unless encoded in formal amendment.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, adaptive_rights_claimants, payer,
    powerless, biographical, constrained, national).

% Federal judges who self-identify as originalists and apply the methodology in constitutional adjudication. They control opinion-writing and docket filtering, enforce the exclusion of non-originalist sources (contemporary moral reasoning, foreign law, living tradition), and derive professional legitimacy from claims of constraint by historical meaning.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Judges and scholars who hold that constitutional meaning evolves with society. In an originalist-dominant institutional configuration, their methodological approach is formally heard in dissent but structurally excluded from controlling opinions on contested constitutional questions.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_jurists, excluded,
    institutional, generational, constrained, national).

% Professional historians who study the founding era. Their empirical findings may support or undermine originalist claims, but their methodological standards (contextualism, skepticism about presentist framing) are selectively appropriated by originalist jurists and ignored when they challenge originalist conclusions.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, legal_historians, observer,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stabilizing interpretive framework that constrains judicial discretion and ties constitutional law to a democratically enacted text, offering predictability and a bulwark against arbitrary judicial rule.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary moral reasoning and adaptive rights-claiming to historical research and originalist legal academia; moves legal outcomes toward regulatory and social arrangements compatible with ratification-era practice.
% ABSENT_VOICES: Living constitutionalist jurists and marginalized rights-seekers whose claims depend on constitutional evolution are formally present in dissent but structurally excluded from controlling interpretive methodology; their methodological objections are treated as anti-democratic judicial activism.
% DISAPPEARANCE_RATIONALE: If the original public meaning constraint vanished, constitutional adjudication would shift to contemporary-value balancing, precedent-driven evolution, or moral reasoning; the conservative legal movement's institutional dominance would lose its primary methodological anchor, and significant doctrinal constraints on federal power and individual rights would become immediately contestable.
% FOUNDING_PROBLEM: Judicial review in a democracy faces a legitimacy problem: unelected judges overriding popular majorities. Fixing constitutional meaning to the ratified text was designed to constrain judges and ground their authority in democratic enactment rather than contemporary preference.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars attest the problem is live (judicial restraint). Progressive legal scholars and historians attest the founding problem has been repurposed â that originalism now functions to empower a specific judicial coalition rather than constrain judges generally. Empirical studies of judicial behavior from outside the benefiting movement suggest originalist judges are no more constrained than their non-originalist peers.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70) is high because the constraint systematically forecloses constitutional arguments grounded in contemporary moral evolution, effectively taxing rights-claims that lack 18th-century analogues. Suppression (0.82) is higher still because the constraint's persistence depends on active gatekeeping: exclusion of non-originalist sources from controlling opinions, credentialing barriers in the legal academy, and appointment filtration. Theater ratio (0.55) reflects the 'law office history' critique â a growing share of originalist argumentation deploys historical rhetoric selectively to reach predetermined outcomes. Accessibility collapse (0.78) is high because once originalism is accepted as the controlling framework, non-originalist alternatives become nearly illegible within the federal appellate system. Resistance (0.60) is moderate-to-high because living constitutionalist jurists and progressive social movements actively contest the framework in dissent and extrajudicial discourse.
 *
 * PERSPECTIVAL GAP:
 *   The conservative legal movement experiences this constraint as a rope â a necessary coordination device that constrains rogue judges and preserves democratic legitimacy. Adaptive rights-claimants experience it as a snare â a structure whose coordination story is cover for blocking their claims. The engine computes this divergence from the structural data: same constraint, same metrics, different directionality derived from beneficiary versus victim position.
 *
 * DIRECTIONALITY LOGIC:
 *   The conservative legal movement sits at the beneficiary end (d near 0.0): the constraint subsidizes their institutional dominance by constitutionalizing their policy preferences without legislative majority-building. Adaptive rights-claimants sit at the target end (d near 1.0): they bear the extraction through systematically lost cases and foreclosed doctrinal paths. The originalist judiciary sits near symmetric but agenda-setter-biased: their professional identity is fused to the method (identity_locked), giving them low personal exit but high control. Legal historians sit near neutral observer positions with mobile exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents the false binary of calling originalism either pure coordination (ignoring the asymmetric beneficiary structure) or pure extraction (ignoring the genuine stabilization and constraint functions it provides). The mandate â constraining judges to democratically ratified text â is partially live (some constraint on pure preference) and partially dead (selective application, outcome-correlated invocation), producing the hybrid classification. The temporal measurements show extraction and theater accumulating over the interval, suggesting coordination has been progressively colonized by extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_reading_kernel_location,
    'This constraint is the originalist reading of the us_constitution_text kernel. Sibling readings (living_constitutionalist, positivist) produce different constraints with different beneficiary structures and epsilon profiles. Does the disagreement originate in the ontology of meaning, the epistemology of interpretation, or the sociology of legal authority?',
    'Comparative structural analysis of the three readings'' outputs: if epsilon and directionality diverge primarily by beneficiary set, the disagreement is institutional; if by accessibility_collapse metrics, it is epistemic.',
    'Determines whether the kernel is a single constraint with measurement-dependent epsilon (violating epsilon-invariance) or three distinct constraints linked by network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_reading_kernel_location, conceptual, 'Structural location of disagreement across kernel readings').

omega_variable(
    historical_meaning_determinacy,
    'For contested constitutional provisions, is the original public meaning sufficiently determinate to resolve modern disputes, or does the method necessarily collapse into covert preference or living constitutionalism at the point of application?',
    'Forensic linguistic and historical analysis of originalist opinions: measure the variance in originalist judges'' conclusions on identical historical questions, controlling for political valence.',
    'If meaning is systematically indeterminate, the extraction is higher than the coordination â the constraint functions as a snare with a coordination cover story. If determinate, the tangled rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_determinacy, empirical, 'Whether original public meaning provides real constraint or performative cover').

omega_variable(
    originalism_as_false_summit,
    'Originalism claims to discover fixed historical meaning as a natural feature of legal reality. Yet identifiable beneficiaries (conservative legal movement) extract institutional dominance from this framing. Is this a genuine interpretive necessity or a constructed tangled rope benefiting specific agents?',
    'Historical sociology of the originalist movement: trace funding, career pipelines, and outcome distributions from 1980â2024. If the method''s dominance correlates with partisan institutional capture and its fixed-meaning claims track political convenience, the natural-law claim is falsified.',
    'Would reclassify the constraint''s claim toward snare if the historical-meaning claim is predominantly performed rather than epistemically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_as_false_summit, empirical, 'Whether originalism is a natural interpretive law or a constructed benefit structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_const_orig_tr_t0, us_constitution_text__originalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_const_orig_tr_t8, us_constitution_text__originalist_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(us_const_orig_tr_t16, us_constitution_text__originalist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(us_const_orig_tr_t24, us_constitution_text__originalist_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(us_const_orig_tr_t32, us_constitution_text__originalist_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement(us_const_orig_tr_t40, us_constitution_text__originalist_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(us_const_orig_be_t0, us_constitution_text__originalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(us_const_orig_be_t8, us_constitution_text__originalist_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(us_const_orig_be_t16, us_constitution_text__originalist_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(us_const_orig_be_t24, us_constitution_text__originalist_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(us_const_orig_be_t32, us_constitution_text__originalist_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(us_const_orig_be_t40, us_constitution_text__originalist_reading, base_extractiveness, 40, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(us_const_orig_su_t0, us_constitution_text__originalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(us_const_orig_su_t8, us_constitution_text__originalist_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(us_const_orig_su_t16, us_constitution_text__originalist_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(us_const_orig_su_t24, us_constitution_text__originalist_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(us_const_orig_su_t32, us_constitution_text__originalist_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(us_const_orig_su_t40, us_constitution_text__originalist_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'U.S. Constitution' conflates multiple structurally distinct constraints. This file isolates the originalist reading (fixed original public meaning). Sibling files isolate the living constitutionalist reading (evolving meaning) and the positivist reading (enactment procedure). Each has distinct epsilon, beneficiary/victim structure, and classification. They form a constraint family linked by kernel_id us_constitution_text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
