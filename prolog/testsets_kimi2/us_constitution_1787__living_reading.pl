% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living Constitutionalism: Evolving Aspirational Framework
 *   domain: constitutional/law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the living reading of the
 *   us_constitution_1787 kernel: the claim that constitutional meaning
 *   evolves with society and the text functions as an aspirational framework
 *   rather than a fixed rule-set. Under this reading, the federal judiciary
 *   claims authority to derive modern rights (privacy, dignity, substantive
 *   due process) and to invalidate democratically enacted statutes that
 *   conflict with evolved norms. The arrangement coordinates societal
 *   adaptation across centuries but concentrates interpretive power in an
 *   unelected judiciary, creating a structurally asymmetric transfer of
 *   authority from legislatures to courts. This story does not adjudicate the
 *   correctness of the reading; it models the constraint the reading
 *   produces.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Primary agenda-setter and beneficiary (institutional/analytical) â administers the living reading and accumulates interpretive authority
 *   - rights_advocates: Primary beneficiary (organized/constrained) â gains expanded protections through judicial recognition of evolving norms
 *   - elected_legislatures: Primary target (institutional/constrained) â bears loss of policy autonomy through judicial invalidation
 *   - originalist_jurists: Excluded voice (organized/constrained) â structurally marginalized in dominant constitutional doctrine
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â provides external critique and corroboration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.62).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.55).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living Constitutionalism: Evolving Aspirational Framework").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional/law/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, '6eeea1ea-43ff-4ab0-a74a-745708aa0b96').
narrative_ontology:cs_kernel_codification('6eeea1ea-43ff-4ab0-a74a-745708aa0b96', fixed_text).
narrative_ontology:cs_authority_grounding('6eeea1ea-43ff-4ab0-a74a-745708aa0b96', lineage).
narrative_ontology:cs_interpretation_layer_present('6eeea1ea-43ff-4ab0-a74a-745708aa0b96').
narrative_ontology:cs_reading_relation('6eeea1ea-43ff-4ab0-a74a-745708aa0b96', us_constitution_1787__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('6eeea1ea-43ff-4ab0-a74a-745708aa0b96', us_constitution_1787__positivist_reading, forecloses).
narrative_ontology:cs_axiom('6eeea1ea-43ff-4ab0-a74a-745708aa0b96', foundational, constitutional_meaning_evolves_with_society).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_society, holdable).
narrative_ontology:cs_axiom_grounding('6eeea1ea-43ff-4ab0-a74a-745708aa0b96', constitutional_meaning_evolves_with_society, deontological).
narrative_ontology:cs_axiom('6eeea1ea-43ff-4ab0-a74a-745708aa0b96', foundational, judicial_prerogative_to_update_meaning).
narrative_ontology:cs_axiom_status(judicial_prerogative_to_update_meaning, holdable).
narrative_ontology:cs_axiom_grounding('6eeea1ea-43ff-4ab0-a74a-745708aa0b96', judicial_prerogative_to_update_meaning, conventional).
narrative_ontology:cs_reference_frame('6eeea1ea-43ff-4ab0-a74a-745708aa0b96', aspirational_constitutional_framework).
narrative_ontology:cs_drift_state('6eeea1ea-43ff-4ab0-a74a-745708aa0b96', contemporary_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6eeea1ea-43ff-4ab0-a74a-745708aa0b96', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, rights_advocates).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, elected_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims authority to interpret the Constitution in light of contemporary values and evolving social conditions, deriving rights and constraints not explicit in the text. Administers the living reading through judicial review, accumulating institutional supremacy over elected branches by asserting the final say on constitutional meaning.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, federal_judiciary, beneficiary).

% Litigate and advocate for expanding constitutional protections (privacy, dignity, autonomy, equality) under the evolving framework. Benefit when courts recognize modern rights claims that legislative majorities have failed to enact or have actively rejected.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, rights_advocates, beneficiary,
    organized, biographical, constrained, national).

% Enact statutes reflecting contemporary policy preferences and majority will; face invalidation when federal courts declare those laws incompatible with evolved constitutional meaning or newly discovered rights. Bound by judicial supremacy with no practical exit from the federal constitutional order short of amendment.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, elected_legislatures, payer,
    institutional, generational, constrained, national).

% Advance interpretive methods tied to fixed textual meaning and ratification-era understanding. Marginalized in courts and legal education committed to the living reading; their arguments are rarely adopted in majority opinions but persist in dissents and academic discourse.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_jurists, excluded,
    organized, generational, constrained, national).

% Analyze and critique the living reading's doctrinal development, comparing it against originalist and positivist alternatives. Provide external corroboration or challenge to claims about the Constitution's design and the judiciary's proper role.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__living_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_1787__living_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional law to adapt to unanticipated social, technological, and moral conditions without requiring formal amendment, maintaining continuity across centuries.
% TRANSFER_FUNCTION: Moves interpretive authority and policy-making power from elected legislatures and popular majorities to the federal judiciary and constitutional rights advocates.
% ABSENT_VOICES: Originalist jurists and textualist scholars are structurally excluded from dominant constitutional doctrine; democratic majorities whose legislation is invalidated have no direct voice in the interpretive method selection.
% DISAPPEARANCE_RATIONALE: If the living reading vanished overnight, decades of constitutional doctrine (privacy, dignity, substantive due process) would collapse; legislatures would regain broad policy autonomy and the judiciary would lose its expansive review authority.
% FOUNDING_PROBLEM: How to maintain the legitimacy and effectiveness of an 18th-century written constitution across centuries of unanticipated social, technological, and moral change without constant formal amendment.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and constitutional theorists outside the judiciary dispute whether the framing generation intended or anticipated non-amendment-driven evolution; some corroborate adaptability as a design feature, others attest that formal amendment was the intended mechanism for major change.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__living_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because the living reading systematically transfers policy-making authority from electorally accountable legislatures to life-tenured judges, overriding majority preferences under the banner of evolved standards. Suppression (0.55) reflects the marginalization of originalist and textualist methods within dominant constitutional doctrine; originalism persists in dissents and academia but is largely suppressed in controlling precedent. Theater ratio (0.40 and rising) captures the growing performative gap between the judiciary's public framing ('we follow evolving social norms') and the reality of elite-driven rights innovation that often runs ahead of or diverges from broad social consensus. Accessibility collapse (0.45) is moderate because alternative interpretive frameworks (originalism, textualism) remain intellectually available even if institutionally disfavored. Resistance (0.50) reflects sustained political and scholarly backlash against judicial activism.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary and rights advocates experience this constraint as necessary coordination: without the living reading, the Constitution would ossify and fail to protect vulnerable minorities or adapt to modern conditions. Elected legislatures and popular majorities experience the same structure as extraction: their policy preferences and democratic choices are overridden by an unelected body claiming to speak for 'evolving' values they never endorsed. The engine computes this divergence from the structural asymmetry in power, exit options, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits near the beneficiary end (low d): it subsidizes its own power and institutional supremacy through the living reading. Rights advocates also sit near the beneficiary end: they receive protections and policy victories unavailable through electoral politics. Elected legislatures sit near the target end (high d): they bear the costs of judicial override and have constrained exit (no practical escape from federal judicial review). Originalist jurists are excluded rather than targeted: their suppression is discursive and professional, not a direct transfer of material resources, but their exclusion enables the constraint's stability.
 *
 * MANDATROPHY ANALYSIS:
 *   The living reading was built to solve amendment paralysis and constitutional obsolescence. If that founding problem were deadâif the amendment process were nimble and the text adequately detailedâthe continued judicial expansion of unenumerated rights would look like mandatrophy. However, the founding problem status is contested: beneficiaries argue the problem remains live because amendment is still practically impossible for controversial rights, while victims argue the problem is largely solved by normal politics and the arrangement now persists as judicial aggrandizement. Classifying as tangled_rope captures this ambiguity: the coordination function (adaptability) is genuine and not merely cover, but the asymmetric extraction (elite capture, democratic override) is equally real. If the coordination function were purely cover, the constraint would be a snare; if the coordination had fully atrophied and only theatrical maintenance remained, it would be a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_reading_kernel_contest,
    'Does the living reading''s expansion of constitutional constraints beyond text and amendment represent legitimate interpretive evolution or an unconstitutional transfer of amendment authority to the judiciary?',
    'Historical analysis of ratification-era interpretive expectations and cross-national constitutional practice; empirical tracking of judicial outcomes against legislative and popular preferences.',
    'If the transfer is illegitimate, the living reading''s extractiveness is higher (it extracts democratic authority) and its coordination function is cover; if legitimate, the extraction is lower and the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_reading_kernel_contest, conceptual, 'Structural ambiguity between this reading and originalist sibling over the locus of constitutional change authority.').

omega_variable(
    elite_capture_of_evolving_norms,
    'Are the ''evolving norms'' the judiciary invokes genuinely reflective of broad societal moral change, or are they captured by narrow professional and cultural elite consensus?',
    'Comparative polling of moral attitudes against judicial rights-recognition timelines; sociological mapping of elite versus mass opinion on contested rights.',
    'If captured by elites, the living reading operates as extraction from democratic majorities to judicial elites; if reflective of broad change, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_evolving_norms, empirical, 'Whether the living reading''s normative evolution is elite-captured or democratically representative.').

omega_variable(
    coordination_extraction_boundary_living,
    'Is the living reading''s adaptability separable from its judicial empowerment, or is the transfer of authority to courts structurally inseparable from the coordination function?',
    'Comparative constitutional analysis of jurisdictions with and without strong-form judicial review but similar constitutional texts; natural experiment from political-branch resistance or jurisdiction stripping.',
    'If separable, the living reading is a Tangled Rope with genuine coordination and separable extraction; if inseparable, the coordination may be inherently extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_living, conceptual, 'Whether the living reading''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__living_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_c_tr_t12, us_constitution_1787__living_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_1787__living_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(us_c_tr_t36, us_constitution_1787__living_reading, theater_ratio, 36, 0.32).
narrative_ontology:measurement(us_c_tr_t48, us_constitution_1787__living_reading, theater_ratio, 48, 0.36).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_1787__living_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(us_c_tr_t70, us_constitution_1787__living_reading, theater_ratio, 70, 0.42).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__living_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(us_c_be_t12, us_constitution_1787__living_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(us_c_be_t24, us_constitution_1787__living_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(us_c_be_t36, us_constitution_1787__living_reading, base_extractiveness, 36, 0.5).
narrative_ontology:measurement(us_c_be_t48, us_constitution_1787__living_reading, base_extractiveness, 48, 0.55).
narrative_ontology:measurement(us_c_be_t60, us_constitution_1787__living_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(us_c_be_t70, us_constitution_1787__living_reading, base_extractiveness, 70, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__living_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(us_c_su_t12, us_constitution_1787__living_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(us_c_su_t24, us_constitution_1787__living_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(us_c_su_t36, us_constitution_1787__living_reading, suppression_requirement, 36, 0.45).
narrative_ontology:measurement(us_c_su_t48, us_constitution_1787__living_reading, suppression_requirement, 48, 0.5).
narrative_ontology:measurement(us_c_su_t60, us_constitution_1787__living_reading, suppression_requirement, 60, 0.53).
narrative_ontology:measurement(us_c_su_t70, us_constitution_1787__living_reading, suppression_requirement, 70, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% The kernel 'us_constitution_1787' decomposes into at least three structurally distinct constraints: originalist_reading (fixed meaning), positivist_reading (text plus amendments), and living_reading (evolving aspirational framework). Each has distinct epsilon, beneficiary/victim structure, and classification. This story instantiates the living reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
