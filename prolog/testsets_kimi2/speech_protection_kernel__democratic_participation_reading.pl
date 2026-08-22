% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__democratic_participation_reading, []).

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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Democratic Participation Reading of Speech Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story captures the democratic participation reading of
 *   the First Amendment speech protection kernel: political expression
 *   necessary for democratic self-governance receives the highest
 *   constitutional protection, while non-political speech (commercial,
 *   private, entertainment) is more readily restricted. It is one reading
 *   among five sibling readings of the same kernel. The constraint operates
 *   as a legal-institutional hierarchy enforced by federal judicial review.
 *
 * KEY AGENTS:
 *   - political_speakers: Primary beneficiary (organized/mobile) â receive heightened protection
 *   - civic_advocacy_groups: Secondary beneficiary (organized/constrained) â mobilization shield
 *   - election_regulators: Primary payer (institutional/constrained) â lose regulatory capacity
 *   - defamation_plaintiffs: Secondary payer (moderate/constrained) â face elevated barriers
 *   - commercial_advertisers: Tertiary payer (powerful/constrained) â intermediate protection
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â enforces the hierarchy
 *   - critical_legal_scholars: Analytical observer â tracks distributional asymmetry
 *   - dignity_framework_advocates: Excluded voice â present globally, excluded domestically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.52).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.55).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Democratic Participation Reading of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '777fb53d-08c2-4ab0-aff3-a1745aa8457d').
narrative_ontology:cs_kernel_codification('777fb53d-08c2-4ab0-aff3-a1745aa8457d', fixed_text).
narrative_ontology:cs_authority_grounding('777fb53d-08c2-4ab0-aff3-a1745aa8457d', lineage).
narrative_ontology:cs_interpretation_layer_present('777fb53d-08c2-4ab0-aff3-a1745aa8457d').
narrative_ontology:cs_reading_relation('777fb53d-08c2-4ab0-aff3-a1745aa8457d', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('777fb53d-08c2-4ab0-aff3-a1745aa8457d', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('777fb53d-08c2-4ab0-aff3-a1745aa8457d', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('777fb53d-08c2-4ab0-aff3-a1745aa8457d', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('777fb53d-08c2-4ab0-aff3-a1745aa8457d', foundational, democratic_self_governance_core).
narrative_ontology:cs_axiom_status(democratic_self_governance_core, holdable).
narrative_ontology:cs_axiom_grounding('777fb53d-08c2-4ab0-aff3-a1745aa8457d', democratic_self_governance_core, deontological).
narrative_ontology:cs_axiom('777fb53d-08c2-4ab0-aff3-a1745aa8457d', foundational, political_speech_hierarchy).
narrative_ontology:cs_axiom_status(political_speech_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('777fb53d-08c2-4ab0-aff3-a1745aa8457d', political_speech_hierarchy, deontological).
narrative_ontology:cs_reference_frame('777fb53d-08c2-4ab0-aff3-a1745aa8457d', democratic_self_governance_framework).
narrative_ontology:cs_drift_state('777fb53d-08c2-4ab0-aff3-a1745aa8457d', contemporary_doctrine, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('777fb53d-08c2-4ab0-aff3-a1745aa8457d', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, civic_advocacy_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, election_regulators).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, defamation_plaintiffs).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, commercial_advertisers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the highest level of judicial protection against state restriction when speaking on matters of public concern, elections, and governmental policy. Their speech is presumptively immune from content-based regulation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_speakers, beneficiary,
    organized, biographical, mobile, national).

% Engage in issue advocacy and electoral mobilization under the shield of heightened constitutional protection, though organizational survival depends on maintaining nonprofit or advocacy status within regulatory frameworks.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, civic_advocacy_groups, beneficiary,
    organized, generational, constrained, national).

% Administer campaign finance and election integrity laws that are routinely invalidated or narrowed when they touch political speech, constraining their statutory mandate and regulatory capacity.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, election_regulators, payer,
    institutional, biographical, constrained, national).

% Seek redress for reputational harm caused by political speech but face elevated constitutional barriers including actual malice requirements and broad protections for criticism of public officials and figures.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, defamation_plaintiffs, payer,
    moderate, biographical, constrained, national).

% Promote products and services through speech that receives intermediate or lesser protection compared to core political speech, making commercial advertising more vulnerable to disclosure requirements and content regulation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, commercial_advertisers, payer,
    powerful, biographical, constrained, national).

% Establishes and enforces the tiered framework of speech protection through constitutional interpretation and judicial review, striking down laws that unduly restrict political expression while permitting greater regulation of non-political speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Analyze the distributional consequences of the speech hierarchy, noting that the framework's formal neutrality masks asymmetric protection of powerful political speakers and systemic exclusion of subordinated voices.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, critical_legal_scholars, observer,
    analytical, generational, analytical, national).

% Advance alternative constitutional frameworks that would subordinate speech protection to dignity and anti-subordination concerns, but are structurally excluded from prevailing US constitutional doctrine which treats such frameworks as outside the acceptable interpretive range.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, dignity_framework_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables informed democratic self-governance by ensuring that citizens can access the political expression necessary to evaluate candidates, policies, and official conduct without state censorship.
% TRANSFER_FUNCTION: Moves constitutional protection against state restriction disproportionately to political speakers and away from regulatory actors and private plaintiffs, while leaving commercial and non-political speech more exposed to permissible regulation.
% ABSENT_VOICES: Dignity-framework advocates and harm-threshold proponents who would restrict political speech functioning as structural subordination or causing demonstrable harm; they are present in comparative and international law discourse but structurally excluded from US constitutional adjudication.
% DISAPPEARANCE_RATIONALE: If the tiered protection vanished overnight, campaign finance law, defamation doctrine, election regulation, and commercial speech frameworks would reorganize around a flat or differently-structured protection scheme; the boundary between permissible and impermissible speech restriction would dissolve.
% FOUNDING_PROBLEM: State censorship of political criticism and government-critical speech, particularly around elections and policy debates, threatening the capacity of citizens to exercise popular sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians attest that state suppression of political dissent was a founding-era problem. However, contemporary critical legal scholars and comparative constitutionalists contest whether the current hierarchical doctrine remains calibrated to that problem or has become an autonomous, self-reinforcing judicial structure that protects power rather than participation.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects the systematic advantage political speakers gain at the expense of regulatory and private plaintiffs who bear the cost of the protection hierarchy. Suppression (0.55) captures the active judicial suppression of alternative frameworks (dignity, harm-threshold) in constitutional doctrine. Theater ratio (0.28) is moderate: much of the doctrine is functional democratic coordination, but a growing share consists in ritual invocation of self-governance rhetoric to justify outcomes that no longer track democratic participation values. Accessibility collapse (0.65) is high because alternative frameworks are largely foreclosed in US courts once the democratic participation frame is accepted. Resistance (0.50) reflects ongoing academic and international criticism.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary experiences this constraint as a legitimate coordination mechanism preserving democratic governance. Political speakers experience it as a protective shield. Election regulators and defamation plaintiffs experience it as an extractive barrier that strips their statutory authority or legal remedies. Commercial advertisers occupy an intermediate position: they benefit from some protection but are disadvantaged relative to political speakers. The engine will compute these seats differently based on the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Political speakers and civic advocacy groups are beneficiaries (low d): the constraint subsidizes their expressive capacity against state interference. Election regulators, defamation plaintiffs, and commercial advertisers are payers (high d): the constraint extracts regulatory authority and legal redress from them. The federal judiciary sits near symmetric as agenda-setter: it gains institutional authority from enforcing the hierarchy but does not capture the extraction as a rent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling in both directions. Against pure rope: the hierarchy creates identifiable victims (regulators, plaintiffs) who bear asymmetric costs, so pure coordination is ruled out. Against pure snare: the constraint solves a genuine coordination problem (preventing state censorship of governance-critical speech), so pure extraction is ruled out. The temporal measurements show theater_ratio rising but not dominating, consistent with a coordination mechanism accumulating extractive drift rather than being born as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint instantiates the democratic_participation_reading of speech_protection_kernel. How would its classification change if the absolutist_reading (near-categorical protection) or dignity_reading (conditional on non-subordination) were the operative framework instead?',
    'Cross-reading corpus comparison: evaluate the same constitutional arrangement under each sibling reading''s axioms to identify how beneficiary/victim distributions and extraction profiles shift.',
    'Under the absolutist reading, victims would diminish and the constraint would likely compute as rope or mountain. Under the dignity reading, protection would be conditional and subordinated groups would appear as victims of currently protected speech, potentially shifting classification toward snare with a different victim distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural sensitivity of classification to kernel reading choice').

omega_variable(
    political_speech_boundary_determinacy,
    'Is the boundary between ''political speech necessary for self-governance'' and other speech categories principled and determinate, or does judicial discretion create arbitrary costs for speakers near the boundary?',
    'Doctrinal history and empirical analysis of judicial outcomes at the political/commercial and political/private speech boundaries.',
    'If the boundary is largely discretionary, effective extraction is higher than the structural measure suggests because speakers near the boundary face unpredictable regulatory costs. If principled, the extraction reflects genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_speech_boundary_determinacy, empirical, 'Uncertainty about whether the speech hierarchy boundary is principled or arbitrary').

omega_variable(
    suppression_of_alternatives,
    'Are dignity-based and harm-threshold readings structurally suppressed in US constitutional doctrine, or merely unselected among coexisting options?',
    'Comparative constitutional analysis tracking the reception of dignity and harm-based arguments in federal courts versus other liberal democracies.',
    'If structurally suppressed, the constraint''s suppression metric understates the true closure of alternatives. If merely unselected, the suppression metric overstates coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_alternatives, conceptual, 'Whether alternative constitutional frameworks are suppressed or merely unselected').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_dp_tr_t0, speech_protection_kernel__democratic_participation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spk_dp_tr_t10, speech_protection_kernel__democratic_participation_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(spk_dp_tr_t20, speech_protection_kernel__democratic_participation_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(spk_dp_tr_t30, speech_protection_kernel__democratic_participation_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(spk_dp_tr_t40, speech_protection_kernel__democratic_participation_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(spk_dp_tr_t50, speech_protection_kernel__democratic_participation_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(spk_dp_be_t0, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(spk_dp_be_t10, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(spk_dp_be_t20, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(spk_dp_be_t30, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(spk_dp_be_t40, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(spk_dp_be_t50, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(spk_dp_su_t0, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(spk_dp_su_t10, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(spk_dp_su_t20, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(spk_dp_su_t30, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(spk_dp_su_t40, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(spk_dp_su_t50, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel decomposes into five constraint stories (absolutist, democratic_participation, harm_threshold, marketplace, dignity) because the natural-language label 'speech protection' conflates structurally distinct claims about the purpose, scope, and hierarchy of protected expression. Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
