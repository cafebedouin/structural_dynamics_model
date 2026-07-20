% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Sovereignty with Notwithstanding Override
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint story instantiates the legislative_sovereignty_reading of
 *   the constitutional_text kernel. Under this reading, the constitutional
 *   text establishes the legislature (Parliament) as the supreme
 *   institutional authority with final say over constitutional meaning,
 *   typically exercised through notwithstanding clauses or ordinary
 *   legislative override. Judicial review is advisory rather than binding.
 *   The constraint coordinates constitutional interpretation by locating
 *   final authority in a single, electorally accountable institution, but
 *   simultaneously extracts from constitutional minorities by permitting
 *   legislative override of rights protections. This is one of three sibling
 *   readings of the constitutional_text kernel; the
 *   judicial_supremacy_reading and popular_sovereignty_reading are authored
 *   as separate constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - parliamentary_majority: Agenda-setter and primary beneficiary (institutional power, national scope, mobile exit through elections) â controls constitutional override capacity.
 *   - constitutional_minorities: Primary payer (moderate power, national scope, constrained exit) â bear the risk of rights override by majority.
 *   - high_courts: Advisory institutional observer (institutional power, national scope, analytical exit) â interpret but cannot finally block legislative will.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.62).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.55).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Sovereignty with Notwithstanding Override").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional/political").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, 'fd45cf4a-98cf-474f-b3ee-1d41ff3dd776').
narrative_ontology:cs_kernel_codification('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776', fixed_text).
narrative_ontology:cs_authority_grounding('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776', lineage).
narrative_ontology:cs_interpretation_layer_present('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776').
narrative_ontology:cs_reading_relation('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776', foundational, parliamentary_supremacy_doctrine).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776', parliamentary_supremacy_doctrine, conventional).
narrative_ontology:cs_axiom('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776', foundational, legislative_override_validity).
narrative_ontology:cs_axiom_status(legislative_override_validity, holdable).
narrative_ontology:cs_axiom_grounding('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776', legislative_override_validity, conventional).
narrative_ontology:cs_reference_frame('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776', parliamentary_sovereignty_framework).
narrative_ontology:cs_drift_state('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776', contemporary_rights_charter_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd45cf4a-98cf-474f-b3ee-1d41ff3dd776', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, parliamentary_majority).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, constitutional_minorities).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, majoritarian_democratic_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds legislative power and can pass ordinary or constitutional legislation asserting final meaning over contested rights. Invokes notwithstanding clauses or simple override when judicial advice conflicts with majority preferences. Retains office through elections and can alter the constraint's operation by majority vote.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, parliamentary_majority, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, parliamentary_majority, beneficiary).

% Groups whose rights protections depend on judicial enforcement or constitutional entrenchment against majority preferences. Face the risk that parliamentary majority can override specific rights protections through legislative procedures. Political exit is constrained by national boundaries; domestic voice is diluted by majority electoral dominance.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_minorities, payer,
    moderate, generational, constrained, national).

% Hear constitutional challenges and issue advisory or non-binding interpretations of legislation relative to rights charters. Lack final authority to invalidate legislation; decisions can be set aside by legislative majority. Retain institutional role in clarifying legal norms but cannot block parliamentary will.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, high_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__legislative_sovereignty_reading, parliamentary_majority).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, electorally accountable locus of constitutional authority, avoiding inter-institutional deadlock over constitutional meaning and locating final interpretive power in a representative legislature rather than unelected courts.
% TRANSFER_FUNCTION: Transfers final constitutional interpretive authority from the judiciary (and diffuse popular mechanisms) to the legislative majority, and transfers the cost of rights override from the majority to constitutional minorities whose judicial protections become non-binding.
% ABSENT_VOICES: Judicial supremacy advocates argue that rights should be insulated from majority override, but their position is non-binding under this reading. Constitutional minorities are formally present in legislative debate but structurally outvoted. International human rights bodies offer external norms that lack domestic enforceability.
% DISAPPEARANCE_RATIONALE: If legislative supremacy and override capacity vanished overnight, constitutional interpretation would shift toward judicial supremacy or popular mechanisms, minority rights protections would gain final judicial enforceability, and the balance between democratic accountability and rights insulation would invert.
% FOUNDING_PROBLEM: How to reconcile unelected judicial review with democratic self-government and prevent constitutional deadlock between branches of government.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary sovereignty advocates and democratic theorists attest the problem of judicial tyranny remains live. Constitutional minority advocates and comparative rights scholars attest the problem is overstated and the arrangement persists as majoritarian domination; external comparative constitutional analysis corroborates both positions.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high: the constraint structurally permits the majority to override minority rights protections, transferring the cost of majoritarian preferences to vulnerable groups. Suppression (0.55) is moderate: judicial alternatives are not abolished but are rendered formally non-binding, collapsing the accessibility of final judicial remedy. Accessibility collapse (0.68) reflects that once legislative supremacy is established, domestic alternatives to majority will are severely limited. Resistance (0.52) captures ongoing contestation by minorities, rights advocates, and judicial supremacy proponents. Theater ratio (0.28) is low-moderate: parliamentary processes are largely functional, though some constitutional rhetoric asserting supremacy is performative. The measurement series tracks gradual extraction accumulation as rights-charter regimes mature alongside override mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The parliamentary majority experiences this constraint as the legitimate expression of democratic self-government and coordination (a rope-like function locating constitutional authority in elected representatives). Constitutional minorities experience it as exposure to unaccountable majority override (a snare-like function extracting their rights security for majority convenience). High courts occupy an intermediate seat: they retain institutional dignity and procedural role but experience subordination of their interpretive function. The engine computes these divergent seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The parliamentary majority is the structural beneficiary and agenda-setter (low d): it collects final constitutional authority and the capacity to legislate without judicial veto. Constitutional minorities are the structural targets (high d): they bear the costs of reduced rights security and limited institutional recourse. High courts are neither beneficiaries nor victims in the extraction chain; their directionality defaults toward symmetric (d ~0.5) due to analytical exit options, though their institutional role is constrained by the override mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â reconciling judicial review with democratic self-government and avoiding inter-branch constitutional deadlock â remains contested and live in many jurisdictions. The constraint retains a genuine coordination function: it provides a clear, electorally accountable locus for resolving constitutional ambiguity. Because the coordination function is still operational and invoked (not merely theatrical), the constraint has not atrophied into a piton. However, if legislative override were exercised routinely rather than exceptionally, the coordination story would degrade into cover for extraction, and the constraint would drift toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_coordination_vs_extraction,
    'Does legislative supremacy solve a genuine democratic coordination problem (avoiding judicial tyranny and deadlock), or does it primarily serve as a structural mechanism for majoritarian extraction from minorities?',
    'Comparative analysis of jurisdictions with and without notwithstanding clauses: if rights outcomes are systematically worse for minorities under legislative supremacy while democratic legitimacy is not measurably enhanced, extraction dominates; if judicial supremacy jurisdictions show comparable or worse democratic deficits, coordination is genuine.',
    'If extraction dominates, classification shifts toward snare; if coordination is genuine and asymmetric costs are incidental, classification remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_coordination_vs_extraction, empirical, 'Whether legislative supremacy is primarily coordination or extraction.').

omega_variable(
    kernel_reading_contest_legislative,
    'This constraint instantiates the legislative_sovereignty_reading of the constitutional_text kernel; how would classification change under judicial_supremacy_reading or popular_sovereignty_reading?',
    'Compare the sibling constraint stories: judicial supremacy likely shifts beneficiary/victim structure (courts as beneficiaries, legislative majorities as victims of constraint) and changes the directionality profile; popular sovereignty may diffuse directionality across the demos.',
    'The current reading''s extraction profile depends on the specific locus of final authority; alternative readings rewire who is coordinated and who pays.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_legislative, conceptual, 'How sibling readings of the same kernel restructure the constraint.').

omega_variable(
    override_usage_pattern,
    'Is the notwithstanding clause or legislative override used sparingly as a democratic safety valve, or routinely as an ordinary legislative tool?',
    'Empirical counting of notwithstanding invocations across jurisdictions; qualitative analysis of whether usage clusters around rights-limiting legislation or coincidental statutory drafting.',
    'Routine usage raises extractiveness and suppression over time, pushing the constraint toward snare; rare, high-salience usage maintains the tangled rope profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_usage_pattern, empirical, 'Whether override mechanisms are exceptional or normalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cslsr_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cslsr_tr_t10, constitutional_text__legislative_sovereignty_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(cslsr_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cslsr_tr_t30, constitutional_text__legislative_sovereignty_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(cslsr_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(cslsr_tr_t50, constitutional_text__legislative_sovereignty_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(cslsr_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cslsr_be_t10, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(cslsr_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(cslsr_be_t30, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(cslsr_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(cslsr_be_t50, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 50, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(constitutional_text__legislative_sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_text kernel, decomposed per the Îµ-invariance principle. The legislative_sovereignty_reading, judicial_supremacy_reading, and popular_sovereignty_reading share the same constitutional domain but instantiate structurally distinct constraints with different Îµ values, beneficiary/victim structures, and loci of final authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
