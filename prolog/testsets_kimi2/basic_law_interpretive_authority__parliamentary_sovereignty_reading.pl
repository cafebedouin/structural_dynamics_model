% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty as Final Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the parliamentary sovereignty reading
 *   of the basic law interpretive authority kernel. Under this reading, the
 *   elected legislature retains final say over constitutional meaning by
 *   virtue of its democratic mandate and representative accountability. The
 *   constraint coordinates democratic policy-making by preventing judicial
 *   gridlock, but simultaneously extracts interpretive autonomy from the
 *   judiciary and rights-security from constitutional minorities. It is
 *   actively enforced through legislative override powers, control of
 *   judicial appointments and budgets, and the doctrinal denial of any higher
 *   law binding Parliament. The claim is tangled_rope because the
 *   coordination function (democratic accountability) and the extraction
 *   function (majoritarian override of rights and courts) are structurally
 *   inseparable in the same institutional arrangement.
 *
 * KEY AGENTS:
 *   - elected_legislature: Primary agenda-setter (institutional/constrained) â claims and enforces final interpretive authority
 *   - political_majority: Primary beneficiary (powerful/mobile) â controls legislative output and benefits from unimpeded majoritarian policy
 *   - judicial_branch: Primary target (institutional/constrained) â bears subordination of its interpretive function to legislative will
 *   - constitutional_minorities: Secondary target (powerless/trapped) â bear vulnerability of rights to legislative override
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â tracks the democratic deficit versus majoritarian threat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.62).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty as Final Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '3afc45ee-4cd9-4d64-9305-f53d4242f053').
narrative_ontology:cs_kernel_codification('3afc45ee-4cd9-4d64-9305-f53d4242f053', formalized).
narrative_ontology:cs_authority_grounding('3afc45ee-4cd9-4d64-9305-f53d4242f053', lineage).
narrative_ontology:cs_interpretation_layer_present('3afc45ee-4cd9-4d64-9305-f53d4242f053').
narrative_ontology:cs_reading_relation('3afc45ee-4cd9-4d64-9305-f53d4242f053', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('3afc45ee-4cd9-4d64-9305-f53d4242f053', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('3afc45ee-4cd9-4d64-9305-f53d4242f053', foundational, parliamentary_finality_doctrine).
narrative_ontology:cs_axiom_status(parliamentary_finality_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('3afc45ee-4cd9-4d64-9305-f53d4242f053', parliamentary_finality_doctrine, conventional).
narrative_ontology:cs_axiom('3afc45ee-4cd9-4d64-9305-f53d4242f053', secondary, no_binding_entrenchment_against_majority).
narrative_ontology:cs_axiom_status(no_binding_entrenchment_against_majority, holdable).
narrative_ontology:cs_axiom_grounding('3afc45ee-4cd9-4d64-9305-f53d4242f053', no_binding_entrenchment_against_majority, conventional).
narrative_ontology:cs_reference_frame('3afc45ee-4cd9-4d64-9305-f53d4242f053', legislative_supremacy_framework).
narrative_ontology:cs_drift_state('3afc45ee-4cd9-4d64-9305-f53d4242f053', contemporary_rights_constitutionalism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3afc45ee-4cd9-4d64-9305-f53d4242f053', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, political_majority).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_branch).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and exercises final interpretive authority over constitutional meaning; can override judicial decisions through ordinary legislation, control judicial appointments and budgets, and denies any higher law binding Parliament. Structurally locked into the role of ultimate constitutional arbiter by the doctrine of parliamentary sovereignty.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Electoral majority that controls the legislative agenda. Benefits from unimpeded implementation of its policy program without judicial veto or entrenched rights constraints that would require supermajoritarian consensus.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, political_majority, beneficiary,
    powerful, biographical, mobile, national).

% Courts expected to defer to legislative judgment on constitutional questions; interpretive autonomy is constrained when the legislature can override, discipline, or strip jurisdiction. Bears the institutional cost of subordinated constitutional review and reduced capacity to protect rights against majoritarian override.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_branch, payer,
    institutional, generational, constrained, national).

% Rights-bearing minorities and marginalized groups whose constitutional claims are subject to legislative override. Lack an institutional veto point against majoritarian interpretation; bear the cost of contingent rights protection that depends on legislative forbearance.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_minorities, payer,
    powerless, generational, trapped, national).

% Analyze the tension between democratic legitimacy and rights protection. Observe whether legislative supremacy functions as democratic coordination or majoritarian extraction, and track the doctrinal and institutional drift of sovereignty claims over time.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Democratic coordination of constitutional meaning through elected representatives who are accountable to the electorate, providing a clear institutional mechanism for resolving constitutional disputes without empowering unelected judges to override popular will.
% TRANSFER_FUNCTION: Transfers final interpretive authority and agenda-setting power from courts and diffuse popular movements to the elected legislature and the political majority that commands it.
% ABSENT_VOICES: Popular constitutionalists who would locate authority in ongoing democratic contestation outside formal institutions; future generations bound by present majorities; international human rights bodies asserting supranational review.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty as final interpretive authority vanished, courts would assert or regain supremacy, rights-based judicial review would expand, legislative agendas would face new veto points, and the constitutional order would rearrange around judicial or popular authority.
% FOUNDING_PROBLEM: How to ensure constitutional interpretation remains democratically accountable and responsive rather than captured by unelected elites or frozen in past entrenchments.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists of democracy attest the problem is live â that judicial supremacy creates a democratic deficit. Rights advocates and constitutional courts attest the problem has shifted: legislative supremacy now threatens minority rights and the rule of law. No neutral consensus exists; corroboration is split by institutional seat.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the legislature can unilaterally override rights interpretations and judicial review outcomes, transferring authority from courts to the political majority. Suppression (0.58) is moderate-to-high: judicial alternatives are structurally suppressed by the doctrine that Parliament cannot bind its successors and by legislative control over court budgets and jurisdiction, but conventions of judicial independence and international human rights law provide residual friction. Theater_ratio (0.30) reflects a moderate performative component â assertions of absolute sovereignty persist even in jurisdictions where international or devolved constraints have eroded operational finality. Accessibility_collapse (0.45) is moderate: alternatives (judicial supremacy, popular constitutionalism) remain intellectually and institutionally visible but are formally foreclosed. Resistance (0.55) is significant, coming from courts, rights advocates, and supranational bodies. The metrics and claim are authored independently: the claim is tangled_rope because the structural logic of the reading inherently couples coordination and extraction, while the metrics describe the empirically observable intensity of that coupling.
 *
 * PERSPECTIVAL GAP:
 *   The legislature and the political majority experience this constraint as democratic coordination â a rope that channels popular will into law without judicial obstruction. The judiciary and constitutional minorities experience it as extraction â a snare that removes their veto points and renders rights contingent on majoritarian forbearance. The engine computes this divergence from the same structural data: low directionality for the majority seat, high directionality for the minority and judicial seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature and political majority are beneficiaries of interpretive finality; their directionality is toward the beneficiary end. The judicial branch and constitutional minorities are victims of override; their directionality is toward the target end. The spatial scope is national, amplifying effective extraction for the trapped minority seat. No override is needed because the structural derivation from beneficiary/victim declarations and exit options correctly maps these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy misclassification by declaring both the coordination function (democratic accountability) and the victim set (judiciary, minorities). A pure coordination reading would ignore the subordination of courts; a pure extraction reading would ignore the genuine democratic linkage between representatives and electorate. The tangled_rope classification captures that the same arrangement does both, and the R5 genealogy records that the founding problem â democratic accountability versus elite entrenchment â is contested rather than resolved, signaling that the arrangement may have outlived its justification for some seats while remaining live for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_coordination_or_extraction,
    'Does legislative finality function as democratic coordination accountable to the people, or as majoritarian extraction that subordinates minorities and the judiciary?',
    'Comparative analysis of jurisdictions with and without strong judicial review: measure minority rights protection and democratic responsiveness across constitutional models.',
    'If primarily extraction, the constraint leans toward snare; if primarily coordination with acceptable side-effects, it stabilizes as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_coordination_or_extraction, conceptual, 'Ambiguity between democratic coordination and majoritarian extraction').

omega_variable(
    erosion_of_sovereignty_doctrine,
    'Has parliamentary sovereignty as a practical constraint eroded to the point of becoming a piton â theatrical assertion without operational finality?',
    'Track the rate of legislative compliance with judicial declarations of incompatibility and the willingness of legislatures to exercise formal override powers.',
    'If enforcement is mostly theatrical while courts de facto control outcomes, the constraint may compute as piton despite claiming tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erosion_of_sovereignty_doctrine, empirical, 'Whether sovereignty doctrine is functionally atrophied').

omega_variable(
    suppression_mechanism_institutional,
    'Is the suppression of judicial alternatives achieved through formal institutional override or informal conventions of legislative supremacy?',
    'Catalog formal override powers versus actual patterns of legislative deference to judicial interpretation.',
    'If suppression is primarily conventional, the constraint''s effective suppression may be lower than formal metrics suggest, altering directionality for the judicial seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional, empirical, 'Formal versus informal suppression mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
