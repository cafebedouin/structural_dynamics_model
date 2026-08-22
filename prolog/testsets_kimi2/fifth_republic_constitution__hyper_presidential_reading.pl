% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Constitution â Hyper-Presidential Reading
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   The French Fifth Republic constitution, read through the
 *   hyper-presidential lens, constructs the President as the direct emanation
 *   of national sovereignty, minimally checked by a legislature whose powers
 *   are structurally subordinated through instruments such as Article 49.3
 *   (engagement de responsabilitÃ©) and Article 16 (pouvoirs exceptionnels).
 *   Under this reading, the constitutional architecture is not a balanced
 *   separation of powers but a deliberate transfer of decisive authority to
 *   the executive, with the parliamentary chambers cast as paying seats that
 *   bear the cost of reduced legislative autonomy. The presidency as an
 *   institution and the incumbent president personally constitute the
 *   beneficiary set; the National Assembly and Senate enter the victim set
 *   whenever presidential constitutional weapons are deployed to bypass or
 *   override legislative will.
 *
 * KEY AGENTS:
 *   - incumbent_president: Agenda-setter and beneficiary (powerful/constrained) â invokes constitutional tools to bypass legislature and captures policy autonomy
 *   - presidency: Beneficiary (institutional/constrained) â accumulates constitutional powers and institutional prerogatives across electoral cycles
 *   - national_assembly: Payer (institutional/constrained) â bears the cost of lost budgetary and legislative initiative, with limited recourse
 *   - senate: Payer (institutional/constrained) â bears the cost of overridden amendments and reduced legislative role
 *   - constitutional_council: Observer (institutional/analytical) â reviews constitutionality but does not resolve the executive-legislative asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.85).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.78).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution â Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '93fba4be-79ed-4b58-886e-51067eb0781c').
narrative_ontology:cs_kernel_codification('93fba4be-79ed-4b58-886e-51067eb0781c', formalized).
narrative_ontology:cs_authority_grounding('93fba4be-79ed-4b58-886e-51067eb0781c', lineage).
narrative_ontology:cs_interpretation_layer_present('93fba4be-79ed-4b58-886e-51067eb0781c').
narrative_ontology:cs_reading_relation('93fba4be-79ed-4b58-886e-51067eb0781c', fifth_republic_constitution__parliamentary_constraint_reading, influences).
narrative_ontology:cs_reading_relation('93fba4be-79ed-4b58-886e-51067eb0781c', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('93fba4be-79ed-4b58-886e-51067eb0781c', foundational, president_embodies_national_will_directly).
narrative_ontology:cs_axiom_status(president_embodies_national_will_directly, holdable).
narrative_ontology:cs_axiom_grounding('93fba4be-79ed-4b58-886e-51067eb0781c', president_embodies_national_will_directly, conventional).
narrative_ontology:cs_axiom('93fba4be-79ed-4b58-886e-51067eb0781c', foundational, legislative_constraint_exception_not_rule).
narrative_ontology:cs_axiom_status(legislative_constraint_exception_not_rule, holdable).
narrative_ontology:cs_axiom_grounding('93fba4be-79ed-4b58-886e-51067eb0781c', legislative_constraint_exception_not_rule, conventional).
narrative_ontology:cs_reference_frame('93fba4be-79ed-4b58-886e-51067eb0781c', direct_presidential_mandate_framework).
narrative_ontology:cs_drift_state('93fba4be-79ed-4b58-886e-51067eb0781c', contemporary_fifth_republic, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('93fba4be-79ed-4b58-886e-51067eb0781c', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, senate).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, presidential_supremacy_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, executive_prerogative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional office of the President, which accumulates constitutional powers of initiative, referendum, emergency decree under Article 16, and dissolution. It benefits from provisions that concentrate legislative and budgetary initiative in the executive, persisting across successive incumbents regardless of political majority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency, beneficiary,
    institutional, generational, constrained, national).

% The sitting President who invokes Article 49.3 to bypass parliamentary votes on legislation or budgets, and may invoke Article 16 in emergencies. Directly sets the governmental agenda and captures policy autonomy that structurally displaces legislative deliberation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, beneficiary).

% The lower house of Parliament, whose legislative initiative and budgetary control are routinely bypassed by Article 49.3. It can attempt a motion of censure, but the political cost and low success rate mean it generally bears the loss of legislative autonomy without effective recourse.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    institutional, biographical, constrained, national).

% The upper house of Parliament, whose amendments can be overridden by the National Assembly under ordinary procedure and whose consent is not required for the most decisive budgetary and legislative measures when the executive invokes its constitutional weapons.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, senate, payer,
    institutional, generational, constrained, national).

% Reviews the constitutionality of laws and procedures but generally defers to the political branches on matters of constitutional structure and the scope of executive prerogatives; its review does not alter the asymmetric concentration of power encoded in the constitutional text.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates executive authority in a single directly elected figure to overcome parliamentary fragmentation and governmental instability, providing decisive national leadership particularly in moments of crisis or deadlock.
% TRANSFER_FUNCTION: Transfers legislative initiative, budgetary control, and emergency decision-making capacity from the parliamentary chambers to the presidential executive, particularly via Article 49.3 and Article 16.
% ABSENT_VOICES: Parliamentary minorities and republican traditionalists who regard legislative supremacy as the core of democratic government are structurally sidelined; the general electorate is invoked as the source of the president's legitimacy but lacks institutional voice between elections.
% DISAPPEARANCE_RATIONALE: If the hyper-presidential mechanisms vanished, the legislature would regain full budgetary and legislative initiative, the prime minister would become accountable to parliament rather than to the president, and executive policy would require legislative authorization â the French state would reorganize around parliamentary constraint.
% FOUNDING_PROBLEM: The chronic governmental instability and executive weakness of the French Third and Fourth Republics, where fragmented legislatures produced short-lived cabinets and policy paralysis.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship outside the presidentialist tradition attests that the Fourth Republic's instability was real but that the Fifth Republic's concentration of power was a chosen remedy rather than an inevitable one; contemporary parliamentary-constraint readings argue the founding problem is solved and the arrangement persists as executive extraction.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85 at interval end) because the president can impose major policy and budgets without legislative consent via Article 49.3, and emergency powers under Article 16 suspend normal legislative control. Suppression is high (0.78) because the constraint persists through active constitutional enforcement mechanisms including dissolution threats and limited censure success. Theater_ratio is substantial (0.52) because the 'national will' rhetoric and ceremonial functions of parliament increasingly mask the actual locus of decision-making. Accessibility_collapse is high (0.70) because alternatives such as full parliamentary sovereignty are intellectually available but constitutionally blocked. Resistance is moderate (0.55) because parliament and public opposition do resist, yet rarely prevail against determined presidential majorities.
 *
 * PERSPECTIVAL GAP:
 *   From the presidential seat, the arrangement appears as necessary executive coordination to avoid parliamentary cacophony and deliver decisive governance. From the legislative seats, the same structure reads as the systematic extraction of legislative capacity, where the executive uses constitutional weapons not as exceptions but as ordinary instruments of governance. The Constitutional Council observes the text but does not resolve this asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent_president and presidency are declared beneficiaries, structurally positioned at the low-d (beneficiary) end: the constraint subsidizes their authority by transferring legislative initiative and budgetary control to the executive. The national_assembly and senate are declared victims, positioned at the high-d (target) end: they bear the extraction of their constitutional powers. No override is required because the structural declarations accurately capture the relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problemâparliamentary instability under the Fourth Republicâwas resolved by the mid-1960s. The hyper-presidential reading nevertheless persists, justifying expanded executive power as the natural order of the Republic rather than a transitional scaffold. The mandatrophy is unresolved: the constraint's founding problem is dead but the arrangement continues and has intensified, drifting toward pure extraction as measured by rising base_extractiveness and suppression_requirement over the interval. The classification as tangled_rope preserves the historical coordination truth while registering the asymmetric extraction that has accumulated around the solved core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contestation_ambiguity,
    'Does the constitutional text of 1958 structurally mandate hyper-presidentialism, or does it permit the parliamentary and cohabitation readings without textual contradiction?',
    'Comparative constitutional analysis of the 1958 text and travaux prÃ©paratoires, examining whether the instruments of presidential dominance are exclusive interpretations or permissible constructions among several.',
    'If the text is genuinely ambiguous, the hyper-presidential reading is one permissible construction rather than the necessary structure â this would reclassify the constraint toward a more contested coordination type rather than asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contestation_ambiguity, conceptual, 'Whether the constitutional text structurally mandates this reading or permits siblings').

omega_variable(
    democratic_legitimacy_source,
    'Is the president''s claim to embody national will structurally derived from direct election, or from the constitutional framework''s interpretive tradition?',
    'Electoral sociology and constitutional history examining whether direct presidential elections produced the democratic mandate claimed, or whether the mandate is retroactively constructed by institutional practice.',
    'If the mandate is weak, the extraction from legislature rests on constitutional machinery rather than democratic will, raising the effective suppression metric and pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_source, empirical, 'Whether presidential authority rests on electoral mandate or institutional construction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fifth_rep_hyp_tr_t0, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fifth_rep_hyp_tr_t8, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(fifth_rep_hyp_tr_t16, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(fifth_rep_hyp_tr_t28, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 28, 0.4).
narrative_ontology:measurement(fifth_rep_hyp_tr_t38, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 38, 0.3).
narrative_ontology:measurement(fifth_rep_hyp_tr_t48, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 48, 0.38).
narrative_ontology:measurement(fifth_rep_hyp_tr_t58, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 58, 0.42).
narrative_ontology:measurement(fifth_rep_hyp_tr_t66, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 66, 0.52).

% Extraction over time
narrative_ontology:measurement(fifth_rep_hyp_be_t0, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fifth_rep_hyp_be_t8, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(fifth_rep_hyp_be_t16, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 16, 0.72).
narrative_ontology:measurement(fifth_rep_hyp_be_t28, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 28, 0.48).
narrative_ontology:measurement(fifth_rep_hyp_be_t38, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 38, 0.65).
narrative_ontology:measurement(fifth_rep_hyp_be_t48, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 48, 0.75).
narrative_ontology:measurement(fifth_rep_hyp_be_t58, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 58, 0.7).
narrative_ontology:measurement(fifth_rep_hyp_be_t66, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 66, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(fifth_rep_hyp_su_t0, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fifth_rep_hyp_su_t8, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(fifth_rep_hyp_su_t16, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(fifth_rep_hyp_su_t28, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 28, 0.45).
narrative_ontology:measurement(fifth_rep_hyp_su_t38, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 38, 0.62).
narrative_ontology:measurement(fifth_rep_hyp_su_t48, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 48, 0.7).
narrative_ontology:measurement(fifth_rep_hyp_su_t58, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 58, 0.65).
narrative_ontology:measurement(fifth_rep_hyp_su_t66, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 66, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fifth_republic_constitution kernel. It shares the same constitutional text with its siblings but assigns different directionalities to the legislature and executive. The epsilon values diverge because the structural relationships differ: here the legislature is victim; in the parliamentary reading it is beneficiary; in the cohabitation reading directionality is split.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
