% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional law / political theory
 *
 * SUMMARY:
 *   This constraint instantiates the parliamentary supremacy reading of the
 *   constitutional interpretive authority kernel. Under this reading, the
 *   elected legislature possesses final interpretive authority and no court
 *   may void a parliamentary act. The doctrine is most closely associated
 *   with the Westminster tradition (Dicey) and coordinates constitutional
 *   finality by assigning unambiguous authority to a democratically
 *   accountable body. It also extracts interpretive discretion from the
 *   judiciary and rights-claimants, concentrating power in the legislative
 *   majority. The claim/metric independence is maintained: the constraint is
 *   claimed as tangled_rope (hybrid coordination and extraction) while
 *   metrics describe moderate-to-substantial extractiveness and active
 *   suppression of judicial review.
 *
 * KEY AGENTS:
 *   - Parliamentary majority (agenda_setter/beneficiary): institutional power, constrained exit â captures interpretive discretion and final law-making authority.
 *   - Judiciary (payer): institutional power, constrained exit â bears the cost of subordination and loss of nullification authority.
 *   - Rights claimants (payer): powerless, constrained exit â bear the risk of majoritarian override without judicial recourse.
 *   - Constitutional scholars (observer): analytical seat â map the doctrine's operation without institutional stake.
 *   - Civil rights organizations (excluded): organized, constrained exit â advocate for judicial review but are structurally absent from interpretive authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.7).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional law / political theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'b41e0d80-0c30-4a12-9ad2-1e924c193d4e').
narrative_ontology:cs_kernel_codification('b41e0d80-0c30-4a12-9ad2-1e924c193d4e', formalized).
narrative_ontology:cs_authority_grounding('b41e0d80-0c30-4a12-9ad2-1e924c193d4e', lineage).
narrative_ontology:cs_interpretation_layer_present('b41e0d80-0c30-4a12-9ad2-1e924c193d4e').
narrative_ontology:cs_reading_relation('b41e0d80-0c30-4a12-9ad2-1e924c193d4e', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('b41e0d80-0c30-4a12-9ad2-1e924c193d4e', constitutional_interpretive_authority__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('b41e0d80-0c30-4a12-9ad2-1e924c193d4e', foundational, elected_legislature_final_interpreter).
narrative_ontology:cs_axiom_status(elected_legislature_final_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('b41e0d80-0c30-4a12-9ad2-1e924c193d4e', elected_legislature_final_interpreter, conventional).
narrative_ontology:cs_axiom('b41e0d80-0c30-4a12-9ad2-1e924c193d4e', secondary, no_judicial_nullification).
narrative_ontology:cs_axiom_status(no_judicial_nullification, holdable).
narrative_ontology:cs_axiom_grounding('b41e0d80-0c30-4a12-9ad2-1e924c193d4e', no_judicial_nullification, conventional).
narrative_ontology:cs_reference_frame('b41e0d80-0c30-4a12-9ad2-1e924c193d4e', westminster_parliamentary_finality).
narrative_ontology:cs_drift_state('b41e0d80-0c30-4a12-9ad2-1e924c193d4e', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b41e0d80-0c30-4a12-9ad2-1e924c193d4e', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliamentary_majority).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the legislative agenda and exercises final interpretive discretion over constitutional meaning; can enact, amend, or repeal any law without risk of judicial nullification. Bears electoral accountability but no superior legal check.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliamentary_majority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliamentary_majority, beneficiary).

% Required to apply and interpret parliamentary acts even where they conflict with judicial understandings of rights or constitutional principle; lacks authority to void legislation. Bears the cost of truncated constitutional authority and institutional subordination.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judiciary, payer,
    institutional, generational, constrained, national).

% Individuals and minority groups seeking protection against legislative overreach lack access to judicial nullification of statutes; must rely on political majorities or international mechanisms. Bears the risk of majoritarian override.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, rights_claimants, payer,
    powerless, biographical, constrained, national).

% Analyze and debate the scope and legitimacy of parliamentary supremacy; inform political and legal discourse without direct institutional authority to alter the constraint.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% Advocate for entrenched rights and judicial review; structurally excluded from the legislative interpretive process and lack institutional veto points within a system of pure parliamentary supremacy.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, civil_rights_organizations, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliamentary_majority).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__parliamentary_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves inter-branch conflict over constitutional meaning by assigning final, unambiguous authority to the elected legislature, preventing deadlock between legislative and judicial branches and securing democratic accountability.
% TRANSFER_FUNCTION: Moves final interpretive discretion and law-making authority from the judiciary and diffuse rights-claimants to the parliamentary majority; transfers the risk of rights override from the state to individuals who lack judicial recourse against statutes.
% ABSENT_VOICES: Civil rights organizations and international human rights bodies are structurally excluded from the legislative interpretive process; their objections to majoritarian override carry no formal weight in the determination of constitutional meaning.
% DISAPPEARANCE_RATIONALE: If parliamentary supremacy vanished overnight, the constitutional order would reorganize around either judicial supremacy (courts voiding legislation) or coordinate construction (inter-branch dialogue); the distribution of final authority would shift fundamentally and institutional roles would rearrange.
% FOUNDING_PROBLEM: Constitutional deadlock between Crown and courts, and the democratic problem of unelected judges overriding the will of elected representatives; the need for a clear, accountable final arbiter in a representative democracy.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative scholars attest to the historical emergence of parliamentary supremacy from Crown-Parliament struggles; however, rights-focused legal scholars and international human rights bodies outside the legislature attest that the founding problem has evolved and now requires judicial oversight, corroborating a shifted-function reading.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderate-high because the legislature gains unchecked interpretive discretion at the expense of minorities and the judiciary; suppression (0.70) is high because the constraint's persistence depends on actively preventing courts from nullifying statutes and on suppressing rival authority claims. Theater ratio (0.35) is moderate: the democratic accountability rationale is genuine, but an increasing share of maintenance is performative assertion of supremacy in the face of human rights and devolution pressures. Accessibility collapse (0.60) reflects that judicial alternatives are substantially closed but not fully (international and political channels remain). Resistance (0.55) captures sustained academic, judicial, and civil-society pushback. The measurement series share one time grid to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The parliamentary majority seat experiences the constraint as necessary democratic coordination (finality, accountability, deadlock prevention). The judiciary and rights-claimant seats experience the same structure as enforced extraction of their authority and protections. The engine computes this divergence from beneficiary/victim declarations and exit modulation; the authored claim does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The parliamentary majority is the structural beneficiary (low directionality â the constraint subsidizes its authority). The judiciary and rights_claimants are structural targets (high directionality â the constraint extracts authority and protection from them). Constitutional scholars sit at the analytical pole with neutral directionality. Civil rights organizations are excluded from the arrangement entirely and would experience high directionality if admitted.
 *
 * MANDATROPHY ANALYSIS:
 *   The six-questions R5 interview prevents mislabeling: the founding problem (democratic finality vs. deadlock) is contested, and its corroboration comes partly from outside the beneficiary set. If the problem were dead and the arrangement persisted, the mismatch flag would fire toward piton. Here, the problem remains contested, supporting the tangled_rope classification against drift into pure snare or inertial piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supremacy_vs_rights_protection,
    'Does parliamentary supremacy structurally require the sacrifice of minority rights protection, or can electoral politics adequately safeguard rights without judicial nullification?',
    'Comparative empirical analysis of rights outcomes in jurisdictions maintaining parliamentary supremacy versus jurisdictions with strong judicial review.',
    'If rights outcomes are systematically worse under parliamentary supremacy, the extraction is asymmetric and the coordination function serves as cover for majoritarian override; if comparable, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supremacy_vs_rights_protection, empirical, 'Whether legislative finality trades off against minority rights protection.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the parliamentary supremacy reading foreclose judicial supremacy in all constitutional frameworks, or can weak forms of judicial review coexist with legislative finality?',
    'Analysis of jurisdictions with declarations of incompatibility (e.g., UK Human Rights Act 1998) to test whether legislative finality and judicial review are logically separable.',
    'If coexistence is structurally possible, the relation to judicial_supremacy_reading should be influences rather than forecloses, altering the kernel family network topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether parliamentary supremacy logically forecloses all judicial review or only strong forms.').

omega_variable(
    cs_framing_underdetermination,
    'Is the commitment system best framed as the institutional form of Parliament itself, or as the legitimacy claim of electoral mandate layered above it?',
    'Historical sociology of constitutional conventions versus normative political theory of democratic mandate.',
    'If the latter framing is adopted, the authority_grounding shifts from lineage/practice toward extraction, changing drift detection thresholds and the interpretation of theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framing of the kernel''s authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cons_tr_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(cons_tr_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 25, 0.35).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cons_be_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cons_be_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(cons_be_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cons_su_t5, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(cons_su_t15, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(cons_su_t25, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 25, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_interpretive_authority kernel, instantiating parliamentary supremacy. Sibling readings instantiate judicial supremacy and coordinate construction. Each reading carries a distinct epsilon and stakeholder structure due to the epsilon-invariance principle; they are linked as a constraint family via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
