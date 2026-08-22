% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Clause 39 as Universal Due Process Guarantee Against Arbitrary State Power
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the liberal due process reading of the Magna
 *   Carta Clause 39 kernel: the claim that the clause establishes a
 *   universal, individual right against arbitrary state deprivation of
 *   liberty, property, or standing, binding on executive power generically
 *   rather than only in the 1215 baronial context. Under this reading the
 *   constraint is expansive — its victim set is broad (anyone subject to
 *   state power whose process might be contested) and its extractiveness
 *   against unchecked executive discretion is high, because the doctrine's
 *   entire point is to make arbitrary executive action costly and reviewable.
 *   This is NOT the feudal_prerogative_reading (which confines the guarantee
 *   to a narrow procedural entitlement within an intact hierarchical order)
 *   nor the originalist_limitation_reading (which confines the guarantee to
 *   specific documented 1215 abuses). Those are separate constraints with
 *   their own ε values; this file's ε describes only the liberal reading's
 *   own account of the standing arrangement.
 *
 * KEY AGENTS:
 *   - rights_bearing_citizens: diffuse population invoking the guarantee
 *   - due_process_litigants: individuals actively contesting state action
 *   - judicial_review_institutions: courts administering and expanding the doctrine
 *   - executive_authority_targets_of_constraint: the state power being constrained
 *   - detained_persons_under_contested_process: those for whom enforcement is tested and often fails
 *   - marginalized_groups_denied_equal_enforcement: those historically excluded from the guarantee's practical reach
 *   - constitutional_historians: analytical observers of the doctrine's genealogy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.71).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.62).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Clause 39 as Universal Due Process Guarantee Against Arbitrary State Power").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, '3ea635bb-787d-45ae-afc4-b64091090d2f').
narrative_ontology:cs_kernel_codification('3ea635bb-787d-45ae-afc4-b64091090d2f', fixed_text).
narrative_ontology:cs_authority_grounding('3ea635bb-787d-45ae-afc4-b64091090d2f', lineage).
narrative_ontology:cs_interpretation_layer_present('3ea635bb-787d-45ae-afc4-b64091090d2f').
narrative_ontology:cs_reading_relation('3ea635bb-787d-45ae-afc4-b64091090d2f', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('3ea635bb-787d-45ae-afc4-b64091090d2f', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('3ea635bb-787d-45ae-afc4-b64091090d2f', foundational, state_power_universally_bound_by_lawful_judgment).
narrative_ontology:cs_axiom_status(state_power_universally_bound_by_lawful_judgment, holdable).
narrative_ontology:cs_axiom_grounding('3ea635bb-787d-45ae-afc4-b64091090d2f', state_power_universally_bound_by_lawful_judgment, deontological).
narrative_ontology:cs_axiom('3ea635bb-787d-45ae-afc4-b64091090d2f', foundational, individual_personhood_supersedes_estate_status).
narrative_ontology:cs_axiom_status(individual_personhood_supersedes_estate_status, holdable).
narrative_ontology:cs_axiom_grounding('3ea635bb-787d-45ae-afc4-b64091090d2f', individual_personhood_supersedes_estate_status, deontological).
narrative_ontology:cs_reference_frame('3ea635bb-787d-45ae-afc4-b64091090d2f', universal_natural_rights_baseline).
narrative_ontology:cs_drift_state('3ea635bb-787d-45ae-afc4-b64091090d2f', contemporary_constitutional_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3ea635bb-787d-45ae-afc4-b64091090d2f', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, rights_bearing_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, due_process_litigants).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, judicial_review_institutions).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_authority_targets_of_constraint).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, detained_persons_under_contested_process).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, marginalized_groups_denied_equal_enforcement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, due_process_litigants).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, rule_of_law_supremacy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, universal_legal_personhood_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke Clause 39's lineage as the ancestral guarantee that the state cannot imprison, dispossess, or outlaw them without lawful judgment. They cannot exit the jurisdiction's authority but rely on courts to enforce the guarantee against executive overreach; the promise is real but its delivery depends entirely on institutions choosing to honor it.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, rights_bearing_citizens, beneficiary,
    moderate, civilizational, constrained, national).

% Individuals actually contesting detention, seizure, or exile at the moment of state action. They bear the cost of litigation and delay while the guarantee is adjudicated, and they are the ones for whom the abstraction becomes concrete — either vindicated by lawful judgment or failed by a court that defers to executive claims of necessity.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, due_process_litigants, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, due_process_litigants, payer).

% Courts and constitutional bodies that read Clause 39 as a living universal guarantee and use it to constrain executive detention, seizure, and punishment powers. They administer the doctrine, decide its scope in each generation, and derive institutional legitimacy from being the guardians of a right traced to 1215.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, judicial_review_institutions, agenda_setter,
    institutional, generational, analytical, national).

% The executive branch, whose discretion to detain, requisition, or act against individuals is checked by the requirement of lawful judgment or law of the land. Under this reading, virtually every unilateral executive action touching liberty or property must clear a due-process threshold, which the executive experiences as an expansive, ever-tightening constraint on emergency and administrative action.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_authority_targets_of_constraint, payer,
    institutional, immediate, constrained, national).

% People held under process the state claims is lawful but the guarantee's promise says must be tested. Where courts decline jurisdiction, defer to security claims, or process is delayed for years, the universal guarantee fails to reach them in practice even though the doctrine is invoked in their name.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, detained_persons_under_contested_process, payer,
    powerless, immediate, trapped, national).

% Groups historically and presently excluded from the guarantee's practical reach — colonized subjects, racialized populations, noncitizens — for whom 'universal' rights were declared in doctrine while enforcement was withheld for generations. They bear the gap between the reading's expansive promise and its selective delivery.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, marginalized_groups_denied_equal_enforcement, payer,
    powerless, generational, trapped, national).

% Study how a 1215 baronial settlement was reinterpreted across centuries into a universal individual-rights doctrine, tracing the doctrinal expansions (Coke, the U.S. and Commonwealth due process traditions) that produced this reading and assessing how much of the universal claim is retrofit.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__liberal_due_process_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__liberal_due_process_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared expectation across citizens, courts, and executive actors that state power to deprive anyone of liberty, property, or standing will be channeled through lawful judgment rather than arbitrary fiat — enabling predictable, contestable state action instead of unreviewable discretion.
% TRANSFER_FUNCTION: Moves the burden of justification from the individual (who must simply submit to state power) to the state (which must demonstrate lawful process), and correspondingly moves practical protection toward whichever individuals can access courts and away from those who cannot.
% ABSENT_VOICES: Those actually detained without effective access to review — historically enslaved persons, colonial subjects, wartime internees, and present-day noncitizens in expedited proceedings — are invoked as the beneficiaries of 'universal' rights but are frequently not the ones courts actually hear from when the doctrine's boundaries are drawn.
% DISAPPEARANCE_RATIONALE: If courts stopped reading Clause 39's lineage as underwriting universal due process, executive detention and seizure powers would lose their principal textual-historical anchor in the Anglo-American constitutional tradition; litigation strategies, judicial review doctrine, and the legitimating narrative of constitutional government would need to be substantially rebuilt on other grounds.
% FOUNDING_PROBLEM: The doctrine as read here answers a problem well beyond 1215: the general problem of unconstrained executive power over any individual's liberty or property, framed as a permanent structural risk in any state, not a specific baronial grievance.
% FOUNDING_PROBLEM_CORROBORATION: Judicial opinions and constitutional scholars within the liberal tradition attest the problem (arbitrary executive power) is perpetually live and the doctrine's universalism is doing real protective work. Legal historians and originalist scholars outside this reading's tradition attest that the 1215 text addressed specific baronial and free-tenant grievances and that the 'universal individual rights' framing is a later doctrinal accretion, not evidence of the founding problem this reading claims.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) because, on this reading's own terms, the doctrine imposes a real and rising cost on executive discretion: every detention, seizure, or administrative action must clear a justificatory threshold that was not historically demanded, and that threshold has tightened over centuries of doctrinal expansion (habeas corpus, procedural due process, equal protection extensions). Suppression is moderate (0.62) because the guarantee's enforcement depends on courts choosing to hear cases and litigants having access — a suppression floor exists wherever access to judicial review is itself gated. Accessibility collapse is moderate-low (0.4): alternatives to due process (summary executive action) remain structurally available and are regularly exercised in emergency, immigration, and national-security contexts, so the constraint has not fully foreclosed the alternative it claims to have foreclosed. Resistance is substantial (0.58): executives across jurisdictions and eras have consistently resisted, narrowed, or carved exceptions into the doctrine, which is exactly what a genuinely constraining (rather than merely theatrical) doctrine should provoke.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial-review seat, this is coordination: a stable, generationally trusted mechanism for checking arbitrary power that legitimates the courts' own role. From the executive seat, the same mechanism is an ever-expanding, actively enforced limitation with rising cost. From the seat of marginalized groups and contested detainees, the doctrine is neither pure coordination nor pure protection — it is a promise whose delivery is gated by access, making its practical extractiveness against them show up as absence of protection rather than presence of burden.
 *
 * DIRECTIONALITY LOGIC:
 *   Judicial review institutions administer the constraint and derive legitimacy and jurisdiction from it — they sit as agenda_setter. Rights-bearing citizens and due-process litigants are named beneficiaries because the doctrine's stated function is to protect them, but litigants also pay through the cost and delay of establishing the guarantee against a resistant executive. The executive is the clearest payer: its discretion is the thing constrained. Detained persons and marginalized groups are payers of a different kind — they are the ones for whom the universal promise is most likely to fail in delivery, making them victims of the gap between doctrine and enforcement rather than of the doctrine's success.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict pairing is authored as contested rather than resolved: this reading insists the founding problem (unconstrained executive power) remains fully live in every generation, which is why the mismatch check does not fire a capture flag here — status is not 'dead.' But the corroboration field records that historians and originalist scholars dispute that the universal-rights framing itself is anything other than a later doctrinal accretion layered onto a narrower text, which is the structural seam this reading's expansiveness rides on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_baronial_scope_ambiguity,
    'Does Clause 39''s guarantee genuinely extend to universal individual rights against the state, or is the universal reading a doctrinal expansion projected backward onto a text that addressed a narrower baronial and free-tenant grievance in 1215?',
    'Comparative textual and historical analysis of the 1215 charter''s actual scope (who counted as a ''free man'' at the time) against the chain of subsequent doctrinal citations (Coke, colonial charters, U.S. and Commonwealth due process case law) that progressively broadened the reading.',
    'If the universal reading is substantially a later construction, this constraint''s high extractiveness and broad victim set describe the constructed doctrine''s operation today, not an original guarantee — which would not change ε for this reading (ε is reading-indexed) but would sharpen the corroboration gap already noted in the six_questions answers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_baronial_scope_ambiguity, conceptual, 'Whether the universal scope this reading claims is textually original or doctrinally accreted.').

omega_variable(
    enforcement_gap_as_extraction_or_failure,
    'Is the persistent gap between the doctrine''s universal promise and its selective enforcement (against marginalized and detained populations) evidence that the constraint itself extracts asymmetrically, or evidence that enforcement institutions fail the constraint independently of its structure?',
    'Cross-jurisdictional comparison of enforcement outcomes where judicial access is equalized (e.g., appointed counsel, expedited review) versus where it is not, isolating whether the gap tracks doctrinal design or resource/access barriers external to the doctrine.',
    'If the gap is intrinsic to how courts have operationalized the doctrine, the tangled_rope classification (genuine coordination function + asymmetric extraction via selective enforcement) is well-supported. If the gap is purely a resourcing failure external to the doctrine, the doctrine itself looks closer to a rope with an unrelated enforcement problem layered on top.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_as_extraction_or_failure, empirical, 'Whether selective enforcement is internal to the constraint''s structure or an external implementation failure.').

omega_variable(
    kernel_framing_under_determination,
    'Given the same clause text, could a defensible single framing be chosen among the feudal, liberal, and originalist readings, or does the kernel itself lack a determinate single referent?',
    'Assess whether any single interpretive community (courts, historians, framers'' intent scholars) commands sufficient authority to fix one reading as canonical, versus whether the readings persist as genuinely coexisting, unresolved framings across different institutional and scholarly communities.',
    'If no single framing commands authority, all three readings remain live, separately-ε''d constraints indefinitely (as currently modeled); if one reading achieves interpretive dominance (e.g., through binding precedent), the dominant reading''s ε would become the operative one for practical purposes while the others become historical/critical readings only.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the three-reading decomposition reflects genuine indeterminacy or an eventually resolvable interpretive contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t20, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(magn_tr_t20, observed).
narrative_ontology:measurement(magn_tr_t40, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(magn_tr_t40, observed).
narrative_ontology:measurement(magn_tr_t60, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement_basis(magn_tr_t60, observed).
narrative_ontology:measurement(magn_tr_t80, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement_basis(magn_tr_t80, observed).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(magn_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t20, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(magn_be_t20, observed).
narrative_ontology:measurement(magn_be_t40, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(magn_be_t40, observed).
narrative_ontology:measurement(magn_be_t60, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(magn_be_t60, observed).
narrative_ontology:measurement(magn_be_t80, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(magn_be_t80, observed).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 100, 0.71).
narrative_ontology:measurement_basis(magn_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(magn_su_t0, observed).
narrative_ontology:measurement(magn_su_t20, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(magn_su_t20, observed).
narrative_ontology:measurement(magn_su_t40, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(magn_su_t40, observed).
narrative_ontology:measurement(magn_su_t60, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(magn_su_t60, observed).
narrative_ontology:measurement(magn_su_t80, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement_basis(magn_su_t80, observed).
narrative_ontology:measurement(magn_su_t100, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 100, 0.62).
narrative_ontology:measurement_basis(magn_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the magna_carta_clause_39 kernel, decomposed per the ε-invariance principle because the natural-language label 'Clause 39' covers structurally distinct claims with different victim sets and extraction profiles. liberal_due_process_reading claims the broadest scope and highest extractiveness against executive power; feudal_prerogative_reading claims a narrow procedural entitlement within an intact hierarchy (low extraction, narrow victim set); originalist_limitation_reading confines the guarantee to closed 1215-specific abuses (extraction bounded to a historical episode, minimal contemporary victim set). All three are linked via affects_constraints; each maintains its own stable ε assessed by its own reading's lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
