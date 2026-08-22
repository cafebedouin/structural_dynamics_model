% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset Parliamentary Sovereignty over Basic Law Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the PARLIAMENTARY SOVEREIGNTY READING of
 *   Israel's Basic Law interpretive authority: the Knesset, as an elected
 *   body, retains ultimate authority to interpret Basic Laws and amend them
 *   via simple majority, including the power to override judicial
 *   interpretations. This is one of three contested readings of the same
 *   constitutional kernel. The other readings—judicial supremacy (courts bind
 *   the Knesset) and balanced contestation (both institutions hold bounded
 *   authority)—are separate constraint stories linked via network
 *   relationships. This story describes ONLY the parliamentary reading's
 *   structural logic and empirical operation, without adjudicating its
 *   validity relative to the siblings. The claim/metric gap is intentional:
 *   parliamentary sovereignty is CLAIMED as rope (coordination of authority,
 *   no party extracts) while the metrics show moderate theater and
 *   extraction, reflecting the reading's empirical contestation and the
 *   suppression machinery needed to maintain parliamentary override authority
 *   against judicial resistance.
 *
 * KEY AGENTS:
 *   - Elected Knesset majority: holds the sovereign interpretive authority under this reading; benefits from unrestricted amendment power; bears the delegitimacy cost of overriding judicial dissent
 *   - Supreme Court judiciary: constrained to advisory/interpretive roles; excluded from binding veto; experiences suppression of its institutional authority claims
 *   - Permanent minorities and excluded groups: have no vote share to block majoritarian constitutional revision; rely entirely on majority restraint; experience high exit costs if constitutional protections are withdrawn
 *   - International treaty bodies: provide normative pressure but lack binding authority; operate in an observational/advisory role
 *   - Constitutional theorists: articulate competing readings; influence the interpretive frame but hold no institutional veto
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.18).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.22).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset Parliamentary Sovereignty over Basic Law Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a').
narrative_ontology:cs_kernel_codification('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a', distributed).
narrative_ontology:cs_authority_grounding('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a', extraction).
narrative_ontology:cs_interpretation_layer_present('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a').
narrative_ontology:cs_reading_relation('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a', foundational, electoral_legitimacy_supremacy).
narrative_ontology:cs_axiom_status(electoral_legitimacy_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a', electoral_legitimacy_supremacy, deontological).
narrative_ontology:cs_axiom('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a', foundational, no_supramajoritarian_veto).
narrative_ontology:cs_axiom_status(no_supramajoritarian_veto, holdable).
narrative_ontology:cs_axiom_grounding('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a', no_supramajoritarian_veto, instrumental).
narrative_ontology:cs_reference_frame('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a', knesset_sovereign_constituent_authority).
narrative_ontology:cs_drift_state('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a', contemporary_post_judicial_activism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('687a4e06-097f-4ba6-9e8a-1f9ea4e1f10a', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, elected_knesset_majority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_judiciary).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, electoral_mandates_transcend_judicial_constraint).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, constitutional_amendment_via_ordinary_legislation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The electoral majority in the Knesset, via constitutional convention and statutory Basic Law design, retains the authority to interpret Basic Laws and amend them via simple majority vote. No supermajority requirement, no constituent assembly, no judicial veto. This reading holds that electoral legitimacy is the supreme legitimacy source and that democratic mandates cannot be bound by prior legislative acts (including prior Basic Laws) that the current majority repudiates.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, elected_knesset_majority, agenda_setter,
    institutional, generational, analytical, national).

% Under this reading, the judiciary interprets and applies law within the sphere the Knesset permits, but possesses no authority to invalidate or constrain Knesset decisions about the Basic Laws themselves. The court's institutional authority is derivative and revocable: the Knesset can override any judicial interpretation, rewrite the Basic Laws, or restrict the court's jurisdiction via legislation. The court can advise, persuade, and articulate alternative readings, but cannot bind the sovereign.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_judiciary, observer).

% Groups without electoral leverage—Arab citizens, ultra-Orthodox women on specific issues, secular minorities on religious law questions—lack the vote share to block majoritarian revision of rights-protecting Basic Laws. Under this reading, their protections depend entirely on the Knesset majority's restraint; they have no court-backed veto. They are excluded from the agenda-setting circle in structural terms.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_population_groups, excluded,
    powerless, generational, trapped, national).

% Human rights treaty monitoring bodies (UN committees, regional courts) can assess whether Israeli legislation complies with international commitments, but under this reading they possess no binding veto power over Knesset decisions. They provide external critique and reputational pressure; the Knesset retains the final interpretive and amendment power.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_treaty_bodies, observer,
    institutional, generational, analytical, global).

% Academic and professional communities articulate alternative readings of the Basic Law framework and the proper division of authority. Under this reading they are normative voices only, without institutional power to bind; their role is to construct arguments and educate, not to adjudicate.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, constitutional_theorists_and_jurists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a definitive decision-making procedure for constitutional matters: the Knesset majority, as the seat of electoral legitimacy, can settle interpretive disputes about the Basic Laws without extended contestation or deadlock. Avoids the coordination problem of which institution resolves constitutional ambiguity.
% TRANSFER_FUNCTION: Transfers authority to make binding determinations about rights and governance structure from a potentially pluralist or contested bench to a single majoritarian institution. Moves the power to override prior constitutional commitments from constituent-assembly mode (hard amendment) to ordinary legislative mode (simple majority).
% ABSENT_VOICES: Permanent minorities, international human rights monitors, and dissenting judges are structurally excluded from the sovereignty circle. They can testify, petition, and publish dissents, but cannot block majoritarian constitutional revision. Constitutional theorists outside the legislature are excluded from binding authority.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty over Basic Law interpretation were foreclosed tomorrow—if the Knesset majority could no longer override judicial readings or amend Basic Laws via simple majority—the fundamental locus of constitutional authority would shift to the judiciary or to a supermajority requirement. Rights protections would become judicially enforced and harder to revise. Government structure and minority protections would depend on judicial interpretation rather than electoral will. The entire institutional order would reorganize.
% FOUNDING_PROBLEM: Israel lacks a written constitution adopted by a constituent assembly. The Basic Laws were drafted piecemeal by elected legislatures for ordinary policy problems. The founding problem is: who interprets these quasi-constitutional texts when their meaning is contested and their supremacy over ordinary law is claimed? Without a founding constituent moment, where does constitutional authority originate?
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars across the ideological spectrum attest that Israel's constitutional origins are ambiguous: some cite the Knesset's de facto constituent role (the court under Barak, plus academic consensus post-2000); others cite the absence of a constituent act and argue only electoral majorities can remedy that absence. No external corroborating authority (no prior constituent assembly exists to appeal to). The contest itself is the testimony—different institutional actors and theorists give incompatible accounts of legitimate constitutional authority.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.18 at endpoint) under this reading because parliamentary sovereignty is characterized as coordination (settling the question of who decides) rather than as exploitative hierarchy. The coordination function is genuine: a majoritarian answer to the problem of constitutional interpretation avoids deadlock and provides predictability. However, extractiveness is non-zero because the reading privileges electoral majorities over countermajoritarian protections; a shifting majority can override prior commitments made by previous majorities (including constitutional commitments), which allows for majoritarian rent-seeking via constitutional amendment. The suppression metric (0.22) reflects the institutional machinery needed to maintain this reading against competing judicial and international claims to authority—the suppression is moderate because the Knesset does not need to silence dissent (courts and minorities can speak), but it must foreclose their ability to bind. Theater ratio (0.31) is moderate-low: some constitutional rhetoric in the Knesset is performative (appeals to democratic will, sovereignty, electoral mandate) but the actual functionality is clear (simple majority amendment, no veto). The measurement series show a rise from 1948 (when the constitutional design was ad hoc and extractiveness was minimal) through 2000 (when judicial claims to supremacy peaked, forcing parliamentary reassertion and raising suppression), then stabilization at a lower level after 2010 as the status quo settled into a contested but operative equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The Knesset majority seats and the judiciary experience this constraint radically differently. From the majority's structural position, it is pure coordination: a democratic solution to interpretive ambiguity that respects electoral legitimacy. From the judiciary's structural position, it is a constraint on institutional authority: the court must recognize that its interpretive power is revocable and subordinate, which shapes what the court can claim and how far it can press. Permanent minorities experience it as exposure: no court-backed veto on majoritarian constitutional revision. The engine computes these divergences from the structural data (power levels, exit options, beneficiary/victim relationships) without adjudicating which perspective is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected Knesset majority is the structural beneficiary (retains sovereignty, can amend at will via simple majority—d near 0.0). The Supreme Court is the structural target (its authority is subordinate, conditional, revocable—d near 0.7). Minorities are targets (exit-trapped, no institutional voice, protection depends on majority restraint—d near 0.85). This directionality follows from the explicit beneficiary/victim declarations: 'elected_knesset_majority' benefits (unrestricted constitutional authority); the Supreme Court and minorities bear the cost (constrained authority, exposure to majoritarian override).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy classification because the founding problem (who resolves constitutional ambiguity when there is no constituent assembly?) remains live under this reading. The reading's answer—the electoral majority—is contested but functional. Theater ratio is moderate (0.31), not high (>0.65), so there is no strong piton signal. However, there is a secondary mandatrophy risk: if the Knesset majority uses its amendment power repeatedly for transient political advantage (e.g., changing the electoral threshold to disadvantage rivals, rewriting rights to target disfavored groups), the constraint's coordination function atrophies and it becomes pure extraction theater. The measurement series show theater rising sharply from 1992 to 2000 (the height of judicial–parliamentary contestation) then stabilizing, suggesting the constraint found an operative equilibrium rather than sliding into pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constituent_authority_derivation,
    'Where does the Knesset''s authority to interpret and amend Basic Laws derive from? Is it inherited from a founding constituent act (which Israel lacks), grounded in electoral sovereignty, or a pragmatic response to constitutional void?',
    'Genealogical analysis of the Basic Laws'' statutory origins and Knesset constitutional practice; comparison with other democracies that lack written constitutions (UK, New Zealand) to isolate the principles at work.',
    'If electoral sovereignty is the proper ground, this reading stands as a coherent account of legitimate authority. If constituent authority is required and lacks grounding in Israeli history, the reading rests on a foundation it cannot provide, and judicial supremacy or balanced contestation become more plausible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constituent_authority_derivation, conceptual, 'The legitimacy basis for Knesset constitutional authority in the absence of a constituent assembly.').

omega_variable(
    majoritarian_protection_boundary,
    'Can a Knesset majority legitimately override a prior majority''s commitments (including constitutional commitments) via simple legislative amendment? Where is the line between legitimate constitutional evolution and majoritarian expropriation of rights?',
    'Comparative constitutional law: study instances where majorities have used amendment power to strip protections (e.g., Germany''s Enabling Act, Hungary''s Basic Law revisions on judicial independence) and instances where supermajority or superlative-entrenchment rules have prevented majoritarian revocation of rights.',
    'If majoritarian override of prior commitments is legitimate, extractiveness remains low and coordination theory holds. If such override is illegitimate (because rights require entrenchment), extractiveness rises sharply and the constraint becomes a tool for majoritarian extraction, shifting the reading toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_protection_boundary, preference, 'The normative boundary between democratic flexibility and constitutional entrenchment.').

omega_variable(
    judicial_institutional_capture,
    'In suppressing judicial claims to constitutional veto authority, does the Knesset majority suppress a genuinely independent institutional check, or does it protect electoral sovereignty from judicial capture of constitutional authority?',
    'Institutional-competence analysis: compare the Knesset''s and Supreme Court''s track records on rights protection, majoritarian deference, and interpretive stability; examine whether judicial activism on Basic Laws has tracked electoral mandates or resisted them.',
    'If courts are competent and independent, suppression of judicial authority is worrying (targeting a check). If courts are captured by elite consensus or ideological commitments, suppression protects electoral accountability. The reading''s own logic depends on which institutional dynamic is operative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_institutional_capture, empirical, 'Whether judicial suppression protects or endangers constitutional governance.').

omega_variable(
    kernel_reading_identity,
    'Is this reading genuinely distinct from the balanced_contestation reading, or is it a repackaging of the same kernel under a majoritarian gloss?',
    'Structural comparison: the balanced reading asserts both institutions hold bounded authority AND the Knesset is constrained by international norms and judicial independence; this reading asserts the Knesset is unconstrained except by treaties. The reading_relations block (forecloses vs. coexists_with) encodes this structural difference; empirical observation of how the two readings handle concrete disputes (e.g., whether a Basic Law amendment invalidating judicial review would be permitted under each) confirms or refutes the distinction.',
    'If the readings are structurally distinct, the engine''s per-seat classification computation will diverge between them. If they collapse into one reading under different labels, the kernel decomposition is invalid and should be collapsed to one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading is a distinct instantiation of the kernel or a rhetorical variant of balanced contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement_basis(basi_tr_t1948, projected).
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1992, 0.12).
narrative_ontology:measurement_basis(basi_tr_t1992, observed).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(basi_tr_t2000, observed).
narrative_ontology:measurement(basi_tr_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement_basis(basi_tr_t2010, observed).
narrative_ontology:measurement(basi_tr_t2018, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2018, 0.31).
narrative_ontology:measurement_basis(basi_tr_t2018, observed).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2024, 0.31).
narrative_ontology:measurement_basis(basi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1948, 0.08).
narrative_ontology:measurement_basis(basi_be_t1948, projected).
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement_basis(basi_be_t1992, observed).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement_basis(basi_be_t2000, observed).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2010, 0.19).
narrative_ontology:measurement_basis(basi_be_t2010, observed).
narrative_ontology:measurement(basi_be_t2018, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2018, 0.21).
narrative_ontology:measurement_basis(basi_be_t2018, observed).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2024, 0.18).
narrative_ontology:measurement_basis(basi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1948, 0.08).
narrative_ontology:measurement_basis(basi_su_t1948, projected).
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1992, 0.18).
narrative_ontology:measurement_basis(basi_su_t1992, observed).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement_basis(basi_su_t2000, observed).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement_basis(basi_su_t2010, observed).
narrative_ontology:measurement(basi_su_t2018, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2018, 0.24).
narrative_ontology:measurement_basis(basi_su_t2018, observed).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2024, 0.22).
narrative_ontology:measurement_basis(basi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_electoral_system__plurality_democracy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_treaty_obligations__supremacy_clause).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel basic_law_interpretive_boundary. All three readings share the same kernel (Israel's constitutional design) but instantiate different structural claims about authority allocation. The parliamentary_sovereignty_reading asserts the Knesset majority holds ultimate authority; the judicial_supremacy_reading asserts the court holds binding veto power; the balanced_contestation_reading asserts both institutions hold bounded authority. These are not the same constraint viewed from different angles—they are genuinely different constraints with different ε values, different beneficiary/victim structures, and different classification paths. Link them via network.affects_constraints to enable the engine to track how constitutional contestation propagates across the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
