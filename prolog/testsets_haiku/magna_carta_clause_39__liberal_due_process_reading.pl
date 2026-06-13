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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Clause 39 Liberal Due Process Reading: Universal Rights Against Arbitrary State Power
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   Clause 39 of Magna Carta (1215) reads: 'No free man shall be seized or
 *   imprisoned, or stripped of his rights or possessions, except by the
 *   lawful judgment of his equals or by the law of the land.' The LIBERAL DUE
 *   PROCESS READING interprets this as establishing a universal principle:
 *   all individuals have the right to due process before state deprivation of
 *   life, liberty, or property, constrained by established law rather than
 *   executive prerogative. This reading emerged gradually from the 17th
 *   century onward (English Bill of Rights, 1689; later American and European
 *   constitutional tradition) and now anchors rule-of-law doctrine globally.
 *   The reading is CONTESTABLE: feudal prerogative readings argue the clause
 *   preserved hierarchical order within established relations; originalist
 *   readings argue it addressed only 1215-specific abuses. This story
 *   instantiates the liberal reading alone, with its own ε,
 *   beneficiary/victim structure, and temporal trajectory. The reading
 *   contest itself is routed to omega variables and cs_structure fields.
 *
 * KEY AGENTS:
 *   - all_citizens: universal beneficiary under liberal reading (protected by due process right)
 *   - executive_authority: payer (constrained from arbitrary action; must justify deprivations procedurally)
 *   - legal_profession: beneficiary (gains institutional authority to mediate state action)
 *   - feudal_nobles: payer (lose prerogative extraction ability; subjected to same universal standard)
 *   - common_people_unfree: excluded analytically (were not in 1215 compact; liberal reading retroactively includes them)
 *   - originalist_interpreters: observer (track the reading's historical divergence from narrow 1215 framing)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.42).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.31).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Clause 39 Liberal Due Process Reading: Universal Rights Against Arbitrary State Power").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, '8e4f8706-ee7d-4f2e-a4a4-56a57d595960').
narrative_ontology:cs_kernel_codification('8e4f8706-ee7d-4f2e-a4a4-56a57d595960', fixed_text).
narrative_ontology:cs_authority_grounding('8e4f8706-ee7d-4f2e-a4a4-56a57d595960', lineage).
narrative_ontology:cs_interpretation_layer_present('8e4f8706-ee7d-4f2e-a4a4-56a57d595960').
narrative_ontology:cs_reading_relation('8e4f8706-ee7d-4f2e-a4a4-56a57d595960', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e4f8706-ee7d-4f2e-a4a4-56a57d595960', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('8e4f8706-ee7d-4f2e-a4a4-56a57d595960', foundational, universal_individual_dignity_principle).
narrative_ontology:cs_axiom_status(universal_individual_dignity_principle, holdable).
narrative_ontology:cs_axiom_grounding('8e4f8706-ee7d-4f2e-a4a4-56a57d595960', universal_individual_dignity_principle, deontological).
narrative_ontology:cs_axiom('8e4f8706-ee7d-4f2e-a4a4-56a57d595960', foundational, procedural_constraint_on_state_discretion).
narrative_ontology:cs_axiom_status(procedural_constraint_on_state_discretion, holdable).
narrative_ontology:cs_axiom_grounding('8e4f8706-ee7d-4f2e-a4a4-56a57d595960', procedural_constraint_on_state_discretion, instrumental).
narrative_ontology:cs_reference_frame('8e4f8706-ee7d-4f2e-a4a4-56a57d595960', universal_due_process_right).
narrative_ontology:cs_drift_state('8e4f8706-ee7d-4f2e-a4a4-56a57d595960', contemporary_2020s, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8e4f8706-ee7d-4f2e-a4a4-56a57d595960', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, all_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, procedural_justice_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, legal_profession).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_authority).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, feudal_nobles_and_landholders).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, individual_dignity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, all citizens are protected by a universal right to due process before deprivation of life, liberty, or property. They benefit from predictable law and procedural safeguards against arbitrary executive action. Their exit is territorial — leaving the jurisdiction is the only true exit; challenging the reading itself requires sustained constitutional contest.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, all_citizens, beneficiary,
    organized, generational, trapped, national).

% Constrained by the reading to justify deprivations through established procedure rather than prerogative discretion. The reading strips away the traditional feudal claim to arbitrary action in service of realm order. Enforcement machinery (courts, parliaments, legal profession) must be maintained to validate executive action against the standard. The executive bears the cost of procedural compliance and judicial review.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_authority, payer,
    institutional, generational, constrained, national).

% Lost the ability to extract from subordinates without procedural constraint under the old prerogative reading. Now subject to the same universal standard. Their property claims face scrutiny; their status hierarchies no longer shield them from due process requirements. They resist the expansion but remain trapped within the jurisdiction.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, feudal_nobles_and_landholders, payer,
    powerful, biographical, constrained, national).

% Gains institutional authority and employment through the requirement that legal procedures be observed and interpreted by trained practitioners. The reading expands the domain where lawyers mediate state action. Professional identity and economic interest are aligned with the universal due process standard.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legal_profession, beneficiary,
    organized, generational, mobile, national).

% Would argue that Clause 39 addresses only procedural form within established hierarchy, not universal rights; that reading survives in customary practice and resistance to the liberal expansion. Excluded from the institutional reading but live in competing constitutional genealogies and in the reading contest itself.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, feudal_prerogative_adherents, excluded,
    institutional, generational, trapped, national).

% Analytical seat tracking the contest: argue that Clause 39 was narrowly drafted to address specific 1215 grievances (arbitrary reliefs, forest rights, wardship abuses) and that universal expansion is a later reading not grounded in the original text. Document the reading's historical genealogy and measure its divergence from 13th-century intent.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, originalist_interpreters, observer,
    organized, generational, analytical, national).

% Were not participants in the 1215 Magna Carta compact (which was an agreement between king and barons). The liberal reading EXPANDS the clause to include them retroactively, but the original structure excluded them. Their inclusion is a reading innovation, not a restatement. They would have objected to their exclusion but were structurally absent from the constitutional conversation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, common_people_and_unfree, excluded,
    powerless, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common procedural standard for all state deprivations of life, liberty, or property: the constraint coordinates expectations around rule of law rather than prerogative discretion. Creates a stable legal framework within which subjects know what triggers state action and what process must be observed.
% TRANSFER_FUNCTION: Transfers authority to adjudicate state action from executive prerogative to legal procedure and judicial review. Moves the burden of justification from subjects (prove why the executive should not act) to the state (prove why this person should be deprived via established process). Moves institutional power from feudal hierarchy to legal institutions.
% ABSENT_VOICES: The common people and unfree were not parties to the 1215 compact and would have objected to their exclusion. Under the feudal prerogative reading, their absence was structural. Under the liberal reading, the expansion to universal rights retroactively includes them, but the historical genealogy shows they were not in the room when the bargain was struck. Modern reading contests include them; 13th-century compact did not.
% DISAPPEARANCE_RATIONALE: If this liberal reading of Clause 39 disappeared and prerogative authority returned unconstrained, state action would no longer require procedural justification. Citizens could be deprived of property, imprisoned, or executed at executive discretion without process. The entire apparatus of courts, written law, and procedural review depends on this reading being institutionalized. Its disappearance would unmake the constitutional structure built upon it.
% FOUNDING_PROBLEM: King John's documented abuses: arbitrary reliefs extracted from heirs, forest laws used as revenue mechanisms, arbitrary imprisonment, wardship exploited as feudal profit. The clause was written to constrain these specific practices within a feudal framework.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (arbitrary royal abuse in 1215) is attested by chroniclers and the clause's own text listing specific grievances (reliefs, wardships, forest rights). The LIBERAL READING's claim that Clause 39 establishes universal rights is NOT attested by 1215 sources; it is a later interpretive expansion. Legal historians outside the benefiting parties (originalist school, feudal-prerogative school) document this gap between the original narrow framing and the later universal expansion. The liberal reading itself acknowledges the historical expansion; legitimacy rests on the claim that universal principles flow from the clause's logic, not its original scope.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).
:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS at 0.42 reflects the reading's constraint on executive discretion: the executive pays through procedural burden, judicial review, and loss of arbitrary power. This is moderate, not high, because the reading simultaneously coordinates expectations and benefits from legitimacy (rule of law is not pure extraction; it provides the executive with rule-based authority). SUPPRESSION at 0.31 is low because the reading's persistence depends on institutional belief (courts, legal profession, educated public) rather than coercive force—the suppression that DOES exist targets feudal prerogative claims, not citizens. THEATER at 0.22 reflects that procedural performance is genuine (courts and trials occur for real; people experience real process) but some procedures are maintained theatrically (forms observed without substance in capture scenarios). ACCESSIBILITY_COLLAPSE at 0.78 is high: once the universal rights principle is articulated and institutionalized, alternatives (prerogative discretion, hierarchical status-based rights) become structurally difficult to claim—the reading forecloses the prerogative alternative within the same legal framework. RESISTANCE at 0.58 reflects that feudal nobility, some executive actors, and prerogative traditionalists actively contest the reading; but the reading has deep institutional support (courts, legal education, written constitution in later systems), so resistance does not prevent institutionalization. The measurement series tracks the reading's EXPANSION over time: extractiveness and theater both rise as the reading moves from narrow procedural constraint (1215-1689) to universal principle (1689-present), capturing the interpretation layer's work in broadening the clause's scope.
 *
 * PERSPECTIVAL GAP:
 *   The executive and feudal nobility perceive the constraint as extractive and constraining (they lose discretionary power); all citizens and the legal profession perceive it as coordinating and protective (establishing rule of law). The engine computes these divergences from the structural data: executive seats get high d (target), citizen/legal seats get low d (beneficiary). The divergence is the point—different seats in the same constraint experience opposite directionality because the constraint redistributes authority FROM executive discretion TO procedure and law.
 *
 * DIRECTIONALITY LOGIC:
 *   All citizens are beneficiaries of the universal right to due process (d near 0.0, subsidized by the constraint's protection); the legal profession is a beneficiary (d near 0.15, gains institutional authority); the executive authority is a payer (d near 0.85, constrained and burdened); feudal nobles are payers (d near 0.75, lose prerogative extraction). The originalist and feudal prerogative readings are EXCLUDED AGENTS from this story's perspective, not stakeholders—they are other readings, not seats in this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The liberal reading shows signs of living mandate: the founding problem (arbitrary state deprivation) remains live in contemporary discourse (emergency powers, detention, property seizure), and the constraint continues to be actively maintained and litigated. The disappearance verdict is world_rearranges because removing this reading would unmake constitutional order globally. NO mandatrophy signal. The theater rising over time reflects the reading's expansion and the procedural apparatus's growth, not degradation of function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_textual_interpretation,
    'Does Clause 39 establish universal rights through natural-law principle underlying the text, or only the narrow procedural forms explicitly stated in 1215 language?',
    'Historical analysis of how 13th-century interpreters read the clause vs. later expansion; comparison of contemporaneous sources and early commentary; textual analysis of the clause''s logical scope vs. its historical application.',
    'If natural-law principle grounds the reading, the expansion to universal rights is interpretive truth-discovery; if only explicit text binds, the expansion is re-writing and the feudal-prerogative or originalist readings become more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_textual_interpretation, conceptual, 'Whether universal rights are inherent to the clause or added by later interpretation.').

omega_variable(
    historical_scope_vs_logical_scope,
    'Did Clause 39 in 1215 apply only to free men (excluding serfs and the unfree), or does the principle it establishes extend universally once articulated?',
    'Examination of 1215 Magna Carta''s explicit language and signatories'' intent; tracing the doctrine''s expansion through English Bill of Rights (1689), American Constitution (13th-14th Amendments), and international human rights; determining whether the expansion is reinterpretation (principle was always there) or novel application.',
    'If the principle was always universal but scope-limited by circumstance, the liberal reading is deepening latent principle; if the expansion is genuinely novel, the reading is a later construction that cannot claim 1215 authority, only 17th-century-onward development.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_scope_vs_logical_scope, empirical, 'Whether universal application is logical expansion or historical innovation.').

omega_variable(
    prerogative_suppression_in_implementation,
    'Is executive suppression of prerogative challenges (feudal-style arbitrary action) experienced as external structural suppression or as internalized legal norm acceptance?',
    'Historical analysis of how executive authority has defended constraints: through coercive legal sanction, through institutional habituation, through ideological commitment to rule of law. Post-suppression trajectory: when prerogative claims surface (wartime emergency powers, colonial administration, authoritarian regression), do they persist or re-emerge as alternative authority claims?',
    'If suppression is primarily structural (courts block prerogative; legal profession enforces constraint), the constraint''s persistence is architecturally enforced. If suppression is internalized (actors have accepted the principle), the constraint is fragile to ideological drift or institutional capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prerogative_suppression_in_implementation, empirical, 'Whether suppression of prerogative claims is structural or internalized.').

omega_variable(
    reading_contest_as_extraction_mechanism,
    'Does the unresolved contest between the three readings (feudal, liberal, originalist) itself function as an extraction mechanism, where different parties appeal to different readings to justify their preferred outcomes?',
    'Analysis of how the three readings are deployed in contemporary litigation and policy: do parties select readings to support pre-determined positions? Does the contest enable jurisdictional arbitrage (shopping for favorable reading)? Does the unresolved state preserve ambiguity that different power seats exploit?',
    'If the reading contest is itself extractive, the constraint''s classification might shift from rope (coordination) toward tangled_rope or snare (extraction with readings as cover). The beneficiary would then be those who can invoke favorable readings (wealthy litigants, powerful states), not universal citizens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_as_extraction_mechanism, empirical, 'Whether the reading contest itself becomes an extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement_basis(magn_tr_t100, observed).
narrative_ontology:measurement(magn_tr_t200, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 200, 0.11).
narrative_ontology:measurement_basis(magn_tr_t200, observed).
narrative_ontology:measurement(magn_tr_t400, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 400, 0.17).
narrative_ontology:measurement_basis(magn_tr_t400, observed).
narrative_ontology:measurement(magn_tr_t600, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 600, 0.2).
narrative_ontology:measurement_basis(magn_tr_t600, observed).
narrative_ontology:measurement(magn_tr_t800, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 800, 0.22).
narrative_ontology:measurement_basis(magn_tr_t800, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement_basis(magn_be_t100, observed).
narrative_ontology:measurement(magn_be_t200, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 200, 0.28).
narrative_ontology:measurement_basis(magn_be_t200, observed).
narrative_ontology:measurement(magn_be_t400, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 400, 0.38).
narrative_ontology:measurement_basis(magn_be_t400, observed).
narrative_ontology:measurement(magn_be_t600, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 600, 0.41).
narrative_ontology:measurement_basis(magn_be_t600, observed).
narrative_ontology:measurement(magn_be_t800, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 800, 0.42).
narrative_ontology:measurement_basis(magn_be_t800, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(magn_su_t0, observed).
narrative_ontology:measurement(magn_su_t100, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement_basis(magn_su_t100, observed).
narrative_ontology:measurement(magn_su_t200, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 200, 0.25).
narrative_ontology:measurement_basis(magn_su_t200, observed).
narrative_ontology:measurement(magn_su_t400, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 400, 0.29).
narrative_ontology:measurement_basis(magn_su_t400, observed).
narrative_ontology:measurement(magn_su_t600, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 600, 0.3).
narrative_ontology:measurement_basis(magn_su_t600, observed).
narrative_ontology:measurement(magn_su_t800, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 800, 0.31).
narrative_ontology:measurement_basis(magn_su_t800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__liberal_due_process_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__originalist_limitation_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, english_bill_of_rights_1689_due_process).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, american_constitutional_due_process_14th_amendment).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Magna Carta Clause 39 kernel. All three readings share the same text but interpret it with fundamentally different scope and principle. The liberal reading expanded the clause from feudal procedure to universal rights; it influences later American and international due process doctrine. The originalist reading constrains it to 1215-specific abuses; it influences strict-construction jurisprudence. The feudal prerogative reading preserves it as hierarchical procedure; it influences residual executive authority claims. Each reading is a separate constraint story with its own ε, beneficiary/victim structure, and type classification. They are linked via network.affects_constraints because institutional credibility of one reading affects the viability of the others—legal systems tend toward one reading or toward contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
