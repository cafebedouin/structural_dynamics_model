% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions — Security Maximization Reading (asymmetric conflict suspension)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story captures the security_maximization_reading of the
 *   1949 Geneva Conventions kernel — the reading that treats the Conventions
 *   as peacetime aspirations that yield to operational necessity in
 *   asymmetric conflict. Under this reading, irregular warfare justifies
 *   suspending most protections to maximize state security. The structural
 *   delta from the baseline Conventions is severe: the unlawful combatant
 *   category expands to deny POW status and habeas corpus; civilian immunity
 *   is degraded via 'human shields' doctrine and accepted collateral damage;
 *   indefinite detention without trial becomes normalized; coercive
 *   interrogation is reclassified as non-torture. The reading claims the type
 *   of a necessary coordination mechanism (rope/scaffold) for state survival
 *   against irregular threats, but the metrics describe a constraint with
 *   high base extractiveness (0.88) and high suppression (0.92) that actively
 *   enforces the exclusion of irregular combatants and civilian populations
 *   from protections. The claim/metric divergence is the measurement — the
 *   engine computes the actual per-seat types from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.88).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.92).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions — Security Maximization Reading (asymmetric conflict suspension)").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, '753a1d56-11c2-4f6c-9775-80d1021a10ba').
narrative_ontology:cs_kernel_codification('753a1d56-11c2-4f6c-9775-80d1021a10ba', formalized).
narrative_ontology:cs_authority_grounding('753a1d56-11c2-4f6c-9775-80d1021a10ba', extraction).
narrative_ontology:cs_interpretation_layer_present('753a1d56-11c2-4f6c-9775-80d1021a10ba').
narrative_ontology:cs_reading_relation('753a1d56-11c2-4f6c-9775-80d1021a10ba', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('753a1d56-11c2-4f6c-9775-80d1021a10ba', geneva_conventions_1949__conditional_reciprocity_reading, influences).
narrative_ontology:cs_axiom('753a1d56-11c2-4f6c-9775-80d1021a10ba', foundational, operational_necessity_supersedes_convention_protections).
narrative_ontology:cs_axiom_status(operational_necessity_supersedes_convention_protections, holdable).
narrative_ontology:cs_axiom_grounding('753a1d56-11c2-4f6c-9775-80d1021a10ba', operational_necessity_supersedes_convention_protections, instrumental).
narrative_ontology:cs_axiom('753a1d56-11c2-4f6c-9775-80d1021a10ba', foundational, irregular_adversary_non_reciprocity_justifies_unilateral_suspension).
narrative_ontology:cs_axiom_status(irregular_adversary_non_reciprocity_justifies_unilateral_suspension, holdable).
narrative_ontology:cs_axiom_grounding('753a1d56-11c2-4f6c-9775-80d1021a10ba', irregular_adversary_non_reciprocity_justifies_unilateral_suspension, conventional).
narrative_ontology:cs_axiom('753a1d56-11c2-4f6c-9775-80d1021a10ba', secondary, unlawful_combatant_category_permits_indefinite_detention_without_trial).
narrative_ontology:cs_axiom_status(unlawful_combatant_category_permits_indefinite_detention_without_trial, holdable).
narrative_ontology:cs_axiom_grounding('753a1d56-11c2-4f6c-9775-80d1021a10ba', unlawful_combatant_category_permits_indefinite_detention_without_trial, instrumental).
narrative_ontology:cs_axiom('753a1d56-11c2-4f6c-9775-80d1021a10ba', secondary, human_shields_doctrine_degrades_attacker_obligations).
narrative_ontology:cs_axiom_status(human_shields_doctrine_degrades_attacker_obligations, holdable).
narrative_ontology:cs_axiom_grounding('753a1d56-11c2-4f6c-9775-80d1021a10ba', human_shields_doctrine_degrades_attacker_obligations, instrumental).
narrative_ontology:cs_reference_frame('753a1d56-11c2-4f6c-9775-80d1021a10ba', geneva_conventions_1949_formal_text).
narrative_ontology:cs_drift_state('753a1d56-11c2-4f6c-9775-80d1021a10ba', post_2001_global_war_on_terror, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('753a1d56-11c2-4f6c-9775-80d1021a10ba', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_military_commands).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, national_security_establishments).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detained_irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, designated_unlawful_combatants).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, operational_necessity_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, state_supremacy_in_security).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, asymmetric_conflict_exceptionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and enforce the operational interpretation of the Conventions in asymmetric conflict. Define 'operational necessity,' designate unlawful combatants, set detention and interrogation policies. Collect the operational discretion and legal cover the reading provides. Face no meaningful accountability for interpretations that expand their authority.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_military_commands, agenda_setter,
    institutional, biographical, arbitrage, global).

% Receive expanded legal authorities, reduced oversight, and institutional mission growth from the reading's normalization of indefinite detention, coercive interrogation, and civilian immunity degradation. The reading validates and resources their counterterrorism and irregular warfare apparatuses.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, national_security_establishments, beneficiary,
    institutional, generational, arbitrage, global).

% Captured in asymmetric conflict, denied POW status, held without charge or trial, subjected to coercive interrogation. No legal exit — habeas corpus suspended, no access to courts, no protecting power. Their legal status is assigned by the detaining authority.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detained_irregular_combatants, payer,
    powerless, immediate, trapped, local).

% Experience degraded protection via 'human shields' doctrine (their presence near combatants reduces attacker obligations) and accepted collateral damage. No exit from conflict zone, no influence on targeting decisions, no effective remedy for violations.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Individuals categorized as outside Convention protections by state designation. The designation itself removes legal protections and is not subject to independent review. Identity-locked because the category is defined by the beneficiary and the designated individual cannot exit the category except by the beneficiary's revocation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, designated_unlawful_combatants, payer,
    powerless, biographical, identity_locked, global).

% Monitor compliance, visit detainees, promote Convention adherence. See the full structural divergence between text and practice. Have moral authority but no enforcement power. Their access and effectiveness depend on state consent.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, icrc_and_treaty_bodies, observer,
    organized, generational, analytical, global).

% Some national courts (e.g., US Supreme Court in Boumediene, UK courts, Israeli HCJ) have pushed back on indefinite detention and unlawful combatant designations. Their rulings create friction but are often circumvented by legislative overrides or jurisdictional limits. They would object more systematically but are excluded from the operational theater.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, domestic_courts_in_some_jurisdictions, excluded,
    powerful, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of how states conduct irregular warfare against non-state adversaries who do not reciprocate Convention obligations — provides a shared (among adopting states) interpretive framework that legitimizes expanded operational latitude.
% TRANSFER_FUNCTION: Moves legal protections, procedural rights, and physical safety from detained irregular combatants, civilian populations, and designated unlawful combatants to state military commands and national security establishments — as operational discretion, legal immunity, and institutional authority.
% ABSENT_VOICES: The detained and designated themselves (trapped/identity_locked, no access to discourse), future civilian populations in conflicts not yet begun, and states that would adopt the humanitarian_ceiling_reading but face security pressure to conform. The excluded stakeholder seat (domestic courts) represents a partial institutional voice that is structurally marginalized in the operational theater.
% DISAPPEARANCE_RATIONALE: If the security_maximization_reading vanished overnight, states would lose the legal framework legitimizing unlawful combatant designations, indefinite detention without trial, and degraded civilian immunity. Detention policies would require legal basis, interrogation would face criminal liability, targeting would require higher verification. The irregular warfare operational model would face immediate legal crisis.
% FOUNDING_PROBLEM: The 1949 Conventions were built to solve the problem of protecting combatants and civilians in interstate wars between regular armies that broadly reciprocate obligations. The security_maximization_reading claims this problem has been superseded by asymmetric conflict against non-reciprocating irregular adversaries, requiring a new operational framework.
% FOUNDING_PROBLEM_CORROBORATION: The security_maximization_reading's beneficiaries (state military commands, national security establishments) attest the founding problem is superseded. The humanitarian_ceiling_reading's proponents (ICRC, human rights NGOs, some international legal scholars, some domestic courts) attest the founding problem persists — civilians and hors de combat fighters still need protection, and the Conventions' rules still apply. The conditional_reciprocity_reading's proponents (some state legal advisors, some international lawyers) attest the problem is partially transformed but reciprocal restraints still function. No neutral arbiter has settled the dispute; the status remains contested across the constraint family.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.88) is high because the reading extracts near-total discretion for state military commands from the protective framework — the protections exist on paper but are suspended in the operational domain where they would constrain. Suppression (0.92) is very high because the constraint's operation depends on actively denying legal status (unlawful combatant designation), denying judicial review (habeas corpus suspension), and normalizing coercive practices — alternatives (full Convention compliance, judicial oversight, criminal prosecution of violators) are structurally suppressed, not merely discouraged. Theater ratio (0.25) is moderate: the Conventions' text and monitoring bodies (ICRC, treaty bodies) remain active, creating a performative compliance layer, but the operational reality diverges substantially. Accessibility collapse (0.35) is moderate: legal alternatives (humanitarian_ceiling_reading, conditional_reciprocity_reading) persist in discourse and some jurisdictions, but the security_maximization_reading dominates operational practice in major state conflicts. Resistance (0.55) is significant: human rights NGOs, some domestic courts, and international bodies resist, but the constraint persists because the beneficiaries (state military commands, national security establishments) control the enforcement apparatus.
 *
 * PERSPECTIVAL GAP:
 *   From the state military command seat, the constraint appears as a necessary adaptation — a coordination mechanism that solves the genuine problem of applying peacetime rules to asymmetric adversaries who do not reciprocate. From the detained irregular combatant seat, the same structure is a snare: protections are suspended, status is denied, and there is no exit. The engine computes this divergence from the declared beneficiary/victim structure and exit options. The reading's own claim (coordination necessity) matches the beneficiary seat's experience; the victim seat's experience computes as snare. The engine captures both.
 *
 * DIRECTIONALITY LOGIC:
 *   State military commands and national security establishments are structural beneficiaries (d near 0.0): they gain operational discretion, legal cover for expanded authorities, and reduced accountability. The constraint subsidizes their preferences. Detained irregular combatants, civilian populations in conflict zones, and designated unlawful combatants are structural victims (d near 1.0): they bear the full cost of suspended protections with no exit (identity_locked or trapped — their legal status is assigned by the beneficiary). The analytical seat (ICRC, treaty bodies, human rights NGOs) sees the full structure but has no enforcement power. The constraint's scope is global (universal treaty regime) but its extraction is concentrated on powerless agents in conflict zones — the scope amplifies effective extraction for those with no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting combatants and civilians in interstate war) is contested as live — the security_maximization_reading argues asymmetric conflict is a novel problem requiring novel rules; the humanitarian_ceiling_reading argues the original problem persists and the Conventions solve it. The security_maximization_reading has not formally resolved its mandatrophy — it claims the founding problem has mutated, not disappeared. The constraint persists because the beneficiaries control the interpretation machinery and the victims have no structural power to force revision. This is not a piton (inertial remnant) — it is actively maintained and expanded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the geneva_conventions_1949 kernel, and does the security_maximization_reading instantiate a structurally distinct constraint from its sibling readings?',
    'Compare the ε values and victim structures across the three declared readings. If ε differs substantially and the victim/beneficiary sets differ structurally, the readings are distinct constraints per the ε-invariance principle.',
    'If readings are distinct constraints, each must be authored separately with its own ε, stakeholders, and classification. The kernel_id links them analytically but does not merge them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the security_maximization_reading is a separate constraint from humanitarian_ceiling_reading and conditional_reciprocity_reading').

omega_variable(
    security_necessity_vs_extraction_boundary,
    'Does the expansion of ''operational necessity'' in asymmetric conflict represent a genuine coordination adaptation to novel threat structures, or does it function as an open-ended extraction license for state security apparatuses?',
    'Track the scope of operations justified under operational necessity over time. If the category expands without corresponding threat-structure change, the coordination cover thins toward pure extraction.',
    'If operational necessity is an extraction license, the constraint is snare/tangled_rope. If it tracks genuine novel coordination needs, it may be rope/scaffold with genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_necessity_vs_extraction_boundary, empirical, 'Whether operational necessity doctrine coordinates or extracts in asymmetric conflict').

omega_variable(
    civilian_immunity_degradation_mechanism,
    'Is the degradation of civilian immunity via ''human shields'' doctrine and collateral damage acceptance driven by genuine tactical impossibility or by lowered verification standards that permit extraction?',
    'Compare civilian casualty rates and targeting verification procedures in conflicts where the security_maximization_reading is invoked versus those where it is not. Control for conflict intensity and adversary type.',
    'If degradation is extraction-driven, the constraint''s suppression and extractiveness metrics are understated; if structurally necessary, the metrics reflect real coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_immunity_degradation_mechanism, empirical, 'Structural vs. extractive driver of civilian immunity degradation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__security_maximization_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_1949__security_maximization_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(gene_tr_t2004, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2004, 0.22).
narrative_ontology:measurement(gene_tr_t2013, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2013, 0.24).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 1949, 0.25).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 1977, 0.4).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2001, 0.7).
narrative_ontology:measurement(gene_be_t2004, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2004, 0.82).
narrative_ontology:measurement(gene_be_t2013, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2013, 0.85).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 1949, 0.3).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 1977, 0.45).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2001, 0.75).
narrative_ontology:measurement(gene_su_t2004, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2004, 0.85).
narrative_ontology:measurement(gene_su_t2013, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2013, 0.89).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__security_maximization_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, international_humanitarian_law_enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, unlawful_combatant_designation_regime).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, indefinite_detention_authority_post_2001).

% DUAL FORMULATION NOTE:
% This constraint is one member of the geneva_conventions_1949 constraint family (kernel_id: geneva_conventions_1949). The three declared readings instantiate structurally distinct constraints with different ε values, different victim/beneficiary structures, and different classifications. The security_maximization_reading has the highest extractiveness (0.88) and is classified as snare. The humanitarian_ceiling_reading has near-zero extractiveness and classifies as mountain/rope. The conditional_reciprocity_reading has intermediate extractiveness and classifies as tangled_rope. They are linked via network.affects_constraints and the shared kernel_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__security_maximization_reading, institutional, 0.1).
constraint_indexing:directionality_override(geneva_conventions_1949__security_maximization_reading, powerless, 0.98).
constraint_indexing:directionality_override(geneva_conventions_1949__security_maximization_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
