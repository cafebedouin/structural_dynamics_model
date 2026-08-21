% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'judicial supremacy' reading of
 *   the kernel 'basic_law_interpretive_authority'. It describes the
 *   institutional arrangement where courts hold final interpretive authority
 *   over constitutional meaning, grounded in specialized legal expertise and
 *   independence from political pressure. This reading emphasizes the
 *   judiciary's role in maintaining constitutional stability and protecting
 *   fundamental rights, often by striking down legislation passed by elected
 *   bodies.
 *
 * KEY AGENTS:
 *   - Judiciary: Primary agenda_setter (institutional/identity_locked) — benefits from institutional authority.
 *   - Elected Legislature: Primary payer (institutional/constrained) — bears costs of blocked legislation.
 *   - Electoral Majorities: Payer (organized/constrained) — bears costs of democratic will being thwarted.
 *   - Legal Profession & Constitutional Scholars: Beneficiaries (organized/mobile) — benefit from elevated expertise and stable interpretive framework.
 *   - Parliamentary Sovereignty Advocates & Popular Constitutionalism Advocates: Excluded (organized/constrained) — their alternative framings are outside the formal process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.75).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '4f51b735-aac6-41d2-b630-5665125b488d').
narrative_ontology:cs_kernel_codification('4f51b735-aac6-41d2-b630-5665125b488d', fixed_text).
narrative_ontology:cs_authority_grounding('4f51b735-aac6-41d2-b630-5665125b488d', lineage).
narrative_ontology:cs_interpretation_layer_present('4f51b735-aac6-41d2-b630-5665125b488d').
narrative_ontology:cs_reading_relation('4f51b735-aac6-41d2-b630-5665125b488d', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('4f51b735-aac6-41d2-b630-5665125b488d', basic_law_interpretive_authority__popular_constitutionalism_reading, forecloses).
narrative_ontology:cs_axiom('4f51b735-aac6-41d2-b630-5665125b488d', foundational, judicial_impartiality_axiom).
narrative_ontology:cs_axiom_status(judicial_impartiality_axiom, holdable).
narrative_ontology:cs_axiom_grounding('4f51b735-aac6-41d2-b630-5665125b488d', judicial_impartiality_axiom, deontological).
narrative_ontology:cs_axiom('4f51b735-aac6-41d2-b630-5665125b488d', foundational, constitutional_text_supremacy_axiom).
narrative_ontology:cs_axiom_status(constitutional_text_supremacy_axiom, holdable).
narrative_ontology:cs_axiom_grounding('4f51b735-aac6-41d2-b630-5665125b488d', constitutional_text_supremacy_axiom, conventional).
narrative_ontology:cs_reference_frame('4f51b735-aac6-41d2-b630-5665125b488d', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('4f51b735-aac6-41d2-b630-5665125b488d', contemporary_political_polarization, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4f51b735-aac6-41d2-b630-5665125b488d', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_scholars).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, political_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final interpretive authority over the constitution, striking down legislation deemed unconstitutional. Benefits from elevated institutional status and independence from direct political pressure. Its professional identity is fused with this role.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Passes laws reflecting the popular will, but these laws are subject to judicial review and potential invalidation. Bears the cost of legislative gridlock and the frustration of policy goals being blocked by unelected judges.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature, payer,
    institutional, biographical, constrained, national).

% Elects representatives whose policy mandates can be overturned by judicial decisions, leading to a sense of democratic deficit. Their ability to directly shape constitutional meaning is suppressed.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, immediate, constrained, national).

% Benefits from the complexity and specialized nature of constitutional law, which requires their expertise for interpretation, litigation, and advising. Their professional standing is enhanced by the judiciary's final authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Their field of study and expertise is elevated by the judiciary's role as the ultimate arbiter of constitutional meaning, providing a stable object of analysis and a clear institutional locus for constitutional development.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, beneficiary,
    organized, biographical, mobile, national).

% Their policy platforms and legislative achievements can be blocked or reshaped by judicial rulings, forcing them to adapt their strategies or engage in protracted political battles over judicial appointments.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, political_parties, payer,
    organized, biographical, constrained, national).

% Argue that the elected legislature should have final interpretive authority, reflecting democratic accountability. They are structurally excluded from the final adjudicatory process under judicial supremacy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, parliamentary_sovereignty_advocates, excluded,
    organized, generational, constrained, national).

% Contend that constitutional meaning should emerge from ongoing democratic contestation and public deliberation, rather than being settled by an elite judicial body. They are outside the formal interpretive framework.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, popular_constitutionalism_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative, and consistent interpretation of constitutional meaning, preventing constant political re-litigation and ensuring legal certainty and the protection of fundamental rights from transient majorities.
% TRANSFER_FUNCTION: Transfers final interpretive power over constitutional meaning from elected legislative and executive branches to the unelected judiciary. It also transfers the costs of resolving fundamental legal disputes to the judicial process, potentially leading to legislative gridlock.
% ABSENT_VOICES: Advocates of parliamentary sovereignty and popular constitutionalism are structurally excluded from the final interpretive process. They would argue for greater democratic accountability and direct popular input in shaping constitutional meaning.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the stability of constitutional meaning would be immediately contested by political branches, leading to a power vacuum or a shift towards legislative supremacy or direct popular interpretation, fundamentally altering the balance of power and legal certainty.
% FOUNDING_PROBLEM: To establish an impartial and stable arbiter of fundamental law, protecting minority rights and long-term constitutional principles from the shifting preferences of political majorities and ensuring a consistent application of the basic law.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and many legal scholars attest that the problem of protecting fundamental law from political expediency is still live. However, political branches and advocates of alternative constitutional theories argue that the founding problem is substantially addressed, and the current arrangement primarily serves to entrench judicial power and preferences.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.65) because judicial review can significantly alter policy outcomes and impose costs on the legislative process and the public will. Suppression is high (0.75) as the finality of judicial decisions actively suppresses alternative interpretations and legislative actions. Theater ratio is moderate (0.30); while judicial independence and expertise are real, the performance of impartiality can sometimes mask policy preferences or institutional self-preservation. The metrics show a gradual increase in extractiveness and suppression over the interval, reflecting the increasing assertiveness of judicial review and the political backlash it has generated.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and its beneficiaries (legal profession, scholars) perceive this arrangement as a necessary 'rope' for constitutional stability and rights protection. However, from the perspective of the elected legislature and electoral majorities, it functions as a 'snare' or 'tangled_rope', extracting democratic accountability and suppressing policy choices. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a clear beneficiary, collecting institutional authority and stability (low d). The legal profession and constitutional scholars also benefit from the system's complexity and their elevated role within it. The elected legislature, electoral majorities, and political parties are targets, bearing the costs of thwarted policy and democratic will (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide stable constitutional meaning and protect rights is still live, but its status is contested. Critics argue that the mechanism has drifted from its original purpose, becoming a tool for judicial policy-making rather than impartial arbitration, thus accumulating extraction. The 'contested' status of the founding problem reflects this ongoing debate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_independence_legitimacy,
    'Does judicial independence truly insulate constitutional interpretation from political pressure, or does it merely shift the locus of political contestation to judicial appointments and institutional legitimacy?',
    'Empirical studies on the correlation between judicial rulings and the political leanings of appointing authorities, and analysis of public and political responses to controversial judicial decisions.',
    'If judicial independence is found to primarily shift political contestation, the ''impartial arbiter'' framing weakens, potentially reclassifying the constraint as more extractive (e.g., Tangled Rope or Snare) due to a higher effective theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_legitimacy, empirical, 'The true nature of judicial independence in practice.').

omega_variable(
    constitutional_meaning_stability_tradeoff,
    'Is the stability of constitutional meaning achieved through judicial supremacy worth the cost of reduced democratic accountability and legislative flexibility?',
    'This is a preference-based question, resolvable through public deliberation, constitutional amendment processes, or shifts in political culture regarding institutional roles.',
    'A societal preference for greater democratic accountability could lead to reforms that reduce judicial supremacy, reclassifying the constraint towards a more coordinative (Rope) or temporary (Scaffold) form, or even dismantling it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_meaning_stability_tradeoff, preference, 'Societal value judgment on the balance between constitutional stability and democratic accountability.').

omega_variable(
    judicial_supremacy_vs_sibling_readings,
    'Given the existence of parliamentary sovereignty and popular constitutionalism as alternative readings, is judicial supremacy the most structurally sound or legitimate approach to constitutional interpretation?',
    'Conceptual analysis of the logical coherence and practical implications of each reading, alongside historical and comparative institutional studies of different constitutional systems.',
    'If a sibling reading is found to be more coherent or legitimate, the ''judicial supremacy'' constraint would be re-evaluated, potentially leading to a reclassification as a Snare (if its coordination function is deemed a cover) or a Piton (if its function has atrophied relative to its maintenance costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_sibling_readings, conceptual, 'The relative structural and normative validity of judicial supremacy compared to alternative constitutional interpretive frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1950, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(basi_tr_t1960, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(basi_tr_t1970, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1970, 0.24).
narrative_ontology:measurement(basi_tr_t1980, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement(basi_tr_t1990, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2000, 0.29).
narrative_ontology:measurement(basi_tr_t2010, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2010, 0.29).
narrative_ontology:measurement(basi_tr_t2020, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2020, 0.3).

% Extraction over time
narrative_ontology:measurement(basi_be_t1950, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(basi_be_t1960, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(basi_be_t1970, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(basi_be_t1980, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(basi_be_t1990, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(basi_be_t2020, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1950, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(basi_su_t1960, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(basi_su_t1970, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(basi_su_t1980, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(basi_su_t1990, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(basi_su_t2020, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_process_efficiency).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, minority_rights_protection).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'basic_law_interpretive_authority' kernel. Each reading represents a different structural claim about where final interpretive authority resides, leading to different ε values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
