% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Interpretive Supremacy: Constitutional Rights Guardianship
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint captures the judicial supremacy reading of constitutional
 *   interpretive authority: courts possess final, unreviewable power to
 *   determine what the constitution means and to nullify legislative acts
 *   deemed unconstitutional. The reading treats constitutional meaning as
 *   something the judiciary authoritatively discovers and pronounces; other
 *   branches and the electorate must defer. This reading contrasts with
 *   coordinate construction (constitution is meaning made through
 *   inter-branch dialogue) and parliamentary supremacy (elected legislature
 *   interprets finally). The judicial supremacy reading legitimates coercion
 *   (legislative nullification, electoral override) via rights-compliance
 *   framing: what would otherwise be judicial override of democratic will is
 *   justified as protection of constitutional rights. The tension between
 *   democratic majoritarian will and judicial rights-protection runs through
 *   the constraint; different stakeholders experience it differently
 *   depending on whether their interests align with the judiciary's
 *   constitutional readings.
 *
 * KEY AGENTS:
 *   - Judiciary: agenda-setter and beneficiary; possesses final interpretive authority, derives institutional power and prestige, shapes policy via constitutional pronouncements
 *   - Legislature: payer; operates under permanent subordination; faces legislative nullification threat; lacks final interpretive voice
 *   - Electoral majorities: payer; electoral outcomes subordinated to judicial constitutional interpretation; preferences overrideable on rights grounds
 *   - Rights-bearing minorities: beneficiary; gain protection when their interests align with judicial readings; lose when they do not
 *   - Unorganized majorities with non-rights interests: payer; lack standing in judicial system; vulnerable to nullification
 *   - Coordinate construction advocates: excluded; their preferred allocation of interpretive authority overridden by judicial veto
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.72).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Interpretive Supremacy: Constitutional Rights Guardianship").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, 'b12e1a07-eb8f-4adf-b46c-4e85506046d0').
narrative_ontology:cs_kernel_codification('b12e1a07-eb8f-4adf-b46c-4e85506046d0', formalized).
narrative_ontology:cs_authority_grounding('b12e1a07-eb8f-4adf-b46c-4e85506046d0', lineage).
narrative_ontology:cs_interpretation_layer_present('b12e1a07-eb8f-4adf-b46c-4e85506046d0').
narrative_ontology:cs_reading_relation('b12e1a07-eb8f-4adf-b46c-4e85506046d0', constitutional_interpretive_authority__coordinate_construction_reading, influences).
narrative_ontology:cs_reading_relation('b12e1a07-eb8f-4adf-b46c-4e85506046d0', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_axiom('b12e1a07-eb8f-4adf-b46c-4e85506046d0', foundational, judicial_finality_in_constitutional_interpretation).
narrative_ontology:cs_axiom_status(judicial_finality_in_constitutional_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('b12e1a07-eb8f-4adf-b46c-4e85506046d0', judicial_finality_in_constitutional_interpretation, deontological).
narrative_ontology:cs_axiom('b12e1a07-eb8f-4adf-b46c-4e85506046d0', foundational, fundamental_rights_require_counter_majoritarian_protection).
narrative_ontology:cs_axiom_status(fundamental_rights_require_counter_majoritarian_protection, holdable).
narrative_ontology:cs_axiom_grounding('b12e1a07-eb8f-4adf-b46c-4e85506046d0', fundamental_rights_require_counter_majoritarian_protection, deontological).
narrative_ontology:cs_reference_frame('b12e1a07-eb8f-4adf-b46c-4e85506046d0', constitutional_rule_of_law_via_judicial_review).
narrative_ontology:cs_drift_state('b12e1a07-eb8f-4adf-b46c-4e85506046d0', contemporary_rights_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b12e1a07-eb8f-4adf-b46c-4e85506046d0', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_rights_holders).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, non_rights_articulated_interests).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, judicial_review_legitimacy).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, fundamental_rights_inviolability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses the final, unreviewable authority to interpret the constitution and nullify legislative acts deemed unconstitutional. Sets the terms by which majoritarian legislation is evaluated against claimed fundamental rights. Derives institutional prestige, budgetary security, and structural independence from this role. The judiciary's interpretive pronouncements shape the policy landscape even when they rest on contested or evolved readings of constitutional text.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, beneficiary).

% Operates under permanent subordination to judicial constitutional interpretation. Any statute can be voided if the judiciary determines it conflicts with its reading of constitutional rights. Must anticipate judicial review when drafting legislation, faces invalidation of settled laws when judicial readings shift, and lacks authoritative recourse short of constitutional amendment (itself subject to judicial narrowing). The legislature's democratic mandate is conditional on judicial constitutional blessing.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, generational, constrained, national).

% Electoral outcomes are subordinated to judicial constitutional interpretation. Majorities can enact policy preferences through representatives, but those outcomes remain subject to judicial nullification on rights grounds. When the judiciary interprets constitutional rights expansively, majoritarian preferences on issues like property regulation, personal conduct, or resource allocation are overridden. Reversal requires either constitutional amendment (high friction) or shift in judicial membership (slow, not electorally direct).
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% Gain protection against majoritarian legislation through judicial enforcement of constitutional rights as the judiciary reads them. Rights-bearing individuals or groups can overturn unfavorable laws via judicial action. The scope of protection depends entirely on the judiciary's interpretation of the constitutional text and on which rights the judiciary recognizes as fundamental. Rights-holders benefit from the constraint when their interests align with the judiciary's constitutional reading; they lose when they do not.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_rights_holders, beneficiary,
    moderate, biographical, mobile, national).

% Interests that cannot be framed as constitutional rights—public welfare, collective goods, distributional preferences of unorganized majorities—have no judicial standing and no protection from legislative override. They are subject to majoritarian legislative process but vulnerable to judicial nullification of legislation that protects them when that legislation conflicts with judicial readings of rights. They lack voice in both the judicial and legislative arenas.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, non_rights_articulated_interests, payer,
    powerless, immediate, trapped, national).

% Believe constitutional meaning is constructed through inter-branch dialogue and political contestation, not through judicial pronouncement. They would argue the judiciary should treat its own constitutional readings as provisional, deferring to legislative judgment on borderline questions and treating constitutional interpretation as a conversation rather than a verdict. They are excluded from the constraint's operation: judicial decisions override their preferred allocation of interpretive authority without their voice structuring the outcome.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, coordinate_construction_advocates, excluded,
    moderate, generational, constrained, national).

% Study and critique the judiciary's interpretive authority, its exercise, and its legitimacy grounds. Produce competing constitutional narratives that may influence future judicial readings or contribute to amendment pressure. Occupy an analytical seat: they measure whether the constraint is functioning as justified.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single authoritative locus for resolving disputes over the meaning of the constitution and the scope of fundamental rights; prevents constitutional meaning from being determinate only through political contestation and majoritarian override. Coordinates public expectations around a stable, judicially-articulated constitutional order rather than allowing constitutional meaning to shift with electoral cycles.
% TRANSFER_FUNCTION: Transfers interpretive authority from the electorate and its representatives to the judiciary; transfers authority to nullify policy outcomes from the majority to rights-bearing minorities recognized by judicial interpretation; transfers risks of legislative invalidity from the judiciary (none) to the legislature (constant) and from majorities to minorities whose interests align with judicial constitutional readings.
% ABSENT_VOICES: Advocates of coordinate constitutional construction (multiple legitimate interpreters in dialogue) are excluded from the constraint's operation—they would argue for inter-branch dialogue, but the supremacy reading forecloses that via unilateral judicial veto. Unorganized majorities with diffuse interests that cannot be framed as rights are also excluded—their interests have no protection mechanism within this constraint.
% DISAPPEARANCE_RATIONALE: If judicial interpretive supremacy vanished overnight, constitutional meaning would immediately become contestable in every legislative session; majorities could enact policies they claim are constitutional, legislatures could enforce their own constitutional readings, and constitutional meaning would drift with political power rather than rest on stable judicial pronouncement. The policy landscape would reorganize around electoral rather than judicial outcomes.
% FOUNDING_PROBLEM: Early constitutional governance relied on the constitution functioning as stable law binding all branches; but without an authoritative interpreter, each branch could claim its own reading was correct, rendering constitutional constraints unenforceable against majoritarian pressure. The judiciary's interpretive authority was established to solve this: one final voice that makes constitutional meaning determinative rather than negotiable.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and constitutional scholars in the originalist/textualist tradition attest the founding problem remains live: without judicial review, legislatures routinely violate constitutional constraints. Advocates of parliamentary supremacy and coordinate construction dispute this: they attest that the founding problem has been solved by democratic accountability and inter-branch negotiation, and the constraint now persists as judicial power preservation, not constitutional necessity. Legislative testimonies, political science literature analyzing democracies without robust judicial review, and international comparative constitutional law all provide corroboration outside the judiciary's direct interest.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.58 because the judiciary's interpretive power, while real and consequential, is legitimated through the genuine coordination function of establishing stable constitutional meaning—not through pure override. But that legitimation covers substantial extraction: the judiciary overrides electoral majorities on rights grounds, transfers authority to itself, and subordinates the legislature. Suppression runs higher (0.72) because the constraint's persistence depends on actively suppressing competing interpretive claims from the coordinate construction and parliamentary supremacy traditions—it requires enforcement (via judicial institutional independence, respect for precedent, and political support for judicial authority) that would collapse if those competing readings gained force. Theater ratio rises over time (0.25→0.42→0.42) because as the constraint matures, more of the judicial apparatus's work is devoted to justifying the interpretive supremacy itself (doctrinal elaboration, distinction-making, managing apparent inconsistencies in rights readings) rather than straightforwardly applying the constitution to new cases. The measurements capture a constraint that is genuinely coordinative (solves the interpretive-authority problem) and genuinely extractive (transfers power to the judiciary, subordinates democratic will), thus tangled_rope.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and rights-bearing minorities see this constraint as coordination (stable constitution, rights protection). The legislature and electoral majorities see it as extraction (democratic will subordinated, outcomes overrideable on contested rights grounds). From the judiciary's seat, the constraint is justified: without final interpretive authority, constitutional meaning would be contested and unenforceable. From the legislature's seat, the constraint is unjust: the judiciary uses rights language to preserve its own power, overriding democratic majorities on marginal constitutional questions. The engine computes per-seat classifications from the structural data (power, exit_options, directionality): the judiciary should compute toward beneficiary-side, the legislature and majorities toward target-side. The authored claim (tangled_rope) reflects this asymmetry—one seat (judiciary) benefits from coordination, one seat (legislature/majorities) pays extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary: d ≈ 0.1-0.2 (near full beneficiary). Institutional power (most powerful atom), generational horizon, analytical exit (they set the terms so 'exit' is not real for them—they can always reinterpret). They derive authority, prestige, budgetary independence, and effective policy veto from the constraint. Beneficiaries = true. Rights-bearing minorities: d ≈ 0.3-0.4 (mild beneficiary to mild target depending on whether their rights align with judicial readings). Moderate power, biographical horizon, mobile exit (can advocate for amendment, change their constitutional position). They benefit when their interests match judicial readings, pay when they do not. Legislature: d ≈ 0.75-0.85 (near full target). Institutional power but constrained by judicial veto, generational horizon, trapped exit (legislatures cannot exit the constitutional order). They pay through legislative nullification, subordination, and conditional authority. Electoral majorities: d ≈ 0.65-0.75 (strong target). Organized power, biographical horizon, constrained exit (electoral processes exist but outcomes remain subject to judicial veto). They pay through electoral override and majoritarian preference rejection. Unorganized majorities with non-rights interests: d ≈ 0.85-0.95 (full target). Powerless, immediate horizon, trapped exit. They pay through vulnerability to nullification and lack of voice. No directionality_overrides needed; structural derivation works.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint could be classified as snare if measured purely as extraction (judiciary coerces outcomes, suppresses competing interpretations, overrides electoral will). It is classified as tangled_rope because the coordination function—providing stable constitutional meaning that makes constitutional constraints binding rather than negotiable—is structurally genuine and non-eliminable. But mandatrophy risk is present: if the founding problem (how to establish stable constitutional meaning) becomes widely accepted as solved, while the constraint persists in extractive form (judiciary overrides will on contested rights), the constraint can drift toward snare classification. The six_questions record this risk: founding_problem_status = contested (some parties attest the problem is solved, others that it remains live). If consensus shifts to 'the problem is solved,' the constraint's legitimacy erodes and extraction becomes visible as pure override. This is not mandatrophy resolved; it is mandatrophy emergent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rights_recognition_scope_ambiguity,
    'Which interests count as constitutional rights worthy of judicial protection, and what is the source of that determination—the constitutional text, the judiciary''s evolved reading, or normative commitments outside the text?',
    'Historical and comparative analysis of which interests the judiciary has recognized as fundamental in different eras (privacy, property, economic liberty, social welfare); correlation with shifts in judicial membership and explicit doctrinal statements about the grounds of rights recognition.',
    'If rights are text-grounded and stable, the constraint functions as coordinate constitutional construction within a shared frame. If rights expand based on judicial policy preferences, the constraint is substantially extractive—the judiciary uses rights language as cover for majoritarian override. If rights are culturally determined and contestable, the constraint''s legitimacy depends entirely on whether the judiciary tracks public moral consensus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rights_recognition_scope_ambiguity, conceptual, 'Whether constitutional rights are discovered from text, evolved by the judiciary, or constructed through political contestation.').

omega_variable(
    coordinate_construction_boundary,
    'Is judicial supremacy structurally necessary for stable constitutional governance, or could coordinate construction (inter-branch dialogue with no single veto) achieve constitutional stability through negotiation and democratic accountability?',
    'Comparative institutional analysis: examination of democracies with coordinate construction (e.g., some Commonwealth systems, Scandinavian multi-branch review), their constitutional stability outcomes, amendment rates, and majoritarianism vs. rights-protection tradeoffs.',
    'If coordinate construction produces comparable stability and rights protection, judicial supremacy is revealed as institutional power preservation, not constitutional necessity—extraction rises. If coordinate construction produces constitutional drift and majoritarian override of rights, judicial supremacy is vindicated as necessary—extraction reconfigures as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_construction_boundary, empirical, 'Whether constitutional interpretation can achieve stability through inter-branch dialogue rather than judicial veto.').

omega_variable(
    democratic_legitimacy_vs_rights_protection_tradeoff,
    'When electoral majorities and judicial interpretations of constitutional rights conflict, which should prevail—and on what normative grounds?',
    'Normative political theory and democratic theory literatures; empirical examination of public opinion on specific rights (abortion, property, speech, religion) vs. judicial outcomes; assessment of how people experience the constraint—as rights protection or majoritarian override.',
    'If rights protection outweighs majoritarian will on most issues, the constraint is beneficiary-sided coordination. If majorities consistently oppose judicial outcomes, the constraint is extractive—the judiciary is overriding legitimate democratic will. The balance determines whether the constraint is best classified as coordination or pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_rights_protection_tradeoff, preference, 'Normative weight placed on electoral democratic will vs. constitutional rights as interpreted judicially.').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is ONE reading (judicial supremacy) of a contested kernel (constitutional interpretive authority). What structural elements would differ if the coordinate construction reading or parliamentary supremacy reading were instantiated instead?',
    'Comparative analysis of the three sibling readings: who enters/exits the beneficiary set, what extraction becomes visible or invisible, what suppression mechanism operates, and what rights-to-protection outcomes result under each reading''s operating principles.',
    'This omega documents that the classification (tangled_rope, snare, rope) is reading-indexed, not topic-indexed. Different readings of the same kernel produce different structural data, different ε values, and potentially different classifications. The constraint''s type is not discovered; it is instantiated by which reading is adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint instantiates the judicial supremacy reading of the constitutional interpretive authority kernel; sibling readings produce structurally different constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cons_tr_t0, projected).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(cons_tr_t50, observed).
narrative_ontology:measurement(cons_tr_t60, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(cons_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cons_be_t0, projected).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 50, 0.59).
narrative_ontology:measurement_basis(cons_be_t50, observed).
narrative_ontology:measurement(cons_be_t60, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(cons_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cons_su_t0, projected).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(cons_su_t50, observed).
narrative_ontology:measurement(cons_su_t60, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(cons_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading (judicial supremacy) of the constitutional_interpretive_authority kernel. All three readings (judicial_supremacy, coordinate_construction, parliamentary_supremacy) share the same referent—who interprets the constitution finally—but instantiate structurally different constraints with different beneficiaries, victims, suppression mechanisms, and ε values. They are related via network.affects_constraints rather than merged into one story; the kernel is a FAMILY of three constraints, one per reading. The judicial supremacy reading influence the others: if courts successfully entrench supremacy, they constrain coordinate construction (by making inter-branch negotiation legally overrideable) and foreclose parliamentary supremacy (by invalidating legislative claims to final authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
