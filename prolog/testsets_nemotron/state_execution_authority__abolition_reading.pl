% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority — Abolition Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   The abolition reading of state execution authority holds that the state
 *   may never execute a person, regardless of the crime or the procedural
 *   safeguards. This reading rejects retribution and deterrence as legitimate
 *   justifications — they are cover stories for state-sanctioned killing.
 *   Every person executed (including the guilty) is a victim; the constraint
 *   extracts life itself with no substitution possible. Wrongful executions
 *   are not errors but proof of systemic illegitimacy. The constraint is
 *   actively enforced through capital statutes, appeals processes, and
 *   execution protocols. No beneficiaries exist in this reading — the state's
 *   claim to moral authority through execution is the extraction mechanism
 *   itself.
 *
 * KEY AGENTS:
 *   - executed_persons: Primary victims (powerless/trapped) — bear the ultimate extraction (life)
 *   - death_row_inmates: Primary victims (powerless/trapped) — live under active threat of extraction
 *   - wrongfully_convicted: Primary victims (powerless/trapped) — demonstrate systemic illegitimacy
 *   - families_of_executed: Secondary victims (powerless/constrained) — bear collateral extraction
 *   - state_prosecutors: Agenda setters (institutional/arbitrage) — wield execution as leverage and authority
 *   - victims_families_retributive: Excluded/beneficiary claimants (organized/constrained) — seek closure through state killing (rejected by this reading)
 *   - abolitionist_advocates: Observers (organized/analytical) — see full structure
 *   - supreme_court_justices: Agenda setters (institutional/arbitrage) — authorize and regulate the machinery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.95).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.92).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority — Abolition Reading").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, 'eb6835d3-0262-4226-9738-300004b20f36').
narrative_ontology:cs_kernel_codification('eb6835d3-0262-4226-9738-300004b20f36', formalized).
narrative_ontology:cs_authority_grounding('eb6835d3-0262-4226-9738-300004b20f36', lineage).
narrative_ontology:cs_interpretation_layer_present('eb6835d3-0262-4226-9738-300004b20f36').
narrative_ontology:cs_reading_relation('eb6835d3-0262-4226-9738-300004b20f36', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('eb6835d3-0262-4226-9738-300004b20f36', state_execution_authority__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('eb6835d3-0262-4226-9738-300004b20f36', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('eb6835d3-0262-4226-9738-300004b20f36', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('eb6835d3-0262-4226-9738-300004b20f36', foundational, retribution_deterrence_not_justifications).
narrative_ontology:cs_axiom_status(retribution_deterrence_not_justifications, holdable).
narrative_ontology:cs_axiom_grounding('eb6835d3-0262-4226-9738-300004b20f36', retribution_deterrence_not_justifications, deontological).
narrative_ontology:cs_reference_frame('eb6835d3-0262-4226-9738-300004b20f36', classical_punitive_authority).
narrative_ontology:cs_drift_state('eb6835d3-0262-4226-9738-300004b20f36', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eb6835d3-0262-4226-9738-300004b20f36', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, wrongfully_convicted).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, death_row_inmates).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_executed).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, right_to_life_absolute).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, state_killing_illegitimate).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, irreversibility_of_execution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons put to death by the state. They bear the ultimate extraction — their lives — with no possibility of restitution, appeal after the fact, or substitution. The constraint takes everything and returns nothing. Exit is impossible by definition.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, national).

% Persons sentenced to death awaiting execution. They live under the active, daily threat of state killing. Their situation is defined by the constraint's imminent enforcement. Exit requires legal intervention (clemency, exoneration, commutation) which the constraint's suppression machinery actively resists.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, death_row_inmates, payer,
    powerless, biographical, trapped, national).

% Persons executed despite factual innocence. Their existence is this reading's proof that the constraint's error rate is structural, not incidental. They bear the extraction plus the specific injustice of state killing of the innocent. No exit — the constraint has already taken their lives.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, wrongfully_convicted, payer,
    powerless, immediate, trapped, national).

% Family members of executed persons. They bear collateral extraction: grief, stigma, loss of kinship, often financial ruin from legal costs. Their situation is shaped by the constraint but they have no standing in its operation. Exit means leaving the jurisdiction or disengaging from the legal process — neither undoes the extraction.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_executed, payer,
    powerless, biographical, constrained, national).

% Prosecutors who seek death sentences. They wield the threat of execution as plea leverage, conviction insurance, and career capital. The constraint grants them coercive power over defendants. They benefit from the constraint's existence (d near beneficiary end) and can exit by declining to seek death — but institutional incentives reward its use.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_prosecutors, agenda_setter,
    institutional, biographical, arbitrage, national).

% Justices who authorize, regulate, and legitimate the machinery of execution through constitutional doctrine. They set the procedural rules that the constraint's suppression operates through. They benefit from the institutional authority the constraint reinforces. Exit means dissent — but the constraint persists regardless of individual votes.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, supreme_court_justices, agenda_setter,
    institutional, generational, arbitrage, national).

% Families of murder victims who seek execution as closure or justice. They claim beneficiary status — the constraint gives them what they say they need. This reading rejects that claim: their 'benefit' is the extraction itself, not a coordination return. They are excluded from the abolition reading's frame because their claimed benefit is the constraint's extraction mechanism. Exit means accepting LWOP or restorative processes — which many reject as insufficient.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, victims_families_retributive, excluded,
    organized, biographical, constrained, national).

% Advocates, lawyers, and organizations working to abolish state execution. They see the full structure: the extraction, the suppression, the lack of beneficiaries, the systemic illegitimacy. They do not collect from the constraint and do not pay into it — they work against it. Their exit is analytical: they can leave the field but the constraint persists.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__abolition_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This arrangement solves no coordination problem. It presents itself as solving 'justice for victims' or 'deterrence of capital crimes' but this reading rejects both as genuine coordination — they are the extraction's cover story.
% TRANSFER_FUNCTION: Moves life itself from executed persons to the state's claim of moral authority. The state gains the ultimate coercive credential (power over life and death); the executed lose everything. No other transfers occur — deterrence and retribution are not real transfers in this reading.
% ABSENT_VOICES: Executed persons (dead, cannot speak); wrongfully convicted who were executed (dead); future persons who will be executed under the constraint (not yet born). Victims' families who oppose execution (often excluded from victim-impact proceedings). International human rights bodies (excluded from domestic constitutional interpretation).
% DISAPPEARANCE_RATIONALE: If state execution vanished overnight, death rows would empty to LWOP, prosecutors would lose their ultimate plea leverage, the machinery of death qualification and execution protocols would be dismantled, and the state's claim to moral authority through killing would collapse. The criminal justice system would reorganize around incapacitation and (potentially) restorative models. The world rearranges because the constraint actively structures prosecutor power, appellate procedure, and the state's legitimacy narrative.
% FOUNDING_PROBLEM: The founding problem was the state's claim to legitimate authority over life and death as the ultimate expression of sovereign power — historically, the power to execute was the marker of sovereign legitimacy. In the modern framing, it was reframed as 'justice for victims of heinous crimes' and 'deterrence of the worst offenses.'
% FOUNDING_PROBLEM_CORROBORATION: The sovereign-authority founding problem is dead — modern states do not need execution to prove sovereignty (abolitionist democracies exist and function). The deterrence founding problem is empirically dead — decades of research show no deterrent effect beyond LWOP (National Research Council 2012, Donohue & Wolfers 2005, etc.). The retribution founding problem is contested — victims' families and some philosophers still claim it, but no corroborating source outside the beneficiary set (prosecutors, politicians, retributivist philosophers) attests it as a live problem requiring execution specifically. LWOP achieves incapacitation; restorative justice addresses victim needs without state killing.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.95, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is near-maximal (0.95) because the constraint takes life — the only asset with no substitute, no compensation, no recovery. Suppression is very high (0.92) because the machinery of execution (capital statutes, death qualification of juries, appeals limits, execution protocols) requires massive active enforcement to persist; alternatives (LWOP, restorative justice) are legally and politically suppressed. Theater ratio is low (0.12) because the performance (due process, proportionality review) is thin relative to the bare fact of killing — the constraint does not pretend to be coordination. Accessibility collapse is high (0.88) because once the state claims the power to kill, alternatives collapse: the logic of 'worst of the worst' expands, procedural protections erode, and the machinery becomes self-justifying. Resistance is moderate (0.45) — abolition movements exist but have not structurally dismantled the constraint in retentionist jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   From the abolition seat: pure snare — no coordination function, only extraction of life. From a retributive seat: the constraint would be tangled_rope — coordination of moral balance (beneficiary: victims' families, society's moral order) with extraction from the executed. From a deterrence seat: also tangled_rope — coordination of crime prevention (beneficiary: potential future victims) with extraction. The engine computes these per-seat types from the structural data; this reading authors the abolition seat's structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Executed persons and death row inmates are full targets (d ≈ 1.0) — the constraint extracts their lives with zero return. Wrongfully convicted are full targets plus systemic proof. Families of executed bear collateral extraction (d ≈ 0.8). State prosecutors and supreme court justices are agenda setters who benefit from the authority the constraint grants (d ≈ 0.15 — near beneficiary end). Victims' families seeking retribution claim beneficiary status but this reading rejects that claim — their 'benefit' is the extraction itself. No stakeholder in this reading occupies a genuine beneficiary seat (d < 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (responding to heinous crime) is live — heinous crimes still occur. But the arrangement built to solve it (state execution) has outlived any legitimate function: deterrence is empirically unsupported, retribution is rejected as legitimate, incapacitation is achieved by LWOP. The constraint persists through institutional inertia, political performance, and the self-justifying logic of 'worst of the worst.' This is mandatrophy: the mandate (justice for victims) has atrophied into its opposite (state killing as justice). The constraint is not a degraded rope (piton) — it is actively maintained, actively enforced, and actively extracts. It is a snare that presents itself as a rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''state_execution_authority'', specifically the abolition_reading, and how do sibling readings change the structural picture?',
    'Comparative analysis of the three declared readings (abolition_reading, retributive_reading, deterrence_reading) to map their distinct beneficiary/victim structures, ε values, and constraint types.',
    'If the kernel is correctly decomposed, each reading gets its own ε and type: abolition_reading is snare with no beneficiaries; retributive_reading and deterrence_reading are tangled_rope with beneficiaries (the state''s moral authority, victims'' families seeking closure) and victims (executed persons). Failure to decompose conflates structurally distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the abolition_reading of kernel state_execution_authority; sibling readings are retributive_reading and deterrence_reading.').

omega_variable(
    retribution_deterrence_legitimacy,
    'Can retributive or deterrence justifications be legitimate beneficiaries of the constraint, or does the abolition reading structurally reject them as cover for extraction?',
    'Examine whether any actor demonstrably benefits from state execution without bearing its costs — e.g., prosecutors gaining conviction leverage, politicians gaining electoral capital, victim families receiving state-sanctioned vengeance. If no net beneficiary exists, the constraint is a pure snare; if beneficiaries exist, it may be tangled_rope from their seat.',
    'If retribution/deterrence are rejected as justifications (this reading''s core axiom), the constraint has zero legitimate beneficiaries — pure snare. If they are accepted as real beneficiary interests, the constraint becomes tangled_rope from those seats. The engine computes per-seat types; this omega records the structural disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retribution_deterrence_legitimacy, conceptual, 'Whether retributive/deterrence functions constitute genuine coordination or are extraction cover.').

omega_variable(
    wrongful_execution_systemic_proof,
    'Does the occurrence of wrongful executions prove the entire system''s illegitimacy, or only its fallibility?',
    'Track exoneration rates, systemic error patterns, and whether post-conviction review structures can reliably prevent execution of the innocent. If error is inherent to the system''s design (cognitive bias, resource asymmetry, political pressure), wrongful execution is systemic proof; if errors are correctable outliers, the system may be reformable.',
    'If systemic illegitimacy is established, the constraint''s extraction is categorical — no procedural fix can make it acceptable. If only fallibility, a ''perfect procedure'' reading could theoretically exist (though this reading rejects it). This drives the ε value toward 0.95.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wrongful_execution_systemic_proof, empirical, 'Whether wrongful executions are systemic feature or correctable bug.').

omega_variable(
    life_imprisonment_substitutability,
    'Is life without parole a qualitatively different and acceptable substitute, or does it inherit the same structural illegitimacy?',
    'Analyze whether LWOP shares the irreversibility, state killing, and moral weight of execution. If LWOP allows for exoneration and release, it is structurally distinct; if LWOP functions as ''death by incarceration'' with identical error stakes, the substitution is illusory.',
    'If LWOP is a genuine substitute, the constraint''s extraction may be lower (coordination function preserved). If LWOP inherits the illegitimacy, the abolition reading''s ε remains near-maximal. This reading treats LWOP as qualitatively different — but that is contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(life_imprisonment_substitutability, conceptual, 'Whether life imprisonment is a valid coordination substitute for execution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 1972, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1972, state_execution_authority__abolition_reading, theater_ratio, 1972, 0.25).
narrative_ontology:measurement(stat_tr_t1976, state_execution_authority__abolition_reading, theater_ratio, 1976, 0.22).
narrative_ontology:measurement(stat_tr_t1980, state_execution_authority__abolition_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(stat_tr_t1990, state_execution_authority__abolition_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(stat_tr_t2000, state_execution_authority__abolition_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(stat_tr_t2010, state_execution_authority__abolition_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(stat_tr_t2020, state_execution_authority__abolition_reading, theater_ratio, 2020, 0.12).

% Extraction over time
narrative_ontology:measurement(stat_be_t1972, state_execution_authority__abolition_reading, base_extractiveness, 1972, 0.85).
narrative_ontology:measurement(stat_be_t1976, state_execution_authority__abolition_reading, base_extractiveness, 1976, 0.88).
narrative_ontology:measurement(stat_be_t1980, state_execution_authority__abolition_reading, base_extractiveness, 1980, 0.9).
narrative_ontology:measurement(stat_be_t1990, state_execution_authority__abolition_reading, base_extractiveness, 1990, 0.92).
narrative_ontology:measurement(stat_be_t2000, state_execution_authority__abolition_reading, base_extractiveness, 2000, 0.93).
narrative_ontology:measurement(stat_be_t2010, state_execution_authority__abolition_reading, base_extractiveness, 2010, 0.94).
narrative_ontology:measurement(stat_be_t2020, state_execution_authority__abolition_reading, base_extractiveness, 2020, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1972, state_execution_authority__abolition_reading, suppression_requirement, 1972, 0.7).
narrative_ontology:measurement(stat_su_t1976, state_execution_authority__abolition_reading, suppression_requirement, 1976, 0.75).
narrative_ontology:measurement(stat_su_t1980, state_execution_authority__abolition_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(stat_su_t1990, state_execution_authority__abolition_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(stat_su_t2000, state_execution_authority__abolition_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(stat_su_t2010, state_execution_authority__abolition_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(stat_su_t2020, state_execution_authority__abolition_reading, suppression_requirement, 2020, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__abolition_reading, 0.0).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% This constraint family (state_execution_authority) decomposes the single label 'death penalty' into three structurally distinct constraints. The abolition_reading has ε ≈ 0.95, no beneficiaries, type snare. The retributive_reading and deterrence_reading each have beneficiaries (moral order, future victims) and victims (executed persons), type tangled_rope. Their ε values are lower because they claim a coordination function. The kernel is the state's claim to execution authority; the readings are the constraints that claim instantiates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__abolition_reading, institutional, 0.15).
constraint_indexing:directionality_override(state_execution_authority__abolition_reading, powerless, 1.0).
constraint_indexing:directionality_override(state_execution_authority__abolition_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
