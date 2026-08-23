% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__living_constitutionalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__living_constitutionalist_reading
 *   human_readable: Living-Constitutionalist Reading: Constitutional Meaning Evolves With Social Values
 *   domain: constitutional law / legal theory / interpretive jurisprudence
 *
 * SUMMARY:
 *   The living-constitutionalist reading of constitutional text authority
 *   operates, as a matter of practice, as a rule licensing federal courts to
 *   determine constitutional meaning by reference to contemporary moral
 *   principles and prevailing social attitudes rather than the adopting
 *   generation's understanding. Instantiated as a governing arrangement, it
 *   concentrates final interpretive authority in an unelected bench with life
 *   tenure, admits unenumerated rights through evolving understanding, and
 *   produced the twentieth century's largest constitutional transformations,
 *   Brown v. Board foremost, without a single Article V amendment. The
 *   standing arrangement this story assesses is that interpretive regime as
 *   operated since roughly 1954. Per the committer-frame rules this file
 *   authors that one reading only: the originalist and positivist siblings
 *   are separate constraints in separate files, linked through network edges,
 *   with their own beneficiary structures and epsilon values. The claimed
 *   type and the authored metrics are independent facts: the claim records
 *   the structure I believe true, a genuine coordination service wrapped
 *   around a real transfer, while the metrics record how the regime has
 *   descriptively operated. KEY AGENTS (by structural relationship): -
 *   federal_judiciary: agenda-setter and collecting seat
 *   (institutional/identity_locked) — administers the reading and accrues the
 *   interpretive authority - evolving_standards_rights_claimants: primary
 *   beneficiary (moderate/constrained) — receive protections available only
 *   through evolving-meaning adjudication - national_preference_coalitions:
 *   secondary beneficiary (organized/mobile) — win nationwide change without
 *   clearing Article V, hedged across channels -
 *   state_legislative_majorities: primary target (organized/trapped) —
 *   statutes invalidated with no exit past the bench -
 *   fixed_meaning_constituencies: target (organized/constrained) — bedrock
 *   expectations overturned by successive updates -
 *   elected_political_branches: dual-positioned (institutional/constrained) —
 *   delegate explosive questions upward yet suffer preemption -
 *   future_generations_bound_by_judicial_settlement: excluded payer
 *   (powerless/trapped) — bound by settlements they never joined -
 *   constitutional_theorists: analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, 0.57).
domain_priors:suppression_score(constitutional_text_authority__living_constitutionalist_reading, 0.61).
domain_priors:theater_ratio(constitutional_text_authority__living_constitutionalist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, extractiveness, 0.57).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__living_constitutionalist_reading, "Living-Constitutionalist Reading: Constitutional Meaning Evolves With Social Values").
narrative_ontology:topic_domain(constitutional_text_authority__living_constitutionalist_reading, "constitutional law / legal theory / interpretive jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__living_constitutionalist_reading, '60034acc-6db5-40e0-bc60-3f616d9f5b92').
narrative_ontology:cs_kernel_codification('60034acc-6db5-40e0-bc60-3f616d9f5b92', fixed_text).
narrative_ontology:cs_authority_grounding('60034acc-6db5-40e0-bc60-3f616d9f5b92', practice).
narrative_ontology:cs_interpretation_layer_present('60034acc-6db5-40e0-bc60-3f616d9f5b92').
narrative_ontology:cs_reading_relation('60034acc-6db5-40e0-bc60-3f616d9f5b92', constitutional_text_authority__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('60034acc-6db5-40e0-bc60-3f616d9f5b92', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('60034acc-6db5-40e0-bc60-3f616d9f5b92', foundational, contemporary_moral_principles_determine_meaning).
narrative_ontology:cs_axiom_status(contemporary_moral_principles_determine_meaning, holdable).
narrative_ontology:cs_axiom_grounding('60034acc-6db5-40e0-bc60-3f616d9f5b92', contemporary_moral_principles_determine_meaning, instrumental).
narrative_ontology:cs_axiom('60034acc-6db5-40e0-bc60-3f616d9f5b92', secondary, unenumerated_rights_recognizable_through_evolving_understanding).
narrative_ontology:cs_axiom_status(unenumerated_rights_recognizable_through_evolving_understanding, holdable).
narrative_ontology:cs_axiom_grounding('60034acc-6db5-40e0-bc60-3f616d9f5b92', unenumerated_rights_recognizable_through_evolving_understanding, deontological).
narrative_ontology:cs_reference_frame('60034acc-6db5-40e0-bc60-3f616d9f5b92', living_charter_evolving_moral_understanding).
narrative_ontology:cs_drift_state('60034acc-6db5-40e0-bc60-3f616d9f5b92', contemporary_originalist_ascendance, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('60034acc-6db5-40e0-bc60-3f616d9f5b92', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, evolving_standards_rights_claimants).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, national_preference_coalitions).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, state_legislative_majorities).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, fixed_meaning_constituencies).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, future_generations_bound_by_judicial_settlement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, elected_political_branches).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, elected_political_branches).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, evolving_standards_of_decency).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, substantive_due_process).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, brown_equal_transformation_precedent).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, judicial_finality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nine unelected federal judges with life tenure decide what the constitutional text means in each generation. Every recognized unenumerated right and every updated reading enlarges the body of doctrine only they may revise. Their rulings bind every state and cannot be appealed past them; the external checks are appointments (slow and lottery-like), impeachment (rare), and jurisdiction-stripping (politically costly). Retirement is possible; abandoning the interpretive role altogether would dissolve the office's purpose.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary, beneficiary).

% Litigants whose claims succeed only if the text's meaning tracks present values: defendants invoking evolving decency standards, couples asserting liberties nowhere enumerated, groups pressing expanded equality readings. They obtain protections no legislature will grant them, but each protection lives at the pleasure of the next Court's composition; the same channel that recognized a liberty in 1973 closed it in 2022.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, evolving_standards_rights_claimants, beneficiary,
    moderate, biographical, constrained, national).

% National majorities on contested social questions who could never assemble two-thirds of Congress plus three-quarters of the states. When courts adopt their preferred reading they win nationwide change without winning Article V; when courts turn against them they lose a settlement they never formally owned. They hedge by litigating and legislating simultaneously, shifting effort to whichever channel currently yields.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, national_preference_coalitions, beneficiary,
    organized, biographical, mobile, national).

% State legislatures whose enactments on abortion, marriage, criminal sentencing, and regulation are invalidated by federal court readings of abstract clauses. They bear the drafting, litigation, and compliance costs of laws that die in court. Their exit is nil: federal supremacy forecloses ignoring adverse rulings, secession is unavailable, and the amendment threshold needed to overrule the Court is effectively unreachable.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, state_legislative_majorities, payer,
    organized, biographical, trapped, regional).

% Citizens and jurists committed to the view that the text means what its adopters understood it to mean. Each judicial update overturns expectations they held as constitutional bedrock. They respond through scholarship, appointment politics, and litigation, and currently hold enough institutional weight to have reversed a landmark settlement, but they cannot recover the decades in which readings they rejected governed.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, fixed_meaning_constituencies, payer,
    organized, generational, constrained, national).

% Congress and the president. They gain when courts absorb explosive questions, since each branch escapes accountability for settling them, and they lose when courts strike their statutes or narrow their powers. Their lever is appointment timing: a slow, stochastic instrument that reshapes readings only years later and never deterministically.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, elected_political_branches, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__living_constitutionalist_reading, elected_political_branches, beneficiary).

% People not yet born or enfranchised who will live under doctrinal settlements negotiated among present elites without their consent or participation. They bear the costs of whatever equilibrium the current interpretive contest produces and hold no seat in it; their only recourse is the same distant amendment process everyone else finds unreachable.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, future_generations_bound_by_judicial_settlement, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__living_constitutionalist_reading, future_generations_bound_by_judicial_settlement, payer).

% Scholars and comparativists who trace the reading's doctrinal consequences across systems, measure its costs and benefits, and testify in the interpretive conflict. They decide nothing and bear nothing; their analyses feed every other seat and the historical record.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text_authority__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles how an eighteenth-century charter of sparse abstractions governs a technological, continental society without convening two-thirds of both houses plus thirty-eight state legislatures for every change, and provides a single authoritative answer to disputed meaning so political conflict resolves instead of festering.
% TRANSFER_FUNCTION: Moves interpretive authority, and with it final policy-setting power on constitutional questions, from ratified public understanding and contemporary legislative majorities to the federal judiciary; concretely, moves decisions on abortion, marriage, school integration, and capital punishment from statehouses to appellate courtrooms.
% ABSENT_VOICES: The ratifying generation is dead and cannot object that its compromises are being reread; future generations live under settlements they never joined; state majorities appear only as losing litigants, never as co-authors, of the meanings that bind them.
% DISAPPEARANCE_RATIONALE: If evolving-meaning adjudication vanished overnight, either ratification-era understanding becomes binding and hundreds of doctrines built since 1954 lose their warrant and must be rebuilt or abandoned, or formal-enactment procedure alone validates constitutional claims and the Court's role shrinks to mechanical application; either path forces wholesale reconstruction of the interpretive settlement.
% FOUNDING_PROBLEM: Keep a short, abstract, deliberately hard-to-amend charter authoritative while letting it govern circumstances its drafters neither imagined nor built machinery for: industrialization, mass technology, transformed social norms.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting seats: the amendment-failure record (more than twelve thousand proposals introduced, twenty-seven ratified) attests the rigidity problem; comparative constitutional scholarship documents every long-lived written constitution confronting the same adaptation gap; even originalist jurists concede the text's abstraction leaves questions their own method strains to close. No corroborating source outside the beneficiary set denies the founding problem exists; the live dispute is over the solution, not the problem.
narrative_ontology:disappearance_verdict(constitutional_text_authority__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__living_constitutionalist_reading, 0.57, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.57: the regime transfers final decision authority on constitutional questions from legislative majorities and ratified public understanding to a bench no electorate controls, a large continuous transfer, but the transfer purchases a real service, adaptation of a charter whose amendment machinery is practically unusable, and distributes diffuse benefits broadly, holding extraction below predatory levels. Suppression 0.61: persistence rests on active machinery, judicial review, stare decisis entrenchment, and federal supremacy precluding state noncompliance, while interpretive alternatives remain intellectually alive, so suppression is coercively significant but not alternative-eliminating. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope. Theater 0.31: the doctrinal core is functional, but a growing share of activity is performative, reliance-interest rhetoric, humility tropes, and formulaic evolving-standards invocations that consolidate discretion while advertising restraint. Accessibility collapse 0.48: alternatives demonstrably survive comprehension, originalism currently commands a Court majority, so understanding the regime does not close the option space. Resistance 0.62: sustained scholarly opposition, appointment warfare, and a completed reversal in 2022 evidence real, occasionally victorious resistance. All measurements share one grid, points at t=0,10,20,35,50,60,70 with the unit mapping to calendar years minus 1954, and every tracked metric is authored at every point. The trajectories oscillate around a rising trend: expansion under sympathetic compositions, consolidation under hostile ones. The oscillation is driven by appointment cycles and performs a secondary maintenance function, since each swing advertises the bench's responsiveness and renews the discretion it exercises; the dips at t=35 and t=70 mark composition turns, not constraint relaxation. The suppression_requirement series is included because enforcement capacity is the traced dynamic here: stare decisis hardened through mid-interval, then partially decayed when the 2022 reversal demonstrated that settlements are reversible after all.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structure. From the bench, the reading is faithful stewardship of a charter the drafters themselves described in adaptable terms; from state capitols, the same doctrine is displacement, since statutes drafted, debated, and enacted die in an appellate courtroom the state cannot appeal past, lobby, or exit. Two nominally same-level seats diverge sharply: national preference coalitions (organized) hold mobile exit, arbitraging between judicial and legislative channels as each swings favorable, while state legislative majorities (also organized) hold trapped exit, because federal supremacy forecloses noncompliance and the amendment threshold is unreachable, so equal organizational standing buys opposite experiences. Elected political branches are genuinely dual-positioned: they shed accountability for explosive questions upward and lose statutory territory to the same mechanism. Rights claimants experience the regime as the only protection channel that ever worked for them and simultaneously as a channel that can be closed behind them, since 2022 converted a protected seat into an exposed one without any change in the underlying population. The engine computes these divergences from the structural asymmetries; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. federal_judiciary anchors the beneficiary pole: it collects the transferred authority, administers the rules, and is identity-locked into the role, placing d near 0.0. evolving_standards_rights_claimants sit mid-low: real gains received, but reversal exposure raises realized cost. national_preference_coalitions derive low-mid d with mobile exit damping their effective burden, since arbitrage converts would-be extraction into channel choice. elected_political_branches mix payer and beneficiary flows, and their partial appointment leverage moderates d below the trapped seats. state_legislative_majorities anchor the target pole: they bear invalidation costs with zero exit past the bench, and their regional scope concentrates incidence. fixed_meaning_constituencies sit near-target with repeated losses of bedrock expectations but constrained, nonzero exit through appointment politics. future_generations derive high d with no compensating flow and no seat, the purest target profile in the story. Coalition note for the trapped seats: state legislative majorities are numerous but coordinate poorly, each preferring that others fund challenge litigation, which keeps effective power below headcount. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms reproduce the relationships faithfully, and the two dual-positioned seats are handled by role pairing rather than numeric correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, governing transformed circumstances with a charter whose amendment machinery is practically unusable, remains live and is corroborated from outside the beneficiary set: the amendment-failure record, the comparative adaptation literature, and originalist concessions about textual abstraction all attest it. Classifying the arrangement as tangled_rope blocks both mislabels. Calling it rope would erase the documented transfer: interpretive finality demonstrably migrated from legislatures and ratified understanding to the bench, and state majorities demonstrably pay for it. Calling it snare would erase the coordination: the same structure resolves disputes peacefully that would otherwise recur as permanent constitutional crisis every generation, and its beneficiaries include populations with no other protection channel. The mandatrophy question is reserved: no sunset exists, none is appropriate, and the mandate has not outlived its function, though the omega founding_problem_liveness records the possibility that the adaptation rationale is becoming cover for settlement-monopoly maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel constitutional_text_authority, namely the living-constitutionalist reading. Which structural elements would the sibling readings (originalist_reading, positivist_reading) change?',
    'Cross-reading corpus comparison: compile the sibling stories and diff their beneficiary/victim sets, epsilon values, and computed types against this file.',
    'Sibling readings relocate the target seats entirely: the originalist sibling makes judicial updating itself the violation and ratification-era constituencies the protected class; the positivist sibling strips moral content from validity analysis altogether. Per-seat classifications computed here do not transfer across files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; siblings instantiate different constraints.').

omega_variable(
    judicial_authority_naturalness,
    'Is concentrating final interpretive authority in the judiciary a genuine adjudication requirement of any durable written constitution, or a self-serving expansion that any settlement mechanism would resist?',
    'Comparative institutional analysis: examine long-lived constitutions that locate interpretive settlement elsewhere (council-based review, referendum-triggered revision) and measure stability and rights-output differences.',
    'If adjudication genuinely requires final judicial settlement, part of the measured extraction is the irreducible price of coordination; if alternatives function, the concentration is discretionary rent and the classification shifts toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_authority_naturalness, empirical, 'Whether judicial finality is structurally necessary or self-assigned.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (judicial review, stare decisis, federal supremacy precluding noncompliance) or internalized (the legal-political culture treats judicial finality as the only conceivable settlement)?',
    'Post-reversal behavior tracing: the 2022 reversal demonstrated a half-century settlement could fall; if defiance litigation and nullification proposals multiplied afterward, internalization was weaker than assumed; if compliance remained automatic, structural mechanisms dominate.',
    'If internalization dominates, effective suppression exceeds the structural measure: targets comply before coercion is applied, and removing the enforcement machinery would not immediately restore alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of the suppression carrying the interpretive monopoly.').

omega_variable(
    authority_grounding_framing,
    'Is the reading''s authority best framed as practice (the accumulated interpretive tradition is the standard, common-law constitutionalism style), expertise (judges'' professional competence in applying moral principles), or lineage (continuity of ancient values through the text)?',
    'Conceptual: identify which framing the reading''s leading exponents treat as defeasible and observe which framing its defenders retreat to under challenge; recent defenses under repudiation pressure leaned on practice and precedent rather than competence.',
    'The practice framing supports interpretation_layer_present=true and lets drift absorb quietly; an expertise framing routes legitimacy through credential evaluation and makes drift visible as professional error; a lineage framing pulls the reading toward its originalist sibling and would soften the declared foreclosure edge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS-framing under-determination in the reading''s authority structure.').

omega_variable(
    beneficiary_seat_instability,
    'Are the evolving-standards beneficiary seats stable beneficiaries, or cohort-unstable seats that flip into paying positions when a successor Court revises the settlement, as 2022 flipped abortion-rights claimants from protected to exposed?',
    'Cohort tracing across Court compositions: measure whether the same claimant populations net-gain or net-lose across successive interpretive regimes spanning several decades.',
    'If seats flip on composition cycles, their derived directionality is phase-dependent and per-seat effective extraction for these agents oscillates with appointment timing rather than reflecting a fixed structural position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_seat_instability, empirical, 'Temporal instability of the beneficiary side''s directionality.').

omega_variable(
    founding_problem_liveness,
    'Is the founding problem, governing changed circumstances with a charter whose amendment machinery is practically unusable, still the operative justification, or has it become cover while the operative function is maintaining the judiciary''s settlement monopoly?',
    'Compare elite support for the reading on questions where updating would cut against judicial power (executive-power cases, immunity rollbacks) versus questions where it protects the bench; lopsided support indicates cover.',
    'If cover dominates, founding_problem_status should read dead and the arrangement persists through inertia and performance, a degraded lifecycle stage rather than a functioning hybrid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_liveness, preference, 'Whether the adaptation rationale remains the live mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__living_constitutionalist_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t35, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 35, 0.23).
narrative_ontology:measurement_basis(cons_tr_t35, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement_basis(cons_tr_t50, observed).
narrative_ontology:measurement(cons_tr_t60, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(cons_tr_t60, observed).
narrative_ontology:measurement(cons_tr_t70, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 70, 0.31).
narrative_ontology:measurement_basis(cons_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t35, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 35, 0.5).
narrative_ontology:measurement_basis(cons_be_t35, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(cons_be_t50, observed).
narrative_ontology:measurement(cons_be_t60, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 60, 0.59).
narrative_ontology:measurement_basis(cons_be_t60, observed).
narrative_ontology:measurement(cons_be_t70, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 70, 0.57).
narrative_ontology:measurement_basis(cons_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t35, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 35, 0.55).
narrative_ontology:measurement_basis(cons_su_t35, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement_basis(cons_su_t50, observed).
narrative_ontology:measurement(cons_su_t60, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement_basis(cons_su_t60, observed).
narrative_ontology:measurement(cons_su_t70, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 70, 0.61).
narrative_ontology:measurement_basis(cons_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% Epsilon-invariance decomposition of the colloquial label 'constitutional interpretation': the label conflates three structurally distinct claims about the same text's authority, authored as separate stories in one family. This file authors the living-constitutionalist reading alone, with its own epsilon (referent: the evolving-meaning interpretive regime as operated since Brown), its own beneficiary set, and its own victim set. The originalist sibling fixes meaning at ratification and reads the same doctrinal stock as deviation; the positivist sibling removes moral content from validity entirely. Each sibling cites the same settlement record, Brown foremost, to opposite conclusions, which is precisely why they are separate constraints rather than observables of one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
