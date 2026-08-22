% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine — Accountability Void Reading
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the accountability_void_reading of the
 *   qualified_immunity_doctrine kernel. The doctrine, judge-made in 1967 and
 *   radicalized in 1982, operates as a near-absolute bar to §1983 liability
 *   for law enforcement officers who violate constitutional rights. The
 *   'clearly established law' standard requires victims to identify a prior
 *   case with nearly identical facts — a structural impossibility for novel
 *   or context-specific violations. The result is a systematic extraction
 *   mechanism: the costs of constitutional violations are transferred from
 *   officers and municipalities to victims, the deterrent function of §1983
 *   is nullified, and policing practices calibrate to the immunity floor
 *   rather than the constitutional ceiling. The Court maintains the doctrine
 *   despite acknowledging it has no statutory basis, because it serves the
 *   institutional interests of the judiciary (docket control, finality), law
 *   enforcement (operational autonomy), and municipal insurers (loss
 *   limitation). This reading treats the doctrine as a snare: pure extraction
 *   with a coordination cover story.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: Primary agenda_setter (institutional/analytical) — authors and maintains the doctrine
 *   - law_enforcement_officers: Primary beneficiary (organized/constrained) — receives near-absolute liability shield
 *   - police_unions: Secondary beneficiary (organized/mobile) — treats immunity as core political achievement
 *   - municipal_insurers: Tertiary beneficiary (powerful/arbitrage) — profits from claim filtration
 *   - constitutional_violation_victims: Primary payer (powerless/trapped) — bears full cost with no remedy
 *   - civil_rights_plaintiffs: Secondary payer/excluded (moderate/constrained) — faces structural dismissal
 *   - marginalized_communities: Tertiary payer/excluded (powerless/trapped) — experiences systemic downstream effects
 *   - state_legislatures: Excluded agenda_setter (institutional/constrained) — theoretically capable but politically locked out
 *   - congress: Excluded agenda_setter (institutional/analytical) — constitutionally authorized but inert
 *   - legal_academy: Observer (analytical/analytical) — produces critique that does not move power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.92).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.89).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine — Accountability Void Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, 'bc2a0197-c980-42ae-a69d-71727b174b23').
narrative_ontology:cs_kernel_codification('bc2a0197-c980-42ae-a69d-71727b174b23', fixed_text).
narrative_ontology:cs_authority_grounding('bc2a0197-c980-42ae-a69d-71727b174b23', lineage).
narrative_ontology:cs_interpretation_layer_present('bc2a0197-c980-42ae-a69d-71727b174b23').
narrative_ontology:cs_reading_relation('bc2a0197-c980-42ae-a69d-71727b174b23', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('bc2a0197-c980-42ae-a69d-71727b174b23', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('bc2a0197-c980-42ae-a69d-71727b174b23', foundational, clearly_established_standard_is_extraction_mechanism).
narrative_ontology:cs_axiom_status(clearly_established_standard_is_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('bc2a0197-c980-42ae-a69d-71727b174b23', clearly_established_standard_is_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('bc2a0197-c980-42ae-a69d-71727b174b23', foundational, judicial_fabrication_of_immunity_lacks_statutory_authorization).
narrative_ontology:cs_axiom_status(judicial_fabrication_of_immunity_lacks_statutory_authorization, holdable).
narrative_ontology:cs_axiom_grounding('bc2a0197-c980-42ae-a69d-71727b174b23', judicial_fabrication_of_immunity_lacks_statutory_authorization, deontological).
narrative_ontology:cs_axiom('bc2a0197-c980-42ae-a69d-71727b174b23', secondary, deterrence_justification_empirically_falsified).
narrative_ontology:cs_axiom_status(deterrence_justification_empirically_falsified, holdable).
narrative_ontology:cs_axiom_grounding('bc2a0197-c980-42ae-a69d-71727b174b23', deterrence_justification_empirically_falsified, empirically_contingent).
narrative_ontology:cs_reference_frame('bc2a0197-c980-42ae-a69d-71727b174b23', harlow_objective_immunity_framework).
narrative_ontology:cs_drift_state('bc2a0197-c980-42ae-a69d-71727b174b23', contemporary_qualified_immunity_regime, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('bc2a0197-c980-42ae-a69d-71727b174b23', '2026-08-14T14:22:17Z').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, police_unions).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, municipal_insurers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_victims).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, marginalized_communities_subject_to_policing).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__accountability_void_reading, official_immunity_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and maintain the qualified immunity doctrine through precedent. Control the 'clearly established law' standard that determines when immunity applies. Their rulings are binding on all lower courts and effectively final for the vast majority of civil rights cases. They face no electoral accountability and serve for life.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, supreme_court_justices, agenda_setter,
    institutional, generational, analytical, national).

% Apply the Supreme Court's qualified immunity framework to individual cases. Must grant immunity unless plaintiffs can identify a prior case with nearly identical facts. Their discretion is narrow — the doctrine structurally compels dismissal in most novel-fact scenarios. Career advancement depends on collegial approval within the judicial hierarchy.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, lower_court_judges, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, lower_court_judges, observer).

% Receive near-absolute protection from personal liability for constitutional violations committed on duty. The doctrine shields them from financial consequences, disciplinary consequences tied to civil liability, and the deterrent effect of potential lawsuits. Their institutional culture treats qualified immunity as an entitlement enabling aggressive policing without legal risk.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    organized, biographical, constrained, local).

% Treat qualified immunity as a core contractual and political achievement. Lobby aggressively against legislative reform at state and federal levels. Use the doctrine's existence to negotiate indemnification provisions that shift all financial risk to municipalities. Their members' qualified immunity protection is a primary recruitment and retention selling point.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, police_unions, beneficiary,
    organized, generational, mobile, national).

% Benefit from the doctrine's filtration effect: qualified immunity dismisses the vast majority of §1983 claims before discovery, drastically reducing defense costs and settlement exposure. Their actuarial models price qualified immunity as a massive loss-reduction mechanism. They lobby to preserve the doctrine and oppose municipal self-insurance reforms that would internalize costs.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, municipal_insurers, beneficiary,
    powerful, biographical, arbitrage, national).

% Bear the full cost of constitutional violations — physical injury, psychological trauma, loss of liberty, financial ruin — with no remedy path. The 'clearly established' standard requires them to find a prior case with functionally identical facts, which is structurally impossible for novel or context-specific violations. Most cannot afford counsel; those who do face dismissal rates exceeding 80% at the motion-to-dismiss stage.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, constitutional_violation_victims, payer,
    powerless, immediate, trapped, local).

% Attempt to vindicate constitutional rights through §1983 litigation. Face a doctrinal gauntlet: qualified immunity at the motion-to-dismiss stage, Monell barriers to municipal liability, and appellate courts that treat qualified immunity as a threshold immunity rather than a defense. Their lawyers operate on contingency with negative expected value for all but the clearest cases. The doctrine selects against novel rights claims.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs, excluded).

% Experience the systemic downstream effects: policing practices calibrated to the immunity floor rather than the constitutional ceiling. Over-policing, excessive force, and discriminatory stops persist because officers internalize that constitutional violations carry no personal cost. Community trust erodes; legitimacy of law enforcement collapses; the social contract frays. No exit from the jurisdiction that polices them.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, marginalized_communities_subject_to_policing, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, marginalized_communities_subject_to_policing, excluded).

% Possess theoretical authority to abrogate qualified immunity through state-law causes of action (as Colorado and New Mexico have done). Face intense police union opposition, municipal cost-shifting fears, and federal preemption uncertainty. Most have not acted; those that have face immediate repeal campaigns. Their structural position is potential agenda-setters currently locked out by organized opposition.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, state_legislatures, excluded,
    institutional, biographical, constrained, regional).

% Possesses constitutional authority to abrogate or modify qualified immunity through legislation amending §1983. Has considered but never passed reform (e.g., Ending Qualified Immunity Act). Structural barriers: filibuster, police union lobbying, municipal liability fears, judicial supremacy norms. Could change the constraint overnight but has not.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, congress, excluded,
    institutional, generational, analytical, national).

% Produces the doctrinal critique establishing that qualified immunity has no basis in the text or history of §1983, was fabricated by the Court in 1967/1982, and operates as a policy choice disguised as legal interpretation. Their scholarship is cited in dissents but does not move the Court. They observe the constraint from outside the power structure that maintains it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, legal_academy, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally: balances the need for vigorous law enforcement against the need to vindicate constitutional rights by protecting officers from liability for reasonable mistakes. Actually: coordinates a system where constitutional violations are filtered out before adjudication, insulating the policing institution from the cost of its own misconduct.
% TRANSFER_FUNCTION: Transfers the cost of constitutional violations from the officers who commit them and the municipalities that employ them to the victims who suffer them. Moves financial risk, deterrent pressure, and accountability from the powerful (officers, departments, insurers) to the powerless (victims, marginalized communities). The transfer is near-total: over 80% of §1983 claims against individual officers are dismissed on qualified immunity grounds at the motion-to-dismiss stage.
% ABSENT_VOICES: The victims whose cases are dismissed before discovery — they never get to testify, never get a jury, never appear in the judicial record as anything but a dismissed complaint. Future victims whose rights will be violated because the deterrent signal was extinguished. State legislatures and Congress, structurally capable of acting but politically locked out. The 'clearly established' standard itself silences novel rights claims: if no prior case exists, the right cannot be 'clearly established,' so the violation cannot be remedied, so no precedent is created — a structural silence machine.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, §1983 litigation would proceed to discovery and trial on the merits in the vast majority of cases. Officers would face personal financial exposure; municipalities would face true cost internalization; insurers would price constitutional misconduct accurately; policing practices would shift toward constitutional compliance to avoid liability. The entire incentive structure of American policing would reorganize around actual constitutional requirements rather than the immunity floor. Victims would gain a functional remedy path. The doctrine is the linchpin of the current accountability architecture — remove it and the architecture collapses.
% FOUNDING_PROBLEM: The Supreme Court in Pierson v. Ray (1967) and Harlow v. Fitzgerald (1982) constructed qualified immunity as a judicial policy response to: (1) fear that §1983 litigation would deter vigorous law enforcement, (2) concern that officers would face ruinous personal liability for good-faith mistakes, (3) the Court's self-assigned role in 'balancing' competing policy interests absent congressional guidance. The founding problem was judicial — the Court perceived a policy vacuum and filled it with a judge-made immunity doctrine.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is dead per the Court's own subsequent jurisprudence and historical scholarship. Harlow eliminated the subjective good-faith inquiry, converting a qualified 'good faith' defense into an objective 'clearly established' standard — admitting the original problem (officer subjective state of mind) was not the real target. Historical scholarship (Baude, 'Is Qualified Immunity Unlawful?'; Schwartz, 'How Qualified Immunity Fails') demonstrates the doctrine has no basis in the text, history, or common-law background of §1983. The Court itself has acknowledged the doctrine is a 'balance' it struck, not a command of Congress. No corroborating source outside the benefiting parties (the Court, police unions, municipal insurers) attests that the founding problem persists — the deterrence fear is empirically unsupported (Schwartz, 'The Case Against Qualified Immunity'), and the good-faith mistake concern was solved by Harlow's objective test. The doctrine persists despite its founding problem being dead, not because of it.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.92 because the doctrine operates as a near-total filtration mechanism: the vast majority of constitutional violation claims are dismissed before discovery, transferring 100% of the cost to victims. The small residual of cases that survive (factually identical precedent exists) does not meaningfully reduce the extraction rate. Suppression is 0.89 because the constraint's persistence depends on active judicial enforcement — the Court must continuously police the 'clearly established' boundary, summarily reverse denials of immunity, and resist legislative abrogation. Theater ratio is only 0.12 because the doctrine's coordination function (protecting reasonable mistakes) was eviscerated by Harlow's conversion to an objective standard; what remains is almost purely extractive. Accessibility collapse is 0.78 (not higher) because state legislative abrogation (Colorado, New Mexico) and occasional circuit splits create narrow exit paths — but these are structurally fragile and cover a tiny fraction of the victim population. Resistance is 0.68 because the doctrine faces sustained scholarly critique, circuit judge dissent, state legislative action, and public opposition — but this resistance has not translated into doctrinal change at the Supreme Court level.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (Supreme Court justices, lower court judges) experience the constraint as a legitimate judicial balancing of policy interests — they see docket management, protection of government function, and institutional stability. The payer seats (victims, plaintiffs, marginalized communities) experience it as a structural denial of the constitutional remedy Congress enacted. The beneficiary seats (officers, unions, insurers) experience it as an earned protection enabling their work. The engine computes per-seat effective extraction from these structural positions: the payer seats see χ near ε (trapped, powerless, local scope); the beneficiary seats see χ near zero or negative (organized, mobile, national scope); the agenda-setter seats see χ modulated by institutional role (institutional power, analytical exit). The claimed type (snare) reflects the payer-seat reality; the Court's self-characterization would be rope or scaffold.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: law_enforcement_officers (direct liability shield), police_unions (political/organizational asset), municipal_insurers (actuarial windfall). Victims declared: constitutional_violation_victims (trapped, no remedy), civil_rights_plaintiffs (constrained, structural dismissal), marginalized_communities_subject_to_policing (trapped, systemic downstream harm). The directionality derivation flows from these declarations: officers have constrained exit (cannot leave policing without career loss) but collect massive benefit → d ≈ 0.15 (beneficiary). Victims are trapped, powerless, local scope → d ≈ 0.95 (full target). Municipal insurers have arbitrage exit (can exit markets, reprice) but collect benefit → d ≈ 0.10. Police unions have mobile exit (political influence across jurisdictions) → d ≈ 0.15. Supreme Court justices have analytical exit (lifetime tenure, no accountability) but are agenda-setters, not beneficiaries per se — their directionality derives from institutional role, not personal gain. The engine computes χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial concern about deterring vigorous policing) is dead — eliminated by the Court's own doctrinal evolution (Harlow's objective test) and empirical falsification (no evidence §1983 deters legitimate policing). The doctrine persists because it now serves the extraction interests of its beneficiaries (officers, unions, insurers) and the institutional interests of its agenda-setters (judicial finality, docket control). This is classic mandatrophy: a constraint whose original justification has evaporated but whose enforcement machinery has been captured by the parties who benefit from its extraction function. The Court's refusal to reconsider, despite explicit invitations from across the ideological spectrum, confirms the capture. The classification as snare (not tangled_rope) is warranted because the coordination function is vestigial — the 'clearly established' standard does not protect reasonable mistakes; it protects all mistakes for which no prior case exists. The coordination cover story is a Potemkin facade.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_framing,
    'Is the qualified immunity doctrine a single constraint with multiple readings, or are these structurally distinct constraints sharing a label?',
    'Apply the ε-invariance test: if the accountability_void_reading (ε≈0.92, snare) and protective_scaffold_reading (ε≈0.3, scaffold) produce different ε values for the same doctrinal machinery, they are different constraints. The test is whether the ''coordination function'' claimed by the scaffold reading is real (would persist without extraction) or a cover story. If the Court would maintain the doctrine even if it provided zero coordination benefit, the scaffold reading is false and the accountability_void_reading captures the true structure.',
    'If the readings are distinct constraints, each gets its own classification and the kernel is a linguistic confusion, not a structural ambiguity. If they are readings of one constraint, the ε-invariance principle is violated and the framework must account for observer-relative classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_framing, conceptual, 'Whether the kernel decomposes into multiple constraints per the ε-invariance principle.').

omega_variable(
    coordination_function_reality,
    'Does the qualified immunity doctrine genuinely coordinate a necessary function (protecting reasonable officers), or is the coordination function a cover story for extraction?',
    'Counterfactual: if the Court adopted a ''reasonable officer'' standard without the ''clearly established'' ratchet (as Harlow''s dissent urged), would officers be deterred from legitimate policing? Empirical evidence from pre-Harlow era and state systems without qualified immunity (Colorado post-2020) suggests no. If the coordination function is empirically false, the doctrine is pure snare.',
    'If coordination is a cover story, the claimed_type snare is structurally correct and the protective_scaffold_reading is a false framing. If coordination is real but extractive, the type is tangled_rope. The current metrics (extraction 0.92, theater 0.12) assume the former.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_reality, empirical, 'Whether the doctrine''s nominal coordination function is genuine or fabricated.').

omega_variable(
    state_abrogation_viability,
    'Can state-level abrogation of qualified immunity (Colorado, New Mexico, Connecticut) meaningfully reduce extraction for victims in those states, or does federal qualified immunity reassert itself through removal jurisdiction and §1983 preemption?',
    'Track §1983 filing outcomes in states that have enacted state-law causes of action with qualified immunity abrogation. Measure: (1) removal rate to federal court, (2) federal court application of federal qualified immunity to state claims, (3) verdict/settlement differential. If federal courts apply federal immunity to state claims via supplemental jurisdiction or preemption, state abrogation is structurally circumvented.',
    'If state abrogation is effective, accessibility_collapse is lower than 0.78 for victims in those states (creating a spatial scope split). If circumvented, the national constraint holds and state action is theatrical. This affects whether the constraint is a single national snare or a fragmented system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_abrogation_viability, empirical, 'Whether state legislative action creates genuine exit paths or is structurally nullified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_avr_tr_t1967, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(qi_avr_tr_t1982, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1982, 0.08).
narrative_ontology:measurement(qi_avr_tr_t1990, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(qi_avr_tr_t2001, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2001, 0.09).
narrative_ontology:measurement(qi_avr_tr_t2009, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2009, 0.1).
narrative_ontology:measurement(qi_avr_tr_t2018, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2018, 0.11).
narrative_ontology:measurement(qi_avr_tr_t2024, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(qi_avr_be_t1967, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(qi_avr_be_t1982, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1982, 0.72).
narrative_ontology:measurement(qi_avr_be_t1990, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(qi_avr_be_t2001, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2001, 0.84).
narrative_ontology:measurement(qi_avr_be_t2009, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2009, 0.87).
narrative_ontology:measurement(qi_avr_be_t2018, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2018, 0.9).
narrative_ontology:measurement(qi_avr_be_t2024, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(qi_avr_su_t1967, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(qi_avr_su_t1982, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1982, 0.75).
narrative_ontology:measurement(qi_avr_su_t1990, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(qi_avr_su_t2001, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2001, 0.83).
narrative_ontology:measurement(qi_avr_su_t2009, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2009, 0.85).
narrative_ontology:measurement(qi_avr_su_t2018, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2018, 0.87).
narrative_ontology:measurement(qi_avr_su_t2024, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 2024, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__accountability_void_reading, 0.1).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, municipal_liability_monell_barrier).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, police_union_collective_bargaining_shield).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, civil_rights_remedy_exhaustion_requirements).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, federal_court_abstention_doctrines).

% DUAL FORMULATION NOTE:
% This reading (accountability_void) and its siblings (protective_scaffold, constitutional_fidelity) form a constraint family decomposing the qualified_immunity_doctrine kernel. The accountability_void_reading has ε≈0.92 (snare); the protective_scaffold_reading would have ε≈0.30 (scaffold, coordination function genuine but transitional); the constitutional_fidelity_reading would have ε≈0.0 (mountain, the doctrine is illegitimate and should not exist — but as a constraint on judicial behavior, it extracts from the Court's legitimacy). The ε values differ by a wide margin because they measure different structural realities: the extraction machine as it operates, the coordination function as it is claimed, and the legitimacy deficit as it corrodes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, institutional, 0.25).
constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, organized, 0.15).
constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, powerful, 0.1).
constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, powerless, 0.95).
constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, moderate, 0.85).
constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
