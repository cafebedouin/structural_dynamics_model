% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Presidency Read as Parliament-Constrained Coordinated Executive
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   This story instantiates the parliamentary-constraint reading of the Fifth
 *   Republic kernel: the President is understood as a coordinated executive
 *   whose policy program only becomes effective law through Assembly
 *   authorization, and whose government answers to Assembly confidence. On
 *   this reading extraction is low — the constraint mostly performs genuine
 *   coordination, preventing unchecked executive concentration — but the
 *   President enters the victim set specifically in the scenario where a
 *   hostile or non-cooperative Assembly majority withholds confidence or
 *   blocks legislation, at which point the formal head of state finds
 *   substantive policy authority transferred to the Assembly majority and its
 *   Prime Minister. This is a different constraint from the
 *   hyper-presidential reading (which treats the same text as licensing
 *   near-unconstrained presidential sovereignty) and from the
 *   cohabitation-equilibrium reading (which treats presidential-prime
 *   ministerial authority as continuously negotiated rather than gated by a
 *   discrete authorization requirement) — each reading is authored separately
 *   with its own epsilon per the epsilon-invariance principle, linked here
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - national_assembly_majority: institutional beneficiary/agenda_setter — holds the authorization gate
 *   - president_when_assembly_withholds_confidence: powerful payer under hostile-majority conditions — constrained exit via dissolution
 *   - prime_minister_and_cabinet: powerful payer/agenda_setter — serves at Assembly's confidence
 *   - opposition_deputies: excluded — objects to the reading's completeness
 *   - citizens_and_electorate: organized beneficiary — benefits from distributed rather than unilateral authority
 *   - constitutional_council: institutional observer — adjudicates which reading governs a given dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.22).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.28).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Presidency Read as Parliament-Constrained Coordinated Executive").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '29331c9f-e7a7-4d62-8006-8c92446986ee').
narrative_ontology:cs_kernel_codification('29331c9f-e7a7-4d62-8006-8c92446986ee', formalized).
narrative_ontology:cs_authority_grounding('29331c9f-e7a7-4d62-8006-8c92446986ee', lineage).
narrative_ontology:cs_interpretation_layer_present('29331c9f-e7a7-4d62-8006-8c92446986ee').
narrative_ontology:cs_reading_relation('29331c9f-e7a7-4d62-8006-8c92446986ee', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('29331c9f-e7a7-4d62-8006-8c92446986ee', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('29331c9f-e7a7-4d62-8006-8c92446986ee', foundational, legislative_authorization_is_binding_gate).
narrative_ontology:cs_axiom_status(legislative_authorization_is_binding_gate, holdable).
narrative_ontology:cs_axiom_grounding('29331c9f-e7a7-4d62-8006-8c92446986ee', legislative_authorization_is_binding_gate, conventional).
narrative_ontology:cs_axiom('29331c9f-e7a7-4d62-8006-8c92446986ee', secondary, confidence_withdrawal_overrides_presidential_mandate).
narrative_ontology:cs_axiom_status(confidence_withdrawal_overrides_presidential_mandate, holdable).
narrative_ontology:cs_axiom_grounding('29331c9f-e7a7-4d62-8006-8c92446986ee', confidence_withdrawal_overrides_presidential_mandate, conventional).
narrative_ontology:cs_reference_frame('29331c9f-e7a7-4d62-8006-8c92446986ee', parliamentary_authorization_primacy).
narrative_ontology:cs_drift_state('29331c9f-e7a7-4d62-8006-8c92446986ee', post_1962_direct_election_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('29331c9f-e7a7-4d62-8006-8c92446986ee', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president_when_assembly_withholds_confidence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, citizens_and_electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_and_cabinet).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_sovereignty_over_policy_implementation).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, confidence_mechanism_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the votes to pass or block legislation the President's government needs to implement policy, and can withdraw confidence from the Prime Minister and cabinet. On this reading, the Assembly's authorization is the operative gate: no statute, no budget line, no implementation without its consent. It sets the practical agenda for what the executive is actually permitted to do.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, agenda_setter).

% Formally head of state and nominal director of national policy, but on this reading the President's program only becomes law through legislative cooperation. When the Assembly majority is hostile or withholds confidence from the government, presidential initiatives stall, are amended beyond recognition, or die in committee. Exit is constrained: dissolution is available but is a high-stakes gamble that can return an even less cooperative Assembly, and constitutional custom limits how often it can be used.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president_when_assembly_withholds_confidence, payer,
    powerful, biographical, constrained, national).

% Serves at the sufferance of the Assembly majority under this reading; must maintain its confidence to remain in office and to get government bills through. Administers the day-to-day execution the Assembly has authorized, but can be dismissed by a successful motion of censure regardless of the President's wishes.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_and_cabinet, payer,
    powerful, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_and_cabinet, agenda_setter).

% Sit in the Assembly but outside the governing majority; can obstruct, amend, and publicize but cannot themselves authorize implementation. They would argue the reading understates how much unilateral latitude the executive retains through decree powers and Article 49.3, but their objection does not enter this reading's account of the ordinary legislative-authorization channel.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, opposition_deputies, excluded,
    organized, biographical, mobile, national).

% Benefit from a system in which policy implementation requires the consent of their elected representatives rather than resting solely on presidential will; can express displeasure with either branch through elections and thereby reshape the majority the executive must satisfy.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, citizens_and_electorate, beneficiary,
    organized, generational, mobile, national).

% Adjudicates disputes over whether legislative authorization was properly obtained and whether presidential or governmental action exceeded its constitutional bounds; does not itself hold a stake in the outcome but its rulings determine which reading of the kernel prevails in a given case.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__parliamentary_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Requiring legislative authorization for policy implementation solves the problem of concentrating unchecked executive power in a single officeholder: it forces presidential initiatives through a body that must build and sustain a durable coalition, distributing the cost of policy error and preventing unilateral capture of the state apparatus.
% TRANSFER_FUNCTION: Moves effective agenda-setting power from the presidency to whichever coalition commands an Assembly majority; when that majority is hostile to the President, the transfer runs the other way from what the presidency's formal title suggests — policy initiative flows to the Assembly and its chosen Prime Minister.
% ABSENT_VOICES: Opposition deputies and minor parties would object that this reading overstates parliamentary control and ignores decree mechanisms (Article 49.3, ordinances) that let the executive bypass ordinary authorization; they are represented in the Assembly but their objection to the reading itself is not part of the reading's own account.
% DISAPPEARANCE_RATIONALE: If the legislative-authorization requirement vanished, the President could implement policy by decree without needing to build or maintain an Assembly majority; governments would no longer fall on confidence votes, cohabitation would become structurally impossible, and the entire coalition-bargaining apparatus that structures French party politics would lose its object.
% FOUNDING_PROBLEM: The instability of the Fourth Republic, where governments fell constantly and no executive could sustain a policy program, was to be solved by a strengthened presidency — but the 1958 constitutional settlement retained the principle that laws and budgets require Assembly passage and that governments require Assembly confidence, so the strengthened executive would still answer to a parliamentary majority for the substance of policy.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and comparative-government analysts outside any French governing coalition attest that the Assembly-authorization requirement remains operative and has bound presidents of varying ambition (e.g., cohabitation periods 1986-88, 1993-95, 1997-2002); proponents of the hyper-presidential reading, drawn largely from within recent presidencies and their allied majorities, dispute that this constraint is the dominant lived reality, arguing that a docile majority renders it inert in practice.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.22-0.30 across the interval) because under this reading the constraint's dominant function is genuine coordination: distributing policy authority so no single office captures the state. The extraction that does exist is concentrated precisely in cohabitation-adjacent moments (1986, 1993-95, 1997-2002) where a hostile Assembly effectively vetoes presidential initiative — reflected in the modest bump in the 1986 and 1997 measurement points. Suppression sits moderate (0.28-0.35): the constraint depends on real institutional mechanisms (confidence votes, legislative committee gates) that can be actively wielded against a president, but these are constitutionally bounded rather than coercive in the snare sense. Theater ratio stays low throughout (0.10-0.16): the authorization requirement is not performative — bills genuinely fail or pass on Assembly votes, and governments genuinely fall on censure motions.
 *
 * PERSPECTIVAL GAP:
 *   From the Assembly majority's seat, this arrangement is coordination working as designed: the body that must answer to the electorate for policy outcomes also controls whether those policies become law. From a President facing a hostile Assembly, the identical structure computes as an extraction of the mandate the presidential election was supposed to confer — the same text, read from the constrained seat, looks like the coordination story is cover for legislative capture of executive intent. The engine computes these divergent seat-level readings from the same structural data; this story does not adjudicate between them, only authors the parliamentary-constraint reading's own account.
 *
 * DIRECTIONALITY LOGIC:
 *   The National Assembly majority is the structural beneficiary on this reading: it collects the practical policy-setting authority the presidency's formal title suggests belongs elsewhere, and it can only be removed through elections it does not itself control the timing of entirely, giving it durable leverage. The President enters the victim/payer set specifically and only when the Assembly majority opposes the presidential program — this is a conditional directionality, not a constant one, which is why the victim group is named 'president_when_assembly_withholds_confidence' rather than 'the_presidency' unconditionally. Citizens benefit from the diffusion of authority across two elected loci rather than concentration in one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Fourth Republic government instability) is contested as live vs. dead: the confidence mechanism and legislative-authorization gate still function exactly as designed whenever an actual Assembly majority opposes the executive, which argues the mandate remains live rather than vestigial. The reading resists mandatrophy mislabeling in the direction of pure extraction — the mechanism is genuinely triggered by real votes, not merely alleged — while acknowledging (via omega) that a docile majority under a strong presidential coattail effect can render the constraint's active operation nearly invisible, which is what motivates the sibling hyper-presidential reading rather than refuting this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    docile_majority_dormancy,
    'When the Assembly majority is politically aligned with and deferential to the President (the modal case for most of the Fifth Republic outside cohabitation), is the legislative-authorization constraint genuinely operative but simply unexercised, or has it atrophied into a formality that a hostile majority alone reactivates?',
    'Compare roll-call independence and amendment rates of government bills across aligned-majority versus cohabitation periods; a constraint that only visibly binds during cohabitation but shows measurable legislative independence even under aligned majorities supports ''operative but unexercised'' rather than ''atrophied''.',
    'If the constraint is dormant rather than operative under aligned majorities, the low extraction and low theater_ratio authored here may understate a piton-like inertial component for most of the historical record, with the tangled_rope/snare character surfacing only episodically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(docile_majority_dormancy, empirical, 'Whether the authorization constraint is continuously operative or dormant absent a hostile majority.').

omega_variable(
    kernel_reading_selection_basis,
    'Which structural signals in the constitutional text and its 1958-2024 practice justify treating ''legislative authorization required'' as the dominant reading rather than ''presidential sovereignty minimally constrained'' (the hyper_presidential_reading) or ''continuously negotiated dual executive'' (the cohabitation_equilibrium_reading)?',
    'Track which reading''s predictions better fit the historical record of legislative defeat rates, censure motions, and Article 49.3 usage across the full 1958-2024 interval, disaggregated by cohabitation vs. non-cohabitation periods.',
    'If the hyper-presidential reading''s predictions fit non-cohabitation periods (the large majority of Fifth Republic history) better than this reading''s predictions, the parliamentary_constraint_reading may be the historically minority-case reading rather than the modal one, which would not change this story''s own epsilon (fixed by construction) but would bear on how much explanatory weight the reading carries relative to its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'The framing choice underlying which reading is treated as primary versus exceptional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(fift_tr_t1971, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1971, 0.11).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1986, 0.13).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1997, 0.14).
narrative_ontology:measurement(fift_tr_t2008, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2008, 0.16).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1958, 0.18).
narrative_ontology:measurement(fift_be_t1971, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1971, 0.2).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1986, 0.28).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1997, 0.3).
narrative_ontology:measurement(fift_be_t2008, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2008, 0.24).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1958, 0.32).
narrative_ontology:measurement(fift_su_t1971, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1971, 0.3).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1986, 0.35).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1997, 0.34).
narrative_ontology:measurement(fift_su_t2008, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2008, 0.29).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the single natural-language label 'the Fifth Republic constitution's executive structure' per the epsilon-invariance principle: this story (parliamentary_constraint_reading, low epsilon, Assembly majority as beneficiary, President as conditional victim), fifth_republic_constitution__hyper_presidential_reading (President as near-unconstrained sovereign, low epsilon from the presidency's own vantage but potentially high from an excluded-opposition vantage), and fifth_republic_constitution__cohabitation_equilibrium_reading (continuous negotiated authority-sharing rather than a discrete authorization gate). Each carries its own epsilon and stakeholder structure; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
