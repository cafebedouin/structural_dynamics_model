% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__coordinate_construction_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__coordinate_construction_reading
 *   human_readable: Dispersed Constitutional Interpretive Authority (Coordinate Construction)
 *   domain: constitutional law/political theory/jurisprudence
 *
 * SUMMARY:
 *   A constitutional order in which no branch holds final authority to say
 *   what the constitution means. Courts announce interpretations in concrete
 *   cases; the legislature answers with amendment, jurisdiction, and
 *   appropriations; the executive answers with appointments, enforcement
 *   choices, and official readings. Constitutional meaning is constructed
 *   through this continuing exchange, and interpretive contests are settled
 *   politically — by coalition-building, supermajorities, and endurance —
 *   rather than by any designated final arbiter. The arrangement tolerates
 *   interpretive instability as the standing price of dispersed authority.
 *   This story instantiates one reading of a contested kernel (see
 *   kernel_context); the claim/metrics split is deliberate: the arrangement
 *   is CLAIMED as tangled_rope from structural analysis, while the metrics
 *   are authored from its observed operation, including a rising
 *   enforcement-intensity series. KEY AGENTS (by structural relationship): -
 *   national_legislature: primary agenda-setter (institutional/constrained) —
 *   converts electoral durability into interpretive leverage through
 *   amendment, jurisdiction control, and the purse -
 *   constitutional_judiciary: beneficiary-administrator
 *   (institutional/generational) — announces binding-in-the-case
 *   interpretations with no guarantee of final acceptance - chief_executive:
 *   beneficiary-agenda-setter (institutional/biographical) — appointments,
 *   enforcement discretion, and official readings -
 *   durable_electoral_coalitions: secondary beneficiaries (organized/mobile)
 *   — convert victories into doctrinal movement, absorb losses episodically -
 *   politically_powerless_minorities: primary target (powerless/trapped) —
 *   claims must survive the political gauntlet with no guaranteed final forum
 *   - settlement_dependent_institutions: secondary target (organized/mobile)
 *   — price recurrent reinterpretation into long-horizon commitments -
 *   comparative_constitutional_scholars: analytical observer — sees the full
 *   cross-system pattern
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, 0.68).
domain_priors:suppression_score(constitutional_interpretive_authority__coordinate_construction_reading, 0.66).
domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__coordinate_construction_reading, "Dispersed Constitutional Interpretive Authority (Coordinate Construction)").
narrative_ontology:topic_domain(constitutional_interpretive_authority__coordinate_construction_reading, "constitutional law/political theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__coordinate_construction_reading, '7c0b1ea1-00b2-4fab-93ad-34528aed8310').
narrative_ontology:cs_kernel_codification('7c0b1ea1-00b2-4fab-93ad-34528aed8310', distributed).
narrative_ontology:cs_authority_grounding('7c0b1ea1-00b2-4fab-93ad-34528aed8310', practice).
narrative_ontology:cs_interpretation_layer_present('7c0b1ea1-00b2-4fab-93ad-34528aed8310').
narrative_ontology:cs_reading_relation('7c0b1ea1-00b2-4fab-93ad-34528aed8310', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('7c0b1ea1-00b2-4fab-93ad-34528aed8310', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_axiom('7c0b1ea1-00b2-4fab-93ad-34528aed8310', foundational, no_branch_final_interpretive_authority).
narrative_ontology:cs_axiom_status(no_branch_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('7c0b1ea1-00b2-4fab-93ad-34528aed8310', no_branch_final_interpretive_authority, deontological).
narrative_ontology:cs_axiom('7c0b1ea1-00b2-4fab-93ad-34528aed8310', secondary, inter_branch_dialogue_improves_interpretation).
narrative_ontology:cs_axiom_status(inter_branch_dialogue_improves_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('7c0b1ea1-00b2-4fab-93ad-34528aed8310', inter_branch_dialogue_improves_interpretation, empirically_contingent).
narrative_ontology:cs_reference_frame('7c0b1ea1-00b2-4fab-93ad-34528aed8310', coordinate_branch_dialogue_equilibrium).
narrative_ontology:cs_drift_state('7c0b1ea1-00b2-4fab-93ad-34528aed8310', contemporary_confirmation_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7c0b1ea1-00b2-4fab-93ad-34528aed8310', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, national_legislature).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, chief_executive).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, durable_electoral_coalitions).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, politically_powerless_minorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, settlement_dependent_institutions).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, checks_and_balances_theory).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, departmentalist_equal_standing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes statutes, initiates constitutional amendments, defines the courts' appellate jurisdiction, and holds the purse. When it disagrees with how another branch reads the constitution, it responds by amending the text, narrowing jurisdiction, withholding funding, or conditioning appointments. Durable electoral majorities convert repeated wins into lasting movement of constitutional meaning. Leaving the arrangement is not available short of revolution; its leverage inside the arrangement depends on staying in the contest.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, national_legislature, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, national_legislature, beneficiary).

% Decides concrete cases and announces what the constitutional text requires in them. Its rulings bind the parties and guide officials, but its readings can be answered by amendment, new appointments, jurisdiction changes, or non-cooperation by the other branches. Judges serve long terms, take a long view, and cannot resign the arrangement; their standing depends on continued acceptance of their role as one voice among several.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_judiciary, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_judiciary, agenda_setter).

% Nominates judges, enforces or declines to enforce rulings, issues directives interpreting ambiguous provisions for the administration, and bargains with the legislature over appointments and legislation. Its interpretive influence peaks with unified political support and ebbs when the other branches align against it. Term limits give it a shorter horizon than the courts.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, chief_executive, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, chief_executive, agenda_setter).

% Sustained voting blocs and the organizations that mobilize them. Each victory lets them place aligned interpreters and pass aligned statutes, moving constitutional meaning without owning any final arbiter; each loss hands the same tools to their opponents. Coalitions form, dissolve, and reform freely, so their exposure is episodic rather than permanent.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, durable_electoral_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Groups too small or too excluded to win elections, confirm appointments, or fund amendment campaigns. Their claims must survive the same political gauntlet everyone else's do, and history shows they frequently do not. They cannot exit the polity, and no institution is obliged to give their claims a final hearing. Whatever protection they receive rises and falls with the sympathies of whichever branch currently listens.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, politically_powerless_minorities, payer,
    powerless, generational, trapped, national).

% Banks, insurers, infrastructure operators, and long-contract parties whose planning requires knowing what the constitutional rules will be a decade out. Recurrent reinterpretation forces them to hedge, restructure, or price ambiguity into every commitment. Larger ones can shift activity across jurisdictions; smaller ones absorb the costs locally.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, settlement_dependent_institutions, payer,
    organized, generational, mobile, national).

% Academics and comparative researchers who document how authority-dispersed systems behave, advise drafters of new constitutions, and testify in reform debates. They observe the full pattern across countries and eras without holding a stake in any single branch's position.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__coordinate_construction_reading, national_legislature).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single institution from capturing constitutional meaning and entrenching itself: each branch holds a practical veto over the others' interpretive claims, so constitutional change requires cross-branch agreement or supermajority consensus, and no loser in a single controversy is permanently disarmed.
% TRANSFER_FUNCTION: Moves interpretive authority and agenda-setting leverage among the branches according to current political strength: electoral winners gain appointments and statutes, courts gain influence when their readings attract compliance, legislatures gain when they can amend or defund. The costs of unsettled meaning fall on those who need final answers — losing litigants, unprotected minorities, long-horizon planners.
% ABSENT_VOICES: Politically powerless minorities and unorganized citizens have no seat in the inter-branch conversation; the dialogue runs among branches, organized interests, and repeat players. They would object that a system with no guaranteed forum leaves their claims hostage to majority sympathy, and their objection registers only when some branch chooses to carry it for them.
% DISAPPEARANCE_RATIONALE: If the dispersion collapsed overnight — if one branch successfully claimed and exercised final interpretive authority — appointments, amendment campaigns, jurisdiction rules, and inter-branch bargaining would all reprice around that claim; the losing branches would reorganize their behavior around appeal to or resistance against the new final arbiter, and the current equilibrium of mutual vetoes would not survive intact.
% FOUNDING_PROBLEM: How to allocate authority over constitutional meaning among co-equal institutions without either letting one capture the constitution or producing paralysis — the anti-aggrandizement problem that dominated post-revolutionary design, sharpened by early crises over which branch speaks for the text.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the branches: founding-era records show the fear of institutional capture expressed across rival factions, including opponents of the eventual design, and comparative constitutional scholarship documents the same problem recurring wherever new constitutions allocate interpretive authority. No source outside the constitutional order altogether attests, and theorists committed to single-arbiter arrangements dispute whether dispersion solves the problem or merely relocates it into permanent political struggle.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__coordinate_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 at interval end) is substantial but bounded: the arrangement delivers real coordination value to every branch (mutual veto against capture), yet identifiable groups pay asymmetrically — minorities whose claims get no guaranteed final forum, and long-horizon actors who price recurrent reinterpretation into every commitment. Suppression (0.66) is authored as a raw structural property, unscaled by power or scope: it measures the enforcement machinery the dispersion now requires — confirmation warfare, jurisdiction bills, defiance brinkmanship — not any per-agent experienced coercion, which the engine scales separately. Theater (0.44) reflects a growing share of dialogue rhetoric functioning as legitimation over what is raw political bargaining, while the bargaining itself remains functionally real. Accessibility collapse is low (0.32): alternative allocations of final authority remain live, argued positions; understanding this arrangement does not eliminate its alternatives. Resistance (0.62) is structural — every periodic supremacy claim by a branch is resistance to the dispersion, and the series records the resulting enforcement escalations. Coordination type is enforcement_mechanism: the arrangement is a governance framework whose coordination product is stabilized mutual restraint, carrying the type's default floor. The three temporal series share one eight-point grid (1803–2026); the 1937 peak-and-relaxation in suppression_requirement records the court-packing confrontation and its negotiated retreat — a local cycle inside the long ratchet — and the base_properties scalars report the interval-end state, measured on the rising phase of the enforcement build-up. Receipt surface: the mechanisms that settle contests — amendment initiation, jurisdiction control, appropriations — run through the legislature, so recycled interpretive leverage lands there most systematically; branches and coalitions receive episodically, hence gain_flow names the legislative seat. Fixing is prohibitive: replacing the dispersion requires supermajority amendment or winning a capture struggle against the other branches' vetoes, a cost no sitting actor can carry relative to the benefit.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from the same structure. From the branch seats the arrangement presents as mutual insurance: each branch's veto protects it from the others' capture, and the dispersion reads as fair terms of coexistence. From the trapped minority seat the same structure presents as a locked door: no institution owes their claims a final hearing, and access depends on winning political sympathy they cannot command. Settlement-dependent institutions sit between — they pay real instability costs but retain partial exit through jurisdictional arbitrage. The engine computes these per-seat classifications from the declared power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The branches are declared beneficiaries with constrained exit: the dispersion subsidizes each of them relative to any single-arbiter world in which two of the three would be subordinated, so their derived directionality sits near the beneficiary end. Durable electoral coalitions are beneficiaries with mobile exit — coalitions reform freely — placing them nearest the subsidy end, though their exposure is episodic. Politically powerless minorities are declared victims with trapped exit: no forum guarantee, no exit from the polity, no coalition path that reliably wins — their derived directionality approaches the full-target end. Settlement-dependent institutions are victims but partially mobile, giving them intermediate-high directionality below the minorities'. No directionality overrides are needed: exit differentiation already separates the two victim classes, and the three branch seats share a genuinely symmetric beneficiary position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating interpretive authority without capture or paralysis — remains live, so no mandatrophy is declared; the R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds consistency, no zombie flag. The tangled_rope claim earns its keep against two mislabelings: reading the arrangement as pure coordination erases the identifiable payers — minorities denied a guaranteed forum are not diffuse overhead but a named class bearing the arrangement's costs; reading it as pure extraction erases the genuine coordination function — mutual veto against capture is a real collective good every branch consumes. The hybrid classification holds both facts without averaging them away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_relative_beneficiary_structure,
    'This story instantiates the coordinate_construction_reading of the constitutional_interpretive_authority kernel; would instantiating a sibling reading (judicial or parliamentary supremacy) restructure the beneficiary and victim sets over the same institutional terrain?',
    'Compile the sibling stories and compare per-seat classifications across readings; divergent victim sets locate the disagreement in the forum-guarantee element rather than in the dispersion mechanics themselves.',
    'Under a final-rights-guardian reading the powerless-minority seat plausibly flips toward protected beneficiary and the legislature toward bearing the costs; the coordinate reading''s victim array is therefore reading-indexed, not terrain-indexed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relative_beneficiary_structure, conceptual, 'Committer routing: the beneficiary/victim structure declared here is a property of this reading, not of the shared institutional terrain.').

omega_variable(
    minority_outcome_parity,
    'Do politically powerless minorities actually secure worse outcomes under dispersed interpretive authority than they would under a final rights-guardian court?',
    'Cross-national comparison of rights-protective outcomes for discrete minorities in authority-dispersed versus court-supremacist systems, controlling for wealth and democracy measures.',
    'Outcome parity would thin the victim declaration and push the arrangement toward a purer coordination classification; large persistent gaps would confirm the payer seat and sustain the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_outcome_parity, empirical, 'Whether the minority victim declaration survives comparative outcome testing.').

omega_variable(
    instability_cost_incidence,
    'Who concretely bears the costs of interpretive instability, and are they concentrated enough to constitute an identifiable paying group?',
    'Measure litigation volume, hedging behavior, and planning-horizon contraction in sectors exposed to recurring reinterpretation, versus sectors insulated by settled doctrine.',
    'If instability costs prove broadly diffused rather than concentrated, the settlement_dependent_institutions victim entry weakens and the effective burden concentrates on the minority seat alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instability_cost_incidence, empirical, 'Concentration versus diffusion of interpretive-instability costs across actor classes.').

omega_variable(
    enforcement_ratchet_or_cycle,
    'Is the rising enforcement-intensity series a one-way ratchet toward hardened inter-branch combat, or a crisis-driven cycle that relaxes after each confrontation?',
    'Extend the series past the current confrontation peak and test whether confirmation-war intensity, jurisdiction bills, and defiance episodes decay after institutional turnover.',
    'A ratchet supports drift toward harder extraction dynamics; a cycle indicates the end-state scalars overstate steady-state suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ratchet_or_cycle, empirical, 'Ratchet versus cycle in the enforcement machinery sustaining the dispersion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__coordinate_construction_reading, 1803, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1803, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 1803, 0.15).
narrative_ontology:measurement(cons_tr_t1857, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 1857, 0.22).
narrative_ontology:measurement(cons_tr_t1937, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement(cons_tr_t1954, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 1954, 0.18).
narrative_ontology:measurement(cons_tr_t1989, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 1989, 0.22).
narrative_ontology:measurement(cons_tr_t2000, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(cons_tr_t2016, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 2016, 0.38).
narrative_ontology:measurement(cons_tr_t2026, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 2026, 0.44).

% Extraction over time
narrative_ontology:measurement(cons_be_t1803, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 1803, 0.42).
narrative_ontology:measurement(cons_be_t1857, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 1857, 0.55).
narrative_ontology:measurement(cons_be_t1937, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 1937, 0.5).
narrative_ontology:measurement(cons_be_t1954, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 1954, 0.48).
narrative_ontology:measurement(cons_be_t1989, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 1989, 0.52).
narrative_ontology:measurement(cons_be_t2000, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(cons_be_t2016, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 2016, 0.64).
narrative_ontology:measurement(cons_be_t2026, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1803, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 1803, 0.2).
narrative_ontology:measurement(cons_su_t1857, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 1857, 0.32).
narrative_ontology:measurement(cons_su_t1937, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 1937, 0.48).
narrative_ontology:measurement(cons_su_t1954, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 1954, 0.4).
narrative_ontology:measurement(cons_su_t1989, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 1989, 0.46).
narrative_ontology:measurement(cons_su_t2000, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement(cons_su_t2016, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement(cons_su_t2026, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 2026, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, parliamentary_supremacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who interprets the constitution' decomposes into three structurally distinct constraints — one per reading of the constitutional_interpretive_authority kernel. This story is the coordinate-construction member; judicial_supremacy_reading and parliamentary_supremacy_reading instantiate the other members, each with its own epsilon, beneficiary/victim sets, and classification. The member a given polity actually operationalizes supplies the legitimacy conditions the others argue against; the edges recorded here declare family membership for contamination-propagation analysis across the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
