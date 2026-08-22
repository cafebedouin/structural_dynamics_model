% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Militia-Conditioned Second Amendment Boundary (Collective-Rights Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates the militia_conditioned_reading of the
 *   second_amendment_boundary kernel: the constitutional arrangement in which
 *   the prefatory 'well regulated Militia' clause defines the scope of the
 *   operative 'keep and bear Arms' clause, bounding the right to collective
 *   defense through organized militia service and leaving possession outside
 *   that context to ordinary democratic regulation. The epsilon referent is
 *   this standing arrangement — the militia-conditioned settlement itself —
 *   assessed by this reading's own lights, which endorse it; epsilon is
 *   therefore authored low-moderate (0.22 at interval end), reflecting
 *   burdens the reading holds democratically warranted plus a modest
 *   extractive residue. The colloquial 'Second Amendment debate' decomposes
 *   into three structurally distinct constraints sharing one text: this
 *   reading (regulatory authority presumed legitimate; victims are possessors
 *   whose claims fail), the individual_right_reading (victim set inverted —
 *   aspiring possessors become rights-holders and regulators the constrained
 *   party), and the insurrectionist_reading (beneficiary set inverted toward
 *   private armed capacity). Each is a separate constraint story with its own
 *   epsilon, beneficiaries, and victims, linked through
 *   network.affects_constraints; the disagreement between them is located
 *   entirely in what the prefatory clause does to the operative clause's
 *   scope.
 *
 * KEY AGENTS:
 *   - federal_appellate_courts: agenda-setting seat (institutional/constrained) — administers the boundary case by case, dismisses individual-possession claims, collects none of its gains
 *   - state_legislatures: primary beneficiary (institutional/constrained) — receive the protected regulatory space the boundary shields and write the regulations that give it practical effect
 *   - restricted_individual_gun_owners: primary target (organized/constrained) — bear restricted possession, denied permits, and lost claims
 *   - unorganized_militia_members: excluded seat (powerless/trapped) — nominally addressed by the prefatory clause, unprotected by the reading, no seat in the adjudication that defines their rights
 *   - second_amendment_scholars: analytical observer — maps the clause-structure question without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.22).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.32).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Militia-Conditioned Second Amendment Boundary (Collective-Rights Reading)").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '2c71bbd3-b128-4d3a-9206-56b90cc12a68').
narrative_ontology:cs_kernel_codification('2c71bbd3-b128-4d3a-9206-56b90cc12a68', fixed_text).
narrative_ontology:cs_authority_grounding('2c71bbd3-b128-4d3a-9206-56b90cc12a68', lineage).
narrative_ontology:cs_interpretation_layer_present('2c71bbd3-b128-4d3a-9206-56b90cc12a68').
narrative_ontology:cs_reading_relation('2c71bbd3-b128-4d3a-9206-56b90cc12a68', second_amendment_boundary__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('2c71bbd3-b128-4d3a-9206-56b90cc12a68', second_amendment_boundary__insurrectionist_reading, forecloses).
narrative_ontology:cs_axiom('2c71bbd3-b128-4d3a-9206-56b90cc12a68', foundational, prefatory_clause_is_scope_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_is_scope_limiting, holdable).
narrative_ontology:cs_axiom_grounding('2c71bbd3-b128-4d3a-9206-56b90cc12a68', prefatory_clause_is_scope_limiting, conventional).
narrative_ontology:cs_axiom('2c71bbd3-b128-4d3a-9206-56b90cc12a68', secondary, possession_outside_organized_militia_is_regulable).
narrative_ontology:cs_axiom_status(possession_outside_organized_militia_is_regulable, holdable).
narrative_ontology:cs_axiom_grounding('2c71bbd3-b128-4d3a-9206-56b90cc12a68', possession_outside_organized_militia_is_regulable, instrumental).
narrative_ontology:cs_reference_frame('2c71bbd3-b128-4d3a-9206-56b90cc12a68', miller_collective_rights_doctrine).
narrative_ontology:cs_drift_state('2c71bbd3-b128-4d3a-9206-56b90cc12a68', post_heller_supreme_court, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('2c71bbd3-b128-4d3a-9206-56b90cc12a68', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, municipal_firearms_regulators).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_health_governance_coalition).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, restricted_individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_collectors).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, gun_rights_litigation_organizations).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, collective_rights_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, miller_militia_relevance_test).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, state_police_power_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which firearm-possession claims survive constitutional review. Under this reading they hold that the prefatory militia clause defines the right's scope, so they dismiss individual-possession challenges and sustain state and local regulation as ordinary police-power legislation. They administer the boundary case by case but collect none of its gains; their institutional stake is doctrinal coherence and the authority to say what the text means. Exit from the arrangement would mean revising the doctrine itself, which they did in 2008.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, federal_appellate_courts, agenda_setter,
    institutional, generational, constrained, national).

% Enact and defend comprehensive firearm regulation — licensing, registration, prohibited-person categories, waiting periods — confident that courts will measure it against a militia-anchored right rather than an individual one. The boundary protects their regulatory space from constitutional challenge; they in turn write the regulations that give the boundary its practical effect. Exit would mean abandoning regulation they believe their constituents demand.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_legislatures, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, state_legislatures, agenda_setter).

% Cities and counties in high-regulation jurisdictions — permit districts, urban centers with concentrated violence — maintain the strictest regimes and rely on the boundary to keep them standing. Their regulatory identity and public-safety portfolios are built on the assumption that possession outside militia service is unprotected. A shift in the boundary dissolves their regulatory architecture case by case.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, municipal_firearms_regulators, beneficiary,
    institutional, biographical, constrained, local).

% Public-health agencies, violence-prevention researchers, and advocacy organizations whose policy program — traceability, licensing, capacity limits — requires constitutional room for comprehensive regulation. The boundary is the premise of their policy space; they supply the evidence base legislatures cite when defending regulations in court.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_health_governance_coalition, beneficiary,
    moderate, generational, constrained, national).

% Law-abiding owners whose possession, carry, or transfer is restricted by regimes the boundary sustains. They bear the costs as denied permits, banned categories, and lost claims in court. They are numerous and politically organized, but their exit runs through the same courts that administer the boundary — litigation, relocation to low-regulation states, or compliance.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, restricted_individual_gun_owners, payer,
    organized, biographical, constrained, national).

% Collectors of historically or mechanically significant arms whose holdings fall under registration bans, capacity rules, or transfer restrictions with no militia relevance. Under this reading their militia connection is nil and their claims fail. Their burden is property- and hobby-specific; exit is dispersal of collections or relocation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_collectors, payer,
    moderate, biographical, constrained, regional).

% Individuals in high-regulation jurisdictions who seek firearms for personal or home defense and are denied under licensing regimes the boundary sustains. Their claim — individual self-protection — is precisely the use this reading does not protect, since it bounds the right to collective defense through militia service. Their burden is immediate and personal; their recourse is compliance, movement, or a change in the reading itself.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants, payer,
    moderate, immediate, constrained, local).

% Advocacy organizations that litigate against the boundary on behalf of owners and fund the counter-doctrinal effort. They expend resources losing challenges under this reading; their organizational purpose is bound to overturning it, which keeps them inside the legal system whose boundary they contest. Unlike their individual members, their payment is strategic and chosen.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_rights_litigation_organizations, payer,
    organized, biographical, constrained, national).

% The body of citizens the prefatory clause nominally addresses — able-bodied residents not enrolled in any organized militia. Under this reading the amendment's protection attaches only through service in a well-regulated militia, and no mechanism enrolls them; they hold a right that never attaches to them. They would object that the reading strips the text of any operative protection for the unorganized, but they have no seat in the adjudication that defines their rights and cannot opt into a militia that does not accept them.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, unorganized_militia_members, excluded,
    powerless, generational, trapped, national).

% Constitutional scholars and historians who analyze the clause-structure question, the founding-era militia record, and the doctrinal consequences of each reading. They collect nothing and pay nothing; their seat is analytical — they map the structure the other seats occupy, and their work supplied the counter-doctrinal movement that ultimately displaced this reading.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, second_amendment_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__militia_conditioned_reading, state_legislatures).
narrative_ontology:fixing_cost_class(second_amendment_boundary__militia_conditioned_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles, for the legal system as a whole, who may decide firearm policy: by reading the right as bounded to organized militia service, the arrangement assigns firearm regulation to ordinary democratic institutions and gives legislatures, courts, and citizens one shared answer to the boundary question instead of case-by-case constitutional conflict over the text's meaning.
% TRANSFER_FUNCTION: Moves decision rights over firearm possession from individual claimants to state and local legislatures and regulators: each dismissed possession claim transfers discretion to the democratic institutions the boundary shields, and with it the practical capacity to condition, register, or prohibit possession.
% ABSENT_VOICES: Unorganized militia members — the citizens the prefatory clause nominally addresses — have no seat in the adjudication that defines their rights; they appear only as litigants losing. Also absent: the founding generation's own excluded classes (enslaved people and others barred from militia service), whose exclusion shaped the militia settlement the clause encodes and who are represented by no modern party to the dispute.
% DISAPPEARANCE_RATIONALE: The boundary is load-bearing for the firearms-regulatory state. If it vanished overnight — if the individual-right reading displaced it everywhere at once — the constitutional footing for comprehensive regulation would collapse, thousands of state and local provisions would become constitutionally vulnerable, licensing and registration architectures would unravel through litigation, and the firearms market and regulatory landscape would reorganize around a much narrower regulatory space. This is broadly what the post-2008 challenge wave in fact did, jurisdiction by jurisdiction.
% FOUNDING_PROBLEM: The founding-era republican problem the amendment's text was written to manage: securing the states' defense and the people's liberty against a professional standing army, through an armed citizenry organized as well-regulated state militias — and fixing in constitutional text the terms on which the new federal government could touch that arrangement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the boundary's beneficiary set: military-historical scholarship on the founding-era standing-army debates documents the problem and its terms, and the statutory record attests its supersession — the Militia Act of 1903 and successor law converted the state militias into a federally organized National Guard, and the United States has maintained a permanent professional military continuously since the Civil War era. No beneficiary of the modern boundary attests that the militia-defense problem remains live.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).
:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.22 because the reading's own lights endorse the arrangement: the burdens it authorizes (licensing, registration, prohibited categories) are held to be democratic police-power governance, and only the surplus — restrictions with no safety or militia rationale falling on collectors and non-militia possessors — counts as extraction beyond the reading's warrant. Suppression (0.32 at interval end) is doctrinal rather than physical: the boundary required courts to actively reject individual-right claims, an enforcement machinery that built from post-Miller dormancy (0.25) through uniform rejection of individual claims in the 1980s-1990s (peak 0.68) and then collapsed when the Supreme Court displaced the reading in 2008 — the suppression_requirement series is authored precisely because the story tracks enforcement-capacity change, not merely extraction drift. Theater is low throughout (0.18 to 0.14): when operative, the boundary was maintained by genuine adjudication, not performance. Accessibility collapse (0.55): within any single legal framework that adopts the reading, the sibling readings have no textual foothold, but the readings persist across parties — courts, scholars, and citizens held all three simultaneously in different seats for decades, and the framework flipped in 2008. Resistance (0.78): the reading met sustained, organized, ultimately victorious resistance — a counter-academic movement from the 1960s onward and a Supreme Court repudiation. Claim and metrics are independent: claimed_type tangled_rope rests on structure (a genuine coordination function — a single shared answer to who decides firearm policy — plus asymmetric costs concentrated on possessors, plus required enforcement), not on the metric profile. All series run on one shared time grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structural data. From the legislature seat the boundary is protected democratic space — it subsidizes governance those seats already wanted to exercise, and effective extraction is damped toward subsidy. From the restricted-owner seat the same boundary is foreclosure: possession claims fail, and exit runs only through the courts that administer the rule, relocation, or compliance. The court seat administers without collecting — its costs are docket and legitimacy, its gains doctrinal authority, placing it near symmetric. The excluded unorganized-militia seat experiences a right that never attaches: total burden (no protected possession at all) with the coordination benefits flowing elsewhere. Among same-level actors, the organized litigation organizations and the organized owner class hold the same power atom but different situations — the organizations pay in lost litigation as a chosen strategy whose purpose is overturning the boundary, while individual owners pay in denied possession as a condition of residence. One text, several constraint-experiences; the engine derives this divergence from the declared positions rather than averaging it away.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: state_legislatures, municipal_firearms_regulators, public_health_governance_coalition — each collects protected regulatory space without bearing the boundary's costs, placing them near the full-beneficiary end (low d, low or negative effective extraction; the municipal regulators' burden is jurisdiction-bound and their scope is local, which the engine reads in scope-scaling). Victims declared: restricted_individual_gun_owners, firearms_collectors, self_defense_claimants — each bears denied possession with constrained exit, placing them near the full-target end (high d, amplified extraction; the self-defense claimants' burden is immediate and local, the collectors' is property-bound and regional). The federal_appellate_courts administer but collect no gains — derived d sits near symmetric. The unorganized_militia_members seat is excluded rather than declared; their structural situation (total burden, no seat) is carried by the excluded stakeholder entry and the absent_voices answer. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation. No directionality overrides are used: the derivation from beneficiary/victim declarations plus exit options produces the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps both halves visible: the boundary genuinely solves a coordination problem (a single shared answer to who decides firearm policy, replacing case-by-case constitutional conflict over the text's meaning) while the same structure concentrates denied-possession costs on a minority with constrained exit. Reading it as pure rope would erase the victim set the structural delta names; reading it as pure snare would erase the real coordination function that constitutional settlement performs. The R5 interview adds the obsolescence dimension: the founding problem the arrangement encoded (militia-based defense against standing armies) is dead as a military matter, yet the arrangement persists — maintained after 2008 not by inertia but by live interpretive conviction in dissenting opinions, state constitutional law, and scholarship. That dead-founding-problem-plus-persistence signature is the mandatrophy-relevant pattern; theater stays low (0.14), so the maintenance is substantive argument rather than performance, and the constraint is not a piton — it is a contested reading kept alive by its holders' commitments, with the status-by-verdict mismatch (dead founding problem, world rearranges on disappearance) flagged for the capture/zombie cross-check rather than reconciled away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story is one reading (militia_conditioned_reading) of the second_amendment_boundary kernel; how would instantiating a sibling reading change the constraint''s structural data and classification?',
    'Generate the sibling readings (individual_right_reading, insurrectionist_reading) as separate constraint stories and compare computed classifications across the kernel family.',
    'Under the individual_right_reading the victim set inverts — aspiring possessors denied permits become rights-holders and regulators become the constrained party — and epsilon is re-authored over the individual-right arrangement; under the insurrectionist_reading the beneficiary set inverts toward private armed capacity. This story''s classification is valid only within its own committer structure; the siblings are different constraints, not measurement error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; siblings are separate constraints.').

omega_variable(
    prefatory_clause_force,
    'Where the readings disagree: does the prefatory ''well regulated Militia'' clause define the operative clause''s scope (this reading), merely announce a purpose without limiting it (individual_right_reading), or reorient the right toward resistance capacity (insurrectionist_reading) — and what textual or historical data could move a framework from one commitment to another?',
    'Clause-structure analysis (operative vs. prefatory grammar), founding-era drafting history of the militia clauses, and doctrinal treatment of prefatory language elsewhere in the Constitution.',
    'If the prefatory clause is scope-defining, the sibling readings lose their textual foothold and this reading''s regulatory permissiveness stands; if purpose-announcing, this reading''s entire structure collapses into the individual-right arrangement and the declared victim set dissolves. The disagreement is located entirely in the prefatory clause''s interpretive force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prefatory_clause_force, conceptual, 'Location of the kernel disagreement: the interpretive force of the prefatory clause.').

omega_variable(
    militia_relevance_boundary,
    'Does this reading''s regulatory permission extend to restrictions with no militia connection — collectors'' holdings, non-militia self-defense — or does the militia purpose itself bound what may be regulated?',
    'Doctrinal development under the reading: whether courts applying it require regulation to serve militia-relevant or public-safety ends, or treat all possession outside organized militia service as regulable at will.',
    'If regulation must be militia-relevant, epsilon rises — restrictions on collectors become extraction beyond the reading''s warrant and the victim set''s burden is partly unwarranted; if all non-militia possession is regulable, the permissiveness is comprehensive and the authored epsilon stands. Determines whether the declared victims bear warranted costs or surplus extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_relevance_boundary, conceptual, 'Whether the reading''s permissiveness is bounded by the militia purpose itself.').

omega_variable(
    miller_scope_underdetermination,
    'United States v. Miller (1939) affirmed on a record that never made the militia-relevance findings its own logic required — does the precedent actually establish this reading''s reference frame, or leave it underdetermined?',
    'Archival analysis of the Miller litigation record and doctrinal audit of how lower courts used Miller between 1939 and 2008.',
    'If Miller underdetermines the frame, the reference frame''s authority was thinner than its enforcement record suggested and the 2008 repudiation is less a reversal than a completion; if it establishes the collective-rights frame, the repudiation is a sharp break with settled doctrine. Affects the severity reading of the drift assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(miller_scope_underdetermination, empirical, 'Whether the controlling precedent establishes or merely gestures at the collective-rights frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 1939, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1939, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1939, 0.18).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1950, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1950, observed).
narrative_ontology:measurement(seco_tr_t1965, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1965, observed).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1980, observed).
narrative_ontology:measurement(seco_tr_t1994, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1994, 0.09).
narrative_ontology:measurement_basis(seco_tr_t1994, observed).
narrative_ontology:measurement(seco_tr_t2001, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement_basis(seco_tr_t2001, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2008, 0.14).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1939, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1939, 0.12).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1950, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1950, 0.13).
narrative_ontology:measurement_basis(seco_be_t1950, observed).
narrative_ontology:measurement(seco_be_t1965, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1965, 0.16).
narrative_ontology:measurement_basis(seco_be_t1965, observed).
narrative_ontology:measurement(seco_be_t1980, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement_basis(seco_be_t1980, observed).
narrative_ontology:measurement(seco_be_t1994, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1994, 0.26).
narrative_ontology:measurement_basis(seco_be_t1994, observed).
narrative_ontology:measurement(seco_be_t2001, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2001, 0.28).
narrative_ontology:measurement_basis(seco_be_t2001, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2008, 0.22).
narrative_ontology:measurement_basis(seco_be_t2008, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1939, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1939, 0.25).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t1950, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement_basis(seco_su_t1950, observed).
narrative_ontology:measurement(seco_su_t1965, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1965, 0.38).
narrative_ontology:measurement_basis(seco_su_t1965, observed).
narrative_ontology:measurement(seco_su_t1980, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement_basis(seco_su_t1980, observed).
narrative_ontology:measurement(seco_su_t1994, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1994, 0.66).
narrative_ontology:measurement_basis(seco_su_t1994, observed).
narrative_ontology:measurement(seco_su_t2001, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement_basis(seco_su_t2001, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2008, 0.32).
narrative_ontology:measurement_basis(seco_su_t2008, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, insurrectionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial 'Second Amendment debate' decomposes into three epsilon-invariant constraints sharing one kernel text, per the epsilon-invariance principle. This story authors epsilon over the militia-conditioned arrangement by its own lights (endorsed, hence low-moderate); the individual_right_reading story authors epsilon over the individual-right arrangement with the victim set inverted (regulators constrained, aspiring possessors as rights-holders); the insurrectionist_reading story authors epsilon over a resistance-capacity arrangement with the beneficiary set inverted toward private armed capacity. The structural link runs through shared text and shared doctrine: adoption of any one reading forecloses the others within a single legal framework while all three persist across parties, so each story links the others — the upstream story (the reading with the longest doctrinal tenure, this one) influences the downstream stories' legitimacy conditions without resolving the dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
