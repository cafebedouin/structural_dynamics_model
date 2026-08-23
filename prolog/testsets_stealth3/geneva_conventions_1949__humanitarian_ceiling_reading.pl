% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949 — Humanitarian Ceiling Reading (Absolute Minimums Regardless of Reciprocity)
 *   domain: international law / armed conflict / political philosophy
 *
 * SUMMARY:
 *   The humanitarian ceiling reading takes the 1949 Geneva Conventions to
 *   establish minimums that bind a state's conduct of war unconditionally —
 *   they do not lapse when the adversary violates them, and they do not bend
 *   to operational necessity. Structurally the arrangement coordinates (a
 *   shared, legible line between lawful and unlawful violence that survives
 *   defection) while transferring asymmetrically (compliant state militaries
 *   pay in tactical freedom and force protection; protected civilians,
 *   detainees, and the wounded collect without reciprocal obligation; armed
 *   groups that reject the system entirely still collect its protections for
 *   their own people). Enforcement is active and growing: a monitoring
 *   mandate held by consent, a prosecutorial layer built since the 1990s, and
 *   domestic implementing statutes that make the floor criminal law inside
 *   complying states. This file is one reading of the geneva_conventions_1949
 *   kernel; the sibling readings are separate constraint files linked through
 *   the network section, and the reading-contest structure is carried in the
 *   omega variables. Claim and metrics are authored independently: the claim
 *   states the type believed structurally true of this reading; the metrics
 *   describe the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - complying_state_militaries: Primary target (institutional/constrained) — bears the compliance burden and the asymmetric cost of adversary non-reciprocity
 *   - protected_civilians_in_conflict_zones: Primary beneficiary (powerless/trapped) — protected status attaches wherever they stand
 *   - enemy_detainees_and_prisoners_of_war: Primary beneficiary (powerless/trapped) — hold enforceable treatment rights in the captor's hands
 *   - wounded_and_shipwrecked_combatants: Beneficiary (powerless/trapped) — care owed by whichever party finds them
 *   - non_reciprocating_armed_groups: Unearned-share collector (organized/mobile) — retains floor protections while externalizing every reciprocal duty
 *   - icrc_protection_mandate: Administrator (institutional/constrained) — holds visitation and tracing mandate by consent
 *   - international_criminal_tribunals: Enforcer (institutional/constrained) — prosecute grave breaches, dependent on state cooperation
 *   - security_advocates_in_defense_establishments: Excluded voice (powerful/constrained) — operational-necessity arguments barred from doctrinal legitimacy
 *   - domestic_publics_of_complying_states: Dual-positioned (organized/mobile) — bear indirect costs now, hold contingent insurance interest later
 *   - ihl_scholars_and_tribunal_monitors: Analytical observer (analytical/analytical) — sees text, enforcement record, and their divergence at once
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.55).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.58).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading (Absolute Minimums Regardless of Reciprocity)").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international law / armed conflict / political philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '6805598a-0368-4e08-bfab-4720ca818b62').
narrative_ontology:cs_kernel_codification('6805598a-0368-4e08-bfab-4720ca818b62', fixed_text).
narrative_ontology:cs_authority_grounding('6805598a-0368-4e08-bfab-4720ca818b62', lineage).
narrative_ontology:cs_interpretation_layer_present('6805598a-0368-4e08-bfab-4720ca818b62').
narrative_ontology:cs_reading_relation('6805598a-0368-4e08-bfab-4720ca818b62', geneva_conventions_1949__conditional_reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('6805598a-0368-4e08-bfab-4720ca818b62', geneva_conventions_1949__security_maximization_reading, forecloses).
narrative_ontology:cs_axiom('6805598a-0368-4e08-bfab-4720ca818b62', foundational, humanitarian_minimums_bind_regardless_of_reciprocity).
narrative_ontology:cs_axiom_status(humanitarian_minimums_bind_regardless_of_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('6805598a-0368-4e08-bfab-4720ca818b62', humanitarian_minimums_bind_regardless_of_reciprocity, deontological).
narrative_ontology:cs_axiom('6805598a-0368-4e08-bfab-4720ca818b62', foundational, operational_necessity_never_suspends_protected_status).
narrative_ontology:cs_axiom_status(operational_necessity_never_suspends_protected_status, holdable).
narrative_ontology:cs_axiom_grounding('6805598a-0368-4e08-bfab-4720ca818b62', operational_necessity_never_suspends_protected_status, deontological).
narrative_ontology:cs_axiom('6805598a-0368-4e08-bfab-4720ca818b62', secondary, unconditional_compliance_sustains_post_conflict_reconciliation).
narrative_ontology:cs_axiom_status(unconditional_compliance_sustains_post_conflict_reconciliation, holdable).
narrative_ontology:cs_axiom_grounding('6805598a-0368-4e08-bfab-4720ca818b62', unconditional_compliance_sustains_post_conflict_reconciliation, instrumental).
narrative_ontology:cs_reference_frame('6805598a-0368-4e08-bfab-4720ca818b62', reciprocity_independent_humanitarian_floor).
narrative_ontology:cs_drift_state('6805598a-0368-4e08-bfab-4720ca818b62', contemporary_asymmetric_conflicts, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6805598a-0368-4e08-bfab-4720ca818b62', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, protected_civilians_in_conflict_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, enemy_detainees_and_prisoners_of_war).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, wounded_and_shipwrecked_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, non_reciprocating_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, domestic_publics_of_complying_states).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, complying_state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, domestic_publics_of_complying_states).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, martens_clause).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, elementary_considerations_of_humanity).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, universal_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, command_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train, plan, and fight under the floor's minimums: lawyers sit in targeting cells, detainees are registered and visited, and tactics that would shorten the war — collective punishment, hostage-taking, reprisal against civilians — are off the table even when the adversary uses them. The formal door out (denunciation under the treaties' final articles) has never been used by any state, and no exit exists mid-conflict.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, complying_state_militaries, payer,
    institutional, generational, constrained, global).

% Live where the fighting happens. The floor forbids directing violence at them, obliges parties to spare and protect them, and does so regardless of which army controls their street. They did not agree to anything, cannot leave, and cannot bargain; everything they receive arrives as another party's duty.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, protected_civilians_in_conflict_zones, beneficiary,
    powerless, biographical, trapped, global).

% Once disarmed and in the captor's power, they acquire rights — humane treatment, registration, contact with a protecting body, a fair hearing before any sentence — that attach to their helplessness rather than to their cause or their conduct. The captor pays the full cost of these duties; the detainee pays nothing.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, enemy_detainees_and_prisoners_of_war, beneficiary,
    powerless, immediate, trapped, global).

% Are owed search, collection, and care by whichever party finds them, friend or enemy. The duty follows their condition, not their allegiance, and survives their own side's misconduct.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, wounded_and_shipwrecked_combatants, beneficiary,
    powerless, immediate, trapped, global).

% Fight from within civilian populations, blur the line the floor depends on, and in many cases deny every protection to the captives they take. Their own captured and wounded members nonetheless keep the floor's protections, and their non-compliance releases no one else from anything. What they give to the structure is, in most cases, nothing; what they take from it is protection for their people and cover for their methods.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, non_reciprocating_armed_groups, beneficiary,
    organized, biographical, mobile, regional).

% Holds the treaties' visiting, tracing, and intermediary mandates. Its method is presence and confidential persuasion: it enters places of detention by consent, registers captives, and remonstrates privately before ever speaking publicly. Its reach ends where a party's consent ends.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_protection_mandate, agenda_setter,
    institutional, civilizational, constrained, global).

% Prosecute grave breaches under ad hoc statutes, the permanent court's statute, and domestic universal-jurisdiction laws. They define which acts become enforceable outrages, but they cannot arrest: every case depends on some state surrendering a suspect or a battlefield collapsing into custody.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_criminal_tribunals, agenda_setter,
    institutional, generational, constrained, global).

% Officers, officials, and allied commentators who hold that asymmetric war punishes restraint and that operational necessity should bend parts of the floor. Under this reading their argument has no legitimate doctrinal home: it surfaces as reservations, interpretive statements, leaked memoranda, and internal dissent rather than as published doctrine.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, security_advocates_in_defense_establishments, excluded,
    powerful, biographical, constrained, national).

% Carry the indirect bill: longer campaigns, casualties absorbed rather than inflicted, and the political strain of fighting with a hand tied while footage shows the other side untied. The same publics also hold a stake in the order continuing — their own soldiers are future potential captives.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, domestic_publics_of_complying_states, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, domestic_publics_of_complying_states, beneficiary).

% Document the gap between official doctrine and field practice, publish commentaries, brief prosecutors, and litigate test cases. They see the whole structure at once: the text, the enforcement record, and the divergence between them.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, ihl_scholars_and_tribunal_monitors, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__humanitarian_ceiling_reading, non_reciprocating_armed_groups).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__humanitarian_ceiling_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a shared, legible line between lawful and unlawful violence operating between enemies who agree on nothing else: common categories of protected persons, common minimum treatment, common prohibitions that survive contact with an adversary who ignores them. The unconditional character is the load-bearing part — if restraint lapsed whenever the other side defected, every violation would license the next and the line itself would dissolve.
% TRANSFER_FUNCTION: Moves operational freedom and force-protection margin from complying state militaries to persons in the enemy's power — civilians, detainees, the wounded — and moves adjudicatory authority over state conduct to courts and monitoring bodies; it also confers access rights on humanitarian organizations inside conflicts they did not start.
% ABSENT_VOICES: Security advocates inside defense establishments, whose operational-necessity case is barred from doctrinal legitimacy by this reading and survives only in reservations and internal dissent; civilians harmed by adversaries who exploit the floor's protections; and the populations of states that never accepted the Additional Protocols. They sit in defense ministries, in ratification-era archives, and in the casualty ledgers of the conflicts themselves.
% DISAPPEARANCE_RATIONALE: Detention and prosecution frameworks would lose their basis overnight: ICRC access agreements, prisoner-of-war registration, grave-breach prosecutions, and the domestic criminal statutes that implement the treaties all presuppose the floor. Conduct of hostilities would reorganize around explicit reciprocity bargaining, and every category the system holds steady — who counts as protected, what counts as a grave breach — would reopen.
% FOUNDING_PROBLEM: The Second World War: industrial-scale atrocity against civilians and detainees, and the demonstrated failure of restraint systems keyed to mutual consent — when one side denied protected status altogether, reciprocity gave the other side its excuse, and the floor vanished exactly where it was needed most.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the judge advocate general corps of mutually hostile states independently teach the same minimums and prosecute their own personnel (each has every incentive to insist the others' compliance is conditional); international criminal tribunals have prosecuted violators from several sides of the same wars; and the pre-1949 historical record, documented by historians with no stake in the treaties, shows what conflicts looked like before the floor existed. No corroborating source attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.55 at interval end: the coordination function is real and primary, but the transfer is asymmetric — compliant militaries pay continuously, and the share collected by parties that never intended to reciprocate grows as enforcement reaches only some violators. Suppression (0.58) is authored as a raw structural property, unscaled by power or scope: the reading forecloses security rationales from legitimate doctrinal expression and backs the prohibition with criminal liability, which is coercive force applied to the state's own action space. Theater (0.44) reflects a widening gap between compliance rhetoric and field practice in asymmetric conflicts — the visiting, registration, and prosecutorial functions are real, but a growing fraction of activity defends the regime's image rather than protected persons. Accessibility collapse (0.48): alternatives persist — denunciation is formally available and unused, reservations and interpretive latitude are exercised, and violation remains possible at reputational cost — so understanding the floor does not close the option space the way a natural limit would. Resistance (0.64) is sustained and organized: defense establishments, reservation diplomacy, and rival readings kept alive inside the same profession. The temporal series run on one shared grid (eight points, 1949-2026) with all three tracked metrics authored at every point; the suppression_requirement series is authored deliberately because the story traces enforcement-capacity change — the prosecutorial layer built from 1993 onward is a genuine intensification, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the complying military's position the arrangement is a cost imposed under conditions of adversary bad faith — restraint priced in casualties, enforced by its own lawyers. From the protected persons' positions it is the only standing barrier between them and abandonment, worth any burden anyone else describes. From the administrator and prosecutor seats it is mandate and jurisdiction — the source of their access and dockets. From the excluded security seat it is a rule that forbids the winning strategy. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Complying state militaries sit at the target end: they pay through the structure (tactical freedom, force protection, compliance infrastructure) and their exit is formally open but practically closed. Protected civilians, detainees, and the wounded sit at the beneficiary end: everything flows to them, nothing from them, and their inability to exit is irrelevant to the duties they trigger. Non-reciprocating armed groups sit nearest the pure-beneficiary pole — they collect protections for their own people while externalizing every reciprocal duty, which is the sharpest asymmetry in the structure and the reason the extraction half of the hybrid does not wash out. Domestic publics of complying states sit near symmetric with a slight target tilt: indirect costs now, contingent insurance later. The ICRC and the tribunals administer and enforce rather than collect; the doctrines the arrangement vindicates (the Martens clause, elementary considerations of humanity, universal jurisdiction, command responsibility) are listed as vindicated propositions, not beneficiaries — they collect no rents. No directionality overrides are used: the beneficiary/victim declarations plus exit options already place every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels guard this classification. Reading the arrangement as pure coordination misses the transfer: the burden lands on a specific payer while a specific class collects without reciprocating, and enforcement selects among violators — that is the hybrid's extraction half, not coordination overhead. Reading it as pure extraction misses the function: the unconditional floor is the only known answer to reciprocity collapse, and the founding problem it solved is live in every current conflict — remove the floor and the line between lawful and unlawful violence dissolves within a campaign. The founding problem is therefore live, the arrangement has not outlived its function, and no mandatrophy is declared; the drift to watch is not obsolescence but selective enforcement, which would convert the hybrid's extraction half into something closer to predation without ever touching the coordination half.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geneva_kernel_reading_contestation,
    'This constraint is one reading of the geneva_conventions_1949 kernel — the humanitarian_ceiling_reading. Which structural facts would change if a sibling reading governed instead?',
    'Compare the compiled sibling stories (geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__security_maximization_reading): victim sets, epsilon, enforcement structure, and per-seat classifications.',
    'Under the reciprocity sibling, every person held by a non-complying adversary loses protected status and the victim set expands sharply; under the maximization sibling the floor becomes default-derogable, state-side extraction falls while detainee-side exposure rises. Per-seat classifications flip for the payer and detainee seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geneva_kernel_reading_contestation, conceptual, 'Committer structure: this file is one of three readings of the GC 1949 kernel; the siblings are separate constraints, not hedges inside this one.').

omega_variable(
    obligation_source_dispute,
    'Where is the reading disagreement located: in whether Common Article 1''s ''in all circumstances'' language makes the duties inherently non-reciprocal, or in how the reprisal and necessity clauses are read?',
    'Textual and jurisprudential analysis: the ICJ Nuclear Weapons advisory opinion, reprisals jurisprudence of the ad hoc tribunals, and the drafting history of Common Articles 1 and 3.',
    'If the duties are inherently non-reciprocal, this reading is the better reading of the text and both siblings are departures from it; if the obligations are bilateralizable, the reciprocity sibling regains textual footing and this reading''s cost profile rests on policy choice rather than text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_source_dispute, conceptual, 'Locates the structural element the sibling readings actually differ on: the source (inherent vs. bilateralized) of the protective duties.').

omega_variable(
    enforcement_selectivity,
    'Does enforcement of grave breaches concentrate on geopolitically weak violators while powerful states'' violations escape prosecution?',
    'Cross-case docket analysis: grave-breach prosecutions and universal-jurisdiction cases mapped against violator state power and alliance position.',
    'Strong selectivity raises effective extraction on weak-state militaries above the authored base rate and pushes the enforcement layer toward predatory operation; even-handed dockets support the hybrid coordination-plus-extraction reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity, empirical, 'Whether the prosecutorial layer enforces the floor evenly or selectively by power.').

omega_variable(
    asymmetric_burden_magnitude,
    'How large is the real operational cost to a complying military of honoring the floor against an adversary that embeds in civilian populations and mistreats captives?',
    'After-action studies and casualty analysis comparing restraint-bound operations with unrestrained counterfactuals; legal-review cost accounting inside targeting cycles.',
    'A large measured burden raises extraction on the payer seat and hardens the extraction half of the hybrid; a negligible burden collapses the extraction claim toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_burden_magnitude, empirical, 'Magnitude of the asymmetric burden this reading imposes on compliant state militaries.').

omega_variable(
    customary_crystallization_status,
    'Is the floor positive treaty construction, or has it crystallized into customary law so deeply that it approaches a pre-legal limit (''elementary considerations of humanity'')?',
    'State-practice and opinio-juris surveys; judicial invocation of the Martens clause; whether any state asserts a lawful right to depart from the core floor rather than merely violating it.',
    'Genuine crystallization moves the constraint toward natural-law certification and makes false-summit evaluation relevant (who benefits from treating the floor as inevitable?); confirmed constructedness keeps it enforcement-dependent and hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_crystallization_status, conceptual, 'Whether the floor is constructed law or crystallized custom approaching natural-law status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_ceiling_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement_basis(geneva_ceiling_tr_t1949, observed).
narrative_ontology:measurement(geneva_ceiling_tr_t1955, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1955, 0.18).
narrative_ontology:measurement_basis(geneva_ceiling_tr_t1955, observed).
narrative_ontology:measurement(geneva_ceiling_tr_t1968, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1968, 0.24).
narrative_ontology:measurement_basis(geneva_ceiling_tr_t1968, observed).
narrative_ontology:measurement(geneva_ceiling_tr_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1977, 0.26).
narrative_ontology:measurement_basis(geneva_ceiling_tr_t1977, observed).
narrative_ontology:measurement(geneva_ceiling_tr_t1994, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement_basis(geneva_ceiling_tr_t1994, observed).
narrative_ontology:measurement(geneva_ceiling_tr_t2006, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2006, 0.36).
narrative_ontology:measurement_basis(geneva_ceiling_tr_t2006, observed).
narrative_ontology:measurement(geneva_ceiling_tr_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2014, 0.4).
narrative_ontology:measurement_basis(geneva_ceiling_tr_t2014, observed).
narrative_ontology:measurement(geneva_ceiling_tr_t2026, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2026, 0.44).
narrative_ontology:measurement_basis(geneva_ceiling_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(geneva_ceiling_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement_basis(geneva_ceiling_be_t1949, observed).
narrative_ontology:measurement(geneva_ceiling_be_t1955, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1955, 0.32).
narrative_ontology:measurement_basis(geneva_ceiling_be_t1955, observed).
narrative_ontology:measurement(geneva_ceiling_be_t1968, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1968, 0.38).
narrative_ontology:measurement_basis(geneva_ceiling_be_t1968, observed).
narrative_ontology:measurement(geneva_ceiling_be_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement_basis(geneva_ceiling_be_t1977, observed).
narrative_ontology:measurement(geneva_ceiling_be_t1994, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1994, 0.45).
narrative_ontology:measurement_basis(geneva_ceiling_be_t1994, observed).
narrative_ontology:measurement(geneva_ceiling_be_t2006, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2006, 0.5).
narrative_ontology:measurement_basis(geneva_ceiling_be_t2006, observed).
narrative_ontology:measurement(geneva_ceiling_be_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2014, 0.53).
narrative_ontology:measurement_basis(geneva_ceiling_be_t2014, observed).
narrative_ontology:measurement(geneva_ceiling_be_t2026, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2026, 0.55).
narrative_ontology:measurement_basis(geneva_ceiling_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(geneva_ceiling_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.25).
narrative_ontology:measurement_basis(geneva_ceiling_su_t1949, observed).
narrative_ontology:measurement(geneva_ceiling_su_t1955, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1955, 0.28).
narrative_ontology:measurement_basis(geneva_ceiling_su_t1955, observed).
narrative_ontology:measurement(geneva_ceiling_su_t1968, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1968, 0.33).
narrative_ontology:measurement_basis(geneva_ceiling_su_t1968, observed).
narrative_ontology:measurement(geneva_ceiling_su_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1977, 0.38).
narrative_ontology:measurement_basis(geneva_ceiling_su_t1977, observed).
narrative_ontology:measurement(geneva_ceiling_su_t1994, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1994, 0.46).
narrative_ontology:measurement_basis(geneva_ceiling_su_t1994, observed).
narrative_ontology:measurement(geneva_ceiling_su_t2006, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2006, 0.52).
narrative_ontology:measurement_basis(geneva_ceiling_su_t2006, observed).
narrative_ontology:measurement(geneva_ceiling_su_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2014, 0.56).
narrative_ontology:measurement_basis(geneva_ceiling_su_t2014, observed).
narrative_ontology:measurement(geneva_ceiling_su_t2026, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(geneva_ceiling_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (the 1949 Geneva text), three readings, three constraints. The ceiling reading (this file) is the ICRC-and-tribunal mainstream; the conditional_reciprocity_reading is the traditional military-doctrine reading; the security_maximization_reading is the revisionist security reading. The readings differ in victim sets (who loses protection when an adversary defects), in epsilon (this reading prices the asymmetric burden highest on compliant militaries; the reciprocity reading shifts exposure onto persons held by non-compliers; the maximization reading makes the floor default-derogable), and in enforcement structure. Downstream pressure: this reading's entrenchment in customary-law discourse raises the legitimacy cost the siblings must pay, which is why this file links outward to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
