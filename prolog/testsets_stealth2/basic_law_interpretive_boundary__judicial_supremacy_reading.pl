% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of the Basic Laws
 *   domain: constitutional/legal-political
 *
 * SUMMARY:
 *   Since the 1995 United Mizrahi Bank decision, Israel's Supreme Court has
 *   treated the Basic Laws as a higher-order legal framework: ordinary
 *   Knesset legislation that contradicts them can be struck down by judicial
 *   order, and the invalidation binds the legislature. This file instantiates
 *   ONE reading of the contested basic_law_interpretive_boundary kernel — the
 *   judicial_supremacy_reading, under which the Court is the authoritative
 *   enforcer of Basic Law supremacy and Knesset output is subject to judicial
 *   nullification. Per the ε-referent rule, extractiveness is authored for
 *   the standing judicial-supremacy arrangement itself, assessed by this
 *   reading's own lights — not for the parliamentary-sovereignty arrangement
 *   its critics would install, and not averaged across readings. The sibling
 *   readings (parliamentary_sovereignty_reading,
 *   balanced_contestation_reading) are separate constraint files linked
 *   through the network section; they redistribute the same institutional
 *   material differently and would yield different beneficiary/victim
 *   structures and different ε.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: Agenda-setter and collecting seat (institutional power, identity_locked exit) — administers the interpretive boundary, decides which statutes fall, and accumulates jurisdictional territory with each invalidation
 *   - rights_claimant_litigants: Organized beneficiary seat (mobile exit) — converts access to litigation into a working veto over policy
 *   - minority_communities_under_court_protection: Powerless beneficiary seat (trapped exit) — depends on judicial precedents because electoral channels rarely deliver their preferred outcomes
 *   - governing_coalition_factions: Primary payer seat (powerful, constrained exit) — sees enacted platform legislation invalidated and lacks a practical override path
 *   - backbench_knesset_members: Secondary payer seat (moderate, constrained exit) — bears discounted legislative work product
 *   - parliamentary_sovereignty_advocates: Excluded seat (powerful, constrained exit) — holds the rival account of the boundary but has no procedural home inside the adjudicative conversation
 *   - attorney_general_office: Observer seat (institutional, analytical exit) — defends government positions while executing court orders against client ministries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.66).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.64).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of the Basic Laws").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional/legal-political").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '35351e5e-5251-45d7-a0e1-d27bb91a84ba').
narrative_ontology:cs_kernel_codification('35351e5e-5251-45d7-a0e1-d27bb91a84ba', fixed_text).
narrative_ontology:cs_authority_grounding('35351e5e-5251-45d7-a0e1-d27bb91a84ba', lineage).
narrative_ontology:cs_interpretation_layer_present('35351e5e-5251-45d7-a0e1-d27bb91a84ba').
narrative_ontology:cs_reading_relation('35351e5e-5251-45d7-a0e1-d27bb91a84ba', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('35351e5e-5251-45d7-a0e1-d27bb91a84ba', basic_law_interpretive_boundary__balanced_contestation_reading, forecloses).
narrative_ontology:cs_axiom('35351e5e-5251-45d7-a0e1-d27bb91a84ba', foundational, basic_law_supremacy_binds_legislature).
narrative_ontology:cs_axiom_status(basic_law_supremacy_binds_legislature, holdable).
narrative_ontology:cs_axiom_grounding('35351e5e-5251-45d7-a0e1-d27bb91a84ba', basic_law_supremacy_binds_legislature, conventional).
narrative_ontology:cs_axiom('35351e5e-5251-45d7-a0e1-d27bb91a84ba', foundational, court_holds_final_interpretive_authority).
narrative_ontology:cs_axiom_status(court_holds_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('35351e5e-5251-45d7-a0e1-d27bb91a84ba', court_holds_final_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('35351e5e-5251-45d7-a0e1-d27bb91a84ba', basic_laws_as_binding_supreme_law).
narrative_ontology:cs_drift_state('35351e5e-5251-45d7-a0e1-d27bb91a84ba', post_2023_constitutional_crisis, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('35351e5e-5251-45d7-a0e1-d27bb91a84ba', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimant_litigants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, minority_communities_under_court_protection).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, governing_coalition_factions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, backbench_knesset_members).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, judicial_review_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The fifteen judges sitting as the Supreme Court and, in its High Court of Justice capacity, as the petition forum. They decide which petitions raise Basic Law questions, define the limits of Knesset authority, and issue orders striking down statutes or provisions. Since the 1995 United Mizrahi Bank decision they treat the Basic Laws as superior to ordinary legislation. Their professional identities and the institution's standing are bound up with continuing to exercise that role; stepping back would repudiate three decades of precedent. Sitting justices hold decisive votes on the committee that appoints their successors.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, beneficiary).

% Individual petitioners and public-interest organizations that bring constitutional petitions against legislation and state action. Access to the courtroom converts into a working channel to block or reshape policy they could not move through elections. Filing is voluntary and they retain the alternative of political advocacy, so participation is a chosen strategy rather than a fixed condition.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimant_litigants, beneficiary,
    organized, biographical, mobile, national).

% Groups — Arab citizens, asylum seekers, LGBTQ Israelis, and in some contexts ultra-Orthodox communities — whose protections depend heavily on court decisions because their preferred policies rarely win legislative majorities. They cannot relocate their citizenship cheaply, and their day-to-day security tracks the durability of judicial precedents.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, minority_communities_under_court_protection, beneficiary,
    powerless, generational, trapped, national).

% The parties holding a Knesset majority at any given time. They pass legislation implementing their platforms and see portions of it invalidated by court order, sometimes years after enactment. Formally they could amend Basic Laws by the same simple majority that passes ordinary bills, but doing so triggers mass protest, international reaction, and internal coalition fracture, as the 2023 override-clause effort showed. Their practical paths are to govern within the existing framework or absorb the costs of open constitutional confrontation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, governing_coalition_factions, payer,
    powerful, immediate, constrained, national).

% Rank-and-file members who vote for bills that may later fall to judicial invalidation. Their legislative work product carries a standing discount: time invested in statutes the court may void. They lack the coalition leverage to force constitutional confrontations and mostly absorb the uncertainty.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, backbench_knesset_members, payer,
    moderate, biographical, constrained, national).

% Senior politicians, some jurists, and movement activists who hold that the Knesset, as the elected representative body, may interpret and amend the Basic Laws by simple majority and may override judicial decisions. They sit outside the courtroom conversation that settles these questions: their position has no procedural home in a petition process run by the institution whose jurisdiction they dispute. Advancing their view requires winning elections and then undertaking constitutional confrontation; several were in the coalition that passed the July 2023 amendment limiting the reasonableness standard.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, parliamentary_sovereignty_advocates, excluded,
    powerful, biographical, constrained, national).

% The state's chief legal office defends government positions in constitutional petitions while also advising that Basic Law supremacy be respected. It occupies a hinge position: it executes court orders against client ministries even when the government disagrees, and its independence is itself a recurring political flashpoint.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, attorney_general_office, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable higher-order commitment framework: electoral transients cannot rewrite the rules of the game mid-cycle, rights protections and institutional boundaries hold across governments, and inter-branch disputes resolve through adjudication rather than raw political force.
% TRANSFER_FUNCTION: Moves final-decision authority over contested legislation from the elected chamber to the bench; moves a working veto over policy to whoever can litigate; moves compliance obligations — re-enactment, redrafting, implementation reversal — onto ministries and the Knesset when statutes or provisions fall.
% ABSENT_VOICES: Holders of the parliamentary-sovereignty reading — senior coalition figures, some jurists, and the movements behind the 2023 override campaign — are structurally absent from the adjudicative conversation that settles the boundary: their position has no procedural home in a petition process administered by the institution whose jurisdiction they dispute. Also absent are non-litigating citizens affected by invalidated statutes who lack the resources or standing to reach the courtroom; the litigation veto belongs to those who can mount a petition.
% DISAPPEARANCE_RATIONALE: If binding judicial invalidation vanished overnight, decades of doctrine built on struck-down or threatened statutes would need wholesale legislative re-adjudication; minority protections would shift from courtroom precedent to electoral bargaining where their sponsors routinely lose; and final-decision authority over the rules of the game would return to whichever faction held a Knesset majority. The institutional landscape would reorganize around the legislature's recovered last word.
% FOUNDING_PROBLEM: Israel operated without a complete formal constitution; the 1992 Basic Laws (Human Dignity and Liberty, Freedom of Occupation) were meant to anchor rights, and United Mizrahi Bank (1995) answered the follow-on problem that a rights charter without enforcement machinery is declaratory only — someone must be able to stop ordinary legislation from contradicting it.
% FOUNDING_PROBLEM_CORROBORATION: That a rights-anchoring problem existed is corroborated from outside the benefiting parties: the 1987 Kahn Commission report (a legislative-academic body that preceded judicial review), comparative scholarship on the 1990s constitutionalization wave, and repeated cross-party declarations supporting entrenched rights in principle. Corroboration for THIS arrangement as the answer comes almost entirely from the Court's own opinions and allied commentary; the Knesset never enacted the general validity clause that would have authorized review expressly, and the coalition parties dispute both the diagnosis and the remedy. No neutral body attests that judicial supremacy specifically — as opposed to some anchoring mechanism — was the mandated solution.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim is tangled_rope because the arrangement has a genuine coordination function — a commitment device that keeps transient majorities from rewriting the rules of the game and gives dispersed minorities a protection channel — AND a clear asymmetric component: the enforcing institution certified its own jurisdiction (no Basic Law contains a general validity clause), the final word moved from the elected chamber to an unelected bench, and each invalidation enlarges the enforcing institution's domain. Metrics describe operation as of interval end: extractiveness 0.66 (substantial but short of pure-extraction levels because the coordination service is real and used voluntarily by litigants); suppression 0.64 as a raw structural property — binding invalidation with no practical override — deliberately NOT scaled by power or scope, since only extractiveness is scaled in the engine's computation; theater_ratio 0.30 (the review machinery functions, but a growing share of activity is legitimacy defense — public justification, recusal drama, jurisdictional argument — rather than adjudication); accessibility_collapse 0.60 (inside the framework, alternatives such as simple-majority override collapse, but the formal amendment route keeps partial alternatives alive); resistance 0.70 (the 2023 reform crisis, override-clause campaigns, and appointment fights show sustained organized pushback). The temporal series run on one shared seven-point grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the coalition benches, the arrangement appears as an unelected veto over their mandate: they passed the laws, the Court voids them, and the formal amendment exit is politically radioactive. From the minority and litigant seats, the same arrangement is the only functioning shield they have. From the bench, it is the faithful execution of a constitutional framework the Court did not write but must maintain. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for rights_claimant_litigants (organized, mobile — nearest the subsidy end among the human seats), minority_communities_under_court_protection (powerless, trapped — deeply sheltered but unable to convert benefit into mobility), and supreme_court_justices (an agenda-setter that also collects: each invalidation transfers decision territory to the bench). Victim declarations drive high directionality for governing_coalition_factions (powerful but constrained — near the full-target end because their exit is blocked by protest economics rather than formal rule) and backbench_knesset_members (moderate, constrained). National spatial scope applies uniformly, and compliance verification is centralized in a single courtroom, which moderates the scope amplification relative to a diffusely verified regime. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already place every seat correctly, including the dual-positioned bench, which both administers and collects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — anchoring rights in a state with no complete formal constitution — was real and predates the Court's assumption of review; the 1987 Kahn Commission process shows legislative intent to constitutionalize independent of the bench. Its status is contested rather than dead: the underlying exposure persists, but whether THIS arrangement answers it, or manufactures the problem it solves, is exactly what the sibling readings dispute. Reading the arrangement as pure coordination mislabels it: it ignores who pays (the elected chamber) and how the enforcement mandate was acquired. Reading it as pure extraction mislabels it the other way: it ignores the voluntary litigation channel and the durable protections minorities cannot obtain electorally. The tangled_rope claim keeps both halves visible. Contested founding status combined with a world_rearranges disappearance verdict produces no zombie flag — the arrangement's function is live enough that overnight removal would force wholesale legislative re-adjudication — but the rising theater trajectory (0.12 to 0.30) marks the growing share of maintenance effort spent defending legitimacy rather than adjudicating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the judicial_supremacy_reading of the basic_law_interpretive_boundary kernel; would the parliamentary_sovereignty or balanced_contestation readings produce a different constraint with different beneficiaries, victims, and epsilon?',
    'Generate the sibling stories and compare computed classifications; the disagreement is located in whether the Basic Laws carry higher-order justiciable normativity and who holds the final interpretive word.',
    'Under parliamentary sovereignty the Knesset seat flips from payer to agenda-setter and the Court loses enforcement standing; under balanced contestation both seats become bounded principals and epsilon falls toward coordination-cost levels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this constraint is one of three readings of the Basic-Law-status kernel.').

omega_variable(
    mizrahi_self_certification,
    'Does the Court''s enforcement mandate derive from the Basic Laws'' enacted content, or from the Court''s own assertion of jurisdiction in United Mizrahi Bank (1995), given that no Basic Law contains a general validity clause authorizing review?',
    'Textual-doctrinal analysis plus archival study of the drafting history of Basic Law: Human Dignity and Liberty; comparison with jurisdictions that enacted explicit validity clauses before activating review.',
    'If self-certified, part of the measured extraction is jurisdictional rent taken by the enforcing institution itself, raising effective extraction on the legislative seats; if derivable from enacted text, the coercive component is lower and closer to ordinary constitutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mizrahi_self_certification, empirical, 'Whether the enforcement mandate is textually grounded or self-granted.').

omega_variable(
    amendment_review_open_question,
    'Are amendments to the Basic Laws themselves subject to judicial review — the identity-clause question left open after the 2023 reasonableness-standard litigation?',
    'Pending Supreme Court disposition of challenges to Basic Law amendments; the Court''s recusal posture during the 2023 crisis deferred the question rather than answering it.',
    'If amendments are reviewable, the payer seats have no remaining formal exit and their effective extraction approaches the full-target end; if unreviewable, a formal exit survives and the arrangement stays hybrid rather than closed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_review_open_question, empirical, 'Whether the legislature retains any formal exit through Basic Law amendment.').

omega_variable(
    resistance_feedback_direction,
    'Does rising political resistance intensify enforcement (a ratchet in which the Court defends its jurisdiction more aggressively) or produce strategic retreat (deference, recusal, narrowed standards)?',
    'Track post-2023 case outcomes, the Court''s handling of Basic Law amendment challenges, and appointment-composition fights through the next election cycle.',
    'Intensification pushes the arrangement toward harder enforcement and a higher suppression trajectory; retreat converts enforcement capacity into performance and drifts the arrangement toward inertial maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resistance_feedback_direction, empirical, 'Direction of the enforcement response to the 2023 legitimacy crisis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bl_judicial_supremacy_tr_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(bl_judicial_supremacy_tr_t0, observed).
narrative_ontology:measurement(bl_judicial_supremacy_tr_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(bl_judicial_supremacy_tr_t5, observed).
narrative_ontology:measurement(bl_judicial_supremacy_tr_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(bl_judicial_supremacy_tr_t10, observed).
narrative_ontology:measurement(bl_judicial_supremacy_tr_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(bl_judicial_supremacy_tr_t15, observed).
narrative_ontology:measurement(bl_judicial_supremacy_tr_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(bl_judicial_supremacy_tr_t20, observed).
narrative_ontology:measurement(bl_judicial_supremacy_tr_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(bl_judicial_supremacy_tr_t25, observed).
narrative_ontology:measurement(bl_judicial_supremacy_tr_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(bl_judicial_supremacy_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(bl_judicial_supremacy_be_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(bl_judicial_supremacy_be_t0, observed).
narrative_ontology:measurement(bl_judicial_supremacy_be_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(bl_judicial_supremacy_be_t5, observed).
narrative_ontology:measurement(bl_judicial_supremacy_be_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(bl_judicial_supremacy_be_t10, observed).
narrative_ontology:measurement(bl_judicial_supremacy_be_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(bl_judicial_supremacy_be_t15, observed).
narrative_ontology:measurement(bl_judicial_supremacy_be_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(bl_judicial_supremacy_be_t20, observed).
narrative_ontology:measurement(bl_judicial_supremacy_be_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement_basis(bl_judicial_supremacy_be_t25, observed).
narrative_ontology:measurement(bl_judicial_supremacy_be_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(bl_judicial_supremacy_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(bl_judicial_supremacy_su_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(bl_judicial_supremacy_su_t0, observed).
narrative_ontology:measurement(bl_judicial_supremacy_su_t5, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(bl_judicial_supremacy_su_t5, observed).
narrative_ontology:measurement(bl_judicial_supremacy_su_t10, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement_basis(bl_judicial_supremacy_su_t10, observed).
narrative_ontology:measurement(bl_judicial_supremacy_su_t15, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(bl_judicial_supremacy_su_t15, observed).
narrative_ontology:measurement(bl_judicial_supremacy_su_t20, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement_basis(bl_judicial_supremacy_su_t20, observed).
narrative_ontology:measurement(bl_judicial_supremacy_su_t25, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement_basis(bl_judicial_supremacy_su_t25, observed).
narrative_ontology:measurement(bl_judicial_supremacy_su_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(bl_judicial_supremacy_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the constitutional status of the Basic Laws' decomposes into three structurally distinct claims that differ on a single binary — the locus of final interpretive authority. Each member has its own epsilon, beneficiary/victim structure, and classification; this member (judicial supremacy) carries the highest extraction on legislative targets because it adds binding nullification to the framework. Members are linked through affects_constraints; downstream pressure runs through appointment composition and amendment politics, which is why this reading influences the operating environment of both siblings even while foreclosing their premises within any single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
