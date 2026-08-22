% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Constitutional Final-Arbiter Monopoly (Judicial Supremacy Reading)
 *   domain: constitutional law/political philosophy/institutional design
 *
 * SUMMARY:
 *   This story instantiates one reading of the
 *   constitutional_authority_boundary kernel: the judicial supremacy reading,
 *   under which the constitutional text establishes courts as final,
 *   unchallengeable arbiters of all constitutional questions, empowered to
 *   invalidate legislative and executive acts with no corrective remedy short
 *   of supermajority amendment. The arrangement coordinates genuinely — it
 *   settles inter-branch disputes over constitutional meaning that would
 *   otherwise be resolved by self-certification, deadlock, or force — while
 *   transferring final decision authority from elected institutions to a
 *   life-tenured judiciary that both administers the arrangement and sits at
 *   its apex. The claim and the metrics are independent authored facts: the
 *   claimed type (tangled_rope) states the structure believed true — a real
 *   coordination function carrying asymmetric extraction — while the metrics
 *   describe the arrangement's observed operation across two centuries.
 *   Sibling readings of the same kernel are separate constraints, not parts
 *   of this one.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: agenda-setter and principal beneficiary (institutional/identity_locked) — administers finality, builds doctrine, collects the final word on constitutional meaning
 *   - elected_legislature: primary payer (powerful/constrained) — policy space bounded by invalidation without remedy
 *   - executive_branch_officials: secondary payer (institutional/constrained) — programs and orders subject to enjoinder and invalidation
 *   - ordinary_citizens_voters: dual-positioned beneficiary/payer (powerless/constrained) — receives rights enforcement, loses electoral settlement of constitutional meaning
 *   - constitutional_litigators_and_academy: secondary beneficiary (organized/mobile) — careers built on arguing and expounding final constructions
 *   - popular_constitutionalists: excluded voice (moderate/trapped) — holds that the people retain interpretive authority; holds no seat in the courtroom conversation
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) — studies alternative allocations of final authority across systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.71).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.6).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Constitutional Final-Arbiter Monopoly (Judicial Supremacy Reading)").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional law/political philosophy/institutional design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '0cdc28cf-a161-4793-ae0b-1596d72256b4').
narrative_ontology:cs_kernel_codification('0cdc28cf-a161-4793-ae0b-1596d72256b4', fixed_text).
narrative_ontology:cs_authority_grounding('0cdc28cf-a161-4793-ae0b-1596d72256b4', lineage).
narrative_ontology:cs_interpretation_layer_present('0cdc28cf-a161-4793-ae0b-1596d72256b4').
narrative_ontology:cs_reading_relation('0cdc28cf-a161-4793-ae0b-1596d72256b4', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('0cdc28cf-a161-4793-ae0b-1596d72256b4', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('0cdc28cf-a161-4793-ae0b-1596d72256b4', foundational, text_commits_final_arbitration_to_courts).
narrative_ontology:cs_axiom_status(text_commits_final_arbitration_to_courts, holdable).
narrative_ontology:cs_axiom_grounding('0cdc28cf-a161-4793-ae0b-1596d72256b4', text_commits_final_arbitration_to_courts, conventional).
narrative_ontology:cs_axiom('0cdc28cf-a161-4793-ae0b-1596d72256b4', foundational, single_final_interpreter_required_for_rule_of_law).
narrative_ontology:cs_axiom_status(single_final_interpreter_required_for_rule_of_law, holdable).
narrative_ontology:cs_axiom_grounding('0cdc28cf-a161-4793-ae0b-1596d72256b4', single_final_interpreter_required_for_rule_of_law, instrumental).
narrative_ontology:cs_reference_frame('0cdc28cf-a161-4793-ae0b-1596d72256b4', text_delegated_judicial_finality).
narrative_ontology:cs_drift_state('0cdc28cf-a161-4793-ae0b-1596d72256b4', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0cdc28cf-a161-4793-ae0b-1596d72256b4', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_litigators_and_academy).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch_officials).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, ordinary_citizens_voters).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, ordinary_citizens_voters).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, final_arbiter_necessity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which constitutional questions reach final resolution, builds the doctrine that defines the limits of legislative and executive power, and enforces its judgments through injunctions, contempt, and remedial decrees. Collects the final word on constitutional meaning, institutional prestige, and policy influence insulated from electoral correction. Its members hold life tenure and their professional identities are constituted by the final-arbiter role; leaving the arrangement would mean dissolving the office itself.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary, beneficiary).

% Passes statutes that govern national life, then watches courts invalidate portions without appeal or override. Its policy program is bounded by judicial constructions it did not choose and cannot revise except through an amendment process requiring supermajorities across many states. Its available responses are litigation strategy, appointment politics that mature over decades, and occasional jurisdiction-stripping proposals that rarely advance.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature, payer,
    powerful, biographical, constrained, national).

% Implements programs and issues orders that courts can enjoin or invalidate, with no corrective channel once final judgment issues. Agencies redesign rules around anticipated judicial constructions; administrations comply with adverse final rulings while litigating future ones. Exit would mean defying court orders, an option carrying constitutional-crisis costs.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch_officials, payer,
    institutional, biographical, constrained, national).

% Receive enforceable rights protections that courts supply against legislative majorities, and lose the ability to settle constitutional disagreements through ordinary electoral politics. They can vote out legislators but cannot vote on constitutional meaning; their access to the arrangement runs through litigants with standing and resources. Personal exit means emigration or disengagement.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, ordinary_citizens_voters, beneficiary,
    powerless, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, ordinary_citizens_voters, payer).

% Supreme Court bar members, public-interest litigators, and constitutional law academics build careers around arguing and expounding the court's final constructions. Demand for their expertise tracks the court's centrality; if final authority moved elsewhere, their skills would migrate to the new forum. They benefit from the arrangement without administering it.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_litigators_and_academy, beneficiary,
    organized, biographical, mobile, national).

% Scholars and movements holding that the people, not courts, retain ultimate interpretive authority. They publish critiques and draft popular-sovereignty reforms but hold no seat in the courtroom conversation; standing doctrine and justiciability rules keep their objections outside the process that settles constitutional meaning.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, popular_constitutionalists, excluded,
    moderate, generational, trapped, national).

% Study how different democracies allocate final constitutional authority — judicial supremacy, parliamentary sovereignty, coordinate construction — and assess the trade-offs each arrangement produces. They take no part in the arrangement and bear neither its costs nor its benefits.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles disputes over constitutional meaning between co-equal branches through a single authoritative forum, preventing each branch from self-certifying the limits of its own power and avoiding deadlock or force as the resolver of last resort.
% TRANSFER_FUNCTION: Moves final decision authority over constitutional meaning from elected legislatures and executives to unelected judges with life tenure; concretely, moves policy outcomes whenever a statute or executive act is invalidated, from electoral majorities to judicial majorities.
% ABSENT_VOICES: Popular constitutionalists and departmentalist legislators would object that the settlement conversation occurs only inside courtrooms; citizens without standing or resources have no seat; the unanimity of settled law reflects who was admitted to argue, not the consent of those governed by the result.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, constitutional meaning would be immediately renegotiated among the branches: either each branch would begin self-certifying its own powers (fragmentation), or the legislature would claim final authority (reallocation). Thousands of precedents would lose their enforcement basis, pending remedies would dissolve, and the basic operating assumption of the legal system — that some institution's constitutional determinations stick — would have to be rebuilt from scratch.
% FOUNDING_PROBLEM: Resolving inter-branch conflicts over constitutional meaning without resort to force or perpetual deadlock: the founding generation needed some mechanism to decide, when the legislature and executive claimed powers the constitution did not grant, whose determination would prevail.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and comparative constitutional designers outside the benefiting parties attest that inter-branch constitutional conflict is a real founding-era problem requiring some settlement mechanism — the pre-establishment record of statutes openly exceeding enumerated powers and the nullification-crisis literature document the cost of having no arbiter. Critics from the same external seats attest the problem is real while disputing that judicial monopoly is its solution. No attesting source inside the judiciary is relied upon.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.71 at interval end) because the counter-majoritarian veto is decoupled from electoral accountability: a judicial majority can permanently remove policy options from the elected branches, and the only formal remedy — constitutional amendment — requires supermajorities that make reversal exceptional. Suppression (0.60) is structural rather than ornamental: once a final judgment issues, no appeal, override, or revisiting channel exists; compliance is secured by norm, remedial power, and the political cost of defiance. Theater ratio (0.28) reflects real doctrinal labor increasingly accompanied by result-driven opinion-writing and neutrality performance. Accessibility collapse (0.55): departmentalist and coordinate-construction alternatives survive as live theory but are foreclosed operationally once supremacy is entrenched. Resistance (0.55): recurring court-curbing bills, jurisdiction-stripping proposals, packing threats, and episodic defiance keep pressure on the arrangement without displacing it. The measurement series share one time grid (0, 40, 80, 120, 160, 200, 220) and trace a full cycle: extractiveness peaks in the early activist era (t≈80), troughs during the political-branch rebalancing crisis (t≈120), then rebuilds through the late-century revival to its contemporary maximum. The cycle is driven by appointment politics interacting with accumulated legitimacy capital — it modulates extraction intensity rather than constituting an extraction mechanism, though each trough-to-peak climb resets the floor higher than the last. Receipt: the gains demonstrably accrue to the constitutional_judiciary seat — final word, prestige, policy leverage — so gain_flow names that seat rather than diffuse. Fixing cost is prohibitive: the actors positioned to alter the arrangement (the legislature, amendment coalitions) face amendment thresholds and legitimacy costs that dwarf any single session's benefit from reform, which is why court-curbing proposals recur without advancing.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat the arrangement is the coordination structure it built, staffs, and sincerely defends — settlement, neutrality, rule of law. From the legislature's seat the same structure operates as a veto exercised over its product by an actor it cannot elect, discipline, or overrule. Citizens straddle the divide: rights protection arrives through the court, and loss of self-government arrives through the same channel. The engine computes these divergent per-seat types from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is declared beneficiary and administers the arrangement, placing it near the beneficiary end (d near 0.0) — its identity lock deepens rather than moderates its positional gain. The legislature and executive are declared victims with constrained exit (amendment is the only formal channel and it is nearly closed), placing them near the target end (d near 1.0). Constitutional litigators and the academy are genuine secondary beneficiaries with mobile exit — their expertise would migrate to whatever forum held final authority — so their derived low d is accurate. Ordinary citizens are the one seat the derivation misreads: their beneficiary declaration would place them near the subsidy end, but their actual position is symmetric (approximately 0.45) because they receive rights enforcement and lose electoral control of constitutional meaning through the same structure; the directionality override on the powerless atom encodes this correction, and no other agent shares that power atom. Popular constitutionalists are excluded rather than coordinated — their exclusion from the settlement conversation is part of what the arrangement maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling inter-branch constitutional conflict without force or deadlock — remains live, so the mismatch consumer should find no dead-mandate flag: status live crossed with verdict world_rearranges is the consistent cell. The tangled_rope classification prevents two opposite mislabels: reading the arrangement as pure rope would erase the legislature's remedy-less position and the judiciary's positional gains; reading it as pure snare would erase the genuine settlement function that even hostile political-science literature concedes. Watch items: if theater_ratio continues climbing while the settlement function migrates into expedited procedural channels, the arrangement drifts toward piton-theatrical maintenance; if the amendment valve is confirmed dead (see omega amendment_valve_functionality), suppression hardens toward the snare boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the constitutional_authority_boundary kernel (the judicial_supremacy_reading); which allocation of final interpretive authority does the constitutional text actually establish?',
    'Sibling-story comparison plus original-public-meaning analysis: compile coordinate_construction_reading and parliamentary_primacy_reading as separate constraints and compare computed classifications; founding-era drafting and ratification records adjudicate the textual question.',
    'If a sibling reading is textually better supported, this constraint''s beneficiary/victim structure dissolves — the judiciary exits the beneficiary set, the legislature''s constrained-policy-space position converts to retained authority, and epsilon falls toward the sibling''s value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story is one of three readings of the constitutional authority kernel; the others are separate constraints.').

omega_variable(
    textual_grant_vs_marbury_construction,
    'Is final arbitral authority granted by the constitutional text itself, or constructed by Marbury v. Madison and consolidated by two centuries of institutional practice?',
    'Founding-era drafting and ratification records, original-public-meaning scholarship, and comparative timing of judicial-supremacy consolidation across jurisdictions with similar texts.',
    'If constructed, the arrangement presents a built structure as textual necessity — identifiable beneficiaries maintaining a constructed constraint dressed as settled constitutional meaning; naturality claims fail and the extraction assessment shifts upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_grant_vs_marbury_construction, empirical, 'Whether the arrangement''s authority claim is textual or constructed and maintained.').

omega_variable(
    amendment_valve_functionality,
    'Does the constitutional amendment process constitute a real remedy against adverse final rulings, or is it practically unavailable?',
    'Historical base rates: amendments proposed to reverse specific rulings versus amendments actually adopted; comparative difficulty measures across amendment mechanisms.',
    'If the valve is dead, effective suppression rises toward the snare boundary (targets with no functional exit); if live, the arrangement retains a genuine accountability channel supporting the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_valve_functionality, empirical, 'Whether the no-remedy feature has a functioning escape valve.').

omega_variable(
    counter_majoritarian_burden_incidence,
    'Who ultimately bears the counter-majoritarian burden — the legislature as an institution, or citizens in their capacity as self-governing equals?',
    'Normative-political theory analysis (dignity-of-legislation arguments versus rights-protection arguments) combined with empirical study of whose considered preferences are overridden by invalidation.',
    'If incidence falls mainly on citizens'' self-government, the victim set should center the electorate rather than the legislature, changing directionality weights and potentially pushing effective extraction higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_majoritarian_burden_incidence, conceptual, 'Locating the ultimate bearer of the counter-majoritarian burden.').

omega_variable(
    compliance_norm_stability,
    'Is political-branch compliance with judicial finality a stable internalized norm, or contingent spending of accumulated judicial legitimacy?',
    'Comparative episode analysis of defiance threats (packing proposals, non-enforcement moments, open criticism campaigns) against judicial approval indices; cross-national persistence data for final-arbiter arrangements.',
    'If compliance is legitimacy-contingent, the arrangement''s persistence depends on continued performance of neutrality — raising theater sensitivity and predicting instability under legitimacy depletion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_norm_stability, empirical, 'Stability of the compliance norm underpinning enforcement of finality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t80, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement_basis(cons_tr_t80, observed).
narrative_ontology:measurement(cons_tr_t120, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 120, 0.18).
narrative_ontology:measurement_basis(cons_tr_t120, observed).
narrative_ontology:measurement(cons_tr_t160, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 160, 0.21).
narrative_ontology:measurement_basis(cons_tr_t160, observed).
narrative_ontology:measurement(cons_tr_t200, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 200, 0.25).
narrative_ontology:measurement_basis(cons_tr_t200, observed).
narrative_ontology:measurement(cons_tr_t220, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 220, 0.28).
narrative_ontology:measurement_basis(cons_tr_t220, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t80, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 80, 0.61).
narrative_ontology:measurement_basis(cons_be_t80, observed).
narrative_ontology:measurement(cons_be_t120, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 120, 0.48).
narrative_ontology:measurement_basis(cons_be_t120, observed).
narrative_ontology:measurement(cons_be_t160, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 160, 0.58).
narrative_ontology:measurement_basis(cons_be_t160, observed).
narrative_ontology:measurement(cons_be_t200, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 200, 0.66).
narrative_ontology:measurement_basis(cons_be_t200, observed).
narrative_ontology:measurement(cons_be_t220, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 220, 0.71).
narrative_ontology:measurement_basis(cons_be_t220, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t80, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement_basis(cons_su_t80, observed).
narrative_ontology:measurement(cons_su_t120, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 120, 0.44).
narrative_ontology:measurement_basis(cons_su_t120, observed).
narrative_ontology:measurement(cons_su_t160, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 160, 0.5).
narrative_ontology:measurement_basis(cons_su_t160, observed).
narrative_ontology:measurement(cons_su_t200, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 200, 0.56).
narrative_ontology:measurement_basis(cons_su_t200, observed).
narrative_ontology:measurement(cons_su_t220, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 220, 0.6).
narrative_ontology:measurement_basis(cons_su_t220, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% The constitutional_authority_boundary kernel decomposes into three epsilon-invariant constraint stories: this judicial_supremacy_reading (courts as final unchallengeable arbiters; judiciary in the beneficiary set, legislature in the victim set, high epsilon), coordinate_construction_reading (distributed interpretive authority; no single final arbiter; different beneficiary/victim geometry), and parliamentary_primacy_reading (legislature retains final authority; judiciary subordinate). Each story carries its own epsilon, stakeholders, and classification; they are linked here for contamination-propagation and family analysis, not merged. This story authors only the judicial supremacy instantiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__judicial_supremacy_reading, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
