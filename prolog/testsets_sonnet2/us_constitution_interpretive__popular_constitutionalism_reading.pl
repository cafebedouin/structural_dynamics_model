% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the popular constitutionalism reading of the
 *   interpretive-authority kernel governing the U.S. Constitution: the claim
 *   that constitutional meaning is legitimately shaped by sustained political
 *   mobilization and democratic contestation among the branches and the
 *   electorate, not exclusively settled by judicial pronouncement. This is
 *   one of three structurally distinct readings of the same kernel (the
 *   others being originalist and living-constitution readings, generated as
 *   separate constraint stories). The referent for extraction here is the
 *   standing arrangement AS THIS READING SEES IT: an interpretive regime
 *   where judicial rulings function as provisional rather than final, subject
 *   to being effectively unwound through sustained political pressure —
 *   assessed by this reading's own lights, not by the rights-protective
 *   alternative it might endorse.
 *
 * KEY AGENTS:
 *   - popular_political_movements: primary beneficiary (organized/mobile) — gains a non-judicial track to constitutional change
 *   - legislative_majorities: primary beneficiary (institutional/mobile) — gains leverage to press statutory boundaries against settled doctrine
 *   - judicial_finality_advocates: primary target (institutional/constrained) — loses the guarantee that rulings are dispositive
 *   - counter_majoritarian_dependent_minorities: primary target (powerless/trapped) — loses the specific protection judicial review was designed to provide against majorities
 *   - supreme_court: agenda_setter and structurally excluded — retains formal ruling authority but loses practical finality
 *   - constitutional_scholars: analytical observer — assesses whether the reading is accurate history or motivated doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.58).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.44).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, 'd0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27').
narrative_ontology:cs_kernel_codification('d0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27', distributed).
narrative_ontology:cs_authority_grounding('d0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27', distributed).
narrative_ontology:cs_reading_relation('d0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27', us_constitution_interpretive__living_constitution_reading, influences).
narrative_ontology:cs_axiom('d0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27', foundational, interpretive_authority_shared_not_judicial_monopoly).
narrative_ontology:cs_axiom_status(interpretive_authority_shared_not_judicial_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('d0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27', interpretive_authority_shared_not_judicial_monopoly, conventional).
narrative_ontology:cs_axiom('d0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27', foundational, constitutional_meaning_settled_through_political_struggle).
narrative_ontology:cs_axiom_status(constitutional_meaning_settled_through_political_struggle, holdable).
narrative_ontology:cs_axiom_grounding('d0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27', constitutional_meaning_settled_through_political_struggle, empirically_contingent).
narrative_ontology:cs_reference_frame('d0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27', reconstruction_era_political_ratification_of_meaning).
narrative_ontology:cs_drift_state('d0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27', post_1980s_judicial_professionalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d0bd539a-b6b8-4e4b-9c4d-c7edaecd2a27', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_dependent_minorities).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, settlement_reliant_institutional_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mobilize outside courts — through electoral coalitions, mass protest, state legislation, and constitutional amendment campaigns — to force reinterpretation of what the Constitution means. When successful, they convert political victory into constitutional meaning without waiting for or deferring to judicial ratification. Reconstruction-era abolitionists, the New Deal coalition, and the civil rights movement are their template cases.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, popular_political_movements, agenda_setter).

% Enact statutes that press against or reinterpret constitutional boundaries, daring courts to strike them down and betting that sustained political support will eventually make the new reading stick regardless of judicial resistance. They gain a second track to constitutional change that bypasses amendment's supermajority requirement and judicial appointment's slow timeline.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, agenda_setter).

% Distrust unelected judges as the final word on fundamental questions and see popular contestation as the only legitimate route to being heard. Gain standing and voice when constitutional meaning is treated as contestable in the political arena rather than settled in chambers they cannot access or influence.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Depend on courts having the last word so that constitutional rulings are stable, predictable, and insulated from majoritarian reversal. Under this reading, their rulings are treated as one input among several rather than dispositive, and a ruling can be effectively overturned by sustained political mobilization even without formal overruling — eroding the authority judicial finality advocates rely on to do their institutional job.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, constrained, national).

% Rely on courts to protect rights against majoritarian override precisely because they lack the numbers to win at the ballot box or in mass mobilization. When constitutional meaning is made contestable by popular political movements, groups that cannot mobilize equivalent political force are exposed to majorities reinterpreting rights protections against them — the same mechanism that empowered abolition and civil rights can, in reverse, unwind protections for groups without comparable mobilizing capacity.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_dependent_minorities, payer,
    powerless, civilizational, trapped, national).

% Businesses, administrative agencies, and lower courts that need a stable constitutional baseline to plan investment, regulatory compliance, and case law. Constant susceptibility of constitutional meaning to renegotiation by political mobilization raises their planning uncertainty and compliance costs, since a settled ruling may not stay settled if enough political pressure accumulates against it.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, settlement_reliant_institutional_actors, payer,
    organized, biographical, constrained, national).

% Issues rulings that are treated, under this reading, as provisional pronouncements subject to being overridden in practice by sustained political mobilization (court-packing threats, jurisdiction-stripping, non-enforcement, or slow attrition through appointments) rather than as final resolutions. The Court retains formal authority to rule but loses the practical guarantee that its rulings stick without political ratification.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, supreme_court, excluded).

% Study and debate whether popular constitutionalism accurately describes how constitutional change has actually occurred historically (Bruce Ackerman's constitutional moments, Reconstruction, the New Deal settlement) versus whether it is a normative program dressed as a descriptive account, used selectively to legitimate outcomes that couldn't be won through ordinary doctrinal argument.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional meaning to track deep and sustained shifts in popular political will when the amendment process is practically unreachable (Article V's supermajority requirements) and judicial appointment turnover is too slow, allowing fundamental disagreements to be resolved through political contestation rather than either constitutional ossification or a small judicial elite unilaterally deciding contested questions.
% TRANSFER_FUNCTION: Moves ultimate interpretive authority from courts (where it nominally sits under judicial review) toward whichever political coalition can sustain electoral and mobilizational pressure long enough to reshape doctrine, appointments, or enforcement — transferring practical constitutional power from institutionally protected, less-numerous groups toward well-organized majorities and movements.
% ABSENT_VOICES: Minorities who lack mobilizing capacity comparable to organized political movements are structurally underrepresented in a framework whose whole mechanism is contestation-through-mobilization; they would object that this reading trades their one durable protection (counter-majoritarian judicial review) for a arena where they are systematically outgunned, but they have no equivalent avenue to press that objection within the framework itself.
% DISAPPEARANCE_RATIONALE: Proponents would say if popular constitutionalism norms disappeared, judicial supremacy would harden and courts would become the sole and final word, cutting off the historically real channel through which movements like abolition and civil rights forced constitutional change. Judicial finality advocates and protected minorities would say the underlying reality (courts already do respond to political pressure, sometimes) would persist regardless of which reading is named as correct — the dispute is over how much legitimacy to grant that responsiveness, not whether it exists.
% FOUNDING_PROBLEM: Article V's amendment process is nearly unreachable in practice (two-thirds of Congress plus three-fourths of states), yet the polity's understanding of fundamental rights and structure has changed dramatically across history (slavery, suffrage, federal economic regulation, criminal procedure) — this reading names the actual historical mechanism (sustained political mobilization forcing judicial and institutional accommodation) by which that change occurred, since the formal amendment process alone cannot account for it.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists outside constitutional law proper (e.g., historical accounts of Reconstruction's aftermath and the New Deal-era court fight) corroborate that political mobilization has historically produced constitutional change independent of, and sometimes despite, contemporaneous judicial doctrine. Judicial finality advocates and minority-rights scholars dispute the normative conclusion drawn from that history, arguing that descriptive accuracy about how change has happened does not establish that treating contestability as legitimate going forward is good for groups without mobilizing power.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, contested).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that this reading redistributes practical interpretive power away from institutionally insulated actors (courts, protected minorities) toward whoever can sustain political mobilization — a real transfer, not merely a coordination gain, even though the reading also solves a genuine problem (Article V's near-total unreachability). Suppression (0.44) is moderate: the reading does not forcibly bar judicial or originalist argument, but it does structurally deny counter-majoritarian minorities an alternative venue once their protection is treated as perpetually re-contestable — there is no clean exit from a arena that runs on mobilizing numbers you may not have. Accessibility collapse is low-moderate (0.35) because judicial-supremacist and originalist readings remain live, competing accounts; nothing about this reading forecloses them as a matter of doctrine, only as a matter of which reading prevails in a given political moment. Resistance is high (0.70) because judicial finality advocates, legal formalists, and protected-minority advocates actively contest the legitimacy of treating rulings as provisional.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular movements and legislative majorities are declared beneficiaries because the reading's entire structural logic assigns them a route to constitutional change that ordinary doctrine and Article V deny them — their directionality sits near the beneficiary end. Judicial finality advocates and counter-majoritarian-dependent minorities are declared victims because the same structural move that empowers movements simultaneously strips the protection minorities structurally depend on: judicial review's value to them IS its resistance to majoritarian reversal, and this reading treats that resistance as provisional. Settlement-reliant institutional actors are victims by a different mechanism — not ideological opposition, but exposure to planning uncertainty when constitutional baselines can shift under sustained political pressure regardless of doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Article V's practical unreachability leaving constitutional meaning unable to track deep political change) is genuinely contested as live or resolved: it remains live in the sense that formal amendment is still essentially unreachable, but reading it as licensing perpetual re-contestation of settled rights protections risks becoming a standing justification for majoritarian override well past the cases (Reconstruction, civil rights) that motivate the reading's descriptive claim. The tangled_rope classification captures this: there is a genuine coordination function (tracking real shifts in fundamental political commitment that Article V cannot process) bound to genuine asymmetric extraction (minorities without mobilizing capacity bear costs the mechanism does not compensate them for), and it requires active political enforcement (sustained mobilization, appointments strategy, non-enforcement threats) to operate at all — it is not self-sustaining coordination the way a pure rope would be.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    descriptive_versus_normative_conflation,
    'Is popular constitutionalism an accurate descriptive account of how constitutional change has actually occurred historically, or a normative program that borrows historical cases (abolition, civil rights) to legitimate contestability as an ongoing practice regardless of whose rights it currently unsettles?',
    'Comparative historical analysis of cases where sustained political mobilization successfully reshaped constitutional doctrine versus cases where it failed or produced rights retrenchment, assessed by scholars outside both the movements invoking the doctrine and the judiciary defending finality.',
    'If primarily descriptive and historically bounded to genuine constitutional-moment cases, the reading is closer to an accurate account of a real (if irregular) mechanism. If used as an open-ended normative license for ongoing majoritarian override of settled rights, the extraction from powerless minorities is closer to a permanent structural feature than a historically contingent one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(descriptive_versus_normative_conflation, conceptual, 'Whether popular constitutionalism describes rare historical ruptures or licenses ongoing majoritarian contestation of rights.').

omega_variable(
    mobilization_asymmetry_persistence,
    'Is the mobilizing-capacity asymmetry between organized majorities and vulnerable minorities a fixed structural feature of this reading, or does it vary enough across issues and eras that minorities sometimes hold the mobilizing advantage (e.g., well-organized advocacy coalitions defending narrow but salient rights)?',
    'Empirical mapping of mobilization outcomes across a range of contested constitutional questions to determine whether minority-protective coalitions have historically matched or exceeded majoritarian mobilizing capacity in specific domains.',
    'If the asymmetry is domain-general and persistent, victim status for counter-majoritarian-dependent minorities is structurally robust. If asymmetry varies significantly by issue, the extraction is more contingent and reading-specific victim/beneficiary assignments would need to be issue-indexed rather than uniform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobilization_asymmetry_persistence, empirical, 'Whether mobilizing-capacity asymmetry disadvantaging minorities is structural or issue-contingent.').

omega_variable(
    cs_framing_underdetermination,
    'Should this reading''s kernel be framed as ''who has final interpretive authority'' (institution-centered: court vs. political branches vs. movements) or as ''what makes a constitutional settlement legitimate'' (legitimacy-centered: the more abstract question of what grounds any interpretive claim''s authority at all)? The institution-centered framing was used here; a legitimacy-centered framing might yield a different cs_pattern by treating popular constitutionalism as a claim about legitimacy conditions rather than about which body''s rulings control.',
    'Compare classification outcomes under both framings: author a parallel cs_structure treating legitimacy-grounding as the kernel and check whether reading_relations and axiom foreclosure differ.',
    'Under the institution-centered framing (adopted here), authority_grounding is naturally ''distributed'' since no single institution is dispositive. Under a legitimacy-centered framing, authority_grounding might instead be ''practice'' (the practice of political contestation itself grounds legitimacy), which could change how axiom overriding interacts with drift_state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative kernel framings (institutional-authority-centered vs legitimacy-centered) could yield different cs_pattern classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1954, 0.18).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1973, 0.28).
narrative_ontology:measurement(us_c_tr_t1992, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1992, 0.32).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2010, 0.36).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1937, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1937, 0.42).
narrative_ontology:measurement(us_c_be_t1954, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1954, 0.38).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1973, 0.45).
narrative_ontology:measurement(us_c_be_t1992, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1992, 0.5).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1937, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1937, 0.3).
narrative_ontology:measurement(us_c_su_t1954, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1954, 0.25).
narrative_ontology:measurement(us_c_su_t1973, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1973, 0.32).
narrative_ontology:measurement(us_c_su_t1992, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1992, 0.36).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2024, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the us_constitution_interpretive kernel, each authored as a separate ε-invariant constraint per the decomposition principle. originalist_reading (meaning fixed at ratification) and living_constitution_reading (meaning evolves via judicial reasoning) are the other two family members; each carries its own extractiveness, beneficiary/victim structure, and claimed type. All three link to each other via affects_constraints because a shift in which reading holds practical dominance in a given era structurally changes the operating conditions (legitimacy, resource availability, doctrinal footing) for the other two — e.g., a period of strong popular-constitutionalist mobilization changes the practical stakes of originalist appointments strategy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
