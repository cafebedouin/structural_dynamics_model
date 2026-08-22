% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism Allocation of Interpretive Authority
 *   domain: legal/political
 *
 * SUMMARY:
 *   This story instantiates the popular_constitutionalism_reading of the
 *   us_constitution_interpretive kernel: the arrangement under which
 *   constitutional meaning is authored through popular political movements,
 *   legislative assertion, and interbranch contestation, with courts as
 *   participants rather than custodians of final meaning. Per the
 *   committer-frame rules, this file generates ONE reading as a clean,
 *   epsilon-invariant constraint — the sibling readings (originalist_reading,
 *   living_constitution_reading) are separate constraint stories linked
 *   through network.affects_constraints, and the contest between readings is
 *   routed to omega variables, not folded into this classification. The
 *   epsilon referent is the standing arrangement described here —
 *   contestation-based interpretive authority as it actually operates — not
 *   any rival allocation and not the reading's idealized self-portrait. Claim
 *   and metrics are independent authored facts: the claimed type
 *   (tangled_rope) reflects the structural belief that the arrangement
 *   possesses BOTH a genuine, irreplaceable coordination function AND
 *   asymmetric extraction through the same structure, actively maintained
 *   against the entrenched contrary practice of judicial supremacy; the
 *   metrics reflect descriptive assessment of that operation, untuned to any
 *   predicted engine output.
 *
 * KEY AGENTS:
 *   - popular_movements: agenda-setter and primary beneficiary (organized/constrained) — runs the contestation machinery, absorbs mobilization costs, collects doctrinal and statutory gains
 *   - legislative_majorities: secondary beneficiary (institutional/constrained) — converts weakened judicial finality into policy space, advantage lasting only while they hold majorities
 *   - anti_elitist_claimants: tertiary beneficiary (moderate/constrained) — gains constitutional access that does not require elite legal credentials
 *   - judicial_finality_advocates: primary target (institutional/identity_locked) — professional identity fused with judicial finality; the arrangement demotes their core premise
 *   - counter_majoritarian_minorities: primary target (powerless/trapped) — passive judicial shield replaced by a mobilization contest they are resourced to lose
 *   - stable_settlement_seekers: target with partial hedges (organized/constrained) — bears recurring uncertainty costs that follow them across forums
 *   - unmobilized_diffuse_interests: excluded voice (powerless/trapped) — priced out of constitutional voice by the mobilization test, absent from the debate
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) — sees the full structure, holds no stake in the allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.46).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.36).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.37).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.37).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism Allocation of Interpretive Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, '2efb9128-6155-4317-ae98-1ee7b364d785').
narrative_ontology:cs_kernel_codification('2efb9128-6155-4317-ae98-1ee7b364d785', fixed_text).
narrative_ontology:cs_authority_grounding('2efb9128-6155-4317-ae98-1ee7b364d785', practice).
narrative_ontology:cs_interpretation_layer_present('2efb9128-6155-4317-ae98-1ee7b364d785').
narrative_ontology:cs_reading_relation('2efb9128-6155-4317-ae98-1ee7b364d785', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2efb9128-6155-4317-ae98-1ee7b364d785', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('2efb9128-6155-4317-ae98-1ee7b364d785', foundational, popular_authorship_of_constitutional_meaning).
narrative_ontology:cs_axiom_status(popular_authorship_of_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('2efb9128-6155-4317-ae98-1ee7b364d785', popular_authorship_of_constitutional_meaning, deontological).
narrative_ontology:cs_axiom('2efb9128-6155-4317-ae98-1ee7b364d785', secondary, judicial_finality_not_entailed_by_review).
narrative_ontology:cs_axiom_status(judicial_finality_not_entailed_by_review, holdable).
narrative_ontology:cs_axiom_grounding('2efb9128-6155-4317-ae98-1ee7b364d785', judicial_finality_not_entailed_by_review, conventional).
narrative_ontology:cs_reference_frame('2efb9128-6155-4317-ae98-1ee7b364d785', popular_sovereign_continuous_authorship).
narrative_ontology:cs_drift_state('2efb9128-6155-4317-ae98-1ee7b364d785', contemporary_court_legitimacy_crisis, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2efb9128-6155-4317-ae98-1ee7b364d785', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_minorities).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, stable_settlement_seekers).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, departmental_review_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mass political movements — abolition, woman suffrage, labor, civil rights — claim that the people acting politically can say what the Constitution requires. They organize campaigns, push amendments and legislation, defy adverse rulings, and treat vindications as their own constitutional achievements. They absorb the costs of sustained mobilization and collect the doctrinal and statutory gains when contestation succeeds. Leaving the arrangement would mean abandoning the claim that their cause is a constitutional one, which is the core of their purpose.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, beneficiary).

% Congressional and state legislative coalitions assert independent authority to read the Constitution — through enforcement legislation, impeachment, confirmation fights, and refusal to acquiesce in rulings they reject. Weakened judicial finality widens their policy space. They cannot exit the constitutional order, and their advantage lasts only as long as they hold majorities, so today's winning coalition is tomorrow's losing side.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, constrained, national).

% Activists and claimants without elite legal credentials who press constitutional arguments through petitions, rallies, and political platforms rather than through Supreme Court litigation. They gain when constitutional standing does not require a lawyer's brief or a judge's invitation. Their access depends on finding allies and attention, which is never guaranteed.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Sitting judges, Supreme Court practitioners, and scholars whose professional standing rests on the proposition that courts have the last word on constitutional meaning. The arrangement demotes that proposition from settled fact to one contested claim among several. Their careers, clerkships, and institutional prestige are built inside the finality framework; abandoning it would mean renouncing the professional identity they have spent their lives constructing, so they defend it instead.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, identity_locked, national).

% Discrete and insular minorities whose historical protection strategy has been to appeal over the heads of majorities to courts willing to enforce constitutional limits. When interpretive authority shifts to whoever can sustain superior political organization, their shield depends on winning mobilization contests they are often resourced to lose. They cannot leave the jurisdiction, and their exposure persists across generations.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, counter_majoritarian_minorities, payer,
    powerless, generational, trapped, national).

% Commercial actors, regulated industries, and institutions that plan around predictable constitutional rules. Each reopening of a settled question imposes repricing, compliance churn, and litigation reserves. They hedge through private ordering and forum selection, but federal constitutional uncertainty follows them across every forum, so the hedge is partial and the costs recur.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, stable_settlement_seekers, payer,
    organized, biographical, constrained, national).

% People harmed by majoritarian policy whose interests are too dispersed to fund organizations, sustain campaigns, or attract coalition partners. The arrangement effectively conditions constitutional voice on mobilization capacity, which they lack. They are not seated in the academic and institutional debate about interpretive authority, though they would object that the debate prices them out of the Constitution.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, unmobilized_diffuse_interests, excluded,
    powerless, generational, trapped, national).

% Political theorists and comparative constitutionalists who study how different democracies allocate final interpretive authority — courts, legislatures, referenda, conventions. They take testimony from every seat, publish the comparisons, and hold no stake in which allocation prevails in the United States.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working answer to who may authoritatively say what the Constitution means between formal amendments: any institution or movement capable of sustaining a constitutional claim may press it, and meaning settles provisionally through political outcomes — elections, enactments, amendments, and occasional acquiescence — rather than through a single tribunal's monopoly. This lets the polity correct entrenched interpretive error (as Reconstruction followed Dred Scott and the New Deal settlement followed the Lochner-era Court) without waiting on the very institution whose error is at issue.
% TRANSFER_FUNCTION: Moves interpretive authority — and with it effective control over constitutional policy — from courts to movements, legislative majorities, and electoral coalitions. Moves the costs of reopened questions onto those who planned around the old answers. Moves the burden of protection onto minorities, who must now sustain political organization to retain what judicial enforcement previously supplied passively.
% ABSENT_VOICES: Unmobilized diffuse interests and minorities without organizational capacity would object that the arrangement installs a mobilization test for constitutional voice; they are present in the polity but absent from the debate, which is conducted among movements, legislators, judges, and academics who all possess organized capacity. Future generations, who inherit whatever settlement contestation produces without participating in it, are likewise unrepresented.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — if constitutional meaning reverted to exclusive judicial custody — the correction mechanism that produced Reconstruction after Dred Scott and the New Deal settlement after the Lochner era would become illegitimate by definition; movements would lose standing to claim constitutional warrant; legislative enforcement powers would shrink toward mere delegation; and the recurring legitimacy crises that follow unpopular rulings would intensify, since the only remaining responses to judicial error would be appointment politics and amendment supermajorities.
% FOUNDING_PROBLEM: Reconcile popular sovereignty with constitutional supremacy: if 'We the People' are the authors of the Constitution, on what ground does an unelected tribunal hold the final word on what the people's charter means — and how can the polity correct the tribunal when it entrenches error?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Alexander Hamilton defended judicial review in Federalist 78 while conceding its countermajoritarian character; Alexander Bickel, a defender of the Court, named 'the countermajoritarian difficulty' as the root problem in The Least Dangerous Branch; ratification-era departmentalists such as Jefferson and Jackson pressed the same reconciliation problem from the opposing side. Adversaries and allies alike attest that the problem the arrangement addresses is real and unresolved.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).
:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The interval maps 0-240 to roughly 1789-2029 CE. All three tracked series share one grid ({0, 40, 80, 120, 160, 200, 240}); the scalar base_properties values reflect the t=240 endpoint, which is projected (revival phase) and marked basis=projected. Extractiveness (epsilon 0.46 at endpoint) is moderate and wave-shaped: it peaks where contestation succeeds at scale and imposes transformation costs on losers (Reconstruction, t~80; New Deal aftermath, t~160) and troughs where judicial supremacy is consolidated and the arrangement lies dormant (t~200). Suppression_requirement tracks the same waves — enforcement peaks when the arrangement must be forced against entrenched judicial authority (military Reconstruction, court-packing confrontation) and collapses in dormancy, when nothing contests. Theater_ratio rises in dormancy (t~200, 0.44) as the arrangement survives mainly as academic performance, and falls when movements practice it. The trajectory is wave-driven rather than strictly cyclic: movement waves (abolition, suffrage, labor, civil rights) supply the oscillation, and the oscillation is a side effect of political cycle timing, not a designed reinforcement schedule — though actors can and do time constitutional confrontations to moments of maximum movement strength, which borders on strategic exploitation of the wave structure. Accessibility_collapse is low (0.30): the rival readings remain fully live and practiced, so understanding this arrangement forecloses nothing. Resistance is substantial (0.62): the federal bench, the Supreme Court bar, and the originalist academy actively defend judicial finality, and Cooper v. Aaron's canonical assertion of supremacy stands as standing institutional opposition. Suppression is authored as a raw structural property (0.36) and is NOT scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the popular_movements seat the arrangement is self-government realized — the same structure that costs others certainty delivers them constitutional agency. From the counter_majoritarian_minorities seat the identical structure is exposure: protection recast as a mobilization contest they are resourced to lose. From the judicial_finality_advocates seat it is expropriation of professional authority held under identity lock. The engine derives these divergent per-seat classifications from the structural data (role, power, exit); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (popular_movements, legislative_majorities, anti_elitist_claimants) pull those seats toward the beneficiary pole; their constrained exits keep them off the arbitrage end. Victim declarations push the three payer seats toward the target pole: counter_majoritarian_minorities sit nearest full-target (trapped, generational exposure), judicial_finality_advocates near-full (identity_locked — the fusion of professional self-concept with judicial finality amplifies their target weight beyond what bare role-declaration yields), and stable_settlement_seekers high-but-not-maximal (partial hedging damps, but federal constitutional uncertainty follows them across forums, so their exit stays constrained rather than arbitrage-grade). popular_movements carry a dual position — they run the arrangement and bear its mobilization costs — which keeps them off the pure-beneficiary pole despite their agenda-setting role. The unmobilized_diffuse_interests seat is authored as excluded, not victim: per the R3 ruling an authored absence feeds commentary and the consensus-provenance check, never a classification override. No directionality overrides are authored — the derivation chain from beneficiary/victim declarations plus exit options produces the correct relationships for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and externally corroborated, so nothing here resolves into mandate-outlived-function: the arrangement is not administering a dead problem theatrically. The tangled-rope structure is what blocks two symmetric misreadings. Reading the arrangement as pure coordination misses that its operation strips a passive shield from counter-majoritarian minorities and levies recurring uncertainty costs on settlement-seekers — extraction through the same structure that coordinates. Reading it as pure extraction misses that the coordination function is genuine and irreplaceable: no rival mechanism corrects entrenched judicial error at constitutional scale. The mismatch consumer finds founding_problem_status=live paired with disappearance_verdict=world_rearranges — no zombie flag; the arrangement earns its persistence the hard way, by doing something the alternatives cannot.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the us_constitution_interpretive kernel; which structural features would change under the sibling readings (originalist_reading, living_constitution_reading)?',
    'Cross-reading comparison of the three family stories: victim sets, epsilon, and enforcement profiles differ by reading; adopt a sibling''s axiom set and recompute the classification.',
    'Under originalist_reading, counter_majoritarian_minorities'' protection profile and stable_settlement_seekers'' cost profile invert; under living_constitution_reading, judicial_finality_advocates move from target toward beneficiary. The classification of THIS story is unaffected; the comparison locates the dispute in who holds interpretive authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one of three readings; siblings are separate constraints, not parameters of this one.').

omega_variable(
    mobilization_cost_exclusion,
    'Does contestation-based authority give effective voice to diffuse and unresourced interests, or does the mobilization requirement systematically exclude them?',
    'Compare constitutional-change participation and outcome access across interest-resource strata; identify which constitutional claims succeed absent organizational backing.',
    'If mobilization costs dominate, unmobilized_diffuse_interests behave as undeclared victims and the arrangement''s extraction rises sharply at powerless seats, adding snare-flavored pressure at those positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobilization_cost_exclusion, empirical, 'Whether the mobilization test for constitutional voice functions as inclusion or exclusion.').

omega_variable(
    judicial_protection_counterfactual,
    'Do minorities dependent on counter-majoritarian judicial protection actually fare worse when interpretive authority is contestation-based than under judicial finality?',
    'Natural experiments comparing minority-protective outcomes won by movement pressure plus legislation against court-only protection, and durability comparisons across regimes (statutory versus doctrinal civil-rights protections).',
    'If judicial protection is durable and superior, the victim declaration for counter_majoritarian_minorities strengthens and epsilon rises; if judicial protection is fragile without popular enforcement, part of the extraction attributed to this reading is misattributed — it belongs to underlying majoritarian pressure present under either allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_protection_counterfactual, empirical, 'Whether the counter-majoritarian-protection victim claim survives the counterfactual.').

omega_variable(
    stability_value_contest,
    'Is constitutional predictability an intrinsic good whose loss counts as extraction, or an elite-sectorial preference dressed as a neutral good?',
    'Conceptual analysis separating rule-of-law interests common to all seats from sectorial planning interests; survey which seats bear uncertainty costs and which monetize flexibility.',
    'If stability is sectorial, stable_settlement_seekers'' target weight drops and epsilon falls; if stability is intrinsic to rule of law, their weight holds and the uncertainty transfer counts fully.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stability_value_contest, conceptual, 'Framing dependence of the stability-cost component of extraction.').

omega_variable(
    founding_design_restoration_question,
    'Was departmental and popular interpretation the founding design — making this reading restorative of the original allocation — or a modern academic construction displacing settled practice?',
    'Ratification-era and early-republic practice scholarship: departmentalist statements (Jefferson, Jackson), the nullification controversy, pre-Marbury review practice, and the historiography of Cooper v. Aaron''s retrospective canonization of judicial supremacy.',
    'If restorative, the reading''s authority claims strengthen against the charge of innovation and its enforcement burden reads as recovery rather than imposition; if constructed, the reading carries the full burden of justifying displacement of long-settled practice, raising its effective enforcement cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_design_restoration_question, empirical, 'Whether the reading restores an original allocation or invents a new one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t80, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement_basis(us_c_tr_t80, observed).
narrative_ontology:measurement(us_c_tr_t120, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 120, 0.24).
narrative_ontology:measurement_basis(us_c_tr_t120, observed).
narrative_ontology:measurement(us_c_tr_t160, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 160, 0.21).
narrative_ontology:measurement_basis(us_c_tr_t160, observed).
narrative_ontology:measurement(us_c_tr_t200, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 200, 0.44).
narrative_ontology:measurement_basis(us_c_tr_t200, observed).
narrative_ontology:measurement(us_c_tr_t240, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 240, 0.37).
narrative_ontology:measurement_basis(us_c_tr_t240, projected).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t80, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 80, 0.52).
narrative_ontology:measurement_basis(us_c_be_t80, observed).
narrative_ontology:measurement(us_c_be_t120, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 120, 0.47).
narrative_ontology:measurement_basis(us_c_be_t120, observed).
narrative_ontology:measurement(us_c_be_t160, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 160, 0.51).
narrative_ontology:measurement_basis(us_c_be_t160, observed).
narrative_ontology:measurement(us_c_be_t200, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 200, 0.41).
narrative_ontology:measurement_basis(us_c_be_t200, observed).
narrative_ontology:measurement(us_c_be_t240, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 240, 0.46).
narrative_ontology:measurement_basis(us_c_be_t240, projected).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t80, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 80, 0.56).
narrative_ontology:measurement_basis(us_c_su_t80, observed).
narrative_ontology:measurement(us_c_su_t120, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 120, 0.49).
narrative_ontology:measurement_basis(us_c_su_t120, observed).
narrative_ontology:measurement(us_c_su_t160, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 160, 0.57).
narrative_ontology:measurement_basis(us_c_su_t160, observed).
narrative_ontology:measurement(us_c_su_t200, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 200, 0.22).
narrative_ontology:measurement_basis(us_c_su_t200, observed).
narrative_ontology:measurement(us_c_su_t240, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 240, 0.36).
narrative_ontology:measurement_basis(us_c_su_t240, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive__living_constitution_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'how the Constitution is interpreted' covers three structurally distinct authority-allocation constraints with different epsilon values and different victim sets. originalist_reading fixes meaning at ratification (low interpretive drift, high settlement value, victims among adaptive-governance claimants); living_constitution_reading routes evolution through reasoned judicial adaptation (victims among original-meaning adherents and settlement-seekers); popular_constitutionalism_reading (this file) routes authority through popular contestation (victims among finality advocates, settlement-seekers, and counter-majoritarian-dependent minorities). No member is cleanly upstream: each reading's persuasive success weakens the others' institutional position, so the edges record mutual structural influence rather than a dependency chain. Each story carries its own stable epsilon; none averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
