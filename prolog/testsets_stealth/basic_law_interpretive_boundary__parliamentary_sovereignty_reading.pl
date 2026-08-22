% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Basic Law Interpretive Boundary — Parliamentary Sovereignty Reading
 *   domain: constitutional/legal-political
 *
 * SUMMARY:
 *   This story instantiates the parliamentary sovereignty reading of the
 *   Basic Law interpretive boundary: the Knesset, as the elected chamber,
 *   holds ultimate authority to interpret and amend the Basic Laws by simple
 *   majority, including the power to re-enact legislation the Supreme Court
 *   has invalidated. On this reading the judiciary advises but does not bind;
 *   no external institution vetoes legislative will; the arrangement's one
 *   conceded external limit is Israel's international treaty obligations. The
 *   epsilon referent is this standing arrangement, assessed through the
 *   reading's own lights: enacting an elected majority's program is
 *   self-governance rather than taking, so base extraction is authored
 *   low-moderate rather than near-zero. The manifest's near-zero bin was
 *   refined upward to 0.26 because the reading's own lights register residual
 *   costs it does not deny — the conceded treaty friction, and the fact that
 *   the same simple-majority channel carrying majoritarian policy also
 *   carries the demotion of the Court and the exposure of electorally
 *   weightless groups. Claimed type and metrics are independent authored
 *   facts: the claim is tangled_rope; the engine computes per-seat
 *   classifications from the structural data, and any divergence between
 *   claim and computed type is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - - knesset_governing_coalition: Agenda-setter (institutional/arbitrage) — drafts Basic Law amendments and override legislation; collects interpretive authority directly
 *   - - general_electorate: Beneficiary with payer exposure (moderate/mobile) — its will enacts without judicial veto; pays the same freedom when its bloc loses
 *   - - supreme_court_justices: Target (institutional/identity_locked) — demoted to advisory function; bears the demotion through its constitutive professional role
 *   - - rights_dependent_minorities: Target (powerless/trapped) — protection reduced to whatever the prevailing majority tolerates
 *   - - knesset_opposition_factions: Target (organized/constrained) — procedurally powerless inside rules the majority writes
 *   - - international_treaty_bodies: Excluded external voice (institutional/arbitrage) — the one conceded external limit, seated outside the process
 *   - - constitutional_law_scholars: Analytical observer (moderate/analytical) — supplies the doctrinal analysis both camps cite, holds no vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.26).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.45).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Basic Law Interpretive Boundary — Parliamentary Sovereignty Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional/legal-political").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'a93f51a0-b408-4d31-ab2f-841a7ecd5cd3').
narrative_ontology:cs_kernel_codification('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3', formalized).
narrative_ontology:cs_authority_grounding('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3', practice).
narrative_ontology:cs_interpretation_layer_present('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3').
narrative_ontology:cs_reading_relation('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3', foundational, elected_chamber_holds_constituent_authority).
narrative_ontology:cs_axiom_status(elected_chamber_holds_constituent_authority, holdable).
narrative_ontology:cs_axiom_grounding('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3', elected_chamber_holds_constituent_authority, deontological).
narrative_ontology:cs_axiom('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3', secondary, judicial_review_subject_to_simple_majority_override).
narrative_ontology:cs_axiom_status(judicial_review_subject_to_simple_majority_override, holdable).
narrative_ontology:cs_axiom_grounding('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3', judicial_review_subject_to_simple_majority_override, conventional).
narrative_ontology:cs_reference_frame('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3', elected_chamber_constituent_sovereignty).
narrative_ontology:cs_drift_state('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3', contemporary_post_2023_reform_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a93f51a0-b408-4d31-ab2f-841a7ecd5cd3', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, general_electorate).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_governing_coalition).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_justices).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rights_dependent_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_opposition_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, general_electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the seat majority; drafts and passes Basic Law amendments and override legislation by simple majority, and controls the committee agenda that determines which interpretive questions ever reach the floor. Its interpretive positions become law without external ratification. Its exits are dissolution and elections, or reshaping procedural rules between elections; losing office returns it to ordinary opposition status.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_governing_coalition, agenda_setter,
    institutional, biographical, arbitrage, national).

% Votes at intervals and receives policy outcomes that track coalition bargains with no judicial veto interposed; when its preferred bloc loses, it lives under rules the other side wrote with the same freedom. Its exit is the ballot box — real but slow, and unavailable between elections.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, general_electorate, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, general_electorate, payer).

% Decide cases under Basic Laws whose final meaning the Knesset claims for itself. Under this arrangement their rulings bind only until the majority overrides them; appointments, jurisdiction, and budget all pass through the political branches. Leaving the bench ends their professional function, and their institutional self-concept is built on deciding finally — which this arrangement does not let them do.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_justices, payer,
    institutional, generational, identity_locked, national).

% Groups whose equal treatment depends on adjudication rather than electoral weight — Arab citizens, non-Orthodox religious streams, LGBTQ Israelis, asylum seekers. They cannot assemble a governing coalition, so their protection under this arrangement reduces to whatever the prevailing majority tolerates. Citizenship ties them to the jurisdiction; emigration is the only full exit and most cannot take it.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rights_dependent_minorities, payer,
    powerless, generational, trapped, national).

% Hold seats but no agenda control; their initiatives die in committee and their constitutional interpretations carry no weight against the majority's. Rights-protecting statutes they manage to pass can be amended away by a later majority they cannot check through the courts. Their remedy is winning a future election; until then they operate inside procedures the majority writes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_opposition_factions, payer,
    organized, biographical, constrained, national).

% UN treaty committees, the ICJ, and counterpart states monitor compliance with conventions Israel has ratified. They hold no seat in the Knesset's process; their instruments are reporting, adverse findings, and reciprocal measures. This reading itself concedes their obligations bind, which makes them the one external voice the arrangement accommodates.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_treaty_bodies, excluded,
    institutional, generational, arbitrage, global).

% Law faculties and the bar produce the doctrinal analyses that both camps cite; they hold no vote and no veto. Several hundred faculty signed public letters against the 2023 legislative package. Their influence runs through argument, students, and clerkship pipelines rather than through procedure.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, constitutional_law_scholars, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, electorally accountable final decision point for constitutional meaning: when Basic Laws and ordinary legislation conflict, or when institutions disagree about what the Basic Laws require, the elected chamber resolves the question by majority vote, and every citizen's preference registers through elections with no unelected veto interposed.
% TRANSFER_FUNCTION: Moves interpretive and amending authority over the Basic Laws from the judiciary to the parliamentary majority; moves policy outcomes toward coalition preferences; moves the burden of rights protection from courts to political mobilization and electoral competition.
% ABSENT_VOICES: International treaty bodies, Palestinian citizens of Israel, and the demoted judiciary have no formal seat in the amendment process; under this reading the Court participates only in an advisory capacity. Their objections register solely through extraparliamentary pressure — protest, treaty findings, professional-body statements — not through the procedure that decides.
% DISAPPEARANCE_RATIONALE: Every constitutional order locates ultimate interpretive authority somewhere. If the Knesset's ultimate authority vanished overnight, that authority would migrate — to the Supreme Court, if invalidation were treated as binding, or into chronic unresolved inter-branch conflict if no holder existed. Coalition legislative programs, the Court's caseload, and minority-protection strategies would all reorganize around whichever body held the final say.
% FOUNDING_PROBLEM: Israel never completed a formal constitution. The Harari compromise of 1950 resolved the impasse by directing the Knesset to enact the constitution chapter-by-chapter as Basic Laws, deliberately deferring both the question of a superior constitutional document and the question of whether the accumulating chapters would bind the legislature that wrote them. The founding problem was how to organize constituent authority without a completed constitution — and who would interpret the chapters meanwhile.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians attest the Harari resolution's deliberate deferral from the Knesset's own 1950 proceedings; the Supreme Court's United Mizrahi Bank decision (1995) attests on its face that the interpretive boundary had gone unsettled for forty-five years before the Court asserted review authority; comparative constitutional scholarship corroborates that the question of who finally decides remains open. None of these corroborating sources belongs to the benefiting parties.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.26 through this reading's own lights: the accountability function is real and the reading counts majoritarian enactment as self-governance, but the same channel demotes the Court and exposes trapped minorities, and the reading itself concedes treaty obligations as a binding limit — hence low-moderate, not near-zero. Suppression (0.45) is a raw structural property, unscaled by power or scope: maintaining judicial subordination requires active measures (court-curbing bills, the 2023 Reasonableness Standard Act, override proposals, appointment-control fights), not mere acquiescence. Theater ratio (0.25) is low-moderate: the authority claim is functionally operative, though the 2023 legislative push carried substantial coalition-messaging content alongside its functional core. Accessibility collapse (0.55): within this reading's frame the rival allocation collapses fairly completely — an unelected final veto is hard to justify once popular sovereignty is granted — but the middle position granting bounded judicial authority alongside Knesset ultimacy remains reachable, so alternatives are only partly closed. Resistance (0.75) is high and honest: weekly mass demonstrations, reservist service refusals, bar association petitions, business-elite statements, and hundreds of faculty signatures met the 2023 package; the powerless minority seats do not resist alone — their coalition with opposition factions, the Court, the bar, and the protest movement is what the resistance figure encodes. The measurement series run on one shared time grid (points 0, 5, 10, 15, 20, 25, 30) with every tracked metric authored at every point. The suppression_requirement series is authored deliberately: this story traces enforcement-capacity change — the build-up of override machinery from scattered court-curbing bills through the 2023 legislative package — so the enforcement trajectory is the dynamic, not noise around a static scalar. Base extractiveness and theater peak at point 25 and ease slightly at 30 (the October 2023 war paused the package and the Court struck the Reasonableness Act in January 2024), while suppression_requirement keeps climbing — enforcement machinery hardening even as the extractive operation stalls.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural facts. From the coalition seat the arrangement is restored democratic accountability: its platform enacts without veto, and obstruction reads as elitism. From the Court's seat the same structure strips the function its professional identity is built on, with appointments and budget held by the branch it would rule against. From the minority seats it is protection reduced to majority tolerance; from the opposition seats, procedural powerlessness with the ballot as the only remedy. The engine derives these divergent per-seat classifications from the power, exit, and role data — the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (general_electorate, knesset_governing_coalition) derive directionality near the beneficiary end: the arrangement subsidizes their will. Declared victims derive near the target end, amplified by exit conditions — the Court's identity_locked exit and the minorities' trapped exit push both toward the full-target end despite the Court's institutional power, while the opposition's constrained exit moderates but does not reverse its position. The electorate sits nearer symmetric than a pure beneficiary reading would suggest: it collects unvetoed enactment when its side governs and pays unvetoed enactment when it does not, which is why it carries a secondary payer role rather than an override. National spatial scope keeps verification comparatively feasible, moderating the scope amplification. International treaty bodies sit outside the domestic derivation entirely — an excluded seat whose leverage the reading concedes rather than an internal beneficiary or victim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — where constituent authority resides under the Harari deferral — is live, not dead: the interpretive boundary is the central unresolved question of Israeli constitutionalism, so no mandatrophy declaration is authored and the mismatch consumer finds status=live paired with verdict=world_rearranges, yielding no zombie flag. The tangled_rope claim earns its keep by blocking both symmetrical errors. Reading the arrangement as pure coordination erases who pays through it: the Court's demotion and the minorities' exposure flow through the same simple-majority channel that carries the accountability function, and holding the arrangement in place requires active enforcement against real resistance. Reading it as pure extraction erases the function the reading exists to protect: a final, electorally accountable decision point is a genuine solution to the inter-branch deadlock the Harari deferral created, and the reading's concession of treaty obligations marks a real limit no pure extraction story would draw. The classification preserves both halves because both are structurally present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the parliamentary sovereignty reading of the basic_law_interpretive_boundary kernel; would the structural classification survive under the judicial supremacy or balanced contestation readings, which allocate the same ultimate authority differently?',
    'Generate the sibling stories and compare computed per-seat classifications across the family; divergence localizes the dispute to the authority-allocation premise rather than to any empirical fact about Israeli politics.',
    'Under the judicial supremacy reading the beneficiary and victim sets invert — the coalition becomes the target and the Court the agenda-setter — and epsilon re-concentrates accordingly; under balanced contestation both seats split. Cross-reading comparison is the corpus''s measure of the kernel dispute itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this file is one reading of a three-reading kernel; the sibling files carry the other authority allocations.').

omega_variable(
    majoritarian_incidence_rotation,
    'Does simple-majority authority over the Basic Laws impose costs that rotate with electoral turnover (ordinary majority-rule incidence, supporting this reading''s low extraction assessment), or costs that persistently land on the same identity groups regardless of who governs (structural exposure of permanent out-minorities)?',
    'Longitudinal analysis of which populations bear the costs of Basic Law amendments across alternating governments since 1995: rotating incidence indicates majoritarian-normal operation; persistent incidence on Arab citizens, non-Orthodox streams, and other electorally weightless groups indicates structural concentration.',
    'Rotating incidence vindicates the reading''s near-self-governance framing; persistent incidence raises effective extraction sharply for trapped seats and pushes the arrangement toward snare-flavored classification despite the genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_incidence_rotation, empirical, 'Whether the costs of unrestrained majority amendment rotate or concentrate.').

omega_variable(
    treaty_carveout_binding_depth,
    'How deeply do international treaty obligations actually constrain Knesset action under this reading, given that the reading concedes they bind while the Knesset controls incorporation, implementation, and response to adverse findings?',
    'Compare compliance rates on ratified obligations before and after override-machinery episodes; track whether treaty-body adverse findings are followed by Knesset correction, disregard, or retaliatory restriction of treaty-body access.',
    'Shallow bindingness widens the arrangement''s effective scope beyond its self-declared limit and raises effective extraction for globally exposed seats; deep bindingness confirms the carve-out as a real external check and supports the reading''s own accounting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_carveout_binding_depth, empirical, 'Real constraintness of the one external limit this reading concedes.').

omega_variable(
    override_machinery_self_sustainment,
    'Once built, does the override and jurisdiction-limiting machinery sustain itself through ordinary politics, or does it require continuous active renewal against resistance?',
    'Track the suppression_requirement series past the 2023-2025 reform crisis: decay toward the pre-crisis baseline indicates episodic construction tied to a particular coalition; continued ratchet indicates self-sustaining machinery that outlives its builders.',
    'Self-sustaining machinery implies permanently elevated suppression and a hardened classification for all seats; episodic machinery keeps the arrangement nearer reversible coordination whose extraction tracks electoral luck.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(override_machinery_self_sustainment, empirical, 'Durability of the enforcement apparatus this reading''s operation requires.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.09).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 15, 0.11).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 25, 0.27).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 30, 0.26).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 5, 0.14).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 15, 0.17).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who decides what the Basic Laws mean' decomposes into three structurally distinct constraints, one per reading of the shared kernel. Each member has its own epsilon, its own beneficiary/victim structure, and its own classification; they are linked here rather than averaged into one story. The judicial supremacy reading is the upstream member (the standing practice since United Mizrahi Bank, higher empirical confidence as the operative arrangement); this parliamentary sovereignty reading is the contesting downstream member whose operation depends on displacing the upstream allocation; the balanced contestation reading overlaps this one on Knesset ultimacy while diverging on binding judicial interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
