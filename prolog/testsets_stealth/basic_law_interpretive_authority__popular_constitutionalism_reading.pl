% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Open Contestation Regime for Constitutional Meaning (Popular Constitutionalism Reading)
 *   domain: constitutional law/political theory/institutional design
 *
 * SUMMARY:
 *   This story instantiates the popular-constitutionalism reading of the
 *   basic_law_interpretive_authority kernel: a constitutional order in which
 *   no institution — court or legislature — holds terminal authority over
 *   what the fundamental law means. Meaning is produced continuously by
 *   elections, social-movement campaigns, amendment politics, provisional
 *   judicial rulings everyone knows are revisable, and public argument; the
 *   arrangement's enforcement consists precisely in defeating every attempt
 *   by any single site to declare the contest closed. The claim and the
 *   metrics are independent authored facts: claimed_type records the
 *   structure I believe true (tangled_rope — a real coordination function
 *   joined to asymmetric extraction through the same mechanism), while the
 *   metric values record the arrangement's observed operation, including a
 *   rising extraction trajectory as mobilization capacity concentrates.
 *   Sibling readings (judicial supremacy, parliamentary sovereignty) are
 *   separate constraint stories with their own epsilon and victim sets; this
 *   file links them through network.affects_constraints and documents the
 *   decomposition in the dual-formulation note.
 *
 * KEY AGENTS:
 *   - mobilized_social_movements: primary beneficiary (organized/constrained) — converts open contestation into constitutional change
 *   - elected_representatives: agenda_setter and secondary beneficiary (institutional/arbitrage) — enforces openness and harvests it
 *   - discrete_and_insular_minorities: primary target (powerless/trapped) — re-fights every settlement each cycle
 *   - low_mobilization_citizens: target (powerless/trapped) — nominal co-authors, effective spectators bearing uncertainty costs
 *   - constitutional_courts: dual-positioned payer/beneficiary (institutional/identity_locked) — lost finality, kept agenda-setting
 *   - constitutional_theorists: analytical observer (moderate/analytical)
 *   - nonparticipating_subjects: excluded (powerless/trapped) — bound by meanings they had no seat in producing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.56).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.62).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Open Contestation Regime for Constitutional Meaning (Popular Constitutionalism Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional law/political theory/institutional design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, 'bc560047-d9d8-48aa-91a4-6fd7a98f74e7').
narrative_ontology:cs_kernel_codification('bc560047-d9d8-48aa-91a4-6fd7a98f74e7', fixed_text).
narrative_ontology:cs_authority_grounding('bc560047-d9d8-48aa-91a4-6fd7a98f74e7', distributed).
narrative_ontology:cs_reading_relation('bc560047-d9d8-48aa-91a4-6fd7a98f74e7', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('bc560047-d9d8-48aa-91a4-6fd7a98f74e7', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('bc560047-d9d8-48aa-91a4-6fd7a98f74e7', foundational, no_terminal_interpretive_authority).
narrative_ontology:cs_axiom_status(no_terminal_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('bc560047-d9d8-48aa-91a4-6fd7a98f74e7', no_terminal_interpretive_authority, deontological).
narrative_ontology:cs_axiom('bc560047-d9d8-48aa-91a4-6fd7a98f74e7', secondary, constitutional_meaning_perpetually_contestable).
narrative_ontology:cs_axiom_status(constitutional_meaning_perpetually_contestable, holdable).
narrative_ontology:cs_axiom_grounding('bc560047-d9d8-48aa-91a4-6fd7a98f74e7', constitutional_meaning_perpetually_contestable, instrumental).
narrative_ontology:cs_reference_frame('bc560047-d9d8-48aa-91a4-6fd7a98f74e7', popular_sovereign_continuing_authorship).
narrative_ontology:cs_drift_state('bc560047-d9d8-48aa-91a4-6fd7a98f74e7', contemporary_post_dobbs, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bc560047-d9d8-48aa-91a4-6fd7a98f74e7', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, mobilized_social_movements).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_representatives).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, discrete_and_insular_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, low_mobilization_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_courts).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_courts).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, anti_ossification_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Civil-rights, labor, religious, and progressive campaigns litigate, march, amend, and elect, and constitutional meaning has historically shifted along the lines their campaigns drew. The open arena is the only lever that has ever worked for political outsiders, so they stay in it; exiting means disbanding and accepting whatever settlement the remaining sites reach without them.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, mobilized_social_movements, beneficiary,
    organized, biographical, constrained, national).

% Legislators and presidents propose jurisdiction-stripping, control appointments, defy or embrace rulings as convenient, and refuse to treat any decision as final. They are the arrangement's working enforcers and simultaneously harvest contestability itself as campaign currency and policy discretion, regardless of which side wins any given contest. Leaving office exits the seat, not the constraint.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_representatives, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_representatives, beneficiary).

% Groups that lose democratic contests must re-fight every settlement in the next cycle; their protections last exactly as long as mobilized support for them does. Emigration or secession is not a realistic option, so their security consists in winning repeatedly, forever.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, discrete_and_insular_minorities, payer,
    powerless, biographical, trapped, national).

% Citizens without money, organization, legal skill, or attention bandwidth are nominally co-authors of constitutional meaning and effectively spectators. They absorb the uncertainty costs of shifting rules without collecting the meaning-making gains, and disengaging only deepens their irrelevance to the contest.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, low_mobilization_citizens, payer,
    powerless, immediate, trapped, national).

% Courts keep issuing rulings that settle disputes provisionally while knowing any decision can be reversed by appointment politics, amendment, or open defiance. They lost finality but retained agenda-setting power over which questions the polity fights about. Their professional identity is fused with being arbiters, so abdication is not an available exit; they continue interpreting inside a frame that demoted them.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_courts, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_courts, beneficiary).

% Scholars map the contest, testify, and supply arguments to every side. They collect no rents from the arrangement and bear none of its recurring costs; their seat is analytical, and their disagreements with each other track the sibling readings rather than any stake in this one's operation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_theorists, observer,
    moderate, generational, analytical, global).

% Disenfranchised residents, children, and future generations are bound by whatever meaning each contest settles but have no vote, movement, or seat in producing it. They would object that perpetual re-contestation lets every present majority re-decide questions that touch them most durably.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, nonparticipating_subjects, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__popular_constitutionalism_reading, elected_representatives).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of who may say what the fundamental law means without creating a single capturable point of failure: interpretive authority is distributed across elections, movements, legislatures, courts, and public discourse, so that freezing meaning against the governed requires winning everywhere at once rather than capturing one bench.
% TRANSFER_FUNCTION: Moves interpretive authority, and the costs of exercising it (attention, organization, litigation, uncertainty), from permanent institutional holders to whichever coalition can mobilize at a given moment; correspondingly moves security of expectations away from anyone who relied on a prior settlement, since every settlement is reopenable.
% ABSENT_VOICES: Nonparticipating subjects — disenfranchised residents, children, future generations — are bound by whatever meaning each contest settles and would object that perpetual re-contestation lets every present majority re-decide questions that touch them; they have no vote, movement, or seat. Losing minorities are present only as the next cycle's appellants.
% DISAPPEARANCE_RATIONALE: If the open-contestation arrangement vanished overnight — if some institution's rulings became terminal — social movements would lose their primary lever and reorganize around appointment politics or amendment campaigns; courts would either entrench as final arbiters or shrink toward advisory bodies; representatives would redirect constitutional argument from the public square to the terminal forum; and every question the arrangement keeps reopenable would lock at whatever settlement stood at midnight.
% FOUNDING_PROBLEM: How to bind a sovereign people to fundamental law without creating an unaccountable interpreter — the problem the founding generation met in the gap between 'the people govern' and 'someone must finally decide,' and that recurs whenever a small body claims the last word on what self-government permits.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: judicial-supremacy and parliamentary-sovereignty theorists engage the same founding problem as the reason their alternatives exist; minority-rights organizations attest the vulnerability side of it; and the historical record of court-curbing episodes and jurisdiction-stripping proposals attests that the problem recurs independently of any faction's advocacy. No party disputes that the problem exists — the dispute is over which arrangement answers it.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.56 (interval end): the arrangement genuinely coordinates — distributing interpretive authority across many sites removes the single capturable point that elite capture would need — but the capacity to contest (organization, funding, attention, legal skill) is unequally distributed, so the recurring costs of perpetual contest concentrate on seats that cannot contest effectively. Suppression is authored at 0.62 as a raw structural property, unscaled by power or scope: holding interpretive authority open is not self-executing; it requires actively defeating terminal claims through appointment warfare, court-curbing and jurisdiction-stripping proposals, selective defiance of rulings, and mobilization against unpopular benches. Theater_ratio 0.38: much participatory activity is functional (campaigns that actually move meaning), but plebiscitary ritual, symbolic hearings, and social-media constitutionalism are a growing performative share. Accessibility_collapse 0.35: the rival readings do not collapse — they remain fully live positions (this is precisely why the kernel is contested), so alternatives persist and the arrangement cannot rely on their absence. Resistance 0.60: judicial supremacists, parliamentary sovereigntists, and minority-rights advocates actively contest the arrangement. The three tracked metrics share one six-point grid (1954-2024); the suppression_requirement series is deliberately non-monotonic (Warren-backlash rise, Bork-era peak, 1990s lull, post-Dobbs surge) because enforcement intensity tracks terminal-claim frequency, not a ratchet. Receipt surface: the extraction's steady-state recipient is elected_representatives, who convert contestability itself into campaign currency and policy discretion regardless of which side wins a given contest; movement gains are contingent on winning, so no other seat captures reliably. Fixing the arrangement — installing any terminal authority — is prohibitive for the seats that could attempt it: it requires the very durable supermajority coalition the arrangement exists to prevent from forming.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute different types from identical structural data. From the mobilized-movement seat the arrangement looks like a rope: the open arena is the only lever that ever worked for outsiders, and its costs are the price of access. From the trapped-minority and spectator-citizen seats the same structure computes as substantially extractive: they supply the uncertainty and re-contestation costs and collect little. From the bench the arrangement is a demotion that leaves agenda power intact — a court that knows its rulings are provisional still chooses which questions the polity fights about. From the representative seat it is an opportunity structure. The engine derives these divergences from power, exit, and beneficiary/victim declarations; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   mobilized_social_movements derive low d (near-beneficiary): the open arena subsidizes exactly their mode of action. elected_representatives sit low-to-mid: they are simultaneously enforcers (agenda_setter) and collectors, so their derived d reflects partial subsidy plus administrative position. discrete_and_insular_minorities and low_mobilization_citizens derive high d (near-full-target): trapped exit, no arbitrage, costs recur every cycle. constitutional_courts net mid-high: loss of finality pushes toward target, retained agenda-setting pulls back toward beneficiary. constitutional_theorists sit near symmetric by construction of the analytical seat. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms reproduce these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binding a sovereign people to fundamental law without creating an unaccountable interpreter — is live, not dead: every generation surfaces questions (executive power, digital surveillance, bodily autonomy) that someone must authoritatively answer, and this arrangement's answer is 'everyone, continuously.' Because founding_problem_status is live and disappearance_verdict is world_rearranges, the mismatch consumer finds no zombie flag. Mandatrophy analysis guards against two mislabels: reading the arrangement as a snare ignores its real coordination function (plurality against capture — the reason movements defend it), and reading it as a pure rope ignores the capacity asymmetry through which the same openness taxes the immobile. Tangled_rope holds both truths. The classification would shift toward snare if the capacity-equality omega resolved adversely (contest capacity permanently concentrated), and toward rope if minority-protection outcomes matched the reading's own wager.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This file instantiates only the popular_constitutionalism_reading of kernel basic_law_interpretive_authority; the judicial_supremacy and parliamentary_sovereignty siblings are distinct constraints with distinct epsilon values and victim sets — does any downstream analysis correctly keep them apart?',
    'Cross-reading comparison across the linked family files, keyed on the terminality element where the readings structurally diverge.',
    'Averaging across readings would launder incompatible arrangements into one classification; the foreclosure edges mean no composite reading is available as a fallback.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame scoping: one reading, one constraint, one epsilon; siblings are separate stories.').

omega_variable(
    mobilization_capacity_equality,
    'Can contestation capacity (organization, funding, attention, legal skill) be equalized enough that the burden of perpetual contest stops concentrating on low-capacity seats?',
    'Compare extraction incidence across jurisdictions and eras with differing civic-infrastructure investment (union density, public financing, legal-aid capacity).',
    'If capacity is equalizable, epsilon trends toward the coordination-cost floor and the type drifts toward rope; if not, the asymmetry hardens and the classification acquires snare flavor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobilization_capacity_equality, empirical, 'Whether the extraction component of this tangled rope is contingent or structural.').

omega_variable(
    minority_protection_comparison,
    'Does open contestation protect discrete minorities better than a terminal court would (the reading''s own empirical wager), or expose them to repeated majoritarian defeat?',
    'Longitudinal comparison of minority-rights outcomes under popular-constitutionalist periods versus judicial-supremacist periods, controlling for baseline public opinion.',
    'Resolves the reading''s central legitimacy claim; adverse findings raise epsilon and push the computed classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_comparison, empirical, 'The reading''s core empirical bet about who protects minorities under each interpretive-authority arrangement.').

omega_variable(
    terminal_claim_recurrence,
    'Can the arrangement survive recurring terminal claims (court-curbing, packing, jurisdiction-stripping, nullification) indefinitely, or does each crisis resolve into one of the sibling readings?',
    'Track the suppression_requirement series against terminal-claim frequency; sustained escalation without resolution signals collapse into a sibling arrangement.',
    'Collapse into judicial supremacy would transfer this story''s victim set into a new configuration; because the readings foreclose one another, no hybrid survives — the arrangement either persists or becomes a sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminal_claim_recurrence, conceptual, 'Where the kernel contest lives: whether non-terminal interpretive authority is a stable equilibrium or a waystation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blia_popular_constitutionalism_tr_t1954, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1954, 0.18).
narrative_ontology:measurement(blia_popular_constitutionalism_tr_t1968, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1968, 0.22).
narrative_ontology:measurement(blia_popular_constitutionalism_tr_t1982, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1982, 0.26).
narrative_ontology:measurement(blia_popular_constitutionalism_tr_t1996, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1996, 0.3).
narrative_ontology:measurement(blia_popular_constitutionalism_tr_t2010, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 2010, 0.34).
narrative_ontology:measurement(blia_popular_constitutionalism_tr_t2024, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(blia_popular_constitutionalism_be_t1954, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1954, 0.36).
narrative_ontology:measurement(blia_popular_constitutionalism_be_t1968, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1968, 0.4).
narrative_ontology:measurement(blia_popular_constitutionalism_be_t1982, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1982, 0.44).
narrative_ontology:measurement(blia_popular_constitutionalism_be_t1996, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1996, 0.48).
narrative_ontology:measurement(blia_popular_constitutionalism_be_t2010, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(blia_popular_constitutionalism_be_t2024, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 2024, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(blia_popular_constitutionalism_su_t1954, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(blia_popular_constitutionalism_su_t1968, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1968, 0.45).
narrative_ontology:measurement(blia_popular_constitutionalism_su_t1982, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(blia_popular_constitutionalism_su_t1996, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1996, 0.48).
narrative_ontology:measurement(blia_popular_constitutionalism_su_t2010, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(blia_popular_constitutionalism_su_t2024, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: basic_law_interpretive_authority decomposes into three sibling readings because 'who interprets the constitution' covers structurally distinct arrangements with distinct epsilon values and victim sets (epsilon-invariance: measuring the kernel through the popular-constitutionalist arrangement versus a judicial-supremacist arrangement yields different extraction profiles — they are different constraints, not one constraint viewed twice). This file is the popular_constitutionalism_reading. The family's ordering runs through terminality: whichever reading captures terminality absorbs the others' contestation traffic. Links here are family edges documenting the decomposition, not independent causal claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
