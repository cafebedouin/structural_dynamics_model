% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Constitutional Meaning as Perpetual Democratic Contestation (Popular Constitutionalism Reading)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the popular constitutionalism reading of the
 *   basic_law_interpretive_authority kernel: constitutional meaning is
 *   treated as perpetually open to democratic re-contestation rather than
 *   settled by a terminal court ruling (judicial_supremacy_reading) or a
 *   terminal legislative act (parliamentary_sovereignty_reading). The
 *   coordination function is real — it prevents a single institution from
 *   permanently freezing contested constitutional questions against future
 *   majorities. The extraction is structural rather than a single villain's
 *   rent: whoever can sustain mobilization longest captures the ability to
 *   reopen settled questions, and that capacity correlates with
 *   organizational resources, not the merits of a claim. This is a distinct
 *   constraint from its sibling readings, not the same constraint viewed
 *   differently — the beneficiary/victim sets, the enforcement mechanism, and
 *   the extraction channel are all structurally different across the three
 *   readings, which is why they are three separate stories rather than one
 *   story with a measurement parameter.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.42).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.38).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Constitutional Meaning as Perpetual Democratic Contestation (Popular Constitutionalism Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, 'e5500a1f-48d3-48fe-bc69-12e808c28ecc').
narrative_ontology:cs_kernel_codification('e5500a1f-48d3-48fe-bc69-12e808c28ecc', distributed).
narrative_ontology:cs_authority_grounding('e5500a1f-48d3-48fe-bc69-12e808c28ecc', distributed).
narrative_ontology:cs_reading_relation('e5500a1f-48d3-48fe-bc69-12e808c28ecc', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e5500a1f-48d3-48fe-bc69-12e808c28ecc', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('e5500a1f-48d3-48fe-bc69-12e808c28ecc', foundational, no_institution_holds_terminal_interpretive_authority).
narrative_ontology:cs_axiom_status(no_institution_holds_terminal_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('e5500a1f-48d3-48fe-bc69-12e808c28ecc', no_institution_holds_terminal_interpretive_authority, conventional).
narrative_ontology:cs_axiom('e5500a1f-48d3-48fe-bc69-12e808c28ecc', foundational, constitutional_meaning_is_constituted_by_ongoing_popular_practice_not_ruling).
narrative_ontology:cs_axiom_status(constitutional_meaning_is_constituted_by_ongoing_popular_practice_not_ruling, holdable).
narrative_ontology:cs_axiom_grounding('e5500a1f-48d3-48fe-bc69-12e808c28ecc', constitutional_meaning_is_constituted_by_ongoing_popular_practice_not_ruling, conventional).
narrative_ontology:cs_reference_frame('e5500a1f-48d3-48fe-bc69-12e808c28ecc', distributed_contestation_baseline).
narrative_ontology:cs_drift_state('e5500a1f-48d3-48fe-bc69-12e808c28ecc', contemporary_polarized_mobilization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5500a1f-48d3-48fe-bc69-12e808c28ecc', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, mobilized_civic_coalitions).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, legislative_majorities_of_the_moment).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, social_movement_organizations).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, minority_rights_claimants_between_electoral_cycles).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, regulated_parties_facing_prolonged_legal_uncertainty).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_court_seeking_settlement).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, sovereignty_of_the_governed).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, constitution_as_living_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize referenda campaigns, mass mobilization, and sustained public argument to reopen constitutional questions that courts or legislatures had treated as settled. They gain standing and influence precisely because no institution can terminate the debate against them; their leverage depends on the contest staying open.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, mobilized_civic_coalitions, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, mobilized_civic_coalitions, agenda_setter).

% Pass statutes that press against or reinterpret constitutional boundaries, betting that sustained public support will let the reading stick even without a terminal court ruling in their favor. Their power rises when interpretive finality is denied to a rival institution, but falls if a future majority reopens the same ground against them.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, legislative_majorities_of_the_moment, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, legislative_majorities_of_the_moment, agenda_setter).

% Build long campaigns (decades, not election cycles) around contested constitutional meanings — abortion, suffrage extension, labor rights. They benefit from the absence of terminal adjudication because it keeps a path open for reversal or extension that a settled doctrine would foreclose.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, social_movement_organizations, beneficiary,
    organized, generational, mobile, national).

% Depend on a stable, judicially settled reading of a constitutional protection to be secure against a hostile electoral majority. Under this reading, the protection they hold today can be reopened and contested by the next mobilized coalition or legislative majority; they cannot buy finality, and their exit options (litigation, emigration, political organizing) are slow and costly relative to the speed at which contestation can shift against them.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, minority_rights_claimants_between_electoral_cycles, payer,
    powerless, biographical, trapped, national).

% Businesses, administrators, and lower courts must act under constitutional provisions whose meaning is permanently contestable rather than settled by a final tribunal. They bear compliance costs, litigation costs, and planning uncertainty because no institution can issue a ruling that reliably stays final across electoral and mobilizational cycles.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, regulated_parties_facing_prolonged_legal_uncertainty, payer,
    moderate, biographical, constrained, national).

% Issues rulings intended to resolve constitutional disputes, but under this reading its rulings are treated as one move in an ongoing contest rather than a terminal word — legislatures reopen the question, movements campaign against the holding, and subsequent majorities relitigate it politically. The court bears the reputational and functional cost of a settlement function it cannot actually discharge, though it retains the formal power to rule.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_court_seeking_settlement, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_court_seeking_settlement, excluded).

% Lack the organizational resources to sustain a multi-decade contestation campaign the way a well-funded movement or a legislative majority can. Their constitutional claims are formally eligible for the same perpetual contest as anyone's, but in practice they cannot compete on the terrain the reading presupposes — they would object that the framework structurally favors whoever can out-mobilize and out-last, not whoever has the stronger claim, but they are rarely positioned to make that argument inside the process itself.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, political_minorities_lacking_mobilization_capacity, excluded,
    powerless, biographical, trapped, national).

% Study how the popular constitutionalism model performs against judicial supremacy and parliamentary sovereignty models across jurisdictions, tracking whether perpetual contestability produces democratic vitality or chronic instability and rights insecurity.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps constitutional meaning open to revision by whichever democratic coalition can sustain the argument over time, preventing any single institution — court or legislature — from freezing contested questions against future majorities. This solves the genuine problem of a written constitution ossifying interpretations that later generations would reject.
% TRANSFER_FUNCTION: Moves interpretive security away from settled minorities and regulated actors who need finality, and toward whichever coalition can currently mobilize sustained public argument — legislative majorities, social movements, and organized civic coalitions. The transfer is temporal: security is taken from 'now' and given to 'whoever outlasts.'
% ABSENT_VOICES: Political minorities without durable mobilization capacity are formally included (anyone can contest) but structurally disadvantaged (contestation rewards organizational endurance, funding, and electoral timing they typically lack). They would object that 'perpetual contestability' quietly favors whoever can sustain a campaign longest, but they are rarely in a position to make that argument register within the same contest.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism's premise disappeared overnight and a terminal adjudicator were installed (court or legislature), settled rights would stop being reopenable by new coalitions, movement strategy would shift from sustained public contestation toward litigation or legislative capture of the terminal body, and regulated parties would gain planning certainty at the cost of losing the channel through which excluded groups currently press unresolved claims.
% FOUNDING_PROBLEM: Written constitutions risk two failure modes: judicial ossification (a court freezes an interpretation that later democratic majorities cannot dislodge) and legislative majoritarian overreach (a temporary majority entrenches its preferences as permanent constitutional meaning). Popular constitutionalism was theorized to solve both by denying either institution terminal authority and keeping meaning answerable to ongoing popular judgment.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside any advocacy coalition attest that the ossification and majoritarian-overreach problems the theory targets are real and recurring across jurisdictions. However, the same scholars note the theory's own remedy — perpetual contestability — produces a documented cost (chronic uncertainty for minorities and regulated actors) that beneficiary coalitions rarely foreground; no institutional beneficiary corroborates the cost side unprompted.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).
:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.28) and rises modestly (to 0.42) over the interval as the model matures and coalitions learn to exploit perpetual contestability strategically (reopening settled questions opportunistically rather than only on principle). Suppression is comparatively low (0.38 at end) because the model's whole premise is the absence of a suppressing terminal authority — but it is not zero, because organizational capacity itself functions as a suppressive filter on who can credibly contest. Theater ratio rises slowly (0.30 at end) reflecting the risk that 'ongoing democratic contestation' becomes a legitimating performance for outcomes actually driven by resource asymmetry between coalitions, rather than genuine popular deliberation.
 *
 * DIRECTIONALITY LOGIC:
 *   Organized coalitions (civic coalitions, movements, current legislative majorities) sit near the beneficiary end: they gain leverage precisely from the absence of a terminal adjudicator, and their mobile/constrained exit options reflect that they can enter and exit the contest strategically. Minority rights claimants and regulated parties sit near the target end: they need finality to plan or to feel secure, and this reading structurally denies it to them regardless of the merits of their claim. The constitutional court is a distinctive payer seat — it retains formal ruling power but cannot discharge the settlement function the office nominally serves, which is a cost born by the institution itself, not just by litigants.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ossification and majoritarian overreach) remains partly live — both failure modes recur across comparative cases — which is why founding_problem_status is 'contested' rather than 'dead.' This blocks a naive mandatrophy verdict: the arrangement is not simply a legacy mandate with no remaining function. But the corroboration record also shows the remedy's own cost (chronic uncertainty for the powerless and the regulated) is real and under-attested by the coalitions who benefit from perpetual contestability, which is exactly the asymmetry a tangled_rope classification is built to hold — genuine coordination function AND asymmetric extraction, both true at once, neither cancelling the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contestability_vs_capture_by_mobilization_capacity,
    'Does perpetual constitutional contestability actually serve democratic self-governance, or does it functionally transfer interpretive power to whichever faction has the greatest sustained organizational capacity, regardless of the popular merits of its constitutional claim?',
    'Comparative study of jurisdictions operating under popular-constitutionalist norms (e.g., episodes of sustained extra-judicial constitutional contestation) tracking whether prevailing readings correlate with organizational resources/duration of campaign versus measures of broad public support at the time of resolution.',
    'If contestability outcomes track organizational endurance more than popular support, the coordination story (democratic responsiveness) is substantially cover for extraction by whichever coalition can outlast rivals — pushing the classification toward a more extraction-dominant tangled_rope or even snare reading for the powerless-claimant seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contestability_vs_capture_by_mobilization_capacity, empirical, 'Whether contestability rewards popular legitimacy or organizational endurance.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Is ''popular constitutionalism'' cleanly distinguishable from ''parliamentary sovereignty'' in practice, or does a legislative majority''s sustained reinterpretation under this reading collapse into the parliamentary_sovereignty_reading whenever that majority happens to persist long enough to entrench its view?',
    'Track specific cases where a legislative majority''s constitutional reinterpretation, initially framed as one move in an ongoing popular contest, becomes de facto terminal because no subsequent countermobilization succeeds within a generation — assess whether this outcome is structurally distinguishable from parliamentary sovereignty ex post.',
    'If the readings converge in practice whenever one side simply outlasts the contest, the three-way kernel decomposition may need a fourth reading capturing the ''contestability that hardens into de facto sovereignty'' case, rather than treating the three readings as exhaustively distinct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Whether popular constitutionalism and parliamentary sovereignty are stably distinct in practice or only in theory.').

omega_variable(
    gridlock_cost_distribution_fairness,
    'Is the distribution of gridlock costs across multiple institutional sites (the reading''s distinguishing structural feature) actually more equitable than concentrating those costs in a single terminal authority, or does distribution simply make the costs harder to locate and contest?',
    'Compare aggregate social cost of prolonged constitutional uncertainty under this model against aggregate cost under judicial_supremacy_reading and parliamentary_sovereignty_reading jurisdictions, controlling for underlying social conflict intensity.',
    'If distributed gridlock costs are higher in aggregate but individually smaller and less visible, the popular constitutionalism reading may be preferred by beneficiary coalitions precisely because its costs are diffuse and hard to attribute — a preference-type omega with direct bearing on the theater_ratio trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gridlock_cost_distribution_fairness, preference, 'Whether distributed gridlock cost is genuinely lower or merely less visible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the single natural-language label 'who holds final constitutional interpretive authority under the Basic Law' per the ε-invariance principle: judicial_supremacy_reading (courts terminal), parliamentary_sovereignty_reading (legislature terminal), and this story, popular_constitutionalism_reading (no terminal authority; meaning perpetually contested). Each reading has a distinct beneficiary/victim structure and a distinct ε — this reading's ε (0.42 at interval end) reflects extraction via mobilization-capacity asymmetry rather than institutional capture, structurally different from the extraction channel in either sibling. All three are linked bidirectionally via affects_constraints because a shift in any one reading's institutional footing (e.g., a court asserting supremacy in practice) directly changes the operating conditions for the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
