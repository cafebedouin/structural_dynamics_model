% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__consensus_safeguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__consensus_safeguard_reading, []).

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
 *   constraint_id: supermajority_threshold__consensus_safeguard_reading
 *   human_readable: Constitutional Amendment Supermajority Threshold (Consensus-Safeguard Reading)
 *   domain: constitutional/political-economic/institutional-design
 *
 * SUMMARY:
 *   This story instantiates the consensus_safeguard_reading of the
 *   supermajority_threshold kernel: the claim that requiring supermajority
 *   assent — two-thirds of each legislative house plus three-quarters of the
 *   states in the United States configuration — ensures that constitutional
 *   change reflects deep, persistent democratic consensus rather than
 *   transient majoritarian passion. The standing arrangement under contest,
 *   and therefore the referent of epsilon, is the high amendment barrier as
 *   it actually operates, assessed by this reading's own lights: the reading
 *   regards the barrier as a democratic quality filter whose costs are the
 *   price of deliberation. The epsilon value is reading-indexed over that
 *   fixed referent; sibling readings (minoritarian_veto_reading,
 *   adaptive_gradient_reading) are separate constraint stories with their own
 *   epsilon, beneficiary/victim structures, and classifications, linked
 *   through network.affects_constraints. Claim and metrics are authored
 *   independently: the reading is claimed as tangled_rope because a genuine
 *   coordination function (credible precommitment, expectation stability) and
 *   asymmetric extraction (state-weighted blocking power, standing dilution
 *   of large-state and territorial voice) demonstrably coexist in the same
 *   structure, while the metrics describe the arrangement's actual operation
 *   without being tuned to any predicted engine output.
 *
 * KEY AGENTS:
 *   - entrenched_regional_minorities: Primary beneficiary (organized/constrained) — residents of sparsely populated states whose equal ratification suffrage converts historical privilege into operative blocking power
 *   - constitutional_stability_dependents: Diffuse beneficiary (moderate/constrained) — households, firms, and governments planning around fundamental law that does not swing with each election
 *   - incumbent_officeholders: Secondary beneficiary (powerful/arbitrage) — officeholders and invested interests shielded from structural revision at negligible personal cost
 *   - blocked_reform_majorities: Primary target (organized/trapped) — coalitions sustaining majority support across cycles whose objectives have no statutory substitute
 *   - large_state_residents: Target (moderate/constrained) — bear standing dilution of ratification weight whenever small-state opposition defeats proposals they support
 *   - unrepresented_territory_residents: Target (powerless/constrained) — live under the constitution with no ratification vote at all
 *   - federal_legislature_gatekeepers: Agenda-setter (institutional/arbitrage) — control proposal timing, mode, and deadlines while themselves bound by the two-thirds proposal requirement
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — produce the amendment-rate and blocking evidence every other seat cites
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__consensus_safeguard_reading, 0.48).
domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, 0.48).
domain_priors:theater_ratio(supermajority_threshold__consensus_safeguard_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__consensus_safeguard_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__consensus_safeguard_reading, "Constitutional Amendment Supermajority Threshold (Consensus-Safeguard Reading)").
narrative_ontology:topic_domain(supermajority_threshold__consensus_safeguard_reading, "constitutional/political-economic/institutional-design").

domain_priors:requires_active_enforcement(supermajority_threshold__consensus_safeguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__consensus_safeguard_reading, '3f625360-3acf-4ef9-814c-1c1cb3d8af65').
narrative_ontology:cs_kernel_codification('3f625360-3acf-4ef9-814c-1c1cb3d8af65', fixed_text).
narrative_ontology:cs_authority_grounding('3f625360-3acf-4ef9-814c-1c1cb3d8af65', lineage).
narrative_ontology:cs_interpretation_layer_present('3f625360-3acf-4ef9-814c-1c1cb3d8af65').
narrative_ontology:cs_reading_relation('3f625360-3acf-4ef9-814c-1c1cb3d8af65', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f625360-3acf-4ef9-814c-1c1cb3d8af65', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('3f625360-3acf-4ef9-814c-1c1cb3d8af65', foundational, amendment_barrier_filters_transient_passion).
narrative_ontology:cs_axiom_status(amendment_barrier_filters_transient_passion, holdable).
narrative_ontology:cs_axiom_grounding('3f625360-3acf-4ef9-814c-1c1cb3d8af65', amendment_barrier_filters_transient_passion, empirically_contingent).
narrative_ontology:cs_axiom('3f625360-3acf-4ef9-814c-1c1cb3d8af65', secondary, supermajority_delay_improves_deliberation_quality).
narrative_ontology:cs_axiom_status(supermajority_delay_improves_deliberation_quality, holdable).
narrative_ontology:cs_axiom_grounding('3f625360-3acf-4ef9-814c-1c1cb3d8af65', supermajority_delay_improves_deliberation_quality, instrumental).
narrative_ontology:cs_reference_frame('3f625360-3acf-4ef9-814c-1c1cb3d8af65', founding_precommitment_architecture).
narrative_ontology:cs_drift_state('3f625360-3acf-4ef9-814c-1c1cb3d8af65', contemporary_polarization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3f625360-3acf-4ef9-814c-1c1cb3d8af65', '2026-06-15T14:22:31Z').
narrative_ontology:cs_kernel_id(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, constitutional_stability_dependents).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, entrenched_regional_minorities).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, incumbent_officeholders).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, blocked_reform_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, large_state_residents).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, unrepresented_territory_residents).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, precommitment_credibility_doctrine).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, counter_majoritarian_difficulty_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households, firms, creditors, and local governments plan around fundamental law that does not change with each election cycle. They receive predictable rules for contracts, rights, and intergovernmental relations in exchange for accepting that some preferred changes arrive late or never. They cannot opt out of the constitutional order short of emigration, and they hold no direct instrument for adjusting the amendment rule itself.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_stability_dependents, beneficiary,
    moderate, generational, constrained, national).

% Residents of sparsely populated states cast ratification votes equal to those of the largest states; thirteen of them can defeat any proposed amendment. Their delegations helped write the rule and their legislatures are the operative blocking point. They gain durable shielding of existing arrangements — Senate composition, water and land-use regimes, rural representation — while bearing no proportionate share of the cost when nationally supported reforms stall.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, entrenched_regional_minorities, beneficiary,
    organized, generational, constrained, national).

% Officeholders and organized interests invested in current arrangements gain protection from structural revision: term limits, apportionment changes, and campaign-finance redesign all stall short of the required counts. They can pursue ordinary legislation through the same institutions regardless, so the high bar costs them little day to day while insuring their positions against adverse supermajorities.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, incumbent_officeholders, beneficiary,
    powerful, biographical, arbitrage, national).

% Coalitions that sustain majority support across multiple election cycles — equal-rights advocates, campaign-finance overhaulers, Electoral College reformers — watch proposals die short of the required thresholds, sometimes after decades of effort. Their objective has no statutory substitute: only the constitutional route reaches it. Leaving the route means abandoning the objective altogether.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, blocked_reform_majorities, payer,
    organized, biographical, trapped, national).

% Residents of populous states carry proportionally far less weight in the ratification formula than residents of small states — a resident of the largest state holds on the order of one-sixtieth the ratification weight of a resident of the smallest. They absorb this dilution every time a proposal they support fails on concentrated small-state opposition, and their only remedy is the very amendment process that dilutes them.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, large_state_residents, payer,
    moderate, biographical, constrained, national).

% Residents of the District of Columbia and the territories live under the constitution with no ratification vote at all: their jurisdictions are never counted toward the three-quarters requirement. Moving to a state would acquire them one, at the cost of home, work, and community. They are bound by provisions they have no formal channel to revise.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, unrepresented_territory_residents, payer,
    powerless, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__consensus_safeguard_reading, unrepresented_territory_residents, excluded).

% The national legislature decides whether proposed amendments reach the states, sets ratification deadlines, and chooses between proposal modes. Its members must themselves assemble two-thirds majorities to propose anything, so the body simultaneously administers the gate and stands behind it — it can bury proposals by inaction while remaining unable to lower the bar for ones it favors.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, federal_legislature_gatekeepers, agenda_setter,
    institutional, biographical, arbitrage, national).

% Comparative and domestic constitutional scholars track amendment rates, blocked proposals, and cross-national variation in amendment difficulty. They produce the datasets and histories that every other seat cites in argument; they collect nothing and pay nothing under the rule, and their assessments divide along the same lines as the disputing parties.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__consensus_safeguard_reading, entrenched_regional_minorities).
narrative_ontology:fixing_cost_class(supermajority_threshold__consensus_safeguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the precommitment problem: by requiring supermajority assent across separately elected bodies and states, it makes fundamental law credible to everyone who plans around it, prevents each electoral winner from rewriting the rules under which future elections occur, and forces geographically broad agreement before shared foundations move.
% TRANSFER_FUNCTION: Moves effective control over constitutional revision away from numerical majorities and toward supermajority coalitions weighted by state equality — transferring amendment authority from population-weighted majorities to state-weighted minorities wherever the two diverge, and transferring security of expectations to all parties in exchange.
% ABSENT_VOICES: Territory residents with no ratification vote, future generations bound by entrenched provisions, and blocked reform coalitions hold no seat in the ratification process. Historically, enslaved people and other disfranchised groups were governed by provisions the threshold protected and had no voice whatsoever in revision — the strongest recorded case of unanimity produced by exclusion rather than agreement.
% DISAPPEARANCE_RATIONALE: Under simple-majority amendment, fundamental law would swing with each unified government: election rules, court jurisdiction, and rights guarantees would be rewritten every cycle, courts would lose the fixed text they interpret, long-term contracts and intergovernmental programs would lose their anchor, and every organized interest would redirect resources toward capturing each successive two-year window. Arrangements across the entire institutional stack depend on the barrier's existence.
% FOUNDING_PROBLEM: The Articles of Confederation required unanimous state consent for revision and had deadlocked for years — the 1787 Convention itself operated beyond its mandate partly because revision was impossible. The drafters sought a middle point: harder to amend than ordinary statute, easier than unanimity, so fundamental law could correct demonstrated defects without lying exposed to factional capture.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era records (Madison's convention notes, Federalist 43 and 49) attest the deadlock-and-faction problem from outside any current beneficiary set; comparative constitutional scholarship (Lutz's amendment-rate compilations, Elster on precommitment) attests the stability function cross-nationally. Critics writing from outside the beneficiary set — the minoritarian-veto literature and institutional-reform scholarship — attest that the original calibration now operates differently than designed. No reliance is placed on beneficiary self-attestation.
narrative_ontology:disappearance_verdict(supermajority_threshold__consensus_safeguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__consensus_safeguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__consensus_safeguard_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__consensus_safeguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__consensus_safeguard_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__consensus_safeguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__consensus_safeguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__consensus_safeguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.48: through this reading's own lights the barrier delivers a real subsidy (credibility of fundamental law) while imposing real, unevenly distributed costs — persistent sub-threshold majorities are overridden, and ratification weight is diluted by state-equal counting. Suppression is authored at 0.48 as a raw structural property, unscaled by power or scope: the barrier does not suppress advocacy or statutory alternatives, but gatekeeping has hardened over the interval (congressional control of proposal mode, ratification deadlines, justiciability doctrines), and for constitution-level objectives the alternative set collapses entirely — hence accessibility_collapse at 0.55, reflecting full collapse for constitutional goals and open statutory substitutes for ordinary policy goals. Theater_ratio at 0.30: the rule performs exactly what it appears to do (block amendments), but deliberation-quality rhetoric has grown relative to actual filtering work as amendment traffic approaches a modern freeze. Resistance at 0.50: reform movements repeatedly contest the threshold itself, yet broad acquiescence persists and twenty-seven amendments prove the route navigable during consensus surges. The temporal series run on one shared seven-point grid (years since ratification: 0, 40, 80, 120, 160, 200, 240) so every tracked metric is authored at every examined time point; the t=240 endpoints are projections. Extractiveness dips at t=120 (Progressive Era successes restored perceived legitimacy after the antebellum peak, when the threshold shielded slaveholding entrenchments) then climbs monotonically through the modern polarization freeze.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute different types from identical structural data. From the agenda-setter and diffuse-beneficiary seats the arrangement presents as prudential design — Federalist 49's insistence that appeals to the people be neither too easy nor too frequent — a coordination mechanism they would rebuild if it vanished. From the trapped reformer seats the same structure operates as a lock with no key: a goal reachable by no other route, defeated indefinitely by coalitions representing a small population fraction. Large-state and territorial payers experience a third texture: not blocked campaigns but standing devaluation of their constitutional voice. The engine computes this per-seat divergence from power, exit, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: constitutional_stability_dependents receive diffuse subsidy (d near the beneficiary end), entrenched_regional_minorities are subsidized through the veto their suffrage buys, and incumbent_officeholders with arbitrage-grade exit sit nearest the beneficiary pole — the barrier insures their positions while costing them little. Victim declarations drive high directionality: blocked_reform_majorities are trapped (their objective has no substitute route), pushing them toward the full-target end; large_state_residents and unrepresented_territory_residents bear structural dilution with constrained exit. One override is declared: the sole institutional-power actor, federal_legislature_gatekeepers, is genuinely dual-positioned — it administers the gate (proposal timing, deadlines, mode selection) yet stands behind it (two-thirds needed to propose) — so the structural derivation from its absence in the beneficiary/victim lists would misplace it; d is overridden to 0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — escaping the Articles of Confederation's unanimity deadlock without exposing fundamental law to factional capture — is authored as contested, not dead: stability remains valuable, but the parties dispute whether the modern problem is passion (as designed for) or deadlock and entrenchment (what the freeze now exhibits). Because founding_problem_status is contested rather than dead, the mismatch consumer (dead-status x world_rearranges) does not fire a zombie flag; the mandate has degraded in calibration, not expired in function. The tangled_rope classification is what prevents mandatrophy misreading in both directions: labeling the threshold pure coordination would erase the documented minority-veto extraction layered onto the precommitment function; labeling it pure extraction would erase the genuine stability subsidy every seat continues to collect. The rising theater_ratio and extractiveness trajectories mark where the calibration-drift hypothesis (the adaptive_gradient sibling) would bite if confirmed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_dominant_effect_contestation,
    'Is the dominant structural effect of the amendment threshold consensus-filtering (this reading), minority entrenchment (the minoritarian_veto_reading), or calibration-dependent (the adaptive_gradient_reading)?',
    'Code every blocked amendment since ratification by supporter geography and support persistence: if most failures reflect narrow regional coalitions defeating persistent national majorities, the minoritarian account dominates; if failures reflect genuinely shallow or decaying support, this reading dominates; if outcomes track threshold calibration error in either direction, the gradient account governs.',
    'Resolution reallocates epsilon across the sibling stories: a minoritarian-dominant finding raises this reading''s effective extraction sharply and pushes classification toward the snare boundary; a filter-dominant finding lowers it toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_dominant_effect_contestation, conceptual, 'Which sibling reading of the supermajority_threshold kernel captures the threshold''s dominant effect.').

omega_variable(
    passion_consensus_discriminator,
    'Can ''transient majoritarian passion'' be distinguished from ''deep persistent consensus'' by any measurable signal available at decision time?',
    'Longitudinal tracking of amendment campaigns comparing proposal-time support with support a decade later (child labor amendment, Equal Rights Amendment, D.C. statehood): test whether sub-threshold support typically decays (filter functioning as claimed) or persists (filter blocking demonstrated consensus).',
    'If persistently supported proposals are common, the threshold''s legitimating distinction is empty, its delay is arbitrary, extraction rises, and the adaptive_gradient_reading''s calibration demand becomes the governing frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passion_consensus_discriminator, empirical, 'Whether the passion/consensus distinction the reading relies on is operationalizable.').

omega_variable(
    stability_attribution_confound,
    'Does constitutional stability causally follow from the amendment threshold, or from confounds such as judicial review, party-system structure, continental scale, and legal tradition?',
    'Cross-national regression of formal amendment difficulty on constitutional longevity controlling for judicial power, party fragmentation, regime age, and legal-family fixed effects.',
    'If attribution fails, the coordination half of the ledger shrinks, effective extraction rises, and the arrangement drifts from tangled_rope toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stability_attribution_confound, empirical, 'Causal attribution of constitutional stability to the threshold versus confounding institutions.').

omega_variable(
    blocking_coalition_population_share,
    'What share of the national population can, at minimum, block ratification under state-equal counting, and how has that share moved with demographic concentration?',
    'Apportionment arithmetic on census data: identify the smallest-population set of thirteen states sufficient to defeat ratification, sum their population shares, and repeat the computation at historical census points.',
    'Quantifies the standing minoritarian component inside this reading''s own frame; a shrinking blocking share strengthens the veto characterization and raises measured extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(blocking_coalition_population_share, empirical, 'Population share of the minimal ratification-blocking coalition over time.').

omega_variable(
    victim_episodic_vs_structural,
    'Are the threshold''s victims only episodic (specific blocked campaigns) or structural (standing dilution of large-state and territorial amendment weight)?',
    'Distinguish one-off blocking losses from permanently reduced per-capita amendment weight; test whether any amendment prioritizing populous states has ever passed against concentrated small-state opposition.',
    'An episodic-only victim set supports a rope-leaning reading with diffuse beneficiaries; standing dilution confirms a persistent victim set and holds the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_episodic_vs_structural, conceptual, 'Whether the victim set is episodic or structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__consensus_safeguard_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smt_csr_tr_t0, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(smt_csr_tr_t0, observed).
narrative_ontology:measurement(smt_csr_tr_t40, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(smt_csr_tr_t40, observed).
narrative_ontology:measurement(smt_csr_tr_t80, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement_basis(smt_csr_tr_t80, observed).
narrative_ontology:measurement(smt_csr_tr_t120, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 120, 0.16).
narrative_ontology:measurement_basis(smt_csr_tr_t120, observed).
narrative_ontology:measurement(smt_csr_tr_t160, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 160, 0.2).
narrative_ontology:measurement_basis(smt_csr_tr_t160, observed).
narrative_ontology:measurement(smt_csr_tr_t200, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 200, 0.26).
narrative_ontology:measurement_basis(smt_csr_tr_t200, observed).
narrative_ontology:measurement(smt_csr_tr_t240, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 240, 0.3).
narrative_ontology:measurement_basis(smt_csr_tr_t240, projected).

% Extraction over time
narrative_ontology:measurement(smt_csr_be_t0, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(smt_csr_be_t0, observed).
narrative_ontology:measurement(smt_csr_be_t40, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(smt_csr_be_t40, observed).
narrative_ontology:measurement(smt_csr_be_t80, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement_basis(smt_csr_be_t80, observed).
narrative_ontology:measurement(smt_csr_be_t120, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 120, 0.33).
narrative_ontology:measurement_basis(smt_csr_be_t120, observed).
narrative_ontology:measurement(smt_csr_be_t160, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 160, 0.36).
narrative_ontology:measurement_basis(smt_csr_be_t160, observed).
narrative_ontology:measurement(smt_csr_be_t200, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 200, 0.44).
narrative_ontology:measurement_basis(smt_csr_be_t200, observed).
narrative_ontology:measurement(smt_csr_be_t240, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 240, 0.48).
narrative_ontology:measurement_basis(smt_csr_be_t240, projected).

% Suppression requirement over time
narrative_ontology:measurement(smt_csr_su_t0, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0, 0.24).
narrative_ontology:measurement_basis(smt_csr_su_t0, observed).
narrative_ontology:measurement(smt_csr_su_t40, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 40, 0.27).
narrative_ontology:measurement_basis(smt_csr_su_t40, observed).
narrative_ontology:measurement(smt_csr_su_t80, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 80, 0.33).
narrative_ontology:measurement_basis(smt_csr_su_t80, observed).
narrative_ontology:measurement(smt_csr_su_t120, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 120, 0.36).
narrative_ontology:measurement_basis(smt_csr_su_t120, observed).
narrative_ontology:measurement(smt_csr_su_t160, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 160, 0.39).
narrative_ontology:measurement_basis(smt_csr_su_t160, observed).
narrative_ontology:measurement(smt_csr_su_t200, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 200, 0.44).
narrative_ontology:measurement_basis(smt_csr_su_t200, observed).
narrative_ontology:measurement(smt_csr_su_t240, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 240, 0.48).
narrative_ontology:measurement_basis(smt_csr_su_t240, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__consensus_safeguard_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__minoritarian_veto_reading).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'supermajority threshold' conflates three structurally distinct claims about the same fixed rule. The consensus_safeguard_reading (this file) authors epsilon for the barrier as a consensus filter; the minoritarian_veto_reading authors epsilon for the same barrier as minority entrenchment (higher epsilon, sharper victim set); the adaptive_gradient_reading authors epsilon for the barrier as a miscalibratable parameter (epsilon contingent on measured consensus-formation rates). Each story carries its own beneficiaries, victims, and claimed type; they are linked pairwise through network.affects_constraints because the safeguard reading supplies the mainstream legitimation that the veto reading attacks and the gradient reading re-describes — the upstream legitimacy claim is cited as evidence within both downstream contests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__consensus_safeguard_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
