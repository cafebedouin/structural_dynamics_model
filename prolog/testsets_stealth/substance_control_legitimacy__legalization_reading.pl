% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Legalization Reading: Adult Autonomy Bounded by Third-Party Harm
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This file instantiates the legalization reading of the
 *   substance_control_legitimacy kernel: the standing arrangement in
 *   jurisdictions where competent adults may lawfully possess and use
 *   specified substances, state authority confined to conduct that reaches
 *   others (impaired driving, diversion to minors, public nuisance), and
 *   supply running through licensed markets. Relative to the kernel's other
 *   readings the structural delta is decisive: users as such sit outside the
 *   harmed set (use is a protected exercise of autonomy, not a condition to
 *   be prevented), the constraint's operative edge is third-party harm, and a
 *   corporate layer enters through the legal market, whose revenue scales
 *   with consumption volume. The claim/metric split is deliberate: the
 *   arrangement is CLAIMED as tangled_rope — a genuine liberty-and-boundary
 *   coordination function carrying an asymmetric extraction layer — while the
 *   metrics are authored from the arrangement's observed operation, including
 *   rising corporate extraction as markets consolidate. Sibling readings are
 *   separate constraints linked in the network section, not folded into this
 *   file's epsilon. KEY AGENTS (by structural relationship): -
 *   adult_substance_users: primary beneficiary (moderate/mobile) — hold the
 *   autonomy the arrangement grants - licensed_market_corporations:
 *   concentrated beneficiary and de facto agenda-shaper (powerful/arbitrage)
 *   — collect the legal market's rents and fund the politics that shapes the
 *   line - impaired_driving_victims: primary third-party payer
 *   (powerless/trapped) — bear crash risks they cannot opt out of -
 *   secondhand_exposure_nonsmokers: payer (organized/constrained) — bear
 *   exposure costs with partial recourse through smoke-free rules -
 *   addiction_susceptible_consumers: extraction payer (powerless/trapped) —
 *   the class whose consumption is commercially cultivated past the
 *   competence premise - state_regulators_and_legislatures: agenda_setter
 *   (institutional/constrained) — draw licensing, age, and marketing lines;
 *   collect tax revenue - constitutional_courts: agenda_setter
 *   (institutional/analytical) — adjudicate the self/other boundary -
 *   neighborhood_residents_near_venues: excluded (moderate/constrained) —
 *   bear localized externalities, rarely seated in licensing decisions -
 *   public_health_epidemiologists: analytical observer
 *   (analytical/analytical) — measure third-party harm and market behavior
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.56).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.35).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Legalization Reading: Adult Autonomy Bounded by Third-Party Harm").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '0b815a63-c4fa-4a51-a827-48a57dd75f77').
narrative_ontology:cs_kernel_codification('0b815a63-c4fa-4a51-a827-48a57dd75f77', formalized).
narrative_ontology:cs_authority_grounding('0b815a63-c4fa-4a51-a827-48a57dd75f77', lineage).
narrative_ontology:cs_interpretation_layer_present('0b815a63-c4fa-4a51-a827-48a57dd75f77').
narrative_ontology:cs_reading_relation('0b815a63-c4fa-4a51-a827-48a57dd75f77', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('0b815a63-c4fa-4a51-a827-48a57dd75f77', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('0b815a63-c4fa-4a51-a827-48a57dd75f77', foundational, competent_adult_autonomy_over_ingestion).
narrative_ontology:cs_axiom_status(competent_adult_autonomy_over_ingestion, holdable).
narrative_ontology:cs_axiom_grounding('0b815a63-c4fa-4a51-a827-48a57dd75f77', competent_adult_autonomy_over_ingestion, deontological).
narrative_ontology:cs_axiom('0b815a63-c4fa-4a51-a827-48a57dd75f77', secondary, police_power_limited_to_other_regarding_harms).
narrative_ontology:cs_axiom_status(police_power_limited_to_other_regarding_harms, holdable).
narrative_ontology:cs_axiom_grounding('0b815a63-c4fa-4a51-a827-48a57dd75f77', police_power_limited_to_other_regarding_harms, deontological).
narrative_ontology:cs_reference_frame('0b815a63-c4fa-4a51-a827-48a57dd75f77', millian_self_other_boundary).
narrative_ontology:cs_drift_state('0b815a63-c4fa-4a51-a827-48a57dd75f77', contemporary_commercial_market_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0b815a63-c4fa-4a51-a827-48a57dd75f77', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, adult_substance_users).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, licensed_market_corporations).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, impaired_driving_victims).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, secondhand_exposure_nonsmokers).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, addiction_susceptible_consumers).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, millian_harm_principle).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, liberty_interest_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase and consume alcohol, cannabis, or other lawful substances for their own purposes. The arrangement guarantees that their choice to use or abstain is theirs to make, with the state stepping in only when their conduct reaches others. Within the arrangement they move freely in and out of the market; losing it would mean criminal liability for private choices.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, adult_substance_users, beneficiary,
    moderate, biographical, mobile, national).

% Produce, brand, and retail lawful substances at scale. Revenue scales with consumption volume, so product design, potency, and marketing budgets aim at heavier use. They fund ballot committees, lobbying, and trade associations that shape licensing and advertising rules, and can shift capital, brands, and formulations across jurisdictions when rules tighten.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, licensed_market_corporations, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, licensed_market_corporations, agenda_setter).

% Share roads with drivers under the influence of alcohol or drugs. They did not choose the risk and cannot price it; their recourse begins only after a crash, through prosecution and civil claims. Many are injured or bereaved before any remedy attaches.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, impaired_driving_victims, payer,
    powerless, biographical, trapped, national).

% Breathe smoke or vapor in homes, multi-unit housing, workplaces, and public spaces they do not control. Organized advocacy has won smoke-free indoor-air rules in many jurisdictions, but exposure in private homes and near venues persists, and moving away is costly.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, secondhand_exposure_nonsmokers, payer,
    organized, biographical, constrained, regional).

% Began using through lawful channels and progressed to compulsive use that survives price increases, health warnings, and their own stated intentions. High-potency products and loyalty pricing are aimed at exactly this segment. Stopping typically requires treatment, relapse is common, and their spending continues regardless.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, addiction_susceptible_consumers, payer,
    powerless, biographical, trapped, national).

% Write and administer licensing, age-verification, potency, advertising, and taxation rules; prosecute impaired driving; and collect excise revenue that funds public services. They face simultaneous pressure from industry to widen the market, from constituents to limit it, and from neighboring governments to harmonize.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_regulators_and_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Hear challenges asking whether a given measure regulates conduct that reaches others or punishes a choice that does not. Their rulings define where the line sits, and they can strike down measures on either side of it.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Live near clusters of retail outlets, consumption lounges, or grow operations. They bear noise, loitering, odor, and property-value effects, and are frequently informed of licensing decisions after approval rather than consulted before.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, neighborhood_residents_near_venues, excluded,
    moderate, biographical, constrained, local).

% Track emergency-department visits, traffic fatalities, dependence prevalence, and youth-use trends across jurisdictions and over time. They publish findings that feed legislative debate and litigation, and hold no stake in market outcomes.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_epidemiologists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__legalization_reading, licensed_market_corporations).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__legalization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws an administrable boundary between private conduct and legitimate public concern: adults' self-regarding choices about ingestion are removed from the criminal docket, freeing enforcement capacity for genuinely other-regarding harms — impaired driving, sales to minors, public nuisance — and replacing case-by-case moral litigation over private life with a stable rule.
% TRANSFER_FUNCTION: Moves decision authority over ingestion from the state to individuals; moves former illicit-market revenue into licensed corporate channels and public treasuries through taxation; and leaves the residual costs of use — crash injuries, secondhand exposure, treatment burdens — on third parties, households, and the heaviest consumers.
% ABSENT_VOICES: Those who bear diffuse harms before they aggregate into identifiable cases: residents near retail clusters learn of licenses after approval; children in households with a dependent user hold no seat anywhere; future heavy users not yet recruited by marketing are represented by no one in licensing or advertising proceedings, where industry presence is dense and continuous.
% DISAPPEARANCE_RATIONALE: If the line vanished overnight the world rearranges in whichever direction it collapsed: toward prohibition, large numbers of users become defendants overnight, licensed markets and their tax streams dissolve, and illicit supply refills the demand; toward no limit at all, age gates, licensing, and impaired-driving enforcement lose their doctrinal anchor and every paternalist measure becomes constitutionally available. Court dockets, public budgets, and market structures all depend on the line holding where it holds.
% FOUNDING_PROBLEM: Overbroad criminal law was punishing self-regarding conduct — filling dockets, fueling illicit markets, and being enforced selectively — without eliminating use, while the deeper question went unanswered: on what ground may the state coerce a competent adult about their own body?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: constitutional-law scholarship and judicial opinions articulate the boundary question independently of market interests; public-health ethics literature frames it as a standing governance problem; and prohibitionist organizations themselves attest the question is live — they dispute the answer, not the existence of the question. No party in the dispute denies that the boundary problem exists.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.56 with the referent fixed as the standing legalized arrangement, assessed by this reading's own lights: the autonomy core is not extraction by the reading's own endorsement, but the reading itself must count what its own terms condemn — corporate profit-maximization aimed at dependence-prone consumers, and third-party harms left unpriced until they cross the harm threshold. Suppression is 0.35 and is authored as a raw structural property, unscaled by power or scope: the arrangement suppresses state paternalism by design (that is its function, not a cost borne by the governed), and its residual suppressive force falls on unlicensed competitors and on third parties who cannot decline exposure. Theater_ratio is 0.35: early build-out was largely functional, but social-equity programs functioning as licensing fig leaves, industry-funded 'responsibility' campaigns, and warning labels nobody reads have grown the performative share. Accessibility_collapse is 0.35 — the alternatives (criminalization, public-health management) remain fully live in sibling jurisdictions and in rollback politics; nothing collapses on understanding. Resistance is 0.60: prohibitionist movements, federal-state conflict, neighborhood opposition, and industry resistance to marketing limits all press on the line continuously. The measurement series run on one shared seven-point grid (T=0 corresponds to the first broad recreational legalization era, roughly 2012; T=14 to the present) so every tracked metric is authored at every examined time point; trajectories are monotonic rather than cyclical, so no intermittent-reinforcement analysis applies.
 *
 * PERSPECTIVAL GAP:
 *   From the licensed_market_corporations seat the arrangement is a lawful market it helped design and defends through ordinary politics; from the impaired_driving_victims and addiction_susceptible_consumers seats the same arrangement is unpriced risk transfer and engineered compulsion. The regulator seat experiences it as a defensible line requiring constant maintenance against both prohibitionist rollback and commercial expansion. The engine computes these divergent per-seat classifications from power, exit, and directional position; the widest divergence in the story runs between the arbitrage-grade corporate seat and the trapped payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations put adult_substance_users near the subsidized end: the arrangement exists to protect their choice space, and declining the market costs them nothing the arrangement imposed. Licensed_market_corporations derive near-full-beneficiary directionality from the beneficiary declaration plus arbitrage-grade exit across jurisdictions. Victim declarations put impaired_driving_victims and addiction_susceptible_consumers near the full-target end, amplified by trapped exit; secondhand_exposure_nonsmokers sit somewhat less extreme because organized advocacy has won partial recourse. One override is declared: the institutional atom (state_regulators_and_legislatures, constitutional_courts) is set to 0.45 because the structural derivation has no beneficiary/victim data for administrators — they collect excise revenue on the benefit side and bear enforcement and administration costs on the cost side, landing near-symmetric with a slight beneficiary tilt from tax receipts. The derivation alone would fall back to a generic default for these seats; the override records their actual dual position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — where the police power stops — remains live, so no obsolescence flag attaches: the arrangement is not administering a dead mandate, and every novel product re-presents the boundary question. The tangled_rope claim guards against mislabeling in both directions. Reading the arrangement as pure coordination (the rope label the reading's own rhetoric invites) would erase the corporate extraction layer that a legal market scaling revenue with consumption volume predictably produces. Reading it as pure extraction (the prohibitionist label) would erase the genuine autonomy function that distinguishes this arrangement from criminalization and that its beneficiaries actually receive. Both halves are structurally present in the same channels: the same licensed market that delivers autonomy delivers engineered dependence, which is why the classification holds them together instead of splitting the arrangement into a benign core and a separable abuse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This file instantiates only the legalization reading of the substance_control_legitimacy kernel; which reading governs a given jurisdiction, and how would the victim set and epsilon assessment change under each sibling?',
    'Per-jurisdiction electoral, legislative, and judicial outcomes selecting among readings; cross-jurisdiction comparison of victim-set composition under whichever reading governs.',
    'Under the prohibition reading, users re-enter the victim set and measured extraction rises sharply; under the harm-reduction reading, the corporate layer is reframed as a manageable health cost and the autonomy axiom loses its foundational status in favor of a duty-to-minimize-harm premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings change the victim set and the assessment of the same referent.').

omega_variable(
    cultivated_competence_boundary,
    'Does dependence cultivated by legal-market product design and marketing void the competence premise for a substantial class of consumers, returning them to the protected set this reading otherwise excludes?',
    'Longitudinal cohort data on dependence onset among consumers recruited by high-potency products versus autonomous-choice patterns, plus litigation discovery of internal marketing and product-design documents.',
    'If competence fails broadly, the reading''s own terms license state intervention in the market and epsilon rises well above the authored value; if competence generally holds, the current profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultivated_competence_boundary, empirical, 'Whether commercially engineered dependence dissolves the competence premise the autonomy grant rests on.').

omega_variable(
    third_party_harm_attribution,
    'How much observed third-party harm (traffic fatalities, emergency-department burden, secondhand exposure) is caused by the legalized arrangement versus displaced from the illicit market it replaced?',
    'Difference-in-differences designs across legalization boundaries and staggered adoption timing, controlling for border spillover.',
    'If harms are largely displaced rather than added, the harm-side component of extraction shrinks and the profile moves toward the coordination pole; if harms are added, extraction grows and the third-party payer seats harden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_attribution, empirical, 'Attribution of third-party harm between the legal regime and the displaced illicit market.').

omega_variable(
    market_entrenchment_irreversibility,
    'Has the licensed market become entrenched enough that repeal is no longer a realistically available alternative, converting a maintained balance into a one-way ratchet?',
    'Tracking repeal attempts, capitalized license values, and state-budget dependence on excise revenue over successive fiscal cycles.',
    'Rising entrenchment raises accessibility collapse, narrows the live alternative set, and shifts the persistence mechanism from ongoing justification to accumulated sunk investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_entrenchment_irreversibility, empirical, 'Whether the legal market''s sunk capital has foreclosed repeal as a practical alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t2, substance_control_legitimacy__legalization_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__legalization_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(subs_tr_t7, substance_control_legitimacy__legalization_reading, theater_ratio, 7, 0.26).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__legalization_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__legalization_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(subs_tr_t14, substance_control_legitimacy__legalization_reading, theater_ratio, 14, 0.35).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(subs_be_t2, substance_control_legitimacy__legalization_reading, base_extractiveness, 2, 0.41).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__legalization_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(subs_be_t7, substance_control_legitimacy__legalization_reading, base_extractiveness, 7, 0.49).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__legalization_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__legalization_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(subs_be_t14, substance_control_legitimacy__legalization_reading, base_extractiveness, 14, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(subs_su_t2, substance_control_legitimacy__legalization_reading, suppression_requirement, 2, 0.46).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__legalization_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(subs_su_t7, substance_control_legitimacy__legalization_reading, suppression_requirement, 7, 0.36).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__legalization_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__legalization_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(subs_su_t14, substance_control_legitimacy__legalization_reading, suppression_requirement, 14, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% One colloquial concept — how societies should govern substance use — decomposes into three structurally distinct constraints sharing the kernel substance_control_legitimacy. Each reading assigns a different victim set and therefore a different epsilon over the same subject matter: the prohibition reading places users themselves in the victim set; this legalization reading removes users from the victim set and locates harm at the third-party boundary plus the corporate layer; the harm-reduction reading reframes the entire subject as managed public health. The family is linked pairwise rather than ordered, because each jurisdiction's choice among readings reshapes the evidence the others cite.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
