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
 *   human_readable: Legalization Reading: Adult Autonomy with Third-Party Harm Limit
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the legalization reading of the
 *   substance_control_legitimacy kernel: the standing arrangement in which
 *   competent adults hold autonomy over substance use and state authority is
 *   confined to preventing third-party harm. The arrangement has a two-faced
 *   structure. Its coordination face is real: it draws the
 *   self-regarding/other-regarding boundary that ends criminal punishment of
 *   use, replaces illicit supply with a licensed, taxed, quality-controlled
 *   channel, and concentrates residual coercion on demonstrable externalities
 *   such as impaired driving. Its extraction face is equally real and
 *   growing: a profit-driven legal market engineers dependence, targets
 *   marketing at heavy and prospective users, and leaves third-party victims
 *   uncompensated, which is exactly the structural delta this reading was
 *   expected to carry. The epsilon referent is the legalized arrangement
 *   itself, assessed by this reading's own lights: the reading holds autonomy
 *   foundational and therefore scores user-facing extraction far below
 *   prohibition levels, while conceding the third-party-harm burden and the
 *   corporate-capture dynamic observable in its own operation. The sibling
 *   readings (prohibition, harm reduction) are separate constraints in
 *   separate files, linked through network.affects_constraints; their content
 *   is deliberately excluded from this story's metrics and classification.
 *
 * KEY AGENTS:
 *   - - competent_adult_users: Primary beneficiary (organized/mobile) — hold the autonomy the arrangement protects
 *   - - licensed_market_industry: Concentrated beneficiary and de facto rule-shaper (institutional/arbitrage) — collects the legal market's rents
 *   - - third_party_harm_victims: Primary target (powerless/trapped) — bear unconsented costs of others' use
 *   - - vulnerable_heavy_users: Dual-positioned target-beneficiary (powerless/trapped) — finance industry margins through dependence
 *   - - state_regulatory_enforcement_agencies: Agenda setter (institutional/constrained) — administers the boundary
 *   - - prohibitionist_advocacy_movements: Excluded premise-holder (organized/constrained) — contests the arrangement from outside its terms
 *   - - public_health_analysts: Analytical observer (analytical/analytical) — sees the full structure, decides nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.52).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.46).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Legalization Reading: Adult Autonomy with Third-Party Harm Limit").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, 'db963dde-7b9a-4fa7-91b0-a007ef42d6ab').
narrative_ontology:cs_kernel_codification('db963dde-7b9a-4fa7-91b0-a007ef42d6ab', formalized).
narrative_ontology:cs_authority_grounding('db963dde-7b9a-4fa7-91b0-a007ef42d6ab', lineage).
narrative_ontology:cs_interpretation_layer_present('db963dde-7b9a-4fa7-91b0-a007ef42d6ab').
narrative_ontology:cs_reading_relation('db963dde-7b9a-4fa7-91b0-a007ef42d6ab', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('db963dde-7b9a-4fa7-91b0-a007ef42d6ab', substance_control_legitimacy__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('db963dde-7b9a-4fa7-91b0-a007ef42d6ab', foundational, competent_adult_bodily_self_determination).
narrative_ontology:cs_axiom_status(competent_adult_bodily_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('db963dde-7b9a-4fa7-91b0-a007ef42d6ab', competent_adult_bodily_self_determination, deontological).
narrative_ontology:cs_axiom('db963dde-7b9a-4fa7-91b0-a007ef42d6ab', secondary, self_regarding_criminalization_net_harmful).
narrative_ontology:cs_axiom_status(self_regarding_criminalization_net_harmful, holdable).
narrative_ontology:cs_axiom_grounding('db963dde-7b9a-4fa7-91b0-a007ef42d6ab', self_regarding_criminalization_net_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('db963dde-7b9a-4fa7-91b0-a007ef42d6ab', millian_self_regarding_autonomy_baseline).
narrative_ontology:cs_drift_state('db963dde-7b9a-4fa7-91b0-a007ef42d6ab', contemporary_corporate_market_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('db963dde-7b9a-4fa7-91b0-a007ef42d6ab', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adult_users).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, licensed_market_industry).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, third_party_harm_victims).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, vulnerable_heavy_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, vulnerable_heavy_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use alcohol, cannabis, nicotine, and other lawful substances as a matter of ordinary life. The arrangement guarantees their access without criminal exposure and leaves moderation choices to them. Their protection depends on the boundary holding against both prohibitionist rollback and industry practices that would recruit them into dependence. Ceasing use or relocating to a differently ruled jurisdiction is genuinely available to the moderate majority, though relocation is costly.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adult_users, beneficiary,
    organized, biographical, mobile, national).

% Produces, advertises, and retails lawful substances under license. Funds the lobbying, ballot committees, and sponsored research that shape the rules governing its own conduct. Profits scale with heavy use, so marketing concentrates on frequent users and prospective new entrants. Operates across many jurisdictions and can shift investment toward the most permissive regulators; domestic exit is unnecessary because its influence makes the rules tractable.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, licensed_market_industry, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, licensed_market_industry, agenda_setter).

% Bear injuries, bereavement, and medical costs imposed by other people's use: impaired drivers, secondhand smoke in shared housing and workplaces, neglect and violence associated with another's intoxication. They never consented to the risks they carry, are diffuse and unorganized, and cannot opt out of shared roads, air, and households. Compensation runs through tort and insurance systems that rarely make them whole.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, third_party_harm_victims, payer,
    powerless, biographical, trapped, national).

% A minority of users whose consumption is frequent, escalating, or dependent. Legality spares them criminal records and gives access to safer product and to treatment without confessing a crime, but the same open market supplies engineered products, round-the-clock availability, and marketing tuned to their habits. Physiological dependence makes stopping costly regardless of what the law permits, and their recurring purchases finance the industry's margins.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, vulnerable_heavy_users, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, vulnerable_heavy_users, beneficiary).

% Administer the boundary: licensing and inspection of producers and retailers, age verification, advertising limits, impaired-driving enforcement, and tax collection. Statute defines their mandate; they cannot abandon third-party-harm enforcement, nor expand into policing competent adults' use, without legislative change. Agency budgets depend in part on the fee and excise streams the legal market generates.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_regulatory_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Campaign continuously in elections, legislatures, and ballot initiatives to recriminalize use or roll back market freedoms, arguing that self-harm is a proper object of state authority. Their organizing is loud and well funded, but the operative framework grants no legitimate standing to their core premise: within this arrangement, criminalizing competent adults' private use is precisely what state authority may not do. They contest the boundary from outside its terms.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibitionist_advocacy_movements, excluded,
    organized, generational, constrained, national).

% Track impairment rates, dependence prevalence, youth uptake, and market concentration across jurisdictions; publish the evidence that both defenders and opponents of the arrangement cite. Hold no stake in the arrangement's survival and no power to alter it; their findings discipline the debate without deciding it.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__legalization_reading, licensed_market_industry).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws and polices the boundary between self-regarding and other-regarding conduct: reserves chemical self-determination to competent adults, channels supply through a licensed and taxed market that displaces illicit production, and concentrates residual state coercion on demonstrable third-party harms such as impaired driving, exposure of non-consenting others, and sales to minors.
% TRANSFER_FUNCTION: Moves enforcement effort off users and onto third-party-harm prevention and market-rule compliance; moves consumer spending from illicit to licensed suppliers, splitting it between corporate margins and tax receipts; and moves the costs of other people's use, including crash injuries, secondhand exposure, and family burden, onto victims who did not consent to bear them.
% ABSENT_VOICES: Third-party victims appear in the statistics but not at the bargaining table: no seat negotiates on their behalf, and compensation is an afterthought of tort and insurance rather than a designed term of the arrangement. Future users recruited by marketing before they can consent competently have no seat at all. Prohibitionist movements speak loudly, but their core premise that self-harm is a legitimate object of state coercion is excluded from the operative framework by design; they contest the boundary from outside its terms.
% DISAPPEARANCE_RATIONALE: If the autonomy boundary vanished overnight, criminal enforcement machinery would rebuild around use itself, millions of otherwise-lawful users would acquire records, supply would migrate back to illicit markets with attendant violence and product poisoning, and the licensed industry would collapse or go underground, while third-party harms would continue unabated and unmanaged by any dedicated enforcement channel.
% FOUNDING_PROBLEM: Mass criminalization of private substance use: discriminatory enforcement, poisoned illicit supply, and incarceration of users whose conduct harmed no one but themselves, combined with the unresolved need to respond collectively to harms users impose on others.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: transportation-safety boards and insurer actuarial data attest that third-party harms remain live and measurable; criminal-justice researchers and civil-liberties litigators attest the overcriminalization problem the arrangement was built to solve; emergency-department surveillance attests the dependence burden the legal market now generates. No party disputes that the underlying problems exist; the contest is over which reading of the kernel addresses them.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.52: the autonomy core keeps the arrangement far less extractive than criminalization for the median user, but the legal market converts dependence into recurring revenue and third-party victims carry uncompensated costs, so the reading's own honest accounting lands slightly above midpoint. Suppression 0.46: coercion is scoped to third-party harms, age gates, advertising limits, and illicit-supply remnants rather than use itself, yet the enforcement apparatus must grow to hold the boundary against both prohibitionist rollback and industry overreach. Theater 0.34: licensing and impaired-driving enforcement are functional, but industry-funded responsibility campaigns, token age-gate compliance, and self-regulation pledges occupy a rising share of the arrangement's visible activity. Accessibility collapse 0.25 is deliberately low: the sibling readings remain live, since several substances stay prohibited inside the same jurisdictions that legalize others and harm-reduction hybrids are spreading, so understanding this arrangement does not close off its alternatives. Resistance 0.60 reflects continuous prohibitionist counter-mobilization, international treaty friction, and libertarian objection to the regulatory apparatus itself. All three temporal series run on one shared grid (t=0 to 30, step 5) so no metric borrows another's end-state values. On the receipt surface: the legal market's rents demonstrably accrue to the licensed industry seat, and correcting the extraction asymmetry is prohibitively costly for the legislature that could act, given campaign-finance dependence, excise-revenue reliance, and the entrenchment of the autonomy core.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical statutory text. From the moderate-user seat the arrangement is a liberty guarantee: conduct that was recently a crime is now ordinary life, and the visible coercion aims elsewhere. From the dependent-user seat the same open market is a predation surface: availability, product engineering, and marketing are tuned to exactly their consumption pattern, and dependence removes the exit the autonomy language presupposes. From the third-party-victim seat the arrangement is an externality ledger: other people's freedom arrives as their uncompensated injury, and compensation systems rarely close the gap. The agenda-setter seat experiences administration and budget dependence; the excluded prohibitionist seat experiences a framework that hears its campaigns but grants its premise no legitimate standing. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent adult users sit near the beneficiary end: the arrangement subsidizes their autonomy and their exit (ceasing use, relocating) is genuinely available to the moderate majority. Licensed industry sits nearest the beneficiary pole on relationship, since the arrangement is the condition of its existence; its institutional power and arbitrage-grade mobility amplify whatever leverage it exercises through the market, but that operates through power, not through a shifted d. Third-party harm victims sit near the full-target end: they bear the arrangement's residual costs without consent, are diffuse, and cannot exit shared roads, air, and households. Vulnerable heavy users are the deliberate override case: the derivation reads a named victim at high d (roughly 0.9), but this seat receives real offsetting benefits, including no criminal record, safer product, and treatment access without confession, so d is overridden down to 0.72, keeping them firmly on the target side while honoring the dual position. State agencies are administered by the arrangement rather than collecting through it; their excise-budget dependence is recorded in the situation text, not converted into beneficiary status.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this arrangement as pure rope would erase its victims: dependence-engineered users and unconsenting third parties are structurally real, and the extraction flowing through the legal market is not coordination overhead. Classifying it as snare would erase exactly what distinguishes this reading from prohibition: users are not the victim set here, which is the structural delta, and the autonomy function is genuine, heavily used, and defended by its beneficiaries. Tangled rope holds both halves. On mandatrophy: the founding problem of mass criminalization is live but receding as legalization spreads, while a successor extraction vector of corporate dependence engineering grows inside the arrangement's own protection; the mandate is mutating rather than dead, so no zombie flag is asserted. The drift risk worth watching is subsystem-level: if third-party-harm enforcement atrophies while industry self-regulation theater expands, the enforcement subsystem drifts toward piton dynamics even while the autonomy core remains load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the legalization reading of the substance_control_legitimacy kernel; what structural differences would instantiating the sibling readings produce?',
    'Generate and classify the sibling stories (substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading) and compare victim sets, beneficiary structures, and computed types across the kernel family.',
    'Under the prohibition reading the victim set expands to include all users and epsilon rises sharply; under the harm reduction reading the user/beneficiary boundary blurs as state authority extends to self-regarding harm minimization. Cross-reading divergence locates the kernel''s contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story is one reading of a three-reading kernel; sibling deltas are recorded here rather than folded into this constraint.').

omega_variable(
    authority_ground_disagreement,
    'Where exactly do the readings disagree: on policy outcomes, or on the ground of state authority itself (deontological autonomy versus consequentialist harm minimization versus moral duty)?',
    'Analyze which structural element each reading''s axioms fix: the legalization reading fixes the ground (autonomy) and derives scope from it; determine whether siblings likewise fix grounds or merely instrument preferences.',
    'If the disagreement is over grounds rather than instrument choice, no empirical outcome data can merge the readings and they remain permanently coexisting constraints; if merely instrumental, convergence under evidence is possible and the family could collapse into fewer stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_ground_disagreement, conceptual, 'Location of the kernel contest: ground-of-authority versus outcome disagreement.').

omega_variable(
    market_extraction_intrinsicness,
    'Is the legal market''s extractiveness intrinsic to the legalization arrangement, or an artifact of weakly regulated implementation?',
    'Cross-jurisdiction comparison of advertising bans, potency caps, and public or nonprofit supply models against dependence-prevalence trajectories.',
    'If implementation-artifact, the extraction component is remediable without touching the autonomy core and the arrangement trends toward rope; if intrinsic, this reading structurally imports corporate extraction wherever enacted and the tangled character is permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_extraction_intrinsicness, empirical, 'Whether corporate extraction rides on legalization necessarily or contingently.').

omega_variable(
    competence_under_commercial_recruitment,
    'Does commercially engineered dependence defeat the competent-adult premise on which the autonomy grant rests?',
    'Addiction neuroscience and preference-formation research applied to legally marketed products, together with judicial treatment of competence in adjacent doctrine.',
    'If market-shaped preference formation defeats competence for a substantial user class, the autonomy axiom narrows and this reading converges structurally toward the harm reduction reading; if competence holds, the extraction critique must route through market regulation rather than autonomy limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_under_commercial_recruitment, conceptual, 'Empirical integrity of the foundational competence premise under commercial recruitment pressure.').

omega_variable(
    third_party_harm_attribution,
    'Which measured third-party harms are attributable to the legalized arrangement versus background rates and enforcement-era artifacts?',
    'Quasi-experimental jurisdiction-pair studies of impairment crashes, secondhand-exposure morbidity, and child-welfare indicators around legalization dates.',
    'Attribution determines whether the arrangement''s remaining coercive core is calibrated to real externalities or inflated or deflated by measurement artifacts, which would recalibrate the third-party-harm enforcement mandate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_party_harm_attribution, empirical, 'Causal attribution of the residual harm burden the arrangement''s enforcement exists to address.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__legalization_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__legalization_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__legalization_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(subs_tr_t25, substance_control_legitimacy__legalization_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__legalization_reading, theater_ratio, 30, 0.34).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__legalization_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__legalization_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__legalization_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(subs_be_t25, substance_control_legitimacy__legalization_reading, base_extractiveness, 25, 0.51).
narrative_ontology:measurement(subs_be_t30, substance_control_legitimacy__legalization_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__legalization_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__legalization_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__legalization_reading, suppression_requirement, 15, 0.39).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(subs_su_t25, substance_control_legitimacy__legalization_reading, suppression_requirement, 25, 0.44).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__legalization_reading, suppression_requirement, 30, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'drug policy' conflates three structurally distinct claims about the ground of state authority over substance use. Per the epsilon-invariance principle this family decomposes into three stories sharing the kernel substance_control_legitimacy: prohibition (criminalization as moral duty; users in the victim set; highest epsilon), legalization (this file; autonomy core with third-party-harm limit and corporate-market extraction; moderate epsilon), and harm reduction (consequentialist harm minimization without criminalization; blurred user/beneficiary boundary). Each neighbor cites the others as evidence: prohibition cites legalization's third-party harms, legalization cites prohibition's enforcement harms, and harm reduction cites both. Each story carries its own epsilon, beneficiaries, and victims; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
