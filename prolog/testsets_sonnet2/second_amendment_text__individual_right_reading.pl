% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment — Individual Right Reading (Personal Self-Defense, Militia-Independent)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates the individual-right reading of the Second
 *   Amendment's kernel text: the operative clause ('the right of the people
 *   to keep and bear Arms, shall not be infringed') is read as securing a
 *   personal entitlement to armed self-defense independent of organized
 *   militia service. This reading became doctrinally dominant with District
 *   of Columbia v. Heller (2008) and was extended in New York State Rifle &
 *   Pistol Association v. Bruen (2022), which imposed a
 *   text-history-tradition test displacing means-end scrutiny for firearm
 *   regulation. The reading coordinates genuine expectations (owners,
 *   industry, and regulators know roughly what regulatory tools remain
 *   available) but does so by transferring regulatory latitude away from
 *   legislatures and communities toward owners and industry, with measurable
 *   costs concentrated on domestic violence survivors and high-mortality
 *   communities — hence the tangled_rope claim rather than pure rope or pure
 *   snare.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: primary beneficiary (moderate/mobile) — expanded personal entitlement
 *   - firearms_industry: primary beneficiary (organized/arbitrage) — market benefits from doctrinal stability
 *   - gun_rights_advocacy_organizations: agenda_setter (organized/arbitrage) — sets and defends the doctrinal frame
 *   - domestic_violence_survivors: primary target (powerless/trapped) — bears lethality risk from constrained disarming tools
 *   - gun_violence_victims: diffuse target (powerless/trapped)
 *   - communities_with_high_firearm_mortality: concentrated target (powerless/trapped)
 *   - state_and_local_legislatures: excluded institutional actor whose traditional regulatory discretion is displaced
 *   - constitutional_courts: analytical observer adjudicating scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.42).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.35).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment — Individual Right Reading (Personal Self-Defense, Militia-Independent)").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '5d9b9616-5b76-4850-9a3a-602116374297').
narrative_ontology:cs_kernel_codification('5d9b9616-5b76-4850-9a3a-602116374297', fixed_text).
narrative_ontology:cs_authority_grounding('5d9b9616-5b76-4850-9a3a-602116374297', lineage).
narrative_ontology:cs_interpretation_layer_present('5d9b9616-5b76-4850-9a3a-602116374297').
narrative_ontology:cs_reading_relation('5d9b9616-5b76-4850-9a3a-602116374297', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('5d9b9616-5b76-4850-9a3a-602116374297', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('5d9b9616-5b76-4850-9a3a-602116374297', foundational, operative_clause_independent_of_prefatory_clause).
narrative_ontology:cs_axiom_status(operative_clause_independent_of_prefatory_clause, holdable).
narrative_ontology:cs_axiom_grounding('5d9b9616-5b76-4850-9a3a-602116374297', operative_clause_independent_of_prefatory_clause, conventional).
narrative_ontology:cs_axiom('5d9b9616-5b76-4850-9a3a-602116374297', foundational, self_defense_as_core_protected_activity).
narrative_ontology:cs_axiom_status(self_defense_as_core_protected_activity, holdable).
narrative_ontology:cs_axiom_grounding('5d9b9616-5b76-4850-9a3a-602116374297', self_defense_as_core_protected_activity, deontological).
narrative_ontology:cs_reference_frame('5d9b9616-5b76-4850-9a3a-602116374297', founding_era_operative_clause_primacy).
narrative_ontology:cs_drift_state('5d9b9616-5b76-4850-9a3a-602116374297', post_heller_bruen_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5d9b9616-5b76-4850-9a3a-602116374297', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, domestic_violence_survivors).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, gun_violence_victims).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, communities_with_high_firearm_mortality).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, adjudicated_prohibited_persons_seeking_narrow_relief).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, textualist_operative_clause_primacy).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, self_defense_as_natural_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own firearms for self-defense, hunting, or recreation under a reading that treats the right as personal and unconditioned on militia membership. Benefit from constitutional protection against many licensing, registration, and carry restrictions; face reduced legal friction acquiring and carrying arms in jurisdictions that must accommodate the individual-right holding.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    moderate, biographical, mobile, national).

% Manufactures and sells firearms and ammunition into a market whose size and legal breathing room depend heavily on the individual-right reading remaining doctrinally dominant. Funds litigation and lobbying to expand and defend the reading; captures direct commercial benefit from every jurisdiction where restrictive regulation is struck down or chilled.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_industry, beneficiary,
    organized, generational, arbitrage, national).

% Litigate, lobby, and shape public discourse to entrench the individual-right reading as settled constitutional law, funding test cases and drafting model legislation. Set the doctrinal agenda that courts and legislatures respond to; their institutional survival and influence depend on the reading's continued dominance.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, gun_rights_advocacy_organizations, beneficiary).

% Face elevated lethality risk when abusers retain firearm access; disarming statutes protecting survivors face heightened scrutiny and litigation challenge under the individual-right reading, and enforcement of existing prohibitions is inconsistently applied. Have little power to alter the doctrinal framework that governs whether an abuser can be disarmed quickly and durably.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, domestic_violence_survivors, payer,
    powerless, immediate, trapped, local).

% Bear the injuries, deaths, and trauma associated with widespread firearm availability. Cannot individually alter access levels; their interests are represented, if at all, through diffuse political coalitions that must overcome the doctrinal and political weight the individual-right reading confers on gun ownership.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_violence_victims, payer,
    powerless, immediate, trapped, national).

% Live with concentrated firearm homicide and suicide rates correlated with permissive access regimes. Local governments attempting stricter regulation face preemption fights and constitutional challenge grounded in the individual-right reading, narrowing the community's own regulatory exit options.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, communities_with_high_firearm_mortality, payer,
    powerless, generational, trapped, regional).

% Individuals with disqualifying histories (certain nonviolent felonies, old convictions) who argue their categorical exclusion is overbroad under an individual-right framework, yet lack the resources or standing to litigate as-applied challenges; the reading's rhetoric of individual right does not translate into practical relief for this group, which is generally excluded from the coalition's advocacy priorities.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, adjudicated_prohibited_persons_seeking_narrow_relief, payer,
    powerless, biographical, constrained, national).

% Historically exercised broad police-power discretion over firearm regulation; under the individual-right reading, many regulatory tools (licensing schemes, carry restrictions, certain weapon bans) face strict or intermediate scrutiny that displaces legislative judgment. Their traditional regulatory voice is structurally diminished relative to the pre-Heller regime.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, state_and_local_legislatures, excluded,
    institutional, generational, constrained, regional).

% Adjudicate the scope of the individual right against competing regulatory claims, applying tiers of scrutiny and historical-tradition tests. Their doctrinal choices determine how far the reading's protective sweep extends into licensing, carry, and prohibited-persons law.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, judicially enforceable baseline entitlement to keep and bear arms for personal use, coordinating expectations among owners, manufacturers, and regulators about what regulation is constitutionally permissible without requiring case-by-case political renegotiation of the right's existence.
% TRANSFER_FUNCTION: Shifts regulatory latitude away from legislatures and toward individual owners and industry: reduced compliance burden and expanded market access for owners and manufacturers is paid for in constrained legislative tools for disarming dangerous individuals and regulating community-level firearm density, with costs falling disproportionately on domestic violence survivors and high-mortality communities.
% ABSENT_VOICES: Domestic violence survivors, gun violence victims, and residents of high-mortality communities are rarely direct parties to the constitutional litigation that shapes the reading's scope; their interests enter mainly through amicus briefs and post-hoc empirical studies rather than as litigants with standing comparable to gun-rights organizations' repeat-player advantage.
% DISAPPEARANCE_RATIONALE: If the individual-right reading were abandoned overnight in favor of a militia-conditioned reading, state and local legislatures would regain substantially freer hand to license, register, restrict, and in some cases ban categories of firearms; the firearms industry's national market would face materially higher regulatory fragmentation and compliance cost; ongoing litigation over carry permits, magazine limits, and prohibited-persons statutes would be decided under a wholly different scrutiny framework.
% FOUNDING_PROBLEM: The reading was advanced to establish that the Second Amendment's operative clause secures a personal right to armed self-defense that does not depend on active militia service, responding to 20th-century regulatory expansion (handgun bans, strict licensing) that gun-rights advocates argued had drifted from the constitutional text's actual guarantee.
% FOUNDING_PROBLEM_CORROBORATION: Gun-rights organizations and a majority of the Supreme Court in Heller (2008) and Bruen (2022) attest the individual right is the correct and continuing constitutional baseline. Public health researchers, several dissenting justices, and comparative-law scholars outside the advocacy coalition attest the reading is a 21st-century doctrinal innovation whose textual and historical grounding remains actively disputed, and that its practical effect has been to narrow regulatory tools correlated with reduced firearm mortality in comparative studies.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).
:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate 0.42, rising over the interval as the reading moved from marginal academic position (pre-1968) to doctrinally dominant holding (post-Heller, post-Bruen) that actively displaces legislative regulatory tools — this is a rising trajectory, not a flat one, because the reading's practical bite on regulation is a 21st-century development. Suppression (0.35) is lower than extraction because the reading itself does not directly coerce anyone into gun ownership; its coercive force operates instead through preemption of legislative alternatives and through strict scrutiny frameworks that suppress regulatory options communities might otherwise choose. Resistance is high (0.72) reflecting the genuinely contested, actively litigated nature of the reading — public health researchers, gun-violence-prevention advocates, and dissenting jurists mount sustained challenges. Accessibility collapse is moderate (0.5): regulatory alternatives are constrained but not eliminated — some licensing and background-check regimes survive scrutiny.
 *
 * PERSPECTIVAL GAP:
 *   From the individual-gun-owner and industry seats, the reading looks like a rope: it coordinates a stable, judicially protected baseline that lets owners and manufacturers plan without fear of shifting political majorities banning categories of arms. From the domestic-violence-survivor and high-mortality-community seats, the same doctrinal structure looks like a tangled rope shading toward snare: the coordination benefit accrues to owners and industry while the cost of narrowed disarming tools and regulatory preemption falls on populations with no litigation standing comparable to the advocacy coalition's repeat-player advantage. The engine should register this asymmetry directly from the beneficiary/victim/enforcement structure authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry are declared beneficiaries because the doctrinal reading directly expands their legal latitude and market conditions — d sits near the beneficiary end, amplified for industry by arbitrage-grade capacity to litigate across jurisdictions. Domestic violence survivors, gun violence victims, and high-mortality communities are declared victims: they are structurally trapped (survivors especially, given the immediate lethality stakes) and bear costs through the same doctrinal mechanism that benefits owners — narrowed disarming and community-regulation tools. Adjudicated prohibited persons seeking narrow relief are a distinct, smaller victim group: the reading's individual-right rhetoric does not translate into practical relief for this group because gun-rights advocacy prioritizes law-abiding-owner test cases over categorical-exclusion challenges, leaving this population excluded from the coalition's litigation agenda despite nominal alignment with 'individual right.'
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabeling errors symmetric to this reading: (1) treating the reading as pure coordination (rope) would erase the documented cost concentration on domestic violence survivors and high-mortality communities, laundering a genuine distributive shift as costless coordination; (2) treating the reading as pure extraction (snare) would erase the genuine coordination value the doctrine provides to owners and manufacturers who benefit from a stable, judicially settled baseline rather than a patchwork of shifting local majorities. The founding_problem interview surfaces the contested genealogy directly: gun-rights advocates attest the problem (textual drift from a personal-right guarantee) remains live; public health researchers and dissenting justices attest the reading is itself the disputed innovation, not a restoration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_history_tradition_test_validity,
    'Does the Bruen text-history-tradition methodology accurately recover founding-era regulatory understanding, or does it selectively privilege historical analogues that favor the individual-right outcome while excluding disfavored ones?',
    'Systematic historical review by legal historians outside the advocacy coalition of the full corpus of founding-era and Reconstruction-era firearm regulation, compared against which analogues courts have accepted or rejected post-Bruen.',
    'If the methodology is shown to be outcome-selective, the reading''s claimed textualist/originalist grounding is substantially weakened, supporting reclassification toward more extractive with less coordination cover; if the methodology proves consistently applied, the coordination claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_history_tradition_test_validity, empirical, 'Whether the historical-analogue test is neutral methodology or outcome-selective.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the individual-right reading the constitutionally correct reading of the kernel text, or is the operative-clause/prefatory-clause relationship genuinely underdetermined such that multiple readings remain equally defensible?',
    'This is not resolvable by further textual analysis alone; it is the subject of the kernel contest itself among individual_right_reading, collective_security_reading, and originalist_civic_virtue_reading, each authored as a separate constraint story.',
    'If the text is genuinely indeterminate, the individual-right reading''s dominance reflects judicial and political power rather than interpretive necessity, which would support authoring extraction as institutionally contingent rather than textually compelled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether kernel indeterminacy underlies the doctrinal dominance of this reading.').

omega_variable(
    disarming_relief_asymmetry,
    'Does the individual-right reading''s rhetoric of personal entitlement translate into meaningful legal relief for narrowly-adjudicated prohibited persons, or does advocacy prioritization leave this population''s claims systematically unlitigated despite doctrinal alignment?',
    'Track the disposition and funding of as-applied Second Amendment challenges brought by non-violent-felony and other narrow-category prohibited persons versus challenges brought by law-abiding-owner plaintiffs, comparing advocacy organization resource allocation.',
    'If relief is systematically unavailable to this group despite doctrinal alignment, this population functions as a victim class whose nominal beneficiary status is illusory — reinforcing rather than undermining the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarming_relief_asymmetry, empirical, 'Whether prohibited-persons relief tracks doctrine or advocacy priorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__individual_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__individual_right_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_text__individual_right_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_text__individual_right_reading, theater_ratio, 2022, 0.2).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_text__individual_right_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__individual_right_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__individual_right_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(seco_be_t1968, second_amendment_text__individual_right_reading, base_extractiveness, 1968, 0.25).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__individual_right_reading, base_extractiveness, 2008, 0.35).
narrative_ontology:measurement(seco_be_t2022, second_amendment_text__individual_right_reading, base_extractiveness, 2022, 0.4).
narrative_ontology:measurement(seco_be_t2026, second_amendment_text__individual_right_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__individual_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(seco_su_t1900, second_amendment_text__individual_right_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement(seco_su_t1968, second_amendment_text__individual_right_reading, suppression_requirement, 1968, 0.2).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__individual_right_reading, suppression_requirement, 2008, 0.3).
narrative_ontology:measurement(seco_su_t2022, second_amendment_text__individual_right_reading, suppression_requirement, 2022, 0.34).
narrative_ontology:measurement(seco_su_t2026, second_amendment_text__individual_right_reading, suppression_requirement, 2026, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__individual_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, domestic_violence_firearm_prohibition_statutes).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, state_concealed_carry_permitting_regimes).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the second_amendment_text kernel. The individual_right_reading (this story) authors moderate-and-rising extraction (0.42) concentrated on disarming-tool constraints; the collective_security_reading authors a materially different beneficiary/victim structure (state regulatory authority as beneficiary, individual owners facing licensing burden as payers) and a lower or differently-shaped extraction profile; the originalist_civic_virtue_reading shares this reading's textual departure point but grounds the right in citizen-soldier capacity rather than personal self-defense, producing different downstream implications for weapon-type scrutiny. All three are linked via affects_constraints and share the fixed kernel text but are NOT the same constraint — each has an independently authored ε per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__individual_right_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
