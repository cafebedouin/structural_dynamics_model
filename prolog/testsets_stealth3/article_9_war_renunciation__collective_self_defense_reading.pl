% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__collective_self_defense_reading, []).

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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9 Collective Self-Defense Reading (Cabinet Custodial Interpretation)
 *   domain: constitutional law/security policy/institutional legitimacy
 *
 * SUMMARY:
 *   This story instantiates the collective_self_defense_reading of the
 *   article_9_war_renunciation kernel: the interpretive regime created by the
 *   July 2014 cabinet decision and codified in the 2015 Legislation for Peace
 *   and Security, under which Japan may use force to defend closely allied
 *   states when an attack on them threatens Japan's own survival, absent any
 *   direct attack on Japan. The standing arrangement under contest, and
 *   therefore the referent for every authored value here, is Article 9 as
 *   administered under cabinet custodial interpretation, assessed by this
 *   reading's own lights. Constraint-family decomposition: the colloquial
 *   label covering Article 9 spans three structurally distinct claims with
 *   different epsilon values, different victim sets, and different failure
 *   modes. The strict_pacifist_reading authors the standing arrangement as a
 *   categorical violation of an absolute prohibition (maximal epsilon, a
 *   universal victim set of everyone subject to militarization). The
 *   inherent_right_reading authors it as bounded minimum-necessary individual
 *   defense (intermediate epsilon, victims confined to those subjected to
 *   force beyond that floor). This reading authors the arrangement as
 *   survival-conditioned collective authorization whose costs fall on
 *   interpretive stability, procedural ratification, and the constituencies
 *   that depended on the narrower reading's fixity, with a victim set that
 *   grows as each expansion accumulates. Each reading is a separate story
 *   with its own epsilon and its own stakeholders; this file links both
 *   siblings through network.affects_constraints and records the
 *   decomposition in the dual-formulation note. Assumptions recorded:
 *   interval time points map to calendar years as t0=2014 through t12=2026;
 *   the terminal row of the measurement grid is marked projected; and the
 *   claim and metrics are independent authored facts. From the authoring seat
 *   the structure is believed to be a hybrid carrying both a genuine
 *   coordination function and real asymmetric extraction, and the metrics
 *   describe what the reading's operation honestly looks like even from
 *   inside its own endorsement. KEY AGENTS (by structural relationship): -
 *   japanese_executive_branch: Primary agenda-setter and principal
 *   beneficiary (institutional/constrained) — produces, staffs, and defends
 *   the reading - us_alliance_managers: Principal external beneficiary
 *   (institutional/mobile) — receives capability without bearing the
 *   constitutional cost - defense_industry_contractors: Secondary beneficiary
 *   (organized/arbitrage) — diversified exposure to expanded procurement -
 *   sdf_service_members: Genuinely dual-positioned seat
 *   (organized/constrained) — gains mission scope and budget, bears
 *   operational risk - pacifist_civil_society: Primary domestic target
 *   (organized/identity_locked) — civic identity fused to the renunciation
 *   baseline - constitutional_law_scholarship: Displaced interpretive
 *   authority (moderate/constrained) - okinawan_base_communities:
 *   Concentrated local cost bearer (powerless/trapped) -
 *   unconsented_electorate: Excluded ratifying authority (organized/trapped)
 *   — the referendum route was bypassed - regional_neighbor_states: Excluded
 *   external affected parties (institutional/mobile) - supreme_court_japan:
 *   Judicial observer seat practicing systematic avoidance
 *   (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.56).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.58).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 Collective Self-Defense Reading (Cabinet Custodial Interpretation)").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional law/security policy/institutional legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, '7cacf7f2-c26c-41b9-974c-66ad16183009').
narrative_ontology:cs_kernel_codification('7cacf7f2-c26c-41b9-974c-66ad16183009', fixed_text).
narrative_ontology:cs_authority_grounding('7cacf7f2-c26c-41b9-974c-66ad16183009', lineage).
narrative_ontology:cs_interpretation_layer_present('7cacf7f2-c26c-41b9-974c-66ad16183009').
narrative_ontology:cs_reading_relation('7cacf7f2-c26c-41b9-974c-66ad16183009', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('7cacf7f2-c26c-41b9-974c-66ad16183009', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('7cacf7f2-c26c-41b9-974c-66ad16183009', foundational, survival_threatening_collective_defense_permitted).
narrative_ontology:cs_axiom_status(survival_threatening_collective_defense_permitted, holdable).
narrative_ontology:cs_axiom_grounding('7cacf7f2-c26c-41b9-974c-66ad16183009', survival_threatening_collective_defense_permitted, instrumental).
narrative_ontology:cs_axiom('7cacf7f2-c26c-41b9-974c-66ad16183009', foundational, cabinet_custodial_interpretation_authority).
narrative_ontology:cs_axiom_status(cabinet_custodial_interpretation_authority, holdable).
narrative_ontology:cs_axiom_grounding('7cacf7f2-c26c-41b9-974c-66ad16183009', cabinet_custodial_interpretation_authority, conventional).
narrative_ontology:cs_reference_frame('7cacf7f2-c26c-41b9-974c-66ad16183009', custodial_flexible_defense_frame).
narrative_ontology:cs_drift_state('7cacf7f2-c26c-41b9-974c-66ad16183009', post_2022_nss_counterstrike_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7cacf7f2-c26c-41b9-974c-66ad16183009', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, japanese_executive_branch).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, us_alliance_managers).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, defense_industry_contractors).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, sdf_service_members).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, pacifist_civil_society).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, constitutional_law_scholarship).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, okinawan_base_communities).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, unconsented_electorate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, sdf_service_members).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, cabinet_custodial_interpretation_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, us_japan_alliance_deterrence_integration).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__collective_self_defense_reading, inherent_self_defense_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets security policy and produced the July 2014 cabinet decision reinterpreting Article 9 to admit collective self-defense, codified in the 2015 Legislation for Peace and Security. Operates the interpretive apparatus through the Cabinet Legislation Bureau and the National Security Secretariat. Collects operational flexibility, alliance credibility, and practical custody of constitutional meaning. Cannot cheaply reverse course without conceding that the reinterpretation was procedurally unlawful, and must continually staff and defend the reading against scholarly and political challenge.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, japanese_executive_branch, agenda_setter,
    institutional, generational, constrained, global).

% Gain a treaty partner able to protect US ships and assets and participate in joint operations, advancing burden-sharing goals pursued across administrations. Receive the operational value of the expanded authorization while bearing none of the domestic constitutional cost of producing it. Retain leverage: attention and force posture can shift toward other partners if Japan resists further integration.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, us_alliance_managers, beneficiary,
    institutional, generational, mobile, global).

% Receive expanded procurement under the 2022 National Security Strategy, including standoff and counterstrike missile programs, alongside relaxed transfer rules under the 2014 Three Principles on Transfer of Defense Equipment. Maintain diversified civilian and defense product lines, so exposure to any single security-policy decision is manageable and reversible.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, defense_industry_contractors, beneficiary,
    organized, biographical, arbitrage, global).

% Gain mission scope, growing budgets, and international roles under the expanded authorization, along with institutional investment and relevance. Simultaneously bear the added personal risk of deployments under ambiguous engagement rules such as kaketsuke-keigo rescue missions, and carry unresolved legal uncertainty about when any particular use of force crosses constitutional lines. Individuals can leave the service; the institution as a whole is committed to whatever missions the authorization admits.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, sdf_service_members, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, sdf_service_members, payer).

% Organized movements and citizen groups, including Article 9 Associations and successive student peace networks, whose civic identity rests on the renunciation clause as a settled national commitment. The reinterpretation displaced the interpretive stability they relied on, and each incremental expansion erodes the baseline further. Abandoning the position would mean surrendering a constitutive commitment, so they persist in protest, education, and litigation despite declining political returns.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, pacifist_civil_society, payer,
    organized, civilizational, identity_locked, national).

% The professional interpretive community whose settled doctrine held collective self-defense unconstitutional under the previously prevailing government reading. The 2014 cabinet decision displaced their authority over constitutional meaning in favor of cabinet custody. A broad majority, including many scholars who supported the underlying substance, publicly opposed the reinterpretation as procedurally unlawful. Their remaining instruments are publication, public statements, and eventual influence through court appointments; meanwhile they operate daily inside a regime that no longer requires their assent.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_law_scholarship, payer,
    moderate, generational, constrained, national).

% Host the installations whose operational tempo rises with expanded joint missions; the Henoko relocation advanced under the same alliance-integration logic. Bear accidents, noise, crime, and land takings concentrated on small islands. Prefectural veto efforts have been overridden by national government action, and the security decisions driving base use are made entirely elsewhere.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, okinawan_base_communities, payer,
    powerless, generational, trapped, local).

% The voting public was never given the referendum that the formal amendment route requires; the reinterpretation proceeded by cabinet decision precisely because that route looked unwinnable. Polling splits roughly evenly on collective self-defense itself, but no ratifying vote was ever taken on the constitutional change. Voters can punish parties in general elections but have no channel for a direct verdict on the alteration itself.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, unconsented_electorate, excluded,
    organized, biographical, trapped, national).

% South Korea, China, and Southeast Asian states observe the normalization of Japanese force projection and adjust their threat assessments and diplomatic posture accordingly. They object through diplomatic channels and alliance consultations but hold no seat in Japan's internal constitutional process; their concerns enter the domestic arrangement only indirectly, refracted through alliance politics.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regional_neighbor_states, excluded,
    institutional, generational, mobile, continental).

% Has never directly adjudicated Article 9's operative meaning, treating the question as intensely political and unsuitable for review. Its practiced avoidance leaves the cabinet reading standing without judicial validation or rejection. Appointment timing gives sitting governments durable influence over the composition of future benches, which reinforces the avoidance equilibrium.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, supreme_court_japan, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__collective_self_defense_reading, japanese_executive_branch).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the standing contradiction between Japan-US mutual defense obligations and Article 9's renunciation by supplying a single interpretive authorization architecture under which joint operations, logistics support, asset protection, and rescue missions can be planned and executed without case-by-case constitutional improvisation.
% TRANSFER_FUNCTION: Moves interpretive authority over the constitution's operative meaning away from the amendment process, the courts, and the scholarly community and into cabinet custody; moves operational risk and basing burden onto service members and host communities; moves alliance credibility value toward the United States.
% ABSENT_VOICES: The electorate never received the referendum the formal amendment route would have required; future generations inherit the elastic precedent with no seat anywhere in the process; neighboring states whose threat calculus shifts hold no place in the domestic interpretive process; scholarly critics were removed from the 2013-14 advisory panel after raising objections, so the decisive conversations were conducted among officials already predisposed to the outcome.
% DISAPPEARANCE_RATIONALE: If the collective-self-defense reading vanished overnight, the 2015 legislation would lose its interpretive foundation, planned and standing joint arrangements would suspend, alliance planning would revert to individual-defense-only cooperation and friction over burden-sharing would spike, and the executive would lose the authorization basis for current deployments. Pacifist and scholarly constituencies would recover their settled baseline, but the surrounding security-policy architecture would visibly reorganize rather than continue as before.
% FOUNDING_PROBLEM: Reconcile Article 9's war renunciation with the treaty obligation to the United States and a deteriorating regional environment, North Korean missile development and Chinese maritime expansion foremost, without pursuing a formal amendment that faced supermajority and ratification obstacles.
% FOUNDING_PROBLEM_CORROBORATION: US alliance managers and successive National Security Strategy documents attest that the security-environment problem is real, and regional military-balance data corroborates it from outside the benefiting parties. Scholarly critics confirm the problem's reality while disputing that cabinet reinterpretation was a lawful solution to it, which is corroboration of the problem combined with dissent on the remedy. No party outside the benefiting coalition attests that the specific survival-threat trigger formulation, as opposed to some amendment or narrower doctrine, was necessary rather than chosen for convenience.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.56 because the arrangement's costs are real yet partially self-justified even from this reading's seat: interpretive authority over the constitution migrated from amendment-and-court channels into cabinet custody, and each subsequent step, the 2015 legislation bundle, kaketsuke-keigo and logistics missions, and the 2022 National Security Strategy acquisition of counterstrike capability, stretched the original survival-threatening/no-other-means/minimum-necessary formula further while officially remaining inside it. Suppression is authored at 0.58 and is structural rather than coercive: the reading holds because alternatives were administratively closed, scholarly critics were dropped from the advisory panel after objecting, the amendment route was bypassed because it looked unwinnable, and the courts practice systematic avoidance. Theater ratio is authored at 0.38 and rising because the three-conditions rhetoric increasingly performs restraint while functioning as the absorption mechanism for incremental expansion; the security review, legislation, and alliance machinery behind it remain substantively functional, so theatricality is a growing fraction of activity, not the dominant one. Accessibility_collapse at 0.55 reflects that returning to the inherent-right reading requires formal amendment, judicial reversal, or sustained electoral turnover, all presently blocked, while scholarly and popular persistence keeps alternatives partly alive. Resistance at 0.62 reflects the 2015 protest wave, organized statements by constitutional scholars including former Cabinet Legislation Bureau directors, and continuing parliamentary opposition, real, sustained, and so far ineffective, which is itself evidence that active enforcement is doing work. All temporal series share one grid (t = 0, 2, 4, 6, 8, 10, 12). Trajectories are a monotonic ratchet rather than a cycle: each expansion normalizes the next, so no oscillatory dynamics are claimed and none are modeled.
 *
 * PERSPECTIVAL GAP:
 *   From the executive and alliance-manager seats the arrangement computes as adaptation: a genuine coordination solution to the treaty-versus-renunciation contradiction, whose costs are the price of survival. From the pacifist, scholarship, and Okinawan seats the same structure computes as dispossession, of settled law, of procedural voice, and of local autonomy respectively. The SDF seat straddles the divide, collecting mission scope and institutional investment while bearing the risk and legal ambiguity that the scope generates. The court seat, practicing avoidance, registers neither cost nor benefit and certifies nothing. These divergences are computed by the engine from the power, exit, and role data authored above; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive low directionalities: the executive branch collects interpretive authority and policy freedom, with its own maintenance burden partially offsetting the subsidy; alliance managers receive operational capability while bearing none of the domestic constitutional cost; contractors hold arbitrage-grade exit, placing them nearest the beneficiary pole. The victim declarations drive high directionalities: pacifist civil society is identity-locked to the renunciation baseline and sits nearest the full-target end; the scholarship community is career- and doctrine-constrained; Okinawan communities are trapped and powerless beneath a national policy they cannot reach; the unconsented electorate carries the procedural harm of a bypassed ratification route. SDF service members sit mid-range as a declared dual-positioned seat. No directionality_overrides are authored: the override surface keys on the power atom, and this story's institutional atom contains structurally unlike seats with opposite relationships (an executive collecting on the beneficiary side and a neutral court observing), so a story-level override keyed to institutional would smear a correction across seats the derivation already handles correctly. The beneficiary/victim and exit declarations are treated as sufficient, and the residual imprecision in the executive's derived value is noted as accepted noise rather than corrected at the wrong granularity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, the tension between the renunciation clause and alliance obligation under a worsening regional environment, remains live, so no dead-mandate verdict is available and the arrangement cannot be dismissed as vestigial. The classification discipline guards against two symmetric mislabels. Reading the arrangement as pure coordination would erase the asymmetric extraction: interpretive monopoly concentrated in the cabinet, a ratification route deliberately bypassed, and basing costs concentrated on communities with no exit. Reading it as pure extraction would erase the genuine, widely demanded coordination function: a substantial plurality of the public accepts some collective-defense capacity, the alliance problem is independently verifiable, and the authorization architecture solved a real contradiction. The mandatrophy-relevant signal in the temporal data is the rising theater ratio: the three-conditions formula began the interval as the reading's limiting device and is drifting toward a rhetorical shell that absorbs whatever expansion the security establishment next proposes. That is the Goodhart signature this measurement series is built to catch, and it is the reason the theater series is tracked on the same grid as extractiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_structure,
    'This constraint is one reading of the article_9_war_renunciation kernel; how would instantiating the strict_pacifist_reading or the inherent_right_reading instead change the structural classification?',
    'Generate the sibling stories and compare classifications: the strict reading yields a prohibition-shaped constraint with high suppression of any forces whatsoever, and the inherent-right reading yields bounded defensive authorization with a materially smaller victim set and lower epsilon.',
    'Sibling instantiation relocates the victim set, this reading uniquely adds those who relied on the inherent-right reading''s stability, and shifts epsilon; cross-reading comparison isolates what the collective extension itself contributes versus what the underlying text contributes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Committer structure: this story is one of three live readings of the Article 9 kernel; the comparison across siblings is deferred to their own files.').

omega_variable(
    survival_trigger_elasticity,
    'What bounds a situation threatening Japans survival? Does the trigger admit contingencies such as a Taiwan conflict or Middle East mine-clearing support, and is there any principled ceiling on its reach?',
    'Accumulated case-by-case cabinet determinations, the response of the 2015 legislated framework to novel requests, or a judicial ruling drawing the boundary; a refused deployment request would reveal the ceiling as clearly as an approved one.',
    'If the trigger is unbounded in practice, the reading operates as elastic delegation and the rising extraction and theater trajectories continue unchecked; if the legislated enumerated cases bind, extraction plateaus near current levels and the reading stabilizes as a bounded tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_trigger_elasticity, empirical, 'Whether the survival-threat trigger is a genuine limit or an expandable authorization surface.').

omega_variable(
    amendment_counterfactual_feasibility,
    'Was formal amendment genuinely unavailable, making cabinet reinterpretation the least-cost path to any collective-defense capacity, or was amendment achievable, making reinterpretation a shortcut taken to avoid sharing the decision with voters?',
    'Counterfactual analysis of Diet seat distributions and polling on Article 9 revision across the period, together with the trajectory of any subsequent amendment attempt under comparable or more favorable coalitions.',
    'If amendment was feasible, the concentration of interpretive authority is substantially rent-taking and the receipt-surface picture sharpens toward captured extraction; if amendment was genuinely blocked, part of the measured extraction is the price of constitutional deadlock and the reading earns partial mitigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_counterfactual_feasibility, conceptual, 'Whether the bypass of the amendment route was necessity or convenience, which calibrates how much of the extraction is attributable to the method rather than the substance.').

omega_variable(
    judicial_deference_persistence,
    'Will the Supreme Court''s avoidance of Article 9 adjudication persist if a direct constitutional challenge to a specific collective-defense deployment reaches it with clean standing?',
    'Lower-court rulings escalating upward, changes in bench composition after political turnover, or the court''s handling of the first well-formed test case brought by litigants such as pacifist civic organizations.',
    'Judicial activation would abruptly reprice accessibility_collapse for the alternative readings in whichever direction the court rules, either validating the cabinet reading and hardening the regime or reopening the interpretive field and repricing suppression downward; prolonged continued avoidance leaves the current deference equilibrium intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_deference_persistence, empirical, 'Durability of the judicial-abstention pillar that currently holds the reading upright without validation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art9_csd_tr_t0, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(art9_csd_tr_t0, observed).
narrative_ontology:measurement(art9_csd_tr_t2, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2, 0.23).
narrative_ontology:measurement_basis(art9_csd_tr_t2, observed).
narrative_ontology:measurement(art9_csd_tr_t4, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement_basis(art9_csd_tr_t4, observed).
narrative_ontology:measurement(art9_csd_tr_t6, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement_basis(art9_csd_tr_t6, observed).
narrative_ontology:measurement(art9_csd_tr_t8, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(art9_csd_tr_t8, observed).
narrative_ontology:measurement(art9_csd_tr_t10, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(art9_csd_tr_t10, observed).
narrative_ontology:measurement(art9_csd_tr_t12, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(art9_csd_tr_t12, projected).

% Extraction over time
narrative_ontology:measurement(art9_csd_be_t0, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(art9_csd_be_t0, observed).
narrative_ontology:measurement(art9_csd_be_t2, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2, 0.43).
narrative_ontology:measurement_basis(art9_csd_be_t2, observed).
narrative_ontology:measurement(art9_csd_be_t4, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement_basis(art9_csd_be_t4, observed).
narrative_ontology:measurement(art9_csd_be_t6, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 6, 0.49).
narrative_ontology:measurement_basis(art9_csd_be_t6, observed).
narrative_ontology:measurement(art9_csd_be_t8, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(art9_csd_be_t8, observed).
narrative_ontology:measurement(art9_csd_be_t10, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(art9_csd_be_t10, observed).
narrative_ontology:measurement(art9_csd_be_t12, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(art9_csd_be_t12, projected).

% Suppression requirement over time
narrative_ontology:measurement(art9_csd_su_t0, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(art9_csd_su_t0, observed).
narrative_ontology:measurement(art9_csd_su_t2, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2, 0.5).
narrative_ontology:measurement_basis(art9_csd_su_t2, observed).
narrative_ontology:measurement(art9_csd_su_t4, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement_basis(art9_csd_su_t4, observed).
narrative_ontology:measurement(art9_csd_su_t6, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(art9_csd_su_t6, observed).
narrative_ontology:measurement(art9_csd_su_t8, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement_basis(art9_csd_su_t8, observed).
narrative_ontology:measurement(art9_csd_su_t10, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(art9_csd_su_t10, observed).
narrative_ontology:measurement(art9_csd_su_t12, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(art9_csd_su_t12, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept of Article 9 decomposes into three stories, this collective_self_defense_reading plus the sibling strict_pacifist_reading and inherent_right_reading files. The label conflates a categorical-prohibition claim, a bounded-individual-defense claim, and a survival-conditioned-collective-authorization claim; these differ in epsilon, in victim sets, and in failure modes, so each is authored separately with a single stable epsilon and the three files are linked pairwise through affects_constraints. The upstream inherent-right reading supplies the premise this reading extends, and this reading's adoption is what consumes the inherent-right reading's stability, which is why the edges run between the siblings in both directions of structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
