% ============================================================================
% CONSTRAINT STORY: us_israel_faa_502b_nonenforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_israel_faa_502b_nonenforcement, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_israel_faa_502b_nonenforcement
 *   human_readable: Non-enforcement of US Foreign Assistance Act Section 502B for Israel
 *   domain: geopolitical/human_rights
 *
 * SUMMARY:
 *   Section 502B of the US Foreign Assistance Act (1974) legally prohibits
 *   security assistance to countries engaged in a consistent pattern of gross
 *   human rights violations. The statute creates a clear enforcement
 *   mechanism: the Executive Branch must certify that a recipient country is
 *   not engaged in such violations, or aid is withheld. For Israel, this
 *   constraint has operated as a de facto non-enforcement regime for decades
 *   — despite documented human rights concerns (Palestinian civilian
 *   casualties, detention practices, settlement policy), the Executive Branch
 *   has consistently certified Israel's compliance or issued presidential
 *   waivers citing national security interests. This constraint exhibits a
 *   classic Tangled Rope structure: Congress enacted 502B as a coordination
 *   mechanism to align security aid with human rights values (coordination
 *   function), but the Executive Branch's selective non-enforcement creates
 *   asymmetric extraction — beneficiaries (Executive, Israeli government,
 *   defense contractors) benefit from unconstrained aid flows, while victims
 *   (Palestinian civilians, international legal order, congressional intent)
 *   bear the cost of enforcement failure. The theater ratio (0.81) reflects
 *   that the statutory apparatus has largely degraded from a functional
 *   enforcement mechanism to a performative ritual: reviews are conducted,
 *   certifications are issued, but the enforcement gate never actually
 *   triggers for Israel.
 *
 * KEY AGENTS:
 *   - US State Department / Executive Branch: Primary beneficiary (institutional/arbitrage) — maintains strategic alliance, enables defense contractor sales, achieves geopolitical coordination goals without legal friction
 *   - Israeli Government: Primary beneficiary (powerful/arbitrage) — receives unrestricted military aid, avoids statutory human rights review mechanism, sustains strategic partnership
 *   - Palestinian Civilian Population: Primary victim (powerless/trapped) — subject to military action enabled by US assistance, no exit option, no mechanism within the constraint to organize or defend interests
 *   - US Congress: Secondary actor (organized/constrained) — enacted the statute, benefits from coordination function, but experiences extraction through Executive circumvention of legislative intent
 *   - Defense Contractors: Secondary beneficiary (powerful/constrained) — sustain large Israel military contracts, face theoretical regulatory risk, have political leverage to influence Executive discretion
 *   - International Human Rights Legal Order: Systemic victim (powerless/trapped) — non-enforcement erodes credibility, establishes precedent for selective enforcement by other aid-providing states
 *   - Section 502B Statutory Apparatus: Institutional actor (institutional/constrained) — the formal mechanism has degraded to theater, exists in form but lacks functional output
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_israel_faa_502b_nonenforcement, 0.58).
domain_priors:suppression_score(us_israel_faa_502b_nonenforcement, 0.72).
domain_priors:theater_ratio(us_israel_faa_502b_nonenforcement, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_israel_faa_502b_nonenforcement, tangled_rope).
narrative_ontology:human_readable(us_israel_faa_502b_nonenforcement, "Non-enforcement of US Foreign Assistance Act Section 502B for Israel").
narrative_ontology:topic_domain(us_israel_faa_502b_nonenforcement, "geopolitical/human_rights").

domain_priors:requires_active_enforcement(us_israel_faa_502b_nonenforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, us_state_department_executive_branch).
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, israeli_government).
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, us_defense_contractors).
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, strategic_alliance_maintenance).
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, palestinian_civilian_population).
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, international_legal_order).
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, congressional_intent).
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, human_rights_enforcement_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN CIVILIANS (SNARE) — Trapped within territory subject to military action enabled by US assistance. No exit option, no advocacy mechanism in the constraint itself, no alternative protection framework. Bears the cost of enforcement failure without means to organize or escape. Maximum extraction from a structural standpoint.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INTERNATIONAL HUMAN RIGHTS ORDER (SNARE) — The non-enforcement of 502B erodes the credibility and enforceability of human rights treaties globally. States observe that selective non-enforcement is possible without consequence. Trapped in a collective action problem where enforcement requires coordination among all aid-providing states, but individual defection (exempting favored allies) carries no cost. High extraction from the corpus of international law.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: US CONGRESS (TANGLED ROPE) — Congress enacted 502B as a coordination mechanism: establish clear human rights standards and enforce them consistently to align aid policy with stated values. Congress also benefits from the coordination function (establishes clear accountability mechanism). However, Congress experiences extraction through selective non-enforcement by the Executive Branch — the Executive circumvents congressional intent via administrative waiver and discretionary certification. Congress has some exit option (override through legislation) but faces political cost and institutional inertia. Mixed coordination + extraction.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: US STATE DEPARTMENT / EXECUTIVE BRANCH (ROPE) — Experiences 502B non-enforcement as a pure coordination mechanism: maintains strategic alliance with Israel, enables defense contractor profit flows, sustains regional alliance structure (Israel as central hub). The Executive has arbitrage exit (can selectively enforce or waive for different countries, can reinterpret statutory language). Net beneficiary — the constraint structure enables the Executive to achieve coordination goals (alliance maintenance) while minimizing domestic political friction. Low or negative extraction experienced.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ISRAELI GOVERNMENT (ROPE) — Benefits from Section 502B non-enforcement as a coordination mechanism that sustains military aid pipeline without legal constraint. The Israeli government experiences the constraint as enabling coordination: access to US military technology, training, and logistical support without the friction of statutory human rights reviews. High arbitrage options (can shift sourcing to other allies, can adjust military strategy). Net beneficiary — experienced as pure coordination benefit.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: US DEFENSE CONTRACTORS (TANGLED ROPE) — Benefit from sustained Israel military aid (coordination benefit via large sales contracts). Also experience extraction through regulatory risk: theoretical possibility of 502B enforcement would interrupt contract flow. Constrained exit — cannot easily diversify from major US ally military platforms, but have sufficient political leverage to influence Executive enforcement discretion. Mixed: major coordination benefit (sustained contracts) with some extraction risk (regulatory uncertainty).
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: SECTION 502B STATUTORY APPARATUS (PITON) — The statute itself has degraded from a functional human rights enforcement mechanism to a performative ritual. Certification letters are filed, human rights assessments are conducted, but the enforcement gate never actually triggers (Israel is always waived or certified despite documented evidence of consistent pattern). Theater ratio (0.81) reflects that the entire legal apparatus is now theater — the mechanism exists, reviews occur, reports are filed, but the functional output (withholding of aid for rights violations) never materializes. Institutional inertia sustains the ritual form despite atrophied function.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / SYSTEMIC VIEW (MOUNTAIN) — From a civilizational perspective, the non-enforcement might appear as an inherent property of sovereign state relations: great powers always shelter favored allies from legal constraint, and this is an immutable feature of international politics. However, the structural data contradicts the mountain classification — ample counter-examples exist (US enforcement of sanctions on Russia, Iran, Syria; enforced arms embargoes on other states; conditional aid regimes with teeth). The mountain classification is a false summit: naturalization of a contingent institutional choice as a law of nature.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_israel_faa_502b_nonenforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_israel_faa_502b_nonenforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_israel_faa_502b_nonenforcement, TR),
    TR >= 0.70.

:- end_tests(us_israel_faa_502b_nonenforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Executive Branch and Israeli government capture substantial benefits (sustained military aid, unconstrained partnership, geopolitical influence) while Palestinian civilians and international legal order bear clear costs (military action without domestic legal constraint, erosion of global human rights enforcement). The extraction is not maximal (0.70+) because: (a) the statutory mechanism still exists and could theoretically be invoked, (b) congressional pressure creates some political cost to non-enforcement, (c) transparency about the non-enforcement exists in public discourse. Suppression (0.72): High. Significant barriers to enforcement include: (a) presidential waiver authority (Veto Act 1962 allows national security override), (b) political cost to administrations (domestic pro-Israel constituencies, campaign funding, media dynamics), (c) congressional political constraints (veto-proof majority required to override president), (d) institutional inertia (decades of non-enforcement establish precedent), (e) securitization framing (Israel aid framed as vital to US Middle East strategy). Theater ratio (0.81): Very high. The constraint has shifted from functional enforcement mechanism to performative ritual. Human rights reviews occur annually, reports are filed, certifications are issued — but the enforcement gate (aid withholding) never triggers. The rising theater ratio over the 30-year interval reflects that as evidence of human rights concerns accumulated, the ceremonial apparatus expanded (more detailed reports, more sophisticated certification language) while actual enforcement outcomes remained constant (zero withholdings). This is diagnostic Goodhart drift: the measurement activity (certification bureaucracy) has become the goal, replacing the original goal (preventing aid to rights violators).
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates maximal perspectival disagreement. Palestinian civilians and international law structures perceive pure extraction (Snare) — they are structurally trapped and benefit from neither coordination nor mitigation. Congress perceives mixed extraction and benefits (Tangled Rope) — the statute they created does provide some coordination benefit (establishes clear standards), but they experience extraction through Executive circumvention. The Executive branch perceives pure coordination (Rope) — 502B non-enforcement enables their strategic alliance goals without friction. Israel perceives coordination benefit (Rope) — sustained military aid flows with minimal legal constraint. Defense contractors perceive mixed coordination-extraction (Tangled Rope) — they benefit from aid flows but face theoretical regulatory risk. The international law order perceives both extraction (eroded enforcement credibility) and theater (ritual certification without consequence). The analytical observer perceives a false Mountain — the temptation to naturalize non-enforcement as inherent to great power politics, when in fact it is a contingent policy choice (demonstrated by US enforcement of 502B against other countries in parallel periods).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position relative to extraction flows: Beneficiaries experience low d (receiving side of extraction). Victims experience high d (bearing side of extraction). The Executive Branch benefits from non-enforcement (d ≈ 0.05, institutional beneficiary with arbitrage exit). Congress benefits from 502B's existence but loses from Executive circumvention (d ≈ 0.50-0.55, organized actor with some exit but facing political cost). Palestinian civilians have no benefit and bear full cost (d ≈ 0.95, powerless/trapped). International legal order receives no direct benefit and bears erosion cost (d ≈ 0.90, powerless/trapped in collective coordination problem). The derivation chain prioritizes: (1) structural beneficiary/victim declarations, (2) exit options (arbitrage vs trapped), (3) power level (institutional beneficiaries with high exit have low d; powerless victims with no exit have high d). No overrides are necessary — the structural relationships map cleanly to directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint avoids the mandatrophy by clearly separating coordination function from extraction mechanism. The coordination function is real: Congress enacted 502B to establish clear human rights standards and align aid policy with stated values. This is genuine coordination — it solves the collective action problem of defining and enforcing human rights criteria. The extraction mechanism is also real: the Executive Branch circumvents the statutory mechanism through waiver authority, selective certification, and securitization framing. Non-enforcement enables sustained aid flows that benefit the Executive, Israel, and defense contractors while imposing costs on Palestinians and international legal order. A false Mountain classification would claim that non-enforcement is inherent to great power politics — an immutable law of state relations. The structural data contradicts this: (a) the US enforces 502B against Syria, Iran, Russia, and other non-allies in parallel periods, (b) congressional intent is clear and opposition to non-enforcement is politically feasible (not prohibited), (c) the Executive's choice to waive/certify is discretionary, not required. The extraction is real (beneficiaries benefit, victims bear cost, enforcement is selective), but not immutable. The Tangled Rope classification correctly captures that this is a hybrid: coordination function (statutory clarity) + extraction mechanism (Executive circumvention + beneficiary insulation from congressional intent).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_trigger_threshold,
    'What constitutes a ''consistent pattern of gross human rights violations'' under 502B — and would Israel''s conduct meet that threshold if applied equally to non-allied states?',
    'Comparative analysis: apply 502B statutory language to Israel''s documented conduct (West Bank settlement patterns, Gaza civilian casualties, detention practices) and compare against enforcement precedents for Syria, Iran, Russia, Yemen. Does the same evidence trigger enforcement for non-allies?',
    'If Israel meets threshold: non-enforcement is pure policy choice (constraint is Snare + Tangled Rope, not Mountain). If threshold is genuinely ambiguous: enforcement discretion is structural (constraint remains Tangled Rope but mandatrophy is less severe). If threshold materially differs for Israel: confirms statutory double standard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_trigger_threshold, empirical, 'Whether Israel''s documented conduct meets statutory human rights violation threshold').

omega_variable(
    congressional_override_feasibility,
    'Could Congress enforce 502B via legislation (defunding military aid, conditional appropriations) without triggering coalition collapse or presidential veto override failure?',
    'Legislative vote analysis; whip count for 2/3 veto override; constituent feedback modeling for swing districts; alliance stability gaming under different aid-withholding scenarios.',
    'If feasible: Congress has real exit option, constraint is more Tangled Rope (mixed) than Snare. If infeasible: Congress is trapped, constraint approaches pure extraction from legislative perspective. Determines whether Congress perspective classifies as Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_override_feasibility, empirical, 'Whether Congress has realistic legislative capacity to enforce 502B').

omega_variable(
    strategic_partnership_dependency,
    'How structurally dependent is US geopolitical positioning in Middle East on Israel military partnership specifically (versus alternative regional alignment structures)? Could US security interests be met via other regional arrangements?',
    'Strategic gaming analysis; counter-factual scenarios with different Israeli policies; assessment of US military requirements in region (logistics, intelligence, force projection) and their dependency on Israeli bases/cooperation vs alternatives.',
    'If highly dependent: non-enforcement is genuine coordination requirement (Executive Rope classification justified). If largely contingent: non-enforcement is discretionary extraction (Executive benefits but no coordination necessity). Determines whether beneficiary''s Rope classification is structurally justified or mislabeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_partnership_dependency, conceptual, 'Structural dependency of US Middle East strategy on Israeli partnership').

omega_variable(
    domestic_political_cost_of_enforcement,
    'What is the actual domestic political cost to a US administration of enforcing 502B against Israel — measured in lost campaign funding, media hostility, Congressional opposition — versus the framing as impossible?',
    'Historical precedent analysis (US enforcement against other major allies); donor sentiment surveys; media landscape analysis; comparison to political cost of other controversial aid decisions (Saudi Arabia, Egypt). Distinguish actual cost from politically-claimed impossibility.',
    'If cost is high but manageable: enforcement is difficult but possible (Congress exit is constrained, not trapped). If cost is genuinely prohibitive: Congress is trapped by structural political facts. If cost is overstated: non-enforcement is policy choice, not structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_political_cost_of_enforcement, empirical, 'Actual domestic political cost of enforcing 502B against Israel').

omega_variable(
    international_precedent_effect,
    'Does selective non-enforcement of 502B for Israel degrade enforcement against other countries? Do other states cite US-Israel non-enforcement as precedent when facing their own 502B reviews?',
    'Document review of State Department determinations for other countries post-2010; track citations to Israel precedent in formal determinations; interview State Department human rights bureau; analyze congressional testimony on comparative enforcement.',
    'If precedent effect is significant: international order extraction is real and measurable (Snare classification for international law perspective is correct). If precedent is contained: extraction is primarily bilateral (affects only Israel relationship). Affects scope of constraint''s reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_precedent_effect, empirical, 'Whether Israel non-enforcement precedent degrades 502B against other countries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_israel_faa_502b_nonenforcement, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(faa502b_tr_t0, us_israel_faa_502b_nonenforcement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(faa502b_tr_t15, us_israel_faa_502b_nonenforcement, theater_ratio, 15, 0.58).
narrative_ontology:measurement(faa502b_tr_t30, us_israel_faa_502b_nonenforcement, theater_ratio, 30, 0.81).

% Extraction over time
narrative_ontology:measurement(faa502b_be_t0, us_israel_faa_502b_nonenforcement, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(faa502b_be_t15, us_israel_faa_502b_nonenforcement, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(faa502b_be_t30, us_israel_faa_502b_nonenforcement, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_israel_faa_502b_nonenforcement, enforcement_mechanism).
narrative_ontology:affects_constraint(us_israel_faa_502b_nonenforcement, us_israel_strategic_partnership).
narrative_ontology:affects_constraint(us_israel_faa_502b_nonenforcement, middle_east_alignment_structure).
narrative_ontology:affects_constraint(us_israel_faa_502b_nonenforcement, global_human_rights_enforcement_credibility).

% DUAL FORMULATION NOTE:
% This constraint decomposes from the broader framing of 'US-Israel policy' into a specific structural mechanism: the statutory non-enforcement of human rights conditionality. The upstream constraint is US Middle East strategy (high-level geopolitical alignment); this constraint is the specific operational mechanism (502B statutory structure). They are linked but distinct — the geopolitical alignment could theoretically operate via other mechanisms (conditional aid tied to specific behaviors rather than human rights standards, or via alternative alliance partners). This story focuses on the 502B mechanism specifically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
