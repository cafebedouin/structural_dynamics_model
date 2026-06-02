% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: Article 17 Complementarity as International Oversight Mechanism (Victims-Centered Reading)
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute establishes the complementarity principle:
 *   the International Criminal Court's jurisdiction is triggered only when a
 *   state is 'unwilling or unable' to prosecute serious international crimes.
 *   This reading instantiates the international_oversight interpretation:
 *   complementarity functions as an accountability-trigger mechanism that
 *   empowers the ICC to intervene when domestic systems fail or are captured.
 *   From this perspective, Article 17 is a key mechanism for protecting
 *   victims in complicit or failed states from systematic impunity. However,
 *   this reading coexists with the national_primacy reading, which treats
 *   complementarity as a protective principle respecting state sovereignty
 *   and primary responsibility for prosecution. The international_oversight
 *   reading interprets 'unwilling or unable' broadly, capturing scenarios
 *   where elites strategically obstruct justice or perform accountability
 *   while protecting aligned actors (victor's justice). The constraint
 *   exhibits significant structural tension: it coordinates genuine
 *   accountability when functioning (benefiting victims, incentivizing state
 *   capacity-building) while simultaneously enabling selective enforcement
 *   biased toward weaker, non-aligned states and exempting powerful states
 *   through UN Security Council deferrals. The mechanism's extractiveness has
 *   increased from 0.35 (when ICC was nascent and focused on mass atrocities
 *   in weak-state contexts) to 0.58 (as pattern of victor's justice becomes
 *   visible and powerful states manipulate complementarity via proxy
 *   prosecutions). Theater ratio has risen from 0.52 to 0.65 as procedural
 *   requirements and admissibility assessments create appearance of rigorous
 *   gatekeeping without reliably filtering based on actual accountability
 *   quality.
 *
 * KEY AGENTS:
 *   - Victims in Complicit or Failed States: Primary victims (powerless/trapped) — benefit structurally from broad 'unwilling or unable' interpretation but face indefinite administrative delays and geopolitical selectivity. Zero exit options.
 *   - Transnational Justice Advocacy Networks: Moderate power (moderate/constrained) — benefit from complementarity (investigation leverage, norm-setting platform) while bearing disproportionate resource costs and reputational risks in complicit states. Constrained exit: can exit specific cases but not the field.
 *   - ICC Prosecutor's Office: Institutional beneficiary (institutional/arbitrage) — experiences complementarity as coordination mechanism that expands prosecutorial discretion. High exit flexibility: can select cases based on cooperativeness and visibility.
 *   - Complicit National Governments: Organized victims (organized/constrained) — formally obligated to prosecute or cooperate (enforced coordination) while extracting benefit from selective enforcement and weak definitions of 'unwilling or unable'. Cannot exit international law system wholesale.
 *   - Powerful States / P5 Members: Mobile beneficiaries (powerful/mobile) — extract maximum benefit from complementarity through UN Security Council deferrals (Article 16) and enforcement selectivity. Can override ICC accountability through geopolitical power.
 *   - International Legal Norm-Building Coalition: Organized stakeholders (organized/constrained) — see complementarity as temporary scaffold for building domestic accountability capacity. Constrained by slow institutional change but maintain strategic optimism about norm entrenchment.
 *   - Rome Statute Institutional Framework: Institutional actor (institutional/arbitrage) — maintains complementarity structure through inertia despite atrophied primary function. Theater ratio elevation indicates degradation from functional enforcement to performative procedure.
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent power distribution (P5 veto, non-signatory immunity) as immutable constraint on international accountability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.58).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.62).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity as International Oversight Mechanism (Victims-Centered Reading)").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, 'f16a6be4-7560-46ac-87cf-a6dd4b0cbade').
narrative_ontology:cs_kernel_codification('f16a6be4-7560-46ac-87cf-a6dd4b0cbade', formalized).
narrative_ontology:cs_authority_grounding('f16a6be4-7560-46ac-87cf-a6dd4b0cbade', extraction).
narrative_ontology:cs_interpretation_layer_present('f16a6be4-7560-46ac-87cf-a6dd4b0cbade').
narrative_ontology:cs_reading_relation('f16a6be4-7560-46ac-87cf-a6dd4b0cbade', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('f16a6be4-7560-46ac-87cf-a6dd4b0cbade', foundational, victims_agency_paramount).
narrative_ontology:cs_axiom_status(victims_agency_paramount, holdable).
narrative_ontology:cs_axiom_grounding('f16a6be4-7560-46ac-87cf-a6dd4b0cbade', victims_agency_paramount, deontological).
narrative_ontology:cs_axiom('f16a6be4-7560-46ac-87cf-a6dd4b0cbade', foundational, icc_intervention_legitimate_when_state_fails).
narrative_ontology:cs_axiom_status(icc_intervention_legitimate_when_state_fails, holdable).
narrative_ontology:cs_axiom_grounding('f16a6be4-7560-46ac-87cf-a6dd4b0cbade', icc_intervention_legitimate_when_state_fails, instrumental).
narrative_ontology:cs_reference_frame('f16a6be4-7560-46ac-87cf-a6dd4b0cbade', complementarity_as_victim_protection).
narrative_ontology:cs_drift_state('f16a6be4-7560-46ac-87cf-a6dd4b0cbade', contemporary_victor_justice_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f16a6be4-7560-46ac-87cf-a6dd4b0cbade', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_accountability_advocates).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, transnational_justice_networks).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, sovereign_state_discretion).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, complicit_national_governments).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, defendants_in_victor_justice_scenarios).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VICTIMS IN COMPLICIT/FAILED STATES (SNARE) — Trapped by absence of domestic accountability mechanisms; dependent on ICC intervention but ICC operates under selective geopolitical constraints. Maximum extraction: victims bear costs of protracted ICCadministrative delays (average 5-8 years to indictment), ongoing security risks, and zero guarantee of prosecution even when ICC investigates. Cannot exit or organize effective pressure. Experiences the complementarity mechanism as pure extraction when their state is unwilling/unable and ICC defers indefinitely.
constraint_indexing:constraint_classification(article_17_complementarity__international_oversight_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRANSNATIONAL JUSTICE ADVOCACY NETWORKS (TANGLED ROPE) — Moderate power; constrained by resource requirements and NGO-state relations. Benefit from complementarity framework (investigation leverage, norm-setting platform) while bearing costs (reputational risk in complicit states, threat to field staff). Experience mixed extraction: coordination function (gathering evidence, victim testimony protocols) alongside asymmetric burden (disproportionate resource cost relative to institutional support). Constrained exit: can exit specific cases but exit from the advocacy field itself carries high cost.
constraint_indexing:constraint_classification(article_17_complementarity__international_oversight_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ICC PROSECUTOR'S OFFICE (ROPE) — Experiences complementarity as pure coordination: the mechanism channels cases toward the ICC, provides legal standing for investigations, and enables resource prioritization. Operates with arbitrage exit: can select which cases to investigate based on cooperativeness, visibility, and political tolerance. Net beneficiary: complementarity expands ICC's mandate and prosecutorial discretion while providing legal cover ('states unwilling or unable') for enforcement selectivity. Sees the mechanism as functional coordination.
constraint_indexing:constraint_classification(article_17_complementarity__international_oversight_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPLICIT NATIONAL GOVERNMENTS (TANGLED ROPE) — Organized actors facing intermittent pressure; constrained exit (cannot exit international law system wholesale without severe economic/diplomatic cost). Experience complementarity as enforced coordination with embedded extraction: formally obligated to prosecute or cooperate (coordination function), but also extract benefit from selective enforcement (victor's justice narrative provides plausible deniability, weak definitions of 'unwilling or unable' enable continuing impunity for aligned elites). Suppression is high: international condemnation, sanctions threat, visa bans, ICC warrants function as coercive levers.
constraint_indexing:constraint_classification(article_17_complementarity__international_oversight_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POWERFUL STATES / P5 MEMBERS (SNARE) — Mobile actors with exit options (can override ICC through UN Security Council deferral under Article 16). Experience complementarity as pure extraction mechanism protecting other P5 members and aligned states from ICC scrutiny: the 'unwilling or unable' standard remains weak enough to shelter geopolitically valuable allies. High suppression: formal ICC immunity for non-signatory states (US, Russia, China) plus deferral power = zero effective constraint on P5 prosecution selectivity. Complementarity enforces accountability FOR weaker states WHILE exempting powerful ones.
constraint_indexing:constraint_classification(article_17_complementarity__international_oversight_reading, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL NORM-BUILDING COALITION (SCAFFOLD) — Organized advocates (treaty bodies, human rights courts, regional criminal courts) see complementarity as a temporary scaffold: the mechanism is gradually building norms of state accountability that will eventually render the ICC unnecessary or subordinate as domestic courts strengthen. Exit structure: the sunset clause is implicit — if domestic rule of law solidifies in currently weak-capacity states, ICC complementarity's role diminishes. Theater ratio is moderate (0.65): formal coordination protocols have genuine function, but also serve symbolic purpose of demonstrating commitment to accountability without guaranteeing outcomes. Coalition experiences mixed success but maintains strategic optimism about long-term norm entrenchment.
constraint_indexing:constraint_classification(article_17_complementarity__international_oversight_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ROME STATUTE INSTITUTIONAL FRAMEWORK (PITON) — The formal treaty apparatus persists largely through institutional inertia. Theater ratio is high (0.65): extensive procedural requirements (referrals, preliminary examinations, admissibility assessments) create appearance of rigorous gatekeeping without reliably filtering based on actual accountability quality. Primary function (guaranteeing prosecution for serious crimes) has atrophied due to geopolitical constraints and resource limits. The Rome Statute system maintains its structure because alternatives haven't fully replaced it and member states have institutional investment in the ICC, not because it achieves its stated accountability mission. Article 17's complementarity framework is a key component of this degradation: it permits states to claim accountability while delivering symbolic prosecution of low-level operatives.
constraint_indexing:constraint_classification(article_17_complementarity__international_oversight_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, state sovereignty is an immutable constraint on international accountability: no enforcement mechanism can compel compliance from a sufficiently powerful unwilling state. The complementarity framework is thus an inherent structural feature of international law, not a variable institutional choice. However, this perspective risks naturalizing what is actually a *contingent power distribution*. The engine's false summit detector will flag this: the 'immutability' is contingent on current geopolitical arrangements (P5 veto power, non-signatory enforcement immunity, deferral authority). The constraint appears as mountain only from the perspective of actors locked into current power structures.
constraint_indexing:constraint_classification(article_17_complementarity__international_oversight_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_17_complementarity__international_oversight_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_17_complementarity__international_oversight_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, TR),
    TR >= 0.70.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The international_oversight reading interprets Article 17 broadly, triggering ICC intervention when states are 'unwilling or unable' to prosecute. This extraction value reflects several mechanisms: (1) Complicit states bear costs of international pressure and potential sanctions despite extracting benefit from selective prosecution of low-level operatives. (2) Victims face indefinite delays (average 5-8 years to indictment) despite trigger mechanism existing. (3) Powerful states extract benefit through Article 16 deferrals while weaker states cannot. The 0.58 value (rather than 0.72+) reflects that genuine coordination exists: Article 17 does create incentive structures for state capacity-building, does provide framework for victim participation, and does generate some accountability outcomes. The extraction is real but not total. Suppression (0.62): High. Multiple layers prevent victims from exercising complementarity rights: ICC resource constraints limit investigation capacity; powerful states block prosecutions via UN deferrals; complicit governments obstructs investigations and witnesses; geopolitical alliances determine case selection. Theater ratio (0.65): High-moderate, rising over interval. Procedural formality of admissibility assessments (preliminary examinations, detailed investigations, lengthy deliberations) creates appearance of rigorous filtering while outcomes show pattern of victor's justice and elite immunity. The rising trajectory reflects increasing visibility of selective enforcement patterns, making procedural legitimacy more theater-dependent. Claimed type (Tangled Rope): Justified by (a) genuine coordination function (state capacity incentives, victim participation protocols), (b) asymmetric extraction (victims wait years; complicit states deliver low-level prosecutions; powerful states escape), (c) requires active enforcement (ICC investigations, state cooperation demands, UN deferrals).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across power positions. Victims in failed states experience Snare (trapped, indefinite extraction). Advocates experience Tangled Rope (benefit from leverage but constrained by resources). ICC Prosecutor experiences Rope (coordination mechanism expanding discretion). Complicit governments experience Tangled Rope (enforced obligations with extraction benefit). Powerful states experience Snare for their allies (can shield via deferrals). Norm-builders experience Scaffold (temporary mechanism with sunset as capacity builds). Rome Statute system itself is Piton (degraded through institutional inertia). Analytical observer risks Mountain (naturalizing geopolitical constraints). The perspectival gap is driven by: (1) Exit-option asymmetry: victims have zero exit; prosecutors have maximum arbitrage; powerful states have deferrals; advocates have constrained exit. (2) Beneficiary/victim differentiation: same mechanism benefits P5 members and disadvantages non-aligned states. (3) Temporal horizon: immediate appearance (Rope coordination) vs. biographical experience (Snare extraction) vs. civilizational structural constraint (Mountain).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the complementarity mechanism. Victims in complicit states: d ≈ 0.95 (full target; trapped exit means maximum f(d) ≈ 1.42). Advocates: d ≈ 0.60 (mixed beneficiary/victim; moderate power with constrained exit means moderate d, f(d) ≈ 1.00). ICC Prosecutor: d ≈ 0.10 (primary beneficiary; institutional power with arbitrage exit means low d, f(d) ≈ -0.01). Complicit governments: d ≈ 0.65 (mixed; organized power with constrained exit, but primarily victims of enforcement pressure, f(d) ≈ 1.15). Powerful states: d ≈ 0.05 (primary beneficiary; powerful with mobile exit via Article 16 deferrals, f(d) ≈ -0.12). No overrides needed: the structural derivation produces accurate d values from beneficiary/victim declarations and exit options. The directionality asymmetry (victims at d=0.95 vs. P5 at d=0.05) is the core feature of this reading — it instantiates the international_oversight axiom that accountability should flow upward from states to ICC when domestic systems fail, but in practice flows downward when powerful states intervene.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The international_oversight reading explicitly embraces the tension between genuine coordination (state capacity-building incentives, victim participation protocols) and systematic extraction (victor's justice, elite immunity, indefinite delays). The reading does not deny the coordination function; it acknowledges it while arguing that the extraction mechanisms are structurally dominant. This resolves mandatrophy by locating complementarity as a legitimate hybrid (Tangled Rope) where the asymmetries are not hidden but explicitly recognized as the cost of international accountability. The reading is mandatrophy-resolved in the sense that it acknowledges full structure: complementarity both coordinates AND extracts. The tension is not eliminated but made explicit. This differs from the national_primacy reading, which would resolve mandatrophy by emphasizing coordination and minimizing extraction (classifying as Rope). The international_oversight reading argues that victims' experience is Snare, not Rope — the extraction is real and measurable — and that acknowledging this is epistemically honest about what Article 17 delivers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unwilling_or_unable_threshold_ambiguity,
    'What concrete behavioral/institutional criteria distinguish a state that is genuinely ''unwilling or unable'' from one that strategically performs willingness while obstructing justice?',
    'Comparative analysis of Article 17 admissibility decisions; coding of prosecution timelines and conviction rates by state; empirical assessment of whether ''unwillingness'' correlates with state capacity vs. political will vs. elite protection strategy',
    'If ''unwilling or unable'' is interpreted narrowly (requires demonstrable sabotage of investigations, explicit impunity decrees): ICC expands intervention, complementarity becomes stricter enforcement mechanism (shifts toward Tangled Rope from state perspective, away from Snare from victim perspective). If interpreted broadly (any sluggishness, any elite protection): complementarity enables victor''s justice and complicit state impunity (reinforces Snare classification for victims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unwilling_or_unable_threshold_ambiguity, empirical, 'Behavioral/institutional criteria for unwillingness vs. inability').

omega_variable(
    victor_justice_versus_genuine_accountability,
    'Does the international_oversight_reading''s reliance on ''low admissibility threshold'' and ''broad interpretation'' of unwillingness create structural incentives for victor''s justice (prosecution of losers, immunity for winners)?',
    'Empirical analysis of ICC indictments by conflict outcome (winners vs. losers); comparison of state cooperation patterns with alliance relationships; historical comparison with post-WWII tribunals and selective prosecution patterns',
    'If victor''s justice signal is substantial and systematic: the constraint delivers extraction mechanism asymmetrically (reinforces Snare from victim perspective, complicates beneficiary claims for ''international advocates''). If distributional bias is minimal: complementarity functions as intended (Tangled Rope with genuine coordination component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victor_justice_versus_genuine_accountability, empirical, 'Empirical measurement of victor''s justice bias in complementarity application').

omega_variable(
    p5_veto_power_structural_contradiction,
    'Does Article 16''s UN Security Council deferral authority fundamentally contradict Article 17''s complementarity mechanism by permitting P5 members to shield aligned states from accountability even when ''unwilling or unable'' criteria are met?',
    'Examination of Article 16 deferrals in practice; analysis of whether P5 veto authority de facto exempts permanent members and their allies from complementarity obligations; assessment of whether the Rome Statute contains a structural inconsistency between Chapters VII and VIII',
    'If contradiction is fundamental (deferral authority nullifies complementarity for geopolitically aligned defendants): the mechanism functions as pure extraction for victims in non-aligned states (reinforces Snare classification). If contradiction is manageable through political pressure: complementarity retains some genuine constraint function (sustains Tangled Rope claim).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p5_veto_power_structural_contradiction, conceptual, 'Whether Article 16 veto undermines Article 17 complementarity').

omega_variable(
    state_capacity_versus_political_will_conflation,
    'Does the ''unwilling or unable'' standard conflate genuine state incapacity (lack of functional courts, trained prosecutors, forensic capacity) with strategic unwillingness (elite protection, political calculation)? Are these structurally distinguishable in admissibility practice?',
    'Detailed examination of ICC admissibility decisions to isolate capacity vs. will determinations; case-by-case assessment of whether capacity-building support was offered before complementarity intervention; correlation analysis of ICC interventions with prior state capacity assessments',
    'If conflation is systematic: ICC intervenes when capacity-building would suffice, undermining the complementarity principle and creating appearance of accountability without addressing root causes (reinforces Piton classification for Rome Statute system). If distinction is maintained: complementarity functions as intended incentive structure (sustains Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_versus_political_will_conflation, empirical, 'Conflation of state capacity and political will in admissibility determinations').

omega_variable(
    kernel_reading_identity_ambiguity,
    'This constraint instantiates the international_oversight_reading of the Article 17 complementarity kernel. What specific commitments about victims'' agency, state accountability hierarchy, and ICC legitimacy distinguish this reading from the national_primacy_reading, and what empirical or political facts would cause this reading to foreclose or be foreclosed by the sibling?',
    'Explicit statement of the axioms distinguishing the two readings (see cs_structure.axioms). Identification of what empirical findings or political developments would force revision of the foundational premises of this reading vs. the sibling. Historical tracking of how the two readings have competed in International Criminal Court jurisprudence.',
    'If the two readings genuinely coexist (different institutions, different states maintain different commitments): classification as Tangled Rope reflects institutional hybridity. If one reading is increasingly foreclosing the other (jurisprudential drift toward one axiom set): constraint classification may shift toward the foreclosing reading''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_ambiguity, conceptual, 'Kernel identity and relationship between international_oversight and national_primacy readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(compl_intl_theater_t0, article_17_complementarity__international_oversight_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(compl_intl_theater_t5, article_17_complementarity__international_oversight_reading, theater_ratio, 5, 0.6).
narrative_ontology:measurement(compl_intl_theater_t10, article_17_complementarity__international_oversight_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(compl_intl_extract_t0, article_17_complementarity__international_oversight_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(compl_intl_extract_t5, article_17_complementarity__international_oversight_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(compl_intl_extract_t10, article_17_complementarity__international_oversight_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(compl_intl_supp_t0, article_17_complementarity__international_oversight_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(compl_intl_supp_t5, article_17_complementarity__international_oversight_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(compl_intl_supp_t10, article_17_complementarity__international_oversight_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, un_security_council_article_16_deferral).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, victor_justice_in_international_tribunals).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, icc_prosecutor_selectivity_bias).

% DUAL FORMULATION NOTE:
% The article_17_complementarity kernel decomposes into two structurally distinct constraint readings: international_oversight_reading (this file, ε=0.58, Tangled Rope with extraction mechanisms dominant) and national_primacy_reading (sibling file, expected ε≈0.35-0.40, Rope with genuine coordination dominant). Each reading instantiates different axioms about state accountability hierarchy. The international_oversight reading emphasizes victims' agency and ICC intervention rights; the national_primacy reading emphasizes state primary responsibility and complementarity as protection against ICC overreach. These are not different measurements of one constraint but different structural interpretations of the Rome Statute kernel. The affects_constraints array links this reading to: (1) its sibling reading (national_primacy), (2) the UN Security Council Article 16 deferral mechanism (upstream constraint that structures complementarity's effectiveness), (3) victor's justice patterns that empirically instantiate the international_oversight reading's concerns, and (4) ICC prosecutor selectivity as the mechanism through which complementarity's extraction manifests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__international_oversight_reading, powerful, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
