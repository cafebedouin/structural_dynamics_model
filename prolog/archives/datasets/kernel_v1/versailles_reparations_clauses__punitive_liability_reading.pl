% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__punitive_liability_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles Reparations Clauses (Punitive Liability Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   Article 231 of the Treaty of Versailles (1919) grounds German reparations
 *   liability in a 'War Guilt Clause' asserting that Germany bears unique
 *   moral and financial responsibility for all war damages and costs. This
 *   constraint story instantiates the PUNITIVE LIABILITY READING — one of
 *   three structurally distinct readings of this contested kernel. Under this
 *   reading, Germany's guilt is categorical and quasi-unlimited; reparations
 *   claims flow from a doctrine of total responsibility. This reading was the
 *   initial Allied interpretation and remained the formal legal position
 *   through the 1920s-1930s, even as economic mechanisms (Dawes Plan 1924,
 *   Young Plan 1929) began to scaffold the extraction through staged payment
 *   schedules and international financing. The punitive liability reading
 *   extracted maximum value during the hyperinflation crisis of 1923 (when
 *   suppression reached 0.85 via military occupation of the Ruhr) and began
 *   to erode as political forces in Germany repudiated the doctrine entirely
 *   (1933). The constraint exhibits classical snare dynamics at the German
 *   taxpayer perspective (trapped, powerless, bearing full extraction), rope
 *   dynamics at the Allied creditor perspective (beneficiary with arbitrage
 *   options), and transitional scaffold dynamics through the Dawes and Young
 *   Plans. The analytical observer who naturalizes reparations as an
 *   immutable law of international relations (mountain perspective) commits a
 *   false summit: the classification depends critically on the legal doctrine
 *   chosen (punitive vs. limited liability). The constraint's theater ratio
 *   rises sharply during hyperinflation (0.65 at 1923) when the formal
 *   reparations apparatus becomes increasingly performative as the German
 *   economy collapses, then moderates as Dawes Plan technical mechanisms take
 *   over.
 *
 * KEY AGENTS:
 *   - German Taxpayers/Workers: Primary victims (powerless/trapped) — bear extraction through taxation, inflation, austerity, and wage suppression across generational timeline
 *   - Weimar State Apparatus: Secondary victim/enforcer hybrid (powerless/trapped) — trapped between external reparations demands and domestic survival; must enforce extraction on own population
 *   - French Government (Primary Allied Beneficiary): Institutional beneficiary (institutional/arbitrage) — receives reparations for reconstruction; has exit via negotiation (Dawes Plan) and military leverage (Ruhr occupation)
 *   - British/American Financial Institutions: Secondary beneficiary (institutional/arbitrage) — finance German reparations via international loans (Dawes loans); arbitrage between lending and collection
 *   - Weimar Political Opposition/Revisionists: Organized agents (organized/constrained) — seek to renegotiate or repudiate liability; have constrained exit through diplomatic channels
 *   - League of Nations: Institutional authority (institutional/arbitrage) — maintains legal form of reparations doctrine; provides negotiation forum; theater-heavy role
 *   - Nazi Regime: Repudiation agent — unilaterally rejects punitive liability reading after 1933; represents endpoint of reading's decay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.68).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.72).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Reparations Clauses (Punitive Liability Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, 'a066fa2f-36df-416a-9e5b-ed20fffeaac8').
narrative_ontology:cs_kernel_codification('a066fa2f-36df-416a-9e5b-ed20fffeaac8', formalized).
narrative_ontology:cs_authority_grounding('a066fa2f-36df-416a-9e5b-ed20fffeaac8', extraction).
narrative_ontology:cs_interpretation_layer_present('a066fa2f-36df-416a-9e5b-ed20fffeaac8').
narrative_ontology:cs_reading_relation('a066fa2f-36df-416a-9e5b-ed20fffeaac8', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('a066fa2f-36df-416a-9e5b-ed20fffeaac8', versailles_reparations_clauses__repudiation_reading, influences).
narrative_ontology:cs_axiom('a066fa2f-36df-416a-9e5b-ed20fffeaac8', foundational, germany_bears_unique_moral_culpability_for_war).
narrative_ontology:cs_axiom_status(germany_bears_unique_moral_culpability_for_war, holdable).
narrative_ontology:cs_axiom_grounding('a066fa2f-36df-416a-9e5b-ed20fffeaac8', germany_bears_unique_moral_culpability_for_war, empirically_contingent).
narrative_ontology:cs_axiom('a066fa2f-36df-416a-9e5b-ed20fffeaac8', secondary, extractive_subordination_justified_by_culpability).
narrative_ontology:cs_axiom_status(extractive_subordination_justified_by_culpability, holdable).
narrative_ontology:cs_axiom_grounding('a066fa2f-36df-416a-9e5b-ed20fffeaac8', extractive_subordination_justified_by_culpability, deontological).
narrative_ontology:cs_reference_frame('a066fa2f-36df-416a-9e5b-ed20fffeaac8', allied_victors_cost_recovery_regime).
narrative_ontology:cs_drift_state('a066fa2f-36df-416a-9e5b-ed20fffeaac8', great_depression_and_regime_change, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a066fa2f-36df-416a-9e5b-ed20fffeaac8', '2026-02-26T14:33:22Z').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, french_reconstruction_priority).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_fiscal_sovereignty).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, weimar_state_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GERMAN WORKER/TAXPAYER (SNARE) — Trapped by Article 231 liability without meaningful consent or exit capacity. Bears extraction through inflation, austerity, and wage suppression across a generational timeline. No alternative framework legitimizes refusal. Experienced as pure extraction with maximal suppression of exit options.
constraint_indexing:constraint_classification(versailles_reparations_clauses__punitive_liability_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WEIMAR STATE APPARATUS (SNARE) — Structurally trapped between external reparations demands and domestic legitimacy. Cannot default on claims (military occupation threat); cannot fund both reparations and state capacity (hyperinflation 1923, fiscal collapse). Suppression operates through military threat and economic strangulation. No exit at biographical horizon.
constraint_indexing:constraint_classification(versailles_reparations_clauses__punitive_liability_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: ALLIED CREDITOR STATES (ROPE) — France, Britain, and Belgium perceive reparations as legitimate debt recovery and coordination mechanism for European reconstruction. From their perspective, the constraint solves the allocation problem: who bears the cost of war? Germany is the clear answer under punitive liability doctrine. They have exit via renegotiation and enforcement leverage. Experienced as pure coordination with significant benefit.
constraint_indexing:constraint_classification(versailles_reparations_clauses__punitive_liability_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: GERMAN POLITICAL OPPOSITION (TANGLED ROPE) — Organized agents (Weimar democrats, industrialists, nationalist revisionists) see reparations as both coordination mechanism (stabilizing European order through cost-sharing) and extractive imposition (unjust burden allocation). They have constrained exit: can renegotiate via Dawes Plan or Young Plan, but cannot reject entirely. Mixed classification reflects both benefits of reduced chaos and harms of fiscal subordination.
constraint_indexing:constraint_classification(versailles_reparations_clauses__punitive_liability_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: INTERNATIONAL FINANCIAL FRAMEWORK (SCAFFOLD) — The Dawes Plan (1924) and Young Plan (1929) represent temporary coordination overlays that reduce pure extraction through staged payment schedules, international loan mechanisms, and renegotiation pathways. These structures have sunset logic: economic recovery makes reparations sustainable; recession triggers revision. Theater is moderate (formal financial governance structures); effective extraction is dampened by staged implementation and modification capacity.
constraint_indexing:constraint_classification(versailles_reparations_clauses__punitive_liability_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: LEAGUE OF NATIONS / INTERNATIONAL LEGAL AUTHORITY (PITON) — The formal legitimizing apparatus (legal pronouncements, treaty ratification, League neutrality) performs authority maintenance even as the reparations mechanism deteriorates toward default (1932-1933). Theater is high: legal form persists even as functional enforcement erodes. The League sees reparations as a settled legal question despite mounting evidence of unsustainability. Institutional inertia maintains the performative apparatus.
constraint_indexing:constraint_classification(versailles_reparations_clauses__punitive_liability_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT) — A civilizational view claims that reparations are an immutable natural law of international relations: 'The aggressor always bears the costs of war it initiates.' This appears as mountain because it universalizes a contingent political doctrine. The engine will detect false summit: identifiable beneficiaries exist (Allied states, reconstruction programs), suppression mechanisms are observable (military occupation, economic pressure), and the classification depends on the legal framing chosen (punitive vs. limited liability). The mountain is a naturalization of political choice.
constraint_indexing:constraint_classification(versailles_reparations_clauses__punitive_liability_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(versailles_reparations_clauses__punitive_liability_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(versailles_reparations_clauses__punitive_liability_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, TR),
    TR >= 0.70.

:- end_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.68): Moderate-high. German fiscal subordination to external creditor claims is substantial but not maximal (hence 0.68 rather than 0.80+). The reading permits renegotiation (Dawes, Young Plans), which provides some agency to target. Initial extractiveness (1919) is lower (0.45) because suppression mechanisms are not yet fully operational; hyperinflation crisis (1923) drives it to peak (0.72) as military occupation (Ruhr) combines with economic collapse; renegotiation frameworks (1924+) moderate it toward 0.55-0.58. SUPPRESSION (0.72): High. Multiple suppression mechanisms operate: (1) Military occupation credibility (Rhineland occupation, Ruhr military intervention 1923), (2) Economic strangulation (trade barriers, capital flight, reparations priority over German imports), (3) Legal doctrine (Article 231 naturalized as binding, delegitimizing refusal), (4) Institutional hierarchy (Allied states can enforce, German state cannot). Suppression starts at 0.85 (1919, immediate occupation) and moderates to 0.58 by 1928 as occupation zone confidence increases and Dawes mechanisms stabilize. THEATER RATIO (0.58): Moderate. The formal legal apparatus (League of Nations, treaty texts, legal opinions) performs authority maintenance, but functional verification of the liability claim is weak. The 'war guilt' premise is contested historiographically; the causality linking German initiation to 'all war costs' is philosophically and empirically ambiguous. Theater rises sharply to 0.65 during hyperinflation (1923) when the reparations mechanism becomes purely performative — the German state cannot pay, but the Allied demand persists. Dawes scaffolding reduces theater to 0.58 by introducing technical financial mechanisms that substitute for moral adjudication. CLAIMED TYPE (Snare): Pure extraction without meaningful coordination function. The constraint does not solve a genuine collective action problem for Germany; it imposes external will on a defeated state. The beneficiaries (Allied creditor states) do receive coordination benefits (systematic cost recovery, order establishment), but this is coordination among the beneficiaries, not with the target. From the German perspective, there is no coordination — only extraction. The snare classification holds when directionality is computed from the powerless German perspective.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence. German workers/taxpayers perceive snare (pure extraction, no exit, generational burden); Allied creditors perceive rope (coordination mechanism for reconstruction, arbitrage exit options); organized German opposition perceives tangled rope (mixed coordination and extraction, constrained negotiation pathways); the League perceives piton (legal form maintained, functional enforcement eroding); the analytical observer who naturalizes reparations risks perceiving mountain (immutable law of war outcomes). The gap reveals that classification is radically observer-dependent. The same structural constraint — Article 231 liability — appears immobile from Germany's structural position and mobile (renegotiable) from the Allied creditor position. The Dawes and Young Plans create a scaffold perspective that partially resolves the gap by converting snare mechanics into staged, technically managed frameworks with built-in revision mechanisms. The False Summit perspective (mountain) demonstrates how naturalizing political doctrines into 'natural law' occludes structural agency: reparations are not an inherent feature of international relations; they are a contingent choice that depends on the legal reading adopted.
 *
 * DIRECTIONALITY LOGIC:
 *   GERMAN TAXPAYER (d ≈ 0.95, powerless/trapped): Derived from victim status + no exit options. The German worker bears extraction with no structural alternative pathway (no arbitrage, no emigration capacity, no domestic political exit during occupation period). The sigmoid f(d) ≈ 1.42 applies maximum multiplier. WEIMAR STATE (d ≈ 0.88, powerless/trapped): Derived from victim status (caught between external demands and internal legitimacy) + severely constrained exit. Structural mobility exists (could theoretically repudiate, as Nazi regime later did), but biographical costs are extreme (military invasion, occupation expansion, capital flight). The state is trapped at biographical horizon even if mobile at generational horizon. ALLIED CREDITORS (d ≈ 0.12, institutional/arbitrage): Derived from beneficiary status + arbitrage exit options. France and Britain receive reparations but can renegotiate, defer, or forgive without structural collapse (Hoover Moratorium, later forgiveness). The sigmoid f(d) ≈ -0.01 applies near-zero or negative multiplier — these agents experience the constraint as coordination (beneficial) rather than extraction (costly). The directionality derivation matches the perspectival classification: beneficiaries see rope, targets see snare.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED VIA POLITICAL REPUDIATION AND ECONOMIC DEFAULT (Type 2 resolution). The punitive liability reading does not resolve through Tangled Rope internal balancing or Scaffold sunset mechanisms. Instead, it collapses under the weight of: (1) Economic unsustainability (1931 Hoover Moratorium effectively ends payments), (2) Political regime change (Nazi repudiation 1933 formally rejects Article 231 doctrine), (3) World depression reducing capacity for all reparations. The theater ratio does not rise to 1.0 because the formal legal apparatus remains nominally in place even after enforcement ceases. The extractiveness drops sharply (0.68 → 0.15 by 1933) not because suppression is removed, but because the target's structural refusal (political repudiation) breaks the mechanism. The classification at t=14 (1933) shifts from Snare toward Piton (theatrical maintenance of defunct claim) or Mountain-from-repudiation-reading (natural law of international power overriding legal form). This reading's mandatrophy is resolved by the successful operation of the rival repudiation reading, which becomes politically dominant after 1933. The punitive liability reading does not self-correct into a sustainable form; it is defeated by a competing political reading that denies its legitimacy entirely. The engine should note this as a case where mandatrophy resolution occurs via reading displacement rather than internal constraint evolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_ambiguity_war_liability,
    'What causal chain establishes Germany''s unique liability for total war costs? Is it initiation of war, alliance choices, military strategy, or domestic political configuration?',
    'Historiographical analysis of causal responsibility for war; comparison of German initiation/alliance decisions vs. Allied decision-making; counterfactual analysis of alternative diplomatic pathways',
    'Narrow causality (Germany initiated) → stronger liability claim. Broad causality (systemic European militarism, arms races, alliance rigidity) → shared responsibility → reparations become unfair extraction. The reading''s entire normative force depends on causality framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_ambiguity_war_liability, conceptual, 'Causal chain establishing unique German liability').

omega_variable(
    proportionality_threshold_unknown,
    'What reparations sum constitutes ''just repayment'' vs. ''extractive impoverishment''? Is Article 231 claim-unlimited or implicitly capped by proportionality?',
    'Economic analysis comparing reparations burden to German GDP, state capacity, and international precedent; comparison to Article 231''s text vs. jurists'' interpretations; longitudinal tracking of explicit vs. implicit ceilings across negotiation rounds (Dawes, Young, Hoover Moratorium)',
    'If proportionality is implicit floor: classification shifts toward Tangled Rope (mixed coordination + extraction within bounds). If unlimited: classification remains Snare (pure extraction). The reading''s mandate depends on this ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_threshold_unknown, empirical, 'Implicit proportionality bounds on reparations claims').

omega_variable(
    reading_alternative_foreclosure,
    'Does the punitive liability reading logically foreclose the limited responsibility reading, or do they coexist as contested readings held by different parties?',
    'Textual analysis of Article 231 preamble and reparations clauses; examination of whether both readings can be held within a single legal framework (interpretive pluralism) or whether accepting one requires rejecting the other''s core premise. Historical analysis of whether both were genuinely live positions among jurists and negotiators.',
    'If forecloses: the reading is absolutist; the limited responsibility position is logically incoherent within this framework. If coexists: the reading is one position in a contested field; the architecture supports multiple readings. This determines whether the engine flags the reading as rigid vs. flexible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_alternative_foreclosure, conceptual, 'Logical relationship between punitive and limited liability readings').

omega_variable(
    suppression_mechanism_visibility,
    'Is the high suppression (0.72) driven primarily by military occupation credibility, economic strangulation capacity, or the legal doctrine''s normative force?',
    'Comparative analysis of enforcement trajectory: which suppression mechanism was operative at each reparation negotiation milestone (Versailles, Ruhr occupation 1923, Dawes 1924, Young 1929, Hoover Moratorium 1931)? Which mechanism would have failed first if removed?',
    'If military occupation is primary: suppression is contingent on occupation (falls when occupation ends). If economic strangulation is primary: suppression persists through trade leverage. If legal doctrine is primary: suppression is ideological (persists through internalization). Mechanism determines the constraint''s stability across the time interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_visibility, empirical, 'Primary suppression mechanism maintaining reparations extraction').

omega_variable(
    mandatrophy_resolution_path,
    'Does the punitive liability reading resolve mandatrophy through explicit legal supersession (e.g., Locarno, Kellogg-Briand), economic default cascading to political overthrow, or gradual erosion into scaffolded framework (Dawes/Young)?',
    'Timeline analysis of reparations intensity/enforceability: track base_extractiveness and theater_ratio across treaty evolution (Versailles → Dawes → Young → Hoover Moratorium → Nazi repudiation). Identify which mechanism (legal, economic, or political) dissolved the punitive liability doctrine.',
    'Legal supersession → clean transition to alternative reading. Economic cascade → reading persists but loses functional power (becomes piton). Political overthrow → reading repudiated by German state, remains contested at international level. Path determines how the engine classifies post-1933 reparations claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_resolution_path, empirical, 'Mechanism of mandatrophy resolution in reparations regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vrep_theater_1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(vrep_theater_1923_hyperinflation, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 4, 0.65).
narrative_ontology:measurement(vrep_theater_1924_dawes, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(vrep_theater_1928_young, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 9, 0.54).

% Extraction over time
narrative_ontology:measurement(vrep_extractiveness_1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vrep_extractiveness_1923, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 4, 0.72).
narrative_ontology:measurement(vrep_extractiveness_1924_dawes, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(vrep_extractiveness_1928_young, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 9, 0.58).
narrative_ontology:measurement(vrep_extractiveness_1931_moratorium, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(vrep_extractiveness_1933_repudiation, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 14, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(vrep_suppression_1919_occupation, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(vrep_suppression_1923_ruhr, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 4, 0.72).
narrative_ontology:measurement(vrep_suppression_1924_dawes_renegotiation, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(vrep_suppression_1928_young_framework, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, weimar_hyperinflation_1923).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, dawes_plan_technical_scaffolding).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, young_plan_renegotiation_framework).

% DUAL FORMULATION NOTE:
% The Versailles reparations regime decomposes into three structurally distinct constraint readings: (1) PUNITIVE LIABILITY READING (this story, ε=0.68, Snare from German perspective) — interprets Article 231 as categorical and quasi-unlimited liability; (2) LIMITED RESPONSIBILITY READING (ε≈0.45, Tangled Rope) — interprets Article 231 within economic capacity bounds and reciprocal Allied accountability; (3) REPUDIATION READING (ε≈0.05-0.10, Mountain-of-political-sovereignty) — denies entire Article 231 framework as invalid. These are not observations of the same constraint; they are instantiations of different readings of the contested kernel 'versailles_reparations_clauses'. The different ε values reflect the reading's structural claim: punitive liability claims maximum extraction; limited responsibility permits moderate extraction within bounds; repudiation permits zero extraction (the target's sovereignty is the natural law). Each reading has its own beneficiary/victim structure, its own perspectives, and its own temporal trajectory. The Dawes and Young Plans represent technical scaffolding that partially bridges punitive and limited readings by introducing renegotiation mechanisms. The hyperinflation crisis (1923) demonstrates the punitive reading's peak suppression moment. The repudiation reading becomes politically dominant after 1933, effectively displacing the punitive reading from international practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__punitive_liability_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
