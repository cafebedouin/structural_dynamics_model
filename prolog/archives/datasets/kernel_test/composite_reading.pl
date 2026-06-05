% ============================================================================
% CONSTRAINT STORY: composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_composite_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: composite_reading
 *   human_readable: Dueling's Decline as Overdetermined Composite Process
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   Dueling's decline across the 18th and early 19th centuries has been
 *   attributed to multiple, reinforcing mechanisms: cultural delegitimization
 *   by Enlightenment thought and Christian revival; material economic
 *   transformation reducing dependence on honor-based reputation; state
 *   enforcement escalation through legal prohibition and execution of nobles;
 *   and institutional substitution through courts, professional credentials,
 *   and market-based status signaling. The composite_reading instantiates the
 *   hypothesis that these mechanisms CONVERGE and REINFORCE rather than
 *   operating independently. Cultural unthinkability becomes more powerful
 *   when state authority simultaneously monopolizes violence; market
 *   expansion becomes acceptable when honor codes are already delegitimized;
 *   legal prohibition becomes enforceable when cultural consensus shifts.
 *   This is the reading of a contested kernel — 'honor settlement legitimacy'
 *   — where the composite_reading claims that multiple structural shifts
 *   conspired to render honor-based conflict resolution both culturally
 *   unthinkable AND practically unnecessary AND institutionally foreclosed.
 *   The ε value (0.52) reflects the empirical reality that no single
 *   mechanism dominates; the suppression value (0.65) reflects the
 *   multi-layered barriers that made exit from honor codes structurally
 *   overdetermined.
 *
 * KEY AGENTS:
 *   - Aristocratic Duelist: Primary victim (powerless/trapped/identity_locked) — bears full extraction cost through coercion into participation and eventual exclusion from social reproduction when practice delegitimizes
 *   - Honor-Bound Community: Secondary victim (moderate/constrained) — benefits from conflict resolution function but also bears participation costs; community gradually fragments as members shift to alternative status mechanisms
 *   - State Legal Authority: Primary beneficiary (institutional/arbitrage) — extracts monopoly over violence and legitimate dispute resolution; enforces through escalating legal penalties
 *   - Commercial Bourgeoisie: Secondary beneficiary (institutional/arbitrage) — benefits from replacement of honor codes with market-based status systems that align with economic activity
 *   - Market-Based Status System: Institutional beneficiary (organized/arbitrage) — educational credentials, professional titles, and wealth accumulation gradually displace honor codes as status-signaling mechanisms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — tracks overdetermination: all mechanisms operate simultaneously, creating irreversibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(composite_reading, 0.52).
domain_priors:suppression_score(composite_reading, 0.65).
domain_priors:theater_ratio(composite_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(composite_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(composite_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(composite_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(composite_reading, tangled_rope).
narrative_ontology:human_readable(composite_reading, "Dueling's Decline as Overdetermined Composite Process").
narrative_ontology:topic_domain(composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(composite_reading, '52b6fd61-d76c-4a2d-9531-a92aefdfb049').
narrative_ontology:cs_created_at('52b6fd61-d76c-4a2d-9531-a92aefdfb049', '').
narrative_ontology:cs_kernel_codification('52b6fd61-d76c-4a2d-9531-a92aefdfb049', distributed).
narrative_ontology:cs_authority_grounding('52b6fd61-d76c-4a2d-9531-a92aefdfb049', extraction).
narrative_ontology:cs_kernel_id(composite_reading, honor_settlement_legitimacy).
narrative_ontology:cs_reading_relation('52b6fd61-d76c-4a2d-9531-a92aefdfb049', contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('52b6fd61-d76c-4a2d-9531-a92aefdfb049', drop_reading, coexists_with).
narrative_ontology:cs_axiom('52b6fd61-d76c-4a2d-9531-a92aefdfb049', foundational, multiple_mechanisms_converge).
narrative_ontology:cs_axiom_status(multiple_mechanisms_converge, holdable).
narrative_ontology:cs_axiom_grounding('52b6fd61-d76c-4a2d-9531-a92aefdfb049', multiple_mechanisms_converge, empirically_contingent).
narrative_ontology:cs_axiom('52b6fd61-d76c-4a2d-9531-a92aefdfb049', secondary, extraction_overdetermined_irreversible).
narrative_ontology:cs_axiom_status(extraction_overdetermined_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('52b6fd61-d76c-4a2d-9531-a92aefdfb049', extraction_overdetermined_irreversible, empirically_contingent).
narrative_ontology:cs_reference_frame('52b6fd61-d76c-4a2d-9531-a92aefdfb049', honor_settlement_legitimacy).
narrative_ontology:cs_drift_state('52b6fd61-d76c-4a2d-9531-a92aefdfb049', industrial_bourgeois_era, gap(axiom_overriding, severe, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(composite_reading, state_legal_authority).
narrative_ontology:constraint_beneficiary(composite_reading, commercial_bourgeoisie).
narrative_ontology:constraint_victim(composite_reading, honor_based_conflict_resolution).
narrative_ontology:constraint_victim(composite_reading, aristocratic_social_reproduction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARISTOCRATIC DUELIST (SNARE) — Honor code remains constitutive of social identity and reputation; participation in dueling appears necessary for maintaining status despite legal prohibition. Exit means social death. The constraint traps through internalized cultural unthinkability of non-participation, reinforced by material exclusion from patronage networks and institutional access.
constraint_indexing:constraint_classification(composite_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HONOR-BOUND COMMUNITY (TANGLED ROPE) — Dueling provides genuine coordination function: signaling trustworthiness, settling disputes without invoking state authority, maintaining internal status hierarchy. But participation carries extraction costs (death, injury, legal penalty, social ostracism). The constraint exhibits both coordination (honor codes do solve internal legitimacy problems) and asymmetric extraction (some agents bear disproportionate costs through coercion into participation).
constraint_indexing:constraint_classification(composite_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONFLICT SETTLEMENT FUNCTION (ROPE) — From the pure coordination standpoint, dueling solves the problem of dispute resolution in contexts where state authority is distant, unreliable, or delegitimized. Gentlemen settling disputes through combat avoids expensive litigation and maintains honor codes without invoking external authority. This perspective sees low-extraction coordination.
constraint_indexing:constraint_classification(composite_reading, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: STATE LEGAL AUTHORITY (TANGLED ROPE) — State benefits from monopolization of violence (extraction) while also coordinating alternative dispute settlement through courts. Legal prohibition of dueling is genuine institutional enforcement requirement, but state also tolerates selective non-enforcement to maintain aristocratic loyalty. Mixed coordination (providing alternative legitimacy pathway) and extraction (claiming monopoly over justice).
constraint_indexing:constraint_classification(composite_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: BOURGEOIS MARKET EXPANSION (SCAFFOLD) — Commercial economy creates alternative status-signaling mechanisms (wealth, education, institutional position) that gradually reduce dependence on honor codes. Market-based reputation gradually replaces honor-based reputation. This perspective sees dueling as temporary coordination mechanism with declining function as material conditions shift. χ remains moderate but theater_ratio falls as honor-based signaling yields to market-based alternatives.
constraint_indexing:constraint_classification(composite_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From the long-term structural perspective, dueling persists as a degraded institutional form maintained by theatrical performance of honor rather than genuine functional necessity. Actual participation declines while the normative claim to honor codes persists through literary representation, historical romance, and residual institutional structures. Theater_ratio = 0.58 captures this bifurcation: much performance, declining function.
constraint_indexing:constraint_classification(composite_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(composite_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(composite_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(composite_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(composite_reading, TR),
    TR >= 0.70.

:- end_tests(composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting that the constraint operates through multiple overlapping mechanisms. The initial extractiveness (0.68) is high because dueling was fully embedded in aristocratic social reproduction — participation was non-optional and carried severe costs (death, injury, legal penalty). The decline to 0.52 reflects partial substitution of alternative legitimacy mechanisms, but extractiveness remains moderate because cultural unthinkability and state prohibition still impose penalties on would-be practitioners. Suppression (0.65): High, reflecting multiple layers of barrier to exit. Material barriers: alternative status mechanisms are available but require capital/education that not all agents can access. Legal barriers: explicit prohibition with execution penalties. Cultural barriers: honor codes remain internalized even as they lose legitimacy, making exit feel like moral failure. Institutional barriers: professional and educational institutions increasingly require renunciation of honor codes as condition of membership. Theater ratio (0.58): Moderate-high, reflecting that by the end of the interval, dueling persists largely through performative retention — literary representation, historiographical romance, residual normative claims — even as actual participation has become rare. The increasing theater ratio (0.35 → 0.58) indicates growing bifurcation between normative claims and practice.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival divergence. The aristocratic duelist experiences maximum extraction (Snare): participation is structurally non-optional despite growing cultural and legal opposition, and exit is identity-dissolving (identity_locked exit). The honor-bound community experiences mixed coordination and extraction (Tangled Rope): dueling genuinely solves internal status problems, but increasingly at unbearable cost. The conflict settlement function sees low-extraction coordination (Rope): dueling works for its intended purpose, independent of broader cultural shifts. The state sees extraction plus coordination (Tangled Rope): monopolizing violence while substituting court-based legitimacy. The market expansion sees temporary coordination with a sunset (Scaffold): honor codes will be fully replaced by market mechanisms on a generational timescale. The analytical observer sees degraded institutional persistence (Piton): the practice persists through theater and historical inertia even as functional necessity declines. No single perspective captures the overdetermination; each observes one mechanism or one level of the multi-layered suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent's structural position and exit options. The aristocratic duelist has d ≈ 0.92 (nearly full target): identity_locked exit means even technical mobility yields no practical choice; the agent experiences maximum f(d) ≈ 1.38. The honor-bound community has d ≈ 0.65 (mostly target): constrained exit with some agency; f(d) ≈ 1.00. The state has d ≈ 0.15 (mostly beneficiary): arbitrage exit and beneficiary status yield low d; f(d) ≈ -0.01. The market expansion has d ≈ 0.20 (beneficiary): arbitrage exit; f(d) ≈ 0.02. These derivations reflect that the constraint's extractiveness is NOT evenly distributed — it concentrates heavily on agents with identity-locked or trapped exit options, while running toward institutional actors with arbitrage options. This asymmetry is the signature of a Tangled Rope at the institutional level (mixed coordination + extraction) combined with Snare-like extraction at the individual level.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading avoids mandatrophy by explicitly modeling the constraint as TANGLED ROPE — mixed coordination and extraction. The honor-based conflict settlement DOES provide genuine coordination function (settling disputes without state authority), but it ALSO operates as an extraction mechanism concentrating costs on trapped agents and benefits on institutional beneficiaries. The perspectives show that the distinction between 'coordination' and 'extraction' is fundamentally observer-dependent: from the settlement function's view, the constraint is Rope (low extraction); from the trapped aristocrat's view, it is Snare (pure extraction); from the state's view, it is Tangled Rope (both coordination and extraction). The composite reading resolves this by accepting that all observations are correct — the constraint IS both coordination and extraction, experienced differently depending on structural position. The theater ratio shows that the functional coordination is declining (theater increases) while the extractive suppression persists (suppression remains high), indicating degradation toward Piton over the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_overdetermination,
    'Which causal pathway — cultural delegitimization, material economic transformation, state enforcement escalation, or institutional substitution — is the primary driver of dueling''s decline?',
    'Temporal sequencing analysis: which mechanism precedes the decline in dueling frequency? Comparative institutional analysis: do regions with strong state enforcement show faster decline than regions with market expansion but weak enforcement? Rhetorical analysis: do contemporary discussions emphasize cultural unthinkability vs. practical unfeasibility?',
    'If cultural delegitimization dominates: constraint is primarily Snare (identity_locked) → identity-locked victim perspective becomes central. If material economic transformation dominates: constraint is Scaffold with genuine sunset logic. If state enforcement dominates: constraint is Snare with external suppression. If institutional substitution dominates: constraint is Rope with low theater. The composite reading holds that ALL mechanisms operate; sibling readings isolate individual pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_overdetermination, empirical, 'Which causal mechanism primarily drives dueling''s decline').

omega_variable(
    cultural_unthinkability_mechanism,
    'Is the decline of dueling driven by genuine cultural delegitimization (the practice becomes morally unthinkable) or by strategic abandonment (the practice becomes strategically irrational while remaining culturally available)?',
    'Literary and rhetorical analysis: do texts show dueling framed as immoral/unthinkable (cultural) vs. as outdated/ineffective (strategic)? Ethnographic analysis: do honor-culture communities continue to endorse dueling norms while declining participation, or do they explicitly repudiate the norms? Exit interviews and memoirs: do agents cite moral conviction or practical pressure?',
    'If cultural delegitimization dominates: the constraint is identity_locked at the aristocratic perspective — agents cannot participate because they have ceased to be the kind of person for whom dueling makes sense. If strategic abandonment dominates: the constraint is constrained or mobile — agents CAN participate but choose not to for rational reasons. This affects whether the classification is Snare (identity-fusion binding) or Tangled Rope (external coercion + mixed benefits).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_unthinkability_mechanism, conceptual, 'Whether decline reflects cultural delegitimization or strategic abandonment').

omega_variable(
    alternative_legitimacy_sufficiency,
    'Do market-based status mechanisms (wealth, education, institutional position, literary fame) fully substitute for honor-based reputation, or do they constitute a parallel system that leaves honor codes partially functional?',
    'Prosopographic analysis: do elite status hierarchies shift from honor-based (genealogy, martial prowess, dueling record) to wealth/education based? Do markets in status goods (educational credentials, professional titles, literary awards) expand to cover functions previously served by dueling? Do residual honor-code communities persist in sectors where market alternatives remain weak?',
    'If full substitution: Scaffold classification holds — the sunset is real and material. If partial parallel: dueling persists in sectors with weak market alternatives, suggesting Tangled Rope rather than Scaffold. This affects whether the constraint has a genuine time-bound exit or merely apparent decline masking persistent extraction in residual populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimacy_sufficiency, empirical, 'Whether market mechanisms fully substitute for honor-based legitimacy').

omega_variable(
    reading_distinction_from_siblings,
    'What structural claim distinguishes the composite_reading from the contraction_reading and drop_reading?',
    'Theoretical definition: composite_reading claims that multiple causal pathways CONVERGE and REINFORCE; neither contraction alone (cultural unthinkability) nor drop alone (state enforcement + legal substitution) fully explains the data — the mechanisms amplify each other. If empirical analysis shows that removing any single mechanism still yields decline (overdetermination), the composite reading holds. If removing one mechanism halts the decline, a single-pathway reading is more parsimonious.',
    'If composite mechanisms truly reinforce: ε remains 0.52 (moderate extraction in a mixed mechanism). If one mechanism dominates: ε should be reclassified to a sibling reading. The composite reading''s claim is that the mechanisms CONVERGE — that cultural delegitimization makes legal enforcement more acceptable, that market expansion makes honor codes less necessary, that state monopoly claims reinforce market-based status systems. Testing this claim requires showing positive correlation between mechanism strength and decline velocity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_distinction_from_siblings, empirical, 'Whether multiple causal mechanisms converge and reinforce dueling''s decline').

omega_variable(
    kernel_framing_contest,
    'Is honor settlement legitimacy itself contested as a kernel concept, or is there consensus that honor codes WERE legitimate but ARE NOW delegitimized?',
    'Historiography and normative theory: do contemporary theorists dispute whether honor-based conflict resolution was ever legitimately grounded, or do they agree on the historical legitimacy but dispute the causes of its decline? Do any contemporary honor-culture communities actively defend dueling as normatively justified (not just historically practiced), or has normative defense abandoned the practice?',
    'If the kernel itself is contested (some frames hold honor legitimate, others do not): the constraint reflects deeper disagreement about legitimate authority and conflict resolution — this is a reading of a genuinely divided kernel. If the legitimacy is historically settled but the CAUSES of decline are contested: the sibling readings represent different causal theories of the same historical fact. The committer frame suggests the former (a truly contested kernel), but the hypothesis points toward the latter (consensus on facts, disagreement on causes).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_contest, conceptual, 'Whether honor settlement legitimacy is itself a contested kernel or merely a historically settled practice with disputed causes of decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(composite_reading, 1650, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, composite_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comp_tr_t15, composite_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(comp_tr_t30, composite_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, composite_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(comp_be_t15, composite_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(comp_be_t30, composite_reading, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(composite_reading, state_monopoly_violence).
narrative_ontology:affects_constraint(composite_reading, market_status_substitution).
narrative_ontology:affects_constraint(composite_reading, aristocratic_institutional_reproduction).

% DUAL FORMULATION NOTE:
% The composite_reading forms a constraint family with contraction_reading and drop_reading. Each story has its own ε value reflecting the explanatory weight of its mechanism: contraction_reading emphasizes cultural delegitimization (likely ε ≈ 0.48, Tangled Rope); drop_reading emphasizes state enforcement (likely ε ≈ 0.55, Snare); composite_reading (ε = 0.52) holds that mechanisms converge. The network edges reflect causal dependencies: state monopoly claims reinforce market expansion (state creates institutional space for commercial legitimacy); market expansion reduces functional necessity of honor codes (makes legal substitution more acceptable); aristocratic institutional reproduction failure drives agents toward market-based status (creates demand for educational/professional credentials). Each story emphasizes different edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(composite_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
