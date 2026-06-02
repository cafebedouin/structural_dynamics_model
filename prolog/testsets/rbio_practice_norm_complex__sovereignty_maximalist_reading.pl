% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: RBIO Sovereignty-Maximalist Reading: Absolute State Sovereignty with Humanitarian Exception Skepticism
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   The sovereignty-maximalist reading of RBIO norms ('Responsibility to
 *   Protect is pretext for regime change; legitimate only when protecting
 *   state against external interference') instantiates one pole of a
 *   contested kernel: what legitimacy conditions justify humanitarian
 *   intervention in the international system? This constraint is one reading
 *   among three structurally distinct alternatives (liberal institutional,
 *   hegemonic extraction). The sovereignty-maximalist reading privileges
 *   state security over human protection, treating humanitarian exceptions as
 *   tools of powerful states to override weaker states' governance choices.
 *   The constraint exhibits all six classification types depending on the
 *   observer's structural position. For repressed populations and
 *   humanitarian norms themselves, it is a snare with no exit. For
 *   authoritarian beneficiaries, it is pure coordination (rope). For liberal
 *   states, it is mixed extraction-coordination (tangled rope). For the UN
 *   apparatus, it is largely performative (piton). For the analytical
 *   observer, it risks naturalizing a contingent institutional arrangement as
 *   immutable law (mountain). The extractiveness trajectory (0.52 → 0.68 over
 *   20 units) reflects increasing strategic invocation of the doctrine by
 *   authoritarian regimes as humanitarian norms gain international legitimacy
 *   — the reading functions as a counterweight, accumulating extraction power
 *   as the competing framework gains force. Theater ratio (0.48 → 0.58) shows
 *   rising performative content: the doctrine is invoked rhetorically by
 *   states that simultaneously engage in selective humanitarian intervention
 *   when geopolitically convenient.
 *
 * KEY AGENTS:
 *   - Authoritarian State Security Apparatus: Primary beneficiary (institutional/arbitrage) — gains cover for repression and insulation from humanitarian intervention claims
 *   - Repressed Populations: Primary victim (powerless/trapped) — lack external recourse; internal exit blocked by state apparatus
 *   - International Humanitarian Norms Corpus: Victim (abstract/trapped) — loses legitimacy and enforcement capacity through systematic downgrading relative to sovereignty
 *   - Liberal Democratic States: Mixed position (organized/constrained) — gain non-interference protection but lose legal justification for preferred interventions; constrained by the doctrine but also benefit from it
 *   - UN Security Council Permanent Members: Institutional beneficiary (institutional/arbitrage) — use sovereignty doctrine selectively, invoking it to block interventions against allies, ignoring it for preferred interventions against adversaries
 *   - International Court of Justice and Treaty Bodies: Institutional actor (institutional/constrained) — bound to apply the sovereignty-maximalist reading despite doctrinal incoherence when faced with atrocity cases
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the contingent Westphalian/post-1945 arrangement as inevitable structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.68).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, snare).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "RBIO Sovereignty-Maximalist Reading: Absolute State Sovereignty with Humanitarian Exception Skepticism").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'd2da1697-bf00-4f20-8e3a-65f50fb18782').
narrative_ontology:cs_kernel_codification('d2da1697-bf00-4f20-8e3a-65f50fb18782', formalized).
narrative_ontology:cs_authority_grounding('d2da1697-bf00-4f20-8e3a-65f50fb18782', extraction).
narrative_ontology:cs_interpretation_layer_present('d2da1697-bf00-4f20-8e3a-65f50fb18782').
narrative_ontology:cs_reading_relation('d2da1697-bf00-4f20-8e3a-65f50fb18782', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2da1697-bf00-4f20-8e3a-65f50fb18782', rbio_practice_norm_complex__hegemonic_extraction_reading, influences).
narrative_ontology:cs_axiom('d2da1697-bf00-4f20-8e3a-65f50fb18782', foundational, absolute_sovereignty_supreme_principle).
narrative_ontology:cs_axiom_status(absolute_sovereignty_supreme_principle, holdable).
narrative_ontology:cs_axiom_grounding('d2da1697-bf00-4f20-8e3a-65f50fb18782', absolute_sovereignty_supreme_principle, deontological).
narrative_ontology:cs_axiom('d2da1697-bf00-4f20-8e3a-65f50fb18782', foundational, humanitarian_intervention_as_regime_change_pretext).
narrative_ontology:cs_axiom_status(humanitarian_intervention_as_regime_change_pretext, holdable).
narrative_ontology:cs_axiom_grounding('d2da1697-bf00-4f20-8e3a-65f50fb18782', humanitarian_intervention_as_regime_change_pretext, empirically_contingent).
narrative_ontology:cs_reference_frame('d2da1697-bf00-4f20-8e3a-65f50fb18782', westphalian_absolute_sovereignty).
narrative_ontology:cs_drift_state('d2da1697-bf00-4f20-8e3a-65f50fb18782', contemporary_humanitarian_advocacy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d2da1697-bf00-4f20-8e3a-65f50fb18782', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governance).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_humanitarian_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REPRESSED POPULATIONS (SNARE) — Trapped without external recourse. The sovereignty-maximalist reading forecloses humanitarian intervention as illegitimate regime-change pretext, leaving victims with no structural exit. Suppression is total: the constraint denies legitimacy to external pressure, and internal exit is blocked by the authoritarian apparatus. Maximum extraction: the regime benefits from external non-interference while the population bears full cost of repression.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__sovereignty_maximalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HUMANITARIAN NORMS AS VICTIM (SNARE) — The norms themselves (prohibition on mass atrocities, right to asylum, protection of non-combatants) are treated as subordinate to sovereignty. The sovereignty-maximalist reading inverts the hierarchy: state interest overrides human protection. The norms corpus bears extraction — it loses legitimacy and enforcement capacity — while the regimes that benefit from the inversion gain cover. Generational horizon reflects accumulated delegitimation of the norms framework across time.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__sovereignty_maximalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: AUTHORITARIAN REGIMES (ROPE) — The sovereignty-maximalist reading is purely coordinative from this perspective. It solves a collective action problem: authoritarian regimes want to suppress humanitarian norms that justify intervention. The reading provides a framework for doing so without appearing illegitimate. The regime experiences the constraint as pure coordination: it defines the rules that protect them. Exit is costless (arbitrage) — the regime can adopt a more permissive humanitarian stance if it benefits them, and the reading simply provides cover if they don't. No extraction experienced; maximum benefit.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__sovereignty_maximalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LIBERAL STATES & ADVOCATES (TANGLED ROPE) — Constrained by the sovereignty-maximalist reading's force in international law and state practice. They benefit from coordination around non-intervention (protecting their own sovereignty from external pressure on human rights issues). They also experience extraction: the reading prevents them from legally justifying humanitarian intervention when they want to. Cost-benefit is mixed: they gain sovereignty protection but lose the legal legitimacy for preferred interventions. Exit is costly (constrained) — abandoning the sovereignty framework leaves them vulnerable to intervention claims against them.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UN & INTERNATIONAL LAW APPARATUS (PITON) — The sovereignty-maximalist reading is largely performative within the institutional structure. States invoke it selectively: they cite sovereignty-as-absolute when it protects them, but ignore it when intervention serves their interests (humanitarian intervention in Kosovo, Libya; regime change in Iraq, Syria proxy wars). The institutional apparatus maintains the theater of absolute sovereignty while practice diverges substantially. Theater ratio is high because the framework persists despite non-compliance. The constraint is inertial: maintained because the alternative (a hierarchical enforcement mechanism with real teeth) threatens all states.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__sovereignty_maximalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL / WESTPHALIAN NATURALIZATION (MOUNTAIN) — From a universal analytical perspective, the reading risks naturalizing the sovereignty principle as an immutable law of international relations. The Westphalian order is presented as inevitable and irreplaceable: states cannot coexist without absolute sovereignty. This perspective treats the reading as a natural law constraint. However, the structural data contradicts mountain classification — identifiable beneficiaries exist (authoritarian regimes), suppression is enforced through state practice and legal doctrine (not immutable), and extractiveness accumulates over time. The engine will detect this as a false summit: the naturalization of what is a contingent institutional arrangement.
constraint_indexing:constraint_classification(rbio_practice_norm_complex__sovereignty_maximalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rbio_practice_norm_complex__sovereignty_maximalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rbio_practice_norm_complex__sovereignty_maximalist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, TR),
    TR >= 0.70.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The reading provides specific beneficiaries (authoritarian regimes and their security apparatus) with insulation from humanitarian pressure and intervention claims. The extraction is severe because it forecloses legitimate external mechanisms for protecting populations. The 16-unit accumulation (0.52 → 0.68) reflects that as humanitarian advocacy increases internationally, the sovereignty-maximalist reading becomes more valuable as a counterweight — it accumulates extraction force through strategic invocation against rising human rights pressure. Suppression (0.72): High. The reading enforces non-interference through multiple mechanisms: UN Charter architecture that privileges state sovereignty, international law doctrine that treats intervention as presumptively illegal, practice patterns where Security Council vetoes block humanitarian action, and normative framing that portrays humanitarian intervention as illegitimate even when human rights abuses are documented. The suppression is not complete (humanitarian intervention occurs despite the doctrine, liberal states resist the absolute reading) but is substantially enforced through institutional machinery. Theater ratio (0.58): Moderate-high. The doctrine exhibits substantial performative content because state practice systematically violates it: powerful states invoke absolute sovereignty to protect allies but support interventions against adversaries. The reading is maintained through rhetorical invocation despite visible non-compliance. The theater has increased (0.48 → 0.58) as the gap between doctrine and practice has widened, requiring more performative re-affirmation to maintain the fiction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from a single set of base properties. Repressed populations see a snare with no exit. Authoritarian regimes see pure coordination (rope) — the reading simply defines legitimate action from their perspective. Liberal states see mixed coordination-extraction (tangled rope) — they benefit from mutual non-interference but lose legal grounds for preferred interventions. The UN apparatus sees performative ritual (piton) — the doctrine persists despite systematic non-compliance. Humanitarian norms see extraction (snare) — their legitimacy is subordinated. The analytical observer risks seeing inevitability (mountain) — the Westphalian arrangement naturalized as structural necessity. The perspectival divergence is maximal: the same structural constraint produces six distinct classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation from beneficiary/victim declarations produces high d values for victims (repressed populations, humanitarian norms corpus) and low d values for beneficiaries (authoritarian regimes). The sigmoid f(d) amplifies experienced extraction for victims (d ≈ 0.92 → f(d) ≈ 1.35) and suppresses it for beneficiaries (d ≈ 0.08 → f(d) ≈ -0.15). Scope is global (σ = 1.2), amplifying the computed chi. For repressed populations: χ = 0.68 × 1.35 × 1.2 ≈ 1.10 (effective extraction exceeds base); for authoritarian beneficiaries: χ = 0.68 × (-0.15) × 1.2 ≈ -0.12 (negative effective extraction because they are beneficiaries). The piton perspective (UN apparatus) has arbitrage exit but institutional power, producing moderate d and moderate chi; the classification derives from theater ratio not from extractiveness. The mountain perspective risks naturalizing the doctrine, but the structural data (identifiable beneficiaries, increasing extractiveness over time) indicates false summit signature.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through reading-level analysis. The sovereignty-maximalist reading IS internally coherent as a normative commitment: it asserts a single foundational axiom (absolute sovereignty as supreme principle) and derives clear implications (humanitarian exceptions are illegitimate, non-interference is sacrosanct). The classification divergence across perspectives is not mandatrophy but correct perspectival variance — the reading legitimately produces different classifications for different structural positions. The mandatrophy arises only if one tries to apply the reading universally across all positions while ignoring structural position: the reading coherently serves authoritarian interests while simultaneously harming humanitarian norm credibility. The engine's false summit detection will fire because beneficiaries are declared on a mountain-adjacent perspective, revealing that the naturalizing impulse is an artifact of analytical position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_intervention_pretext_empirical_threshold,
    'At what empirical threshold of atrocity magnitude or scale does humanitarian concern become genuine vs pretext for regime change?',
    'Comparative case analysis of humanitarian interventions: correlation between stated humanitarian justification and revealed geopolitical motivation (post-hoc documents, patterns of selective intervention); severity-threshold analysis (do larger atrocities receive intervention?)',
    'If low threshold (minor abuses trigger intervention): humanitarian exception is real mechanism, not pretext — read the reading as foreclosed. If high threshold (only mega-scale atrocities justify action): humanitarian exception is selective tool used selectively by powerful states — reading stands. If no clear correlation: humanitarian framing is decoupled from actual intervention decisions — strongest evidence for pretext claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_intervention_pretext_empirical_threshold, empirical, 'Whether humanitarian intervention threshold reflects genuine concern or geopolitical selectivity').

omega_variable(
    sovereignty_principle_contingency,
    'Is absolute state sovereignty a contingent institutional arrangement (Westphalian, post-1945 UN Charter origin) or a necessary logical feature of any decentralized state system?',
    'Historical analysis of pre-Westphalian, non-Western, and post-Westphalian state systems; examination of alternative legitimacy structures (city-states, empires, federal systems); philosophical argument for whether decentralized authority requires sovereignty-as-absolute or whether conditional/layered sovereignty is structurally feasible',
    'If contingent: the sovereignty-maximalist reading is a particular institutional choice — not natural law. Reading becomes explicitly a preferred framing (axiom holdable/conventional), and liberal/hegemonic readings become live alternatives. If necessary: the reading instantiates genuine limits on feasible order. If irreducible disagreement: the reading instantiates a contested axiom (status holdable but under permanent challenge) — classical coexists_with structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_principle_contingency, conceptual, 'Whether absolute sovereignty is contingent or structurally necessary').

omega_variable(
    regime_beneficiary_identification_precision,
    'Do the beneficiaries of sovereignty-maximalist norms cluster specifically around authoritarian regimes, or do all regimes benefit equally from non-interference norms?',
    'Statistical analysis of which regimes invoke sovereignty against humanitarian critique; correlation analysis of regime type (democratic vs authoritarian) with invocation patterns; case studies of transparency in regime selection (do democracies cite absolute sovereignty when human rights violations are exposed?)',
    'If authoritarian-clustered: the reading has asymmetric beneficiary structure — snare classification holds. If universal: the reading is genuinely coordinative (pure rope from all states'' perspective). If mixed pattern: some regimes use it selectively as extraction tool (snare), others genuinely as coordination (rope) — evidence for tangled_rope from state perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regime_beneficiary_identification_precision, empirical, 'Whether sovereignty-maximalist framing benefits all states or clusters among authoritarian regimes').

omega_variable(
    alternative_legitimacy_framework_feasibility,
    'Is a decentralized international order with legitimate conditional intervention (liberal or hegemonic alternative readings) structurally stable, or does conditional intervention create incentive structures that lead to systematic abuse?',
    'Scenario modeling of intervention systems with different legitimacy criteria; historical analysis of conditional intervention periods (Cold War proxy systems, post-Cold War NATO expansions, humanitarian intervention waves); principal-agent analysis of who controls conditionality decisions',
    'If conditional intervention is unstable and leads to abuse: sovereignty-maximalist reading might be second-best constraint (prevents worse outcome). If stable: reading becomes choice between feasible alternatives — coexists_with becomes clearer. If abuse trajectory is worse under conditional system: reading gains pragmatic (instrumental) justification beyond axiomatic logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_legitimacy_framework_feasibility, empirical, 'Stability and abuse trajectories of alternative intervention legitimacy frameworks').

omega_variable(
    reading_institutional_lineage_drift,
    'Has the sovereignty-maximalist reading drifted from its original Westphalian/UN Charter grounding as practice has diverged, or does it persist as coherent doctrine despite non-compliance?',
    'Textual analysis of foundational documents (Treaty of Westphalia, UN Charter, International Court of Justice cases) vs contemporary invocation patterns; identification of doctrinal reinterpretation attempts to accommodate humanitarian exceptions; analysis of whether the reading has fractured into multiple incompatible versions',
    'If persistent as coherent doctrine: the reading remains stable; drift is in practice, not principle (cs_structure.drift_state = practice_drift). If reinterpreted to accommodate humanitarian exceptions: the axiom has been effectively overridden within the tradition (status overridden). If fractured: the reading has become incoherent — multiple incompatible versions circulating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_institutional_lineage_drift, empirical, 'Coherence and doctrinal stability of sovereignty-maximalist reading over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_sov_max_tr_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(rbio_sov_max_tr_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(rbio_sov_max_tr_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(rbio_sov_max_be_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(rbio_sov_max_be_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(rbio_sov_max_be_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rbio_sov_max_su_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(rbio_sov_max_su_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(rbio_sov_max_su_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, humanitarian_intervention_legitimacy_gap).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_practice_vs_treaty_doctrine_divergence).

% DUAL FORMULATION NOTE:
% The RBIO norm complex decomposes into three structurally distinct readings (constraints) with different ε values and beneficiary/victim configurations. The sovereignty-maximalist reading (this file) has ε=0.68 (high extraction). The liberal institutional reading should have lower ε (around 0.35-0.45, treating humanitarian intervention as genuine coordination with minor extraction overhead). The hegemonic reading should have higher ε (around 0.75-0.82, treating humanitarian framing as pure cover for great-power extraction). All three readings share the same kernel (what legitimates intervention?) but instantiate different structural claims about beneficiaries, victims, and the authenticity of humanitarian motivation. Each reading should be generated as a separate constraint story linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
