% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__neoliberal_convertibility, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods Neoliberal Convertibility Constraint
 *   domain: international_political_economy/monetary_regime
 *
 * SUMMARY:
 *   The Bretton Woods treaty (1944) established fixed exchange rates,
 *   currency convertibility (especially for capital transactions), and the
 *   IMF/World Bank as enforcement machinery. This constraint story
 *   instantiates the neoliberal reading: the same rules that appear as
 *   neutral coordination devices for stable trade are read as structural
 *   discipline on national governments, subordinating domestic policy
 *   autonomy to capital mobility. The reading emphasizes how convertibility
 *   rules suppress capital controls—framed as necessary policy tools in the
 *   keynesian embedded liberalism reading but as illegitimate barriers to
 *   efficiency in the neoliberal reading. Extractiveness rises over the
 *   interval as capital liberalization deepens and the IMF conditionality
 *   machinery strengthens; theater rises as the constraint's operation shifts
 *   from trade-stabilization rhetoric toward capital-discipline rhetoric. The
 *   claim/metric gap is deliberate and central to the kernel contest: this
 *   story claims tangled_rope (genuine coordination + asymmetric extraction),
 *   while the keynesian_embedded_liberalism reading would claim rope (pure
 *   coordination), and the sovereignty_defense reading would claim snare
 *   (pure extraction of monetary authority). The engine computes per-seat
 *   divergence; the authoring surface names what the neoliberal seat believes
 *   is true.
 *
 * KEY AGENTS:
 *   - International financial capital — the primary beneficiary, gaining secured cross-border mobility
 *   - Capital-exporting nations (US, UK, Western Europe) — institutional beneficiaries enforcing the system
 *   - Developing nations with capital controls — primary victims, losing policy autonomy
 *   - Bretton Woods architects and IMF enforcement machinery — the agenda-setters and constraint administrators
 *   - Keynesian embedded-liberalism advocates (excluded) — would contest the reading by emphasizing how Bretton Woods protects, not enables, capital mobility
 *   - Sovereignty-defense advocates (excluded) — would contest by emphasizing external monetary discipline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.59).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.59).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Neoliberal Convertibility Constraint").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_regime").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, 'f6f4b27e-7324-4898-93f8-97fc0a6b109c').
narrative_ontology:cs_kernel_codification('f6f4b27e-7324-4898-93f8-97fc0a6b109c', fixed_text).
narrative_ontology:cs_authority_grounding('f6f4b27e-7324-4898-93f8-97fc0a6b109c', extraction).
narrative_ontology:cs_interpretation_layer_present('f6f4b27e-7324-4898-93f8-97fc0a6b109c').
narrative_ontology:cs_reading_relation('f6f4b27e-7324-4898-93f8-97fc0a6b109c', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, forecloses).
narrative_ontology:cs_reading_relation('f6f4b27e-7324-4898-93f8-97fc0a6b109c', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('f6f4b27e-7324-4898-93f8-97fc0a6b109c', foundational, capital_mobility_pareto_superior).
narrative_ontology:cs_axiom_status(capital_mobility_pareto_superior, holdable).
narrative_ontology:cs_axiom_grounding('f6f4b27e-7324-4898-93f8-97fc0a6b109c', capital_mobility_pareto_superior, empirically_contingent).
narrative_ontology:cs_axiom('f6f4b27e-7324-4898-93f8-97fc0a6b109c', secondary, convertibility_requires_capital_controls_suppression).
narrative_ontology:cs_axiom_status(convertibility_requires_capital_controls_suppression, holdable).
narrative_ontology:cs_axiom_grounding('f6f4b27e-7324-4898-93f8-97fc0a6b109c', convertibility_requires_capital_controls_suppression, instrumental).
narrative_ontology:cs_reference_frame('f6f4b27e-7324-4898-93f8-97fc0a6b109c', bretton_woods_capital_liberalization_mandate).
narrative_ontology:cs_drift_state('f6f4b27e-7324-4898-93f8-97fc0a6b109c', post_1990_financial_globalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f6f4b27e-7324-4898-93f8-97fc0a6b109c', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_capital).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_exporting_nations).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_corporations).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_nations_with_capital_controls).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_policy_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_nation_policymakers).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_mobility_enhances_global_efficiency).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, currency_convertibility_indicates_systemic_health).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains unprecedented mobility across borders under convertibility rules. Can move capital to wherever returns are highest, disciplining governments that attempt redistributive or protective policies. Benefits from the constraint's suppression of capital controls that would trap wealth or redirect it to domestic investment.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Their multinational firms and financial institutions gain secure cross-border asset positions. The constraint converts their historical capital advantage into structural enforcement—they can export without fearing host-nation seizure or nationalization of profits. Enforceable convertibility is their protection.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_exporting_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Must abandon or minimize capital controls to remain in good standing within the Bretton Woods system and later IMF/World Bank frameworks. They lose the ability to direct domestic savings to state development projects, to tax capital flight, or to prevent sudden withdrawal of foreign investment. Their policy tools shrink while external capital disciplines them.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_nations_with_capital_controls, payer,
    moderate, generational, constrained, global).

% A non-agent entity representing the legal/institutional capacity of national governments to set monetary policy, manage inflation, direct credit allocation, and protect domestic industry without external financial discipline. Convertibility rules subordinate this capacity to capital mobility and currency stability requirements.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_policy_autonomy, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_policy_autonomy).

% Gain assured ability to repatriate profits in hard currency, to shift production across borders without expropriation risk, and to escape the wage and tax pressures of any single jurisdiction. The constraint removes the protective tariffs and capital-directing policies that would have limited their mobility.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Design and enforce the treaty framework. In the neoliberal reading, they construct rules that appear neutral (currency convertibility, fixed exchange rates) but structurally embed the preference for capital mobility over national policy space. They administer the IMF conditionality machinery that institutionalizes this constraint.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_architects, agenda_setter,
    institutional, generational, analytical, global).

% Would argue for Bretton Woods as a compromise that PROTECTS domestic policy space by constraining international capital flows, not enabling them. Their reading is structurally excluded from this neoliberal instantiation; their voice—that convertibility should be managed to preserve full employment and redistribution—is not seated at the system's design table once the neoliberal reading dominates institutional interpretation.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_embedded_liberalism_advocates, excluded,
    moderate, biographical, constrained, global).

% Would argue that Bretton Woods imposes external monetary discipline on national central banks, subordinating monetary sovereignty to exchange-rate stability and IMF surveillance. In the neoliberal reading they are not heard; the reading frames monetary sovereignty as less important than capital-market efficiency.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, sovereignty_defense_advocates, excluded,
    moderate, biographical, constrained, global).

% Face pressure from IMF conditionality (tied to lending) to open capital accounts, remove controls, and prioritize currency stability and debt service over domestic employment or investment. Their policy choices narrow; deviation from the convertibility framework triggers capital flight and currency crisis, making exit costly.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_nation_policymakers, payer,
    moderate, biographical, constrained, regional).

% Examines the Bretton Woods system from outside the advocacy positions, noting how the same treaty can be read as either protecting or constraining national autonomy depending on which mechanism (capital discipline vs. foreign-exchange stability) is the analytical focus.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, observer_institutional_economist, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_capital).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a stable international monetary system with fixed exchange rates and currency convertibility, enabling predictable cross-border trade and investment, and establishing a common reference frame (the US dollar peg) for global commerce.
% TRANSFER_FUNCTION: Moves policy-setting authority from national governments (especially developing nations) to international financial markets and creditor institutions. Capital that would have been subject to national direction under controls flows to highest-return uses globally, generating returns for capital exporters and multinational firms while constraining government options in capital-importing nations.
% ABSENT_VOICES: Keynesian economists and full-employment advocates (who supported Bretton Woods as embedded liberalism protecting policy space) are structurally excluded from this reading. Sovereignty-defense advocates (who oppose external monetary discipline) are also not seated. Labor movements in developing nations whose interests depend on capital-directing policies are absent from the treaty's negotiating table.
% DISAPPEARANCE_RATIONALE: If Bretton Woods convertibility rules disappeared, nations would restore capital controls, domestic investment priorities would reemerge, multinational profit repatriation would face friction and taxation, and the disciplinary power of capital mobility to enforce fiscal and monetary orthodoxy would dissolve. The distribution of policy authority would shift backward to national governments.
% FOUNDING_PROBLEM: Post-WWII instability: competitive currency devaluations, capital flight, and monetary chaos during the 1930s suggested that without a fixed anchor and orderly conversion mechanism, international commerce could not recover. The founding problem was exchange-rate volatility and the lack of a credible medium for international trade settlement.
% FOUNDING_PROBLEM_CORROBORATION: Capital exporters and financial institutions attest the founding problem remains live—volatility and capital controls still threaten predictability. Developing-nation policymakers and labor advocates attest the founding problem is solved but the system now serves extraction: currency stability is achieved; the constraint now persists to discipline national governments and enable capital mobility. Academic consensus from heterodox economists and historical analyses outside the Bretton Woods institutions support the shifted-function reading.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.35 at founding, projected) because the architects present Bretton Woods as mutual stabilization, and the coordination function is genuinely real—trade does stabilize under fixed rates and convertibility. But extractiveness rises to 0.68 as capital liberalization deepens (1960s onward): the constraint becomes the mechanism through which developing nations' policy options narrow in service of capital mobility. By 1980, the neoliberal reading is institutionally dominant; the IMF conditionality framework explicitly links lending to capital-account opening and anti-inflationary discipline. Suppression rises (0.30→0.59) because enforcement shifts from passive rule-compliance to active coercion: IMF structural adjustment programs, capital-flight pressure on nations trying capital controls, and the threat of exclusion from the financial system suppress alternative policies. Theater ratio rises (0.15→0.41) as the constraint's justification migrates from 'stable trade' to 'efficient capital allocation'—the theatrical element is that the same rules are described in both ways. Accessibility of alternatives collapses moderately (0.62): a developing nation can theoretically reimpose capital controls, but the cost (capital flight, currency crisis, IMF exclusion, austerity pressure) makes the alternative practically unavailable. Resistance is high (0.71) because policymakers in developing nations have continuously contested the constraint through alternative institutions (non-aligned movement, UNCTAD, calls for a new international economic order), even though they lack enforcement power to override it. The measurement grid is shared across all three metrics at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From the capital-exporting beneficiary seat, this is rope—genuine coordination that enables predictable returns on foreign investment, underpinned by reasonable rules. From the developing-nation victim seat, this is tangled rope or snare—they coordinate on currency stability (a real good) but pay extraction via suppressed capital autonomy. From the Bretton Woods architect seat, it is tangled rope: they coordinate on post-war stability AND extract policy authority from nations that might otherwise pursue full employment or redistribution. The engine derives per-seat types from power, exit, beneficiary/victim status, and measurement data; the authored claim (tangled_rope) reflects the neoliberal seat's own structural position—it acknowledges both coordination and extraction, whereas the keynesian reading would deny extraction exists.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial capital holds institutional power and arbitrage-grade exit: can move to any jurisdiction with open capital accounts. Directionality d ≈ 0.15 (strong beneficiary, low d). Capital-exporting nations hold institutional power and arbitrage exit (their multinationals can operate globally): d ≈ 0.20 (beneficiary). Developing nations hold moderate power but constrained exit: they can try capital controls but face crushing pressure; d ≈ 0.80 (target/victim). 'Domestic policy autonomy' is a non-agent placeholder: it is not a real actor, so it does not feed directionality computation—it is listed to clarify what is being extracted (the capacity to set policy), not because it has a seat at the table. Extraction flows FROM developing nations (and from their constrained policymakers) TO international financial capital, mediated by the Bretton Woods rules and IMF enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids misclassification as pure extraction by declaring genuine coordination (fixed exchange rates, convertible currency, trade predictability)—the tangled_rope type holds. But the theater_ratio rising from 0.15 to 0.41 flags mandate drift: the original mandate was 'prevent 1930s-style competitive devaluations'; by 1980, the operative mandate has become 'discipline national governments to open capital accounts and accept capital discipline.' The mandatrophy detection is: founding_problem='exchange-rate volatility', founding_problem_status='contested' (because capital exporters say it's still live, while developing nations say it's solved and the system now serves extraction), disappearance_verdict='world_rearranges' (the constraint is not natural). The mismatch (dead founding problem + world_rearranges verdict) surfaces the mandate drift without requiring the author to assert mandatrophy directly—the engine computes it from the structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Is currency convertibility and fixed exchange-rate stability structurally inseparable from the suppression of capital controls, or could one achieve the same coordination benefits with managed convertibility and permitted capital controls?',
    'Comparative institutional analysis of alternative post-war monetary arrangements (Keynes''s bancor proposal vs. Bretton Woods), and historical counterfactual: what policy options would have been available to developing nations under a managed-convertibility regime?',
    'If separable, the suppression of capital controls is pure extraction riding on a real coordination function (supports tangled_rope classification). If inseparable, part of what is measured as extraction is the necessary cost of the coordination itself (raises the boltzmann floor and may shift classification toward rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether Bretton Woods coordination and capital-control suppression are structurally joined or could be disentangled.').

omega_variable(
    reading_institutional_dominance,
    'Why did the neoliberal_convertibility reading become institutionally dominant by the 1970s-80s, when the original Bretton Woods compromise (1944) was more consistent with embedded_liberalism (permitting capital controls)?',
    'Institutional history: who changed the interpretation (Reagan/Thatcher administrations, IMF management shifts, shift from fixed to floating rates), under what material pressure (US capital-export interests, petrodollar system, Cold War alignment), and how did they reframe the treaty to justify full convertibility?',
    'If reframing was driven by power and interest rather than by any textual evolution, it demonstrates the kernel''s interpretive malleability—the same treaty can bear different readings, and the instantiated reading depends on which institutional coalition controls the interpretation authority. This supports the framing of this story as ONE reading among contested alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_institutional_dominance, empirical, 'Institutional-history mechanism of reading dominance shift.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of capital-control advocacy (suppression measured at 0.59) structural—external enforcement by IMF conditionality and capital-flight threat—or has it become internalized in developing-nation policymakers'' beliefs that capital controls are inherently inefficient?',
    'Post-exit trajectory study: if developing nations that escape IMF oversight (oil exporters, China) readily re-adopt capital controls without ideological resistance, the suppression is primarily structural; if policymakers continue accepting capital-control limitations even when external pressure eases, the suppression has partially internalized.',
    'If internalized, effective suppression is higher than the structural measure—policymakers carry the constraint with them even after external pressure is removed. This affects the exit options classification for developing-nation policymakers (may shift from ''constrained'' toward ''identity_locked'').',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism for capital-control advocacy.').

omega_variable(
    kernel_reading_contestation,
    'This story instantiates neoliberal_convertibility as ONE reading of the bretton_woods_treaty_substrate kernel. Are the three sibling readings (keynesian_embedded_liberalism, sovereignty_defense) held by genuinely distinct institutional coalitions, or are they ex-post rationalization by actors unhappy with the outcome?',
    'Historical textual analysis: did Keynes and White (the architects) intend embedded liberalism (controls permitted) or neoliberal convertibility (controls forbidden)? What did the original treaty articles actually authorize? Did the shift to full convertibility require treaty amendment or was it an interpretation shift?',
    'If the original Articles authorized both readings (ambiguous kernel), this supports the framing of multiple readings as live within the same text. If the Articles clearly authorized embedded liberalism and neoliberal reading required reinterpretation, it demonstrates reading drift driven by power, not by textual evolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Whether the three readings are equiprobable from the founding text or whether the neoliberal reading represents interpretive dominance of the original embedded-liberalism intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1944, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1944, 0.15).
narrative_ontology:measurement_basis(bret_tr_t1944, projected).
narrative_ontology:measurement(bret_tr_t1960, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1960, 0.22).
narrative_ontology:measurement_basis(bret_tr_t1960, observed).
narrative_ontology:measurement(bret_tr_t1970, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1970, 0.32).
narrative_ontology:measurement_basis(bret_tr_t1970, observed).
narrative_ontology:measurement(bret_tr_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1980, 0.39).
narrative_ontology:measurement_basis(bret_tr_t1980, observed).
narrative_ontology:measurement(bret_tr_t1990, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1990, 0.41).
narrative_ontology:measurement_basis(bret_tr_t1990, observed).
narrative_ontology:measurement(bret_tr_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2000, 0.41).
narrative_ontology:measurement_basis(bret_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement_basis(bret_be_t1944, projected).
narrative_ontology:measurement(bret_be_t1960, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1960, 0.48).
narrative_ontology:measurement_basis(bret_be_t1960, observed).
narrative_ontology:measurement(bret_be_t1970, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement_basis(bret_be_t1970, observed).
narrative_ontology:measurement(bret_be_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement_basis(bret_be_t1980, observed).
narrative_ontology:measurement(bret_be_t1990, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1990, 0.67).
narrative_ontology:measurement_basis(bret_be_t1990, observed).
narrative_ontology:measurement(bret_be_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement_basis(bret_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1944, 0.3).
narrative_ontology:measurement_basis(bret_su_t1944, projected).
narrative_ontology:measurement(bret_su_t1960, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1960, 0.42).
narrative_ontology:measurement_basis(bret_su_t1960, observed).
narrative_ontology:measurement(bret_su_t1970, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement_basis(bret_su_t1970, observed).
narrative_ontology:measurement(bret_su_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement_basis(bret_su_t1980, observed).
narrative_ontology:measurement(bret_su_t1990, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement_basis(bret_su_t1990, observed).
narrative_ontology:measurement(bret_su_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2000, 0.59).
narrative_ontology:measurement_basis(bret_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.25).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_conditionality_structural_adjustment).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_account_liberalization_mandate).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the bretton_woods_treaty_substrate kernel. The same 1944 Articles of Agreement are read by the keynesian_embedded_liberalism reading as protecting national policy space through managed convertibility, and by the sovereignty_defense reading as imposing external monetary discipline. All three readings share the same fixed text (kernel) but decompose it into different constraint structures with different beneficiary/victim sets. This story's ε (0.68, extraction-focused) differs from keynesian_embedded_liberalism's ε (lower, coordination-focused). The constraint families are linked via network.affects_constraints; see cs_structure.reading_relations for the logical relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
