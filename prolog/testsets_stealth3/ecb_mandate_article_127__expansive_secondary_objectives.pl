% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate Expansive Secondary-Objectives Reading (Article 127 TFEU)
 *   domain: monetary policy/constitutional law/eu institutional governance
 *
 * SUMMARY:
 *   This story instantiates the expansive_secondary_objectives reading of the
 *   ECB mandate kernel (Article 127(1) TFEU): the claim that the treaty's
 *   secondary objectives — supporting the Union's general economic policies,
 *   including employment and growth — carry operational weight whenever price
 *   stability is not threatened, with the 'without prejudice' clause
 *   authorizing discretionary balancing rather than strict lexicographic
 *   priority. Per the epsilon-invariance principle, the three readings of the
 *   Article 127 kernel are separate constraints with separate stories; this
 *   file authors only this reading, and the siblings
 *   (orthodox_price_stability, climate_incorporation) are linked via
 *   network.affects_constraints. The standing arrangement under contest is
 *   the operative mandate-as-read-expansively: the practice, ratified by the
 *   CJEU (Gauweiler 2015; Weiss-CJEU 2018) and formalized in the 2021
 *   strategy review, of citing the support clause and weighting employment
 *   and growth in program design and policy calibration. The claim/metric gap
 *   is deliberate: from the authoring seat the constraint is CLAIMED as
 *   tangled_rope — a genuine coordination function (a twenty-state currency
 *   union needs a workable multi-objective framework) fused with asymmetric
 *   transfer (savers pay, debtors and the Council collect) — while the
 *   metrics are authored independently as descriptively true of the
 *   arrangement's operation.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: Agenda-setter (institutional/constrained) — interprets Article 127, sets policy, collects the interpretive-discretion gain, absorbs contestation costs (constitutional-court challenges, recorded dissent)
 *   - highly_indebted_sovereigns: Primary beneficiary (institutional/trapped) — concentrated gains via spread compression and crisis-program backstops; cannot devalue or exit
 *   - eurozone_mortgage_debtors: Beneficiary (moderate/constrained) — rate relief and asset-value support; part of the gain capitalizes into house prices
 *   - eurozone_workers: Beneficiary (organized/constrained) — employment-weighted policy support; indirect real-wage exposure in hot phases
 *   - eurozone_savers: Primary payer (moderate/mobile) — bears the real-return transfer; no institutional seat; individually mobile but collectively unorganized
 *   - national_parliaments: Excluded voice (institutional/trapped) — bear the political fallout of monetary decisions with no formal role under treaty independence
 *   - european_court_of_justice: Observer (institutional/analytical) — ratifies the reading's breadth under deferential proportionality review
 *   - german_federal_constitutional_court: Observer (institutional/analytical) — contests the reading's limits (Weiss 2020); attests both the underlying problem and the need for bounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.58).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.58).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate Expansive Secondary-Objectives Reading (Article 127 TFEU)").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary policy/constitutional law/eu institutional governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, 'efdf5028-c155-4579-9dec-8e98b1cbd04f').
narrative_ontology:cs_kernel_codification('efdf5028-c155-4579-9dec-8e98b1cbd04f', fixed_text).
narrative_ontology:cs_authority_grounding('efdf5028-c155-4579-9dec-8e98b1cbd04f', lineage).
narrative_ontology:cs_interpretation_layer_present('efdf5028-c155-4579-9dec-8e98b1cbd04f').
narrative_ontology:cs_reading_relation('efdf5028-c155-4579-9dec-8e98b1cbd04f', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('efdf5028-c155-4579-9dec-8e98b1cbd04f', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('efdf5028-c155-4579-9dec-8e98b1cbd04f', foundational, secondary_objectives_operationally_weightable).
narrative_ontology:cs_axiom_status(secondary_objectives_operationally_weightable, holdable).
narrative_ontology:cs_axiom_grounding('efdf5028-c155-4579-9dec-8e98b1cbd04f', secondary_objectives_operationally_weightable, conventional).
narrative_ontology:cs_axiom('efdf5028-c155-4579-9dec-8e98b1cbd04f', foundational, without_prejudice_authorizes_discretionary_balancing).
narrative_ontology:cs_axiom_status(without_prejudice_authorizes_discretionary_balancing, holdable).
narrative_ontology:cs_axiom_grounding('efdf5028-c155-4579-9dec-8e98b1cbd04f', without_prejudice_authorizes_discretionary_balancing, conventional).
narrative_ontology:cs_reference_frame('efdf5028-c155-4579-9dec-8e98b1cbd04f', multi_objective_balanced_mandate).
narrative_ontology:cs_drift_state('efdf5028-c155-4579-9dec-8e98b1cbd04f', post_2021_strategy_review, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('efdf5028-c155-4579-9dec-8e98b1cbd04f', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_workers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_mortgage_debtors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, highly_indebted_sovereigns).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_savers).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, secondary_objectives_support_clause_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, gauweiler_weiss_proportionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 127(1) TFEU and sets eurozone monetary policy. Since 2012 it has cited the support clause and employment and growth considerations in program design (OMT, PSPP, PEPP) and in the 2021 strategy review formally adopted a symmetric 2 percent target with explicit employment emphasis. The elasticity of the 'without prejudice' clause is its operational room: each unit of interpretive breadth is discretion it exercises. It absorbs the costs of contestation — constitutional-court challenges, recorded dissent, credibility attacks — and cannot exit the treaty framework that both empowers and binds it.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Gain when employment and growth carry weight in policy: the reading licenses tolerating above-target inflation and accommodative stances that support hiring, and it gives unions and works councils a textual hook for demanding that the central bank weigh jobs alongside prices. They bear an indirect cost in hot phases through real-wage erosion. They have no seat in the Governing Council; their influence routes through national governments and social-partner consultation, and labor mobility within the union is limited by language, housing, and benefit-portability frictions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_workers, beneficiary,
    organized, biographical, constrained, continental).

% Hold variable-rate and newly issued fixed-rate mortgages. Accommodative policy and crisis programs lower servicing costs and support property values; the reading is the legal foundation for the programs that did this. Part of the gain capitalizes into house prices, so households entering after an accommodation phase capture less than incumbent owners. An individual household cannot exit the rate regime; it can only time entry or exit the market at a price.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_mortgage_debtors, beneficiary,
    moderate, biographical, constrained, continental).

% High-debt member states (the periphery) receive the largest concentrated gains: crisis-era programs built on this reading compressed sovereign spreads and lowered funding costs by amounts measured in hundreds of billions of cumulative debt-service relief. The reading is the legal predicate for the backstops that made their debt fundable at all in crisis windows. They cannot devalue, cannot print, and euro exit is legally ambiguous and economically ruinous; their fiscal rules simultaneously constrain the alternatives they could use instead.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, highly_indebted_sovereigns, beneficiary,
    institutional, generational, trapped, continental).

% Bear the transfer side: negative real deposit rates and the inflation tax on cash and fixed-income holdings during accommodative phases are the direct cost of the policy space this reading opens. They hold no institutional seat — no saver or creditor representation exists in the Governing Council or in the accountability dialogue. Individual savings can migrate to equities, real assets, or other currencies, but migration is costly, information-intensive, and unevenly available; deposit-heavy households without advisory access bear the cost without the exit. As a class they are diffuse and have not coordinated.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_savers, payer,
    moderate, biographical, mobile, continental).

% Bear the political fallout of monetary decisions — housing-cost grievances, inflation anger, blame for austerity-adjacent outcomes — while having no formal role: the treaty assigns monetary policy to an independent central bank, and national parliamentary oversight is limited to indirect channels. They would demand accountability for discretionary balancing of the mandate if they had a lever; the European Parliament's monetary dialogue is consultative and has never reversed a Council decision. There is no exit from the treaty allocation of competence.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, national_parliaments, excluded,
    institutional, generational, trapped, national).

% Adjudicated the mandate's breadth: in Gauweiler (2015) and the Weiss reference (2018) it ratified the crisis programs and the expansive use of the support clause while deferring heavily to the ECB's technical judgment. Its proportionality review is the principal legal check on how far the reading can stretch, and its deference is itself a structural fact about how much enforcement the reading requires.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, european_court_of_justice, observer,
    institutional, generational, analytical, continental).

% Contested the reading's limits: its Weiss judgment (2020) found the PSPP proportionality review deficient, questioned the program's ultra vires status, and gave the ECB a compliance deadline. Its reasoning attests from outside the benefiting parties both that the crisis-era problem the reading addresses was real and that the discretion it licensed required bounds. Its standing pressure shapes the outer edge of the interpretive space.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, german_federal_constitutional_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__expansive_secondary_objectives, highly_indebted_sovereigns).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__expansive_secondary_objectives, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single workable monetary framework for twenty sovereign states with divergent cycles and no common fiscal authority. The expansive reading lets one instrument dampen asymmetric shocks — peripheral debt crises, unemployment divergence — that member-state fiscal capacity cannot coordinate among themselves; it anchors expectations while leaving the central bank room to respond when price stability is not the binding constraint.
% TRANSFER_FUNCTION: Moves real wealth from net creditors and savers to net debtors — mortgage borrowers and indebted sovereigns — via tolerated inflation and suppressed rates; separately, it moves interpretive discretion and policy autonomy to the Governing Council itself, insulated from electoral correction by treaty design.
% ABSENT_VOICES: Savers and creditors have no institutional seat anywhere in the arrangement — not in the Council, not in the accountability dialogue, not in program design; they are the payer class with no counter-seat. National parliaments are excluded by treaty allocation of competence despite bearing the political fallout. The orthodox reading's proponents sit inside the Council and dissent is recorded, but once a majority forms, their position has no formal channel — the minority's reading is outvoted, not heard.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished overnight — the Council reverting to strict orthodoxy and the CJEU line repudiated — periphery sovereign spreads would re-widen immediately, the legal foundation of the OMT/PSPP/PEPP program lineage would collapse, highly indebted members would face a funding crisis the union has no other instrument to meet, and the employment-weighted policy stance formalized in 2021 would reverse. The eurozone's crisis-management architecture would be legally and financially destabilized within weeks.
% FOUNDING_PROBLEM: The mandate text was written at Maastricht to solve the German historical problem — a central bank with an inflation bias must be credibly insulated — while a secondary clause was inserted as a political compromise binding the new institution to the broader European project: the ESCB shall 'support the general economic policies in the Union.' The expansive reading was operationalized two decades later against a different problem: one currency, twenty divergent economies, no fiscal union, and a sovereign-debt crisis that the price-stability instrument alone could not answer.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the benefiting parties. The German Federal Constitutional Court's Weiss reasoning — an adverse seat — attests both that the crisis-era divergence problem was real enough to license extraordinary programs and that the discretion they required demanded proportionality bounds. IMF Article IV consultations and OECD structural analyses corroborate that divergence mechanisms persist. Recorded council dissent and the post-2021 inflation surge attest, from inside and outside, that the original price-stability-credibility problem is also still live. No party denies both problems exist; the contest is over which is primary and whether the current breadth is proportionate to either.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.58: the reading licenses real transfers — negative real returns to savers, inflation tolerance — and concentrates interpretive discretion in an unaccountable body; but a substantial share of its operation is the legitimate price of stabilizing a currency union with one instrument and no fiscal union, which this reading's own lights classify as coordination cost rather than extraction. Suppression 0.58 is structural, not internalized: treaty independence, CJEU deference, and the absence of any saver or creditor seat block correction of the interpretive regime; savers' exit is individually possible (asset migration) but collectively inert because the class cannot coordinate — mobility without organization does not discipline the arrangement, which is why a moderate power atom with mobile exit still experiences sustained extraction. Theater 0.40: support-clause invocations mix real deliberative content with cover language for decisions already made; theater peaked at peak contestation (2020) and eased as the 2021 strategy review made the balancing explicit rather than implied. Accessibility_collapse 0.40: the orthodox alternative remains a live position — hawkish dissent, the BVerfG ultra vires doctrine, treaty-change proposals — so alternatives are suppressed in operation but not extinguished. Resistance 0.55: institutionalized and real (Weiss judgment, constitutional objection, recorded dissent), not diffuse grumbling. The measurement series run on one shared nine-point grid (every tracked metric authored at every point, basis observed); suppression_requirement is tracked because the story's narrative is precisely enforcement-capacity change — the interpretive enforcement machinery (ratifications, proportionality defense, independence shields) was built up from 2008 through the 2020 BVerfG confrontation, then partially stood down as formalization reduced the defensive enforcement the reading needed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the Governing Council's seat the arrangement is a mandate faithfully executed with the breadth the text permits — coordination it stewards, discretion the treaty grants. From the saver's seat the same structure is a transfer machine with no counter-seat: gains flow to debtors and to the Council's own discretion, costs land on unrepresented creditors, and the accountability channels that exist have never reversed a decision. From the trapped sovereign's seat it is existential backstop — the difference between fundable and unfundable debt. From the BVerfG's seat it is an ultra vires risk to be bounded, not a benefit stream. The engine computes per-seat classifications from the structural data; the divergence between the agenda-setter's coordination story and the payer's extraction story is the measurement this corpus exists to take, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: eurozone_workers (organized, constrained exit — near the beneficiary end, damped slightly by indirect inflation exposure); eurozone_mortgage_debtors (moderate, constrained — near the beneficiary end, with the caveat that part of their gain capitalizes into house prices, a within-class spread the structural declaration cannot express; tracked in the transfer-magnitude omega); highly_indebted_sovereigns (institutional, trapped — nearest the full-beneficiary end; trapped exit amplifies how much of the arrangement's relief they capture, which is why they are the named gain_flow seat). Payer: eurozone_savers (moderate power, mobile exit — high directionality damped by exit; the damping is real for asset-rich savers and much less real for deposit-only households, another within-class spread the scalar cannot express). The agenda-setter Council carries no beneficiary or victim declaration, so its directionality falls to the canonical fallback; its true position — collects the discretion gain, bears contestation costs — sits beneficiary-side but well short of zero. No directionality overrides were authored: the override mechanism keys on the power atom, and this story's institutional seats (indebted sovereigns, two courts, and the Council) hold genuinely different structural relationships despite sharing the institutional atom, so a power-atom override would misdescribe more seats than it corrects; the role-plus-exit derivation is the better instrument here.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. The orthodox seat would see pure discretion-rent — extraction dressed in treaty language — and classify snare; the tangled_rope classification insists the coordination function is real: a monetary union without fiscal union needs a mandate that can answer asymmetric shocks, and the crisis-era programs this reading licenses demonstrably held the union together at moments when the alternative was breakup. This reading's own seat would see near-pure coordination — rope; the classification insists on the asymmetric side: identifiable payers, identifiable discretion rents, and a requirement of active enforcement (ratification, proportionality defense, majority discipline) to hold the interpretive regime against orthodox challenge. Mandatrophy status: the founding problem — asymmetric-shock management in a monetary union without fiscal capacity — is contested-live, not dead, so the founding-problem-status-by-disappearance mismatch flag does not fire; the arrangement is not a zombie. The lifecycle risk runs the other way: if EMU fiscal capacity materializes at scale, the crisis-coordination function atrophies while the interpretive machinery and precedent persist — a degraded-inertia trajectory in which the clause is maintained by habit and institutional identity rather than function. The fiscal-union-counterfactual omega tracks exactly this fork.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel ecb_mandate_article_127 (the expansive_secondary_objectives reading). Sibling readings — orthodox_price_stability and climate_incorporation — would change the beneficiary set (orthodox: price-stability constituency and savers; climate: climate-exposed actors and transition-finance channels) and the operational content of the mandate. Where exactly is the disagreement located?',
    'Structural analysis of the disputed element: whether ''without prejudice'' confers operational discretion (this reading) or declarative priority only (orthodox), and whether general-policy support is discretionary (this reading) or mandatory-and-specific via the Article 11 TFEU integration clause (climate reading). Resolution comes from CJEU jurisprudence development, treaty-change proposals, or a formal ECB mandate clarification.',
    'If the orthodox reading prevailed, this constraint''s beneficiary set collapses to the price-stability constituency and the transfer function largely disappears; if the climate reading prevailed, the discretionary balancing space becomes a mandatory integration duty with a different victim set. This file''s epsilon, beneficiaries, and classification are valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one of three readings of the Article 127 kernel; the contest is over the operational content of the without-prejudice clause.').

omega_variable(
    without_prejudice_elasticity,
    'How much operational discretion does the ''without prejudice'' clause actually confer under CJEU proportionality review, as against the discretion the Governing Council claims in practice?',
    'Systematic comparison of the Council''s stated mandate justifications against the bounds the CJEU has actually enforced (Gauweiler, Weiss-CJEU, subsequent preliminary references), plus any future treaty-clarification text.',
    'If the clause''s legal elasticity is much narrower than the Council''s operational use, a large share of the measured extraction is interpretive rent beyond mandate — pushing the effective classification toward the snare boundary; if the clause genuinely licenses the practiced breadth, the extraction sits closer to the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(without_prejudice_elasticity, empirical, 'Gap between claimed and legally bounded interpretive discretion.').

omega_variable(
    saver_debtor_transfer_magnitude,
    'What is the realized magnitude of the real-return transfer from savers and creditors to mortgage debtors and indebted sovereigns under the reading''s sustained operation, net of the macroeconomic stabilization benefits those same groups receive?',
    'ECB distributional wealth accounts, occasional papers on the distributional effects of monetary policy, and independent fiscal-cost accounting of spread compression and program holdings.',
    'A large net transfer with concentrated receipts would raise effective extraction for the payer seat and pressure the classification toward the snare boundary; a small net transfer would support the reading''s own coordination framing and pull toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saver_debtor_transfer_magnitude, empirical, 'Size of the distributional transfer that constitutes the constraint''s extraction.').

omega_variable(
    accountability_sufficiency,
    'Do the existing accountability channels — European Parliament monetary dialogue, CJEU deferential proportionality review, published accounts and dissent records — adequately check the Council''s discretionary balancing, or is the interpretive regime effectively self-certifying?',
    'Post-Weiss institutional analysis: track whether proportionality review has ever actually constrained a program, whether monetary dialogue has ever altered a decision, and whether the 2020 BVerfG pressure produced durable review changes.',
    'If accountability is insufficient, the measured suppression is a hard structural feature of the arrangement rather than an interpretive artifact — raising effective suppression and strengthening the extraction side of the classification; if sufficient, part of the suppression measure reflects contestation friction that accountability absorbs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_sufficiency, empirical, 'Adequacy of the checks on discretionary mandate interpretation.').

omega_variable(
    fiscal_union_counterfactual,
    'If the eurozone develops standing fiscal capacity (joint safe asset, central fiscal instrument capable of absorbing asymmetric shocks), does this reading''s crisis-coordination function atrophy while the interpretive machinery persists, or does divergence prove structural enough that the function remains live?',
    'Institutional evolution of EMU fiscal architecture: observe whether fiscal instruments of sufficient scale are enacted and whether the Council''s reliance on secondary-objective reasoning declines in parallel or persists.',
    'Atrophy would put the constraint on a degraded-inertia lifecycle — maintained by precedent and institutional habit rather than function; persistence would stabilize it as a permanent coordination-plus-transfer arrangement. The trajectory determines whether the current classification is a steady state or a life-stage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_union_counterfactual, conceptual, 'Lifecycle question: does fiscal-union development obsolete the reading''s coordination function?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t1993, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 1993, 0.15).
narrative_ontology:measurement_basis(ecb__tr_t1993, observed).
narrative_ontology:measurement(ecb__tr_t1999, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 1999, 0.18).
narrative_ontology:measurement_basis(ecb__tr_t1999, observed).
narrative_ontology:measurement(ecb__tr_t2003, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2003, 0.2).
narrative_ontology:measurement_basis(ecb__tr_t2003, observed).
narrative_ontology:measurement(ecb__tr_t2008, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2008, 0.3).
narrative_ontology:measurement_basis(ecb__tr_t2008, observed).
narrative_ontology:measurement(ecb__tr_t2012, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2012, 0.38).
narrative_ontology:measurement_basis(ecb__tr_t2012, observed).
narrative_ontology:measurement(ecb__tr_t2015, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(ecb__tr_t2015, observed).
narrative_ontology:measurement(ecb__tr_t2020, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2020, 0.45).
narrative_ontology:measurement_basis(ecb__tr_t2020, observed).
narrative_ontology:measurement(ecb__tr_t2021, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2021, 0.42).
narrative_ontology:measurement_basis(ecb__tr_t2021, observed).
narrative_ontology:measurement(ecb__tr_t2024, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(ecb__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(ecb__be_t1993, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 1993, 0.28).
narrative_ontology:measurement_basis(ecb__be_t1993, observed).
narrative_ontology:measurement(ecb__be_t1999, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 1999, 0.3).
narrative_ontology:measurement_basis(ecb__be_t1999, observed).
narrative_ontology:measurement(ecb__be_t2003, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2003, 0.31).
narrative_ontology:measurement_basis(ecb__be_t2003, observed).
narrative_ontology:measurement(ecb__be_t2008, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement_basis(ecb__be_t2008, observed).
narrative_ontology:measurement(ecb__be_t2012, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2012, 0.48).
narrative_ontology:measurement_basis(ecb__be_t2012, observed).
narrative_ontology:measurement(ecb__be_t2015, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2015, 0.53).
narrative_ontology:measurement_basis(ecb__be_t2015, observed).
narrative_ontology:measurement(ecb__be_t2020, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement_basis(ecb__be_t2020, observed).
narrative_ontology:measurement(ecb__be_t2021, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2021, 0.57).
narrative_ontology:measurement_basis(ecb__be_t2021, observed).
narrative_ontology:measurement(ecb__be_t2024, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(ecb__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t1993, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 1993, 0.35).
narrative_ontology:measurement_basis(ecb__su_t1993, observed).
narrative_ontology:measurement(ecb__su_t1999, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 1999, 0.38).
narrative_ontology:measurement_basis(ecb__su_t1999, observed).
narrative_ontology:measurement(ecb__su_t2003, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2003, 0.4).
narrative_ontology:measurement_basis(ecb__su_t2003, observed).
narrative_ontology:measurement(ecb__su_t2008, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement_basis(ecb__su_t2008, observed).
narrative_ontology:measurement(ecb__su_t2012, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement_basis(ecb__su_t2012, observed).
narrative_ontology:measurement(ecb__su_t2015, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement_basis(ecb__su_t2015, observed).
narrative_ontology:measurement(ecb__su_t2020, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement_basis(ecb__su_t2020, observed).
narrative_ontology:measurement(ecb__su_t2021, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement_basis(ecb__su_t2021, observed).
narrative_ontology:measurement(ecb__su_t2024, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(ecb__su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, climate_incorporation).

% DUAL FORMULATION NOTE:
% Constraint family: Article 127(1) TFEU is a single kernel — one treaty clause — whose natural-language label ('the ECB mandate') covers three structurally distinct claims, decomposed per the epsilon-invariance principle. This story instantiates the expansive reading (secondary objectives operationally weightable when price stability is not threatened). The orthodox reading (exclusive 2 percent focus; secondary objectives non-operational) is the historical baseline from which this reading drifted and is the position this reading's enforcement machinery must hold off. The climate-incorporation reading (mandatory climate-risk integration via Article 11 TFEU) is downstream: this reading's adoption creates the interpretive space — the legitimacy condition that secondary objectives can carry operational weight — that climate integration requires. The three stories have different epsilon values, different beneficiary sets, and different operational content; they are linked here and in their own files via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
