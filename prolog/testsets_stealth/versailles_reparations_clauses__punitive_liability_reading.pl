% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [RESOLVED MANDATROPHY]
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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles Article 231 Punitive-Liability Regime (Unlimited Reparations Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates one reading of the Versailles reparations kernel:
 *   the punitive-liability reading, under which Germany bears unique moral
 *   and financial responsibility for the total cost of the war and Article
 *   231 warrants claims bounded only by what can be collected. The standing
 *   arrangement under contest is the Article 231-anchored claim structure as
 *   it operated from the treaty's signature through the London Schedule, the
 *   Ruhr occupation, the Dawes and Young restructurings, and the collapse of
 *   payments by 1932. The reading's own lights supply the warrant for the
 *   claims — they do not shrink the measured burden, because a claim uncapped
 *   by design is maximally extractive as a structure regardless of whether
 *   one endorses its moral premise. Sibling readings (capacity-bounded
 *   liability; repudiation of a dictated treaty) are separate constraint
 *   files linked through the network section. KEY AGENTS (by structural
 *   relationship): - allied_creditor_states: Primary beneficiary
 *   (institutional/mobile) — receive the transfers and set the schedule -
 *   allied_reparations_commission: Agenda setter (institutional/constrained)
 *   — administers assessment, supervision, default declarations -
 *   german_workers_taxpayers: Primary target (powerless/trapped) — bear the
 *   payments through taxes, wages, and shortages -
 *   ruhr_industrial_communities: Secondary target (moderate/trapped) — bear
 *   occupation and passive-resistance costs directly -
 *   german_heavy_industrialists: Dual-positioned payer (powerful/arbitrage) —
 *   formally liable, materially cushioned by inflation and asset repricing -
 *   german_reich_government: Payer and domestic administrator
 *   (institutional/constrained) — delivers payments, chooses the resistance
 *   instruments - american_treasury_and_banks: Excluded creditor
 *   (institutional/mobile) — outside the treaty, upstream in the debt chain,
 *   funds the 1924-29 recycling - independent_economic_expertise: Analytical
 *   observer (analytical/analytical) — capacity assessments every adjustment
 *   plan adopted
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.85).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.72).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Article 231 Punitive-Liability Regime (Unlimited Reparations Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, '8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b').
narrative_ontology:cs_kernel_codification('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b', fixed_text).
narrative_ontology:cs_authority_grounding('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b', extraction).
narrative_ontology:cs_interpretation_layer_present('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b').
narrative_ontology:cs_reading_relation('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b', versailles_reparations_clauses__repudiation_reading, forecloses).
narrative_ontology:cs_axiom('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b', foundational, german_unique_war_guilt).
narrative_ontology:cs_axiom_status(german_unique_war_guilt, holdable).
narrative_ontology:cs_axiom_grounding('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b', german_unique_war_guilt, empirically_contingent).
narrative_ontology:cs_axiom('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b', secondary, total_cost_liability_entitlement).
narrative_ontology:cs_axiom_status(total_cost_liability_entitlement, overridden).
narrative_ontology:cs_axiom_grounding('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b', total_cost_liability_entitlement, conventional).
narrative_ontology:cs_reference_frame('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b', victor_adjudicated_unlimited_liability).
narrative_ontology:cs_drift_state('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b', lausanne_settlement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8e43d2b9-f214-4d7c-bf3c-5563dc9ca77b', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, ruhr_industrial_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, german_heavy_industrialists).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_heavy_industrialists).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_reich_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The victorious coalition that wrote the schedule. Through the London ultimatum they fixed a nominal sum and delivery program, then collected in gold, marks, coal, timber, ships, and livestock. Receipts went to reconstruction budgets in Belgium and France and to servicing inter-allied war debts. Their enforcement ladder ran from blockade and sanctions threats to occupying the Ruhr when deliveries lapsed. Loosening the claims meant telling their own publics the money was forgone; France resisted that above all.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, beneficiary,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, agenda_setter).

% The standing body created by the treaty to assess German capacity, fix annuities, supervise delivery, and declare default. Its findings triggered sanctions automatically. It could inspect German budgets and customs administration. Its authority existed only so long as the claims it administered did.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_reparations_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Carried the payments as taxes, wage restraint, and shortages — food and coal were short through 1919-1923. They had no vote in the schedule and no way to move their labor abroad at scale; capital could flee, wages could not. Occupation-era unemployment and the currency collapse destroyed savings and normalized precarity.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers, payer,
    powerless, biographical, trapped, national).

% Lived inside the enforcement action itself: after the 1923 occupation, mines and railways passed under the occupier's control, workers conducted passive resistance on strike pay printed in Berlin, and the region absorbed the direct costs — deportations, shootings, halted production — of the collision between the schedule and German default.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, ruhr_industrial_communities, payer,
    moderate, biographical, trapped, regional).

% Formally liable through corporate taxation and delivery quotas, but the currency collapse let them repay mark debts in worthless paper, reprice plant against foreign competitors, and buy assets cheaply; some firms took occupation-era contracts and benefited from the reconstruction demand the payments financed next door. Their exposure to the schedule was real but hedged in ways the worker-taxpayer seat's was not.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_heavy_industrialists, payer,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, german_heavy_industrialists, beneficiary).

% Owed the deliveries and administered their raising: taxation, customs, the printing press. It chose the resistance instruments — passive-resistance subsidies, discount-rate passivity — and negotiated every adjustment from Spa through Lausanne. Its fiscal sovereignty was the thing the schedule reached into; its room to refuse was bounded by occupation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_reich_government, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, german_reich_government, agenda_setter).

% Refused to ratify the treaty and held no seat in its machinery, yet sat upstream of the whole money flow: the Allied governments owed Washington for war purchases, and after 1924 private American lending to Germany recycled the annuities back across the Atlantic. When the lending stopped in 1929, the payment chain broke within two years.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, american_treasury_and_banks, excluded,
    institutional, generational, mobile, global).

% Analysts outside every government delegation — most prominently the Treasury economist who resigned in 1919 — argued the schedule exceeded any payable figure and that the moral framing was expedient cover. Their capacity estimates became the working basis of every later adjustment plan, and the expert committees of 1924 and 1929 institutionalized their method.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, independent_economic_expertise, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__punitive_liability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a single negotiated machinery for allocating the financial burdens of a continental war: fixed schedules, a joint commission to assess capacity and supervise delivery, and automatic sanctions for default — replacing piecemeal seizure by individual victors and giving the creditor coalition one common settlement instrument. It also anchored the victors' expectations for settling inter-allied war debts out of German receipts.
% TRANSFER_FUNCTION: Moved gold, foreign exchange, coal, coke, timber, dyestuffs, merchant shipping, and livestock from the German economy to the Allied creditor states — Belgium and France first in kind and cash, Britain applying receipts to its own transatlantic war-debt service — with American private lending recycling a large share of the payments between 1924 and 1929.
% ABSENT_VOICES: The German delegation was handed the terms without negotiation and signed under threat of resumed blockade and invasion; German fiscal authorities could appear before the Reparations Commission but held no vote. Neutral economic expertise was outside the room in 1919 — Keynes resigned rather than attach his name to the schedule — and entered only when crisis forced the Dawes inquiry. The populations whose taxes and wages would carry the payments had no seat anywhere in the process.
% DISAPPEARANCE_RATIONALE: Remove the clauses overnight in 1919 and there is no London ultimatum, no Ruhr occupation, no hyperinflationary passive resistance financed by printing, no Dawes loan cycle tying German revival to American credit, and no grievance narrative ready-made for the movement that repudiated the treaty outright. Allied reconstruction budgets and inter-allied debts require some other settlement mechanism; the political history of the Weimar Republic and the timing of its collapse are not plausibly the same.
% FOUNDING_PROBLEM: The victors faced reconstruction bills in Flanders and northern France, inter-allied war debts, and domestic electorates unwilling to tax themselves for the whole cost. Article 231 was drafted to establish a legal basis for charging Germany with civilian damage — its authors' notes describe a liability-framing device — but the punitive reading converted it into a moral warrant for the total cost of the war.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Keynes's contemporaneous dissent (his resignation letter and The Economic Consequences of the Peace) attests the schedule exceeded any defensible reading of the damage provision; the American government declined to subscribe to the war-guilt premise and stayed outside the treaty; the mixed-nationality Dawes and Young committees attested that capacity, not culpability, set what could be paid. After 1929 no Allied government defended the unlimited claim in substance — the silence at Lausanne is itself corroboration that the founding problem no longer lived.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.85) because the claim architecture was uncapped by design: the London Schedule's nominal 132 billion gold marks dwarfed any plausible annuity, and the A+B bond structure kept the unpaid balance outstanding as a permanent lien. The scalar characterizes the punitive-liability structure at its operative peak (1921-1923); the temporal series traces realized operation, which escalated to the Ruhr occupation and then decayed through Dawes, Young, the Hoover Moratorium, and Lausanne — the 1931-1932 points document enforcement failure, not redesign of the claim, which is why the series endpoint sits below the structural scalar. Suppression (0.72) is authored as a raw structural property, unscaled by power or scope: the regime's persistence required blockade leverage, sanctions machinery, and finally military occupation; unilateral default was punished, not merely discouraged. Theater is low (0.18) across the operative life because enforcement was materially real — coal trains, inspectors, troops — and turns upward only as the machinery outlives the payments. Accessibility collapse is moderate (0.58): exit routes existed — default, negotiation, inflation erosion, revisionist diplomacy — but each carried severe punishment, so alternatives were degraded rather than erased. Resistance is high (0.78): passive resistance, hyperinflation, serial default, and final repudiation. All three tracked series run on one shared eight-point grid (1919, 1921, 1923, 1925, 1927, 1929, 1931, 1932). Suppression_requirement is authored because enforcement capacity is the dynamic this story traces — a ratchet to occupation in 1923, then decay as force gave way to supervised borrowing.
 *
 * PERSPECTIVAL GAP:
 *   The creditor seat computes a settlement it built and polices: lawful satisfaction of an admitted debt, with the commission as neutral administration. The German payer seats compute the same structure as open-ended liability enforced by hunger and occupation. Inside the creditor coalition the seats diverge further: France's receipt-and-security interest favored pressing the claim regardless of capacity, while Britain's trade interest required a solvent Germany and pushed toward capacity-bounded schedules — same power atom, different exit horizons, different experienced constraint. German heavy industry sits between: formally a payer, materially hedged by inflation and asset repricing, so its experienced burden sits well below the worker-taxpayer seat's.
 *
 * DIRECTIONALITY LOGIC:
 *   Allied creditor states derive d near the beneficiary pole: declared beneficiaries with mobile exit and institutional power. German workers and taxpayers and the Ruhr communities derive d near the target pole: declared victims, trapped exit, little organized power. The Reich government is a payer with administrative duties — high d, slightly damped by its seat at every negotiating table. Heavy industrialists are the structural exception: absent from the victim roll, formally paying but positioned to arbitrage the currency collapse, so their effective directionality sits well below the other German seats — the derivation from the beneficiary/victim rolls alone would miss this, which is why their dual position is declared on the stakeholder surface rather than forced into the victim array. American creditors are outside the governed set entirely: an excluded seat, upstream in the debt chain, feeding the system with recycled credit rather than drawing from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — financing reconstruction and inter-allied settlement out of German transfers — was dead by 1931: the Hoover Moratorium suspended payments and Lausanne abandoned the claim in substance. The machinery persisted past its function: the commission's apparatus, the transfer bureaucracy built under Young, and the token bond at Lausanne were maintained for face-saving and legal continuity, which is the rising theater series after 1929. Declaring mandatrophy resolved keeps the classification honest in both directions: the arrangement is not pure extraction with coordination as cover, because a real allocation-and-settlement function operated and was repeatedly renegotiated through bargaining channels (Spa, Wiesbaden, Dawes, Young); nor is it clean coordination, because the same channels enforced asymmetric payment under threat of occupation. The dead-status-plus-world-rearranges mismatch is the intended signal here: the structure outlived its mandate by roughly a decade of increasingly theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the versailles_reparations_clauses kernel; what structural differences would the sibling readings produce if instantiated?',
    'Classify the sibling stories (versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading) over the same referent and compare per-seat types, epsilon, and victim sets across the family.',
    'The limited reading bounds claims by capacity (lower epsilon, weaker victim asymmetry, bargaining-centered enforcement); the repudiation reading voids the obligation outright (claim-side epsilon collapses toward zero, enforcement becomes purely defensive). The divergence locates the dispute in the semantic force of Article 231 and the validity of a dictated treaty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: kernel membership, reading identity, and expected sibling deltas.').

omega_variable(
    article_231_semantic_force,
    'Is the word ''responsibility'' in Article 231 a moral judgment of unique guilt (this reading) or a legal formality fixing civil liability for civilian damage as a precondition to a capped schedule?',
    'Drafting history — Phillimore and Smuts memoranda, the Franco-American exchange of letters of June 1919 — together with subsequent official Allied usage of the clause.',
    'If formality, the quasi-unlimited claim lacks textual warrant and this reading''s epsilon rests on political imposition rather than the clause itself; if moral judgment, the clause is the extraction instrument and the reading''s warrant is internal to the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_231_semantic_force, conceptual, 'Where the kernel contest is located: the semantic force of the war-guilt clause.').

omega_variable(
    german_transfer_capacity,
    'What was Germany''s sustainable external transfer capacity in the 1920s relative to the scheduled annuities?',
    'Balance-of-payments reconstruction from contemporary sources (Keynes, the Dawes and Young committee data) and later quantitative historiography.',
    'If capacity sat far below schedule, realized extraction was enforcement-limited rather than claim-limited and the suppression series carries more of the burden story; if capacity approached schedule, the claim''s size rather than enforcement explains the payment collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(german_transfer_capacity, empirical, 'Capacity-versus-claim gap underlying the payment record.').

omega_variable(
    hyperinflation_strategic_or_collapse,
    'Was the 1923 mark collapse a deliberate strategy to destroy the mark-denominated basis of the claims, or fiscal collapse under occupation costs?',
    'Reichsbank and Finance Ministry records, Cuno-era cabinet minutes, and discount-policy decisions assessed against the contemporary alternatives.',
    'Strategic inflation raises the resistance attributable to the payer seat and lowers realized extraction independently of enforcement; accidental collapse attributes the same erosion to state incapacity instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hyperinflation_strategic_or_collapse, empirical, 'Attribution of the inflation episode within the resistance record.').

omega_variable(
    lausanne_finality,
    'Did the Lausanne settlement extinguish the punitive claim structure or leave it dormant behind the token bond?',
    'Legal status of the never-issued Lausanne bond, subsequent German legislation of 1933, and creditor-government archives on revivability.',
    'If dormant, the late-interval theater measures a living claim maintained performatively (piton-direction drift inside a tangled-rope life); if extinguished, the theater measures wind-down of a dead mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lausanne_finality, empirical, 'End-state status of the claim structure after 1932.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(versailles_punitive_tr_t1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement(versailles_punitive_tr_t1921, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1921, 0.12).
narrative_ontology:measurement(versailles_punitive_tr_t1923, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1923, 0.08).
narrative_ontology:measurement(versailles_punitive_tr_t1925, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1925, 0.22).
narrative_ontology:measurement(versailles_punitive_tr_t1927, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1927, 0.28).
narrative_ontology:measurement(versailles_punitive_tr_t1929, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1929, 0.35).
narrative_ontology:measurement(versailles_punitive_tr_t1931, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1931, 0.5).
narrative_ontology:measurement(versailles_punitive_tr_t1932, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1932, 0.6).

% Extraction over time
narrative_ontology:measurement(versailles_punitive_be_t1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1919, 0.74).
narrative_ontology:measurement(versailles_punitive_be_t1921, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1921, 0.82).
narrative_ontology:measurement(versailles_punitive_be_t1923, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1923, 0.9).
narrative_ontology:measurement(versailles_punitive_be_t1925, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1925, 0.8).
narrative_ontology:measurement(versailles_punitive_be_t1927, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1927, 0.76).
narrative_ontology:measurement(versailles_punitive_be_t1929, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1929, 0.7).
narrative_ontology:measurement(versailles_punitive_be_t1931, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1931, 0.48).
narrative_ontology:measurement(versailles_punitive_be_t1932, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1932, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(versailles_punitive_su_t1919, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1919, 0.65).
narrative_ontology:measurement(versailles_punitive_su_t1921, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1921, 0.7).
narrative_ontology:measurement(versailles_punitive_su_t1923, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1923, 0.88).
narrative_ontology:measurement(versailles_punitive_su_t1925, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1925, 0.6).
narrative_ontology:measurement(versailles_punitive_su_t1927, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1927, 0.52).
narrative_ontology:measurement(versailles_punitive_su_t1929, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1929, 0.45).
narrative_ontology:measurement(versailles_punitive_su_t1931, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1931, 0.3).
narrative_ontology:measurement(versailles_punitive_su_t1932, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1932, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one fixed treaty text (Articles 231-247), three structurally distinct claims. This punitive reading carries the highest epsilon and the enforcement-heavy victim structure; the limited_responsibility_reading shares the referent but authors lower epsilon over a capacity-bounded claim with bargaining-centered enforcement; the repudiation_reading voids the obligation and authors the claim-side epsilon near zero. The punitive reading is upstream historically (operative 1919-1923), and its enforcement crises created the institutional conditions in which the limited reading was codified (Dawes, Young); the repudiation reading captured German politics by 1933. Epsilon differs across the family because the readings locate the claim's warrant differently, not because they measure different arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
