% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate Article 127 — Expansive Secondary-Objectives Reading
 *   domain: monetary policy/constitutional law/EU institutional governance
 *
 * SUMMARY:
 *   Article 127(1) TFEU directs the European System of Central Banks to
 *   maintain price stability and, 'without prejudice to that objective,' to
 *   support the Union's general economic policies. This story instantiates
 *   the expansive reading of that kernel: the mandate as a hierarchical but
 *   operational framework in which employment and growth carry real weight
 *   whenever price stability is not threatened, with the 'without prejudice'
 *   clause authorizing discretionary balancing. The epsilon referent is the
 *   standing arrangement under contest — the ECB's mandate as read and
 *   operated under this reading — assessed by the reading's own lights, which
 *   concede genuine distributional transfer and an accountability deficit
 *   even while asserting the arrangement's legitimacy. Sibling readings
 *   (orthodox price-stability exclusivity; required climate incorporation)
 *   are separate constraints with their own epsilon values and beneficiary
 *   structures; they are linked by network edges, not averaged into this one.
 *   The claim/metric relationship is deliberately unreconciled: the reading
 *   is claimed as a tangled structure — real coordination and real asymmetric
 *   transfer through the same instrument — and the metrics are authored from
 *   the observed operation, including the post-2022 partial retrenchment.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: agenda setter and institutional beneficiary (institutional/identity_locked) — controls the reading, collects discretionary authority, constituted by the mandate it administers
 *   - labor_market_participants: primary beneficiary (organized/mobile) — gains employment-supportive weighting when inflation is on target
 *   - indebted_households_and_firms: primary beneficiary (moderate/trapped) — debt service falls under accommodative exercise
 *   - asset_market_investors: beneficiary (powerful/arbitrage) — valuations rise under accommodation; repositions globally
 *   - eurozone_periphery_governments: beneficiary (powerful/constrained) — financing costs ease under the broad reading they advocate
 *   - savers_and_fixed_income_holders: primary payer (moderate/mobile) — real returns compress under prolonged accommodation
 *   - northern_creditor_member_states: primary payer (powerful/constrained) — bears inflation and moral-hazard exposure, litigates from inside
 *   - eurozone_electorates: excluded voice (organized/constrained) — bears distributional outcomes with no direct vote
 *   - cjeu: institutional reviewer (institutional/analytical) — bounds the balancing through proportionality review
 *   - german_federal_constitutional_court: national reviewer (institutional/analytical) — contests the settlement from a constitutional seat
 *   - climate_policy_advocates: excluded voice (organized/constrained) — would occupy the discretionary space differently
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.48).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.5).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.48).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate Article 127 — Expansive Secondary-Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary policy/constitutional law/EU institutional governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, '32180c3f-eacb-41ec-9653-9fe8bce187e5').
narrative_ontology:cs_kernel_codification('32180c3f-eacb-41ec-9653-9fe8bce187e5', fixed_text).
narrative_ontology:cs_authority_grounding('32180c3f-eacb-41ec-9653-9fe8bce187e5', lineage).
narrative_ontology:cs_interpretation_layer_present('32180c3f-eacb-41ec-9653-9fe8bce187e5').
narrative_ontology:cs_reading_relation('32180c3f-eacb-41ec-9653-9fe8bce187e5', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('32180c3f-eacb-41ec-9653-9fe8bce187e5', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('32180c3f-eacb-41ec-9653-9fe8bce187e5', foundational, secondary_objectives_operational_when_primary_unthreatened).
narrative_ontology:cs_axiom_status(secondary_objectives_operational_when_primary_unthreatened, holdable).
narrative_ontology:cs_axiom_grounding('32180c3f-eacb-41ec-9653-9fe8bce187e5', secondary_objectives_operational_when_primary_unthreatened, conventional).
narrative_ontology:cs_axiom('32180c3f-eacb-41ec-9653-9fe8bce187e5', secondary, monetary_policy_is_inherently_distributional).
narrative_ontology:cs_axiom_status(monetary_policy_is_inherently_distributional, holdable).
narrative_ontology:cs_axiom_grounding('32180c3f-eacb-41ec-9653-9fe8bce187e5', monetary_policy_is_inherently_distributional, empirically_contingent).
narrative_ontology:cs_reference_frame('32180c3f-eacb-41ec-9653-9fe8bce187e5', flexible_balancing_framework).
narrative_ontology:cs_drift_state('32180c3f-eacb-41ec-9653-9fe8bce187e5', contemporary_post_strategic_review, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('32180c3f-eacb-41ec-9653-9fe8bce187e5', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, labor_market_participants).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households_and_firms).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, asset_market_investors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_periphery_governments).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, savers_and_fixed_income_holders).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, northern_creditor_member_states).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, secondary_objectives_operational_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the single monetary policy for the euro area and decides, case by case, how much operational weight employment and growth receive when inflation is at target. It interprets its own mandate, documents proportionality under legal challenge, and accumulates expanded discretionary authority each time a novel tool is sustained by the courts. It cannot rewrite the treaty that empowers it, and abandoning the mandate would dissolve the institutional identity the mandate constitutes — its legitimacy and its function are the same object.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, beneficiary).

% Workers and their unions, whose employment and wage outcomes improve when monetary policy leans against downturns. They gain from the permission to weight employment when inflation is on target. They hold no seat in the Governing Council and reach policy indirectly through wage bargaining, national governments, and social-partner consultation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, labor_market_participants, beneficiary,
    organized, biographical, mobile, continental).

% Mortgage holders, leveraged firms, and high-debt sovereigns whose debt service falls when policy stays accommodative. Their obligations cannot be walked away from without default, so they are bound to the interest-rate path the Governing Council sets; the balancing discretion operates in their favor whenever it is exercised.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households_and_firms, beneficiary,
    moderate, biographical, trapped, continental).

% Holders of bonds and equities whose valuations rise under asset purchases and low rates. They reposition globally at low cost, anticipate Governing Council decisions ahead of publication, and their market reactions feed back into the policy calculus as a standing constituency for accommodation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, asset_market_investors, beneficiary,
    powerful, immediate, arbitrage, global).

% Governments of high-debt member states whose financing costs fall under accommodative policy and crisis tools justified in part by the general-economic-policies clause. They advocate the broadest possible reading of their own accord but do not control the balance; leaving the currency would cost more than staying.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_periphery_governments, beneficiary,
    powerful, biographical, constrained, national).

% Retail depositors and holders of fixed-income retirement savings whose real returns compress under prolonged accommodation and negative rates. Their interests are diffuse and weakly organized relative to debtor and asset constituencies; exit into real assets or foreign currency exists in principle but is costly, unfamiliar, and impractical for most.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, savers_and_fixed_income_holders, payer,
    moderate, biographical, mobile, continental).

% Governments and publics of creditor states that bear inflation risk and moral-hazard exposure when policy runs accommodative in ways that ease periphery debt dynamics. They contest the arrangement through constitutional litigation and Council politics; euro exit would cost more than remaining, so they fight from inside the structure they object to.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, northern_creditor_member_states, payer,
    powerful, generational, constrained, national).

% The publics of the member states, who bear the distributional consequences of every balancing decision — in mortgage rates, savings returns, and employment — yet vote in no Governing Council and can hold no direct vote on its decisions. Accountability runs through thin channels: parliamentary hearings, governments that appoint rather than instruct, and court review after the fact.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_electorates, excluded,
    organized, biographical, constrained, continental).

% The Court of Justice of the European Union reviews whether Governing Council measures stay within the mandate, applying proportionality review. Its rulings (Gauweiler, Weiss) define how much balancing the treaty text tolerates and how much justification the Council must produce.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, cjeu, observer,
    institutional, generational, analytical, continental).

% Reviews ECB measures against German constitutional law and found the Governing Council's proportionality documentation insufficient in its 2020 PSPP judgment. It cannot strike down EU law directly but exerts sustained pressure on the interpretive settlement from a national constitutional seat.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, german_federal_constitutional_court, observer,
    institutional, generational, analytical, national).

% Environmental policy constituencies and EU institutions pressing for climate integration in asset purchases and collateral frameworks. Under this reading they hold no operational claim on the balance and would object that the discretionary space should carry environmental conditionality rather than being allocated case by case.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, climate_policy_advocates, excluded,
    organized, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__expansive_secondary_objectives, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single credible monetary framework for twenty sovereign economies: anchors inflation expectations around a common target, supplies one decision point for the currency, and — under this reading — permits counter-cyclical weight on employment and growth when price stability is not threatened, stabilizing output without unanchoring expectations.
% TRANSFER_FUNCTION: Moves distributional position through the policy stance: from savers, fixed-income holders, and creditor-state constituencies toward debtors, workers, periphery sovereigns, and asset holders during accommodative exercise, and partially back during tightening. The reading allocates the discretion over when and how far that transfer runs, and to whom the decision itself belongs.
% ABSENT_VOICES: Eurozone electorates bear every balancing outcome but hold no vote in the Governing Council; their accountability channels (parliamentary hearings, appointing governments, after-the-fact court review) are thin relative to the stakes. Savers are diffuse and weakly organized against well-positioned debtor and asset constituencies. Climate-policy constituencies would object that the discretionary space should carry environmental conditionality; under this reading they are outside the balance entirely.
% DISAPPEARANCE_RATIONALE: If the mandate and its expansive reading vanished overnight, the euro would lose its monetary authority's legal foundation: inflation expectations would unanchor, national central banks would have no common decision point, and the member states would face immediate treaty renegotiation under crisis conditions. Every distributional arrangement built on the policy stance — periphery debt dynamics, asset valuations, savings returns — would rearrange around whatever authority replaced it.
% FOUNDING_PROBLEM: The treaty's drafters needed a monetary authority for a currency union that would be credible against the 1970s inflation record — hence price stability as the primary objective — while remaining tethered to the Union's broader economic objectives of growth and employment, hence the 'without prejudice' second sentence. The two-sentence structure encoded both commitments and left their operational relationship deliberately unsettled.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the German Federal Constitutional Court's jurisprudence attests price-stability primacy as the mandate's core from a contesting seat; the Bundesbank tradition and orthodox monetary economics attest that the balancing question remains unresolved; national parliamentary debates across creditor states register the same live contest. The ECB's own 2021 strategic review documents that the operational relationship between the objectives required formal settlement two decades after founding — though the ECB is an interested party and its attestation is corroborating only in conjunction with the external sources.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.48, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.48 at interval end) because the arrangement's coordination core is real — the mandate anchors expectations and provides the euro's single decision point — while the discretionary space generates recurring asymmetric transfers whose costs land on identifiable seats. Suppression is 0.5: structural lock-in is heavy (euro exit is prohibitive, treaty amendment requires unanimity and ratification in every member state) but contestation channels exist and are used (courts, parliaments, the strategic review itself). Theater is 0.3: the secondary-objectives language does real allocative work during crisis operations and is invoked more rhetorically in calm periods. The measurement series runs on one shared time grid (1999, 2003, 2008, 2012, 2016, 2020, 2023, 2025) with all three tracked metrics authored at every point. The extraction trajectory is cyclical rather than monotonic: it rises through each crisis-driven accommodation (2008-2012, 2014-2020) and partially recedes during tightening (post-2022), but each cycle ends above its predecessor — a precedent ratchet in which novel tools (OMT, PSPP, PEPP) remain available after the emergency passes. Suppression_requirement is authored because this story specifically tracks enforcement-capacity change: the machinery of legal defense (proportionality documentation, institutional advocacy before the CJEU and the German Federal Constitutional Court) built up through the litigation era and partially eased after the strategic review formalized the reading. Theater dips during active crisis operations, when the language is doing real work, and recovers in calmer periods. The oscillation is driven by the policy cycle (crisis, accommodation, inflation, tightening) and functions partly as intermittent reinforcement: each emergency normalizes a precedent the calm phase never fully withdraws. base_properties were measured at the 2025 end-state: post-tightening, post-strategic-review, with the ratcheted baseline in place. Receipt: the distributional gains are diffuse across beneficiary seats, but the institutional rent — accumulated discretionary authority, an expanded formal remit — concentrates demonstrably on the Governing Council, which is why gain_flow names that seat.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the Governing Council's position the arrangement is a mandate faithfully executed — the treaty's own second sentence doing what it says. From the saver and creditor-state seats the same structure operates as a recurring transfer decided without their vote and justified after the fact. From the orthodox camp's seat (holders of the sibling reading) the expansive reading is mandate erosion; from this reading's seat it is the mandate's proper operation — that divergence belongs to the sibling files, not to this one. Two member-state governments sit at the same formal power level on opposite sides: northern creditor states and periphery debtor states differ not in institutional standing but in net debt position and inflation exposure — the constraint-specific factor that differentiates same-level actors. The inter-institutional gap is equally real: the CJEU polices the balance from within the EU legal order; the German Federal Constitutional Court contests it from a national constitutional seat with no direct strike-down power but real pressure capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   The Governing Council sits near the beneficiary end: it collects discretionary authority and is structurally subsidized by the reading, though its identity-lock to the mandate means the arrangement also binds it — it both collects and is constituted, which is why its exit is authored identity_locked rather than arbitrage. Workers, debtors, periphery governments, and asset holders derive low directionality from their beneficiary declarations — the exercised discretion runs in their favor, and the asset holders' arbitrage-grade exit puts them nearest the subsidy end. Savers derive high directionality from their payer declaration; the creditor states derive high directionality amplified by their constrained exit — formally powerful, structurally unable to leave, they sit near the full-target end despite their institutional weight. The electorates carry no beneficiary or victim declaration and sit near the canonical midpoint: they both gain and pay diffusely, and their defining structural fact is absence from the decision seat rather than position in the transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem — a currency union needs a monetary authority credible on price stability yet tethered to the Union's broader economic objectives — remains live, and the founding_problem_status is authored live with external corroboration. The classification prevents two mislabelings. Reading the mandate as pure coordination would erase the identifiable cost-bearers (savers, creditor-state constituencies) and the unaccountable character of the balancing discretion; reading it as pure extraction would erase the expectation-anchoring and stabilization function that makes the arrangement load-bearing for the entire currency area. The tangled structure — genuine coordination and genuine asymmetric transfer operating through the same instrument, held up by active legal enforcement — is the honest description, and the per-seat computation is where the divergence between those two mislabelings actually lives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operative_reading_of_kernel,
    'This constraint is one reading of the Article 127 kernel — which reading is the operative constraint: exclusive price-stability focus (orthodox_price_stability), operational secondary objectives (this story), or required climate integration (climate_incorporation)?',
    'CJEU jurisprudence on the second sentence of Article 127(1), the codification achieved by the 2021 strategic review, and the appointment politics that shape Governing Council majorities.',
    'Under the orthodox reading the beneficiary set collapses to price-stability constituencies and the balancing discretion disappears; under climate incorporation the beneficiary set extends to environmental constituencies and the discretion becomes a directed obligation. Each resolution yields a different constraint with a different epsilon, not a reweighting of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operative_reading_of_kernel, conceptual, 'Committer structure: this story instantiates one of three live readings of the ecb_mandate_article_127 kernel; the operative reading is contested and the disagreement is located in the operative weight of the ''without prejudice'' clause.').

omega_variable(
    without_prejudice_operative_weight,
    'Does the ''without prejudice'' clause carry operative legal weight authorizing discretionary balancing, or is it declaratory language that leaves price stability the sole operational objective?',
    'A definitive CJEU ruling on the clause''s normative force, or treaty revision clarifying the mandate''s structure.',
    'If merely declaratory, this reading''s authorization collapses toward the orthodox reading and the declared beneficiary structure dissolves; if operative, the balancing discretion stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(without_prejudice_operative_weight, conceptual, 'Whether the treaty clause authorizes real balancing or is hortatory support language.').

omega_variable(
    stabilization_tradeoff_at_target,
    'When inflation is at target, does operational weight on employment and growth cost price stability over the relevant horizon, or are the objectives coincident at target?',
    'Macroeconomic evidence on the output-inflation tradeoff at target and on the distributional incidence of past accommodative episodes.',
    'If the objectives are coincident at target, the reading''s discretion is near-costless coordination and extraction falls toward the coordination floor; if a tradeoff exists, the transfer from savers and creditor states is real and extraction rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stabilization_tradeoff_at_target, empirical, 'Whether the balancing discretion carries real distributional cost or is a free lunch at target.').

omega_variable(
    democratic_accountability_adequacy,
    'Is the discretionary balancing exercised under accountability adequate to its distributional stakes?',
    'Institutional analysis of European Parliament hearings, national parliament oversight, and appointment accountability, measured against the distributional magnitude of the decisions taken.',
    'If accountability is inadequate, effective suppression rises above the structural measure — the arrangement is harder to contest than its formal channels suggest; if adequate, the coordination component of the structure is stronger than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_accountability_adequacy, empirical, 'Adequacy of oversight over balancing decisions by an unelected body.').

omega_variable(
    precedent_ratchet_permanence,
    'Is the post-crisis baseline ratchet permanent — do emergency tools and expanded readings persist after the emergency passes — or do they fully unwind under sustained orthodox pressure?',
    'Observation across the next full policy cycle: whether PEPP-era tools and the strategic-review framework survive a sustained tightening-and-contestation phase.',
    'If the ratchet unwinds, the extraction trajectory is cyclical without drift; if it holds, base extractiveness trends upward across cycles and the arrangement drifts toward heavier extraction with each emergency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(precedent_ratchet_permanence, empirical, 'Permanence of the crisis-precedent ratchet in the reading''s operational baseline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 1999, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_mandate_expansive_tr_t1999, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 1999, 0.3).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t1999, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2003, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2003, 0.28).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2003, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2008, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2008, 0.24).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2008, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2012, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2012, 0.22).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2012, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2016, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2016, 0.25).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2016, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2020, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2020, 0.22).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2020, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2023, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2023, 0.27).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2023, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2025, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2025, 0.3).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(ecb_mandate_expansive_be_t1999, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 1999, 0.35).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t1999, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2003, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2003, 0.36).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2003, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2008, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2008, 0.44).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2008, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2012, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2012, 0.5).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2012, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2016, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2016, 0.56).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2016, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2020, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2020, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2023, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2023, 0.52).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2023, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2025, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2025, 0.48).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(ecb_mandate_expansive_su_t1999, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 1999, 0.4).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t1999, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2003, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2003, 0.42).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2003, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2008, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2008, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2012, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2012, 0.55).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2012, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2016, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2016, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2020, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2020, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2023, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2023, 0.58).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2023, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2025, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% The colloquial label 'the ECB mandate' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle: an exclusivity claim (orthodox_price_stability — beneficiaries concentrate on price-stability constituencies, negligible discretionary transfer), a permissive-balancing claim (this story — real balancing with identifiable cost-bearers), and an integration obligation (climate_incorporation — discretion converted into directed obligation with a different beneficiary set again). The treaty text is the upstream authority cited by both downstream readings; this reading structurally influences the climate reading because a permissive-balancing precedent lowers the legitimacy barrier to adding further operational objectives, without foreclosing the orthodox camp's position. Each member is authored as a separate file; this file links both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
