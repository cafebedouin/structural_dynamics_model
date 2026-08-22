% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: OST Article II Non-Appropriation Deferral to Future International Regime (International Regime Reading)
 *   domain: international_law/space_governance/commons
 *
 * SUMMARY:
 *   Article II of the Outer Space Treaty (1967) declares that celestial
 *   bodies are the "province of all mankind" and cannot be appropriated by
 *   any state. The treaty is silent on whether private actors can appropriate
 *   extracted resources or whether even state-licensed extraction constitutes
 *   prohibited appropriation. The international-regime reading holds that
 *   Article II deliberately defers this question to a future multilateral
 *   regime (via Article XI's mechanism for creating new binding agreements).
 *   Under this reading, neither extraction-permissive nor conservation
 *   interpretations carry treaty authority; legal uncertainty persists;
 *   first-mover firms operate in a grey zone; regime negotiation has stalled.
 *   The constraint is a Scaffold—a temporary deferral whose stated
 *   justification is eventual regime transition, but that transition has not
 *   occurred in 59 years. Theater ratio is high (0.71) because a large share
 *   of diplomatic and legal activity is procedural maintenance of the
 *   deferral question (COPUOS committees, draft regimes, academic commentary)
 *   rather than functional resolution. Measurement series show rising
 *   extractiveness and theater since ~2005 when space-mining ventures became
 *   technologically feasible and the deferral's real-world consequences
 *   became concrete.
 *
 * KEY AGENTS:
 *   - First-mover firms (spacefaring companies with mining technology): operate in grey zone, benefit from absence of binding regime, face risk if conservation reading prevails.
 *   - Spacefaring states (OST signatories with space agencies): set regime-negotiation agenda, can propose or block multilateral regimes, interests divided between extraction-favorable and conservation-favorable positions.
 *   - Conservation advocates (environmental groups, developing states, indigenous-rights organizations): argue Article II prohibits extraction, bear cost of negotiation delay, lose bargaining leverage as facts-on-the-ground accumulate.
 *   - Non-spacefaring and resource-restricted states: would benefit from equitable regime; excluded from negotiating authority; pay by having outcomes imposed.
 *   - Regime negotiation (the Article XI mechanism itself): absent as binding framework; its non-existence is the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.52).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.38).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.52).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Non-Appropriation Deferral to Future International Regime (International Regime Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_law/space_governance/commons").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, 'd22f1ae3-22aa-4ccc-b0be-3f8276b57294').
narrative_ontology:cs_kernel_codification('d22f1ae3-22aa-4ccc-b0be-3f8276b57294', fixed_text).
narrative_ontology:cs_authority_grounding('d22f1ae3-22aa-4ccc-b0be-3f8276b57294', distributed).
narrative_ontology:cs_reading_relation('d22f1ae3-22aa-4ccc-b0be-3f8276b57294', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_reading_relation('d22f1ae3-22aa-4ccc-b0be-3f8276b57294', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_axiom('d22f1ae3-22aa-4ccc-b0be-3f8276b57294', foundational, article_ii_defers_appropriation_to_regime).
narrative_ontology:cs_axiom_status(article_ii_defers_appropriation_to_regime, holdable).
narrative_ontology:cs_axiom_grounding('d22f1ae3-22aa-4ccc-b0be-3f8276b57294', article_ii_defers_appropriation_to_regime, conventional).
narrative_ontology:cs_axiom('d22f1ae3-22aa-4ccc-b0be-3f8276b57294', foundational, article_xi_mechanism_governs_regime_adoption).
narrative_ontology:cs_axiom_status(article_xi_mechanism_governs_regime_adoption, holdable).
narrative_ontology:cs_axiom_grounding('d22f1ae3-22aa-4ccc-b0be-3f8276b57294', article_xi_mechanism_governs_regime_adoption, conventional).
narrative_ontology:cs_reference_frame('d22f1ae3-22aa-4ccc-b0be-3f8276b57294', treaty_interpretation_principle_deferred_subjects).
narrative_ontology:cs_drift_state('d22f1ae3-22aa-4ccc-b0be-3f8276b57294', contemporary_space_mining_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d22f1ae3-22aa-4ccc-b0be-3f8276b57294', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, first_mover_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, regime_negotiation_stalled_actors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, conservation_advocates).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, resource_development_restricted_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, regime_negotiation_stalled_actors).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, treaty_interpretation_principle_deferred_subjects).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, article_xi_delegative_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Space mining and resource extraction companies operate in the regulatory grey zone created by Article II's deferred appropriation question. The absence of an authoritative multilateral regime gives them de facto freedom to prospect, extract, and sell resources while formal prohibition remains unresolved. They benefit from legal uncertainty that permits operation before any extraction-blocking regime is negotiated. Exit: if a conservation-reading regime is adopted, their operations become legally untenable; they can abandon claims but not retroactively recover investment.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Nations that would economically benefit from space resource extraction but are constrained by treaty ambiguity and first-mover disadvantage. They cannot act without risking international delegitimacy (if the conservation reading gains traction) or without capital, technology, and existing claims that first movers have already staked. They bear the opportunity cost of deferral: the window for resource access at acceptable cost narrows as first movers consolidate.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, resource_development_restricted_states, payer,
    moderate, generational, constrained, global).

% Environmental groups, indigenous-rights organizations, and states favoring preservation argue that Article II's non-appropriation principle prohibits resource extraction by any actor. The deferral to an international regime they have not yet won translates to operational delay: they must negotiate, litigate, or build consensus while first-mover operations accumulate facts-on-the-ground. Their bargaining position weakens the longer the regime negotiation stalls; they pay in opportunity cost and negotiating leverage.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, conservation_advocates, payer,
    organized, civilizational, constrained, global).

% Nation-states and blocks that benefit from deferral itself—those that prefer regime negotiation remain deadlocked rather than resolved. Some resource-rich but technologically weak states benefit from delay (it prevents first-mover dominance from becoming irreversible); some spacefaring states benefit from delay (it keeps options open without committing to either extraction or conservation). This group includes actors whose interests are distributed across both extraction and conservation readings, so deadlock preserves their optionality. They pay by bearing the reputational cost of stalled multilateralism and risk escalating claims-jumping.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, regime_negotiation_stalled_actors, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__international_regime, regime_negotiation_stalled_actors, payer).

% The analytical observation that no single international authority has legitimacy to adjudicate Article II's appropriation question absent explicit new treaty (Article XI's delegative mechanism). This is not an actor but the structural fact the international_regime reading rests on: Article II is incomplete without a multilateral regime framework, and that framework does not yet exist.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, interpretation_authority_absent, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__international_regime, interpretation_authority_absent).

% States with space capability and treaty authority (OST signatories with active space agencies and diplomatic voice) participate in regime negotiation. They set the agenda for new regime discussions, can propose or block regimes, and bear the cost of negotiation stalling. Some favor extraction-permissive regimes, others conservation; their divergence is why negotiation stalls. They do not directly extract or conserve but administer the treaty authority that could resolve it.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, spacefaring_states, agenda_setter,
    institutional, generational, mobile, global).

% States party to the OST with no space capability or minimal voice in regime negotiations. They would benefit from a conservation-reading regime (preserves the commons) or a development-equity regime (assures them access or benefit-sharing if extraction is permitted). Their exclusion from regime-setting negotiations means their interests are not centered in the bilateral/multilateral bargaining. They pay by having outcomes imposed on them.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states, excluded,
    moderate, generational, constrained, global).

% The institutional mechanism (Article XI, delegated regime-setting) whose absence is the constraint's defining feature. No regime exists; one would resolve the deferral. This is the sunset condition: if a regime is negotiated and adopted, the deferral constraint is discharged.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, future_multilateral_regime_framework, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__international_regime, future_multilateral_regime_framework).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__international_regime, first_mover_firms).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__international_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article II defers the appropriation question to a future multilateral regime, preserving treaty coherence by acknowledging that neither extraction nor conservation could be unilaterally authoritative without new agreement. The coordination problem solved is: how to govern the commons of space without splintering the treaty itself into conflicting interpretations.
% TRANSFER_FUNCTION: The deferral transfers bargaining authority (and delay cost) from the present moment to a future regime negotiation. First movers gain temporary operational freedom; conservation advocates gain time to build opposition; resource-restricted states lose opportunity; spacefaring states gain authority to set terms but bear negotiation burden. No direct material transfer occurs under this constraint; the transfer is of legal authority and temporal leverage.
% ABSENT_VOICES: Small island states and non-spacefaring developing nations would argue for equitable benefit-sharing if extraction is permitted, or for conservation if prohibition is chosen. Their voices are effectively excluded from regime-setting negotiations, which are dominated by spacefaring states and multinational extraction firms. They would demand that any future regime embed equity and consent mechanisms.
% DISAPPEARANCE_RATIONALE: If the deferral evaporated—if Article II were reinterpreted to authoritatively endorse either extraction or conservation—the legal grey zone would close and resource allocation would crystallize into one regime or the other. First movers would either consolidate their claims (if extraction is authorized) or face seizure and loss (if conservation is authorized). The entire space economy would reorganize around the new legal baseline.
% FOUNDING_PROBLEM: When the OST was drafted (1966–1967), space resource extraction was theoretical and remote. The treaty did not resolve whether the non-appropriation principle applied to resource extraction because the question was not urgent. Article II leaves the question to be answered by future international agreement, with Article XI providing the mechanism (new treaty, protocol, or binding regime).
% FOUNDING_PROBLEM_CORROBORATION: Spacefaring states and firms attest that extraction was not the intended target of Article II at the time of drafting. Conservation advocates and non-spacefaring states attest that the principle was always intended to prohibit de facto appropriation and that ambiguity reflects inadequate drafting, not deferred intent. OST scholarship is divided; UN COPUOS debates have produced no consensus on whether the founding problem is still live or already settled by the principle's language.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52) is moderate because the deferral itself does not directly extract value in the way a snare does—no one is paying tribute to anyone. Instead, the deferral creates regulatory freedom for first movers and delay costs for conservation advocates. The measurement is of how much the standing arrangement (deferral + grey zone) advantages extraction-capable actors over conservation-preferred actors, relative to what a binding regime would do. Suppression (0.38) is lower than extractiveness because the deferral is maintained more by diplomatic inertia and regime-negotiation deadlock than by active coercive enforcement. No one is being forcibly silenced; rather, conservation advocates lack the votes to block extraction operations and first-mover states lack the votes to adopt an extraction-permissive regime. Theater ratio rises sharply from 0.05 (1967, when extraction was theoretical) to 0.71 (2026, when firms operate but no regime has been negotiated); this climb reflects the increasing gap between the treaty's procedural gesture (deferral to future regime) and its functional output (continued ambiguity). Accessibility collapse is low (0.32) because alternatives remain visible: spacefaring states could adopt a conservation regime (formally banning extraction), an extraction-permissive regime (codifying property rights), or abandon the OST; firms could apply for national licenses or operate unilaterally; conservation advocates could pursue ICJ cases. The constraint's force is not that alternatives are invisible but that no single actor can impose one. Resistance is moderate-high (0.58) because conservation advocates actively resist extraction operations and propose conservation regimes; first movers resist conservation through diplomatic lobbying and operational assertion; spacefaring states resist each other's proposed regimes. The deferral persists because none of these resistances is strong enough to overcome stalemate.
 *
 * PERSPECTIVAL GAP:
 *   From a first-mover firm's seat, the deferral is enabling—it permits operation while legal authority is unsettled. From a conservation-advocate's seat, the deferral is disabling—it prevents them from winning a binding prohibition while first movers consolidate claims. From a spacefaring state's seat, the deferral is a stalled negotiation—they bear the cost of deadlock and risk of unilateral action breaking the treaty's coherence. The engine should compute different types per seat: first movers perceive Rope (coordination problem of regime uncertainty solved in their favor); conservation advocates perceive Tangled Rope or Snare (extraction masked as deferred governance); spacefaring states perceive Scaffold (intended transition mechanism that has failed to transition). This reading declares the constraint at the system level as Scaffold (the treaty's intended structure).
 *
 * DIRECTIONALITY LOGIC:
 *   First-mover firms are beneficiaries (d near 0.2–0.3): they benefit from the grey zone's operational freedom and have arbitrage-grade exit (they can exit extraction and shift to terrestrial resources if conservation prevails; they have capital and technology to absorb regime shift costs). Conservation advocates are targets (d near 0.7–0.8): they bear the cost of delay and erosion of their negotiating position; they are trapped in the OST framework (exit would mean abandoning the treaty entirely); their exit is identity-locked to conservation ideology. Resource-restricted states are symmetric-to-target (d near 0.6): they lose opportunity and have constrained exit (they can propose alternatives in COPUOS but cannot override spacefaring states). Regime-stalled actors benefit from the deferral itself (they prefer deadlock to either resolution) but also bear the reputational cost and risk of unilateral action; they are near-symmetric (d near 0.5). The reading's beneficiary group (first movers + actors preferring stalled negotiation) is smaller and more powerful than the victim group (conservation advocates + resource-restricted states); this asymmetry is why the deferral persists despite organized opposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was regime negotiation and transition. That mandate is dead: no binding regime has been adopted in 59 years and none is near. Yet the constraint (Article II's deferral language) persists because it is embedded in the foundational treaty, disenfranchising no particular seat's core function (spacefaring states still negotiate, firms still operate, advocates still advocate), and the cost of formal amendment exceeds the cost of indefinite deferral. This is a textbook Scaffold-to-Piton trajectory: the sunset clause (regime adoption) was never triggered; the constraint has accrued inertial characteristics (theater ratio 0.71 reflects mostly procedural maintenance without functional forward motion); and no single seat has enough power or motivation to force closure. Classification as Scaffold rather than Piton is justified because the formal mechanism (Article XI) and the stated justification (regime negotiation) are still formally active, even if empirically stalled. A Piton reading would emphasize that the administrative machinery (COPUOS regime committees) persists as theatrical performance. The reading remains Scaffold because the constraint's classification depends on recognizing the intended transition as the justification, not on whether that transition has actually occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contention_distribution,
    'Is the international_regime reading held primarily by spacefaring states seeking to preserve optionality, or is it genuinely the middle-ground consensus reading among OST scholars and COPUOS participants?',
    'Analysis of COPUOS voting records, OST scholarly consensus (treatises, law-review citations), and diplomatic statements; identification of which seats actively defend the deferral vs. which tolerate it as a stalling tactic.',
    'If spacefaring-state preference explains the reading''s persistence, the reading is a veneer for extraction-friendly delay, and the constraint is more extractive than the 0.52 score suggests. If genuinely balanced among scholars and seats, the reading''s authority is more robust. If the reading is actively opposed by conservation-advocate states but maintained by spacefaring states'' superior power, the reading is a false consensus maintained by power asymmetry (rendering it Tangled Rope rather than Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contention_distribution, empirical, 'Whose reading is this, really? Consensus or power-imposed?').

omega_variable(
    article_xi_delegative_mechanism_scope,
    'Does Article XI''s mechanism for creating binding regimes actually permit a protocol that authoritatively interprets Article II (settles the extraction question), or does Article XI''s scope not extend to interpretation of the original treaty text?',
    'Legal analysis of Article XI''s language and OST scholarship; test case: proposal of a binding interpretation protocol and examination of whether spacefaring states accept it as within Article XI''s scope or reject it as requiring full treaty amendment (which has higher hurdle).',
    'If Article XI permits binding interpretation: the sunset clause (regime adoption) is within the treaty''s own machinery and deferral can be formally discharged. If Article XI does not extend to interpretation: regime negotiation is technically impossible unless parties agree to treat any protocol as binding interpretation (a softer consensus); in that case, the deferral may be indefinite by design, and the constraint is structurally Piton (cannot be discharged by the stated mechanism).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_xi_delegative_mechanism_scope, conceptual, 'Is the sunset clause (future regime) actually reachable via Article XI, or is it structurally blocked by the treaty''s own amendment requirements?').

omega_variable(
    suppression_mechanism_state_vs_firm,
    'Is the measured suppression (0.38) maintained by active state-level enforcement against conservation-reading adoption (diplomatic blocking, COPUOS voting blocks), or by implicit firm-level coordination to avoid triggering clarity (avoiding aggressive extraction claims that would provoke conservation response)?',
    'Examination of COPUOS voting patterns and diplomatic records: do spacefaring states actively block conservation-regime proposals, or do conservation regimes simply fail to attract proposed sponsors? Examination of firm behavior: do extraction companies self-limit operations to avoid provoking conservation backlash, or do they assert maximalist extraction claims?',
    'If state enforcement dominates: suppression is top-down control of the deferral; the constraint is more coercive. If firm coordination dominates: suppression is internalized norm compliance; the constraint is less coercive. If both: they reinforce each other; removing one does not collapse the deferral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_state_vs_firm, empirical, 'Is suppression structural (state) or internalized (firm) or both?').

omega_variable(
    deferral_to_regime_vs_indefinite_ambiguity,
    'Was the deferral in 1967 intended as a genuine interim measure pending regime negotiation, or as a permanent evasion of the non-appropriation question that both parties (states and pro-extraction interests) preferred to avoid permanently?',
    'Historical analysis of treaty negotiation records, drafting committee minutes, and statements of intent by Soviet and US delegations; comparison with Article XI''s original scope as intended in 1967 vs. how it is understood now.',
    'If genuinely interim: the constraint''s mandate is regime negotiation and the Scaffold classification is appropriate. If permanent evasion: the deferral was always a Piton (indefinite theatrical procedure), and the classification should reflect that even the original intent was not transition. This changes how we interpret the sunset clause: real deadline vs. permanent stall.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deferral_to_regime_vs_indefinite_ambiguity, empirical, 'Was deferral always meant to transition, or was indefinite ambiguity the actual intent?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1967, 0.05).
narrative_ontology:measurement_basis(ost__tr_t1967, projected).
narrative_ontology:measurement(ost__tr_t1990, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1990, 0.15).
narrative_ontology:measurement_basis(ost__tr_t1990, projected).
narrative_ontology:measurement(ost__tr_t2005, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2005, 0.35).
narrative_ontology:measurement_basis(ost__tr_t2005, observed).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2015, 0.55).
narrative_ontology:measurement_basis(ost__tr_t2015, observed).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2020, 0.68).
narrative_ontology:measurement_basis(ost__tr_t2020, observed).
narrative_ontology:measurement(ost__tr_t2026, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2026, 0.71).
narrative_ontology:measurement_basis(ost__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement_basis(ost__be_t1967, projected).
narrative_ontology:measurement(ost__be_t1990, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement_basis(ost__be_t1990, projected).
narrative_ontology:measurement(ost__be_t2005, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement_basis(ost__be_t2005, observed).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement_basis(ost__be_t2015, observed).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement_basis(ost__be_t2020, observed).
narrative_ontology:measurement(ost__be_t2026, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(ost__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1967, 0.1).
narrative_ontology:measurement_basis(ost__su_t1967, projected).
narrative_ontology:measurement(ost__su_t1990, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement_basis(ost__su_t1990, projected).
narrative_ontology:measurement(ost__su_t2005, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement_basis(ost__su_t2005, observed).
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2015, 0.34).
narrative_ontology:measurement_basis(ost__su_t2015, observed).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2020, 0.36).
narrative_ontology:measurement_basis(ost__su_t2020, observed).
narrative_ontology:measurement(ost__su_t2026, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2026, 0.38).
narrative_ontology:measurement_basis(ost__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__international_regime, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_xi_regime_negotiation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, space_mining_regulatory_authority).

% DUAL FORMULATION NOTE:
% Article II non-appropriation is a contested kernel instantiated by three distinct constraint readings: commons_conservation (extraction prohibited), extraction_permissive (extraction permitted, no territorial claim), and international_regime (question deferred, this reading). Each reading has different ε, beneficiary/victim structure, and classification. The three readings coexist as live positions held by different spacefaring states, conservation advocates, and extraction firms. This story instantiates the international_regime reading, which holds that Article II deliberately defers the appropriation question to a future multilateral regime under Article XI. The other two readings are separate constraint stories (constraint_ids: ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive). All three are linked via network.affects_constraints and share the same kernel_id in their cs_structure (when populated). The ε-invariance test: the referent (the standing arrangement under contest) is identical for all three readings—Article II's text and its operation in international law—but the three readings assess that referent differently (conservation reads it as prohibitive, extraction-permissive reads it as permissive, international-regime reads it as deferred). Each reading authors its own ε independently from that fixed referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
