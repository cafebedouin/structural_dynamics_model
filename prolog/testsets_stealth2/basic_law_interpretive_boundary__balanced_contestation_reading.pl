% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary — Balanced Contestation Reading
 *   domain: constitutional/legal/institutional
 *
 * SUMMARY:
 *   Israel's constitutional order allocates final interpretive authority over
 *   the Basic Laws through a contested boundary rather than a settled
 *   supremacy clause: the Supreme Court interprets within a jurisdictional
 *   domain marked by justiciability, standing, and deference doctrines, while
 *   the Knesset retains constituent power to legislate and amend, bounded by
 *   international obligations and judicial-independence norms. Neither
 *   institution holds final authority; the boundary persists through active
 *   contestation — doctrine on the Court's side, amendment and override
 *   politics on the Knesset's — with the executive as third party in the
 *   negotiation. This story authors ONLY the balanced-contestation reading of
 *   that arrangement: the standing arrangement under contest is the bounded
 *   mutual-authority equilibrium itself, and epsilon (0.58) is assessed by
 *   this reading's own lights against that referent — not against the
 *   judicial-supremacy or parliamentary-sovereignty arrangements that the
 *   sibling stories cover. The claim/metric pairing is deliberate and
 *   independently authored: the reading claims tangled_rope (genuine
 *   coordination through bounded mutual authority plus real, domain-rotating
 *   asymmetric costs, actively enforced), while the metrics describe the
 *   arrangement's observed operation over 1995-2025, a period of steady
 *   extractive-ward drift that culminated in the 2023-2024 crisis.
 *
 * KEY AGENTS:
 *   - supreme_court_of_israel: Co-agenda-setter and beneficiary (institutional/identity_locked) — administers the justiciability and deference doctrines marking the boundary's Court-side edge; collects interpretive authority in its domains
 *   - knesset_legislative_majorities: Co-agenda-setter and beneficiary (institutional/constrained) — administers the amendment and override levers on the boundary's legislative side; retains sovereign legislative power in most domains
 *   - rights_claimants_in_deferred_domains: Primary payer (powerless/trapped) — bears the arrangement's recurring costs where the Court defers to the elected branches
 *   - occupied_territories_residents: Primary payer and excluded voice (powerless/trapped) — governed by the arrangement's outputs with no seat in the dialogue and no vote
 *   - legislative_minorities: Payer with episodic beneficiary position (organized/constrained) — pay when the Court defers, protected when it reviews
 *   - international_treaty_partners: Beneficiary (institutional/mobile) — receive legislative constraint via obligations without internal enforcement power
 *   - israeli_public_electorate: Near-symmetric beneficiary/payer (organized/constrained) — receives accountability and rights protection, pays crisis uncertainty
 *   - comparative_constitutional_scholars: Analytical observer (analytical/analytical) — documents the boundary's drift and supplies the comparative evidence both sides invoke
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.58).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.65).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary — Balanced Contestation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional/legal/institutional").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '8e9eb897-241f-4744-a9e8-eca5cea9efd5').
narrative_ontology:cs_kernel_codification('8e9eb897-241f-4744-a9e8-eca5cea9efd5', fixed_text).
narrative_ontology:cs_authority_grounding('8e9eb897-241f-4744-a9e8-eca5cea9efd5', distributed).
narrative_ontology:cs_reading_relation('8e9eb897-241f-4744-a9e8-eca5cea9efd5', basic_law_interpretive_boundary__judicial_supremacy_reading, influences).
narrative_ontology:cs_reading_relation('8e9eb897-241f-4744-a9e8-eca5cea9efd5', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('8e9eb897-241f-4744-a9e8-eca5cea9efd5', foundational, no_institution_holds_final_interpretive_authority).
narrative_ontology:cs_axiom_status(no_institution_holds_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('8e9eb897-241f-4744-a9e8-eca5cea9efd5', no_institution_holds_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('8e9eb897-241f-4744-a9e8-eca5cea9efd5', foundational, institutional_dialogue_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(institutional_dialogue_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8e9eb897-241f-4744-a9e8-eca5cea9efd5', institutional_dialogue_constitutes_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('8e9eb897-241f-4744-a9e8-eca5cea9efd5', bounded_mutual_authority_framework).
narrative_ontology:cs_drift_state('8e9eb897-241f-4744-a9e8-eca5cea9efd5', post_2023_judicial_contestation_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8e9eb897-241f-4744-a9e8-eca5cea9efd5', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_of_israel).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_legislative_majorities).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, international_treaty_partners).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_public_electorate).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, rights_claimants_in_deferred_domains).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, occupied_territories_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_public_electorate).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_pluralism_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__balanced_contestation_reading, dialogic_review_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_independence_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws and reviews ordinary legislation, and administers the doctrines of justiciability, standing, and deference that mark where its authority ends. Its rulings bind lower courts and the government, but its authority over Knesset legislation — especially Basic Law amendments — is precisely what the boundary contest decides; in 2024 it invalidated an amendment to a Basic Law for the first time. Its institutional identity is constituted by its interpretive role: stepping outside the constitutional order would dissolve it. It collects interpretive authority in its domains and loses ground when the coalition legislates around its rulings.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_of_israel, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court_of_israel, beneficiary).

% Passes ordinary legislation and amends Basic Laws by coalition majority, and administers the levers on the legislative side of the boundary: override-clause bills, changes to the Judicial Selection Committee, and new Basic Laws that restructure review. Coalitions last a few years on average, so the Court's generational patience reads as entrenchment from this seat. It retains sovereign legislative power in most policy domains but pays when the Court strikes its legislation, when international partners condition cooperation on compliance, and when judicial-independence norms raise the political cost of court-curbing bills.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_legislative_majorities, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_legislative_majorities, beneficiary).

% Litigants in domains where the Court applies deference doctrines — security measures, immigration and asylum, planning and land, administrative detention — including Palestinian citizens, asylum seekers, and detainees. They bear the arrangement's recurring costs when the Court declines to intervene against the elected branches, and they cannot exit the jurisdiction or take their claims to another forum.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, rights_claimants_in_deferred_domains, payer,
    powerless, immediate, trapped, national).

% Opposition factions and the constituencies they represent. They pay when the Court defers and a governing coalition legislates over their objection; they gain when the Court strikes rights-infringing legislation. Their protection against the majority runs through the very boundary the majority seeks to move, which ties their fortunes to an institution they do not control.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_minorities, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_minorities, beneficiary).

% UN treaty bodies, foreign governments, and international organizations whose interests are served by a Knesset constrained by international obligations. They receive compliance through reporting, observation, and pressure — statements, aid conditionality, diplomatic cost — but hold no direct enforcement power inside the constitutional order and can disengage, sanction, or downgrade relations if compliance fails.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_treaty_partners, beneficiary,
    institutional, generational, mobile, global).

% Voters receive both democratic accountability (the coalition's legislative power) and, in some domains, rights protection (the Court's review). They pay uncertainty costs during boundary crises: the 2023-2024 period produced mass protests, reserve-duty refusal threats, credit-rating downgrades, and a wartime constitutional standoff. Exit means emigration, available to a minority; their organized voice runs through elections and street mobilization, not through any formal constitutional-ratification mechanism.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_public_electorate, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, israeli_public_electorate, payer).

% Residents of the West Bank governed by Knesset legislation and military orders subject to the Court's review, without citizenship or vote in the polity whose institutions set the rules that bind them. Deference doctrines apply to them most heavily; they bear the boundary's costs at the highest rate and hold no seat in the institutional dialogue among Court, Knesset, and executive.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, occupied_territories_residents, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, occupied_territories_residents, excluded).

% Israeli and comparative constitutional scholars who document the boundary's operation, its drift, and its crises; both institutional sides cite their work, and their analyses supply the comparative evidence (dialogic review in other systems) that each side marshals for its own position. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__balanced_contestation_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__balanced_contestation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates final interpretive authority over the Basic Laws so that neither the Court nor the Knesset can monopolize constitutional meaning: the Court interprets within a bounded jurisdictional domain (justiciability, standing, and deference doctrines mark its edges), while the Knesset retains constituent power to legislate and amend, bounded by international obligations and judicial-independence norms. The arrangement solves a real collective-action problem — a constitutional order with no settled interpretive authority would collapse into recurring zero-sum crises — by keeping both rights protection and democratic accountability in operation while holding either institution's monopoly claim in check.
% TRANSFER_FUNCTION: Moves interpretive authority — and the policy-determining power that rides on it — between the Court and the Knesset depending on domain: in security, immigration, planning, and territories domains the Court's deference doctrines transfer authority to the elected branches; in civil-rights and administrative-fairness domains review transfers authority to the Court. Compliance costs land on rights-claimants in deferred domains and on governing coalitions in reviewed domains.
% ABSENT_VOICES: Occupied-territories residents and rights-claimants in deference-heavy domains bear the boundary's recurring costs but hold no seat in the institutional dialogue — the negotiation runs among the Court, the Knesset, and the executive, not with those who pay when the Court defers. The public at large has no referendum or citizens'-assembly mechanism; its voice enters only through elections and street mobilization, and the 2023 protest movement was that voice operating outside any formal channel.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, one institution's claim to final authority would immediately fill the vacuum: either the Court's invalidations become unconditionally binding (the judicial-supremacy arrangement) or the Knesset's simple majority becomes constitutionally final, including over review (the parliamentary-sovereignty arrangement). The 2023-2024 crisis demonstrated the rearrangement dynamics — an override-clause attempt and the Court's first-ever invalidation of a Basic Law amendment each moved the order toward one pole and triggered mass mobilization, reserve-duty refusal threats, and international pressure. The constitutional order would reorganize around whichever pole moved first, with the losing institution's function either abolished or subordinated.
% FOUNDING_PROBLEM: Israel's constitutional order was built incrementally: the Knesset legislated Basic Laws beginning in 1948-1950 without a formal entrenched constitution, and until 1995 no institution held settled authority to interpret them against ordinary legislation. The founding problem was who decides what the Basic Laws mean when the Court and the Knesset disagree — a question the 1992 constitutional revolution (Basic Law: Human Dignity and Liberty) and the 1995 United Mizrahi Bank decision made unavoidable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative constitutional scholarship (Israeli and international) documents the unresolved finality question as the central structural fact of the order; the 2023 electoral cycle and mass-protest movement made the judiciary's authority the central public issue, attested in polling and party platforms; UN treaty-body reviews and foreign-government statements attest that the international-obligations constraint is contested rather than settled. The two benefiting institutions dispute each other's account — the Court attests the problem is live in its docket, the coalition attests it is live in its legislative agenda — so the live status does not rest on either beneficiary's self-report alone.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate and rising (0.28 to 0.58 across the interval) because the boundary's costs are asymmetric by domain and rotate between payer seats: rights-claimants pay in deference domains (security, immigration, planning, the territories), governing coalitions pay in reviewed domains, and as contestation hardened after 2020 the negotiation itself became a recurring cost imposed on all parties. Suppression (0.65 at end) reflects the coercive machinery now required to hold the boundary — the Court's first-ever invalidation of a Basic Law amendment (2024), the coalition's override-clause and Judicial Selection Bill attempts, appointment-committee stalemates — where mutual-restraint norms once sufficed; this is a story that specifically tracks enforcement-capacity change, so suppression_requirement is authored as a rising series. Theater_ratio rises from 0.10 to 0.40 as 'dialogue' rhetoric increasingly covers unilateral maximalist moves: override clauses introduced to discipline the Court rather than passed, committee hearings staged for coalition messaging, the Court's own dialogic language accompanying maximalist rulings. Accessibility_collapse is low (0.30): the defining feature of this arrangement is that the alternatives — the two sibling readings — remain fully live; the boundary does not eliminate them, it holds them in tension. Resistance is high (0.72): each institution actively contests the other's claims, and the public mobilized at historic scale in 2023. All three metric series run on one shared time grid (seven points, five-year spacing) so every metric is authored at every examined time point. The trajectory is monotonic escalation, not cyclical: norm erosion and polarization compound rather than oscillate, and no intermittent-reinforcement mechanism drives the pattern.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the Court's seat the arrangement is a protected but besieged domain: it experiences the Knesset's amendment and override politics as the cost imposed on it, and its identity-lock (its institutional role IS interpretive authority) makes exit unthinkable. From the governing-coalition seat the same arrangement is an unaccountable veto it cannot fully remove: review, international obligations, and independence norms are the costs, and the coalition's biographical time horizon makes the Court's generational patience look like entrenchment. From the payer seats the arrangement's costs are concrete and domain-specific — a detainee's deferred petition, a minority's outvoted rights, a resident's lack of any vote — while its coordination benefits are diffuse and deferred; these seats lack coalition power individually, and the only coalition that materialized (the 2023 protest movement) was the general public's, not theirs. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The Court and the governing coalition are the dual agenda-setter/beneficiary seats: each administers one side of the boundary (the Court the justiciability and deference doctrines, the Knesset the amendment and override levers) and each collects authority in its own domains, so both derive low-to-moderate d — net beneficiaries of the arrangement's persistence, though each pays episodically when the other's lever prevails. The pure payer seats are rights-claimants in deferred domains and occupied-territories residents (trapped, powerless — d near the full-target end) and legislative minorities (constrained — high d, with episodic benefit through review). International treaty partners are beneficiaries at low d with mobile exit: they receive legislative constraint through obligations and pressure without bearing enforcement costs inside the order. The general public sits near symmetric — it receives both accountability and rights protection while paying uncertainty costs during crises. Gain receipt is genuinely domain-rotating: the Court collects interpretive authority in some domains, the coalition collects legislative authority in others, and no single seat captures the arrangement's gains — which is why gain_flow is authored as diffuse, an affirmative claim checked against every named seat, rather than naming either institution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling who decides what the Basic Laws mean when the institutions disagree — remains live, so the arrangement has not outlived its function and mandatrophy does not apply. The classification discipline matters in both directions here. Calling the arrangement a pure rope would erase the real, asymmetric costs that fall on trapped payer seats in deference domains; calling it a snare would erase the genuine coordination that keeps two legitimate constitutional claims from collapsing into a zero-sum monopoly, and the broad beneficiary class (including legislative minorities who depend on review for their protection) that the arrangement's operation serves. The tangled_rope claim holds both facts together: coordination through bounded mutual authority, asymmetric domain-rotating costs borne by identifiable payers, and persistence only through active enforcement that is now intensifying rather than atrophying. The measurement series show drift toward harder enforcement and more performative dialogue — the signature of a live contest hardening, not a dead mandate being theatrically maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'What would each sibling reading of the basic_law_interpretive_boundary kernel change structurally, and where exactly is the disagreement located?',
    'Structural comparison across the family: under judicial_supremacy_reading the Court captures the gains and the coalition, minorities, and deference-domain claimants pay (costs concentrate in the Court''s favor); under parliamentary_sovereignty_reading the coalition captures the gains and rights-claimants pay (costs concentrate in the Knesset''s favor). The disagreement is located at the finality question — whether any institution''s claim to final interpretive authority prevails — which no empirical finding alone can settle.',
    'If the polity resolves finality toward either sibling, this constraint''s costs collapse into a concentrated form accruing to a single seat, the classification shifts toward capture for whichever seat loses, and the balanced reading itself dissolves as a live arrangement rather than remaining a contested equilibrium.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Committer structure: this story is one reading of the basic_law_interpretive_boundary kernel; sibling readings concentrate costs on different loser seats.').

omega_variable(
    domain_varying_epsilon,
    'Does the scalar epsilon mask large domain variance — substantially higher costs in deference domains (security, immigration, planning, the occupied territories) and substantially lower in civil-rights domains?',
    'Domain-resolved docket analysis: classify the Court''s constitutional docket by policy domain and deference rate over the interval, and weight the arrangement''s cost profile by domain share.',
    'If deference domains dominate the docket, the effective cost borne by trapped payer classes is substantially higher than the scalar suggests and the arrangement drifts toward pure extraction for those classes; if civil-rights domains dominate, the coordination component is understated and the arrangement is closer to a workable bargain than the scalar indicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_varying_epsilon, empirical, 'Single-scalar epsilon versus domain-rotating cost structure.').

omega_variable(
    mutual_commitment_vs_power_balance,
    'Is the boundary held by genuine mutual constitutional commitment (a durable arrangement) or by a temporary balance of power that collapses when one side gains decisive strength?',
    'Counterfactual evidence from the 2023-2024 crisis: the boundary held through mass mobilization, military-reserve pressure, and international pressure rather than through either institution''s self-restraint. Observe whether mutual restraint re-emerges after the crisis without external pressure sustaining it.',
    'If power balance, the arrangement is a transitional equilibrium that a future decisive coalition collapses discontinuously; classification should weight the enforcement dependency heavily and treat persistence as contingent. If commitment, the arrangement is durable and the rising enforcement series reflects the crisis episode rather than structural fragility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_commitment_vs_power_balance, empirical, 'Durable mutual commitment versus contingent balance-of-power persistence.').

omega_variable(
    international_obligations_bindingness,
    'Are the international obligations constraining the Knesset justiciable and enforceable inside the constitutional order, or hortatory pressure the coalition can discount?',
    'Compliance-outcome tracking: whether treaty-body recommendations and foreign-government pressure change legislation and administrative practice, or are absorbed as costless criticism.',
    'If hortatory, the legislature''s boundedness is overstated and the arrangement drifts toward the parliamentary-sovereignty sibling; the balanced reading''s cost attribution to international constraint would need reweighting and the beneficiary position of international treaty partners would weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_obligations_bindingness, empirical, 'Real versus hortatory bindingness of the international-obligation constraint on the legislature.').

omega_variable(
    dialogue_era_idealization,
    'Was the pre-crisis period (1995-2020) a genuine mutual-restraint equilibrium, or a retrospective idealization of what was substantially Court dominance that the coalition''s backlash merely exposed?',
    'Re-read the era''s full record: override attempts, appointment fights, and governing-coalition responses to unpopular rulings across the whole period, not only the crisis years, and compare the Court''s win rate and doctrinal assertiveness against the dialogue narrative.',
    'If the era was Court dominance, the balanced reading''s reference frame is a normative aspiration rather than a description of a past state, and the current drift is a return to contest rather than a departure from balance — changing the drift_state interpretation, the persistence outlook, and the credibility of this reading against its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dialogue_era_idealization, conceptual, 'Whether the balanced reading''s reference frame describes a real historical equilibrium or an idealization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blib_balanced_contestation_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(blib_balanced_contestation_tr_t0, observed).
narrative_ontology:measurement(blib_balanced_contestation_tr_t5, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(blib_balanced_contestation_tr_t5, observed).
narrative_ontology:measurement(blib_balanced_contestation_tr_t10, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(blib_balanced_contestation_tr_t10, observed).
narrative_ontology:measurement(blib_balanced_contestation_tr_t15, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(blib_balanced_contestation_tr_t15, observed).
narrative_ontology:measurement(blib_balanced_contestation_tr_t20, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(blib_balanced_contestation_tr_t20, observed).
narrative_ontology:measurement(blib_balanced_contestation_tr_t25, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 25, 0.34).
narrative_ontology:measurement_basis(blib_balanced_contestation_tr_t25, observed).
narrative_ontology:measurement(blib_balanced_contestation_tr_t30, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(blib_balanced_contestation_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(blib_balanced_contestation_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(blib_balanced_contestation_be_t0, observed).
narrative_ontology:measurement(blib_balanced_contestation_be_t5, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement_basis(blib_balanced_contestation_be_t5, observed).
narrative_ontology:measurement(blib_balanced_contestation_be_t10, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(blib_balanced_contestation_be_t10, observed).
narrative_ontology:measurement(blib_balanced_contestation_be_t15, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement_basis(blib_balanced_contestation_be_t15, observed).
narrative_ontology:measurement(blib_balanced_contestation_be_t20, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement_basis(blib_balanced_contestation_be_t20, observed).
narrative_ontology:measurement(blib_balanced_contestation_be_t25, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(blib_balanced_contestation_be_t25, observed).
narrative_ontology:measurement(blib_balanced_contestation_be_t30, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(blib_balanced_contestation_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(blib_balanced_contestation_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(blib_balanced_contestation_su_t0, observed).
narrative_ontology:measurement(blib_balanced_contestation_su_t5, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 5, 0.24).
narrative_ontology:measurement_basis(blib_balanced_contestation_su_t5, observed).
narrative_ontology:measurement(blib_balanced_contestation_su_t10, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement_basis(blib_balanced_contestation_su_t10, observed).
narrative_ontology:measurement(blib_balanced_contestation_su_t15, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 15, 0.36).
narrative_ontology:measurement_basis(blib_balanced_contestation_su_t15, observed).
narrative_ontology:measurement(blib_balanced_contestation_su_t20, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(blib_balanced_contestation_su_t20, observed).
narrative_ontology:measurement(blib_balanced_contestation_su_t25, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 25, 0.56).
narrative_ontology:measurement_basis(blib_balanced_contestation_su_t25, observed).
narrative_ontology:measurement(blib_balanced_contestation_su_t30, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(blib_balanced_contestation_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who interprets the Basic Laws' covers three structurally distinct authority arrangements with different epsilon, different beneficiary structures, and different failure modes; per the epsilon-invariance principle they are authored as separate constraint stories in one family. This story's epsilon (0.58) reflects the balanced arrangement's domain-rotating costs borne by trapped payer seats; judicial_supremacy_reading concentrates costs on the legislative and payer seats; parliamentary_sovereignty_reading concentrates them on rights-claimants. The upstream/downstream structure runs through this reading: the balanced arrangement's dialogic practice is the pathway through which both siblings advance incrementally, so this story links to both siblings via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
