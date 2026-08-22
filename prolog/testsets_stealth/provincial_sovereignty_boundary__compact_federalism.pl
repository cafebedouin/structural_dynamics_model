% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Compact Federalism Sovereignty Boundary (Provincial-Consent Reading)
 *   domain: political/constitutional/economic
 *
 * SUMMARY:
 *   Canadian federalism is organized around a contested commitment: where
 *   does provincial sovereignty end and federal authority begin? This story
 *   instantiates one reading of that kernel — compact federalism, the claim
 *   that Confederation was a compact among sovereign provinces, that
 *   provinces retain residual sovereignty, that federal authority is
 *   legitimate only where provincial consent extends to it, that equalization
 *   is negotiable rather than owed, that climate policy is subject to
 *   provincial override, and that exit requires negotiation rather than
 *   permission. As an operating arrangement the reading is a hybrid: the
 *   federation it constitutes solves real collective-action problems (defense
 *   pooling, a common currency and tariff wall, fiscal risk-pooling across
 *   regional cycles, jurisdictional quarantine of regionally divergent
 *   policy), and the same structure moves real costs onto parties who never
 *   consented to it — Indigenous nations whose sovereignty the compact
 *   presupposes away, businesses and consumers paying provincially erected
 *   trade barriers, net-payer provinces contributing to a transfer formula
 *   they contest, and exit-aspiring provinces bound to negotiate under
 *   duress. The claim and the metrics are authored independently: the claimed
 *   type states what I believe is structurally true of the arrangement; the
 *   metrics state what I believe is descriptively true of its operation
 *   across the 159-year interval. Where the engine's per-seat computations
 *   diverge from the claim, that divergence is the measurement the corpus
 *   exists to take.
 *
 * KEY AGENTS:
 *   - provincial_governments: Primary agenda-setter and beneficiary (institutional/constrained) — administer the reserved jurisdictions, assert the consent conditions, and are bound by the compact they invoke
 *   - federal_government: Dual agenda-setter/beneficiary (institutional/arbitrage) — administers transfers and the exit frame, pays authority where consent binds, routes around provincial vetoes through the spending power
 *   - equalization_recipient_provinces: Beneficiary (moderate/trapped) — net receivers of the fiscal pool, dependent on its persistence
 *   - equalization_net_payer_provinces: Payer (powerful/constrained) — net contributors contesting the formula without unilateral withholding power
 *   - indigenous_nations: Primary victim and excluded party (powerless/trapped) — sovereignty divided without consent, absent from the bargaining table
 *   - interprovincial_trade_barrier_payers: Victim (moderate/constrained) — bear the cost of provincially erected market barriers
 *   - secession_ambition_provinces: Victim (organized/trapped) — exit aspirants bound to negotiate under duress
 *   - supreme_court_canada: Analytical observer (institutional/analytical) — adjudicates the boundary and drives its drift without formal amendment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.62).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.6).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Compact Federalism Sovereignty Boundary (Provincial-Consent Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political/constitutional/economic").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, 'd4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0').
narrative_ontology:cs_kernel_codification('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0', fixed_text).
narrative_ontology:cs_authority_grounding('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0', lineage).
narrative_ontology:cs_interpretation_layer_present('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0').
narrative_ontology:cs_reading_relation('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0', foundational, confederation_was_compact_of_sovereign_provinces).
narrative_ontology:cs_axiom_status(confederation_was_compact_of_sovereign_provinces, holdable).
narrative_ontology:cs_axiom_grounding('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0', confederation_was_compact_of_sovereign_provinces, empirically_contingent).
narrative_ontology:cs_axiom('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0', foundational, federal_authority_conditional_on_provincial_consent).
narrative_ontology:cs_axiom_status(federal_authority_conditional_on_provincial_consent, holdable).
narrative_ontology:cs_axiom_grounding('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0', federal_authority_conditional_on_provincial_consent, deontological).
narrative_ontology:cs_reference_frame('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0', compact_of_sovereign_provinces).
narrative_ontology:cs_drift_state('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0', contemporary, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('d4b81be2-33cf-44a5-b2f0-fa2f0b6edfe0', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, equalization_recipient_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, indigenous_nations).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, interprovincial_trade_barrier_payers).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, secession_ambition_provinces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, equalization_net_payer_provinces).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, compact_theory_of_confederation).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, provincial_residual_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, duty_to_negotiate_secession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ten provincial governments administer the domains the 1867 bargain assigned to local jurisdiction — natural resources, health, education, civil law — and negotiate as a class with the federal government over fiscal transfers and policy implementation. They assert the compact reading through premiers' conferences, court reference questions, override legislation, and, recently, sovereignty statutes declaring federal programs inoperative within their borders. They cannot unilaterally leave the arrangement they invoke: their revenue base, debt instruments, and trade access all run through it. What flows to them is jurisdictional shielding and bargaining leverage; what binds them is the same compact's requirement of negotiated resolution.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provincial_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, provincial_governments, beneficiary).

% Administers the federation: national taxation, the equalization formula, defense, currency, and the reference power to test the boundary in court. It converts provincial sovereignty claims into negotiation leverage and holds the intermediation role between regions. It can route around provincial vetoes through the spending power, criminal-law backstops, and declaratory authority, and it benefits from the negotiated-exit framing that channels secession aspiration into talks its instruments dominate. It pays governing authority in the domains where provincial consent effectively conditions action.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, federal_government, beneficiary).

% Provinces whose fiscal capacity falls below the national average receive unconditional transfers through the equalization formula, underwriting public services at near-national standards without above-norm tax effort. Their budgets are structured around the transfers; a formula that reduced them would force immediate service cuts or tax increases. They defend the program's persistence while formally agreeing, under the compact reading, that its terms are a matter for negotiation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, equalization_recipient_provinces, beneficiary,
    moderate, generational, trapped, regional).

% Provinces above the national fiscal-capacity standard contribute disproportionately through federal taxation that funds the transfer system. Alberta has run sustained campaigns against the formula and its legislature has passed sovereignty legislation invoking provincial authority against federal programs. It cannot withhold its contribution unilaterally — the money moves through federal taxation — but it can raise the political price of the arrangement through resource leverage, referendum campaigns, and brinkmanship.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, equalization_net_payer_provinces, payer,
    powerful, biographical, constrained, regional).

% First Nations, Inuit, and Métis governments hold sovereignty claims that predate the 1867 bargain, and were party to none of its founding conferences. The compact's division of powers placed 'Indians and lands reserved' under federal jurisdiction and provincial laws of general application over their territories without their consent. They bear the governing costs of a two-order sovereignty system that administers their lands, resources, and members, while their own jurisdictional claims proceed case-by-case through litigation and consultation duties. They are absent from the federal-provincial tables where the boundary is renegotiated in practice.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, indigenous_nations, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, indigenous_nations, excluded).

% Businesses and consumers moving goods, services, credentials, and alcohol across provincial lines pay the price of provincially erected barriers — procurement preferences, professional certification walls, marketing-board regimes, direct-delivery restrictions — each justified as an exercise of provincial jurisdiction. The aggregate cost is a meaningful share of national productivity. Individual firms can relocate or restructure at cost; the payers as a class cannot exit the national market that the barriers fragment.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, interprovincial_trade_barrier_payers, payer,
    moderate, biographical, constrained, national).

% Provinces and provincial movements that have sought to leave — Quebec through two referendums, Alberta through separation polling and sovereignty legislation — confront an exit that is negotiable rather than exercisable. Any negotiation would proceed under conditions of currency union, debt division, border and trade access, and Indigenous treaty rights, all held in the federation's hands. The 1998 Supreme Court reference confirmed a duty to negotiate following a clear referendum majority while ruling unilateral departure unconstitutional. The aspiration is organized, funded, and electorally real; the exit is conditional on terms its seekers do not set.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, secession_ambition_provinces, payer,
    organized, generational, trapped, regional).

% Adjudicates the boundary. It broadened provincial property-and-civil-rights jurisdiction in the early era, re-centered federal authority in the modern era, ruled the compact theory not law while affirming a duty to negotiate departure, and upheld the federal carbon-pricing backstop against provincial objection. Its reference answers convert political contest into legal settlement and are the principal mechanism through which the boundary moves without formal amendment.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, supreme_court_canada, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools defense, debt service, currency, tariff policy, and intercolonial infrastructure among formerly separate colonies; allocates jurisdiction so that regionally divergent domains (resources, education, health delivery, civil law) remain locally governed; risk-pools regional fiscal capacity through federal transfers. Solves the collective-action problems of small adjacent economies that individually could not finance a railway or resist absorption by the United States.
% TRANSFER_FUNCTION: Moves fiscal capacity from above-average to below-average provinces through federal taxation and equalization; moves policy authority between orders of government — toward the provinces when consent conditions and override statutes bite, toward the federal government when spending power and criminal-law backstops bite; and moves exit-leverage to the federal seat, since any departing province must convert its aspiration into negotiated terms on currency, debt, borders, and trade.
% ABSENT_VOICES: Indigenous nations were absent from every founding bargain and remain outside the federal-provincial tables where the boundary is renegotiated; consent to the sovereignty division was never sought from them. Individual citizens consent only through provincial governments claiming to speak for 'sovereign' provinces — no direct ratification of compact terms exists. Municipalities, as creatures of the provinces, are doubly absent. Each would object to terms negotiated over them: Indigenous nations to the sovereignty division itself, citizens to barriers and transfers they cannot vote on directly.
% DISAPPEARANCE_RATIONALE: If the consent conditions, the negotiable-equalization norm, the override capacity, and the negotiated-exit rule all dissolved overnight, the federation would reorganize within years: either toward a subordination model in which federal authority proceeds unconditioned (internal trade liberalized by federal power, climate and resource policy imposed federally, exit treated as permission-requiring), or toward fragmentation as provinces treat jurisdiction as fully theirs and exit as exercisable. Fiscal transfers, climate implementation, and the secession question are all organized around this boundary; none would hold its current shape.
% FOUNDING_PROBLEM: The 1860s problems of the British North American colonies: legislative deadlock in the united Province of Canada, defense against United States expansionism after the Civil War, financing an intercolonial railway no single colony could fund, and the loss of reciprocal trade with the United States — addressed by pooling debt, defense, and infrastructure while quarantining local identity and jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: The founding problems themselves are corroborated by non-beneficiary historiography: the standard accounts of Confederation (defense anxiety, railway finance, legislative deadlock) rest on the documentary record of the 1860s conferences, not on the beneficiaries' testimony. On status, corroboration splits — regional-economics scholarship outside the benefiting governments attests that regional fiscal disparity and continental-integration management remain live problems, while secessionist and centralist scholars alike attest that the original problems are dead and the arrangement persists on successor functions. No single outside source attests the compact's continuing necessity in its 1867 terms.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.62 at interval end) because the arrangement's operation moves real value between identifiable parties: fiscal transfers from net payers, market access from trade-barrier payers, governing authority from Indigenous nations, and negotiated terms from exit aspirants — while its coordination function (defense pooling, currency, risk-pooling, jurisdictional quarantine of regional divergence) is genuine and continuously used. Suppression (0.60) is structural: the arrangement is not self-executing — it is held up by reference litigation, override statutes, formula entrenchment, and the duress structure of exit negotiation; provinces retain real counter-leverage (resources, referendums, the notwithstanding clause), so suppression is high but not total. Suppression is authored as a raw structural property; the engine, not the author, scales extractiveness by directionality and scope. Theater (0.44) is moderate: compact rhetoric — 'national unity', 'federal-provincial respect', sovereignty statutes with no justiciable effect — is a large share of the arrangement's activity, but core functions (equalization flows, jurisdiction administration, negotiated accords) are real. Accessibility collapse (0.45) is low-moderate: the alternative readings are visible and partly live (the courts have operated the subordination reading for decades; resource primacy is constitutionally texted; exit referendums occur), so alternatives do not fully collapse. Resistance (0.78) is very high: this boundary is the most continuously contested structure in Canadian politics — two referendums on exit, sovereignty statutes, equalization-referendum campaigns, decades of first ministers' conferences. The measurement series run on one shared nine-point grid, all three metrics authored at every point. The series oscillate rather than drift monotonically: extraction and suppression dip at the judicial-provincial-rights zenith (~1927) and the post-referendum accommodation era (~1998-2007), and peak at centralization episodes (war and welfare-state expansion, the 1982 patriation, the 2021 carbon-pricing backstop). The cycle driver is alternating federal capacity and provincial assertion, with accommodation following near-breaks; the oscillation is partly an extraction mechanism — each accommodation resets the baseline slightly higher (a ratchet-with-release pattern, visible in the rising envelope of all three series). Suppression_requirement is tracked deliberately: the story's enforcement machinery genuinely changes across the interval — disallowance era, spending-power era, judicial-reconciliation era, override era.
 *
 * PERSPECTIVAL GAP:
 *   The federal seat experiences the boundary as something it both administers and routes around: its arbitrage-grade exit (spending power, criminal-law backstops, declaratory authority) means the consent conditions bind it less than they bind the provinces that invoke them. The provincial seats experience the same structure as both shield and cage — jurisdictional protection in reserved domains, compulsory negotiation in contested ones. The Indigenous seat experiences only the cage: the boundary was drawn over them without consent, so their position carries near-maximal cost-bearing with no offsetting coordination benefit. The trade-barrier payers experience diffuse cost with no seat at any table. Same-level dynamics matter: the federal and provincial seats hold equal formal constitutional standing, yet power differs because the boundary's enforcement instruments are asymmetrically distributed — the federal seat holds routing tools the provinces lack, while provinces hold territorial and resource leverage the federal seat cannot replicate. The victim seats are not natural coalition partners — Indigenous nations, secessionist provinces, and trade-barrier payers hold opposed positions on exit — which is part of what keeps the arrangement's cost-bearing stable despite diffuse discontent.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary and victim declarations drive the engine's per-seat derivation, and no override was needed because the structural data differentiates the seats. provincial_governments and federal_government sit near the beneficiary end — they administer and draw on the arrangement — with the federal seat pushed up from the pure-beneficiary position by the authority it pays in consent-conditional domains (captured by its secondary role and arbitrage exit rather than by an override, since an override keyed to the institutional power atom could not distinguish the two governments). equalization_recipient_provinces sit near the beneficiary end; trapped exit amplifies their stake in the arrangement's persistence. equalization_net_payer_provinces sit near the target end: net contributors with no unilateral withholding. indigenous_nations sit at the target end — trapped exit, no declared benefit, costs landing without offset. interprovincial_trade_barrier_payers sit near the target end as diffuse, weakly organized cost-bearers. secession_ambition_provinces sit at the target end: the duress conditions of any exit negotiation are the cost they bear. Larger spatial scope (national) modestly amplifies effective extraction through verification difficulty; the regional seats carry regional scope accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 1860s defense, railway finance, legislative deadlock — is dead; the arrangement persists on successor functions (fiscal risk-pooling, jurisdictional shields, exit management). Authoring founding_problem_status as contested rather than dead keeps the mismatch consumer honest: this is not a zombie maintained purely theatrically. It is actively maintained by governments with concentrated stakes — theater_ratio is 0.44, not the 0.6+ of performative maintenance — and its enforcement machinery is real (suppression 0.60, with a live override-era ratchet). The tangled_rope claim prevents both mislabels: reading the arrangement as pure coordination would erase the Indigenous, secessionist, and trade-barrier victim seats; reading it as pure extraction would erase the coordination function that ten governments and the federal seat draw on daily and that its targets partially defend. The mandate has transformed rather than atrophied: the constraint's persistence is explained by live, concentrated beneficiary stakes, not inertia, and no seat both administers it and could cheaply replace it — fixing it requires formal constitutional amendment under rules the arrangement itself controls, which is why fixing_cost is prohibitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_nature_historical_question,
    'Was Confederation in fact a compact among pre-existing sovereign provinces, as this reading holds, or a legislative act of the Imperial Parliament constituting subordinate provinces, as the subordination reading holds?',
    'Historical-legal analysis of the 1860s conference records, colonial statutes, and early Privy Council jurisprudence; answerable in principle from the documentary record, though contested for a century and a half and formally rejected by the Supreme Court in the 1998 Secession Reference.',
    'If the subordination reading is historically correct, this constraint''s consent conditions lack their claimed foundation and the boundary collapses toward federally-defined limits; if the compact reading is correct, provincial consent claims carry constitutional force the current legal order denies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_nature_historical_question, empirical, 'Historical dispute over the founding nature of the federation underlying the consent conditions.').

omega_variable(
    indigenous_consent_defect,
    'Can the compact''s consent-based legitimacy survive the absence of Indigenous consent — the ''compact among sovereign provinces'' presupposes Crown sovereignty acquired over territories and peoples who were party to no bargain?',
    'Section 35 jurisprudence, treaty-implementation litigation, and UNDRIP-consistent legislation; the structural question is whether the legal order can reconcile the two-order compact with third-order Indigenous jurisdiction without renegotiating the compact''s parties.',
    'If the defect is recognized as structural rather than curable by consultation duties, the constraint''s victim set expands and its coordination legitimacy drops on the Indigenous-facing face of the boundary — cost-bearing there approaches the pure end while the provincial-federal face remains hybrid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_consent_defect, conceptual, 'Whether the compact reading''s consent principle is self-undermining given the parties it excludes.').

omega_variable(
    exit_duress_structural,
    'Is the duress under which exit negotiations would occur eliminable — could a province negotiate departure from a position of genuine choice — or structural, such that ''negotiable exit'' always operates as binding under conditions the departing province does not set?',
    'Scenario modeling of secession negotiations (currency, debt division, trade access, Indigenous treaties) against comparable cases (Scotland 2014, Brexit); fiscal analysis of whether any negotiation path exists that leaves the departing province''s terms meaningfully open.',
    'If the duress is structural, the negotiable-exit provision functions as a binding mechanism rather than a fair process — the secession seat''s cost-bearing rises toward the pure end and the coordination story weakens on that face; if eliminable in principle, the provision is genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_duress_structural, empirical, 'Whether the exit-negotiation duress is contingent or structural.').

omega_variable(
    equalization_negotiability_gap,
    'Is equalization in fact negotiable, as the reading holds, or entrenched — does the formula auto-renew with only episodic renegotiation, such that net payers'' ''negotiability'' is formal rather than real?',
    'Track formula renegotiation episodes (the 1982 entrenchment commitment, the 2007 and 2018 changes) against renewal-without-renegotiation intervals; measure net-payer political pressure against actual formula movement.',
    'If entrenched, the reading''s negotiability claim is performative and the fiscal transfer from net payers operates without their consent — raising measured cost-bearing on the payer seat; if genuinely negotiable, the coordination framing strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equalization_negotiability_gap, empirical, 'Whether equalization is actually renegotiable or entrenched in practice.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel of this commitment system the constitutional text (as authored here: fixed_text, lineage authority), or the fiscal-transfer architecture with sovereignty language as its legitimation — and does the alternative framing change the classification?',
    'Test which framing better predicts the boundary''s actual movement: if the boundary moves with fiscal renegotiations rather than textual interpretation, the fiscal-architecture framing is the better kernel description.',
    'Under the fiscal-architecture framing, the arrangement reads as a resource-allocation mechanism whose sovereignty contestation is cover — the coordination function strengthens while the consent-based legitimacy claims lose their grounding, and classification would weight the fiscal cost streams more heavily.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative kernel framings (text versus fiscal architecture) under-determine the commitment-system classification.').

omega_variable(
    provincial_identity_lock,
    'Is provincial resistance to federal policy structural (constitutional tools, fiscal leverage) or internalized (regional identities — Québécois distinctness, Western alienation — that make compromise feel like betrayal), and in what proportion?',
    'Post-referendum and post-sovereignty-act attitude trajectories: if resistance persists when structural tools are withdrawn or fail, the internalized component is substantial.',
    'If substantially internalized, the boundary''s suppressive force persists beyond its structural instruments — suppression measured on structural tools alone understates what the arrangement''s targets experience, and provincial exit options are more identity-locked than the legal position suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provincial_identity_lock, empirical, 'Structural versus internalized component of provincial resistance to the federal seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 0, 159).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(prov_tr_t0, observed).
narrative_ontology:measurement(prov_tr_t20, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(prov_tr_t20, observed).
narrative_ontology:measurement(prov_tr_t40, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(prov_tr_t40, observed).
narrative_ontology:measurement(prov_tr_t60, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(prov_tr_t60, observed).
narrative_ontology:measurement(prov_tr_t80, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 80, 0.35).
narrative_ontology:measurement_basis(prov_tr_t80, observed).
narrative_ontology:measurement(prov_tr_t100, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(prov_tr_t100, observed).
narrative_ontology:measurement(prov_tr_t120, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 120, 0.45).
narrative_ontology:measurement_basis(prov_tr_t120, observed).
narrative_ontology:measurement(prov_tr_t140, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 140, 0.4).
narrative_ontology:measurement_basis(prov_tr_t140, observed).
narrative_ontology:measurement(prov_tr_t159, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 159, 0.44).
narrative_ontology:measurement_basis(prov_tr_t159, observed).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(prov_be_t0, observed).
narrative_ontology:measurement(prov_be_t20, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(prov_be_t20, observed).
narrative_ontology:measurement(prov_be_t40, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(prov_be_t40, observed).
narrative_ontology:measurement(prov_be_t60, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 60, 0.5).
narrative_ontology:measurement_basis(prov_be_t60, observed).
narrative_ontology:measurement(prov_be_t80, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 80, 0.58).
narrative_ontology:measurement_basis(prov_be_t80, observed).
narrative_ontology:measurement(prov_be_t100, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 100, 0.6).
narrative_ontology:measurement_basis(prov_be_t100, observed).
narrative_ontology:measurement(prov_be_t120, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 120, 0.63).
narrative_ontology:measurement_basis(prov_be_t120, observed).
narrative_ontology:measurement(prov_be_t140, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 140, 0.57).
narrative_ontology:measurement_basis(prov_be_t140, observed).
narrative_ontology:measurement(prov_be_t159, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 159, 0.62).
narrative_ontology:measurement_basis(prov_be_t159, observed).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(prov_su_t0, observed).
narrative_ontology:measurement(prov_su_t20, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(prov_su_t20, observed).
narrative_ontology:measurement(prov_su_t40, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(prov_su_t40, observed).
narrative_ontology:measurement(prov_su_t60, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 60, 0.45).
narrative_ontology:measurement_basis(prov_su_t60, observed).
narrative_ontology:measurement(prov_su_t80, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 80, 0.5).
narrative_ontology:measurement_basis(prov_su_t80, observed).
narrative_ontology:measurement(prov_su_t100, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 100, 0.55).
narrative_ontology:measurement_basis(prov_su_t100, observed).
narrative_ontology:measurement(prov_su_t120, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 120, 0.65).
narrative_ontology:measurement_basis(prov_su_t120, observed).
narrative_ontology:measurement(prov_su_t140, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 140, 0.52).
narrative_ontology:measurement_basis(prov_su_t140, observed).
narrative_ontology:measurement(prov_su_t159, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 159, 0.6).
narrative_ontology:measurement_basis(prov_su_t159, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, resource_allocation).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sovereignty question' covers three structurally distinct claims about one kernel, decomposed per the epsilon-invariance principle. This file is the compact reading only: consent-conditional federal authority with negotiable equalization and negotiated exit. constitutional_subordination instantiates the creature-of-statute reading — different victim set (provinces as rights-bearers against federal overreach rather than Indigenous nations and exit aspirants) and different extractiveness. resource_sovereignty_primacy instantiates the s.92A ownership reading — victim set concentrated on federal climate policy and interprovincial infrastructure. The compact reading is upstream of the resource reading historically (the provincial-power jurisprudence the compact tradition built is the operating terrain of the ownership claim — hence the influences edge), and the compact and subordination readings foreclose each other as descriptions of the founding: no single framework can hold both 'provinces entered as sovereigns' and 'provinces were constituted as subordinates'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
