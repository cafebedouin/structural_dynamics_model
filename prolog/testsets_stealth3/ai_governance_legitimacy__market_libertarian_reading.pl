% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: Market-Libertarian Reading of AI Governance Legitimacy
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint is the market-libertarian READING of the contested kernel
 *   'AI governance legitimacy': the standing arrangement it instantiates is
 *   one in which legitimacy for governing AI development flows exclusively
 *   through voluntary exchange and pre-political property rights, collective
 *   mandates are defined as illegitimate coercion, dignity is protected
 *   through exit options and competitive markets, and enforcement runs
 *   through contract law, private arbitration, and reputational mechanisms.
 *   The reading partially absorbs the encyclical's subsidiarity principle
 *   (decentralization is endorsed) while categorically rejecting its
 *   solidarity demands. Assessed by the reading's own lights, the arrangement
 *   is largely consensual coordination with modest inherent friction — hence
 *   low authored epsilon (0.25), NOT the high epsilon a rival reading would
 *   author over the same referent. KEY AGENTS (by structural relationship): -
 *   ai_platform_operating_firms: de facto agenda setter
 *   ([institutional]/[arbitrage]) — drafts the terms constituting the
 *   governance layer and collects the surplus -
 *   ai_founders_and_entrepreneurs: primary beneficiary
 *   ([powerful]/[arbitrage]) - venture_capital_investors: primary beneficiary
 *   ([institutional]/[arbitrage]) - high_autonomy_professionals: beneficiary
 *   whose exit-dignity works as promised ([organized]/[mobile]) -
 *   monopsony_workers: primary target ([powerless]/[trapped]) -
 *   data_subjects_without_bargaining_power: target
 *   ([powerless]/[constrained]) - externality_bearing_communities: target
 *   ([moderate]/[constrained]) - third_parties_to_deployment: excluded voice
 *   ([powerless]/[trapped]) — affected without ever contracting -
 *   comparative_governance_scholars: analytical observer — sees the full
 *   four-reading structure. The claim/metric gap is deliberate: the reading
 *   CLAIMS mountain (property rights as pre-political, emerging naturally)
 *   while the authored metrics describe a defended, actively resisted
 *   construct with accumulating extraction at its consent-degraded margins —
 *   the engine measures that divergence; do not reconcile the claim to the
 *   metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.25).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.5).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "Market-Libertarian Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__market_libertarian_reading).
domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '92a5f183-11e3-4d1a-bacb-61e64c345ac6').
narrative_ontology:cs_kernel_codification('92a5f183-11e3-4d1a-bacb-61e64c345ac6', distributed).
narrative_ontology:cs_authority_grounding('92a5f183-11e3-4d1a-bacb-61e64c345ac6', lineage).
narrative_ontology:cs_interpretation_layer_present('92a5f183-11e3-4d1a-bacb-61e64c345ac6').
narrative_ontology:cs_reading_relation('92a5f183-11e3-4d1a-bacb-61e64c345ac6', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('92a5f183-11e3-4d1a-bacb-61e64c345ac6', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('92a5f183-11e3-4d1a-bacb-61e64c345ac6', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_axiom('92a5f183-11e3-4d1a-bacb-61e64c345ac6', foundational, consent_sufficiency_of_legitimacy).
narrative_ontology:cs_axiom_status(consent_sufficiency_of_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('92a5f183-11e3-4d1a-bacb-61e64c345ac6', consent_sufficiency_of_legitimacy, deontological).
narrative_ontology:cs_axiom('92a5f183-11e3-4d1a-bacb-61e64c345ac6', secondary, dispersed_discovery_beats_central_mandate).
narrative_ontology:cs_axiom_status(dispersed_discovery_beats_central_mandate, holdable).
narrative_ontology:cs_axiom_grounding('92a5f183-11e3-4d1a-bacb-61e64c345ac6', dispersed_discovery_beats_central_mandate, empirically_contingent).
narrative_ontology:cs_reference_frame('92a5f183-11e3-4d1a-bacb-61e64c345ac6', prepolitical_voluntary_exchange_order).
narrative_ontology:cs_drift_state('92a5f183-11e3-4d1a-bacb-61e64c345ac6', contemporary_regulatory_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('92a5f183-11e3-4d1a-bacb-61e64c345ac6', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, ai_founders_and_entrepreneurs).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, venture_capital_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_professionals).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, monopsony_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, data_subjects_without_bargaining_power).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, externality_bearing_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, ai_platform_operating_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft the terms of service, API licenses, and employment agreements that constitute the de facto governance layer for AI development and deployment. Enforce the voluntarist boundary through contract enforcement, mandatory arbitration clauses, account and access termination, and funded litigation against regulatory overreach. Collect the surplus that flows from setting the terms others must accept.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, ai_platform_operating_firms, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__market_libertarian_reading, ai_platform_operating_firms, beneficiary).

% Build and scale AI ventures under minimal collective-mandate overhead: fast deployment cycles, freedom to define product terms bilaterally, and no obligation to secure prior collective approval. Can restructure incorporation, relocate operations, or recharacterize assets across jurisdictions if any single regulator closes in.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, ai_founders_and_entrepreneurs, beneficiary,
    powerful, biographical, arbitrage, global).

% Allocate capital on the expectation that deployed systems will not face ex-post mandate clawbacks, revenue caps, or compulsory sharing requirements. Portfolio returns depend on the voluntarist boundary holding; they defend it through limited-partner influence, policy advocacy, and jurisdiction selection for fund domicile.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, venture_capital_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Skilled engineers, researchers, and executives whose scarcity gives them individual bargaining power. Negotiate compensation and working conditions contract by contract, move freely between employers and countries, and experience exit as a fully adequate protection — the dignity-through-exit mechanism works as advertised from where they stand.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_professionals, beneficiary,
    organized, biographical, mobile, global).

% Perform annotation, content moderation, data-labeling, and platform-dependent gig work for dominant buyers of their labor. Face few alternative employers offering comparable wages, sign arbitration clauses that foreclose collective legal recourse, and find that the exit the framework promises as dignity-protection is nominal: leaving means income loss without comparable alternatives.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, monopsony_workers, payer,
    powerless, biographical, trapped, global).

% Supply the behavioral data and engagement that trains and monetizes AI systems under clickwrap consent — formally voluntary, practically take-it-or-leave-it. Cannot negotiate terms individually, cannot price their contribution, and find that opting out of the digital infrastructure carries escalating costs in employment, banking, and social participation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, data_subjects_without_bargaining_power, payer,
    powerless, biographical, constrained, global).

% Host data centers and energy loads, absorb labor-market displacement, and carry the diffuse costs of recommendation and moderation systems deployed into their information environments. Within this framework their remedy is limited to persuading firms to mitigate voluntarily, because collective mandates are precisely what the framework defines as illegitimate; their exit options are relocation at generational cost.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, externality_bearing_communities, payer,
    moderate, generational, constrained, regional).

% People scored, moderated, filtered, priced, or ranked by deployed systems who were never party to any contract governing them: job applicants ranked by hiring models, borrowers scored by lending systems, citizens shaped by recommendation infrastructures. No consent mechanism reaches them; their interests register only when translated into a property interest someone will pay for.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, third_parties_to_deployment, excluded,
    powerless, generational, trapped, global).

% Political theologians, legal scholars, and political economists who map the competing legitimacy sources for AI governance — consent, magisterial authority, technical performance, democratic deliberation — and trace how each distributes costs and protections across the seats above. Hold no stake in the arrangement and can see the full structure.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, comparative_governance_scholars, observer,
    analytical, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI innovation and investment across millions of dispersed actors without requiring prior agreement on a common conception of the good: property rights let actors commit resources, prices and contracts transmit dispersed knowledge, and bilateral exchange plus private dispute resolution substitutes for centralized authorization.
% TRANSFER_FUNCTION: Moves decision authority over AI development from collective and political bodies to owners of capital, code, and compute; moves the returns from deployed systems to founders, investors, and platform operators; moves deployment risks and adjustment costs onto workers, data subjects, and third parties who lack the bargaining position to refuse.
% ABSENT_VOICES: Third parties to deployment have no seat — they never signed anything and so have no standing the framework recognizes. Communities hosting infrastructure and bearing externalities appear only as prospective counterparties, not as constituencies. The encyclical's solidarity constituency — the vulnerable whose protection the tradition says requires binding mandates — is present only as a rejected interlocutor. Their interests enter the system solely when capitalized into someone's property interest.
% DISAPPEARANCE_RATIONALE: If the voluntarist-legitimacy arrangement vanished overnight — if legitimacy were relocated to mandates or deliberation — the entire AI investment and legal stack would lose its authorization logic: IP and licensing regimes, venture fund structures, terms-of-service governance, arbitration-based dispute resolution, and jurisdictional arbitrage strategies all presuppose that bilateral consent confers legitimacy. Capital would repricing en masse, deployment would stall pending new authorization forms, and the AI economy would reorganize around whatever replaced consent as the legitimacy source.
% FOUNDING_PROBLEM: How to legitimate rapid technological development in religiously and morally pluralistic societies without requiring prior consensus on the good — protecting innovation simultaneously against state planning, sectarian definition of the common good, and majoritarian mandates, by grounding legitimacy in the one act available to everyone regardless of worldview: voluntary exchange.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set on the problem's reality: the liberal-neutralist political philosophy tradition independently attests that pluralism poses a genuine legitimation problem for technological governance. Contested on adequacy from outside as well: Catholic social teaching and public-interest technology scholarship attest that categorically excluding collective mandates leaves the present governance-gap problems unsolved. No source outside the benefiting parties attests that the founding problem is settled by this arrangement; the dispute over its status is itself externally documented.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.25) because the reading's own lights certify the core exchange structure as consensual; the residual reflects what even a committed adherent must concede — degraded consent at the monopsony margin, third-party costs no contract reached, and the inherent cost of the enforcement infrastructure itself (contract litigation, arbitration administration, reputation systems). Suppression (0.50, a raw structural property — the engine scales only extractiveness by directionality and scope) captures that the arrangement actively forecloses alternative governance forms: collective mandates are not merely outcompeted but defined as illegitimate, and the boundary is maintained by real machinery — litigation against regulatory overreach, arbitration-clause expansion, jurisdiction-shopping, and policy advocacy — hence requires_active_enforcement is declared true even under the mountain claim. Theater (0.30) tracks the widening gap between exit-as-dignity rhetoric and actual exit availability for the bottom half of the bargaining distribution; the activity is mostly functional (genuine contracting, genuine innovation) but the dignity guarantee is increasingly performed rather than delivered. Accessibility_collapse (0.65): within the reading's framework, once the pre-political-rights premise is granted, mandate-based alternatives collapse almost entirely — but rival frameworks remain fully live in the world, so collapse is far short of the 0.85+ of a genuine natural law. Resistance (0.70): the constraint meets sustained, organized resistance — the encyclical tradition itself, legislative movements toward AI mandates, labor organizing against arbitration clauses — which is precisely the profile of a claimed summit that must be defended rather than a law that simply holds. Temporal series share one grid (t=0,3,6,9,12,15) across all tracked metrics; extraction creeps up slowly (T17 will register the accumulation as a hypothesis, not a reclassification) while suppression requirement climbs faster, modeling the enforcement ratchet as mandate attempts grew. Receipt surface: gains demonstrably accrue to the seat that drafts and enforces the terms — gain_flow names ai_platform_operating_firms. Fixing is prohibitive for whoever could fix it: the seats with the capacity (operators, capital holders) would bear concentrated costs for diffuse benefits, and no seat faces a favorable fix ratio.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the founder, investor, and high-autonomy professional seats, the arrangement is experienced as protective coordination: consent, mobility, and exit deliver exactly the dignity the reading promises, and effective extraction lands near zero or negative (subsidy). From the monopsony worker and data-subject seats, the same structure operates as degraded-consent extraction: formally voluntary transactions whose practical refusal costs are ruinous, with the framework's own dignity mechanism (exit) failing precisely for them. From the community and third-party seats, it operates as unpriced imposition — costs arriving through channels no consent instrument governs. The engine derives these per-seat classifications from the power, exit, and role data; the authored mountain claim belongs to the reading's beneficiary-side vantage and does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the d-near-zero end: founders, investors, and autonomous professionals all collect from the arrangement and hold arbitrage-grade or mobile exits, pushing their derived directionality hard toward full-beneficiary. Targets cluster at the d-near-one end: monopsony workers (trapped — the worst exit position the framework offers anyone) derive the highest d; data subjects (constrained) and externality-bearing communities (constrained, moderate power, generational horizon) sit slightly lower but firmly target-side. The platform-operator seat is agenda_setter with secondary beneficiary: it both administers the boundary and collects the receipts, deriving low d with amplified effective weight from institutional power at global scope. The excluded third parties feed no derivation — they are commentary-grade absence (R3), the structural proof that unanimity here arises partly because the worst-off were never in the room.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimating innovation amid pluralism) is contested rather than dead, so no mandatrophy/zombie declaration is authored and none should fire: the arrangement has not outlived its function so much as its function has become disputed. The classification guards against both symmetrical errors. Mislabeling this as pure extraction would erase the genuine coordination function — consent-based legitimation really does solve a real pluralism problem, really does mobilize dispersed knowledge, and really does protect dignity for the seats with actual exit; flattening that into a snare would discard the reading's true insight. Accepting the mountain claim uncritically would erase the accumulating extraction the temporal record shows — creeping epsilon growth at the consent-degraded margins and a hardening enforcement ratchet are the signature of a defended construct, not a natural law. The piton path is also guarded: theater is present (0.30 and rising) but the coordination function remains substantially live, so theatricality here is symptom, not diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prepolitical_rights_natural_or_constructed,
    'Are property rights over data, models, and compute genuinely pre-political features of political reality (a mountain), or a constructed legal allocation — extended from physical-property intuitions by identifiable interested parties during the past century — that concentrates advantage among capital holders?',
    'Comparative legal-historical analysis of how data and model property regimes were actually enacted and lobbied, cross-jurisdictional variation in what counts as protectable, and the track record of ''pre-political'' allocations being legislatively redrawn when coalitions shifted.',
    'If constructed, the mountain claim is a false summit and the constraint computes as extractive-with-beneficiaries (FSM territory), reclassifying the reading''s arrangement as defended rent-bearing structure; if genuinely pre-political, the low authored epsilon is confirmed and mandate-based rivals lose their normative footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prepolitical_rights_natural_or_constructed, empirical, 'Whether the reading''s naturality claim survives scrutiny of how the property regimes were built.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the market_libertarian_reading of kernel ai_governance_legitimacy — what structurally changes if a sibling reading were adopted instead?',
    'Adopting the magisterial_subsidiarity_reading relocates legitimacy to Magisterium-interpreted common-good conformity, converting the standing market arrangement''s residual frictions into substantial extraction (its authored epsilon over this same referent would be far higher) and converting the reading''s ''illegitimate coercion'' verdicts into binding obligations; adopting democratic_pluralist_reading makes mandates legitimate outputs of inclusive public reason, raising measured extraction identically while dissolving the naturality claim; adopting technocratic_optimization_reading demotes consent to one parameter among welfare considerations. The disagreement is located in the legitimacy SOURCE and in whether unconsented obligations bind.',
    'Classification of THIS arrangement flips from low-extraction mountain-like structure to substantially extractive structure under either the magisterial or democratic reading — the epsilon divergence across readings over one shared referent is the committer contest made measurable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame record: this file is one reading of a four-reading kernel; sibling adoptions change victim sets and epsilon over the same standing arrangement.').

omega_variable(
    exit_dignity_coverage_failure,
    'Does the exit-options dignity mechanism actually protect those without market power — monopsony workers, data subjects, third parties — or does the reading''s central dignity claim hold only for the already-autonomous?',
    'Longitudinal wage and mobility studies of dominated labor markets, natural experiments from jurisdictions strengthening worker exit (portability rules, classification reform) versus jurisdictions relying on pure voluntarism, and audit studies of practical opt-out feasibility for data subjects.',
    'If exit is nominal-only at the bottom of the distribution, the reading''s foundational dignity claim fails for exactly the population it invokes it to protect, and the residual epsilon understates real extraction — the arrangement would compute as extractive even by broadly sympathetic lights; if exit is substantively available, the low epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_dignity_coverage_failure, empirical, 'Scope of the dignity-through-exit guarantee across the bargaining-power distribution.').

omega_variable(
    subsidiarity_solidarity_boundary,
    'Where exactly does the reading draw the line between subsidiarity-style decentralization it endorses and solidarity-style demand it calls illegitimate coercion — and is that line principled or drawn wherever mandates begin to bind capital?',
    'Doctrinal analysis comparing the reading''s endorsed decentralization instruments (local option, private association, voluntary standards) against the encyclical''s solidarity demands (universal destination of goods, protection of the vulnerable), testing whether the rejection criterion is consent-dependence or incidence-on-capital.',
    'If the line is consent-dependence, the reading is internally coherent and its partial endorsement of subsidiarity is principled; if the line tracks incidence on capital holders, the endorsement is selective appropriation and the constraint carries ideological-lock-in dynamics worth a separate story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidiarity_solidarity_boundary, conceptual, 'Principled vs. interested location of the endorse/reject boundary within the encyclical''s social teaching.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_g_tr_t3, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement(ai_g_tr_t6, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement(ai_g_tr_t9, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 9, 0.26).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 15, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(ai_g_be_t3, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 3, 0.19).
narrative_ontology:measurement(ai_g_be_t6, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 6, 0.21).
narrative_ontology:measurement(ai_g_be_t9, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 9, 0.23).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 15, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_g_su_t3, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 3, 0.34).
narrative_ontology:measurement(ai_g_su_t6, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(ai_g_su_t9, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 9, 0.42).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 15, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, democratic_pluralist_reading).

% DUAL FORMULATION NOTE:
% This story is one member of a four-story constraint family decomposing the colloquial label 'AI governance legitimacy' per the epsilon-invariance principle. The single label covers four structurally distinct claims with different epsilon values, different beneficiary sets, and different victim sets: this reading (consent-sufficiency; low authored epsilon over a market-order referent), magisterial_subsidiarity_reading (Magisterium-interpreted common-good conformity; would author high epsilon for this same market referent), technocratic_optimization_reading (welfare-maximization authority), and democratic_pluralist_reading (deliberative consent of the governed). Each is authored as its own clean, epsilon-invariant file; the upstream/downstream citation traffic runs mainly from this reading and the magisterial reading into the democratic and technocratic ones, since both newer readings define themselves against the older legitimacy claims. Edges here are family links, not endorsements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
