% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: NDC Voluntary Pledge Architecture (Sovereigntist Reading)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the sovereigntist reading of the Paris
 *   Agreement's Article 4 NDC architecture: nationally determined
 *   contributions are voluntary, self-determined pledges, and the
 *   arrangement's defining feature is what it does not compel — no target is
 *   internationally binding, no penalty attaches to non-achievement, and any
 *   party may withdraw on twelve months' notice after three years of
 *   membership. Assessed by this reading's own lights, the standing
 *   arrangement is a low-extraction coordination system: it builds shared
 *   measurement, reporting, and review infrastructure while preserving
 *   exclusive national jurisdiction over energy policy. The claim/metric
 *   independence rule applies throughout: rope is my structural claim, and
 *   the metrics are my separate descriptive assessment of how the arrangement
 *   actually operates. The sibling readings (supranational, equity) are
 *   separate constraints in separate files; their critiques are routed to
 *   omega variables rather than folded into this classification. KEY AGENTS
 *   (by structural relationship): - fossil_dependent_exporters: primary
 *   beneficiary (organized/mobile) — preserve export demand and development
 *   pathways at minimal membership cost - rapidly_industrializing_economies:
 *   primary beneficiary (institutional/mobile) — no externally imposed
 *   peaking schedule - developed_economy_parties: beneficiary with payer
 *   secondary role (institutional/mobile) — buy legitimacy and coordination
 *   goods with voluntary finance and reporting - climate_vulnerable_alliance:
 *   payer (organized/trapped) — supply diplomatic labor, bear
 *   aggregate-shortfall exposure, hold no lever over others' pledges -
 *   unfccc_secretariat: agenda_setter (institutional/constrained) —
 *   administers registry, reviews, and stocktake logistics; decides nothing
 *   of substance - independent_climate_assessment_bodies: analytical observer
 *   (organized/analytical) — measures the pledge-implementation gap from
 *   outside - youth_climate_movements: excluded voice (organized/trapped) —
 *   acts on the stakes, holds no seat
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.2).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.12).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "NDC Voluntary Pledge Architecture (Sovereigntist Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international_climate_governance/treaty_law/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, '44c30efb-b93b-47fd-bc38-3cfaed546880').
narrative_ontology:cs_kernel_codification('44c30efb-b93b-47fd-bc38-3cfaed546880', fixed_text).
narrative_ontology:cs_authority_grounding('44c30efb-b93b-47fd-bc38-3cfaed546880', self_enforcing).
narrative_ontology:cs_reading_relation('44c30efb-b93b-47fd-bc38-3cfaed546880', paris_article_4_ndc__paris_article_4_ndc_supranational_reading, forecloses).
narrative_ontology:cs_reading_relation('44c30efb-b93b-47fd-bc38-3cfaed546880', paris_article_4_ndc__paris_article_4_ndc_equity_reading, coexists_with).
narrative_ontology:cs_axiom('44c30efb-b93b-47fd-bc38-3cfaed546880', foundational, pledge_content_domestically_determined).
narrative_ontology:cs_axiom_status(pledge_content_domestically_determined, holdable).
narrative_ontology:cs_axiom_grounding('44c30efb-b93b-47fd-bc38-3cfaed546880', pledge_content_domestically_determined, conventional).
narrative_ontology:cs_axiom('44c30efb-b93b-47fd-bc38-3cfaed546880', secondary, compliance_through_facilitation_not_penalties).
narrative_ontology:cs_axiom_status(compliance_through_facilitation_not_penalties, holdable).
narrative_ontology:cs_axiom_grounding('44c30efb-b93b-47fd-bc38-3cfaed546880', compliance_through_facilitation_not_penalties, conventional).
narrative_ontology:cs_reference_frame('44c30efb-b93b-47fd-bc38-3cfaed546880', sovereign_consent_pledge_review).
narrative_ontology:cs_drift_state('44c30efb-b93b-47fd-bc38-3cfaed546880', post_first_global_stocktake, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('44c30efb-b93b-47fd-bc38-3cfaed546880', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_exporters).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, rapidly_industrializing_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, developed_economy_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, developed_economy_parties).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_alliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States and blocs whose fiscal stability and export revenue depend on continued global fossil fuel demand — oil and gas exporters, coal-reliant economies. The arrangement asks nothing of their energy mix: no body can set a phase-out date for them, and the consensus rule lets them shape or block text that would. Membership costs them little — reports, delegations, occasional finance pledges — and staying inside preserves a seat where demand-side pressure can be managed. Leaving would forfeit that seat and gain nothing, since nothing binds them here anyway.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_exporters, beneficiary,
    organized, generational, mobile, global).

% Large emerging economies whose development strategies still run through expanding energy consumption. Under this architecture they choose their own peaking timelines and target shapes; no external schedule is imposed, and the differentiation language they negotiated remains in the treaty preamble. They submit pledges sized to domestic plans, report on them, and face no consequence if delivery lags. Exit is available and occasionally threatened, but the venue's legitimacy and its technology-cooperation channels are worth the modest dues.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, rapidly_industrializing_economies, beneficiary,
    institutional, generational, mobile, global).

% Industrialized parties that accept the framework as the price of universal membership. They gain a common accounting standard, a venue where their own ambition can be showcased, and insulation from the binding-target politics that sank the previous treaty. They pay in voluntary finance contributions, reporting labor, and reputational exposure when delivery trails pledges. Withdrawal is legally straightforward — one large party has exercised it twice — but carries a diplomatic cost they have been willing to pay only intermittently.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, developed_economy_parties, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__sovereigntist_reading, developed_economy_parties, payer).

% Coalitions of small island and least-developed states facing existential exposure to warming they did little to cause. They send delegations, negotiate text, and supply much of the process's moral energy; what comes back depends entirely on other parties' discretionary choices, since nothing in the architecture compels anyone's ambition. Their alternative to participation is isolation from the only universal venue that records their plight — so they stay, spend scarce capacity, and hold a veto over rules but no lever over outcomes.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, climate_vulnerable_alliance, payer,
    organized, immediate, trapped, global).

% The treaty body that keeps the machinery running: maintains the NDC registry, organizes transparency reviews and the global stocktake synthesis, services the annual conferences. It administers everything and decides nothing of substance — rules require party consensus, and it serves at the parties' pleasure, funded by their contributions. Its institutional survival depends on the process continuing, whatever the process produces.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Research consortia and UN assessment programs outside the negotiation rooms that compare announced pledges against measured trajectories and published implementation gaps. They consume the transparency outputs, translate them into public scorecards, and are the seat that sees the full spread between what parties say and what emissions do. They collect nothing from the process and answer to no party.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, independent_climate_assessment_bodies, observer,
    organized, biographical, analytical, global).

% Mass mobilizations and advocacy networks acting on the stakes the negotiations manage. They have no formal seat: no vote, no pledge, no delegation rights in the sessions that determine the architecture. Their influence runs through protest, litigation, and pressure on national politics — channels that exist entirely outside the treaty structure they are trying to move.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, youth_climate_movements, excluded,
    organized, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__sovereigntist_reading, diffuse).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the shared machinery without which national climate efforts cannot be compared or aggregated: a common reporting and accounting framework, a public NDC registry, five-yearly global stocktakes that synthesize collective progress, and a standing negotiation forum. It solves the information and measurement coordination problem — who is doing what, stated comparably — while deliberately leaving the level of effort to each party.
% TRANSFER_FUNCTION: Moves little of material value. What circulates is attention, reputational standing, and administrative effort: parties transfer reporting and diplomatic labor into the process; the stocktake and peer review return normative pressure and public visibility; voluntary finance pledges move funds outside the compliance core. No compulsory transfer runs through the pledge architecture itself.
% ABSENT_VOICES: Future generations and populations already experiencing climate losses hold no seat; youth movements mobilize outside the process with no formal access to pledge-setting. Within the process, climate-vulnerable delegations are present but structurally voiceless over others' pledges — consensus gives every party a veto over rules yet no lever over anyone's ambition. Advocates of binding accountability operate outside the consensus and would object to the voluntariness itself.
% DISAPPEARANCE_RATIONALE: The reporting infrastructure, registry, stocktake cycle, and diplomatic focal point would vanish; climate diplomacy would reorganize within years around minilateral substitutes — G20 statements, bilateral deals, sectoral clubs — with worse comparability and no universal membership. The physical emissions trajectory would shift little at first, since the architecture compels nothing, but the loss of a common measurement standard and a universal venue would degrade every subsequent coordination attempt.
% FOUNDING_PROBLEM: After the previous treaty's compliance architecture failed — one major emitter never ratified it, another withdrew to escape penalties — the founding problem was designing a framework that could achieve near-universal participation, including major emitters and developing economies, by making commitment content a matter of national choice while still generating upward ambition pressure through transparency and periodic review.
% FOUNDING_PROBLEM_CORROBORATION: The participation diagnosis is corroborated outside the benefiting parties by treaty-design scholarship documenting the prior treaty's ratification failure, and by the near-universal ratification that followed. The status is disputed: UNEP Emissions Gap assessments and independent pledge-tracking consortia attest that the ambition problem the architecture was meant to progressively solve remains unsolved, while party communiqués attest the process functions as designed. No neutral source attests that the founding problem is simply dead.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).
:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.20 at interval end) because the architecture compels nothing material: its costs are reporting labor, diplomatic attendance, and reputational exposure, and its headline discipline — the five-year ratchet — operates through facilitation rather than sanction. Suppression is very low (0.12): exit is codified, non-ratification remains available, and free-riding inside the regime carries no formal consequence; the series declines across the interval as enforcement expectations atrophied from an already-facilitative baseline. Theater is moderate and rising (0.42): pledge announcements, conference ceremonies, and stocktake spectacle increasingly outrun implementation linkage, though the transparency machinery still produces real, comparable data. Accessibility collapse is low (0.25): alternatives — exit, minilateralism, open free-riding — survive contact with the arrangement. Resistance is low-moderate (0.30): the arrangement is resisted mainly by those who want it to bind harder, a resistance to its weakness rather than to its operation. The three measurement series share one six-point grid (2015–2025 at two-year steps, t=0 to t=10). Dynamics are monotone drift with episodic pulses around stocktake years; the pulses register as steps in theater rather than sustained oscillation, so no cyclical series is asserted and no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the fossil-exporter and industrializer seats the arrangement computes as near-pure coordination: it costs little, compels nothing, and protects jurisdiction. From the vulnerable-alliance seat the same structure is a venue that consumes scarce diplomatic capacity and returns outcomes wholly contingent on others' discretion — the experience of paying into a system whose benefits are priced beyond one's reach. The secretariat experiences administration without authority; the assessment bodies see the pledge-implementation gap that participant seats do not price into their own ledgers. The engine computes these per-seat classifications from the structural data; this story authors the data, not the verdicts.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (fossil exporters, industrializers, developed parties) derive directionality near the beneficiary pole: the arrangement subsidizes their preferred outcome — sovereignty retained, compulsion avoided — and their mobile exit options anchor them at low d. The vulnerable alliance is the hardest seat: it is deliberately not declared a victim under this reading, which attributes their exposure to aggregate emissions rather than to anything the architecture takes from them; but trapped exit and one-way cost-bearing push its derived d toward the middle-upper range rather than the beneficiary pole. The secretariat is administratively central yet collects no rents. No directionality overrides are authored: the beneficiary declarations plus exit options capture the structure, and the residual ambiguity about the vulnerable seat is carried as an omega (vulnerable_seat_cost_attribution) instead of a forced override.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against two mislabels. Against snare: nothing is compelled, no seat captures the gains, and exit is codified — the coercive signature is absent. Against piton: the coordination function (common accounting, transparency, universal venue) is still performed and consumed, so the theater ratio, while rising, has not swallowed the function. The live risk this reading registers is drift: if implementation linkage continues to decouple from pledge performance, theater crosses the functional threshold and the arrangement degrades toward theatrical maintenance — the trajectory the rising theater series tracks. Mandatrophy is not resolved: the founding participation problem was solved by design, but the framework's ongoing warrant — progressive ambition — is exactly what the independent assessments dispute, leaving the mandate's status contested rather than dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading correctly characterizes Article 4''s legal force — voluntary self-determination (this file), binding ratcheting accountability (supranational_reading), or CBDR-structured differentiation (equity_reading)?',
    'Authoritative interpretation: ICJ and ITLOS jurisprudence on the obligatory character of the Paris provisions, CMA decisions on implementation, and accumulated state practice and opinio juris.',
    'If the supranational reading prevails, epsilon rises sharply, victims appear (states subject to international accountability), and the computed type shifts toward tangled_rope; if the equity reading prevails, the beneficiary and victim sets restructure along development-status lines. This file''s rope classification holds only within the sovereigntist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of the paris_article_4_ndc kernel; sibling readings instantiate different constraints with different epsilon and victim structures.').

omega_variable(
    informal_enforcement_question,
    'Does the absence of formal penalty mean absence of effective compulsion, or do reputational cycles, stocktake naming, and finance-linkage function as informal enforcement that the suppression scalar undercounts?',
    'Behavioral studies of NDC revision following stocktake rounds and peer review; analysis of whether finance access and diplomatic standing vary measurably with pledge performance.',
    'Demonstrable informal enforcement would raise effective suppression and extraction and push the computed type toward tangled_rope; confirmed non-enforcement supports the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_enforcement_question, empirical, 'Whether pledge-and-review exerts real compulsion beneath its formally voluntary surface.').

omega_variable(
    vulnerable_seat_cost_attribution,
    'Do climate-vulnerable states bear costs imposed by the voluntary architecture itself (a venue that consumes their diplomatic capacity and returns wholly discretionary outcomes), or are their losses attributable to aggregate emissions regardless of pledge design?',
    'Counterfactual treaty-design analysis: would a binding-compliance counterfactual plausibly have produced emission paths sufficient to alter vulnerable states'' exposure, and at what participation cost?',
    'If the architecture itself imposes the cost, victims must be declared, the vulnerable seat''s directionality moves toward the target pole, and the type shifts toward tangled_rope; if not, the current no-victim structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_seat_cost_attribution, conceptual, 'Where the equity reading''s critique would enter this story if credited — the boundary between coordination-failure cost and imposed cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement(pari_tr_t2, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2, 0.27).
narrative_ontology:measurement(pari_tr_t4, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement(pari_tr_t6, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement(pari_tr_t8, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(pari_be_t2, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2, 0.24).
narrative_ontology:measurement(pari_be_t4, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 4, 0.23).
narrative_ontology:measurement(pari_be_t6, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement(pari_be_t8, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 8, 0.21).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 10, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(pari_su_t2, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2, 0.17).
narrative_ontology:measurement(pari_su_t4, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 4, 0.16).
narrative_ontology:measurement(pari_su_t6, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 6, 0.14).
narrative_ontology:measurement(pari_su_t8, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 8, 0.13).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, information_standard).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc_supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc_equity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Paris NDC regime' decomposes into three structurally distinct constraints, one per reading of the Article 4 kernel. This file is the sovereigntist reading (epsilon 0.20, no declared victims, rope claim). The supranational reading authors the same standing arrangement as binding ratcheting accountability with higher epsilon and state-subject victims; the equity reading restructures beneficiaries and victims along development-status lines. Epsilon differs across the family because each reading assesses the same arrangement by its own lights, not because the arrangement differs; each file links the others here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
